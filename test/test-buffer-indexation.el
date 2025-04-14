;;; test-buffer-indexation.el --- Tests for buffer indexation  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'phpinspect-buffer)
(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))

(ert-deftest phpinspect-index-incomplete-class-const ()
  "This is a functional test, to confirm that indexation is executed
without errors being thrown."
  (with-temp-buffer
    (let ((buffer (phpinspect-claim-buffer (current-buffer) (phpinspect--make-dummy-project))))
      (insert "class C { private ?\\DateTime $d; public function __construct() {}")
      (phpinspect-buffer-update-project-index buffer)

      (goto-char 10)
      (insert " const ")

      (phpinspect-buffer-update-project-index buffer)
      (insert "a")
      (phpinspect-buffer-update-project-index buffer))))

(ert-deftest phpinspect-index-anonymous-class-return ()
  "Confirm that anonymous classes can be parsed/indexed without errors.
Does not test any related functionalities."
  (with-temp-buffer
    (let ((buffer (phpinspect-claim-buffer (current-buffer) (phpinspect--make-dummy-project))))
      (insert "return new class() { private ?\\DateTime $d; public function __construct() {}")
      (phpinspect-buffer-update-project-index buffer))))

(ert-deftest phpinspect-index-after-fix-imports-deleted-use ()
  (with-temp-buffer
    (let ((buffer (phpinspect-claim-buffer
                   (current-buffer) (phpinspect--make-dummy-project))))
      (insert "<?php

namespace Tests\\Unit;

use App\\CIS\\Reports\\Functions\\ReferringItems;
use App\\CIS\\Reports\\ReportState;
use App\\CIS\\Reports\\TwigBuilder;
use Database\\Seeders\\TreeSeeder;
use League\\Tactician\\CommandBus;
use Tests\\TestCase;

class ReportTest extends TestCase
{
    private ReportState $reportState;
    private TreeSeeder $treeSeeder;

    private $foo;

    public function setUp(): void
    {
        $this->bus = resolve(CommandBus::class);
    }
}")

      (phpinspect-buffer-update-project-index buffer)

      (phpinspect-fix-imports)
      (phpinspect-buffer-update-project-index buffer))))

(ert-deftest phpinspect-index-this ()
  (with-temp-buffer
    (let ((buffer (phpinspect-claim-buffer
                   (current-buffer) (phpinspect--make-dummy-composer-project-with-code))))

      (insert "<?php

namespace Z;

class A
{
    private Barry $barry;

    static function makeB(): \\B
    {
    }

    function nananana()
    {
        $this->t = self::makeB();
        $this->harry = $this->barry->getHarry();
    }
}")
      (phpinspect-buffer-update-project-index buffer)

      (let ((typedef (phpinspect-project-get-typedef
                      (phpinspect-buffer-project buffer)
                      (phpinspect--make-type :name "\\Z\\A")))
            property)

        (should typedef)
        (should (setq property (phpi-typedef-get-property typedef "t")))

        (should (phpinspect--type= (phpinspect--make-type :name "\\B")
                                   (phpi-prop-type property)))

        (should (setq property (phpi-typedef-get-property typedef "harry")))

        (should (phpinspect--type= phpinspect--unknown-type (phpi-prop-type property)))

        ;; Adds import for "Barry"
        (phpinspect-fix-imports)
        (phpinspect-buffer-update-project-index buffer)

        (should (setq property (phpi-typedef-get-property typedef "harry")))
        (should (phpinspect--type= (phpinspect--make-type :name "\\App\\Harry")
                                   (phpi-prop-type property)))))))

(ert-deftest phpinspect-sync-property-type ()
  (with-temp-buffer
    (let ((buffer (phpinspect-claim-buffer
                   (current-buffer) (phpinspect--make-dummy-composer-project-with-code))))

      (insert "<?php

namespace Example;

use DateTime;

class Sample
{
    private DateTime $date;

    public function setDate(DateTime $date)
    {
        $this->date = $date;
    }

    public function getDate(): DateTime
    {
        return $this->date;
    }
}")
      (phpinspect-buffer-update-project-index buffer)

      (let ((typedef (phpinspect-project-get-typedef
                      (phpinspect-buffer-project buffer)
                      (phpinspect--make-type :name "\\Example\\Sample")))
            property)

        (should typedef)
        (should (setq property (phpi-typedef-get-property typedef "date")))

        (should (phpinspect--type= (phpinspect--make-type :name "\\DateTime")
                                   (phpi-prop-type property)))

        ;; Change the type of the property to DateTimeImmutable
        (goto-char 70) ;; Position right after "private "
        (delete-region 70 78) ;; Delete "DateTime"
        (insert "\\DateTimeImmutable")
        (phpinspect-buffer-update-project-index buffer)

        (should (setq property (phpi-typedef-get-property typedef "date")))
        (should (phpi-prop-type property))
        (should (phpinspect--type= (phpinspect--make-type :name "\\DateTimeImmutable")
                                   (phpi-prop-type property)))

        ;; Change the type of the property to string
        (goto-char 71) ;; Position right after "private "
        (delete-region 71 89) ;; Delete "DateTimeImmutable"
        (insert "string")
        (phpinspect-buffer-update-project-index buffer)

        (should (setq property (phpi-typedef-get-property typedef "date")))
        (should (phpinspect--type= (phpinspect--make-type :name "\\string")
                                   (phpi-prop-type property)))

        ;; Remove the property
        (goto-char 63) ;; Position right before "private string $date;"
        (delete-region 63 82) ;; Delete "private string $date;"
        (phpinspect-buffer-update-project-index buffer)

        (should-not (phpi-typedef-get-property typedef "date"))))))

(provide 'test-buffer-indexation)
;;; test-buffer-indexation.el ends here
