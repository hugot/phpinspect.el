;; test-project.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Hugo Thunnissen <devel@hugot.nl>

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

(require 'ert)
(require 'phpinspect-project)
(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))


(ert-deftest phpinspect-project-purge ()
  (let ((project (phpinspect--make-project)))
    (phpinspect-project-purge project)

    (should (eq t (phpinspect-project-purged project)))))

(ert-deftest phpinspect-project-watch-file-and-purge ()
  (let* ((root (make-temp-file "phpinspect-test" 'dir))
         (fs (phpinspect-make-fs))
         (watch-file (concat root "/watch1"))
         (project (phpinspect--make-project :fs fs :root root)))
    (phpinspect-project-watch-file project watch-file #'ignore)

    (phpinspect-project-purge project)

    (should (= 0 (length (hash-table-values (phpinspect-project-file-watchers project)))))))

(ert-deftest phpinspect-project-no-enqueue ()
  (let* ((project (phpinspect--make-dummy-composer-project-with-code))
         (bar (phpinspect-project-get-typedef-extra-or-create
               project (phpinspect--make-type :name "\\App\\Bar") 'no-enqueue))
         ;; Fetch foo without passing no-enqueue
         (foo (phpinspect-project-get-typedef-extra-or-create
               project (phpinspect--make-type :name "\\App\\Foo")))
         (baz  (phpinspect-project-get-typedef-extra-or-create
               project (phpinspect--make-type :name "\\App\\Baz")))
         (barry (phpinspect-project-get-typedef-extra-or-create
                 project (phpinspect--make-type :name "\\App\\Barry")))
         (harry (phpinspect-project-get-typedef-extra-or-create
                 project (phpinspect--make-type :name "\\App\\Harry"))))
    ;; Bar includes foo's method
    (should bar)
    (should (= 2 (length (phpi-typedef-get-methods bar))))

    ;; Foo is loaded/indexed
    (should foo)
    (should (= 1 (length (phpi-typedef-get-methods foo))))

    ;; Baz should be loaded as dependency of foo
    (should baz)
    (should (phpi-typedef-initial-index baz))

    ;; barry and harry are not loaded (worker is null, so they are not indexed
    ;; in the background either).
    (should harry)
    (should-not (phpi-typedef-initial-index harry))
    (should barry)
    (should-not (phpi-typedef-initial-index barry))

    ;; Trigger index of barry
    (phpinspect-project-get-typedef-extra-or-create
     project (phpinspect--make-type :name "\\App\\Barry") 'no-enqueue)

    ;; barry and harry should be loaded now (harry as dependency of barry)
    (should (phpi-typedef-initial-index barry))
    (should (phpi-typedef-initial-index harry))))

(ert-deftest phpinspect-project-late-static-binding ()
  (let* ((project (phpinspect--make-dummy-composer-project-with-code))
         (bar (phpinspect-project-get-typedef-extra-or-create
               project (phpinspect--make-type :name "\\App\\Bar") 'no-enqueue)))

    (should bar)
    (should (= 2 (length (phpi-typedef-get-methods bar))))

    (let ((method (phpi-typedef-get-method bar "do")))
      (should (phpinspect--type= (phpinspect--make-type :name "\\App\\Bar")
                                   (phpi-method-return-type method)))
      (should (phpinspect--type= (phpinspect--make-type :name "\\App\\Bar")
                                   (phpi-fn-return-type method))))))
