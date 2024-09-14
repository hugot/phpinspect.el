;;; test-completion.el --- Unit tests for phpinspect-completion.el  -*- lexical-binding: t; -*-

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

(require 'phpinspect-completion)
(require 'phpinspect-buffer)
(require 'ert)

(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))

(ert-deftest phpinspect-dont-complete-in-function-declaration ()
    (with-temp-buffer
    (insert "<?php

namespace App;

class Foo
{

 public function  ()")

    (backward-char 3)

    (phpinspect-claim-buffer
     (current-buffer) (phpinspect--make-dummy-composer-project-with-code))

    (let* ((query (phpinspect--get-completion-query))
           result)

      (setq result (phpinspect-completion-query-execute query))
      (should-not (phpinspect--completion-list-has-candidates result))

      (insert "F")

      (setq query (phpinspect--get-completion-query)
            result (phpinspect-completion-query-execute query))
      (should-not (phpinspect--completion-list-has-candidates result)))))

(ert-deftest phpinspect-complete-types-at-point ()
  (with-temp-buffer
    (insert "<?php

namespace App;

use \\DateTime;

new ")

    (phpinspect-claim-buffer
     (current-buffer) (phpinspect--make-dummy-composer-project-with-code))

    (let* ((query (phpinspect--get-completion-query))
	       (completions (phpinspect-completion-query-execute query))
           (types (phpinspect--completion-list-strings completions)))

	  (should (length= types 6))
	  (should (equal (sort (list "Bar" "Barry" "Baz" "Foo" "Harry" "DateTime") #'string<)
		             (sort (mapcar #'substring-no-properties types) #'string<))))))


(ert-deftest phpinspect-complete-inside-scope ()
    (with-temp-buffer
    (insert "<?php

namespace App;

use \\DateTime;

class A {
public ")

    (phpinspect-claim-buffer
     (current-buffer) (phpinspect--make-dummy-composer-project-with-code))

    (let* ((query (phpinspect--get-completion-query))
	       (completions (phpinspect-completion-query-execute query))
           (strings (phpinspect--completion-list-strings completions)))

      (should strings))))

(ert-deftest phpinspect-dont-complete-after-method-body ()
    (with-temp-buffer
    (insert "<?php

namespace App;

use \\DateTime;

class A {
public function a() {} ")

    (phpinspect-claim-buffer
     (current-buffer) (phpinspect--make-dummy-composer-project-with-code))

    (let* ((query (phpinspect--get-completion-query))
	       (completions (phpinspect-completion-query-execute query))
           (strings (phpinspect--completion-list-strings completions)))

      (should-not strings))))

(ert-deftest phpinspect-dont-complete-within-comments ()
  (let ((cases (list "function a() { new // "
                     "/* "
                     "// ")))
    (dolist (prefix cases)
      (with-temp-buffer
        (insert "<?php

namespace App;

use \\DateTime;

class A { ")
        (insert prefix)

        (phpinspect-claim-buffer
         (current-buffer) (phpinspect--make-dummy-composer-project-with-code))

        (let* ((query (phpinspect--get-completion-query))
	           (completions (phpinspect-completion-query-execute query))
               (strings (phpinspect--completion-list-strings completions)))

          (should-not (phpinspect--completion-list-has-candidates completions))
          (should-not strings))))))
