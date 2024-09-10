;;; test-suggest.el --- Unit tests for phpinspect-suggest.el  -*- lexical-binding: t; -*-

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

(require 'phpinspect-suggest)
(require 'phpinspect-buffer)
(require 'ert)

(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))

(ert-deftest phpinspect-suggest-variables-at-point-foreach ()
  (with-temp-buffer
    (insert "<?php foreach ($array as $key => $value) {")

    (let* ((buffer (phpinspect-make-buffer :buffer (current-buffer)))

	   (rctx (phpinspect-get-resolvecontext
		  (phpinspect--make-dummy-project) (phpinspect-buffer-parse-map buffer) (point)))
	   (variables (phpinspect-suggest-variables-at-point rctx)))

      (should (length= variables 3))
      (should (equal (list "array" "key" "value")
		     (sort (mapcar #'phpi-var-name variables) #'string<))))))

(ert-deftest phpinspect-suggest-types-at-point ()
  (with-temp-buffer
    (insert "<?php
use App\\Baz;
use App\\Foo;

new ")

    (let* ((buffer (phpinspect-make-buffer
		    :buffer (current-buffer)
		    :-project (phpinspect--make-dummy-composer-project-with-code)))
	   (rctx (phpinspect-buffer-get-resolvecontext buffer (point)))
	   (types (phpinspect-suggest-words-at-point rctx)))
      (setq-local phpinspect-current-buffer buffer)
      (add-hook 'after-change-functions #'phpinspect-after-change-function)
      (should (length= types 2))
      (should (equal (list "\\App\\Baz" "\\App\\Foo")
		     (sort (mapcar #'phpinspect--type-name types) #'string<)))

      (insert "Fo")
      (setq rctx (phpinspect-buffer-get-resolvecontext buffer (point))
	    types (phpinspect-suggest-words-at-point rctx))
      (should (length= types 2))
      (should (equal (list "\\App\\Baz" "\\App\\Foo")
		     (sort (mapcar #'phpinspect--type-name types) #'string<))))))

(ert-deftest phpinspect-suggest-types-at-point-include-current-namespace ()
  (with-temp-buffer
    (insert "<?php

namespace App;

use \\DateTime;

new ")

      (let* ((buffer (phpinspect-make-buffer
		      :buffer (current-buffer)
		      :-project (phpinspect--make-dummy-composer-project-with-code)))
	     (rctx (phpinspect-buffer-get-resolvecontext buffer (point)))
	     (types (phpinspect-suggest-words-at-point rctx)))
	(should (length= types 6))
	(should (equal (list "\\App\\Bar" "\\App\\Barry" "\\App\\Baz" "\\App\\Foo" "\\App\\Harry" "\\DateTime")
		       (sort (mapcar #'phpinspect--type-name types) #'string<))))))

(ert-deftest phpinspect-suggest-keywords-at-point-class-body ()
  (with-temp-buffer
    (insert "<?php

namespace App;

class Foo
{

 ")

      (let* ((buffer (phpinspect-make-buffer
		      :buffer (current-buffer)
		      :-project (phpinspect--make-dummy-composer-project-with-code)))
	     (rctx (phpinspect-buffer-get-resolvecontext buffer (point)))
	     (words (phpinspect-suggest-words-at-point rctx)))
	(should (equal (list "const" "function" "private" "protected" "public" "static")
		       (sort (mapcar #'phpinspect-suggest-keyword-word words) #'string<))))))

(ert-deftest phpinspect-suggest-words-at-point-scope ()
  (with-temp-buffer
    (insert "<?php

namespace App;

class Foo
{

public ")

    (let* ((buffer (phpinspect-claim-buffer
		    (current-buffer)
		    (phpinspect--make-dummy-composer-project-with-code)))
	     (rctx (phpinspect-buffer-get-resolvecontext buffer (point)))
	     (results (phpinspect-suggest-words-at-point rctx))
	     (types (seq-filter #'phpinspect--type-p results))
	     (words (seq-filter #'phpinspect-suggest-keyword-p results)))
	(should (length= results (+ (length types) (length words))))
	(should (equal (list "function" "static")
		       (sort (mapcar #'phpinspect-suggest-keyword-word words) #'string<)))

	(insert "F")

	(setq rctx (phpinspect-buffer-get-resolvecontext buffer (point))
	      results (phpinspect-suggest-words-at-point rctx)
	      types (seq-filter #'phpinspect--type-p results)
	      words (seq-filter #'phpinspect-suggest-keyword-p results))
	(should (length= results (+ (length types) (length words))))
	(should (equal (list "function" "static")
		           (sort (mapcar #'phpinspect-suggest-keyword-word words) #'string<))))))

(ert-deftest phpinspect-suggest-static-method-at-point ()
  (dolist (prefix (list "Foo::" "$bar = Foo::"))

    (with-temp-buffer
      (insert "<?php

namespace App;

 ")
      (insert prefix)

      (let* ((buffer (phpinspect-claim-buffer
		              (current-buffer)
		              (phpinspect--make-dummy-composer-project-with-code)))
	         (rctx (phpinspect-buffer-get-resolvecontext buffer (point)))
	         (results (phpinspect-suggest-attributes-at-point rctx t))_)

        (should (length= results 1))
        (should (equal (list "dont")
                       (mapcar #'phpi-fn-name results)))))))
