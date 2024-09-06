;;; test-token-predicates.el --- Unit tests for phpinspect-token-predicates.el  -*- lexical-binding: t; -*-

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

(ert-deftest phpinspect-incomplete-token-p ()
  (let ((tokens-should `((:public)
			 (:function (:word "ba"))
			 (:function (:list))
			 ,(cadr (phpinspect-parse-string "function ba ()"))
			 ,(cadr (phpinspect-parse-string "function ()"))
			 ,(cadr (phpinspect-parse-string "function ();"))
			 ,(cadr (phpinspect-parse-string "function () {}"))
			 ,(cadr (phpinspect-parse-string "function function ()"))
			 (:static)
			 (:static (:variable "foo"))
			 (:const (:word "AAAA"))
			 (:public (:variable "foo") (:assignment "=") (:incomplete-array))))

	(tokens-should-not `(,(cadr (phpinspect-parse-string "function ba () {}"))
			     ,(cadr (phpinspect-parse-string "function ba ();"))
			     ,(cadr (phpinspect-parse-string "function function ();"))
			     (:public (:variable "a") (:terminator ";")))))

    (dolist (token tokens-should)
      (should (phpinspect-incomplete-token-p token)))

    (dolist (token tokens-should-not)
      (should-not (phpinspect-incomplete-token-p token)))))
