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
