;;; phpinspect-test.el --- Unit tests for phpinslect.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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
(require 'phpinspect)

(defvar phpinspect-test-php-file-directory
  (concat
   (file-name-directory
    (or load-file-name
        buffer-file-name))
   "/fixtures")
  "Directory with syntax trees of example PHP files.")

(defun phpinspect-test-read-fixture-tree (name)
  (with-temp-buffer
    (insert-file-contents-literally (concat phpinspect-test-php-file-directory "/" name ".el"))
    (read (current-buffer))))

(defun phpinspect-test-parse-fixture-code (name)
  (phpinspect-parse-file
   (concat phpinspect-test-php-file-directory "/" name ".php")))

(ert-deftest phpinspect-parse-namespaced-class ()
  "Test phpinspect-parse on a namespaced class"
    (should
   (equal (phpinspect-test-read-fixture-tree "NamespacedClass")
          (phpinspect-test-parse-fixture-code "NamespacedClass"))))

(ert-deftest phpinspect-parse-block ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-tree "Block")
          (phpinspect-test-parse-fixture-code "Block"))))

(ert-deftest phpinspect-parse-functions ()
  "Test phpinspect-parse for php functions"
  (should
   (equal (phpinspect-test-read-fixture-tree "Functions")
          (phpinspect-test-parse-fixture-code "Functions"))))

(ert-deftest phpinspect-parse-namespaced-functions ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-tree "NamespacedFunctions")
          (phpinspect-test-parse-fixture-code "NamespacedFunctions"))))

(provide 'phpinspect-test)
;;; phpinspect-test.el ends here
