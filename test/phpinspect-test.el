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

(defun phpinspect-test-read-fixture-data (name)
  (with-temp-buffer
    (insert-file-contents-literally (concat phpinspect-test-php-file-directory "/" name ".eld"))
    (read (current-buffer))))

(defun phpinspect-test-parse-fixture-code (name)
  (phpinspect-parse-file
   (concat phpinspect-test-php-file-directory "/" name ".php")))

(ert-deftest phpinspect-parse-namespaced-class ()
  "Test phpinspect-parse on a namespaced class"
  (should
   (equal (phpinspect-test-read-fixture-data "NamespacedClass")
          (phpinspect-test-parse-fixture-code "NamespacedClass"))))

(ert-deftest phpinspect-parse-block ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "Block")
          (phpinspect-test-parse-fixture-code "Block"))))

(ert-deftest phpinspect-parse-functions ()
  "Test phpinspect-parse for php functions"
  (should
   (equal (phpinspect-test-read-fixture-data "Functions")
          (phpinspect-test-parse-fixture-code "Functions"))))

(ert-deftest phpinspect-parse-namespaced-functions ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "NamespacedFunctions")
          (phpinspect-test-parse-fixture-code "NamespacedFunctions"))))

(ert-deftest phpinspect-parse-variable ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "Variable")
          (phpinspect-test-parse-fixture-code "Variable"))))

(ert-deftest phpinspect-parse-word ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "Word")
          (phpinspect-test-parse-fixture-code "Word"))))

(ert-deftest phpinspect-parse-array ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "Array")
          (phpinspect-test-parse-fixture-code "Array"))))


(ert-deftest phpinspect-parse-short-function ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "ShortFunction")
          (phpinspect-test-parse-fixture-code "ShortFunction"))))

(ert-deftest phpinspect-parse-two-short-functions ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "TwoShortFunctions")
          (phpinspect-test-parse-fixture-code "TwoShortFunctions"))))

(ert-deftest phpinspect-parse-small-namespaced-class ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "SmallNamespacedClass")
          (phpinspect-test-parse-fixture-code "SmallNamespacedClass"))))

;; If this test fails, the syntax tree has a breaking change in it. Regenerate the
;; fixtures and fix anything that is broken.
(ert-deftest phpinspect-syntax-tree-change ()
  (let ((index (phpinspect--index-tokens
                (phpinspect-test-parse-fixture-code "IndexClass1")))
        (expected-result (phpinspect--index-tokens
                          (phpinspect-test-read-fixture-data "IndexClass1"))))
    (should (equal index expected-result))))

(ert-deftest phpinspect-index-tokens ()
  (should (equal
           (phpinspect--index-tokens
            (phpinspect-test-read-fixture-data "IndexClass1"))
           (phpinspect-test-read-fixture-data "IndexClass1-indexed"))))

(ert-deftest phpinspect-merge-class-indexes ()
  (should (equal
           (phpinspect--merge-indexes
            (phpinspect-test-read-fixture-data "IndexClass1-indexed")
            (phpinspect-test-read-fixture-data "IndexClass2-indexed"))
           (phpinspect-test-read-fixture-data
            "class-index-1-2-undestructive-merge"))))

(ert-deftest phpinspect-find-innermost-incomplete-nested-token ()
  (let ((resolvecontext (phpinspect--get-resolvecontext
                         (phpinspect-test-read-fixture-data "IncompleteClass"))))

    (should (equal (phpinspect--resolvecontext-subject resolvecontext)
                   '((:variable "this")
                     (:object-attrib (:word "em"))
                     (:object-attrib nil))))

    (should (phpinspect-root-p
             (car (last (phpinspect--resolvecontext-enclosing-tokens
                         resolvecontext)))))

    (should (phpinspect-incomplete-list-p
             (car (phpinspect--resolvecontext-enclosing-tokens
                   resolvecontext))))

    (should (phpinspect-incomplete-function-p
             (cadr (phpinspect--resolvecontext-enclosing-tokens
                    resolvecontext))))

    (should (phpinspect-incomplete-class-p
             (cadddr (phpinspect--resolvecontext-enclosing-tokens
                     resolvecontext))))))

(provide 'phpinspect-test)
;;; phpinspect-test.el ends here
