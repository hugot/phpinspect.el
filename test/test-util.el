;;; test-util.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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

(require 'phpinspect-util)

(ert-deftest phpinspect--pattern ()
  (let* ((a "a")
         (pattern1 (phpinspect--make-pattern :m `(,a) :m * :m "b"))
         (pattern2 (phpinspect--make-pattern :f #'listp :m * :m "b")))

    (should (equal '(:m ("a") :m * :m "b") (phpinspect--pattern-code pattern1)))
    (should (equal '(:f listp :m * :m "b") (phpinspect--pattern-code pattern2)))

    (dolist (pattern `(,pattern1 ,pattern2))
      (should (phpinspect--pattern-match pattern '(("a") "c" "b")))
      (should (equal '(("a") "c" "b") (phpinspect--pattern-match pattern '(("a") "c" "b"))))
      (should (phpinspect--pattern-match pattern '(("a") nil "b")))
      (should-not (phpinspect--pattern-match pattern '(1 nil "b")))
      (should-not (phpinspect--pattern-match pattern '(("a") nil "b" "c"))))))

(ert-deftest phpinspect--pattern-concat ()
  (let* ((pattern1 (phpinspect--make-pattern :m "a" :m * :m "b"))
         (pattern2 (phpinspect--make-pattern :f #'stringp :m * :m "D"))
         (result (phpinspect--pattern-concat pattern1 pattern2)))

    (should (equal '(:m "a" :m * :m "b" :f stringp :m * :m "D") (phpinspect--pattern-code result)))

    (should (phpinspect--pattern-match result '("a" "anything" "b" "astring" nil "D")))))

(ert-deftest phpinspect--pattern-match-partially ()
  (let ((result (phpinspect--match-sequence '((:variable "this") (:object-attrib (:word "em")))
                  :m '(:variable "this")
                  :m '(:object-attrib (:word "not-a-match")))))

    (should-not result)))

(ert-deftest phpinspect--pattern-match-rest ()
  (should (phpinspect--match-sequence '((:variable "this") (:object-attrib (:word "em")) (:list))
            :m '(:variable "this")
            :m *
            :rest '(:list)))

  (should (phpinspect--match-sequence '((:variable "this") (:object-attrib (:word "em")) (:list))
            :m '(:variable "this")
            :m *
            :rest *))

  (should (phpinspect--match-sequence '((:variable "this") (:object-attrib (:word "em")) (:list))
            :m '(:variable "this")
            :m *
            :rest '(:list)))

  (should (phpinspect--match-sequence '((:variable "this") (:object-attrib (:word "em")) (:list))
            :m '(:variable "this")
            :rest *))

  (should-not (phpinspect--match-sequence '((:variable "this") (:object-attrib (:word "em")) (:variable "ba") (:list))
                :m '(:variable "this")
                :rest '(:list)))

  (should-not (phpinspect--match-sequence '((:variable "this") (:object-attrib (:word "em")) (:list))
                :m '(:variable "this")
                :m *)))
