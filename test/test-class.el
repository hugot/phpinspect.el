;; test-class.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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
(require 'phpinspect-class)

(ert-deftest phpinspect--merge-method-return-type ()
  (let* ((class-name (phpinspect--make-type :name "\\Something"))
         (method1 (phpinspect--make-function
                  :name "fun"
                  :return-type (phpinspect--make-type :name "\\array")))
         (method2 (phpinspect--make-function
                   :name "fun"
                   :return-type (phpinspect--make-type :name "\\bool")))
         (result (phpinspect--merge-method class-name method1 method2)))

    (should (phpinspect--type= (phpinspect--make-type :name "\\bool")
                               (phpinspect--function-return-type result)))
    (should (phpinspect--type= (phpinspect--make-type :name "\\bool")
                               (phpinspect--function-return-type method1)))))
