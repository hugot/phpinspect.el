;; test-type.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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

(require 'phpinspect-type)

(ert-deftest phpinspect--resolve-late-static-binding ()
  (let* ((sets '(("\\bool" . "\\bool")
                 ("\\static" . "\\AType")
                 ("\\this" . "\\AType"))))
    (dolist (set sets)
      (should (phpinspect--type=
               (phpinspect--resolve-late-static-binding
                (phpinspect--make-type :name (car set))
                (phpinspect--make-type :name "\\AType"))

               (phpinspect--make-type :name (cdr set)))))))
