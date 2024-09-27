;; test-meta.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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
(require 'phpinspect-meta)

(ert-deftest phpinspect-meta-start-relative-to-parent ()
  (let ((meta (phpinspect-make-meta nil 10 20 "" 'token))
        (parent1 (phpinspect-make-meta nil 9 22 "" 'token))
        (parent2 (phpinspect-make-meta nil 0 100 "" 'token)))
    (phpinspect-meta-set-parent meta parent1)
    (phpinspect-meta-set-parent parent1 parent2)

    (should (= 10 (phpinspect-meta-start meta)))

    (phpinspect-meta-shift parent2  20)
    (should (= 30 (phpinspect-meta-start meta)))

    (should (phpinspect-meta-overlaps-point meta 30))))


(ert-deftest phpinspect-meta-detach ()
  (let ((meta (phpinspect-make-meta nil 10 20 "" 'token))
        (parent (phpinspect-make-meta nil 9 22 "" 'token)))
    (phpinspect-meta-set-parent meta parent)

    (should (length= (phpinspect-meta-flatten parent) 2))

    (phpinspect-meta-detach-parent meta)

    (should (length= (phpinspect-meta-flatten parent) 1))))

(ert-deftest phpinspect-meta-find-child-starting-at ()
  (let ((grandchild (phpinspect-make-meta nil 14 16 "" 'grandchild))
        (meta (phpinspect-make-meta nil 10 20 "" 'child))
        (parent (phpinspect-make-meta nil 9 22 "" 'parent)))
    (phpinspect-meta-set-parent meta parent)
    (phpinspect-meta-set-parent grandchild meta)

    (should (phpinspect-meta-find-child-starting-at parent 10))

    (should (eq grandchild (phpinspect-meta-find-child-starting-at-recursively parent 14)))))
