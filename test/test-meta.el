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

(ert-deftest phpinspect-meta-iterator ()
  (let* ((meta (phpinspect-make-meta nil 10 20 "" 'token))
         (firstchild (phpinspect-make-meta nil 10 12 "" 'token))
         (secondchild (phpinspect-make-meta nil 13 15 "" 'token))
         (parent1 (phpinspect-make-meta nil 9 22 "" 'token))
         (sibling (phpinspect-make-meta nil 30 55 "" 'token))
         (parent2 (phpinspect-make-meta nil 0 100 "" 'token))
         iterator)
    (phpinspect-meta-set-parent meta parent1)
    (phpinspect-meta-set-parent parent1 parent2)
    (phpinspect-meta-set-parent sibling parent2)
    (phpinspect-meta-set-parent firstchild meta)
    (phpinspect-meta-set-parent secondchild meta)

    (setq iterator (phpinspect-make-meta-iterator parent2))

    (should (eq meta (phpinspect-meta-iterator-token-at-point iterator 10)))
    (should (eq sibling (phpinspect-meta-iterator-token-at-point iterator 30)))
    (should (eq meta (phpinspect-meta-iterator-token-at-point iterator 10)))
    (should (eq firstchild (phpinspect-meta-iterator-token-at-point iterator 10)))
    (should (eq secondchild (phpinspect-meta-iterator-token-at-point iterator 13)))
    (should (eq meta (phpinspect-meta-iterator-token-at-point iterator 10)))
    (should (eq firstchild (phpinspect-meta-iterator-token-at-point iterator 10)))
    (should (eq sibling (phpinspect-meta-iterator-token-at-point iterator 30)))))
