;; test-splayt.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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

(require 'phpinspect-splayt)

(ert-deftest phpinspect-splayt-node-rotate ()
  (let* ((one (phpinspect-make-splayt-node 1 "one"))
         (four (phpinspect-make-splayt-node 4 "four"))
         (three (phpinspect-make-splayt-node
                3 "three"
                one
                four)))
    (setf (phpinspect-splayt-node-parent four) three)
    (setf (phpinspect-splayt-node-parent one) three)
    (phpinspect-splayt-node-rotate-right three)

    (should (eq one (phpinspect-splayt-node-parent three)))
    (should (eq three (phpinspect-splayt-node-parent four)))
    (should (eq three (phpinspect-splayt-node-right one)))
    (should (eq four (phpinspect-splayt-node-right three)))
    (should-not (phpinspect-splayt-node-left one))
    (should-not (phpinspect-splayt-node-left four))
    (should-not (phpinspect-splayt-node-left three))

    (phpinspect-splayt-node-rotate-left one)

    (should (eq one (phpinspect-splayt-node-left three)))
    (should (eq three (phpinspect-splayt-node-parent four)))
    (should (eq three (phpinspect-splayt-node-parent one)))
    (should (eq four (phpinspect-splayt-node-right three)))
    (should (eq one (phpinspect-splayt-node-left three)))))

(ert-deftest phpinspect-splayt ()
  (let ((tree (phpinspect-make-splayt)))
    (phpinspect-splayt-insert tree 9 "nine")
    (phpinspect-splayt-insert tree 3 "three")
    (phpinspect-splayt-insert tree 11 "eleven")
    (phpinspect-splayt-insert tree 8 "eight")
    (phpinspect-splayt-insert tree 12 "twelve")
    (phpinspect-splayt-insert tree 4 "four")
    (phpinspect-splayt-insert tree 1 "one")


    (should (string= "eight" (phpinspect-splayt-find tree 8)))
    (should (string= "one" (phpinspect-splayt-find tree 1)))
    (should (string= "three" (phpinspect-splayt-find tree 3)))
    (should (string= "nine" (phpinspect-splayt-find tree 9)))
    (should (string= "four" (phpinspect-splayt-find tree 4)))
    (should (string= "twelve" (phpinspect-splayt-find tree 12)))
    (should (string= "eleven" (phpinspect-splayt-find tree 11)))

    (let ((expected (sort (copy-sequence '("nine" "three" "eleven" "eight" "twelve" "four" "one")) #'string-lessp))
          (result))

      (phpinspect-splayt-traverse (item tree)
        (push item result))

      (setq result (sort result #'string-lessp))

      (should (equal expected result)))))

(ert-deftest phpinspect-splayt-traverse ()
  (let ((tree (phpinspect-make-splayt)))
    (phpinspect-splayt-insert tree 9 "nine")
    (phpinspect-splayt-insert tree 3 "three")
    (phpinspect-splayt-insert tree 11 "eleven")
    (phpinspect-splayt-insert tree 8 "eight")
    (phpinspect-splayt-insert tree 12 "twelve")
    (phpinspect-splayt-insert tree 4 "four")
    (phpinspect-splayt-insert tree 1 "one")

    (let ((expected (sort (copy-sequence '("nine" "three" "eleven" "eight" "twelve" "four" "one")) #'string-lessp))
          (result))

      (phpinspect-splayt-traverse (item tree)
        (push item result))

      (setq result (sort result #'string-lessp))

      (should (equal expected result)))))

(ert-deftest phpinspect-splayt-traverse-lr ()
  (let ((tree (phpinspect-make-splayt)))
    (phpinspect-splayt-insert tree 9 "nine")
    (phpinspect-splayt-insert tree 3 "three")
    (phpinspect-splayt-insert tree 11 "eleven")
    (phpinspect-splayt-insert tree 8 "eight")
    (phpinspect-splayt-insert tree 12 "twelve")
    (phpinspect-splayt-insert tree 4 "four")
    (phpinspect-splayt-insert tree 1 "one")

    (let ((expected '("one" "three"  "four" "eight" "nine" "eleven" "twelve"))
          result)
      (phpinspect-splayt-traverse-lr (item tree)
        (setq result (nconc result (list item))))

      (should (equal expected result)))))

(ert-deftest phpinspect-splayt-find-smallest-after ()
  (let ((tree (phpinspect-make-splayt)))
    (phpinspect-splayt-insert tree 9 "nine")
    (phpinspect-splayt-insert tree 3 "three")
    (phpinspect-splayt-insert tree 11 "eleven")
    (phpinspect-splayt-insert tree 8 "eight")
    (phpinspect-splayt-insert tree 12 "twelve")
    (phpinspect-splayt-insert tree 4 "four")
    (phpinspect-splayt-insert tree 1 "one")


    (should (string= "nine" (phpinspect-splayt-find-smallest-after tree 8)))
    (should (string= "three" (phpinspect-splayt-find-smallest-after tree 1)))))


(ert-deftest phpinspect-splayt-find-largest-before ()
  (let ((tree (phpinspect-make-splayt)))
    (phpinspect-splayt-insert tree 9 "nine")
    (phpinspect-splayt-insert tree 3 "three")
    (phpinspect-splayt-insert tree 11 "eleven")
    (phpinspect-splayt-insert tree 8 "eight")
    (phpinspect-splayt-insert tree 12 "twelve")
    (phpinspect-splayt-insert tree 4 "four")
    (phpinspect-splayt-insert tree 1 "one")


    (should (string= "four" (phpinspect-splayt-find-largest-before tree 8)))
    (should (string= "eleven" (phpinspect-splayt-find-largest-before tree 12)))
    (should (string= "one" (phpinspect-splayt-find-largest-before tree 2)))
    (should (string= "twelve" (phpinspect-splayt-find-largest-before tree 13)))))


(ert-deftest phpinspect-splayt-find-all-after ()
  (let ((tree (phpinspect-make-splayt)))
    (phpinspect-splayt-insert tree 9 "nine")
    (phpinspect-splayt-insert tree 3 "three")
    (phpinspect-splayt-insert tree 11 "eleven")
    (phpinspect-splayt-insert tree 8 "eight")
    (phpinspect-splayt-insert tree 12 "twelve")
    (phpinspect-splayt-insert tree 4 "four")
    (phpinspect-splayt-insert tree 1 "one")


    (should (equal (sort (copy-sequence '("eight" "nine" "eleven" "twelve")) #'string-lessp)
                   (sort (phpinspect-splayt-find-all-after tree 7) #'string-lessp)))))

(ert-deftest phpinspect-splayt-to-list ()
  (let ((tree (phpinspect-make-splayt)))
    (phpinspect-splayt-insert tree 3 "three")
    (phpinspect-splayt-insert tree 1 "one")
    (phpinspect-splayt-insert tree 2 "two")


    (should (equal '("one" "two" "three") (phpinspect-splayt-to-list tree)))))

(ert-deftest phpinspect-splayt-find-all-between ()
  (let ((tree (phpinspect-make-splayt)))
    (phpinspect-splayt-insert tree 9 "nine")
    (phpinspect-splayt-insert tree 3 "three")
    (phpinspect-splayt-insert tree 11 "eleven")
    (phpinspect-splayt-insert tree 8 "eight")
    (phpinspect-splayt-insert tree 12 "twelve")
    (phpinspect-splayt-insert tree 4 "four")
    (phpinspect-splayt-insert tree 1 "one")

    (should (equal '("three" "four") (phpinspect-splayt-find-all-between tree 1 5)))))
