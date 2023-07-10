;;; test-buffer.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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
(require 'phpinspect-tree)
(require 'phpinspect-buffer)


(ert-deftest phpinspect-ll-seq-elt ()
  "Test `seq-elt' implementation for linked list."

  (let ((list (phpinspect-make-ll
               :value "a"
               :right (phpinspect-make-ll :value "b"
                                          :right (phpinspect-make-ll :value "c")))))
    (should (string= "a" (seq-elt list 0)))
    (should (string= "b" (seq-elt list 1)))
    (should (string= "c" (seq-elt list 2)))
    (should-not (seq-elt list 3))))

(ert-deftest phpinspect-ll-push ()
  (let ((list (phpinspect-make-ll)))
    (phpinspect-ll-push "a" list)
    (phpinspect-ll-push "b" list)
    (phpinspect-ll-push "c" list)


    (should (string= "c" (seq-elt list 0)))
    (should (string= "b" (seq-elt list 1)))
    (should (string= "a" (seq-elt list 2)))
    (should (string= "c" (phpinspect-llnode-value
                          (phpinspect-llnode-left
                           (phpinspect-ll-link list (seq-elt list 1))))))
    (should (string= "b" (phpinspect-llnode-value
                          (phpinspect-llnode-left
                           (phpinspect-ll-link list (seq-elt list 2))))))))


(ert-deftest phpinspect-ll-link ()
  (let ((list (phpinspect-make-ll))
        (link-value)
        (link))
    (phpinspect-ll-push "a" list)
    (phpinspect-ll-push "b" list)
    (phpinspect-ll-push "c" list)

    (setq link-value (seq-elt list 1))
    (setq link (phpinspect-ll-link list link-value))

    (should (eq link-value (phpinspect-llnode-value link)))))

(ert-deftest phpinspect-ll-insert-right ()
  (let ((list (phpinspect-make-ll))
        (link-value)
        (link))
    (phpinspect-ll-push "a" list)
    (phpinspect-ll-push "b" list)
    (phpinspect-ll-push "c" list)

    (setq link-value (seq-elt list 1))
    (setq link (phpinspect-ll-link list link-value))

    (phpinspect-ll-insert-right link "aba")
    (should (string= "aba" (seq-elt list 2)))
    (should (string= "a" (seq-elt list 3)))))

(ert-deftest phpinspect-ll-insert-left ()
  (let ((list (phpinspect-make-ll))
        (link-value)
        (link))
    (phpinspect-ll-push "a" list)
    (phpinspect-ll-push "b" list)
    (phpinspect-ll-push "c" list)

    (setq link-value (seq-elt list 1))
    (setq link (phpinspect-ll-link list link-value))

    (phpinspect-ll-insert-left link "aba")
    (should (string= "aba" (seq-elt list 1)))
    (should (string= "c" (seq-elt list 0)))

    (should (string= "b" (seq-elt list 2)))))

(ert-deftest phpinspect-ll-seq-into ()
  (let ((list (phpinspect-make-ll)))
    (phpinspect-ll-push "d" list)
    (phpinspect-ll-push "c" list)
    (phpinspect-ll-push "b" list)
    (phpinspect-ll-push "a" list)

    (should (equal '("a" "b" "c" "d") (seq-into list 'list)))))

(ert-deftest phpinspect-ll-seq-take-while ()
  (let ((list (phpinspect-make-ll))
        (result))
    (phpinspect-ll-push "bla" list)
    (phpinspect-ll-push "foo" list)
    (phpinspect-ll-push "a" list)
    (phpinspect-ll-push "a" list)
    (phpinspect-ll-push "a" list)

    (setq result (seq-take-while (lambda (a) (string= a "a"))
                                 list))

    (seq-map (lambda (a) (should (string= a "a")))
             result)

    (should (string= "aaa" (apply #'concat (seq-into result 'list))))))

(ert-deftest phpinspect-ll-seq-take-while-subset ()
  "seq-take-while should also work from a different start link than
the start of the list."
  (let ((list (phpinspect-make-ll))
        (start-link)
        (result))
    (phpinspect-ll-push "bla" list)
    (phpinspect-ll-push "foo" list)
    (phpinspect-ll-push "a" list)
    (phpinspect-ll-push "a" list)
    (phpinspect-ll-push "a" list)
    (phpinspect-ll-push "a" list)
    (phpinspect-ll-push "a" list)

    (setq start-link (phpinspect-ll-link list (seq-elt list 2)))

    (setq result (seq-take-while (lambda (a) (string= a "a"))
                                 start-link))

    (seq-map (lambda (a) (should (string= a "a")))
             result)

    (should (string= "aaa" (apply #'concat (seq-into result 'list))))))

(ert-deftest phpinspect-ll-seq-find ()
  (let ((list (phpinspect-make-ll)))
    (phpinspect-ll-push "d" list)
    (phpinspect-ll-push "c" list)
    (phpinspect-ll-push "b" list)
    (phpinspect-ll-push "a" list)

    (should (string= "c" (seq-find (lambda (c) (string= "c" c)) list)))))

(ert-deftest phpinspect-ll-link ()
  (let ((list (phpinspect-make-ll))
        (value1 "a")
        (value2 "b")
        (value3 "c")
        (value4 "d"))
    (phpinspect-ll-push value1 list)
    (should (phpinspect-ll-link list value1))
    (phpinspect-ll-push value2 list)
    (should (phpinspect-ll-link list value2))

    (phpinspect-ll-insert-right (phpinspect-ll-link list value1) value3)
    (should (phpinspect-ll-link list value3))

    (phpinspect-ll-insert-left (phpinspect-ll-link list value3) value4)
    (should (phpinspect-ll-link list value3))))

(ert-deftest phpinspect-ll-seq-length ()
  (let ((list (phpinspect-make-ll)))
    (phpinspect-ll-push "d" list)
    (phpinspect-ll-push "c" list)
    (phpinspect-ll-push "b" list)
    (phpinspect-ll-push "a" list)

    (should (= 4 (seq-length list)))))

(ert-deftest phpinspect-ll-seq-find ()
  (let ((list (phpinspect-make-ll)))
    (phpinspect-ll-push "target1" list)
    (phpinspect-ll-push "target2" list)
    (phpinspect-ll-push "target3" list)
    (phpinspect-ll-push "target4" list)

    (should-not (seq-find #'not list))
    (should (string= "target1" (seq-find (lambda (s) (string= "target1" s))
                                         list)))
    (should (string= "target2" (seq-find (lambda (s) (string= "target2" s))
                                         list)))
    (should (string= "target3" (seq-find (lambda (s) (string= "target3" s))
                                         list)))
    (should (string= "target4" (seq-find (lambda (s) (string= "target4" s))
                                         list)))))

(ert-deftest phpinspect-slice-seq-find ()
  (let ((list (phpinspect-make-ll))
        (target1 "target1")
        (target4 "target4")
        (slice))
    (phpinspect-ll-push target1 list)
    (phpinspect-ll-push "target2" list)
    (phpinspect-ll-push "target3" list)
    (phpinspect-ll-push target4 list)

    (setq slice (phpinspect-make-slice :start (phpinspect-ll-link list target4)
                                       :end (phpinspect-ll-link list target1)))

    (should-not (seq-find #'not slice))
    (should (string= "target1" (seq-find (lambda (s) (string= "target1" s))
                                         slice)))
    (should (string= "target2" (seq-find (lambda (s) (string= "target2" s))
                                         slice)))
    (should (string= "target3" (seq-find (lambda (s) (string= "target3" s))
                                         slice)))
    (should (string= "target4" (seq-find (lambda (s) (string= "target4" s))
                                         slice)))))

(ert-deftest phpinspect-slice-detach ()
  (let ((list (phpinspect-make-ll))
        (val1 "c")
        (slice)
        (detached-list))
    (phpinspect-ll-push "d" list)
    (phpinspect-ll-push val1 list)
    (phpinspect-ll-push "b" list)
    (phpinspect-ll-push "a" list)

    (setq slice (phpinspect-make-slice :start list
                                       :end (phpinspect-ll-link list val1)))

    (setq detached-list (phpinspect-slice-detach slice))

    (should-not (eq detached-list list))
    (should (string= "d" (apply #'concat (seq-into list 'list))))
    (should (string= "abc" (apply #'concat (seq-into detached-list 'list))))))


(ert-deftest phpinspect-tree-insert-enclosing-node ()
  (let ((tree (phpinspect-make-tree :start 10 :end 100))
        (node (phpinspect-make-tree :start 9 :end 200)))
    (phpinspect-tree-insert-node tree node)

    (should (= 9 (phpinspect-tree-start tree)))
    (should (= 200 (phpinspect-tree-end tree)))
    (should (= 10 (phpinspect-tree-start node)))
    (should (= 100 (phpinspect-tree-end node)))

    (should (eq node (phpinspect-llnode-value
                      (phpinspect-tree-children tree))))))

(ert-deftest phpinspect-tree-insert-enclosing-node-into-tree-with-parent ()
  (let* ((parent (phpinspect-make-tree :start 0 :end 200))
         (tree (phpinspect-make-tree :start 10 :end 100))
         (node (phpinspect-make-tree :start 11 :end 50)))
    (phpinspect-tree-insert-node parent tree)
    (phpinspect-tree-insert-node tree node)

    (should (eq parent (phpinspect-tree-parent tree)))
    (should (eq tree (phpinspect-tree-parent node)))
    (should (eq node (phpinspect-llnode-value
                      (phpinspect-tree-children tree))))))

(ert-deftest phpinspect-tree-insert-nested ()
  (let ((tree (phpinspect-make-tree :start 0 :end 500))
        (node2 (phpinspect-make-tree :start 20 :end 200))
        (node3 (phpinspect-make-tree :start 9 :end 20))
        (node4 (phpinspect-make-tree :start 21 :end 44))
        (node1 (phpinspect-make-tree :start 9 :end 200)))

    (should (phpinspect-tree-parent (phpinspect-tree-insert-node tree node1)))
    (should (phpinspect-tree-parent(phpinspect-tree-insert-node tree node2)))
    (should (phpinspect-tree-parent (phpinspect-tree-insert-node tree node3)))
    (should (phpinspect-tree-parent (phpinspect-tree-insert-node tree node4)))

    (should (phpinspect-ll-link (phpinspect-tree-children (phpinspect-tree-parent node1)) node1))
    (should (phpinspect-ll-link (phpinspect-tree-children (phpinspect-tree-parent node2)) node2))
    (should (phpinspect-ll-link (phpinspect-tree-children (phpinspect-tree-parent node3)) node3))
    (should (phpinspect-ll-link (phpinspect-tree-children (phpinspect-tree-parent node4)) node4))

    (should (= 0 (phpinspect-tree-start tree)))
    (should (= 500 (phpinspect-tree-end tree)))

    (should (= 1 (seq-length (phpinspect-tree-children tree))))
    (let ((firstchild (seq-elt (phpinspect-tree-children tree) 0)))
      (should (eq node1 firstchild))
      (should (= 2 (seq-length (phpinspect-tree-children firstchild))))
      (should (eq node3 (seq-elt (phpinspect-tree-children firstchild) 0)))
      (should (eq node2 (seq-elt (phpinspect-tree-children firstchild) 1))))

    (should (eq node4 (seq-elt (phpinspect-tree-children node2) 0)))))

(ert-deftest phpinspect-tree-insert-returns-node ()
  "Because returning things from lisp functions can be kind of a hassle sometimes ;).

Tests whether phpinspect-tree-insert-node actually returns the
correct node (the one that the nodes values were stored in, or
the node iteself if it has been stored intact)."
  (let* ((tree (phpinspect-make-tree :start 0 :end 500))
         (node1 (phpinspect-make-tree :start 0 :end 800))
         (node2 (phpinspect-make-tree :start 20 :end 200))
         (node3 (phpinspect-make-tree :start 9 :end 20))
         (node4 (phpinspect-make-tree :start 21 :end 44))
         (node1-return (phpinspect-tree-insert-node tree node1))
         (node2-return (phpinspect-tree-insert-node tree node2))
         (node3-return (phpinspect-tree-insert-node tree node3))
         (node4-return (phpinspect-tree-insert-node tree node4)))


    (should (eq tree node1-return))
    (should (= 800 (phpinspect-tree-end tree)))
    (should (eq node2 node2-return))
    (should (= 20 (phpinspect-tree-start node2-return)))
    (should (eq node3 node3-return))
    (should (= 9 (phpinspect-tree-start node3-return)))
    (should (eq node4 node4-return))
    (should (= 21 (phpinspect-tree-start node4-return)))))

(ert-deftest phpinspect-tree-traverse-overlapping-point ()
  (let ((tree (phpinspect-make-tree :start 0 :end 500 :value "tree"))
        (node1 (phpinspect-make-tree :start 9 :end 200 :value "node1"))
        (node2 (phpinspect-make-tree :start 20 :end 200 :value "node2"))
        (node3 (phpinspect-make-tree :start 9 :end 20 :value "node3"))
        (node4 (phpinspect-make-tree :start 21 :end 44 :value "node4"))
        (result))

    (phpinspect-tree-insert-node tree node1)
    (phpinspect-tree-insert-node tree node2)
    (phpinspect-tree-insert-node tree node3)
    (phpinspect-tree-insert-node tree node4)

    (setq result (phpinspect-tree-traverse-overlapping tree 22))
    (should (equal '("node4" "node2" "node1" "tree") result))))

(ert-deftest phpinspect-tree-traverse-overlapping-region ()
  (let ((tree (phpinspect-make-tree :start 0 :end 500 :value "tree"))
        (node1 (phpinspect-make-tree :start 9 :end 200 :value "node1"))
        (node2 (phpinspect-make-tree :start 20 :end 200 :value "node2"))
        (node3 (phpinspect-make-tree :start 9 :end 20 :value "node3"))
        (node4 (phpinspect-make-tree :start 21 :end 44 :value "node4"))
        (result))

    (phpinspect-tree-insert-node tree node1)
    (phpinspect-tree-insert-node tree node2)
    (phpinspect-tree-insert-node tree node3)
    (phpinspect-tree-insert-node tree node4)

    (setq result (phpinspect-tree-traverse-overlapping tree (phpinspect-make-region 18 22)))
    (should (equal '("node3" "node4" "node2" "node1" "tree") result))))

(ert-deftest phpinspect-tree-find-smallest-overlapping-set ()
  (let ((tree (phpinspect-make-tree :start 0 :end 500 :value "tree"))
        (node1 (phpinspect-make-tree :start 9 :end 200 :value "node1"))
        (node2 (phpinspect-make-tree :start 20 :end 200 :value "node2"))
        (node3 (phpinspect-make-tree :start 44 :end 60 :value "node3"))
        (node4 (phpinspect-make-tree :start 21 :end 44 :value "node4"))
        (result))
    (phpinspect-tree-insert-node tree node1)
    (phpinspect-tree-insert-node tree node2)
    (phpinspect-tree-insert-node tree node3)
    (phpinspect-tree-insert-node tree node4)

    (should (phpinspect-tree-overlaps tree (phpinspect-make-region 24 55)))

    (setq result (phpinspect-tree-find-smallest-overlapping-set
                  tree (phpinspect-make-region 24 55)))
    (should (equal '("node4" "node3") result))))

(ert-deftest phpinspect-tree-find-node-starting-at ()
  (let ((tree (phpinspect-make-tree :start 0 :end 500 :value "tree"))
        (node1 (phpinspect-make-tree :start 9 :end 200 :value "node1"))
        (node2 (phpinspect-make-tree :start 20 :end 200 :value "node2"))
        (node3 (phpinspect-make-tree :start 44 :end 60 :value "node3"))
        (node4 (phpinspect-make-tree :start 21 :end 44 :value "node4"))
        (result))
    (phpinspect-tree-insert-node tree node1)
    (phpinspect-tree-insert-node tree node2)
    (phpinspect-tree-insert-node tree node3)
    (phpinspect-tree-insert-node tree node4)

    (setq result (phpinspect-tree-find-node-starting-at tree 44))
    (should (eq node3 result))
    (should-not (phpinspect-tree-find-node-starting-at tree 45))))

(ert-deftest phpinspect-tree-overlaps-point ()
  (let ((tree (phpinspect-make-tree :start  5 :end 10)))
    (should (phpinspect-tree-overlaps tree 5))

    ;; An interval's end is its delimtiter and should not be regarded as part of
    ;; it.
    (should-not (phpinspect-tree-overlaps tree 10))

    (should-not (phpinspect-tree-overlaps tree 4))
    (should-not (phpinspect-tree-overlaps tree 11))))

(ert-deftest phpinspect-tree-overlaps-region ()
  (let ((tree (phpinspect-make-tree :start  5 :end 10)))
    (should (phpinspect-tree-overlaps tree (phpinspect-make-region 0 6)))
    (should-not (phpinspect-tree-overlaps tree (phpinspect-make-region 0 5)))
    (should (phpinspect-tree-overlaps tree (phpinspect-make-region 9 11)))
    (should-not (phpinspect-tree-overlaps tree (phpinspect-make-region 10 11)))))

(ert-deftest phpinspect-tree-encloses ()
  (let ((tree (phpinspect-make-tree :start  5 :end 10)))
    (should (phpinspect-tree-encloses tree (phpinspect-make-tree :start 5 :end 10)))
    (should (phpinspect-tree-encloses tree (phpinspect-make-tree :start 5 :end 9)))))

(ert-deftest phpinspect-tree-insert-same-size ()
  (let* ((tree (phpinspect-make-tree :start  5 :end 10))
         (node (phpinspect-tree-insert-node tree (phpinspect-make-tree :start 5 :end 10))))

    (should (eq node (seq-elt (phpinspect-tree-children tree) 0)))))
