;;; splay-tree.el --- Benchmarks of phpinspect-splayt.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: benchmark

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

;;; Code:


(require 'phpinspect-splayt)

(defun swap (LIST el1 el2)
  "in LIST swap indices EL1 and EL2 in place"
  (let ((tmp (elt LIST el1)))
    (setf (elt LIST el1) (elt LIST el2))
    (setf (elt LIST el2) tmp)))

(defun shuffle (LIST)
  "Shuffle the elements in LIST.
shuffling is done in place."
  (cl-loop for i in (reverse (number-sequence 1 (1- (length LIST))))
        do (let ((j (random (+ i 1))))
             (swap LIST i j)))
  LIST)

(defconst phpi-randomized-access (shuffle (number-sequence 1 10000)))
(defconst phpi-interval-access (number-sequence 1 10000 50))

(let ((tree (phpinspect-make-splayt))
      result)
  (message "Splay tree 10000 insertions:")
  (garbage-collect)

  (setq result
        (benchmark-run 1
          (dotimes (i 10000)
            (phpinspect-splayt-insert tree i 'value))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "Splay tree 10000 lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dotimes (i 10000)
            (phpinspect-splayt-find tree i))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "Splay tree 10000 randomized lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dolist (i phpi-randomized-access)
            (phpinspect-splayt-find tree i))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "Splay tree 10000 repeated (constant number 5) lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dotimes (_i 10000)
            (phpinspect-splayt-find tree 5))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "Splay tree repeated lookups (repeat x10, interval 50):")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dolist (i phpi-interval-access)
            (dotimes (_i 10)
              (phpinspect-splayt-find tree i)))))


  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "Splay tree 10000 items traversal:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (phpinspect-splayt-traverse (i tree)
            (ignore i))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))


  (message "Splay tree 10000 items LR traversal:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (phpinspect-splayt-traverse-lr (i tree)
            (ignore i))))
  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result)))



(let (map result)
  (message "Hashtable 10000 insertions:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (progn
            (setq map (make-hash-table :test #'eq :size 10000 :rehash-size 1.5))
            (dotimes (i 10000)
              (puthash i 'value map)))))
  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "Hashtable 10000 lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dotimes (i 10000)
            (ignore (gethash i map)))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))


  (message "Hashtable 10000 iterations:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (ignore (maphash (lambda (k v) k v) map))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result)))


(require 'avl-tree)
(let ((avl-tree (avl-tree-create #'car-less-than-car))
      result)
  (message "AVL tree 10000 insertions:")
  (garbage-collect)

  (setq result
        (benchmark-run 1
          (dotimes (i 10000)
            (avl-tree-enter avl-tree (cons i 'value)))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "AVL tree 10000 lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dotimes (i 10000)
            (avl-tree-member avl-tree (cons i nil)))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "AVL tree 10000 randomized lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dolist (i phpi-randomized-access)
            (avl-tree-member avl-tree (cons i nil)))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "AVL tree 10000 repeated (constant number 5) lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dotimes (_i 10000)
            (avl-tree-member avl-tree (cons 5 nil)))))

    (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "AVL tree repeated lookups (repeat x10, interval 50):")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dolist (i phpi-interval-access)
            (dotimes (_i 10)
              (avl-tree-member avl-tree (cons i nil))))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "AVL tree 10000 items LR traversal:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (iter-do (i (avl-tree-iter avl-tree))
            (ignore i))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result)))

;; Results (ran at [2024-09-08 12:38])

;; Splay tree 10000 insertions:
;; Elapsed time: 0.006073 (0.000000 in 0 GC’s)
;; Splay tree 10000 lookups:
;; Elapsed time: 0.003990 (0.000000 in 0 GC’s)
;; Splay tree 10000 randomized lookups:
;; Elapsed time: 0.021418 (0.000000 in 0 GC’s)
;; Splay tree 10000 repeated (constant number 5) lookups:
;; Elapsed time: 0.000526 (0.000000 in 0 GC’s)
;; Splay tree repeated lookups (repeat x10, interval 50):
;; Elapsed time: 0.000445 (0.000000 in 0 GC’s)
;; Splay tree 10000 items traversal:
;; Elapsed time: 0.005147 (0.000000 in 0 GC’s)
;; Splay tree 10000 items LR traversal:
;; Elapsed time: 0.000764 (0.000000 in 0 GC’s)

;; Hashtable 10000 insertions:
;; Elapsed time: 0.000457 (0.000000 in 0 GC’s)
;; Hashtable 10000 lookups:
;; Elapsed time: 0.000250 (0.000000 in 0 GC’s)
;; Hashtable 10000 iterations:
;; Elapsed time: 0.000135 (0.000000 in 0 GC’s)

;; AVL tree 10000 insertions:
;; Elapsed time: 0.009241 (0.000000 in 0 GC’s)
;; AVL tree 10000 lookups:
;; Elapsed time: 0.003807 (0.000000 in 0 GC’s)
;; AVL tree 10000 randomized lookups:
;; Elapsed time: 0.004335 (0.000000 in 0 GC’s)
;; AVL tree 10000 repeated (constant number 5) lookups:
;; Elapsed time: 0.003099 (0.000000 in 0 GC’s)
;; AVL tree repeated lookups (repeat x10, interval 50):
;; Elapsed time: 0.000650 (0.000000 in 0 GC’s)
;; AVL tree 10000 items LR traversal:
;; Elapsed time: 0.004814 (0.000000 in 0 GC’s)
