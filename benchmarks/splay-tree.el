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

(defconst phpi-randomized-access (shuffle (number-sequence 0 10000)))

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
;; Elapsed time: 0.006082 (0.000000 in 0 GC’s)
;; Splay tree 10000 lookups:
;; Elapsed time: 0.003710 (0.000000 in 0 GC’s)
;; Splay tree 10000 randomized lookups:
;; Elapsed time: 0.021548 (0.000000 in 0 GC’s)
;; Splay tree 10000 repeated (constant number 5) lookups:
;; Elapsed time: 0.000574 (0.000000 in 0 GC’s)
;; Splay tree 10000 items traversal:
;; Elapsed time: 0.170038 (0.108029 in 3 GC’s)
;; Splay tree 10000 items LR traversal:
;; Elapsed time: 0.000919 (0.000000 in 0 GC’s)
;; Hashtable 10000 insertions:
;; Elapsed time: 0.000757 (0.000000 in 0 GC’s)
;; Hashtable 10000 lookups:
;; Elapsed time: 0.000253 (0.000000 in 0 GC’s)
;; Hashtable 10000 iterations:
;; Elapsed time: 0.000137 (0.000000 in 0 GC’s)
;; AVL tree 10000 insertions:
;; Elapsed time: 0.010190 (0.000000 in 0 GC’s)
;; AVL tree 10000 lookups:
;; Elapsed time: 0.003857 (0.000000 in 0 GC’s)
;; AVL tree 10000 randomized lookups:
;; Elapsed time: 0.004604 (0.000000 in 0 GC’s)
;; AVL tree 10000 repeated (constant number 5) lookups:
;; Elapsed time: 0.003011 (0.000000 in 0 GC’s)
;; AVL tree 10000 items LR traversal:
;; Elapsed time: 0.005048 (0.000000 in 0 GC’s)
