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
(message "starting bench")

(defun shuffle (list)
  (let* ((vec (seq-into list 'vector))
         (length (length vec))
         (n 0))
    (while (< n length)
      (let ((i (+ n (random (- length n))))
            (tmp (aref vec n)))
        (setf (aref vec n) (aref vec i)
              (aref vec i) tmp))
      (cl-incf n))
    (seq-into vec 'list)))

(defconst phpi-randomized-access (shuffle (number-sequence 1 1000000)))
(defconst phpi-interval-access (number-sequence 1 1000000 50))

(let ((tree (phpinspect-make-splayt))
      result)
  (message "Splay tree 1000000 insertions:")
  (garbage-collect)

  (setq result
        (benchmark-run 1
          (dotimes (i 1000000)
            (phpinspect-splayt-insert tree i 'value))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "Splay tree 1000000 lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dotimes (i 1000000)
            (phpinspect-splayt-find tree i))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "Splay tree 1000000 randomized lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dolist (i phpi-randomized-access)
            (phpinspect-splayt-find tree i))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "Splay tree 1000000 repeated (constant number 5) lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dotimes (_i 1000000)
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

  (message "Splay tree 1000000 items traversal:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (phpinspect-splayt-traverse (i tree)
            (ignore i))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))


  (message "Splay tree 1000000 items LR traversal:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (phpinspect-splayt-traverse-lr (i tree)
            (ignore i))))
  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result)))



(let (map result)
  (message "Hashtable 1000000 insertions:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (progn
            (setq map (make-hash-table :test #'eq :size 1000000 :rehash-size 1.5))
            (dotimes (i 1000000)
              (puthash i 'value map)))))
  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "Hashtable 1000000 lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dotimes (i 1000000)
            (ignore (gethash i map)))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))


  (message "Hashtable 1000000 iterations:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (ignore (maphash (lambda (k v) k v) map))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result)))


(require 'avl-tree)
(let ((avl-tree (avl-tree-create #'car-less-than-car))
      result)
  (message "AVL tree 1000000 insertions:")
  (garbage-collect)

  (setq result
        (benchmark-run 1
          (dotimes (i 1000000)
            (avl-tree-enter avl-tree (cons i 'value)))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "AVL tree 1000000 lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dotimes (i 1000000)
            (avl-tree-member avl-tree (cons i nil)))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "AVL tree 1000000 randomized lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dolist (i phpi-randomized-access)
            (avl-tree-member avl-tree (cons i nil)))))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result))

  (message "AVL tree 1000000 repeated (constant number 5) lookups:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (dotimes (_i 1000000)
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

  (message "AVL tree 1000000 items LR traversal:")
  (garbage-collect)
  (setq result
        (benchmark-run 1
          (avl-tree-mapc #'ignore avl-tree)))

  (message "Elapsed time: %f (%f in %d GC's)"
           (car result) (caddr result) (cadr result)))
