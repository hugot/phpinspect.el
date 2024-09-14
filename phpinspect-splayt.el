;;; phpinspect-splayt.el --- A Splay Tree Implementation  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 2.1.0

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
;; A splay tree implementation exclusively using `cons' and `list' data
;; structures with the aim to have as low a memory footprint as possible.
;;
;; Important functions:
;; - `phpinspect-splayt-insert'
;; - `phpinspect-splayt-find'
;; - `phpinspect-splayt-find-smallest-after'
;; - `phpinspect-splayt-find-all-after'
;; - `phpinspect-splayt-traverse'
;;;
;;
;; DEVELOPING
;;
;; The main aim for this tree implementation is to be reasonably fast and
;; comfortable to use for most of phpinspect's common operations. That means:
;;
;; - Fast insertion of sequential keys (for example when parsing a buffer from left to right)
;; - Consing as few bytes as possible by keeping the data structure simple, to avoid GC pauses as much as possible
;; - Fast repeated acces of "hot" regions (for example the edited region of a buffer)
;; - A straight forward public API to retrieve sets of nodes
;;
;; ** Inline Functions **
;; There is a lot of use of `define-inline' in this file. Most of these inlines
;; improve performance significantly. This is especially true for smaller
;; inlined functions. It might be possible to change one or two of the larger
;; functions to normal defuns without reintroducing a lot of ovehead. If you
;; want to do this to make debugging the code a little easier, and you can
;; backup that it doesn't impact performance all that much with some benchmarks
;; (especially parse-file.el in the benchmarks folder), your PR will be welcomed.
;;

;;; Code:

(define-inline phpinspect-make-splayt-node (key value &optional left right parent temp-store)
  (inline-quote (cons (cons ,key ,value) (list ,left ,right ,parent ,temp-store))))

(define-inline phpinspect-splayt-node-left (node)
  (inline-quote (cadr ,node)))

(define-inline phpinspect-splayt-node-right (node)
  (inline-quote (caddr ,node)))

(define-inline phpinspect-splayt-node-key (node)
  (inline-quote (caar ,node)))

(define-inline phpinspect-splayt-node-value (node)
  (inline-quote (cdar ,node)))

(define-inline phpinspect-splayt-node-parent (node)
  (inline-quote (cadddr ,node)))

(define-inline phpinspect-splayt-node-temp-store (node)
  "Dedicated place to store data when necessary. Mostly used for
rotations without instantiating a lexical environment with
`let'. When only used once or twice in a function call, this
apeared to be a little more performant than using `let'."
  (inline-quote (car (cddddr ,node))))

(define-inline phpinspect-splayt-node-update-parent (node parent new-val)
  (inline-letevals (node parent new-val)
    (inline-quote
     (if (eq ,node (phpinspect-splayt-node-left ,parent))
         (setf (phpinspect-splayt-node-left ,parent) ,new-val)
       (setf (phpinspect-splayt-node-right ,parent) ,new-val)))))

(define-inline phpinspect-make-splayt (&optional root-node)
  (inline-quote
   (cons 'phpinspect-splayt ,root-node)))

(define-inline phpinspect-splayt-root-node (splayt)
  (inline-quote (cdr ,splayt)))

(define-inline phpinspect-splayt-empty-p (splayt)
  (inline-quote (not (phpinspect-splayt-root-node ,splayt))))

(define-inline phpinspect-splayt-node-rotate-right (node &optional splayt)
  (inline-letevals (node splayt)
    (inline-quote
     (progn
       ;; Save right node of left child
       (setf (phpinspect-splayt-node-temp-store ,node)
             (phpinspect-splayt-node-right (phpinspect-splayt-node-left ,node)))

       ;; Update node parent to reference left as child
       (when (phpinspect-splayt-node-parent ,node)
         (phpinspect-splayt-node-update-parent
          ,node (phpinspect-splayt-node-parent ,node) (phpinspect-splayt-node-left ,node)))

       ;; Set left node new parent
       (setf (phpinspect-splayt-node-parent (phpinspect-splayt-node-left ,node))
             (phpinspect-splayt-node-parent ,node))

       ;; Set left node as node's new parent
       (setf (phpinspect-splayt-node-parent ,node)
             (phpinspect-splayt-node-left ,node))

       ;; Set node as left's right child
       (setf (phpinspect-splayt-node-right (phpinspect-splayt-node-left ,node)) ,node)

       ;; Set left's right child as node's left child
       (setf (phpinspect-splayt-node-left ,node) (phpinspect-splayt-node-temp-store ,node))

       ;; Update new left child's parent
       (when (phpinspect-splayt-node-left ,node)
         (setf (phpinspect-splayt-node-parent (phpinspect-splayt-node-left ,node)) ,node))

       ;; Update root node of tree when necessary
       (when (and ,splayt (eq ,node (phpinspect-splayt-root-node ,splayt)))
         (setf (phpinspect-splayt-root-node ,splayt) (phpinspect-splayt-node-parent ,node)))))))

(define-inline phpinspect-splayt-node-rotate-left (node &optional splayt)
  (inline-letevals (node splayt)
    (inline-quote
     (progn
       ;; Save left node of right child
       (setf (phpinspect-splayt-node-temp-store ,node)
             (phpinspect-splayt-node-left (phpinspect-splayt-node-right ,node)))

       ;; Update node parent to reference right as child
       (when (phpinspect-splayt-node-parent ,node)
         (phpinspect-splayt-node-update-parent
          ,node (phpinspect-splayt-node-parent ,node) (phpinspect-splayt-node-right ,node)))

       ;; Set right node new parent
       (setf (phpinspect-splayt-node-parent (phpinspect-splayt-node-right ,node))
             (phpinspect-splayt-node-parent ,node))

       ;; Set right node as node's new parent
       (setf (phpinspect-splayt-node-parent ,node)
             (phpinspect-splayt-node-right ,node))

       ;; Set node as right's left child
       (setf (phpinspect-splayt-node-left (phpinspect-splayt-node-right ,node)) ,node)

       ;; Set right's left child as node's  right child
       (setf (phpinspect-splayt-node-right ,node) (phpinspect-splayt-node-temp-store ,node))

       ;; Update new right child's parent
       (when (phpinspect-splayt-node-right ,node)
         (setf (phpinspect-splayt-node-parent (phpinspect-splayt-node-right ,node)) ,node))

       ;; Update root node of tree when necessary
       (when (and ,splayt (eq ,node (phpinspect-splayt-root-node ,splayt)))
         (setf (phpinspect-splayt-root-node ,splayt) (phpinspect-splayt-node-parent ,node)))))))

(define-inline phpinspect-splayt-node-grandparent (node)
  (inline-quote (phpinspect-splayt-node-parent (phpinspect-splayt-node-parent ,node))))

(define-inline phpinspect-splay (splayt node)
  (inline-letevals (splayt node)
    (let ((parent (inline-quote (phpinspect-splayt-node-parent ,node)))
          (grandparent (inline-quote (phpinspect-splayt-node-grandparent ,node))))
      (inline-quote
       (progn
         (while ,parent
           (if (phpinspect-splayt-node-grandparent ,node)
               (cond
                ;; Zig-Zig rotation
                ((and (eq ,parent (phpinspect-splayt-node-left ,grandparent))
                      (eq ,node (phpinspect-splayt-node-left ,parent)))
                 (phpinspect-splayt-node-rotate-right ,grandparent ,splayt)
                 (phpinspect-splayt-node-rotate-right ,parent ,splayt))

                ;; Zag-Zag rotation
                ((and (eq (phpinspect-splayt-node-parent ,node)
                          (phpinspect-splayt-node-right (phpinspect-splayt-node-grandparent ,node)))
                      (eq ,node (phpinspect-splayt-node-right (phpinspect-splayt-node-parent ,node))))
                 (phpinspect-splayt-node-rotate-left ,grandparent ,splayt)
                 (phpinspect-splayt-node-rotate-left ,parent ,splayt))

                ;; Zig-Zag rotation
                ((and (eq ,parent (phpinspect-splayt-node-right ,grandparent))
                      (eq ,node (phpinspect-splayt-node-left ,parent)))
                 (phpinspect-splayt-node-rotate-right ,parent ,splayt)
                 (phpinspect-splayt-node-rotate-left ,parent ,splayt))

                ;; Zag-Zig rotation
                ((and (eq ,parent (phpinspect-splayt-node-left ,grandparent))
                      (eq ,node (phpinspect-splayt-node-right ,parent)))
                 (phpinspect-splayt-node-rotate-left ,parent ,splayt)
                 (phpinspect-splayt-node-rotate-right ,parent ,splayt))
                (t
                 (error "Failed in determining rotation strategy.")))

             ;; Else
             (if (eq ,node (phpinspect-splayt-node-left ,parent))
                 (phpinspect-splayt-node-rotate-right ,parent ,splayt)
               (phpinspect-splayt-node-rotate-left ,parent ,splayt))))

         ,node)))))

(define-inline phpinspect-splayt-insert-node (splayt node)
  (inline-letevals (splayt node (parent (inline-quote (phpinspect-splayt-node-temp-store ,node))))
    (inline-quote
     (if (phpinspect-splayt-empty-p ,splayt)
         (setf (phpinspect-splayt-root-node ,splayt) ,node)
       (progn
         (setf ,parent (phpinspect-splayt-find-insertion-node ,splayt (phpinspect-splayt-node-key ,node)))
         (unless ,parent
           (error "Error: failed to find parent node for %s" ,node))

         (setf (phpinspect-splayt-node-parent ,node) ,parent)
         (if (< (phpinspect-splayt-node-key ,parent) (phpinspect-splayt-node-key ,node))
             (setf (phpinspect-splayt-node-right ,parent) ,node)
           (setf (phpinspect-splayt-node-left ,parent) ,node))

         (phpinspect-splay ,splayt ,node))))))

(defmacro phpinspect-splayt-node-traverse (place-and-node &rest body)
  (declare (indent 1))
  (let ((place (car place-and-node))
        (current-sym (gensym))
        (stack-sym (gensym))
        (queue-sym (gensym))
        (reverse-sym (gensym))
        (node-sym (gensym))
        (size-sym (gensym)))
    `(let* ((,node-sym ,(cadr place-and-node))
            ;; Make place locally scoped variable if a symbol
            (,queue-sym (when ,node-sym
                          (list ,node-sym)))
            (,reverse-sym t)
            ,current-sym
            ,size-sym
            ,stack-sym)

       (while ,queue-sym
         (setq ,size-sym (length ,queue-sym))

         (while (> ,size-sym 0)
           (setq ,current-sym (car (last ,queue-sym))
                 ,queue-sym (butlast ,queue-sym))

           (if ,reverse-sym
               (push ,current-sym ,stack-sym)
             (let ((,place ,current-sym))
               ,@body))

           (when (phpinspect-splayt-node-right ,current-sym)
             (push (phpinspect-splayt-node-right ,current-sym) ,queue-sym))

           (when (phpinspect-splayt-node-left ,current-sym)
             (push (phpinspect-splayt-node-left ,current-sym) ,queue-sym))

           (setq ,size-sym (- ,size-sym 1)))

         (when ,reverse-sym
           (while ,stack-sym
             (setq ,current-sym (pop ,stack-sym))
             (let ((,place ,current-sym))
               ,@body)))

         (setq ,reverse-sym (not ,reverse-sym)))

       nil)))

(defmacro phpinspect-splayt-traverse (place-and-splayt &rest body)
  "Traverse SPLAYT, executing BODY.

The PLACE is assigned the value of each node.

Traversal is breadth-first to take advantage of the splay trees'
main benefit: the most accessed interval of keys is likely to be
near the top of the tee.

(fn (PLACE SPLAYT) BODY...)"
  (declare (indent 1))
  `(phpinspect-splayt-node-traverse
       (,(car place-and-splayt) (phpinspect-splayt-root-node ,(cadr place-and-splayt)))
     (setf ,(car place-and-splayt) (phpinspect-splayt-node-value ,(car place-and-splayt)))
     ,@body))

(defmacro phpinspect-splayt-node-traverse-lr (place-and-node &rest body)
  (declare (indent 1))
  (let ((place (car place-and-node))
        (current (gensym))
        (stack (gensym)))
    `(let* ((,current ,(cadr place-and-node))
            ,stack)
       (while (or ,stack ,current)
         (if ,current
             (progn
               (push ,current ,stack)
               (setq ,current (phpinspect-splayt-node-left ,current)))
           (setq ,current (pop ,stack))
           (let ((,place ,current))
             ,@body)
           (setq ,current (phpinspect-splayt-node-right ,current)))))))

(defmacro phpinspect-splayt-traverse-lr (place-and-splayt &rest body)
  "Traverse SPLAYT depth-first from left to right,executing BODY.

The PLACE is assigned the value of each node.

(fn (PLACE SPLAYT) BODY...)"
  (declare (indent 1))
  `(phpinspect-splayt-node-traverse-lr
       (,(car place-and-splayt) (phpinspect-splayt-root-node ,(cadr place-and-splayt)))
       (let ((,(car place-and-splayt) (phpinspect-splayt-node-value ,(car place-and-splayt))))
         ,@body)))

(define-inline phpinspect-splayt-node-key-less-p (node key)
  (inline-quote (> ,key (phpinspect-splayt-node-key ,node))))

(define-inline phpinspect-splayt-node-key-le-p (node key)
  (inline-quote (>= ,key (phpinspect-splayt-node-key ,node))))

(define-inline phpinspect-splayt-node-key-equal-p (node key)
  (inline-quote (= ,key (phpinspect-splayt-node-key ,node))))

(define-inline phpinspect-splayt-node-key-greater-p (node key)
  (inline-quote (< ,key (phpinspect-splayt-node-key ,node))))

(define-inline phpinspect-splayt-node-key-ge-p (node key)
  (inline-quote (<= ,key (phpinspect-splayt-node-key ,node))))

(define-inline phpinspect-splayt-find-node (splayt key)
  (inline-letevals (splayt key)
    (inline-quote
     (let ((current (phpinspect-splayt-root-node ,splayt)))
       (catch 'return
         (while current
           (if (= ,key (phpinspect-splayt-node-key current))
               (progn
                 (phpinspect-splay ,splayt current)
                 (throw 'return current))
             (if (phpinspect-splayt-node-key-greater-p current ,key)
                 (setq current (phpinspect-splayt-node-left current))
               (setq current (phpinspect-splayt-node-right current))))))))))

(define-inline phpinspect-splayt-find-insertion-node (splayt key)
  (inline-letevals (splayt key)
    (inline-quote
     (let ((current (phpinspect-splayt-root-node ,splayt)))
       (catch 'return
         (while current
           (if (or (and (phpinspect-splayt-node-key-greater-p current ,key)
                        (not (phpinspect-splayt-node-left current)))
                   (and (phpinspect-splayt-node-key-le-p current ,key)
                        (not (phpinspect-splayt-node-right current))))
               (throw 'return current)
             (if (< ,key (phpinspect-splayt-node-key current))
                 (setq current (phpinspect-splayt-node-left current))
               (setq current (phpinspect-splayt-node-right current))))))))))

(define-inline phpinspect-splayt-find-smallest-node-after (splayt key)
  (inline-letevals (splayt key)
    (inline-quote
     (let ((current (phpinspect-splayt-root-node ,splayt))
           smallest)

       (catch 'break
         (while current
           (cond
            ((phpinspect-splayt-node-key-greater-p current ,key)
             (when (and smallest
                      (phpinspect-splayt-node-key-greater-p
                       current (phpinspect-splayt-node-key smallest)))
               (throw 'break nil))

             (setf smallest current
                   current (phpinspect-splayt-node-left current)))
            ((phpinspect-splayt-node-right current)
             (setf current (phpinspect-splayt-node-right current)))
            (t (throw 'break nil)))))

       smallest))))

(define-inline phpinspect-splayt-find-largest-node-before (splayt key)
  (inline-letevals (splayt key)
    (inline-quote
     (let ((current (phpinspect-splayt-root-node ,splayt))
           largest)

       (catch 'break
         (while current
           (cond
            ((and (phpinspect-splayt-node-key-less-p current ,key))
             (when (and largest
                        (phpinspect-splayt-node-key-less-p
                         current (phpinspect-splayt-node-key largest)))
               (throw 'break nil))
             (setf largest current
                   current (phpinspect-splayt-node-right current)))
            ((phpinspect-splayt-node-left current)
             (setf current (phpinspect-splayt-node-left current)))
            (t (throw 'break nil)))))

       largest))))

(defun phpinspect-splayt-find-all-after (splayt key)
  "Find all values in SPLAYT with a key higher than KEY."
  (let* ((first (phpinspect-splayt-find-smallest-node-after splayt key))
         (all (cons nil nil))
         (all-rear all))
    (while first
      (setq all-rear (setcdr all-rear (cons (phpinspect-splayt-node-value first) nil)))

      (phpinspect-splayt-node-traverse (sibling (phpinspect-splayt-node-right first))
        (setq all-rear (setcdr all-rear (cons (phpinspect-splayt-node-value sibling) nil))))

      (if (and (phpinspect-splayt-node-parent first)
               (eq first (phpinspect-splayt-node-left (phpinspect-splayt-node-parent first))))
          (setq first (phpinspect-splayt-node-parent first))
        (setq first nil)))
    (cdr all)))

(defun phpinspect-splayt-find-all-between (splayt key-min key-max)
  (let* ((first (phpinspect-splayt-find-smallest-node-after splayt key-min))
         (all (cons nil nil))
         (all-rear all))
    (catch 'return
      (while first
        (setq all-rear (setcdr all-rear (cons (phpinspect-splayt-node-value first) nil)))

        (phpinspect-splayt-node-traverse-lr (sibling (phpinspect-splayt-node-right first))
          (when (>= (phpinspect-splayt-node-key sibling) key-max)
            (throw 'return (cdr all)))
          (setq all-rear (setcdr all-rear (cons (phpinspect-splayt-node-value sibling) nil))))

        (if (and (phpinspect-splayt-node-parent first)
                 (eq first (phpinspect-splayt-node-left (phpinspect-splayt-node-parent first))))
            (setq first (phpinspect-splayt-node-parent first))
          (setq first nil)))
      (cdr all))))

(defun phpinspect-splayt-find-all-before (splayt key)
  "Find all values in SPLAYT with a key higher than KEY."
  (let* ((first (phpinspect-splayt-find-largest-node-before splayt key))
         (all (cons nil nil))
         (all-rear all))
    (while first
      (setq all-rear (setcdr all-rear (cons (phpinspect-splayt-node-value first) nil)))

      (phpinspect-splayt-node-traverse (sibling (phpinspect-splayt-node-left first))
        (setq all-rear (setcdr all-rear (cons (phpinspect-splayt-node-value sibling) nil))))

      (if (and (phpinspect-splayt-node-parent first)
               (eq first (phpinspect-splayt-node-right (phpinspect-splayt-node-parent first))))
          (setq first (phpinspect-splayt-node-parent first))
        (setq first nil)))
    (cdr all)))

(defun phpinspect-splayt-to-list (tree)
  "Convert TREE to an ordered list."
  (let* ((list (cons nil nil))
         (rear list))
    (phpinspect-splayt-traverse-lr (val tree)
      (setq rear (setcdr rear (cons val nil))))

    (cdr list)))

(cl-defmethod seq-do (func (tree (head phpinspect-splayt)))
  (phpinspect-splayt-traverse-lr (val tree)
    (funcall func val)))

(defun phpinspect-splayt-find-smallest-after (splayt key)
  "Find value of node with smallest key that is higher than KEY in SPLAYT."
  (phpinspect-splayt-node-value
   (phpinspect-splay
    splayt (phpinspect-splayt-find-smallest-node-after splayt key))))

(defun phpinspect-splayt-find-largest-before (splayt key)
  "Find value of node with smallest key that is higher than KEY in SPLAYT."
   (phpinspect-splayt-node-value
    (phpinspect-splay
     splayt (phpinspect-splayt-find-largest-node-before splayt key))))

(defun phpinspect-splayt-find (splayt key)
  (phpinspect-splayt-node-value (phpinspect-splayt-find-node splayt key)))

(defun phpinspect-splayt-insert (tree key value)
  "Insert KEY as VALUE into TREE."
  (phpinspect-splayt-insert-node tree (phpinspect-make-splayt-node key value)))

(provide 'phpinspect-splayt)
