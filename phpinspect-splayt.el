;;; phpinspect-splayt.el --- A Splay Tree Implementation  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 0

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
;; - `phpinspect-splayt-traverse'

;;; Code:

(defsubst phpinspect-make-splayt-node (key value &optional left right)
  (cons (cons key value) (cons left right)))

(defsubst phpinspect-splayt-node-left (node)
  (cadr node))

(defsubst phpinspect-splayt-node-right (node)
  (cddr node))

(defsubst phpinspect-splayt-node-key (node)
  (caar node))

(defsubst phpinspect-splayt-node-value (node)
  (cdar node))

(gv-define-setter phpinspect-splayt-node-left (left node) `(setcar (cdr ,node) ,left))
(gv-define-setter phpinspect-splayt-node-right (right node) `(setcdr (cdr ,node) ,right))
(gv-define-setter phpinspect-splayt-node-key (key node) `(setcar (car ,node) ,value))
(gv-define-setter phpinspect-splayt-node-value (value node) `(setcdr (car ,node) ,value))

(defsubst phpinspect-splayt-node-update-parent (node parent new-val)
  (if (eq node (phpinspect-splayt-node-left parent))
      (setf (phpinspect-splayt-node-left parent) new-val)
    (setf (phpinspect-splayt-node-right parent) new-val)))

(defsubst phpinspect-make-splayt (&optional root-node)
  (cons root-node nil))

(defsubst phpinspect-splayt-root-node (splayt)
  (car splayt))

(gv-define-setter phpinspect-splayt-root-node (node splayt) `(setcar ,splayt ,node))

(defsubst phpinspect-splayt-node-rotate-right (node &optional parent splayt)
  (let* ((left (phpinspect-splayt-node-left node))
         (left-right (phpinspect-splayt-node-right left)))
    (setf (phpinspect-splayt-node-right left) node)
    (setf (phpinspect-splayt-node-left node) left-right)

    (when (and splayt (eq node (phpinspect-splayt-root-node splayt)))
      (setf (phpinspect-splayt-root-node splayt) left))

    (when parent
      (phpinspect-splayt-node-update-parent node parent left))))

(defsubst phpinspect-splayt-node-rotate-left (node &optional parent splayt)
  (let* ((right (phpinspect-splayt-node-right node))
         (right-left (phpinspect-splayt-node-left right)))
    (setf (phpinspect-splayt-node-left right) node)
    (setf (phpinspect-splayt-node-right node) right-left)

    (when (and splayt (eq node (phpinspect-splayt-root-node splayt)))
      (setf (phpinspect-splayt-root-node splayt) right))

    (when parent
      (phpinspect-splayt-node-update-parent node parent right))))

(defsubst phpinspect-make-splayt-nav (splayt &optional current-node parents)
  (cons splayt (cons (or current-node (phpinspect-splayt-root-node splayt))
                     parents)))

(defsubst phpinspect-splayt-nav-splayt (nav)
  (car nav))

(defsubst phpinspect-splayt-nav-current (nav)
  (cadr nav))

(defsubst phpinspect-splayt-nav-parents (nav)
  (cddr nav))

(gv-define-setter phpinspect-splayt-nav-current (node nav) `(setcar (cdr ,nav) ,node))
(gv-define-setter phpinspect-splayt-nav-parents (parents nav) `(setcdr (cdr ,nav) ,parents))

(defsubst phpinspect-splayt-nav-right (nav)
  (push (phpinspect-splayt-nav-current nav) (phpinspect-splayt-nav-parents nav))
  (setf (phpinspect-splayt-nav-current nav)
        (phpinspect-splayt-node-right (phpinspect-splayt-nav-current nav))))

(defsubst phpinspect-splayt-nav-left (nav)
  (push (phpinspect-splayt-nav-current nav) (phpinspect-splayt-nav-parents nav))
  (setf (phpinspect-splayt-nav-current nav)
        (phpinspect-splayt-node-left (phpinspect-splayt-nav-current nav))))

(defsubst phpinspect-splayt-nav-has-left-p (nav)
  (phpinspect-splayt-node-left (phpinspect-splayt-nav-current nav)))

(defsubst phpinspect-splayt-nav-has-right-p (nav)
  (phpinspect-splayt-node-right (phpinspect-splayt-nav-current nav)))

(defsubst phpinspect-splayt-nav-up (nav)
  (setf (phpinspect-splayt-nav-current nav)
        (pop (phpinspect-splayt-nav-parents nav))))

(defsubst phpinspect-splayt-nav-current-value (nav)
  (phpinspect-splayt-node-value (phpinspect-splayt-nav-current nav)))

(defsubst phpinspect-splayt-nav-current-key (nav)
  (phpinspect-splayt-node-key (phpinspect-splayt-nav-current nav)))

(defsubst phpinspect-splayt-insert (splayt key value)
  (phpinspect-splayt-insert-node splayt (phpinspect-make-splayt-node key value)))

(defsubst phpinspect-splay (splayt node parents)
  (let (grandparent great-grandparent)
    (while parents
      (setq parent (pop parents))
      (setq grandparent (pop parents))

      (if grandparent
          (cond
           ;; Zig-Zig rotation
           ((and (eq parent (phpinspect-splayt-node-left grandparent))
                 (eq node (phpinspect-splayt-node-left parent)))
            (phpinspect-splayt-node-rotate-right grandparent (car parents) splayt)
            (phpinspect-splayt-node-rotate-right parent (car parents) splayt))

           ;; Zag-Zag rotation
           ((and (eq parent (phpinspect-splayt-node-right grandparent))
                 (eq node (phpinspect-splayt-node-right parent)))
            (phpinspect-splayt-node-rotate-left grandparent (car parents) splayt)
            (phpinspect-splayt-node-rotate-left parent (car parents) splayt))

           ;; Zig-Zag rotation
           ((and (eq parent (phpinspect-splayt-node-right grandparent))
                 (eq node (phpinspect-splayt-node-left parent)))
            (phpinspect-splayt-node-rotate-right parent grandparent splayt)
            (phpinspect-splayt-node-rotate-left grandparent (car parents) splayt))

           ;; Zag-Zig rotation
           ((and (eq parent (phpinspect-splayt-node-left grandparent))
                 (eq node (phpinspect-splayt-node-right parent)))
            (phpinspect-splayt-node-rotate-left parent grandparent splayt)
            (phpinspect-splayt-node-rotate-right grandparent (car parents) splayt))
           (t
            (error "Failed in determining rotation strategy")))
        ;; Else
        (if (eq node (phpinspect-splayt-node-left parent))
            (phpinspect-splayt-node-rotate-right parent (car parents) splayt)
          (phpinspect-splayt-node-rotate-left parent (car parents) splayt))))))

(defsubst phpinspect-splayt-insert-node (splayt node)
  (if (not (phpinspect-splayt-root-node splayt))
      (setf (phpinspect-splayt-root-node splayt) node)

    ;; Else
    (let ((nav (phpinspect-make-splayt-nav splayt)))
      (catch 'break
        (while t
          (if (< (phpinspect-splayt-node-key node)
                 (phpinspect-splayt-nav-current-key nav))
              (if (phpinspect-splayt-nav-has-left-p nav)
                  (phpinspect-splayt-nav-left nav)
                (setf (phpinspect-splayt-node-left (phpinspect-splayt-nav-current nav))
                      node)
                (throw 'break nil))

            ;; Else
            (if (phpinspect-splayt-nav-has-right-p nav)
                (phpinspect-splayt-nav-right nav)
              (setf (phpinspect-splayt-node-right (phpinspect-splayt-nav-current nav))
                    node)
              (throw 'break nil))))))))

(defmacro phpinspect-splayt-traverse (place-and-splayt &rest body)
  "Traverse splay tree in cadr of PLACE-AND-SPLAYT, executing BODY.

The car of PLACE-AND-SPLAYT is assigned the value of each node.

Traversal is breadth-first to take advantage of the splay trees
main benefit: the most accessed interval of keys is likely to be
near the top of the tee."
  (declare (indent 1))
  (let ((place (car place-and-splayt))
        (current-sym (gensym))
        (splayt-sym (gensym))
        (stack-sym (gensym))
        (queue-sym (gensym))
        (reverse-sym (gensym))
        (size-sym (gensym)))
    `(let* ((,splayt-sym ,(cadr place-and-splayt))
            ;; Make place locally scoped variable if a symbol
            (,queue-sym (list (phpinspect-splayt-root-node ,splayt-sym)))
            (,reverse-sym t)
            ,size-sym
            ,stack-sym
            ,(if (symbolp place) place (gensym)))

       (while ,queue-sym
         (setq ,size-sym (length ,queue-sym))

         (while (> ,size-sym 0)
           (setq ,current-sym (car (last ,queue-sym))
                 ,queue-sym (butlast ,queue-sym))

           (if ,reverse-sym
               (push ,current-sym ,stack-sym)
             (setf ,place (phpinspect-splayt-node-value ,current-sym))
             ,@body)

           (when (phpinspect-splayt-node-right ,current-sym)
             (setq ,queue-sym (nconc ,queue-sym (list (phpinspect-splayt-node-right ,current-sym)))))

           (when (phpinspect-splayt-node-left ,current-sym)
             (setq ,queue-sym (nconc ,queue-sym (list (phpinspect-splayt-node-left ,current-sym)))))

           (setq ,size-sym (- ,size-sym 1)))

         (when ,reverse-sym
           (while ,stack-sym
             (setq ,current-sym (pop ,stack-sym))
             (setf ,place (phpinspect-splayt-node-value ,current-sym))
             ,@body))

         (setq ,reverse-sym (not ,reverse-sym))))))

(defsubst phpinspect-splayt-find (splayt key &optional navigator matcher continue-predicate)
  (unless navigator (setq navigator #'<))
  (unless matcher (setq matcher #'=))

  (let ((nav (phpinspect-make-splayt-nav splayt))
        current next)
    (when (phpinspect-splayt-nav-current nav)
      (catch 'found
        (while t
          (setq current (phpinspect-splayt-nav-current nav)
                next nil)
          (cond
           ((funcall navigator key (phpinspect-splayt-nav-current-key nav))
            (when (phpinspect-splayt-nav-has-left-p nav)
              (phpinspect-splayt-nav-left nav)
              (setq next (phpinspect-splayt-nav-current nav))))
           (t
            (when (phpinspect-splayt-nav-has-right-p nav)
              (phpinspect-splayt-nav-right nav)
              (setq next (phpinspect-splayt-nav-current nav)))))

          (if (funcall matcher key (phpinspect-splayt-node-key current))
              (when (or (not next)
                        (not continue-predicate)
                        (not (funcall continue-predicate key (phpinspect-splayt-node-key next))))
                (phpinspect-splay
                 splayt current
                 (if next (cdr (phpinspect-splayt-nav-parents nav)) (phpinspect-splayt-nav-parents nav)))
                (throw 'found (phpinspect-splayt-node-value current)))
            (unless next
              (throw 'found nil))))))))

(provide 'phpinspect-splayt)
