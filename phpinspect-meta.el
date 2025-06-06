;;; phpinspect-meta.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 3.0.1

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
;; IMPLEMENTATION CONTEXT
;;
;; This file contains code for the storing and reading of metadata related to
;; the tokens in a parsed syntax tree.
;;
;; Phpinspect's parser uses basic lists as datastructure for its parsing
;; result.  The simplicity of the datastructure makes it performant as the amount
;; of GC's triggered by simple lists is not as large as when using more complex
;; datastructures.
;;
;; A drawback of this simplicity is that attaching additional metadata to the
;; parsed tokens is nontrivial.  A list is a list: There is no way to store
;; additional metadata in a separate slot that is not part of its overall
;; members.  This is fine when parsing entire files and indexing their
;; contents.  Efficiently maintaining the state of code in a buffer, however, is
;; a more involved process.  It requires the storage of more metadata than just
;; the parsed tokens.
;;
;; For this reason, phpinspect uses a metadata tree for live buffers.  The
;; metadata tree can be interacted with through functions prefixed with
;; "phpinspect-meta".  It is an n-ary tree that can be navigated and searched
;; through using the start position of parsed tokens.  Each metadata object
;; stores its direct children in a binary search tree (see phpinspect-splayt.el).
;;
;; The metadata tree makes it easy to dig down in a buffer's syntax tree and
;; determine the context from which a user is interacting with the PHP code in
;; it.
;;
;;; Code:

(require 'phpinspect-splayt)
(require 'phpinspect-token-predicates)

(eval-and-compile
  (defvar phpinspect-meta--point-offset-base nil
    "This variable overrides `phpinspect-meta-start'. Normally,
metadata objects derive their start position relative to their
parents. This is at some performance cost because the start
position is re-calculated each time it is accessed.

There are scenarios, most notably when parsing incrementally,
where the start position of the token is already known while
interacting with the metadata object. In such cases this variable
can be set to override the start position, preventing any
additional calculations.

Note: Use this sparingly in scenarios where performance is of
significant importance. The code that uses this will be harder to
maintain. It requires a lot of mental tracing to know when to
set/unset this variable."))

(define-inline phpinspect-make-meta
  (parent start end whitespace-before token &optional children parent-offset)
  "Create a metadata object for TOKEN."
  (inline-letevals (start end)
    (inline-quote (list 'meta ,parent ,start ,end ,whitespace-before ,token
                        (or ,children (phpinspect-make-splayt)) ,parent-offset))))

(define-inline phpinspect-meta-p (meta)
  (inline-quote (eq 'meta (car-safe ,meta))))

(define-inline phpinspect-meta-parent (meta)
  (inline-quote (cadr ,meta)))

(define-inline phpinspect-meta-children (meta)
  (inline-quote (car (nthcdr 6 ,meta))))

(define-inline phpinspect-meta-parent-offset (meta)
  (inline-quote (car (nthcdr 7 ,meta))))


(define-inline phpinspect-meta-token (meta)
  (inline-quote (car (nthcdr 5 ,meta))))

(define-inline phpinspect-meta-absolute-end (meta)
  (inline-quote (cadddr ,meta)))

(define-inline phpinspect-meta-whitespace-before (meta)
  (inline-quote (car (cddddr ,meta))))

(define-inline phpinspect-meta-parent-start (meta)
  "Calculate parent start position iteratively based on parent offsets."
  (inline-letevals (meta)
    (inline-quote
     (let ((start (or (phpinspect-meta-parent-offset ,meta) 0))
           (current ,meta))
       (while (phpinspect-meta-parent current)
         (setq current (phpinspect-meta-parent current)
               start (+ start (or (phpinspect-meta-parent-offset current) 0))))

       (+ (phpinspect-meta-absolute-start current) start)))))

(define-inline phpinspect-meta--start (meta)
  "Calculate the start position of META."
  (inline-letevals (meta)
    (inline-quote
     (if (phpinspect-meta-parent ,meta)
         (+ (phpinspect-meta-parent-start (phpinspect-meta-parent ,meta))
            (phpinspect-meta-parent-offset ,meta))
       (phpinspect-meta-absolute-start ,meta)))))

(define-inline phpinspect-meta-start (meta)
  "Start position of META.

Either calculates relative to parent or returns
`phpinspect-meta--point-offset-base' when that variable is set."
  (inline-quote
   (or phpinspect-meta--point-offset-base (phpinspect-meta--start ,meta))))

(define-inline phpinspect-meta-width (meta)
  (inline-letevals (meta)
    (inline-quote
     (- (phpinspect-meta-absolute-end ,meta) (phpinspect-meta-absolute-start ,meta)))))

(define-inline phpinspect-meta-end (meta)
  (inline-letevals (meta)
    (inline-quote
     (+ (phpinspect-meta-start ,meta) (phpinspect-meta-width ,meta)))))

(defun phpinspect-meta-find-root (meta)
  "Find the root node of META's tree."
  (while (phpinspect-meta-parent meta)
    (setq meta (phpinspect-meta-parent meta)))
  meta)

(defun phpinspect-meta-sort-width (meta1 meta2)
  (< (phpinspect-meta-width meta1) (phpinspect-meta-width meta2)))

(defun phpinspect-meta-sort-start (meta1 meta2)
  (< (phpinspect-meta-start meta1) (phpinspect-meta-start meta2)))

(define-inline phpinspect-meta-absolute-start (meta)
  (inline-quote (caddr ,meta)))

(define-inline phpinspect-meta-overlaps-point (meta point)
  "Check if META's region overlaps POINT."
  (inline-letevals (point meta)
        (inline-quote
         (and (> (phpinspect-meta-end ,meta) ,point)
              (<= (phpinspect-meta-start ,meta) ,point)))))

(defun phpinspect-meta-find-parent-matching-token (meta predicate)
  "Find a parent metadata node of META, the token of which matches PREDICATE."
  (if (funcall predicate (phpinspect-meta-token meta))
      meta
    (catch 'found
      (while (phpinspect-meta-parent meta)
        (setq meta (phpinspect-meta-parent meta))
        (when (funcall predicate (phpinspect-meta-token meta))
          (throw 'found meta))))))

(define-inline phpinspect-meta-set-parent (meta parent)
  (inline-letevals (meta parent)
    (inline-quote
     (progn
       (when ,parent
         (setf (phpinspect-meta-parent-offset ,meta)
               (- (phpinspect-meta-start ,meta) (phpinspect-meta-start ,parent)))
         (phpinspect-meta-add-child ,parent ,meta))
       (setf (phpinspect-meta-parent ,meta) ,parent)
       ,meta))))

;; Note: using defsubst here causes a byte code overflow
(defun phpinspect-meta-add-child (meta child)
  (phpinspect-splayt-insert (phpinspect-meta-children meta) (phpinspect-meta-parent-offset child) child))

(define-inline phpinspect-meta-detach-parent (meta)
  (inline-letevals (meta)
    (inline-quote
     (when (phpinspect-meta-parent ,meta)
       ;; Delete self from parent
       (phpinspect-splayt-delete
        (phpinspect-meta-children (phpinspect-meta-parent ,meta))
        (phpinspect-meta-parent-offset ,meta))

       ;; Update absolute start and end
       (setf (phpinspect-meta-absolute-end ,meta) (phpinspect-meta-end ,meta))
       ;; Note: start should always be updated after end, as end is derived from
       ;; it.
       (setf (phpinspect-meta-absolute-start ,meta) (phpinspect-meta-start ,meta))
       (setf (phpinspect-meta-parent ,meta) nil)))))

(defun phpinspect-meta-shift (meta delta)
  "Move META by DELTA characters.

Negative DELTA will move to the left. Positive DELTA will move to the right.

All children are moved together (as they calculate their
positions based on the parent)."
  (setf (phpinspect-meta-absolute-start meta) (+ (phpinspect-meta-start meta) delta))
  (setf (phpinspect-meta-absolute-end meta) (dlet ((phpinspect-meta--point-offset-base nil))
                                              (+ (phpinspect-meta-end meta) delta))))

(defun phpinspect-meta-right-siblings (meta)
  (sort
   (phpinspect-splayt-find-all-after
    (phpinspect-meta-children (phpinspect-meta-parent meta)) (phpinspect-meta-parent-offset meta))
   #'phpinspect-meta-sort-start))

(defun phpinspect-meta-left-sibling-tokens (meta)
  (let* ((tokens (cons nil nil))
         (rear tokens))
    (dolist (sibling (phpinspect-meta-left-siblings meta))
      (setq rear (setcdr rear (cons (phpinspect-meta-token sibling) nil))))
    (cdr tokens)))

(defun phpinspect-meta-token-with-left-siblings (meta)
  "Return a list containing tokens of META and all of its left siblings."
  (nconc (phpinspect-meta-left-sibling-tokens meta) (list (phpinspect-meta-token meta))))

(defun phpinspect-meta-left-siblings (meta)
  (sort
   (phpinspect-splayt-find-all-before
    (phpinspect-meta-children (phpinspect-meta-parent meta)) (phpinspect-meta-parent-offset meta))
   #'phpinspect-meta-sort-start))

(define-inline phpinspect-meta--point-offset (meta point)
  (inline-quote
   (- ,point (or phpinspect-meta--point-offset-base (phpinspect-meta-start ,meta)))))

(cl-defmethod phpinspect-meta-find-left-sibling ((meta (head meta)))
  (when (phpinspect-meta-parent meta)
    (phpinspect-splayt-find-largest-before (phpinspect-meta-children (phpinspect-meta-parent meta))
                                           (phpinspect-meta-parent-offset meta))))

(cl-defmethod phpinspect-meta-find-left-sibling-matching-token ((meta (head meta)) predicate)
  (when-let ((parent (phpinspect-meta-parent meta)))
    (catch 'phpinspect--return
      (while (setq meta (phpinspect-splayt-find-largest-before (phpinspect-meta-children parent)
                                                               (phpinspect-meta-parent-offset meta)))
        (when (funcall predicate (phpinspect-meta-token meta))
          (throw 'phpinspect--return meta))

        (unless (phpinspect-comment-p (phpinspect-meta-token meta))
          (throw 'phpinspect--return nil))))))

(define-inline phpinspect-meta-find-right-sibling (meta)
  (inline-letevals (meta)
    (inline-quote
     (when (phpinspect-meta-parent ,meta)
       (phpinspect-splayt-find-smallest-after (phpinspect-meta-children (phpinspect-meta-parent ,meta))
                                              (phpinspect-meta-parent-offset ,meta))))))

(cl-defmethod phpinspect-meta-find-right-sibling-matching-token ((meta (head meta)) predicate)
  (when-let ((parent (phpinspect-meta-parent meta)))
    (catch 'phpinspect--return
      (while (setq meta (phpinspect-splayt-find-smallest-after (phpinspect-meta-children parent)
                                                               (phpinspect-meta-parent-offset meta)))
        (when (funcall predicate (phpinspect-meta-token meta))
          (throw 'phpinspect--return meta))

        (unless (phpinspect-comment-p (phpinspect-meta-token meta))
          (throw 'phpinspect--return nil))))))

(define-inline phpinspect-meta-find-overlapping-child (meta point)
  (inline-letevals (meta point)
    (inline-quote
     (if-let ((child (phpinspect-splayt-find-largest-before
                      (phpinspect-meta-children ,meta)
                      ;; Use point +1 as a child starting at point still overlaps
                      (+ (phpinspect-meta--point-offset ,meta ,point) 1))))
         (and (phpinspect-meta-overlaps-point child ,point) child)))))

(cl-defmethod phpinspect-meta-find-overlapping-children ((meta (head meta)) (point integer))
  (let ((child meta)
        children)
    (while (and (setq child (phpinspect-meta-find-overlapping-child child point))
                (phpinspect-meta-overlaps-point child point))
      (push child children))
    children))

(define-inline  phpinspect-meta-find-child-starting-at (meta point)
  (inline-letevals (meta point)
    (inline-quote
     (phpinspect-splayt-find (phpinspect-meta-children ,meta) (phpinspect-meta--point-offset ,meta ,point)))))

(define-inline phpinspect-meta-find-child-starting-at-recursively (meta point)
  (inline-letevals (point)
    (inline-quote
     (catch 'phpinspect--return
       (dlet ((phpinspect-meta--point-offset-base (phpinspect-meta-start ,meta)))
         (let ((meta ,meta))
           (while meta
             (if-let ((child (phpinspect-meta-find-child-starting-at meta ,point)))
                 (throw 'phpinspect--return child)
               (setq meta
                     (phpinspect-splayt-find-largest-before
                      (phpinspect-meta-children meta)
                      (phpinspect-meta--point-offset meta ,point))
                     phpinspect-meta--point-offset-base
                     (if meta (+ (phpinspect-meta-parent-offset meta) phpinspect-meta--point-offset-base)
                       phpinspect-meta--point-offset-base))))))))))

(defun phpinspect-meta-find-child-before (meta point)
  "Find the last child of META before POINT."
  (phpinspect-splayt-find-largest-before
   (phpinspect-meta-children meta) (phpinspect-meta--point-offset meta point)))

(cl-defmethod phpinspect-meta-find-child-after ((meta (head meta)) (point integer))
  (phpinspect-splayt-find-smallest-after
   (phpinspect-meta-children meta) (phpinspect-meta--point-offset meta point)))

(define-inline phpinspect-meta-find-child-after-recursively (meta point)
  (inline-letevals (point)
    (inline-quote
     (catch 'phpinspect--return
       (dlet ((phpinspect-meta--point-offset-base (phpinspect-meta-start ,meta)))
         (let ((meta ,meta))
           (while meta
             (if-let ((child (phpinspect-meta-find-child-after meta ,point)))
                 (throw 'phpinspect--return child)
               (setq meta
                     (or (phpinspect-splayt-find-smallest-after
                          (phpinspect-meta-children meta)
                          (phpinspect-meta--point-offset meta ,point))
                         (phpinspect-splayt-find-largest-before
                          (phpinspect-meta-children meta)
                          (phpinspect-meta--point-offset meta ,point)))
                     phpinspect-meta--point-offset-base
                     (if meta (+ (phpinspect-meta-parent-offset meta) phpinspect-meta--point-offset-base)
                       phpinspect-meta--point-offset-base))))))))))

(cl-defmethod phpinspect-meta-find-child-before-recursively ((meta (head meta)) (point integer))
  (let ((child meta)
        last)
    (while (setq child (phpinspect-meta-find-child-before child point))
      (setq last child))
    last))

(cl-defmethod phpinspect-meta-find-children-after ((meta (head meta)) (point integer))
  (sort
   (phpinspect-splayt-find-all-after
    (phpinspect-meta-children meta) (phpinspect-meta--point-offset meta point))
   #'phpinspect-meta-sort-start))

(cl-defmethod phpinspect-meta-find-children-before ((meta (head meta)) (point integer))
  (sort (phpinspect-splayt-find-all-before
         (phpinspect-meta-children meta) (phpinspect-meta--point-offset meta point))
        #'phpinspect-meta-sort-start))

(defun phpinspect-meta-find-first-child-matching (meta predicate)
  (cl-assert (phpinspect-meta-p meta))

  (catch 'return
    (phpinspect-splayt-traverse-lr (child (phpinspect-meta-children meta))
      (when (funcall predicate child)
        (throw 'return child)))))

(cl-defmethod phpinspect-meta-find-first-child-matching-token ((meta (head meta)) predicate)
  (catch 'return
    (phpinspect-splayt-traverse-lr (child (phpinspect-meta-children meta))
      (when (funcall predicate (phpinspect-meta-token child))
        (throw 'return child)))))

(defun phpinspect-meta-find-children-matching-token (meta predicate)
  (let (result)
    (phpinspect-splayt-traverse-lr (child (phpinspect-meta-children meta))
      (when (funcall predicate (phpinspect-meta-token child))
        (push child result)))
    result))

(defun phpinspect-meta-flatten (meta)
  "Flatten META and all its children into an unordered list."
  (when meta
    (let ((stack (list meta))
          (result (list meta)))

      (while-let ((current (pop stack)))
        (phpinspect-splayt-traverse-lr (child (phpinspect-meta-children current))
          (push child result)
          (push child stack)))

      result)))

(cl-defmethod phpinspect-meta-last-child ((meta (head meta)))
  (phpinspect-meta-find-child-before meta (phpinspect-meta-end meta)))

(cl-defmethod phpinspect-meta-first-child ((meta (head meta)))
  (phpinspect-meta-find-child-after meta (- (phpinspect-meta-start meta) 1)))

(defun phpinspect-meta-token-predicate (token-predicate)
  "Wrap TOKEN-PREDICATE in a closure that operates on metadata.

The returned closure takes a metadata object as argument and then
calls TOKEN-PREDICATE on its token
slot (`phpinspect-meta-token')."
  (lambda (meta) (funcall token-predicate (phpinspect-meta-token meta))))

(defun phpinspect-meta-string (meta)
  (if meta
      (format "[start: %d, end: %d, token: %s]"
              (phpinspect-meta-start meta) (phpinspect-meta-end meta) (phpinspect-meta-token meta))
    "[nil]"))

(provide 'phpinspect-meta)
;;; phpinspect-meta.el ends here
