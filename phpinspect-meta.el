;;; phpinspect-meta.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'phpinspect-splayt)

(define-inline phpinspect-make-meta
  (parent start end whitespace-before token &optional overlay children parent-offset)
  (inline-quote (list 'meta ,parent ,start ,end ,whitespace-before ,token ,overlay
                      ;;,children ,parent-offset)))
                      (or ,children (phpinspect-make-splayt)) ,parent-offset)))

(define-inline phpinspect-meta-parent (meta)
  (inline-quote (cadr ,meta)))

(define-inline phpinspect-meta-children (meta)
  (inline-quote (car (nthcdr 7 ,meta))))

(define-inline phpinspect-meta-parent-offset (meta)
  (inline-quote (car (nthcdr 8 ,meta))))

(define-inline phpinspect-meta-overlay (meta)
  (inline-quote (car (nthcdr 6 ,meta))))

(define-inline phpinspect-meta-token (meta)
  (inline-quote (car (nthcdr 5 ,meta))))

(define-inline phpinspect-meta-absolute-end (meta)
  (inline-quote (cadddr ,meta)))

(define-inline phpinspect-meta-whitespace-before (meta)
  (inline-quote (car (cddddr ,meta))))

(defun phpinspect-meta-start (meta)
  (if (phpinspect-meta-parent meta)
      (+ (phpinspect-meta-start (phpinspect-meta-parent meta))
         (phpinspect-meta-parent-offset meta))
    (phpinspect-meta-absolute-start meta)))

(defun phpinspect-meta-end (meta)
  (+ (phpinspect-meta-start meta) (phpinspect-meta-width meta)))

(defsubst phpinspect-meta-width (meta)
  (- (phpinspect-meta-absolute-end meta) (phpinspect-meta-absolute-start meta)))

(defun phpinspect-meta-sort-width (meta1 meta2)
  (< (phpinspect-meta-width meta1) (phpinspect-meta-width meta2)))

(defun phpinspect-meta-sort-start (meta1 meta2)
  (< (phpinspect-meta-start meta1) (phpinspect-meta-start meta2)))

(define-inline phpinspect-meta-absolute-start (meta)
  (inline-quote (caddr ,meta)))

(defsubst phpinspect-meta-overlaps-point (meta point)
  (and (> (phpinspect-meta-end meta) point)
       (<= (phpinspect-meta-start meta) point)))

(defun phpinspect-meta-find-parent-matching-token (meta predicate)
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
       (setcar (cdr ,meta) ,parent)))))

;; Note: using defsubst here causes a byte code overflow
(defun phpinspect-meta-add-child (meta child)
  (phpinspect-splayt-insert (phpinspect-meta-children meta) (phpinspect-meta-parent-offset child) child))

(define-inline phpinspect-meta-detach-parent (meta)
  (inline-letevals (meta)
    (inline-quote
     (when (phpinspect-meta-parent ,meta)
       ;; Update absolute start and end
       (setf (phpinspect-meta-absolute-start ,meta) (phpinspect-meta-start ,meta))
       (setf (phpinspect-meta-absolute-end ,meta) (phpinspect-meta-end ,meta))
       (setf (phpinspect-meta-parent ,meta) nil)))))

(defun phpinspect-meta-shift (meta delta)
  (setf (phpinspect-meta-absolute-start meta) (+ (phpinspect-meta-start meta) delta))
  (setf (phpinspect-meta-absolute-end meta) (+ (phpinspect-meta-end meta) delta)))

(defun phpinspect-meta-right-siblings (meta)
  (mapcar #'phpinspect-meta-token
          (sort
           (phpinspect-splayt-find-all-after
            (phpinspect-meta-children (phpinspect-meta-parent meta)) (phpinspect-meta-parent-offset meta))
           #'phpinspect-meta-sort-start)))


(defun phpinspect-meta-string (meta)
  (format "[start: %d, end: %d, token: %s]"
          (phpinspect-meta-start meta) (phpinspect-meta-end meta) (phpinspect-meta-token meta)))


(provide 'phpinspect-meta)
;;; phpinspect-meta.el ends here
