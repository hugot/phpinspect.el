;;; phpinspect-edtrack.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'phpinspect-util)
(require 'phpinspect-token-predicates)

(eval-when-compile
  (require 'phpinspect-meta)
  (phpinspect--declare-log-group 'edtrack))

(cl-defstruct (phpinspect-edtrack (:constructor phpinspect-make-edtrack))
  (edits nil
         :type list)
  (taint-pool nil
              :type list)
  (last-edit nil
             :type cons
             :documentation "Last registered edit")
  (last-edit-start -1
                   :type integer
                   :documentation "Last registered edit start position"))

(defsubst phpinspect-edit-original-end (edit)
  (or (caar edit) 0))

(defsubst phpinspect-edit-delta (edit)
  (let ((delta (or (cdar edit) 0))
        (previous-edit edit))
    (while (setq previous-edit (cdr previous-edit))
      (setq delta (+ delta (cdar previous-edit))))
    delta))

(defsubst phpinspect-edit-end (edit)
  (let ((end (or (caar edit) 0))
        (previous-edit (cdr edit)))
    (+ end (phpinspect-edit-delta previous-edit))))

(defsubst phpinspect-edtrack-original-position-at-point (track point)
  (let ((edit (phpinspect-edtrack-edits track))
        (encroached)
        (pos))
    (while (and edit (<= point (phpinspect-edit-end edit)))
      (setq edit (cdr edit)))

    (setq pos (- point (phpinspect-edit-delta edit)))

    ;; When point is within the edit delta's range, correct the delta by the
    ;; amount of encroachment.
    (if (< 0 (setq encroached (- (+ (phpinspect-edit-end edit) (or (cdar edit) 0)) point)))
        (+ pos encroached)
      pos)))

(defsubst phpinspect-edtrack-current-position-at-point (track point)
  (let ((edit (phpinspect-edtrack-edits track))
        (encroached)
        (pos))
    (while (and edit (<= point (phpinspect-edit-original-end edit)))
      (setq edit (cdr edit)))

    (setq pos (+ point (phpinspect-edit-delta edit)))

    (if (< 0 (setq encroached (- (+ (phpinspect-edit-original-end edit) (or (cdar edit) 0)) point)))
        (- pos encroached)
      pos)))

(define-inline phpinspect-taint-start (taint)
  (inline-quote (car ,taint)))

(define-inline phpinspect-taint-end (taint)
  (inline-quote (cdr ,taint)))

(define-inline phpinspect-make-taint (start end)
  (inline-quote (cons ,start ,end)))

(defsubst phpinspect-taint-overlaps-point (taint point)
  (and (> (phpinspect-taint-end taint) point)
       (<= (phpinspect-taint-start taint) point)))

(defsubst phpinspect-taint-overlaps-region (taint start end)
  (or (phpinspect-taint-overlaps-point taint start)
      (phpinspect-taint-overlaps-point taint end)
      (and (> end (phpinspect-taint-start taint))
           (<= start (phpinspect-taint-start taint)))
      (and (> end (phpinspect-taint-end taint))
           (<= start (phpinspect-taint-end taint)))))

(defsubst phpinspect-taint-overlaps (taint1 taint2)
  (or (phpinspect-taint-overlaps-point taint1 (phpinspect-taint-start taint2))
      (phpinspect-taint-overlaps-point taint1 (phpinspect-taint-end taint2))
      (phpinspect-taint-overlaps-point taint2 (phpinspect-taint-start taint1))
      (phpinspect-taint-overlaps-point taint2 (phpinspect-taint-end taint1))))

(defsubst phpinspect-taint-overlaps-meta (taint meta)
  (or (phpinspect-taint-overlaps-point taint (phpinspect-meta-start meta))
      (phpinspect-taint-overlaps-point taint (phpinspect-meta-end meta))
      (phpinspect-meta-overlaps-point meta (phpinspect-taint-start taint))
      (phpinspect-meta-overlaps-point meta (phpinspect-taint-end taint))))

(defsubst phpinspect-edtrack-register-taint (track start end)
  (let ((pool (phpinspect-edtrack-taint-pool track))
        (idx 0)
        (overlap-start)
        (overlap-end)
        (left-neighbour)
        (taint (phpinspect-make-taint start end)))
    (catch 'break
      (while pool
        (if (phpinspect-taint-overlaps taint (car pool))
            (progn
              (when (< (phpinspect-taint-start (car pool)) start)
                (setcar taint (phpinspect-taint-start (car pool))))
              (when (> (phpinspect-taint-end (car pool)) end)
                (setcdr taint (phpinspect-taint-end (car pool))))

              (when (not overlap-start)
                (setq overlap-start idx))
              (setq overlap-end idx))

          ;; Else
          (when overlap-start
            (throw 'break nil))

          (when (> start (phpinspect-taint-end (car pool)))
            (setq left-neighbour pool)
            (throw 'break nil)))

        (setq pool (cdr pool)
              idx (+ idx 1))))

    (cond (overlap-start
           (setq pool (phpinspect-edtrack-taint-pool track))
           (setcar (nthcdr overlap-start pool) taint)
           (setcdr (nthcdr overlap-start pool) (nthcdr (+ 1 overlap-end) pool)))
          (left-neighbour
           (setcdr left-neighbour (cons taint (cdr left-neighbour))))
          (t
           (push taint (phpinspect-edtrack-taint-pool track))))))

(defsubst phpinspect-edtrack-register-edit (track start end pre-change-length)
  (phpinspect--log
   "Edtrack registered change: [start: %d, end: %d, pre-change-length: %d]"
   start end pre-change-length)

  (let ((original-start (phpinspect-edtrack-original-position-at-point track start)))
    (phpinspect-edtrack-register-taint
     track original-start (+ original-start pre-change-length)))

  (let ((edit-before (phpinspect-edtrack-edits track)))
    (while (and edit-before (< end (phpinspect-edit-end edit-before)))
      (setq edit-before (cdr edit-before)))

    (let ((delta ;; The delta of this edit.
           (- (- end start) pre-change-length))
          new-edit)
      (setq new-edit (cons
                        ;; The end location of the edited region, before being
                        ;; edited, with the delta edits that happened at preceding
                        ;; points in the buffer subtratted. This corresponds with
                        ;; the original position of the region end before the
                        ;; buffer was ever edited.
                        (phpinspect-edtrack-original-position-at-point
                         track (+ start pre-change-length))
                        delta))
        (if edit-before
            (progn
              (setcdr edit-before (cons (car edit-before) (cdr edit-before)))
              (setcar edit-before new-edit))
          (if (phpinspect-edtrack-edits track)
              (push new-edit (cdr (last (phpinspect-edtrack-edits track))))
            (push new-edit (phpinspect-edtrack-edits track)))))))


(defsubst phpinspect-edtrack-clear-taint-pool (track)
  (setf (phpinspect-edtrack-taint-pool track) nil))

(defsubst phpinspect-edtrack-clear (track)
  (setf (phpinspect-edtrack-edits track) nil)
  (setf (phpinspect-edtrack-last-edit track) nil)
  (setf (phpinspect-edtrack-last-edit-start track) -1)
  (phpinspect-edtrack-clear-taint-pool track))


(defsubst phpinspect-edtrack-make-taint-iterator (track)
  (cons (car (phpinspect-edtrack-taint-pool track))
        (cl-copy-list (cdr (phpinspect-edtrack-taint-pool track)))))

(define-inline phpinspect-taint-iterator-current (iter)
  (inline-quote (car ,iter)))

(define-inline phpinspect-taint-iterator-follow (iter pos)
  (inline-letevals (iter pos)
    (inline-quote
     (or (while (and (phpinspect-taint-iterator-current ,iter)
                     (> ,pos (phpinspect-taint-end
                              (phpinspect-taint-iterator-current ,iter))))
           (setf (phpinspect-taint-iterator-current ,iter) (pop (cdr ,iter))))
         (phpinspect-taint-iterator-current ,iter)))))

(define-inline phpinspect-taint-iterator-token-is-tainted-p (iter meta)
  (inline-letevals (iter meta)
    (inline-quote
     (or (and (phpinspect-taint-iterator-follow ,iter (phpinspect-meta-start ,meta))
              (phpinspect-taint-overlaps-meta
               (phpinspect-taint-iterator-current ,iter) ,meta))
	 ;; Incomplete tokens should be regarded as tainted regardless of
	 ;; region, as their end-position is unlikely to be correct.
	 (phpinspect-incomplete-token-p (phpinspect-meta-token ,meta))))))

(define-inline phpinspect-taint-iterator-region-is-tainted-p (iter start end)
  (inline-letevals (iter start end)
    (inline-quote
     (and (phpinspect-taint-iterator-follow ,iter ,start)
          (phpinspect-taint-overlaps-region
           (phpinspect-taint-iterator-current ,iter) ,start ,end)))))

(provide 'phpinspect-edtrack)
;;; phpinspect-edtrack.el ends here
