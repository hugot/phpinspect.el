;;; phpinspect-edtrack.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(cl-defstruct (phpinspect-edtrack (:constructor phpinspect-make-edtrack))
  (edits nil
         :type list)
  (taint-pool nil
              :type list))

(defsubst phpinspect-edtrack-make-taint-iterator (track)
  (cons (car (phpinspect-edtrack-taint-pool track))
        (cl-copy-list (cdr (phpinspect-edtrack-taint-pool track)))))

(defsubst phpinspect-taint-iterator-token-is-tainted-p (iter meta)
  (let ((current (car iter)))
    (when current
      (while (and current (> (phpinspect--meta-start meta) (phpinspect-taint-end current)))
        (setq current (pop (cdr iter))))

      (and current (phpinspect-taint-overlaps-meta current meta)))))

(defsubst phpinspect-edit-original-end (edit)
  (or (caar edit) 0))

(defsubst phpinspect-edit-end (edit)
  (if edit
      (let ((end (or (caar edit) 0))
            (delta 0)
            (previous-edit (cdr edit)))
        (+ end (phpinspect-edit-delta previous-edit)))))

(defsubst phpinspect-edit-delta (edit)
  (let ((delta (or (cdar edit) 0))
        (previous-edit edit))
    (while (setq previous-edit (cdr previous-edit))
      (setq delta (+ delta (cdar previous-edit))))
    delta))

(defsubst phpinspect-edtrack-original-position-at-point (track point)
  (let ((edit (phpinspect-edtrack-edits track)))
    (while (and edit (< point (phpinspect-edit-end edit)))
      (setq edit (cdr edit)))

    (- point (phpinspect-edit-delta edit))))

(defsubst phpinspect-edtrack-current-position-at-point (track point)
  (let ((edit (phpinspect-edtrack-edits track)))
    (while (and edit (< point (phpinspect-edit-original-end edit)))
      (setq edit (cdr edit)))

    (+ point (phpinspect-edit-delta edit))))

(defsubst phpinspect-edtrack-register-edit (track start end pre-change-length)
  (let ((edit (phpinspect-edtrack-edits track)))
    (while (and edit (< end (phpinspect-edit-end edit)))
      (setq edit (cdr edit)))

    (let* ((new-edit (cons (- (+ start pre-change-length) (phpinspect-edit-delta edit)) (- (- end start) pre-change-length))))
      (if edit
          (progn
            (setcdr edit (cons (car edit) (cdr edit)))
            (setcar edit new-edit))
        (if (phpinspect-edtrack-edits track)
            (push new-edit (cdr (last (phpinspect-edtrack-edits track))))
          (push new-edit (phpinspect-edtrack-edits track)))))))

(defsubst phpinspect-taint-start (taint)
  (car taint))

(defsubst phpinspect-taint-end (taint)
  (cdr taint))

(defsubst phpinspect-make-taint (start end)
  (cons start end))

(defsubst phpinspect-taint-overlaps-point (taint point)
  (and (> (phpinspect-taint-end taint) point)
       (<= (phpinspect-taint-start taint) point)))

(defsubst phpinspect-taint-overlaps (taint1 taint2)
  (or (phpinspect-taint-overlaps-point taint1 (phpinspect-taint-start taint2))
      (phpinspect-taint-overlaps-point taint1 (phpinspect-taint-end taint2))
      (phpinspect-taint-overlaps-point taint2 (phpinspect-taint-start taint1))
      (phpinspect-taint-overlaps-point taint2 (phpinspect-taint-end taint1))))

(defsubst phpinspect-taint-overlaps-meta (taint meta)
  (or (phpinspect-taint-overlaps-point taint (phpinspect--meta-start meta))
      (phpinspect-taint-overlaps-point taint (phpinspect--meta-end meta))
      (phpinspect--meta-overlaps-point meta (phpinspect-taint-start taint))
      (phpinspect--meta-overlaps-point meta (phpinspect-taint-end taint))))

(defsubst phpinspect-edtrack-clear-taint-pool (track)
  (setf (phpinspect-edtrack-taint-pool track) nil))

(defsubst phpinspect-edtrack-clear (track)
  (setf (phpinspect-edtrack-edits track) nil)
  (phpinspect-edtrack-clear-taint-pool track))

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

(provide 'phpinspect-edtrack)
;;; phpinspect-edtrack.el ends here
