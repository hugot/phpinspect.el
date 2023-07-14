;;; phpinspect-bmap.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(cl-defstruct (phpinspect-bmap (:constructor phpinspect-make-bmap))
  (starts (make-hash-table :test #'eql
                           :size (floor (/ (point-max) 4))
                           :rehash-size 1.5))
  (ends (make-hash-table :test #'eql
                           :size (floor (/ (point-max) 4))
                           :rehash-size 1.5))
  (meta (make-hash-table :test #'eq
                           :size (floor (/ (point-max) 4))
                           :rehash-size 1.5))
  (overlays nil
            :type list)
  (current-row nil
               :type list)
  (current-row-start nil
                     :type integer)
  (current-row-end nil
                   :type integer)
  (whitespace-before ""
                     :type string))

(defsubst phpinspect--make-meta (parent start end whitespace-before token &optional overlayed)
  (list parent start end whitespace-before token overlayed))

(defsubst phpinspect--meta-parent (meta)
  (car meta))

(gv-define-setter phpinspect--meta-end (end meta) `(setcar (cddr ,meta) ,end))
(gv-define-setter phpinspect--meta-start (start meta) `(setcar (cdr ,meta) ,start))
(gv-define-setter phpinspect--meta-overlayed (overlayed meta) `(setcar (nthcdr 5 ,meta) ,overlayed))

(defsubst phpinspect--meta-overlayed-p (meta overlay)
  (eq (phpinspect--meta-overlayed meta) overlay))

(defsubst phpinspect--meta-overlayed (meta)
  (car (nthcdr 5 meta)))

(defsubst phpinspect--meta-token (meta)
  (car (cddddr meta)))

(defsubst phpinspect--meta-end (meta)
  (caddr meta))

(defsubst phpinspect--meta-start (meta)
  (cadr meta))

(defsubst phpinspect--meta-overlaps-point (meta point)
  (and (> (phpinspect--meta-end meta) point)
       (<= (phpinspect--meta-start meta) point)))

(defsubst phpinspect-bmap-register-whitespace (bmap whitespace)
  (setf (phpinspect-bmap-whitespace-before bmap) whitespace))

(defsubst phpinspect-bmap-register (bmap start end token)
  (let* ((starts (phpinspect-bmap-starts bmap))
         (ends (phpinspect-bmap-ends bmap))
         (meta (phpinspect-bmap-meta bmap))
         (current-row (phpinspect-bmap-current-row bmap))
         (current-row-start (phpinspect-bmap-current-row-start bmap))
         (current-row-end (phpinspect-bmap-current-row-end bmap))
         (existing-end (gethash end ends))
         (whitespace-before (phpinspect-bmap-whitespace-before bmap))
         (token-meta (phpinspect--make-meta nil start end whitespace-before token)))
    (setf (phpinspect-bmap-whitespace-before bmap) "")

    (puthash start token-meta starts)

    (if existing-end
        (push token existing-end)
      (puthash end (list token-meta) ends))

    (puthash token token-meta meta)

    (cond ((not current-row-start)
           (setf (phpinspect-bmap-current-row-start bmap) start)
           (setf (phpinspect-bmap-current-row-end bmap) end))
          ((and (>= end current-row-end)
                (<= start current-row-start))
           (dolist (child current-row)
             ;; Set parent
             (setcar child token-meta))
           (setf (phpinspect-bmap-current-row-start bmap) nil)
           (setf (phpinspect-bmap-current-row bmap) nil))
          ((> end current-row-end)
           (setf (phpinspect-bmap-current-row-end bmap) end)))

    (push token-meta (phpinspect-bmap-current-row bmap))))

(defsubst phpinspect-overlay-p (overlay)
  (eq 'overlay (car overlay)))

(defsubst phpinspect-overlay-wrap-meta (overlay meta)
  (when meta
    (setq meta (cl-copy-list meta))
    (setf (phpinspect--meta-start meta)
          (+ (phpinspect--meta-start meta) (phpinspect-overlay-delta overlay)))
    (setf (phpinspect--meta-end meta)
          (+ (phpinspect--meta-end meta) (phpinspect-overlay-delta overlay)))
    (setf (phpinspect--meta-overlayed meta) overlay)

    meta))

(cl-defmethod phpinspect-bmap-token-starting-at ((overlay (head overlay)) point)
  (phpinspect-overlay-wrap-meta
   overlay
   (phpinspect-bmap-token-starting-at
    (phpinspect-overlay-bmap overlay) (- point (phpinspect-overlay-delta overlay)))))

(cl-defmethod phpinspect-bmap-token-starting-at ((bmap phpinspect-bmap) point)
  (let ((overlay (phpinspect-bmap-overlay-at-point bmap point)))
    (if overlay
          (phpinspect-bmap-token-starting-at overlay point)
      (gethash point (phpinspect-bmap-starts bmap)))))

(defsubst phpinspect-bmap-overlay-at-point (bmap point)
  (catch 'found
    (dolist (overlay (phpinspect-bmap-overlays bmap))
      (when (phpinspect-overlay-overlaps-point overlay point)
        (throw 'found overlay)))))

;; (cl-defmethod phpinspect-bmap-tokens-overlapping ((bmap phpinspect-bmap) point)
;;   (

(cl-defmethod phpinspect-bmap-token-meta ((overlay (head overlay)) token)
  (phpinspect-bmap-token-meta (phpinspect-overlay-bmap overlay) token))

(cl-defmethod phpinspect-bmap-token-meta ((bmap phpinspect-bmap) token)
  (or (gethash token (phpinspect-bmap-meta bmap))
      (let ((found?))
        (catch 'found
          (dolist (overlay (phpinspect-bmap-overlays bmap))
            (when (setq found? (phpinspect-bmap-token-meta overlay token))
              (throw 'found found?)))))))

(defsubst phpinspect-probably-token-p (token)
  (and (listp token)
       (symbolp (car token))))

(defsubst phpinspect-overlay-overlaps-point (overlay point)
  (and (> (phpinspect-overlay-end overlay) point)
       (<= (phpinspect-overlay-start overlay) point)))

(defsubst phpinspect-overlay-bmap (overlay)
  (car (nthcdr 4 overlay)))

(defsubst phpinspect-overlay-delta (overlay)
  (cadddr overlay))

(defsubst phpinspect-overlay-start (overlay)
  (cadr overlay))

(defsubst phpinspect-overlay-end (overlay)
  (caddr overlay))

(defsubst phpinspect-bmap-overlay (bmap bmap-overlay token-meta pos-delta)
  (let* ((overlays (phpinspect-bmap-overlays bmap))
         (start (+ (phpinspect--meta-start token-meta) pos-delta))
         (end (+ (phpinspect--meta-end token-meta) pos-delta))
         (overlay `(overlay ,start ,end ,pos-delta ,bmap-overlay))
         (before))
    (phpinspect-bmap-register bmap start end (phpinspect--meta-token token-meta))

    (if overlays
        (progn
          (catch 'break
            (while (setq before (car overlays))
              (if (> (phpinspect-overlay-start overlay) (phpinspect-overlay-end before))
                  (throw 'break nil)
                (setq overlays (cdr overlays)))))

          (if before
              ;; Append after
              (setcdr overlays (cons overlay (cdr overlays)))
            ;; Append at end of overlay list
            (setcdr (last (phpinspect-bmap-overlays bmap)) overlay)))

      (push overlay (phpinspect-bmap-overlays bmap)))))

(provide 'phpinspect-bmap)
;;; phpinspect-bmap.el ends here
