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
  (token-stack nil
               :type list)
  (overlays nil
            :type list)
  (last-token-start nil
                     :type integer))

(defsubst phpinspect-make-region (start end)
  (list start end))

(defalias 'phpinspect-region-start #'car)
(defalias 'phpinspect-region-end #'cadr)

(defsubst phpinspect-region-size (region)
  (- (phpinspect-region-end region) (phpinspect-region-start region)))

(defsubst phpinspect-region> (reg1 reg2)
  (> (phpinspect-region-size reg1) (phpinspect-region-size reg2)))

(defsubst phpinspect-region< (reg1 reg2)
  (< (phpinspect-region-size reg1) (phpinspect-region-size reg2)))

(defsubst phpinspect-region-overlaps-point (reg point)
  (and (> (phpinspect-region-end reg) point)
       (<= (phpinspect-region-start reg) point)))

(defsubst phpinspect-region-overlaps (reg1 reg2)
  (or (phpinspect-region-reg2s-point reg1 (phpinspect-region-start reg2))
      (phpinspect-region-reg2s-point reg1 (- (phpinspect-region-end reg2) 1))
      (phpinspect-region-reg2s-point reg2 (phpinspect-region-start reg1))
      (phpinspect-region-reg2s-point reg2 (- (phpinspect-region-end reg1) 1))))

(defsubst phpinspect-region-encloses (reg1 reg2)
  (and (<= (phpinspect-region-start reg1) (phpinspect-region-start reg2))
       (>= (phpinspect-region-end reg1) (phpinspect-region-end reg2))))

(defsubst phpinspect-make-meta (parent start end whitespace-before token &optional overlay)
  (list 'meta parent start end whitespace-before token overlay))

(defsubst phpinspect-meta-parent (meta)
  (cadr meta))

(gv-define-setter phpinspect-meta-end (end meta) `(setcar (cdddr ,meta) ,end))
(gv-define-setter phpinspect-meta-start (start meta) `(setcar (cddr ,meta) ,start))
(gv-define-setter phpinspect-meta-overlay (overlay meta) `(setcar (nthcdr 6 ,meta) ,overlay))
(gv-define-setter phpinspect-meta-parent (parent meta) `(setcar (cdr ,meta) ,parent))

(defsubst phpinspect-meta-overlay (meta)
  (car (nthcdr 6 meta)))

(defsubst phpinspect-meta-token (meta)
  (car (nthcdr 5 meta)))

(defsubst phpinspect-meta-end (meta)
  (cadddr meta))

(defsubst phpinspect-meta-width (meta)
  (- (phpinspect-meta-end meta) (phpinspect-meta-start meta)))

(defun phpinspect-meta-sort-width (meta1 meta2)
  (< (phpinspect-meta-width meta1) (phpinspect-meta-width meta2)))

(defsubst phpinspect-meta-start (meta)
  (caddr meta))

(defsubst phpinspect-meta-overlaps-point (meta point)
  (and (> (phpinspect-meta-end meta) point)
       (<= (phpinspect-meta-start meta) point)))

(defsubst phpinspect-meta-find-parent-matching-token (meta predicate)
  (if (funcall predicate (phpinspect-meta-token meta))
      meta
    (catch 'found
      (while (phpinspect-meta-parent meta)
        (setq meta (phpinspect-meta-parent meta))
        (when (funcall predicate (phpinspect-meta-token meta))
          (throw 'found meta))))))

(gv-define-setter phpinspect-overlay-end (end overlay) `(setcar (cddr ,overlay) ,end))
(gv-define-setter phpinspect-overlay-start (start overlay) `(setcar (cdr ,overlay) ,start))
(gv-define-setter phpinspect-overlay-delta (delta overlay) `(setcar (cdddr ,overlay) ,delta))

(defsubst phpinspect-overlay-bmap (overlay)
  (car (nthcdr 4 overlay)))

(defsubst phpinspect-overlay-delta (overlay)
  (cadddr overlay))

(defsubst phpinspect-overlay-start (overlay)
  (cadr overlay))

(defsubst phpinspect-overlay-end (overlay)
  (caddr overlay))

(defsubst phpinspect-overlay-token-meta (overlay)
  (car (nthcdr 5 overlay)))

(defsubst phpinspect-overlay-overlaps-point (overlay point)
  (and (> (phpinspect-overlay-end overlay) point)
       (<= (phpinspect-overlay-start overlay) point)))

(defmacro phpinspect-bmap-iterate-region (region place-and-bmap &rest body)
  (declare (indent defun))
  (let ((place (car place-and-bmap))
        (bmap (gensym))
        (bmap-stack (gensym))
        (region-start (gensym))
        (region-end (gensym)))
    `(let ((,bmap)
           (,bmap-stack (list ,(cadr place-and-bmap)))
           (,region-start (car ,region))
           (,region-end (cadr ,region)))
       (while (setq ,bmap (pop ,bmap-stack))
         (phpinspect-bmap-iterate (,place ,bmap)
           (when (and (<= ,region-start
                          (phpinspect-meta-start ,place))
                      (>= ,region-end
                          (phpinspect-meta-end ,place)))
             ,@body))))))

(defmacro phpinspect-bmap-iterate (place-and-bmap &rest body)
  (declare (indent defun))
  (let ((place (car place-and-bmap))
        (bmap (gensym))
        (bmap-stack (gensym))
        (_ignored (gensym))
        (overlay-start (gensym))
        (overlay-end (gensym)))
    `(let ((,bmap-stack (list ,(cadr place-and-bmap)))
           (,bmap))
       (while (setq ,bmap (pop ,bmap-stack))
         (if (phpinspect-overlay-p ,bmap)
             (let ((,overlay-start (phpinspect-overlay-start ,bmap))
                   (,overlay-end (phpinspect-overlay-end ,bmap)))
               (maphash (lambda (,_ignored ,place)
                          (setq ,place (phpinspect-overlay-wrap-meta ,bmap ,place))
                          (when (and (<= ,overlay-start
                                         (phpinspect-meta-start ,place))
                                     (>= ,overlay-end
                                         (phpinspect-meta-end ,place)))
                            (if (phpinspect-meta-overlay ,place)
                              (push (phpinspect-meta-overlay ,place) ,bmap-stack)
                              ,@body)))
                        (phpinspect-bmap-meta (phpinspect-overlay-bmap ,bmap))))
           (maphash (lambda (,_ignored ,place)
                      (if (phpinspect-meta-overlay ,place)
                          (push (phpinspect-meta-overlay ,place) ,bmap-stack)
                        ,@body))
                    (phpinspect-bmap-meta ,bmap)))))))

(defsubst phpinspect-bmap-register (bmap start end token &optional whitespace-before overlay)
  (let* ((starts (phpinspect-bmap-starts bmap))
         (ends (phpinspect-bmap-ends bmap))
         (meta (phpinspect-bmap-meta bmap))
         (last-token-start (phpinspect-bmap-last-token-start bmap))
         (existing-end (gethash end ends))
         (token-meta (phpinspect-make-meta nil start end whitespace-before token overlay)))
    (unless whitespace-before
      (setq whitespace-before ""))

    (puthash start token-meta starts)

    (if existing-end
        (push token existing-end)
      (puthash end (list token-meta) ends))

    (puthash token token-meta meta)

    (when (and last-token-start
               (<= start last-token-start))
      (let ((child)
            (stack (phpinspect-bmap-token-stack bmap)))

        (while (and (car stack) (>= (phpinspect-meta-start (car stack))
                                    start))
          (setq child (pop stack))
          (setf (phpinspect-meta-parent child) token-meta)
          (when (phpinspect-meta-overlay child)
            (setf (phpinspect-meta-parent
                   (phpinspect-overlay-token-meta
                    (phpinspect-meta-overlay child)))
                  token-meta)))

        (setf (phpinspect-bmap-token-stack bmap) stack)))

    (setf (phpinspect-bmap-last-token-start bmap) start)
    (push token-meta (phpinspect-bmap-token-stack bmap))))

(defsubst phpinspect-overlay-p (overlay)
  (and (listp overlay)
       (eq 'overlay (car overlay))))

(defsubst phpinspect-overlay-wrap-meta (overlay meta)
  (when meta
    (setq meta (cl-copy-list meta))
    (setf (phpinspect-meta-start meta)
          (+ (phpinspect-meta-start meta) (phpinspect-overlay-delta overlay)))
    (setf (phpinspect-meta-end meta)
          (+ (phpinspect-meta-end meta) (phpinspect-overlay-delta overlay)))

    (when (phpinspect-meta-overlay meta)
      (let ((meta-overlay (cl-copy-list (phpinspect-meta-overlay meta))))
        (setf (phpinspect-overlay-start meta-overlay)
              (+ (phpinspect-overlay-start meta-overlay)
                 (phpinspect-overlay-delta overlay)))
        (setf (phpinspect-overlay-end meta-overlay)
              (+ (phpinspect-overlay-end meta-overlay)
                 (phpinspect-overlay-delta overlay)))
        (setf (phpinspect-overlay-delta meta-overlay)
              (+ (phpinspect-overlay-delta meta-overlay)
                 (phpinspect-overlay-delta overlay)))
        (setf (phpinspect-meta-overlay meta) meta-overlay)))

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

(cl-defmethod phpinspect-bmap-tokens-ending-at ((overlay (head overlay)) point)
  (mapcar (lambda (meta) (phpinspect-overlay-wrap-meta overlay meta))
          (phpinspect-bmap-tokens-ending-at
           (phpinspect-overlay-bmap overlay) (- point (phpinspect-overlay-delta overlay)))))

(cl-defmethod phpinspect-bmap-tokens-ending-at ((bmap phpinspect-bmap) point)
  (let ((overlay (phpinspect-bmap-overlay-at-point bmap point)))
    (if overlay
          (phpinspect-bmap-tokens-ending-at overlay point)
      (gethash point (phpinspect-bmap-ends bmap)))))

(defsubst phpinspect-bmap-overlay-at-point (bmap point)
  (catch 'found
    (dolist (overlay (phpinspect-bmap-overlays bmap))
      (when (phpinspect-overlay-overlaps-point overlay point)
        (throw 'found overlay)))))

(defsubst phpinspect-bmap-tokens-overlapping (bmap point)
  (let ((tokens))
    (phpinspect-bmap-iterate (meta bmap)
      (when (phpinspect-meta-overlaps-point meta point)
        (push meta tokens)))

    (sort tokens #'phpinspect-meta-sort-width)))

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

(defsubst phpinspect-bmap-last-token-before-point (bmap point)
  (let* ((ends (phpinspect-bmap-ends bmap))
         (ending))
    (unless (hash-table-empty-p ends)
      (while (not (or (<= point 0) (setq ending (phpinspect-bmap-tokens-ending-at bmap point))))
        (setq point (- point 1)))
      (car (last ending)))))

(defsubst phpinspect-bmap-overlay (bmap bmap-overlay token-meta pos-delta &optional whitespace-before)
  (let* ((overlays (phpinspect-bmap-overlays bmap))
         (start (+ (phpinspect-meta-start token-meta) pos-delta))
         (end (+ (phpinspect-meta-end token-meta) pos-delta))
         (overlay `(overlay ,start ,end ,pos-delta ,bmap-overlay ,token-meta))
         (before))
    (phpinspect-bmap-register bmap start end (phpinspect-meta-token token-meta) whitespace-before overlay)

    (if overlays
        (progn
          (catch 'break
            (while (setq before (car overlays))
              (if (> (phpinspect-overlay-start overlay) (phpinspect-overlay-end before))
                  (throw 'break nil)
                (setq overlays (cdr overlays)))))

          (if (and before (cdr overlays))
              ;; Append after
              (progn
                (setcdr overlays (cons overlay (cdr overlays))))
            ;; Append at end of overlay list
            (nconc (phpinspect-bmap-overlays bmap) (list overlay))))

      ;; No exising overlays, overwrite
      (push overlay (phpinspect-bmap-overlays bmap)))))

(defun phpinspect-bmap-make-location-resolver (bmap)
  (lambda (token)
    (let ((meta (phpinspect-bmap-token-meta bmap token)))
      (if meta
          (phpinspect-make-region (phpinspect-meta-start meta)
                                  (phpinspect-meta-end meta))
        (phpinspect-make-region 0 0)))))

(provide 'phpinspect-bmap)
;;; phpinspect-bmap.el ends here
