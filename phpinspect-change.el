;;; phpinspect-change.el --- A structure that represents buffer changes  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Hugo Thunnissen

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: languages

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

;;; Code:

(require 'phpinspect-bmap)

(cl-defstruct (phpinspect-change (:constructor phpinspect-make-change)
                                 (:conc-name phpi-change-))
  (synced-p t :type boolean)
  (start nil :type integer)
  (end nil :type integer)
  (prev-length nil :type integer)
  (content nil :type string))

(defun phpi-change-apply (change buffer)
  (let ((start (phpi-change-start change))
        (pre-change-length (phpi-change-prev-length change))
        (content (phpi-change-content change)))

    (with-current-buffer buffer
      (delete-region start (+ start pre-change-length))
      (goto-char start)
      (when content
        (insert content)))))

(defun phpi-change-create (buffer start end pre-change-length)
  (with-current-buffer buffer
    (let (content)
      (when (not (= start end))
        (setq content (buffer-substring-no-properties start end)))

      (phpinspect-make-change
       :start start
       :end end
       :prev-length pre-change-length
       :content content))))

(defun phpi-change-prev-end (change)
  (+ (phpi-change-start change) (phpi-change-prev-length change)))

(defun phpi-change-cur-length (change)
  (- (phpi-change-end change) (phpi-change-start change)))

(defun phpi-change-delta (change)
  (- (phpi-change-cur-length change) (phpi-change-prev-length change)))

(defun phpi-calculate-point (delta prev-end point)
  ;;(message "delta %s, prev-end %s, point %s" delta prev-end point)
  (let ((delta-region (if (> 0 delta)
                          (phpinspect-make-region (+ prev-end delta) prev-end)
                        (phpinspect-make-region prev-end (+ prev-end delta)))))
    ;;(message "Delta region: %s " delta-region)
    (if (> 0 delta)
        (if (> prev-end point)
            (if (phpinspect-region-overlaps-point delta-region point)
                (phpinspect-region-start delta-region)
              point)
          (if (< prev-end point)
              (+ point delta)
            point))
      (if (< 0 delta)
          (if (< prev-end point)
              (if (phpinspect-region-overlaps-point delta-region point)
                  (progn
                    (message "WTF OVERLAP")
                    (+ (phpinspect-region-end delta-region)
                       (- (phpinspect-region-end delta-region) point)))
                (message "+ pooint delta")
                (+ point delta))
            (message "NOPERS")
            point)
        (message "WAT")
        point))))

(defun phpi-change-post-position (change point)
  (phpi-calculate-point
   (phpi-change-delta change)
   (phpi-change-prev-end change)
   point))

(defun phpi-change-pre-position (change point)
  (if (<= (phpi-change-end change) point)
      (- point (phpi-change-delta change))
    (if (and (< 0 (phpi-change-delta change))
             (> (phpi-change-end change) point)
             (<= (phpi-change-prev-end change) point))
        (progn
          (phpi-change-prev-end change))
      point)))

(defun phpi-change-overlaps-point (change point)
  (and (> (phpi-change-end change) point)
       (<= (phpi-change-start change) point)))

(defun phpi-change-overlaps-pre-point (change point)
  (and (> (phpi-change-prev-end change) point)
       (<= (phpi-change-start change) point)))

(defun phpi-change-tainted-token-p (change meta)
  (or (phpi-change-overlaps-pre-point change (phpinspect-meta-start meta))
      (phpi-change-overlaps-pre-point change (phpinspect-meta-end meta))
      (phpinspect-meta-overlaps-point meta (phpi-change-start change))
      (phpinspect-meta-overlaps-point meta (phpi-change-prev-end change))))

(defun phpi-change-tainted-region-p (change start end)
  (or (phpi-change-overlaps-pre-point change start)
      (phpi-change-overlaps-pre-point change end)
      (and (> end (phpi-change-start change))
           (<= start (phpi-change-start change)))
      (and (> end (phpi-change-prev-end change))
           (<= start (phpi-change-prev-end change)))))

(provide 'phpinspect-change)
;;; phpinspect-change.el ends here
