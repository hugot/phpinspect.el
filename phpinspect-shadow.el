;;; phpinspect-shadow.el --- Shadow buffers for phpinspect buffers  -*- lexical-binding: t; -*-

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

;; (require 'cl-macs)

;; (defvar phpinspect--shadow-counter 0)

;; (defvar phpinspect-shadow-pause-time 0.05)

;; (define-error 'phpinspect-wakeup-shadow
;;   "This error is used to wakeup the shadow thread.")


;; (cl-defstruct (phpinspect-shadow (:constructor phpinspect-make-shadow-generated)
;;                                  (:conc-name phpi-shadow-))
;;   (origin nil :type phpinspect-buffer)
;;   (buffer nil :type buffer)
;;   (queue nil :type phpinspect--queue)
;;   (thread nil :type thread)
;;   (id nil :type integer))

;; (defun phpi-shadow-wakeup-thread (shadow)
;;   (thread-signal (phpi-shadow-thread shadow) 'phpi-wakeup-shadow nil))

;; (defun phpi-shadow-thread-check-pause ()
;;   (ignore-error phpinspect-wakeup-shadow
;;     (if (or (phpinspect--input-pending-p)
;;             quit-flag)
;;         (let* ((mx (make-mutex))
;;                (continue (make-condition-variable mx)))
;;           (phpinspect-thread-pause phpinspect-shadow-pause-time mx continue))
;;       (thread-yield))))

;; (defun phpi-shadow-make-queue-subscription (shadow)
;;   (lambda ()
;;     (setf (phpi-shadow-synced-p shadow) nil)
;;     (phpi-shadow-wakeup-thread shadow)))

;; (defun phpi-shadow--thread-make-parser-interrupt-predicate ()
;;   (lambda () (phpi-shadow-thread-check-pause) nil))

;; (defun phpi-shadow-process-change (shadow change)
;;   (with-current-buffer (phpi-shadow-buffer shadow)
;;     (phpi-change-apply change)

;;     (let ((buffer (phpi-shadow-origin shadow))
;;           (pctx (phpinspect-make-pctx
;;                  :incremental t
;;                  :previous-bmap (phpinspect-buffer-map buffer)
;;                  :bmap (phpinspect-make-bmap)
;;                  :change change
;;                  :interrupt-predicate (phpi-shadow--thread-make-parser-interrupt-predicate))))

;;       ;; Parse new content
;;       (with-current-buffer (phpi-shadow-buffer shadow)
;;         (phpinspect-with-parse-context pctx
;;           (setf (phpinspect-buffer-tree buffer) (phpinspect-parse-current-buffer))))

;;       (phpinspect-buffer--set-map buffer (phpinspect-pctx-bmap pctx)))))

;; (defun phpi-shadow-make-thread-function (shadow)
;;   (lambda ()
;;     (let ((inhibit-quit t))
;;       (while t
;;         (if-let ((task (phpinspect-queue-dequeue (phpi-shadow-queue shadow))))
;;             (progn
;;               (pcase task
;;                 ((pred phpinspect-change-p)
;;                  (phpi-shadow-process-change shadow task))
;;                 (_
;;                  (phpinspect-message
;;                   "Shadow thread received unsupported task type: %s"
;;                   (type-of task))))

;;               ;; Rest after task completion
;;               (phpi-shadow-thread-check-pause))

;;           ;; No work to do, join main thread
;;           (setf (phpi-shadow-synced-p shadow) t)
;;           (ignore-error 'phpinspect-wakeup-shadow
;;             (thread-join main-thread)))))))

;; (defun phpi-shadow-thead-live-p (shadow)
;;   (thread-live-p (phpi-shadow-thread shadow)))

;; (defun phpi-shadow-await-synced (shadow &optional allow-interrupt)
;;   (cl-assert (phpi-shadow-thread-live-p shadow))

;;   (while (not (phpi-shadow-synced-p shadow))
;;     (when (and allow-interupt (phpinspect--input-pending-p))
;;       (throw 'phpinspect-interrupted nil))

;;     (sleep-for 0.005)))

;; (defun phpi-shadow-make-thread (shadow)
;;   (make-thread
;;    (phpi-shadow-make-thread-function shadow)
;;    (format " **phpinspect-shadow-thread**<%d>" (phpi-shadow-id shadow))))

;; (defun phpinspect-make-shadow (origin)
;;   (let* ((id (cl-incf phpinspect--shadow-counter))
;;          (shadow (phpinspect-make-shadow-generated
;;                   :origin origin
;;                   :buffer (generate-new-buffer
;;                            (format " **phpinspect-shadow**<%d>" id))
;;                   :id id)))

;;     ;; Copy buffer contents
;;     (with-current-buffer (phpi-shadow-buffer shadow)
;;       (insert (phpinspect-with-current-buffer origin (buffer-string))))

;;     (setf (phpi-shadow-queue shadow)
;;           (phpinspect-make-queue (phpi-shadow-make-queue-subscription shadow))

;;           (phpi-shadow-thread shadow)
;;           (phpi-shadow-make-thread shadow))))

;; (defun phpi-shadow-register-change (shadow change)
;;   (phpinspect-queue-enqueue (phpi-shadow-queue  shadow) change))

;; (cl-defstruct (phpinspect-change (:constructor phpinspect-make-change)
;;                                  (:conc-name phpi-change-))
;;   (synced-p t :type boolean)
;;   (start nil :type integer)
;;   (end nil :type integer)
;;   (prev-length nil :type integer)
;;   (content nil :type string))

;; (defun phpi-change-apply (change buffer)
;;   (let ((start (phpi-change-start change))
;;         (end (phpi-change-end change))
;;         (pre-change-length (phpi-change-prev-length change))
;;         (content (phpi-change-content change)))

;;     (with-current-buffer buffer
;;       (delete-region start (+ start pre-change-length))
;;       (goto-char start)
;;       (when content
;;         (insert content)))))

;; (defun phpi-change-create (buffer start end pre-change-length)
;;   (with-current-buffer buffer
;;     (let (content)
;;       (when (not (= start end))
;;         (setq content (buffer-substring-no-properties start end)))

;;       (phpinspect-make-change
;;        :start start
;;        :end end
;;        :prev-length pre-change-length
;;        :content content))))

;; (defun phpi-change-prev-end (change)
;;   (+ (phpi-change-start change) (phpi-change-prev-length change)))

;; (defun phpi-change-cur-length (change)
;;   (- (phpi-change-end change) (phpi-change-start change)))

;; (defun phpi-change-delta (change)
;;   (- (phpi-change-prev-length change) (phpi-change-cur-length change)))

;; (defun phpi-change-post-position (change point)
;;   (if (> point (phpi-change-end change))
;;         (if (> (phpi-change-prev-end change) point)
;;             (phpi-change-end change)
;;           (- point (phpi-change-delta change)))
;;     point))

;; (defun phpi-change-pre-position (change point)
;;   (if (> point (phpi-change-end change))
;;       (+ (phpi-change-delta change) point)
;;     point))

;; (defun phpi-change-overlaps-point (change point)
;;   (and (> (phpi-change-end change) point)
;;        (<= (phpi-change-start change) point)))

;; (defun phpi-change-overlaps-pre-point (change point)
;;   (and (> (phpi-change-prev-end change) point)
;;        (<= (phpi-change-start change) point)))

;; (defun phpi-change-tainted-token-p (change meta)
;;   (or (phpi-change-overlaps-pre-point change (phpinspect-meta-start meta))
;;       (phpi-change-overlaps-pre-point change (phpinspect-meta-end meta))
;;       (phpinspect-meta-overlaps-point meta (phpi-change-start change))
;;       (phpinspect-meta-overlaps-point meta (phpi-change-prev-end change))))

(provide 'phpinspect-shadow)
;;; phpinspect-shadow.el ends here
