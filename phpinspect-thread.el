;;; phpinspect-thread.el --- Threading functions for phpinspect  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>

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

(require 'phpinspect-queue)
(require 'phpinspect-util)

(cl-defstruct (phpi-condition (:constructor phpi--make-condition))
  (-value nil)
  (-condvar nil :type condition-variable)
  (-mx nil :type mutex))

(gv-define-setter phpi-condition-value (val condition)
  `(phpi-condition--set-value ,condition ,val))

(define-error 'phpinspect-kill-thread
              "Thread killed")

(define-error 'phpinspect-job-queue-end
              "Job queue ended")

(defun phpi-condition-value (condition)
  (phpi-condition--value condition))

(defun phpi-condition-notify (condition)
  (with-mutex (phpi-condition--mx condition)
    (condition-notify (phpi-condition--condvar condition) t)))

(defun phpi-condition--set-value (condition value)
  (setf (phpi-condition--value condition) value)
  (phpi-condition-notify condition))

(defun phpi-condition-wait (condition &optional predicate)
  (let ((mx (phpi-condition--mx condition))
        (condvar (phpi-condition--condvar condition))
        result)

    (if predicate
        (while (not (funcall predicate (setq result (phpi-condition--value condition))))
          (with-mutex mx
            (condition-wait condvar)))

      (let ((start-val (phpi-condition--value condition)))
        (while (eq start-val (setq result (phpi-condition--value condition)))
          (with-mutex mx
            (condition-wait condvar)))))

    result))

(defun phpi-make-condition (&optional value name)
  (let* ((mx (make-mutex))
         (condvar (make-condition-variable mx name)))
    (phpi--make-condition :-mx mx :-condvar condvar :-value value)))

(defvar phpinspect--main-thread-starving (phpi-make-condition 'no))

(defun phpi-thread-kill (thread)
  (when (thread-live-p thread)
    (thread-signal thread 'phpinspect-kill-thread nil)))

(defmacro phpi-run-threaded (thread-name &rest body)
  (declare (indent 1))
  (let ((err-sym (gensym)))
  `(make-thread
    (lambda ()
      (condition-case ,err-sym
          (let ((inhibit-quit t))
            (progn ,@body))
        (phpinspect-kill-thread)
        (error
         (phpinspect-message
          "Thread [%s (exited)] encountered an error: %s"
          (thread-name (current-thread))
          ,err-sym))))

    ,thread-name)))

(define-inline phpi--main-thread-starving-p ()
  (inline-quote
   (if (or quit-flag (phpinspect--input-pending-p))
       'yes
     'no)))

(define-inline phpi-main-thread-starving-p ()
  (inline-quote
   (let ((starving (phpi--main-thread-starving-p)))
     (setf (phpi-condition-value phpinspect--main-thread-starving) starving)
     (if (eq 'yes starving)
         t
       nil))))

(defun phpi-job-queue-end ()
  (signal 'phpinspect-job-queue-end nil))

(define-inline phpi-thread-yield ()
  "Like `thread-yield', but takes extra care not to yield the main thread.

If current thread is the main thread, this function does nothing."
  (inline-quote
   (unless (eq main-thread (current-thread))
       (thread-yield))))

(defmacro phpi-progn (&rest body)
  `(prog1
       (progn ,@body)
     (phpi-thread-yield)))

(cl-defstruct (phpinspect-job-queue (:constructor phpi--make-job-queue)
                                    (:conc-name phpi-job-queue-)
                                    (:include phpinspect-queue))
  (thread nil :type thread))

(defun phpi-start-job-queue (name job-handler)
  (declare (indent 1))
  (let* ((condition (phpi-make-condition nil (format "%s condition" name)))
         queue)
    (setq queue (phpi--make-job-queue
                 :subscription
                 (lambda ()
                   (setf (phpi-condition-value condition)
                         (phpinspect-queue-first queue)))))

    (setf (phpi-job-queue-thread queue)
          (phpi-run-threaded (format "(job queue) %s" name)
            (let (ended
                  (inhibit-quit t))
              (catch 'phpi--break
                (while t
                  (if-let ((job (phpinspect-queue-dequeue queue)))
                      (phpi-progn
                       (condition-case nil
                           (funcall job-handler job)
                         (phpinspect-job-queue-end
                          ;; If job queue end is signaled, exit after queue has
                          ;; been fully depleted.
                          (setq ended t)
                          (unless (phpinspect-queue-first queue)
                            (throw 'phpi--break nil)))))
                    (if ended
                        ;; End was signaled previously and the queue is empty. Exit.
                        (throw 'phpi--break nil)
                      (progn
                        (setf (phpi-condition--value condition)
                              (phpinspect-queue-first queue))
                        (phpi-condition-wait condition #'identity)))))))))
    queue))

(defun phpi-job-queue-live-p (queue)
  (and (threadp (phpi-job-queue-thread queue))
       (thread-live-p (phpi-job-queue-thread queue))))

(defun phpi-job-queue-kill (queue)
  (when (phpi-job-queue-live-p queue)
    (phpi-thread-kill (phpi-job-queue-thread queue))))


(provide 'phpinspect-thread)
;;; phpinspect-thread.el ends here
