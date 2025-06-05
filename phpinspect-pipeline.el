;;; phpinspect-pipeline.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

;;; Code:
(require 'phpinspect-queue)
(require 'phpinspect-util)
(require 'phpinspect-thread)

(define-error 'phpinspect-pipeline-incoming "Signal for incoming pipeline data")
(define-error 'phpinspect-pipeline-error "Signal for pipeline errors")

(defun phpinspect-pipeline-error-p (obj)
  (eq 'phpinspect-pipeline-error (car-safe obj)))

(cl-defstruct (phpinspect-pipeline-end (:constructor phpinspect-make-pipeline-end))
  (value nil
         :type any)
  (error nil)
  (thread nil
          :type thread))

(cl-defstruct (phpinspect-pipeline-emission (:constructor phpinspect-make-pipeline-emission))
  (collection nil
              :type list))

(cl-defstruct (phpinspect-pipeline-thread (:constructor phpinspect-make-pipeline-thread))
  (in-queue nil
            :type phpinspect-queue)
  (end nil
         :type boolean))

(cl-defstruct (phpinspect-pipeline-ctx (:constructor phpinspect-make-pipeline-ctx))
  (threads nil
           :type alist))

(cl-defmethod phpinspect-pipeline-ctx-register-thread ((ctx phpinspect-pipeline-ctx) thread in-queue)
  (push (cons thread (phpinspect-make-pipeline-thread :in-queue in-queue))
        (phpinspect-pipeline-ctx-threads ctx)))

(cl-defmethod phpinspect-pipeline-ctx-get-thread ((ctx phpinspect-pipeline-ctx) thread)
  (alist-get thread (phpinspect-pipeline-ctx-threads ctx)
             nil nil #'eq))

(cl-defmethod phpinspect-pipeline-ctx-register-end ((ctx phpinspect-pipeline-ctx) (end phpinspect-pipeline-end))
  (let ((thread (phpinspect-pipeline-ctx-get-thread ctx (phpinspect-pipeline-end-thread end))))
    (unless (phpinspect-pipeline-thread-end thread)
      (setf (phpinspect-pipeline-thread-end thread) end))))

(cl-defmethod phpinspect-pipeline-ctx-close ((ctx phpinspect-pipeline-ctx))
  (let (errors err end thread-live)
    (dolist (thread (phpinspect-pipeline-ctx-threads ctx))
      (setq end (phpinspect-pipeline-thread-end (cdr thread))
            err (or  (thread-last-error (car thread))
                     (and end (phpinspect-pipeline-end-error end)))
            thread-live (thread-live-p (car thread)))

      (when thread-live
        (if end
            ;; Give thread cpu time to wrap up
            (thread-join (car thread))
          ;; Even if thread is still live, it should have signaled its end at
          ;; this point.
          (setq errors (nconc errors (list (format "Thread %s is still running when pipeline is closing"
                                                   (thread-name (car thread))))))))

      (when err
        (setq errors (nconc errors (list (format "Thread %s signaled error: %s"
                                                 (thread-name (car thread))
                                                 err)))))
      (unless end
        (setq errors (nconc errors (list (format "Thread %s never ended"
                                                 (thread-name (car thread))))))))

    (when errors
      (signal 'phpinspect-pipeline-error errors))))

(define-inline phpinspect-pipeline-emit (data)
  (inline-letevals (data)
    (inline-quote
     (throw 'phpinspect-pipeline-emit ,data))))

(define-inline phpinspect-pipeline-emit-all (collection)
  (inline-letevals (collection)
    (inline-quote
     (throw 'phpinspect-pipeline-emit
            (if ,collection
                (phpinspect-make-pipeline-emission
                 :collection ,collection)
              ,collection)))))

(defmacro phpinspect-pipeline-end (&optional value)
  (if value
      `(throw 'phpinspect-pipeline-emit
              (phpinspect-make-pipeline-end :value ,value :thread (current-thread)))
  `(throw 'phpinspect-pipeline-emit
          (phpinspect-make-pipeline-end :thread (current-thread)))))

(define-inline phpinspect-pipeline-pause ()
  "Pause the current pipeline thread"
  (inline-quote (phpi-thread-yield)))

(define-inline phpinspect--read-pipeline-emission (&rest body)
  (push 'progn body)
  (inline-quote
   (catch 'phpinspect-pipeline-emit
     ,body
     nil)))

(define-inline phpinspect--pipeline-enqueue (queue emission &optional no-notify)
  (inline-letevals (queue emission no-notify)
    (inline-quote
     (when ,emission
       (if (phpinspect-pipeline-emission-p ,emission)
           (when (phpinspect-pipeline-emission-collection ,emission)
             (while (cdr (phpinspect-pipeline-emission-collection ,emission))
               (phpinspect-queue-enqueue
                ,queue (pop (phpinspect-pipeline-emission-collection ,emission))
                ,no-notify))
             (phpinspect-queue-enqueue
              ,queue (pop (phpinspect-pipeline-emission-collection ,emission)) ,no-notify))
         (if (and (phpinspect-pipeline-end-p ,emission)
                  (phpinspect-pipeline-end-value ,emission))
             (progn
               (phpinspect-queue-enqueue ,queue (phpinspect-pipeline-end-value ,emission) ,no-notify)
               (phpinspect-queue-enqueue ,queue ,emission ,no-notify))
           (phpinspect-queue-enqueue ,queue ,emission ,no-notify)))))))

(defun phpinspect--pipeline-parse-step (step-arguments)
  (pcase-let ((`(,name ,step)
               (if (listp step-arguments)
                   (list (car step-arguments)
                         (apply #'phpinspect--make-pipeline-step
                                (append (cdr step-arguments)
                                        (list :name (car step-arguments)))))
                 (list step-arguments
                       (phpinspect--make-pipeline-step :name step-arguments)))))

    (unless (and (symbolp name) (fboundp name))
      (error "Pipeline step name must be a symbol bound to a function"))

    step))

(defun phpinspect--pipeline-parse-steps (arguments-plist &optional steps async)
  (if arguments-plist
      (let ((key (pop arguments-plist))
            (value (pop arguments-plist)))
        (pcase key
          (:into
           (phpinspect--pipeline-parse-steps
            arguments-plist
            (cons (phpinspect--pipeline-parse-step value) steps)
            async))
          (:async
           (phpinspect--pipeline-parse-steps
            arguments-plist steps value))
          (_ (error "Unexpected pipeline argument key: %s" key))))

    (list async steps)))

(defun phpinspect-pipeline-step-format-name (step)
  (format "PHPInspect pipeline thread [%s]"
          (symbol-name (phpinspect--pipeline-step-name step))))

(defun phpinspect--pipeline-fn-wrap-with-read-pipeline (fn step)
  (if (phpinspect--pipeline-step-with-auto-emit step)
      fn
    (lambda (task) (phpinspect--read-pipeline-emission (funcall fn task)))))

(defun phpinspect--pipeline-fn-wrap-with-end-handler (fn pipeline-ctx)
  (lambda (task)
    (let ((result (funcall fn task)))
      (when (phpinspect-pipeline-end-p result)
        (phpinspect-pipeline-ctx-register-end pipeline-ctx result))

      result)))

(defun phpinspect--pipeline-fn-wrap-with-error-handler (fn)
  (lambda (task)
    (condition-case err
        (funcall fn task)
      (t
       (phpinspect-make-pipeline-end
        :error err :thread (current-thread))))))

(defun phpinspect--pipeline-fn-wrap-with-ctx (fn ctx)
  (if ctx
      (lambda (task)
        (funcall fn ctx task))
    (lambda (task) (funcall fn task))))

(defun phpinspect--pipeline-fn-wrap-with-emitter (fn out-queue)
  (lambda (task)
    (let ((emission (if (phpinspect-pipeline-end-p task)
                        (phpinspect-make-pipeline-end :thread (current-thread))
                      (funcall fn task))))
      (phpinspect--pipeline-enqueue out-queue emission)
      emission)))

(defun phpinspect--pipeline-make-run-step-function (pipeline-ctx step out-queue)
  (let ((step-fn (thread-first
                   (phpinspect--pipeline-step-name step)
                   (phpinspect--pipeline-fn-wrap-with-ctx (phpinspect--pipeline-step-with-context step))
                   (phpinspect--pipeline-fn-wrap-with-read-pipeline step)
                   (phpinspect--pipeline-fn-wrap-with-error-handler)
                   (phpinspect--pipeline-fn-wrap-with-emitter out-queue)
                   (phpinspect--pipeline-fn-wrap-with-end-handler pipeline-ctx))))
    (lambda (task)
      (prog1 (funcall step-fn task)
        (when (phpinspect-pipeline-end-p task)
          (phpi-job-queue-end))))))

(defun phpinspect--pipeline-chain (ctx steps &optional out-queue)
  (if-let ((step (pop steps)))
      (let* ((job-queue (phpi-start-job-queue (phpinspect-pipeline-step-format-name step)
                          (phpinspect--pipeline-make-run-step-function ctx step out-queue))))
        (phpinspect-pipeline-ctx-register-thread ctx (phpi-job-queue-thread job-queue) job-queue)
        (phpinspect--pipeline-chain ctx steps job-queue))
    out-queue))

(defun phpinspect-pipeline (seed-form &rest arguments-plist)
  (declare (indent defun))
  (pcase-let ((`(,async ,steps) (phpinspect--pipeline-parse-steps arguments-plist))
              (ctx (phpinspect-make-pipeline-ctx)))
    (when seed-form
      (let* (results
             (in-queue (phpi-start-job-queue "Pipeline result accumulator"
                         (lambda (result)
                           (if (phpinspect-pipeline-end-p result)
                               (phpi-progn
                                (setq results (nreverse results))
                                (phpi-job-queue-end))
                             (push result results))))))

        (let ((out-queue (phpinspect--pipeline-chain ctx steps in-queue)))
          (phpinspect--pipeline-enqueue
           out-queue
           (phpinspect-make-pipeline-emission :collection seed-form) 'no-notify)

          (phpinspect--pipeline-enqueue
           out-queue (phpinspect-make-pipeline-end :thread (current-thread))))

        (if async
            (phpi-run-threaded "Pipeline result awaiter"
              (thread-join (phpi-job-queue-thread in-queue))

              (condition-case err
                  (progn
                    (phpinspect-pipeline-ctx-close ctx)
                    ;; async consumers may use the result being non-nil as a
                    ;; means to determine whether the pipeline has finished
                    ;; executing or not. So we return a symbol when the result
                    ;; is nil to prevent consumer threads from waiting
                    ;; endlessly.
                    (funcall async (or results 'phpinspect-pipeline-nil-result) nil))
                (t
                 (funcall async results err))))
          (progn
            (thread-join (phpi-job-queue-thread in-queue))
            (phpinspect-pipeline-ctx-close ctx)
            results))))))


(cl-defstruct (phpinspect--pipeline-step (:constructor phpinspect--make-pipeline-step))
  (with-context nil
           :type any
           :documentation
           "An object that is passed as first argument to all step executions")
  (with-auto-emit nil
                  :type boolean)
  (name nil
        :type symbol
        :documentation
        "The name of this step"))

(provide 'phpinspect-pipeline)
;;; phpinspect-pipeline.el ends here
