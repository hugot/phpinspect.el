;;; phpinspect-pipeline.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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
(require 'phpinspect-queue)
(require 'phpinspect-util)

(define-error 'phpinspect-pipeline-incoming "Signal for incoming pipeline data")
(define-error 'phpinspect-pipeline-error "Signal for pipeline errors")

(defcustom phpinspect-pipeline-pause-time 0.5
  "Number of seconds to pause a pipeline thread when emacs receives
user input. This is similar to `phpinspect-worker-pause-time',
but pipelines are meant to run in bursts. For that reason, the
default pause time for pipelines is lower to be a little more
aggressive in hogging cpu time.

Set this variable to a higher value if you experience a lot of
jitter when editing during pipeline operations.  At the time of
writing, pipelines are used to refresh the project
index/autoloader and for the indexation of \"include\"
directories."
  :type 'number
  :group 'phpinspect)

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
    (setf (phpinspect-pipeline-thread-end thread) end)))

(cl-defmethod phpinspect-pipeline-ctx-close ((ctx phpinspect-pipeline-ctx))
  (let (errors err end thread-live)
    (dolist (thread (phpinspect-pipeline-ctx-threads ctx))
      (setq end (phpinspect-pipeline-thread-end (cdr thread))
            err (or  (thread-last-error (car thread))
                     (and end (phpinspect-pipeline-end-error end)))
            thread-live (thread-live-p (car thread)))

      (when thread-live
        (if end
            (setq errors (nconc errors (list (format "Thread %s ended pipeline, but is still running"
                                                     (thread-name (car thread))))))
          (setq errors (nconc errors (list (format "Thread %s is still running when pipeline is closing"
                                                   (thread-name (car thread))))))))

      (when err
        (setq errors (nconc errors (list (format "Thread %s signaled error: %s"
                                                 (thread-name (car thread))
                                                 err)))))
      (unless end
        (setq errors (nconc errors (list (format "Thread %s never ended"
                                                 (thread-name (car thread)))))))

      (when (thread-live-p (car thread))
        (thread-signal (car thread) 'quit nil)))

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
  (inline-quote
   (if (phpinspect--input-pending-p)
       (let ((mx (make-mutex)))
         (phpinspect-thread-pause
          phpinspect-pipeline-pause-time mx (make-condition-variable mx "phpinspect-pipeline-pause")))
     (thread-yield))))

(define-inline phpinspect--read-pipeline-emission (&rest body)
  (push 'progn body)
  (inline-quote
   (catch 'phpinspect-pipeline-emit
     ,body
     nil)))

(defmacro phpinspect--run-as-pipeline-step (func-name queue consumer-queue pipeline-ctx &optional local-ctx)
  (unless (symbolp func-name)
    (error "Function name must be a symbol, got: %s" func-name))


  (let* ((thread-name (concat "phpinspect-pipeline-" (symbol-name func-name)))
         (statement (list func-name))
         (statement-rear statement)
         (incoming (gensym "incoming"))
         (outgoing (gensym "outgoing"))
         (inc-queue (gensym "queue"))
         (out-queue (gensym "queue"))
         (context-sym (gensym "context"))
         (continue-running (gensym "continue-running"))
         (pctx-sym (gensym "pipeline-ctx"))
         (incoming-end (gensym "incoming-end"))
         (end (gensym "end")))

      (when local-ctx
        (setq statement-rear (setcdr statement-rear (cons context-sym nil))))

      (setq statement-rear (setcdr statement-rear (cons incoming nil)))

      `(let ((,inc-queue ,queue)
             (,out-queue ,consumer-queue)
             (,context-sym ,local-ctx)
             (,pctx-sym ,pipeline-ctx))
         (make-thread
          (lambda ()
            (let ((,continue-running t)
                  ,incoming ,outgoing ,end ,incoming-end)

              (phpinspect-pipeline--register-wakeup-function ,inc-queue)
              (while ,continue-running
                (condition-case err
                    (progn
                      (phpinspect-pipeline-pause)
                      ;; Prevent quitting during step execution, as this could
                      ;; break data integrity.
                      (let ((inhibit-quit t))
                        (setq ,incoming (phpinspect-pipeline-receive ,inc-queue))

                        (if (phpinspect-pipeline-end-p ,incoming)
                            (progn
                              (setq ,incoming-end ,incoming)
                              (when (phpinspect-pipeline-end-value ,incoming)
                                (progn
                                  (setq ,incoming (phpinspect-pipeline-end-value ,incoming)
                                        ,outgoing (phpinspect--read-pipeline-emission ,statement))
                                  (phpinspect-pipeline--enqueue ,out-queue ,outgoing 'no-notify)))

                              (setq ,end (phpinspect-make-pipeline-end :thread (current-thread)))
                              (phpinspect-pipeline-ctx-register-end ,pctx-sym ,end)
                              (setq ,continue-running nil)
                              (phpinspect-pipeline--enqueue ,out-queue ,end))

                          ;; Else
                          (setq ,outgoing (phpinspect--read-pipeline-emission ,statement))
                          (when (phpinspect-pipeline-end-p ,outgoing)
                            (setq ,end (phpinspect-make-pipeline-end :thread (current-thread)))
                            (phpinspect-pipeline-ctx-register-end ,pctx-sym ,end)
                            (setq ,continue-running nil))
                          (phpinspect-pipeline--enqueue ,out-queue ,outgoing))))
                  (quit (ignore-error phpinspect-pipeline-incoming
                          (phpinspect-pipeline-pause)))
                  (phpinspect-pipeline-incoming)
                  (t (phpinspect-message "Pipeline thread errored: %s" err)
                     (setq ,end (phpinspect-make-pipeline-end :thread (current-thread) :error err))
                     (setq ,continue-running nil)
                     (phpinspect-pipeline-ctx-register-end ,pctx-sym ,end)
                     (phpinspect-pipeline--enqueue ,out-queue ,end))))))
          ,thread-name))))


(defun phpinspect--chain-pipeline-steps (steps start-queue end-queue ctx)
  (let ((result (gensym "result"))
        (incoming (gensym "incoming"))
        (outgoing (gensym "outgoing"))
        (ctx-sym (gensym "ctx"))
        body name step statement)
    (while (setq step (pop steps))
      (setq name (phpinspect--pipeline-step-name step))

      (setq statement
            (if (phpinspect--pipeline-step--context-var-name step)
                `(phpinspect--run-as-pipeline-step
                  ,name ,incoming ,outgoing ,ctx-sym ,(phpinspect--pipeline-step--context-var-name step))
              `(phpinspect--run-as-pipeline-step ,name ,incoming ,outgoing ,ctx-sym)))
      (setq body (nconc body `(,(if steps
                                    `(setq ,outgoing (phpinspect-make-queue))
                                  `(setq ,outgoing ,end-queue))
                               (phpinspect-pipeline-ctx-register-thread ,ctx-sym ,statement ,incoming)
                               (setq ,incoming ,outgoing)))))

    `(let ((,incoming ,start-queue) (,ctx-sym ,ctx) ,result ,outgoing)
       ,@body)))

(cl-defstruct (phpinspect--pipeline-step (:constructor phpinspect--make-pipeline-step))
  (context nil
           :type any
           :documentation
           "An object that is passed as first argument to all step executions")
  (-context-var-name nil
                     :type symbol
                     :documentation
                     "Variable name used to store context in")
  (name nil
        :type symbol
        :documentation
        "The name of this step"))

(defmacro phpinspect--pipeline (seed-form &rest parameters)
  (let (key value steps let-vars)

    (while parameters
      (setq key (pop parameters)
            value (pop parameters))

      (pcase key
        (:into
         (let* ((construct-params (cons nil nil))
                (cons-params-rear construct-params)
                parameters name)

           (if (listp value)
               (progn
                 (setq name (car value)
                       parameters (cdr value)))
             (setq name value))

           (unless (symbolp name)
             (error "Step name should be a symbol"))

           (let (key value)
             (while parameters
               (setq key (pop parameters)
                     value (pop parameters))
               (setq key (intern (string-replace ":with-" ":" (symbol-name key))))
               (setq cons-params-rear
                     (setcdr cons-params-rear (cons key (cons value nil))))))
           (push (apply #'phpinspect--make-pipeline-step `(,@(cdr construct-params) :name ,name))
                 steps)))
        (_ (error "unexpected key %s" key))))

    (setq steps (nreverse steps))

    (dolist (step steps)
      (when (phpinspect--pipeline-step-context step)
        (setf (phpinspect--pipeline-step--context-var-name step) (gensym "ctx"))
        (push `(,(phpinspect--pipeline-step--context-var-name step)
                ,(phpinspect--pipeline-step-context step))
              let-vars)))

    (let ((queue-sym (gensym "queue"))
          (end-queue-sym (gensym "end-queue"))
          (ctx-sym (gensym "ctx"))
          (recv-sym (gensym))
          (result-sym (gensym))
          (seed-sym (gensym))
          (collecting-sym (gensym)))
      `(progn
         (when (eq main-thread (current-thread))
           (error "Pipelines should not run in the main thread"))

         (let* (,@let-vars
                (,ctx-sym (phpinspect-make-pipeline-ctx))
                (,queue-sym (phpinspect-make-queue))
                (,end-queue-sym (phpinspect-make-queue))
                (,collecting-sym t)
                ,recv-sym ,result-sym ,seed-sym)

           ,(phpinspect--chain-pipeline-steps steps queue-sym end-queue-sym ctx-sym)

           (setq ,seed-sym ,seed-form)
           (when ,seed-sym
             (phpinspect-pipeline--enqueue
              ,queue-sym
              (phpinspect-make-pipeline-emission :collection ,seed-sym) 'no-notify))

           (phpinspect-pipeline--enqueue
            ,queue-sym (phpinspect-make-pipeline-end :thread (current-thread)))

           (while ,collecting-sym
             (ignore-error phpinspect-pipeline-incoming
               (progn
                 (phpinspect-pipeline--register-wakeup-function ,end-queue-sym)
                 (while (not (phpinspect-pipeline-end-p
                              (setq ,recv-sym (phpinspect-pipeline-receive ,end-queue-sym))))
                   (setq ,result-sym (nconc ,result-sym (list ,recv-sym))))
                 (setq ,collecting-sym nil))))

           (phpinspect-pipeline-ctx-close ,ctx-sym)
           ,result-sym)))))

(defmacro phpinspect-pipeline (seed-form &rest parameters)
  (declare (indent defun))
  (let ((result (gensym))
        (async-sym (gensym))
        key value async macro-params)
    (while parameters
      (setq key (pop parameters)
            value (pop parameters))

      (pcase key
        (:async (setq async value))
        (_ (setq macro-params (nconc macro-params (list key value))))))

    `(if-let ((,async-sym ,async))
         (make-thread
          (lambda ()
            (condition-case err
                (let ((,result (phpinspect--pipeline ,seed-form ,@macro-params)))
                  (funcall ,async-sym (or ,result 'phpinspect-pipeline-nil-result) nil))
              (t (funcall ,async-sym nil err))))
          "phpinspect-pipeline-async")
       (phpinspect--pipeline ,seed-form ,@macro-params))))

(define-inline phpinspect-pipeline-receive (queue)
  (inline-letevals (queue)
    (inline-quote
     (let ((val))
       (while (not (setq val (phpinspect-queue-dequeue ,queue)))
         (thread-yield))
       val))))

(defun phpinspect-pipeline-step-name (name &optional suffix)
  (intern (concat (symbol-name name) (if suffix (concat "-" suffix) ""))))

(define-inline phpinspect-pipeline--register-wakeup-function (queue)
  (inline-quote
   (let ((thread (current-thread)))
     (setf (phpinspect-queue-subscription ,queue)
           (lambda () (thread-signal thread 'phpinspect-pipeline-incoming nil))))))

(define-inline phpinspect-pipeline--enqueue (queue emission &optional no-notify)
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
         (phpinspect-queue-enqueue ,queue ,emission ,no-notify))))))

(provide 'phpinspect-pipeline)
;;; phpinspect-pipeline.el ends here
