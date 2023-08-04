;;; phpinspect-pipeline.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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
(require 'phpinspect-worker)
(require 'phpinspect-queue)

(define-error 'phpinspect-pipeline-incoming "Signal for incoming pipeline data")
(define-error 'phpinspect-pipeline-error "Signal for pipeline errors")

(cl-defstruct (phpinspect-pipeline-end (:constructor phpinspect-make-pipeline-end))
  (value nil
         :type any)
  (thread nil
          :type thread))

(cl-defstruct (phpinspect-pipeline-thread (:constructor phpinspect-make-pipeline-thread))
  (in-queue nil
            :type phpinspect-queue)
  (ended nil
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
    (setf (phpinspect-pipeline-thread-ended thread) t)))

(cl-defmethod phpinspect-pipeline-ctx-close ((ctx phpinspect-pipeline-ctx))
  (let (errors err ended thread-live)
    (dolist (thread (phpinspect-pipeline-ctx-threads ctx))
      (setq err (thread-last-error (car thread))
            ended (phpinspect-pipeline-thread-ended (cdr thread))
            thread-live (thread-live-p (car thread)))

      (when thread-live
        (if ended
            (setq errors (nconc errors (list (format "Thread %s ended pipeline, but is still running"
                                                     (thread-name (car thread))))))
          (setq errors (nconc errors (list (format "Thread %s is still running when pipeline is closing"
                                                   (thread-name (car thread))))))))


      (when (thread-last-error (car thread))
        (setq errors (nconc errors (list (format "Thread %s signaled error: %s"
                                                 (thread-name (car thread))
                                                 (thread-last-error (car thread)))))))
      (unless ended
        (setq errors (nconc errors (list (format "Thread %s never ended"
                                                 (thread-name (car thread)))))))

      (when (thread-live-p (car thread))
        (thread-signal (car thread) 'quit nil)))

    (when errors
      (signal 'phpinspect-pipeline-error errors))))


(defmacro phpinspect-pipeline-emit (data)
  `(throw 'phpinspect-pipeline-emit ,data))

(defmacro phpinspect-pipeline-end (&optional value)
  (if value
      `(throw 'phpinspect-pipeline-emit
              (phpinspect-make-pipeline-end :value ,value :thread (current-thread)))
  `(throw 'phpinspect-pipeline-emit
          (phpinspect-make-pipeline-end :thread (current-thread)))))

(define-inline phpinspect-pipeline-pause ()
  "Pause the current pipeline thread"
  (inline-quote
   (if (input-pending-p)
       (let ((mx (make-mutex)))
         (phpinspect-thread-pause 1 mx (make-condition-variable mx "phpinspect-pipeline-pause")))
     (thread-yield))))

(defmacro phpinspect-pipeline-generator (queue &rest body)
  (declare (indent 1))

  (let ((result-sym (gensym))
        (queue-sym (gensym)))
    `(let (,result-sym
           (,queue-sym ,queue))
       (while (setq ,result-sym (progn ,@body))
         (phpinspect-queue-enqueue ,queue-sym ,result-sym)
         (phpinspect-pipeline-pause))

       (phpinspect-queue-enqueue ,queue-sym (phpinspect-make-pipeline-end :thread (current-thread))))))

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
                `(,(phpinspect-pipeline-step-name name "create")
                  ,incoming ,outgoing ,ctx-sym ,(phpinspect--pipeline-step--context-var-name step))
              `(,(phpinspect-pipeline-step-name name "create") ,incoming ,outgoing ,ctx-sym)))
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

(defmacro phpinspect-pipeline (&rest parameters)
  (let (key value steps body let-vars)
    (catch 'break
      (while parameters
        (setq key (pop parameters)
              value (pop parameters))

        (pcase key
          (:into
           (let ((parameters)
                 (name)
                 (construct-params))
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
                 (when (eq :with-context key)
                   (setq value `(quote ,value)))
                 (setq key (intern (string-replace ":with-" ":" (symbol-name key))))
                 (setq construct-params (nconc construct-params (list key value)))))
             (push (eval `(phpinspect--make-pipeline-step ,@construct-params :name (quote ,name)))
                   steps)))
          (_ (if (keywordp key)
                 (error "unexpected key %s" key)
               (setq body `(,key))
               (throw 'break nil))))))

    (when value
      (setq body (nconc body (list value))))

    (when parameters
      (setq body (nconc body parameters)))

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
          (errors (gensym))
          (result-sym (gensym))
          (collecting-sym (gensym)))
      `(progn
             (when (eq main-thread (current-thread))
               (error "Pipelines should not run in the main thread"))

             (let* (,@let-vars
                    (,ctx-sym (phpinspect-make-pipeline-ctx))
                    (,queue-sym (phpinspect-make-queue))
                    (,end-queue-sym (phpinspect-make-queue))
                    (,collecting-sym t)
                    ,recv-sym ,result-sym)

               ,(phpinspect--chain-pipeline-steps steps queue-sym end-queue-sym ctx-sym)

               (phpinspect-pipeline-generator ,queue-sym
                 ,@body)

               (while ,collecting-sym
                 (ignore-error 'phpinspect-pipeline-incoming
                     (progn
                       (phpinspect-pipeline--register-wakeup-function ,end-queue-sym)
                       (while (not (phpinspect-pipeline-end-p
                                    (setq ,recv-sym (phpinspect-pipeline-receive ,end-queue-sym))))
                         (setq ,result-sym (nconc ,result-sym (list ,recv-sym))))
                       (setq ,collecting-sym nil))))

               (phpinspect-pipeline-ctx-close ,ctx-sym)
               ,result-sym)))))

(defmacro phpinspect-pipeline-async (callback &rest parameters)
  (declare (indent 1))
  `(make-thread
    (lambda ()
      (condition-case err
          (let ((result (phpinspect-pipeline ,@parameters)))
            (funcall ,callback result nil))
        (t (funcall ,callback nil err))))
    "phpinspect-pipeline-async"))

(define-inline phpinspect-pipeline-receive (queue)
  (inline-letevals (queue)
    `(or (phpinspect-queue-dequeue ,queue)
         (let ((mx (make-mutex)))
           (with-mutex mx
             (condition-wait (make-condition-variable mx "phpinspect-pipeline-receive")))
           (phpinspect-queue-dequeue ,queue)))))

(defun phpinspect-pipeline-step-name (name &optional suffix)
  (intern (concat (symbol-name name) (if suffix (concat "-" suffix) ""))))

(define-inline phpinspect-pipeline--register-wakeup-function (queue)
  (inline-quote
   (let ((thread (current-thread)))
     (setf (phpinspect-queue-subscription ,queue)
           (lambda () (thread-signal thread 'phpinspect-pipeline-incoming nil))))))

(defmacro phpinspect-define-pipeline-step (name function-name)
  (unless (symbolp name)
    (error "name must be a symbol"))

  (unless (symbolp function-name)
    (error "function-name must be a symbol"))

  (let ((execute-function (phpinspect-pipeline-step-name name "execute"))
        (constructor-function (phpinspect-pipeline-step-name name "create")))

    `(progn
       (define-inline ,execute-function (input &optional context)
         (if context
             (inline-quote
              (catch 'phpinspect-pipeline-emit
                ,(append `(,function-name) '(,context) '(,input))
                nil))
           (inline-quote
            (catch 'phpinspect-pipeline-emit
              ,(append `(,function-name) '(,input))
              nil))))

       (define-inline ,constructor-function (queue consumer-queue pipeline-ctx &optional context)
         (inline-letevals (queue consumer-queue context)
           (let ((thread-name ,(concat "phpinspect-pipeline-" (symbol-name name)))
                 (statement (list (quote ,execute-function))))
             ,@(list
                '(let ((incoming (gensym "incoming"))
                       (outgoing (gensym "outgoing"))
                       (inc-queue (gensym "queue"))
                       (out-queue (gensym "queue"))
                       (context-sym (gensym "context"))
                       (continue-running (gensym "continue-running"))
                       (original-thread (gensym "original-thread"))
                       (pctx-sym (gensym "pipeline-ctx"))
                       (incoming-end (gensym "incoming-end"))
                       (end (gensym "end")))

                   (setq statement (nconc statement (list incoming)))
                   (unless (and (inline-const-p context) (not (inline-const-val context)))
                     (setq statement (nconc statement (list context-sym))))

                   (inline-quote
                    (let ((,original-thread (current-thread))
                          (,inc-queue ,queue)
                          (,out-queue ,consumer-queue)
                          (,context-sym ,context)
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
                                   (catch 'phpinspect-pipeline-break
                                     (while ,continue-running
                                       (setq ,incoming (phpinspect-pipeline-receive ,inc-queue))
                                       (if (phpinspect-pipeline-end-p ,incoming)
                                           (progn
                                             (setq ,incoming-end ,incoming)
                                             (when (phpinspect-pipeline-end-value ,incoming)
                                               (progn
                                                 (setq ,incoming (phpinspect-pipeline-end-value ,incoming)
                                                       ,outgoing ,statement)
                                                 (phpinspect-queue-enqueue ,out-queue ,outgoing 'no-notify)))

                                             (setq ,end (phpinspect-make-pipeline-end :thread (current-thread)))
                                             (phpinspect-pipeline-ctx-register-end ,pctx-sym ,end)
                                             (setq ,continue-running nil)
                                             (phpinspect-queue-enqueue ,out-queue ,end))

                                         ;; Else
                                         (setq ,outgoing ,statement)
                                         (when (phpinspect-pipeline-end-p ,outgoing)
                                           (setq ,end (phpinspect-make-pipeline-end :thread (current-thread)))
                                           (phpinspect-pipeline-ctx-register-end ,pctx-sym ,end)
                                           (setq ,continue-running nil))
                                         (phpinspect-queue-enqueue ,out-queue ,outgoing))

                                       (when ,end
                                         (throw 'phpinspect-pipeline-break nil)))))
                               (phpinspect-pipeline-incoming)
                               (t (message "Pipeline thread errored: %s" err)
                                  (setq ,continue-running nil)
                                  (phpinspect-pipeline-ctx-register-end
                                   ,pctx-sym
                                   (phpinspect-make-pipeline-end :thread (current-thread))))))))
                       ,thread-name)))))))))))

(provide 'phpinspect-pipeline)
;;; phpinspect-pipeline.el ends here
