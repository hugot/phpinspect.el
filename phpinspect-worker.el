;;; phpinspect-worker.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'phpinspect-project)
(require 'phpinspect-index)
(require 'phpinspect-class)

(defvar phpinspect-worker nil
  "Contains the phpinspect worker that is used by all projects.")

(cl-defstruct (phpinspect-queue-item
               (:constructor phpinspect-make-queue-item))
  (next nil
        :type phpinspect-queue-item
        :documentation
        "The next item in the queue")
  (thing nil
         :type any
         :documentation
         "The thing stored in the queue")
  (previous nil
            :type phpinspect-queue-item
            :documentation
            "The previous item in the queue")
  (subscription nil
                :type function
                :read-only t
                :documentation
                "A function that should be called when items are
                enqueued."))

(defsubst phpinspect-make-queue (&optional subscription)
  (phpinspect-make-queue-item :subscription subscription))

;; Recursion causes max-eval-depth error here for long queues. Hence the loop
;; implementation for these two functions.
(cl-defmethod phpinspect-queue-last ((item phpinspect-queue-item))
  "Get the last item in the queue that ITEM is part of."
  (while (phpinspect-queue-item-next item)
    (setq item (phpinspect-queue-item-next item)))
  item)

(cl-defmethod phpinspect-queue-first ((item phpinspect-queue-item))
  "Get the first item in the queue that ITEM is part of."
  (while (phpinspect-queue-item-previous item)
      (setq item (phpinspect-queue-item-previous item)))
  item)

(cl-defmethod phpinspect-queue-enqueue ((item phpinspect-queue-item) thing)
  "Add THING to the end of the queue that ITEM is part of."
  (let ((last (phpinspect-queue-last item)))
    (if (not (phpinspect-queue-item-thing last))
        (setf (phpinspect-queue-item-thing last) thing)
      (setf (phpinspect-queue-item-next last)
            (phpinspect-make-queue-item
             :previous last
             :thing thing
             :subscription (phpinspect-queue-item-subscription item)))))
  (when (phpinspect-queue-item-subscription item)
    (funcall (phpinspect-queue-item-subscription item))))

(cl-defmethod phpinspect-queue-dequeue ((item phpinspect-queue-item))
  "Remove the thing at the front of the queue that ITEM is part of an return it."
  (let* ((first (phpinspect-queue-first item))
         (thing (phpinspect-queue-item-thing first))
         (next (phpinspect-queue-item-next first)))
    (when next (setf (phpinspect-queue-item-previous next) nil))
    (cond ((and (eq item first) (not next))
           (setf (phpinspect-queue-item-thing item)
                 nil))
          ((eq item first)
           (setf (phpinspect-queue-item-thing item)
                 (phpinspect-queue-item-thing next))
           (setf (phpinspect-queue-item-next item)
                 (phpinspect-queue-item-next next))))
    thing))

(defmacro phpinspect-doqueue (place-and-queue &rest body)
  "Loop over queue defined in PLACE-AND-QUEUE executing BODY.

PLACE-AND-QUEUE is a two-member list. The first item should be
the place that the current thing in the queue should be assigned
to upon each iteration. The second item should be a queue-item
belonging to the queue that must be iterated over.

BODY can be any form."
  (declare (indent defun))
  (let ((item-sym (gensym))
        (place (car place-and-queue))
        (queue (cadr place-and-queue)))
    `(let* ((,item-sym (phpinspect-queue-first ,queue))
            (,place (phpinspect-queue-item-thing ,item-sym)))
       (when ,place
         ,@body
         (while (setq ,item-sym (phpinspect-queue-item-next ,item-sym))
           (setq ,place (phpinspect-queue-item-thing ,item-sym))
           ,@body)))))

(cl-defmethod phpinspect-queue-find
  ((item phpinspect-queue-item) thing comparison-func)
  "Find THING in the queue that ITEM is part of using COMPARISON-FUNC."
  (catch 'found
    (phpinspect-doqueue (current-thing item)
      (when (funcall comparison-func current-thing thing)
        (throw 'found current-thing)))))

(cl-defmethod phpinspect-queue-enqueue-noduplicate
  ((item phpinspect-queue-item) thing comparison-func)

  (when (not (phpinspect-queue-find item thing comparison-func))
    (phpinspect-queue-enqueue item thing)))

(cl-defmethod phpinspect-queue-await-insert ((item phpinspect-queue-item))
  (condition-wait (phpinspect-queue-item-insert item)))

(cl-defstruct (phpinspect-worker
               (:constructor phpinspect-make-worker-generated))
  (queue nil
         :type phpinspect-queue-item
         :documentation
         "The queue of tasks that are pending")
  (thread nil
          :type thread
          :documentation
          "The thread of this worker")
  (continue-running nil
                    :type bool
                    :documentation
                    "Whether or not the thread should continue
running. If this is nil, the thread isstopped.")
  (skip-next-pause nil
                   :type bool
                   :documentation
                   "Whether or not the thread should skip its next scheduled pause."))

(cl-defstruct (phpinspect-dynamic-worker
               (:constructor phpinspect-make-dynamic-worker-generated))
  "A dynamic worker is nothing other than an object that is
supported by all of the same methods as a `phpinspect-worker`,
but relies on an underlying, global worker to actually do the
work. The reason for its implementation is to allow users to
manage phpinspect's worker thread centrally in a dynamic
variable, while also making the behaviour of objects that depend
on the worker independent of dynamic variables during testing.")

(cl-defmethod phpinspect-resolve-dynamic-worker ((worker phpinspect-dynamic-worker))
  phpinspect-worker)

(defsubst phpinspect-make-dynamic-worker ()
  (phpinspect-make-dynamic-worker-generated))

(defsubst phpinspect-make-worker ()
  "Create a new worker object."
  (let ((worker (phpinspect-make-worker-generated)))
    (setf (phpinspect-worker-queue worker)
          (phpinspect-make-queue (phpinspect-worker-make-wakeup-function worker)))
    worker))

(define-error 'phpinspect-wakeup-thread
  "This error is used to wakeup the index thread")

(cl-defgeneric phpinspect-worker-make-wakeup-function (worker)
  "Create a function that can be used to wake up WORKER's thread.")

(cl-defmethod phpinspect-worker-make-wakeup-function ((worker phpinspect-worker))
  (lambda ()
    (when (eq main-thread (thread--blocker (phpinspect-worker-thread worker)))
      (thread-signal (phpinspect-worker-thread worker)
                     'phpinspect-wakeup-thread nil))))

(cl-defmethod phpinspect-worker-make-wakeup-function ((worker phpinspect-dynamic-worker))
  (phpinspect-worker-make-wakeup-function (phpinspect-resolve-dynamic-worker worker)))

(cl-defgeneric phpinspect-worker-live-p (worker)
  "Just a shorthand to check whether or not the WORKER's thread is running.")

(cl-defmethod phpinspect-worker-live-p ((worker phpinspect-worker))
  (when (phpinspect-worker-thread worker)
    (thread-live-p (phpinspect-worker-thread worker))))

(cl-defmethod phpinspect-worker-live-p ((worker phpinspect-dynamic-worker))
  (phpinspect-worker-live-p (phpinspect-resolve-dynamic-worker worker)))

(cl-defgeneric phpinspect-worker-enqueue (worker task)
  "Enqueue a TASK to be executed by WORKER.")

(cl-defmethod phpinspect-worker-enqueue ((worker phpinspect-worker) task)
  (phpinspect-queue-enqueue (phpinspect-worker-queue worker) task))

(cl-defmethod phpinspect-worker-enqueue ((worker phpinspect-dynamic-worker) task)
  (phpinspect-worker-enqueue (phpinspect-resolve-dynamic-worker worker)
                             task))

(defun phpinspect-thread-pause (pause-time mx continue)
  "Pause current thread using MX and CONTINUE for PAUSE-TIME idle seconds.

PAUSE-TIME must be the idle time that the thread should pause for.
MX must be a mutex
CONTINUE must be a condition-variable"
  (phpinspect--log "Worker thead is paused for %d seconds" pause-time)
  (run-with-idle-timer
   pause-time
   nil
   (lambda () (with-mutex mx (condition-notify continue))))
  (with-mutex mx (condition-wait continue))
  (phpinspect--log "Index thread continuing"))

(cl-defgeneric phpinspect-worker-make-thread-function (worker)
  "Create a function that can be used to start WORKER's thread.")

(cl-defmethod phpinspect-worker-make-thread-function ((worker phpinspect-worker))
  (lambda ()
    (while (phpinspect-worker-continue-running worker)
      ;; This error is used to wake up the thread when new tasks are added to the
      ;; queue.
      (ignore-error 'phpinspect-wakeup-thread
        (let* ((task (phpinspect-queue-dequeue (phpinspect-worker-queue worker)))
               (mx (make-mutex))
               (continue (make-condition-variable mx)))
          (if task
              (phpinspect-task-execute task worker)
            ;; else: join with the main thread until wakeup is signaled
            (thread-join main-thread))

          ;; Pause for a second after indexing something, to allow user input to
          ;; interrupt the thread.
          (unless (phpinspect-worker-skip-next-pause worker)
            (phpinspect-thread-pause 1 mx continue))
          (setf (phpinspect-worker-skip-next-pause worker) nil))))
    (phpinspect--log "Worker thread exiting")
    (message "phpinspect worker thread exited")))

(cl-defmethod phpinspect-worker-make-thread-function ((worker phpinspect-dynamic-worker))
  (phpinspect-worker-make-thread-function
   (phpinspect-resolve-dynamic-worker worker)))

(cl-defgeneric phpinspect-worker-start (worker)
  "Start WORKER's thread.")

(cl-defmethod phpinspect-worker-start ((worker phpinspect-worker))
  (if (phpinspect-worker-live-p worker)
      (error "Attempt to start a worker that is already running")
    (progn
      (setf (phpinspect-worker-continue-running worker) t)
      (setf (phpinspect-worker-thread worker)
            (make-thread (phpinspect-worker-make-thread-function worker))))))

(cl-defmethod phpinspect-worker-start ((worker phpinspect-dynamic-worker))
  (phpinspect-worker-start (phpinspect-resolve-dynamic-worker worker)))

(cl-defgeneric phpinspect-worker-stop (worker)
  "Stop the worker")

(cl-defmethod phpinspect-worker-stop ((worker phpinspect-worker))
  (setf (phpinspect-worker-continue-running worker) nil))

(cl-defmethod phpinspect-worker-stop ((worker phpinspect-dynamic-worker))
  (phpinspect-worker-stop (phpinspect-resolve-dynamic-worker worker)))

(defun phpinspect-ensure-worker ()
  (interactive)
  (when (not phpinspect-worker)
    (setq phpinspect-worker (phpinspect-make-worker)))

  (when (not (phpinspect-worker-live-p phpinspect-worker))
    (phpinspect-worker-start phpinspect-worker)))

(defun phpinspect-stop-worker ()
  (interactive)
  (phpinspect-worker-stop phpinspect-worker))

(cl-defstruct (phpinspect-index-task
               (:constructor phpinspect-make-index-task-generated))
  "Represents an index task that can be executed by a `phpinspect-worker`."
  (project nil
           :type phpinspect--project
           :documentation
           "The project that the task should be executed for.")
  (type nil
        :type phpinspect--type
        :documentation
        "The type whose file should be indexed."))

(cl-defgeneric phpinspect-make-index-task ((project phpinspect--project)
                                          (type phpinspect--type))
  (phpinspect-make-index-task-generated
   :project project
   :type type))

(cl-defgeneric phpinspect-task-execute (task worker)
  "Execute TASK for WORKER.")

(cl-defmethod phpinspect-task-execute ((task phpinspect-index-task)
                                       (worker phpinspect-worker))
  "Execute index TASK for WORKER."
  (let ((project (phpinspect-index-task-project task))
        (is-native-type (phpinspect--type-is-native
                         (phpinspect-index-task-type task))))
    (phpinspect--log "Indexing class %s for project in %s from index thread"
                     (phpinspect-index-task-type task)
                     (phpinspect--project-root project))

    (if is-native-type
        (progn
          (phpinspect--log "Skipping indexation of native type %s"
                           (phpinspect-index-task-type task))

          ;; We can skip pausing when a native type is encountered
          ;; and skipped, as we haven't done any intensive work that
          ;; may cause hangups.
          (setf (phpinspect-worker-skip-next-pause worker) t))
      (let* ((type (phpinspect-index-task-type task))
             (root-index (phpinspect--index-type-file project type)))
        (when root-index
          (phpinspect--project-add-index project root-index))))))


(provide 'phpinspect-worker)
;;; phpinspect-worker.el ends here
