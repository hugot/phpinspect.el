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
(require 'phpinspect-util)
(require 'phpinspect-project)
(require 'phpinspect-index)
(require 'phpinspect-class)
(require 'phpinspect-queue)
(require 'phpinspect-pipeline)

(defcustom phpinspect-worker-pause-time 1
  "Number of seconds that `phpinspect-worker' should pause when
user input is detected. A higher value means better
responsiveness, at the cost of slower code indexation. On modern
hardware this probably doesn't need to be tweaked."
  :type 'number
  :group 'phpinspect)

(defvar phpinspect-worker nil
  "Contains the phpinspect worker that is used by all projects.")

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
running. If this is nil, the thread is stopped.")
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

(cl-defmethod phpinspect-resolve-dynamic-worker ((_worker phpinspect-dynamic-worker))
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

(cl-defmethod phpinspect-worker-wakeup ((worker phpinspect-worker))
  (when (eq main-thread (thread--blocker (phpinspect-worker-thread worker)))
    (thread-signal (phpinspect-worker-thread worker)
                   'phpinspect-wakeup-thread nil)))

(cl-defmethod phpinspect-worker-make-wakeup-function ((worker phpinspect-worker))
  (lambda ()
    (phpinspect-worker-wakeup worker)))

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
  "Specialized enqueuement method for index tasks. Prevents
indexation tasks from being added when there are identical tasks
already present in the queue."
  (phpinspect-queue-enqueue-noduplicate (phpinspect-worker-queue worker) task #'phpinspect-task=))

(cl-defmethod phpinspect-worker-enqueue ((worker phpinspect-dynamic-worker) task)
  (phpinspect-worker-enqueue (phpinspect-resolve-dynamic-worker worker)
                             task))
(cl-defgeneric phpinspect-worker-make-thread-function (worker)
  "Create a function that can be used to start WORKER's thread.")

(cl-defmethod phpinspect-worker-make-thread-function ((worker phpinspect-worker))
  (lambda ()
    (while (phpinspect-worker-continue-running worker)
      ;; This error is used to wake up the thread when new tasks are added to the
      ;; queue.
      (condition-case err
          (ignore-error 'phpinspect-wakeup-thread
            (let* ((task (phpinspect-queue-dequeue (phpinspect-worker-queue worker)))
                   (mx (make-mutex))
                   (continue (make-condition-variable mx)))
              (if task
                  ;; Execute task if it belongs to a project that has not been
                  ;; purged (meaning that it is still actively used).
                  (unless (phpinspect-project-purged (phpinspect-task-project task))
                    (phpinspect-task-execute task worker))
                ;; else: join with the main thread until wakeup is signaled
                (thread-join main-thread))

              ;; Pause for a second after indexing something, to allow user input to
              ;; interrupt the thread.
              (unless (or (not (input-pending-p))
                          (phpinspect-worker-skip-next-pause worker))
                (phpinspect-thread-pause phpinspect-worker-pause-time mx continue))
              (setf (phpinspect-worker-skip-next-pause worker) nil)))
        ((debug error) (thread-signal main-thread 'phpinspect-worker-error err))
        (t (phpinspect--log "Phpinspect worker thread errored :%s" err))))
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
            ;; Use with-temp-buffer so as to not associate thread with the
            ;; current buffer. Otherwise, the buffer associated with this thread
            ;; will be unkillable while the thread is running.
            (with-temp-buffer
              (make-thread (phpinspect-worker-make-thread-function worker) "phpinspect-worker"))))))

(cl-defmethod phpinspect-worker-start ((worker phpinspect-dynamic-worker))
  (phpinspect-worker-start (phpinspect-resolve-dynamic-worker worker)))

(cl-defgeneric phpinspect-worker-stop (worker)
  "Stop the worker")

(cl-defmethod phpinspect-worker-stop ((worker phpinspect-worker))
  (setf (phpinspect-worker-continue-running worker) nil)
  (phpinspect-worker-wakeup worker))

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

;;; TASKS
;; The rest of this file contains task definitions. Tasks represent actions that
;; can be executed by `phpinspect-worker'. Some methods are required to be
;; implemented for all tasks, while others aren't.

;; REQUIRED METHODS:
;;  - phpinspect-task-execute
;;  - phpinspect-task-project

;; OPTIONAL METHODS:
;;  - phpinspect-task=

;;; Code:

(cl-defgeneric phpinspect-task-execute (task worker)
  "Execute TASK for WORKER.")

(cl-defmethod phpinspect-task= (_task1 _task2)
  "Whether or not TASK1 and TASK2 are set to execute the exact same action."
  nil)

(cl-defgeneric phpinspect-task-project (task)
  "The project that this task belongs to.")


;;; INDEX TASK
(cl-defstruct (phpinspect-index-task
               (:constructor phpinspect-make-index-task-generated))
  "Represents an index task that can be executed by a `phpinspect-worker`."
  (project nil
           :type phpinspect-project
           :documentation
           "The project that the task should be executed for.")
  (type nil
        :type phpinspect--type
        :documentation
        "The type whose file should be indexed."))

(cl-defgeneric phpinspect-make-index-task ((project phpinspect-project)
                                          (type phpinspect--type))
  (phpinspect-make-index-task-generated
   :project project
   :type type))

(cl-defmethod phpinspect-task-project ((task phpinspect-index-task))
  (phpinspect-index-task-project task))


(cl-defmethod phpinspect-task= ((task1 phpinspect-index-task) (task2 phpinspect-index-task))
  (and (eq (phpinspect-index-task-project task1)
           (phpinspect-index-task-project task2))
       (phpinspect--type= (phpinspect-index-task-type task1) (phpinspect-index-task-type task2))))

(cl-defmethod phpinspect-task-execute ((task phpinspect-index-task)
                                       (worker phpinspect-worker))
  "Execute index TASK for WORKER."
  (let ((project (phpinspect-index-task-project task))
        (is-native-type (phpinspect--type-is-native
                         (phpinspect-index-task-type task))))
    (phpinspect--log "Indexing class %s for project in %s as task."
                     (phpinspect-index-task-type task)
                     (phpinspect-project-root project))

    (cond (is-native-type
           (phpinspect--log "Skipping indexation of native type %s as task"
                            (phpinspect-index-task-type task))

           ;; We can skip pausing when a native type is encountered
           ;; and skipped, as we haven't done any intensive work that
           ;; may cause hangups.
           (setf (phpinspect-worker-skip-next-pause worker) t))
          (t
           (let* ((type (phpinspect-index-task-type task))
                  (root-index (phpinspect-project-index-type-file project type)))
             (when root-index
               (phpinspect-project-add-index project root-index)))))))

;;; INDEX FILE TASK
(cl-defstruct (phpinspect-index-dir-task (:constructor phpinspect-make-index-dir-task))
  "A task for the indexation of files"
  (project nil
           :type phpinspect-project)
  (dir nil
       :type string))

(cl-defmethod phpinspect-task=
  ((task1 phpinspect-index-dir-task) (task2 phpinspect-index-dir-task))
  (and (eq (phpinspect-index-dir-task-project task1)
           (phpinspect-index-dir-task-project task2))
       (string= (phpinspect-index-dir-task-dir task1)
                (phpinspect-index-dir-task-dir task2))))

(cl-defmethod phpinspect-task-project ((task phpinspect-index-dir-task))
  (phpinspect-index-dir-task-project task))

(cl-defmethod phpinspect-task-execute ((task phpinspect-index-dir-task)
                                       (_worker phpinspect-worker))
  (phpinspect--log "Entering..")
  (let* ((project (phpinspect-index-dir-task-project task))
         (fs (phpinspect-project-fs project))
         (dir (phpinspect-index-dir-task-dir task)))
    (phpinspect--log "Indexing directory %s" dir)
    (phpinspect-pipeline (phpinspect-fs-directory-files-recursively fs dir "\\.php$")
      :into (phpinspect-project-add-file-index :with-context project))))

(provide 'phpinspect-worker)
;;; phpinspect-worker.el ends here
