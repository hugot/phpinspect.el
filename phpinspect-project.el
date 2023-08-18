;;; phpinspect-project.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc

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

(require 'phpinspect-project-struct)
(require 'phpinspect-autoload)
(require 'phpinspect-worker)
(require 'phpinspect-index)
(require 'phpinspect-class)
(require 'phpinspect-type)
(require 'phpinspect-fs)
(require 'filenotify)

(defvar phpinspect-auto-reindex nil
  "Whether or not phpinspect should automatically search for new
files. The current implementation is clumsy and can result in
serious performance hits. Enable at your own risk (:")

(defvar phpinspect-project-root-function #'phpinspect--find-project-root
  "Function that phpinspect uses to find the root directory of a project.")

(defvar-local phpinspect--buffer-project nil
  "The root directory of the PHP project that this buffer belongs to")

(defmacro phpinspect-project-edit (project &rest body)
  (declare (indent 1))
  `(unless (phpinspect-project-read-only-p ,project)
     ,@body))

(defsubst phpinspect-current-project-root ()
  "Call `phpinspect-project-root-function' with ARGS as arguments."
  (unless (and (boundp 'phpinspect--buffer-project) phpinspect--buffer-project)
    (set (make-local-variable 'phpinspect--buffer-project) (funcall phpinspect-project-root-function)))
  phpinspect--buffer-project)

(cl-defmethod phpinspect-project-purge ((project phpinspect-project))
  "Disable all background processes for project and put it in a `purged` state."
  (maphash (lambda (_ watcher) (file-notify-rm-watch watcher))
           (phpinspect-project-file-watchers project))

  (setf (phpinspect-project-file-watchers project)
         (make-hash-table :test #'equal :size 10000 :rehash-size 10000))
  (setf (phpinspect-project-purged project) t))

(cl-defmethod phpinspect-project-watch-file ((project phpinspect-project)
                                              filepath
                                              callback)
  (phpinspect-project-edit project
    (let ((watcher (file-notify-add-watch filepath '(change) callback)))
      (puthash filepath watcher (phpinspect-project-file-watchers project)))))

(cl-defmethod phpinspect-project-add-return-types-to-index-queueue
  ((project phpinspect-project) methods)
  (phpinspect-project-edit project
    (dolist (method methods)
      (when (phpinspect--function-return-type method)
        (phpinspect-project-enqueue-if-not-present
         project
         (phpinspect--function-return-type method))))))

(cl-defmethod phpinspect-project-add-variable-types-to-index-queue
  ((project phpinspect-project) variables)
  (phpinspect-project-edit project
    (dolist (var variables)
      (when (phpinspect--variable-type var)
        (phpinspect-project-enqueue-if-not-present project (phpinspect--variable-type var))))))

(cl-defmethod phpinspect-project-enqueue-if-not-present
  ((project phpinspect-project) (type phpinspect--type))
  (phpinspect-project-edit project
    (unless (phpinspect--type-is-native type)
      (let ((class (phpinspect-project-get-class project type)))
        (when (or (not class)
                  (not (or (phpinspect--class-initial-index class))))
          (when (not class)
            (setq class (phpinspect-project-create-class project type)))
          (unless (or (phpinspect--type= phpinspect--null-type type)
                      (phpinspect--type-is-native type))
            (phpinspect--log "Adding unpresent class %s to index queue" type)
            (phpinspect-worker-enqueue (phpinspect-project-worker project)
                                       (phpinspect-make-index-task project type))))))))

(cl-defmethod phpinspect-project-add-class-attribute-types-to-index-queue
  ((project phpinspect-project) (class phpinspect--class))
  (phpinspect-project-edit project
    (phpinspect-project-add-return-types-to-index-queueue
     project
     (phpinspect--class-get-method-list class))
    (phpinspect-project-add-return-types-to-index-queueue
     project
     (phpinspect--class-get-static-method-list class))
    (phpinspect-project-add-variable-types-to-index-queue
     project
     (phpinspect--class-variables class))))

(cl-defmethod phpinspect-project-add-index
  ((project phpinspect-project) (index (head phpinspect--root-index)) &optional index-imports)
  (phpinspect-project-edit project
    (when index-imports
      (phpinspect-project-enqueue-imports project (alist-get 'imports (cdr index))))

    (dolist (indexed-class (alist-get 'classes (cdr index)))
      (phpinspect-project-add-class project (cdr indexed-class) index-imports))

    (dolist (func (alist-get 'functions (cdr index)))
      (phpinspect-project-set-function project func))))

(cl-defmethod phpinspect-project-set-function
  ((project phpinspect-project) (func phpinspect--function))
  (phpinspect-project-edit project
    (puthash (phpinspect--function-name-symbol func) func
             (phpinspect-project-function-index project))))

(cl-defmethod phpinspect-project-get-function
  ((project phpinspect-project) (name (head phpinspect-name)))
  (gethash name (phpinspect-project-function-index project)))

(cl-defmethod phpinspect-project-get-function-or-extra
  ((project phpinspect-project) (name (head phpinspect-name)))
  (or (phpinspect-project-get-function project name)
      (and (phpinspect-project-extra-function-retriever project)
           (funcall (phpinspect-project-extra-function-retriever project)
                    name))))

(cl-defmethod phpinspect-project-delete-function
  ((project phpinspect-project) (name (head phpinspect-name)))
  (phpinspect-project-edit project
    (remhash name (phpinspect-project-function-index project))))

(cl-defmethod phpinspect-project-get-functions ((project phpinspect-project))
  (let ((funcs))
    (maphash
     (lambda (_name func) (push func funcs))
     (phpinspect-project-function-index project))

    funcs))

(cl-defmethod phpinspect-project-get-functions-with-extra ((project phpinspect-project))
  (let ((funcs))
    (maphash
     (lambda (_name func) (push func funcs))
     (phpinspect-project-function-index project))

    (if (phpinspect-project-extra-function-retriever project)
        (nconc funcs (funcall (phpinspect-project-extra-function-retriever project) nil))
      funcs)))

(cl-defmethod phpinspect-project-enqueue-imports
  ((project phpinspect-project) imports)
  (phpinspect-project-edit project
    (dolist (import imports)
      (when import
        (phpinspect--log "Adding import to index queue: %s" import)
        (phpinspect-project-enqueue-if-not-present project (cdr import))))))

(cl-defmethod phpinspect-project-delete-class ((project phpinspect-project) (class phpinspect--class))
  (phpinspect-project-delete-class project (phpinspect--class-name class)))

(cl-defmethod phpinspect-project-delete-class ((project phpinspect-project) (class-name phpinspect--type))
  (phpinspect-project-edit project
    (remhash (phpinspect--type-name-symbol class-name) (phpinspect-project-class-index project))))

(cl-defmethod phpinspect-project-add-class
  ((project phpinspect-project) (indexed-class (head phpinspect--indexed-class)) &optional index-imports)
  (phpinspect-project-edit project
    (if (not (alist-get 'class-name (cdr indexed-class)))
        (phpinspect--log "Error: Class with declaration %s does not have a name" (alist-get 'declaration indexed-class))
      ;; Else
      (let* ((class-name (phpinspect--type-name-symbol
                          (alist-get 'class-name (cdr indexed-class))))
             (class (gethash class-name
                             (phpinspect-project-class-index project))))
        (unless class
          (setq class (phpinspect--make-class-generated
                       :class-retriever (phpinspect-project-make-class-retriever project))))

        (when index-imports
          (phpinspect-project-enqueue-imports
           project (alist-get 'imports (cdr indexed-class))))

        (phpinspect--class-set-index class indexed-class)
        (puthash class-name class (phpinspect-project-class-index project))
        (phpinspect-project-add-class-attribute-types-to-index-queue project class)))))

(cl-defmethod phpinspect-project-set-class
  ((project phpinspect-project) (class-fqn phpinspect--type) (class phpinspect--class))
  (phpinspect-project-edit project
    (puthash (phpinspect--type-name-symbol class-fqn)
             class
             (phpinspect-project-class-index project))))

(cl-defmethod phpinspect-project-create-class
  ((project phpinspect-project) (class-fqn phpinspect--type))
  (phpinspect-project-edit project
    (let ((class (phpinspect--make-class-generated
                  :class-retriever (phpinspect-project-make-class-retriever project))))
      (phpinspect-project-set-class project class-fqn class)
      class)))

(cl-defmethod phpinspect-project-get-class-create
  ((project phpinspect-project) (class-fqn phpinspect--type) &optional no-enqueue)
  (let ((class (phpinspect-project-get-class project class-fqn)))
    (unless class
      (phpinspect-project-edit project
        (setq class (phpinspect-project-create-class project class-fqn))
        (unless no-enqueue
          (phpinspect-project-enqueue-if-not-present project class-fqn))))
    class))

(cl-defmethod phpinspect-project-get-class-extra-or-create
  ((project phpinspect-project) (class-fqn phpinspect--type) &optional no-enqueue)
  (or (phpinspect-project-get-class-or-extra project class-fqn)
      (phpinspect-project-get-class-create project class-fqn no-enqueue)))

(defalias 'phpinspect-project-add-class-if-missing #'phpinspect-project-get-class-create)

(cl-defmethod phpinspect-project-get-class
  ((project phpinspect-project) (class-fqn phpinspect--type))
  "Get indexed class by name of CLASS-FQN stored in PROJECT."
  (let ((class (gethash (phpinspect--type-name-symbol class-fqn)
                        (phpinspect-project-class-index project))))
    (when (and class (phpinspect-project-read-only-p project)
               (not (phpinspect--class-read-only-p class)))
      (setf (phpinspect--class-read-only-p class) t))

    class))

(cl-defmethod phpinspect-project-get-class-or-extra
  ((project phpinspect-project) (class-fqn phpinspect--type))
  (or (phpinspect-project-get-class project class-fqn)
      (and (phpinspect-project-extra-class-retriever project)
           (funcall (phpinspect-project-extra-class-retriever project)
                    class-fqn))))

(cl-defmethod phpinspect-project-get-type-filepath
  ((project phpinspect-project) (type phpinspect--type) &optional index-new)
  "Retrieve filepath to TYPE definition file.

when INDEX-NEW is non-nil, new files are added to the index
before the search is executed."
  (let* ((autoloader (phpinspect-project-autoload project)))
    (when (eq index-new 'index-new)
      (phpinspect-project-edit project
        (phpinspect-autoloader-refresh autoloader)))
    (let* ((result (phpinspect-autoloader-resolve
                    autoloader (phpinspect--type-name-symbol type))))
      (if (not result)
          ;; Index new files and try again if not done already.
          (if (eq index-new 'index-new)
              nil
            (when phpinspect-auto-reindex
              (phpinspect--log "Failed finding filepath for type %s. Retrying with reindex."
                               (phpinspect--type-name type))
              (phpinspect-project-get-type-filepath project type 'index-new)))
        result))))

(cl-defmethod phpinspect-project-index-type-file
  ((project phpinspect-project) (type phpinspect--type))
  "Index the file that TYPE is expected to be defined in."

  (condition-case error
      (let* ((file (phpinspect-project-get-type-filepath project type))
             (visited-buffer (when file (find-buffer-visiting file))))
        (when file
          (if visited-buffer
              (with-current-buffer visited-buffer (phpinspect-index-current-buffer))
            (with-temp-buffer (phpinspect-project-index-file project file)))))
    (file-missing
     (phpinspect--log "Failed to find file for type %s:  %s" type error)
     nil)))

(cl-defmethod phpinspect-project-index-file
  ((project phpinspect-project) (filename string))
  "Index "
  (let ((fs (phpinspect-project-fs project)))
    (with-temp-buffer
      (phpinspect-fs-insert-file-contents fs filename 'prefer-async)
      (phpinspect-index-current-buffer))))

(cl-defmethod phpinspect-project-add-file-index ((project phpinspect-project) (filename string))
  (phpinspect-project-add-index project (phpinspect-project-index-file project filename)))

(defcustom phpinspect-projects nil
  "PHPInspect Projects."
  :type '(alist :key-type string
                :value-type (alist :key-type symbol
                                   :options ((include-dirs (repeat string)))))
  :group 'phpinspect)

(defun phpinspect-project-make-file-indexer (project)
  (lambda (filename)
    (phpinspect-project-add-file-index project filename)))

(defun phpinspect-project-make-root-resolver (project)
  (lambda () (phpinspect-project-root project)))

(defun phpinspect-project-make-class-retriever (project)
  (lambda (type)
    (or (phpinspect-project-get-class-or-extra project type)
        (phpinspect-project-get-class-create project type))))

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

(cl-defmethod phpinspect-make-index-task ((project phpinspect-project)
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

(provide 'phpinspect-project)
;;; phpinspect-project.el ends here
