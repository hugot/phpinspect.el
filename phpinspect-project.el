;;; phpinspect-project.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(require 'phpinspect-project-struct)
(require 'phpinspect-autoload)
(require 'phpinspect-worker)
(require 'phpinspect-index)
(require 'phpinspect-typedef)
(require 'phpinspect-type)
(require 'phpinspect-fs)
(require 'phpinspect-typedef)
(require 'phpinspect-method-cell)
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
      (when (phpi-method-return-type method)
        (phpinspect-project-enqueue-if-not-present
         project
         (phpi-method-return-type method))))))

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
      (let ((typedef (phpinspect-project-get-typedef project type)))
        (when (or (not typedef)
                  (not (or (phpi-typedef-initial-index typedef))))
          (unless (or (phpinspect--type= phpinspect--null-type type)
                      (phpinspect--type-is-native type))
            (phpinspect--log "Adding unpresent typedef %s to index queue" type)
            (phpinspect-worker-enqueue (phpinspect-project-worker project)
                                       (phpinspect-make-index-task project type))))))))

(defun phpinspect-project-enqueue-types (project types)
  (dolist (type types)
    (phpinspect-project-enqueue-if-not-present project type)))

(cl-defmethod phpinspect-project-add-index
  ((project phpinspect-project) (index (head phpinspect--root-index)) &optional index-dependencies)
  (phpinspect-project-edit project
    (when index-dependencies
      (phpinspect-project-enqueue-imports project (alist-get 'imports (cdr index))))

    (dolist (indexed-typedef (alist-get 'classes (cdr index)))
      (phpinspect-project-add-typedef project (cdr indexed-typedef) index-dependencies))

    (dolist (func (alist-get 'functions (cdr index)))
      (phpinspect-project-set-function project func))))

(cl-defmethod phpinspect-project-add-index ((_project phpinspect-project) _index)
  (cl-assert (not _index))
  (phpinspect--log "phpinspect-project-add-index: ignoring added nil index"))

(cl-defmethod phpinspect-project-set-function
  ((project phpinspect-project) (func phpinspect--function))
  (phpinspect-project-edit project
    (puthash (phpinspect--function-name-symbol func) func
             (phpinspect-project-function-index project))))

(cl-defmethod phpinspect-project-get-function
  ((project phpinspect-project) (name string))
  (phpinspect-project-get-function project (phpinspect-intern-name name)))

(cl-defmethod phpinspect-project-get-function
  ((project phpinspect-project) (name (head phpinspect-name)))
  (gethash name (phpinspect-project-function-index project)))

(cl-defmethod phpinspect-project-get-function-or-extra ((project phpinspect-project) name)
  (or (phpinspect-project-get-function project name)
      (and (phpinspect-project-extra-function-retriever project)
           (funcall (phpinspect-project-extra-function-retriever project)
                    name))))

(defun phpinspect-project-get-function-return-type (project function-name)
  (when-let ((fn (phpinspect-project-get-function-or-extra project function-name)))
    (phpinspect--function-return-type fn)))

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

(cl-defmethod phpinspect-project-delete-typedef ((project phpinspect-project) (typedef phpinspect-typedef))
  (phpinspect-project-delete-typedef project (phpi-typedef-name typedef)))

(cl-defmethod phpinspect-project-delete-typedef ((project phpinspect-project) (typedef-name phpinspect--type))
  (phpinspect-project-edit project
    (remhash (phpinspect--type-name-symbol typedef-name) (phpinspect-project-typedef-index project))))

(cl-defmethod phpinspect-project-add-typedef
  ((project phpinspect-project) (indexed-typedef (head phpinspect--indexed-class)) &optional index-dependencies)
  (phpinspect-project-edit project
    (if (not (alist-get 'class-name (cdr indexed-typedef)))
        (phpinspect--log "Error: Typedef with declaration %s does not have a name" (alist-get 'declaration indexed-typedef))
      ;; Else
      (let* ((typedef-type-name (alist-get 'class-name (cdr indexed-typedef)))
             (typedef-name (phpinspect--type-name-symbol typedef-type-name))
             (typedef (gethash typedef-name
                               (phpinspect-project-typedef-index project))))
        (unless typedef
          (setq typedef (phpinspect-make-typedef
                         typedef-type-name (phpinspect-project-make-typedef-retriever project))))

        (phpi-typedef-set-index typedef indexed-typedef)

        (when index-dependencies
          (phpinspect-project-enqueue-types project (phpi-typedef-get-dependencies typedef)))

        (puthash typedef-name typedef (phpinspect-project-typedef-index project))))))

(cl-defmethod phpinspect-project-set-typedef
  ((project phpinspect-project) (typedef-fqn phpinspect--type) (typedef phpinspect-typedef))
  (phpinspect-project-edit project
    (puthash (phpinspect--type-name-symbol typedef-fqn)
             typedef
             (phpinspect-project-typedef-index project))))

(cl-defmethod phpinspect-project-create-typedef
  ((project phpinspect-project) (typedef-fqn phpinspect--type))
  (phpinspect-project-edit project
    (let ((typedef (phpinspect-make-typedef typedef-fqn (phpinspect-project-make-typedef-retriever project))))
      (phpinspect-project-set-typedef project typedef-fqn typedef)
      typedef)))

(cl-defmethod phpinspect-project-get-typedef-create
  ((project phpinspect-project) (typedef-fqn phpinspect--type) &optional no-enqueue)
  "Get typedef object belonging to TYPEDEF-FQN from PROJECT.

If the typedef does exist on the filesystem but has not yet been
indexed, it will be queued for indexation and an empty typedef
object (awaiting indedaxation) is returned.

If NO-ENQUEUE is non-nil, the typedef will not be queued for
indexation, but indexed synchronously before returning."
  (let ((typedef (phpinspect-project-get-typedef project typedef-fqn)))
    (unless typedef
      (phpinspect-project-edit project
        (setq typedef (phpinspect-project-create-typedef project typedef-fqn))
        (unless no-enqueue
          (phpinspect-project-enqueue-if-not-present project typedef-fqn))))

    (phpinspect--log "Got project typedef, no-enqueue is set to: %s, initial-index is: %s"
                     no-enqueue (phpi-typedef-initial-index typedef))

    (phpinspect-project-edit project
      (when no-enqueue
        (phpinspect-project-ensure-index-typedef-and-dependencies project typedef)))
    typedef))

(cl-defmethod phpinspect-project-get-typedef-extra-or-create
  ((project phpinspect-project) (typedef-fqn phpinspect--type) &optional no-enqueue)
  (or (phpinspect-project-get-typedef-or-extra project typedef-fqn no-enqueue)
      (phpinspect-project-get-typedef-create project typedef-fqn no-enqueue)))

(defun phpinspect-project-ensure-index-typedef-and-dependencies (project typedef)
  (unless (phpi-typedef-initial-index typedef)
    (phpinspect-project-add-index
     project
     (phpinspect-project-index-type-file project (phpi-typedef-name typedef))))

  (unless (phpi-typedef--dependencies-loaded typedef)
    (dolist (dep (phpi-typedef-get-dependencies typedef))
      (unless (phpi-typedef-initial-index
               (phpinspect-project-get-typedef-create project dep))
        (phpinspect-project-add-index
         project
         (phpinspect-project-index-type-file project dep))))

    (dolist (extended (phpi-typedef-subscribed-to-types typedef))
      (phpinspect-project-ensure-index-typedef-and-dependencies
       project (phpinspect-project-get-typedef-create project extended)))

    (setf (phpi-typedef--dependencies-loaded typedef) t)))

(cl-defmethod phpinspect-project-get-typedef
  ((project phpinspect-project) (typedef-fqn phpinspect--type) &optional index)
  "Get indexed typedef by name of TYPEDEF-FQN stored in PROJECT."
  (let ((typedef (gethash (phpinspect--type-name-symbol typedef-fqn)
                          (phpinspect-project-typedef-index project))))
    (when typedef
      (when (and (phpinspect-project-read-only-p project)
                 (not (phpi-typedef-read-only-p typedef)))
        (setf (phpi-typedef-read-only-p typedef) t))


      (when index
        (phpinspect-project-ensure-index-typedef-and-dependencies project typedef)))
    typedef))

(cl-defmethod phpinspect-project-get-typedef-or-extra
  ((project phpinspect-project) (typedef-fqn phpinspect--type) &optional index)
  (or (phpinspect-project-get-typedef project typedef-fqn index)
      (and (phpinspect-project-extra-typedef-retriever project)
           (funcall (phpinspect-project-extra-typedef-retriever project)
                    typedef-fqn))))

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
        (if file
          (if visited-buffer
              (with-current-buffer visited-buffer (phpinspect-index-current-buffer))
            (with-temp-buffer (phpinspect-project-index-file project file)))
          (phpinspect--log "Failed to determine filepath for type %s" (phpinspect--type-name type))))
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

(defun phpinspect-project-make-typedef-retriever (project)
  (lambda (type)
    (or (phpinspect-project-get-typedef-or-extra project type)
        (phpinspect-project-get-typedef-create project type))))

(defvar phpinspect--project-type-name-history nil
  "History list to use for `phpinspect-project-read-type-name'")

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
    (phpinspect--log "Indexing typedef %s for project in %s as task."
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
               (phpinspect-project-add-index project root-index 'index-dependencies)))))))

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
