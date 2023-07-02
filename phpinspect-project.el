;;; phpinspect-project.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(require 'phpinspect-class)
(require 'phpinspect-type)
(require 'phpinspect-fs)
(require 'filenotify)

(cl-defstruct (phpinspect-project (:constructor phpinspect--make-project))
  (class-index (make-hash-table :test 'eq :size 100 :rehash-size 40)
               :type hash-table
               :documentation
               "A `hash-table` that contains all of the currently
indexed classes in the project")
  (fs nil
      :type phpinspect-fs
      :documentation
      "The filesystem object through which this project's files
can be accessed.")
  (autoload nil
    :type phpinspect-autoload
    :documentation
    "The autoload object through which this project's type
definitions can be retrieved")
  (worker nil
          :type phpinspect-worker
          :documentation
          "The worker that this project may queue tasks for")
  (root nil
        :type string
        :documentation
        "The root directory of this project")
  (purged nil
          :type boolean
          :documentation "Whether or not the project has been purged or not.
Projects get purged when they are removed from the global cache.")
  (file-watchers (make-hash-table :test #'equal :size 10000 :rehash-size 10000)
                 :type hash-table
                 :documentation "All active file watchers in this project,
indexed by the absolute paths of the files they're watching."))

(cl-defgeneric phpinspect-project-add-class
    ((project phpinspect-project) (class (head phpinspect--indexed-class)))
  "Add an indexed CLASS to PROJECT.")

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
  (let ((watcher (file-notify-add-watch filepath '(change) callback)))
    (puthash filepath watcher (phpinspect-project-file-watchers project))))

(cl-defmethod phpinspect-project-add-return-types-to-index-queueue
  ((project phpinspect-project) methods)
  (dolist (method methods)
    (when (phpinspect--function-return-type method)
      (phpinspect-project-enqueue-if-not-present
       project
       (phpinspect--function-return-type method)))))

(cl-defmethod phpinspect-project-add-variable-types-to-index-queue
  ((project phpinspect-project) variables)
  (dolist (var variables)
    (when (phpinspect--variable-type var)
      (phpinspect-project-enqueue-if-not-present project (phpinspect--variable-type var)))))

(cl-defmethod phpinspect-project-enqueue-if-not-present
  ((project phpinspect-project) (type phpinspect--type))
  (unless (phpinspect--type-is-native type)
    (let ((class (phpinspect-project-get-class project type)))
      (when (or (not class)
                (not (or (phpinspect--class-initial-index class))))
        (when (not class)
          (setq class (phpinspect-project-create-class project type)))
        (phpinspect--log "Adding unpresent class %s to index queue" type)
        (phpinspect-worker-enqueue (phpinspect-project-worker project)
                                   (phpinspect-make-index-task project type))))))

(cl-defmethod phpinspect-project-add-class-attribute-types-to-index-queue
  ((project phpinspect-project) (class phpinspect--class))
  (phpinspect-project-add-return-types-to-index-queueue
   project
   (phpinspect--class-get-method-list class))
  (phpinspect-project-add-return-types-to-index-queueue
   project
   (phpinspect--class-get-static-method-list class))
  (phpinspect-project-add-variable-types-to-index-queue
   project
   (phpinspect--class-variables class)))

(cl-defmethod phpinspect-project-add-index
  ((project phpinspect-project) (index (head phpinspect--root-index)))
  (dolist (indexed-class (alist-get 'classes (cdr index)))
    (phpinspect-project-add-class project (cdr indexed-class))))

(cl-defmethod phpinspect-project-enqueue-imports
  ((project phpinspect-project) imports)
  (dolist (import imports)
    (when import
      (phpinspect--log "Adding import to index queue: %s" import)
      (phpinspect-project-enqueue-if-not-present project (cdr import)))))

(cl-defmethod phpinspect-project-add-class
  ((project phpinspect-project) (indexed-class (head phpinspect--indexed-class)))
  (let* ((class-name (phpinspect--type-name-symbol
                      (alist-get 'class-name (cdr indexed-class))))
         (class (gethash class-name
                         (phpinspect-project-class-index project))))
    (unless class
      (setq class (phpinspect--make-class-generated :project project)))

    (phpinspect--class-set-index class indexed-class)
    (puthash class-name class (phpinspect-project-class-index project))
    (phpinspect-project-add-class-attribute-types-to-index-queue project class)))

(cl-defmethod phpinspect-project-set-class
  ((project phpinspect-project) (class-fqn phpinspect--type) (class phpinspect--class))
  (puthash (phpinspect--type-name-symbol class-fqn)
           class
           (phpinspect-project-class-index project)))

(cl-defmethod phpinspect-project-create-class
  ((project phpinspect-project) (class-fqn phpinspect--type))
  (let ((class (phpinspect--make-class-generated :project project)))
    (phpinspect-project-set-class project class-fqn class)
    class))

(cl-defmethod phpinspect-project-get-class-create
  ((project phpinspect-project) (class-fqn phpinspect--type))
  (let ((class (phpinspect-project-get-class project class-fqn)))
    (unless class
      (setq class (phpinspect-project-create-class project class-fqn))
      (phpinspect-project-enqueue-if-not-present project class-fqn))
    class))

(defalias 'phpinspect-project-add-class-if-missing #'phpinspect-project-get-class-create)

(cl-defmethod phpinspect-project-get-class
  ((project phpinspect-project) (class-fqn phpinspect--type))
  "Get indexed class by name of CLASS-FQN stored in PROJECT."
  (gethash (phpinspect--type-name-symbol class-fqn)
           (phpinspect-project-class-index project)))

(cl-defmethod phpinspect-project-get-type-filepath
  ((project phpinspect-project) (type phpinspect--type) &optional index-new)
  "Retrieve filepath to TYPE definition file.

when INDEX-NEW is non-nil, new files are added to the index
before the search is executed."
  (let* ((autoloader (phpinspect-project-autoload project)))
    (when (eq index-new 'index-new)
      (phpinspect-autoloader-refresh autoloader))
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
             (visited-buffer (when file (find-buffer-visiting file)))
             (new-index)
             (class-index))
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

(provide 'phpinspect-project)
;;; phpinspect-project.el ends here
