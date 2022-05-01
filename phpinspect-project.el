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

(cl-defstruct (phpinspect--project (:constructor phpinspect--make-project-cache))
  (class-index (make-hash-table :test 'eq :size 100 :rehash-size 40)
               :type hash-table
               :documentation
               "A `hash-table` that contains all of the currently
indexed classes in the project")
  (worker nil
          :type phpinspect-worker
          :documentation
          "The worker that this project may queue tasks for")
  (root nil
        :type string
        :documentation
        "The root directory of this project"))

(cl-defgeneric phpinspect--project-add-class
    ((project phpinspect--project) (class (head phpinspect--indexed-class)))
  "Add an indexed CLASS to PROJECT.")

(cl-defmethod phpinspect--project-add-return-types-to-index-queueue
  ((project phpinspect--project) methods)
  (dolist (method methods)
    (when (phpinspect--function-return-type method)
      (phpinspect--project-enqueue-if-not-present
       project
       (phpinspect--function-return-type method)))))

(cl-defmethod phpinspect--project-add-variable-types-to-index-queue
  ((project phpinspect--project) variables)
  (dolist (var variables)
    (when (phpinspect--variable-type var)
      (phpinspect--project-enqueue-if-not-present project (phpinspect--variable-type var)))))

(cl-defmethod phpinspect--project-enqueue-if-not-present
  ((project phpinspect--project) (type phpinspect--type))
  (let ((class (phpinspect--project-get-class project type)))
    (when (or (not class)
              (not (or (phpinspect--class-initial-index class)
                       (phpinspect--class-index-queued class))))
      (when (not class)
        (setq class (phpinspect--project-create-class project type)))
      (phpinspect--log "Adding unpresent class %s to index queue" type)
      (setf (phpinspect--class-index-queued class) t)
      (phpinspect-worker-enqueue (phpinspect--project-worker project)
                                 (phpinspect-make-index-task project type)))))

(cl-defmethod phpinspect--project-add-class-attribute-types-to-index-queue
  ((project phpinspect--project) (class phpinspect--class))
  (phpinspect--project-add-return-types-to-index-queueue
   project
   (phpinspect--class-get-method-list class))
  (phpinspect--project-add-return-types-to-index-queueue
   project
   (phpinspect--class-get-static-method-list class))
  (phpinspect--project-add-variable-types-to-index-queue
   project
   (phpinspect--class-variables class)))

(cl-defmethod phpinspect--project-add-index
  ((project phpinspect--project) (index (head phpinspect--root-index)))
  (dolist (indexed-class (alist-get 'classes (cdr index)))
    (phpinspect--project-add-class project (cdr indexed-class))))

(cl-defmethod phpinspect--project-enqueue-imports
  ((project phpinspect--project) imports)
  (dolist (import imports)
    (when import
      (phpinspect--log "Adding import to index queue: %s" import)
      (phpinspect--project-enqueue-if-not-present project (cdr import)))))

(cl-defmethod phpinspect--project-add-class
  ((project phpinspect--project) (indexed-class (head phpinspect--indexed-class)))
  (let* ((class-name (phpinspect--type-name-symbol
                      (alist-get 'class-name (cdr indexed-class))))
         (class (gethash class-name
                         (phpinspect--project-class-index project))))
    (unless class
      (setq class (phpinspect--make-class-generated :project project)))

    (phpinspect--class-set-index class indexed-class)
    (puthash class-name class (phpinspect--project-class-index project))
    (phpinspect--project-add-class-attribute-types-to-index-queue project class)))

(cl-defgeneric phpinspect--project-get-class
    ((project phpinspect--project) (class-fqn phpinspect--type))
  "Get indexed class by name of CLASS-FQN stored in PROJECT.")

(cl-defmethod phpinspect--project-set-class
  ((project phpinspect--project) (class-fqn phpinspect--type) (class phpinspect--class))
  (puthash (phpinspect--type-name-symbol class-fqn)
           class
           (phpinspect--project-class-index project)))

(cl-defmethod phpinspect--project-create-class
  ((project phpinspect--project) (class-fqn phpinspect--type))
  (let ((class (phpinspect--make-class-generated :project project)))
    (phpinspect--project-set-class project class-fqn class)
    class))

(cl-defmethod phpinspect--project-get-class-create
  ((project phpinspect--project) (class-fqn phpinspect--type))
  (let ((class (phpinspect--project-get-class project class-fqn)))
    (unless class
      (setq class (phpinspect--project-create-class project class-fqn))
      (phpinspect--project-enqueue-if-not-present project class-fqn))
    class))

(defalias 'phpinspect--project-add-class-if-missing #'phpinspect--project-get-class-create)

(cl-defmethod phpinspect--project-get-class
  ((project phpinspect--project) (class-fqn phpinspect--type))
  (gethash (phpinspect--type-name-symbol class-fqn)
           (phpinspect--project-class-index project)))

(provide 'phpinspect-project)
;;; phpinspect-project.el ends here
