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
    (when (not (phpinspect--project-get-class project (phpinspect--function-return-type method)))
      (phpinspect--queue-enqueue-noduplicate phpinspect--index-queue
                                             (phpinspect--make-index-task
                                              (phpinspect--project-root project)
                                              (phpinspect--function-return-type method))
                                             #'phpinspect--index-task=))))

(cl-defmethod phpinspect--project-add-class
  ((project phpinspect--project) (indexed-class (head phpinspect--indexed-class)))
  (let* ((class-name (phpinspect--type-name-symbol
                      (alist-get 'class-name (cdr indexed-class))))
         (existing-class (gethash class-name
                                  (phpinspect--project-class-index project))))
    (if existing-class
        (progn
          (phpinspect--class-set-index existing-class indexed-class)
          (phpinspect--project-add-return-types-to-index-queueue
           project
           (phpinspect--class-get-method-list existing-class)))
      (let ((new-class (phpinspect--make-class-generated :project project)))
        (phpinspect--class-set-index new-class indexed-class)
        (puthash class-name new-class (phpinspect--project-class-index project))
        (phpinspect--project-add-return-types-to-index-queueue
         project
         (phpinspect--class-get-method-list new-class))))))

(cl-defgeneric phpinspect--project-get-class
    ((project phpinspect--project) (class-fqn phpinspect--type))
  "Get indexed class by name of CLASS-FQN stored in PROJECT.")

(cl-defmethod phpinspect--project-get-class-create
  ((project phpinspect--project) (class-fqn phpinspect--type))
  (let ((class (phpinspect--project-get-class project class-fqn)))
    (unless class
      (setq class (phpinspect--make-class-generated :project project))
      (puthash (phpinspect--type-name-symbol class-fqn)
               class
               (phpinspect--project-class-index project))
      (phpinspect--queue-enqueue-noduplicate
       phpinspect--index-queue
       (phpinspect--make-index-task (phpinspect--project-root project)
                                    class-fqn)
       #'phpinspect--index-task=))
    class))

(defalias 'phpinspect--project-add-class-if-missing #'phpinspect--project-get-class-create)

(cl-defmethod phpinspect--project-get-class
  ((project phpinspect--project) (class-fqn phpinspect--type))
  (gethash (phpinspect--type-name-symbol class-fqn)
           (phpinspect--project-class-index project)))

(provide 'phpinspect-project)
;;; phpinspect-project.el ends here
