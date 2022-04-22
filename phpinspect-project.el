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
indexed classes in the project"))

(cl-defgeneric phpinspect--project-add-class
    ((project phpinspect--project) (class (head phpinspect--indexed-class)))
  "Add an indexed CLASS to PROJECT.")

(cl-defmethod phpinspect--project-add-class
  ((project phpinspect--project) (indexed-class (head phpinspect--indexed-class)))
  (let* ((class-name (phpinspect--type-name-symbol
                      (alist-get 'class-name (cdr indexed-class))))
         (existing-class (gethash class-name
                                  (phpinspect--project-class-index project))))
    (if existing-class
        (phpinspect--class-set-index existing-class indexed-class)
      (let ((new-class (phpinspect--make-class-generated :project project)))
        (phpinspect--class-set-index new-class indexed-class)
        (puthash class-name new-class (phpinspect--project-class-index project))))))

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

      (let* ((class-file (phpinspect-class-filepath class-fqn))
             (visited-buffer (when class-file (find-buffer-visiting class-file)))
             (new-index)
             (class-index))

        (phpinspect--log "No existing index for FQN: %s" class-fqn)
        (phpinspect--log "filepath: %s" class-file)
        (when class-file
          (if visited-buffer
              (setq new-index (with-current-buffer visited-buffer
                                (phpinspect--index-current-buffer)))
            (setq new-index (phpinspect-index-file class-file)))
          (setq class-index
                (alist-get class-fqn (alist-get 'classes new-index)
                           nil
                           nil
                           #'phpinspect--type=))
          (when class-index
            (phpinspect--class-set-index class class-index)))))
    class))

(cl-defmethod phpinspect--project-get-class
  ((project phpinspect--project) (class-fqn phpinspect--type))
  (gethash (phpinspect--type-name-symbol class-fqn)
           (phpinspect--project-class-index project)))

(provide 'phpinspect-project)
;;; phpinspect-project.el ends here
