;;; phpinspect-suggest.el --- PHP parsing and completion package -*- lexical-binding: t; -*-

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

(require 'phpinspect-resolvecontext)
(require 'phpinspect-resolve)
(require 'phpinspect-parser)
(require 'phpinspect-type)
(require 'phpinspect-project)
(require 'phpinspect-class)

(defun phpinspect-suggest-variables-at-point (resolvecontext)
  (phpinspect--log "Suggesting variables at point")
  (let ((variables))
    (dolist (token (phpinspect--resolvecontext-enclosing-tokens resolvecontext))
      (when (phpinspect-not-class-p token)
        (let ((token-list token)
              (potential-variable))
        (while token-list
          (setq potential-variable (pop token-list))
          (cond ((phpinspect-variable-p potential-variable)
                 (phpinspect--log "Pushing variable %s" potential-variable)
                 (push (phpinspect--make-variable
                        :name (cadr potential-variable)
                        :type phpinspect--null-type)
                       variables))
                ((phpinspect-function-p potential-variable)
                 (push (phpinspect-function-block potential-variable) token-list)
                 (dolist (argument (phpinspect-function-argument-list potential-variable))
                   (when (phpinspect-variable-p argument)
                     (push (phpinspect--make-variable
                            :name (cadr argument)
                            :type phpinspect--null-type)
                           variables))))
                ((phpinspect-block-p potential-variable)
                 (dolist (nested-token (cdr potential-variable))
                   (push nested-token token-list))))))))

    ;; Only return variables that have a name. Unnamed variables are just dollar
    ;; signs (:
    (seq-filter #'phpinspect--variable-name variables)))

(defun phpinspect-get-cached-project-class-methods (project-root class-fqn &optional static)
    (phpinspect--log "Getting cached project class methods for %s (%s)"
                   project-root class-fqn)
    (when project-root
      (let ((class (phpinspect-get-or-create-cached-project-class
                    project-root
                    class-fqn)))
        (when class
          (phpinspect--log "Retrieved class index, starting method collection %s (%s)"
                           project-root class-fqn)
          (if static
              (phpinspect--class-get-static-method-list class)
            (phpinspect--class-get-method-list class))))))

(defun phpinspect--get-methods-for-class
    (resolvecontext buffer-classes class &optional static)
  "Extract all possible methods for a class from `buffer-classes` and the class index.
`buffer-classes` will be preferred because their data should be
more recent"
  (let ((methods (phpinspect-get-cached-project-class-methods
                  (phpinspect--resolvecontext-project-root
                   resolvecontext)
                  class
                  static))
        (buffer-index (alist-get class buffer-classes nil nil #'phpinspect--type=)))
      (phpinspect--log "Getting methods for class (%s)" class)
      (when buffer-index
          (dolist (method (alist-get (if static 'static-methods 'methods)
                                     buffer-index))
            (push method methods)))
      (unless methods
        (phpinspect--log "Failed to find methods for class %s :(" class))
      methods))

(defun phpinspect--get-variables-for-class (buffer-classes class-name &optional static)
  (let ((class (phpinspect-get-or-create-cached-project-class
                (phpinspect-current-project-root)
                class-name)))
    (when class
      (if static
          (append (phpinspect--class-get-static-variables class) (phpinspect--class-get-constants class))
        (phpinspect--class-get-variables class)))))

(defun phpinspect--make-method-lister (resolvecontext buffer-classes &optional static)
  (lambda (fqn)
    (phpinspect--get-methods-for-class resolvecontext buffer-classes fqn static)))

(defun phpinspect-suggest-attributes-at-point
    (resolvecontext &optional static)
  "Suggest object or class attributes at point.

RESOLVECONTEXT must be a structure of the type
`phpinspect--resolvecontext'.  The PHP type of its subject is
resolved to provide completion candidates.

If STATIC is non-nil, candidates are provided for constants,
static variables and static methods."
  (let* ((buffer-index phpinspect--buffer-index)
         (buffer-classes (alist-get 'classes (cdr buffer-index)))
         (type-resolver (phpinspect--make-type-resolver-for-resolvecontext
                         resolvecontext))
         (method-lister (phpinspect--make-method-lister
                         resolvecontext
                         buffer-classes
                         static)))
    (let ((statement-type (phpinspect-resolve-type-from-context
                           resolvecontext
                           type-resolver)))
      (when statement-type
        (let ((type (funcall type-resolver statement-type)))
          (append (phpinspect--get-variables-for-class
                   buffer-classes
                   type
                   static)
                  (funcall method-lister type)))))))

(provide 'phpinspect-suggest)