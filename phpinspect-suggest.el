;;; phpinspect-suggest.el --- PHP parsing and completion package -*- lexical-binding: t; -*-

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

(require 'phpinspect-resolvecontext)
(require 'phpinspect-resolve)
(require 'phpinspect-token-predicates)
(require 'phpinspect-type)
(require 'phpinspect-project)
(require 'phpinspect-class)

(defun phpinspect-suggest-functions (rctx)
  (let* ((project (phpinspect--resolvecontext-project rctx)))
    (phpinspect-project-get-functions-with-extra project)))

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
        (phpinspect--log (if class
                             "Retrieved class index, starting method collection %s (%s)"
                           "No class index found in %s for %s")
                         project-root class-fqn)

        (when class
          (if static
              (phpinspect--class-get-static-method-list class)
            (phpinspect--class-get-method-list class))))))

(defun phpinspect--get-methods-for-class
    (resolvecontext class &optional static)
  "Find all known cached methods for CLASS."
  (or (phpinspect-get-cached-project-class-methods
       (phpinspect--resolvecontext-project-root resolvecontext)
       class static)
      (progn (phpinspect--log "Failed to find methods for class %s :(" class) nil)))

(defun phpinspect--get-variables-for-class (class-name &optional static)
  (let ((class (phpinspect-get-or-create-cached-project-class
                (phpinspect-current-project-root)
                class-name)))
    (when class
      (if static
          (append (phpinspect--class-get-static-variables class) (phpinspect--class-get-constants class))
        (phpinspect--class-get-variables class)))))

(defun phpinspect--make-method-lister (resolvecontext &optional static)
  (lambda (fqn)
    (phpinspect--get-methods-for-class resolvecontext fqn static)))

(defun phpinspect-suggest-attributes-at-point
    (resolvecontext &optional static)
  "Suggest object or class attributes at point.

RESOLVECONTEXT must be a structure of the type
`phpinspect--resolvecontext'.  The PHP type of its subject is
resolved to provide completion candidates.

If STATIC is non-nil, candidates are provided for constants,
static variables and static methods."
  ;; Strip away the existing (incomplete) attribute token. Otherwise, resolving
  ;; a type from this context while the user has already typed part of an
  ;; attribute name could return the type of an existing attribute that matches
  ;; the incomplete name. (this could for example result in methods of the type
  ;; of $this->entity to be suggested when we really want more suggestions for
  ;; attributes of the type $this like $this->entityRepository). Essentially, we
  ;; convert the subject $this->entity into $this so that only the type of $this
  ;; (or whatever comes before the attribute accessor token (-> or ::)) is
  ;; actually resolved.
  (when (phpinspect-attrib-p (car (last (phpinspect--resolvecontext-subject resolvecontext))))
    (setf (phpinspect--resolvecontext-subject resolvecontext)
          (butlast (phpinspect--resolvecontext-subject resolvecontext))))

  (let* ((type-resolver (phpinspect--make-type-resolver-for-resolvecontext
                         resolvecontext))
         (method-lister (phpinspect--make-method-lister
                         resolvecontext
                         static)))
    (let ((statement-type (phpinspect-resolve-type-from-context
                           resolvecontext
                           type-resolver)))
      (when statement-type
        (let ((type (funcall type-resolver statement-type)))
          (append (phpinspect--get-variables-for-class
                   type
                   static)
                  (funcall method-lister type)))))))

(provide 'phpinspect-suggest)
