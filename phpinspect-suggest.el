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

(phpinspect--declare-log-group 'suggest)

(defun phpinspect-suggest-functions (rctx)
  (let* ((project (phpinspect--resolvecontext-project rctx)))
    (phpinspect-project-get-functions-with-extra project)))

(defun phpinspect-suggest-variables-at-point (resolvecontext)
  (phpinspect--log "Suggesting variables at point")

  (let ((variables (make-hash-table :test 'equal)))
    (dolist (token (phpinspect--resolvecontext-enclosing-tokens resolvecontext))
      (when (phpinspect-not-class-p token)
        (let ((token-list token)
              (potential-variable))
        (while token-list
          (setq potential-variable (pop token-list))
          (cond ((phpinspect-variable-p potential-variable)
                 (puthash (cadr potential-variable)
                          (phpinspect--make-variable :name (cadr potential-variable))
                          variables))
                ((phpinspect-function-p potential-variable)
                 (push (phpinspect-function-block potential-variable) token-list)
                 (dolist (argument (phpinspect-function-argument-list potential-variable))
                   (when (phpinspect-variable-p argument)
                     (puthash (cadr argument)
                              (phpinspect--make-variable :name (cadr argument))
                              variables))))
                ((phpinspect-block-p potential-variable)
                 (dolist (nested-token (cdr potential-variable))
                   (push nested-token token-list))))))))

    (let (variable-list)
      (dolist (var (hash-table-values variables))
        ;; Only return variables that have a name. Unnamed variables are just dollar
        ;; signs (:
        (when (phpinspect--variable-name var)
          (setf (phpinspect--variable-type var)
                (phpinspect-resolve-type-from-context
                 (phpinspect--repurpose-resolvecontext
                  resolvecontext nil `(:variable ,(phpinspect--variable-name var)))))

          (push var variable-list)))

      variable-list)))

(defun phpinspect-get-cached-project-class-methods (rctx class-fqn &optional static)
  (phpinspect--log "Getting cached project class methods for %s" class-fqn)
  (let ((class (phpinspect-rctx-get-or-create-cached-project-class rctx class-fqn 'no-enqueue)))
    (phpinspect--log (if class
                         "Retrieved class index, starting method collection %s"
                       "No class index found for %s")
                     class-fqn)
    (when class
      (if static
          (phpinspect--class-get-static-method-list class)
        (phpinspect--class-get-method-list class)))))

(defun phpinspect--get-methods-for-class
    (resolvecontext class &optional static)
  "Find all known cached methods for CLASS."
  (or (phpinspect-get-cached-project-class-methods resolvecontext class static)
      (progn (phpinspect--log "Failed to find methods for class %s :(" class) nil)))

(defun phpinspect--get-variables-for-class (rctx class-name &optional static)
  (let ((class (phpinspect-rctx-get-or-create-cached-project-class
                rctx class-name 'no-enqueue)))
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
  (phpinspect--log "Suggesting attributes at point")
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
      (phpinspect--log "Statement type: %s" statement-type)
      (when statement-type
        (let ((type (funcall type-resolver statement-type)))
          (when-let ((result
                      (append (phpinspect--get-variables-for-class
                               resolvecontext type static)
                              (funcall method-lister type))))
            (phpinspect--log "Returning attributes %s" result)
            result))))))

(provide 'phpinspect-suggest)
