;;; phpinspect-suggest.el --- PHP parsing and completion package -*- lexical-binding: t; -*-

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

(require 'phpinspect-resolvecontext)
(require 'phpinspect-resolve)
(require 'phpinspect-token-predicates)
(require 'phpinspect-type)
(require 'phpinspect-project)
(require 'phpinspect-typedef)

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
                ((phpinspect-block-or-list-p potential-variable)
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

(cl-defstruct (phpinspect-suggest-keyword
	       (:constructor phpinspect-make-suggest-keyword-generated))
  (word nil
	:type string))

(defun phpinspect-make-suggest-keyword (word)
  (phpinspect-make-suggest-keyword-generated :word word))

(defconst phpinspect-class-body-keywords (list "const" "private" "public" "protected" "static" "function")
  "Keywords that can be used within a class")

(defconst phpinspect-scope-body-keywords (list "function" "static")
  "Keywords that can be used within a statement after a scope keyword.")

(defconst phpinspect-static-body-keywords (list "function")
  "Keywords that can be used within a statement after a static keyword.")

(defun phpinspect-suggest-class-body-keywords ()
  (mapcar #'phpinspect-make-suggest-keyword phpinspect-class-body-keywords))

(defun phpinspect-suggest-scope-body-keywords ()
  (mapcar #'phpinspect-make-suggest-keyword phpinspect-scope-body-keywords))

(defun phpinspect-suggest-static-body-keywords ()
  (mapcar #'phpinspect-make-suggest-keyword phpinspect-static-body-keywords))

(defun phpinspect-suggest-types-at-point (rctx &optional instantiable)
  (let* ((resolver (phpinspect--make-type-resolver-for-resolvecontext rctx))
	 ;; FIXME: this does not include types in the stub cache
	 (types (mapcar #'cdr (phpinspect-type-resolver--types resolver)))
	 (namespace (phpinspect-type-resolver--namespace resolver)))

    ;; Include types found within the same namespace
    (when namespace
      (setq types
	    (append types
		    (phpinspect-autoloader-get-own-types-in-namespace
		     (phpinspect-project-autoload
		      (phpinspect--resolvecontext-project rctx))
		     namespace))))

    ;; Native types are not instantiable
    (if instantiable
	types
      (append types phpinspect-native-types))))

(defun phpinspect-suggest-words-at-point (rctx)
  (let ((parent (or
		 ;; When the subject is an empty keyword body, use it as
		 ;; parent. Context is right after the keyword.
		 (and-let* ((subject (phpinspect--resolvecontext-subject rctx))
			       ((phpinspect-keyword-body-p (car subject)))
			       ((length= (car subject) 1))
			       ((car subject))))
		 ;; Otherwise, use first enclosing token as usual.
		 (car (phpinspect--resolvecontext-enclosing-tokens rctx)))))
    (or (cond ((phpinspect-class-p parent)
	       (phpinspect-suggest-class-body-keywords))
	      ((phpinspect-scope-p parent)
	       (append
		(phpinspect-suggest-scope-body-keywords)
		(phpinspect-suggest-types-at-point rctx)))
	      ((phpinspect-static-p parent)
	       (phpinspect-suggest-static-body-keywords))

	      ((or
		;; Inside argument list in function declaration
		(and (phpinspect-list-p parent)
			(phpinspect-declaration-p
			 (cadr (phpinspect--resolvecontext-enclosing-tokens rctx))))
		;; Class/function declaration
		(phpinspect-declaration-p parent))
	       (phpinspect-suggest-types-at-point rctx))

	      ((and-let* ((subject (phpinspect--resolvecontext-subject rctx) )
			  ((length= subject 1))
			  ((phpinspect-new-p (car subject)))
			  ((phpinspect-suggest-types-at-point rctx 'instantiable))))))

	;; Don't complete functions inside function/class declaration
	(unless (or (phpinspect-declaration-p parent)
		    ;; No use completing function names inside comments
		    (seq-find #'phpinspect-comment-p
			      (phpinspect--resolvecontext-enclosing-tokens rctx)))
	  (append
	   (phpinspect-suggest-types-at-point rctx 'instantiable)
	  (phpinspect-suggest-functions rctx))))))

(defun phpinspect-get-cached-project-typedef-methods (rctx class-fqn &optional static)
  (phpinspect--log "Getting cached project class methods for %s" class-fqn)
  (let ((class (phpinspect-rctx-get-typedef rctx class-fqn 'no-enqueue)))
    (phpinspect--log (if class
                         "Retrieved class index, starting method collection %s"
                       "No class index found for %s")
                     class-fqn)
    (when class
      (if static
          (phpi-typedef-get-static-methods class)
        (phpi-typedef-get-methods class)))))

(defun phpinspect--get-methods-for-class
    (resolvecontext class &optional static)
  "Find all known cached methods for CLASS."
  (or (phpinspect-get-cached-project-typedef-methods resolvecontext class static)
      (progn (phpinspect--log "Failed to find methods for class %s :(" class) nil)))

(defun phpinspect--get-variables-for-class (rctx class-name &optional static)
  (let ((class (phpinspect-rctx-get-typedef rctx class-name 'no-enqueue)))
    (when class
      (if static
          (append (phpi-typedef-get-static-properties class) (phpi-typedef-get-constants class))
        (phpi-typedef-get-properties class)))))

(defun phpinspect--make-method-lister (resolvecontext &optional static)
  (lambda (fqn)
    (phpinspect--get-methods-for-class resolvecontext fqn static)))

(cl-defmethod phpinspect--candidate-scope ((candidate phpinspect--function))
  (phpinspect--function-scope candidate))

(cl-defmethod phpinspect--candidate-scope ((candidate phpinspect-method))
  (phpinspect--function-scope (phpi-method-definition candidate)))

(cl-defmethod phpinspect--candidate-scope ((candidate phpinspect--variable))
  (phpinspect--variable-scope candidate))

(cl-defmethod phpinspect--candidate-scope ((candidate phpinspect-property))
  (phpi-prop-scope candidate))


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

  (let* ((enclosing-class (seq-find #'phpinspect-class-p
                                    (phpinspect--resolvecontext-enclosing-tokens resolvecontext)))
         (type-resolver (phpinspect--make-type-resolver-for-resolvecontext
                         resolvecontext))
         (method-lister (phpinspect--make-method-lister
                         resolvecontext
                         static))
         (statement-type (phpinspect-resolve-type-from-context
                           resolvecontext
                           type-resolver static)))

    (phpinspect--log "Statement type: %s" statement-type)
    (when statement-type
      (when-let* ((type (funcall type-resolver statement-type))
                  (result
                   (append (phpinspect--get-variables-for-class
                            resolvecontext type static)
                           (funcall method-lister type))))

        ;; Filter out candidates according to scoping rules
        (if-let ((enclosing-class-name
                  (and enclosing-class
                       (phpinspect--get-class-name-from-token enclosing-class)))
                 (enclosing-type (funcall type-resolver (phpinspect--make-type :name enclosing-class-name)))
                 ((phpinspect--type= type enclosing-type)))
            ;; We're inside the class being completed. Return all possible
            ;; attributes as scoping doesn't matter.
            result
          ;; We're outside the class being completed. Return only public
          ;; attributes.
          (let (filtered)
            (dolist (candidate result)
              (when (phpinspect-public-p (phpinspect--candidate-scope candidate))
                (push candidate filtered)))
            filtered))))))

(provide 'phpinspect-suggest)
