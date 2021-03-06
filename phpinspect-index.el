;;; phpinspect-index.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'phpinspect-util)
(require 'phpinspect-project)
(require 'phpinspect-type)

(defun phpinspect--function-from-scope (scope)
  (cond ((and (phpinspect-static-p (cadr scope))
              (phpinspect-function-p (caddr scope)))
         (caddr scope))
        ((phpinspect-function-p (cadr scope))
         (cadr scope))
        (t nil)))

(defun phpinspect-var-annotation-p (token)
  (phpinspect-token-type-p token :var-annotation))

(defun phpinspect-return-annotation-p (token)
  (phpinspect-token-type-p token :return-annotation))

(defun phpinspect--index-function-arg-list (type-resolver arg-list)
  (let ((arg-index)
        (current-token)
        (arg-list (cl-copy-list arg-list)))
    (while (setq current-token (pop arg-list))
      (cond ((and (phpinspect-word-p current-token)
                  (phpinspect-variable-p (car arg-list)))
             (push `(,(cadr (pop arg-list))
                     ,(funcall type-resolver (phpinspect--make-type :name (cadr current-token))))
                   arg-index))
            ((phpinspect-variable-p (car arg-list))
             (push `(,(cadr (pop arg-list))
                     nil)
                   arg-index))))
    (nreverse arg-index)))

(defsubst phpinspect--should-prefer-return-annotation (type)
  "When the return annotation should be preferred over typehint of TYPE, if available."
  (or (not type)
      (phpinspect--type= type phpinspect--object-type)))

(defun phpinspect--index-function-from-scope (type-resolver scope comment-before)
  (let* ((php-func (cadr scope))
         (declaration (cadr php-func))
         (type (if (phpinspect-word-p (car (last declaration)))
                   (funcall type-resolver
                            (phpinspect--make-type :name (cadar (last declaration)))))))

    ;; @return annotation. When dealing with a collection, we want to store the
    ;; type of its members.
    (let* ((is-collection
            (when type
              (member (phpinspect--type-name type) phpinspect-collection-types)))
           (return-annotation-type
            (when (or (phpinspect--should-prefer-return-annotation type) is-collection)
              (cadadr
               (seq-find #'phpinspect-return-annotation-p
                         comment-before)))))
      (phpinspect--log "found return annotation %s when type is %s"
                       return-annotation-type
                       type)

      (when return-annotation-type
        (cond ((phpinspect--should-prefer-return-annotation type)
               (setq type (funcall type-resolver
                                   (phpinspect--make-type :name return-annotation-type))))
              (is-collection
               (phpinspect--log "Detected collection type in: %s" scope)
               (setf (phpinspect--type-contains type)
                     (funcall type-resolver
                              (phpinspect--make-type :name return-annotation-type)))
               (setf (phpinspect--type-collection type) t)))))

    (phpinspect--make-function
     :scope `(,(car scope))
     :name (cadadr (cdr declaration))
     :return-type (if type (funcall type-resolver type)
                    phpinspect--null-type)
     :arguments (phpinspect--index-function-arg-list
                 type-resolver
                 (phpinspect-function-argument-list php-func)))))

(defun phpinspect--index-const-from-scope (scope)
  (phpinspect--make-variable
   :scope `(,(car scope))
   :name (cadr (cadr (cadr scope)))))

(defun phpinspect--var-annotations-from-token (token)
  (seq-filter #'phpinspect-var-annotation-p token))

(defun phpinspect--index-variable-from-scope (type-resolver scope comment-before)
  "Index the variable inside `scope`."
  (let* ((var-annotations (phpinspect--var-annotations-from-token comment-before))
         (variable-name (cadr (cadr scope)))
         (type (if var-annotations
                   ;; Find the right annotation by variable name
                   (or (cadr (cadr (seq-find (lambda (annotation)
                                               (string= (cadr (caddr annotation)) variable-name))
                                             var-annotations)))
                       ;; Give up and just use the last one encountered
                       (cadr (cadr (car (last var-annotations))))))))
    (phpinspect--log "calling resolver from index-variable-from-scope")
    (phpinspect--make-variable
     :name variable-name
     :scope `(,(car scope))
     :type (if type (funcall type-resolver (phpinspect--make-type :name type))))))

(defun phpinspect-doc-block-p (token)
  (phpinspect-token-type-p token :doc-block))

(defun phpinspect--get-class-name-from-token (class-token)
  (let ((subtoken (seq-find (lambda (word)
                              (and (phpinspect-word-p word)
                                   (not (string-match
                                         (concat "^" (phpinspect-handler-regexp 'class-keyword))
                                         (concat (cadr word) " ")))))
                            (cadr class-token))))
    (cadr subtoken)))

(defun phpinspect--index-class (imports type-resolver class)
  "Create an alist with relevant attributes of a parsed class."
  (phpinspect--log "INDEXING CLASS")
  (let ((methods)
        (static-methods)
        (static-variables)
        (variables)
        (constants)
        (extends)
        (implements)
        (class-name (phpinspect--get-class-name-from-token class))
        ;; Keep track of encountered comments to be able to use type
        ;; annotations.
        (comment-before))

    ;; Find out what the class extends or implements
    (let ((enc-extends nil)
          (enc-implements nil))
      (dolist (word (cadr class))
        (if (phpinspect-word-p word)
            (cond ((string= (cadr word) "extends")
                   (phpinspect--log "Class %s extends other classes" class-name)
                   (setq enc-extends t))
                  ((string= (cadr word) "implements")
                   (setq enc-extends nil)
                   (phpinspect--log "Class %s implements in interface" class-name)
                   (setq enc-implements t))
                  (t
                   (phpinspect--log "Calling Resolver from index-class on %s" (cadr word))
                   (cond (enc-extends
                          (push (funcall type-resolver (phpinspect--make-type
                                                        :name (cadr word)))
                                extends))
                         (enc-implements
                          (push (funcall type-resolver (phpinspect--make-type
                                                        :name (cadr word)))
                                implements))))))))

    (dolist (token (caddr class))
      (cond ((phpinspect-scope-p token)
             (cond ((phpinspect-const-p (cadr token))
                    (push (phpinspect--index-const-from-scope token) constants))

                   ((phpinspect-variable-p (cadr token))
                    (push (phpinspect--index-variable-from-scope type-resolver
                                                                 token
                                                                 comment-before)
                          variables))

                   ((phpinspect-static-p (cadr token))
                    (cond ((phpinspect-function-p (cadadr token))
                           (push (phpinspect--index-function-from-scope type-resolver
                                                                        (list (car token)
                                                                              (cadadr token))
                                                                        comment-before)
                                 static-methods))

                          ((phpinspect-variable-p (cadadr token))
                           (push (phpinspect--index-variable-from-scope type-resolver
                                                                        (list (car token)
                                                                              (cadadr token))
                                                                        comment-before)
                                 static-variables))))
                   (t
                    (phpinspect--log "comment-before is: %s" comment-before)
                    (push (phpinspect--index-function-from-scope type-resolver
                                                                 token
                                                                 comment-before)
                          methods))))
            ((phpinspect-static-p token)
             (cond ((phpinspect-function-p (cadr token))
                    (push (phpinspect--index-function-from-scope type-resolver
                                                                 `(:public
                                                                   ,(cadr token))
                                                                 comment-before)
                          static-methods))

                   ((phpinspect-variable-p (cadr token))
                    (push (phpinspect--index-variable-from-scope type-resolver
                                                                 `(:public
                                                                   ,(cadr token))
                                                                 comment-before)
                          static-variables))))
            ((phpinspect-const-p token)
             ;; Bare constants are always public
             (push (phpinspect--index-const-from-scope (list :public token))
                   constants))
            ((phpinspect-function-p token)
             ;; Bare functions are always public
             (push (phpinspect--index-function-from-scope type-resolver
                                                          (list :public token)
                                                          comment-before)
                   methods))
            ((phpinspect-doc-block-p token)
             (phpinspect--log "setting comment-before %s" token)
             (setq comment-before token))

            ;; Prevent comments from sticking around too long
            (t
             (phpinspect--log "Unsetting comment-before")
             (setq comment-before nil))))

    ;; Dirty hack that assumes the constructor argument names to be the same as the object
    ;; attributes' names.
    ;;;
    ;; TODO: actually check the types of the variables assigned to object attributes
    (let* ((constructor-sym (phpinspect-intern-name "__construct"))
           (constructor (seq-find (lambda (method)
                                    (eq (phpinspect--function-name-symbol method)
                                        constructor-sym))
                                  methods)))
      (when constructor
        (phpinspect--log "Constructor was found")
        (dolist (variable variables)
          (when (not (phpinspect--variable-type variable))
            (phpinspect--log "Looking for variable type in constructor arguments (%s)"
                             variable)
            (let ((constructor-parameter-type
                   (car (alist-get (phpinspect--variable-name variable)
                                   (phpinspect--function-arguments constructor)
                                   nil nil #'string=))))
              (if constructor-parameter-type
                  (setf (phpinspect--variable-type variable)
                        (funcall type-resolver constructor-parameter-type))))))))

    (let ((class-name (funcall type-resolver (phpinspect--make-type :name class-name))))
      `(,class-name .
                    (phpinspect--indexed-class
                     (class-name . ,class-name)
                     (imports . ,imports)
                     (methods . ,methods)
                     (static-methods . ,static-methods)
                     (static-variables . ,static-variables)
                     (variables . ,variables)
                     (constants . ,constants)
                     (extends . ,extends)
                     (implements . ,implements))))))

(defsubst phpinspect-namespace-body (namespace)
  "Return the nested tokens in NAMESPACE tokens' body.
Accounts for namespaces that are defined with '{}' blocks."
  (if (phpinspect-block-p (caddr namespace))
      (cdaddr namespace)
    (cdr namespace)))

(defun phpinspect--index-classes (imports classes &optional namespace indexed)
  "Index the class tokens in `classes`, using the imports in `imports`
as Fully Qualified names. `namespace` will be assumed the root
namespace if not provided"
  (if classes
      (let ((class (pop classes)))
        (push (phpinspect--index-class
               imports
               (phpinspect--make-type-resolver imports class namespace)
               class)
              indexed)
        (phpinspect--index-classes imports classes namespace indexed))
    (nreverse indexed)))

(defun phpinspect--use-to-type (use)
  (let* ((fqn (cadr (cadr use)))
         (type (phpinspect--make-type :name (if (string-match "^\\\\" fqn)
                                                fqn
                                              (concat "\\" fqn))
                                      :fully-qualified t))
         (type-name (if (and (phpinspect-word-p (caddr use))
                             (string= "as" (cadr (caddr use))))
                        (cadr (cadddr use))
                      (progn (string-match "[^\\]+$" fqn)
                             (match-string 0 fqn)))))
    (cons (phpinspect-intern-name type-name) type)))

(defun phpinspect--uses-to-types (uses)
  (mapcar #'phpinspect--use-to-type uses))

(defun phpinspect--index-namespace (namespace)
  (phpinspect--index-classes
   (phpinspect--uses-to-types (seq-filter #'phpinspect-use-p namespace))
   (seq-filter #'phpinspect-class-p namespace)
   (cadadr namespace)))

(defun phpinspect--index-namespaces (namespaces &optional indexed)
  (if namespaces
      (progn
        (push (phpinspect--index-namespace (pop namespaces)) indexed)
        (phpinspect--index-namespaces namespaces indexed))
    (apply #'append (nreverse indexed))))

(defun phpinspect--index-functions (&rest _args)
  "TODO: implement function indexation. This is a stub function.")

(defun phpinspect--index-tokens (tokens)
  "Index TOKENS as returned by `phpinspect--parse-current-buffer`."
  (let ((imports (phpinspect--uses-to-types (seq-filter #'phpinspect-use-p tokens))))
    `(phpinspect--root-index
      (imports . ,imports)
      ,(append
        (append '(classes)
                (phpinspect--index-namespaces (seq-filter #'phpinspect-namespace-p tokens))
                (phpinspect--index-classes
                 imports
                 (seq-filter #'phpinspect-class-p tokens))))
      (functions))
    ;; TODO: Implement function indexation
    ))

(defun phpinspect-index-file (file-name)
  (phpinspect--index-tokens (phpinspect-parse-file file-name)))

(defun phpinspect-get-or-create-cached-project-class (project-root class-fqn)
  (when project-root
    (let ((project (phpinspect--cache-get-project-create
                    (phpinspect--get-or-create-global-cache)
                    project-root)))
      (phpinspect--project-get-class-create project class-fqn))))

(defun phpinspect--index-current-buffer ()
  (phpinspect--index-tokens (phpinspect-parse-current-buffer)))

(defun phpinspect-index-current-buffer ()
  "Index a PHP file for classes and the methods they have"
  (phpinspect--index-tokens (phpinspect-parse-current-buffer)))

(cl-defmethod phpinspect--index-type-file ((project phpinspect--project)
                                           (type phpinspect--type))
  (condition-case error
      (let* ((class-file (with-temp-buffer
                           (cd (phpinspect--project-root project))
                           (phpinspect-type-filepath type)))
             (visited-buffer (when class-file (find-buffer-visiting class-file)))
             (new-index)
             (class-index))
        (when class-file
          (if visited-buffer
              (with-current-buffer visited-buffer (phpinspect--index-current-buffer))
            (phpinspect-index-file class-file))))
    (file-missing
     (phpinspect--log "Failed to find file for type %s:  %s" type error)
     nil)))

(defsubst phpinspect--index-thread-enqueue (task)
  (phpinspect--queue-enqueue-noduplicate phpinspect--index-queue
                                         task
                                         #'phpinspect--index-task=))

(provide 'phpinspect-index)
;;; phpinspect-index.el ends here
