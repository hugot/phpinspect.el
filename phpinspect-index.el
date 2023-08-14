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
(require 'phpinspect-parser)

(defun phpinspect--function-from-scope (scope)
  (cond ((and (phpinspect-static-p (cadr scope))
              (phpinspect-function-p (caddr scope)))
         (caddr scope))
        ((phpinspect-function-p (cadr scope))
         (cadr scope))
        (t nil)))

(defun phpinspect--index-function-arg-list (type-resolver arg-list &optional add-used-types)
  (let ((arg-index)
        (current-token)
        (arg-list (cl-copy-list arg-list)))
    (while (setq current-token (pop arg-list))
      (cond ((and (phpinspect-word-p current-token)
                  (phpinspect-variable-p (car arg-list)))
             (push `(,(cadr (pop arg-list))
                     ,(funcall type-resolver (phpinspect--make-type :name (cadr current-token))))
                   arg-index)
             (when add-used-types (funcall add-used-types (list (cadr current-token)))))
            ((phpinspect-variable-p (car arg-list))
             (push `(,(cadr (pop arg-list))
                     nil)
                   arg-index))))
    (nreverse arg-index)))

(defsubst phpinspect--should-prefer-return-annotation (type)
  "Returns non-nil if return annotation should supersede typehint
of TYPE, if available."
  (or (not type)
      (phpinspect--type= type phpinspect--object-type)))

(defun phpinspect--index-function-from-scope (type-resolver scope comment-before &optional add-used-types namespace)
  "Index a function inside SCOPE token using phpdoc metadata in COMMENT-BEFORE.

If ADD-USED-TYPES is set, it must be a function and will be
called with a list of the types that are used within the
function (think \"new\" statements, return types etc.)."
  (phpinspect--log "Indexing function")
  (let* ((php-func (cadr scope))
         (declaration (cadr php-func))
         (type (if (phpinspect-word-p (car (last declaration)))
                   (funcall type-resolver
                            (phpinspect--make-type :name (cadar (last declaration)))))))

    (phpinspect--log "Checking function return annotations")

    ;; @return annotation. When dealing with a collection, we want to store the
    ;; type of its members.
    (let* ((return-annotation-type
            (cadadr (seq-find #'phpinspect-return-annotation-p comment-before)))
           (is-collection
            (and type
                 (phpinspect--type-is-collection type))))
      (phpinspect--log "found return annotation %s in %s when type is %s"
                       return-annotation-type comment-before type)

      (unless (stringp return-annotation-type)
        (phpinspect--log "Discarding invalid return annotation type %s" return-annotation-type)
        (setq return-annotation-type nil))

      (when return-annotation-type
        (when (string-suffix-p "[]" return-annotation-type)
          (setq is-collection t)
          (setq return-annotation-type (string-trim-right return-annotation-type "\\[\\]")))

        (cond ((phpinspect--should-prefer-return-annotation type)
               (setq type (funcall type-resolver
                                   (phpinspect--make-type :name return-annotation-type))))
              (is-collection
               (phpinspect--log "Detected collection type in: %s" scope)
               (setf (phpinspect--type-contains type)
                     (funcall type-resolver
                              (phpinspect--make-type :name return-annotation-type)))
               (setf (phpinspect--type-collection type) t)))))

    (when add-used-types
      (let ((used-types (phpinspect--find-used-types-in-tokens
                         `(,(seq-find #'phpinspect-block-p php-func)))))
        (when type (push (phpinspect--type-bare-name type) used-types))
        (funcall add-used-types used-types)))

    (phpinspect--log "Creating function object")
    (phpinspect--make-function
     :scope `(,(car scope))
     :token php-func
     :name (concat (if namespace (concat namespace "\\") "") (cadadr (cdr declaration)))
     :return-type (or type phpinspect--null-type)
     :arguments (phpinspect--index-function-arg-list
                 type-resolver
                 (phpinspect-function-argument-list php-func)
                 add-used-types))))

(define-inline phpinspect--safe-cadr (list)
  (inline-letevals (list)
    (inline-quote
     (when (listp ,list) (cadr ,list)))))

(defun phpinspect--index-const-from-scope (scope)
  (phpinspect--make-variable
   :scope `(,(car scope))
   :mutability `(,(caadr scope))
   :name (phpinspect--safe-cadr (phpinspect--safe-cadr (phpinspect--safe-cadr scope)))))

(defun phpinspect--var-annotations-from-token (token)
  (seq-filter #'phpinspect-var-annotation-p token))

(defun phpinspect--index-variable-from-scope (type-resolver scope comment-before &optional static)
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
     ;; Static class variables are always prefixed with dollar signs when
     ;; referenced.
     :name (if static (concat "$" variable-name) variable-name)
     :scope `(,(car scope))
     :lifetime (when static '(:static))
     :type (if type (funcall type-resolver (phpinspect--make-type :name type))))))

(defun phpinspect-doc-block-p (token)
  (phpinspect-token-type-p token :doc-block))

(defun phpinspect--get-class-name-from-token (class-token)
  (let ((subtoken (seq-find (lambda (word)
                              (and (phpinspect-word-p word)
                                   (not (string-match
                                         (concat "^" (phpinspect-handler-regexp class-keyword))
                                         (concat (cadr word) " ")))))
                            (cadr class-token))))
    (cadr subtoken)))


(defsubst phpinspect--index-method-annotations (type-resolver comment)
  (let ((annotations (seq-filter #'phpinspect-method-annotation-p comment))
        (methods))
    (dolist (annotation annotations)
      (let ((return-type) (name) (arg-list))
        (when (> (length annotation) 2)
          (cond ((and (phpinspect-word-p (nth 1 annotation))
                      (phpinspect-word-p (nth 2 annotation))
                      (phpinspect-list-p (nth 3 annotation)))
                 (setq return-type (cadr (nth 1 annotation)))
                 (setq name (cadr (nth 2 annotation)))
                 (setq arg-list (nth 3 annotation)))
                ((and (phpinspect-word-p (nth 1 annotation))
                      (phpinspect-list-p (nth 2 annotation)))
                 (setq return-type "void")
                 (setq name (cadr (nth 1 annotation)))
                 (setq arg-list (nth 2 annotation))))

          (when name
            (push (phpinspect--make-function
                   :scope '(:public)
                   :name name
                   :return-type (funcall type-resolver (phpinspect--make-type :name return-type))
                   :arguments (phpinspect--index-function-arg-list type-resolver arg-list))
                  methods)))))
    methods))

(defun phpinspect--index-class-declaration (decl type-resolver)
  ;; Find out what the class extends or implements
  (let (encountered-extends encountered-implements encountered-class
        class-name extends implements used-types)
    (dolist (word decl)
      (if (phpinspect-word-p word)
          (cond ((string= (cadr word) "extends")
                 (phpinspect--log "Class %s extends other classes" class-name)
                 (setq encountered-extends t))
                ((string= (cadr word) "implements")
                 (setq encountered-extends nil)
                 (phpinspect--log "Class %s implements in interface" class-name)
                 (setq encountered-implements t))
                ((string= (cadr word) "class")
                 (setq encountered-class t))
                (t
                 (phpinspect--log "Calling Resolver from index-class on %s" (cadr word))
                 (cond (encountered-extends
                        (push (funcall type-resolver (phpinspect--make-type
                                                      :name (cadr word)))
                              extends)
                        (push (cadr word) used-types))
                       (encountered-implements
                        (push (funcall type-resolver (phpinspect--make-type
                                                      :name (cadr word)))
                              implements)
                        (push (cadr word) used-types))
                       (encountered-class
                        (setq class-name (funcall type-resolver (phpinspect--make-type :name (cadr word)))
                              encountered-class nil)))))))

    (list class-name extends implements used-types)))


(defun phpinspect--index-class (imports type-resolver location-resolver class &optional doc-block)
  "Create an alist with relevant attributes of a parsed class."
  (phpinspect--log "INDEXING CLASS")
  (let ((methods)
        (static-methods)
        (static-variables)
        (variables)
        (constants)
        (extends)
        (implements)
        (class-name)
        ;; Keep track of encountered comments to be able to use type
        ;; annotations.
        (comment-before)
        ;; The types that are used within the code of this class' methods.
        (used-types)
        (add-used-types))
    (setq add-used-types
          (lambda (additional-used-types)
            (if used-types
                (nconc used-types additional-used-types)
              (setq used-types additional-used-types))))

    (pcase-setq `(,class-name ,extends ,implements ,used-types)
                (phpinspect--index-class-declaration (cadr class) type-resolver))


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
                                                                        comment-before
                                                                        add-used-types)
                                 static-methods))

                          ((phpinspect-variable-p (cadadr token))
                           (push (phpinspect--index-variable-from-scope type-resolver
                                                                        (list (car token)
                                                                              (cadadr token))
                                                                        comment-before
                                                                        'static)
                                 static-variables))))
                   (t
                    (phpinspect--log "comment-before is: %s" comment-before)
                    (push (phpinspect--index-function-from-scope type-resolver
                                                                 token
                                                                 comment-before
                                                                 add-used-types)
                          methods))))
            ((phpinspect-static-p token)
             (cond ((phpinspect-function-p (cadr token))
                    (push (phpinspect--index-function-from-scope type-resolver
                                                                 `(:public
                                                                   ,(cadr token))
                                                                 comment-before
                                                                 add-used-types)
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
                                                          comment-before
                                                          add-used-types)
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

    ;; Add method annotations to methods
    (when doc-block
      (setq methods
            (nconc methods (phpinspect--index-method-annotations type-resolver doc-block))))

    `(,class-name .
                  (phpinspect--indexed-class
                   (complete . ,(not (phpinspect-incomplete-class-p class)))
                   (class-name . ,class-name)
                   (declaration . ,(seq-find #'phpinspect-declaration-p class))
                   (location . ,(funcall location-resolver class))
                   (imports . ,imports)
                   (methods . ,methods)
                   (static-methods . ,static-methods)
                   (static-variables . ,static-variables)
                   (variables . ,variables)
                   (constants . ,constants)
                   (extends . ,extends)
                   (implements . ,implements)
                   (used-types . ,(mapcar #'phpinspect-intern-name
                                          (seq-uniq used-types #'string=)))))))

(defsubst phpinspect-namespace-body (namespace)
  "Return the nested tokens in NAMESPACE tokens' body.
Accounts for namespaces that are defined with '{}' blocks."
  (if (phpinspect-block-p (caddr namespace))
      (cdaddr namespace)
    (cdr namespace)))

(defun phpinspect--index-classes-in-tokens
    (imports tokens type-resolver-factory location-resolver &optional namespace)
  "Index the class tokens among TOKENS.

NAMESPACE will be assumed the root namespace if not provided"
  (let ((comment-before)
        (indexed))
    (dolist (token tokens)
      (cond ((phpinspect-doc-block-p token)
             (setq comment-before token))
            ((phpinspect-class-p token)
             (push (phpinspect--index-class
                    imports (funcall type-resolver-factory imports token namespace)
                    location-resolver token comment-before)
                   indexed)
             (setq comment-before nil))))
    indexed))

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

(defun phpinspect--index-namespace (namespace type-resolver-factory location-resolver)
  (let* (used-types
         (index
          `((classes . ,(phpinspect--index-classes-in-tokens
                         (phpinspect--uses-to-types (seq-filter #'phpinspect-use-p namespace))
                         namespace
                         type-resolver-factory location-resolver (cadadr namespace)))
            (functions . ,(phpinspect--index-functions-in-tokens
                           namespace
                           type-resolver-factory
                           (phpinspect--uses-to-types (seq-filter #'phpinspect-use-p namespace))
                           (cadadr namespace)
                           (lambda (types) (setq used-types (nconc used-types types))))))))

    (push `(used-types . ,used-types) index)))

(defun phpinspect--index-namespaces
    (namespaces type-resolver-factory location-resolver &optional indexed)
  (if namespaces
      (let ((namespace-index
             (phpinspect--index-namespace
              (pop namespaces) type-resolver-factory location-resolver)))

        (if indexed
            (progn
              (nconc (alist-get 'used-types indexed)
                     (alist-get 'used-types namespace-index))
              (nconc (alist-get 'classes indexed)
                     (alist-get 'classes namespace-index))
              (nconc (alist-get 'functions indexed)
                     (alist-get 'functions namespace-index)))
          (setq indexed namespace-index))
        (phpinspect--index-namespaces
         namespaces type-resolver-factory location-resolver indexed))
    indexed))

(defun phpinspect--index-functions-in-tokens (tokens type-resolver-factory &optional imports namespace add-used-types)
  "TODO: implement function indexation. This is a stub function."
  (let ((type-resolver (funcall type-resolver-factory imports nil namespace))
        comment-before functions)
    (dolist (token tokens)
      (cond ((phpinspect-comment-p token)
             (setq comment-before token))
            ((phpinspect-function-p token)
             (push (phpinspect--index-function-from-scope
                    type-resolver `(:public ,token) comment-before add-used-types
                    namespace)
                   functions))))
    functions))

(defun phpinspect--find-used-types-in-tokens (tokens)
  "Find usage of the \"new\" keyword in TOKENS.

Return value is a list of the types that are \"newed\"."
  (let ((previous-tokens)
        (used-types))
    (while tokens
      (let ((token (pop tokens))
            (previous-token (car previous-tokens)))
        (cond ((and (phpinspect-word-p previous-token)
                    (string= "new" (cadr previous-token))
                    (phpinspect-word-p token))
               (let ((type (cadr token)))
                 (when (not (string-match-p "\\\\" type))
                   (push type used-types))))
              ((and (phpinspect-static-attrib-p token)
                    (phpinspect-word-p previous-token))
               (let ((type (cadr previous-token)))
                 (when (not (string-match-p "\\\\" type))
                   (push type used-types))))
              ((phpinspect-object-attrib-p token)
               (let ((lists (seq-filter #'phpinspect-list-p token)))
                 (dolist (list lists)
                   (setq used-types (append (phpinspect--find-used-types-in-tokens (cdr list))
                                            used-types)))))
              ((or (phpinspect-list-p token) (phpinspect-block-p token))
               (setq used-types (append (phpinspect--find-used-types-in-tokens (cdr token))
                                        used-types))))

        (push token previous-tokens)))
    used-types))

(defun phpinspect--index-tokens (tokens &optional type-resolver-factory location-resolver)
  "Index TOKENS as returned by `phpinspect--parse-current-buffer`."
  (or
   (condition-case-unless-debug err
       (progn
         (unless type-resolver-factory
           (setq type-resolver-factory #'phpinspect--make-type-resolver))

         (unless location-resolver
           (setq location-resolver (lambda (_) (list 0 0))))

         (let* ((imports (phpinspect--uses-to-types (seq-filter #'phpinspect-use-p tokens)))
                (namespace-index
                 (phpinspect--index-namespaces (seq-filter #'phpinspect-namespace-p tokens)
                                               type-resolver-factory
                                               location-resolver)))
           `(phpinspect--root-index
             (imports . ,imports)
             (classes ,@(append
                         (alist-get 'classes namespace-index)
                         (phpinspect--index-classes-in-tokens
                          imports tokens type-resolver-factory location-resolver)))
             (used-types ,@(mapcar #'phpinspect-intern-name
                                   (seq-uniq
                                    (append
                                     (alist-get 'used-types namespace-index)
                                     (phpinspect--find-used-types-in-tokens tokens))
                                    #'string=)))
             (functions . ,(append
                            (alist-get 'functions namespace-index)
                            (phpinspect--index-functions-in-tokens
                             tokens type-resolver-factory imports))))))
     (t
      (phpinspect--log "phpinspect--index-tokens failed: %s. Enable debug-on-error for backtrace." err)
      nil))
   '(phpinspect--root-index)))

(defun phpinspect-get-or-create-cached-project-class (project-root class-fqn)
  (when project-root
    (let ((project (phpinspect--cache-get-project-create
                    (phpinspect--get-or-create-global-cache)
                    project-root)))
      (phpinspect-project-get-class-create project class-fqn))))


(cl-defmethod phpinspect-index-get-class
  ((index (head phpinspect--root-index)) (class-name phpinspect--type))
  (alist-get class-name (alist-get 'classes index)
             nil nil #'phpinspect--type=))

(defun phpinspect-index-current-buffer ()
  "Index a PHP file for classes and the methods they have"
  (phpinspect--index-tokens (phpinspect-parse-current-buffer)))

(provide 'phpinspect-index)
;;; phpinspect-index.el ends here
