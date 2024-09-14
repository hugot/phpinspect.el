;;; phpinspect-index.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'phpinspect-util)
(require 'phpinspect-type)
(require 'phpinspect-token-predicates)
(require 'phpinspect-parser)

(defun phpinspect--function-from-scope (scope)
  (cond ((and (phpinspect-static-p (cadr scope))
              (phpinspect-function-p (caddr scope)))
         (caddr scope))
        ((phpinspect-function-p (cadr scope))
         (cadr scope))
        (t nil)))

(defun phpinspect--index-function-arg-list (type-resolver arg-list &optional add-used-types comment-before)
  (let ((arg-index)
        (current-token)
        param-annotation
        (arg-list (cl-copy-list arg-list)))
    (while (setq current-token (pop arg-list))
      (cond ((and (phpinspect-word-p current-token)
                  (phpinspect-variable-p (car arg-list)))
             (push (cons (cadr (pop arg-list))
                         (funcall type-resolver
                                  (phpinspect--make-type :name (cadr current-token))))
                   arg-index)
             (when add-used-types (funcall add-used-types (list (cadr current-token)))))
            ((and (phpinspect-variable-p (car arg-list))
                  comment-before
                  (setq param-annotation
                        (phpinspect--find-var-annotation-for-variable
                         comment-before (cadr (car arg-list)) #'phpinspect-param-annotation-p)))
             (push (cons (cadr (pop arg-list))
                         (funcall type-resolver
                                  (phpinspect--make-type
                                   :name (phpinspect-var-annotation-type param-annotation))))
                   arg-index)

             (when add-used-types
               (funcall add-used-types
                        (list (phpinspect-var-annotation-type param-annotation)))))
            ((phpinspect-variable-p (car arg-list))
             (push (cons (cadr (pop arg-list)) nil)
                   arg-index))))
    (nreverse arg-index)))

(defsubst phpinspect--should-prefer-return-annotation (type)
  "Returns non-nil if return annotation should supersede typehint
of TYPE, if available."
  (or (not type)
      (phpinspect--type= type phpinspect--object-type)))

(defun phpinspect--index-function-declaration (declaration type-resolver add-used-types &optional comment-before)
  (let (current name function-args return-type)
    (catch 'break
      (while (setq current (pop declaration))
        (cond ((and (phpinspect-word-p current)
                    (phpinspect-word-p (car declaration))
                    (string= "function" (cadr current)))
               (setq name (cadr (pop declaration))))
              ((phpinspect-list-p current)
               (setq function-args
                     (phpinspect--index-function-arg-list
                      type-resolver current add-used-types comment-before))

               (when (setq return-type (seq-find #'phpinspect-word-p declaration))
                 (when add-used-types
                   (funcall add-used-types (list (cadr return-type))))
                 (setq return-type (funcall type-resolver
                                            (phpinspect--make-type :name (cadr return-type)))))

               (throw 'break nil)))))

    (list name function-args return-type)))

(defun phpinspect--apply-annotation-type (annotation-type-string type type-resolver)
  "Add/modify information of a resolved TYPE based on ANNOTATION-TYPE-STRING.

Annotation type is resolved using TYPE-RESOLVER.
"
  (if (stringp annotation-type-string)
      (let ((is-collection (phpinspect--type-is-collection type)))
        (phpinspect--log "found annotation type string %s when type is %s"
                         annotation-type-string type)

        (when (string-suffix-p "[]" annotation-type-string)
          (setq is-collection t)
          (setq annotation-type-string (string-trim-right annotation-type-string "\\[\\]")))


        (cond ((phpinspect--should-prefer-return-annotation type)
               ;; Sometimes it's better to override the typehint with a more narrow annotation
               (setq type (funcall type-resolver
                                   (phpinspect--make-type :name annotation-type-string))))

              (is-collection
               ;; Collections need to have their "contains" slot set
               (phpinspect--log "Detected collection type, setting contains from annotation type '%s'"
                                annotation-type-string)
               (setf (phpinspect--type-contains type)
                     (funcall type-resolver
                              (phpinspect--make-type :name annotation-type-string)))
               (setf (phpinspect--type-collection type) t))))
    ;; else
    (phpinspect--log "Discarding invalid annotation type %s" annotation-type-string))
  type)


(defun phpinspect--index-function-from-scope (type-resolver scope comment-before &optional add-used-types namespace)
  "Index a function inside SCOPE token using phpdoc metadata in COMMENT-BEFORE.

If ADD-USED-TYPES is set, it must be a function and will be
called with a list of the types that are used within the
function (think \"new\" statements, return types etc.)."
  (phpinspect--log "Indexing function")
  (let* ((php-func (seq-find #'phpinspect-function-p scope))
         (declaration (cadr php-func))
         name type arguments throws used-types)

    (pcase-setq `(,name ,arguments ,type)
                (phpinspect--index-function-declaration
                 declaration type-resolver add-used-types comment-before))

    ;; Note: Anonymous functions should not be indexed! (or if they are, they
    ;; should at least not be visible from various UIs unless assigned to a
    ;; variable as a closure). It is up to the caller of this function to
    ;; determine the right behavior.
    (unless name (setq name "anonymous"))

    (phpinspect--log "Checking function return annotations")

    ;; @return annotation. When dealing with a collection, we want to store the
    ;; type of its members.
    (when-let* ((return-annotation-type
                 (cadadr (seq-find #'phpinspect-return-annotation-p comment-before))))
      (if type
          (phpinspect--apply-annotation-type return-annotation-type type type-resolver)
        (when (stringp return-annotation-type)
          (setq type (funcall type-resolver (phpinspect--make-type :name return-annotation-type))))))

    (when-let ((throw-annotations (seq-filter #'phpinspect-throws-annotation-p comment-before)))
      (dolist (tr throw-annotations)
        (when-let ((type (phpinspect-var-annotation-type tr)))
          (push (funcall type-resolver (phpinspect--make-type :name type)) throws)
          (push type used-types))))


    (when add-used-types
      (setq used-types (nconc used-types
                              (phpinspect--find-used-types-in-tokens
                               `(,(seq-find #'phpinspect-block-p php-func)))))
      (funcall add-used-types used-types))

    (phpinspect--log "Creating function object")
    (phpinspect--make-function
     :scope `(,(car scope))
     :token php-func
     :name (concat (if namespace (concat namespace "\\") "") name)
     :throws throws
     :return-type type
     :arguments arguments)))

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

(define-inline phpinspect-var-annotation-variable (annotation)
  "Return ANNOTATION's variable name if and only if its structure is correct."
  (inline-letevals ((variable-name (inline-quote (cadr (caddr ,annotation)))))
    (inline-quote (and (stringp ,variable-name)
                       ,variable-name))))

(define-inline phpinspect-var-annotation-type (annotation)
  "Returns ANNOTATION's variable type if and only if its structure is correct."
  (inline-letevals ((variable-type (inline-quote (cadadr ,annotation))))
    (inline-quote (and (stringp ,variable-type) ,variable-type))))

(defun phpinspect--find-var-annotation-for-variable (annotation-list variable &optional predicate)
  (catch 'return
    (dolist (annotation annotation-list)
      (when (and (or (phpinspect-var-annotation-p annotation) (and predicate (funcall predicate annotation)))
                 (phpinspect-var-annotation-variable annotation)
                 (equal (phpinspect-var-annotation-variable annotation)
                        variable))
        (throw 'return annotation)))
    nil))

(defun phpinspect--variable-type-string-from-comment (comment variable-name)
  (let* ((var-annotations (phpinspect--var-annotations-from-token comment))
         (type (if var-annotations
                   ;; Find the right annotation by variable name
                   (or (cadr (cadr (seq-find (lambda (annotation)
                                               (string= (cadr (caddr annotation)) variable-name))
                                             var-annotations)))
                       ;; Give up and just use the last one encountered
                       (cadr (cadr (car (last var-annotations))))))))
    ;; If type is not a string, the annotation is probably invalid and we should
    ;; return nil.
    (when (stringp type) type)))

(defun phpinspect--index-variable-from-scope (type-resolver scope comment-before &optional static add-used-types)
  "Index the variable inside SCOPE, use doc in COMMENT-BEFORE if missing typehint.

Provide STATIC if the variable was defined as static.

SCOPE should be a scope token (`phpinspect-scope-p')."
  (let (readonly)
    (when (and (phpinspect-word-p (cadr scope))
               (equal (cadr scope) `(:word "readonly")))
      (setq readonly t)
      (setcdr scope (cddr scope)))

    (setq scope (take 5 (seq-filter #'phpinspect-not-comment-p scope)))
    (let (variable-name type)
      (cond
       ((phpinspect--match-sequence (take 3 scope)
          ;; Sequence: scope-type, typehint, variable [ = value ]
          :m * :f #'phpinspect-word-p :f #'phpinspect-variable-p)
        (setq variable-name (cadr (nth 2 scope)))
        (setq type (cadr (nth 1 scope))))
       ((phpinspect--match-sequence (take 2 scope)
          ;; Sequence: variable [ = value ]
          :m * :f #'phpinspect-variable-p)
        (setq variable-name (cadr (nth 1 scope))
              ;; Since no typehint is available, attempt extracting one from a
              ;; docstring.
              type (phpinspect--variable-type-string-from-comment
                    comment-before variable-name)))
       (t
        (setq variable-name (cadr (seq-find #'phpinspect-variable-p scope)))))

      (when add-used-types
        (funcall add-used-types (phpinspect--find-used-types-in-tokens scope)))

      (phpinspect--make-variable
       ;; Static class variables are always prefixed with dollar signs when
       ;; referenced.
       :readonly readonly
       :name (if static (concat "$" variable-name) variable-name)
       :scope `(,(car scope))
       :lifetime (when static '(:static))
       :type (when type
               (when add-used-types (funcall add-used-types (list type)))
               (phpinspect--apply-annotation-type
                (phpinspect--variable-type-string-from-comment comment-before variable-name)
                (funcall type-resolver (phpinspect--make-type :name type))
                type-resolver))))))

(defun phpinspect-doc-block-p (token)
  (phpinspect-token-type-p token :doc-block))

(defsubst phpinspect--index-method-annotations (type-resolver comment)
  (let ((annotations (seq-filter #'phpinspect-method-annotation-p comment))
        (static-methods)
        (methods))
    (dolist (annotation annotations)
      (let ((return-type) (name) (arg-list) (static))
        ;; Annotation is static
        (when (phpinspect--match-sequence annotation
                :m :method-annotation :m '(:word "static") :rest *)
          (setcdr annotation (cddr annotation))
          (setq static t))

        (cond
         ;; Sequence is: return-type, method-name, method signature
         ((phpinspect--match-sequence annotation
            :m :method-annotation
            :f #'phpinspect-word-p
            :f #'phpinspect-word-p
            :f #'phpinspect-list-p
            :rest *)
          (setq return-type (cadr (nth 1 annotation)))
          (setq name (cadr (nth 2 annotation)))
          (setq arg-list (nth 3 annotation)))
         ;; Sequence is: method-name, method-signature
         ((phpinspect--match-sequence annotation
            :m :method-annotation
            :f #'phpinspect-word-p
            :f #'phpinspect-list-p
            :rest *)
          (setq return-type "void")
          (setq name (cadr (nth 1 annotation)))
          (setq arg-list (nth 2 annotation))))

        (when name
            (push (phpinspect--make-function
                   :scope '(:public)
                   :name name
                   :return-type (funcall type-resolver (phpinspect--make-type :name return-type))
                   :arguments (phpinspect--index-function-arg-list type-resolver arg-list))
                  (if static static-methods methods)))))
    (list static-methods methods)))

(defun phpinspect--index-class (imports type-resolver location-resolver class &optional doc-block)
  "Create an alist with relevant attributes of a parsed class."
  (phpinspect--log "INDEXING CLASS")
  (let ((methods)
        (trait-config)
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

                   ((seq-find #'phpinspect-variable-p token)
                    (push (phpinspect--index-variable-from-scope
                           type-resolver token comment-before nil add-used-types)
                          variables))

                   ((phpinspect-static-p (cadr token))
                    (cond ((phpinspect-function-p (cadadr token))
                           (push (phpinspect--index-function-from-scope type-resolver
                                                                        (list (car token)
                                                                              (cadadr token))
                                                                        comment-before
                                                                        add-used-types)
                                 static-methods))

                          ((seq-find #'phpinspect-variable-p (cdadr token))
                           (push (phpinspect--index-variable-from-scope
                                  type-resolver `(,(car token) ,@(cdadr token))
                                  comment-before 'static add-used-types)
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

                   ((seq-find #'phpinspect-variable-p (cdr token))
                    (push (phpinspect--index-variable-from-scope
                           type-resolver `(:public ,@(cdr token)) comment-before
                           'static add-used-types)
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
             (setq comment-before token))

            ;; Prevent comments from sticking around too long
            ((and (phpinspect-use-p token) (phpinspect-word-p (cadr token)))
             ;; Trait use statement
             (setq trait-config
                   (nconc trait-config
                          (phpinspect--index-trait-use token type-resolver add-used-types))))
            (t
             (setq comment-before nil))))

    ;; Dirty hack that assumes the constructor argument names to be the same as the object
    ;; attributes' names.
    ;;;
    ;; TODO: actually check the types of the variables assigned to object properties
    ;;
    ;; Note: The indexing code in phpinspect-buffer does check the variable
    ;; types which are actually assigned to object properties for classes opened
    ;; in buffers.
    (let* ((constructor-sym (phpinspect-intern-name "__construct"))
           (constructor (seq-find (lambda (method)
                                    (eq (phpinspect--function-name-symbol method)
                                        constructor-sym))
                                  methods)))
      (when constructor
        (phpinspect--log "Constructor was found for %s" class-name)
        (dolist (variable variables)
          (when (not (phpinspect--variable-type variable))
            (phpinspect--log "Looking for variable type in constructor arguments (%s)"
                             variable)
            (let ((constructor-parameter-type
                   (phpinspect--function-argument-type
                         constructor (phpinspect--variable-name variable))))
              (if constructor-parameter-type
                  (setf (phpinspect--variable-type variable)
                        (funcall type-resolver constructor-parameter-type))))))))

    ;; Add method annotations to methods
    (when doc-block
      (let ((result-tuple (phpinspect--index-method-annotations type-resolver doc-block)))
        (setq static-methods
              (nconc static-methods (car result-tuple)))
        (setq methods
              (nconc methods (cadr result-tuple)))))

    `(,class-name .
                  (phpinspect--indexed-class
                   (complete . ,(not (phpinspect-incomplete-class-p class)))
                   (trait-config . ,trait-config)
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

(defun phpinspect--index-namespace (namespace type-resolver-factory location-resolver)
  (let* (used-types
         (imports (phpinspect--uses-to-types (seq-filter #'phpinspect-use-p namespace)))
         (index
          `((classes . ,(phpinspect--index-classes-in-tokens
                         imports
                         namespace
                         type-resolver-factory location-resolver (cadadr namespace)))
            (functions . ,(phpinspect--index-functions-in-tokens
                           namespace
                           type-resolver-factory
                           (phpinspect--uses-to-types (seq-filter #'phpinspect-use-p namespace))
                           (cadadr namespace)
                           (lambda (types) (setq used-types (nconc used-types (mapcar #'phpinspect-intern-name types)))))))))

    (let (class-names)
      (dolist (class (alist-get 'classes index))
        (push (car class) class-names)
        (setq used-types (nconc used-types (alist-get 'used-types class))))

      (setq used-types (seq-uniq used-types #'eq))

      (push `(namespaces . ((,(cadadr namespace) . ((location  . ,(funcall location-resolver namespace))
                                                    (imports . ,imports)
                                                    (used-types . ,used-types)
                                                    (classes . ,class-names)))))
            index)
    index)))

(defun phpinspect--index-namespaces
    (namespaces type-resolver-factory location-resolver &optional indexed)
  (if namespaces
      (let ((namespace-index
             (phpinspect--index-namespace
              (pop namespaces) type-resolver-factory location-resolver)))

        (if indexed
            (progn
              (nconc (alist-get 'namespaces indexed)
                     (alist-get 'namespaces namespace-index))
              (nconc (alist-get 'classes indexed)
                     (alist-get 'classes namespace-index))
              (nconc (alist-get 'functions indexed)
                     (alist-get 'functions namespace-index)))
          (setq indexed namespace-index))
        (phpinspect--index-namespaces
         namespaces type-resolver-factory location-resolver indexed))
    indexed))



(defun phpinspect--index-functions-in-tokens
    (tokens type-resolver-factory &optional imports namespace add-used-types type-resolver)
  "Index functions in TOKENS."
  (setq type-resolver (or type-resolver (funcall type-resolver-factory imports nil namespace)))
  (let (comment-before functions function)
    (dolist (token tokens)
      (cond ((phpinspect-comment-p token)
             (setq comment-before token))
            ((phpinspect-function-p token)
             (setq function (phpinspect--index-function-from-scope
                             type-resolver `(:public ,token) comment-before add-used-types
                             namespace))
             (unless (phpinspect--function-anonymous-p function)
             (push function functions)))
            ((phpinspect-block-or-list-p token)
             (dolist (fn (phpinspect--index-functions-in-tokens
                          (cdr token) type-resolver-factory imports namespace
                          add-used-types type-resolver))
               (unless (phpinspect--function-anonymous-p fn)
                 (push fn functions))))))

    functions))

(defun phpinspect-function-declaration (function-token)
  (seq-find #'phpinspect-declaration-p function-token))

(defun phpinspect--find-used-types-in-function-token (token)
  (let* (types
         (add-types (lambda (add-types)
                      (setq types (nconc types add-types))))
         (resolver (phpinspect--make-type-resolver nil)))

    (phpinspect--index-function-declaration
     (phpinspect-function-declaration token) resolver add-types)

    (nconc types (phpinspect--find-used-types-in-tokens (phpinspect-function-block token)))))


(defun phpinspect--find-used-types-in-tokens (tokens)
  "Find usage of bareword types in TOKENS.

Covers usage of types:
- with the \"new\" keyword
- as function argument/return types
- Types used in annotations (@var, @throws etc.)

see `phpinspect--index-class' for indexation of types used in
classes (like property typehints).

Returns a list of type name strings."
  (let* ((previous-tokens)
         (used-types (cons nil nil))
         (used-types-rear used-types))
    (while tokens
      (let ((token (pop tokens))
            (previous-token (car previous-tokens)))

        (cond ((and (phpinspect-word-p previous-token)
                    (member (cadr previous-token) `("new" "instanceof"))
                    (phpinspect-word-p token))
               (let ((type (cadr token)))
                 (when (not (string-match-p "\\\\" type))
                   (setq used-types-rear (setcdr used-types-rear (cons type nil))))))
              ((phpinspect-comment-p token)
               (setq used-types-rear
                     (nconc used-types-rear (phpinspect--find-used-types-in-tokens (cdr token)))))
              ((and (or (phpinspect-var-annotation-p token)
                        (phpinspect-param-annotation-p token)
                        (phpinspect-throws-annotation-p token))
                    (phpinspect-var-annotation-type token))
               (setq used-types-rear
                     (setcdr used-types-rear (cons (phpinspect-var-annotation-type token) nil))))

              ((and (phpinspect-list-p token)
                    (phpinspect-word-p (cadr token))
                    (or
                     ;; type cast
                     (= 1 (length (cdr token)))
                     ;; Try/catch
                     (and (phpinspect-word-p previous-token)
                          (string= "catch" (cadr previous-token)))))
               (setq used-types-rear (setcdr used-types-rear (cons (cadadr token) nil))))
              ((and (phpinspect-static-attrib-p token)
                    (phpinspect-word-p previous-token))
               (let ((type (cadr previous-token)))
                 (when (not (string-match-p "\\\\" type))
                   (setq used-types-rear (setcdr used-types-rear (cons type nil))))))
              ((phpinspect-object-attrib-p token)
               (let ((lists (seq-filter #'phpinspect-list-p token)))
                 (dolist (list lists)
                   (setq used-types-rear
                         (nconc used-types-rear
                                (phpinspect--find-used-types-in-tokens (cdr list)))
                         used-types-rear (last used-types-rear)))))
              ((phpinspect-function-p token)
               (setq used-types-rear
                     (nconc used-types-rear (phpinspect--find-used-types-in-function-token token))
                     used-types-rear
                     (nconc used-types-rear
                            (phpinspect--find-used-types-in-tokens (cdr (phpinspect-function-block token))))
                     used-types-rear (last used-types-rear)))
              ((or (phpinspect-list-p token)
                   (phpinspect-block-p token)
                   (phpinspect-array-p token)
                   (phpinspect-scope-p token)
                   (phpinspect-static-p token)
                   (phpinspect-const-p token))
               (setq used-types-rear
                     (nconc used-types-rear (phpinspect--find-used-types-in-tokens (cdr token)))
                     used-types-rear (last used-types-rear))))
        (push token previous-tokens)))
    (cdr used-types)))

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
             (namespaces . ,(alist-get 'namespaces namespace-index))
             (classes ,@(append
                         (alist-get 'classes namespace-index)
                         (phpinspect--index-classes-in-tokens
                          imports tokens type-resolver-factory location-resolver)))
             (used-types ,@(mapcar #'phpinspect-intern-name
                                   (seq-uniq
                                    (append
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

(cl-defmethod phpinspect-index-get-class
  ((index (head phpinspect--root-index)) (class-name phpinspect--type))
  (alist-get class-name (alist-get 'classes index)
             nil nil #'phpinspect--type=))

(defun phpinspect-index-current-buffer ()
  "Index a PHP file for classes and the methods they have"
  (phpinspect--index-tokens (phpinspect-parse-current-buffer)))


(defun phpinspect--index-trait-use (token type-resolver add-used-types)
  (cl-assert (phpinspect-use-p token))
  (setq token (cdr (seq-filter #'phpinspect-not-comment-p token)))

  (let ((block? (car (last token)))
        used-types config)
    (when (phpinspect-end-of-statement-p block?)
      (setq token (butlast token)))

    (setq token (seq-filter #'phpinspect-not-comma-p token))

    (dolist (word token)
      (when (phpinspect-word-p word)
        (push (cadr word) used-types)
        (push `(,(funcall type-resolver (phpinspect--make-type :name (cadr word))))
              config)))

    (when (phpinspect-block-p block?)
      (setq block? (cdr (seq-filter #'phpinspect-not-comment-p block?)))

      (while block?
        (cond ((phpinspect-comma-p (car block?))
               (pop block?))
              ;; Override
              ((phpinspect--match-sequence block?
                 :f #'phpinspect-word-p
                 :f #'phpinspect-static-attrib-p
                 :m '(:word "insteadof")
                 :f #'phpinspect-word-p
                 :rest *)
               (let* ((type (funcall type-resolver (phpinspect--make-type :name (cadr (nth 3 block?)))))
                      (t-config (assoc type config #'phpinspect--type=)))
                 (when t-config
                   (push `(override ,(cadadr (cadr block?))
                                    ,(funcall type-resolver
                                              (phpinspect--make-type :name (cadar block?))))
                         (cdr t-config))))

               (setq block? (nthcdr 4 block?)))

              ;; alias
              ((phpinspect--match-sequence block?
                 :f #'phpinspect-word-p
                 :f #'phpinspect-static-attrib-p
                 :m '(:word "as")
                 :f #'phpinspect-word-p
                 :rest *)
               (let* ((type (funcall type-resolver (phpinspect--make-type :name (cadar block?))))
                      (t-config (assoc type config #'phpinspect--type=)))

                 (when t-config
                   (push `(alias ,(cadadr (cadr block?)) ,(cadr (nth 3 block?)))
                         (cdr t-config))))

               (setq block? (nthcdr 4 block?)))
              (t (pop block?)))))

    (when add-used-types
      (funcall add-used-types used-types))
    config))

(provide 'phpinspect-index)
;;; phpinspect-index.el ends here
