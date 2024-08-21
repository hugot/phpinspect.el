;;; phpinspect-resolve.el --- PHP parsing and completion package -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 1.1.0

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
(require 'phpinspect-cache)
(require 'phpinspect-class)
(require 'phpinspect-type)
(require 'phpinspect-token-predicates)

(phpinspect--declare-log-group 'resolve)

(cl-defstruct (phpinspect--assignment
               (:constructor phpinspect--make-assignment))
  (ctx nil
       :type phpinspect--assignment-context
       :documentation "The context that the assignment was found in")
  (to nil
      :type phpinspect-variable
      :documentation "The variable that is assigned to")
  (from nil
        :type phpinspect-token
        :documentation "The token that is assigned from"))

(defsubst phpinspect-maybe-assignment-p (token)
  "Like `phpinspect-assignment-p', but includes \"as\" barewords as possible tokens."
  (or (phpinspect-assignment-p token)
      (equal '(:word "as") token)))

(define-inline phpinspect-not-assignment-p (token)
  "Inverse of applying `phpinspect-assignment-p to TOKEN."
  (inline-quote
   (not (phpinspect-maybe-assignment-p ,token))))

(cl-defstruct (phpinspect--assignment-context
               (:constructor phpinspect--make-assignment-context)
               (:conc-name phpinspect--actx-))
  (annotations nil
               :type list
               :documentation "List of var annotations available in context")
  (tokens nil
          :type list)
  (preceding-assignments nil
                         :type list))

(defun phpinspect--find-assignment-ctxs-in-token (token &optional assignments-before var-annotations)
  (when (keywordp (car token))
    (setq token (cdr token)))

  (setq var-annotations (or var-annotations (cons nil nil)))

  (let ((statements (phpinspect--split-statements token))
        assignments blocks-or-lists)
    (dolist (statement statements)
      (phpinspect--log "Finding assignment in statement '%s'" statement)
      (when (seq-find #'phpinspect-maybe-assignment-p statement)
        (phpinspect--log "Found assignment statement")
        (push (phpinspect--make-assignment-context
               :annotations var-annotations
               :tokens statement
               :preceding-assignments assignments-before)
              assignments)
        (setq assignments-before assignments))

      ;; Find all var annotations in statement.
      (when-let* ((comments (seq-filter #'phpinspect-comment-p statement)))
        (dolist (comment comments)
          (dolist (token comment)
            (when (phpinspect-var-annotation-p token)
              ;; Intentionally destructively modify annotation list so that all
              ;; assignments have the same annotations available to them.
              (push token (cdr var-annotations))))))

      (when (setq blocks-or-lists (seq-filter #'phpinspect-block-or-list-p statement))
        (dolist (block-or-list blocks-or-lists)
          (phpinspect--log "Found block or list %s" block-or-list)
          (let ((local-assignments
                 (phpinspect--find-assignment-ctxs-in-token
                  block-or-list assignments-before var-annotations)))
            (dolist (local-assignment (nreverse local-assignments))
              (push local-assignment assignments))
            (setq assignments-before assignments)))))

    ;; return
    (phpinspect--log "Found assignments in token: %s" assignments)
    (phpinspect--log "Found statements in token: %s" statements)
    assignments))

(defun phpinspect--find-assignment-by-predicate (assignment-ctxs predicate)
  "Find first assignment in ASSIGNMENT-TOKENS matching PREDICATE.

Destructively removes tokens from the end of ASSIGNMENT-TOKENS."
  (catch 'return
    (dolist (actx assignment-ctxs)
      (let ((assignment (phpinspect--actx-tokens actx)))
        (let* ((is-loop-assignment nil)
               (left-of-assignment
                (seq-filter #'phpinspect-not-comment-p
                            (seq-take-while #'phpinspect-not-assignment-p assignment)))
               (right-of-assignment
                (seq-filter
                 #'phpinspect-not-comment-p
                 (cdr (seq-drop-while
                       (lambda (elt)
                         (if (phpinspect-maybe-assignment-p elt)
                             (progn
                               (when (equal '(:word "as") elt)
                                 (phpinspect--log "It's a loop assignment %s" elt)
                                 (setq is-loop-assignment t))
                               nil)
                           t))
                       assignment)))))

          (if is-loop-assignment
              (when (funcall predicate right-of-assignment)
                ;; Masquerade as an array access assignment
                (setq left-of-assignment (append left-of-assignment '((:array))))
                (throw 'return (phpinspect--make-assignment :to right-of-assignment
                                                            :from left-of-assignment
                                                            :ctx actx)))
            (when (funcall predicate left-of-assignment)
              (throw 'return (phpinspect--make-assignment :from right-of-assignment
                                                          :to left-of-assignment
                                                          :ctx actx)))))))
      nil))

(defsubst phpinspect-drop-preceding-barewords (statement)
  (while (and statement (phpinspect-word-p (cadr statement)))
    (pop statement))
  statement)

(defsubst phpinspect-get-cached-project-class (rctx class-fqn)
  (phpinspect-project-get-class-or-extra (phpinspect--resolvecontext-project rctx) class-fqn))

(defun phpinspect-get-cached-project-class-methods (rctx class-fqn &optional static)
    (phpinspect--log "Getting cached project class methods for %s"
                     class-fqn)
    (let ((class (phpinspect-rctx-get-or-create-cached-project-class rctx class-fqn)))
      (when class
        (phpinspect--log "Retrieved class index, starting method collection for %s"
                         class-fqn)
        (if static
            (phpinspect--class-get-static-method-list class)
          (phpinspect--class-get-method-list class)))))

(defsubst phpinspect-get-cached-project-class-method-type (rctx class-fqn method-name)
  (let* ((class (phpinspect-rctx-get-or-create-cached-project-class rctx class-fqn))
         (method))
    (when class
      (setq method
            (phpinspect--class-get-method class (phpinspect-intern-name method-name)))
      (when method
        (phpinspect--function-return-type method)))))

(defsubst phpinspect-get-cached-project-class-variable-type
  (rctx class-fqn variable-name)
  (phpinspect--log "Getting cached project class variable type %s::%s"
                   class-fqn variable-name)
  (let ((found-variable
         (phpinspect--class-get-variable
          (phpinspect-rctx-get-or-create-cached-project-class rctx class-fqn)
          variable-name)))
    (when found-variable
      (phpinspect--variable-type found-variable))))

(defsubst phpinspect-get-cached-project-class-static-method-type
  (rctx class-fqn method-name)
  (let* ((class (phpinspect-rctx-get-or-create-cached-project-class rctx class-fqn))
         (method))
    (when class
      (setq method
            (phpinspect--class-get-static-method
             class
             (phpinspect-intern-name method-name)))
      (when method
        (phpinspect--function-return-type method)))))

(defun phpinspect-get-derived-statement-type-in-block
    (resolvecontext statement php-block type-resolver &optional function-arg-list assignments)
  "Get type of RESOLVECONTEXT subject in PHP-BLOCK.

Use TYPE-RESOLVER and FUNCTION-ARG-LIST in the process.

An example of a derived statement would be the following php code:
$variable->attribute->method();
$variable->attribute;
$variable->method();
self::method();
ClassName::method();
$variable = ClassName::method();
$variable = $variable->method();"
  (phpinspect--get-derived-statement-type-in-block
   resolvecontext statement php-block
   (or assignments (phpinspect--find-assignment-ctxs-in-token php-block))
   type-resolver function-arg-list))

(defun phpinspect--get-derived-statement-type-in-block
    (resolvecontext statement php-block assignments type-resolver &optional function-arg-list)
  "Determine the type that STATEMENT evaluates to in RESOLVECONTEXT.

PHP-BLOCK should be the block that STATEMENT was found in.
ASSIGNMENTS should be a list of assignment-contexts.

A statement is derived when it contains multiple components that
derive off a base token. Object property access is an example of
a derived statement. In the statement $foo->bar which is parsed
into ((:variable foo) (:object-attrib (:word bar))), the
value/type of ->bar must be derived from the type of $foo. So
->bar derives from the base token $foo."

    ;; A derived statement can be an assignment itself.
    (when (seq-find #'phpinspect-assignment-p statement)
      (phpinspect--log "Derived statement is an assignment: %s" statement)
      (setq statement (cdr (seq-drop-while #'phpinspect-not-assignment-p statement))))
    (phpinspect--log "Get derived statement type in block: (truncated, real length: %d) %s"
                     (length statement)
                     (take 10 statement))
    (let* ((first-token (pop statement))
           (current-token)
           (previous-attribute-type))
      ;; No first token means we were passed an empty list.
      (when (and first-token
                 (setq previous-attribute-type
                       (or
                           ;; Statements starting with a bare word can indicate a static
                           ;; method call. These could be statements with "return" or
                           ;; another bare-word at the start though, so we drop preceding
                           ;; barewords when they are present.
                           (when (phpinspect-word-p first-token)
                             (when (phpinspect-word-p (car statement))
                               (setq statement (phpinspect-drop-preceding-barewords
                                                statement))
                               (setq first-token (pop statement)))
                             (funcall type-resolver (phpinspect--make-type
                                                     :name (cadr first-token))))

                           ;; First token is a list, for example "(new DateTime())"
                           (when (phpinspect-list-p first-token)
                             (phpinspect--interpret-expression-type-in-context
                              resolvecontext php-block type-resolver (cdr first-token)
                              function-arg-list assignments))

                           ;; No bare word, assume we're dealing with a variable.
                           (when (phpinspect-variable-p first-token)
                             (phpinspect--get-variable-type-in-block
                              resolvecontext (cadr first-token) php-block assignments
                              type-resolver function-arg-list)))))

        (phpinspect--log "Statement: %s" statement)
        (phpinspect--log "Starting attribute type: %s" previous-attribute-type)
        (while (setq current-token (pop statement))
          (phpinspect--log "Current derived statement token: %s" current-token)
          (cond ((phpinspect-object-attrib-p current-token)
                 (let ((attribute-word (cadr current-token)))
                   (when (phpinspect-word-p attribute-word)
                     (if (phpinspect-list-p (car statement))
                         (progn
                           (pop statement)
                           (setq previous-attribute-type
                                 (or
                                 (phpinspect-get-cached-project-class-method-type
                                  resolvecontext
                                  (funcall type-resolver previous-attribute-type)
                                  (cadr attribute-word))
                                  previous-attribute-type)))
                       (setq previous-attribute-type
                             (or
                              (phpinspect-get-cached-project-class-variable-type
                                resolvecontext
                               (funcall type-resolver previous-attribute-type)
                               (cadr attribute-word))
                              previous-attribute-type))))))
                ((phpinspect-static-attrib-p current-token)
                 (let ((attribute-word (cadr current-token)))
                   (phpinspect--log "Found attribute word: %s" attribute-word)
                   (phpinspect--log "checking if next token is a list. Token: %s"
                                    (car statement))
                   (when (phpinspect-word-p attribute-word)
                     (if (phpinspect-list-p (car statement))
                         (progn
                           (pop statement)
                           (setq previous-attribute-type
                                 (or
                                  (phpinspect-get-cached-project-class-static-method-type
                                   resolvecontext
                                   (funcall type-resolver previous-attribute-type)
                                   (cadr attribute-word))
                                  previous-attribute-type)))))))
                ((and previous-attribute-type (phpinspect-array-p current-token))
                 (setq previous-attribute-type
                       (or (phpinspect--type-contains previous-attribute-type)
                           previous-attribute-type)))))
        (phpinspect--log "Found derived type: %s" previous-attribute-type)
        ;; Make sure to always return a FQN
        (funcall type-resolver previous-attribute-type))))

(defun phpinspect-get-variable-type-in-block
    (resolvecontext variable-name php-block type-resolver &optional function-arg-list)
  (phpinspect--get-variable-type-in-block
   resolvecontext variable-name php-block
   (phpinspect--find-assignment-ctxs-in-token php-block)
   type-resolver function-arg-list))

(defun phpinspect--get-variable-type-in-block
    (resolvecontext variable-name php-block assignments type-resolver &optional function-arg-list)
  "Find the type of VARIABLE-NAME in PHP-BLOCK using TYPE-RESOLVER.

Returns either a FQN or a relative type name, depending on
whether or not the root variable of the assignment value (right
side of assignment) can be found in FUNCTION-ARG-LIST.

When PHP-BLOCK belongs to a function, supply FUNCTION-ARG-LIST to
resolve types of function argument variables."

  (phpinspect--log "Looking for assignments of variable %s in php block" variable-name)
  (if (string= variable-name "this")
      (funcall type-resolver (phpinspect--make-type :name "self"))
    (phpinspect--get-pattern-type-in-block
     resolvecontext (phpinspect--make-pattern :m `(:variable ,variable-name))
     php-block assignments type-resolver function-arg-list)))

(defun phpinspect-get-pattern-type-in-block
    (resolvecontext pattern php-block type-resolver &optional function-arg-list)
  "Find the type of PATTERN in PHP-BLOCK using TYPE-RESOLVER.

PATTERN must be an object of the type `phpinspect--pattern'.

Returns either a FQN or a relative type name, depending on
whether or not the root variable of the assignment value (right
side of assignment) needs to be extracted from FUNCTION-ARG-LIST.

When PHP-BLOCK belongs to a function, supply FUNCTION-ARG-LIST to
resolve types of function argument variables."
  (phpinspect--get-pattern-type-from-assignments
   resolvecontext pattern php-block
   (phpinspect--find-assignment-ctxs-in-token php-block)
   type-resolver function-arg-list))

(defun phpinspect--get-pattern-type-in-block
    (resolvecontext pattern php-block assignments type-resolver &optional function-arg-list)
  (phpinspect--get-pattern-type-from-assignments
   resolvecontext pattern php-block assignments type-resolver function-arg-list))


(defun phpinspect--get-pattern-type-from-assignments
    (resolvecontext pattern php-block assignments type-resolver &optional function-arg-list)
  (let* ((last-assignment
          (phpinspect--find-assignment-by-predicate
           assignments (phpinspect--pattern-matcher pattern)))
         (last-assignment-value (when last-assignment
                                  (phpinspect--assignment-from last-assignment)))
         (pattern-code (phpinspect--pattern-code pattern))
         (result))
    (phpinspect--log "Looking for assignments of pattern %s in assignment list of length %d"
                     pattern-code (length assignments))

    (if (not last-assignment)
        (when (and (= (length pattern-code) 2) (phpinspect-variable-p (cadr pattern-code)))
          (let ((variable-name (cadadr pattern-code)))
            (progn
              (phpinspect--log "No assignments found for variable %s, checking function arguments: %s"
                               variable-name function-arg-list)
              (setq result (phpinspect-get-variable-type-in-function-arg-list
                            variable-name type-resolver function-arg-list)))))

      (setq result
            (phpinspect--interpret-expression-type-in-context
             resolvecontext php-block type-resolver
             last-assignment-value function-arg-list
             (phpinspect--actx-preceding-assignments
              (phpinspect--assignment-ctx last-assignment)))))

    (phpinspect--log "Type interpreted from last assignment expression of pattern %s: %s"
                     pattern-code result)

    ;; Unable to resolve a type from the code. When the pattern is for a
    ;; variable, attempt to find a @var annotation for the specified variable.
    (when (and (not result) last-assignment
               (and (= (phpinspect--pattern-length pattern) 1)
                    (phpinspect-variable-p (cadr pattern-code))))
      (when-let* ((annotation (phpinspect--find-var-annotation-for-variable
                                (phpinspect--actx-annotations
                                 (phpinspect--assignment-ctx last-assignment))
                               (cadadr pattern-code)))
                  (annotation-type (phpinspect-var-annotation-type annotation)))
        (setq result (funcall type-resolver (phpinspect--make-type :name annotation-type)))))

    (when (and result (phpinspect--type-collection result) (not (phpinspect--type-contains result)))
      (phpinspect--log (concat
                        "Interpreted type %s is a collection type, but 'contains'"
                        "attribute is not set. Attempting to infer type from context")
                       result)
      (setq result (phpinspect--copy-type result))
      (let ((concat-pattern
             (phpinspect--pattern-concat
              pattern (phpinspect--make-pattern :f #'phpinspect-array-p))))
        (phpinspect--log "Inferring type of concatenated pattern %s"
                         (phpinspect--pattern-code concat-pattern))
        (setf (phpinspect--type-contains result)
              (phpinspect--get-pattern-type-from-assignments
               resolvecontext concat-pattern php-block assignments
               type-resolver function-arg-list))))
    ;; return
    result))

(defun phpinspect--split-statements (tokens &optional predicate)
  "Split TOKENS into separate statements.

If PREDICATE is provided, it is used as additional predicate to
determine whether a token delimits a statement."
  (let ((sublists)
        (current-sublist))
    (dolist (thing tokens)
      (if (or (phpinspect-statement-introduction-p thing)
              (when predicate (funcall predicate thing)))
          (when current-sublist
            (when (phpinspect-block-p thing)
              (push thing current-sublist))
            (push (nreverse current-sublist) sublists)
            (setq current-sublist nil))
        (push thing current-sublist)))
    (when current-sublist
      (push (nreverse current-sublist) sublists))
    (nreverse sublists)))

(defun phpinspect-get-variable-type-in-function-arg-list (variable-name type-resolver arg-list)
  "Infer VARIABLE-NAME's type from typehints in
ARG-LIST. ARG-LIST should be a list token as returned by
`phpinspect--list-handler` (see also `phpinspect-list-p`)"
  (let ((arg-no (seq-position arg-list
                              variable-name
                              (lambda (token variable-name)
                                (and (phpinspect-variable-p token)
                                     (string= (car (last token)) variable-name))))))
    (if (and arg-no
             (> arg-no 0))
        (let ((arg (elt arg-list (- arg-no 1))))
          (if (phpinspect-word-p arg)
              (funcall type-resolver
                       (phpinspect--make-type :name (car (last arg))))
            nil)))))

(defun phpinspect--interpret-expression-type-in-context
    (resolvecontext php-block type-resolver expression &optional function-arg-list assignments)
  "Infer EXPRESSION's type from provided context.

Use RESOLVECONTEXT, PHP-BLOCK, TYPE-RESOLVER and
FUNCTION-ARG-LIST as contextual information to infer type of
EXPRESSION.

An expression can be any sequence of tokens that evaluates to a
value/type."
  (phpinspect--log "Interpreting type of expression (truncated, full-length: %s) %s"
                   (length expression)
                   (take 10 expression))

  (cond ((phpinspect-array-p (car expression))
         (let ((collection-contains)
               (collection-items (phpinspect--split-statements (cdr (car expression))))
               (count 0))
           (phpinspect--log "Checking collection items in array token of length: %d"
                            (length collection-items))
           (while (and (< count (length collection-items))
                       (not collection-contains))
             (setq collection-contains
                   (phpinspect--interpret-expression-type-in-context
                    resolvecontext php-block type-resolver
                    (elt collection-items count) function-arg-list assignments)
                   count (+ count 1)))

           (phpinspect--log "Collection contained: %s" collection-contains)

           (phpinspect--make-type :name "\\array"
                                  :fully-qualified t
                                  :collection t
                                  :contains collection-contains)))
        ((and (phpinspect-word-p (car expression))
              (string= (cadar expression) "new"))
         (funcall
          type-resolver (phpinspect--make-type :name (cadadr expression))))

        ((phpinspect--match-sequence expression
           :f #'phpinspect-word-p
           :f #'phpinspect-list-p)
         (phpinspect-rctx-get-function-return-type resolvecontext (cadar expression)))

        ((and (phpinspect-list-p (car expression))
              (= 1 (length (cdar expression)))
              (phpinspect-word-p (cadar expression)))
         ;; expression starts with "(word)", so it is a type cast. Return the
         ;; type of the cast.
         (funcall type-resolver (phpinspect--make-type :name (car (cdadar expression)))))

        ((and (> (length expression) 1)
              (seq-find (lambda (part) (or (phpinspect-attrib-p part)
                                           (phpinspect-array-p part)))
                        expression))
         (phpinspect--log "Expression is a derived statement")
         (phpinspect--get-derived-statement-type-in-block
          resolvecontext expression php-block assignments
          type-resolver function-arg-list))

        ((phpinspect-list-p (car expression))
         (phpinspect--interpret-expression-type-in-context
          resolvecontext php-block type-resolver (cdar expression)
          function-arg-list assignments))
        ;; Expression is a (chain of) assignments. The right-most subexpression
        ;; is the type it evaluates to.
        ((seq-find #'phpinspect-assignment-p expression)
         (phpinspect--interpret-expression-type-in-context
          resolvecontext php-block type-resolver
          (car (last (phpinspect--split-statements expression #'phpinspect-maybe-assignment-p)))
          function-arg-list assignments))

        ;; If the right of an assignment is just $variable;, we can check if it is a
        ;; function argument and otherwise recurse to find the type of that variable.
        ((phpinspect-variable-p (car expression))
         (phpinspect--log "Variable was assigned with the value of another variable: %s"
                          expression)
         (or (when function-arg-list
               (phpinspect-get-variable-type-in-function-arg-list
                (cadar expression)
                type-resolver function-arg-list))
             (phpinspect--get-variable-type-in-block
              resolvecontext (cadar expression) php-block assignments type-resolver function-arg-list)))))


(defun phpinspect-resolve-type-from-context (resolvecontext &optional type-resolver)
  "Resolve the type that RESOLVECONTEXT's subject evaluates to."
  ;; Subject should be a statement, not a single token.
  (when (phpinspect-probably-token-p (phpinspect--resolvecontext-subject resolvecontext))
    (setf (phpinspect--resolvecontext-subject resolvecontext)
          (list (phpinspect--resolvecontext-subject resolvecontext))))

  (unless type-resolver
    (setq type-resolver
          (phpinspect--make-type-resolver-for-resolvecontext resolvecontext)))

  (phpinspect--log "Looking for type of statement: %s in nested token"
                   (phpinspect--resolvecontext-subject resolvecontext))
  ;; Find all enclosing tokens that aren't classes. Classes do not contain variable
  ;; assignments which have effect in the current scope, which is what we're trying
  ;; to find here to infer the statement type.
  (let ((enclosing-tokens (seq-filter #'phpinspect-not-class-p
                                       (phpinspect--resolvecontext-enclosing-tokens
                                        resolvecontext)))
        (enclosing-token)
        (type))
    (while (and enclosing-tokens (not type))
      ;;(phpinspect--log "Trying to find type in %s" enclosing-token)
      (setq enclosing-token (pop enclosing-tokens))

      (setq type
            (cond ((phpinspect-namespace-p enclosing-token)
                   (phpinspect-get-derived-statement-type-in-block
                    resolvecontext
                    (phpinspect--resolvecontext-subject
                     resolvecontext)
                    (or (phpinspect-namespace-block enclosing-token)
                        enclosing-token)
                    type-resolver))
                  ((or (phpinspect-block-p enclosing-token)
                       (phpinspect-root-p enclosing-token))
                   (phpinspect-get-derived-statement-type-in-block
                    resolvecontext
                    (phpinspect--resolvecontext-subject
                     resolvecontext)
                    enclosing-token
                    type-resolver))
                  ((phpinspect-function-p enclosing-token)
                   (phpinspect-get-derived-statement-type-in-block
                    resolvecontext
                    (phpinspect--resolvecontext-subject
                     resolvecontext)
                    (phpinspect-function-block enclosing-token)
                    type-resolver
                    (phpinspect-function-argument-list enclosing-token))))))
    type))

(defun phpinspect--function-get-pattern-type (fn rctx pattern type-resolver)
  (phpinspect-get-pattern-type-in-block
   rctx pattern
   (phpinspect-function-block (phpinspect--function-token fn))
   type-resolver
   (phpinspect-function-argument-list (phpinspect--function-token fn))))


(cl-defmethod phpinspect--class-resolve-property-type
  ((class phpinspect--class) (project phpinspect-project) (property-name string) type-resolver class-token-meta)
  "Resolve type of POPERTY-NAME in the context of CLASS using
CLASS-TOKEN-META as parse result."
  (let ((pattern (phpinspect--make-pattern
                  :m `(:variable "this")
                  :m `(:object-attrib (:word ,property-name))))
        (rctx (phpinspect--make-resolvecontext :enclosing-tokens (list (phpinspect-meta-token class-token-meta))
                                               :enclosing-metadata (list class-token-meta)
                                               :project project))
        (constructor-name (phpinspect-intern-name "__construct")))

    (or
     (when-let ((constructor (phpinspect--class-get-method class constructor-name)))
       (phpinspect--function-get-pattern-type constructor rctx pattern type-resolver))
     (catch 'found
       (dolist (method (phpinspect--class-get-method-list class))
         (unless (eq constructor-name (phpinspect--function-name-symbol method))
           (when-let ((result (phpinspect--function-get-pattern-type method rctx pattern type-resolver)))
             (throw 'found result))))
       nil))))

(cl-defmethod phpinspect--class-resolve-property-type
  ((_class phpinspect--class) (_project phpinspect-project) property-name &rest _ignored)
  ;; Catch-all for cases where one attempts to resolve a nil property
  ;; name. Saves an if-statement for the caller.
  ;; Can't resolve property type when property name is nil, so we do nothing.
  (cl-assert (not property-name))
  nil)

(provide 'phpinspect-resolve)
