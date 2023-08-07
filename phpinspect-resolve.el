;;; phpinspect-resolve.el --- PHP parsing and completion package -*- lexical-binding: t; -*-

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
(require 'phpinspect-type)
(require 'phpinspect-parser)

(cl-defstruct (phpinspect--assignment
               (:constructor phpinspect--make-assignment))
  (to nil
      :type phpinspect-variable
      :documentation "The variable that is assigned to")
  (from nil
        :type phpinspect-token
        :documentation "The token that is assigned from"))

(define-inline phpinspect-statement-introduction-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (phpinspect-return-p ,token)
         (phpinspect-end-of-statement-p ,token)
         (phpinspect-function-p ,token)))))

(defsubst phpinspect-block-or-list-p (token)
  (or (phpinspect-block-p token)
      (phpinspect-list-p token)))

(defsubst phpinspect-maybe-assignment-p (token)
  "Like `phpinspect-assignment-p', but includes \"as\" barewords as possible tokens."
  (or (phpinspect-assignment-p token)
      (equal '(:word "as") token)))

(cl-defgeneric phpinspect--find-assignments-in-token (token)
  "Find any assignments that are in TOKEN, at top level or nested in blocks"
  (when (keywordp (car token))
    (setq token (cdr token)))

  (let ((assignments)
        (blocks-or-lists)
        (statements (phpinspect--split-statements token)))
    (dolist (statement statements)
      (when (seq-find #'phpinspect-maybe-assignment-p statement)
        (phpinspect--log "Found assignment statement")
        (push statement assignments))

      (when (setq blocks-or-lists (seq-filter #'phpinspect-block-or-list-p statement))
        (dolist (block-or-list blocks-or-lists)
          (phpinspect--log "Found block or list %s" block-or-list)
          (let ((local-assignments (phpinspect--find-assignments-in-token block-or-list)))
            (dolist (local-assignment (nreverse local-assignments))
              (push local-assignment assignments))))))

    ;; return
    (phpinspect--log "Found assignments in token: %s" assignments)
    (phpinspect--log "Found statements in token: %s" statements)
    assignments))

(defsubst phpinspect-not-assignment-p (token)
  "Inverse of applying `phpinspect-assignment-p to TOKEN."
  (not (phpinspect-maybe-assignment-p token)))

(defsubst phpinspect-not-comment-p (token)
  (not (phpinspect-comment-p token)))

(defun phpinspect--find-assignments-by-predicate (token predicate)
  (let ((variable-assignments)
        (all-assignments (phpinspect--find-assignments-in-token token)))
    (dolist (assignment all-assignments)
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
              (push (phpinspect--make-assignment :to right-of-assignment
                                                 :from left-of-assignment)
                    variable-assignments))
          (when (funcall predicate left-of-assignment)
            (push (phpinspect--make-assignment :from right-of-assignment
                                               :to left-of-assignment)
                  variable-assignments)))))
    (phpinspect--log "Returning the thing %s" variable-assignments)
    (nreverse variable-assignments)))

(defsubst phpinspect-drop-preceding-barewords (statement)
  (while (and statement (phpinspect-word-p (cadr statement)))
    (pop statement))
  statement)

;; TODO: the use of this function and similar ones should be replaced with code
;; that uses locally injected project objects in stead of retrieving the project
;; object through global variables.
(defsubst phpinspect-get-cached-project-class (project-root class-fqn)
  (when project-root
    (phpinspect-project-get-class
     (phpinspect--cache-get-project-create (phpinspect--get-or-create-global-cache)
                                           project-root)
     class-fqn)))

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

(defmacro phpinspect-find-function-in-list (method-name list)
  (let ((break-sym (gensym))
        (method-name-sym (gensym)))
    `(let ((,method-name-sym (phpinspect-intern-name ,method-name)))
       (catch (quote ,break-sym)
         (dolist (func ,list)
           (when (eq (phpinspect--function-name-symbol func)
                     ,method-name-sym)
             (throw (quote ,break-sym) func)))))))

(defsubst phpinspect-get-cached-project-class-method-type
  (project-root class-fqn method-name)
    (when project-root
    (let* ((class (phpinspect-get-or-create-cached-project-class project-root class-fqn))
           (method))
      (when class
        (setq method
              (phpinspect--class-get-method class (phpinspect-intern-name method-name)))
        (when method
          (phpinspect--function-return-type method))))))

(defsubst phpinspect-get-cached-project-class-variable-type
  (project-root class-fqn variable-name)
  (phpinspect--log "Getting cached project class variable type for %s (%s::%s)"
                   project-root class-fqn variable-name)
  (when project-root
    (let ((found-variable
           (phpinspect--class-get-variable
            (phpinspect-get-or-create-cached-project-class project-root class-fqn)
            variable-name)))
      (when found-variable
        (phpinspect--variable-type found-variable)))))

(defsubst phpinspect-get-cached-project-class-static-method-type
  (project-root class-fqn method-name)
  (when project-root
    (let* ((class (phpinspect-get-or-create-cached-project-class project-root class-fqn))
           (method))
      (when class
        (setq method
              (phpinspect--class-get-static-method
               class
               (phpinspect-intern-name method-name)))
        (when method
          (phpinspect--function-return-type method))))))

(defun phpinspect-get-derived-statement-type-in-block
    (resolvecontext statement php-block type-resolver &optional function-arg-list)
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
    ;; A derived statement can be an assignment itself.
    (when (seq-find #'phpinspect-assignment-p statement)
      (phpinspect--log "Derived statement is an assignment: %s" statement)
      (setq statement (cdr (seq-drop-while #'phpinspect-not-assignment-p statement))))
    (phpinspect--log "Get derived statement type in block: %s" statement)
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

                           ;; No bare word, assume we're dealing with a variable.
                           (when (phpinspect-variable-p first-token)
                             (phpinspect-get-variable-type-in-block
                              resolvecontext
                              (cadr first-token)
                              php-block
                              type-resolver
                              function-arg-list)))))

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
                                   (phpinspect--resolvecontext-project-root
                                    resolvecontext)
                                   (funcall type-resolver previous-attribute-type)
                                   (cadr attribute-word))
                                  previous-attribute-type)))
                       (setq previous-attribute-type
                             (or
                              (phpinspect-get-cached-project-class-variable-type
                               (phpinspect--resolvecontext-project-root
                                resolvecontext)
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
                                   (phpinspect--resolvecontext-project-root
                                    resolvecontext)
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

;;;;
;; TODO: since we're passing type-resolver to all of the get-variable-type functions now,
;; we may as well always return FQNs in stead of relative type names.
;;;;
(defun phpinspect-get-variable-type-in-block
    (resolvecontext variable-name php-block type-resolver &optional function-arg-list)
  "Find the type of VARIABLE-NAME in PHP-BLOCK using TYPE-RESOLVER.

Returns either a FQN or a relative type name, depending on
whether or not the root variable of the assignment value (right
side of assignment) can be found in FUNCTION-ARG-LIST.

When PHP-BLOCK belongs to a function, supply FUNCTION-ARG-LIST to
resolve types of function argument variables."
  (phpinspect--log "Looking for assignments of variable %s in php block" variable-name)
  (if (string= variable-name "this")
      (funcall type-resolver (phpinspect--make-type :name "self"))
    (phpinspect-get-pattern-type-in-block
     resolvecontext (phpinspect--make-pattern :m `(:variable ,variable-name))
     php-block type-resolver function-arg-list)))

(defun phpinspect-get-pattern-type-in-block
    (resolvecontext pattern php-block type-resolver &optional function-arg-list)
  "Find the type of PATTERN in PHP-BLOCK using TYPE-RESOLVER.

PATTERN must be an object of the type `phpinspect--pattern'.

Returns either a FQN or a relative type name, depending on
whether or not the root variable of the assignment value (right
side of assignment) needs to be extracted from FUNCTION-ARG-LIST.

When PHP-BLOCK belongs to a function, supply FUNCTION-ARG-LIST to
resolve types of function argument variables."
  (let* ((assignments
          (phpinspect--find-assignments-by-predicate
           php-block (phpinspect--pattern-matcher pattern)))
         (last-assignment (when assignments (car (last assignments))))
         (last-assignment-value (when last-assignment
                                  (phpinspect--assignment-from last-assignment)))
         (pattern-code (phpinspect--pattern-code pattern))
         (result))
    (phpinspect--log "Looking for assignments of pattern %s in php block" pattern-code)

    (if (not assignments)
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
             last-assignment-value function-arg-list)))

    (phpinspect--log "Type interpreted from last assignment expression of pattern %s: %s"
                     pattern-code result)

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
              (phpinspect-get-pattern-type-in-block
               resolvecontext concat-pattern php-block
               type-resolver function-arg-list))))

    ; return
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
    (resolvecontext php-block type-resolver expression &optional function-arg-list)
  "Infer EXPRESSION's type from provided context.

Use RESOLVECONTEXT, PHP-BLOCK, TYPE-RESOLVER and
FUNCTION-ARG-LIST as contextual information to infer type of
EXPRESSION."

  ;; When the right of an assignment is more than $variable; or "string";(so
  ;; (:variable "variable") (:terminator ";") or (:string "string") (:terminator ";")
  ;; in tokens), we're likely working with a derived assignment like $object->method()
  ;; or $object->attributen
  (cond ((phpinspect-array-p (car expression))
         (let ((collection-contains)
               (collection-items (phpinspect--split-statements (cdr (car expression))))
               (count 0))
           (phpinspect--log "Checking collection items: %s" collection-items)
           (while (and (< count (length collection-items))
                       (not collection-contains))
             (setq collection-contains
                   (phpinspect--interpret-expression-type-in-context
                    resolvecontext php-block type-resolver
                    (elt collection-items count) function-arg-list)
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
        ((and (> (length expression) 1)
              (seq-find (lambda (part) (or (phpinspect-attrib-p part)
                                               (phpinspect-array-p part)))
                        expression))
         (phpinspect--log "Variable was assigned with a derived statement")
         (phpinspect-get-derived-statement-type-in-block
          resolvecontext expression php-block
          type-resolver function-arg-list))

        ;; If the right of an assignment is just $variable;, we can check if it is a
        ;; function argument and otherwise recurse to find the type of that variable.
        ((phpinspect-variable-p (car expression))
         (phpinspect--log "Variable was assigned with the value of another variable: %s"
                          expression)
         (or (when function-arg-list
               (phpinspect-get-variable-type-in-function-arg-list
                (cadar expression)
                type-resolver function-arg-list))
             (phpinspect-get-variable-type-in-block resolvecontext
                                                    (cadar expression)
                                                    php-block
                                                    type-resolver
                                                    function-arg-list)))))


(defun phpinspect-resolve-type-from-context (resolvecontext &optional type-resolver)
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

(provide 'phpinspect-resolve)
