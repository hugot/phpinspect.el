; phpinspect.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

;; See docstrings for documentation, starting with `phpinspect-mode'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'obarray)

;; internal dependencies
(require 'phpinspect-cache)
(require 'phpinspect-parser)
(require 'phpinspect-project)
(require 'phpinspect-util)
(require 'phpinspect-type)
(require 'phpinspect-index)
(require 'phpinspect-class)
(require 'phpinspect-worker)
(require 'phpinspect-autoload)
(require 'phpinspect-imports)
(require 'phpinspect-buffer)
(require 'phpinspect-resolvecontext)

(defvar phpinspect-auto-reindex nil
  "Whether or not phpinspect should automatically search for new
files. The current implementation is clumsy and can result in
serious performance hits. Enable at your own risk (:")

(defvar-local phpinspect--buffer-index nil
  "The result of the last successfull parse + index action
  executed by phpinspect for the current buffer")

(defvar phpinspect-cache ()
  "In-memory nested key-value store used for caching by
phpinspect")

(defvar phpinspect-insert-file-contents-function #'insert-file-contents-literally
  "Function that phpinspect uses to insert file contents into a buffer.")

(defvar phpinspect-type-filepath-function #'phpinspect-get-class-filepath
  "Function that phpinspect uses to find the filepath of a class by its FQN.")

(defvar phpinspect-project-root-file-list
  '("composer.json" "composer.lock" ".git" ".svn" ".hg")
  "List of files that could indicate a project root directory.")

(defvar phpinspect--last-completion-list nil
  "Used internally to save metadata about completion options
  between company backend calls")

(defvar phpinspect-eldoc-word-width 14
  "The maximum width of words in eldoc strings.")

(cl-defstruct (phpinspect--completion
               (:constructor phpinspect--construct-completion))
  "Contains a possible completion value with all it's attributes."
  (value nil :type string)
  (meta nil :type string)
  (annotation nil :type string)
  (kind nil :type symbol))


(cl-defgeneric phpinspect--make-completion (completion-candidate)
  "Creates a `phpinspect--completion` for a possible completion
candidate. Candidates can be indexed functions and variables.")

(cl-defmethod phpinspect--make-completion
  ((completion-candidate phpinspect--function))
  "Create a `phpinspect--completion` for COMPLETION-CANDIDATE."
  (phpinspect--construct-completion
   :value (phpinspect--function-name completion-candidate)
   :meta (concat "(" (mapconcat (lambda (arg)
                                  (concat (phpinspect--format-type-name (cadr arg)) " "
                                          "$" (if (> (length (car arg)) 8)
                                                  (truncate-string-to-width (car arg) 8 nil)
                                                (car arg))))
                                (phpinspect--function-arguments completion-candidate)
                                ", ")
                 ") "
                 (phpinspect--format-type-name (phpinspect--function-return-type completion-candidate)))
   :annotation (concat " "
                       (phpinspect--type-bare-name
                        (phpinspect--function-return-type completion-candidate)))
   :kind 'function))


(defsubst phpinspect-cache-project-class (project-root indexed-class)
  (when project-root
    (phpinspect-project-add-class
     (phpinspect--cache-get-project-create (phpinspect--get-or-create-global-cache)
                                           project-root)
     indexed-class)))

(defsubst phpinspect-get-cached-project-class (project-root class-fqn)
  (when project-root
    (phpinspect-project-get-class
     (phpinspect--cache-get-project-create (phpinspect--get-or-create-global-cache)
                                           project-root)
     class-fqn)))

(defun phpinspect-get-project-class-inherit-classes (project-root class)
  (let ((classnames `(,@(alist-get 'extends class)
                      ,@(alist-get 'implements class)))
        (classes))

    (phpinspect--log "Found inherit classes: %s" classnames)
    (while classnames
      (let ((inherit-class (phpinspect-get-or-create-cached-project-class
                            project-root
                            (pop classnames))))
        (push inherit-class classes)
        (dolist (nested-class (phpinspect-get-project-class-inherit-classes
                               project-root
                               inherit-class))
          (push nested-class classes))))

    (seq-uniq classes #'eq)))

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
           (seq-find (lambda (variable)
                       (string= (phpinspect--variable-name variable) variable-name))
                     (phpinspect--class-variables
                      (phpinspect-get-or-create-cached-project-class
                       project-root
                       class-fqn)))))
      (when found-variable
        (phpinspect--variable-type found-variable)))))

;; (defsubst phpinspect-get-cached-project-class-static-method-type
;;   (project-root class-fqn method-name)
;;   (when project-root
;;     (let* ((found-method
;;             (phpinspect-find-function-in-list
;;              method-name
;;              (phpinspect-get-cached-project-class-methods project-root class-fqn 'static))))
;;       (when found-method
;;         (phpinspect--function-return-type found-method)))))

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

(defun phpinspect-parse-file (file)
  (with-temp-buffer
    (phpinspect-insert-file-contents file)
    (phpinspect-parse-current-buffer)))

(defun phpinspect-parse-current-buffer ()
  (phpinspect-parse-buffer-until-point
   (current-buffer)
   (point-max)))

(defun phpinspect-parse-string-to-bmap (string)
  (with-temp-buffer
    (insert string)
    (let ((context (phpinspect-make-pctx :incremental t
                                         :bmap (phpinspect-make-bmap))))
      (phpinspect-with-parse-context context
        (phpinspect-parse-current-buffer))

      (phpinspect-pctx-bmap context))))

(defun phpinspect-parse-string (string)
  (with-temp-buffer
    (insert string)
    (phpinspect-parse-current-buffer)))

(defun phpinspect--split-statements (tokens &optional predicate)
  "Split TOKENS into separate statements.

If PREDICATE is provided, it is used as additional predicate to
determine whether a token delimits a statement."
  (let ((sublists)
        (current-sublist))
    (dolist (thing tokens)
      (if (or (phpinspect-end-of-statement-p thing)
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

(defun phpinspect-eldoc-function ()
  "An `eldoc-documentation-function` implementation for PHP files.

Ignores `eldoc-argument-case` and `eldoc-echo-area-use-multiline-p`.

TODO:
 - Respect `eldoc-echo-area-use-multiline-p`
 - This function is too big and has repetitive code. Split up and simplify.
"
  (catch 'phpinspect-parse-interrupted
    (let* ((token-map (phpinspect-buffer-parse-map phpinspect-current-buffer))
           (resolvecontext (phpinspect-get-resolvecontext token-map (point)))
           (parent-token (car (phpinspect--resolvecontext-enclosing-tokens
                               resolvecontext)))
           (enclosing-token (cadr (phpinspect--resolvecontext-enclosing-tokens
                                   resolvecontext)))
           (statement (phpinspect--resolvecontext-subject resolvecontext))
           (arg-list)
           (type-resolver (phpinspect--make-type-resolver-for-resolvecontext
                           resolvecontext))
           (static))

      (phpinspect--log  "Eldoc statement before checking outside list: %s" statement)
      (when (and (phpinspect-list-p parent-token) enclosing-token)
        (setq statement
              (phpinspect-find-statement-before-point
               token-map (phpinspect-bmap-token-meta token-map enclosing-token)
               (phpinspect-meta-end
                (phpinspect-bmap-token-meta token-map parent-token)))))

      (phpinspect--log "Enclosing token: %s" enclosing-token)
      (phpinspect--log  "Eldoc statement: %s" statement)

      (setq arg-list (seq-find #'phpinspect-list-p (reverse statement)))

      (when (and (phpinspect-list-p arg-list)
                 enclosing-token
                 (or (phpinspect-object-attrib-p (car (last statement 2)))
                     (setq static (phpinspect-static-attrib-p (car (last statement 2))))))

        ;; Set resolvecontext subject to the last statement in the enclosing token, minus
        ;; the method name. The last enclosing token is an incomplete list, so point is
        ;; likely to be at a location inside a method call like "$a->b->doSomething(". The
        ;; resulting subject would be "$a->b".
        (setf (phpinspect--resolvecontext-subject resolvecontext)
              (phpinspect--get-last-statement-in-token (butlast statement 2)))

        (let* ((type-of-previous-statement
                (phpinspect-resolve-type-from-context resolvecontext type-resolver))
               (method-name-sym (phpinspect-intern-name (cadr (cadar (last statement 2)))))
               (class (phpinspect-project-get-class-create
                       (phpinspect--cache-get-project-create
                        (phpinspect--get-or-create-global-cache)
                        (phpinspect--resolvecontext-project-root resolvecontext))
                       type-of-previous-statement))
               (method (when class
                         (if static
                             (phpinspect--class-get-static-method class method-name-sym)
                           (phpinspect--class-get-method class method-name-sym)))))
          (phpinspect--log "Eldoc method name: %s" method-name-sym)
          (phpinspect--log "Eldoc type of previous statement: %s"
                           type-of-previous-statement)
          (phpinspect--log "Eldoc method: %s" method)
          (when method
            (let ((arg-count -1)
                  (comma-count
                   (length (seq-filter #'phpinspect-comma-p arg-list))))
              (concat (truncate-string-to-width
                       (phpinspect--function-name method) phpinspect-eldoc-word-width) ": ("
                       (mapconcat
                        (lambda (arg)
                          (setq arg-count (+ arg-count 1))
                          (if (= arg-count comma-count)
                              (propertize (concat
                                           "$"
                                           (truncate-string-to-width
                                            (car arg)
                                            phpinspect-eldoc-word-width)
                                           " "
                                           (phpinspect--format-type-name (or (cadr arg) "")))
                                          'face 'eldoc-highlight-function-argument)
                            (concat "$"
                                    (truncate-string-to-width (car arg)
                                                              phpinspect-eldoc-word-width)
                                    (if (cadr arg) " " "")
                                    (phpinspect--format-type-name (or (cadr arg) "")))))
                        (phpinspect--function-arguments method)
                        ", ")
                       "): "
                       (phpinspect--format-type-name
                        (phpinspect--function-return-type method))))))))))

(cl-defstruct (phpinspect--assignment
               (:constructor phpinspect--make-assignment))
  (to nil
      :type phpinspect-variable
      :documentation "The variable that is assigned to")
  (from nil
        :type phpinspect-token
        :documentation "The token that is assigned from"))

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
                           (phpinspect-get-variable-type-in-block
                            resolvecontext
                            (cadr first-token)
                            php-block
                            type-resolver
                            function-arg-list))))

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


(defun phpinspect-resolve-type-from-context (resolvecontext type-resolver)
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


(defun phpinspect--get-variables-for-class (buffer-classes class-name &optional static)
  (let ((class (phpinspect-get-or-create-cached-project-class
                (phpinspect-current-project-root)
                class-name)))
    ;; TODO return static variables/constants when static is set
    (when class
      (phpinspect--class-variables class))))

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

(defun phpinspect-after-change-function (start end pre-change-length)
  (when phpinspect-current-buffer
    (phpinspect-buffer-register-edit phpinspect-current-buffer start end pre-change-length)))

(defun phpinspect--init-mode ()
  "Initialize the phpinspect minor mode for the current buffer."
  (setq phpinspect-current-buffer (phpinspect-make-buffer :buffer (current-buffer)))
  (add-hook 'after-change-functions #'phpinspect-after-change-function)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends #'phpinspect-company-backend)


  (set (make-local-variable 'eldoc-documentation-function)
       #'phpinspect-eldoc-function)

  (make-local-variable 'eldoc-message-commands)
  (eldoc-add-command 'c-electric-paren)
  (eldoc-add-command 'c-electric-backspace)

  (phpinspect-ensure-worker)
  (phpinspect--after-save-action)

  (add-hook 'after-save-hook #'phpinspect--after-save-action nil 'local))

(defun phpinspect--after-save-action ()
  "This is intended to be run every time a phpinspect buffer is saved.

It indexes the entire buffer and updates
`phpinspect--buffer-index'.  The buffer index is merged into the
project-wide index (stored in `phpinspect-cache') afterwards.
Assuming that files are only changed from within Emacs, this
keeps the cache valid.  If changes are made outside of Emacs,
users will have to use \\[phpinspect-purge-cache]."
  (when (and (boundp 'phpinspect-mode) phpinspect-mode)
    (setq phpinspect--buffer-index
          (phpinspect--index-tokens
           (phpinspect-buffer-reparse phpinspect-current-buffer)))
    (let ((imports (alist-get 'imports phpinspect--buffer-index))
          (project (phpinspect--cache-get-project-create
                    (phpinspect--get-or-create-global-cache)
                    (phpinspect-current-project-root))))

      (dolist (class (alist-get 'classes phpinspect--buffer-index))
        (when class
          (phpinspect-project-add-class project (cdr class))

          (let ((imports (alist-get 'imports (cdr class))))
            (when imports
              (phpinspect-project-enqueue-imports project imports)))))


      (when imports (phpinspect-project-enqueue-imports project imports)))))


(defun phpinspect--disable-mode ()
  "Clean up the buffer environment for the mode to be disabled."
  (setq phpinspect-current-buffer nil)
  (kill-local-variable 'phpinspect--buffer-project)
  (kill-local-variable 'phpinspect--buffer-index)
  (kill-local-variable 'company-backends)
  (kill-local-variable 'eldoc-documentation-function)
  (kill-local-variable 'eldoc-message-commands))

(defun phpinspect--mode-function ()
  (if (and (boundp 'phpinspect-mode) phpinspect-mode)
      (phpinspect--init-mode)
    (phpinspect--disable-mode)))

(define-minor-mode phpinspect-mode
  "A minor mode for intelligent completion for and interaction
with PHP files.

To initially index a project, use M-x `phpinspect-index-current-project'
in a buffer of one of the project files. Project root is detected with
`phpinspect-project-root-file-list'.

For completion see the company-mode backend:
`phpinspect-company-backend'.

For eldoc see `phpinspect-eldoc-function'.

For finding/opening class files see
 `phpinspect-find-own-class-file' (bound to \\[phpinspect-find-own-class-file]) and
 `phpinspect-find-class-file' (bound to \\[phpinspect-find-class-file]).

To automatically add missing use statements for used classes to a
visited file, use `phpinspect-fix-imports'
(bound to \\[phpinspect-fix-imports]].)

Example configuration:

  (defun my-php-personal-hook ()
    ;; Assuming you already have company-mode enabled, these settings
    ;; add some IDE-like flair to it. This is of course not required, do
    ;; with it what you like.
    (setq-local company-minimum-prefix-length 0)
    (setq-local company-tooltip-align-annotations t)
    (setq-local company-idle-delay 0.1)

    ;; If you don't have company-mode enabled by default, uncomment this line:
    ;; (company-mode)

    ;; By default, phpinspect-mode adds itself as a backend to
    ;; the `company-backends' of the current buffer. You can completely
    ;; disable all other backends with the statement below.
    (setq-local company-backends '(phpinspect-company-backend))

    ;; Shortcut to add use statements for classes you use.
    (define-key php-mode-map (kbd \"C-c u\") 'phpinspect-fix-imports)

    ;; Shortcuts to quickly search/open files of PHP classes.
    ;; You can make these local to php-mode, but making them global
    ;; like this makes them work in other modes/filetypes as well, which
    ;; can be handy when jumping between templates, config files and PHP code.
    (global-set-key (kbd \"C-c a\") 'phpinspect-find-class-file)
    (global-set-key (kbd \"C-c c\") 'phpinspect-find-own-class-file)

    ;; Enable phpinspect-mode
    (phpinspect-mode))

  (add-hook 'php-mode-hook #'my-php-personal-hook)

;; End example configuration."
    :after-hook (phpinspect--mode-function))

(defun phpinspect--find-class-token (token)
  "Recurse into token tree until a class is found."
  (when (and (listp token) (> (length token) 1))
    (let ((last-token (car (last token))))
      (cond ((phpinspect-class-p token) token)
            (last-token
             (phpinspect--find-class-token last-token))))))

(defun phpinspect--find-innermost-incomplete-class (token)
  (let ((last-token (car (last token))))
    (cond ((phpinspect-incomplete-class-p token) token)
          ((phpinspect-incomplete-token-p last-token)
           (phpinspect--find-innermost-incomplete-class last-token)))))

(defun phpinspect--find-last-variable-position-in-token (token)
  "Find the last variable that can be encountered in the top
level of a token. Nested variables are ignored."
  (let ((i (length token)))
    (while (and (not (= 0 i))
                (not (phpinspect-variable-p
                      (car (last token i)))))
      (setq i (- i 1)))

    (if (not (= i 0))(- (length token)  i))))

(defun phpinspect--make-method-lister (resolvecontext buffer-classes &optional static)
  (lambda (fqn)
    (phpinspect--get-methods-for-class resolvecontext buffer-classes fqn static)))

(defun phpinspect--buffer-index (buffer)
  (with-current-buffer buffer phpinspect--buffer-index))

(defsubst phpinspect-not-variable-p (token)
  (not (phpinspect-variable-p token)))

(cl-defmethod phpinspect--make-completion
  ((completion-candidate phpinspect--variable))
  (phpinspect--construct-completion
   :value (phpinspect--variable-name completion-candidate)
   :meta (phpinspect--format-type-name
          (or (phpinspect--variable-type completion-candidate)
              phpinspect--null-type))
   :annotation (concat " "
                       (phpinspect--type-bare-name
                        (or (phpinspect--variable-type completion-candidate)
                            phpinspect--null-type)))
   :kind 'variable))

(cl-defstruct (phpinspect--completion-list
               (:constructor phpinspect--make-completion-list))
  "Contains all data for a completion at point"
  (completions (obarray-make)
               :type obarray
               :documentation
               "A list of completion strings"))

(cl-defgeneric phpinspect--completion-list-add
    (comp-list completion)
  "Add a completion to a completion-list.")

(cl-defmethod phpinspect--completion-list-add
  ((comp-list phpinspect--completion-list) (completion phpinspect--completion))
  (unless (intern-soft (phpinspect--completion-value completion)
                      (phpinspect--completion-list-completions comp-list))
    (set (intern (phpinspect--completion-value completion)
                 (phpinspect--completion-list-completions comp-list))
         completion)))

(cl-defmethod phpinspect--completion-list-get-metadata
  ((comp-list phpinspect--completion-list) (completion-name string))
  (let ((comp-sym (intern-soft completion-name
                               (phpinspect--completion-list-completions comp-list))))
    (when comp-sym
      (symbol-value comp-sym))))


(cl-defmethod phpinspect--completion-list-strings
  ((comp-list phpinspect--completion-list))
  (let ((strings))
    (obarray-map (lambda (sym) (push (symbol-name sym) strings))
                 (phpinspect--completion-list-completions comp-list))
    strings))

(defun phpinspect--suggest-attributes-at-point
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

(defun phpinspect--get-last-statement-in-token (token)
  (setq token (cond ((phpinspect-function-p token)
                     (phpinspect-function-block token))
                    ((phpinspect-namespace-p token)
                     (phpinspect-namespace-block token))
                    (t token)))
  (nreverse
   (seq-take-while
    (let ((keep-taking t) (last-test nil))
      (lambda (elt)
        (when last-test
          (setq keep-taking nil))
        (setq last-test (phpinspect-variable-p elt))
        (and keep-taking
             (not (phpinspect-end-of-statement-p elt))
             (listp elt))))
    (reverse token))))

(defun phpinspect--suggest-variables-at-point (resolvecontext)
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

(defun phpinspect--suggest-at-point ()
  (phpinspect--log "Entering suggest at point. Point: %d" (point))
  (let* ((bmap (phpinspect-buffer-parse-map phpinspect-current-buffer))
         (resolvecontext (phpinspect-get-resolvecontext bmap (point)))
         (last-tokens (last (phpinspect--resolvecontext-subject resolvecontext) 2)))
    (phpinspect--log "Subject: %s" (phpinspect--resolvecontext-subject
                                    resolvecontext))
    (phpinspect--log "Last tokens: %s" last-tokens)
    (cond ((and (phpinspect-object-attrib-p (car last-tokens))
                (phpinspect-word-p (cadr last-tokens)))
           (phpinspect--log "word-attributes")
           (phpinspect--suggest-attributes-at-point resolvecontext))
          ((phpinspect-object-attrib-p (cadr last-tokens))
           (phpinspect--log "object-attributes")
           (phpinspect--suggest-attributes-at-point resolvecontext))
          ((phpinspect-static-attrib-p (cadr last-tokens))
           (phpinspect--log "static-attributes")
           (phpinspect--suggest-attributes-at-point token-tree resolvecontext t))
          ((phpinspect-variable-p (car(phpinspect--resolvecontext-subject
                                       resolvecontext)))
           (phpinspect--suggest-variables-at-point resolvecontext)))))


(defun phpinspect-company-backend (command &optional arg &rest _ignored)
  "A company backend for PHP."
  (interactive (list 'interactive))
  (cond
   ((eq command 'interactive)
    (company-begin-backend 'company-phpinspect-backend))
   ((eq command 'prefix)
    (cond ((looking-back "->[A-Za-z_0-9-]*")
           (let ((match (match-string 0)))
             (substring match 2 (length match))))
          ((looking-back "::[A-Za-z_0-9-]*")
           (let ((match (match-string 0)))
             (substring match 2 (length match))))
          ((looking-back "\\$[A-Za-z_0-9-]*")
           (let ((match (match-string 0)))
             (substring match 1 (length match))))))
   ((eq command 'post-completion)
    (when (eq 'function (phpinspect--completion-kind
                         (phpinspect--completion-list-get-metadata
                          phpinspect--last-completion-list
                          arg)))
      (insert "(")))
   ((eq command 'candidates)
    (catch 'phpinspect-parse-interrupted
      (let ((completion-list (phpinspect--make-completion-list))
            (candidates))
        (dolist (completion (phpinspect--suggest-at-point))
          (phpinspect--completion-list-add
           completion-list
           (phpinspect--make-completion completion)))

        (setq candidates
              (seq-filter (lambda (completion)
                            (when completion
                              (string-match (concat "^" (regexp-quote arg))
                                            completion)))
                          (phpinspect--completion-list-strings
                           completion-list)))
        (setq phpinspect--last-completion-list completion-list)
        candidates)))
   ((eq command 'annotation)
    (concat " " (phpinspect--completion-annotation
                 (phpinspect--completion-list-get-metadata
                  phpinspect--last-completion-list
                  arg))))
   ((eq command 'kind)
    (phpinspect--completion-kind
     (phpinspect--completion-list-get-metadata
      phpinspect--last-completion-list
      arg)))
   ((eq command 'meta)
    (phpinspect--completion-meta
              (phpinspect--completion-list-get-metadata phpinspect--last-completion-list arg)))))

(defun phpinspect--get-or-create-global-cache ()
  "Get `phpinspect-cache'.
If its value is nil, it is created and then returned."
  (or phpinspect-cache
      (setq phpinspect-cache (phpinspect--make-cache))))

(defun phpinspect-purge-cache ()
  "Assign a fresh, empty cache object to `phpinspect-cache'.
This effectively purges any cached code information from all
currently opened projects."
  (interactive)
  (when phpinspect-cache
    ;; Allow currently known cached projects to cleanup after themselves
    (maphash (lambda (_ project)
               (phpinspect-project-purge project))
             (phpinspect--cache-projects phpinspect-cache)))

  ;; Assign a fresh cache object
  (setq phpinspect-cache (phpinspect--make-cache)))


(defmacro phpinspect-json-preset (&rest body)
  "Default options to wrap around `json-read' and similar BODY."
  `(let ((json-object-type 'hash-table)
	     (json-array-type 'list)
	     (json-key-type 'string))
     ,@body))

(defsubst phpinspect-insert-file-contents (&rest args)
  "Call `phpinspect-insert-file-contents-function' with ARGS as arguments."
  (apply phpinspect-insert-file-contents-function args))

(defun phpinspect-get-all-fqns (&optional filter)
  "Return a list of all FQNS congruent with FILTER in the currently active project.

FILTER must be nil or the symbol 'own' if FILTER is 'own', only
fully qualified names from the project's source, and not its
dependencies, are returned."
  (let* ((project (phpinspect--cache-get-project-create
                   (phpinspect--get-or-create-global-cache)
                   (phpinspect-current-project-root)))
         (autoloader (phpinspect-project-autoload project)))
    (let ((fqns))
      (maphash (lambda (type _) (push (symbol-name type) fqns))
               (if (eq 'own filter)
                   (phpinspect-autoloader-own-types autoloader)
                 (phpinspect-autoloader-types autoloader)))
      fqns)))

;;;###autoload
(defun phpinspect-find-class-file (fqn)
  "`find-file', but for FQNs of PHP classes.

When called interactively, presents the the user with a list of
available FQNs in a project.  This may require
`phpinspect-index-current-project' to have run once for the
project directory before it can be used."
  (interactive (list (phpinspect--make-type
                      :name (completing-read "Class: " (phpinspect-get-all-fqns)))))
  (find-file (phpinspect-type-filepath fqn)))

(defun phpinspect-find-own-class-file (fqn)
  "`phpinspect-find-class-file', but for non-vendored classes.

When called interactively, presents the user with a list of
available FQNs for classes in the current project, which aren't
located in \"vendor\" folder."
  (interactive (list (phpinspect--make-type
                      :name
                      (completing-read "Class: " (phpinspect-get-all-fqns 'own)))))
  (find-file (phpinspect-type-filepath fqn)))

(defsubst phpinspect-type-filepath (fqn)
  "Call `phpinspect-type-filepath-function' with FQN as argument."
  (funcall phpinspect-type-filepath-function fqn))

(defun phpinspect-get-class-filepath (class &optional index-new)
  "Retrieve filepath to CLASS definition file.

when INDEX-NEW is non-nil, new files are added to the index
before the search is executed."
  (let* ((project (phpinspect--cache-get-project-create
                   (phpinspect--get-or-create-global-cache)
                   (phpinspect-current-project-root))))
    (phpinspect-project-get-type-filepath project class index-new)))


(defun phpinspect-unique-strings (strings)
  (seq-filter
   (let ((last-line nil))
     (lambda (line)
       (let ((return-line (unless (and last-line (string= last-line line))
                            line)))
         (setq last-line line)
         return-line)))
   strings))

(defun phpinspect-index-current-project ()
  "Index all available FQNs in the current project."
  (interactive)
  (let* ((project (phpinspect--cache-get-project-create
                  (phpinspect--get-or-create-global-cache)
                  (phpinspect-current-project-root)))
         (autoloader (phpinspect-project-autoload project)))
    (phpinspect-autoloader-refresh autoloader)
    (message (concat "Refreshed project autoloader. Found %d types within project,"
                     " %d types total.")
             (hash-table-count (phpinspect-autoloader-own-types autoloader))
             (hash-table-count (phpinspect-autoloader-types autoloader)))))

(defun phpinspect-unique-lines ()
  (let ((unique-lines (phpinspect-unique-strings (split-string (buffer-string) "\n" nil nil))))
    (erase-buffer)
    (insert (string-join unique-lines "\n"))))

(provide 'phpinspect)
;;; phpinspect.el ends here
