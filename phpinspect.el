;; phpinspect.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(defvar-local phpinspect--buffer-index nil
  "The result of the last successfull parse + index action
  executed by phpinspect for the current buffer")

(defvar phpinspect-cache ()
  "In-memory nested key-value store used for caching by
phpinspect")

(defvar phpinspect-insert-file-contents-function #'insert-file-contents-literally
  "Function that phpinspect uses to insert file contents into a buffer.")

(defvar phpinspect-project-root-function #'phpinspect--find-project-root
  "Function that phpinspect uses to find the root directory of a project.")

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

(defvar phpinspect-index-executable
  (concat (file-name-directory
           (or load-file-name
               buffer-file-name))
          "/phpinspect-index.bash")
  "The path to the exexutable file that indexes class file names.
Should normally be set to \"phpinspect-index.bash\" in the source
  file directory.")


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

(cl-defstruct (phpinspect--resolvecontext
            (:constructor phpinspect--make-resolvecontext))
  (subject nil
           :type phpinspect--token
           :documentation
           "The statement we're trying to resolve the type of.")
  (project-root nil
                :type string
                :documentation
                "The root directory of the project we're resolving types for.")
  (enclosing-tokens nil
                    :type list
                    :documentation
                    "Tokens that enclose the subject."))

(cl-defmethod phpinspect--resolvecontext-push-enclosing-token
  ((resolvecontext phpinspect--resolvecontext) enclosing-token)
  "Add ENCLOSING-TOKEN to RESOLVECONTEXTs enclosing token stack."
  (push enclosing-token (phpinspect--resolvecontext-enclosing-tokens
                         resolvecontext)))

(defun phpinspect--get-resolvecontext (token &optional resolvecontext)
  "Find the deepest nested incomplete token in TOKEN.
If RESOLVECONTEXT is nil, it is created.  Returns RESOLVECONTEXT
of type `phpinspect--resolvecontext' containing the last
statement of the innermost incomplete token as subject
accompanied by all of its enclosing tokens."
  (unless resolvecontext
    (setq resolvecontext (phpinspect--make-resolvecontext
                          :project-root (phpinspect-project-root))))

  (let ((last-token (car (last token)))
        (last-encountered-token (car
                                 (phpinspect--resolvecontext-enclosing-tokens
                                  resolvecontext))))
    (if (and (or (phpinspect-function-p last-encountered-token)
                 (phpinspect-class-p last-encountered-token))
             (phpinspect-block-p token))
        ;; When a class or function has been inserted already, its block
        ;; doesn't need to be added on top.
        (phpinspect--resolvecontext-push-enclosing-token resolvecontext nil)
      (phpinspect--resolvecontext-push-enclosing-token resolvecontext token))

    (if (phpinspect-incomplete-token-p last-token)
        (phpinspect--get-resolvecontext last-token resolvecontext)
    ;; else
    (setf (phpinspect--resolvecontext-subject resolvecontext)
          (phpinspect--get-last-statement-in-token token))

    ;; Delete all occurences of nil caused higher up in the function.
    (cl-delete nil (phpinspect--resolvecontext-enclosing-tokens
                    resolvecontext))
    resolvecontext)))


(defsubst phpinspect-cache-project-class (project-root indexed-class)
  (when project-root
    (phpinspect--project-add-class
     (phpinspect--cache-get-project-create (phpinspect--get-or-create-global-cache)
                                           project-root)
     indexed-class)))

(defsubst phpinspect-get-cached-project-class (project-root class-fqn)
  (when project-root
    (phpinspect--project-get-class
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

(defun phpinspect-parse-string (string)
  (with-temp-buffer
    (insert string)
    (phpinspect-parse-current-buffer)))

(defun phpinspect--split-list (predicate list)
  (let ((sublists)
        (current-sublist))
    (dolist (thing list)
      (if (funcall predicate thing)
          (when current-sublist
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
  (phpinspect--log "Starting eldoc function execution")
  (let* ((token-tree (phpinspect-parse-buffer-until-point (current-buffer) (point)))
         (resolvecontext (phpinspect--get-resolvecontext token-tree))
         (incomplete-token (car (phpinspect--resolvecontext-enclosing-tokens
                                 resolvecontext)))
         (enclosing-token (cadr (phpinspect--resolvecontext-enclosing-tokens
                                 resolvecontext)))
         (statement (phpinspect--get-last-statement-in-token
                     enclosing-token))
         (type-resolver (phpinspect--make-type-resolver-for-resolvecontext
                               resolvecontext))
         (static))
    (phpinspect--log "Enclosing token: %s" enclosing-token)
    (phpinspect--log "reference token: %s" (car (last statement 2)))

    (when (and (phpinspect-incomplete-list-p incomplete-token)
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
             (class (phpinspect--project-get-class-create
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
                 (length (seq-filter #'phpinspect-comma-p incomplete-token))))
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
                      (phpinspect--function-return-type method)))))))))

(defsubst phpinspect-block-or-list-p (token)
  (or (phpinspect-block-p token)
      (phpinspect-list-p token)))

(defsubst phpinspect-maybe-assignment-p (token)
  "Like `phpinspect-assignment-p', but includes \"as\" barewords as possible tokens."
  (or (phpinspect-assignment-p token)
      (equal '(:word "as") token)))

(cl-defgeneric phpinspect--find-assignments-in-token (token)
  "Find any assignments that are in TOKEN, at top level or nested in blocks"
  (let ((assignments)
        (block-or-list)
        (statements (phpinspect--split-list #'phpinspect-end-of-statement-p token)))
    (dolist (statement statements)
      (cond ((seq-find #'phpinspect-assignment-p statement)
             (phpinspect--log "Found assignment statement")
             (push statement assignments))
            ((setq block-or-list (seq-find #'phpinspect-block-or-list-p statement))
             (phpinspect--log "Found block or list %s" block-or-list)
             (setq assignments
                   (append
                    (phpinspect--find-assignments-in-token block-or-list)
                    assignments)))))
    ;; return
    (phpinspect--log "Found assignments in token: %s" assignments)
    (phpinspect--log "Found statements in token: %s" statements)
    assignments))

(cl-defmethod phpinspect--find-assignments-in-token ((token (head :list)))
  "Find assignments that are in a list token."
  (phpinspect--log "looking for assignments in list %s" token)
  (seq-filter
   (lambda (statement)
     (phpinspect--log "checking statement %s" statement)
     (seq-find #'phpinspect-maybe-assignment-p statement))
   (phpinspect--split-list #'phpinspect-end-of-statement-p (cdr token))))

(defsubst phpinspect-not-assignment-p (token)
  "Inverse of applying `phpinspect-assignment-p to TOKEN."
  (not (phpinspect-maybe-assignment-p token)))

(defun phpinspect--find-assignment-values-for-variable-in-token (variable-name token)
  "Find all assignments of variable VARIABLE-NAME in TOKEN."
  (let ((variable-assignments)
        (all-assignments (phpinspect--find-assignments-in-token token)))
    (dolist (assignment all-assignments)
      (let* ((is-loop-assignment nil)
             (left-of-assignment
              (seq-take-while #'phpinspect-not-assignment-p assignment))
             (right-of-assignment
              (cdr (seq-drop-while (lambda (elt)
                                     (if (phpinspect-maybe-assignment-p elt)
                                         (progn
                                           (when (equal '(:word "as") elt)
                                             (phpinspect--log "It's a loop assignment %s" elt)
                                             (setq is-loop-assignment t))
                                           nil)
                                       t))
                                   assignment))))
        (if is-loop-assignment
            (when (member `(:variable ,variable-name) right-of-assignment)
              (push left-of-assignment variable-assignments))
          (when (member `(:variable ,variable-name) left-of-assignment)
            (push right-of-assignment variable-assignments)))))
    (nreverse variable-assignments)))

    ;;   (if (or (member `(:variable ,variable-name)
    ;;                   (seq-take-while #'phpinspect-not-assignment-p
    ;;                                   assignment))5
    ;;           (and (phpinspect-list-p (car assignment))
    ;;                (member `(:variable ,variable-name) (car assignment)))
    ;;           (and (member '(:word "as") assignment)
    ;;                (member `(:variable ,variable-name)
    ;;                        (seq-drop-while (lambda (elt)
    ;;                                          (not (equal '(:word "as") elt)))))))
    ;;       (push assignment variable-assignments)))
    ;; (nreverse variable-assignments)))

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
                                  previous-attribute-type)))))))))
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
    ;; else
    (let* ((assignments
            (phpinspect--find-assignment-values-for-variable-in-token variable-name php-block))
           (last-assignment-value (when assignments (car (last assignments)))))

      (phpinspect--log "Last assignment: %s" last-assignment-value)
      (phpinspect--log "Current block: %s" php-block)
      ;; When the right of an assignment is more than $variable; or "string";(so
      ;; (:variable "variable") (:terminator ";") or (:string "string") (:terminator ";")
      ;; in tokens), we're likely working with a derived assignment like $object->method()
      ;; or $object->attribute
      (cond ((and (phpinspect-word-p (car last-assignment-value))
                  (string= (cadar last-assignment-value) "new"))
             (funcall type-resolver (phpinspect--make-type :name (cadadr last-assignment-value))))
            ((and (> (length last-assignment-value) 1)
                  (seq-find #'phpinspect-attrib-p last-assignment-value))
             (phpinspect--log "Variable was assigned with a derived statement")
             (phpinspect-get-derived-statement-type-in-block resolvecontext
                                                             last-assignment-value
                                                             php-block
                                                             type-resolver
                                                             function-arg-list))
            ;; If the right of an assignment is just $variable;, we can check if it is a
            ;; function argument and otherwise recurse to find the type of that variable.
            ((phpinspect-variable-p (car last-assignment-value))
             (phpinspect--log "Variable was assigned with the value of another variable: %s"
                              last-assignment-value)
             (or (when function-arg-list
                   (phpinspect-get-variable-type-in-function-arg-list (cadar last-assignment-value)
                                                                      type-resolver
                                                                      function-arg-list))
                 (phpinspect-get-variable-type-in-block resolvecontext
                                                        (cadar last-assignment-value)
                                                        php-block
                                                        type-resolver
                                                        function-arg-list)))
            ((not assignments)
             (phpinspect--log "No assignments found for variable %s, checking function arguments"
                              variable-name)
             (phpinspect-get-variable-type-in-function-arg-list variable-name
                                                                type-resolver
                                                                function-arg-list))))))


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
                (phpinspect-project-root)
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


(defun phpinspect--init-mode ()
  "Initialize the phpinspect minor mode for the current buffer."

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
    (setq phpinspect--buffer-index (phpinspect--index-current-buffer))
    (let ((imports (alist-get 'imports phpinspect--buffer-index))
          (project (phpinspect--cache-get-project-create
                    (phpinspect--get-or-create-global-cache)
                    (phpinspect-project-root))))

      (dolist (class (alist-get 'classes phpinspect--buffer-index))
        (when class
          (phpinspect--project-add-class project (cdr class))

          (let ((imports (alist-get 'imports (cdr class))))
            (when imports
              (phpinspect--project-enqueue-imports project imports)))))


      (when imports (phpinspect--project-enqueue-imports project imports)))))


(defun phpinspect--disable-mode ()
  "Clean up the buffer environment for the mode to be disabled."
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
visited file, use `phpinspect-fix-uses-interactive'
(bound to \\[phpinspect-fix-uses-interactive]].)

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
    (define-key php-mode-map (kbd \"C-c u\") 'phpinspect-fix-uses-interactive)

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
    (token-tree resolvecontext &optional static)
  "Suggest object or class attributes at point.

TOKEN-TREE must be a syntax tree containing enough context to
infer the types of the preceding statements

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

(defun phpinspect--make-type-resolver-for-resolvecontext
    (resolvecontext)
  (let ((namespace-or-root
         (seq-find #'phpinspect-namespace-or-root-p
                   (phpinspect--resolvecontext-enclosing-tokens
                    resolvecontext)))
        (namespace-name))
    (when (phpinspect-namespace-p namespace-or-root)
      (setq namespace-name (cadadr namespace-or-root))
      (setq namespace-or-root (phpinspect-namespace-body namespace-or-root)))

      (phpinspect--make-type-resolver
       (phpinspect--uses-to-types
        (seq-filter #'phpinspect-use-p namespace-or-root))
       (seq-find #'phpinspect-class-p
                   (phpinspect--resolvecontext-enclosing-tokens
                    resolvecontext))
       namespace-name)))

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
      (phpinspect--log "Entering suggest at point." )
  (let* ((token-tree (phpinspect-parse-buffer-until-point (current-buffer) (point)))
         (resolvecontext (phpinspect--get-resolvecontext token-tree))
         (last-tokens (last (phpinspect--resolvecontext-subject resolvecontext) 2)))
    (phpinspect--log "Subject: %s" (phpinspect--resolvecontext-subject
                                    resolvecontext))
    (phpinspect--log "Last tokens: %s" last-tokens)
    (cond ((and (phpinspect-object-attrib-p (car last-tokens))
                (phpinspect-word-p (cadr last-tokens)))
           (phpinspect--log "word-attributes")
           (phpinspect--suggest-attributes-at-point token-tree
                                                    resolvecontext))
          ((phpinspect-object-attrib-p (cadr last-tokens))
           (phpinspect--log "object-attributes")
           (phpinspect--suggest-attributes-at-point token-tree resolvecontext))
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
      candidates))
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
  (setq phpinspect-cache (phpinspect--make-cache)))

(defun phpinspect--locate-dominating-project-file (start-file)
  "Locate the first dominating file in `phpinspect-project-root-file-list`.
Starts looking at START-FILE and then recurses up the directory
hierarchy as long as no matching files are found.  See also
`locate-dominating-file'."
  (let ((dominating-file))
    (seq-find (lambda (file)
                (setq dominating-file (locate-dominating-file start-file file)))
              phpinspect-project-root-file-list)
    dominating-file))

(defun phpinspect--find-project-root (&optional start-file)
  "(Attempt to) Find the root directory of the visited PHP project.
If a found project root has a parent directory called \"vendor\",
the search continues upwards. See also
`phpinspect--locate-dominating-project-file'.

If START-FILE is provided, searching starts at the directory
level of START-FILE in stead of `default-directory`."
  (let ((project-file (phpinspect--locate-dominating-project-file
                       (or start-file default-directory))))
    (phpinspect--log "Checking for project root at  %s" project-file)
    (when project-file
      (let* ((directory (file-name-directory project-file))
             (directory-slugs (split-string (expand-file-name directory) "/")))
        (if (not (member "vendor" directory-slugs))
            (expand-file-name directory)
          ;; else. Only continue if the parent directory is not "/"
          (let ((parent-without-vendor
                 (string-join (seq-take-while (lambda (s) (not (string= s "vendor" )))
                                              directory-slugs)
                              "/")))
            (when (not (or (string= parent-without-vendor "/")
                           (string= parent-without-vendor "")))
              (phpinspect--find-project-root parent-without-vendor))))))))

(defsubst phpinspect-project-root ()
  "Call `phpinspect-project-root-function' with ARGS as arguments."
  (unless (and (boundp 'phpinspect--buffer-project) phpinspect--buffer-project)
    (set (make-local-variable 'phpinspect--buffer-project) (funcall phpinspect-project-root-function)))
  phpinspect--buffer-project)

;; Use statements
;;;###autoload
(defun phpinspect-fix-uses-interactive ()
  "Add missing use statements to the currently visited PHP file."
  (interactive)
  (let ((project-root (phpinspect-project-root)))
    (when project-root
      (save-buffer)
      (let* ((phpinspect-json (shell-command-to-string
			                   (format "cd %s && %s fxu --json %s"
				                       (shell-quote-argument project-root)
                                       (shell-quote-argument phpinspect-index-executable)
				                       (shell-quote-argument buffer-file-name)))))
	    (let* ((json-object-type 'hash-table)
		       (json-array-type 'list)
		       (json-key-type 'string)
		       (phpinspect-json-data (json-read-from-string phpinspect-json)))
	      (maphash #'phpinspect-handle-phpinspect-json phpinspect-json-data))))))

(defun phpinspect-handle-phpinspect-json (class-name candidates)
  "Handle key value pair of classname and FQN's"
  (let ((ncandidates (length candidates)))
    (cond ((= 1 ncandidates)
           (phpinspect-add-use (pop candidates)))
          ((= 0 ncandidates)
           (message "No use statement found for class \"%s\"" class-name))
          (t
           (phpinspect-add-use (completing-read "Class: " candidates))))))

;; TODO: Implement this using the parser in stead of regexes.
(defun phpinspect-add-use (fqn) "Add use statement to a php file"
       (save-excursion
         (let ((current-char (point)))
	   (goto-char (point-min))
	   (cond
	    ((re-search-forward "^use" nil t) (forward-line 1))
	    ((re-search-forward "^namespace" nil t) (forward-line 2))
	    ((re-search-forward
	      "^\\(abstract \\|/\\* final \\*/ ?\\|final \\|\\)\\(class\\|trait\\|interface\\)"
              nil )
	     (forward-line -1)
	     (phpinspect-goto-first-line-no-comment-up)))

	   (insert (format "use %s;%c" fqn ?\n))
	   (goto-char current-char))))

(defun phpinspect-goto-first-line-no-comment-up ()
  "Go up until a line is encountered that does not start with a comment."
  (when (string-match "^\\( ?\\*\\|/\\)" (thing-at-point 'line t))
	(forward-line -1)
	(phpinspect-goto-first-line-no-comment-up)))

(defsubst phpinspect-insert-file-contents (&rest args)
  "Call `phpinspect-insert-file-contents-function' with ARGS as arguments."
  (apply phpinspect-insert-file-contents-function args))

(defun phpinspect-get-all-fqns (&optional fqn-file)
  (unless fqn-file
    (setq fqn-file "uses"))
  (with-temp-buffer
    (phpinspect-insert-file-contents
     (concat (phpinspect-project-root) "/.cache/phpinspect/" fqn-file))
    (split-string (buffer-string) (char-to-string ?\n))))

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
                      (completing-read "Class: " (phpinspect-get-all-fqns "uses_own")))))
  (find-file (phpinspect-type-filepath fqn)))

(defsubst phpinspect-type-filepath (fqn)
  "Call `phpinspect-type-filepath-function' with FQN as argument."
  (funcall phpinspect-type-filepath-function fqn))

(defun phpinspect-get-class-filepath (class &optional index-new)
  "Retrieve filepath to CLASS definition file.

when INDEX-NEW is non-nil, new files are added to the index
before the search is executed."
  (when (eq index-new 'index-new)
    (with-temp-buffer
      (call-process phpinspect-index-executable nil (current-buffer) nil "index" "--new")))
  (let* ((default-directory (phpinspect-project-root))
         (result (with-temp-buffer
                   (phpinspect--log "dir: %s" default-directory)
                   (phpinspect--log "class: %s" (string-remove-prefix
                                                 "\\"
                                                 (phpinspect--type-name class)))
                   (list (call-process phpinspect-index-executable
                                       nil
                                       (current-buffer)
                                       nil
                                       "fp" (string-remove-prefix
                                             "\\"
                                             (phpinspect--type-name class)))
                           (buffer-string)))))
    (if (not (= (car result) 0))
        (progn
          (phpinspect--log "Got non-zero return value %d Retrying with reindex. output: \"%s\""
                           (car result)
                           (cadr result))
          ;; Index new files and try again if not done already.
          (if (eq index-new 'index-new)
              nil
            (phpinspect-get-class-filepath class 'index-new)))
      (concat (string-remove-suffix "/" default-directory)
              "/"
              (string-remove-prefix "/" (string-trim (cadr result)))))))

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
  "Index all available FQNs in the current project.

Index is stored in files in the .cache directory of
the project root."
  (interactive)
  (let* ((default-directory (phpinspect-project-root)))
    (with-current-buffer (get-buffer-create "**phpinspect-index**")
      (goto-char (point-max))
      (make-process
       :command `(,phpinspect-index-executable "index")
       :name "phpinspect-index-current-project"
       :buffer (current-buffer))

      (display-buffer (current-buffer) `(display-buffer-at-bottom (window-height . 10)))
      (set-window-point (get-buffer-window (current-buffer) nil)
                        (point-max)))))

(defun phpinspect-unique-lines ()
  (let ((unique-lines (phpinspect-unique-strings (split-string (buffer-string) "\n" nil nil))))
    (erase-buffer)
    (insert (string-join unique-lines "\n"))))

(provide 'phpinspect)
;;; phpinspect.el ends here
