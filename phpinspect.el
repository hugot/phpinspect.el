;;; phpinspect.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(defvar phpinspect-parser-obarray (obarray-make)
  "An obarray containing symbols for all phpinspect (sub)parsers.")

(defvar phpinspect-handler-obarray (obarray-make)
  "An obarray containing symbols for all phpinspect parser handlers.")

(defvar phpinspect-name-obarray (obarray-make)
  "An obarray containing symbols for all encountered names in
PHP. Used to optimize string comparison.")

(defvar-local phpinspect--buffer-index nil
  "The result of the last successfull parse + index action
  executed by phpinspect for the current buffer")

(defvar phpinspect--debug nil
  "Enable debug logs for phpinspect by setting this variable to true")

(defvar phpinspect-cache ()
  "In-memory nested key-value store used for caching by
phpinspect")

(defvar phpinspect-insert-file-contents-function #'insert-file-contents-literally
  "Function that phpinspect uses to insert file contents into a buffer.")

(defvar phpinspect-project-root-function #'phpinspect--find-project-root
  "Function that phpinspect uses to find the root directory of a project.")

(defvar phpinspect-class-filepath-function #'phpinspect-get-class-filepath
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

(defconst phpinspect-native-types
  ;; self, parent and resource are not valid type name.
  ;; see https://www.php.net/manual/ja/language.types.declarations.php
  '("array" "bool" "callable" "float" "int" "iterable" "mixed" "object" "string" "void"))

(defvar phpinspect-collection-types
  '("\\array" "\\iterable" "\\SplObjectCollection" "\\mixed")
  "FQNs of types that should be treated as collecitons when inferring types.")

(defsubst phpinspect-intern-name (name)
  (intern name phpinspect-name-obarray))

(eval-when-compile
  (define-inline phpinspect--word-end-regex ()
    (inline-quote "\\([[:blank:]]\\|[^0-9a-zA-Z_]\\)")))

(defun phpinspect-list-handlers ()
  (let ((handlers))
    (mapatoms (lambda (handler)
                (push (symbol-name handler) handlers))
              phpinspect-handler-obarray)
    handlers))

(defun phpinspect-describe-handler (handler-name)
  "Display a buffer containing HANDLER-NAMEs docstring and attribute-plist."
  (interactive (list (completing-read "Pick a handler:" (phpinspect-list-handlers))))
  (with-current-buffer (get-buffer-create "phpinspect-handler-description")
    (insert (concat
             (pp (symbol-value (intern handler-name phpinspect-handler-obarray)))
             "\n"
             (documentation (intern handler-name phpinspect-handler-obarray))))
    (pop-to-buffer (current-buffer))))

(defsubst phpinspect--strip-last-char (string)
  (substring string 0 (- (length string) 1)))

(defsubst phpinspect-munch-token-without-attribs (string token-keyword)
  "Return a token of type TOKEN-KEYWORD with STRING as value.
If STRING has text properties, they are stripped."
  (let ((value (copy-sequence string))
        (length (length string)))
    (forward-char length)
    (set-text-properties 0 length nil value)
    (list token-keyword value)))


(defsubst phpinspect-token-type-p (object type)
  "Returns t if OBJECT is a token of type TYPE.
Type can be any of the token types returned by
`phpinspect-parse-buffer-until-point`"
  (and (listp object) (eq (car object) type)))

(defsubst phpinspect-object-attrib-p (token)
  (phpinspect-token-type-p token :object-attrib))

(defsubst phpinspect-static-attrib-p (token)
  (phpinspect-token-type-p token :static-attrib))

(defsubst phpinspect-attrib-p (token)
  (or (phpinspect-object-attrib-p token)
      (phpinspect-static-attrib-p token)))

(defun phpinspect-html-p (token)
  (phpinspect-token-type-p token :html))

(defun phpinspect-comma-p (token)
  (phpinspect-token-type-p token :comma))

(defsubst phpinspect-terminator-p (token)
  (phpinspect-token-type-p token :terminator))

(defsubst phpinspect-end-of-token-p (token)
  (or (phpinspect-terminator-p token)
      (phpinspect-comma-p token)
      (phpinspect-html-p token)))

(defsubst phpinspect-end-of-statement-p (token)
  (or (phpinspect-end-of-token-p token)
      (phpinspect-block-p token)))

(defsubst phpinspect-incomplete-block-p (token)
  (phpinspect-token-type-p token :incomplete-block))

(defsubst phpinspect-block-p (token)
  (or (phpinspect-token-type-p token :block)
      (phpinspect-incomplete-block-p token)))

(defun phpinspect-end-of-use-p (token)
  (or (phpinspect-block-p token)
      (phpinspect-end-of-token-p token)))

(defun phpinspect-static-p (token)
  (phpinspect-token-type-p token :static))

(defsubst phpinspect-incomplete-const-p (token)
  (phpinspect-token-type-p token :incomplete-const))

(defsubst phpinspect-const-p (token)
  (or (phpinspect-token-type-p token :const)
      (phpinspect-incomplete-const-p token)))

(defsubst phpinspect-scope-p (token)
  (or (phpinspect-token-type-p token :public)
      (phpinspect-token-type-p token :private)
      (phpinspect-token-type-p token :protected)))

(defsubst phpinspect-namespace-p (object)
  (phpinspect-token-type-p object :namespace))

(defun phpinspect-incomplete-class-p (token)
  (and (phpinspect-class-p token)
       (phpinspect-incomplete-block-p (car (last token)))))

(defun phpinspect-incomplete-namespace-p (token)
  (and (phpinspect-namespace-p token)
       (or (phpinspect-incomplete-block-p (car (last token)))
           (phpinspect-incomplete-class-p (car (last token))))))

(defun phpinspect-function-p (token)
  (phpinspect-token-type-p token :function))


(defun phpinspect-class-p (token)
  (phpinspect-token-type-p token :class))

(defun phpinspect-incomplete-method-p (token)
  (or (phpinspect-incomplete-function-p token)
      (and (phpinspect-scope-p token)
           (phpinspect-incomplete-function-p (car (last token))))
      (and (phpinspect-scope-p token)
           (phpinspect-static-p (car (last token)))
           (phpinspect-incomplete-function-p (car (last (car (last token))))))
      (and (phpinspect-scope-p token)
           (phpinspect-function-p (car (last token))))))

(defun phpinspect-incomplete-function-p (token)
  (and (phpinspect-function-p token)
       (phpinspect-incomplete-block-p (car (last token)))))

(defsubst phpinspect-incomplete-list-p (token)
  (phpinspect-token-type-p token :incomplete-list))

(defsubst phpinspect-list-p (token)
  (or (phpinspect-token-type-p token :list)
      (phpinspect-incomplete-list-p token)))

(defun phpinspect-declaration-p (token)
  (phpinspect-token-type-p token :declaration))

(defsubst phpinspect-assignment-p (token)
  (phpinspect-token-type-p token :assignment))

(defun phpinspect-function-argument-list (php-func)
  "Get the argument list of a function"
  (seq-find #'phpinspect-list-p (seq-find #'phpinspect-declaration-p php-func nil) nil))

(defsubst phpinspect-variable-p (token)
  (phpinspect-token-type-p token :variable))

(defsubst phpinspect-word-p (token)
  (phpinspect-token-type-p token :word))

(defsubst phpinspect-incomplete-array-p (token)
  (phpinspect-token-type-p token :incomplete-array))

(defsubst phpinspect-array-p (token)
  (or (phpinspect-token-type-p token :array)
      (phpinspect-incomplete-array-p token)))

(defsubst phpinspect-incomplete-token-p (token)
  (or (phpinspect-incomplete-class-p token)
      (phpinspect-incomplete-block-p token)
      (phpinspect-incomplete-list-p token)
      (phpinspect-incomplete-array-p token)
      (phpinspect-incomplete-const-p token)
      (phpinspect-incomplete-function-p token)
      (phpinspect-incomplete-method-p token)
      (phpinspect-incomplete-namespace-p token)))

(defun phpinspect--static-terminator-p (token)
  (or (phpinspect-function-p token)
      (phpinspect-end-of-token-p token)))

(defun phpinspect--scope-terminator-p (token)
  (or (phpinspect-function-p token)
      (phpinspect-end-of-token-p token)
      (phpinspect-const-p token)
      (phpinspect-static-p token)))

(defun phpinspect-namespace-keyword-p (token)
  (and (phpinspect-word-p token) (string= (car (last token)) "namespace")))

(defun phpinspect-use-keyword-p (token)
  (and (phpinspect-word-p token) (string= (car (last token)) "use")))


(defsubst phpinspect-root-p (object)
  (phpinspect-token-type-p object :root))

(defsubst phpinspect-namespace-or-root-p (object)
  (or (phpinspect-namespace-p object)
      (phpinspect-root-p object)))

(defun phpinspect-use-p (object)
  (phpinspect-token-type-p object :use))

(defun phpinspect-comment-p (token)
  (phpinspect-token-type-p token :comment))

(defsubst phpinspect-class-block (class)
  (caddr class))

(defsubst phpinspect-namespace-block (namespace)
  (when (and (= (length namespace) 3)
             (phpinspect-block-p (caddr namespace)))
    (caddr namespace)))

(defsubst phpinspect-function-block (php-func)
  (caddr php-func))

(defsubst phpinspect-not-class-p (token)
  "Apply inverse of `phpinspect-class-p' to TOKEN."
  (not (phpinspect-class-p token)))

(defmacro phpinspect-defhandler (name arguments docstring attribute-plist &rest body)
  "Define a parser handler that becomes available for use with phpinspect-parse.

A parser handler is a function that is able to identify and parse
tokens from PHP code at `point` in the current buffer.  It's
return value must be the resulting token.  Aside from parsing it
has to manage the state of `point` in a way that it skips over
the tokens it has parsed.  That way the next handler can
correctly pick up from where it has left off.

Parser handlers are unrolled in a `cond` statement by
`phpinspect-make-parser`.  The resulting code is something akin
to the following:

(while ...
  (cond (((looking-at \"{\")
          (funcall block-handler (match-string 0) max-point)
         ((looking-at \"\\$\")
          (funcall variable-handler ...
etc.

NAME must be a symbol.  It does not need to be prefixed with a
\"namespace\" because parser handlers are stored in their own
obarray (`phpinspect-handler-obarray`).

ARGUMENTS must an argument list as accepted by `lambda`.  A
handler must be able to accept 2 arguments: START-TOKEN and
MAX-POINT.  START-TOKEN is the match string that resulted from
the comparison of the handlers' `regexp` attribute with the text
at `point`.  MAX-POINT is the point in the current buffer up
until which the parser is supposed to parse.  For some tokens you
may not want/need to respect MAX-POINT, in which case you can
ignore it.

DOCSTRING is mandatory.  It should contain a description of the
tokens the handler is able to process and (if present) any
particularities of the handler.

ATTRIBUTE-PLIST is a plist that must contain at least a `regexp` key.
    Possible keys:
        - regexp: The regular expression that marks the start of the token.

BODY is a function body as accepted by `lambda` that parses the
text at point and returns the resulting token.

When altering/adding handlers during runtime, make sure to purge
the parser cache to make sure that your new handler functions are used.
You can purge the parser cache with \\[phpinspect-purge-parser-cache]."
  (declare (indent defun))
  (when (not (symbolp name))
    (error "In definition of phpinspect handler %s: NAME bust be a symbol" name))

  (when (not (plist-member attribute-plist 'regexp))
    (error "In definition of phpinspect handler %s ATTRIBUTE-PLIST must contain key `regexp`"
           name))

  ;; Eval regexp. It might be a `concat` statement and we don't want to be executing that
  ;; every time the parser advances one character and has to check for the regexp
  ;; occurence.
  (setq attribute-plist (plist-put attribute-plist 'regexp
                                   (eval (plist-get attribute-plist 'regexp)
                                         t)))
  (let ((name (symbol-name name)))
    `(progn
       (set (intern ,name phpinspect-handler-obarray) (quote ,attribute-plist))
       (defalias (intern ,name phpinspect-handler-obarray)
         #'(lambda (,@arguments)
             ,docstring
             ,@body))
       (unless (byte-code-function-p
                (symbol-function
                 (intern ,name phpinspect-handler-obarray)))
         (byte-compile (intern ,name phpinspect-handler-obarray))))))

(defun phpinspect-get-parser-create (tree-type &rest parser-parameters)
  "Retrieve a parser for TREE-TYPE from `phpinspect-parser-obarray'.

TREE-TYPE must be a symbol or keyword representing the type of
the token the parser is able to parse.

If a parser by TREE-TYPE doesn't exist, it is created by callng
`phpinspect-make-parser` with TREE-TYPE as first argument and
PARSER-PARAMETERS as the rest of the arguments.  The resulting
parser function is then returned in byte-compiled form."
  (let* ((parser-name (symbol-name tree-type))
         (parser-symbol (intern-soft parser-name phpinspect-parser-obarray)))
    (or (and parser-symbol (symbol-function parser-symbol))
        (defalias (intern parser-name phpinspect-parser-obarray)
          (byte-compile (apply #'phpinspect-make-parser
                               `(,tree-type ,@parser-parameters)))))))

(defun phpinspect-purge-parser-cache ()
  "Empty `phpinspect-parser-obarray`.

This is useful when you need to change parser handlers or parsers
during runtime.  Parsers are implemented with macros, so changing
handler functions without calling this function will often not
have any effect."
  (interactive)
  (setq phpinspect-parser-obarray (obarray-make)))

(defun phpinspect-make-parser (tree-type handler-list &optional delimiter-predicate)
  "Create a parser function using the handlers by names defined in HANDLER-LIST.

See also `phpinspect-defhandler`.

TREE-TYPE must be a symbol or a keyword representing the token
type.

HANDLER-LIST must be a list of either symbol or string
representation of handler symbols which can be found in
`phpinspect-handler-obarray`.

DELIMITER-PREDICATE must be a function.  It is passed the last
parsed token after every handler iteration.  If it evaluates to
something other than nil, parsing is deemed completed and the
loop exits.  An example use case of this is to determine the end
of a statement.  You can use `phpinspect-terminator-p` as
delimiter predicate and have parsing stop when the last parsed
token is \";\", which marks the end of a statement in PHP."
  (let ((handlers (mapcar
                   (lambda (handler-name)
                     (let* ((handler-name (symbol-name handler-name))
                            (handler (intern-soft handler-name phpinspect-handler-obarray)))
                       (if handler
                           handler
                         (error "No handler found by name \"%s\"" handler-name))))
                   handler-list))
        (delimiter-predicate (if (symbolp delimiter-predicate)
                                 `(quote ,delimiter-predicate)
                               delimiter-predicate)))
    `(lambda (buffer max-point &optional continue-condition)
       (with-current-buffer buffer
         (let ((tokens)
               (delimiter-predicate (when (functionp ,delimiter-predicate) ,delimiter-predicate)))
           (while (and (< (point) max-point)
                       (if continue-condition (funcall continue-condition) t)
                       (not (if delimiter-predicate
                                (funcall delimiter-predicate (car (last tokens)))
                              nil)))
             (cond ,@(mapcar
                      (lambda (handler)
                        `((looking-at ,(plist-get (symbol-value handler) 'regexp))
                          (let ((token (funcall ,(symbol-function handler)
                                                (match-string 0)
                                                max-point)))
                            (when token
                              (if (null tokens)
                                  (setq tokens (list token))
                                (nconc tokens (list token)))))))
                      handlers)
                   (t (forward-char))))
           (push ,tree-type tokens))))))

(phpinspect-defhandler comma (comma &rest _ignored)
  "Handler for comma tokens"
  (regexp ",")
  (phpinspect-munch-token-without-attribs comma :comma))

(phpinspect-defhandler word (word &rest _ignored)
  "Handler for bareword tokens"
  (regexp "[A-Za-z_\\][\\A-Za-z_0-9]*")
  (let ((length (length word)))
    (forward-char length)
    (set-text-properties 0 length nil word)
    (list :word word)))

(defsubst phpinspect-handler (handler-name)
  (intern-soft (symbol-name handler-name) phpinspect-handler-obarray))

(defsubst phpinspect-handler-regexp (handler-name)
  (plist-get (symbol-value (phpinspect-handler handler-name)) 'regexp))

(defsubst phpinspect--parse-annotation-parameters (parameter-amount)
  (let* ((words)
         (word-handler (phpinspect-handler 'word))
         (word-regexp (phpinspect-handler-regexp 'word))
         (variable-handler (phpinspect-handler 'variable))
         (variable-regexp (phpinspect-handler-regexp 'variable)))
    (while (not (or (looking-at "\\*/") (= (length words) parameter-amount)))
      (forward-char)
      (cond ((looking-at word-regexp)
             (push (funcall word-handler (match-string 0)) words))
            ((looking-at variable-regexp)
             (push (funcall variable-handler (match-string 0)) words))))
    (nreverse words)))

(phpinspect-defhandler annotation (start-token &rest _ignored)
  "Handler for in-comment @annotations"
  (regexp "@")
  (forward-char (length start-token))
  (if (looking-at (phpinspect-handler-regexp 'word))
      (let ((annotation-name (match-string 0)))
        (forward-char (length annotation-name))
        (cond ((string= annotation-name "var")
               ;; The @var annotation accepts 2 parameters:
               ;; the type and the $variable name
               (append (list :var-annotation)
                       (phpinspect--parse-annotation-parameters 2)))
              ((string= annotation-name "return")
               ;; The @return annotation only accepts 1 word as parameter:
               ;; The return type
               (append (list :return-annotation)
                       (phpinspect--parse-annotation-parameters 1)))
              ((string= annotation-name "param")
                 ;; The @param annotation accepts 2 parameters:
                 ;; The type of the param, and the param's $name
                 (append (list :param-annotation)
                         (phpinspect--parse-annotation-parameters 2)))
              (t
               (list :annotation annotation-name))))
    (list :annotation nil)))

(phpinspect-defhandler tag (start-token max-point)
  "Handler that discards any inline HTML it encounters"
  (regexp "\\?>")
  (forward-char (length start-token))
  (or (re-search-forward "<\\?php\\|<\\?" nil t)
      (goto-char max-point))
  (list :html))

(phpinspect-defhandler comment (start-token max-point)
  "Handler for comments and doc blocks"
  (regexp "#\\|//\\|/\\*")
  (forward-char (length start-token))

  (cond ((string-match "/\\*" start-token)
         (let* ((continue-condition (lambda () (not (looking-at "\\*/"))))
                (parser (phpinspect-get-parser-create
                         :doc-block
                         '(annotation whitespace)))
                (doc-block (funcall parser (current-buffer) max-point continue-condition)))
           (forward-char 2)
           doc-block))
        (t
         (let ((parser (phpinspect-get-parser-create :comment '(tag) #'phpinspect-html-p))
               (end-position (line-end-position)))
           (funcall parser (current-buffer) end-position)))))

(phpinspect-defhandler variable (start-token &rest _ignored)
  "Handler for tokens indicating reference to a variable"
  (regexp "\\$")
  (forward-char (length start-token))
  (if (looking-at (phpinspect-handler-regexp 'word))
      (phpinspect-munch-token-without-attribs (match-string 0) :variable)
    (list :variable nil)))

(phpinspect-defhandler whitespace (whitespace &rest _ignored)
  "Handler that discards whitespace"
  (regexp "[[:blank:]]+")
  (forward-char (length whitespace)))

(phpinspect-defhandler equals (equals &rest _ignored)
  "Handler for strict and unstrict equality comparison tokens."
  (regexp "===?")
  (phpinspect-munch-token-without-attribs equals :equals))

(phpinspect-defhandler assignment-operator (operator &rest _ignored)
  "Handler for tokens indicating that an assignment is taking place"
  (regexp "[+-]?=")
  (phpinspect-munch-token-without-attribs operator :assignment))

(phpinspect-defhandler terminator (terminator &rest _ignored)
  "Handler for statement terminators."
  (regexp ";")
  (phpinspect-munch-token-without-attribs terminator :terminator))

(phpinspect-defhandler use-keyword (start-token max-point)
  "Handler for the use keyword and tokens that might follow to give it meaning"
  (regexp (concat "use" (phpinspect--word-end-regex)))
  (setq start-token (phpinspect--strip-last-char start-token))
  (forward-char (length start-token))

  (let ((parser (phpinspect-get-parser-create
                 :use
                 '(word tag block-without-scopes terminator)
                 #'phpinspect-end-of-use-p)))
    (funcall parser (current-buffer) max-point)))

(phpinspect-defhandler attribute-reference (start-token &rest _ignored)
  "Handler for references to object attributes, or static class attributes."
  (regexp "->\\|::")
  (forward-char (length start-token))
  (looking-at (phpinspect-handler-regexp 'word))
  (let ((name (if (looking-at (phpinspect-handler-regexp 'word))
                  (funcall (phpinspect-handler 'word) (match-string 0))
                nil)))
    (cond
     ((string= start-token "::")
      (list :static-attrib name))
     ((string= start-token "->")
      (list :object-attrib name)))))

(phpinspect-defhandler namespace (start-token max-point)
  "Handler for the namespace keyword. This is a special one
 because it is not always delimited by a block like classes or
 functions. This handler parses the namespace declaration, and
 then continues to parse subsequent tokens, only stopping when
 either a block has been parsed or another namespace keyword has
 been encountered."
  (regexp (concat "namespace" (phpinspect--word-end-regex)))
  (setq start-token (phpinspect--strip-last-char start-token))
  (forward-char (length start-token))
  (phpinspect-parse-with-handler-list
   (current-buffer)
   :namespace
   max-point
   (lambda () (not (looking-at (phpinspect-handler-regexp 'namespace))))
   #'phpinspect-block-p))

(phpinspect-defhandler const-keyword (start-token max-point)
  "Handler for the const keyword."
  (regexp (concat "const" (phpinspect--word-end-regex)))
  (setq start-token (phpinspect--strip-last-char start-token))
  (forward-char (length start-token))
  (let* ((parser (phpinspect-get-parser-create
                  :const
                  '(word comment assignment-operator string array
                         terminator)
                  #'phpinspect-end-of-token-p))
         (token (funcall parser (current-buffer) max-point)))
    (when (phpinspect-incomplete-token-p (car (last token)))
      (setcar token :incomplete-const))
    token))

(phpinspect-defhandler string (start-token &rest _ignored)
  "Handler for strings"
  (regexp "\"\\|'")
  (list :string (phpinspect--munch-string start-token)))

(phpinspect-defhandler block-without-scopes (start-token max-point)
  "Handler for code blocks that cannot contain scope, const or
static keywords with the same meaning as in a class block."
  (regexp "{")
  (forward-char (length start-token))
  (let* ((complete-block nil)
         (parser (phpinspect-get-parser-create
                  :block
                  '(array tag equals list comma
                          attribute-reference variable
                          assignment-operator whitespace
                          function-keyword word terminator here-doc
                          string comment block-without-scopes)))
         (continue-condition (lambda ()
                               (not (and (char-equal (char-after) ?})
                                         (setq complete-block t)))))
         (parsed (funcall parser (current-buffer) max-point continue-condition)))
    (if complete-block
        (forward-char)
      (setcar parsed :incomplete-block))
    parsed))


(phpinspect-defhandler class-block (start-token max-point)
  "Handler for code blocks that cannot contain classes"
  (regexp "{")
  (forward-char (length start-token))
  (let* ((complete-block nil)
         (parser (phpinspect-get-parser-create
                  :block
                  '(array tag equals list comma
                          attribute-reference variable
                          assignment-operator whitespace scope-keyword
                          static-keyword const-keyword use-keyword
                          function-keyword word terminator here-doc
                          string comment block)))
         (continue-condition (lambda ()
                               (not (and (char-equal (char-after) ?})
                                         (setq complete-block t)))))
         (parsed (funcall parser (current-buffer) max-point continue-condition)))
    (if complete-block
        (forward-char)
      (setcar parsed :incomplete-block))
    parsed))

(phpinspect-defhandler block (start-token max-point)
  "Handler for code blocks"
  (regexp "{")
  (forward-char (length start-token))
  (let* ((complete-block nil)
         (continue-condition (lambda ()
                               ;; When we encounter a closing brace for this
                               ;; block, we can mark the block as complete.
                               (not (and (char-equal (char-after) ?})
                                         (setq complete-block t)))))
         (parsed (phpinspect-parse-with-handler-list
                  (current-buffer) :block max-point continue-condition)))
    (if complete-block
        ;; After meeting the char-after requirement above, we need to move
        ;; one char forward to prevent parent-blocks from exiting because
        ;; of the same char.
        (forward-char)
      (setcar parsed :incomplete-block))
    parsed))

(phpinspect-defhandler here-doc (start-token &rest _ignored)
  "Handler for heredocs. Discards their contents."
  (regexp "<<<")
  (forward-char (length start-token))
  (if (looking-at "[A-Za-z0-9'\"\\_]+")
      (re-search-forward (concat "^" (regexp-quote (match-string 0))) nil t))
  (list :here-doc))


(defun phpinspect--munch-string (start-token)
  "Consume text at point until a non-escaped `START-TOKEN` is found.

Returns the consumed text string without face properties."
  (forward-char (length start-token))
  (let ((start-point (point)))
    (cond ((looking-at start-token)
           (forward-char)
           "")
          ((looking-at (concat "\\([\\][\\]\\)+" (regexp-quote start-token)))
           (let ((match (match-string 0)))
             (forward-char (length match))
             (buffer-substring-no-properties start-point
                                             (+ start-point (- (length match)
                                                               (length start-token))))))
          (t
           (re-search-forward (format "\\([^\\]\\([\\][\\]\\)+\\|[^\\]\\)%s"
                                      (regexp-quote start-token))
                              nil t)
           (buffer-substring-no-properties start-point (- (point) 1))))))

(phpinspect-defhandler list (start-token max-point)
  "Handler for php syntactic lists (Note: this does not include
datatypes like arrays, merely lists that are of a syntactic
nature like argument lists"
  (regexp "(")
  (forward-char (length start-token))
  (let* ((complete-list nil)
         (php-list (funcall
                    (phpinspect-get-parser-create
                     :list
                     '(array tag equals list comma
                             attribute-reference variable
                             assignment-operator whitespace
                             function-keyword word terminator here-doc
                             string comment block-without-scopes))
                    (current-buffer)
                    max-point
                    (lambda () (not (and (char-equal (char-after) ?\)) (setq complete-list t)))))))

    (if complete-list
        ;; Prevent parent-lists (if any) from exiting by skipping over the
        ;; ")" character
        (forward-char)
      (setcar php-list :incomplete-list))
    php-list))

;; TODO: Look into using different names for function and class :declaration tokens. They
;; don't necessarily require the same handlers to parse.
(defsubst phpinspect-get-or-create-declaration-parser ()
  (phpinspect-get-parser-create :declaration
                                '(comment word list terminator tag)
                                #'phpinspect-end-of-token-p))


(phpinspect-defhandler function-keyword (start-token max-point)
  "Handler for the function keyword and tokens that follow to give it meaning"
  (regexp (concat "function" (phpinspect--word-end-regex)))
  (setq start-token (phpinspect--strip-last-char start-token))
  (let* ((parser (phpinspect-get-or-create-declaration-parser))
         (continue-condition (lambda () (not (char-equal (char-after) ?{))))
         (declaration (funcall parser (current-buffer) max-point continue-condition)))
    (if (phpinspect-end-of-token-p (car (last declaration)))
        (list :function declaration)
      (list :function
            declaration
            (funcall (phpinspect-handler 'block-without-scopes)
                     (char-to-string (char-after)) max-point)))))

(phpinspect-defhandler scope-keyword (start-token max-point)
  "Handler for scope keywords"
  (regexp (mapconcat (lambda (word)
                       (concat word (phpinspect--word-end-regex)))
                     (list "public" "private" "protected")
                     "\\|"))
  (setq start-token (phpinspect--strip-last-char start-token))
  (forward-char (length start-token))
  (funcall (phpinspect-get-parser-create
            (cond ((string= start-token "public") :public)
                  ((string= start-token "private") :private)
                  ((string= start-token "protected") :protected))
            '(function-keyword static-keyword const-keyword
                               variable here-doc string terminator tag comment)
            #'phpinspect--scope-terminator-p)
           (current-buffer)
           max-point))

(phpinspect-defhandler static-keyword (start-token max-point)
  "Handler for the static keyword"
  (regexp (concat "static" (phpinspect--word-end-regex)))
  (setq start-token (phpinspect--strip-last-char start-token))
  (forward-char (length start-token))
  (funcall (phpinspect-get-parser-create
            :static
            '(comment function-keyword variable array word
                      terminator tag)
            #'phpinspect--static-terminator-p)
           (current-buffer)
           max-point))

(phpinspect-defhandler fat-arrow (arrow &rest _ignored)
  "Handler for the \"fat arrow\" in arrays and foreach expressions"
  (regexp "=>")
  (phpinspect-munch-token-without-attribs arrow :fat-arrow))

(phpinspect-defhandler array (start-token max-point)
  "Handler for arrays, in the bracketet as well as the list notation"
  (regexp "\\[\\|array(")
  (forward-char (length start-token))
  (let* ((end-char (cond ((string= start-token "[") ?\])
                         ((string= start-token "array(") ?\))))
         (end-char-reached nil)
         (token (funcall (phpinspect-get-parser-create
                          :array
                          '(comment comma list here-doc string
                                    array variable attribute-reference
                                    word fat-arrow))
                         (current-buffer)
                         max-point
                         (lambda () (not (and (char-equal (char-after) end-char)
                                              (setq end-char-reached t)))))))

    ;; Skip over the end char to prevent enclosing arrays or lists
    ;; from terminating.
    (if end-char-reached
        (forward-char)
      ;; Signal incompleteness when terminated because of max-point
      (setcar token :incomplete-array))
    token))

(phpinspect-defhandler class-keyword (start-token max-point)
  "Handler for the class keyword, and tokens that follow to define
the properties of the class"
  (regexp (concat "\\(abstract\\|final\\|class\\|interface\\|trait\\)"
                  (phpinspect--word-end-regex)))
  (setq start-token (phpinspect--strip-last-char start-token))
  (list :class (funcall (phpinspect-get-or-create-declaration-parser)
                        (current-buffer)
                        max-point
                        (lambda () (not (char-equal (char-after) ?{))))
        (funcall (phpinspect-handler 'class-block)
                 (char-to-string (char-after)) max-point)))

(defun phpinspect-parse-with-handler-list
    (buffer tree-type max-point &optional continue-condition delimiter-predicate)
  "Parse BUFFER for TREE-TYPE tokens until MAX-POINT.

Stop at CONTINUE-CONDITION or DELIMITER-PREDICATE.

This just calls `phpinspect-get-parser-create` to make a parser
that contains all handlers necessary to parse code."
  (let ((parser (phpinspect-get-parser-create
                 tree-type
                 '(array tag equals list comma
                         attribute-reference variable
                         assignment-operator whitespace scope-keyword
                         static-keyword const-keyword use-keyword
                         class-keyword function-keyword word terminator
                         here-doc string comment block)
                 delimiter-predicate)))
    (funcall parser buffer max-point continue-condition)))


(defun phpinspect-parse-buffer-until-point (buffer point)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "<\\?php\\|<\\?" nil t)
      (funcall (phpinspect-get-parser-create
                :root
                '(namespace array equals list comma
                            attribute-reference variable assignment-operator
                            whitespace scope-keyword static-keyword
                            const-keyword use-keyword class-keyword
                            function-keyword word terminator here-doc string
                            comment tag block))
               (current-buffer)
               point))))

(cl-defstruct (phpinspect--type
               (:constructor phpinspect--make-type-generated))
  "Represents an instance of a PHP type in the phpinspect syntax tree."
  (name-symbol nil
               :type symbol
               :documentation
               "Symbol representation of the type name.")
  (collection nil
              :type bool
              :documentation
              "Whether or not the type is a collection")
  (contains nil
            :type phpinspect--type
            :documentation
            "When the type is a collection, this attribute is set to the type that the collection is expected to contain")
  (fully-qualified nil
                   :type bool
                   :documentation
                   "Whether or not the type name is fully qualified"))

(defsubst phpinspect--wrap-plist-name-in-symbol (property-list)
  (let ((new-plist)
        (wrap-value))
    (dolist (item property-list)
      (when wrap-value
        (setq item `(phpinspect-intern-name ,item))
        (setq wrap-value nil))
      (when (eq item :name)
        (setq item :name-symbol)
        (setq wrap-value t))
      (push item new-plist))
    (nreverse new-plist)))

(defmacro phpinspect--make-type (&rest property-list)
  `(phpinspect--make-type-generated
    ,@(phpinspect--wrap-plist-name-in-symbol property-list)))

(defconst phpinspect--null-type (phpinspect--make-type :name "\\null"))

(cl-defmethod phpinspect--type-set-name ((type phpinspect--type) (name string))
  (setf (phpinspect--type-name-symbol type) (phpinspect-intern-name name)))

(cl-defmethod phpinspect--type-name ((type phpinspect--type))
  (symbol-name (phpinspect--type-name-symbol type)))

(cl-defmethod phpinspect--type= ((type1 phpinspect--type) (type2 phpinspect--type))
  (eq (phpinspect--type-name-symbol type1) (phpinspect--type-name-symbol type2)))

(cl-defstruct (phpinspect--function (:constructor phpinspect--make-function-generated))
  "A PHP function."
  (name-symbol nil
               :type symbol
               :documentation
               "A symbol associated with the name of the function")
  (scope nil
         :type phpinspect-scope
         :documentation
         "When the function is a method, this should contain the
scope of the function as returned by `phpinspect-parse-scope`.")
  (arguments nil
             :type list
             :documentation
             "A simple list with function arguments and their
types in tuples. Each list should have the name of the variable
as first element and the type as second element.")
  (return-type nil
               :type phpinspect--type
               :documentation
               "A phpinspect--type object representing the the
return type of the function."))

(defmacro phpinspect--make-function (&rest property-list)
  `(phpinspect--make-function-generated
    ,@(phpinspect--wrap-plist-name-in-symbol property-list)))

(cl-defmethod phpinspect--function-set-name ((func phpinspect--function) (name string))
  (setf (phpinspect--function-name-symbol func) (intern name phpinspect-name-obarray)))

(cl-defgeneric phpinspect--function-name ((func phpinspect--function)))

(cl-defmethod phpinspect--function-name ((func phpinspect--function))
  (symbol-name (phpinspect--function-name-symbol func)))


(cl-defstruct (phpinspect--variable (:constructor phpinspect--make-variable))
  "A PHP Variable."
  (name nil
        :type string
        :documentation
        "A string containing the name of the variable.")
  (scope nil
         :type phpinspect-scope
         :documentation
         "When the variable is an object attribute, this should
contain the scope of the variable as returned by
`phpinspect-parse-scope`")
  (type nil
        :type string
        :documentation
        "A string containing the FQN of the variable's type"))

(cl-defstruct (phpinspect--completion
               (:constructor phpinspect--construct-completion))
  "Contains a possible completion value with all it's attributes."
  (value nil :type string)
  (meta nil :type string)
  (annotation nil :type string)
  (kind nil :type symbol))

(cl-defgeneric phpinspect--format-type-name (name)
  (string-remove-prefix "\\" name))

(cl-defmethod phpinspect--format-type-name ((type phpinspect--type))
  (phpinspect--format-type-name (phpinspect--type-name type)))

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

(defun phpinspect-toggle-logging ()
  (interactive)
  (if (setq phpinspect--debug (not phpinspect--debug))
      (message "Enabled phpinspect logging.")
    (message "Disabled phpinspect logging.")))


(defsubst phpinspect--log (&rest args)
  (when phpinspect--debug
    (with-current-buffer (get-buffer-create "**phpinspect-logs**")
      (unless window-point-insertion-type
        (set (make-local-variable 'window-point-insertion-type) t))
      (goto-char (buffer-end 1))
      (insert (concat "[" (format-time-string "%H:%M:%S") "]: "
                           (apply #'format args) "\n")))))

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


;; (defun phpinspect-get-cached-project-class-methods
;;     (project-root class-fqn &optional static)
;;   (phpinspect--log "Getting cached project class methods for %s (%s)"
;;                    project-root class-fqn)
;;   (when project-root
;;     (let ((index (phpinspect-get-or-create-cached-project-class
;;                   project-root
;;                   class-fqn)))
;;       (when index
;;         (phpinspect--log "Retrieved class index, starting method collection %s (%s)"
;;                          project-root class-fqn)

;;         (let ((methods-key (if static 'static-methods 'methods))
;;               (methods))

;;           (dolist (class (phpinspect-get-project-class-inherit-classes
;;                           project-root
;;                           index))
;;             (dolist (method (alist-get methods-key class))
;;               (push method methods)))

;;           (dolist (method (alist-get methods-key index))
;;             (push method methods))

;;           (nreverse methods))))))

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
          (phpinspect--class-get-method-list class)))))

(defmacro phpinspect-find-function-in-list (method-name list)
  (let ((break-sym (gensym))
        (method-name-sym (gensym)))
    `(let ((,method-name-sym (phpinspect-intern-name ,method-name)))
       (catch (quote ,break-sym)
         (dolist (func ,list)
           (when (eq (phpinspect--function-name-symbol func)
                     ,method-name-sym)
             (throw (quote ,break-sym) func)))))))

;; (defsubst phpinspect-get-cached-project-class-method-type
;;   (project-root class-fqn method-name)
;;   (when project-root
;;     (phpinspect--log "Getting cached project class method type for %s (%s::%s)"
;;                      project-root class-fqn method-name)
;;     (let ((found-method
;;             (phpinspect-find-function-in-list
;;              method-name
;;              (phpinspect-get-cached-project-class-methods project-root class-fqn))))
;;       (when found-method
;;         (phpinspect--log "Found method: %s" found-method)
;;         (phpinspect--function-return-type found-method)))))

(defsubst phpinspect-get-cached-project-class-method-type
  (project-root class-fqn method-name)
    (when project-root
    (let* ((class (phpinspect-get-cached-project-class project-root class-fqn))
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
    (let* ((class (phpinspect-get-cached-project-class project-root class-fqn))
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

(defun phpinspect-get-variable-type-in-function-arg-list (variable-name arg-list)
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
              (car (last arg))
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
             (class-methods (phpinspect-get-cached-project-class-methods
                            (phpinspect--resolvecontext-project-root resolvecontext)
                            type-of-previous-statement
                            static))
             (method (and class-methods
                          (phpinspect-find-function-in-list method-name class-methods))))
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
    (phpinspect--log "Yayo Returning %s" assignments)
    (phpinspect--log "Yayo had statements %s" statements)
    assignments))

(cl-defmethod phpinspect--find-assignments-in-token ((token (head :list)))
  "Find assignments that are in a list token."
  (phpinspect--log "looking for assignments in list %s" token)
  (let ((ajaja (seq-filter
                (lambda (statement)
                  (phpinspect--log "checking statement %s" statement)
                  (seq-find #'phpinspect-maybe-assignment-p statement))
                (phpinspect--split-list #'phpinspect-end-of-statement-p (cdr token)))))
    (phpinspect--log "Found ajaja %s" ajaja)
    ajaja))

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
      ;; When the right of an assignment is more than $variable; or "string";(so
      ;; (:variable "variable") (:terminator ";") or (:string "string") (:terminator ";")
      ;; in tokens), we're likely working with a derived assignment like $object->method()
      ;; or $object->attribute
      (cond ((and (phpinspect-word-p (car last-assignment-value))
                  (string= (cadar last-assignment-value) "new"))
             (funcall type-resolver (phpinspect--make-type :name (cadadr last-assignment-value))))
            ((and (> (length last-assignment-value) 2)
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
             (phpinspect--log "Variable was assigned with the value of another variable")
             (or (when function-arg-list
                   (phpinspect-get-variable-type-in-function-arg-list (cadar last-assignment-value)
                                                                      function-arg-list))
                 (phpinspect-get-variable-type-in-block resolvecontext
                                                        (cadar last-assignment-value)
                                                        php-block
                                                        type-resolver
                                                        function-arg-list)))
            ((not assignments)
             (phpinspect--log "No assignments found for variable %s, checking function arguments" variable-name)
             (phpinspect-get-variable-type-in-function-arg-list variable-name function-arg-list))))))


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

(defun phpinspect--function-from-scope (scope)
  (cond ((and (phpinspect-static-p (cadr scope))
              (phpinspect-function-p (caddr scope)))
         (caddr scope))
        ((phpinspect-function-p (cadr scope))
         (cadr scope))
        (t nil)))

(defun phpinspect--make-type-resolver (types &optional token-tree namespace)
  "Little wrapper closure to pass around and resolve types with."

  (let* ((inside-class
          (if token-tree (or (phpinspect--find-innermost-incomplete-class token-tree)
                             (phpinspect--find-class-token token-tree))))
         (inside-class-name (if inside-class (phpinspect--get-class-name-from-token
                                              inside-class)))
         (self-type (phpinspect--make-type :name "self"))
         (static-type (phpinspect--make-type :name "static")))
    (lambda (type)
      (phpinspect--type-resolve
       types
       namespace
       (if (and inside-class-name (or (phpinspect--type= type self-type)
                                      (phpinspect--type= type static-type)))
           (progn
             (phpinspect--log "Returning inside class name for %s : %s"
                              type inside-class-name)
             (phpinspect--make-type :name inside-class-name))
         ;; else
         type)))))


(defun phpinspect--resolve-type-name (types namespace type)
  "Get the FQN for TYPE, using TYPES and NAMESPACE as context.

TYPES must be an alist with class names as cars and FQNs as cdrs.
NAMESPACE may be nil, or a string with a namespace FQN."
  (phpinspect--log "Resolving %s from namespace %s" type namespace)
  ;; Absolute FQN
  (cond ((string-match "^\\\\" type)
         type)

        ;; Native type
        ((member type phpinspect-native-types)
         (concat "\\" type))

        ;; Relative FQN
        ((and namespace (string-match "\\\\" type))
         (concat "\\" namespace "\\" type))

        ;; Clas|interface|trait name
        (t (let ((from-types (assoc-default (phpinspect-intern-name type) types #'eq)))
             (concat "\\" (cond (from-types
                                 (phpinspect--type-name from-types))
                                (namespace
                                 (concat namespace "\\" type))
                                (t type)))))))

(cl-defmethod phpinspect--type-resolve (types namespace (type phpinspect--type))
  (unless (phpinspect--type-fully-qualified type)
    (phpinspect--type-set-name
     type
     (phpinspect--resolve-type-name types namespace (phpinspect--type-name type)))
    (setf (phpinspect--type-fully-qualified type) t))
  type)

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

(defun phpinspect--index-function-from-scope (type-resolver scope comment-before)
  (let* ((php-func (cadr scope))
         (declaration (cadr php-func))
         (type (if (phpinspect-word-p (car (last declaration)))
                   (phpinspect--make-type :name (cadar (last declaration))))))

    ;; @return annotation. Has precedence over typehint when dealing with a collection.
    (let* ((is-collection
           (when type
             (member (phpinspect--type-name
                      (funcall type-resolver type)) phpinspect-collection-types)))
           (return-annotation-type
            (when (or (not type) is-collection)
              (cadadr
               (seq-find #'phpinspect-return-annotation-p
                         comment-before)))))
      (phpinspect--log "found return annotation %s" return-annotation-type)

      (when return-annotation-type
        (cond ((not type)
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

(defun phpinspect--index-class (type-resolver class)
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
                     (methods . ,methods)
                     (class-name . ,class-name)
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

(defun phpinspect--index-classes (types classes &optional namespace indexed)
  "Index the class tokens in `classes`, using the types in `types`
as Fully Qualified names. `namespace` will be assumed the root
namespace if not provided"
  (if classes
      (let ((class (pop classes)))
        (push (phpinspect--index-class
               (phpinspect--make-type-resolver types class namespace)
               class)
              indexed)
        (phpinspect--index-classes types classes namespace indexed))
    (nreverse indexed)))

(defun phpinspect--use-to-type (use)
  (let* ((fqn (cadr (cadr use)))
         (type (phpinspect--make-type :name fqn :fully-qualified t))
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
  `(phpinspect--root-index
    ,(append
      (append '(classes)
              (phpinspect--index-namespaces (seq-filter #'phpinspect-namespace-p tokens))
              (phpinspect--index-classes
               (phpinspect--uses-to-types (seq-filter #'phpinspect-use-p tokens))
               (seq-filter #'phpinspect-class-p tokens))))
    (functions))
  ;; TODO: Implement function indexation
  )

;; (defun phpinspect--get-or-create-index-for-class-file (class-fqn)
;;   (phpinspect--log "Getting or creating index for %s" class-fqn)
;;   (phpinspect-get-or-create-cached-project-class
;;    (phpinspect-project-root)
;;    class-fqn))

(defun phpinspect-index-file (file-name)
  (phpinspect--index-tokens (phpinspect-parse-file file-name)))

(defun phpinspect-get-or-create-cached-project-class (project-root class-fqn)
  (when project-root
    (let ((project (phpinspect--cache-get-project-create
                    (phpinspect--get-or-create-global-cache)
                    project-root)))
      (phpinspect--project-get-class-create project class-fqn))))

    ;; (let ((existing-index (phpinspect-get-cached-project-class
    ;;                        project-root
    ;;                        class-fqn)))
    ;;   (or
    ;;    existing-index
    ;;    (progn
    ;;      (let* ((class-file (phpinspect-class-filepath class-fqn))
    ;;             (visited-buffer (when class-file (find-buffer-visiting class-file)))
    ;;             (new-index))

    ;;        (phpinspect--log "No existing index for FQN: %s" class-fqn)
    ;;        (phpinspect--log "filepath: %s" class-file)
    ;;        (when class-file
    ;;          (if visited-buffer
    ;;              (setq new-index (with-current-buffer visited-buffer
    ;;                                (phpinspect--index-current-buffer)))
    ;;            (setq new-index (phpinspect-index-file class-file)))
    ;;          (dolist (class (alist-get 'classes new-index))
    ;;            (when class
    ;;              (phpinspect-cache-project-class
    ;;               project-root
    ;;               (cdr class))))
    ;;          (alist-get class-fqn (alist-get 'classes new-index)
    ;;                     nil
    ;;                     nil
    ;;                     #'phpinspect--type=))))))))


(defun phpinspect--index-current-buffer ()
  (phpinspect--index-tokens (phpinspect-parse-current-buffer)))

(defun phpinspect-index-current-buffer ()
  "Index a PHP file for classes and the methods they have"
  (phpinspect--index-tokens (phpinspect-parse-current-buffer)))

;; (defun phpinspect--get-variables-for-class (buffer-classes class &optional static)
;;   (let ((class-index (or (assoc-default class buffer-classes #'phpinspect--type=)
;;                          (phpinspect--get-or-create-index-for-class-file class))))
;;     (when class-index
;;       (if static
;;           (append (alist-get 'static-variables class-index)
;;                   (alist-get 'constants class-index))
;;         (alist-get 'variables class-index)))))

(defun phpinspect--get-variables-for-class (buffer-classes class-name &optional static)
  (let ((class (phpinspect-get-or-create-cached-project-class
                (phpinspect-project-root)
                class-name)))
    (when class (phpinspect--class-variables class))))

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
    (dolist (class (alist-get 'classes phpinspect--buffer-index))
      (when class
        (phpinspect-cache-project-class (phpinspect-project-root)
                                        (cdr class))))))

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

(cl-defgeneric phpinspect--merge-indexes (index1 index2)
  "Merge two phpinspect index types into one and return it")

(cl-defmethod phpinspect--merge-indexes
  ((class1 (head phpinspect--indexed-class))
   (class2 (head phpinspect--indexed-class)))
  "Merge two indexed classes."
  (let* ((class1-methods (alist-get 'methods (cdr class1)))
         (class1-variables (alist-get 'variables (cdr class1))))
    (dolist (method (alist-get 'methods (cdr class2)))
      (cl-pushnew method class1-methods :test #'equal))
    (setf (alist-get 'methods (cdr class1)) class1-methods)

    (dolist (variable (alist-get 'variables (cdr class2)))
      (cl-pushnew variable class1-variables :test #'equal))
    (setf (alist-get 'variables (cdr class1)) class1-variables))
  class1)

(cl-defmethod phpinspect--merge-indexes
  ((index1 (head phpinspect--root-index))
   (index2 (head phpinspect--root-index)))
  (let ((index1-classes (alist-get 'classes (cdr index1)))
        (index2-classes (alist-get 'classes (cdr index2))))
    (dolist (class index2-classes)
      (when class
        (let* ((class-name (alist-get 'class-name (cdr class)))
               (existing-class (alist-get class-name index1-classes nil nil 'phpinspect--type=)))
          (if existing-class
              (progn
                (phpinspect--log "Found existing class in root index: %s" class-name)
                (setcdr (assoc class-name index1-classes)
                        (phpinspect--merge-indexes existing-class (cdr class))))
            ;; else
            (phpinspect--log "Didn't find existing class in root index: %s" class-name)
            (push class index1-classes)))))
    (setf (alist-get 'classes index1) index1-classes)
    index1))

(defsubst phpinspect-not-variable-p (token)
  (not (phpinspect-variable-p token)))

(defun phpinspect--get-bare-class-name-from-fqn (fqn)
  (car (last (split-string fqn "\\\\"))))

(cl-defmethod phpinspect--type-bare-name ((type phpinspect--type))
  (phpinspect--get-bare-class-name-from-fqn (phpinspect--type-name type)))

(cl-defmethod phpinspect--make-completion
  ((completion-candidate phpinspect--variable))
  (phpinspect--construct-completion
   :value (phpinspect--variable-name completion-candidate)
   :meta (phpinspect--format-type-name
          (phpinspect--variable-type completion-candidate))
   :annotation (concat " "
                       (phpinspect--type-bare-name
                        (or (phpinspect--variable-type completion-candidate)
                            (phpinspect--make-type :name "\\"))))
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
    variables))

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


(cl-defstruct (phpinspect--cache (:constructor phpinspect--make-cache))
  (active-projects nil
                   :type alist
                   :documentation
                   "An `alist` that contains the root directory
                   paths of all currently active phpinspect
                   projects")
  (projects (make-hash-table :test 'equal :size 10)
            :type hash-table
            :documentation
            "A `hash-table` with the root directories of projects
as keys and project caches as values."))

(cl-defstruct (phpinspect--project (:constructor phpinspect--make-project-cache))
  (class-index (make-hash-table :test 'eq :size 100 :rehash-size 40)
               :type hash-table
               :documentation
               "A `hash-table` that contains all of the currently
indexed classes in the project"))

(cl-defstruct (phpinspect--class (:constructor phpinspect--make-class-generated))
  (project nil
           :type phpinspect--project
           :documentaton
           "The project that this class belongs to")
  (index nil
         :type phpinspect--indexed-class
         :documentation
         "The index that this class is derived from")
  (methods (make-hash-table :test 'eq :size 20 :rehash-size 20)
           :type hash-table
           :documentation
           "All methods, including those from extended classes.")
  (static-methods (make-hash-table :test 'eq :size 20 :rehash-size 20)
                  :type hash-table
                  :documentation
                  "All static methods this class provides,
                  including those from extended classes.")
  (variables nil
             :type list
             :documentation
             "Variables that belong to this class.")
  (extended-classes (make-hash-table :test 'eq)
                    :type hash-table
                    :documentation
                    "All extended/implemented classes.")
  (subscriptions nil
                 :type list
                 :documentation
                 "A list of subscription functions that should be
                 called whenever anything about this class is
                 updated"))

(cl-defmethod phpinspect--class-trigger-update ((class phpinspect--class))
  (dolist (sub (phpinspect--class-subscriptions class))
    (funcall sub class)))

(cl-defmethod phpinspect--class-set-index ((class phpinspect--class)
                                           (index (head phpinspect--indexed-class)))
  (setf (phpinspect--class-index class) index)
  (dolist (method (alist-get 'methods index))
    (phpinspect--class-update-method class method))

  (dolist (method (alist-get 'static-methods index))
    (phpinspect--class-update-static-method class method))

  (setf (phpinspect--class-variables class)
        (alist-get 'variables index))

  (setf (phpinspect--class-extended-classes class)
        (seq-filter
         #'phpinspect--class-p
         (mapcar
          (lambda (class-name)
            (phpinspect--project-get-class-create (phpinspect--class-project class)
                                           class-name))
          `(,@(alist-get 'implements index) ,@(alist-get 'extends index)))))

  (dolist (extended (phpinspect--class-extended-classes class))
    (phpinspect--class-incorporate class extended)
    (phpinspect--class-subscribe class extended))

  (phpinspect--class-trigger-update class))

(cl-defmethod phpinspect--class-get-method ((class phpinspect--class) method-name)
  (gethash method-name (phpinspect--class-methods class)))

(cl-defmethod phpinspect--class-get-static-method ((class phpinspect--class) method-name)
  (gethash method-name (phpinspect--class-static-methods class)))

(cl-defmethod phpinspect--class-set-method ((class phpinspect--class)
                                            (method phpinspect--function))
  (phpinspect--log "Adding method by name %s to class"
                   (phpinspect--function-name method))
  (puthash (phpinspect--function-name-symbol method)
           method
           (phpinspect--class-methods class)))

(cl-defmethod phpinspect--class-set-static-method ((class phpinspect--class)
                                                   (method phpinspect--function))
  (puthash (phpinspect--function-name-symbol method)
           method
           (phpinspect--class-static-methods class)))

(cl-defmethod phpinspect--class-get-method-return-type
  ((class phpinspect--class) (method-name symbol))
  (let ((method (phpinspect--class-get-method class method-name)))
    (when method
      (phpinspect--function-return-type method))))

(cl-defmethod phpinspect--class-get-method-list ((class phpinspect--class))
  (let ((methods))
    (maphash (lambda (key method)
               (push method methods))
             (phpinspect--class-methods class))
    methods))

(cl-defmethod phpinspect--class-update-static-method ((class phpinspect--class)
                                                      (method phpinspect--function))
  (let ((existing (gethash (phpinspect--function-name-symbol method)
                           (phpinspect--class-static-methods class))))
    (if existing
        (progn
          (unless (eq (phpinspect--function-return-type method)
                    phpinspect--null-type)
          (setf (phpinspect--function-return-type existing)
                (phpinspect--function-return-type method))
          (setf (phpinspect--function-arguments existing)
                (phpinspect--function-arguments method))))
      (phpinspect--class-set-static-method class method))))

(cl-defmethod phpinspect--class-update-method ((class phpinspect--class)
                                               (method phpinspect--function))
  (let ((existing (gethash (phpinspect--function-name-symbol method)
                           (phpinspect--class-methods class))))
    (if existing
        (progn
          (unless (eq (phpinspect--function-return-type method)
                    phpinspect--null-type)
          (setf (phpinspect--function-return-type existing)
                (phpinspect--function-return-type method))
          (setf (phpinspect--function-arguments existing)
                (phpinspect--function-arguments method))))
      (phpinspect--class-set-method class method))))

(cl-defmethod phpinspect--class-incorporate ((class phpinspect--class)
                                             (other-class phpinspect--class))
  (maphash (lambda (k method)
             (phpinspect--class-update-method class method))
           (phpinspect--class-methods other-class)))

(cl-defmethod phpinspect--class-subscribe ((class phpinspect--class)
                                           (subscription-class phpinspect--class))
  (let ((update-function
         (lambda (new-class)
           (phpinspect--class-incorporate class new-class)
           (phpinspect--class-trigger-update class))))
    (push update-function (phpinspect--class-subscriptions subscription-class))))
1
(cl-defgeneric phpinspect--cache-getproject
    ((cache phpinspect--cache) (project-name string))
  "Get project by PROJECT-NAME that is located in CACHE.")

(cl-defmethod phpinspect--cache-getproject
  ((cache phpinspect--cache) (project-root string))
  (gethash project-root (phpinspect--cache-projects cache)))

(cl-defgeneric phpinspect--cache-get-project-create
    ((cache phpinspect--cache) (project-root string))
  "Get a project that is located in PROJECT-ROOT from CACHE.
If no such project exists in the cache yet, it is created and
then returned.")

(cl-defmethod phpinspect--cache-get-project-create
  ((cache phpinspect--cache) (project-root string))
  (or (phpinspect--cache-getproject cache project-root)
      (puthash project-root
               (phpinspect--make-project-cache)
               (phpinspect--cache-projects cache))))

(cl-defgeneric phpinspect--project-add-class
    ((project phpinspect--project) (class (head phpinspect--indexed-class)))
  "Add an indexed CLASS to PROJECT.")

(cl-defmethod phpinspect--project-add-class
  ((project phpinspect--project) (indexed-class (head phpinspect--indexed-class)))
  (let* ((class-name (phpinspect--type-name-symbol
                      (alist-get 'class-name (cdr indexed-class))))
         (existing-class (gethash class-name
                                  (phpinspect--project-class-index project))))
    (if existing-class
        (phpinspect--class-set-index existing-class indexed-class)
      (let ((new-class (phpinspect--make-class-generated :project project)))
        (phpinspect--class-set-index new-class indexed-class)
        (puthash class-name new-class (phpinspect--project-class-index project))))))

(cl-defgeneric phpinspect--project-get-class
    ((project phpinspect--project) (class-fqn phpinspect--type))
  "Get indexed class by name of CLASS-FQN stored in PROJECT.")

(cl-defmethod phpinspect--project-get-class-create
  ((project phpinspect--project) (class-fqn phpinspect--type))
  (let ((class (phpinspect--project-get-class project class-fqn)))
    (unless class
      (setq class (phpinspect--make-class-generated :project project))
      (puthash (phpinspect--type-name-symbol class-fqn)
               class
               (phpinspect--project-class-index project))

      (let* ((class-file (phpinspect-class-filepath class-fqn))
             (visited-buffer (when class-file (find-buffer-visiting class-file)))
             (new-index)
             (class-index))

        (phpinspect--log "No existing index for FQN: %s" class-fqn)
        (phpinspect--log "filepath: %s" class-file)
        (when class-file
          (if visited-buffer
              (setq new-index (with-current-buffer visited-buffer
                                (phpinspect--index-current-buffer)))
            (setq new-index (phpinspect-index-file class-file)))
          (setq class-index
                (alist-get class-fqn (alist-get 'classes new-index)
                           nil
                           nil
                           #'phpinspect--type=))
          (when class-index
            (phpinspect--class-set-index class class-index)))))
    class))

(cl-defmethod phpinspect--project-get-class
  ((project phpinspect--project) (class-fqn phpinspect--type))
  (gethash (phpinspect--type-name-symbol class-fqn)
           (phpinspect--project-class-index project)))

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
  (interactive (list (completing-read "Class: " (phpinspect-get-all-fqns))))
  (find-file (phpinspect-class-filepath fqn)))

(defun phpinspect-find-own-class-file (fqn)
  "`phpinspect-find-class-file', but for non-vendored classes.

When called interactively, presents the user with a list of
available FQNs for classes in the current project, which aren't
located in \"vendor\" folder."
  (interactive (list (completing-read "Class: " (phpinspect-get-all-fqns "uses_own"))))
  (find-file (phpinspect-class-filepath fqn)))

(defsubst phpinspect-class-filepath (fqn)
  "Call `phpinspect-class-filepath-function' with FQN as argument."
  (funcall phpinspect-class-filepath-function fqn))

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
