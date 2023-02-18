;;; phpinspect-parser.el --- PHP parsing module  -*- lexical-binding: t; -*-

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

(defvar phpinspect-parser-obarray (obarray-make)
  "An obarray containing symbols for all phpinspect (sub)parsers.")

(defvar phpinspect-handler-obarray (obarray-make)
  "An obarray containing symbols for all phpinspect parser handlers.")

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

(defun phpinspect-function-block (token)
  (cadr token))

(defun phpinspect-annotation-p (token)
  (phpinspect-token-type-p token :annotation))

(defun phpinspect-method-annotation-p (token)
  (phpinspect-token-type-p token :method-annotation))

(defun phpinspect-var-annotation-p (token)
  (phpinspect-token-type-p token :var-annotation))

(defun phpinspect-return-annotation-p (token)
  (phpinspect-token-type-p token :return-annotation))

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
  (or (phpinspect-token-type-p token :comment)
      (phpinspect-token-type-p token :doc-block)))

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
                          (let ((start-position (point))
                                (token (funcall ,(symbol-function handler)
                                                (match-string 0)
                                                max-point)))
                            (when token
                              (if (null tokens)
                                  (setq tokens (list token))
                                (progn
                                  (nconc tokens (list token))))

                                  ;; When parsing within a buffer that has
                                  ;; `phpinspect-current-buffer` set, update the
                                  ;; token location map. Usually, this variable
                                  ;; is set when `phpinspect-mode` is active.
                                  (when phpinspect-current-buffer
                                    (puthash token
                                             (phpinspect-make-region start-position
                                                                     (point))
                                             (phpinspect-buffer-location-map
                                              phpinspect-current-buffer)))))))
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
         (list-handler (phpinspect-handler 'list))
         (list-regexp (phpinspect-handler-regexp 'list))
         (word-handler (phpinspect-handler 'word))
         ;; Return annotations may end with "[]" for collections.
         (word-regexp (concat (phpinspect-handler-regexp 'word) "\\(\\[\\]\\)?"))
         (variable-handler (phpinspect-handler 'variable))
         (variable-regexp (phpinspect-handler-regexp 'variable))
         (annotation-regexp (phpinspect-handler-regexp 'annotation)))
    (while (not (or (looking-at annotation-regexp)
                    (= (point) (point-max))
                    (= (length words) parameter-amount)))
      (cond ((looking-at list-regexp)
             (push (funcall list-handler (match-string 0) (point-max)) words))
            ((looking-at word-regexp)
             (push (funcall word-handler (match-string 0)) words))
            ((looking-at variable-regexp)
             (push (funcall variable-handler (match-string 0)) words))
            (t (forward-char))))
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
              ((string= annotation-name "method")
               (append (list :method-annotation)
                       (phpinspect--parse-annotation-parameters 3)))
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
         (let* ((region-start (point))
                ;; Move to the end of the comment region
                (region-end
                 (progn
                   (while (not (or (= max-point (point)) (looking-at "\\*/")))
                     (forward-char))
                   (point)))
                (comment-contents (buffer-substring region-start region-end))
                (parser (phpinspect-get-parser-create
                         :doc-block
                         '(annotation whitespace)))
                (doc-block (with-temp-buffer
                             (insert comment-contents)
                             (goto-char (point-min))
                             (funcall parser (current-buffer) (point-max)))))
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
  (let ((parser (phpinspect-get-parser-create
                 :declaration
                 '(comment word list terminator tag)
                 #'phpinspect-end-of-token-p)))
    (lambda (&rest arguments)
      (let ((result (apply parser arguments)))
        (if (phpinspect-terminator-p (car (last result)))
          (butlast result)
          result)))))


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

(provide 'phpinspect-parser)
;;; phpinspect-parser.el ends here
