;;; phpinspect-parser.el --- PHP parsing module  -*- lexical-binding: t; -*-

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
(require 'phpinspect-edtrack)
(require 'phpinspect-bmap)
(require 'phpinspect-meta)
(require 'phpinspect-parse-context)
(require 'phpinspect-token-predicates)

(eval-when-compile
  (define-inline phpinspect--word-end-regex ()
    (inline-quote "\\([[:blank:]]\\|[^0-9a-zA-Z_]\\)")))

(define-inline phpinspect--strip-word-end-space (string)
  (inline-letevals (string)
    (inline-quote
     (progn
       (substring ,string 0 (- (length ,string) 1))))))

(define-inline phpinspect-munch-token-without-attribs (string token-keyword)
  "Return a token of type TOKEN-KEYWORD with STRING as value.
If STRING has text properties, they are stripped."
  (inline-letevals (string token-keyword)
    (inline-quote
     (let ((value (copy-sequence ,string))
           (length (length ,string)))
       (forward-char length)
       (set-text-properties 0 length nil value)
       (list ,token-keyword value)))))

(eval-and-compile
  (defun phpinspect-handler-func-name (handler-name)
    (intern (concat "phpinspect--" (symbol-name handler-name) "-handler")))

  (defun phpinspect-handler-regexp-func-name (handler-name)
    (intern (concat "phpinspect--" (symbol-name handler-name) "-handler-regexp")))

  (defun phpinspect-parser-func-name (name &optional suffix)
    (intern (concat "phpinspect--parse-" (symbol-name name) (if suffix (concat "-" suffix) "")))))

(defmacro phpinspect-defhandler (name arguments docstring attribute-alist &rest body)
  "Define a parser handler that becomes available for use with phpinspect-parse.

A parser handler is a function that is able to identify and parse
tokens from PHP code at `point` in the current buffer.  It's
return value must be the resulting token.  Aside from parsing it
has to manage the state of `point` in a way that it skips over
the tokens it has parsed.  That way the next handler can
correctly pick up from where it has left off.

Parser handlers are unrolled in a `cond` statement by
`phpinspect-make-parser-function` and equivalents.  The resulting
code is something akin to the following:

(while ...
  (cond (((looking-at \"{\")
          (funcall block-handler (match-string 0) max-point)
         ((looking-at \"\\$\")
          (funcall variable-handler ...
etc.

NAME must be a symbol.  It does not need to be prefixed with a
\"namespace\" because parser handlers are automatically prefixed.

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

ATTRIBUTE-ALIST is an alist that must contain at least a `regexp` key.
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

  (when (not (alist-get 'regexp attribute-alist))
    (error "In definition of phpinspect handler %s ATTRIBUTE-PLIST must contain key `regexp`"
           name))

  (let ((regexp-inline-name (phpinspect-handler-regexp-func-name name))
        (inline-name (phpinspect-handler-func-name name)))
    `(progn
       (define-inline ,regexp-inline-name ()
         (inline-letevals ((regexp ,(alist-get 'regexp attribute-alist)))
           (inline-quote ,(quote ,regexp))))

       (defsubst ,inline-name (,@arguments)
         ,docstring
         ,@body)

       (put (quote ,inline-name) 'phpinspect--handler t))))

(eval-when-compile
  (defun phpinspect-make-parser-function (name tree-type handlers &optional delimiter-predicate delimiter-condition)
    "Create a parser function using the handlers by names defined in HANDLER-LIST.

See also `phpinspect-defhandler`.

TREE-TYPE must be a symbol or a keyword representing the token
type.

HANDLERS must be a list of symbols referring to existing
parser handlers defined using `phpinspect-defhandler'.

DELIMITER-PREDICATE must be a function.  It is passed the last
parsed token after every handler iteration.  If it returns
something other than nil, parsing is deemed completed and the
loop exits.  An example use case of this is to determine the end
of a statement.  You can use `phpinspect-terminator-p` as
delimiter predicate and have parsing stop when the last parsed
token is \";\", which marks the end of a statement in PHP."
    (cl-assert (symbolp delimiter-predicate))
    `(defun ,(phpinspect-parser-func-name name "simple") (buffer max-point &optional skip-over continue-condition &rest _ignored)
       (with-current-buffer buffer
         (let* ((tokens (cons ,tree-type nil))
                (tokens-rear tokens)
                token)
           (when skip-over (forward-char skip-over))
           (while (and (< (point) max-point)
                       (if continue-condition (funcall continue-condition) t)
                       (not ,(if delimiter-predicate
                                 (if delimiter-condition
                                     `(and (,delimiter-condition tokens)
                                           (,delimiter-predicate (car (last tokens))))
                                   `(,delimiter-predicate (car (last tokens))))
                               nil)))
             (cond ,@(mapcar
                      (lambda (handler)
                        `((looking-at (,(phpinspect-handler-regexp-func-name handler)))
                          (setq token (,(phpinspect-handler-func-name handler) (match-string 0) max-point))
                          (when token
                            (setq tokens-rear (setcdr tokens-rear (cons token nil))))))
                      handlers)
                   (t (forward-char))))

           ;; Return
           tokens))))


  (defun phpinspect-make-incremental-parser-function
      (name tree-type handlers &optional delimiter-predicate delimiter-condition)
    "Like `phpinspect-make-parser-function', but returned function
is able to reuse an already parsed tree."
    (cl-assert (symbolp delimiter-predicate))
    `(defun ,(phpinspect-parser-func-name name "incremental")
         (context buffer max-point &optional skip-over continue-condition root)
       (with-current-buffer buffer
         (let* ((tokens (cons ,tree-type nil))
                (tokens-rear tokens)
                (root-start (point))
                (bmap (phpinspect-pctx-bmap context))
                (previous-bmap (phpinspect-pctx-previous-bmap context))
                (edtrack (phpinspect-pctx-edtrack context))
                (taint-iterator (when edtrack (phpinspect-edtrack-make-taint-iterator edtrack)))
                (check-interrupt (phpinspect-pctx-interrupt-predicate context))

                ;; Loop variables
                (start-position)
                (original-position)
                (current-end-position)
                (existing-meta)
                (delta)
                (token))
           (when skip-over (forward-char skip-over))

           (phpinspect-pctx-save-whitespace context
             (when (looking-at (phpinspect-handler-regexp whitespace))
               (,(phpinspect-handler-func-name 'whitespace) (match-string 0)))

             (while (and (< (point) max-point)
                         (if continue-condition (funcall continue-condition) t)
                         (not ,(if delimiter-predicate
                                   (if delimiter-condition
                                       `(and (,delimiter-condition tokens)
                                             (,delimiter-predicate (car (last tokens))))
                                     `(,delimiter-predicate (car (last tokens))))
                                 nil)))
               (when check-interrupt
                 (phpinspect-pctx-check-interrupt context))

               (setq start-position (point))
               (cond ((and previous-bmap edtrack
                           (setq existing-meta
                                 (phpinspect-bmap-token-starting-at
                                  previous-bmap
                                  (setq original-position
                                        (phpinspect-edtrack-original-position-at-point edtrack start-position))))
                           (not (or (phpinspect-root-p (phpinspect-meta-token existing-meta))
                                    (phpinspect-taint-iterator-token-is-tainted-p taint-iterator existing-meta))))
                      (setq delta (- start-position original-position)
                            current-end-position (+ (phpinspect-meta-end existing-meta) delta)
                            token (phpinspect-meta-token existing-meta))

                      ;;(message "Reusing token  %s at point %s" (phpinspect-meta-string existing-meta) (point))
                      ;; Re-register existing token
                      (phpinspect-bmap-overlay
                       bmap previous-bmap existing-meta delta
                       (phpinspect-pctx-consume-whitespace context))

                      (goto-char current-end-position)

                      ;; Skip over whitespace after so that we don't do a full
                      ;; run down all of the handlers during the next iteration
                      (when (looking-at (phpinspect-handler-regexp whitespace))
                        (,(phpinspect-handler-func-name 'whitespace) (match-string 0))))
                     ,@(mapcar
                        (lambda (handler)
                          `((looking-at (,(phpinspect-handler-regexp-func-name handler)))
                            (setq token (,(phpinspect-handler-func-name handler) (match-string 0) max-point))
                            (when token
                              (phpinspect-pctx-register-token context token start-position (point)))))
                        handlers)
                     ;; When no handlers match, whitespace can be discarded (if
                     ;; we call forward-char, it probably won't be accurate
                     ;; anymore anyways. One reason that no handlers matched
                     ;; could be that this parser does not have the whitespace
                     ;; handler and as such does not contain relevant
                     ;; whitespace.
                     (t (phpinspect-pctx-consume-whitespace context)
                        (forward-char)))
               (when token
                 (setq tokens-rear (setcdr tokens-rear (cons token nil)))
                 (setq token nil)))

             ;; When there is unconsumed whitespace, move back. It should not be
             ;; included in the current parent token's length.
             (backward-char (length (phpinspect-pctx-consume-whitespace context))))

           (when root
             (phpinspect-pctx-register-token context tokens root-start (point)))

           ;; Return
           tokens))))

  (cl-defstruct (phpinspect-parser (:constructor phpinspect-make-parser))
    (name 'root
          :type symbol
          :read-only t)
    (tree-keyword "root"
                  :type string
                  :read-only t
                  :documentation "Name of the keyword that is used as car of the
root token, in string form without \":\" prefix.")
    (handlers '(array tag equals list comma
                      attribute-reference variable
                      assignment-operator whitespace scope-keyword
                      static-keyword const-keyword use-keyword
                      class-keyword function-keyword word terminator
                      here-doc string string-concatenator comment block)
              :type list
              :read-only t
              :documentation "A list of symbols referring to the
handlers that this parser uses.")
    (delimiter-predicate nil
                         :type function
                         :read-only t
                         :documentation "A predicate function that is passed each
parsed token. When the predicate returns a non-nil value, the parser stops
executing.")
    (delimiter-condition nil
                         :type function
                         :read-only t
                         :documentation "A predicate function
 that is passed the list of parsed tokens after each parsed
 token. If it returns a non-nil value, the delimiter-predicate is
 invoked and checked.")
    (func nil
          :type function
          :documentation "The parser function.")
    (incremental-func nil
                      :type function
                      :documentation "Incemental parser function"))

  (cl-defmethod phpinspect-parser-compile ((parser phpinspect-parser))
    "Create/return parser function."
    (or (phpinspect-parser-func parser)
        (setf (phpinspect-parser-func parser)
              (phpinspect-make-parser-function
               (phpinspect-parser-name parser)
               (intern (concat ":" (phpinspect-parser-tree-keyword parser)))
               (phpinspect-parser-handlers parser)
               (phpinspect-parser-delimiter-predicate parser)
               (phpinspect-parser-delimiter-condition parser)))))

  (cl-defmethod phpinspect-parser-compile-incremental ((parser phpinspect-parser))
    "Like `phpinspect-parser-compile', but for an incremental
version of the parser function."
    (or (phpinspect-parser-incremental-func parser)
        (setf (phpinspect-parser-incremental-func parser)
              (phpinspect-make-incremental-parser-function
               (phpinspect-parser-name parser)
               (intern (concat ":" (phpinspect-parser-tree-keyword parser)))
               (phpinspect-parser-handlers parser)
               (phpinspect-parser-delimiter-predicate parser)
               (phpinspect-parser-delimiter-condition parser)))))

  (cl-defmethod phpinspect-parser-compile-entry ((parser phpinspect-parser))
    (let ((func-name (phpinspect-parser-func-name (phpinspect-parser-name parser)))
          (incremental-name (phpinspect-parser-func-name (phpinspect-parser-name parser) "incremental"))
          (simple-name (phpinspect-parser-func-name (phpinspect-parser-name parser) "simple")))
      `(defun ,func-name (buffer max-point &optional skip-over continue-condition root)
         "Parse BUFFER, starting at point and ending at MAX-POINT.

If SKIP-OVER is non-nil, it must be a number of characters that
to skip over before starting to parse.

If CONTINUE-CONDITION is non-nil, it must be a function. It will
be called after each parsed child token with the token as
argument. If the return value is nil, parsing is stopped.

If ROOT is non-nil, this signals that there is no parent parser
that will take care of registering metadata for the parser's
returned token tree. So the parser should register the metadata
of the root of its returned tree itself, before
returning. Currently, token metadata is only registered when
parsing incrementally."
         (if (and phpinspect-parse-context
                  (phpinspect-pctx-incremental phpinspect-parse-context))
             (,incremental-name phpinspect-parse-context buffer max-point skip-over continue-condition root)
           (,simple-name buffer max-point skip-over continue-condition root)))))

  ) ;; End of eval-when-compile

(defmacro phpinspect-defparser (name &rest parameters)
  (declare (indent 1))
  (unless (symbolp name)
    (error "Name must be a symbol"))

  (setq parameters (nconc parameters (list :name `(quote ,name))))

  (let* ((func-name (phpinspect-parser-func-name name))
         (simple-name (phpinspect-parser-func-name name "simple"))
         (incremental-name (phpinspect-parser-func-name name "incremental")))

    `(eval-when-compile
       (let ((parser (phpinspect-make-parser ,@parameters)))
         (defconst ,simple-name parser)
         (defconst ,incremental-name parser)

         (put (quote ,simple-name) 'phpinspect--parser t)
         (put (quote ,incremental-name) 'phpinspect--incremental-parser t)

         ;; Stub function to please the byte compiler (real function will be
         ;; defined by `phpinspect-define-parser-functions'.
         (defun ,func-name (_buffer _max-point &optional _skip-over _continue-condition _root))))))

(define-inline phpinspect-parser-func-bound-p (symbol)
  (inline-quote (get ,symbol 'phpinspect--parser)))

(define-inline phpinspect-incremental-parser-func-bound-p (symbol)
  (inline-quote (get ,symbol 'phpinspect--incremental-parser)))

(defun phpinspect-handler-bound-p (symbol)
  (get symbol 'phpinspect--handler))

(defmacro phpinspect-define-parser-functions ()
   (let (names incremental-names function-definitions)
     (obarray-map (lambda (sym)
                    (cond ((phpinspect-parser-func-bound-p sym)
                           (push sym names))
                          ((phpinspect-incremental-parser-func-bound-p sym)
                           (push sym incremental-names))))
                  obarray)

     (dolist (name names)
       (push (phpinspect-parser-compile-entry (symbol-value name))
             function-definitions))

     (dolist (name names)
       (push (phpinspect-parser-compile (symbol-value name))
             function-definitions))

     (dolist (name incremental-names)
       (push (phpinspect-parser-compile-incremental (symbol-value name))
             function-definitions))

     (push 'progn function-definitions)

     function-definitions))

(phpinspect-defhandler comma (comma &rest _ignored)
  "Handler for comma tokens"
  ((regexp . ","))
  (phpinspect-munch-token-without-attribs comma :comma))

(phpinspect-defhandler word (word &rest --length)
  "Handler for bareword tokens"
  ((regexp . "[A-Za-z_\\][\\A-Za-z_0-9]*"))
  (setq --length (length word))
  (forward-char --length)
  (set-text-properties 0 --length nil word)
  (list :word word))

(defmacro phpinspect-handler-regexp (handler-name)
  (unless (symbolp handler-name)
    (error "handler-name must be a known value and a symbol at compile time, name"))

  (let ((name (phpinspect-handler-regexp-func-name handler-name)))
    `(,name)))

(phpinspect-defhandler variable (start-token &rest _ignored)
  "Handler for tokens indicating reference to a variable"
  ((regexp . "\\$"))
  (forward-char (length start-token))
  (if (looking-at (phpinspect-handler-regexp word))
      (phpinspect-munch-token-without-attribs (match-string 0) :variable)
    (list :variable nil)))

(phpinspect-defparser list
  :tree-keyword "list"
  :handlers '(array tag equals list comma
                    attribute-reference variable assignment-operator
                    whitespace function-keyword word terminator here-doc
                    string string-concatenator comment block-without-scopes))

(phpinspect-defhandler list (start-token max-point)
  "Handler for php syntactic lists (Note: this does not include
datatypes like arrays, merely lists that are of a syntactic
nature like argument lists"
  ((regexp . "("))
  (let* ((complete-list nil)
         (php-list (phpinspect--parse-list
                    (current-buffer)
                    max-point
                    (length start-token)
                    (lambda () (not (and (char-equal (char-after) ?\)) (setq complete-list (point))))))))

    (if complete-list
        ;; Prevent parent-lists (if any) from exiting by skipping over the
        ;; ")" character
        (goto-char (+ complete-list 1))
      (setcar php-list :incomplete-list))
    php-list))

(defsubst phpinspect--parse-annotation-parameters (parameter-amount)
  (let* ((words)
         (list-regexp (phpinspect-handler-regexp list))
         ;; Return annotations may end with "[]" for collections.
         (word-regexp (concat (phpinspect-handler-regexp word) "\\(\\[\\]\\)?"))
         (variable-regexp (phpinspect-handler-regexp variable))
         (annotation-regexp (phpinspect-handler-regexp annotation)))
    (while (not (or (looking-at annotation-regexp)
                    (= (point) (point-max))
                    (= (length words) parameter-amount)))
      (cond ((looking-at list-regexp)
             (push (phpinspect--list-handler (match-string 0) (point-max)) words))
            ((looking-at word-regexp)
             (push (phpinspect--word-handler (match-string 0)) words))
            ((looking-at variable-regexp)
             (push (phpinspect--variable-handler (match-string 0)) words))
            (t (forward-char))))
    (nreverse words)))

(phpinspect-defhandler annotation (start-token &rest _ignored)
  "Handler for in-comment @annotations"
  ((regexp . "@"))
  (forward-char (length start-token))
  (if (looking-at (phpinspect-handler-regexp word))
      (let ((annotation-name (match-string 0)))
        (forward-char (length annotation-name))
        (cond ((string= annotation-name "var")
               ;; The @var annotation accepts 2 parameters:
               ;; the type and the $variable name
               (cons :var-annotation
                     (phpinspect--parse-annotation-parameters 2)))
              ((string= annotation-name "return")
               ;; The @return annotation only accepts 1 word as parameter:
               ;; The return type
               (cons :return-annotation
                     (phpinspect--parse-annotation-parameters 1)))
              ((string= annotation-name "param")
                 ;; The @param annotation accepts 2 parameters:
                 ;; The type of the param, and the param's $name
                 (cons :param-annotation
                       (phpinspect--parse-annotation-parameters 2)))
              ((string= annotation-name "method")
               (cons :method-annotation
                     (phpinspect--parse-annotation-parameters 4)))
              ((string= annotation-name "throws")
               (cons :throws-annotation
                     (phpinspect--parse-annotation-parameters 1)))
              (t
               (list :annotation annotation-name))))
    (list :annotation nil)))

(phpinspect-defhandler tag (start-token max-point)
  "Handler that discards any inline HTML it encounters"
  ((regexp . "\\?>"))
  (forward-char (length start-token))
  (or (re-search-forward "<\\?php\\|<\\?" nil t)
      (goto-char max-point))
  (list :html))

(phpinspect-defparser doc-block
  :tree-keyword "doc-block"
  :handlers '(annotation whitespace))

(phpinspect-defparser comment
  :tree-keyword "comment"
  :handlers '(tag)
  :delimiter-predicate #'phpinspect-html-p)

(phpinspect-defhandler comment (start-token max-point)
  "Handler for comments and doc blocks"
  ((regexp . "#\\|//\\|/\\*"))
  (forward-char (length start-token))

  (cond ((string-match "/\\*" start-token)
         (let* ((region-start (point))
                (found-delimiter nil)
                ;; Move to the end of the comment region
                (region-end
                 (progn
                   (while (not (or (= max-point (point))
                                   (and (looking-at "\\*/") (setq found-delimiter t))))
                     (forward-char))
                   (point)))
                (doc-block (save-restriction
                             (goto-char region-start)
                             (narrow-to-region region-start region-end)
                             (phpinspect--parse-doc-block (current-buffer) (point-max)))))
           ;; If a delimiter (*/) was found, skip over it.
           (when found-delimiter
             (forward-char 2))
           doc-block))
        (t
         (let* ((end-position (line-end-position))
		        (token
		         (phpinspect--parse-comment (current-buffer) end-position)))
	       ;; Move to start of next line (absorb end of line)
	       (while (not (or (bolp) (= max-point (point))))
	         (forward-char))
	       token))))

(phpinspect-defhandler class-variable (start-token &rest _ignored)
  "Handler for tokens indicating reference to a variable"
  ((regexp . "\\$"))
  (forward-char (length start-token))
  (if (looking-at (phpinspect-handler-regexp word))
      (phpinspect-munch-token-without-attribs (match-string 0) :class-variable)
    (list :class-variable nil)))

(phpinspect-defhandler whitespace (whitespace &rest _ignored)
  "Handler that discards whitespace"
  ((regexp . "[[:blank:]\n]+"))
  (when phpinspect-parse-context
    (phpinspect-pctx-register-whitespace phpinspect-parse-context whitespace))
  (forward-char (length whitespace)))

(phpinspect-defhandler equals (equals &rest _ignored)
  "Handler for strict and unstrict equality comparison tokens."
  ((regexp . "===?"))
  (phpinspect-munch-token-without-attribs equals :equals))

(phpinspect-defhandler assignment-operator (operator &rest _ignored)
  "Handler for tokens indicating that an assignment is taking place"
  ((regexp . "[+-]?="))
  (phpinspect-munch-token-without-attribs operator :assignment))

(phpinspect-defhandler string-concatenator (token &rest _ignored)
  "Handler for string concatenator tokens. (the . operator)."
  ((regexp . "\\."))
  (phpinspect-munch-token-without-attribs token :string-concatenator))

(phpinspect-defhandler terminator (terminator &rest _ignored)
  "Handler for statement terminators."
  ((regexp . ";"))
  (phpinspect-munch-token-without-attribs terminator :terminator))

(phpinspect-defparser use
  :tree-keyword "use"
  :handlers '(comment word tag block-without-scopes comma terminator)
  :delimiter-predicate #'phpinspect-end-of-use-p)

(phpinspect-defparser use-trait
  :tree-keyword "use-trait"
  :handlers '(comment word tag block-without-scopes comma terminator)
  :delimiter-predicate #'phpinspect-end-of-use-p)


(phpinspect-defhandler use-keyword (start-token max-point)
  "Handler for the use keyword and tokens that might follow to give it meaning"
  ((regexp . (concat "use" (phpinspect--word-end-regex))))
  (setq start-token (phpinspect--strip-word-end-space start-token))
  (forward-char (length start-token))
  (phpinspect--parse-use (current-buffer) max-point))

(phpinspect-defhandler use-trait-keyword (start-token max-point)
  "Handler for the use keyword within the body of a class."
  ((regexp . (concat "use" (phpinspect--word-end-regex))))
  (setq start-token (phpinspect--strip-word-end-space start-token))
  (forward-char (length start-token))
  (phpinspect--parse-use-trait (current-buffer) max-point))


(phpinspect-defhandler attribute-reference (start-token &rest _ignored)
  "Handler for references to object attributes, or static class attributes."
  ((regexp . "->\\|::"))
  (forward-char (length start-token))
  (looking-at (phpinspect-handler-regexp word))
  (let ((name (if (looking-at (phpinspect-handler-regexp word))
                  (phpinspect--word-handler (match-string 0))
                nil)))
    (cond
     ((string= start-token "::")
      (list :static-attrib name))
     ((string= start-token "->")
      (list :object-attrib name)))))

(define-inline phpinspect--namespace-should-end-at-block-p (tokens)
  (inline-quote
   (>= 4 (length ,tokens))))

(phpinspect-defparser namespace
  :tree-keyword "namespace"
  :delimiter-condition #'phpinspect--namespace-should-end-at-block-p
  :delimiter-predicate #'phpinspect-block-p)

(phpinspect-defhandler namespace (start-token max-point)
  "Handler for the namespace keyword. This is a special one
 because it is not always delimited by a block like classes or
 functions. This handler parses the namespace declaration, and
 then continues to parse subsequent tokens, only stopping when
 either a block has been parsed or another namespace keyword has
 been encountered."
  ((regexp . (concat "namespace" (phpinspect--word-end-regex))))
  (setq start-token (phpinspect--strip-word-end-space start-token))
  (forward-char (length start-token))
  (phpinspect--parse-namespace
   (current-buffer)
   max-point
   nil
   (lambda () (not (looking-at (phpinspect-handler-regexp namespace))))))

(phpinspect-defparser const
  :tree-keyword "const"
  :handlers '(word comment assignment-operator string string-concatenator array terminator)
  :delimiter-predicate #'phpinspect-end-of-token-p)

(phpinspect-defhandler const-keyword (start-token max-point)
  "Handler for the const keyword."
  ((regexp . (concat "const" (phpinspect--word-end-regex))))
  (setq start-token (phpinspect--strip-word-end-space start-token))
  (forward-char (length start-token))

  (setq start-token (phpinspect--parse-const (current-buffer) max-point))
  (when (phpinspect-incomplete-token-p (car (last start-token)))
    (setcar start-token :incomplete-const))
  start-token)

(phpinspect-defhandler string (start-token &rest _ignored)
  "Handler for strings"
  ((regexp . "\\(\"\\|'\\)"))
  (list :string (phpinspect--munch-string start-token)))

(phpinspect-defparser block-without-scopes
  :tree-keyword "block"
  :handlers '(array tag equals string-concatenator list comma attribute-reference variable
                    assignment-operator whitespace function-keyword word
                    terminator here-doc string comment block-without-scopes))

(phpinspect-defhandler block-without-scopes (start-token max-point)
  "Handler for code blocks that cannot contain scope, const or
static keywords with the same meaning as in a class block."
  ((regexp . "{"))
  (let* ((complete-block nil)
         (continue-condition (lambda ()
                               (not (and (char-equal (char-after) ?})
                                         (setq complete-block (point))))))
         (parsed (phpinspect--parse-block-without-scopes
                  (current-buffer) max-point (length start-token) continue-condition)))
    (if complete-block
        (goto-char (+ complete-block 1))
      (setcar parsed :incomplete-block))
    parsed))

(phpinspect-defparser class-block
  :tree-keyword "block"
  :handlers '(array tag equals list comma string-concatenator attribute-reference
                    class-variable assignment-operator whitespace scope-keyword
                    static-keyword const-keyword use-trait-keyword function-keyword
                    word terminator here-doc string comment block))

(phpinspect-defhandler class-block (start-token max-point)
  "Handler for code blocks that cannot contain classes"
  ((regexp . "{"))
  (let* ((complete-block nil)
         (continue-condition (lambda ()
                               (not (and (char-equal (char-after) ?})
                                         (setq complete-block (point))))))
         (parsed (phpinspect--parse-class-block
                  (current-buffer) max-point (length start-token) continue-condition)))
    (if complete-block
        (goto-char (+ complete-block 1))
      (setcar parsed :incomplete-block))
    parsed))

(phpinspect-defparser block
  :tree-keyword "block")

(phpinspect-defhandler block (start-token max-point)
  "Handler for code blocks"
  ((regexp . "{"))
  (let* ((complete-block nil)
         (continue-condition (lambda ()
                               ;; When we encounter a closing brace for this
                               ;; block, we can mark the block as complete.
                               (not (and (char-equal (char-after) ?})
                                         (setq complete-block (point))))))
         (parsed (phpinspect--parse-block
                  (current-buffer) max-point (length start-token) continue-condition)))
    (if complete-block
        ;; After meeting the char-after requirement above, we need to move
        ;; one char forward to prevent parent-blocks from exiting because
        ;; of the same char.
        (goto-char (+ complete-block 1))
      (setcar parsed :incomplete-block))
    parsed))

(phpinspect-defhandler here-doc (start-token &rest _ignored)
  "Handler for heredocs. Discards their contents."
  ((regexp . "<<<"))
  (forward-char (length start-token))
  (if (looking-at "[A-Za-z0-9'\"\\_]+")
      (re-search-forward (concat "^" (regexp-quote (match-string 0))) nil t))
  (list :here-doc))


(define-inline phpinspect--munch-string (start-token)
  "Consume text at point until a non-escaped `START-TOKEN` is found.

Returns the consumed text string without face properties."
  (inline-letevals (start-token)
    (inline-quote
     (progn
       (forward-char (length ,start-token))

       (let ((start-point (point)))
         (cond ((looking-at ,start-token)
                (forward-char)
                "")
               ((looking-at (concat "\\([\\][\\]\\)+" (regexp-quote ,start-token)))
                (let ((match (match-string 0)))
                  (forward-char (length match))
                  (buffer-substring-no-properties start-point
                                                  (+ start-point (- (length match)
                                                                    (length ,start-token))))))
               (t
                (re-search-forward (format "\\([^\\]\\([\\][\\]\\)+\\|[^\\]\\)%s"
                                           (regexp-quote ,start-token))
                                   nil t)
                (buffer-substring-no-properties start-point (- (point) 1)))))))))


(phpinspect-defparser declaration
  :tree-keyword "declaration"
  :handlers '(comment word list terminator tag)
  :delimiter-predicate #'phpinspect-end-of-token-p)

(phpinspect-defhandler function-keyword (start-token max-point)
  "Handler for the function keyword and tokens that follow to give it meaning"
  ((regexp . (concat "function" (phpinspect--word-end-regex))))
  (setq start-token (phpinspect--strip-word-end-space start-token))
  (let* ((continue-condition (lambda () (not (or (char-equal (char-after) ?{)
                                                           (char-equal (char-after) ?})))))
         (declaration (phpinspect--parse-declaration (current-buffer) max-point nil continue-condition 'root)))
    (if (phpinspect-end-of-token-p (car (last declaration)))
        ;; Abstract function?
        (list :function declaration)
      ;; Parse body.
      `(:function
        ,declaration
       ,@(cdr (phpinspect--parse-function-body (current-buffer) max-point))))))
;; (phpinspect--block-without-scopes-handler
;;  (char-to-string (char-after)) max-point)))))

(phpinspect-defparser scope-public
  :tree-keyword "public"
  :handlers '(function-keyword static-keyword const-keyword class-variable here-doc
                               string string-concatenator terminator tag comment
                               assignment-operator array word)
  :delimiter-predicate #'phpinspect--scope-terminator-p)

(phpinspect-defparser scope-private
  :tree-keyword "private"
  :handlers '(function-keyword static-keyword const-keyword class-variable here-doc
                               string string-concatenator terminator tag comment
                               assignment-operator array word)
  :delimiter-predicate #'phpinspect--scope-terminator-p)

(phpinspect-defparser scope-protected
  :tree-keyword "protected"
  :handlers '(function-keyword static-keyword const-keyword class-variable here-doc
                               string string-concatenator terminator tag comment
                               assignment-operator array word)
  :delimiter-predicate #'phpinspect--scope-terminator-p)

(phpinspect-defhandler scope-keyword (start-token max-point)
  "Handler for scope keywords"
  ((regexp . (mapconcat (lambda (word)
                          (concat word (phpinspect--word-end-regex)))
                        (list "public" "private" "protected")
                        "\\|")))
  (setq start-token (phpinspect--strip-word-end-space start-token))
  (forward-char (length start-token))
  (cond ((string= start-token "public") (phpinspect--parse-scope-public (current-buffer) max-point))
        ((string= start-token "private") (phpinspect--parse-scope-private (current-buffer) max-point))
        ((string= start-token "protected") (phpinspect--parse-scope-protected (current-buffer) max-point))))

(phpinspect-defparser static
  :tree-keyword "static"
  :handlers '(comment function-keyword class-variable array word terminator tag)
  :delimiter-predicate #'phpinspect--static-terminator-p)

(phpinspect-defhandler static-keyword (start-token max-point)
  "Handler for the static keyword"
  ((regexp . (concat "static" (phpinspect--word-end-regex))))
  (setq start-token (phpinspect--strip-word-end-space start-token))
  (forward-char (length start-token))
  (phpinspect--parse-static (current-buffer) max-point))

(phpinspect-defhandler fat-arrow (arrow &rest _ignored)
  "Handler for the \"fat arrow\" in arrays and foreach expressions"
  ((regexp . "=>"))
  (phpinspect-munch-token-without-attribs arrow :fat-arrow))

(phpinspect-defparser array
  :tree-keyword "array"
  :handlers '(comment comma list here-doc string array variable
                      attribute-reference word fat-arrow))

(phpinspect-defhandler array (start-token max-point)
  "Handler for arrays, in the bracketet as well as the list notation"
  ((regexp . "\\[\\|array("))
  (let* ((end-char (cond ((string= start-token "[") ?\])
                         ((string= start-token "array(") ?\))))
         (end-char-reached nil)
         (token (phpinspect--parse-array
                 (current-buffer)
                 max-point
                 (length start-token)
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
  ;; FIXME: "case" keyworded enum cases are not recognized/parsed into anything
  ;; other than "word" tokens. Enums might require a different (specialized)
  ;; handler to parse into an indexable tree. For now, this works to get basic
  ;; functionality (enum methods) as enum case support hasn't been implemented
  ;; yet.
  ((regexp . (concat "\\(abstract\\|final\\|class\\|interface\\|trait\\|enum\\)"
                     (phpinspect--word-end-regex))))
  (setq start-token (phpinspect--strip-word-end-space start-token))
  `(:class ,(phpinspect--parse-declaration
                (current-buffer)
                max-point
                nil
                (lambda () (not (char-equal (char-after) ?{)))
                'root)
           ,@(cdr (phpinspect--parse-class-body (current-buffer) max-point nil))))

(phpinspect-defparser class-body
  :handlers '(whitespace comment class-block)
  :delimiter-predicate #'phpinspect-block-p)

(phpinspect-defparser function-body
  :handlers '(whitespace comment block-without-scopes)
  :delimiter-predicate #'phpinspect-block-p)

(phpinspect-defparser root
  :tree-keyword "root"
  :handlers '(namespace array equals list comma attribute-reference variable
                        assignment-operator whitespace scope-keyword
                        static-keyword const-keyword use-keyword class-keyword
                        function-keyword word terminator here-doc string string-concatenator
                        comment tag block))

(defun phpinspect-parse-current-buffer ()
  (phpinspect-parse-buffer-until-point
   (current-buffer)
   (point-max)))

(defun phpinspect-parse-string (string)
  (with-temp-buffer
    (insert string)
    (phpinspect-parse-current-buffer)))

(defun phpinspect-parse-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (phpinspect-parse-current-buffer)))

;; Define all registered parser functions
(eval-and-compile
  (phpinspect-define-parser-functions))

(defun phpinspect-parse-buffer-until-point (buffer point)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "<\\?php\\|<\\?" nil t)
      (phpinspect--parse-root (current-buffer) point nil nil 'root))))

(provide 'phpinspect-parser)
;;; phpinspect-parser.el ends here
