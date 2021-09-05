;; -*- lexical-binding: t; -*-

(require 'php-project)
(require 'cl-lib)
(require 'json)

(cl-defstruct (phpinspect--function (:constructor phpinspect--make-function))
  "A PHP function."
  (name nil
        :type string
        :documentation
        "A string containing the name of the function")
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
               :type string
               :documentation
               "A string containing the FQN of the return value
of the function."))

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

(defun phpinspect--format-type-name (name)
  (string-remove-prefix "\\" name))

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
                       (phpinspect--get-bare-class-name-from-fqn
                        (or (phpinspect--function-return-type completion-candidate)
                            "")))
   :kind 'function))

(defvar phpinspect--debug nil
  "Enable debug logs for phpinspect by setting this variable to true")

(defun phpinspect-toggle-logging ()
  (interactive)
  (if (setq phpinspect--debug (not phpinspect--debug))
      (message "Enabled phpinspect logging.")
    (message "Disabled phpinspect logging.")))

(defconst phpinspect-native-types
  ;; self, parent and resource are not valid type name.
  ;; see https://www.php.net/manual/ja/language.types.declarations.php
  '("array" "bool" "callable" "float" "int" "iterable" "mixed" "object" "string" "void"))

(eval-when-compile
  (defun phpinspect--word-end-regex ()
    "[[:blank:]]")

  (defsubst phpinspect--strip-last-char (string)
    (substring string 0 (- (length string) 1)))

  (defmacro phpinspect--handler (regex function)
    (list 'cons regex (list 'quote function))))

(defmacro phpinspect-defhandler (name regex docstring function)
  ;; Lets make sure that defuns and substs are only referenced by
  ;; their name and not their entire definitions.
  (let ((function-name (cond ((and (listp function)
                                   (or (eq (car function) 'defun)
                                       (eq (car function) 'defsubst)
                                       (and (eq (car function) 'quote)
                                            (symbolp (car (last function))))))
                              (eval function))
                             (t (error (concat "`phpinspect-defhandler`: function must "
                                               "be a quoted function name, a `defun` "
                                               "or a `defsubst` %S provided")
                                       function)))))
    `(progn
       ;; If function is a defun, we'll need to have evaluated it.
       ,function
       (defmacro ,name ()
         ,(concat "This is a generated macro, see `phpinspect-defhandler`\n\n"
                  "ATTRIBUTES:\n"
                  "Token regex: " (eval regex) "\n\n"
                  "Parser function:\n" (with-output-to-string (pp (list 'quote function-name)))
                  "\n\n"
                  "DESCRIPTION\n"
                  docstring)
         (list 'phpinspect--handler ,regex (quote ,function-name))))))

(defmacro phpinspect-munch-token-without-attribs (text-object token-keyword)
  "Return a token by name of `token-keyword` with the contents of
the passed text object as value. The text object will be
stripped of all text attributes"
  `(let ((text ,text-object) (length (length ,text-object)))
     (forward-char length)
     (set-text-properties 0 length nil text)
     (list ,token-keyword text)))

(defmacro phpinspect-parse
    (buffer tree-type handler-list max-point &optional continue-condition delimiter-predicate)
  "Parse the current buffer using the handler macros provided in
`handler-list`, unrolling them in a `cond` statement which checks
their token regexes one by one and runs their parser functions
when one of them matches."
  (unless continue-condition (setq continue-condition t))
  `(with-current-buffer ,buffer
     (let ((tokens (list)))
       (while ,(append `(and (< (point) ,max-point))
                       (list continue-condition)
                       `((not ,(if (functionp (eval delimiter-predicate))
                                   (list (eval delimiter-predicate)
                                         '(car (last tokens)))
                                 nil))))
         ,(append `(cond)
                  (mapcar
                   (lambda (handler)
                     `((looking-at ,(car (eval handler)))
                       (let ((token (,(cdr (eval handler))
                                     (match-string 0)
                                     ,max-point)))
                         (unless (null token)
                           (if (null tokens)
                               (setq tokens (list token))
                             (nconc tokens (list token)))))))
                   handler-list)
                  '((t (forward-char)))))
       (push ,tree-type tokens))))

(eval-and-compile
  ;; Because some of the handler macros are mutually dependent on each
  ;; other, we need to wrap their definition in an eval-and-compile
  ;; body.

  (phpinspect-defhandler
   phpinspect--comma-handler ","
   "Handler for comma tokens"
   (defun phpinspect--munch-comma (comma &rest ignored)
     (phpinspect-munch-token-without-attribs comma :comma)))

  (phpinspect-defhandler
   phpinspect--word-handler "[A-Za-z_\\][\\A-Za-z_0-9]*"
   "Handler for bareword tokens"
   (defun phpinspect--munch-word (word &rest ignored)
     (let ((length (length word)))
       (forward-char length)
       (set-text-properties 0 length nil word)
       (list :word word))))


  (defun phpinspect--parse-annotation-parameters (parameter-amount)
    (let (words)
      (while (not (or (looking-at "\\*/") (= (length words) parameter-amount)))
        (forward-char)
        (cond ((looking-at (car (phpinspect--word-handler)))
               (push (phpinspect--munch-word (match-string 0)) words))
              ((looking-at (car (phpinspect--variable-handler)))
               (push (phpinspect--parse-variable (match-string 0)) words))))
      (nreverse words)))

  (phpinspect-defhandler
   phpinspect--annotation-handler "@"
   "Handler for in-comment @annotations"
   (defun phpinspect--parse-var-annotation (start-token max-point)
     (forward-char (length start-token))
     (if (looking-at (car (phpinspect--word-handler)))
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
                  (let ((word-count 0))
                    ;; The @param annotation accepts 2 parameters:
                    ;; The type of the param, and the param's $name
                    (append (list :param-annotation)
                            (phpinspect--parse-annotation-parameters 2))))
                 (t
                  (list :annotation annotation-name))))
       (list :annotation nil))))

  (phpinspect-defhandler
   phpinspect--tag-handler "\\?>"
   "Handler that discards any inline HTML it encounters"
   (defun phpinspect--discard-html (start-token max-point)
     (forward-char (length start-token))
     (or (re-search-forward "<\\?php\\|<\\?" nil t)
         (goto-char max-point))
     (list :html)))

  (phpinspect-defhandler
   phpinspect--comment-handler "#\\|//\\|/\\*"
   "Handler for comments and doc blocks"
   (defun phpinspect--parse-comment (start-token max-point)
     (forward-char (length start-token))
     (cond ((string-match "/\\*" start-token)
            (let ((doc-block (phpinspect-parse
                              (current-buffer)
                              :doc-block
                              ((phpinspect--annotation-handler)
                               (phpinspect--whitespace-handler))
                              max-point
                              (not (looking-at "\\*/")))))
              (forward-char 2)
              doc-block))
           (t
            (let ((end-position (line-end-position)))
              (phpinspect-parse
               (current-buffer)
               :comment
               ((phpinspect--tag-handler))
               end-position
               t
               'phpinspect-html-p))))))

  (phpinspect-defhandler
   phpinspect--variable-handler "\\$"
   "Handler for tokens indicating reference to a variable"
   (defun phpinspect--parse-variable (start-token &rest ignored)
     (forward-char (length start-token))
     (if (looking-at (car (phpinspect--word-handler)))
         (phpinspect-munch-token-without-attribs (match-string 0) :variable)
       (list :variable nil))))


  (phpinspect-defhandler
   phpinspect--whitespace-handler "[[:blank:]]+"
   "Handler that discards whitespace"
   (defun phpinspect--discard-whitespace (whitespace &rest ignored)
     (forward-char (length whitespace))))

  (phpinspect-defhandler
   phpinspect--equals-handler "===?"
   "Handler for strict and unstrict equality comparison tokens"
   (defun phpinspect--munch-equals (equals &rest ignored)
     (phpinspect-munch-token-without-attribs equals :equals)))

  (phpinspect-defhandler
   phpinspect--assignment-operator-handler "[+-]?="
   "Handler for tokens indicating that an assignment is taking place"
   (defun phpinspect--munch-assignment-operator (operator &rest ignored)
     (phpinspect-munch-token-without-attribs operator :assignment)))

  (phpinspect-defhandler
   phpinspect--statement-terminator-handler ";"
   "Handler for statement terminators"
   (defun phpinspect--munch-statement-terminator (terminator &rest ignored)
     (phpinspect-munch-token-without-attribs terminator :terminator)))

  (phpinspect-defhandler
   phpinspect--use-keyword-handler (concat "use" (phpinspect--word-end-regex))
   "Handler for the use keyword and tokens that might follow to give it meaning"
   (defun phpinspect--parse-use (start-token max-point)
     (setq start-token (phpinspect--strip-last-char start-token))
     (forward-char (length start-token))
     (phpinspect-parse
      (current-buffer)
      :use
      ((phpinspect--word-handler)
       (phpinspect--tag-handler)
       (phpinspect--block-without-classes-handler)
       (phpinspect--statement-terminator-handler))
      max-point
      t
      'phpinspect-end-of-use-p)))

  (phpinspect-defhandler
   phpinspect--attribute-reference-handler "->\\|::"
   "Handler for references to object attributes, or static class attributes"
   (defun phpinspect--parse-attribute-reference (start-token &rest ignored)
     (forward-char (length start-token))
     (looking-at (car (phpinspect--word-handler)))
     (let ((name (if (looking-at (car (phpinspect--word-handler)))
                     (phpinspect--munch-word (match-string 0))
                   nil)))
       (cond
        ((string= start-token "::")
         (list :static-attrib name))
        ((string= start-token "->")
         (list :object-attrib name))))))

  (phpinspect-defhandler
   phpinspect--namespace-keyword-handler (concat "namespace" (phpinspect--word-end-regex))
   "Handler for the namespace keyword. This is a special one
 because it is not always delimited by a block like classes or
 functions. This handler parses the namespace declaration, and
 then continues to parse subsequent tokens, only stopping when
 either a block has been parsed or another namespace keyword has
 been encountered."
   (defun phpinspect--parse-namespace (start-token max-point)
     "Nest all statements after a 'namespace' keyword in its own token"
     (setq start-token (phpinspect--strip-last-char start-token))
     (forward-char (length start-token))
     (phpinspect--parse-with-handler-alist
      (current-buffer)
      :namespace
      max-point
      (not (looking-at (car (phpinspect--namespace-keyword-handler))))
      'phpinspect-block-p)))

  (phpinspect-defhandler
   phpinspect--const-keyword-handler (concat "const" (phpinspect--word-end-regex))
   "Handler for the const keyword"
   (defun phpinspect--parse-const (start-token max-point)
     (setq start-token (phpinspect--strip-last-char start-token))
     (forward-char (length start-token))
     (let ((token (phpinspect-parse
                   (current-buffer)
                   :const
                   ((phpinspect--word-handler)
                    (phpinspect--comment-handler)
                    (phpinspect--assignment-operator-handler)
                    (phpinspect--string-handler)
                    (phpinspect--array-handler)
                    (phpinspect--statement-terminator-handler))
                   max-point
                   t
                   'phpinspect-end-of-statement-p)))

       (when (phpinspect-incomplete-token-p (car (last token)))
         (setcar token :incomplete-const))
       token)))
  
  (phpinspect-defhandler
   phpinspect--string-handler "\"\\|'"
   "Handler for strings"
   (defun phpinspect--parse-string (start-token &rest ignored)
     (list :string (phpinspect--munch-string start-token))))

  (phpinspect-defhandler
   phpinspect--block-without-classes-handler "{"
   "Handler for code blocks that cannot contain classes"
   (defun phpinspect--parse-block-without-classes (start-token max-point)
     (forward-char (length start-token))
     (let* ((complete-block nil)
            (parsed (phpinspect-parse
                     (current-buffer)
                     :block
                     ((phpinspect--array-handler)
                      (phpinspect--tag-handler)
                      (phpinspect--equals-handler)
                      (phpinspect--list-handler)
                      (phpinspect--comma-handler)
                      (phpinspect--attribute-reference-handler)
                      (phpinspect--variable-handler)
                      (phpinspect--assignment-operator-handler)
                      (phpinspect--whitespace-handler)
                      (phpinspect--scope-keyword-handler)
                      (phpinspect--static-keyword-handler)
                      (phpinspect--const-keyword-handler)
                      (phpinspect--use-keyword-handler)
                      (phpinspect--function-keyword-handler)
                      (phpinspect--word-handler)
                      (phpinspect--statement-terminator-handler)
                      (phpinspect--here-doc-handler)
                      (phpinspect--string-handler)
                      (phpinspect--comment-handler)
                      (phpinspect--block-handler))
                     max-point
                     (not (and (char-equal (char-after) ?}) (setq complete-block t))))))
       (if complete-block
           (forward-char)
         (setcar parsed :incomplete-block))
       parsed)))


  (phpinspect-defhandler
   phpinspect--block-handler "{"
   "Handler for code blocks"
   (defun phpinspect--parse-block (start-token max-point)
     (forward-char (length start-token))
     (let* ((complete-block nil)
            (parsed (phpinspect--parse-with-handler-alist
                     (current-buffer)
                     :block
                     max-point
                     ;; When we encounter a closing brace for this
                     ;; block, we can mark the block as complete.
                     (not (and (char-equal (char-after) ?}) (setq complete-block t))))))

       (if complete-block
           ;; After meeting the char-after requirement above, we need to move
           ;; one char forward to prevent parent-blocks from exiting because
           ;; of the same char.
           (forward-char)
         (setcar parsed :incomplete-block))
       parsed)))

  (phpinspect-defhandler
   phpinspect--here-doc-handler "<<<"
   "Handler for heredocs"
   (defun phpinspect--discard-heredoc (start-token point-max)
     (forward-char (length start-token))
     (if (looking-at "[A-Za-z0-9'\"\\_]+")
         (re-search-forward (concat "^" (regexp-quote (match-string 0))) nil t))
     (list :here-doc)))


  (defun phpinspect--munch-string (start-token)
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

  (phpinspect-defhandler
   phpinspect--list-handler "("
   "Handler for php syntactic lists (Note: this does not include
 datatypes like arrays, merely lists that are of a syntactic
 nature like argument lists"
   (defun phpinspect--parse-list (start-token max-point)
     (forward-char (length start-token))
     (let* ((complete-list nil)
            (php-list (phpinspect--parse-with-handler-alist
                       (current-buffer)
                       :list
                       max-point
                       (not (and (char-equal (char-after) ?\)) (setq complete-list t))))))

       (if complete-list
           ;; Prevent parent-lists (if any) from exiting by skipping over the
           ;; ")" character
           (forward-char)
         (setcar php-list :incomplete-list))
       php-list)))

  (phpinspect-defhandler
   phpinspect--function-keyword-handler (concat "function" (phpinspect--word-end-regex))
   "Handler for the function keyword and tokens that follow to give it meaning"
   (defun phpinspect--parse-function (start-token max-point)
     (setq start-token (phpinspect--strip-last-char start-token))
     (let ((declaration (phpinspect-parse
                         (current-buffer)
                         :declaration
                         ((phpinspect--comment-handler)
                          (phpinspect--word-handler)
                          (phpinspect--list-handler)
                          (phpinspect--statement-terminator-handler)
                          (phpinspect--tag-handler))
                         max-point
                         (not (char-equal (char-after) ?{))
                         'phpinspect-end-of-statement-p)))
       (if (phpinspect-end-of-statement-p (car (last declaration)))
           (list :function declaration)
         (list :function
               declaration
               (phpinspect--parse-block (char-to-string (char-after)) max-point))))))

  (phpinspect-defhandler
   phpinspect--scope-keyword-handler (mapconcat (lambda (word)
                                                  (concat word (phpinspect--word-end-regex)))
                                                (list "public" "private" "protected")
                                                "\\|")
   "Handler for scope keywords"
   (defun phpinspect--parse-scope (start-token max-point)
     (setq start-token (phpinspect--strip-last-char start-token))
     (forward-char (length start-token))
     (phpinspect-parse
      (current-buffer)
      (cond ((string= start-token "public") :public)
            ((string= start-token "private") :private)
            ((string= start-token "protected") :protected))
      ((phpinspect--function-keyword-handler)
       (phpinspect--static-keyword-handler)
       (phpinspect--const-keyword-handler)
       (phpinspect--variable-handler)
       (phpinspect--here-doc-handler)
       (phpinspect--string-handler)
       (phpinspect--statement-terminator-handler)
       (phpinspect--tag-handler)
       (phpinspect--comment-handler))
      max-point
      nil
      'phpinspect--scope-terminator-p)))

  (phpinspect-defhandler
   phpinspect--static-keyword-handler (concat "static" (phpinspect--word-end-regex))
   "Handler for the static keyword"
   (defun phpinspect--parse-static (start-token max-point)
     (setq start-token (phpinspect--strip-last-char start-token))
     (forward-char (length start-token))
     (phpinspect-parse
      (current-buffer)
      :static
      ((phpinspect--comment-handler)
       (phpinspect--function-keyword-handler)
       (phpinspect--variable-handler)
       (phpinspect--array-handler)
       (phpinspect--word-handler)
       (phpinspect--statement-terminator-handler)
       (phpinspect--tag-handler))
      max-point
      t
      'phpinspect--static-terminator-p)))

  (phpinspect-defhandler
   phpinspect--fat-arrow-handler "=>"
   "Handler for the \"fat arrow\" in arrays and foreach expressions"
   (defun phpinspect--munch-fat-arrow (arrow &rest ignored)
     (phpinspect-munch-token-without-attribs arrow :fat-arrow)))

  (phpinspect-defhandler
   phpinspect--array-handler "\\[\\|array("
   "Handler for arrays, in the bracketet as well as the list notation"
   (defun phpinspect--parse-array (start-token max-point)
     (forward-char (length start-token))
     (let* ((end-char (cond ((string= start-token "[") ?\])
                            ((string= start-token "array(") ?\))))
            (end-char-reached nil)
            (token (phpinspect-parse
                    (current-buffer)
                    :array
                    ((phpinspect--comment-handler)
                     (phpinspect--comma-handler)
                     (phpinspect--list-handler)
                     (phpinspect--here-doc-handler)
                     (phpinspect--string-handler)
                     (phpinspect--array-handler)
                     (phpinspect--variable-handler)
                     (phpinspect--attribute-reference-handler)
                     (phpinspect--word-handler)
                     (phpinspect--fat-arrow-handler))
                    max-point
                    (not (and (char-equal (char-after) end-char) (setq end-char-reached t))))))

       ;; Skip over the end char to prevent enclosing arrays or lists
       ;; from terminating.
       (if end-char-reached
           (forward-char)
         ;; Signal incompleteness when terminated because of max-point
         (setcar token :incomplete-array))
       token)))

  (phpinspect-defhandler
   phpinspect--class-keyword-handler (concat "\\(abstract\\|final\\|class\\|interface\\|trait\\)"
                                             (phpinspect--word-end-regex))
   "Handler for the class keyword, and tokens that follow to define
the properties of the class"
   (defun phpinspect--parse-class (start-token max-point)
     (setq start-token (phpinspect--strip-last-char start-token))
     (list :class (phpinspect-parse
                   (current-buffer)
                   :declaration
                   ((phpinspect--comment-handler)
                    (phpinspect--word-handler)
                    (phpinspect--tag-handler))
                   max-point
                   (not (char-equal (char-after) ?{)))
           (phpinspect--parse-block-without-classes (char-to-string (char-after)) max-point))))

  (defmacro phpinspect--parse-with-handler-alist
      (buffer tree-type max-point &optional continue-condition delimiter-predicate)
    (list 'phpinspect-parse
          buffer
          tree-type
          '((phpinspect--array-handler)
            (phpinspect--tag-handler)
            (phpinspect--equals-handler)
            (phpinspect--list-handler)
            (phpinspect--comma-handler)
            (phpinspect--attribute-reference-handler)
            (phpinspect--variable-handler)
            (phpinspect--assignment-operator-handler)
            (phpinspect--whitespace-handler)
            (phpinspect--scope-keyword-handler)
            (phpinspect--static-keyword-handler)
            (phpinspect--const-keyword-handler)
            (phpinspect--use-keyword-handler)
            (phpinspect--class-keyword-handler)
            (phpinspect--function-keyword-handler)
            (phpinspect--word-handler)
            (phpinspect--statement-terminator-handler)
            (phpinspect--here-doc-handler)
            (phpinspect--string-handler)
            (phpinspect--comment-handler)
            (phpinspect--block-handler))
          max-point
          continue-condition
          delimiter-predicate))


  (defun phpinspect-parse-buffer-until-point (buffer point)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "<\\?php\\|<\\?" nil t)
        (phpinspect-parse
         (current-buffer)
         :root
         ((phpinspect--namespace-keyword-handler)
          (phpinspect--array-handler)
          (phpinspect--equals-handler)
          (phpinspect--list-handler)
          (phpinspect--comma-handler)
          (phpinspect--attribute-reference-handler)
          (phpinspect--variable-handler)
          (phpinspect--assignment-operator-handler)
          (phpinspect--whitespace-handler)
          (phpinspect--scope-keyword-handler)
          (phpinspect--static-keyword-handler)
          (phpinspect--const-keyword-handler)
          (phpinspect--use-keyword-handler)
          (phpinspect--class-keyword-handler)
          (phpinspect--function-keyword-handler)
          (phpinspect--word-handler)
          (phpinspect--statement-terminator-handler)
          (phpinspect--here-doc-handler)
          (phpinspect--string-handler)
          (phpinspect--comment-handler)
          (phpinspect--tag-handler)
          (phpinspect--block-handler))
         point))))

  ;; End of eval-and-compile body
  )

(defsubst phpinspect--log (&rest args)
  (when phpinspect--debug
    (with-current-buffer (get-buffer-create "**phpinspect-logs**")
      (goto-char (buffer-end 1))
      (insert (concat (apply 'format args) "\n")))))

(defun phpinspect-parse-current-buffer ()
  (phpinspect-parse-buffer-until-point
   (current-buffer)
   (point-max)))


(defsubst phpinspect-type-p (object type)
  "Returns t if OBJECT is a token of type TYPE.
Type can be any of the token types returned by
`phpinspect-parse-buffer-until-point`"
  (and (listp object) (eq (car object) type)))

(defun phpinspect-html-p (token)
  (phpinspect-type-p token :html))

(defun phpinspect-comma-p (token)
  (phpinspect-type-p token :comma))

(defun phpinspect-end-of-statement-p (token)
  (or (phpinspect-terminator-p token)
      (phpinspect-comma-p token)
      (phpinspect-html-p token)))

(defun phpinspect-end-of-use-p (token)
  (or (phpinspect-block-p token)
      (phpinspect-end-of-statement-p token)))


(defun phpinspect-static-p (token)
  (phpinspect-type-p token :static))

(defun phpinspect-const-p (token)
  (or (phpinspect-type-p token :const)
      (phpinspect-incomplete-const-p token)))

(defun phpinspect-scope-p (token)
  (or (phpinspect-type-p token :public)
      (phpinspect-type-p token :private)
      (phpinspect-type-p token :protected)))

(defun phpinspect-block-p (token)
  (or (phpinspect-type-p token :block) (phpinspect-incomplete-block-p token)))

(defun phpinspect-incomplete-block-p (token)
  (phpinspect-type-p token :incomplete-block))

(defun phpinspect-incomplete-class-p (token)
  (and (phpinspect-class-p token)
       (phpinspect-incomplete-block-p (car (last token)))))

(defun phpinspect-incomplete-namespace-p (token)
  (and (phpinspect-namespace-p token)
       (or (phpinspect-incomplete-block-p (car (last token)))
           (phpinspect-incomplete-class-p (car (last token))))))


(defun phpinspect-function-p (token)
  (phpinspect-type-p token :function))

(defun phpinspect-class-p (token)
  (phpinspect-type-p token :class))

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

(defun phpinspect-list-p (token)
  (or (phpinspect-type-p token :list)
      (phpinspect-incomplete-list-p token)))

(defun phpinspect-declaration-p (token)
  (phpinspect-type-p token :declaration))

(defsubst phpinspect-assignment-p (token)
  (phpinspect-type-p token :assignment))

(defun phpinspect-function-argument-list (php-func)
  "Get the argument list of a function"
  (seq-find 'phpinspect-list-p (seq-find 'phpinspect-declaration-p php-func nil) nil))

(defun phpinspect-variable-p (token)
  (phpinspect-type-p token :variable))

(defun phpinspect-word-p (token)
  (phpinspect-type-p token :word))

(defsubst phpinspect-incomplete-list-p (token)
  (phpinspect-type-p token :incomplete-list))

(defsubst phpinspect-array-p (token)
  (or (phpinspect-type-p token :array)
      (phpinspect-incomplete-array-p token)))

(defsubst phpinspect-incomplete-array-p (token)
  (phpinspect-type-p token :incomplete-array))

(defsubst phpinspect-incomplete-const-p (token)
  (phpinspect-type-p token :incomplete-const))

(defun phpinspect-incomplete-token-p (token)
  (or (phpinspect-incomplete-class-p token)
      (phpinspect-incomplete-block-p token)
      (phpinspect-incomplete-list-p token)
      (phpinspect-incomplete-array-p token)
      (phpinspect-incomplete-const-p token)
      (phpinspect-incomplete-function-p token)
      (phpinspect-incomplete-method-p token)
      (phpinspect-incomplete-namespace-p token)))

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

(defun phpinspect--static-terminator-p (token)
  (or (phpinspect-function-p token)
      (phpinspect-end-of-statement-p token)))

(defun phpinspect--scope-terminator-p (token)
  (or (phpinspect-function-p token)
      (phpinspect-end-of-statement-p token)
      (phpinspect-const-p token)
      (phpinspect-static-p token)))

(defun phpinspect-namespace-keyword-p (token)
  (and (phpinspect-word-p token) (string= (car (last token)) "namespace")))

(defun phpinspect-terminator-p (token)
  (phpinspect-type-p token :terminator))

(defun phpinspect-use-keyword-p (token)
  (and (phpinspect-word-p token) (string= (car (last token)) "use")))

(defun phpinspect-namespace-p (object)
  (phpinspect-type-p object :namespace))

(defun phpinspect-use-p (object)
  (phpinspect-type-p object :use))

(defun phpinspect-comment-p (token)
  (phpinspect-type-p token :comment))

(defun phpinspect--split-list (predicate list)
  (seq-reduce (let ((current-sublist))
                (lambda (result elt)
                  (if (funcall predicate elt)
                      (progn
                        (push elt current-sublist)
                        (push (nreverse current-sublist) result)
                        (setq current-sublist nil))
                    (push elt current-sublist))
                  result))
              list
              nil))

(defun phpinspect-eldoc-function ()
  "An `eldoc-documentation-function` implementation for PHP files.

Ignores `eldoc-argument-case` and `eldoc-echo-area-use-multiline-p`.

TODO: 
 - Respect `eldoc-echo-area-use-multiline-p`
 - This function is too big and has repetitive code. Split up and simplify.
"
  (phpinspect--log "Starting eldoc function execution")
  (let* ((token-tree (phpinspect-parse-buffer-until-point (current-buffer) (point)))
         (namespace (phpinspect--find-innermost-incomplete-namespace token-tree))
         (incomplete-token (phpinspect--find-innermost-incomplete-token token-tree))
         (enclosing-token (phpinspect--find-token-enclosing-innermost-incomplete-token token-tree))
         (type-resolver)
         (static))
    (when (and (phpinspect-incomplete-list-p incomplete-token)
               enclosing-token
               (or (phpinspect-object-attrib-p (car (last enclosing-token 2)))
                   (setq static (phpinspect-static-attrib-p (car (last enclosing-token 2))))))
      (if namespace
          (setq type-resolver (phpinspect--make-type-resolver-for-namespace
                               namespace
                               token-tree))
        ;; else
        (setq type-resolver (phpinspect--make-type-resolver
                             (phpinspect--uses-to-types
                              (seq-filter 'phpinspect-use-p token-tree)))))
      (let* ((previous-statement (phpinspect--get-last-statement-in-token (butlast enclosing-token 2)))
             (type-of-previous-statement
              (phpinspect-get-type-of-derived-statement-in-token
               previous-statement
               (or namespace token-tree)
               type-resolver))
             (method-name (cadr (cadar (last enclosing-token 2))))
             (class-index (and type-of-previous-statement
                               (phpinspect--get-or-create-index-for-class-file
                                type-of-previous-statement)))
             (method (and class-index
                          (seq-find
                           (lambda (func)
                             (when func
                               (string= method-name
                                        (phpinspect--function-name func))))
                           (alist-get (if static 'static-methods 'methods)
                                      class-index)))))
        (phpinspect--log "Eldoc method name: %s" method-name)
        (phpinspect--log "Eldoc method: %s" method)
        (when method
          (let ((arg-count -1)
                (comma-count
                 (length (seq-filter 'phpinspect-comma-p incomplete-token))))
            (concat (truncate-string-to-width
                     (phpinspect--function-name method) 14) ": ("
                     (mapconcat
                      (lambda (arg)
                        (setq arg-count (+ arg-count 1))
                        (if (= arg-count comma-count)
                            (propertize (concat
                                         "$"
                                         (truncate-string-to-width (car arg) 8)
                                         " "
                                         (phpinspect--format-type-name (or (cadr arg) "")))
                                        'face 'eldoc-highlight-function-argument)
                          (concat "$"
                                  (truncate-string-to-width (car arg) 8)
                                  " "
                                  (phpinspect--format-type-name (or (cadr arg) "")))))
                      (phpinspect--function-arguments method)
                      ", ")
                     "): "
                     (phpinspect--format-type-name
                      (phpinspect--function-return-type method)))))))))


(defun phpinspect--find-assignments-in-token (token)
  "Find any assignments that are in TOKEN, at top level or nested in blocks"
  (let ((assignments)
        (code-block)
        (statements (phpinspect--split-list
                     (lambda (elt)
                       (or (phpinspect-terminator-p elt)
                           (phpinspect-block-p elt)))
                     token)))
    (dolist (statement statements)
      (cond ((seq-find 'phpinspect-assignment-p statement)
             (phpinspect--log "Found assignment statement")
             (push statement assignments))
            ((setq code-block (seq-find 'phpinspect-block-p statement))
             (setq assignments
                   (append
                    (phpinspect--find-assignments-in-token code-block)
                    assignments)))))
    ;; return
    (phpinspect--log "assignments: %s" assignments)
    assignments))

(defun phpinspect-not-assignment-p (token)
  "Inverse of applying `phpinspect-assignment-p to TOKEN."
  (not (phpinspect-assignment-p token)))

(defun phpinspect--find-assignments-of-variable-in-token (variable-name token)
  "Find all assignments of variable VARIABLE-NAME in TOKEN."
  (let ((variable-assignments)
        (all-assignments (phpinspect--find-assignments-in-token token)))
    (dolist (assignment all-assignments)
      (if (or (member `(:variable ,variable-name)
                      (seq-take-while 'phpinspect-not-assignment-p
                                      assignment))
              (and (phpinspect-list-p (car assignment))
                   ((member `(:variable ,variable-name) (car assignment)))))
          (push assignment variable-assignments)))
    (nreverse variable-assignments)))



(defun phpinspect-get-derived-statement-type-in-block
    (statement php-block type-resolver &optional function-arg-list)
  "Get type of derived STATEMENT in PHP-BLOCK using
TYPE-RESOLVER and FUNCTION-ARG-LIST.

An example of a derived statement would be the following php code:
$variable->attribute->method();
$variable->attribute;
$variable->method();
self::method();
ClassName::method();
$variable = ClassName::method();
$variable = $variable->method();"
  ;; A derived statement can be an assignment itself.
  (when (seq-find 'phpinspect-assignment-p statement)
    (phpinspect--log "Derived statement is an assignment: %s" statement)
    (setq statement (cdr (seq-drop-while 'phpinspect-not-assignment-p statement))))
  (phpinspect--log "Get derived statement type in block: %s" statement)
  (let* ((first-token (pop statement))
         (current-token)
         (previous-attribute-type))
    ;; No first token means we were passed an empty list.
    (when (and first-token
               (setq previous-attribute-type
                     ;; Statements that are only bare words can be something preceding
                     ;; a static attribute that is not passed to this function. For
                     ;; example "return self" could have prefixed another attribute
                     ;; that the caller is trying to derive. Therefore we just try to
                     ;; resolve the type of the last bare word in the statement.
                     (or (when (and (phpinspect-word-p first-token)
                                    (seq-every-p 'phpinspect-word-p statement))
                           (setq statement (last statement))
                           (funcall type-resolver (cadr (pop statement))))

                         ;; Statements starting with a bare word can indicate a static
                         ;; method call. These could be statements with "return" or
                         ;; another bare-word at the start though, so we dop tokens
                         ;; from the statement until it starts with a static attribute
                         ;; refererence (::something in PHP code).
                         (when (phpinspect-word-p first-token)
                           (while (and first-token
                                       (not (phpinspect-static-attrib-p
                                             (car statement))))
                             (setq first-token (pop statement)))
                           (funcall type-resolver (cadr first-token)))

                         ;; No bare word, assume we're dealing with a variable.
                         (phpinspect-get-variable-type-in-block
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
                                 (phpinspect--get-project-root)
                                 (funcall type-resolver previous-attribute-type)
                                 (cadr attribute-word))
                                previous-attribute-type)))
                     (setq previous-attribute-type
                           (or
                            (phpinspect-get-cached-project-class-variable-type
                             (phpinspect--get-project-root)
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
                                 (phpinspect--get-project-root)
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
    (variable-name php-block type-resolver &optional function-arg-list)
  "Find the type of VARIABLE-NAME in PHP-BLOCK using TYPE-RESOLVER.

Returns either a FQN or a relative type name, depending on
whether or not the root variable of the assignment value (right
side of assignment) can be found in FUNCTION-ARG-LIST.

When PHP-BLOCK belongs to a function, supply FUNCTION-ARG-LIST to
resolve types of function argument variables."
  (phpinspect--log "Looking for assignments of variable %s in php block" variable-name)
  (if (string= variable-name "this")
      (funcall type-resolver "self")
    ;; else
    (let* ((assignments
            (phpinspect--find-assignments-of-variable-in-token variable-name php-block))
           (last-assignment (when assignments (car (last assignments))))
           (right-of-assignment (when assignments (cdr (seq-drop-while 'phpinspect-not-assignment-p
                                                                       last-assignment)))))
      (phpinspect--log "Assignments: %s" assignments)
      (phpinspect--log "Last assignment: %s" right-of-assignment)
      ;; When the right of an assignment is more than $variable; or "string";(so
      ;; (:variable "variable") (:terminator ";") or (:string "string") (:terminator ";")
      ;; in tokens), we're likely working with a derived assignment like $object->method()
      ;; or $object->attribute
      (cond ((and (phpinspect-word-p (car right-of-assignment))
                  (string= (cadar right-of-assignment) "new"))
             (funcall type-resolver (cadadr right-of-assignment)))
            ((and (> (length right-of-assignment) 2)
                  (seq-find 'phpinspect-attrib-p right-of-assignment))
             (phpinspect--log "Variable was assigned with a derived statement")
             (phpinspect-get-derived-statement-type-in-block right-of-assignment
                                                             php-block
                                                             type-resolver
                                                             function-arg-list))
            ;; If the right of an assignment is just $variable;, we can check if it is a
            ;; function argument and otherwise recurse to find the type of that variable.
            ((phpinspect-variable-p (car right-of-assignment))
             (phpinspect--log "Variable was assigned with the value of another variable")
             (or (when function-arg-list
                   (phpinspect-get-variable-type-in-function-arg-list (cadar right-of-assignment)
                                                                      function-arg-list))
                 (phpinspect-get-variable-type-in-block (cadar right-of-assignment)
                                                        php-block
                                                        type-resolver
                                                        function-arg-list)))
            ((not assignments)
             (phpinspect--log "No assignments found for variable %s, checking function arguments" variable-name)
             (phpinspect-get-variable-type-in-function-arg-list variable-name function-arg-list))))))


(defun phpinspect-get-derived-statement-type-in-function (statement php-func type-resolver)
  "Attempt to find the type of the php statement in php function
token `php-func`.  If no type can be found, this function
evaluates to nil."
  (let* ((arg-list (phpinspect-function-argument-list php-func)))
    (phpinspect-get-derived-statement-type-in-block statement
                                                    (car (last php-func))
                                                    type-resolver
                                                    arg-list)))

(defun phpinspect-get-derived-statement-type-in-method (statement php-method type-resolver)
  "Find the type of a statement in a php method."
  (cond ((phpinspect-function-p php-method)
         (phpinspect-get-derived-statement-type-in-function statement
                                                            php-method
                                                            type-resolver))
        ((phpinspect-scope-p php-method)
         (phpinspect-get-derived-statement-type-in-method statement
                                                          (car (last php-method))
                                                          type-resolver))
        ((phpinspect-static-p php-method)
         (phpinspect-get-derived-statement-type-in-method statement
                                                          (car (last php-method))
                                                          type-resolver))
        (t (error (concat "phpinspect-get-derived-statement-type-in-method: "
                          "php-method must be either a function or a scoped function")))))

(defun phpinspect-get-derived-statement-type-in-class (statement php-class type-resolver)
  (phpinspect--log "recursing into class")
  (let ((last-token-in-class-block (car (last (car (last php-class))))))
    (phpinspect--log "last token in class block: %s" last-token-in-class-block)
    (cond ((phpinspect-incomplete-method-p last-token-in-class-block)
           (phpinspect-get-derived-statement-type-in-method statement
                                                            (car (last (car (last php-class))))
                                                            type-resolver))
          ;; We're dealing with a const statement, so not a block, but that doesn't matter
          ;; much for the outcome. We're not trying to check syntax after all, just trying
          ;; to guess the type of the statement as well as we can.
          ((phpinspect-incomplete-const-p last-token-in-class-block)
           (phpinspect--log "Found incomplete constant")
           (phpinspect-get-derived-statement-type-in-block
            statement
            last-token-in-class-block
            type-resolver)))))

(defun phpinspect-get-type-of-derived-statement-in-token (statement token type-resolver)
  (phpinspect--log "Looking for type of statement: %s in token: %s" statement token)
  (cond ((phpinspect-namespace-p token)
         (if (phpinspect-incomplete-block-p (car (last token)))
             (phpinspect-get-type-of-derived-statement-in-token statement
                                                                (car (last (car (last token))))
                                                                type-resolver)
           (phpinspect-get-type-of-derived-statement-in-token statement
                                                              (car (last token))
                                                              type-resolver)))
        ((phpinspect-incomplete-block-p token)
         (phpinspect-get-derived-statement-type-in-block statement token type-resolver))
        ((phpinspect-class-p token)
         (phpinspect-get-derived-statement-type-in-class statement token type-resolver))
        ((phpinspect-incomplete-function-p token)
         (phpinspect-get-derived-statement-type-in-function statement token type-resolver))))

(defun phpinspect--function-from-scope (scope)
  (cond ((and (phpinspect-static-p (cadr scope))
              (phpinspect-function-p (caddr scope)))
         (caddr scope))
        ((phpinspect-function-p (cadr scope))
         (cadr scope))
        (t nil)))

(defun phpinspect--make-type-resolver (types &optional token-tree namespace)
  "Little wrapper closure to pass around and resolve types with."
  (unless namespace (setq namespace ""))
  (let* ((inside-class
          (if token-tree (or (phpinspect--find-innermost-incomplete-class token-tree)
                             (phpinspect--find-class-token token-tree))))
         (inside-class-name (if inside-class (phpinspect--get-class-name-from-token
                                              inside-class))))
    (lambda (type)
      (phpinspect--real-type
       types
       namespace
       (if (or (string= type "self") (string= type "static"))
           (progn
             (phpinspect--log "Returning inside class name for %s : %s"
                              type inside-class-name)

             inside-class-name)
         ;; else
         type)))))

(defun phpinspect--real-type (types namespace type)
  "Get the FQN for `type`, using `types` as an alist to retrieve
said FQN's by class name"
  (phpinspect--log "Resolving %s from namespace %s" type namespace)
  ;; Absolute FQN
  (cond ((string-match "^\\\\" type)
         type)

        ;; Native type
        ((member type phpinspect-native-types)
         (concat "\\" type))

        ;; Relative FQN
        ((string-match "\\\\" type)
         (concat "\\" namespace "\\" type))

        ;; Clas|interface|trait name
        (t (concat "\\" (or (assoc-default type types 'string=) (concat namespace "\\" type))))))

(defun phpinspect-var-annotation-p (token)
  (phpinspect-type-p token :var-annotation))

(defun phpinspect-return-annotation-p (token)
  (phpinspect-type-p token :return-annotation))

(defun phpinspect--index-function-arg-list (type-resolver arg-list)
  (let ((arg-index)
        (current-token)
        (arg-list (copy-list arg-list)))
    (while (setq current-token (pop arg-list))
      (cond ((and (phpinspect-word-p current-token)
               (phpinspect-variable-p (car arg-list)))
          (push `(,(cadr (pop arg-list))
                  ,(funcall type-resolver (cadr current-token)))
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
                   (cadar (last declaration))
                 ;; @return annotation
                 (cadadr
                  (seq-find 'phpinspect-return-annotation-p
                            comment-before)))))
    (phpinspect--make-function
     :scope `(,(car scope))
     :name (cadadr (cdr declaration))
     :return-type (when type (funcall type-resolver type))
     :arguments (phpinspect--index-function-arg-list
                 type-resolver
                 (phpinspect-function-argument-list php-func)))))

(defun phpinspect--index-const-from-scope (scope)
  (phpinspect--make-variable
   :scope `(,(car scope))
   :name (cadr (cadr (cadr scope)))))

(defun phpinspect--var-annotations-from-token (token)
  (seq-filter 'phpinspect-var-annotation-p token))

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
     :type (if type (funcall type-resolver type)))))

(defun phpinspect-doc-block-p (token)
  (phpinspect-type-p token :doc-block))

(defun phpinspect--get-class-name-from-token (class-token)
  (let ((subtoken (seq-find (lambda (word)
                              (and (phpinspect-word-p word)
                                   (not (string-match
                                         (concat "^" (car (phpinspect--class-keyword-handler)))
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
                   (phpinspect--log "Extends was true")
                   (setq enc-extends t))
                  ((string= (cadr word) "implements")
                   (setq enc-extends nil)
                   (phpinspect--log "Implements was true")
                   (setq enc-implements t))
                  (t
                   (phpinspect--log "Calling Resolver from index-class on %s" (cadr word))
                   (cond (enc-extends (push (funcall type-resolver (cadr word)) extends))
                         (enc-implements (push (funcall type-resolver (cadr word)) implements))))))))

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
                    (push (phpinspect--index-function-from-scope type-resolver
                                                                 token
                                                                 comment-before)
                          methods))))

            ((phpinspect-const-p token)
             ;; Bare constants are always public
             (push (phpinspect--index-const-from-scope (list :public token))
                   constants))
            ((phpinspect-function-p token)
             ;; Bare functions are always public
             (push (phpinspect--index-function-from-scope type-resolver (list :public token) comment-before)
                   methods))
            ((phpinspect-doc-block-p token)
             (setq comment-before token))

            ;; Prevent comments from sticking around too long
            (t (setq comment-before nil))))

    ;; Dirty hack that assumes the constructor argument names to be the same as the object
    ;; attributes' names.
    ;;;
    ;; TODO: actually check the types of the variables assigned to object attributes
    (let ((constructor (seq-find (lambda (method)
                                   (string= (phpinspect--function-name method)
                                            "__construct"))
                                 methods)))
      (when constructor
        (phpinspect--log "Constructor was found")
        (dolist (variable variables)
          (when (not (phpinspect--variable-type variable))
            (phpinspect--log "Looking for variable type")
            (let ((constructor-parameter-type
                   (car (alist-get (phpinspect--variable-name variable)
                                   (phpinspect--function-arguments constructor)
                                   nil nil 'string=))))
              (if constructor-parameter-type
                  (setf (phpinspect--variable-type variable)
                        (funcall type-resolver constructor-parameter-type))))))))

    (let ((class-name (funcall type-resolver class-name)))
      `(,class-name .
                    (phpinspect--class
                     (methods . ,methods)
                     (class-name . ,class-name)
                     (static-methods . ,static-methods)
                     (static-variables . ,static-variables)
                     (variables . ,variables)
                     (constants . ,constants)
                     (extends . ,extends)
                     (implements . ,implements))))))


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
         (type-name (if (and (phpinspect-word-p (caddr use))
                             (string= "as" (cadr (caddr use))))
                        (cadr (cadddr use))
                      (progn (string-match "[^\\]+$" fqn)
                             (match-string 0 fqn)))))
    (cons type-name fqn)))

(defun phpinspect--uses-to-types (uses)
  (mapcar 'phpinspect--use-to-type uses))

(defun phpinspect--index-namespace (namespace)
  (phpinspect--index-classes
   (phpinspect--uses-to-types (seq-filter 'phpinspect-use-p namespace))
   (seq-filter 'phpinspect-class-p namespace)
   (cadadr namespace)))

(defun phpinspect--index-namespaces (namespaces &optional indexed)
  (if namespaces
      (progn
        (push (phpinspect--index-namespace (pop namespaces)) indexed)
        (phpinspect--index-namespaces namespaces indexed))
    (apply 'append (nreverse indexed))))

(defun phpinspect--index-functions (&rest args)
  "TODO: implement function indexation. This is a stub function.")

(defun phpinspect--index-tokens (tokens)
  "Index TOKENS as returned by `phpinspect--parse-current-buffer`."
  `(phpinspect--root-index
    ,(append
      (append '(classes)
              (phpinspect--index-namespaces (seq-filter 'phpinspect-namespace-p tokens))
              (phpinspect--index-classes
               (phpinspect--uses-to-types (seq-filter 'phpinspect-use-p tokens))
               (seq-filter 'phpinspect-class-p tokens))))
    (functions))
  ;; TODO: Implement function indexation
  )

(defun phpinspect--get-or-create-index-for-class-file (class-fqn)
  (phpinspect--log "Getting or creating")
  (phpinspect-get-or-create-cached-project-class
   (phpinspect--get-project-root)
   class-fqn))

(defun phpinspect-get-or-create-cached-project-class (project-root class-fqn)
  (let ((existing-index (phpinspect-get-cached-project-class
                         project-root
                         class-fqn)))
    (or
     existing-index
     (progn
       (let* ((class-file (phpinspect-get-class-filepath class-fqn))
              (visited-buffer (when class-file (find-buffer-visiting class-file)))
              (new-index))

         (phpinspect--log "FQN: %s" class-fqn)
         (phpinspect--log "filepath: %s" class-file)
         (when class-file
           (if visited-buffer
               (setq new-index (with-current-buffer visited-buffer
                                 (phpinspect--index-current-buffer)))
             (setq new-index (with-temp-buffer
                               (insert-file-contents-literally class-file)
                               (phpinspect--index-current-buffer))))
           (phpinspect--log "New index: %s" new-index)
           (dolist (class (alist-get 'classes new-index))
             (when class
               (phpinspect-cache-project-class
                (phpinspect--get-project-root)
                (cdr class))))
           (alist-get class-fqn (alist-get 'classes new-index)
                      nil
                      nil
                      'string=)))))))


(defun phpinspect--index-current-buffer ()
  (phpinspect--index-tokens (phpinspect-parse-current-buffer)))

(defun phpinspect-index-current-buffer ()
  "Index a PHP file for classes and the methods they have"
  (phpinspect--index-tokens (phpinspect-parse-current-buffer)))

(defun phpinspect--get-variables-for-class (buffer-classes class &optional static)
  (let ((class-index (or (assoc-default class buffer-classes 'string=)
                         (phpinspect--get-or-create-index-for-class-file class))))
    (when class-index
      (if static
          (append (alist-get 'static-variables class-index)
                  (alist-get 'constants class-index))
        (alist-get 'variables class-index)))))


(defun phpinspect--get-methods-for-class (buffer-classes class &optional static)
  "Extract all possible methods for a class from `buffer-classes` and the class index.
`buffer-classes` will be preferred because their data should be
more recent"
  (let ((class-index (or (alist-get class buffer-classes nil nil 'string=)
                         (phpinspect--get-or-create-index-for-class-file class))))
    (phpinspect--log "Getting methods for class (%s)" class)
    (phpinspect--log "index: %s" class-index)
    (if class-index
        ;; Use nreverse to give precedence to interfaces and abstract class method
        ;; typehints and doc blocks.
        ;; TODO: Merge this somehow with phpinspect-get-cached-project-class-methods
        (nreverse
         (append (alist-get (if static 'static-methods 'methods) class-index)
                 (apply 'append
                        (mapcar (lambda (inherit-class)
                                  (phpinspect--log "Inherit class: %s" inherit-class)
                                  (phpinspect--get-methods-for-class
                                   buffer-classes
                                   inherit-class
                                   static))
                                (append (alist-get 'extends class-index)
                                        (alist-get 'implements class-index))))))
      ;; else
      (phpinspect--log "Unable to complete for %s :(" class) nil)))

(defun phpinspect--init-mode ()
  "Initialize the phpinspect minor mode for the current buffer."

  (make-variable-buffer-local 'company-backends)
  (add-to-list 'company-backends 'phpinspect-company-backend)

  (make-variable-buffer-local 'eldoc-documentation-function)
  (setq eldoc-documentation-function 'phpinspect-eldoc-function)

  (make-variable-buffer-local 'eldoc-message-commands)
  (eldoc-add-command 'c-electric-paren)
  (eldoc-add-command 'c-electric-backspace)

  (phpinspect--after-save-action)
  (add-hook 'after-save-hook 'phpinspect--after-save-action nil 'local))

(defun phpinspect--after-save-action ()
  "Hook that should be run after saving a buffer that has
phpinspect-mode enabled. Indexes the entire buffer and updates
`phpinspect--buffer-index`. Merges the buffer index into the
project-wide index afterwards."
  (when (and (boundp phpinspect-mode) phpinspect-mode)
    (setq phpinspect--buffer-index (phpinspect--index-current-buffer))
    (dolist (class (alist-get 'classes phpinspect--buffer-index))
      (when class
        (phpinspect-cache-project-class (phpinspect--get-project-root)
                                        (cdr class))))))

(defun phpinspect--disable-mode ()
  "Clean up the buffer environment for the mode to be disabled."
  (kill-local-variable 'phpinspect--buffer-index)
  (kill-local-variable 'company-backends)
  (kill-local-variable 'eldoc-documentation-function)
  (kill-local-variable 'eldoc-message-commands))

(defun phpinspect--mode-function ()
  (if (and (boundp phpinspect-mode) phpinspect-mode)
      (phpinspect--init-mode)
    (phpinspect--disable-mode)))

(define-minor-mode phpinspect-mode "Activate phpinspect-mode"
  :after-hook (phpinspect--mode-function))

(defun phpinspect--find-innermost-incomplete-namespace (token)
  (let ((last-token (car (last token))))
    (cond ((phpinspect-incomplete-namespace-p token) token)
          ((phpinspect-incomplete-token-p last-token)
           (phpinspect--find-innermost-incomplete-namespace last-token)))))

(defun phpinspect--find-innermost-incomplete-block (token &optional last-block)
  (when (phpinspect-incomplete-block-p token)
    (setq last-block token))

  (let ((last-token (car (last token))))
    (if (phpinspect-incomplete-token-p last-token)
        (phpinspect--find-innermost-incomplete-block last-token last-block)
      last-block)))


(defun phpinspect--find-class-token (token)
  "Recurse into token tree until a class is found."
  (let ((last-token (car (last token))))
    (cond ((phpinspect-class-p token) token)
          (last-token
           (phpinspect--find-class-token last-token)))))

(defun phpinspect--find-token-enclosing-innermost-incomplete-token (token &optional enclosing-token)
  "Like `phpinspect--find-innermost-incomplete-token` but returns
  the enclosing incomplete token if there is one"
  (let ((last-token (car (last token))))
    (if (phpinspect-incomplete-token-p last-token)
        (phpinspect--find-token-enclosing-innermost-incomplete-token last-token token)
      enclosing-token)))


(defun phpinspect--find-innermost-incomplete-class (token)
  (let ((last-token (car (last token))))
    (cond ((phpinspect-incomplete-class-p token) token)
          ((phpinspect-incomplete-token-p last-token)
           (phpinspect--find-innermost-incomplete-class last-token)))))

(defun phpinspect--find-innermost-incomplete-function (token)
  (let ((last-token (car (last token))))
    (cond ((phpinspect-incomplete-function-p token) token)
          ((phpinspect-incomplete-token-p last-token)
           (phpinspect--find-innermost-incomplete-function last-token)))))

(defun phpinspect--find-innermost-incomplete-token (token)
  (phpinspect--log "Checking token %s" token)
  (let ((last-token (car (last token))))
    (if (phpinspect-incomplete-token-p last-token)
        (phpinspect--find-innermost-incomplete-token last-token)
      token)))

(defvar-local phpinspect--buffer-index nil
  "The result of the last successfull parse + index action
  executed by phpinspect for the current buffer")

(defvar phpinspect-projects '()
  "Currently active phpinspect projects and their buffers")

(defun phpinspect--find-last-variable-position-in-token (token)
  "Find the last variable that can be encountered in the top
level of a token. Nested variables are ignored."
  (let ((i (length token)))
    (while (and (not (= 0 i))
                (not (phpinspect-variable-p
                      (car (last token i)))))
      (setq i (- i 1)))
    
    (if (not (= i 0))(- (length token)  i))))

(defun phpinspect--make-method-lister (buffer-classes &optional static)
  (lambda (fqn)
    (phpinspect--get-methods-for-class buffer-classes fqn static)))

(defun phpinspect--make-method-resolver (buffer-classes)
  (lambda (class method-name)
    (seq-find (lambda (method)
                (string= (cadr method) method-name))
              (phpinspect--get-methods-for-class buffer-classes))))

(defsubst phpinspect-object-attrib-p (token)
  (phpinspect-type-p token :object-attrib))

(defsubst phpinspect-static-attrib-p (token)
  (phpinspect-type-p token :static-attrib))

(defsubst phpinspect-attrib-p (token)
  (or (phpinspect-object-attrib-p token)
      (phpinspect-static-attrib-p token)))

(defun phpinspect--buffer-index (buffer)
  (with-current-buffer buffer phpinspect--buffer-index))

(cl-defgeneric phpinspect--merge-indexes (index1 index2)
  "Merge two phpinspect index types into one and return it")

(cl-defmethod phpinspect--merge-indexes
  ((class1 (head phpinspect--class))
   (class2 (head phpinspect--class)))
  "Merge two indexed classes."
  (let* ((class1-methods (alist-get 'methods (cdr class1)))
         (class1-variables (alist-get 'variables (cdr class1))))
    (dolist (method (alist-get 'methods (cdr class2)))
      (add-to-list 'class1-methods method))
    (setf (alist-get 'methods (cdr class1)) class1-methods)

    (dolist (variable (alist-get 'variables (cdr class2)))
      (add-to-list 'class1-variables variable))
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
               (existing-class (alist-get class-name index1-classes nil nil 'string=)))
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

(cl-defmethod phpinspect--make-completion
  ((completion-candidate phpinspect--variable))
  (phpinspect--construct-completion
   :value (phpinspect--variable-name completion-candidate)
   :meta (phpinspect--variable-type completion-candidate)
   :annotation (concat " "
                       (phpinspect--get-bare-class-name-from-fqn
                        (or (phpinspect--variable-type completion-candidate)
                            "")))
   :kind 'variable))

(cl-defstruct (phpinspect--completion-list
               (:constructor phpinspect--make-completion-list))
  "Contains all data for a completion at point"
  (completions nil
               :type list
               :documentation
               "A list of completion strings")
  (metadata (make-hash-table :size 20 :test 'equal)
            :type hash-table
            :documentation
            "A hash-table with `phpinspect--completion` structures."))

(cl-defgeneric phpinspect--completion-list-add
    (comp-list completion)
  "Add a completion to a completion-list.")

(cl-defmethod phpinspect--completion-list-add
  ((comp-list phpinspect--completion-list) (completion phpinspect--completion))
  (when (not (gethash (phpinspect--completion-value completion)
                      (phpinspect--completion-list-metadata comp-list)))
    (push (phpinspect--completion-value completion)
          (phpinspect--completion-list-completions comp-list))
    (puthash (phpinspect--completion-value completion)
             completion
             (phpinspect--completion-list-metadata comp-list))))


(defun phpinspect--suggest-attributes-at-point (token-tree incomplete-token &optional static)
  (let* ((buffer-classes (phpinspect--merge-indexes
                          phpinspect--buffer-index
                          (phpinspect--index-tokens token-tree)))
         (namespace (phpinspect--find-innermost-incomplete-namespace
                     token-tree))
         (type-resolver (phpinspect--make-type-resolver-for-namespace namespace token-tree))
         (method-lister (phpinspect--make-method-lister buffer-classes static)))
    (let ((statement-type (phpinspect-get-type-of-derived-statement-in-token
                           (phpinspect--get-last-statement-in-token incomplete-token)
                           namespace
                           type-resolver)))
      (when statement-type
        (let ((completion-list (phpinspect--make-completion-list))
              (type (funcall type-resolver statement-type)))
          (append (phpinspect--get-variables-for-class
                   buffer-classes
                   type
                   static)
                  (funcall method-lister type)))))))

(defun phpinspect--make-type-resolver-for-namespace (namespace-token &optional token-tree)
  (phpinspect--make-type-resolver
   (phpinspect--uses-to-types
    (seq-filter 'phpinspect-use-p namespace-token))
   token-tree
   (cadadr namespace-token)))

(defun phpinspect--get-last-statement-in-token (token)
  (nreverse
   (seq-take-while
    (let ((keep-taking t) (last-test nil))
      (lambda (elt)
        (when last-test
          (setq keep-taking nil))
        (setq last-test (phpinspect-variable-p elt))
        (and keep-taking
             (not (phpinspect-terminator-p elt))
             (listp elt))))
    (reverse token))))

(defun phpinspect--suggest-variables-at-point (token-tree token)
  (let ((assignments (phpinspect--find-assignments-in-token
                      (if (phpinspect-incomplete-list-p token)
                          (phpinspect--find-innermost-incomplete-block token-tree)
                        token)))
        (variables)
        (func (phpinspect--find-innermost-incomplete-function token-tree)))
    (dolist (assignment assignments)
      (dolist (token assignment)
        (when (phpinspect-variable-p token)
          (push (phpinspect--make-variable
                 :name (cadr token)
                 :type "")
                variables))))

    (when func
      (dolist (token (phpinspect-function-argument-list func))
        (when (phpinspect-variable-p token)
          (push (phpinspect--make-variable
                 :name (cadr token)
                 :type "")
                variables))))
    variables))

(defun phpinspect--suggest-at-point ()
  (let* ((token-tree (phpinspect-parse-buffer-until-point (current-buffer) (point)))
         (incomplete-token (phpinspect--find-innermost-incomplete-token token-tree))
         (last-tokens (last incomplete-token 2)))
    (cond ((and (phpinspect-object-attrib-p (car last-tokens))
                (phpinspect-word-p (cadr last-tokens)))
           (phpinspect--log "word-attributes")
           (phpinspect--suggest-attributes-at-point token-tree
                                                    incomplete-token))
          ((phpinspect-object-attrib-p (cadr last-tokens))
           (phpinspect--log "object-attributes")
           (phpinspect--suggest-attributes-at-point token-tree incomplete-token))
          ((phpinspect-static-attrib-p (cadr last-tokens))
           (phpinspect--log "static-attributes")
           (phpinspect--suggest-attributes-at-point token-tree incomplete-token t))
          ((phpinspect-variable-p (cadr last-tokens))
           (phpinspect--suggest-variables-at-point token-tree incomplete-token)))))

(defvar phpinspect--last-completion-list nil
  "Used internally to save metadata about completion options
  between company backend calls")

(defun phpinspect-company-backend (command &optional arg &rest ignored)
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
          ((looking-back "\$[A-Za-z_0-9-]*")
           (let ((match (match-string 0)))
             (substring match 1 (length match))))))
   ((eq command 'post-completion)
    (when (eq 'function (phpinspect--completion-kind
                         (gethash arg (phpinspect--completion-list-metadata
                                       phpinspect--last-completion-list))))
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
                        (seq-uniq (phpinspect--completion-list-completions
                                   completion-list)
                                  'string=)))
      (setq phpinspect--last-completion-list completion-list)
      candidates))
   ((eq command 'annotation)
    (concat " " (phpinspect--completion-annotation
                 (gethash arg
                          (phpinspect--completion-list-metadata
                           phpinspect--last-completion-list)))))
   ((eq command 'kind)
    (phpinspect--completion-kind
     (gethash arg (phpinspect--completion-list-metadata
                   phpinspect--last-completion-list))))
   ((eq command 'meta)
    (phpinspect--completion-meta
     (gethash arg
              (phpinspect--completion-list-metadata phpinspect--last-completion-list))))))

(defvar phpinspect-cache ()
  "In-memory nested key-value store used for caching by
phpinspect")

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
  (class-index (make-hash-table :test 'equal :size 100 :rehash-size 40)
               :type hash-table
               :documentation
               "A `hash-table` that contains all of the currently
indexed classes in the project"))

(cl-defgeneric phpinspect--cache-getproject
    ((cache phpinspect--cache) (project-name string))
  "Get project that is located in `project-root`.")

(cl-defmethod phpinspect--cache-getproject
  ((cache phpinspect--cache) (project-root string))
  (gethash project-root (phpinspect--cache-projects cache)))

(cl-defgeneric phpinspect--cache-get-project-create
    ((cache phpinspect--cache) (project-root string))
  "Get a project that is located in `project-root` from the cache. If no such project exists in the cache yet, it is created and then returned.")

(cl-defmethod phpinspect--cache-get-project-create
  ((cache phpinspect--cache) (project-root string))
  (or (phpinspect--cache-getproject cache project-root)
      (puthash project-root
               (phpinspect--make-project-cache)
               (phpinspect--cache-projects cache))))

(cl-defgeneric phpinspect--project-add-class
    ((project phpinspect--project) (class (head phpinspect--class)))
  "Add an indexed class to a `phpinspect--project`")

(cl-defmethod phpinspect--project-add-class
  ((project phpinspect--project) (class (head phpinspect--class)))
  (let* ((class-name (alist-get 'class-name (cdr class)))
         (existing-class (gethash class-name
                                  (phpinspect--project-class-index project))))
    (puthash class-name
             (if existing-class
                 (phpinspect--merge-indexes existing-class class)
               class)
             (phpinspect--project-class-index project))))

(cl-defgeneric phpinspect--project-get-class
    ((project phpinspect--project) (class-fqn string))
  "Get indexed class by name of CLASS-FQN stored in PROJECT")

(cl-defmethod phpinspect--project-get-class
  ((project phpinspect--project) (class-fqn string))
  (gethash class-fqn
           (phpinspect--project-class-index project)))

(defun phpinspect--get-or-create-global-cache ()
  (or phpinspect-cache
      (setq phpinspect-cache (phpinspect--make-cache))))

(defsubst phpinspect-cache-project-class (project-root indexed-class)
  (phpinspect--project-add-class
   (phpinspect--cache-get-project-create (phpinspect--get-or-create-global-cache)
                                         project-root)
   indexed-class))

(defsubst phpinspect-get-cached-project-class (project-root class-fqn)
  (phpinspect--project-get-class
   (phpinspect--cache-get-project-create (phpinspect--get-or-create-global-cache)
                                         project-root)
   class-fqn))

(defsubst phpinspect-get-cached-project-class-method-type
  (project-root class-fqn method-name)
  (phpinspect--log "Getting cached project class method type for %s (%s::%s)"
                   project-root class-fqn method-name)
  (let ((found-method
         (seq-find (lambda (method)
                     (and (string= (phpinspect--function-name method) method-name)
                          (phpinspect--function-return-type method)))
                   (phpinspect-get-cached-project-class-methods
                    project-root
                    class-fqn))))
    (when found-method
      (phpinspect--log "Found method: %s" found-method)
      (phpinspect--function-return-type found-method))))

(defsubst phpinspect-get-cached-project-class-variable-type
  (project-root class-fqn variable-name)
  (let ((found-variable
         (seq-find (lambda (variable)
                     (string= (phpinspect--variable-name variable) variable-name))
                   (alist-get 'variables
                              (phpinspect-get-or-create-cached-project-class
                               project-root
                               class-fqn)))))
    (when found-variable
      (phpinspect--variable-type found-variable))))

(defun phpinspect-get-cached-project-class-methods
    (project-root class-fqn &optional static)
  (phpinspect--log "Getting cached project class methods for %s (%s)"
                   project-root class-fqn)
  (let ((index (phpinspect-get-or-create-cached-project-class
                project-root
                class-fqn)))
    (when index
      (phpinspect--log "Retrieved class index, starting method collection %s (%s)"
                       project-root class-fqn)
      ;; Use nreverse to give precedence to interface and abstract class return
      ;; types. Those are usually more well documented.
      (nreverse
       (append (alist-get (if static 'static-methods 'methods)
                          index)
               (apply 'append
                      (mapcar (lambda (class-fqn)
                                (phpinspect-get-cached-project-class-methods
                                 project-root class-fqn static))
                              (append
                               (alist-get 'extends index)
                               (alist-get 'implements index)))))))))

(defsubst phpinspect-get-cached-project-class-static-method-type
  (project-root class-fqn method-name)
  (let* ((found-method
          (seq-find (lambda (method)
                      (and (string= (phpinspect--function-name method) method-name)
                           (phpinspect--function-return-type method)))
                    (phpinspect-get-cached-project-class-methods
                     project-root
                     class-fqn
                     'static))))
    (when found-method
      (phpinspect--function-return-type found-method))))

(defun phpinspect-purge-cache ()
  (interactive)
  (setq phpinspect-cache (phpinspect--make-cache)))


(defvar phpinspect-index-executable
  (concat (file-name-directory
           (or load-file-name
               buffer-file-name))
          "/phpinspect-index.bash")
  "The path to the exexutable file that indexes class file names
  for phpinspect. Should normally be set to
  \"phpinspect-index.bash\" in the source file directory.")

(defun phpinspect--get-project-root ()
  (let ((project-root-slugs (split-string (php-project-get-root-dir) "/")))
    (expand-file-name (string-join
                       (if (member "vendor" project-root-slugs)
                           (seq-take-while (lambda (elt) (not (string= elt "vendor")))
                                           project-root-slugs)
                         project-root-slugs)
                       "/"))))

;; Use statements
;;;###autoload
(defun phpinspect-fix-uses-interactive () "Add missing use statements to a php file"
       (interactive)
       (save-buffer)
       (let* ((project-root (phpinspect--get-project-root))
              (phpinspect-json (shell-command-to-string
			  (format "cd %s && %s fxu --json %s"
				      (shell-quote-argument (phpinspect--get-project-root))
                      (shell-quote-argument phpinspect-index-executable)
				      (shell-quote-argument buffer-file-name)))))
	 (let* ((json-object-type 'hash-table)
		(json-array-type 'list)
		(json-key-type 'string)
		(phpinspect-json-data (json-read-from-string phpinspect-json)))
	   (maphash 'phpinspect-handle-phpinspect-json phpinspect-json-data))))

(defun phpinspect-handle-phpinspect-json (class-name candidates)
  "Handle key value pair of classname and FQN's"
  (let ((ncandidates (length candidates)))
    (cond ((= 1 ncandidates)
           (phpinspect-add-use (pop candidates)))
          ((= 0 ncandidates)
           (message "No use statement found for class \"%s\"" class-name))
          (t
           (phpinspect-add-use (completing-read "Class: " candidates))))))

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
	   (if (string-match "^\\( ?\\*\\|/\\)" (thing-at-point 'line t))
	       ((lambda ()
		  (forward-line -1)
		  (phpinspect-goto-first-line-no-comment-up)))))



(defun phpinspect-get-all-fqns (&optional fqn-file)
  (unless fqn-file
    (setq fqn-file "uses"))
  (with-temp-buffer
    (insert-file-contents-literally
     (concat (phpinspect--get-project-root) "/.cache/phpinspect/" fqn-file))
    (split-string (buffer-string) (char-to-string ?\n))))
      
;;;###autoload
(defun phpinspect-find-class-file (class)
  (interactive (list (completing-read "Class: " (phpinspect-get-all-fqns))))
  (find-file (phpinspect-get-class-filepath class)))

(defun phpinspect-find-own-class-file (class)
  (interactive (list (completing-read "Class: " (phpinspect-get-all-fqns "uses_own"))))
  (find-file (phpinspect-get-class-filepath class)))


(defun phpinspect-get-class-filepath (class &optional index-new)
  (phpinspect--log "%s" (phpinspect--get-project-root))
  (when (eq index-new 'index-new)
    (with-temp-buffer
      (call-process phpinspect-index-executable nil (current-buffer) nil "index" "--new")))
  (let* ((default-directory (phpinspect--get-project-root))
         (result (with-temp-buffer
                   (phpinspect--log "dir: %s" default-directory)
                   (phpinspect--log "class: %s" (string-remove-prefix "\\" class))
                   (list (call-process phpinspect-index-executable
                                       nil
                                       (current-buffer)
                                       nil
                                       "fp" (string-remove-prefix "\\" class))
                           (buffer-string)))))
    (if (not (= (car result) 0))
        ;; Index new files and try again if not done already.
        (if (eq index-new 'index-new)
            nil
          (phpinspect-get-class-filepath class 'index-new))
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
  (interactive)
  (let* ((default-directory (phpinspect--get-project-root)))
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
