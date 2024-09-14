;;; phpinspect.el --- PHP parsing and code intelligence package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 2.1.0
;; Package-Requires: ((compat "29"))
;; URL: https://github.com/hugot/phpinspect.el

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

;; PHPInspect is a minor mode that provides code intelligence for PHP in Emacs.
;; At its core is a PHP parser implemented in Emacs Lisp.  PHPInspect comes with
;; backends for `completion-at-point`, `company-mode` and `eldoc`.  A backend
;; for `xref` (which provides go-to-definition functionality) is planned to be
;; implemented at a later date.
;;
;; See docstrings for elaborate documentation on how to use the mode, starting
;; with `phpinspect-mode'. Also see M-x customize-group RET phpinspect RET.

;;; News:

;; Version 2.1.0

;; - Implemented basic support for completion of keywords and type names
;; - Made completion of function names, type names and keywords context-aware so
;;   that the provided completions are relevant at point.
;; - Implemented interactive function to insert type names and automatically
;;   import them when necessary, called `phpinspect-insert-type'.

;; - Fixed end-of-buffer error bugs in parsing of comments
;; - Fixed bug in `phpinspect-suggest-variables-at-point' which caused local
;;   "foreach" variables to not be suggested.
;; - Fixed infinite recursion bug caused by typedef dependent on itself (only
;;   occured when the type definition could not be found via the autoloader)

;; - Made incremental parser more reliable by not adopting incomplete tokens
;;   into new tree.
;; - Increased overall test coverage of phpinspect-suggest.el and
;;   phpinspect-completion.el

;; Version 2.0.1

;; - Fixed bug in `phpinspect-fix-imports' that caused it to not function
;;   properly for types used in within classes.

;; Version 2.0.0

;; - Implemented support for traits
;; - Implemented more accurate/nuanced in-memory representation of types and
;;   inherited properties/methods.  `phpinspect--class' has been removed and
;;   replaced with `phpinspect-typedef', the the function prefix of which is
;;   "phpi-typedef-".  This change is backwards-incompatible due to various name
;;   changes, refactorings and function/type removals.
;; - Improved completion performance by re-using completion lists when possible.
;; - Introduced new customizable variable: `phpinspect-imports-remove-unused',
;;   which enables/disables this behaviour for `phpinspect-fix-imports'.
;;   The default is disabled (nil).
;; - Increased test coverage for the parser and fixed various bugs discovered in
;;   the process.

;; Version 1.2.0

;; - Fixed bug in the resolving of function call return types.
;; - Implemented parsing of string concatenation tokens (.), allowing a more
;;   accurate determination of the statement to provide information about.
;;   (read: "\'a string\' . $foo->" now actually yields completion results
;;   instead of failing).

;;; Code:

(require 'cl-lib)
(require 'json)

;; internal dependencies
(require 'phpinspect-cache)
(require 'phpinspect-parser)
(require 'phpinspect-project)
(require 'phpinspect-util)
(require 'phpinspect-type)
(require 'phpinspect-index)
(require 'phpinspect-typedef)
(require 'phpinspect-worker)
(require 'phpinspect-autoload)
(require 'phpinspect-imports)
(require 'phpinspect-buffer)
(require 'phpinspect-resolvecontext)
(require 'phpinspect-eldoc)
(require 'phpinspect-suggest)
(require 'phpinspect-completion)

(defvar phpinspect-insert-file-contents-function #'insert-file-contents-literally
  "Function that phpinspect uses to insert file contents into a buffer.")

(defvar phpinspect-type-filepath-function #'phpinspect-get-class-filepath
  "Function that phpinspect uses to find the filepath of a class by its FQN.")

(define-inline phpinspect-type-filepath (fqn)
  "Call `phpinspect-type-filepath-function' with FQN as argument."
  (inline-quote
   (funcall phpinspect-type-filepath-function ,fqn)))

(defun phpinspect-parse-string-to-bmap (string)
  (with-temp-buffer
    (insert string)
    (let ((context (phpinspect-make-pctx :incremental t
                                         :bmap (phpinspect-make-bmap))))
      (phpinspect-with-parse-context context
        (phpinspect-parse-current-buffer))

      (phpinspect-pctx-bmap context))))

(defun phpinspect--init-mode ()
  "Initialize the phpinspect minor mode for the current buffer."
  (phpinspect-ensure-worker)
  (when (and phpinspect-load-stubs (not phpinspect-stub-cache))
    (phpinspect-load-stub-index))

  (setq phpinspect-current-buffer
        (phpinspect-make-buffer :buffer (current-buffer)))

  (phpinspect-register-current-buffer
   (lambda () (phpinspect-buffer-reset phpinspect-current-buffer)))
  (add-hook 'kill-buffer-hook #'phpinspect-unregister-current-buffer)

  (add-hook 'after-change-functions #'phpinspect-after-change-function)

  (when (featurep 'company)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends #'phpinspect-company-backend))

  (add-hook 'completion-at-point-functions #'phpinspect-complete-at-point nil 'local)


  (set (make-local-variable 'eldoc-documentation-function)
       #'phpinspect-eldoc-function)

  (make-local-variable 'eldoc-message-commands)
  (eldoc-add-command 'c-electric-paren)
  (eldoc-add-command 'c-electric-backspace)

  (phpinspect--after-save-action)

  (add-hook 'after-save-hook #'phpinspect--after-save-action nil 'local))

(defun phpinspect--after-save-action ()
  "This is intended to be run every time a phpinspect buffer is saved.

Reparses the entire buffer without token reuse."
  (when (and (boundp 'phpinspect-mode) phpinspect-mode)
    (phpinspect-buffer-reindex phpinspect-current-buffer)))

(defun phpinspect--disable-mode ()
  "Clean up the buffer environment for the mode to be disabled."
  (setq phpinspect-current-buffer nil)
  (kill-local-variable 'phpinspect--buffer-project)
  (kill-local-variable 'company-backends)
  (kill-local-variable 'eldoc-documentation-function)
  (kill-local-variable 'eldoc-message-commands)
  (phpinspect-unregister-current-buffer))

(defun phpinspect--mode-function ()
  (if (and (boundp 'phpinspect-mode) phpinspect-mode)
      (phpinspect--init-mode)
    (phpinspect--disable-mode)))

(defgroup phpinspect '((phpinspect-projects custom-variable))
  "PHPInspect, PHP code intelligence and completion"
  :group 'programming
  :group 'php
  :link '(function-link phpinspect-mode))

(define-minor-mode phpinspect-mode
  "A minor mode for intelligent completion for and interaction
with PHP files.

See also: \\[customize-group] RET phpinspect RET .

To initially index a project, use M-x `phpinspect-index-current-project'
in a buffer of one of the project files. Project root is detected with
`phpinspect-project-root-file-list'.

For completion see `phpinspect-complete-at-point' which is
automatically added to `completion-at-point-functions' when
phpinspect-mode is activated.

For company users, there is also
`phpinspect-company-backend'. This is automatically added to
`company-backends' when company is detected.

For eldoc see `phpinspect-eldoc-function'.

For finding/opening class files see
 `phpinspect-find-own-class-file' (bound to \\[phpinspect-find-own-class-file]) and
 `phpinspect-find-class-file' (bound to \\[phpinspect-find-class-file]).

To automatically add missing use statements for used classes to a
visited file, use `phpinspect-fix-imports'
(bound to \\[phpinspect-fix-imports]].)

By default, phpinspect looks for a composer.json file that can be
used to get autoload information for the classes that are present
in your project. It is also possible to index an entire directory
by adding it as an include dir. To do this, use
\\[phpinspect-project-add-include-dir]. Include directories can
be edited at all times using \\[customize-group] RET phpinspect.

Because of limitations in the current autoloader implementation,
you will have to run \\[phpinspect-index-current-project] every
time you create a new autoloadable file.

Example configuration if you already have a completion
UI (Company, Corfu) setup that can take advantage of completion
at point (capf) functions:

    (defun my-php-personal-hook ()
      ;; Shortcut to add use statements for classes you use.
      (define-key php-mode-map (kbd \"C-c u\") #\\='phpinspect-fix-imports)

      ;; Shortcuts to quickly search/open files of PHP classes.
      ;; You can make these local to php-mode, but making them global
      ;; like this makes them work in other modes/filetypes as well, which
      ;; can be handy when jumping between templates, config files and PHP code.
      (global-set-key (kbd \"C-c a\") #\\='phpinspect-find-class-file)
      (global-set-key (kbd \"C-c c\") #\\='phpinspect-find-own-class-file)

      ;; Enable phpinspect-mode
      (phpinspect-mode))

    (add-hook \\='php-mode-hook #\\='my-php-personal-hook)


Example configuration for `company-mode':

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
      (setq-local company-backends \\='(phpinspect-company-backend))

      ;; Shortcut to add use statements for classes you use.
      (define-key php-mode-map (kbd \"C-c u\") #\\='phpinspect-fix-imports)

      ;; Shortcuts to quickly search/open files of PHP classes.
      ;; You can make these local to php-mode, but making them global
      ;; like this makes them work in other modes/filetypes as well, which
      ;; can be handy when jumping between templates, config files and PHP code.
      (global-set-key (kbd \"C-c a\") #\\='phpinspect-find-class-file)
      (global-set-key (kbd \"C-c c\") #\\='phpinspect-find-own-class-file)

      ;; Enable phpinspect-mode
      (phpinspect-mode))

    (add-hook \\='php-mode-hook #\\='my-php-personal-hook)

    ;; End example configuration."
  :after-hook (phpinspect--mode-function)
  :keymap  (list (cons (kbd "C-c u") #'phpinspect-fix-imports)))

(defun phpinspect--suggest-at-point ()
  (phpinspect--log "Entering suggest at point. Point: %d" (point))
  (phpinspect-completion-query-execute
   (phpinspect-make-completion-query
    :buffer phpinspect-current-buffer
    :completion-point (phpinspect--determine-completion-point)
    :point (point))))

(eval-when-compile
  (declare-function company-begin-backend "company.el"))

(defun phpinspect-company-backend (command &optional arg &rest _ignored)
  "A company backend for PHP."
  (interactive (list 'interactive))
  (require 'company)
  (cond
   ((eq command 'interactive)
    (company-begin-backend 'company-phpinspect-backend))
   ((eq command 'prefix)
    (cond ((looking-back "->[A-Za-z_0-9-]*" nil)
           (let ((match (match-string 0)))
             (substring match 2 (length match))))
          ((looking-back "::[A-Za-z_0-9-]*" nil)
           (let ((match (match-string 0)))
             (substring match 2 (length match))))
          ((looking-back "\\$[A-Za-z_0-9-]" nil)
           (let ((match (match-string 0)))
             (substring match 1 (length match))))
          ((looking-back "[A-Za-z_0-9-]+" nil t)
           (match-string 0))))
   ((eq command 'post-completion)
    (when (eq 'function (phpinspect--completion-kind
                         (phpinspect--completion-list-get-metadata
                          phpinspect--last-completion-list
                          arg)))
      (insert "(")))
   ((eq command 'candidates)
    (catch 'phpinspect-parse-interrupted
      (let ((completion-list (phpinspect--suggest-at-point))
            (candidates))

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

(defsubst phpinspect-insert-file-contents (&rest args)
  "Call `phpinspect-insert-file-contents-function' with ARGS as arguments."
  (apply phpinspect-insert-file-contents-function args))

(defun phpinspect-get-all-fqns (&optional filter)
  "Return a list of all FQNS congruent with FILTER in the currently active project.

FILTER must be nil or the symbol `own' if FILTER is `own', only
fully qualified names from the project's source, and not its
dependencies, are returned."
  (let* ((project (phpinspect--cache-get-project-create
                   (phpinspect--get-or-create-global-cache)
                   (phpinspect-current-project-root)))
         (autoloader (phpinspect-project-autoload project)))
    (let ((fqns))
      (maphash (lambda (type _) (push (phpinspect-name-string type) fqns))
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

(defun phpinspect-get-class-filepath (class &optional index-new)
  "Retrieve filepath to CLASS definition file.

when INDEX-NEW is non-nil, new files are added to the index
before the search is executed."
  (let* ((project (phpinspect--cache-get-project-create
                   (phpinspect--get-or-create-global-cache)
                   (phpinspect-current-project-root))))
    (phpinspect-project-get-type-filepath project class index-new)))

(defun phpinspect-project-refresh-autoloader (project)
  (interactive (list (phpinspect--cache-get-project-create
                      (phpinspect--get-or-create-global-cache)
                      (phpinspect-current-project-root))))
  (let* ((autoloader (phpinspect-project-autoload project)))
    ;; Update display so that it is clear to the user that emacs is
    ;; responsive. Otherwise the autoloader refresh thread hogging the cpu will
    ;; make it look like emacs is not responsive, especially when M-x uses some
    ;; kind of completion framework, in which case the completion popup will
    ;; appear frozen while the thread is executing.
    (redisplay)

    (phpinspect-autoloader-refresh autoloader)))

(defun phpinspect-index-current-project ()
  "Index all available FQNs in the current project."
  (interactive)
  (let* ((project (phpinspect--cache-get-project-create
                  (phpinspect--get-or-create-global-cache)
                  (phpinspect-current-project-root))))
    (phpinspect-project-refresh-autoloader project)
    (phpinspect-project-enqueue-include-dirs project)))

(provide 'phpinspect)
;;; phpinspect.el ends here
