;;; phpinspect.el --- PHP parsing and code intelligence package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 3.0.0
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
;; backends for `completion-at-point` and `eldoc`.  A backend
;; for `xref` (which provides go-to-definition functionality) is planned to be
;; implemented at a later date.
;;
;; See docstrings for elaborate documentation on how to use the mode, starting
;; with `phpinspect-mode'. Also see M-x customize-group RET phpinspect RET.

;;; News:

;; Version 3.0.0

;; - Reworked collaborative threading approach which should result in better
;;   overall responsiveness.
;; - Made parsing and indexation of buffer fully async and pro-active
;;   using collaborative threads.
;; - Removed company-mode backend (use capf instead)
;; - Added tests, fixed various bugs and made incremental indexation
;;   of buffer code more robust.
;; - Implemented "classmap" autoload directive

;; Version 3.0.0

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
(require 'phpinspect-name)

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

  (phpinspect-claim-buffer (current-buffer))

  (phpinspect-register-current-buffer
   (lambda () (phpinspect-buffer-reset phpinspect-current-buffer)))
  (add-hook 'kill-buffer-hook #'phpinspect-unregister-current-buffer)

  (add-hook 'completion-at-point-functions #'phpinspect-complete-at-point nil 'local)


  (add-to-list 'eldoc-documentation-functions #'phpinspect-eldoc-function)

  (make-local-variable 'eldoc-message-commands)
  (eldoc-add-command 'c-electric-paren)
  (eldoc-add-command 'c-electric-backspace)

  (phpinspect--after-save-action)

  (add-hook 'after-save-hook #'phpinspect--after-save-action nil 'local))

(defun phpinspect--after-save-action ()
  "This is intended to be run every time a phpinspect buffer is saved.

Reparses the entire buffer without token reuse."
  (when (and (boundp 'phpinspect-mode) phpinspect-mode)
    ;; Make sure that the project's autoloader is aware of the file
    (when-let ((file-name (buffer-file-name))
               (project (phpinspect-buffer-project phpinspect-current-buffer))
               (autoloader (phpinspect-project-autoload project)))
      (phpinspect-autoloader-ensure-file-indexed autoloader file-name))))

(defun phpinspect--disable-mode ()
  "Clean up the buffer environment for the mode to be disabled."
  (setq phpinspect-current-buffer nil)
  (kill-local-variable 'phpinspect--buffer-project)
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

For eldoc see `phpinspect-eldoc-function'.

For finding/opening class files see
 `phpinspect-find-own-class-file' (bound to \\[phpinspect-find-own-class-file]) and
 `phpinspect-find-class-file' (bound to \\[phpinspect-find-class-file]).

To automatically add missing use statements for used classes to a
visited file, use `phpinspect-fix-imports'
(bound to \\[phpinspect-fix-imports]].)

By default, phpinspect loads code like PHP does: via standards
compliant autoloading. Upon opening a file and activating
phpinspect-mode, phpinspect will look for a composer.json file to
extract autoload-information from. Supported autoload directives
are:
  - files: list of files to parse/index wholesale
  - PSR-0: directory with nested subdirectories structured according to
           the namespacing scheme.
  - PSR-4: PSR-0 directory with namespace prefix
  - classmap: Directories/files to parse and index wholesale.
              (not enabled by default, see additional note)

Note on classmap directive: As of [2024-09-28], the classmap
autoload directive has been implemented but is not enabled by
default. It can be enabled by setting
`phpinspect-autoload-classmaps' to `t'.

It is also possible to wholesale index an entire directory by
adding it as an include dir. To do this, use
\\[phpinspect-project-add-include-dir]. Include directories can
be edited at all times using \\[customize-group] RET phpinspect.
Include dirs do not depend on the project using composer.

Because of limitations in the current autoloader implementation,
you will have to run \\[phpinspect-index-current-project] when
you delete a file, for it to be removed from the autoloader.

Example configuration if you already have a completion
UI (Company, Corfu) setup that can take advantage of completion
at point (capf) functions:

With `use-package':
    (use-package phpinspect
      :ensure nil
      :commands (phpinspect-mode)
      :bind ((\"C-c c\" . phpinspect-find-own-class-file)
             (\"C-c u\" . phpinspect-fix-imports)
             :map phpinspect-mode-map
             (\"C-c a\" . phpinspect-find-class-file))
      ;; Automatically add missing imports before saving a file
      :hook ((before-save . phpinspect-fix-imports))
      :custom (phpinspect-autoload-classmaps t
               \"Enable classmap autoload directive\"))

With a classic hook function:
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

;; end example configuration."

  :after-hook (phpinspect--mode-function)
  :keymap  (list (cons (kbd "C-c u") #'phpinspect-fix-imports)))

(defun phpinspect--suggest-at-point ()
  (phpinspect--log "Entering suggest at point. Point: %d" (point))
  (phpinspect-completion-query-execute
   (phpinspect-make-completion-query
    :buffer phpinspect-current-buffer
    :completion-point (phpinspect--determine-completion-point)
    :point (point))))

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

    (phpinspect-autoloader-refresh autoloader nil 'report-progress)))

(defun phpinspect-index-current-project ()
  "Index all available FQNs in the current project."
  (interactive)
  (let* ((project (phpinspect--cache-get-project-create
                  (phpinspect--get-or-create-global-cache)
                  (phpinspect-current-project-root))))
    (phpinspect-project-refresh-autoloader project)
    (phpinspect-project-enqueue-include-dirs project)))

(defun phpinspect-insert-skeleton ()
  "Insert a PHP opening tag and a namespace at the top of the buffer.

Only works when in a PSR0 or PSR4 autoload-able directory."
  (interactive)
  (when (and phpinspect-mode buffer-file-name)
    (when-let ((type-name (phpinspect-autoloader-request-type-name
                           (phpinspect-project-autoload
                            (phpinspect-current-project))
                           buffer-file-name)))
      (save-excursion
        (goto-char (point-min))

        (insert (format "<?php

namespace %s;

" (phpinspect-name-non-fqn-string
   (phpinspect-name-namespace type-name))))))))

(when (featurep 'autoinsert)
  (define-auto-insert "\\.php$" #'phpinspect-insert-skeleton))

(provide 'phpinspect)
;;; phpinspect.el ends here
