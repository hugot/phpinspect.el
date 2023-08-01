;;; phpinspect.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 0
;; Package-Requires: ((compat "29"))

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
(require 'phpinspect-eldoc)
(require 'phpinspect-suggest)
(require 'phpinspect-completion)

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

(defsubst phpinspect-cache-project-class (project-root indexed-class)
  (when project-root
    (phpinspect-project-add-class
     (phpinspect--cache-get-project-create (phpinspect--get-or-create-global-cache)
                                           project-root)
     indexed-class)))

(defun phpinspect-parse-string-to-bmap (string)
  (with-temp-buffer
    (insert string)
    (let ((context (phpinspect-make-pctx :incremental t
                                         :bmap (phpinspect-make-bmap))))
      (phpinspect-with-parse-context context
        (phpinspect-parse-current-buffer))

      (phpinspect-pctx-bmap context))))

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

(defun phpinspect--buffer-index (buffer)
  (with-current-buffer buffer phpinspect--buffer-index))

(defun phpinspect--suggest-at-point ()
  (phpinspect--log "Entering suggest at point. Point: %d" (point))
  (phpinspect-completion-query-execute
   (phpinspect-make-completion-query
    :buffer phpinspect-current-buffer
    :completion-point (phpinspect--determine-completion-point)
    :point (point))))

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

(provide 'phpinspect)
;;; phpinspect.el ends here
