# phpinspect.el
PHPInspect is a minor mode that provides code intelligence for PHP in Emacs. At
its core is a PHP parser implemented in Emacs Lisp. PHPInspect comes with
backends for `completion-at-point`, `company-mode` and `eldoc`. A backend for
`xref` (which provides go-to-definition functionality) is planned to be
implemented at a later date. The main documentation of the mode is in the
docstring of the mode itself (`C-h f phpinspect-mode RET` to view, or read it in
the source code of [phpinspect.el](phpinspect.el)).

## Projects and Finding Types
By default, phpinspect will recognize composer projects and read their
composer.json files for autoload information which is used to find files in
which the types/classes/functions you use in your code are defined. It is also
possible to add an "include directory" of files that should always be read and
indexed for a certain project. To do this, open a file in a project and run `M-x
phpinspect-project-add-include-dir`. You can also edit the list of include
directories via `M-x customize-goup RET phpinspect RET`.

## Example Configuration
If you already have a completion UI setup that is able to use
`completion-at-point-functions` as completion source, you can basically just
enable phpinspect-mode and you'll be good to go. An example of a basic mode hook
configuration to get the most out of phpinspect is the following:

```elisp
(defun my-php-personal-hook ()
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
```

## Example config with company mode setup

```elisp
;;;###autoload
(defun my-php-personal-hook ()
  ;; It is important to enable `company-mode' before setting
  ;; the variables below.
  (company-mode)
  (setq-local company-minimum-prefix-length 0)
  (setq-local company-tooltip-align-annotations t)
  (setq-local company-idle-delay 0.1)
  (setq-local company-backends '(phpinspect-company-backend))

  ;; Shortcut to add use statements for classes you use.
  (define-key php-mode-map (kbd "C-c u") 'phpinspect-fix-imports)

  ;; Shortcuts to quickly search/open files of PHP classes.
  (global-set-key (kbd "C-c a") 'phpinspect-find-class-file)
  (global-set-key (kbd "C-c c") 'phpinspect-find-own-class-file)

  (phpinspect-mode))

(add-hook 'php-mode-hook #'my-php-personal-hook)
```

## Install

```bash
git clone https://git.snorba.art/hugo/phpinspect.el ~/projects/phpinspect.el
```

```elisp
(add-to-list 'load-path "~/projects/phpinspect.el")
(require 'phpinspect)
```

## Compilation
It is highly recommended to byte- or native compile phpinspect. Aside from the
normal performance boost that this brings to most packages, it can reduce
phpinspect's parsing time by up to 90%. It especially makes a difference when
incrementally parsing edited buffers. For example:

### benchmarks/parse-file.el uncompiled on Ryzen 5 3600 (time in seconds):
```
Incremental parse (warmup):
Elapsed time: 0.168390 (0.019751 in 1 GC’s)
Incremental parse:
Elapsed time: 0.143811 (0.000000 in 0 GC’s)
Incremental parse (no edits):
Elapsed time: 0.000284 (0.000000 in 0 GC’s)
Incremental parse repeat (no edits):
Elapsed time: 0.000241 (0.000000 in 0 GC’s)
Incremental parse after buffer edit:
Elapsed time: 0.012449 (0.000000 in 0 GC’s)
Incremental parse after 2 more edits:
Elapsed time: 0.015839 (0.000000 in 0 GC’s)
Bare (no token reuse) parse (warmup):
Elapsed time: 0.048996 (0.000000 in 0 GC’s)
Bare (no token reuse) parse:
Elapsed time: 0.052495 (0.000000 in 0 GC’s)
```

### benchmarks/parse-file.el with native compilation on Ryzen 5 3600 (time in seconds):
```
Incremental parse (warmup):
Elapsed time: 0.023432 (0.000000 in 0 GC’s)
Incremental parse:
Elapsed time: 0.018350 (0.000000 in 0 GC’s)
Incremental parse (no edits):
Elapsed time: 0.000076 (0.000000 in 0 GC’s)
Incremental parse repeat (no edits):
Elapsed time: 0.000058 (0.000000 in 0 GC’s)
Incremental parse after buffer edit:
Elapsed time: 0.001212 (0.000000 in 0 GC’s)
Incremental parse after 2 more edits:
Elapsed time: 0.001381 (0.000000 in 0 GC’s)
Bare (no token reuse) parse (warmup):
Elapsed time: 0.013874 (0.000000 in 0 GC’s)
Bare (no token reuse) parse:
Elapsed time: 0.013878 (0.000000 in 0 GC’s)
```

## Development

### Running tests
Tests are implemented using `ert`. You can run them in batch mode with the following
command:

```bash
emacs -L ./ -batch -l ert -l ./phpinspect.el -l ./test/phpinspect-test.el -f ert-run-tests-batch-and-exit
```
