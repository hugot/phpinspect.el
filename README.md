# phpinspect.el

WIP. More documentation is in the making.

## Example config

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
  (define-key php-mode-map (kbd "C-c u") 'phpinspect-fix-uses-interactive)

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

## Development

### Running tests
Tests are implemented using `ert`. You can run them in batch mode with the following
command:

```bash
emacs -L ./ -batch -l ert -l ./phpinspect.el -l ./test/phpinspect-test.el -f ert-run-tests-batch-and-exit
```
