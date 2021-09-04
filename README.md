# phpinspect.el

WIP. Documentation is in the making.

Example config:

```elisp
;;;###autoload
(defun my-php-personal-hook ()
  (set (make-local-variable 'company-minimum-prefix-length) 0)
  (set (make-local-variable 'company-tooltip-align-annotations) t)
  (set (make-local-variable 'company-idle-delay) 0.1)
  (set (make-local-variable 'company-backends) '(phpinspect-company-backend))

  ;; Shortcut to add use statements for classes you use.
  (define-key php-mode-map (kbd "C-c u") 'phpinspect-fix-uses-interactive)
  
  ;; Shortcuts to quickly search/open files of PHP classes.
  (global-set-key (kbd "C-c a") 'phpinspect-find-class-file)
  (global-set-key (kbd "C-c c") 'phpinspect-find-own-class-file)

  (phpinspect-mode))
  
  (add-hook 'php-mode-hook 'my-php-personal-hook)

```
