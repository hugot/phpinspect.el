;;; install-deps.el --- Install dependencies -*- lexical-binding: t -*-

(require 'lisp-mnt)

(let* ((project-dir (file-name-parent-directory (file-name-directory (macroexp-file-name))))
       (file (expand-file-name "phpinspect.el" project-dir))
       dependencies)

  (with-temp-buffer
    (insert-file-contents file)
    (setq dependencies (read (lm-header-multiline "package-requires")))
    (dolist (dep dependencies)
      (package-install (car dep)))))
