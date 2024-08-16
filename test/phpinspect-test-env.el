;;; phpinspect-test-env.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

(require 'phpinspect-worker)
(require 'phpinspect-cache)
(require 'phpinspect-parser)

;; Make sure that the worker is running. TODO: fully encapsulate the worker the
;; data types that are used in tests so that we don't depend on some global
;; worker object for tests.
(phpinspect-ensure-worker)
(phpinspect-purge-cache)

(defvar phpinspect-test-directory
  (file-name-directory (macroexp-file-name))
  "Directory that phpinspect tests reside in.")

(defun phpinspect--make-dummy-project (&optional fs project-root)
  (setq fs (or fs (phpinspect-make-virtual-fs))
        project-root (or project-root "could never be a real project root"))

  (let ((project (phpinspect--make-project
                  :root project-root
                  :fs fs
                  :autoload (phpinspect-make-autoloader
                             :fs fs
                             :project-root-resolver (lambda () project-root))
                  :worker 'nil-worker)))
    (setf (phpinspect-autoloader-file-indexer (phpinspect-project-autoload project))
          (phpinspect-project-make-file-indexer project))

    project))


(defvar phpinspect-test-php-file-directory
  (expand-file-name "fixtures" phpinspect-test-directory)
  "Directory with syntax trees of example PHP files.")

(defun phpinspect-test-read-fixture-data (name)
  (with-temp-buffer
    (insert-file-contents-literally (concat phpinspect-test-php-file-directory "/" name ".eld"))
    (read (current-buffer))))

(defun phpinspect-test-read-fixture-serialization (name)
  (with-temp-buffer
    (insert-file-contents-literally (concat phpinspect-test-php-file-directory "/" name ".eld"))
    (eval (read (current-buffer)) t)))

(defun phpinspect-test-parse-fixture-code (name)
  (phpinspect-parse-file
   (concat phpinspect-test-php-file-directory "/" name ".php")))

(provide 'phpinspect-test-env)
