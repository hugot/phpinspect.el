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
  (file-name-directory
   (or load-file-name
       buffer-file-name))
  "Directory that phpinspect tests reside in.")


(defvar phpinspect-test-php-file-directory
  (concat
   (file-name-directory
    (or load-file-name
        buffer-file-name))
   "/fixtures")
  "Directory with syntax trees of example PHP files.")

(defun phpinspect-test-read-fixture-data (name)
  (with-temp-buffer
    (insert-file-contents-literally (concat phpinspect-test-php-file-directory "/" name ".eld"))
    (read (current-buffer))))

(defun phpinspect-test-read-fixture-serialization (name)
  (with-temp-buffer
    (insert-file-contents-literally (concat phpinspect-test-php-file-directory "/" name ".eld"))
    (eval (read (current-buffer)))))

(defun phpinspect-test-parse-fixture-code (name)
  (phpinspect-parse-file
   (concat phpinspect-test-php-file-directory "/" name ".php")))

(provide 'phpinspect-test-env)
