;;; phpinspect-test-env.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

(require 'phpinspect-worker)
(require 'phpinspect-cache)
(require 'phpinspect-parser)

(require 'phpinspect-imports)
;; Always enable experimental features in the test suite
(setq phpinspect-imports-remove-unused t)

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

(defun phpinspect--make-dummy-composer-project-with-code ()
  (let ((fs (phpinspect-make-virtual-fs)))
    (phpinspect-virtual-fs-set-file
      fs
      "/project/root/composer.json"
      "{ \"autoload\": { \"psr-4\": {\"App\\\\\": [\"src/\", \"lib\"]}}}")

    (phpinspect-virtual-fs-set-file fs
      "/project/root/src/Foo.php"
      "<?php namespace App; trait Foo { public function do(): static {} public static function dont(): Baz {} }")

    (phpinspect-virtual-fs-set-file fs
      "/project/root/src/Baz.php"
      "<?php namespace App; class Baz { public function amBaz(): bool {} }")

    (phpinspect-virtual-fs-set-file fs
      "/project/root/src/Bar.php"
      "<?php namespace App; class Bar { use Foo; public function foo(): Foo {} }")

    (phpinspect-virtual-fs-set-file fs
      "/project/root/src/Harry.php"
      "<?php namespace App; class Harry { public function amBarry(): bool {} }")


    (phpinspect-virtual-fs-set-file fs
      "/project/root/src/Barry.php"
      "<?php namespace App; class Barry { public function getHarry(): Harry {} }")


    (let* ((project (phpinspect--make-dummy-project fs "/project/root"))
           (autoload (phpinspect-project-autoload project))
           result error)

      (phpinspect-autoloader-refresh autoload (lambda (res err)
                                                (setq result res error err)))

      (while (not (or result error))
        (thread-yield))

      project)))

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
