; test-autoload.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Hugo Thunnissen <devel@hugot.nl>

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

;;

;;; Code:


(require 'ert)
(require 'phpinspect-fs)
(require 'phpinspect-autoload)
(require 'phpinspect-resolvecontext)
(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))


(ert-deftest phpinspect-filename-to-typename ()
  (should (eq (phpinspect-intern-name "\\Foo\\Bar") (phpinspect-filename-to-typename "src/" "src/Foo////////Bar.php")))

  (should (eq (phpinspect-intern-name "\\Foo\\Bar") (phpinspect-filename-to-typename "src/somewhere/else/" "src/somewhere/else/Foo/Bar.php"))))


(ert-deftest phpinspect-find-composer-json-files ()
  (let* ((fs (phpinspect-make-virtual-fs)))
    (phpinspect-virtual-fs-set-file fs
      "/root/composer.json"
      "{ \"autoload\": { \"psr-4\": {\"WoW\\\\Dwarves\\\\\": \"src/\"}}}")

    (phpinspect-virtual-fs-set-file fs
      "/root/vendor/runescape/client/composer.json"
      "{\"autoload\": { \"psr-0\": {\"Runescape\\\\Banana\\\\\": [\"src/\", \"lib\"]}}}")


    (phpinspect-virtual-fs-set-file fs
      "/root/vendor/apples/pears/composer.json"
     "{\"autoload\": { \"psr-0\": {\"Runescape\\\\Banana\\\\\": [\"src/\", \"lib\"]}}}")

    (let ((sorter (lambda (file1 file2) (string-lessp (cdr file1) (cdr file2)))))

      (should (equal (sort (copy-sequence
                            '((vendor . "/root/vendor/apples/pears/composer.json")
                              (vendor . "/root/vendor/runescape/client/composer.json")
                              (local . "/root/composer.json")))
                           sorter)
                     (sort (phpinspect-find-composer-json-files fs "/root")
                           sorter))))))

(ert-deftest phpinspect-autoload-composer-json-iterator ()
  (let* ((fs (phpinspect-make-virtual-fs))
         (autoloader (phpinspect-make-autoloader
                      :fs fs
                      :project-root-resolver (lambda () "/root")
                      :file-indexer
                      (phpinspect-project-make-file-indexer
                       (phpinspect--make-project :root "/root" :fs fs))))
         result error)


      (phpinspect-virtual-fs-set-file fs
      "/root/composer.json"
      "{ \"autoload\": { \"psr-4\": {\"WoW\\\\Dwarves\\\\\": \"src/\"}}}")

    (phpinspect-virtual-fs-set-file fs
      "/root/vendor/runescape/client/composer.json"
      "{\"autoload\": { \"psr-0\": {\"Runescape\\\\Banana\\\\\": [\"src/\", \"lib\"]}}}")


    (phpinspect-virtual-fs-set-file fs
      "/root/vendor/apples/pears/composer.json"
      "{\"autoload\": { \"psr-0\": {\"Runescape\\\\Banana\\\\\": [\"src/\", \"lib\"]},
                        \"psr-4\": {\"Another\\\\Namespace\\\\\": [\"separate/\"]}}}")

    (phpinspect-pipeline (phpinspect-find-composer-json-files fs "/root")
      :async (lambda (res err)
               (setq result res
                     error err))
      :into (phpinspect-iterate-composer-jsons :with-context autoloader))

    (while (not (or result error))
      (thread-yield))

    (should-not error)

    (should (= 4 (length result)))
    (should (= 2 (length (seq-filter #'phpinspect-psr0-p result))))
    (should (= 2 (length (seq-filter #'phpinspect-psr4-p result))))))

(ert-deftest phpinspect-al-put-type-bag ()
  (let ((al (phpinspect-make-autoloader)))
    (phpinspect-autoloader-put-type-bag al (phpinspect-intern-name "\\App\\Place\\Mountain"))
    (phpinspect-autoloader-put-type-bag al (phpinspect-intern-name "\\App\\Earth\\Mountain"))

    (should (equal `(,(phpinspect-intern-name "\\App\\Place\\Mountain")
                     ,(phpinspect-intern-name "\\App\\Earth\\Mountain"))
                   (phpinspect-autoloader-get-type-bag al (phpinspect-intern-name "Mountain"))))))

(ert-deftest phpinspect-al-strategy-execute ()
  (let* ((fs (phpinspect-make-virtual-fs))
         (project (phpinspect--make-project :root "/project/root" :fs fs))
         (autoloader (phpinspect-make-autoloader
                      :fs fs
                      :project-root-resolver (lambda () "/project/root")
                      :file-indexer (phpinspect-project-make-file-indexer project)))
         result error)

    (setf (phpinspect-project-autoload project) autoloader)

    (phpinspect-virtual-fs-set-file
     fs
     "/project/root/composer.json"
     "{ \"autoload\": { \"psr-4\": {\"App\\\\Banana\\\\\": [\"src/\", \"lib\"]}}}")

    (phpinspect-virtual-fs-set-file fs "/project/root/src/TestClass.php" "")

    (phpinspect-virtual-fs-set-file
     fs
    "/project/root/vendor/runescape/client/composer.json"
    "{\"autoload\": { \"psr-0\": {\"Runescape\\\\Banana\\\\\": [\"src/\", \"lib\"]}}}")

    (phpinspect-virtual-fs-set-file
     fs "/project/root/vendor/runescape/client/src/TestClass.php" "")

     (phpinspect-virtual-fs-set-file
      fs
      "/project/root/vendor/runescape/client/src/Runescape/Banana/App.php"
      "")

     (phpinspect-virtual-fs-set-file
      fs "/project/root/vendor/runescape/client/src/LibClass.php" "")

     (phpinspect-virtual-fs-set-file
      fs
      "/project/root/vendor/not-runescape/wow/composer.json"
      "{ \"autoload\": { \"psr-4\": {\"WoW\\\\Dwarves\\\\\": \"src/\"},
                         \"files\": [ \"include/FilesList.php\"]}}")

     (phpinspect-virtual-fs-set-file fs
       "/project/root/vendor/not-runescape/wow/include/FilesList.php"
       "<?php class FilesList { function list() {} }")

     (phpinspect-virtual-fs-set-file
       fs "/project/root/vendor/not-runescape/wow/src/TestClass.php" "")

    (phpinspect-pipeline (phpinspect-find-composer-json-files fs "/project/root")
      :async (lambda (res err)
               (setq result res
                     error err))
      :into (phpinspect-iterate-composer-jsons :with-context autoloader)
      :into phpinspect-al-strategy-execute)

    (while (not (or result error))
      (thread-yield))

    (should-not error)

    (should-not (hash-table-empty-p (phpinspect-autoloader-own-types autoloader)))
    (should-not (hash-table-empty-p (phpinspect-autoloader-types autoloader)))

    (should (phpinspect-project-get-typedef project (phpinspect--make-type :name "\\FilesList")))

    (should (string= "/project/root/vendor/runescape/client/src/Runescape/Banana/App.php"
                     (phpinspect-autoloader-resolve
                      autoloader
                      (phpinspect-intern-name "\\Runescape\\Banana\\App"))))
    (should (string= "/project/root/vendor/not-runescape/wow/src/TestClass.php"
                     (phpinspect-autoloader-resolve
                      autoloader
                      (phpinspect-intern-name "\\WoW\\Dwarves\\TestClass"))))))
