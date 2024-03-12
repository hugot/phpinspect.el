;; test-buffer.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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
(require 'phpinspect-parser)
(require 'phpinspect-buffer)
(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))

(ert-deftest phpinspect-buffer-region-lookups ()
  (let* (parsed)
    (with-temp-buffer
      (insert-file-contents (concat phpinspect-test-php-file-directory "/NamespacedClass.php"))
      (setq phpinspect-current-buffer
            (phpinspect-make-buffer :buffer (current-buffer)))
      (setq parsed (phpinspect-buffer-parse phpinspect-current-buffer))

      (let* ((class (seq-find #'phpinspect-class-p
                              (seq-find #'phpinspect-namespace-p parsed)))
             (classname (car (cddadr class))))

        (let ((tokens (phpinspect-buffer-tokens-enclosing-point
                       phpinspect-current-buffer 617)))
          (should (eq classname
                      (phpinspect-meta-token (car tokens))))
          (should (phpinspect-declaration-p (phpinspect-meta-token (cadr tokens))))
          (should (eq class (phpinspect-meta-token (caddr tokens)))))))))


(ert-deftest phpinspect-parse-buffer-no-current ()
  "Confirm that the parser is still functional with
`phpinspect-current-buffer' unset."
  (let* ((parsed
          (with-temp-buffer
            (should-not phpinspect-current-buffer)
            (insert-file-contents (expand-file-name "NamespacedClass.php" phpinspect-test-php-file-directory))
            (phpinspect-parse-current-buffer))))

    (should (cdr parsed))))

(cl-defstruct (phpinspect-document (:constructor phpinspect-make-document))
  (buffer (get-buffer-create
                  (generate-new-buffer-name "**phpinspect-document** shadow buffer") t)
                 :type buffer
                 :documentation
                 "A hidden buffer with a reference version of the document."))

(cl-defmethod phpinspect-document-apply-edit
  ((document phpinspect-document) start end delta contents)
  (with-current-buffer (phpinspect-document-buffer document)
      (goto-char start)
      (delete-region (point) (- end delta))
      (insert contents)))

(cl-defmethod phpinspect-document-set-contents
  ((document phpinspect-document) (contents string))
  (with-current-buffer (phpinspect-document-buffer document)
    (erase-buffer)
    (insert contents)))

(defmacro phpinspect-document-setq-local (document &rest assignments)
  (declare (indent 1))
  `(with-current-buffer (phpinspect-document-buffer ,document)
     (setq-local ,@assignments)))

(defmacro phpinspect-with-document-buffer (document &rest body)
  (declare (indent 1))
  `(with-current-buffer (phpinspect-document-buffer ,document)
     ,@body))

(cl-defmethod phpinspect-document-contents ((document phpinspect-document))
  (with-current-buffer (phpinspect-document-buffer document)
    (buffer-string)))

(ert-deftest phpinspect-buffer-parse-incrementally ()
  (let* ((document (phpinspect-make-document))
         (buffer (phpinspect-make-buffer
                  :buffer (phpinspect-document-buffer document)))
         (parsed))
    ;; TODO: write tests for more complicated cases (multiple edits, etc.)
    (phpinspect-document-set-contents document "<?php function Bello() { echo 'Hello World!'; if ($name) { echo 'Hello ' . $name . '!';} }")

    (setq parsed (phpinspect-buffer-parse buffer))
    (should parsed)

    (let* ((enclosing-bello (phpinspect-buffer-tokens-enclosing-point buffer 18))
           (bello (car enclosing-bello))
           (enclosing-bello1)
           (bello1)
           (bello2))
      (should (equal '(:word "Bello") (phpinspect-meta-token bello)))
      (should parsed)

      ;; Delete function block opening brace
      (phpinspect-document-apply-edit document 24 24 -1 "")
      (should (string= "<?php function Bello()  echo 'Hello World!'; if ($name) { echo 'Hello ' . $name . '!';} }"
                       (phpinspect-document-contents document)))
      (phpinspect-buffer-register-edit buffer 24 24 1)
      (setq parsed (phpinspect-buffer-parse buffer))
      (should parsed)
      (setq enclosing-bello1 (phpinspect-buffer-tokens-enclosing-point buffer 18))
      (setq bello1 (car enclosing-bello1))
      (should (eq (phpinspect-meta-token bello) (phpinspect-meta-token bello1)))

      (should (phpinspect-declaration-p (phpinspect-meta-token (phpinspect-meta-parent bello))))
      (should (phpinspect-declaration-p (phpinspect-meta-token (phpinspect-meta-parent bello1))))

      (should (phpinspect-function-p (phpinspect-meta-token (phpinspect-meta-parent (phpinspect-meta-parent bello)))))
      (should (phpinspect-function-p (phpinspect-meta-token (phpinspect-meta-parent (phpinspect-meta-parent bello1)))))

      (let ((function (phpinspect-meta-token (phpinspect-meta-parent (phpinspect-meta-parent bello1)))))
        (should (= 2 (length function)))
        (should (phpinspect-declaration-p (cadr function)))
        (should (member '(:word "Bello") (cadr function)))
        (should (member '(:word "echo") (cadr function))))

      (phpinspect-document-apply-edit document 24 25 1 "{")
      (should (string= "<?php function Bello() { echo 'Hello World!'; if ($name) { echo 'Hello ' . $name . '!';} }"
                       (phpinspect-document-contents document)))
      (phpinspect-buffer-register-edit buffer 24 25 0)
      (setq parsed (phpinspect-buffer-parse buffer))
      (should parsed)
      (setq bello2 (car (phpinspect-buffer-tokens-enclosing-point buffer 18)))
      (should (eq (phpinspect-meta-token bello) (phpinspect-meta-token bello2))))))

(ert-deftest phpinspect-buffer-parse-incrementally-position-change ()
  (with-temp-buffer
    (let ((buffer (phpinspect-make-buffer :buffer (current-buffer))))
      (insert "<?php
declare(strict_types=1);

namespace App\\Controller\\Api\\V1;

class AccountStatisticsController {

    function __construct(){}
}")

      (setq-local phpinspect-test-buffer t)
      (add-to-list 'after-change-functions
                   (lambda (start end pre-change-length)
                     (when (boundp 'phpinspect-test-buffer)
                       (phpinspect-buffer-register-edit buffer start end pre-change-length))))

      (let* ((bmap (phpinspect-buffer-parse-map buffer))
             (class-location 67)
             (class (phpinspect-bmap-token-starting-at bmap class-location)))

        (should class)
        (should (phpinspect-class-p (phpinspect-meta-token class)))
        (should (= class-location (phpinspect-meta-start class)))

        (goto-char 65)
        (let ((edit-string "use Symfony\\Component\\HttpFoundation\\JsonResponse;\n")
              bmap class tokens-enclosing use-statement)
          (insert edit-string)

          (setq bmap (phpinspect-buffer-parse-map buffer)
                class (phpinspect-bmap-token-starting-at bmap (+ 67 (length edit-string))))

          (setq class-location (+ class-location (length edit-string)))
          (should class)
          (should (phpinspect-class-p (phpinspect-meta-token class)))
          (should (= class-location (phpinspect-meta-start class)))

          (setq tokens-enclosing (phpinspect-bmap-tokens-overlapping bmap class-location))
          (setq class (seq-find (lambda (meta) (phpinspect-class-p (phpinspect-meta-token meta)))
                                tokens-enclosing))
          (should  class)
          (should (= class-location (phpinspect-meta-start class)))
          (should (phpinspect-class-p (phpinspect-meta-token class)))

          (setq use-statement (phpinspect-bmap-token-starting-at bmap 65))
          (should use-statement)
          (should (phpinspect-use-p (phpinspect-meta-token use-statement)))
          (should (seq-find #'phpinspect-use-p (seq-find #'phpinspect-namespace-p (phpinspect-buffer-tree buffer))))

          (let ((second-use))
            (goto-char 65)
            (setq edit-string "use Another\\Use\\Statement;\n")
            (insert edit-string)

            (setq class-location (+ class-location (length edit-string)))
            (setq bmap (phpinspect-buffer-parse-map buffer)
                  class (phpinspect-bmap-token-starting-at bmap class-location))

            (should class)

            (setq second-use (phpinspect-bmap-token-starting-at bmap 65))
            (should second-use)

            (setq class (phpinspect-bmap-token-starting-at bmap class-location))
            (should  class)
            (should (= class-location (phpinspect-meta-start class)))
            (should (phpinspect-class-p (phpinspect-meta-token class)))))))))


(ert-deftest phpinspect-buffer-parse-incrementally-multiedit ()
  (let* ((document (phpinspect-make-document))
         (buffer (phpinspect-make-buffer
                  :buffer (phpinspect-document-buffer document)))
         parsed parsed-after current-tree)

    (phpinspect-document-set-contents
     document
     "<?php


namespace XXX;

use ZZZ\\zzz;





class YYY {

    function Foo() {
        if(bar()) {
            return $baz->bip->bop(bar($bim), $bom)
        }
    }
}")

    (phpinspect-document-setq-local document
      phpinspect-current-buffer buffer)
    (phpinspect-with-document-buffer document
      (setq buffer-undo-list nil)
      (add-hook 'after-change-functions #'phpinspect-after-change-function))

    (setq parsed (phpinspect-buffer-parse buffer 'no-interrupt))

    ;; Delete lines before class
    (phpinspect-with-document-buffer document
      (goto-char 40)
      (kill-line)
      (kill-line)
      (kill-line))

    (setq parsed-after (phpinspect-buffer-parse buffer 'no-interrupt))

    (should (equal parsed parsed-after))

    ;; Delete namespace declaration
    (phpinspect-with-document-buffer document
      (goto-char 9)
      (kill-line))

    (setq parsed-after (phpinspect-buffer-parse buffer 'no-interrupt))
    (setq current-tree (phpinspect-with-document-buffer document
                         (goto-char (point-min))
                         (phpinspect-parse-buffer-until-point (current-buffer) (point-max))))

    (should (equal current-tree parsed-after))

    ;;Bring back the namespace declaration
    (phpinspect-with-document-buffer document
      (undo-start)
      (undo-more 1))

    (setq parsed-after (phpinspect-buffer-parse buffer 'no-interrupt))

    (should (equal parsed parsed-after))))


(ert-deftest phpinspect-buffer-index-classes ()
  (let* ((buffer (phpinspect-make-buffer :-project (phpinspect--make-project :autoload (phpinspect-make-autoloader))))
         (namespaces (phpinspect-make-splayt))
         (declarations (phpinspect-make-splayt))
         (classes (phpinspect-make-splayt))
         (root (phpinspect-make-meta nil 1 200 "" 'root)))
    (phpinspect-splayt-insert
     namespaces 1 (phpinspect-meta-set-parent
                   (phpinspect-make-meta nil 1 100 "" '(:namespace (:word "TestNamespace") (:terminator ";")))
                   root))
    (phpinspect-splayt-insert
     declarations 20
     (phpinspect-make-meta nil 20 40 "" '(:declaration (:word "class") (:word "TestClass") (:word "extends") (:word "OtherTestClass"))))


    (phpinspect-splayt-insert classes 20 (phpinspect-make-meta nil 20 80 "" '(:class (:comment "bla") '(:declaration (:word "class") (:word "TestClass") (:word "extends") (:word "OtherTestClass")))))

    (phpinspect-buffer-index-declarations buffer declarations)
    (phpinspect-buffer-index-namespaces buffer namespaces)
    (phpinspect-buffer-index-classes buffer classes)

    (should (phpinspect-project-get-class (phpinspect-buffer-project buffer) (phpinspect--make-type :name "\\TestNamespace\\TestClass")))

    (should (= 2 (hash-table-count (phpinspect-project-class-index (phpinspect-buffer-project buffer)))))
    (should (= 1 (length (phpinspect--class-extended-classes
                          (phpinspect-project-get-class
                           (phpinspect-buffer-project buffer)
                           (phpinspect--make-type :name "\\TestNamespace\\TestClass"))))))

    (let ((new-declarations (phpinspect-make-splayt))
          (new-classes (phpinspect-make-splayt)))
      (phpinspect-splayt-insert
       new-declarations
       20
       (phpinspect-meta-set-parent
        (phpinspect-make-meta nil 20 40 "" '(:declaration (:word "class") (:word "TestClass")))
        root))

      (phpinspect-splayt-insert
       new-classes 20
       (phpinspect-meta-set-parent
        (phpinspect-make-meta nil 20 80 "" '(:class (:comment "bla") '(:declaration (:word "class") (:word "TestClass"))))
        root))

      (setf (phpinspect-buffer-map buffer) (phpinspect-make-bmap :-root-meta root))

      (phpinspect-buffer-index-declarations buffer new-declarations)
      (phpinspect-buffer-index-classes buffer new-classes)
      (should (phpinspect-project-get-class
               (phpinspect-buffer-project buffer)
               (phpinspect--make-type :name "\\TestNamespace\\TestClass")))

      (should (= 0 (length (phpinspect--class-extended-classes
                          (phpinspect-project-get-class
                           (phpinspect-buffer-project buffer)
                           (phpinspect--make-type :name "\\TestNamespace\\TestClass")))))))

    (let ((new-classes (phpinspect-make-splayt))
          (new-root (phpinspect-make-meta nil 1 400 "" 'new-root)))
      (setf (phpinspect-bmap--root-meta (phpinspect-buffer-map buffer)) new-root)
      (phpinspect-buffer-index-classes buffer new-classes)

      (should-not (phpinspect-project-get-class
                   (phpinspect-buffer-project buffer)
                   (phpinspect--make-type :name "\\TestNamespace\\TestClass")))

      (should (= 1 (hash-table-count (phpinspect-project-class-index (phpinspect-buffer-project buffer))))))))

(ert-deftest phpinspect-buffer-index-functions ()
  (let ((buffer (phpinspect-make-buffer :-project (phpinspect--make-project :autoload (phpinspect-make-autoloader))))
        (namespaces (phpinspect-make-splayt))
        (declarations  (phpinspect-make-splayt))
        (classes (phpinspect-make-splayt))
        (functions (phpinspect-make-splayt)))

    (phpinspect-splayt-insert
     namespaces 10
     (phpinspect-make-meta nil 10 200 "" '(:namespace (:word "NS") (:terminator ";"))))


    (phpinspect-splayt-insert
     declarations 20
     (phpinspect-make-meta nil 20 30 "" '(:declaration (:word "class") (:word "TestClass"))))
    (phpinspect-splayt-insert
     classes 20
     (phpinspect-make-meta nil 20 70 "" '(:class (:declaration (:word "class") (:word "TestClass")))))


    (phpinspect-splayt-insert
     declarations 40
     (phpinspect-make-meta nil 40 45 "" '(:declaration (:word "testMethod") (:list) (:word "RelativeType"))))

    (phpinspect-splayt-insert
     functions 40
     (phpinspect-make-meta nil 40 50 "" '(:function (:declaration (:word "testMethod") (:list) (:word "RelativeType")))))

    (phpinspect-buffer-index-declarations buffer declarations)
    (phpinspect-buffer-index-namespaces buffer namespaces)
    (phpinspect-buffer-index-classes buffer classes)

    (phpinspect-buffer-index-functions buffer functions)

    (should (phpinspect-project-get-class
             (phpinspect-buffer-project buffer)
             (phpinspect--make-type :name "\\NS\\TestClass")))

    (should (= 1 (hash-table-count (phpinspect--class-methods
                                    (phpinspect-project-get-class
                                     (phpinspect-buffer-project buffer)
                                     (phpinspect--make-type :name "\\NS\\TestClass"))))))

    (setf (phpinspect-buffer-map buffer) (phpinspect-make-bmap :-root-meta (phpinspect-make-meta nil 1 400 "" 'root)))

    (phpinspect-buffer-index-functions buffer (phpinspect-make-splayt))

    (should (= 0 (hash-table-count (phpinspect--class-methods
                                    (phpinspect-project-get-class
                                     (phpinspect-buffer-project buffer)
                                     (phpinspect--make-type :name "\\NS\\TestClass"))))))))

(ert-deftest phpinspect-buffer-index-class-variables ()
  (let ((buffer (phpinspect-make-buffer :-project (phpinspect--make-project :autoload (phpinspect-make-autoloader))))
        (namespaces (phpinspect-make-splayt))
        (declarations  (phpinspect-make-splayt))
        (classes (phpinspect-make-splayt))
        (functions (phpinspect-make-splayt))
        (variables (phpinspect-make-splayt)))

    (phpinspect-splayt-insert
     functions 60
     (phpinspect-make-meta
      nil 60 65 ""
      (cadr (phpinspect-parse-string
       "<?php function __construct(array $thing) { $this->banana = $thing; }"))))


    (phpinspect-splayt-insert
     declarations 20
     (phpinspect-make-meta nil 20 30 "" '(:declaration (:word "class") (:word "TestClass"))))
    (phpinspect-splayt-insert
     classes 20
     (phpinspect-make-meta nil 20 70 "" '(:class (:declaration (:word "class") (:word "TestClass")))))

    (phpinspect-splayt-insert
     variables 33
     (phpinspect-make-meta nil 33 50 "" '(:class-variable "banana")))

    (phpinspect-splayt-insert
     variables 54
     (phpinspect-make-meta nil 54 60 "" '(:const (:word "CONSTANT"))))

    (phpinspect-buffer-index-declarations buffer declarations)
    (phpinspect-buffer-index-namespaces buffer namespaces)
    (phpinspect-buffer-index-classes buffer classes)
    (phpinspect-buffer-index-functions buffer functions)

    (phpinspect-buffer-index-class-variables buffer variables)

    (should (phpinspect-project-get-class
             (phpinspect-buffer-project buffer)
             (phpinspect--make-type :name "\\TestClass")))

    (should (= 2 (length (phpinspect--class-variables
                          (phpinspect-project-get-class
                           (phpinspect-buffer-project buffer)
                           (phpinspect--make-type :name "\\TestClass"))))))


    (should (= 1 (length (phpinspect--class-get-constants
                          (phpinspect-project-get-class
                           (phpinspect-buffer-project buffer)
                           (phpinspect--make-type :name "\\TestClass"))))))

    (should (phpinspect--type= (phpinspect--make-type :name "\\array")
                              (phpinspect--variable-type
                               (phpinspect--class-get-variable
                                (phpinspect-project-get-class
                                 (phpinspect-buffer-project buffer)
                                 (phpinspect--make-type :name "\\TestClass"))
                                "banana"))))))

(ert-deftest phpinspect-buffer-map-imports ()
  (with-temp-buffer
    (let ((buffer (phpinspect-make-buffer :buffer (current-buffer))))
      (insert "<?php
declare(strict_types=1);

namespace App\\Controller\\Api\\V1;

use Illuminate\\Database\\Eloquent\\Model;
use Illuminate\\Database\\Eloquent\\Relations\\Relation;
use Illuminate\\Support\\Facades\\Auth;

class AccountStatisticsController {

    function __construct(){}
}")
      (let ((bmap (phpinspect-buffer-parse-map buffer)))
        (should (equal
                 `((:use (:word "Illuminate\\Database\\Eloquent\\Model") (:terminator ";"))
                   (:use (:word "Illuminate\\Database\\Eloquent\\Relations\\Relation") (:terminator ";"))
                   (:use (:word "Illuminate\\Support\\Facades\\Auth") (:terminator ";")))
                 (mapcar #'phpinspect-meta-token
                         (phpinspect-splayt-to-list (phpinspect-bmap-imports bmap)))))))))
