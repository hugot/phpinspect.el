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
(require 'phpinspect-imports)
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

      (let* ((function-meta (phpinspect-meta-parent (phpinspect-meta-parent bello1)))
             (function (phpinspect-meta-token function-meta)))
        ;; The uncomplete function body was absorbed by the declaration up until
        ;; the first semicolon.
        (should (= 2 (length function)))
        (should (phpinspect-declaration-p (cadr function)))
        (should (member '(:word "Bello") (cadr function)))
        (should (member '(:word "echo") (cadr function)))

        ;; rest of tokens is absorbed by parent.
        (should (equal `(:word "if")  (phpinspect-meta-token (phpinspect-meta-find-right-sibling function-meta)))))

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

(ert-deftest phpinspect-buffer-parse-incrementally-class-with-method ()
  (with-temp-buffer
  (let* ((buffer (phpinspect-make-buffer
                  :buffer (current-buffer))))


    (setq-local phpinspect-current-buffer buffer)
    (insert
     "<?php

namespace XXX;

class AAA {

    function bbb () {
        $banana = 'aaa';
    }

}

")

    (add-hook 'after-change-functions #'phpinspect-after-change-function)
    (phpinspect-buffer-parse buffer 'no-interrupt)

    (let ((switch t)
          (delta 0))

      (dotimes (_i 30)
        (if switch
            (progn
              (setq delta -4)
              (goto-char 63)
              (delete-char 4))
          (setq delta 0)
          (goto-char 59)
          (insert "    "))
        (setq switch (not switch))

        (phpinspect-buffer-parse buffer 'no-interrupt)

        (let ((token (phpinspect-bmap-last-token-before-point
                      (phpinspect-buffer-map buffer) (+ 68 delta))))
          (should token)
          (should (phpinspect-variable-p (phpinspect-meta-token token)))
          (should (string= "banana" (cadr (phpinspect-meta-token token))))
          ;; Direct parent should be the function block
          (should (phpinspect-block-p (phpinspect-meta-token (phpinspect-meta-parent token))))
          ;; Function ancestor should remain intact
          (should (phpinspect-meta-find-parent-matching-token token #'phpinspect-function-p))))))))

(ert-deftest phpinspect-buffer-parse-incrementally-use ()
  (with-temp-buffer
  (let* ((buffer (phpinspect-make-buffer
                  :buffer (current-buffer))))


    (setq-local phpinspect-current-buffer buffer)

    (insert
     "<?php

namespace XXX;

use ZZZ\\zzz;
use AAA\\BBB;  // comment

use CCC;

")

    (add-hook 'after-change-functions #'phpinspect-after-change-function)
    (phpinspect-buffer-parse buffer 'no-interrupt)
    (let ((switch nil)
          (delta 0))

      (dotimes (_i 30)
        (if switch
            (progn
              (setq delta 0)
              (goto-char 44)
              (insert "hh")
              (should (phpinspect-edtrack-edits (phpinspect-buffer-edit-tracker buffer)))
              (should (= 51 (phpinspect-edtrack-current-position-at-point (phpinspect-buffer-edit-tracker buffer) 49))))
          (progn
            (setq delta (- 2))
            (goto-char 44)
            (delete-char 2)
            (should (phpinspect-edtrack-edits (phpinspect-buffer-edit-tracker buffer)))
            (should (= 47 (phpinspect-edtrack-current-position-at-point (phpinspect-buffer-edit-tracker buffer) 49)))))

        (setq switch (not switch))

        (phpinspect-buffer-parse buffer 'no-interrupt)

    (let ((use (phpinspect-find-first-use (phpinspect-meta-find-first-child-matching-token
                                           (phpinspect-buffer-root-meta buffer)
                                           #'phpinspect-namespace-p))))

      (should use)
      (should (= 2 (length (phpinspect-meta-whitespace-before use))))
      (should (= 24 (phpinspect-meta-start use)))
      (should (= 36 (phpinspect-meta-end use)))

      (let ((sibling (phpinspect-meta-find-right-sibling use)))
        (should sibling)
        (should (= 37 (phpinspect-meta-start sibling)))
        (should (= (+ delta 49) (phpinspect-meta-end sibling)))

        (let ((2nd-sibling (phpinspect-meta-find-right-sibling (phpinspect-meta-find-right-sibling sibling))))
          (should 2nd-sibling)
          (should (= (+ delta 63) (phpinspect-meta-start 2nd-sibling)))
          (should (= (+ delta 71) (phpinspect-meta-end 2nd-sibling)))))))))))

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

    (should (phpinspect-project-get-typedef (phpinspect-buffer-project buffer) (phpinspect--make-type :name "\\TestNamespace\\TestClass")))

    (should (= 2 (hash-table-count (phpinspect-project-typedef-index (phpinspect-buffer-project buffer)))))
    (should (= 1 (length (phpi-typedef-subscribed-to-types
                          (phpinspect-project-get-typedef
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
      (should (phpinspect-project-get-typedef
               (phpinspect-buffer-project buffer)
               (phpinspect--make-type :name "\\TestNamespace\\TestClass")))

      (should (= 0 (length (phpi-typedef-subscribed-to-types
                          (phpinspect-project-get-typedef
                           (phpinspect-buffer-project buffer)
                           (phpinspect--make-type :name "\\TestNamespace\\TestClass")))))))

    (let ((new-classes (phpinspect-make-splayt))
          (new-root (phpinspect-make-meta nil 1 400 "" 'new-root)))
      (setf (phpinspect-bmap--root-meta (phpinspect-buffer-map buffer)) new-root)
      (phpinspect-buffer-index-classes buffer new-classes)

      (should-not (phpinspect-project-get-typedef
                   (phpinspect-buffer-project buffer)
                   (phpinspect--make-type :name "\\TestNamespace\\TestClass")))

      (should (= 1 (hash-table-count (phpinspect-project-typedef-index (phpinspect-buffer-project buffer))))))))

(ert-deftest phpinspect-buffer-index-functions ()
  (with-temp-buffer
    (let ((buffer (phpinspect-make-buffer
                   :buffer (current-buffer)
                   :-project (phpinspect--make-project :autoload (phpinspect-make-autoloader)))))
      (insert "<?php
namespace NS;

class TestClass
{
    function testMethod(): RelativeType {}
}")
      (phpinspect-buffer-update-project-index buffer)


      (should (phpinspect-project-get-typedef
               (phpinspect-buffer-project buffer)
               (phpinspect--make-type :name "\\NS\\TestClass")))

      (should (= 1 (length (phpi-typedef-get-methods
                            (phpinspect-project-get-typedef
                             (phpinspect-buffer-project buffer)
                             (phpinspect--make-type :name "\\NS\\TestClass"))))))

      (setf (phpinspect-buffer-map buffer) (phpinspect-make-bmap :-root-meta (phpinspect-make-meta nil 1 400 "" 'root)))

      (phpinspect-buffer-index-functions buffer (phpinspect-make-splayt))

      (should (= 0 (length (phpi-typedef-get-methods
                            (phpinspect-project-get-typedef
                             (phpinspect-buffer-project buffer)
                             (phpinspect--make-type :name "\\NS\\TestClass")))))))))

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

    (should (phpinspect-project-get-typedef
             (phpinspect-buffer-project buffer)
             (phpinspect--make-type :name "\\TestClass")))

    (should (= 1 (length (phpi-typedef-get-properties
                          (phpinspect-project-get-typedef
                           (phpinspect-buffer-project buffer)
                           (phpinspect--make-type :name "\\TestClass"))))))


    (should (= 1 (length (phpi-typedef-get-constants
                          (phpinspect-project-get-typedef
                           (phpinspect-buffer-project buffer)
                           (phpinspect--make-type :name "\\TestClass"))))))

    (should (phpinspect--type= (phpinspect--make-type :name "\\array")
                              (phpi-var-type
                               (phpi-typedef-get-property
                                (phpinspect-project-get-typedef
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


(ert-deftest phpinspect-buffer-index-typehinted-class-variables ()
  (with-temp-buffer
    (let ((buffer (phpinspect-make-buffer
                   :buffer (current-buffer)
                   :-project (phpinspect--make-project :autoload (phpinspect-make-autoloader)))))
      (insert "<?php
declare(strict_types=1);

namespace App\\Controller\\Api\\V1;

use Illuminate\\Database\\Eloquent\\Model;
use Illuminate\\Database\\Eloquent\\Relations\\Relation;
use Illuminate\\Support\\Facades\\Auth;

class AccountStatisticsController {

    public Model $model;
    private Model $privModel;
    static Relation $relation;
    public static Relation $staticRelation;
    /* @var Relation[] */
    private array $relations;
}")

      (phpinspect-buffer-update-project-index buffer)

      (let ((class (phpinspect-project-get-typedef
                    (phpinspect-buffer-project buffer)
                    (phpinspect--make-type
                     :name "\\App\\Controller\\Api\\V1\\AccountStatisticsController"
                     :fully-qualified t))))
        (should class)

        (let ((model (phpi-typedef-get-property class "model"))
              (priv-model (phpi-typedef-get-property class "privModel"))
              ;; Static variables are stored with "$" prefix
              (relation (phpi-typedef-get-property class "$relation"))
              (static-relation (phpi-typedef-get-property class "$staticRelation"))
              (relations (phpi-typedef-get-property class "relations")))
          (should model)
          (should priv-model)
          (should relation)
          (should static-relation)
          (should relations)

          (let ((model-type (phpinspect--make-type
                             :name "\\Illuminate\\Database\\Eloquent\\Model"
                             :fully-qualified t))
                (relation-type (phpinspect--make-type
                             :name "\\Illuminate\\Database\\Eloquent\\Relations\\Relation"
                             :fully-qualified t))
                (array-type (phpinspect--make-type :name "\\array" :fully-qualified t)))

            (should (phpi-var-type model))
            (should (phpinspect--type= model-type (phpi-var-type model)))
            (should (phpi-var-type priv-model))
            (should (phpinspect--type= model-type (phpi-var-type priv-model)))
            (should (phpi-var-type relation))
            (should (phpinspect--type= relation-type (phpi-var-type relation)))
            (should (phpi-var-type static-relation))
            (should (phpinspect--type= relation-type (phpi-var-type static-relation)))

            (should (phpinspect--type= array-type (phpi-var-type relations)))
            (should (phpinspect--type=
                     relation-type
                     (phpinspect--type-contains (phpi-var-type relations))))))))))


(ert-deftest phpinspect-buffer-parse-incrementally-unfinished-variable-scope ()
  (with-temp-buffer
  (let* ((buffer (phpinspect-make-buffer
                  :buffer (current-buffer)
                  :-project (phpinspect--make-dummy-project))))


    (setq-local phpinspect-current-buffer buffer)
    (insert
     "<?php

namespace XXX;

class AAA {

    function bbb () {
        $banana = 'aaa';
    }
}

")

    (add-hook 'after-change-functions #'phpinspect-after-change-function)
    (phpinspect-buffer-parse buffer 'no-interrupt)

    ;; move to after class brace
    (goto-char 35)
    (insert "\n public Foo ")
    (phpinspect-buffer-parse buffer 'no-interrupt)
    (phpinspect-buffer-update-project-index buffer)

    ;; We're just confirming that the above code doesn't error
    (should t))))

(ert-deftest phpinspect-buffer-parse-incrementally-trait-config ()
  (with-temp-buffer
    (let* ((project (phpinspect--make-dummy-composer-project-with-code))
           (buffer (phpinspect-make-buffer :-project project :buffer (current-buffer))))

      (insert "<?php

namespace Foo;

class Bar {

    function baz(): string {}
}")

      ;; Make sure trait typedef is loaded
      (should (phpinspect-project-get-typedef-extra-or-create
               project (phpinspect--make-type :name "\\App\\Foo") 'no-enqueue))

      (setq-local phpinspect-current-buffer buffer)
      (add-hook 'after-change-functions #'phpinspect-after-change-function)
      (phpinspect-buffer-parse buffer 'no-interrupt)
      (phpinspect-buffer-update-project-index buffer)

      (let ((class (phpinspect-project-get-typedef
                    project (phpinspect--make-type :name "\\Foo\\Bar"))))

        (should class)
        (should (= 1 (length (phpi-typedef-get-methods class))))

        (goto-char 36)
        (insert "use \\App\\Foo { \\App\\Foo::do as dooo }")

        (phpinspect-buffer-parse buffer 'no-interrupt)
        (phpinspect-buffer-update-project-index buffer)

        (should (= 2 (length (phpi-typedef-get-methods class))))

        (goto-char 36)
        (kill-line)
        (phpinspect-buffer-parse buffer 'no-interrupt)
        (phpinspect-buffer-update-project-index buffer)

        (should (= 1 (length (phpi-typedef-get-methods class))))))))


(ert-deftest phpinspect-buffer-parse-incrementally-class-block-scope ()
  (with-temp-buffer
    (let* ((project (phpinspect--make-dummy-composer-project-with-code))
           (buffer (phpinspect-make-buffer :-project project :buffer (current-buffer))))

      (insert "<?php class A { public function A() {} }")

      (setq-local phpinspect-current-buffer buffer)
      (add-hook 'after-change-functions #'phpinspect-after-change-function)
      (let ((expected `(:root
			(:class
			 (:declaration
			  (:word "class")
			  (:word "A"))
			 (:block
			  (:public ;; This test explicitly tests that the
				   ;; "public" word doesn't lose its first
				   ;; character after edits.
			   (:function
			    (:declaration
			     (:word "function")
			     (:word "A")
			     (:list))
			    (:block)))))))
	    (result (phpinspect-buffer-parse buffer 'no-interrupt)))

	(should result)
	(should (equal expected result))

	(goto-char 17)
	(delete-char -1)
	(setq result (phpinspect-buffer-parse buffer 'no-interrupt))
	(should result)
	(should (equal expected result))

	(backward-char)
	(insert " ")
	(setq result (phpinspect-buffer-parse buffer 'no-interrupt))
	(should result)
	(should (equal expected result))))))

(ert-deftest phpinspect-buffer-index-update-method-name ()
    (with-temp-buffer
      (let* ((project (phpinspect--make-project :autoload (phpinspect-make-autoloader)))
	     (buffer (phpinspect-make-buffer :buffer (current-buffer) :-project project))
	     (class-type (phpinspect--make-type :name "\\NS\\TestClass" :fully-qualified t)))
      (insert "<?php
namespace NS;

class TestClass
{
    function testMe(): RelativeType {}
}")
      (setq-local phpinspect-current-buffer buffer)
      (add-hook 'after-change-functions #'phpinspect-after-change-function)
      (phpinspect-buffer-update-project-index buffer)

      (goto-char 59)
      (insert "thod")

      (phpinspect-buffer-parse buffer 'no-interrupt)
      (phpinspect-buffer-update-project-index buffer)

      (let ((typedef (phpinspect-project-get-typedef project class-type)))
	(should typedef)

	(should (length= (phpi-typedef-get-methods typedef) 1 ))

	(let ((method (phpi-typedef-get-method typedef "testMethod")))
	  (should method))))))

(ert-deftest phpinspect-buffer-parse-incrementally-comment ()
    (with-temp-buffer
      (let* ((project (phpinspect--make-project :autoload (phpinspect-make-autoloader)))
	     (buffer (phpinspect-make-buffer :buffer (current-buffer) :-project project)))
	(insert "<?php\n\n// \n")
	(setq-local phpinspect-current-buffer buffer)
	(add-hook 'after-change-functions #'phpinspect-after-change-function)

	(should (equal '(:root (:comment))
		       (phpinspect-buffer-parse buffer)))

	(goto-char 11)
	(insert "word")
	(should (equal '(:root (:comment))
		       (phpinspect-buffer-parse buffer))))))
