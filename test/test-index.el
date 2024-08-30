;;; test-index.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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
(require 'phpinspect-index)
(require 'phpinspect-parse-context)
(require 'phpinspect-bmap)
(require 'phpinspect-parser)
(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))


(ert-deftest phpinspect-index-static-methods ()
  (let* ((class-tokens
          `(:root
            (:class
             (:declaration (:word "class") (:word "Potato"))
             (:block
              (:static
               (:function (:declaration (:word "function")
                                        (:word "staticMethod")
                                        (:list (:variable "untyped")
                                               (:comma)
                                               (:word "array")
                                               (:variable "things")))
                          (:block)))))))
         (index (eval (phpinspect--serialize-root-index
                       (phpinspect--index-tokens class-tokens))))
         (expected-index
          `(phpinspect--root-index
            (imports)
            (classes
             (,(phpinspect--make-type :name "\\Potato" :fully-qualified t)
              phpinspect--indexed-class
              (complete . t)
              (trait-config)
              (class-name . ,(phpinspect--make-type :name "\\Potato" :fully-qualified t))
              (declaration . (:declaration (:word "class") (:word "Potato")))
              (location . (0 0))
              (imports)
              (methods)
              (static-methods . (,(phpinspect--make-function
                                   :name "staticMethod"
                                   :scope '(:public)
                                   :token '(:function (:declaration (:word "function")
                                        (:word "staticMethod")
                                        (:list (:variable "untyped")
                                               (:comma)
                                               (:word "array")
                                               (:variable "things")))
                                                      (:block))
                                   :arguments `(("untyped" . nil)
                                                ("things" . ,(phpinspect--make-type :name "\\array"
                                                                                  :collection t
                                                                                  :fully-qualified t)))
                                   :return-type nil)))
              (static-variables)
              (variables)
              (constants)
              (extends)
              (implements)
              (used-types . (,(phpinspect-intern-name "array")))))
            (used-types)
            (functions))))
    (should (equal expected-index index))))

(ert-deftest phpinspect-index-used-types-in-class ()
  (let* ((result (phpinspect--index-tokens
                  (phpinspect-parse-string
                   "<?php namespace Field; class Potato extends Cheese, Bacon implements Ham, Bagel {

use UsedTrait;

private array $bong = [ PropertyArray::class => PropertyArrayItem::class ];

private PropertyType $property;

/** @param ParamAnnotation $par
@throws ThrowAnnotationException */
*/
public function makeThing($par): Thing
{
if ((new Monkey())->tree() === true) {
   return new ExtendedThing();
}
return StaticThing::create(new ThingFactory())->makeThing((((new Potato())->antiPotato(new OtherThing(function (InnerFunctionParam $param) {
if ($param instanceof InstanceOffed) {
/** @var VarAnnotation $bing */
try {
  $bing = [ ArrayKey::class => [ NestedArrayKey::class => (CastedType) NestedArray::call(), 'ba' => ArrayItem::class ], ];
} catch (CaughtException $e) {
// nothing
}
}
})))));
}")))
         (used-types (alist-get 'used-types (car (alist-get 'classes result)))))

    (should (equal
             (mapcar #'phpinspect-intern-name
                     (sort
                      (copy-sequence
                       '("Cheese" "Bacon" "Ham" "Bagel" "Monkey" "ExtendedThing"
                         "StaticThing" "Thing" "ThingFactory" "Potato" "OtherThing"
                         "InnerFunctionParam" "PropertyType" "InstanceOffed"
                         "NestedArray" "UsedTrait" "VarAnnotation" "ParamAnnotation"
                         "ThrowAnnotationException" "CaughtException" "CastedType"
                         "NestedArrayKey" "ArrayKey" "ArrayItem" "PropertyArray"
                         "PropertyArrayItem" "array"))
                      #'string<))
             (sort used-types (lambda (s1 s2) (string< (phpinspect-name-string s1) (phpinspect-name-string s2))))))))

(ert-deftest phpinspect--find-used-types-in-tokens ()
  (let ((blocks `(
                  ((:block (:word "return")
                           (:word "new")
                           (:word "Response")
                           (:list))
                   ("Response"))
                  ((:block (:list (:word "new") (:word "Response"))
                           (:object-attrib (:word "someMethod")
                                           (:list (:word "new")
                                                  (:word "Request"))))
                   ("Request" "Response")))))
    (dolist (set blocks)
      (let ((result (phpinspect--find-used-types-in-tokens (car set))))
        (should (equal (sort (copy-sequence (cadr set)) #'string-lessp) (sort result #'string-lessp)))))))

(ert-deftest phpinspect-index-method-annotations ()
  (let* ((result (phpinspect--index-tokens
                  (phpinspect-parse-string
                   "<?php

/* @method int peel(bool $fast, array $loose)
                           * @method Banana duplicate() Words after annotation
                            @method hold() **/
                           class Banana {}")))
         (class (car (alist-get 'classes result)))
         (methods (alist-get 'methods class)))
    (should (= 3 (length methods)))
    (dolist (method methods)
      (should (member (phpi-fn-name method)
                      '("duplicate" "hold" "peel")))

      (cond ((string= (phpi-fn-name method)
                      "duplicate")
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\Banana" :fully-qualified t)
                      (phpi-fn-return-type method))))
            ((string= (phpi-fn-name method)
                      "peel")
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\int" :fully-qualified t)
                      (phpi-fn-return-type method)))

             (should (= 2 (length (phpi-fn-arguments method))))
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\array" :fully-qualified t)
                      (phpi-fn-argument-type method "loose")))

             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\bool" :fully-qualified t)
                      (phpi-fn-argument-type method "fast"))))
            ((string= (phpi-fn-name method)
                      "hold")
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\void" :fully-qualified t)
                      (phpi-fn-return-type method))))))))

(ert-deftest phpinspect-index-static-method-annotations ()
  (let* ((result (phpinspect--index-tokens
                  (phpinspect-parse-string
                   "<?php

/* @method static int peel(bool $fast, array $loose)
                           * @method static Banana create()
                            @method static hold() **/
                           class Banana {}")))
         (class (car (alist-get 'classes result)))
         (methods (alist-get 'static-methods class)))
    (should (= 3 (length methods)))
    (dolist (method methods)
      (should (member (phpi-fn-name method)
                      '("create" "hold" "peel")))

      (cond ((string= (phpi-fn-name method)
                      "duplicate")
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\Banana" :fully-qualified t)
                      (phpi-fn-return-type method))))
            ((string= (phpi-fn-name method)
                      "peel")
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\int" :fully-qualified t)
                      (phpi-fn-return-type method)))

             (should (= 2 (length (phpi-fn-arguments method))))
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\array" :fully-qualified t)
                      (phpi-fn-argument-type method "loose")))
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\bool" :fully-qualified t)
                      (phpi-fn-argument-type method "fast"))))
            ((string= (phpi-fn-name method)
                      "hold")
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\void" :fully-qualified t)
                      (phpi-fn-return-type method))))))))

(require 'phpinspect-serialize)

(ert-deftest phpinspect-index-tokens-class ()
  (let* ((index1
          (eval
           (phpinspect--serialize-root-index
            (phpinspect--index-tokens
             (phpinspect-test-read-fixture-data "IndexClass1")))))
         (index2
          (phpinspect-test-read-fixture-serialization "IndexClass1-indexed"))
         (index1-class (car (alist-get 'classes index1)))
         (index2-class (car (alist-get 'classes index2))))

    (dolist (key '(class-name imports methods static-methods static-variables variables constants extends implements))
      (should (equal (alist-get key index1-class)
                     (alist-get key index2-class))))))

(ert-deftest phpinspect-index-bmap-class ()
  (let* ((pctx (phpinspect-make-pctx :incremental t :bmap (phpinspect-make-bmap)))
         (tree))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "IndexClass1.php" phpinspect-test-php-file-directory))
      (setf (phpinspect-pctx-bmap pctx) (phpinspect-make-bmap))
      (phpinspect-with-parse-context pctx (setq tree (phpinspect-parse-current-buffer))))
    (let* ((index1
            (eval
             (phpinspect--serialize-root-index
              (phpinspect--index-tokens tree
                                        nil
                                        (phpinspect-bmap-make-location-resolver
                                         (phpinspect-pctx-bmap pctx))))))
           (index2
            (phpinspect-test-read-fixture-serialization "IndexClass1-indexed"))
           (index1-class (car (alist-get 'classes index1)))
           (index2-class (car (alist-get 'classes index2))))

      (dolist (key '(imports methods static-methods static-variables variables constants extends implements))
        (should (equal (alist-get key index1-class)
                       (alist-get key index2-class))))

      (should (alist-get 'location index1-class))
      (should (alist-get 'location index1-class)))))

(ert-deftest phpinspect-index-functions ()
  (let* ((code "<?php
use Example\\Thing;

function test_func(): array {}

/** @param \\array $bing */
function example($bing): Thing {}")
         (tokens (phpinspect-parse-string code))
         (index (phpinspect--index-tokens tokens))
         functions)

    (should (setq functions (alist-get 'functions index)))
    (should (= 2 (length functions)))
    (should (string= "test_func" (phpi-fn-name (cadr functions))))
    (should (string= "example" (phpi-fn-name (car functions))))

    (let ((example (car functions)))
      (should (= 1 (length (phpi-fn-arguments example))))
      (should (phpinspect--type=
               (phpinspect--make-type :name "\\array")
               (phpi-fn-argument-type example "bing"))))

    (should (phpinspect--type= (phpinspect--make-type :name "\\array")
                               (phpi-fn-return-type (cadr functions))))
    (should (phpinspect--type= (phpinspect--make-type :name "\\Example\\Thing")
                               (phpi-fn-return-type (car functions))))))

(ert-deftest phpinspect-index-functions-in-namespace ()
  (let* ((code "<?php
namespace Local;

use Example\\Thing;

function test_func(): array {}

function example(Firewall $wall): Thing {}")
         (tokens (phpinspect-parse-string code))
         (index (phpinspect--index-tokens tokens))
         (namespace (alist-get "Local" (alist-get 'namespaces index) nil nil #'string=))
         functions)

    (should namespace)

    (should (setq functions (alist-get 'functions index)))
    (should (= 2 (length functions)))
    (should (string= "Local\\test_func" (phpi-fn-name (cadr functions))))
    (should (string= "Local\\example" (phpi-fn-name (car functions))))

    (should (phpinspect--type= (phpinspect--make-type :name "\\array")
                               (phpi-fn-return-type (cadr functions))))
    (should (phpinspect--type= (phpinspect--make-type :name "\\Example\\Thing")
                               (phpi-fn-return-type (car functions))))
    (should (alist-get 'used-types namespace))
    (should (= 3 (length (alist-get 'used-types namespace))))
    (should (member (phpinspect-intern-name "Firewall") (alist-get 'used-types namespace)))
    (should (member (phpinspect-intern-name "array") (alist-get 'used-types namespace)))
    (should (member (phpinspect-intern-name "Thing") (alist-get 'used-types namespace)))))

(ert-deftest phpinspect-index-typehinted-variables ()
  (with-temp-buffer
    (let ((project (phpinspect--make-project :autoload (phpinspect-make-autoloader))))
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
}")
      (phpinspect-project-add-index project (phpinspect-index-current-buffer))

      (let ((class (phpinspect-project-get-typedef
                   project
                    (phpinspect--make-type
                     :name "\\App\\Controller\\Api\\V1\\AccountStatisticsController"
                     :fully-qualified t))))
        (should class)

        (let ((model (phpi-typedef-get-property class "model"))
              (priv-model (phpi-typedef-get-property class "privModel"))
              ;; Static variables are stored with "$" prefix
              (relation (phpi-typedef-get-property class "$relation"))
              (static-relation (phpi-typedef-get-property class "$staticRelation")))
          (should model)
          (should priv-model)
          (should relation)
          (should static-relation)

          (let ((model-type (phpinspect--make-type
                             :name "\\Illuminate\\Database\\Eloquent\\Model"
                             :fully-qualified t))
                (relation-type (phpinspect--make-type
                             :name "\\Illuminate\\Database\\Eloquent\\Relations\\Relation"
                             :fully-qualified t)))

            (should (phpi-var-type model))
            (should (phpinspect--type= model-type (phpi-var-type model)))
            (should (phpi-var-type priv-model))
            (should (phpinspect--type= model-type (phpi-var-type priv-model)))
            (should (phpi-var-type relation))
            (should (phpinspect--type= relation-type (phpi-var-type relation)))
            (should (phpi-var-type static-relation))
            (should (phpinspect--type= relation-type (phpi-var-type static-relation)))))))))


(ert-deftest phpinspect-index-return-type-annotation-for-method ()
  (with-temp-buffer
    (let ((project (phpinspect--make-project :autoload (phpinspect-make-autoloader))))
      (insert "<?php
declare(strict_types=1);

namespace App\\Controller\\Api\\V1;

class AccountStatisticsController {
/**
 * @return $this
 */
public function doStuff()
{
}
}")
      (phpinspect-project-add-index project (phpinspect-index-current-buffer))

      (let* ((type (phpinspect--make-type
                     :name "\\App\\Controller\\Api\\V1\\AccountStatisticsController"
                     :fully-qualified t))
             (class (phpinspect-project-get-typedef project type)))

        (should class)

        (let ((method (phpi-typedef-get-method class "doStuff")))
          (should method)
          (should (phpi-method-return-type method))
          (should (phpinspect--type= type (phpi-method-return-type method))))))))

(ert-deftest phpinspect-index-nested-functions ()
  (with-temp-buffer
    (let* ((code "<php

if (true) {
    function conditional() {
    }
}

if (something()) {
    if (other()) {
        function nestedConditional() {
        }
    }
}")
           (index (phpinspect--index-tokens (phpinspect-parse-string code)))
           (functions (alist-get 'functions index)))

      (should functions)
      (should (= 2 (length functions)))

      (let ((nestedConditional (car functions))
            (conditional (cadr functions)))
        (should (string= "conditional" (phpi-fn-name conditional)))
        (should (string= "nestedConditional" (phpi-fn-name nestedConditional)))))))

(ert-deftest phpinspect-index-trait-use ()
  (let* ((tree (with-temp-buffer
                 (insert "B, C { C::foo insteadof B, B::bar as banana }")
                 (goto-char (point-min))
                 (phpinspect--parse-use (current-buffer) (point-max))))
         (expected `((,(phpinspect--make-type :name "\\C" :fully-qualified t))
                     (,(phpinspect--make-type :name "\\B" :fully-qualified t)
                      (alias "bar" "banana")
                      (override "foo" ,(phpinspect--make-type :name "\\C" :fully-qualified t)))))
         (index (phpinspect--index-trait-use tree (phpinspect--make-type-resolver nil nil nil) nil)))

    (should index)
    (should (equal expected index))))
