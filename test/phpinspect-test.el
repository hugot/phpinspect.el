;;; phpinspect-test.el --- Unit tests for phpinslect.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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
(require 'phpinspect)

;; Make sure that the worker is running. TODO: fully encapsulate the worker the
;; data types that are used in tests so that we don't depend on some global
;; worker object for tests.
(phpinspect-ensure-worker)

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

(ert-deftest phpinspect-parse-namespaced-class ()
  "Test phpinspect-parse on a namespaced class"
  (should
   (equal (phpinspect-test-read-fixture-data "NamespacedClass")
          (phpinspect-test-parse-fixture-code "NamespacedClass"))))

(ert-deftest phpinspect-parse-block ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "Block")
          (phpinspect-test-parse-fixture-code "Block"))))

(ert-deftest phpinspect-parse-functions ()
  "Test phpinspect-parse for php functions"
  (should
   (equal (phpinspect-test-read-fixture-data "Functions")
          (phpinspect-test-parse-fixture-code "Functions"))))

(ert-deftest phpinspect-parse-namespaced-functions ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "NamespacedFunctions")
          (phpinspect-test-parse-fixture-code "NamespacedFunctions"))))

(ert-deftest phpinspect-parse-variable ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "Variable")
          (phpinspect-test-parse-fixture-code "Variable"))))

(ert-deftest phpinspect-parse-word ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "Word")
          (phpinspect-test-parse-fixture-code "Word"))))

(ert-deftest phpinspect-parse-array ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "Array")
          (phpinspect-test-parse-fixture-code "Array"))))


(ert-deftest phpinspect-parse-short-function ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "ShortFunction")
          (phpinspect-test-parse-fixture-code "ShortFunction"))))

(ert-deftest phpinspect-parse-two-short-functions ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "TwoShortFunctions")
          (phpinspect-test-parse-fixture-code "TwoShortFunctions"))))

(ert-deftest phpinspect-parse-small-namespaced-class ()
  "Test phpinspect-parse for php blocks"
  (should
   (equal (phpinspect-test-read-fixture-data "SmallNamespacedClass")
          (phpinspect-test-parse-fixture-code "SmallNamespacedClass"))))

;; If this test fails, the syntax tree has a breaking change in it. Regenerate the
;; fixtures and fix anything that is broken.
(ert-deftest phpinspect-syntax-tree-change ()
  (let ((index (phpinspect--index-tokens
                (phpinspect-test-parse-fixture-code "IndexClass1")))
        (expected-result (phpinspect--index-tokens
                          (phpinspect-test-read-fixture-data "IndexClass1"))))
    (should (equal index expected-result))))

(ert-deftest phpinspect-index-tokens-class ()
  (let* ((index1
          (phpinspect--index-tokens
           (phpinspect-test-read-fixture-data "IndexClass1")))
         (index2
          (phpinspect-test-read-fixture-serialization "IndexClass1-indexed"))
         (index1-class (cdr (alist-get 'classes index1)))
         (index2-class (cdr (alist-get 'classes index2))))
    (dolist (key '(class-name imports methods static-methods static-variables variables constants extends implements))
      (should (equal (alist-get key index1-class)
                     (alist-get key index2-class))))))

(ert-deftest phpinspect-get-resolvecontext ()
  (let ((resolvecontext (phpinspect--get-resolvecontext
                         (phpinspect-test-read-fixture-data "IncompleteClass"))))

    (should (equal (phpinspect--resolvecontext-subject resolvecontext)
                   '((:variable "this")
                     (:object-attrib (:word "em"))
                     (:object-attrib nil))))

    (should (phpinspect-root-p
             (car (last (phpinspect--resolvecontext-enclosing-tokens
                         resolvecontext)))))

    (should (phpinspect-incomplete-list-p
             (car (phpinspect--resolvecontext-enclosing-tokens
                   resolvecontext))))

    (should (phpinspect-incomplete-function-p
             (cadr (phpinspect--resolvecontext-enclosing-tokens
                    resolvecontext))))

    (should (phpinspect-incomplete-class-p
             (cadddr (phpinspect--resolvecontext-enclosing-tokens
                      resolvecontext))))))

(ert-deftest phpinspect-type-resolver-for-resolvecontext ()
  (let* ((resolvecontext (phpinspect--get-resolvecontext
                         (phpinspect-test-read-fixture-data "IncompleteClass")))
         (type-resolver (phpinspect--make-type-resolver-for-resolvecontext
                         resolvecontext)))

    (should (phpinspect--type= (phpinspect--make-type :name  "\\array")
                               (funcall type-resolver
                                        (phpinspect--make-type :name "array"))))
    (should (phpinspect--type= (phpinspect--make-type :name  "\\array")
                               (funcall type-resolver
                                        (phpinspect--make-type :name "\\array"))))
    (should (phpinspect--type= (phpinspect--make-type
                                :name  "\\Symfony\\Component\\HttpFoundation\\Response")
                     (funcall type-resolver (phpinspect--make-type :name "Response"))))
    (should (phpinspect--type= (phpinspect--make-type :name  "\\Response")
                               (funcall type-resolver
                                        (phpinspect--make-type :name "\\Response"))))
    (should (phpinspect--type= (phpinspect--make-type :name  "\\App\\Controller\\GastonLagaffe")
                               (funcall type-resolver
                                        (phpinspect--make-type :name "GastonLagaffe"))))
    (should (phpinspect--type=
             (phpinspect--make-type :name  "\\App\\Controller\\Dupuis\\GastonLagaffe")
             (funcall type-resolver
                      (phpinspect--make-type :name "Dupuis\\GastonLagaffe"))))))

(ert-deftest phpinspect-type-resolver-for-resolvecontext-namespace-block ()
  (let* ((resolvecontext (phpinspect--get-resolvecontext
                          (phpinspect-test-read-fixture-data
                           "IncompleteClassBlockedNamespace")))
         (type-resolver (phpinspect--make-type-resolver-for-resolvecontext
                         resolvecontext)))

    (should (phpinspect--type= (phpinspect--make-type :name "\\array")
                               (funcall type-resolver (phpinspect--make-type :name "array"))))
    (should (phpinspect--type= (phpinspect--make-type :name  "\\array")
                               (funcall type-resolver (phpinspect--make-type :name "\\array"))))
    (should (phpinspect--type= (phpinspect--make-type
                                :name  "\\Symfony\\Component\\HttpFoundation\\Response")
                               (funcall type-resolver (phpinspect--make-type :name "Response"))))
    (should (phpinspect--type= (phpinspect--make-type :name  "\\Response")
                               (funcall type-resolver (phpinspect--make-type :name "\\Response"))))
    (should (phpinspect--type= (phpinspect--make-type :name  "\\App\\Controller\\GastonLagaffe")
                               (funcall type-resolver (phpinspect--make-type
                                                       :name "GastonLagaffe"))))
    (should (phpinspect--type= (phpinspect--make-type
                                :name  "\\App\\Controller\\Dupuis\\GastonLagaffe")
                     (funcall type-resolver (phpinspect--make-type :name "Dupuis\\GastonLagaffe"))))))

(ert-deftest phpinspect-type-resolver-for-resolvecontext-multiple-namespace-blocks ()
  (let* ((resolvecontext (phpinspect--get-resolvecontext
                          (phpinspect-test-read-fixture-data
                           "IncompleteClassMultipleNamespaces")))
         (type-resolver (phpinspect--make-type-resolver-for-resolvecontext
                         resolvecontext)))

    (should (phpinspect--type= (phpinspect--make-type :name "\\array")
                               (funcall type-resolver
                                        (phpinspect--make-type :name "array"))))
    (should (phpinspect--type= (phpinspect--make-type :name  "\\array")
                               (funcall type-resolver
                                        (phpinspect--make-type :name "\\array"))))
    (should (phpinspect--type= (phpinspect--make-type
                                :name  "\\Symfony\\Component\\HttpFoundation\\Response")
                     (funcall type-resolver (phpinspect--make-type :name "Response"))))
    (should (phpinspect--type= (phpinspect--make-type :name  "\\Response")
                               (funcall type-resolver
                                        (phpinspect--make-type :name "\\Response"))))
    (should (phpinspect--type= (phpinspect--make-type :name  "\\App\\Controller\\GastonLagaffe")
                     (funcall type-resolver (phpinspect--make-type :name "GastonLagaffe"))))
    (should (phpinspect--type= (phpinspect--make-type
                                :name  "\\App\\Controller\\Dupuis\\GastonLagaffe")
                               (funcall type-resolver (phpinspect--make-type
                                                       :name "Dupuis\\GastonLagaffe"))))))

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
         (index (phpinspect--index-tokens class-tokens))
         (expected-index
          `(phpinspect--root-index
            (imports)
            (classes
             (,(phpinspect--make-type :name"\\Potato" :fully-qualified t)
              phpinspect--indexed-class
              (class-name . ,(phpinspect--make-type :name "\\Potato" :fully-qualified t))
              (imports)
              (methods)
              (static-methods . (,(phpinspect--make-function
                                   :name "staticMethod"
                                   :scope '(:public)
                                   :arguments `(("untyped" nil)
                                                ("things" ,(phpinspect--make-type :name "\\array"
                                                                                  :fully-qualified t)))
                                   :return-type phpinspect--null-type)))
              (static-variables)
              (variables)
              (constants)
              (extends)
              (implements)))
            (functions))))
    ;; (pp expected-index)
    ;; (pp index))
    (should (equal expected-index index))))

(ert-deftest phpinspect-resolve-type-from-context ()
  (let* ((token-tree (phpinspect-parse-string "
namespace Amazing;

class FluffBall
{
    private $fluffer;

    public function __construct(Fluffer $fluffer)
    {
        $this->fluffer = $fluffer;
    }

    public function beFluffy(): void
    {
        $ball = $this->fluffer;

        if ($ball) {
            if(isset($ball->fluff()->poof->upFluff->"))
        (fluffer  (phpinspect-parse-string "
namespace Amazing;

use Vendor\\FluffLib\\Fluff;

class Fluffer
{
    public function fluff(): Fluff
    {
    }
}"))
        (vendor-fluff (phpinspect-parse-string "
namespace Vendor\\FluffLib;
class Fluff
{
    /**
     * @var FlufferUpper
     */
    public $poof;
}"))
        (vendor-fluffer-upper (phpinspect-parse-string "
namespace Vendor\\FluffLib;
class FlufferUpper
{
    public $upFluff;
    public function __construct(DoubleFluffer $upFluff)
    {
        $this->upFluff = $upFluff;
    }
}"))
        (phpinspect-project-root-function (lambda () "phpinspect-test"))
        (context (phpinspect--get-resolvecontext token-tree)))

    (setf (phpinspect--resolvecontext-project-root context)
          "phpinspect-test")

    (phpinspect-purge-cache)
    (dolist (tree (list token-tree fluffer vendor-fluff vendor-fluffer-upper))
      (phpinspect-cache-project-class
       "phpinspect-test"
       (cdar (alist-get 'classes (cdr (phpinspect--index-tokens tree))))))

    (should (phpinspect--type=
             (phpinspect--make-type :name "\\Vendor\\FluffLib\\DoubleFluffer")
             (phpinspect-resolve-type-from-context
              context
              (phpinspect--make-type-resolver-for-resolvecontext
               context))))))

(ert-deftest phpinspect-eldoc-function-for-object-method ()
  (let* ((php-code "
class Thing
{
    function getThis(\\DateTime $moment, Thing $thing, $other): static
    {
        return $this;
    }

    function doStuff()
    {
        $this->getThis(")
         (tokens (phpinspect-parse-string php-code))
         (index (phpinspect--index-tokens tokens))
         (phpinspect-project-root-function (lambda () "phpinspect-test"))
         (phpinspect-eldoc-word-width 100))
    (phpinspect-purge-cache)
    (phpinspect-cache-project-class
     (phpinspect-project-root)
     (cdar (alist-get 'classes (cdr index))))

    (should (string= "getThis: ($moment DateTime, $thing Thing, $other): Thing"
                   (with-temp-buffer
                     (insert php-code)
                     (phpinspect-eldoc-function))))))

(ert-deftest phpinspect-eldoc-function-for-static-method ()
  (let* ((php-code "
class Thing
{
    static function doThing(\\DateTime $moment, Thing $thing, $other): static
    {
        return $this;
    }

    function doStuff()
    {
        self::doThing(")
         (tokens (phpinspect-parse-string php-code))
         (index (phpinspect--index-tokens tokens))
         (phpinspect-project-root-function (lambda () "phpinspect-test"))
         (phpinspect-eldoc-word-width 100))
    (phpinspect-purge-cache)
    (phpinspect-cache-project-class
     (phpinspect-project-root)
     (cdar (alist-get 'classes (cdr index))))

    (should (string= "doThing: ($moment DateTime, $thing Thing, $other): Thing"
                   (with-temp-buffer
                     (insert php-code)
                     (phpinspect-eldoc-function))))))


(ert-deftest phpinspect-resolve-type-from-context-static-method ()
  (let* ((php-code "
class Thing
{
    static function doThing(\\DateTime $moment, Thing $thing, $other): static
    {
        return $this;
    }

    function doStuff()
    {
        self::doThing()->")
         (tokens (phpinspect-parse-string php-code))
         (index (phpinspect--index-tokens tokens))
         (phpinspect-project-root-function (lambda () "phpinspect-test"))
         (phpinspect-eldoc-word-width 100)
         (context (phpinspect--get-resolvecontext tokens)))
    (phpinspect-purge-cache)
    (phpinspect-cache-project-class
     (phpinspect-project-root)
     (cdar (alist-get 'classes (cdr index))))

    (should (phpinspect--type= (phpinspect--make-type :name "\\Thing")
                               (phpinspect-resolve-type-from-context
                                context
                                (phpinspect--make-type-resolver-for-resolvecontext
                                 context))))))

(ert-deftest phpinspect-resolve-type-from-context-static-method-with-preceding-words ()
  (let* ((php-code "
class Thing
{
    static function doThing(\\DateTime $moment, Thing $thing, $other): static
    {
        return $this;
    }

    function doStuff()
    {
        if (true) {
            return self::doThing()->")
         (tokens (phpinspect-parse-string php-code))
         (index (phpinspect--index-tokens tokens))
         (phpinspect-project-root-function (lambda () "phpinspect-test"))
         (phpinspect-eldoc-word-width 100)
         (context (phpinspect--get-resolvecontext tokens)))
    (phpinspect-purge-cache)
    (phpinspect-cache-project-class
     (phpinspect-project-root)
     (cdar (alist-get 'classes (cdr index))))

    (should (phpinspect--type= (phpinspect--make-type :name "\\Thing")
                     (phpinspect-resolve-type-from-context
                      context
                      (phpinspect--make-type-resolver-for-resolvecontext
                       context))))))

(ert-deftest phpinspect-get-last-statement-in-token-with-static-attribute-context ()
  (let* ((php-code-function "
    function doStuff()
    {
        return self::doThing()")
           (php-code-block "
    {
        return self::doThing()")
           (php-code-preceding-block "
    function doStuff()
    {
        if (true === true) {
            forach ($things as $k => $v) {
            }
        }
        self::doThing()")
           (php-code-bare "Thing::something(); Something::other()")
           (get-last-statement
            (lambda (php-code)
              (phpinspect--get-last-statement-in-token
               (car (cdr (phpinspect-parse-string php-code)))))))

      (should (equal `((:word "return") (:word "self") (:static-attrib (:word "doThing"))
                       (:list))
                     (funcall get-last-statement php-code-function)))
      (should (equal `((:word "return") (:word "self") (:static-attrib (:word "doThing"))
                       (:list))
                     (funcall get-last-statement php-code-block)))
      (should (equal `((:word "self") (:static-attrib (:word "doThing"))
                       (:list))
                     (funcall get-last-statement php-code-preceding-block)))
      (should (equal `((:word "Something") (:static-attrib (:word "other"))
                       (:list))
                     (phpinspect--get-last-statement-in-token
                      (phpinspect-parse-string php-code-bare))))))

(load-file (concat phpinspect-test-directory "/test-worker.el"))
(load-file (concat phpinspect-test-directory "/test-autoload.el"))

(provide 'phpinspect-test)
;;; phpinspect-test.el ends here
