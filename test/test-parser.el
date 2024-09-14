;; test-parser.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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

(require 'phpinspect-parser)
(require 'phpinspect-index)
(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))


(ert-deftest phpinspect-parse-bmap ()
  (let* ((ctx (phpinspect-make-pctx :incremental t :bmap (phpinspect-make-bmap)))
         (code "
class TestClass {
    public function getCurrentStatisticAction(): JsonResponse
    {
        $statistic = $this->repository->getCurrentStatistic();
        if (!$this->authorization->isGranted(EntityAction::VIEW, $statistic)) {
            return $this->responder->respondUnauthorized();
        }

        return $this->responder->respond($statistic);
    }
}")
         (bmap))
    (phpinspect-with-parse-context ctx
      (phpinspect-parse-string code))
    (setq bmap (phpinspect-pctx-bmap ctx))

    (let ((enclosing (phpinspect-bmap-tokens-overlapping bmap 350))
          (parent))
      (should enclosing)
      (should (phpinspect-variable-p (phpinspect-meta-token (car enclosing))))
      (should (string= "statistic" (cadr (phpinspect-meta-token (car enclosing)))))
      (should (phpinspect-meta-parent (car enclosing)))

      (setq parent (phpinspect-meta-parent (car enclosing)))
      (should (phpinspect-list-p (phpinspect-meta-token parent)))
      (should (phpinspect-block-p (phpinspect-meta-token (phpinspect-meta-parent parent)))))))

(ert-deftest phpinspect-parse-comma ()
  (let* ((code "(,)")
         (ctx (phpinspect-make-pctx :incremental t :bmap (phpinspect-make-bmap)))
         (parsed (phpinspect-with-parse-context ctx
                   (phpinspect-parse-string code)))
         (comma (cadadr parsed))
         (map (phpinspect-pctx-bmap ctx))
         (comma-meta))
    (should (equal '(:root (:list (:comma ","))) parsed))
    (should (equal '(:comma ",") comma))

    (should (setq comma-meta (phpinspect-bmap-token-meta map comma)))
    (should (= 1 (phpinspect-meta-width comma-meta)))))


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

(ert-deftest phpinspect-parse-class-with-trait-uses ()
  (let ((tree (phpinspect-parse-string "class A { use B, C { C::foo insteadof A } }")))

    (should tree)
    (should (equal
             '(:root (:class (:declaration (:word "class") (:word "A"))
                             (:block
                              (:use-trait (:word "B") (:comma ",") (:word "C")
                                          (:block
                                           (:word "C") (:static-attrib (:word "foo"))
                                           (:word "insteadof") (:word "A"))))))
             tree))))

(ert-deftest phpinspect-parse-interface-methods ()
  (let ((result (phpinspect-parse-string "
interface Test
{
    public function __call($method, $parameters);

    public static function __callStatic($method, $parameters);
}"))
        (expected '(:root
                    (:class
                     (:declaration
                      (:word "interface")
                      (:word "Test"))
                     (:block
                      (:public
                       (:function
                        (:declaration
                         (:word "function")
                         (:word "__call")
                         (:list
                          (:variable "method")
                          (:comma ",")
                          (:variable "parameters"))
                         (:terminator ";"))))
                      (:public
                       (:static
                        (:function
                         (:declaration
                          (:word "function")
                          (:word "__callStatic")
                          (:list
	                       (:variable "method")
	                       (:comma ",")
	                       (:variable "parameters"))
                          (:terminator ";"))))))))))

    (should (equal expected result))))

(ert-deftest phpinspect-parse-incomplete-comments ()
  (dolist (code (list "//" "/*" "// " "/* "))

    (should (phpinspect-parse-string code))))
