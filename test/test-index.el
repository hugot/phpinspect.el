;;; test-index.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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
(require 'phpinspect-index)

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
              (location . (0 0))
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
              (implements)
              (used-types . (,(phpinspect-intern-name "array")))))
            (used-types)
            (functions))))
    (should (equal expected-index index))))

(ert-deftest phpinspect-index-used-types-in-class ()
  (let* ((result (phpinspect--index-tokens
                  (phpinspect-parse-string
                   "<?php namespace Field; class Potato extends Cheese, Bacon implements Ham, Bagel {
public function makeThing(): Thing
{
if ((new Monkey())->tree() === true) {
   return new ExtendedThing();
}
return StaticThing::create(new ThingFactory())->makeThing((((new Potato())->antiPotato(new OtherThing()))));
}")))
         (used-types (alist-get 'used-types (car (alist-get 'classes result)))))
    (should (equal
             (mapcar #'phpinspect-intern-name
                     (sort
                      '("Cheese" "Bacon" "Ham" "Bagel" "Monkey" "ExtendedThing"
                        "StaticThing" "Thing" "ThingFactory" "Potato" "OtherThing")
                      #'string<))
             (sort used-types (lambda (s1 s2) (string< (symbol-name s1) (symbol-name s2))))))))

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
        (should (equal (cadr set) result))))))

(ert-deftest phpinspect-index-method-annotations ()
  (let* ((result (phpinspect--index-tokens
                  (phpinspect-parse-string
                   "<?php

/* @method int peel(bool $fast, array $loose)
                           * @method Banana duplicate()
                            @method hold() **/
                           class Banana {}")))
         (class (car (alist-get 'classes result)))
         (methods (alist-get 'methods class)))
    (should (= 3 (length methods)))
    (dolist (method methods)
      (should (member (phpinspect--function-name method)
                      '("duplicate" "hold" "peel")))

      (cond ((string= (phpinspect--function-name method)
                      "duplicate")
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\Banana" :fully-qualified t)
                      (phpinspect--function-return-type method))))
            ((string= (phpinspect--function-name method)
                      "peel")
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\int" :fully-qualified t)
                      (phpinspect--function-return-type method)))

             (should (= 2 (length (phpinspect--function-arguments method))))
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\array" :fully-qualified t)
                      (car (alist-get
                            "loose" (phpinspect--function-arguments method) nil nil #'string=))))
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\bool" :fully-qualified t)
                      (car (alist-get
                            "fast" (phpinspect--function-arguments method) nil nil #'string=)))))
            ((string= (phpinspect--function-name method)
                      "hold")
             (should (phpinspect--type=
                      (phpinspect--make-type :name "\\void" :fully-qualified t)
                      (phpinspect--function-return-type method))))))))
