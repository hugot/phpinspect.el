;; test-class.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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
(require 'phpinspect-class)
(require 'phpinspect-project)
(require 'subr-x)
(require 'phpinspect-worker)

(phpinspect-ensure-worker)

(ert-deftest phpinspect--merge-method-return-type ()
  (let* ((class-name (phpinspect--make-type :name "\\Something"))
         (method1 (phpinspect--make-function
                  :name "fun"
                  :return-type (phpinspect--make-type :name "\\array")))
         (method2 (phpinspect--make-function
                   :name "fun"
                   :return-type (phpinspect--make-type :name "\\bool")))
         (result (phpinspect--merge-method class-name method1 method2)))

    (should (phpinspect--type= (phpinspect--make-type :name "\\bool")
                               (phpinspect--function-return-type result)))
    (should (phpinspect--type= (phpinspect--make-type :name "\\bool")
                               (phpinspect--function-return-type method1)))))

(ert-deftest phpinspect-class-incorporate ()
  (let ((class (phpinspect--make-class-generated))
        (other-class (phpinspect--make-class-generated)))
    (phpinspect--class-set-index class `(phpinspect--indexed-class (class-name . ,(phpinspect--make-type :name "Class"))))
    (phpinspect--class-set-index other-class `(phpinspect--indexed-class (class-name . ,(phpinspect--make-type :name "OtherClass"))))
    (phpinspect--class-update-method
     class (phpinspect--make-function :name "test" :return-type phpinspect--null-type))

    (phpinspect--class-update-method
     other-class (phpinspect--make-function :name "other-test" :return-type phpinspect--null-type))

    (phpinspect--class-incorporate class other-class)

    (should (= 2 (length (hash-table-values (phpinspect--class-methods class)))))
    (should (= 1 (length (hash-table-values (phpinspect--class-subscriptions other-class)))))

    (phpinspect--class-set-index
     class
     `(phpinspect--indexed-class
       (complete . t)
       (class-name . ,(phpinspect--make-type :name "Class"))
       (methods . ,(list (phpinspect--make-function :name "test" :return-type phpinspect--null-type)
                         (phpinspect--make-function :name "foobar" :return-type phpinspect--null-type)))))

    (should (= 3 (length (hash-table-values (phpinspect--class-methods class)))))

    (phpinspect--class-incorporate class other-class)
    (should (= 3 (length (hash-table-values (phpinspect--class-methods class)))))

    (phpinspect--class-set-index
     class
     `(phpinspect--indexed-class
       (complete . t)
       (class-name . ,(phpinspect--make-type :name "Class"))
       (methods . ,(list (phpinspect--make-function :name "foobar" :return-type phpinspect--null-type)))))

    (should (= 2 (length (hash-table-values (phpinspect--class-methods class)))))
    (should (phpinspect--class-get-method class (phpinspect-intern-name "other-test")))
    (should (phpinspect--class-get-method class (phpinspect-intern-name "foobar")))

    (phpinspect--class-set-index
     class
     `(phpinspect--indexed-class
       (complete . t)
       (class-name . ,(phpinspect--make-type :name "Class"))
       (methods)))

    (should (= 1 (length (hash-table-values (phpinspect--class-methods class)))))
    (should (phpinspect--class-get-method class (phpinspect-intern-name "other-test")))

    (phpinspect--class-incorporate class other-class)
    (should (= 1 (length (hash-table-values (phpinspect--class-methods class)))))
    (should (= 1 (length (hash-table-values (phpinspect--class-subscriptions other-class)))))))

(ert-deftest phpinspect--class-update-declaration ()
  (let ((class (phpinspect--make-class-generated :project (phpinspect--make-project))))
    (phpinspect--class-update-declaration class '(:declaration (:word "class") (:word "TestClass")
                                                               (:word "extends") (:word "OtherClass")
                                                               (:word "implements") (:word "ImplClass"))
                                          nil "NS")
    (should (= 2 (length (phpinspect--class-extended-classes class))))
    (should (phpinspect--type= (phpinspect--make-type :name "\\NS\\TestClass" :fully-qualified t)
                               (phpinspect--class-name class)))))
