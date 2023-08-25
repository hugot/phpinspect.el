; test-cache.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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
(require 'phpinspect-cache)

(ert-deftest phpinspect-cache-insert-type ()
  (let ((cache (phpinspect-make-cache))
        result)
    (phpinspect-cache-transact cache '((label test))
      :insert (phpinspect--make-type :name "\\TestClass") :as 'class)

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :get (phpinspect--make-type :name "\\TestClass") :as 'class))

    (should result)
    (should (listp result))
    (should (= 1 (length result)))
    (should (phpinspect-cache-type-p (car result)))

    (phpinspect-cache-transact cache '((label test))
      :insert (phpinspect--make-type :name "\\TestInterface") :as 'interface)

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :get (phpinspect--make-type :name "\\TestInterface") :as 'interface))

    (should result)
    (should (listp result))
    (should (= 1 (length result)))
    (should (phpinspect-cache-type-p (car result)))

    ;; When a query defines an entity category other than the one the existing
    ;; entity was inserted as, nothing should be returned.
    (setq result
          (phpinspect-cache-transact cache '((label test))
            :get (phpinspect--make-type :name "\\TestInterface") :as 'class))

    (should-not result)

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :get (phpinspect--make-type :name "\\TestInterface") :as 'type))

    (should result)

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :get `(,(phpinspect--make-type :name "\\TestInterface")
                   ,(phpinspect--make-type :name "\\TestClass"))
            :as 'type))
    (should result)
    (should (= 2 (length result)))
    (should (seq-every-p #'phpinspect-cache-type-p result))

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :get `(,(phpinspect--make-type :name "\\TestInterface")
                   ,(phpinspect--make-type :name "\\TestClass"))
            :as 'interface))
    (should result)
    (should (= 1 (length result)))
    (should (seq-every-p #'phpinspect-cache-type-p result))

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :get '* :as 'type))
    (should result)
    (should (= 2 (length result)))
    (should (seq-every-p #'phpinspect-cache-type-p result))

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :get (phpinspect--make-type :name "\\TestClass") :as 'type))

    (should result)

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :delete (phpinspect--make-type :name "\\TestClass") :as 'type))

    (should result)
    (should (phpinspect-cache-type-p (car result)))

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :get (phpinspect--make-type :name "\\TestClass") :as 'type))
    (should-not result)

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :delete (phpinspect--make-type :name "\\TestClass") :as 'type))
    (should-not result)

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :get (phpinspect--make-type :name "\\TestInterface") :as 'type))
    (should result)

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :delete (phpinspect--make-type :name "\\TestInterface") :as 'class))
    (should-not result)

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :get (phpinspect--make-type :name "\\TestInterface") :as 'type))
    (should result)

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :delete (phpinspect--make-type :name "\\TestInterface") :as 'interface))
    (should result)

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :get (phpinspect--make-type :name "\\TestInterface") :as 'type))
    (should-not result)))

(ert-deftest phpinspect-cache-namespace-query ()
  (let ((cache (phpinspect-make-cache))
        result)
    (phpinspect-cache-transact cache '((label test))
      :insert (list (phpinspect--make-type :name "\\Namespace1\\TestClass")
                    (phpinspect--make-type :name "\\Namespace2\\TestClass")
                    (phpinspect--make-type :name "\\Namespace2\\TestClass1"))
      :as 'class)

    (setq result (phpinspect-cache-transact cache '((label test))
                   :get '* :as 'class :in (phpinspect-intern-name "\\Namespace1")))

    (should result)
    (should (= 1 (length result)))
    (should (eq (phpinspect-intern-name "\\Namespace1\\TestClass")
                (phpinspect-cache-type-name (car result))))

        (setq result (phpinspect-cache-transact cache '((label test))
                   :get '* :as 'class :in (phpinspect-intern-name "\\Namespace2")))

    (should result)
    (should (= 2 (length result)))))

(ert-deftest phpinspect-cache-delete-wildcard-types ()
  (let ((cache (phpinspect-make-cache))
        result)
    (phpinspect-cache-transact cache '((label test))
      :insert (list (phpinspect--make-type :name "\\Namespace1\\TestClass")
                    (phpinspect--make-type :name "\\Namespace2\\TestClass")
                    (phpinspect--make-type :name "\\Namespace2\\TestClass1"))
      :as 'class)


    (phpinspect-cache-transact cache '((label test))
      :delete '* :as 'class)

    (should-not (phpinspect-cache-transact cache '((label test))
                  :get '* :as 'class))))

(ert-deftest phpinspect-cache-delete-wildcard-namespace-types ()
  (let ((cache (phpinspect-make-cache))
        result)
    (phpinspect-cache-transact cache '((label test))
      :insert (list (phpinspect--make-type :name "\\Namespace1\\TestClass")
                    (phpinspect--make-type :name "\\Namespace2\\TestClass")
                    (phpinspect--make-type :name "\\Namespace2\\TestClass1"))
      :as 'class)


    (phpinspect-cache-transact cache '((label test))
      :delete '* :as 'class :in (phpinspect-intern-name "\\Namespace2"))

    (setq result (phpinspect-cache-transact cache '((label test)) :get '* :as 'class))
    (should result)
    (should (= 1 (length result)))
    (should (eq (phpinspect-intern-name "\\Namespace1\\TestClass")
                (phpinspect-cache-type-name (car result))))))

(ert-deftest phpinspect-cache-insert-function ()
  (let ((cache (phpinspect-make-cache))
        result)
    (setq result (phpinspect-cache-transact cache '((label test))
                   :insert (phpinspect--make-function :name "test_func")
                   :as 'function))

    (should result)
    (should (phpinspect--function-p (car result)))
    (should (= 1 (length result)))

    (setq result (phpinspect-cache-transact cache '((label test))
                   :insert (list (phpinspect--make-function :name "test_func")
                                 (phpinspect--make-function :name "other_func"))
                   :as 'function
                   :in (phpinspect-intern-name "\\Namespace1")))
    (should result)
    (should (= 2 (length result)))

    (setq result (phpinspect-cache-transact cache '((label test))
                   :get (phpinspect-intern-name "\\test_func")
                   :as 'function))

    (should result)
    (should (phpinspect--function-p (car result)))
    (should (= 1 (length result)))

    (setq result (phpinspect-cache-transact cache '((label test))
                   :delete (phpinspect-intern-name "\\test_func")
                   :as 'function))

    (should result)
    (should (phpinspect--function-p (car result)))
    (should (= 1 (length result)))

    (setq result (phpinspect-cache-transact cache '((label test))
                   :get '*
                   :as 'function
                   :in (phpinspect-intern-name "\\Namespace1")))
    (should result)
    (should (= 2 (length result)))

    (setq result (phpinspect-cache-transact cache '((label test))
                   :delete '*
                   :as 'function
                   :in (phpinspect-intern-name "\\Namespace1")))
    (should result)
    (should (= 2 (length result)))

    (phpinspect-cache-transact cache '((label test))
      :insert (list (phpinspect--make-function :name "\\Ns\\test_func")
                    (phpinspect--make-function :name "\\Ns\\other_func")
                    (phpinspect--make-function :name "\\root_func"))
      :as 'function)

    (setq result (phpinspect-cache-transact cache '((label test))
                   :get '* :as 'function :in (phpinspect-intern-name "\\Ns")))
    (should result)
    (should (= 2 (length result)))

    (setq result (phpinspect-cache-transact cache '((label test))
                   :get '* :as 'function :in (phpinspect-intern-name "\\")))
    (should result)
    (should (= 1 (length result)))

    (setq result (phpinspect-cache-transact cache '((label test))
                   :get '* :as 'function))
    (should result)
    (should (= 3 (length result)))

    (setq result (phpinspect-cache-transact cache '((label test))
                   :delete '* :as 'function))
    (should result)
    (should (= 3 (length result)))))

(ert-deftest phpinspect-insert-type-extending/implementing ()
  (let ((cache (phpinspect-make-cache))
        result)

    (setq result
          (phpinspect-cache-transact cache '((label test))
            :insert (phpinspect--make-type :name "\\Namespace1\\TestClass")
            :as 'class
            :extending (phpinspect--make-type :name "\\App\\TestClassAbstract")
            :implementing (phpinspect--make-type :name "\\App\\TestInterface")))

    (should result)
    (should (= 1 (length result)))

    (setq result (car result))

    (should (phpinspect-cache-type-get-implements result))
    (should (= 1 (length (phpinspect-cache-type-get-implements result))))
    (should (eq (phpinspect-intern-name "\\App\\TestInterface")
                (car (phpinspect-cache-type-get-implements result))))

    (setq result (phpinspect-cache-transact cache '((label test))
                   :get '*
                   :implementing (phpinspect-intern-name "\\App\\TestInterface")
                   :as 'type))

    (should result)
    (should (= 1 (length result)))
    (should (eq (phpinspect-intern-name "\\Namespace1\\TestClass")
                (phpinspect-cache-type-name (car result))))

    (setq result (phpinspect-cache-transact cache '((label test))
                   :get '*
                   :extending (phpinspect-intern-name "\\App\\TestClassAbstract")
                   :as 'type))

    (should result)
    (should (= 1 (length result)))
    (should (eq (phpinspect-intern-name "\\Namespace1\\TestClass")
                (phpinspect-cache-type-name (car result))))

    (setq result (phpinspect-cache-transact cache '((label test))
                   :get '*
                   :extending (phpinspect-intern-name "\\App\\TestClass")
                   :as 'type))

    (should-not result)))
