;;; phpinspect-name.el --- Objects and functions for PHP names  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 3.0.1

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

;; A name can be any arbitrary string used to refer to any declarative entity in
;; PHP code. In phpinspect, a name could be the name of function, a type or a
;; namespace.
;;
;; Names are created using the `phpinspect-intern-name' function. Calling
;; `phpinspect-intern-name' twice with identical strings as arguments is
;; guaranteed to return objects which are `eq' to each other.
;;
;; Name objects are expected to be immutable. They should never be modified by
;; code external to this file after creation.

;;; Code:

(defun phpinspect-make-name-hash ()
  (make-hash-table :test #'equal :size 5000 :rehash-size 1.2))

(defvar phpinspect-names (phpinspect-make-name-hash)
  "An hash-table containing cons cells representing encountered names in
PHP code. Used to optimize string comparison. See also `phpinspect-intern-name'")

(defun phpinspect-intern-name (name-string)
  "Return a name object associated with NAME-STRING.

Calling this function multiple times with identical strings as
arguments is guaranteed to return objects which are `eq' to each
other."
  (or (gethash name-string phpinspect-names)
      (puthash name-string (list 'phpinspect-name name-string nil nil) phpinspect-names)))

(define-inline phpinspect-name--namespace (name)
  "A cache slot for namespace part of NAME."
  (inline-quote (cadddr ,name)))

(define-inline phpinspect-name--base (name)
  "A cache slot for unqualified part of NAME."
  (inline-quote (caddr ,name)))

(define-inline phpinspect-name-string (name)
  "Slot for the string that NAME is the interned form of."
  (inline-quote (cadr ,name)))

(define-inline phpinspect-name-p (name)
  "Returns true if NAME is an instance of `phpinspect-name'."
  (inline-quote (eq 'phpinspect-name (car ,name))))

(defun phpinspect-name-string-base (fqn)
  "Returns the unqualified part of FQN."
  (car (last (split-string fqn "\\\\"))))

(defun phpinspect-name-string-namespace (fqn)
  "Returns the namespace part of FQN."
  (string-trim-right fqn "\\\\?[^\\]+"))

(define-inline phpinspect-name-base (name)
  "Returns the unqualified part of NAME.

Return value is itself an instance of `phpinspect-name'."
  (inline-letevals (name)
    (inline-quote
     (with-memoization (phpinspect-name--base ,name)
       (thread-last (phpinspect-name-string ,name)
                    (phpinspect-name-string-base)
                    (phpinspect-intern-name))))))

(define-inline phpinspect-name-namespace (name)
  "Returns the namespace part of NAME.

Return value is itself an instance of `phpinspect-name'."
  (inline-letevals (name)
    (inline-quote
     (with-memoization (phpinspect-name--namespace ,name)
       (thread-last (phpinspect-name-string ,name)
                    (phpinspect-name-string-namespace)
                    (phpinspect-intern-name))))))

(define-inline phpinspect-name-non-fqn-string (name)
  (inline-quote
   (string-trim-left (phpinspect-name-string ,name) "\\\\")))

(defun phpinspect-names-to-alist (names)
  (let ((alist))
    (dolist (name names)
      (push (cons (phpinspect-name-string name) name) alist))
    alist))

(defsubst phpinspect--wrap-plist-name-in-symbol (property-list)
  (let ((new-plist)
        (wrap-value))
    (dolist (item property-list)
      (when wrap-value
        (setq item `(phpinspect-intern-name ,item))
        (setq wrap-value nil))
      (when (eq item :name)
        (setq wrap-value t))
      (push item new-plist))
    (nreverse new-plist)))

(provide 'phpinspect-name)
