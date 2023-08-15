;;; phpinspect-token-predicates.el --- Predicates for phpinspect-parser tokens types  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 0

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

;;; Code:


(define-inline phpinspect-token-type-p (object type)
  "Returns t if OBJECT is a token of type TYPE.
Type can be any of the token types returned by
`phpinspect-parse-buffer-until-point`"
  (inline-letevals (object)
    (inline-quote
     (and (listp ,object) (eq (car ,object) ,type)))))

(defsubst phpinspect-object-attrib-p (token)
  (phpinspect-token-type-p token :object-attrib))

(defsubst phpinspect-static-attrib-p (token)
  (phpinspect-token-type-p token :static-attrib))

(defsubst phpinspect-attrib-p (token)
  (or (phpinspect-object-attrib-p token)
      (phpinspect-static-attrib-p token)))

(defun phpinspect-html-p (token)
  (phpinspect-token-type-p token :html))

(defun phpinspect-comma-p (token)
  (phpinspect-token-type-p token :comma))

(defsubst phpinspect-terminator-p (token)
  (phpinspect-token-type-p token :terminator))

(defsubst phpinspect-end-of-token-p (token)
  (or (phpinspect-terminator-p token)
      (phpinspect-comma-p token)
      (phpinspect-html-p token)))

(defsubst phpinspect-incomplete-block-p (token)
  (phpinspect-token-type-p token :incomplete-block))

(defsubst phpinspect-block-p (token)
  (or (phpinspect-token-type-p token :block)
      (phpinspect-incomplete-block-p token)))

(defsubst phpinspect-end-of-statement-p (token)
  (or (phpinspect-end-of-token-p token)
      (phpinspect-block-p token)))

(defun phpinspect-end-of-use-p (token)
  (or (phpinspect-block-p token)
      (phpinspect-end-of-token-p token)))

(defun phpinspect-static-p (token)
  (phpinspect-token-type-p token :static))

(defsubst phpinspect-incomplete-const-p (token)
  (phpinspect-token-type-p token :incomplete-const))

(defsubst phpinspect-const-p (token)
  (or (phpinspect-token-type-p token :const)
      (phpinspect-incomplete-const-p token)))

(define-inline phpinspect-scope-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (phpinspect-token-type-p ,token :public)
         (phpinspect-token-type-p ,token :private)
         (phpinspect-token-type-p ,token :protected)))))

(define-inline phpinspect-namespace-p (object)
  (inline-quote
   (phpinspect-token-type-p ,object :namespace)))

(defun phpinspect-incomplete-class-p (token)
  (and (phpinspect-class-p token)
       (phpinspect-incomplete-block-p (car (last token)))))

(defun phpinspect-incomplete-namespace-p (token)
  (and (phpinspect-namespace-p token)
       (or (phpinspect-incomplete-block-p (car (last token)))
           (phpinspect-incomplete-class-p (car (last token))))))

(define-inline phpinspect-function-p (token)
  (inline-quote (phpinspect-token-type-p ,token :function)))

(define-inline phpinspect-class-p (token)
  (inline-quote (phpinspect-token-type-p ,token :class)))

(defun phpinspect-incomplete-method-p (token)
  (or (phpinspect-incomplete-function-p token)
      (and (phpinspect-scope-p token)
           (phpinspect-incomplete-function-p (car (last token))))
      (and (phpinspect-scope-p token)
           (phpinspect-static-p (car (last token)))
           (phpinspect-incomplete-function-p (car (last (car (last token))))))
      (and (phpinspect-scope-p token)
           (phpinspect-function-p (car (last token))))))

(defun phpinspect-incomplete-function-p (token)
  (and (phpinspect-function-p token)
       (phpinspect-incomplete-block-p (car (last token)))))

(defsubst phpinspect-incomplete-list-p (token)
  (phpinspect-token-type-p token :incomplete-list))

(defsubst phpinspect-list-p (token)
  (or (phpinspect-token-type-p token :list)
      (phpinspect-incomplete-list-p token)))

(define-inline phpinspect-declaration-p (token)
  (inline-quote
   (phpinspect-token-type-p ,token :declaration)))

(defsubst phpinspect-assignment-p (token)
  (phpinspect-token-type-p token :assignment))

(defun phpinspect-function-argument-list (php-func)
  "Get the argument list of a function"
  (seq-find #'phpinspect-list-p (seq-find #'phpinspect-declaration-p php-func nil) nil))

(defun phpinspect-annotation-p (token)
  (phpinspect-token-type-p token :annotation))

(defun phpinspect-method-annotation-p (token)
  (phpinspect-token-type-p token :method-annotation))

(defun phpinspect-var-annotation-p (token)
  (phpinspect-token-type-p token :var-annotation))

(defun phpinspect-return-annotation-p (token)
  (phpinspect-token-type-p token :return-annotation))

(define-inline phpinspect-class-variable-p (token)
  (inline-quote (phpinspect-token-type-p ,token :class-variable)))

(define-inline phpinspect-variable-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (phpinspect-token-type-p ,token :variable)
         (phpinspect-token-type-p ,token :class-variable)))))

(defsubst phpinspect-word-p (token)
  (phpinspect-token-type-p token :word))

(defsubst phpinspect-incomplete-array-p (token)
  (phpinspect-token-type-p token :incomplete-array))

(defsubst phpinspect-array-p (token)
  (or (phpinspect-token-type-p token :array)
      (phpinspect-incomplete-array-p token)))

(defsubst phpinspect-root-p (object)
  (phpinspect-token-type-p object :root))

(defun phpinspect-incomplete-token-p (token)
  (or (phpinspect-incomplete-root-p token)
      (phpinspect-incomplete-class-p token)
      (phpinspect-incomplete-block-p token)
      (phpinspect-incomplete-list-p token)
      (phpinspect-incomplete-array-p token)
      (phpinspect-incomplete-const-p token)
      (phpinspect-incomplete-function-p token)
      (phpinspect-incomplete-method-p token)
      (phpinspect-incomplete-namespace-p token)))

(defun phpinspect-incomplete-root-p (token)
  (and (phpinspect-root-p token)
       (seq-find #'phpinspect-incomplete-token-p (cdr token))))

(defun phpinspect--static-terminator-p (token)
  (or (phpinspect-function-p token)
      (phpinspect-end-of-token-p token)))

(defun phpinspect--scope-terminator-p (token)
  (or (phpinspect-function-p token)
      (phpinspect-end-of-token-p token)
      (phpinspect-const-p token)
      (phpinspect-static-p token)))

(defsubst phpinspect-enclosing-token-p (token)
  "Returns t when a token can enclose other tokens"
  (or
   (phpinspect-list-p token)
   (phpinspect-block-p token)
   (phpinspect-class-p token)
   (phpinspect-function-p token)
   (phpinspect-array-p token)
   (phpinspect-scope-p token)
   (phpinspect-static-p token)
   (phpinspect-const-p token)))

(defun phpinspect-namespace-keyword-p (token)
  (and (phpinspect-word-p token) (string= (car (last token)) "namespace")))

(defun phpinspect-use-keyword-p (token)
  (and (phpinspect-word-p token) (string= (car (last token)) "use")))


(defsubst phpinspect-namespace-or-root-p (object)
  (or (phpinspect-namespace-p object)
      (phpinspect-root-p object)))

(define-inline phpinspect-use-p (object)
  (inline-quote (phpinspect-token-type-p ,object :use)))

(defun phpinspect-comment-p (token)
  (or (phpinspect-token-type-p token :comment)
      (phpinspect-token-type-p token :doc-block)))

(defsubst phpinspect-class-block (class)
  (caddr class))

(define-inline phpinspect-namespace-is-blocked-p (namespace)
  (inline-letevals (namespace)
    (inline-quote
     (and (= (length ,namespace) 3) (phpinspect-block-p (caddr ,namespace))))))

(defsubst phpinspect-namespace-block (namespace)
  (when (phpinspect-namespace-is-blocked-p namespace)
    (caddr namespace)))

(defsubst phpinspect-function-block (php-func)
  (caddr php-func))

(defsubst phpinspect-not-class-p (token)
  "Apply inverse of `phpinspect-class-p' to TOKEN."
  (not (phpinspect-class-p token)))

(defsubst phpinspect-probably-token-p (token)
  (and (listp token)
       (keywordp (car token))))

(provide 'phpinspect-token-predicates)
