;;; phpinspect-token-predicates.el --- Predicates for phpinspect-parser tokens types  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 2.1.0

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
     (and (eq (car-safe ,object) ,type)))))

(define-inline phpinspect-object-attrib-p (token)
  (inline-quote
   (phpinspect-token-type-p ,token :object-attrib)))

(defun phpinspect-string-concatenator-p (token)
  (phpinspect-token-type-p token :string-concatenator))

(define-inline phpinspect-static-attrib-p (token)
  (inline-quote
   (phpinspect-token-type-p ,token :static-attrib)))

(define-inline phpinspect-attrib-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (phpinspect-object-attrib-p ,token)
	 (phpinspect-static-attrib-p ,token)))))

(defun phpinspect-html-p (token)
  (phpinspect-token-type-p token :html))

(defun phpinspect-comma-p (token)
  (phpinspect-token-type-p token :comma))

(defun phpinspect-not-comma-p (token)
  (not (phpinspect-comma-p token)))

(define-inline phpinspect-terminator-p (token)
  (inline-quote
   (phpinspect-token-type-p ,token :terminator)))

(define-inline phpinspect-end-of-token-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (phpinspect-terminator-p ,token)
	 (phpinspect-comma-p ,token)
	 (phpinspect-html-p ,token)))))

(define-inline phpinspect-incomplete-block-p (token)
  (inline-quote
   (phpinspect-token-type-p ,token :incomplete-block)))

(defsubst phpinspect-block-p (token)
  (or (phpinspect-token-type-p token :block)
      (phpinspect-incomplete-block-p token)))

(defsubst phpinspect-end-of-statement-p (token)
  (or (phpinspect-end-of-token-p token)
      (phpinspect-block-p token)))

(defun phpinspect-end-of-use-p (token)
  (or (phpinspect-block-p token)
      (phpinspect-terminator-p token)))

(define-inline phpinspect-static-p (token)
  (inline-quote
   (phpinspect-token-type-p ,token :static)))

(define-inline phpinspect-incomplete-const-p (token)
  (inline-quote
   (phpinspect-token-type-p ,token :incomplete-const)))

(define-inline phpinspect-const-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (phpinspect-token-type-p ,token :const)
	 (phpinspect-incomplete-const-p ,token)))))

(define-inline phpinspect-public-p (token)
  (inline-quote (phpinspect-token-type-p ,token :public)))

(define-inline phpinspect-protected-p (token)
  (inline-quote (phpinspect-token-type-p ,token :protected)))

(define-inline phpinspect-scope-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (phpinspect-token-type-p ,token :public)
         (phpinspect-token-type-p ,token :private)
         (phpinspect-token-type-p ,token :protected)))))

(define-inline phpinspect-namespace-p (object)
  (inline-quote
   (phpinspect-token-type-p ,object :namespace)))

(define-inline phpinspect-incomplete-class-p (token)
  (inline-letevals (token)
    (inline-quote
     (and (phpinspect-class-p ,token)
	  (phpinspect-incomplete-block-p (car (last ,token)))))))

(define-inline phpinspect-incomplete-namespace-p (token)
  (inline-letevals (token)
    (inline-quote
     (and (phpinspect-namespace-p ,token)
	  (or (phpinspect-incomplete-block-p (car (last ,token)))
              (phpinspect-incomplete-class-p (car (last ,token))))))))

(define-inline phpinspect-function-p (token)
  (inline-quote (phpinspect-token-type-p ,token :function)))

(define-inline phpinspect-class-p (token)
  (inline-quote (phpinspect-token-type-p ,token :class)))

(defun phpinspect-not-list-p (token)
  (not (phpinspect-list-p token)))

(define-inline phpinspect-incomplete-list-p (token)
  (inline-quote
   (phpinspect-token-type-p ,token :incomplete-list)))

(define-inline phpinspect-list-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (phpinspect-token-type-p ,token :list)
	 (phpinspect-incomplete-list-p ,token)))))

(defsubst phpinspect-block-or-list-p (token)
  (or (phpinspect-block-p token)
      (phpinspect-list-p token)))

(define-inline phpinspect-declaration-p (token)
  (inline-quote
   (phpinspect-token-type-p ,token :declaration)))

(defsubst phpinspect-assignment-p (token)
  (phpinspect-token-type-p token :assignment))

(defun phpinspect-function-argument-list (php-func)
  "Get the argument list of a function"
  (seq-find #'phpinspect-list-p (seq-find #'phpinspect-declaration-p php-func nil) nil))

(defun phpinspect-equals-p (token)
  (phpinspect-token-type-p token :equals))

(defun phpinspect-annotation-p (token)
  (phpinspect-token-type-p token :annotation))

(defun phpinspect-method-annotation-p (token)
  (phpinspect-token-type-p token :method-annotation))

(defun phpinspect-var-annotation-p (token)
  (phpinspect-token-type-p token :var-annotation))

(defun phpinspect-param-annotation-p (token)
  (phpinspect-token-type-p token :param-annotation))

(defun phpinspect-throws-annotation-p (token)
  (phpinspect-token-type-p token :throws-annotation))

(defun phpinspect-return-annotation-p (token)
  (phpinspect-token-type-p token :return-annotation))

(define-inline phpinspect-class-variable-p (token)
  (inline-quote (phpinspect-token-type-p ,token :class-variable)))

(define-inline phpinspect-variable-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (phpinspect-token-type-p ,token :variable)
         (phpinspect-token-type-p ,token :class-variable)))))

(define-inline phpinspect-word-p (token)
  (inline-quote
   (phpinspect-token-type-p ,token :word)))

(define-inline phpinspect-incomplete-array-p (token)
  (inline-quote
   (phpinspect-token-type-p ,token :incomplete-array)))

(define-inline phpinspect-array-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (phpinspect-token-type-p ,token :array)
	 (phpinspect-incomplete-array-p ,token)))))

(define-inline phpinspect-root-p (object)
  (inline-quote
   (phpinspect-token-type-p ,object :root)))

(define-inline  phpinspect-keyword-body-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (phpinspect-scope-p ,token)
	     (phpinspect-static-p ,token)
         (phpinspect-declaration-p ,token)
	     (phpinspect-function-p ,token)
	     (phpinspect-const-p ,token)))))

(define-inline phpinspect--incomplete-generic-keyword-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (length< ,token 2)
	 (phpinspect-incomplete-token-p (car (last ,token)))
	 (not (phpinspect-end-of-statement-p (car (last ,token))))))))

(define-inline phpinspect-incomplete-keyword-body-p (token)
  (inline-letevals (token)
    (inline-quote
     (and (phpinspect-keyword-body-p ,token)
	  (if (phpinspect-function-p ,token)
	      (phpinspect-incomplete-function-p ,token)
	    (phpinspect--incomplete-generic-keyword-p ,token))))))

(define-inline phpinspect-fat-arrow-p (token)
  (inline-quote
   (phpinspect-token-type-p ,token :fat-arrow)))

(define-inline phpinspect-atom-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (phpinspect-word-p ,token)
	 (phpinspect-terminator-p ,token)
	 (phpinspect-variable-p ,token)
	 (phpinspect-comma-p ,token)
	 (phpinspect-fat-arrow-p ,token)
	 (phpinspect-attrib-p ,token)))))

(define-inline phpinspect-incomplete-root-p (token)
  (inline-letevals (token)
    (inline-quote
     (and (phpinspect-root-p ,token)
	  (seq-find #'phpinspect-incomplete-token-p (cdr ,token))))))

;; Note: `phpinspect-incomplete-token-p' is used in the taint iterator during
;; incremental parses to determine whether a token should be reparsed or not. It
;; should not perform very complicated logic because it can be called a lot of
;; times per parse.
(defun phpinspect-incomplete-token-p (token)
  (unless (phpinspect-atom-p token)
    (or (phpinspect-incomplete-root-p token)
	(phpinspect-incomplete-class-p token)
	(phpinspect-incomplete-block-p token)
	(phpinspect-incomplete-list-p token)
	(phpinspect-incomplete-array-p token)
	(phpinspect-incomplete-const-p token)
	(phpinspect-incomplete-namespace-p token)
	(phpinspect-incomplete-keyword-body-p token))))

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

(define-inline phpinspect-use-import-p (token)
  (inline-quote (phpinspect-token-type-p ,token :use)))

(define-inline phpinspect-use-trait-p (token)
  (inline-quote (phpinspect-token-type-p ,token :use-trait)))

(define-inline phpinspect-use-p (object)
  (inline-quote
   (or (phpinspect-use-import-p ,object)
       (phpinspect-use-trait-p ,object))))

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

(define-inline phpinspect-not-comment-p (token)
  (inline-quote
   (not (phpinspect-comment-p ,token))))

(defun phpinspect-incomplete-function-p (token)
  (when (phpinspect-function-p token)
    (let (declaration terminator arg-list name block function-keyword)
      (dolist (component token)
	(cond ((phpinspect-declaration-p component)
	       (setq declaration component)
	       (dolist (subcomp component)
		 (cond ((and (not arg-list) (phpinspect-list-p subcomp))
			(setq arg-list subcomp))
		       ((and (not arg-list) (phpinspect-word-p subcomp))
			;; First word encountered will be a function
			;; keyword. After that comes the function name.
			(if (and (not function-keyword) (string= "function" (cadr subcomp)))
			    (setq function-keyword subcomp)
			  (setq name subcomp)))
		       ((and arg-list (phpinspect-terminator-p subcomp))
			(setq terminator subcomp)))))
	      ((phpinspect-block-p component)
	       (setq block component))))

      (or
       ;; Incomplete block = incomplete function
       (phpinspect-incomplete-block-p block)
       ;; No declaration, arg-list or name = incomplete function
       (not (and declaration arg-list name))
       ;; terminator = abstract function, which is complete. No block and no
       ;; terminator = incomplete.
       (not (or block terminator))))))

(provide 'phpinspect-token-predicates)
