;;; phpinspect-type.el --- Data structures that represent phpinspect types  -*- lexical-binding: t; -*-

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

(require 'phpinspect-util)
(require 'phpinspect-token-predicates)

(eval-when-compile
  (require 'phpinspect-parser))


(cl-defstruct (phpinspect--type
               (:constructor phpinspect--make-type-generated)
               (:copier phpinspect--copy-type))
  "Represents an instance of a PHP type in the phpinspect syntax tree."
  (name-symbol nil
               :type symbol
               :documentation
               "Symbol representation of the type name.")
  (collection nil
              :type bool
              :documentation
              "Whether or not the type is a collection")
  (contains nil
            :type phpinspect--type
            :documentation
            "When the type is a collection, this attribute is set to the type
that the collection is expected to contain")
  (fully-qualified nil
                   :type bool
                   :documentation
                   "Whether or not the type name is fully qualified"))

(defmacro phpinspect--make-type (&rest property-list)
  `(phpinspect--make-type-generated
    ,@(phpinspect--wrap-plist-name-in-symbol property-list)))

(defun phpinspect--make-types (type-names)
  (mapcar (lambda (name) (phpinspect--make-type :name name))
          type-names))

(defconst phpinspect-native-typenames
  ;; self, parent and resource are not valid type name.
  ;; see https://www.php.net/manual/ja/language.types.declarations.php
  ;;;
  ;; However, this list does not need to be valid, it just needs to contain the
  ;; list of type names that we should not attempt to resolve relatively.
  '("array" "bool" "callable" "float" "int" "iterable" "mixed" "object" "string" "void" "self" "static" "this"))

(defconst phpinspect-native-types
  (phpinspect--make-types (mapcar (lambda (name) (concat "\\" name))
                                  phpinspect-native-typenames)))

(defvar phpinspect-collection-types
  (phpinspect--make-types '("\\array" "\\iterable" "\\SplObjectCollection" "\\mixed"))
  "FQNs of types that should be treated as collecitons when inferring types.")

(defconst phpinspect--object-type (phpinspect--make-type :name "\\object" :fully-qualified t))
(defconst phpinspect--static-type (phpinspect--make-type :name "\\static" :fully-qualified t))
(defconst phpinspect--self-type (phpinspect--make-type :name "\\self" :fully-qualified t))
(defconst phpinspect--this-type (phpinspect--make-type :name "\\this" :fully-qualified t))
(defconst phpinspect--null-type (phpinspect--make-type :name "\\null" :fully-qualified t))

(cl-defmethod phpinspect--type-set-name ((type phpinspect--type) (name string))
  (setf (phpinspect--type-name-symbol type) (phpinspect-intern-name name)))

(cl-defmethod phpinspect--type-does-late-static-binding ((type phpinspect--type))
  "Whether or not TYPE is used for late static binding.
See https://wiki.php.net/rfc/static_return_type ."
  (or (phpinspect--type= type phpinspect--static-type)
      (phpinspect--type= type phpinspect--this-type)))

(cl-defmethod phpinspect--resolve-late-static-binding
  ((type phpinspect--type)
   (class-type phpinspect--type))
  (if (phpinspect--type-does-late-static-binding type)
      class-type
    type))

(defsubst phpinspect--type-is-native (type)
  (catch 'found
    (dolist (native phpinspect-native-types)
      (when (phpinspect--type= type native)
        (throw 'found t)))))

(defsubst phpinspect--type-is-collection (type)
  (catch 'found
    (dolist (collection phpinspect-collection-types)
      (when (phpinspect--type= type collection)
        (throw 'found t)))))


(cl-defmethod phpinspect--type-name ((type phpinspect--type))
  (symbol-name (phpinspect--type-name-symbol type)))

(defun phpinspect--get-bare-class-name-from-fqn (fqn)
  (car (last (split-string fqn "\\\\"))))

(cl-defmethod phpinspect--type-bare-name ((type phpinspect--type))
  "Return just the name, without namespace part, of TYPE."
  (phpinspect--get-bare-class-name-from-fqn (phpinspect--type-name type)))

(cl-defmethod phpinspect--type= ((type1 phpinspect--type) (type2 phpinspect--type))
  (eq (phpinspect--type-name-symbol type1) (phpinspect--type-name-symbol type2)))

(defun phpinspect--resolve-type-name (types namespace type)
  "Get the FQN for TYPE, using TYPES and NAMESPACE as context.

TYPES must be an alist with class names as cars and FQNs as cdrs.
NAMESPACE may be nil, or a string with a namespace FQN."
  (phpinspect--log "Resolving %s from namespace %s" type namespace)
  ;; Absolute FQN
  (cond ((string-match "^\\\\" type)
         type)

        ;; Native type
        ((member type phpinspect-native-typenames)
         (concat "\\" type))

        ;; Relative FQN
        ((and namespace (string-match "\\\\" type))
         (concat "\\" namespace "\\" type))

        ;; Clas|interface|trait name
        (t (let ((from-types (assoc-default (phpinspect-intern-name type) types #'eq)))
             (cond (from-types
                    (phpinspect--type-name from-types))
                   (namespace
                    (concat "\\" namespace "\\" type))
                   (t (concat "\\" type)))))))

(cl-defmethod phpinspect--type-resolve (types namespace (type phpinspect--type))
  (unless (phpinspect--type-fully-qualified type)
    (phpinspect--type-set-name
     type
     (phpinspect--resolve-type-name types namespace (phpinspect--type-name type)))
    (setf (phpinspect--type-fully-qualified type) t))
  (when (phpinspect--type-is-collection type)
    (setf (phpinspect--type-collection type) t))
  type)

(defun phpinspect--find-innermost-incomplete-class (token)
  (let ((last-token (car (last token))))
    (cond ((phpinspect-incomplete-class-p token) token)
          ((phpinspect-incomplete-token-p last-token)
           (phpinspect--find-innermost-incomplete-class last-token)))))

(defun phpinspect--find-class-token (token)
  "Recurse into token tree until a class is found."
  (when (and (listp token) (> (length token) 1))
    (let ((last-token (car (last token))))
      (cond ((phpinspect-class-p token) token)
            (last-token
             (phpinspect--find-class-token last-token))))))

(defun phpinspect--make-type-resolver (types &optional token-tree namespace)
  "Little wrapper closure to pass around and resolve types with."
  (let* ((inside-class
          (and token-tree (or (phpinspect--find-innermost-incomplete-class token-tree)
                              (phpinspect--find-class-token token-tree))))
         (inside-class-name
          (and inside-class (phpinspect--get-class-name-from-token inside-class))))
    (lambda (type)
      (phpinspect--type-resolve
       types
       namespace
       (if (and inside-class-name (phpinspect--type= type phpinspect--self-type))
           (progn
             (phpinspect--log "Returning inside class name for %s : %s"
                              type inside-class-name)
             (phpinspect--make-type :name inside-class-name))
         ;; else
         type)))))

(cl-defgeneric phpinspect--format-type-name (name)
  (if name
      (error "Unexpected value: %s" name)
    "unknown-type"))

(cl-defmethod phpinspect--format-type-name ((name string))
  (string-remove-prefix "\\" name))

(cl-defmethod phpinspect--format-type-name ((type phpinspect--type))
  (phpinspect--format-type-name (phpinspect--type-name type)))

(cl-defstruct (phpinspect--function (:constructor phpinspect--make-function-generated)
                                    (:copier phpinspect--copy-function))
  "A PHP function."
  (name-symbol nil
               :type symbol
               :documentation
               "A symbol associated with the name of the function")
  (token nil
         :type phpinspect-function-p
         :documentation
         "The tokens with which this function was declared.")
  (-inherited nil
                 :type boolean
                 :documentation
                 "Whether this function has been incorporated into a class as
method of an extended class.")
  (scope nil
         :type phpinspect-scope
         :documentation
         "When the function is a method, this should contain the
scope of the function as returned by `phpinspect-parse-scope`.")
  (arguments nil
             :type list
             :documentation
             "A simple list with function arguments and their
types in tuples. Each list should have the name of the variable
as first element and the type as second element.")
  (return-type nil
               :type phpinspect--type
               :documentation
               "A phpinspect--type object representing the the
return type of the function."))

(defmacro phpinspect--make-function (&rest property-list)
  `(phpinspect--make-function-generated
    ,@(phpinspect--wrap-plist-name-in-symbol property-list)))

(cl-defmethod phpinspect--function-set-name ((func phpinspect--function) (name string))
  (setf (phpinspect--function-name-symbol func) (intern name phpinspect-name-obarray)))

(define-inline phpinspect--function-name (func)
  (inline-quote (symbol-name (phpinspect--function-name-symbol ,func))))

(cl-defstruct (phpinspect--variable (:constructor phpinspect--make-variable))
  "A PHP Variable."
  (name nil
        :type string
        :documentation
        "A string containing the name of the variable.")
  (scope nil
         :documentation
         "When the variable is an object attribute, this should
contain the scope of the variable as returned by
`phpinspect-parse-scope'")
  (lifetime nil
            :documentation
            "The lifetime of the variable (e.g. whether it is static or not). Will
contain the parsed keyword token indicating the lifetime of the variable")
  (mutability nil
              :documentation
              "The mutability of the variable (e.g. whether it is constant or
not). Will contain the parsed keyword token indicating the
mutability of the variable")
  (type nil
        :type string
        :documentation
        "A string containing the FQN of the variable's type"))

(defun phpinspect--variable-static-p (variable)
  (phpinspect-static-p (phpinspect--variable-lifetime variable)))

(defun phpinspect--variable-const-p (variable)
  (phpinspect-const-p (phpinspect--variable-mutability variable)))

(defun phpinspect--variable-vanilla-p (variable)
  (not (or (phpinspect--variable-static-p variable)
           (phpinspect--variable-const-p variable))))

(defun phpinspect--use-to-type (use)
  (let* ((fqn (cadr (cadr use)))
         (type (phpinspect--make-type :name (if (string-match "^\\\\" fqn)
                                                fqn
                                              (concat "\\" fqn))
                                      :fully-qualified t))
         (type-name (if (and (phpinspect-word-p (caddr use))
                             (string= "as" (cadr (caddr use))))
                        (cadr (cadddr use))
                      (progn (string-match "[^\\]+$" fqn)
                             (match-string 0 fqn)))))
    (cons (phpinspect-intern-name type-name) type)))

(defun phpinspect--uses-to-types (uses)
  (mapcar #'phpinspect--use-to-type uses))

(defun phpinspect--get-class-name-from-token (class-token)
  (let ((subtoken (seq-find (lambda (word)
                              (and (phpinspect-word-p word)
                                   (not (string-match
                                         (concat "^" (phpinspect--class-keyword-handler-regexp))
                                         (concat (cadr word) " ")))))
                            (cadr class-token))))
    (cadr subtoken)))

(defun phpinspect--index-class-declaration (decl type-resolver)
  ;; Find out what the class extends or implements
  (let (encountered-extends encountered-implements encountered-class
        class-name extends implements used-types)
    (dolist (word decl)
      (if (phpinspect-word-p word)
          (cond ((string= (cadr word) "extends")
                 (phpinspect--log "Class %s extends other classes" class-name)
                 (setq encountered-extends t))
                ((string= (cadr word) "implements")
                 (setq encountered-extends nil)
                 (phpinspect--log "Class %s implements in interface" class-name)
                 (setq encountered-implements t))
                ((string= (cadr word) "class")
                 (setq encountered-class t))
                (t
                 (phpinspect--log "Calling Resolver from index-class on %s" (cadr word))
                 (cond (encountered-extends
                        (push (funcall type-resolver (phpinspect--make-type
                                                      :name (cadr word)))
                              extends)
                        (push (cadr word) used-types))
                       (encountered-implements
                        (push (funcall type-resolver (phpinspect--make-type
                                                      :name (cadr word)))
                              implements)
                        (push (cadr word) used-types))
                       (encountered-class
                        (setq class-name (funcall type-resolver (phpinspect--make-type :name (cadr word)))
                              encountered-class nil)))))))

    (list class-name extends implements used-types)))

(defun phpinspect-namespace-name (namespace)
  (or (and (phpinspect-namespace-p namespace)
           (phpinspect-word-p (cadr namespace))
           (cadadr namespace))
      ""))

(provide 'phpinspect-type)
;;; phpinspect-type.el ends here
