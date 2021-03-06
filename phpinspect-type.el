;;; phpinspect-type.el --- Data structures that represent phpinspect types  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc

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

(cl-defstruct (phpinspect--type
               (:constructor phpinspect--make-type-generated))
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
            "When the type is a collection, this attribute is set to the type that the collection is expected to contain")
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

(cl-defmethod phpinspect--type-name ((type phpinspect--type))
  (symbol-name (phpinspect--type-name-symbol type)))

(defun phpinspect--get-bare-class-name-from-fqn (fqn)
  (car (last (split-string fqn "\\\\"))))

(cl-defmethod phpinspect--type-bare-name ((type phpinspect--type))
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
  type)

(defun phpinspect--make-type-resolver (types &optional token-tree namespace)
  "Little wrapper closure to pass around and resolve types with."

  (let* ((inside-class
          (if token-tree (or (phpinspect--find-innermost-incomplete-class token-tree)
                             (phpinspect--find-class-token token-tree))))
         (inside-class-name (if inside-class (phpinspect--get-class-name-from-token
                                              inside-class))))
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

(cl-defgeneric phpinspect--function-name ((func phpinspect--function)))

(cl-defmethod phpinspect--function-name ((func phpinspect--function))
  (symbol-name (phpinspect--function-name-symbol func)))


(cl-defstruct (phpinspect--variable (:constructor phpinspect--make-variable))
  "A PHP Variable."
  (name nil
        :type string
        :documentation
        "A string containing the name of the variable.")
  (scope nil
         :type phpinspect-scope
         :documentation
         "When the variable is an object attribute, this should
contain the scope of the variable as returned by
`phpinspect-parse-scope`")
  (type nil
        :type string
        :documentation
        "A string containing the FQN of the variable's type"))


(provide 'phpinspect-type)
;;; phpinspect-type.el ends here
