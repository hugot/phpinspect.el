;;; phpinspect-typedef.el --- Models for PHP type definitions  -*- lexical-binding: t; -*-

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
(require 'phpinspect-method-cell)
(require 'phpinspect-property-cell)
(require 'phpinspect-type)

(cl-defstruct (phpinspect-typedef
               (:constructor phpinspect-make-typedef-generated)
               (:conc-name phpi-typedef-))
  "A typedef represents the definition of a PHP type, be it class,
interface, trait or enum."
  (read-only-p nil
               :type boolean
               :documentation
               "Whether this typedef is read-only, meaning that its data
should never be changed. Methods and functions that are meant to
manipulate typedef data should become no-ops when this slot has a
non-nil value.")
  (retriever nil
                   :type lambda
                   :documentaton
                   "A function that returns typedefs for types
(should accept object of type `phpinspect--type' as argument)")
  (methods nil
           :type phpinspect-method-collection)
  (static-methods nil
                  :type phpinspect-method-collection)
  (properties nil
              :type list)
  (initial-index nil
                 :type bool)
  (trait-config nil
                :documentation
                "The configuration of traits that are used in this type")
  (-dependencies nil
                 :documentation "cache for dependencies")
  (-dependencies-loaded nil
                        :documentation "Whether dependencies have
 been loaded or not. This slot is externally mutated by projects.")
  (method-adder nil
                :type phpinspect-method-adder)
  (subscribed-types nil
                    :type list
                    :documentation "list of phpinspect--type structures.")
  (subscribed-to-types nil
                       :type list
                       :documentation "list of phpinspect--type structures")
  (name nil
        :type phpinspect--type)
  (declaration nil
               :type phpinspect-declaration-p))

(defun phpinspect-make-typedef (type &optional retriever)
  (cl-assert (phpinspect--type-p type))

  (let ((def (phpinspect-make-typedef-generated
              :name type
              :retriever retriever
	      :properties (phpinspect-make-property-collection :home-type type)
              :methods (phpinspect-make-method-collection :home-type type)
              :static-methods (phpinspect-make-method-collection :home-type type)
              :method-adder (phpinspect-make-method-adder))))
    def))

(define-inline phpi--typedef-get-foreign-type (def type)
  (inline-letevals (def type)
    (inline-quote
     (when-let ((retriever (phpi-typedef-retriever ,def)))
       (funcall retriever ,type)))))

(defmacro phpi--typedef-edit (typedef &rest body)
  "Declare intent to edit TYPEDEF in BODY.

Conditionally executes BODY depending on
`phpi-typedef-read-only-p' value."
  (declare (indent 1))
  (let ((typedef-sym (gensym))
        (return-sym (gensym)))
    `(let ((,typedef-sym ,typedef)
           ,return-sym)

       (cl-assert
        (phpinspect-typedef-p ,typedef-sym) t
        "First argument of `phpi--typedef-edit' must be of type `phpinspect-typedef'")

       (unless (phpi-typedef-read-only-p ,typedef-sym)
         (setq ,return-sym (progn ,@body))

         ;; Clear cache and unset dependencies loaded.
         (setf (phpi-typedef--dependencies ,typedef-sym) nil)
         (setf (phpi-typedef--dependencies-loaded ,typedef-sym) nil)

         ,return-sym))))

(cl-defstruct (phpinspect-method-adder
               (:constructor phpinspect-make-method-adder)
               (:conc-name phpi-ma-))
  "The method adder adds methods to a method-collection, aliasing
and/or overriding methods when appropriate."
  (aliases nil
           :type alist)
  (overrides nil
             :type alist))

(defun phpi-ma-get-overrides (ma type)
  (alist-get type (phpi-ma-overrides ma) nil nil #'phpinspect--type=))

(defun phpi-ma-get-aliases (ma type)
  (alist-get type (phpi-ma-aliases ma) nil nil #'phpinspect--type=))

(defun phpi-ma-set-config (ma config)
  "Set type method import config for MA to CONFIG.

This function reconfigures MA to add methods according to the
rules specified in CONFIG. CONFIG is expected to be a list of the
structure returned by `phpinspect--index-trait-use'."
  (setf (phpi-ma-aliases ma) nil)
  (setf (phpi-ma-overrides ma) nil)

  (dolist (typeconf config)
    (let ((type (car typeconf))
          overrides aliases)
      (dolist (setting (cdr typeconf))
        (pcase (car setting)
          ('alias (push (cons (phpinspect-intern-name (cadr setting))
                              (phpinspect-intern-name (caddr setting)))
                        aliases))
          ('override (push (cons (phpinspect-intern-name (cadr setting))
                                 (caddr setting))
                           overrides))
          (_ (error (format "Invalid type configuration instruction %s" (car setting))))))

      (push (cons type aliases) (phpi-ma-aliases ma))
      (push (cons type overrides) (phpi-ma-overrides ma)))))

(defun phpi-ma-add (ma mcol method &optional overwrite)
  (let ((overrides (phpi-ma-get-overrides ma (phpi-method-origin-type method)))
        (aliases (phpi-ma-get-aliases ma (phpi-method-origin-type method)))
        alias)

    ;; Can't alias if overriding, can't override if aliasing
    (cond ((and overrides (alist-get (phpi-method-name method) overrides))
           ;; Override basically just means insert and overwrite without checking
           (phpi-mcol-add mcol method 'overwrite))
          ((and aliases (setq alias (alist-get (phpi-method-name method) aliases)))
           (let ((copy (phpi-copy-method method)))
             ;; Rename method to alias, record original name in aliased-from slot
             (setf (phpi-method-aliased-from copy) (phpi-method-name method)
                   (phpi-method-name copy) alias)
             (phpi-mcol-add mcol copy overwrite)))
          (t
           ;; Not dealing with aliases or overrides, just add the methods,
           ;; respecting `overwrite'.
           (phpi-mcol-add mcol method overwrite)))))

(defun phpi-typedef-get-methods (def)
  (phpi-mcol-list-active (phpi-typedef-methods def)))

(defun phpi-typedef-get-static-methods (def)
  (phpi-mcol-list-active (phpi-typedef-static-methods def)))

(cl-defmethod phpi-typedef-get-method ((def phpinspect-typedef) (method-name (head phpinspect-name)))
  (phpi-mcol-get-active-method (phpi-typedef-methods def) method-name))

(cl-defmethod phpi-typedef-get-method ((def phpinspect-typedef) (method-name string))
  (phpi-typedef-get-method def (phpinspect-intern-name method-name)))

(cl-defmethod phpi-typedef-get-static-method ((def phpinspect-typedef) (method-name (head phpinspect-name)))
  (phpi-mcol-get-active-method (phpi-typedef-static-methods def) method-name))

(cl-defmethod phpi-typedef-get-static-method ((def phpinspect-typedef) (method-name string))
  (phpi-typedef-get-static-method def (phpinspect-intern-name method-name)))

(cl-defmethod phpi-typedef-set-method ((def phpinspect-typedef) (m phpinspect-method) &optional no-propagate)
  (phpi--typedef-edit def
    (phpi-ma-add (phpi-typedef-method-adder def) (phpi-typedef-methods def) m 'overwrite)
    (unless no-propagate
      (phpi-typedef-trigger-subscriber-method-update def m))))

(cl-defmethod phpi-typedef-set-method ((def phpinspect-typedef) (fn phpinspect--function) &optional no-propagate)
    (phpi-typedef-set-method def (phpinspect-make-method (phpi-typedef-name def) fn) no-propagate))

(cl-defmethod phpi-typedef-set-static-method ((def phpinspect-typedef) (m phpinspect-method) &optional no-propagate)
  (phpi--typedef-edit def
    (phpi-ma-add (phpi-typedef-method-adder def) (phpi-typedef-static-methods def) m 'overwrite)
    (unless no-propagate
      (phpi-typedef-trigger-subscriber-method-update def m 'static))))

(cl-defmethod phpi-typedef-set-static-method ((def phpinspect-typedef) (fn phpinspect--function) &optional no-propagate)
  (phpi-typedef-set-static-method def (phpinspect-make-method (phpi-typedef-name def) fn) no-propagate))

(cl-defmethod phpi-typedef-delete-method ((def phpinspect-typedef) (name (head phpinspect-name)))
  (phpi--typedef-edit def
    (phpi-mcol-delete-for-type
     (phpi-typedef-methods def) (phpi-typedef-name def) name)
    (phpi-typedef-trigger-subscriber-method-delete def (phpi-typedef-name def) name)

    (phpi-mcol-delete-for-type
     (phpi-typedef-static-methods def) (phpi-typedef-name def) name)
    (phpi-typedef-trigger-subscriber-method-delete def (phpi-typedef-name def) name 'static)))

(cl-defmethod phpi-typedef-delete-method ((def phpinspect-typedef) (name string))
  (phpi-typedef-delete-method def (phpinspect-intern-name name)))

(cl-defmethod phpi-typedef-delete-method ((def phpinspect-typedef) (fn phpinspect--function))
  (phpi-typedef-delete-method def (phpinspect--function-name-symbol fn)))

(defun phpi-typedef-set-trait-config (def config)
  "Configures CONFIG for typedef DEF.

Note: this function returns all types found in CONFIG."
  (phpi--typedef-edit def
    (let ((existing-config (phpi-typedef-trait-config def))
          types)
      ;; A configuration that is exactly the same doesn't need to be applied twice
      (unless (equal config existing-config)
        (when existing-config
          ;; This can probably be made more sophisticated by only applying the
          ;; difference, but just deleting all incorporated methods will do for
          ;; an initial implementation.
          (dolist (use existing-config)
            (when-let ((fd (phpi--typedef-get-foreign-type def (car use))))
              (phpi-typedef-unsubscribe-from-foreign-typedef def fd))))
        ;; Apply new config
        (phpi-ma-set-config (phpi-typedef-method-adder def) config))
      (dolist (cons config)
        (push (car cons) types))

      (setf (phpi-typedef-trait-config def) config)
      ;; return all types that were in config
      types)))

(defun phpi-typedef-add-foreign-members (def foreign-def)
  "Add methods and properties to DEF from FOREIGN-DEF."
  (phpi--typedef-edit def
    (when foreign-def
      (let ((ma (phpi-typedef-method-adder def))
	    (methods (phpi-typedef-methods def))
	    (st-methods (phpi-typedef-static-methods def))
	    (props (phpi-typedef-properties def)))

	(dolist (method (phpi-typedef-get-methods foreign-def))
          (phpi-ma-add ma methods method))

	(dolist (method (phpi-typedef-get-static-methods foreign-def))
          (phpi-ma-add ma st-methods method))

	(dolist (prop (phpi-pcol-list-active (phpi-typedef-properties foreign-def)))
	  (phpi-pcol-add props prop))))))

(defun phpi-typedef-subscribe-to-foreign-typedef (def foreign-def)
  (phpi--typedef-edit def
    (push (phpi-typedef-name def) (phpi-typedef-subscribed-types foreign-def))
    (push (phpi-typedef-name foreign-def) (phpi-typedef-subscribed-to-types def))))

(defun phpi-typedef-unsubscribe-from-foreign-typedef (def foreign-def)
  "Unsubscribe DEF from changes in FOREIGN-DEF.

This undoes any links and data sharing between this type and any
extended classes, used traits or implemented interfaces."
  (phpi--typedef-edit def
    (setf (phpi-typedef-subscribed-types foreign-def)
          (cl-remove (phpi-typedef-name def) (phpi-typedef-subscribed-types foreign-def)
                     :test #'phpinspect--type=))

    (setf (phpi-typedef-subscribed-to-types def)
          (cl-remove (phpi-typedef-name foreign-def) (phpi-typedef-subscribed-types def)
                     :test #'phpinspect--type=))

    ;; Remove all members of foreign typedef
    (let ((ft (phpi-typedef-name foreign-def)))
      (phpi-mcol-delete-for-type (phpi-typedef-methods def) ft)
      (phpi-mcol-delete-for-type (phpi-typedef-static-methods def) ft)
      (phpi-pcol-delete-for-type (phpi-typedef-properties def) ft))))

(defun phpi-typedef-trigger-subscriber-update (def)
  "Incorporate DEF into subscribed typedefs."
  (dolist (type (phpi-typedef-subscribed-types def))
    (when-let ((foreign-def (phpi--typedef-get-foreign-type def type)))
      (phpi-typedef-add-foreign-members foreign-def def)
      ;; Notify subscribers of upstream changes
      (phpi-typedef-trigger-subscriber-update foreign-def))))

(defun phpi-typedef-trigger-subscriber-method-update (def method &optional static)
  "Update downstream subscribers of DEF by setting METHOD.

If STATIC is non-nil, updates static method."
  (dolist (type (phpi-typedef-subscribed-types def))
    (when-let ((foreign-def (phpi--typedef-get-foreign-type def type)))
      (if static
          (phpi-typedef-set-static-method foreign-def method)
        (phpi-typedef-set-method foreign-def method)))))

(defun phpi-typedef-trigger-subscriber-property-update (def property)
  "Update downstream subscribers of DEF by setting PROPERTY."
  (dolist (type (phpi-typedef-subscribed-types def))
    (when-let ((foreign-def (phpi--typedef-get-foreign-type def type)))
      (phpi-typedef-set-property foreign-def property))))

(defun phpi-typedef-trigger-subscriber-property-delete (def origin-type property-name)
  "Update subscribers of DEF by deleting PROPERTY-NAME of origin TYPE."
  (dolist (subtype (phpi-typedef-subscribed-types def))
    (when-let ((foreign-def (phpi--typedef-get-foreign-type def subtype)))
      (phpi--typedef-edit foreign-def
	(phpi-pcol-delete-for-type
	 (phpi-typedef-properties foreign-def) origin-type property-name)
	(phpi-typedef-trigger-subscriber-property-delete foreign-def origin-type property-name)))))

(defun phpi-typedef-trigger-subscriber-method-delete (def type method-name &optional static)
  "Update subscribers of DEF by deleting METHOD-NAME of origin TYPE."
  (dolist (subtype (phpi-typedef-subscribed-types def))
    (when-let ((foreign-def (phpi--typedef-get-foreign-type def subtype)))
      (phpi--typedef-edit foreign-def
        (phpi-mcol-delete-for-type
         (if static (phpi-typedef-static-methods foreign-def) (phpi-typedef-methods foreign-def))
         type method-name))

      (phpi-typedef-trigger-subscriber-method-delete foreign-def type method-name static))))

(defun phpi-typedef-set-name (def type)
  "Set the TYPE name of DEF.

TYPE must be a structure of type `phpinspect--type'."
  (phpi--typedef-edit def
    (cl-assert (phpinspect--type-p type))

    (let ((existing (phpi-typedef-name def)))
      ;; Only update if type name is actually different
      (unless (phpinspect--type= existing type)
        (setf (phpi-typedef-name def) type)
        (phpi-mcol-set-home-type (phpi-typedef-methods def) type)
        (phpi-mcol-set-home-type (phpi-typedef-static-methods def) type)
	(phpi-pcol-set-home-type (phpi-typedef-properties def) type)))))

(defun phpi-typedef-update-declaration (def declaration imports namespace-name trait-config)
  "Update declaration of DEF.

DECLARATION must be a token of type `phpinspect-declaration-p`.

IMPORTS must be an alist of imported types as returned by
`phpinspect--uses-to-types'.

NAMESPACE-NAME must be a string containing the name of the
namespace enclosing DEF.

TRAIT-CONFIG must be a trait configuration as returned by
`phpinspect--index-trait-use'."
  (phpi--typedef-edit def
    (pcase-let ((`(,type ,extends ,implements ,_used-types)
                 (phpinspect--index-class-declaration
                  declaration (phpinspect--make-type-resolver
                               (phpinspect--uses-to-types imports) nil namespace-name))))
      (phpi-typedef-set-name def type)
      (setf (phpi-typedef-declaration def) declaration)
      (phpi-typedef-update-extensions
       def `(,@extends ,@implements ,@(phpi-typedef-set-trait-config def trait-config))))))

(defun phpi-typedef-update-extensions (def extensions)
  "Sets extended types of DEF to EXTENSIONS.

EXTENSIONS must be a list of structures of the type
`phpinspect--type'. These are used to fetch typedefs belonging to
them, which are then incorporated into DEF's properties."
  (phpi--typedef-edit def
    (when-let ((subscriptions (phpi-typedef-subscribed-to-types def)))
      (dolist (sub subscriptions)
        (unless (cl-member sub extensions :test #'phpinspect--type=)
          ;; No longer an extended type, unsubscribe
          (when-let ((fd (phpi--typedef-get-foreign-type def sub)))
            (phpi-typedef-unsubscribe-from-foreign-typedef def fd))))

      (dolist (ext extensions)
        (when (cl-member ext subscriptions :test #'phpinspect--type=)
            ;; Already subscribed, no refresh needed
          (delq ext extensions))))

    (when-let ((foreign-defs
                (seq-filter
                 #'phpinspect-typedef-p
                 (mapcar
                  (lambda (class-name)
                    (phpi--typedef-get-foreign-type def class-name))
                  extensions))))

      (dolist (fd foreign-defs)
        (phpi-typedef-add-foreign-members def fd)
        (phpi-typedef-subscribe-to-foreign-typedef def fd)))))

(cl-defmethod phpi-typedef-set-index ((def phpinspect-typedef)
                                      (index (head phpinspect--indexed-class)))
  "Update all of DEF's properties using the contents of INDEX."
  (phpi--typedef-edit def
    (setf (phpi-typedef-declaration def) (alist-get 'declaration index))

    (let ((ma (phpi-typedef-method-adder def))
          (mcol (phpi-typedef-methods def))
          (stmcol (phpi-typedef-static-methods def))
          (home-type (phpi-typedef-name def))
	  (pcol (phpi-typedef-properties def)))

    ;; Override methods when class seems syntactically correct (has balanced braces)
      (when (alist-get 'complete index)
        ;; Delete all known own methods and properties
        (phpi-mcol-delete-for-type mcol home-type)
        (phpi-mcol-delete-for-type stmcol home-type)
	(phpi-pcol-delete-for-type pcol home-type))

      (dolist (method (alist-get 'methods index))
        (phpi-ma-add ma mcol (phpinspect-make-method home-type method) 'overwrite))

      (dolist (method (alist-get 'static-methods index))
        (phpi-ma-add ma stmcol (phpinspect-make-method home-type method) 'overwrite))

      (setf (phpi-typedef-initial-index def) t)

      (let-alist index
	(dolist (var (append .variables .constants .static-variables))
	  (phpi-pcol-add pcol (phpinspect-make-property home-type var))))

      (phpi-typedef-update-extensions
       def `(,@(alist-get 'implements index)
             ,@(alist-get 'extends index)
             ,@(phpi-typedef-set-trait-config def (alist-get 'trait-config index))))

      (phpi-typedef-trigger-subscriber-update def))))

(cl-defmethod phpi-typedef-get-method-return-type
  ((def phpinspect-typedef) (method-name (head phpinspect-name)) &optional tryhard)
  (phpi-mcol-get-return-type (phpi-typedef-methods def) method-name tryhard))

(cl-defmethod phpi-typedef-get-static-method-return-type
  ((def phpinspect-typedef) (method-name (head phpinspect-name)) &optional tryhard)
  (phpi-mcol-get-return-type (phpi-typedef-static-methods def) method-name tryhard))

(cl-defmethod phpi-typedef-set-property ((def phpinspect-typedef) (prop phpinspect-property))
  (phpi--typedef-edit def
    (phpi-pcol-add (phpi-typedef-properties def) prop)
    (phpi-typedef-trigger-subscriber-property-update def prop)))

(cl-defmethod phpi-typedef-set-property ((def phpinspect-typedef)
                                         (var phpinspect--variable))
  (phpi-typedef-set-property
   def (phpinspect-make-property (phpi-typedef-name def) var)))

(cl-defmethod phpi-typedef-delete-property ((def phpinspect-typedef)
					    (name (head phpinspect-name))
					    &optional origin-type)
  (phpi--typedef-edit def
    (phpi-pcol-delete-for-type
     (phpi-typedef-properties def) (phpi-typedef-name def) name)
    (phpi-typedef-trigger-subscriber-property-delete
     def (or origin-type (phpi-typedef-name def)) name)))

(cl-defmethod phpi-typedef-delete-property ((def phpinspect-typedef)
                                            (var phpinspect--variable))
  (phpi-typedef-delete-property
   def (phpinspect-intern-name (phpinspect--variable-name var))))

(cl-defmethod phpi-typedef-get-properties ((def phpinspect-typedef))
  (seq-filter #'phpi-prop-vanilla-p
	      (phpi-pcol-list-active (phpi-typedef-properties def))))

(cl-defmethod phpi-typedef-get-static-properties ((def phpinspect-typedef))
  (seq-filter #'phpi-prop-static-p
	      (phpi-pcol-list-active (phpi-typedef-properties def))))

(cl-defmethod phpi-typedef-get-constants ((def phpinspect-typedef))
  (seq-filter #'phpi-prop-const-p
	      (phpi-pcol-list-active (phpi-typedef-properties def))))

(cl-defmethod phpi-typedef-get-property
  ((def phpinspect-typedef) (prop-name string))
  (phpi-pcol-get-active-property
   (phpi-typedef-properties def) (phpinspect-intern-name prop-name)))

(defun phpi-typedef-get-dependencies (def)
  "Gets types that DEF directly depends on.

Return value is a list if structures of the type
`phpinspect--type'."
  (with-memoization (phpi-typedef--dependencies def)
    (let ((deps (phpi-typedef-subscribed-to-types def)))
      (dolist (method (phpi-typedef-get-methods def))
        (when (phpi-method-return-type method)
          (push (phpi-method-return-type method) deps)))

      (dolist (method (phpi-typedef-get-static-methods def))
        (when (phpi-method-return-type method)
          (push (phpi-method-return-type method) deps)))

      (dolist (prop (phpi-pcol-list-active (phpi-typedef-properties def)))
        (when (phpi-prop-type prop)
          (push (phpi-prop-type prop) deps)))

      (phpinspect--types-uniq deps (phpi-typedef-name def)))))


(provide 'phpinspect-typedef)
;;; phpinspect-typedef.el ends here
