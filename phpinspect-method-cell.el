;;; phpinspect-method-cell.el --- Models for PHP method definitions  -*- lexical-binding: t; -*-

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

;; This file contains struct definitions and logic for the management of
;; (collections of) methods associated with a specific type.

;;; Code:

(require 'cl-macs)
(require 'phpinspect-util)
(require 'phpinspect-type)

(cl-defstruct (phpinspect-method
               (:constructor phpinspect-make-method-generated)
               (:conc-name phpi-method-)
               (:copier phpi-copy-method))
  (name nil
        :type phpinspect-name)
  (aliased-from nil
                :type phpinspect-name)
  (origin-type nil
               :type phpinspect--type)
  (-return-type nil
                :type phpinspect--type)
  (definition  nil
               :type phpinspect--function))

(defun phpinspect-make-method (origin-type definition)
  "Create a method for ORIGIN-TYPE, defined in DEFINITION.

ORIGIN-TYPE must be a structure of type `phpinspect--type'.
DEFINITION must be a structure of type `phpinspect--function'."
  (phpinspect-make-method-generated
   :name (phpinspect--function-name-symbol definition)
   :origin-type origin-type
   :definition definition))

(defun phpi-method-name-string (method)
  (phpinspect-name-string (phpi-method-name method)))

(cl-defstruct (phpinspect-method-cell
               (:constructor phpinspect-make-method-cell)
               (:conc-name phpi-mc-))
  (name nil
        :type phpinspect-name)
  (own nil
       :type phpinspect-method)
  (trait nil
         :type phpinspect-method)
  (inherited nil
             :type phpinspect-method)
  (interface nil
             :type phpinspect-method))

(cl-defstruct (phpinspect-method-collection
               (:constructor phpinspect-make-method-collection)
               (:conc-name phpi-mcol-))
  (home-type nil
             :type phpinspect--type)
  (cells nil
         :type alist
         :documentation "<phpinspect-name, phpinspect-method-cell>"))

(defun phpi-mcol-set-home-type (mcol type)
  "Set home type of MCOL to TYPE.

Also updates all members of MCOL with the same origin-type."
  (setf (phpi-mcol-home-type mcol) type)
  (dolist (method (phpi-mcol-list-own mcol))
    (setf (phpi-method-origin-type method) type)))

(defun phpi-mcol-find-cell (mcol method-name &optional remove)
  (cl-assert (phpinspect-name-p method-name))

  (alist-get method-name (phpi-mcol-cells mcol) nil remove #'eq))

(defun phpi-mcol-find-cell-create (mcol method-name)
  (let ((cell (phpi-mcol-find-cell mcol method-name)))
    (unless cell
      (setq cell (phpinspect-make-method-cell
                  :name method-name))
      (push (cons method-name cell) (phpi-mcol-cells mcol)))

    cell))

(defun phpi-mcol-add (mcol method &optional overwrite)
  (let ((cell (phpi-mcol-find-cell-create mcol (phpi-method-name method))))
    ;; insert when not present or overwriting
    (when (or (not (phpi-mc-get-for-type-category
                    cell (phpi-mcol-home-type mcol) (phpi-method-origin-type method))))
              overwrite)
      (phpi-mc-set cell (phpi-mcol-home-type mcol) (phpi-method-origin-type method) method)
      cell))

(defun phpi-mcol-list-own (mcol)
  (let (list)
    (dolist (cons (phpi-mcol-cells mcol))
      (when-let ((method (phpi-mc-own (cdr cons))))
        (push method list)))
    list))

(defun phpi-mcol-list-active (mcol)
  "List all active methods in MCOL."
  (let (list)
    (dolist (cons (phpi-mcol-cells mcol))
      (when-let ((method (phpi-mc-get-active (cdr cons))))
        (push method list)))
    list))

(defun phpi-method-set-return-type (method type)
  (setf (phpi-method--return-type method) type))

(defun phpi-method-original-return-type (method)
  (phpinspect--function-return-type (phpi-method-definition method)))

(defun phpi-method-does-late-static-binding (method)
  (when-let ((og-type (phpi-method-original-return-type method)))
    (phpinspect--type-does-late-static-binding og-type)))

(defun phpi-method-resolve-late-static-binding (method home-type)
  "Destructively modify METHOD to return HOME-TYPE if late static binding."
  (when-let ((og-type (phpi-method-original-return-type method)))
    (phpi-method-set-return-type
     method (phpinspect--resolve-late-static-binding og-type home-type))))

(defun phpi-mc-set (cell home-type origin-type method)
  (let ((late-static-binding (and method (phpi-method-does-late-static-binding method))))
    (if (phpinspect--type= home-type origin-type)
        ;; Method belongs to home type
        (progn
          (when late-static-binding
            (phpi-method-resolve-late-static-binding method home-type))

          (setf (phpi-mc-own cell) method))

      ;; resolve late static binding
      (when late-static-binding
        ;; Copy on write (we don't want to change the return type for the
        ;; original typedef as well).
        (setq method (phpi-copy-method method))
        (phpi-method-resolve-late-static-binding method home-type))

      ;; Method is from a trait, interface or inherited
      (pcase (phpinspect--type-category origin-type)
        ('trait (setf (phpi-mc-trait cell) method))
        ('interface (setf (phpi-mc-interface cell) method))
        ;; class or abstract class
        (_ (setf (phpi-mc-inherited cell) method) home-type)))))

(defun phpi-mc-get-for-type-category (cell home-type type)
  (if (phpinspect--type= home-type type)
      (phpi-mc-own cell)
    (pcase (phpinspect--type-category type)
      ('trait (phpi-mc-trait cell))
      ('interface (phpi-mc-interface cell))
      (_ (phpi-mc-inherited cell)))))

(defun phpi-method-origin-type= (method type)
  (phpinspect--type= (phpi-method-origin-type method) type))

(defun phpi-mc-get-for-type (cell type)
  (catch 'phpinspect--break
    (dolist (method (list (phpi-mc-own cell) (phpi-mc-trait cell)
                          (phpi-mc-inherited cell) (phpi-mc-interface cell)))
      (when (and method (phpi-method-origin-type= method type))
        (throw 'phpinspect--break method)))
    nil))

(defun phpi-mcol-delete (mcol method-name)
  (phpi-mcol-find-cell mcol method-name 'remove))

(defun phpi-mc-empty-p (cell)
  "CELL is empty when it contains no methods."
  (not (phpi-mc-get-active cell)))

(defun phpi-mc-get-active (cell)
  "Get active method from CELL, according to PHP precendence order.

PHP's precendence order is described in the documentation for
traits, located at:
https://www.php.net/manual/en/language.oop5.traits.php"
  (or (phpi-mc-own cell)
      (phpi-mc-trait cell)
      (phpi-mc-inherited cell)
      (phpi-mc-interface cell)))

(defun phpi-mc-get-return-type (cell)
  (when-let ((method (phpi-mc-get-active cell)))
    (phpi-method-return-type method)))

(define-inline phpi-try-method-return-type (method?)
  (inline-letevals (method?)
    (inline-quote
     (and ,method? (phpi-method-return-type ,method?)))))

(defun phpi-mc-get-return-type-tryhard (cell)
  (or (phpi-try-method-return-type (phpi-mc-own cell))
      (phpi-try-method-return-type (phpi-mc-trait cell))
      (phpi-try-method-return-type (phpi-mc-inherited cell))
      (phpi-try-method-return-type (phpi-mc-interface cell))))

(defun phpi-method-return-type (method)
  (or (phpi-method--return-type method)
      (phpinspect--function-return-type (phpi-method-definition method))))

(defun phpi-mcol-delete-for-type (mcol type &optional name)
  "Delete from MCOL all methods that originate from TYPE.

When NAME is provided, only method with NAME is deleted."
  (if name
      ;; Name is provided, only delete method with NAME.
      (when-let ((cell (phpi-mcol-find-cell mcol name)))
        (when (phpi-mc-get-for-type cell type)
          (phpi-mc-set cell (phpi-mcol-home-type mcol) type nil))

        (when (phpi-mc-empty-p cell)
          (phpi-mcol-delete mcol name)))

    (let ((cells (phpi-mcol-cells mcol)))
      (dolist (cons cells)
        (let ((cell (cdr cons)))
          (when (phpi-mc-get-for-type cell type)
            (phpi-mc-set cell (phpi-mcol-home-type mcol) type nil))

          ;; mark cell for deletion by setting car and cdr of alist member to
          ;; nil
          (when (phpi-mc-empty-p cell)
            (setcar cons nil)
            (setcdr cons nil))))
      ;; Delete all empty cells
      (setf (phpi-mcol-cells mcol) (delete (cons nil nil) cells)))))

(defun phpi-mcol-get-active-method (mcol name)
  (when-let ((cell (phpi-mcol-find-cell mcol name)))
    (phpi-mc-get-active cell)))

(defun phpi-mcol-get-return-type (mcol name &optional tryhard)
  "Get the returntype of method with NAME from MCOL.

If TRYHARD is provided and non-nil, also check for possible
return types in extended classes, traits and interfaces."
  (when-let ((cell (phpi-mcol-find-cell mcol name)))
    (if tryhard
        (phpi-mc-get-return-type-tryhard cell)
      (phpi-mc-get-return-type cell))))

;; Helpers for generic property access
(cl-defmethod phpi-fn-name ((fn phpinspect--function))
  (phpinspect--function-name fn))

(cl-defmethod phpi-fn-name ((method phpinspect-method))
  (phpi-method-name-string method))

(cl-defmethod phpi-fn-name-symbol ((fn phpinspect--function))
  (phpinspect--function-name-symbol fn))

(cl-defmethod phpi-fn-name-symbol ((fn phpinspect-method))
  (phpi-method-name-string fn))

(cl-defmethod phpi-fn-arguments ((method phpinspect-method))
  (phpi-fn-arguments (phpi-method-definition method)))

(cl-defmethod phpi-fn-arguments ((fn phpinspect--function))
  (phpinspect--function-arguments fn))

(cl-defmethod phpi-fn-return-type ((fn phpinspect--function))
  (or (phpinspect--function-return-type fn) phpinspect--unknown-type))

(cl-defmethod phpi-fn-return-type ((method phpinspect-method))
  (or (phpi-method-return-type method) phpinspect--unknown-type))

(cl-defmethod phpi-fn-argument-type ((fn phpinspect--function) argument-name)
  (phpinspect--function-argument-type fn argument-name))

(cl-defmethod phpi-fn-argument-type ((method phpinspect-method) argument-name)
  (phpi-fn-argument-type (phpi-method-definition method) argument-name))

(cl-defmethod phpi-fn-anonymous-p ((fn phpinspect--function))
  (phpinspect--function-anonymous-p fn))

(cl-defmethod phpi-fn-anonymous-p ((_method phpinspect-method))
  nil)

(cl-defmethod phpi-fn-token ((fn phpinspect--function))
  (phpinspect--function-token fn))

(cl-defmethod phpi-fn-token ((method phpinspect-method))
  (phpi-fn-token (phpi-method-definition method)))

(provide 'phpinspect-method-cell)
;;; phpinspect-method-cell.el ends here
