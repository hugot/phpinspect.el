;;; phpinspect-property-cell.el --- Models for PHP property definitions  -*- lexical-binding: t; -*-

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

;; There's a good amount of code from phpinspect-method-cell.el copied over to
;; this one with small modifications to cater to class properties instead of
;; methods. The alternative would have been to create some kind of reusable
;; generic implementation, but the subtle differences in the ways property
;; inheritance and method inheritance work makes this more complicated than it's
;; worth.

;;; Code:

(require 'cl-macs)
(require 'phpinspect-type)

(cl-defstruct (phpinspect-property
	       (:constructor phpinspect-make-property-generated)
	       (:conc-name phpi-prop-))
  (origin-type nil
	       :type phpinspect--type)
  (name nil
	:type phpinspect-name)
  (definition nil
	      :type phpinspect--variable))

(defun phpinspect-make-property (origin-type definition)
  (phpinspect-make-property-generated
   :name (phpinspect-intern-name (phpinspect--variable-name definition))
   :origin-type origin-type
   :definition definition))

(cl-defstruct (phpinspect-property-cell
	       (:constructor phpinspect-make-property-cell)
	       (:conc-name phpi-pc-))
  (own nil
       :type phpinspect-property)
  (inherited nil
	     :type phpinspect-property))

(cl-defstruct(phpinspect-property-collection
	      (:constructor phpinspect-make-property-collection)
	      (:conc-name phpi-pcol-))
  (home-type nil
	     :type phpinspect--type)
  (cells nil
	 :type list
	 :documentation "<string,phpinspect-property-cell>"))

(defun phpi-pcol-find-cell (pcol prop-name &optional remove)
  (cl-assert (phpinspect-name-p prop-name))

  (alist-get prop-name (phpi-pcol-cells pcol) nil remove #'eq))

(defun phpi-pcol-find-cell-create (pcol prop-name)
  (let ((cell (phpi-pcol-find-cell pcol prop-name)))
    (unless cell
      (setq cell (phpinspect-make-property-cell))
      (push (cons prop-name cell) (phpi-pcol-cells pcol)))

    cell))

(defun phpi-prop-inheritable-p (prop)
  (and-let* ((var (phpi-prop-definition prop))
	     (scope (phpinspect--variable-scope var))
	     ((or (phpinspect-protected-p scope)
		  (phpinspect-public-p scope))))))

(defun phpi-pc-set (cell home-type origin-type property)
  (if (phpinspect--type= home-type origin-type)
      (setf (phpi-pc-own cell) property)
    ;; Prop is not from home-type, only add if it can be inherited.
    (when (or (not property) ;; Deletion, always allowed
	      (phpi-prop-inheritable-p property)) ;; Update, only when inheritable
      (setf (phpi-pc-inherited cell) property))))

(defun phpi-pcol-add (pcol property)
  (let ((cell (phpi-pcol-find-cell-create pcol (phpi-prop-name property))))
    (phpi-pc-set
     cell (phpi-pcol-home-type pcol) (phpi-prop-origin-type property) property)))

(defun phpi-pcol-list-active (pcol)
  (let (active)
    (dolist (cons (phpi-pcol-cells pcol))
      (when-let ((prop (phpi-pc-get-active (cdr cons))))
	(push prop active)))

    active))

(defun phpi-pc-empty-p (cell)
  "CELL is empty when it contains no methods."
  (not (phpi-pc-get-active cell)))

(defun phpi-pc-get-active (cell)
  "Get active property from CELL, according to PHP precendence order."
  (or (phpi-pc-own cell)
      (phpi-pc-inherited cell)))

(defun phpi-pcol-get-active-property (mcol name)
  (when-let ((cell (phpi-pcol-find-cell mcol name)))
    (phpi-pc-get-active cell)))

(defun phpi-pcol-list-own (pcol)
  (let (list)
    (dolist (cons (phpi-pcol-cells pcol))
      (when-let ((prop (phpi-pc-own (cdr cons))))
        (push prop list)))
    list))

(defun phpi-pcol-set-home-type (pcol type)
  "Set home type of PCOL to TYPE.

Also updates all members of PCOL with the same origin-type."
  (setf (phpi-pcol-home-type pcol) type)
  (dolist (prop (phpi-pcol-list-own pcol))
    (setf (phpi-prop-origin-type prop) type)))

(defun phpi-pc-get-for-type (cell type)
  (cl-block try-get
    (dolist (prop (list (phpi-pc-own cell)
			(phpi-pc-inherited cell)))
      (when (and prop (phpinspect--type= type (phpi-prop-origin-type prop)))
	(cl-return-from try-get prop)))))

(defun phpi-pcol-delete (pcol prop-name)
  (phpi-pcol-find-cell pcol prop-name t))

(defun phpi-pcol-delete-for-type (pcol type &optional name)

    "Delete from PCOL all properties that originate from TYPE.

When NAME is provided, only property with NAME is deleted."
  (if name
      ;; Name is provided, only delete method with NAME.
      (when-let ((cell (phpi-pcol-find-cell pcol name)))
        (when (phpi-pc-get-for-type cell type)
          (phpi-pc-set cell (phpi-pcol-home-type pcol) type nil))

        (when (phpi-pc-empty-p cell)
          (phpi-pcol-delete pcol name)))

    (let ((cells (phpi-pcol-cells pcol)))
      (dolist (cons cells)
        (let ((cell (cdr cons)))
          (when (phpi-pc-get-for-type cell type)
            (phpi-pc-set cell (phpi-pcol-home-type pcol) type nil))

          ;; mark cell for deletion by setting car and cdr of alist member to
          ;; nil
          (when (phpi-pc-empty-p cell)
            (setcar cons nil)
            (setcdr cons nil))))
      ;; Delete all empty cells
      (setf (phpi-pcol-cells pcol) (delete (cons nil nil) cells)))))

(defun phpi-prop-static-p (prop)
  (phpinspect--variable-static-p (phpi-prop-definition prop)))

(defun phpi-prop-const-p (prop)
  (phpinspect--variable-const-p (phpi-prop-definition prop)))

(defun phpi-prop-vanilla-p (prop)
  (phpinspect--variable-vanilla-p (phpi-prop-definition prop)))

(defun phpi-prop-type (prop)
  (phpinspect--variable-type (phpi-prop-definition prop)))

(defun phpi-prop-scope (prop)
  (phpinspect--variable-scope (phpi-prop-definition prop)))

;; helper functions
(cl-defmethod phpi-var-type ((var phpinspect--variable))
  (phpinspect--variable-type var))

(cl-defmethod phpi-var-type ((prop phpinspect-property))
  (phpi-prop-type prop))

(cl-defmethod phpi-var-name ((var phpinspect--variable))
  (phpinspect--variable-name var))

(cl-defmethod phpi-var-name ((prop phpinspect-property))
  (phpinspect-name-string (phpi-prop-name prop)))

(cl-defmethod phpi-var-vanilla-p ((variable phpinspect--variable))
  (phpinspect--variable-vanilla-p variable))

(cl-defmethod phpi-var-vanilla-p ((prop phpinspect-property))
  (phpi-prop-vanilla-p prop))

(provide 'phpinspect-property-cell)
;;; phpinspect-property-cell.el ends here
