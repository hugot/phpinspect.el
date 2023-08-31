;;; phpinspect-class.el --- PHP parsing module  -*- lexical-binding: t; -*-

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

(require 'phpinspect-type)
(require 'phpinspect-class-struct)

(defmacro phpinspect--class-edit (class &rest body)
  "Declare intent to edit CLASS in BODY.

Conditionally executes BODY depending on
`phpinspect--class-read-only-p' value."
  (declare (indent 1))
  `(unless (phpinspect--class-read-only-p ,class)
     ,@body))

(cl-defmethod phpinspect--class-trigger-update ((class phpinspect--class))
  (dolist (sub (hash-table-values (phpinspect--class-subscriptions class)))
    (funcall sub class)))

(cl-defmethod phpinspect--class-update-extensions ((class phpinspect--class) extensions)
  (phpinspect--class-edit class
    (setf (phpinspect--class-extended-classes class)
          (seq-filter
           #'phpinspect--class-p
           (mapcar
            (lambda (class-name)
              (funcall (phpinspect--class-class-retriever class) class-name))
            extensions)))

    (dolist (extended (phpinspect--class-extended-classes class))
      (phpinspect--class-incorporate class extended))))


(cl-defmethod phpinspect--class-set-index ((class phpinspect--class)
                                           (index (head phpinspect--indexed-class)))
  (phpinspect--class-edit class
    (setf (phpinspect--class-declaration class) (alist-get 'declaration index))
    (setf (phpinspect--class-name class) (alist-get 'class-name index))

    ;; Override methods when class seems syntactically correct (has balanced braces)
    (when (alist-get 'complete index)
      (let ((methods (phpinspect--class-methods class))
            (static-methods (phpinspect--class-static-methods class)))

        (dolist (method (hash-table-values methods))
          (unless (phpinspect--function--inherited method)
            (remhash (phpinspect--function-name-symbol method) methods)))
        (dolist (method (hash-table-values static-methods))
          (unless (phpinspect--function--inherited method)
            (remhash (phpinspect--function-name-symbol method) static-methods)))))

    (setf (phpinspect--class-initial-index class) t)
    (setf (phpinspect--class-index class) index)

  (dolist (method (alist-get 'methods index))
    (phpinspect--class-update-method class method))

  (dolist (method (alist-get 'static-methods index))
    (phpinspect--class-update-static-method class method))

  (setf (phpinspect--class-variables class)
        (append (alist-get 'variables index)
                (alist-get 'constants index)
                (alist-get 'static-variables index)))

  (phpinspect--class-update-extensions
   class `(,@(alist-get 'implements index) ,@(alist-get 'extends index)))

  (phpinspect--class-trigger-update class)))

(cl-defmethod phpinspect--class-update-declaration
  ((class phpinspect--class) declaration imports namespace-name)
  (phpinspect--class-edit class
    (pcase-let ((`(,_ign ,class-name ,extends ,implements ,_used-types)
                 (phpinspect--index-class-declaration
                  declaration (phpinspect--make-type-resolver
                               (phpinspect--uses-to-types imports) nil namespace-name))))
      (setf (phpinspect--class-name class) class-name)
      (setf (phpinspect--class-declaration class) declaration)
      (phpinspect--class-update-extensions class `(,@extends ,@implements)))))

(cl-defmethod phpinspect--class-get-method ((class phpinspect--class) (method-name (head phpinspect-name)))
  (gethash method-name (phpinspect--class-methods class)))

(cl-defmethod phpinspect--class-get-static-method ((class phpinspect--class) (method-name (head phpinspect-name)))
  (gethash method-name (phpinspect--class-static-methods class)))

(cl-defmethod phpinspect--class-get-variable
  ((class phpinspect--class) (variable-name string))
  (catch 'found
    (dolist (variable (phpinspect--class-variables class))
      (when (string= variable-name (phpinspect--variable-name variable))
        (throw 'found variable)))))

(cl-defmethod phpinspect--class-set-variable ((class phpinspect--class)
                                              (var phpinspect--variable))
  (phpinspect--class-edit class
    (push var (phpinspect--class-variables class))))

(cl-defmethod phpinspect--class-delete-variable ((class phpinspect--class)
                                                 (var phpinspect--variable))
  (phpinspect--class-edit class
    (setf (phpinspect--class-variables class)
          (seq-filter (lambda (clvar) (not (eq var clvar)))
                      (phpinspect--class-variables class)))))

(cl-defmethod phpinspect--class-get-variables ((class phpinspect--class))
  (seq-filter #'phpinspect--variable-vanilla-p (phpinspect--class-variables class)))

(cl-defmethod phpinspect--class-get-static-variables ((class phpinspect--class))
  (seq-filter #'phpinspect--variable-static-p (phpinspect--class-variables class)))

(cl-defmethod phpinspect--class-get-constants ((class phpinspect--class))
  (seq-filter #'phpinspect--variable-const-p (phpinspect--class-variables class)))

(cl-defmethod phpinspect--add-method-copy-to-map
  ((map hash-table)
   (class-name phpinspect--type)
   (method phpinspect--function))
  (setq method (phpinspect--copy-function method))

  (setf (phpinspect--function-return-type method)
        (phpinspect--resolve-late-static-binding
         (phpinspect--function-return-type method)
         class-name))

  (puthash (phpinspect--function-name-symbol method)
           method
           map))

(cl-defmethod phpinspect--class-set-method ((class phpinspect--class)
                                            (method phpinspect--function))
  (phpinspect--class-edit class
    (phpinspect--log "Adding method by name %s to class"
                     (phpinspect--function-name method))
    (phpinspect--add-method-copy-to-map
     (phpinspect--class-methods class)
     (phpinspect--class-name class)
     method)))

(cl-defmethod phpinspect--class-set-static-method ((class phpinspect--class)
                                                   (method phpinspect--function))
  (phpinspect--class-edit class
    (phpinspect--add-method-copy-to-map
     (phpinspect--class-static-methods class)
     (phpinspect--class-name class)
     method)))

(cl-defmethod phpinspect--class-delete-method ((class phpinspect--class) (method phpinspect--function))
  (phpinspect--class-edit class
    (remhash (phpinspect--function-name-symbol method) (phpinspect--class-static-methods class))
    (remhash (phpinspect--function-name-symbol method) (phpinspect--class-methods class))))

(cl-defmethod phpinspect--class-get-method-return-type
  ((class phpinspect--class) (method-name (head phpinspect-name)))
  (let ((method (phpinspect--class-get-method class method-name)))
    (when method
      (phpinspect--function-return-type method))))

(cl-defmethod phpinspect--class-get-static-method-return-type
  ((class phpinspect--class) (method-name (head phpinspect-name)))
  (let ((method (phpinspect--class-get-static-method class method-name)))
    (when method
      (phpinspect--function-return-type method))))

(cl-defmethod phpinspect--class-get-method-list ((class phpinspect--class))
  (hash-table-values (phpinspect--class-methods class)))

(cl-defmethod phpinspect--class-get-static-method-list ((class phpinspect--class))
  (hash-table-values (phpinspect--class-static-methods class)))


(cl-defmethod phpinspect--merge-method ((class-name phpinspect--type)
                                        (existing phpinspect--function)
                                        (method phpinspect--function)
                                        &optional extended)
  (let ((new-return-type (phpinspect--resolve-late-static-binding
                          (phpinspect--function-return-type method)
                          class-name)))
    (unless (phpinspect--type= new-return-type phpinspect--null-type)
      (phpinspect--log "method return type %s" (phpinspect--function-return-type method))
      (setf (phpinspect--function-return-type existing)
            new-return-type))

    (setf (phpinspect--function--inherited existing)
          extended)

    (setf (phpinspect--function-arguments existing)
          (phpinspect--function-arguments method)))
  existing)

(cl-defmethod phpinspect--class-update-static-method ((class phpinspect--class)
                                                      (method phpinspect--function)
                                                      &optional extended)
  (phpinspect--class-edit class
    (let ((existing (gethash (phpinspect--function-name-symbol method)
                             (phpinspect--class-static-methods class))))
      (if existing
          (phpinspect--merge-method (phpinspect--class-name class) existing method extended)
        (setf (phpinspect--function--inherited method) extended)
        (phpinspect--class-set-static-method class method)))))

(cl-defmethod phpinspect--class-update-method ((class phpinspect--class)
                                               (method phpinspect--function)
                                               &optional extended)
  (phpinspect--class-edit class
    (let* ((existing (gethash (phpinspect--function-name-symbol method)
                              (phpinspect--class-methods class))))

      (if existing
          (phpinspect--merge-method (phpinspect--class-name class) existing method extended)
        (setf (phpinspect--function--inherited method) extended)
        (phpinspect--class-set-method class method)))))

;; FIXME: Remove inherited methods when they no longer exist in parent classes
;; (and/or the current class in the case of abstract methods).
(cl-defmethod phpinspect--class-incorporate ((class phpinspect--class)
                                             (other-class phpinspect--class))
  (phpinspect--class-edit class
    (dolist (method (phpinspect--class-get-method-list other-class))
      (phpinspect--class-update-method class method 'extended))

    (dolist (method (phpinspect--class-get-static-method-list other-class))
      (phpinspect--class-update-static-method class method 'extended))

    (phpinspect--class-subscribe class other-class)))

(cl-defmethod phpinspect--class-subscribe ((class phpinspect--class)
                                           (subscription-class phpinspect--class))
  (phpinspect--class-edit class
    (unless (gethash subscription-class (phpinspect--class-subscriptions class))
      (let ((update-function
             (lambda (new-class)
               (phpinspect--class-edit class
                 (phpinspect--class-incorporate class new-class)
                 (phpinspect--class-trigger-update class)))))
        (puthash subscription-class update-function
                 (phpinspect--class-subscriptions subscription-class))))))

(provide 'phpinspect-class)
;;; phpinspect-class.el ends here
