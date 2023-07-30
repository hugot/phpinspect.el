;;; phpinspect-class.el --- PHP parsing module  -*- lexical-binding: t; -*-

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

(require 'phpinspect-type)

(cl-defstruct (phpinspect--class (:constructor phpinspect--make-class-generated))
  (project nil
           :type phpinspect-project
           :documentaton
           "The project that this class belongs to")
  (index nil
         :type phpinspect--indexed-class
         :documentation
         "The index that this class is derived from")
  (methods (make-hash-table :test 'eq :size 20 :rehash-size 20)
           :type hash-table
           :documentation
           "All methods, including those from extended classes.")
  (static-methods (make-hash-table :test 'eq :size 20 :rehash-size 20)
                  :type hash-table
                  :documentation
                  "All static methods this class provides,
                  including those from extended classes.")
  (variables nil
             :type list
             :documentation
             "Variables that belong to this class.")
  (extended-classes (make-hash-table :test 'eq)
                    :type hash-table
                    :documentation
                    "All extended/implemented classes.")
  (subscriptions nil
                 :type list
                 :documentation
                 "A list of subscription functions that should be
                 called whenever anything about this class is
                 updated")
  (initial-index nil
                 :type bool
                 :documentation
                 "A boolean indicating whether or not this class
                 has been indexed yet."))

(cl-defmethod phpinspect--class-trigger-update ((class phpinspect--class))
  (dolist (sub (phpinspect--class-subscriptions class))
    (funcall sub class)))

(cl-defmethod phpinspect--class-set-index ((class phpinspect--class)
                                           (index (head phpinspect--indexed-class)))
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

  (setf (phpinspect--class-extended-classes class)
        (seq-filter
         #'phpinspect--class-p
         (mapcar
          (lambda (class-name)
            (phpinspect-project-get-class-create (phpinspect--class-project class)
                                           class-name))
          `(,@(alist-get 'implements index) ,@(alist-get 'extends index)))))

  (dolist (extended (phpinspect--class-extended-classes class))
    (phpinspect--class-incorporate class extended)
    (phpinspect--class-subscribe class extended))

  (phpinspect--class-trigger-update class))

(cl-defmethod phpinspect--class-get-method ((class phpinspect--class) (method-name symbol))
  (gethash method-name (phpinspect--class-methods class)))

(cl-defmethod phpinspect--class-get-static-method ((class phpinspect--class) (method-name symbol))
  (gethash method-name (phpinspect--class-static-methods class)))

(cl-defmethod phpinspect--class-get-variable
  ((class phpinspect--class) (variable-name string))
  (catch 'found
    (dolist (variable (phpinspect--class-variables class))
      (when (string= variable-name (phpinspect--variable-name variable))
        (throw 'found variable)))))

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
  (phpinspect--log "Adding method by name %s to class"
                   (phpinspect--function-name method))
  (phpinspect--add-method-copy-to-map
   (phpinspect--class-methods class)
   (alist-get 'class-name (phpinspect--class-index class))
   method))

(cl-defmethod phpinspect--class-set-static-method ((class phpinspect--class)
                                                   (method phpinspect--function))
    (phpinspect--add-method-copy-to-map
   (phpinspect--class-static-methods class)
   (alist-get 'class-name (phpinspect--class-index class))
   method))

(cl-defmethod phpinspect--class-get-method-return-type
  ((class phpinspect--class) (method-name symbol))
  (let ((method (phpinspect--class-get-method class method-name)))
    (when method
      (phpinspect--function-return-type method))))

(cl-defmethod phpinspect--class-get-static-method-return-type
  ((class phpinspect--class) (method-name symbol))
  (let ((method (phpinspect--class-get-static-method class method-name)))
    (when method
      (phpinspect--function-return-type method))))

(cl-defmethod phpinspect--class-get-method-list ((class phpinspect--class))
  (hash-table-values (phpinspect--class-methods class)))

(cl-defmethod phpinspect--class-get-static-method-list ((class phpinspect--class))
  (hash-table-values (phpinspect--class-static-methods class)))


(cl-defmethod phpinspect--merge-method ((class-name phpinspect--type)
                                        (existing phpinspect--function)
                                        (method phpinspect--function))
  (let ((new-return-type (phpinspect--resolve-late-static-binding
                          (phpinspect--function-return-type method)
                          class-name)))
    (unless (phpinspect--type= new-return-type phpinspect--null-type)
      (phpinspect--log "method return type %s" (phpinspect--function-return-type method))
      (setf (phpinspect--function-return-type existing)
            new-return-type))

    (setf (phpinspect--function-arguments existing)
          (phpinspect--function-arguments method)))
  existing)

(cl-defmethod phpinspect--class-update-static-method ((class phpinspect--class)
                                                      (method phpinspect--function))
  (let ((existing (gethash (phpinspect--function-name-symbol method)
                           (phpinspect--class-static-methods class))))
    (if existing
        (phpinspect--merge-method
         (alist-get 'class-name (phpinspect--class-index class))
         existing method)
      (phpinspect--class-set-static-method class method))))

(cl-defmethod phpinspect--class-update-method ((class phpinspect--class)
                                               (method phpinspect--function))
  (let* ((existing (gethash (phpinspect--function-name-symbol method)
                            (phpinspect--class-methods class))))

    (if existing
        (phpinspect--merge-method
         (alist-get 'class-name (phpinspect--class-index class))
         existing method)
      (phpinspect--class-set-method class method))))

(cl-defmethod phpinspect--class-incorporate ((class phpinspect--class)
                                             (other-class phpinspect--class))

  (dolist (method (phpinspect--class-get-method-list other-class))
    (phpinspect--class-update-method class method))

    (dolist (method (phpinspect--class-get-static-method-list other-class))
      (phpinspect--class-update-static-method class method)))



(cl-defmethod phpinspect--class-subscribe ((class phpinspect--class)
                                           (subscription-class phpinspect--class))
  (let ((update-function
         (lambda (new-class)
           (phpinspect--class-incorporate class new-class)
           (phpinspect--class-trigger-update class))))
    (push update-function (phpinspect--class-subscriptions subscription-class))))

(provide 'phpinspect-class)
;;; phpinspect-class.el ends here
