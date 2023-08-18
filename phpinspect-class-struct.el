;;; phpinspect-class-struct.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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


(cl-defstruct (phpinspect--class (:constructor phpinspect--make-class-generated))
  (class-retriever nil
                   :type lambda
                   :documentaton
                   "A function that returns classes for types
(should accept `phpinspect--type' as argument)")

  (read-only-p nil
               :type boolean
               :documentation
               "Whether this class instance is read-only, meaning that its data
should never be changed. Methods and functions that are meant to
manipulate class data should become no-ops when this slot has a
non-nil value.")
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
  (name nil
        :type phpinspect--type)
  (variables nil
             :type list
             :documentation
             "Variables that belong to this class.")
  (extended-classes nil
                    :type list
                    :documentation
                    "All extended/implemented classes.")
  (subscriptions (make-hash-table :test #'eq :size 10 :rehash-size 1.5)
                 :type hash-table
                 :documentation
                 "A list of subscription functions that should be
                 called whenever anything about this class is
                 updated")
  (declaration nil)
  (initial-index nil
                 :type bool
                 :documentation
                 "A boolean indicating whether or not this class
                 has been indexed yet."))


(provide 'phpinspect-class-struct)
