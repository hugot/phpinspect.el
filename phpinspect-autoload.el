;;; phpinspect-autoload.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(require 'cl-lib)

(cl-defstruct (phpinspect-autoloader
               (:constructor phpinspect-make-autoloader))
  (types (make-hash-table :test 'eq)
         :type hash-table
         :documentation
         "The types that can be autoloaded through this autoloader.")
  (strategies nil
              :type list
              :documentation
              "The strategies that this autoloader can employ for its purpose."))

(cl-defstruct (phpinspect-directory
               (:constructor phpinspect-make-directory))
  (location nil
            :type string
            :documentation
            "The filepath to the directory"))

(cl-defstruct (phpinspect-virtual-directory
               (:constructor phpinspect-make-virtual-directory))
  (location nil
            :type string
            :documentation
            "The filepath to the directory")
  (files (make-hash-table :test 'equal)
         :type hash-table
         :documentation
         "The files in the virtual directory"))

(cl-defgeneric phpinspect-directory-list-files (directory)
  "List all PHP files in DIRECTORY")

(cl-defmethod phpinspect-directory-list-files ((dir phpinspect-directory))
  "List all PHP files in DIR."
  (directory-files-recursively (phpinspect-directory-location dir)
                               ".*.php$"
                               t ;; Ignore directories that cannot be read
                               t ;; follow symlinks
                               ))

(cl-defmethod phpinspect-directory-list-files ((dir phpinspect-virtual-directory))
  "List all virtual files that DIR contains."
  (let ((files))
    (maphash (lambda (file _)
               (push file files))
             (phpinspect-virtual-directory-files dir))
    files))

(cl-defgeneric phpinspect-directory-list-files (directory)
  "List all PHP files in DIRECTORY")

(cl-defmethod phpinspect-directory-get-location ((dir phpinspect-directory))
  "Get location of PHP dir."
  (phpinspect-directory-location dir))

(cl-defmethod phpinspect-directory-get-location ((dir phpinspect-virtual-directory))
  "List all virtual files that DIR contains."
  (phpinspect-virtual-directory-location dir))

(cl-defmethod phpinspect-directory-insert-file-contents ((dir phpinspect-directory)
                                                         (file string))
  (insert-file-contents file))

(cl-defmethod phpinspect-directory-insert-file-contents ((dir phpinspect-virtual-directory)
                                                         (file string))
  (insert (gethash file (phpinspect-virtual-directory-files dir))))

(cl-defstruct (phpinspect-psr0
               (:constructor phpinspect-make-psr0-generated))
  (prefix nil
          :type string
          :documentation "The namespace prefix for which the directories contain code.")
  (directories nil
             :type list
             :documentation
             "The directories that this autoloader finds code in."))

(cl-defstruct (phpinspect-psr4
               (:constructor phpinspect-make-psr4-generated))
  (prefix nil
          :type string
          :documentation "The namespace prefix for which the directories contain code.")
  (directories nil
             :type list
             :documentation
             "The directories that this autoloader finds code in."))

(cl-defstruct (phpinspect-classmap
               (:constructor phpinspect-make-classmap-generated))
  (directories nil
             :type list
             :documentation
             "The directories that this autoloader finds code in."))

(cl-defgeneric phpinspect-al-strategy-fill-typehash (strategy typehash)
  "Make STRATEGY return a map with type names as keys and the
  paths to the files they are defined in as values.")

(defsubst phpinspect-filename-to-typename (dir filename &optional prefix)
  (phpinspect-intern-name
   (concat "\\"
           (or prefix "")
           (replace-regexp-in-string "/"
                                     "\\\\"
                                     (string-remove-suffix
                                      ".php"
                                      (string-remove-prefix
                                       (phpinspect-directory-get-location dir)
                                       filename))))))

(cl-defmethod phpinspect-al-strategy-fill-typehash ((strategy phpinspect-psr0)
                                                     typehash)
  (dolist (dir (phpinspect-psr0-directories strategy))
    (dolist (file (phpinspect-directory-list-files dir))
      (puthash (phpinspect-filename-to-typename dir file) file typehash))))

(cl-defmethod phpinspect-al-strategy-fill-typehash ((strategy phpinspect-psr4)
                                                    typehash)
  (let ((prefix (phpinspect-psr4-prefix strategy)))
    (dolist (dir (phpinspect-psr4-directories strategy))
      (dolist (file (phpinspect-directory-list-files dir))
        (puthash (phpinspect-filename-to-typename dir file prefix) file typehash)))))

(provide 'phpinspect-autoload)
;;; phpinspect-autoload.el ends here
