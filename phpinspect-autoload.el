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
(require 'phpinspect-project)
(require 'phpinspect-fs)

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

(cl-defstruct (phpinspect-autoloader
               (:constructor phpinspect-make-autoloader))
  (project nil
           :type phpinspect--project
           :documentation "The project that this autoloader can find files for")
  (own-types (make-hash-table :test 'eq :size 10000 :rehash-size 10000)
             :type hash-table
             :documentation "The internal types that can be
             autoloaded through this autoloader")
  (types (make-hash-table :test 'eq :size 10000 :rehash-size 10000)
         :type hash-table
         :documentation
         "The external types that can be autoloaded through this autoloader."))

(defun phpinspect-make-autoload-definition-closure (project-root fs typehash)
  "Create a closure that can be used to `maphash' the autoload section of a composer-json."
  (lambda (type prefixes)
    (let ((strategy))
      (cond
       ((string= "psr-0" type)
        (maphash
         (lambda (prefix directory-paths)
           (when (stringp directory-paths) (setq directory-paths (list directory-paths)))
           (setq strategy (phpinspect-make-psr0-generated :prefix prefix))
           (dolist (path directory-paths)
             (push (concat project-root "/" path)
                   (phpinspect-psr0-directories strategy))))
         prefixes))
       ((string= "psr-4" type)
        (maphash
         (lambda (prefix directory-paths)
           (when (stringp directory-paths) (setq directory-paths (list directory-paths)))
           (setq strategy (phpinspect-make-psr4-generated :prefix prefix))
             (dolist (path directory-paths)
               (push (concat project-root "/" path)
                     (phpinspect-psr4-directories strategy))))
         prefixes))
       (t (phpinspect--log "Unsupported autoload strategy \"%s\" encountered" type)))

      (when strategy
        (phpinspect-al-strategy-fill-typehash strategy fs typehash)))))

(cl-defmethod phpinspect--read-json-file (fs file)
  (with-temp-buffer
    (phpinspect-fs-insert-file-contents fs file)
    (goto-char 0)
    (phpinspect-json-preset (json-read))))

(cl-defmethod phpinspect-autoloader-refresh ((autoloader phpinspect-autoloader))
  "Refresh autoload definitions by reading composer.json files
  from the project and vendor folders."
  (let* ((project-root (phpinspect--project-root (phpinspect-autoloader-project autoloader)))
         (fs (phpinspect--project-fs (phpinspect-autoloader-project autoloader)))
         (vendor-dir (concat project-root "/vendor"))
         (composer-json (phpinspect--read-json-file
                         fs
                         (concat project-root "/composer.json")))
         (project-autoload (gethash "autoload" composer-json))
         (own-types (make-hash-table :test 'eq :size 10000 :rehash-size 10000))
         (types (make-hash-table :test 'eq :size 10000 :rehash-size 10000)))

    (when project-autoload
      (maphash (phpinspect-make-autoload-definition-closure project-root fs own-types)
               project-autoload)

      (maphash (phpinspect-make-autoload-definition-closure project-root fs types)
               project-autoload))

    (when (phpinspect-fs-file-directory-p fs vendor-dir)
      (dolist (author-dir (phpinspect-fs-directory-files fs vendor-dir))
        (when (phpinspect-fs-file-directory-p fs author-dir)
          (dolist (dependency-dir (phpinspect-fs-directory-files fs author-dir))
            (when (and (phpinspect-fs-file-directory-p fs dependency-dir)
                       (phpinspect-fs-file-exists-p fs (concat dependency-dir "/composer.json")))
              (let* ((dependency-json (phpinspect--read-json-file
                                       fs
                                       (concat dependency-dir "/composer.json")))
                     (dependency-autoload (gethash "autoload" dependency-json)))
                (when dependency-autoload
                  (maphash (phpinspect-make-autoload-definition-closure
                            dependency-dir fs types)
                           dependency-autoload))))))))

      (setf (phpinspect-autoloader-own-types autoloader) own-types)
      (setf (phpinspect-autoloader-types autoloader) types)))

(cl-defmethod phpinspect-autoloader-resolve ((autoloader phpinspect-autoloader)
                                            typename-symbol)
  (or (gethash typename-symbol (phpinspect-autoloader-own-types autoloader))
      (gethash typename-symbol (phpinspect-autoloader-types autoloader))))


(cl-defgeneric phpinspect-al-strategy-fill-typehash (strategy fs typehash)
  "Make STRATEGY return a map with type names as keys and the
  paths to the files they are defined in as values.")

(defsubst phpinspect-filename-to-typename (dir filename &optional prefix)
  (phpinspect-intern-name
   (concat "\\"
           (or prefix "")
           (replace-regexp-in-string
            "/" "\\\\"
            (string-remove-suffix
             ".php"
             (string-remove-prefix dir filename))))))

(cl-defmethod phpinspect-al-strategy-fill-typehash ((strategy phpinspect-psr0)
                                                    fs
                                                    typehash)
  (dolist (dir (phpinspect-psr0-directories strategy))
    (dolist (file (phpinspect-fs-directory-files-recursively fs dir "\\.php$"))
      (puthash (phpinspect-filename-to-typename dir file) file typehash))))

(cl-defmethod phpinspect-al-strategy-fill-typehash ((strategy phpinspect-psr4)
                                                    fs
                                                    typehash)
  (let ((prefix (phpinspect-psr4-prefix strategy)))
    (dolist (dir (phpinspect-psr4-directories strategy))
      (dolist (file (phpinspect-fs-directory-files-recursively fs dir "\\.php$"))
        (puthash (phpinspect-filename-to-typename dir file prefix) file typehash)))))

(provide 'phpinspect-autoload)
;;; phpinspect-autoload.el ends here
