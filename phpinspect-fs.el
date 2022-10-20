;;; phpinspect-fs.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(cl-defstruct (phpinspect-fs (:constructor phpinspect-make-fs)))

(cl-defstruct (phpinspect-virtual-fs (:constructor phpinspect-make-virtual-fs))
  "A rough in-memory filesystem. Useful for testing."
  (files (make-hash-table :test 'equal)
                :type hash-table
                :documentation
                "The files in the virtual filesystem"))

(defsubst phpinspect-make-virtual-file (contents)
  (list contents (current-time)))

(defalias 'phpinspect-virtual-file-modification-time #'cadr)
(defalias 'phpinspect-virtual-file-contents #'car)

(cl-defmethod phpinspect-virtual-fs-set-file ((fs phpinspect-virtual-fs)
                                              path
                                              contents)
  (puthash path (phpinspect-make-virtual-file contents)
           (phpinspect-virtual-fs-files fs)))

(cl-defgeneric phpinspect-fs-file-exists-p (fs file))
(cl-defgeneric phpinspect-fs-file-directory-p (fs file))
(cl-defgeneric phpinspect-fs-file-modification-time (fs file))
(cl-defgeneric phpinspect-fs-insert-file-contents (fs file))
(cl-defgeneric phpinspect-fs-directory-files (fs directory match))
(cl-defgeneric phpinspect-fs-directory-files-recursively (fs directory match))

(cl-defmethod phpinspect-fs-file-exists-p ((fs phpinspect-fs) file)
  (file-exists-p file))

(cl-defmethod phpinspect-fs-file-exists-p ((fs phpinspect-virtual-fs) file)
  (and (gethash file (phpinspect-virtual-fs-files fs)) t))

(cl-defmethod phpinspect-fs-file-directory-p ((fs phpinspect-fs) file)
  (file-directory-p file))

(cl-defmethod phpinspect-fs-file-directory-p ((fs phpinspect-virtual-fs) file)
  (setq file (concat (string-remove-suffix "/" file) "/"))
  (let ((is-directory? nil))
    (maphash
     (lambda (existing-file _ignored)
       (when (string-prefix-p (file-name-directory existing-file)
                              file)
         (setq is-directory? t)))
     (phpinspect-virtual-fs-files fs))
    is-directory?))

(cl-defmethod phpinspect-fs-file-modification-time ((fs phpinspect-virtual-fs) file)
  (let ((file (gethash file (phpinspect-virtual-fs-files fs))))
    (when file
      (phpinspect-virtual-file-modification-time file))))

(cl-defmethod phpinspect-fs-file-modification-time ((fs phpinspect-fs) file)
  (let ((attributes (file-attributes file)))
    (when attributes
      (file-attribute-modification-time attributes))))

(cl-defmethod phpinspect-fs-insert-file-contents ((fs phpinspect-fs) file)
  (insert-file-contents-literally file))

(cl-defmethod phpinspect-fs-insert-file-contents ((fs phpinspect-virtual-fs) file)
  (let ((file-obj (gethash file (phpinspect-virtual-fs-files fs))))
    (when file (insert (or (phpinspect-virtual-file-contents file-obj) "")))))

(cl-defmethod phpinspect-fs-directory-files ((fs phpinspect-fs) directory &optional match)
  (directory-files directory t match t))

(cl-defmethod phpinspect-fs-directory-files ((fs phpinspect-virtual-fs) directory &optional match)
  (setq directory  (replace-regexp-in-string "[/]+" "/" (concat directory "/")))
  (let ((files))
    (maphash
     (lambda (file _ignored)
       (let ((basename (replace-regexp-in-string "/.*$" "" (string-remove-prefix directory file))))
         (when (and (string-prefix-p directory file)
                    (string-match-p "^[^/]*$" basename)
                    (if match (string-match-p match basename) t))
           (cl-pushnew (concat directory basename) files :test #'string=))))
     (phpinspect-virtual-fs-files fs))
    files))

(cl-defmethod phpinspect-fs-directory-files-recursively ((fs phpinspect-fs) directory &optional match)
  (directory-files-recursively directory
                               match
                               t ;; Ignore directories that cannot be read
                               t ;; follow symlinks
                               ))

(cl-defmethod phpinspect-fs-directory-files-recursively ((fs phpinspect-virtual-fs) directory &optional match)
  (setq directory  (replace-regexp-in-string "[/]+" "/" (concat directory "/")))
  (let ((files))
    (maphash
     (lambda (file _ignored)
       (when (and (string-prefix-p directory file)
                  (if match (string-match-p match file) t))
         (push file files)))
     (phpinspect-virtual-fs-files fs))
    files))

(provide 'phpinspect-fs)
;;; phpinspect-fs.el ends here
