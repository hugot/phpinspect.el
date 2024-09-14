;;; phpinspect-fs.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(defconst phpinspect--cat-executable (executable-find "cat")
  "The executable used to read files asynchronously from the filesystem.")

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

(defun phpinspect-virtual-fs-set-file (fs path contents)
  (declare (indent defun))
  (puthash path (phpinspect-make-virtual-file contents)
           (phpinspect-virtual-fs-files fs)))

(cl-defgeneric phpinspect-fs-file-exists-p (fs file))
(cl-defgeneric phpinspect-fs-file-directory-p (fs file))
(cl-defgeneric phpinspect-fs-file-modification-time (fs file))
(cl-defgeneric phpinspect-fs-insert-file-contents (fs file &optional prefer-async)
  "Insert file contents of FILE.

When PREFER-ASYNC is set and FS supports it, effort is made to
execute the insertion asynchronously in scenario's where this can
prevent the main thread (or other running threads) from stalling
while the current thread executes. When running in the main
thread, PREFER-ASYNC has no effect.")
(cl-defgeneric phpinspect-fs-directory-files (fs directory match))
(cl-defgeneric phpinspect-fs-directory-files-recursively (fs directory match))

(cl-defmethod phpinspect-fs-file-exists-p ((_fs phpinspect-fs) file)
  (file-exists-p file))

(cl-defmethod phpinspect-fs-file-exists-p ((fs phpinspect-virtual-fs) file)
  (and (gethash file (phpinspect-virtual-fs-files fs)) t))

(cl-defmethod phpinspect-fs-file-directory-p ((_fs phpinspect-fs) file)
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

(cl-defmethod phpinspect-fs-file-modification-time ((_fs phpinspect-fs) file)
  (let ((attributes (file-attributes file)))
    (when attributes
      (file-attribute-modification-time attributes))))


(defsubst phpinspect--insert-file-contents-asynchronously (file)
  "Inserts FILE contents into the current buffer asynchronously,
while blocking the current thread.

Errors when executed in main thread, as it should be used to make
background operations less invasive. Usage in the main thread can
only be the result of a logic error."
  (let* ((thread (current-thread))
         (mx (make-mutex))
         (condition (make-condition-variable mx))
         (err)
         (sentinel
          (lambda (process event)
            (with-mutex mx
              (if (string-match-p "^\\(deleted\\|exited\\|failed\\|connection\\)" event)
                  (progn
                    (setq err (format "cat process %s failed with event: %s" process event))
                    (condition-notify condition))
                (when (string-match-p "^finished" event)
                  (condition-notify condition)))))))
    (when (not phpinspect--cat-executable)
      (error
       "ERROR: phpinspect--insert-file-contents-asynchronously called when cat-executable is not set"))

    (when (eq thread main-thread)
      (error "ERROR: phpinspect--insert-file-contents-asynchronously called from main-thread"))

    (with-mutex mx
      (make-process :name "phpinspect--insert-file-contents-asynchronously"
                    :command `(,phpinspect--cat-executable ,file)
                    :buffer (current-buffer)
                    :sentinel sentinel)

      (condition-wait condition)
      (when err (error err)))))

(cl-defmethod phpinspect-fs-insert-file-contents ((_fs phpinspect-fs) file &optional prefer-async)
  "Insert file contents from FILE. "
  (if (and prefer-async (not (eq (current-thread) main-thread))
           phpinspect--cat-executable)
      (phpinspect--insert-file-contents-asynchronously file)
    (insert-file-contents-literally file)))

(cl-defmethod phpinspect-fs-insert-file-contents ((fs phpinspect-virtual-fs) file &optional _ignored)
  (let ((file-obj (gethash file (phpinspect-virtual-fs-files fs))))
    (when file (insert (or (phpinspect-virtual-file-contents file-obj) "")))))

(cl-defmethod phpinspect-fs-directory-files ((_fs phpinspect-fs) directory &optional match)
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

(cl-defmethod phpinspect-fs-directory-files-recursively ((_fs phpinspect-fs) directory &optional match)
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
