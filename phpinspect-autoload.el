;;; phpinspect-autoload.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'phpinspect-fs)
(require 'phpinspect-util)
(require 'phpinspect-type)
(require 'phpinspect-pipeline)
(require 'json)

(cl-defstruct (phpinspect-psr0
               (:constructor phpinspect-make-psr0-generated))
  (prefix nil
          :type string
          :documentation "The namespace prefix for which the directories contain code.")
  (autoloader nil
              :type phpinspect-autoloader)
  (own nil
       :type boolean)
  (fs nil
      :type phpinspect-fs)
  (directories nil
             :type list
             :documentation
             "The directories that this autoloader finds code in."))

(cl-defstruct (phpinspect-psr4
               (:constructor phpinspect-make-psr4-generated))
  (prefix nil
          :type string
          :documentation "The namespace prefix for which the directories contain code.")
  (autoloader nil
              :type phpinspect-autoloader)
  (own nil
       :type boolean)
  (fs nil
      :type phpinspect-fs)
  (directories nil
             :type list
             :documentation
             "The directories that this autoloader finds code in."))

(cl-defstruct (phpinspect-files (:constructor phpinspect-make-files))
  (autoloader nil)
  (list nil
        :type list
        :documentation
        "List of files to be indexed"))

(cl-defstruct (phpinspect-autoloader
               (:constructor phpinspect-make-autoloader))
  (refresh-thread nil
                  :type thread)
  (fs nil
      :type phpinspect-fs)
  (file-indexer nil
                :type function)
  (project-root-resolver nil
                         :type function)
  (own-types (make-hash-table :test 'eq :size 10000 :rehash-size 10000)
             :type hash-table
             :documentation "The internal types that can be
             autoloaded through this autoloader")
  (types (make-hash-table :test 'eq :size 10000 :rehash-size 10000)
         :type hash-table
         :documentation
         "The external types that can be autoloaded through this autoloader.")
  (type-name-fqn-bags (make-hash-table :test 'eq :size 3000 :rehash-size 3000)
                      :type hash-table
                      :documentation
                      "Hash table that contains lists of fully
qualified names congruent with a bareword type name. Keyed by
bareword typenames."))

;; FIXME: This is another scenario where an LRU Cache might come in handy (we
;; don't want to re-compare string prefixes everytime the same namespace is
;; checked).
(defun phpinspect-autoloader-get-own-types-in-namespace (al namespace)
  (cl-assert (stringp namespace))
  (let ((namespace-fqn (phpinspect--resolve-type-name nil nil namespace))
	types)
    (dolist (name (hash-table-keys (phpinspect-autoloader-own-types al)))
      (when (string-prefix-p namespace-fqn (phpinspect-name-string name))
	(push (phpinspect--make-type-generated :name-symbol name :fully-qualified t)
	      types)))
    types))

(cl-defmethod phpinspect--read-json-file (fs file)
  (with-temp-buffer
    (phpinspect-fs-insert-file-contents fs file)
    (goto-char 0)
    (phpinspect-json-preset (json-read))))

(define-inline phpinspect-filename-to-typename (dir filename &optional prefix)
  (inline-quote
   (phpinspect-intern-name
    (replace-regexp-in-string
     "\\\\[\\]+"
     "\\\\"
     (concat "\\"
             (or ,prefix "")
             (replace-regexp-in-string
              "/" "\\\\"
              (string-remove-suffix
               ".php"
               (string-remove-prefix ,dir ,filename))))))))

(defun phpinspect-find-composer-json-files (fs project-root)
  "Find all composer.json files that are relevant for a project.

Usually, the relevant files are the current project's
composer.json and the composer.json files of all dependencies in
the vendor directory."
  (let ((cj-path (concat project-root "/composer.json"))
        (vendor-dir (concat project-root "/vendor"))
        files)
    (when (phpinspect-fs-file-exists-p fs cj-path)
      (push `(local . ,cj-path) files))

    (when (phpinspect-fs-file-directory-p fs vendor-dir)
      (dolist (author-dir (phpinspect-fs-directory-files fs vendor-dir))
        (when (and (phpinspect-fs-file-directory-p fs author-dir)
                   ;; Exclude current/parent directory
                   (not (member (file-name-base author-dir) (list ".." "."))))
          (dolist (dependency-dir (phpinspect-fs-directory-files fs author-dir))
            (setq cj-path (concat dependency-dir "/composer.json"))
            (when (and (phpinspect-fs-file-directory-p fs dependency-dir)
                       (phpinspect-fs-file-exists-p fs cj-path))
              (push `(vendor . ,cj-path) files))))))
    files))

(cl-defmethod phpinspect-al-strategy-execute ((strat phpinspect-psr4))
  (let* ((fs (phpinspect-psr4-fs strat))
         (al (phpinspect-psr4-autoloader strat))
         (own (phpinspect-psr4-own strat))
         (own-typehash (phpinspect-autoloader-own-types al))
         (typehash (phpinspect-autoloader-types al))
         (prefix (phpinspect-psr4-prefix strat))
         type-fqn)
    (dolist (dir (phpinspect-psr4-directories strat))
      (when (phpinspect-fs-file-directory-p fs dir)
        (dolist (file (phpinspect-fs-directory-files-recursively fs dir "\\.php$"))
          (setq type-fqn (phpinspect-filename-to-typename dir file prefix))
          (phpinspect-autoloader-put-type-bag al type-fqn)
          (puthash type-fqn file typehash)
          (when own
            (puthash type-fqn file own-typehash)))))))

(cl-defmethod phpinspect-al-strategy-execute ((strat phpinspect-psr0))
  (let* ((fs (phpinspect-psr0-fs strat))
         (al (phpinspect-psr0-autoloader strat))
         (own (phpinspect-psr0-own strat))
         (own-typehash (phpinspect-autoloader-own-types al))
         (typehash (phpinspect-autoloader-types al))
         type-fqn)
    (dolist (dir (phpinspect-psr0-directories strat))
      (dolist (file (phpinspect-fs-directory-files-recursively fs dir "\\.php$"))
        (setq type-fqn (phpinspect-filename-to-typename dir file))
        (phpinspect-autoloader-put-type-bag al type-fqn)
        (puthash type-fqn file typehash)
        (when own
          (puthash type-fqn file own-typehash))))))

(cl-defmethod phpinspect-al-strategy-execute ((strat phpinspect-files))
  (phpinspect--log "indexing files list: %s" (phpinspect-files-list strat))
  (let* ((indexer (phpinspect-autoloader-file-indexer (phpinspect-files-autoloader strat)))
         (wrapped-indexer (lambda (file)
                            (condition-case-unless-debug err
                                (funcall indexer file)
                              (t (phpinspect--log "Error indexing file %s: %s" file err))))))
    (phpinspect-pipeline (phpinspect-files-list strat)
      :into (funcall :with-context wrapped-indexer))))

(cl-defmethod phpinspect-autoloader-put-type-bag ((al phpinspect-autoloader) (type-fqn (head phpinspect-name)))
  (let* ((type-name (phpinspect-intern-name
                     (car (last (split-string (phpinspect-name-string type-fqn) "\\\\")))))
         (bag (gethash type-name (phpinspect-autoloader-type-name-fqn-bags al))))
    (if bag
        (setcdr bag (cons type-fqn (cdr bag)))
      (push type-fqn bag)
      (puthash type-name bag (phpinspect-autoloader-type-name-fqn-bags al)))))

(defun phpinspect-autoloader-get-type-names (al)
  (hash-table-keys (phpinspect-autoloader-type-name-fqn-bags al)))

(cl-defmethod phpinspect-autoloader-get-type-bag ((al phpinspect-autoloader) (type-name (head phpinspect-name)))
  (gethash type-name (phpinspect-autoloader-type-name-fqn-bags al)))

(cl-defmethod phpinspect-iterate-composer-jsons
  ((al phpinspect-autoloader) file)
  (let* ((fs (phpinspect-autoloader-fs  al))
         (project-root (file-name-directory (cdr file)))
         json batch)

    (condition-case err
        (setq json (phpinspect--read-json-file fs (cdr file)))
      (t (phpinspect-message "Error parsing composer json at %s : %s " (cdr file) err)))

    (when json
      (dolist (autoload (list (gethash "autoload" json)
                              (when (eq 'local (car file)) (gethash "autoload-dev" json))))
        (when (hash-table-p autoload)
          (maphash
           (lambda (type prefixes)
             (let ((strategy))
               (pcase type
                 ("psr-0"
                  (maphash
                   (lambda (prefix directory-paths)
                     (when (stringp directory-paths)
                       (setq directory-paths (list directory-paths)))
                     (setq strategy (phpinspect-make-psr0-generated
                                     :autoloader al
                                     :fs fs
                                     :prefix prefix
                                     :own (eq 'local (car file))))
                     (dolist (path directory-paths)
                       (push (file-name-concat project-root path)
                             (phpinspect-psr0-directories strategy)))
                     (push strategy batch))
                   prefixes))
                 ("psr-4"
                  (maphash
                   (lambda (prefix directory-paths)
                     (when (stringp directory-paths)
                       (setq directory-paths (list directory-paths)))
                     (setq strategy (phpinspect-make-psr4-generated
                                     :fs fs
                                     :autoloader al
                                     :prefix prefix
                                     :own (eq 'local (car file))))
                     (dolist (path directory-paths)
                       (push (file-name-concat project-root path)
                             (phpinspect-psr4-directories strategy)))
                     (push strategy batch))
                   prefixes))
                 ("files"
                  (setq strategy
                        (phpinspect-make-files
                         :list (mapcar
                                (lambda (file) (file-name-concat project-root file))
                                prefixes)
                         :autoloader al))
                  (push strategy batch))
                 (_ (phpinspect--log "Unsupported autoload strategy \"%s\" encountered" type)))))
           autoload)))
      (phpinspect--log "Batch: %s" (length batch))
      (phpinspect-pipeline-emit-all batch))))


(cl-defmethod phpinspect-autoloader-resolve ((autoloader phpinspect-autoloader)
                                             (typename (head phpinspect-name)))
  ;; Wait for pending refresh if not running in main thread.
  (unless (eq main-thread (current-thread))
    (when (and (phpinspect-autoloader-refresh-thread autoloader)
               (thread-live-p (phpinspect-autoloader-refresh-thread autoloader)))
      (phpinspect--log
       "Pausing thread %s to await autoload refresh completion"
       (thread-name (current-thread)))
      (thread-join (phpinspect-autoloader-refresh-thread autoloader))
      (phpinspect--log "Autoload refresh completed, continuing waiting thread %s"
                       (thread-name (current-thread)))))

  (or (gethash typename (phpinspect-autoloader-own-types autoloader))
      (gethash typename (phpinspect-autoloader-types autoloader))))

(cl-defmethod phpinspect-autoloader-refresh ((autoloader phpinspect-autoloader) &optional async-callback)
  "Refresh autoload definitions by reading composer.json files
  from the project and vendor folders."
  (let* ((project-root (funcall (phpinspect-autoloader-project-root-resolver autoloader)))
         (fs (phpinspect-autoloader-fs autoloader)))
    (setf (phpinspect-autoloader-type-name-fqn-bags autoloader)
          (make-hash-table :test 'eq :size 3000 :rehash-size 3000))
    (setf (phpinspect-autoloader-own-types autoloader)
          (make-hash-table :test 'eq :size 10000 :rehash-size 10000))
    (setf (phpinspect-autoloader-types autoloader)
          (make-hash-table :test 'eq :size 10000 :rehash-size 10000))

    (setf (phpinspect-autoloader-refresh-thread autoloader)
          (phpinspect-pipeline (phpinspect-find-composer-json-files fs project-root)
            :async (or async-callback
                       (lambda (_result error)
                         (if error
                             (phpinspect-message "Error during autoloader refresh: %s" error)
                           (phpinspect-message
                            (concat "Refreshed project autoloader. Found %d types within project,"
                                    " %d types total.")
                            (hash-table-count (phpinspect-autoloader-own-types autoloader))
                            (hash-table-count (phpinspect-autoloader-types autoloader))))))
            :into (phpinspect-iterate-composer-jsons :with-context autoloader)
            :into phpinspect-al-strategy-execute))))

(provide 'phpinspect-autoload)
;;; phpinspect-autoload.el ends here
