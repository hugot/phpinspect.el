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

(cl-defstruct (phpinspect-psrX
               (:constructor phpinspect-make-psrX-generated))
  "A base structure to be included in PSR autoload strategy
structures."
  (autoloader nil
              :type phpinspect-autoloader)
  (own nil
       :type boolean)
  (fs nil
      :type phpinspect-fs)
  (directories nil
             :type list
             :documentation
             "The directories that this autoloader finds code in.")
  (types-found
   nil
   :type list
   :documentation
   "List of fully qualified type names that was found during the last
execution of this strategy."))

(cl-defstruct (phpinspect-psr0
               (:constructor phpinspect-make-psr0-generated)
               (:include phpinspect-psrX))
  "PSR0 autoload strategy.")

(cl-defstruct (phpinspect-psr4
               (:constructor phpinspect-make-psr4-generated)
               (:include phpinspect-psrX))
  "PSR4 autoload strategy."
    (prefix nil
          :type string
          :documentation "The namespace prefix for which the directories contain code."))

(cl-defmethod phpinspect-psrX-filename-to-typename ((psr4 phpinspect-psr4) directory filename)
  (phpinspect-filename-to-typename directory filename (phpinspect-psr4-prefix psr4)))

(cl-defmethod phpinspect-psrX-filename-to-typename ((_psr0 phpinspect-psr0) directory filename)
  (phpinspect-filename-to-typename directory filename))

(cl-defstruct (phpinspect-files (:constructor phpinspect-make-files))
  (autoloader nil)
  (list nil
        :type list
        :documentation
        "List of files to be indexed"))

(cl-defgeneric phpinspect-al-strategy-request-type-name (_strategy _file-name)
  "Returns FQN when STRATEGY is responsible for autoloading FILE-NAME.

STRATEGY is responsible for autoloading FILE-NAME when FILE-NAME
is in a (sub)directory that STRATEGY resolves types for.

This function returns a fully qualified type name regardless of
whether the type name is currently known to the autoloader or
not. To check if the type name is known to the autoloader, use
`phpinspect-autoloader-resolve'. Returned type names are inferred
from FILE-NAME.

Note: at the moment, this function only returns type names for
PSR autoload strategies. The nature of \"files\" and \"classmap\"
autoload strategies makes it impossible to guess what the fully
qualified name of a type should be based on the file name alone."
  nil)

(cl-defmethod phpinspect-al-strategy-request-type-name ((strategy phpinspect-psrX) (file-name string))
  (let ((file-name (expand-file-name file-name)))
    (catch 'phpinspect--return
      (dolist (dir (phpinspect-psrX-directories strategy))
        (when (string-prefix-p (expand-file-name dir) file-name)
          (throw 'phpinspect--return
                 (phpinspect-psrX-filename-to-typename strategy dir file-name)))))))

(defun phpinspect-autoloader-ensure-file-indexed (autoloader file-name)
  (catch 'phpinspect--break
    (dolist (strat (phpinspect-autoloader-local-strategies autoloader))
      (when-let ((type-name (phpinspect-al-strategy-request-type-name strat file-name)))
        (unless (phpinspect-autoloader-resolve autoloader type-name)
          ;; File is not known to autoloader, re-index is required
          (phpinspect-al-strategy-re-execute strat)
          (throw 'phpinspect--break t))))))

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
  (own-types
   (make-hash-table :test 'eq :size 10000 :rehash-size 10000)
   :type hash-table
   :documentation
   "The types that are local to the project that this autoloader belongs to.")

  (types
   (make-hash-table :test 'eq :size 10000 :rehash-size 10000)
   :type hash-table
   :documentation
   "All types that are avaibale in the project that this autoloader
belongs to.")

  (type-name-fqn-bags
   (make-hash-table :test 'eq :size 3000 :rehash-size 3000)
   :type hash-table
   :documentation
   "Hash table that contains lists of fully qualified names that
share a common bare name (see `phpinspect--type-base-name').

For example, The key \"Response\" could contain:
  - \\App\\Response
  - \\FrameWork\\Response

A common use for this table is to find fully qualified names that
could be added as imports for an ambiguous bare type name.")

  (local-strategies
   nil
   :documentation
   "List of autoload strategies local to the project."))

;; FIXME: This is another scenario where an LRU Cache might come in handy (we
;; don't want to re-compare string prefixes everytime the same namespace is
;; checked).
(defun phpinspect-autoloader-get-own-types-in-namespace (al namespace)
  (cl-assert (stringp namespace))
  (let ((namespace-fqn (phpinspect--resolve-type-name nil nil namespace))
	types)
    (dolist (name (hash-table-keys (phpinspect-autoloader-own-types al)))
      (when (string-prefix-p namespace-fqn (phpinspect-name-string name))
	(push (phpinspect--make-type-generated :name name :fully-qualified t)
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

(cl-defgeneric phpinspect-al-strategy-re-execute (strat)
  "Execute STRAT again after a previous execution.

For some strategies, this will entail the removal of types
discovered during the previous execution of STRAT before
executing it again."
  (phpinspect-al-strategy-execute strat))

(defun phpinspect-autoloader-delete-type (autoloader type-name)
  "Remove all knowledge of TYPE-NAME from AUTOLOADER."
  (let* ((base-name (phpinspect-name-base type-name))
         (type-bag (phpinspect-autoloader-get-type-bag autoloader base-name)))
    (puthash base-name
             (delq type-name type-bag)
             (phpinspect-autoloader-type-name-fqn-bags autoloader))
    (remhash type-name (phpinspect-autoloader-types autoloader))
    (remhash type-name (phpinspect-autoloader-own-types autoloader))))

(cl-defmethod phpinspect-al-strategy-re-execute ((strat phpinspect-psrX))
  "Remove STRAT's found types from the autoloader and execute it again.

This function removes all types discovered during the previous
execution of the strategy from the autoloader and then
re-executes the strategy."
  (let ((autoloader (phpinspect-psrX-autoloader strat)))
    (dolist (fqn (phpinspect-psrX-types-found strat))
      (phpinspect-autoloader-delete-type autoloader fqn))

    (setf (phpinspect-psrX-types-found strat) nil)
    (phpinspect-al-strategy-execute strat)))

(cl-defmethod phpinspect-al-strategy-execute ((strat phpinspect-psrX))
  (let* ((fs (phpinspect-psrX-fs strat))
         (al (phpinspect-psrX-autoloader strat))
         (own (phpinspect-psrX-own strat))
         (own-typehash (phpinspect-autoloader-own-types al))
         (typehash (phpinspect-autoloader-types al))
         types-found type-fqn)
    (dolist (dir (phpinspect-psrX-directories strat))
      (dolist (file (phpinspect-fs-directory-files-recursively fs dir "\\.php$"))
        (setq type-fqn (phpinspect-psrX-filename-to-typename strat dir file))

        (push type-fqn types-found)
        (phpinspect-autoloader-put-type-bag al type-fqn)
        (puthash type-fqn file typehash)

        (when own
          (puthash type-fqn file own-typehash))))

    (setf (phpinspect-psrX-types-found strat) types-found)
    (when own
      (push strat (phpinspect-autoloader-local-strategies al)))))

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
  (let* ((base-name (phpinspect-name-base type-fqn))
         (bag (gethash base-name (phpinspect-autoloader-type-name-fqn-bags al))))
    (if bag
        (setcdr bag (cons type-fqn (cdr bag)))
      (push type-fqn bag)
      (puthash base-name bag (phpinspect-autoloader-type-name-fqn-bags al)))))

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
                   (lambda (_prefix directory-paths)
                     (when (stringp directory-paths)
                       (setq directory-paths (list directory-paths)))
                     (setq strategy (phpinspect-make-psr0-generated
                                     :autoloader al
                                     :fs fs
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
      (phpinspect--log "Number of autoload strategies in batch: %s" (length batch))
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

    (let ((time-start (current-time)))
      (setf (phpinspect-autoloader-refresh-thread autoloader)
            (phpinspect-pipeline (phpinspect-find-composer-json-files fs project-root)
              :async (or async-callback
                         (lambda (_result error)
                           (if error
                               (phpinspect-message "Error during autoloader refresh: %s" error)
                             (phpinspect-message
                              (concat "Refreshed project autoloader. Found %d types within project,"
                                      " %d types total. (finished in %d ms)")
                              (hash-table-count (phpinspect-autoloader-own-types autoloader))
                              (hash-table-count (phpinspect-autoloader-types autoloader))
                              (string-to-number (format-time-string "%s%3N" (time-since time-start)))))))
              :into (phpinspect-iterate-composer-jsons :with-context autoloader)
              :into phpinspect-al-strategy-execute)))))

(provide 'phpinspect-autoload)
;;; phpinspect-autoload.el ends here
