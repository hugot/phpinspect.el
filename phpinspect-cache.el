;;; phpinspect.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(require 'phpinspect-project)
(require 'phpinspect-autoload)
(require 'phpinspect-worker)

(defvar phpinspect-cache nil
  "An object used to store and access metadata of PHP projects.")

(cl-defstruct (phpinspect--cache (:constructor phpinspect--make-cache))
  (projects (make-hash-table :test 'equal :size 10)
            :type hash-table
            :documentation
            "A `hash-table` with the root directories of projects
as keys and project caches as values."))

(defun phpinspect--get-or-create-global-cache ()
  "Get `phpinspect-cache'.
If its value is nil, it is created and then returned."
  (or phpinspect-cache
      (setq phpinspect-cache (phpinspect--make-cache))))

(defun phpinspect-purge-cache ()
  "Assign a fresh, empty cache object to `phpinspect-cache'.
This effectively purges any cached code information from all
currently opened projects."
  (interactive)
  (when phpinspect-cache
    ;; Allow currently known cached projects to cleanup after themselves
    (maphash (lambda (_ project)
               (phpinspect-project-purge project))
             (phpinspect--cache-projects phpinspect-cache)))

  ;; Assign a fresh cache object
  (setq phpinspect-cache (phpinspect--make-cache)))

(cl-defmethod phpinspect--cache-getproject
  ((cache phpinspect--cache) (project-root string))
  (gethash project-root (phpinspect--cache-projects cache)))

(defun phpinspect-get-or-create-cached-project-class (project-root class-fqn)
  (when project-root
    (let ((project (phpinspect--cache-get-project-create
                    (phpinspect--get-or-create-global-cache)
                    project-root)))
      (phpinspect-project-get-class-create project class-fqn))))

(cl-defmethod phpinspect--cache-get-project-create
  ((cache phpinspect--cache) (project-root string))
    "Get a project that is located in PROJECT-ROOT from CACHE.
If no such project exists in the cache yet, it is created and
then returned."
  (let ((project (phpinspect--cache-getproject cache project-root)))
    (unless project
      (setq project (puthash project-root
                             (phpinspect--make-project
                              :fs (phpinspect-make-fs)
                              :root project-root
                              :worker (phpinspect-make-dynamic-worker))
                             (phpinspect--cache-projects cache)))
      (let ((autoloader (phpinspect-make-autoloader
                         :fs (phpinspect-project-fs project)
                         :file-indexer (phpinspect-project-make-file-indexer project)
                         :project-root-resolver (phpinspect-project-make-root-resolver project))))        (setf (phpinspect-project-autoload project) autoloader)
        (phpinspect-autoloader-refresh autoloader)
        (phpinspect-project-enqueue-include-dirs project)))
    project))

(defun phpinspect-project-enqueue-include-dirs (project)
  (interactive (list (phpinspect--cache-get-project-create
                      (phpinspect--get-or-create-global-cache)
                      (phpinspect-current-project-root))))
  (let ((dirs (alist-get 'include-dirs
                         (alist-get (phpinspect-project-root project)
                                    phpinspect-projects
                                    nil nil #'string=))))
    (dolist (dir dirs)
      (message "enqueueing dir %s" dir)
      (phpinspect-worker-enqueue
       (phpinspect-project-worker project)
       (phpinspect-make-index-dir-task :dir dir :project project)))))

(defun phpinspect-project-add-include-dir (dir)
  "Configure DIR as an include dir for the current project."
  (interactive (list (read-directory-name "Include Directory: ")))
  (custom-set-variables '(phpinspect-projects))
  (let ((existing
         (alist-get (phpinspect-current-project-root) phpinspect-projects nil #'string=)))
    (if existing
        (push dir (alist-get 'include-dirs existing))
      (push `(,(phpinspect-current-project-root) . ((include-dirs . (,dir)))) phpinspect-projects)))

  (customize-save-variable 'phpinspect-projects phpinspect-projects)

  (phpinspect-project-enqueue-include-dirs (phpinspect--cache-get-project-create
                                            (phpinspect--get-or-create-global-cache)
                                            (phpinspect-current-project-root))))

(provide 'phpinspect-cache)
;;; phpinspect.el ends here
