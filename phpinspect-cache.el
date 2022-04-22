;;; phpinspect.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(require 'phpinspect-project)

(cl-defstruct (phpinspect--cache (:constructor phpinspect--make-cache))
  (active-projects nil
                   :type alist
                   :documentation
                   "An `alist` that contains the root directory
                   paths of all currently active phpinspect
                   projects")
  (projects (make-hash-table :test 'equal :size 10)
            :type hash-table
            :documentation
            "A `hash-table` with the root directories of projects
as keys and project caches as values."))

(cl-defgeneric phpinspect--cache-getproject
    ((cache phpinspect--cache) (project-name string))
  "Get project by PROJECT-NAME that is located in CACHE.")

(cl-defmethod phpinspect--cache-getproject
  ((cache phpinspect--cache) (project-root string))
  (gethash project-root (phpinspect--cache-projects cache)))

(cl-defgeneric phpinspect--cache-get-project-create
    ((cache phpinspect--cache) (project-root string))
  "Get a project that is located in PROJECT-ROOT from CACHE.
If no such project exists in the cache yet, it is created and
then returned.")

(cl-defmethod phpinspect--cache-get-project-create
  ((cache phpinspect--cache) (project-root string))
  (or (phpinspect--cache-getproject cache project-root)
      (puthash project-root
               (phpinspect--make-project-cache)
               (phpinspect--cache-projects cache))))

(provide 'phpinspect-cache)
;;; phpinspect.el ends here
