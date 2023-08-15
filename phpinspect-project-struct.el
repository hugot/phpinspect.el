;;; phpinspect-project-struct.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(eval-when-compile
  (declare-function phpinspect-make-dynamic-worker "phpinspect-worker.el"))


(cl-defstruct (phpinspect-project (:constructor phpinspect--make-project))
  (class-index (make-hash-table :test 'eq :size 100 :rehash-size 1.5)
               :type hash-table
               :documentation
               "A `hash-table` that contains all of the currently
indexed classes in the project")
  (function-index (make-hash-table :test 'eq :size 100 :rehash-size 2.0)
                  :type hash-table
                  :documentation
                  "A hash able that contains all of the currently indexed functions
in the project")
  (function-token-index (make-hash-table :test 'eq :size 100 :rehash-size 1.5))
  (fs nil
      :type phpinspect-fs
      :documentation
      "The filesystem object through which this project's files
can be accessed.")
  (autoload nil
    :type phpinspect-autoload
    :documentation
    "The autoload object through which this project's type
definitions can be retrieved")
  (worker (progn
            (unless (featurep 'phpinspect-worker)
              (require 'phpinspect-worker))
            (phpinspect-make-dynamic-worker))
          :type phpinspect-worker
          :documentation
          "The worker that this project may queue tasks for")
  (root nil
        :type string
        :documentation
        "The root directory of this project")
  (purged nil
          :type boolean
          :documentation "Whether or not the project has been purged or not.
Projects get purged when they are removed from the global cache.")
  (file-watchers (make-hash-table :test #'equal :size 10000 :rehash-size 10000)
                 :type hash-table
                 :documentation "All active file watchers in this project,
indexed by the absolute paths of the files they're watching."))

(provide 'phpinspect-project-struct)
