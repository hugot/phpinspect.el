;;; install-deps.el --- Install package dependencies  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: script

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

(require 'lisp-mnt)

(let* ((project-dir (file-name-parent-directory (file-name-directory (macroexp-file-name))))
       (file (expand-file-name "phpinspect.el" project-dir))
       dependencies)

  (with-temp-buffer
    (insert-file-contents file)
    (setq dependencies (read (lm-header-multiline "package-requires")))
    (dolist (dep dependencies)
      (package-install (car dep)))))
