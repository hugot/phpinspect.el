;; test-project.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Hugo Thunnissen <devel@hugot.nl>

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

;;

;;; Code:

(require 'ert)
(require 'phpinspect-project)

(ert-deftest phpinspect-project-purge ()
  (let ((project (phpinspect--make-project)))
    (phpinspect-project-purge project)

    (should (eq t (phpinspect-project-purged project)))))

(ert-deftest phpinspect-project-watch-file-and-purge ()
  (let* ((root (make-temp-file "phpinspect-test" 'dir))
         (fs (phpinspect-make-fs))
         (watch-file (concat root "/watch1"))
         (project (phpinspect--make-project :fs fs :root root)))
    (phpinspect-project-watch-file project watch-file #'ignore)

    (phpinspect-project-purge project)

    (should (= 0 (length (hash-table-values (phpinspect-project-file-watchers project)))))))
