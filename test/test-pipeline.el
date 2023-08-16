;;; test-pipeline.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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

(require 'phpinspect-pipeline)

(defun phpinspect--correct-the-record (input)
  (phpinspect-pipeline-emit
   (format "It's not %s, but GNU/%s" input input)))

(ert-deftest phpinspect-pipeline ()
  (let (result error)

    (phpinspect-pipeline (list "Linux" "Emacs")
      :into phpinspect--correct-the-record
      :async (lambda (res err)
               (setq result res
                     error err)))

    (while (not (or result error))
      (thread-yield))

    (should (equal '("It's not Linux, but GNU/Linux" "It's not Emacs, but GNU/Emacs")
                   result))
    (should-not error)))

(defun phpinspect--aah-it-broke (input)
  (signal 'it-brokey input))

(ert-deftest phpinspect-pipeline-error ()

  (let (result error)
    (phpinspect-pipeline (list "Holy smokey")
      :into phpinspect--aah-it-broke
      :async (lambda (res err)
               (setq result res
                     error err)))

    (while (not (or result error))
      (thread-yield))

    (should error)
    (should (equal '(phpinspect-pipeline-error
                     "Thread phpinspect-pipeline-phpinspect--aah-it-broke signaled error: (it-brokey . Holy smokey)")
                   error))))
