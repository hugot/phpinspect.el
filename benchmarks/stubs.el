;;; stubs.el --- Benchmarks of phpinspect stub index dump and load times  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: benchmark

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

(require 'phpinspect-cache)

(let (result)

  (message "Building and loading stub cache")
  (garbage-collect)
  (setq result
        (benchmark-run 1 (phpinspect-build-stub-cache)))
  (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))

  (message "Building stub cache")
  (garbage-collect)
  (setq result
        (benchmark-run 1 (phpinspect-build-stub-index)))
  (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))

  (message "Building and dumping stub cache")
  (garbage-collect)
  (setq result
        (benchmark-run 1 (phpinspect-dump-stub-index)))
  (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))

  (message "Loading stub cache")
  (garbage-collect)
  (setq result
        (benchmark-run 1 (phpinspect-load-stub-index)))
  (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result)))
