;;; appendage.el --- Benchmarks of list appendage  -*- lexical-binding: t; -*-

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


(message "20000 appendages using nconc")
(garbage-collect)
(benchmark
 1 '(let (list)
      (dotimes (i 20000)
        (setq list (nconc list (list i))))

      list))

(message "20000 appendages using push + nreverse")
(garbage-collect)
(benchmark
 1 '(let (list)
      (dotimes (i 20000)
        (push i list))

      (nreverse list)))

(message "20000 appendages using rear pointer")
(garbage-collect)
(benchmark
 1 '(let* ((list (cons nil nil))
           (rear list))

      (dotimes (i 20000)
        (setq rear (setcdr rear (cons i nil))))

      (cdr list)))
