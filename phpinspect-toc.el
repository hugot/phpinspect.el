;;; phpinspect-toc.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(require 'phpinspect-splayt)
(eval-when-compile
  (require 'phpinspect-meta))

(defun phpinspect-make-toc (&optional tree)
  (let ((table (make-hash-table :test #'eq :size 20 :rehash-size 2.0)))
    (if tree
        (phpinspect-splayt-traverse-lr (meta tree)
          (puthash (phpinspect-meta-token meta) meta table))
      (setq tree (phpinspect-make-splayt)))

    (list tree table)))

(define-inline phpinspect-toc-register (toc meta)
  (inline-letevals (toc meta)
    (inline-quote
     (progn
       (phpinspect-splayt-insert (phpinspect-toc-tree ,toc) (phpinspect-meta-start ,meta) ,meta)
       (puthash (phpinspect-meta-token ,meta) ,meta (phpinspect-toc-table ,toc))))))

(define-inline phpinspect-toc-tree (toc)
  (inline-quote (car ,toc)))

(define-inline phpinspect-toc-table (toc)
  (inline-quote (cadr ,toc)))

(defun phpinspect-toc-update (toc new-tree current-root)
  (let ((current-tree (phpinspect-toc-tree toc))
        (new-table (make-hash-table :test #'eq :size 20 :rehash-size 2.0))
        new deleted)

    (phpinspect-splayt-traverse-lr (meta new-tree)
      (puthash (phpinspect-meta-token meta) meta new-table)
      (push meta new))

    (phpinspect-splayt-traverse-lr (meta current-tree)
      (if (eq (phpinspect-meta-find-root meta) current-root)
          (progn
            (phpinspect-splayt-insert new-tree (phpinspect-meta-start meta) meta)
            (puthash (phpinspect-meta-token meta) meta new-table))
        (push meta deleted)))

    (setf (phpinspect-toc-tree toc) new-tree)
    (setf (phpinspect-toc-table toc) new-table)

    (list new deleted)))

(defun phpinspect-toc-token-at-point (toc point)
  (let ((result (phpinspect-splayt-find-largest-before (phpinspect-toc-tree toc) (+ point 1))))
    (and result (phpinspect-meta-overlaps-point result point) result)))

(defun phpinspect-toc-token-at-or-after-point (toc point)
  (phpinspect-splayt-find-smallest-after (phpinspect-toc-tree toc) (- point 1)))

(defun phpinspect-toc-tokens-in-region (toc start end)
  (phpinspect-splayt-find-all-between (phpinspect-toc-tree toc) start end))

(provide 'phpinspect-toc)
