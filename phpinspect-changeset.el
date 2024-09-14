;;; phpinspect-changeset.el --- Metadata changeset module  -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'phpinspect-meta))

(define-inline phpinspect-make-changeset (meta)
  (inline-letevals (meta)
    (inline-quote
     (list (phpinspect-meta-start ,meta) (phpinspect-meta-end ,meta)
           (phpinspect-meta-parent ,meta) (phpinspect-meta-overlay ,meta)
           (phpinspect-meta-parent-offset ,meta) ,meta))))

(define-inline phpinspect-changeset-start (set)
  (inline-quote (car ,set)))

(define-inline phpinspect-changeset-end (set)
  (inline-quote (cadr ,set)))

(define-inline phpinspect-changeset-parent (set)
  (inline-quote (caddr ,set)))

(define-inline phpinspect-changeset-overlay (set)
  (inline-quote (cadddr ,set)))

(define-inline phpinspect-changeset-parent-offset (set)
  (inline-quote (car (cddddr ,set))))

(define-inline phpinspect-changeset-meta (set)
  (inline-quote (car (nthcdr 5 ,set))))

(define-inline phpinspect-changeset-revert (changeset)
  (inline-letevals (changeset)
    (inline-quote
     (progn
       (setf (phpinspect-meta-parent (phpinspect-changeset-meta ,changeset))
             (phpinspect-changeset-parent ,changeset))
       (setf (phpinspect-meta-overlay (phpinspect-changeset-meta ,changeset))
             (phpinspect-changeset-overlay ,changeset))
       (setf (phpinspect-meta-absolute-start (phpinspect-changeset-meta ,changeset))
             (phpinspect-changeset-start ,changeset))
       (setf (phpinspect-meta-absolute-end (phpinspect-changeset-meta ,changeset))
             (phpinspect-changeset-end ,changeset))
       (setf (phpinspect-meta-parent-offset (phpinspect-changeset-meta ,changeset))
             (phpinspect-changeset-parent-offset ,changeset))))))

(provide 'phpinspect-changeset)
;;; phpinspect-changeset.el ends here
