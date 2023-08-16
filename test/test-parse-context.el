;;; phpinspect-test.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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
(require 'phpinspect-parse-context)
(require 'phpinspect-meta)
(require 'phpinspect-bmap)

(ert-deftest phpinspect-pctx-cancel ()
  (let ((meta (phpinspect-make-meta nil 10 20 "    " 'token 'overlay nil))
        (pctx (phpinspect-make-pctx :bmap (phpinspect-make-bmap))))
    (phpinspect-with-parse-context pctx
      (phpinspect-meta-with-changeset meta
        (setf (phpinspect-meta-absolute-start meta) 222)
        (setf (phpinspect-meta-absolute-end meta) 1234)
        (phpinspect-meta-set-parent meta (phpinspect-make-meta nil 1 2000 "" 'parent-token))
        (setf (phpinspect-meta-overlay meta) 'not-overlay)))

    (should (= 222 (phpinspect-meta-start meta)))
    (should (= 1234 (phpinspect-meta-end meta)))
    (should (phpinspect-meta-parent meta))
    (should (eq 'not-overlay (phpinspect-meta-overlay meta)))
    (should (= 221 (phpinspect-meta-parent-offset meta)))

    (phpinspect-pctx-cancel pctx)

    (should (= 10 (phpinspect-meta-start meta)))
    (should (= 20 (phpinspect-meta-end meta)))
    (should-not (phpinspect-meta-parent meta))
    (should-not (phpinspect-meta-parent-offset meta))
    (should (eq 'overlay (phpinspect-meta-overlay meta)))))
