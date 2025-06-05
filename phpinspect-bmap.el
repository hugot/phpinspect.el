;;; phpinspect-bmap.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 3.0.0

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

;; This file contains code for the mapping of tokens to metadata objects and
;; vice versa. See also phpinspect-meta.el.
;;
;; The code in this file is heavily used in phpinspect-parser.el for incremental
;; parsing.

;;; Code:

(require 'phpinspect-splayt)
(require 'phpinspect-meta)
(require 'phpinspect-parse-context)
(require 'phpinspect-util)
(require 'compat)
(require 'phpinspect-token-predicates)

(defvar phpinspect-bmap-map-token-metadata nil
  "Set to non-nil value to make bmap save metadata in
 `phpinspect-token-references'.

When this variable is non-nil, `phpinspect-bmap-register' will
store references from tokens to their metadata objects in
`phpinspect-token-references'.

These references can be fetched via `phpinspect-bmap-token-meta'.

Useful for testing or debugging.")

(defvar phpinspect-token-references
  (make-hash-table :test #'eq :size 1000 :rehash-size 2 :weakness 'value)
  "A hash-table which allows lookups of token metadata objects by their tokens.")

(eval-when-compile
  (require 'cl-macs)

  (defvar phpinspect-parse-context nil
    "dummy for compilation")


  (phpinspect--declare-log-group 'bmap))

(cl-defstruct (phpinspect-bmap (:constructor phpinspect-make-bmap))
  "A bmap, short for buffer-map, is a structure whose purpose is to
map parsed tokens to metadata about them and vice versa."
  (meta (make-hash-table :test #'eq
                           :size (floor (/ (point-max) 2))
                           :rehash-size 1.5)
        :documentation "A hash-table containing all newly registered tokens.")
  (token-stack nil
               :type list)
  (-root-meta nil
              :type phpinspect-meta)
  (last-meta nil :type phpinspect-meta)
  (last-token-start nil
                    :type integer)
  (recycled-p nil
              :type boolean
              :documentation "Whether bmap contains recycled tokens")
  (pos-filter nil
              :type bool-vector))

(define-inline phpinspect-bmap-root-meta (bmap)
  (inline-letevals (bmap)
    (inline-quote
     (with-memoization (phpinspect-bmap--root-meta ,bmap)
       (when-let ((last-meta (phpinspect-bmap-last-meta ,bmap)))
         (if (phpinspect-root-p (phpinspect-meta-token last-meta))
             last-meta
           (phpinspect-meta-find-parent-matching-token last-meta #'phpinspect-root-p)))))))


(defsubst phpinspect-bmap-register (bmap start end token &optional whitespace-before recycle)
  "Register TOKEN with START and END region as queryable metadata in BMAP.

If RECYCLE is non-nil, it is assumed to be a `phpinspect-meta'
object and re-used instead of instantiating a new object."
  (let* ((meta (phpinspect-bmap-meta bmap))
         (last-token-start (phpinspect-bmap-last-token-start bmap))
         (token-meta (or recycle (phpinspect-make-meta nil start end whitespace-before token))))

    (when (< end start)
      (error "Token %s ends before it starts. Start: %s, end: %s" token start end))

    (unless whitespace-before
      (setq whitespace-before ""))

    (puthash token token-meta meta)
    (when phpinspect-bmap-map-token-metadata
      (puthash token token-meta phpinspect-token-references))

     (when (and last-token-start
                (<= start last-token-start))
       (let ((child)
             (stack (phpinspect-bmap-token-stack bmap)))
         (while (and (car stack) (>= (phpinspect-meta-start (car stack)) start))
           (setq child (pop stack))
           (phpinspect-meta-set-parent child token-meta))

         (setf (phpinspect-bmap-token-stack bmap) stack)))

     (setf (phpinspect-bmap-last-token-start bmap) start)
     (push token-meta (phpinspect-bmap-token-stack bmap))
     (setf (phpinspect-bmap-last-meta bmap) token-meta)))

(define-inline phpinspect-pctx-register-token (pctx token start end)
  (inline-letevals (pctx)
    (inline-quote
     (phpinspect-bmap-register
      (phpinspect-pctx-bmap ,pctx) ,start ,end ,token (phpinspect-pctx-consume-whitespace ,pctx)))))


(define-inline phpinspect-bmap-token-starting-at (bmap point)
  (inline-letevals (bmap point)
    (inline-quote
     (when-let ((root-meta (phpinspect-bmap-root-meta ,bmap)))
       (if (= ,point (phpinspect-meta-start root-meta))
           root-meta
         (phpinspect-meta-find-child-starting-at-recursively
          root-meta ,point))))))

(define-inline phpinspect-bmap-token-starting-after (bmap point)
  (inline-letevals (bmap point)
    (inline-quote
     (let ((root-meta (phpinspect-bmap-root-meta ,bmap)))
       (phpinspect-meta-find-child-after-recursively root-meta ,point)))))

(defun phpinspect-bmap-tokens-overlapping (bmap point)
  (sort
   (phpinspect-meta-find-overlapping-children (phpinspect-bmap-root-meta bmap) point)
   #'phpinspect-meta-sort-width))

(defun phpinspect-bmap-token-meta (_bmap token)
  "Get metadata object associated with TOKEN.

Requires `phpinspect-bmap-map-token-metadata' to be
non-nil. Otherwise no references are not stored in
`phpinspect-token-references' while parsing.

The _BMAP parameter is not used as references are stored in a
global variable. It has been kept as an argument for backwards
compatibility with tests and for easy refactoring later on."
  (unless phpinspect-bmap-map-token-metadata
    (error "`phpinspect-bmap-map-token-metadata' is not enabled"))

  (gethash token phpinspect-token-references))

(defun phpinspect-bmap-last-token-before-point (bmap point)
  "Search backward in BMAP for last token ending before POINT."
  (let ((root-meta (phpinspect-bmap-root-meta bmap)))
    (phpinspect-meta-find-child-before-recursively root-meta point)))

(define-inline phpinspect-bmap-recycle (bmap token-meta pos-delta &optional whitespace-before)
  "Re-use TOKEN-META as a token in BMAP, applying POS-DELTA.

TOKEN-META start and end positions are shifted by POS-DELTA. See
`phpinspect-meta-shift'.

If WHITESPACE-BEFORE is provided, it is assigned to TOKEN-META's
whitespace-before slot.

Before mutating TOKEN-META, its slots are saved in a changeset
which can be used to revert the changes. Changesets are managed
via `phpinspect-parse-context'."
  (inline-letevals (bmap token-meta pos-delta whitespace-before)
    (inline-quote
     (let* ((start (+ (phpinspect-meta-start ,token-meta) ,pos-delta))
            (end (+ (phpinspect-meta-end ,token-meta) ,pos-delta)))

         (setf (phpinspect-bmap-recycled-p ,bmap) t)


         (phpinspect-meta-detach-parent ,token-meta)
         (phpinspect-meta-shift ,token-meta ,pos-delta)

         (dlet ((phpinspect-meta--point-offset-base nil))
           (phpinspect-bmap-register
            ,bmap start end (phpinspect-meta-token ,token-meta) ,whitespace-before ,token-meta))))))

(defun phpinspect-make-region (start end)
  (list start end))

(defalias 'phpinspect-region-start #'car)
(defalias 'phpinspect-region-end #'cadr)

(defun phpinspect-region-size (region)
  (- (phpinspect-region-end region) (phpinspect-region-start region)))

(defun phpinspect-region> (reg1 reg2)
  (> (phpinspect-region-size reg1) (phpinspect-region-size reg2)))

(defun phpinspect-region< (reg1 reg2)
  (< (phpinspect-region-size reg1) (phpinspect-region-size reg2)))

(defun phpinspect-region-overlaps-point (reg point)
  (and (> (phpinspect-region-end reg) point)
       (<= (phpinspect-region-start reg) point)))

(defun phpinspect-region-overlaps (reg1 reg2)
  (or (phpinspect-region-overlaps-point reg1 (phpinspect-region-start reg2))
      (phpinspect-region-overlaps-point reg1 (- (phpinspect-region-end reg2) 1))
      (phpinspect-region-overlaps-point reg2 (phpinspect-region-start reg1))
      (phpinspect-region-overlaps-point reg2 (- (phpinspect-region-end reg1) 1))))

(defun phpinspect-region-encloses (reg1 reg2)
  (and (<= (phpinspect-region-start reg1) (phpinspect-region-start reg2))
       (>= (phpinspect-region-end reg1) (phpinspect-region-end reg2))))

(defun phpinspect-bmap-make-location-resolver (bmap)
  (lambda (token)
    (let ((meta (phpinspect-bmap-token-meta bmap token)))
      (if meta
          (phpinspect-make-region (phpinspect-meta-start meta)
                                  (phpinspect-meta-end meta))
        (phpinspect-make-region 0 0)))))

(provide 'phpinspect-bmap)
;;; phpinspect-bmap.el ends here
