;;; phpinspect-buffer.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc

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

(require 'phpinspect-tree)
(require 'phpinspect-edtrack)

(defvar-local phpinspect-current-buffer nil
  "An instance of `phpinspect-buffer' local to the active
buffer. This variable is only set for buffers where
`phpinspect-mode' is active. Also see `phpinspect-buffer'.")


(cl-defstruct (phpinspect-buffer (:constructor phpinspect-make-buffer))
  "An object containing phpinspect related metadata linked to an
emacs buffer."
  (buffer nil
          :type buffer
          :documentation "The associated emacs buffer")
  (tree (phpinspect-make-tree)
             :type phpinspect-tree
             :documentation
             "A tree containing metadata associated with tokens.")
  (edit-tracker (phpinspect-make-edtrack)
                :type phpinspect-edtrack)
  (whitespace nil
              :type string
              :documentation
              "Whitespace parsed before the next token to be parsed"))

(cl-defmethod phpinspect-buffer-parse ((buffer phpinspect-buffer))
  "Parse the PHP code in the the emacs buffer that this object is
linked with."
  (phpinspect-buffer-propagate-taints buffer)
  (with-current-buffer (phpinspect-buffer-buffer buffer)
    (let* ((tree (phpinspect-make-tree :start (point-min)
                                       :end (+ 1 (point-max))))
           (buffer-tree (phpinspect-buffer-tree buffer))
           (ctx (phpinspect-make-pctx
                 :tree tree
                 :incremental t
                 :previous-tree (unless (phpinspect-tree-empty-p buffer-tree) buffer-tree)
                 :edtrack (phpinspect-buffer-edit-tracker buffer))))
      (phpinspect-with-parse-context ctx
        (let ((parsed (phpinspect-parse-current-buffer)))
          ;; Set tree root to the child containing the root parsed token.
          (setq tree (seq-elt (phpinspect-tree-children tree) 0))
          (setf (phpinspect-tree-parent tree) nil)
          (setf (phpinspect-buffer-tree buffer) tree)
          (phpinspect-edtrack-clear (phpinspect-buffer-edit-tracker buffer))

          ;; return
          parsed)))))

(cl-defmethod phpinspect-buffer-reparse ((buffer phpinspect-buffer))
  (setf (phpinspect-buffer-tree buffer) (phpinspect-make-tree))
  (phpinspect-buffer-parse buffer))

(defsubst phpinspect-buffer-parse-tree (buffer)
  (phpinspect-buffer-parse buffer)
  (phpinspect-buffer-tree buffer))

(cl-defmethod phpinspect-buffer-register-edit
  ((buffer phpinspect-buffer) (start integer) (end integer) (pre-change-length integer))
  (phpinspect-edtrack-register-edit
   (phpinspect-buffer-edit-tracker buffer) start end pre-change-length))

(cl-defmethod phpinspect-buffer-propagate-taints ((buffer phpinspect-buffer))
  (let ((tracker (phpinspect-buffer-edit-tracker buffer)))
    (when (phpinspect-edtrack-has-taints-p tracker)
      (seq-doseq (taint (phpinspect-tree-children (phpinspect-edtrack-taint-pool tracker)))
        (let* ((region (phpinspect-make-region (phpinspect-tree-start taint)
                                               (phpinspect-tree-end taint)))
               (tainted (phpinspect-tree-traverse-overlapping
                         (phpinspect-buffer-tree buffer) region)))

          (dolist (meta tainted)
            (setf (phpinspect-meta-tainted meta) t))))

      (phpinspect-edtrack-clear-taints tracker))))

(cl-defmethod phpinspect-buffer-tokens-enclosing-point ((buffer phpinspect-buffer) point)
  (phpinspect-tree-traverse-overlapping (phpinspect-buffer-tree buffer) point))

(provide 'phpinspect-buffer)
