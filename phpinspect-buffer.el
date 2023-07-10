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
          (setf (phpinspect-edtrack-edits (phpinspect-buffer-edit-tracker buffer))
                (phpinspect-make-ll))

          ;; return
          parsed)))))

(cl-defmethod phpinspect-buffer-register-edit
  ((buffer phpinspect-buffer) (start integer) (end integer) (pre-change-length integer))
  (let* ((edit
          (phpinspect-edtrack-register-edit
           (phpinspect-buffer-edit-tracker buffer) start end pre-change-length))
         (region (phpinspect-make-region (phpinspect-edit-original-start edit)
                                         (phpinspect-edit-original-end edit)))
         (tainted (phpinspect-tree-traverse-overlapping
                   (phpinspect-buffer-tree buffer) region)))

    (dolist (meta tainted)
      (phpinspect-tree-traverse (node (phpinspect-meta-tree meta))
        (when (phpinspect-tree-overlaps node region)
          (setf (phpinspect-meta-tainted (phpinspect-tree-value node)) t))))))

(cl-defmethod phpinspect-buffer--register-token
  ((buffer phpinspect-buffer) token start end handler)
  (let* ((meta (phpinspect-make-meta
                :token token
                :handler handler
                :whitespace-before (phpinspect-buffer-whitespace buffer)))
        (node (phpinspect-tree-insert
               (phpinspect-buffer-tree buffer) start end meta)))
    (setf (phpinspect-meta-tree meta) node)
    (setf (phpinspect-buffer-whitespace buffer) "")))

(cl-defmethod phpinspect-buffer-get-token-metadata ((buffer phpinspect-buffer) token)
  nil)

(cl-defmethod phpinspect-buffer-token-location ((buffer phpinspect-buffer) token)
  (phpinspect-token-metadata-region (phpinspect-buffer-get-token-metadata buffer token)))

(cl-defmethod phpinspect-buffer-tokens-enclosing-point ((buffer phpinspect-buffer) point)
  (phpinspect-tree-traverse-overlapping (phpinspect-buffer-tree buffer) point))


(provide 'phpinspect-buffer)
