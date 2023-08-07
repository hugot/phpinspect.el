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

(require 'phpinspect-parser)
(require 'phpinspect-bmap)
(require 'phpinspect-edtrack)
(require 'phpinspect-index)

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
  (tree nil
        :documentation
        "Parsed token tree that resulted from last parse")
  (map nil
       :type phpinspect-bmap)
  (project nil
           :type phpinspect-project)
  (edit-tracker (phpinspect-make-edtrack)
                :type phpinspect-edtrack))

(cl-defmethod phpinspect-buffer-parse ((buffer phpinspect-buffer) &optional no-interrupt no-index)
  "Parse the PHP code in the the emacs buffer that this object is
linked with."
  (let ((tree))
  (if (or (not (phpinspect-buffer-tree buffer))
          (phpinspect-edtrack-taint-pool (phpinspect-buffer-edit-tracker buffer)))
      (with-current-buffer (phpinspect-buffer-buffer buffer)
        (let* ((map (phpinspect-make-bmap))
               (buffer-map (phpinspect-buffer-map buffer))
               (ctx (phpinspect-make-pctx
                     :interrupt-predicate (unless no-interrupt #'input-pending-p)
                     :bmap map
                     :incremental t
                     :previous-bmap buffer-map
                     :edtrack (phpinspect-buffer-edit-tracker buffer))))
          (phpinspect-with-parse-context ctx
            (phpinspect--log "Parsing buffer")
            (let ((parsed (phpinspect-parse-current-buffer)))
              (setf (phpinspect-buffer-map buffer) map)
              (setf (phpinspect-buffer-tree buffer) parsed)
              (phpinspect-edtrack-clear (phpinspect-buffer-edit-tracker buffer))

              ;; set return value
              (setq tree parsed)

              (unless (or no-index
                          (not (phpinspect-buffer-project buffer)))
                (phpinspect--log "Adding buffer index to project")
                (phpinspect-project-add-index
                 (phpinspect-buffer-project buffer)
                 (phpinspect--index-tokens tree nil (phpinspect-buffer-location-resolver buffer))
                 'index-imports))))))
    ;; Else: Just return last parse result
    (setq tree (phpinspect-buffer-tree buffer)))

  tree))


(cl-defmethod phpinspect-buffer-reparse ((buffer phpinspect-buffer))
  (setf (phpinspect-buffer-tree buffer) nil)
  (setf (phpinspect-buffer-map buffer) (phpinspect-make-bmap))
  (phpinspect-edtrack-clear (phpinspect-buffer-edit-tracker buffer))
  (phpinspect-buffer-parse buffer 'no-interrupt))

(defsubst phpinspect-buffer-parse-map (buffer)
  (phpinspect-buffer-parse buffer)
  (phpinspect-buffer-map buffer))

(cl-defmethod phpinspect-buffer-register-edit
  ((buffer phpinspect-buffer) (start integer) (end integer) (pre-change-length integer))
  "Mark a region of the buffer as edited."

  ;; Take into account "atoms" (tokens without clear delimiters like words,
  ;; variables and object attributes. The meaning of these tokens will change as
  ;; they grow or shrink, so their ful regions need to be marked for a reparse).
  (save-excursion
    (goto-char start)
    (when (looking-back "\\($|->|::\\)?[^][)(}{[:blank:]\n;'\"]+" nil t)
      (setq start (- start (length (match-string 0))))
      (setq pre-change-length (+ pre-change-length (length (match-string 0))))))

  (phpinspect-edtrack-register-edit
   (phpinspect-buffer-edit-tracker buffer) start end pre-change-length))

(cl-defmethod phpinspect-buffer-tokens-enclosing-point ((buffer phpinspect-buffer) point)
  (phpinspect-bmap-tokens-overlapping (phpinspect-buffer-map buffer) point))

(cl-defmethod phpinspect-buffer-token-meta ((buffer phpinspect-buffer) token)
  (phpinspect-bmap-token-meta (phpinspect-buffer-map buffer) token))

(cl-defmethod phpinspect-buffer-location-resolver ((buffer phpinspect-buffer))
  "Derive location resolver from BUFFER's buffer map. Guarantees to
retrieve the lastest available map of BUFFER upon first
invocation, but subsequent invocations will not update the used
map afterwards, so don't keep the resolver around for long term
use."
  (let ((bmap-resolver))
    (lambda (token)
      (funcall (with-memoization bmap-resolver
                 (phpinspect-bmap-make-location-resolver (phpinspect-buffer-map buffer)))
               token))))

(cl-defmethod phpinspect-buffer-root-meta ((buffer phpinspect-buffer))
  (phpinspect-bmap-root-meta (phpinspect-buffer-map buffer)))

(defun phpinspect-display-buffer-tree ()
  (interactive)
  (when phpinspect-current-buffer
    (let ((buffer phpinspect-current-buffer))
      (pop-to-buffer (generate-new-buffer "phpinspect-buffer-tree"))
      (insert (pp-to-string (phpinspect-buffer-parse buffer 'no-interrupt)))
      (read-only-mode))))

(defun phpinspect-display-buffer-index ()
  (interactive)
  (when phpinspect-current-buffer
    (let ((buffer phpinspect-current-buffer))
      (pop-to-buffer (generate-new-buffer "phpinspect-buffer-tree"))
      (insert (pp-to-string (phpinspect--index-tokens (phpinspect-buffer-parse buffer 'no-interrupt))))
      (read-only-mode))))


(provide 'phpinspect-buffer)
