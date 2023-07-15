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

;;(require 'phpinspect-tree)
(require 'phpinspect-bmap)
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
  (tree nil
        :documentation
        "Parsed token tree that resulted from last parse")
  (map nil
       :type phpinspect-bmap)
  (edit-tracker (phpinspect-make-edtrack)
                :type phpinspect-edtrack))

(cl-defmethod phpinspect-buffer-parse ((buffer phpinspect-buffer))
  "Parse the PHP code in the the emacs buffer that this object is
linked with."
  (with-current-buffer (phpinspect-buffer-buffer buffer)
    (let* ((map (phpinspect-make-bmap))
           (buffer-map (phpinspect-buffer-map buffer))
           (ctx (phpinspect-make-pctx
                 :bmap map
                 :incremental t
                 :previous-bmap buffer-map
                 :edtrack (phpinspect-buffer-edit-tracker buffer))))
      (phpinspect-with-parse-context ctx
        (let ((parsed (phpinspect-parse-current-buffer)))
          (setf (phpinspect-buffer-map buffer) map)
          (setf (phpinspect-buffer-tree buffer) parsed)
          (phpinspect-edtrack-clear (phpinspect-buffer-edit-tracker buffer))

          ;; return
          parsed)))))

(cl-defmethod phpinspect-buffer-reparse ((buffer phpinspect-buffer))
  (setf (phpinspect-buffer-map buffer) (phpinspect-make-bmap))
  (phpinspect-buffer-parse buffer))

(defsubst phpinspect-buffer-parse-map (buffer)
  (phpinspect-buffer-parse buffer)
  (phpinspect-buffer-map buffer))

(cl-defmethod phpinspect-buffer-register-edit
  ((buffer phpinspect-buffer) (start integer) (end integer) (pre-change-length integer))
  (phpinspect-edtrack-register-edit
   (phpinspect-buffer-edit-tracker buffer) start end pre-change-length))

(cl-defmethod phpinspect-buffer-tokens-enclosing-point ((buffer phpinspect-buffer) point)
  (phpinspect-bmap-tokens-overlapping (phpinspect-buffer-map buffer) point))

(cl-defmethod phpinspect-buffer-token-meta ((buffer phpinspect-buffer) token)
  (phpinspect-bmap-token-meta (phpinspect-buffer-map buffer) token))

(cl-defmethod phpinspect-buffer-location-resover ((buffer phpinspect-buffer))
  (phpinspect-bmap-make-location-resolver (phpinspect-buffer-map buffer)))

(provide 'phpinspect-buffer)
