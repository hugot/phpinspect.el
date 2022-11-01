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

(defvar-local phpinspect-current-buffer nil
  "An instance of `phpinspect-buffer' local to the active
buffer. This variable is only set for buffers where
`phpinspect-mode' is active. Also see `phpinspect-buffer'.")

(defsubst phpinspect-make-region (start end)
  (list start end))

(defalias 'phpinspect-region-start #'car)
(defalias 'phpinspect-region-end #'cadr)

(cl-defstruct (phpinspect-buffer (:constructor phpinspect-make-buffer))
  "An object containing phpinspect related metadata linked to an
emacs buffer."
  (buffer nil
          :type buffer
          :documentation "The underlying emacs buffer")
  (location-map (make-hash-table :test 'eq :size 400 :rehash-size 400)
                :type hash-table
                :documentation
                "A map that lets us look up the character
positions of a token within this buffer.")
  (tree nil
        :type list
        :documentation
        "An instance of a token tree as returned by
`phpinspect--index-tokens'. Meant to be eventually consistent
with the contents of the buffer."))

(cl-defmethod phpinspect-buffer-parse ((buffer phpinspect-buffer))
  "Parse the PHP code in the the emacs buffer that this object is
linked with."
  (with-current-buffer (phpinspect-buffer-buffer buffer)
    (setf (phpinspect-buffer-location-map buffer)
          (make-hash-table :test 'eq
                           :size 400
                           :rehash-size 400))

    (let ((tree (phpinspect-parse-current-buffer)))
      (setf (phpinspect-buffer-tree buffer) tree)
      tree)))

(cl-defmethod phpinspect-buffer-token-location ((buffer phpinspect-buffer) token)
  (gethash token (phpinspect-buffer-location-map buffer)))

(cl-defmethod phpinspect-buffer-tokens-enclosing-point ((buffer phpinspect-buffer) point)
  (let ((tokens))
    (maphash
     (lambda (token region)
       (when (and (<= (phpinspect-region-start region) point)
                  (>= (phpinspect-region-end region) point))
         (push token tokens)))
     (phpinspect-buffer-location-map buffer))
    tokens))

(provide 'phpinspect-buffer)
