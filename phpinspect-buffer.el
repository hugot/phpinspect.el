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

(defvar-local phpinspect-current-buffer nil
  "An instance of `phpinspect-buffer' local to the active
buffer. This variable is only set for buffers where
`phpinspect-mode' is active. Also see `phpinspect-buffer'.")


(cl-defstruct (phpinspect-token-metadata (:constructor phpinspect-make-token-metadata))
  "An object that represents the metadata associated with a parsed token."
  (token nil
         :type phpinspect-token
         :documentation
         "The token that metadata is associated with.")
  (location nil
            :type phpinspect-region
            :documentation
            "The region that token occupies.")
  (handler nil
           :type phpinspect-handler
           :documentation
           "The handler that was used to parse token. (see `phpinspect-defhandler')"))

(cl-defstruct (phpinspect-buffer (:constructor phpinspect-make-buffer))
  "An object containing phpinspect related metadata linked to an
emacs buffer."
  (buffer nil
          :type buffer
          :documentation "The underlying emacs buffer")
  (metadata-map (make-hash-table :test 'eq :size 400 :rehash-size 400)
                :type hash-table
                :documentation
                "A map containing metadata associated with tokens.")
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
    (setf (phpinspect-buffer-metadata-map buffer)
          (make-hash-table :test 'eq
                           :size 400
                           :rehash-size 400))

    (let ((tree (phpinspect-parse-current-buffer)))
      (setf (phpinspect-buffer-tree buffer) tree)
      tree)))

(cl-defmethod phpinspect-buffer-set-token-metadata
  ((buffer phpinspect-buffer) token (metadata phpinspect-token-metadata))
  "Set the METADATA associated with TOKEN that was parsed in BUFFER"
  (puthash token metadata (phpinspect-buffer-metadata-map buffer)))

(cl-defmethod phpinspect-buffer-get-token-metadata ((buffer phpinspect-buffer) token)
  (gethash token (phpinspect-buffer-metadata-map buffer)))

(cl-defmethod phpinspect-buffer-token-location ((buffer phpinspect-buffer) token)
  (phpinspect-token-metadata-location (phpinspect-buffer-get-token-metadata buffer token)))

(cl-defmethod phpinspect-buffer-tokens-enclosing-point ((buffer phpinspect-buffer) point)
  (let ((tokens))
    (maphash
     (lambda (token meta)
       (let ((region (phpinspect-token-metdata-location meta)))
         (when (and (<= (phpinspect-region-start region) point)
                    (>= (phpinspect-region-end region) point))
           (push token tokens))))
     (phpinspect-buffer-metadata-map buffer))
    (sort tokens (lambda (tok1 tok2)
                   (phpinspect-region< (phpinspect-buffer-token-location tok1)
                                       (phpinspect-buffer-token-location tok2))))))

(cl-defmethod phpinspect-buffer-tokens-overlapping-region
  ((buffer phpinspect-buffer) (start integer) (end integer))
  (let ((tokens)
        (query-region (phpinspect-make-region start end)))
    (maphash (lambda (token metadata)
               (when (phpinspect-region-overlaps
                      query-region (phpinspect-token-metadata-location metadata))
                 (push token tokens)))
             (phpinspect-buffer-metadata-map buffer))
    tokens))

(provide 'phpinspect-buffer)
