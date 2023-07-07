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
  (region nil
          :type phpinspect-region
          :documentation
          "The region that token occupies.")
  (tree nil
        :type phpinspect-tree)
  (handler nil
           :type phpinspect-handler
           :documentation
           "The handler that was used to parse token. (see `phpinspect-defhandler')"))

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
  (metadata-map (make-hash-table :test 'eq :size 3000 :rehash-size 2.0)
                :type hash-table
                :documentation
                "A map containing metadata associated with tokens.")
  (edit-tracker (phpinspect-make-edit-tracker)
                :type phpinspect-edit-tracker))

(cl-defmethod phpinspect-buffer-parse ((buffer phpinspect-buffer))
  "Parse the PHP code in the the emacs buffer that this object is
linked with."
  (with-current-buffer (phpinspect-buffer-buffer buffer)
    (let ((tree (phpinspect-make-tree :start (point-min)
                                      :end (point-max))))
      (setf (phpinspect-buffer-tree buffer) tree)
      (setf (phpinspect-buffer-metadata-map buffer)
            (make-hash-table :test 'eq :size 3000 :rehash-size 1.5))

      (let ((parsed (phpinspect-parse-current-buffer)))
        ;; Set tree root to the child containing the root parsed token.
        (setq tree (seq-elt (phpinspect-tree-children tree) 0))
        (setf (phpinspect-tree-parent tree) nil)
        (setf (phpinspect-buffer-tree buffer) tree)

        ;; return
        parsed))))

;; (cl-defmethod phpinspect-buffer-parse-incrementally ((buffer phpinspect-buffer) &optional edits)
;;   (let* ((edits (or edits (phpinspect-edit-tracker-edits
;;                            (phpinspect-buffer-edit-tracker buffer))))
;;          (edit (phpinspect-queue-dequeue edits)))


(cl-defmethod phpinspect-buffer-register-edit
  ((buffer phpinspect-buffer) (start integer) (end integer) (pre-change-length integer))
  (let ((contents (buffer-substring start end)))
    (phpinspect-edit-tracker-register
     (phpinspect-buffer-edit-tracker buffer) start end pre-change-length contents)))

(cl-defmethod phpinspect-buffer-queue-full-parse ((buffer phpinspect-buffer))
  (phpinspect--log "Attempted to queue full parse"))

(defmacro phpinspect-buffer-with-tree (buffer tree &rest body)
  (declare (indent 2))
  (let ((tree-store-sym (gensym)))
  `(unwind-protect
       (let ((,tree-store-sym (phpinspect-buffer-tree ,buffer)))
         (setf (phpinspect-buffer-tree ,buffer ,tree))
         ,@body)
     (setf (phpinspect-buffer-tree buffer) ,tree-store-sym))))

(cl-defmethod phpinspect-buffer-set-token-metadata
  ((buffer phpinspect-buffer) token (metadata phpinspect-token-metadata))
  "Set the METADATA associated with TOKEN that was parsed in BUFFER"
  (setf (phpinspect-token-metadata-token metadata) token)

  (let ((metadata-existing (gethash token (phpinspect-buffer-metadata-map buffer))))
    (if metadata-existing
        (setf (gv-deref metadata-existing) metadata)
      (progn
        (setq metadata-ref (gv-ref metadata))
        (puthash token metadata-ref (phpinspect-buffer-metadata-map buffer))
        (let* ((region (phpinspect-token-metadata-region metadata))
               (tree-node
                (phpinspect-tree-insert (phpinspect-buffer-tree buffer)
                                        (phpinspect-region-start region)
                                        (phpinspect-region-end region)
                                        metadata)))
          (setf (phpinspect-token-metadata-tree metadata) tree-node))))))

(cl-defmethod phpinspect-buffer-get-token-metadata ((buffer phpinspect-buffer) token)
  (let ((ref (gethash token (phpinspect-buffer-metadata-map buffer))))
    (when ref (gv-deref ref))))

(cl-defmethod phpinspect-buffer-token-location ((buffer phpinspect-buffer) token)
  (phpinspect-token-metadata-region (phpinspect-buffer-get-token-metadata buffer token)))

(cl-defmethod phpinspect-buffer-tokens-enclosing-point ((buffer phpinspect-buffer) point)
  (mapcar #'phpinspect-token-metadata-token
          (phpinspect-tree-traverse-overlappig (phpinspect-buffer-tree buffer) point)))

(cl-defstruct (phpinspect-edit-tracker (:constructor phpinspect-make-edit-tracker))
  (edits (phpinspect-make-queue)))

(cl-defstruct (phpinspect-edit (:constructor phpinspect-make-edit))
  (contents ""
            :type string
            :documentation "The contents of the edit")
  (region nil
          :type phpinspect-region
          :documentation
          "The region in which the edit took place")
  (delta 0
         :type integer
         :documentation
         "The change in width of the edit region"))

(cl-defmethod phpinspect-edit-tracker-register
  ((tracker phpinspect-edit-tracker) (start integer) (end integer)
   (pre-change-length integer) (contents string))
  (phpinspect-queue-enqueue
   (phpinspect-edit-tracker-edits tracker)
   (phpinspect-make-edit :region (phpinspect-make-region start end)
                         :delta (- (- end start) pre-change-length)
                         :contents contents)))


(provide 'phpinspect-buffer)
