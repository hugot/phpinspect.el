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

;; (cl-defmethod phpinspect-buffer-reparse
;;   ((buffer phpinspect-buffer) (meta phpinspect-token-meta) (edit phpinspect-edit))


(cl-defmethod phpinspect-buffer-parse-incrementally ((buffer phpinspect-buffer))
    (let ((edits
           (phpinspect-edit-tracker-edits (phpinspect-buffer-edit-tracker buffer)))
          (buffer-tree (phpinspect-buffer-tree buffer)))
      (seq-doseq (edit edits)
        (let* ((edit-region (phpinspect-edit-region edit))
               (edit-delta (phpinspect-edit-delta edit))
               (parsed-start)
               (parsed-end)
               (metas (phpinspect-tree-find-smallest-overlapping-set
                       buffer-tree edit-region)))
          (condition-case err
              (dolist (meta metas)
                (setq meta (gv-deref meta))
                (let ((region (phpinspect-token-metadata-region meta))
                      (handler-function (symbol-function
                                         (phpinspect-token-metadata-handler meta)))
                      (temp-tree (phpinspect-make-tree
                                  ;; We start parsing at the start of the token
                                  :start (phpinspect-region-start region)
                                  ;; We expect to end parsing at the end of the
                                  ;; edit region, but this could not be the case
                                  ;; as some handlers ignore point-max.
                                  :end (point-max)
                                  :value 'temp-tree-root))
                      (parsed)
                      (parsed-tree))
                  (phpinspect-buffer-with-tree buffer temp-tree
                    (with-current-buffer (phpinspect-buffer-buffer buffer)
                      (save-excursion
                        (goto-char (phpinspect-region-start region))
                        (unless (looking-at
                                 (phpinspect-handler-regexp
                                  (phpinspect-token-metadata-handler meta)))
                          ;; The token type changed, reparsing the parent is
                          ;; required to determine its new nature.
                          (throw 'recurse-parent))

                        (setq parsed
                              (funcall handler-function (match-string 0)
                                       (phpinspect-region-end edit-region)))

                        (phpinspect-set-token-metadata-when-current-buffer
                         parsed (phpinspect-region-start region) (point)
                         (phpinspect-token-metadata-handler meta)))))

                  (setq parsed-tree
                        (phpinspect-tree-detach
                         (seq-elt (phpinspect-tree-children temp-tree) 0)))
                  (cond ((> (phpinspect-tree-end parsed-tree)
                            (+ (phpinspect-region-end region) edit-delta))
                         ;; Changes in edit region changed meaning of tokens
                         ;; outside of it.
                         (throw 'recurse-parent))
                        ((< (phpinspect-tree-end parsed-tree)
                            (+ (phpinspect-region-end region) edit-delta))
                         ;; No edits after, but token did not grow to expected
                         ;; size.
                         (unless (phpinspect-edit-tracker-edit-after edit)
                           (throw 'recurse-parent)))
                        ((


                        -




;;   (with-current-buffer (phpinspect-buffer-buffer buffer)
;;     (let ((edits
;;            (phpinspect-edit-tracker-edits (phpinspect-buffer-edit-tracker buffer)))
;;           (buffer-tree (phpinspect-buffer-tree buffer)))
;;       (unwind-protect
;;           (progn
;;             (seq-doseq (edit edits)
;;               (let* ((edit-region (phpinspect-edit-region edit))
;;                      (metas (phpinspect-tree-find-smallest-overlapping-set
;;                              buffer-tree edit-region)))
;;                 (condition-case err
;;                     (dolist (meta metas)
;;                       (setq meta (gv-deref meta))
;;                       (let ((region (phpinspect-token-metadata-region meta))
;;                             (handler-function (symbol-function
;;                                                (phpinspect-token-metadata-handler meta)))
;;                             (temp-tree (phpinspect-make-tree
;;                                         ;; We start parsing at the start of the token
;;                                         :start (phpinspect-region-start region)
;;                                         ;; We expect to end parsing at the end of the edit region
;;                                         :end (phpinspect-region-end edit-region)
;;                                         :value 'temp-tree-root))
;;                             (parse-result)
;;                             (parse-result-tree))
;;                         (save-excursion
;;                           (goto-char (phpinspect-region-start region))
;;                           ;; Override buffer tree
;;                           (setf (phpinspect-buffer-tree buffer) temp-tree)
;;                           (setq parse-result (funcall handler-function))

;;                           (phpinspect-set-token-metadata-when-current-buffer
;;                            parse-result (phpinspect-region-start region) (point)
;;                            (phpinspect-token-metadata-handler meta))

;;                           (if (eq (phpinspect-tree-value temp-tree) 'temp-tree-root)
;;                               (setq parse-result-tree
;;                                     (phpinspect-tree-detach
;;                                      (seq-elt (phpinspect-tree-children temp-tree) 0)))
;;                             ;; New token width exceeds edit region.
;;                             (phpinspect-buffer-queue-full-parse buffer)
;;                             (throw 'break))

                          ;; New token region encloses editen region, stop parsing.
        ;;                   (let* ((new-meta (gv-deref (phpinspect-tree-value parse-result-tree)))
        ;;                          (new-token (phpinspect-token-metadata-token new-meta)))
        ;;                     (if (and (phpinspect-incomplete-token-p new-token)
        ;;                              (phpinspect-edit-tracker-edit-after
        ;;                               (phpinspect-buffer-edit-tracker buffer) edit))
        ;;                         ;; The token is incomplete, and there are edits after this token

        ;;                     (when (phpinspect-region-encloses new-meta edit-region)
        ;;                       (throw 'break))))))
        ;;           (break)))))
        ;; ;; Restore buffer tree
        ;; (setf (phpinspect-buffer-tree buffer) buffer-tree)))))


        ;; (if (< 1 (length metas))
        ;;     (setq reparse-meta (phpinspect-tree-value
        ;;                         (phpinspect-tree-parent
        ;;                          (phpinspect-token-metadata-tree (car metas)))))
        ;;   (setq reparse-meta (car metas)))

        ;; (if reparse-meta
        ;;     (phpinspect-buffer-reparse buffer reparse-meta edit)
        ;;   ;; Nothing found to reparse.. Just reparse entire buffer to be sure
        ;;   (phpinspect--log "[WARNING] phpinspect-buffer-parse-incrementally: No reparsable tokens found, reparsing entire buffer.")
        ;;   (phpinspect-buffer-parse buffer))))))

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
  (edits (phpinspect-make-ll)))

(cl-defstruct (phpinspect-edit (:constructor phpinspect-make-edit))
  (region nil
          :type phpinspect-region
          :documentation
          "The region in which the edit took place")
  (delta 0
         :type integer
         :documentation
         "The change in width of the edit region"))

(cl-defmethod phpinspect-edit-concat ((edit1 phpinspect-edit) (edit2 phpinspect-edit))
  "Absorb EDIT2 into EDIT1. Returns EDIT1."
  (let ((min) (max) (delta)
        (region1 (phpinspect-edit-region edit1))
        (region2 (phpinspect-edit-region edit2)))
    (if (< (phpinspect-region-start region1)
           (phpinspect-region-start region2))
        (setq min (phpinspect-region-start region1))
      (setq min (phpinspect-region-start region2)))

    (if (> (phpinspect-region-end region1)
           (phpinspect-region-end region2))
        (setq max (phpinspect-region-end region1))
      (setq max (phpinspect-region-end region2)))

    (setq delta (+ (phpinspect-edit-delta edit1)
                   (phpinspect-edit-delta edit2)))

    (setf (phpinspect-edit-delta edit1) delta)
    (setf (phpinspect-edit-region edit1) (phpinspect-make-region min max))
    edit1))

(cl-defmethod phpinspect-edit-overlaps ((edit1 phpinspect-edit) (edit2 phpinspect-edit))
  (phpinspect-region-overlaps (phpinspect-edit-region edit1)
                              (phpinspect-edit-region edit2)))

(cl-defmethod phpinspect-edit-could-overlap ((edit1 phpinspect-edit) (edit2 phpinspect-edit))
  (> (phpinspect-region-end (phpinspect-edit-region edit1))
     (phpinspect-region-start (phpinspect-edit-region edit2))))

(cl-defmethod phpinspect-edit-tracker-mark
  ((tracker phpinspect-edit-tracker) (start integer) (end integer) (pre-change-length integer))
  (let* ((edits (phpinspect-edit-tracker-edits tracker))
         (edit (phpinspect-make-edit :region (phpinspect-make-region start end)
                                     :delta (- (- end start) pre-change-length)))
         (first-in-range (seq-find (lambda (e) (phpinspect-edit-could-overlap edit e))
                                   edits))
         (insertion-link)
         (first-overlapper)
         (overlappers))

    (when first-in-range
      (if (phpinspect-edit-overlaps edit first-in-range)
          (progn
            ;; If this is the first overlapper, we should insert before it.
            (setq insertion-link (phpinspect-llnode-left
                                  (phpinspect-ll-link edits first-in-range)))
            (setq first-overlapper first-in-range))
        ;; If this is not an overlapper, we should insert after it, as the
        ;; entire region spans a range before the edit.
        (setq insertion-link (phpinspect-ll-link edits first-in-range))))

    (unless first-overlapper
      (setq first-overlapper (seq-find (lambda (e) (phpinspect-edit-overlaps edit e))
                                       (or insertion-link
                                           (phpinspect-ll-link edits first-in-range)))))

    (when first-overlapper
      (setq overlappers
            (seq-take-while (lambda (e) (phpinspect-edit-overlaps e edit))
                            (phpinspect-ll-link edits first-overlapper)))

      ;; Break overlappers away from edit list, as they're about to be replaced.
      (setq overlappers (phpinspect-slice-detach overlappers)))

    ;; Absorb all overlapping edits into our newly created edit
    (seq-doseq (overlap overlappers)
      (phpinspect-edit-concat edit overlap))

    (if insertion-link
        (phpinspect-ll-insert-right insertion-link edit)
      (phpinspect-ll-push edit edits))))

(cl-defmethod phpinspect-edit-tracker-unmark
  ((tracker phpinspect-edit-tracker) (start integer) (end integer))
  (let* ((region (phpinspect-make-region start end))
         (edits (phpinspect-edit-tracker-edits tracker))
         (first-enclosing
          (seq-find (lambda (e) (phpinspect-region-encloses
                                 region (phpinspect-edit-region e)))
                    edits)))
    (when first-enclosing
      (phpinspect-slice-detach
       (seq-take-while (lambda (e) (phpinspect-region-encloses
                                    region (phpinspect-edit-region e)))
                       edits)))))

(cl-defmethod phpinspect-edit-tracker-edit-after
  ((tracker phpinspect-edit-tracker) (edit phpinspect-edit))
  (phpinspect-llnode-value
   (phpinspect-llnode-right
   (phpinspect-ll-link
    (phpinspect-edit-tracker-edits tracker) edit))))

(provide 'phpinspect-buffer)
