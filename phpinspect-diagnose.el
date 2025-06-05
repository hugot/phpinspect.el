;;; phpinspect-diagnose.el --- Diagnose problems in phpinspect's internal state  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <hugo@yournextconcepts.com>
;; Keywords: languages

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

(require 'phpinspect-buffer)
(require 'phpinspect-meta)

;;;###autoload
(defun phpinspect-buffer-diagnose-tree (buffer)
  "Diagnose problems in BUFFER's token (metadata) tree."
  (interactive (list phpinspect-current-buffer))
  (cl-assert (phpinspect-buffer-p buffer))

  (phpinspect-meta-diagnose-parent-child-relations
   (phpinspect-buffer-root-meta buffer))

  (phpinspect-message "Finished diagnosis, no problems found"))

(defun phpinspect-meta-children-as-string-safe (meta)
  (let (result)
    (phpinspect-splayt-traverse-lr (child (phpinspect-meta-children meta))
      (if (eq child meta)
          (push "[self]" result )
        (push (phpinspect-meta-string-safe meta) result)))
    (string-join result ", ")))

(defun phpinspect-meta-string-safe (meta &optional start)
  (setq start (or start 0))

  (dlet ((phpinspect-meta--point-offset-base start))
    (if meta
        (format "[start: %d, end: %d, token (possibly incomplete): %s, children: %s]"
                (phpinspect-meta-start meta)
                (phpinspect-meta-end meta)
                (if (phpinspect-atom-p (phpinspect-meta-token meta))
                    (seq-subseq (phpinspect-meta-token meta) 0 2)
                  (car (phpinspect-meta-token meta)))
                (phpinspect-meta-children-as-string-safe meta))
      "[nil]")))

(defun phpinspect-meta-diagnose-parent-child-relations (meta)
  "Find problems in parent-child relations of META and descendants."
  (when meta
    (let ((stack (list meta)))
      (while-let ((current (pop stack)))
        ;; A token cannot be a parent of itself.
        (when (eq (phpinspect-meta-parent current) current)
          (let ((message
                 (format "Cyclic parent-child relation detected at (current): %s"
                         (phpinspect-meta-string-safe current))))
            (error message)))

        (phpinspect-splayt-traverse-lr (child (phpinspect-meta-children current))
          (when (eq child current)
            (let ((message
                   (format "Cyclic parent-child relation detected at (child): %s"
                           (phpinspect-meta-string-safe child))))
              (error message)))


          (push child stack))))))

(provide 'phpinspect-diagnose)
;;; phpinspect-diagnose.el ends here
