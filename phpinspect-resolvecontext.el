;;; phpinspect-resolvecontext.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(require 'phpinspect-bmap)
(require 'phpinspect-project)
(require 'phpinspect-parser)
(require 'phpinspect-type)

(cl-defstruct (phpinspect--resolvecontext
            (:constructor phpinspect--make-resolvecontext))
  (subject nil
           :type phpinspect--token
           :documentation
           "The statement we're trying to resolve the type of.")
  (project-root nil
                :type string
                :documentation
                "The root directory of the project we're resolving types for.")
  (enclosing-tokens nil
                    :type list
                    :documentation
                    "Tokens that enclose the subject."))

(cl-defmethod phpinspect--resolvecontext-push-enclosing-token
  ((resolvecontext phpinspect--resolvecontext) enclosing-token)
  "Add ENCLOSING-TOKEN to RESOLVECONTEXTs enclosing token stack."
  (push enclosing-token (phpinspect--resolvecontext-enclosing-tokens
                         resolvecontext)))

(defsubst phpinspect-blocklike-p (token)
  (or (phpinspect-block-p token)
      (phpinspect-function-p token)
      (phpinspect-class-p token)
      (phpinspect-namespace-p token)))

(defun phpinspect-find-statement-before-point (bmap meta point)
  (let ((children (reverse (cdr (phpinspect-meta-token meta)))))
    (let ((previous-siblings))
      (catch 'return
        (dolist (child children)
          (when (phpinspect-probably-token-p child)
            (setq child (phpinspect-bmap-token-meta bmap child))
            (when (< (phpinspect-meta-start child) point)
              (if (and (not previous-siblings) (phpinspect-blocklike-p (phpinspect-meta-token child)))
                  (progn
                    (throw 'return (phpinspect-find-statement-before-point bmap child point)))
                (when (phpinspect-end-of-statement-p (phpinspect-meta-token child))
                  (throw 'return previous-siblings))
                (push (phpinspect-meta-token child) previous-siblings)))))
        previous-siblings))))

(cl-defmethod phpinspect-get-resolvecontext
  ((bmap phpinspect-bmap) (point integer))
  (let* ((enclosing-tokens)
         ;; When there are no enclosing tokens, point is probably at the absolute
         ;; end of the buffer, so we find the last child before point.
         (subject (phpinspect-bmap-last-token-before-point bmap point))
         (subject-token)
         (siblings))
    (phpinspect--log "Last token before point: %s" subject)
    ;; Dig down through tokens that can contain statements
    (catch 'break
      (while (and subject
                  (phpinspect-enclosing-token-p (phpinspect-meta-token subject))
                  (cdr (phpinspect-meta-token subject)) 0)
        (let ((new-subject
               (phpinspect-bmap-token-meta
                bmap (car (last (cdr (phpinspect-meta-token subject)))))))
          (if new-subject
              (setq subject new-subject)
            (throw 'break nil)))))

    (phpinspect--log "Initial resolvecontext subject token: %s"
             (phpinspect-meta-token subject))
    (when subject
      (setq subject-token
            (phpinspect-find-statement-before-point
             bmap
             (phpinspect-meta-parent subject)
             point))

      (phpinspect--log "Ultimate resolvecontext subject token: %s. Parent: %s"
                       subject-token (phpinspect-meta-token
                                      (phpinspect-meta-parent subject)))

      ;; Iterate through subject parents to build stack of enclosing tokens
      (let ((parent (phpinspect-meta-parent subject)))
        (while parent
          (let ((granny (phpinspect-meta-parent parent)))
            (unless (and (phpinspect-block-p (phpinspect-meta-token parent))
                         (or (not granny)
                             (phpinspect-function-p (phpinspect-meta-token granny))
                             (phpinspect-class-p (phpinspect-meta-token granny))))
              (push (phpinspect-meta-token parent) enclosing-tokens))
            (setq parent (phpinspect-meta-parent parent))))))

    (phpinspect--make-resolvecontext
     :subject (phpinspect--get-last-statement-in-token subject-token)
     :enclosing-tokens (nreverse enclosing-tokens)
     :project-root (phpinspect-current-project-root))))

(defun phpinspect--get-resolvecontext (token &optional resolvecontext)
  "Find the deepest nested incomplete token in TOKEN.
If RESOLVECONTEXT is nil, it is created.  Returns RESOLVECONTEXT
of type `phpinspect--resolvecontext' containing the last
statement of the innermost incomplete token as subject
accompanied by all of its enclosing tokens."
  (unless resolvecontext
    (setq resolvecontext (phpinspect--make-resolvecontext
                          :project-root (phpinspect-current-project-root))))

  (let ((last-token (car (last token)))
        (last-encountered-token (car
                                 (phpinspect--resolvecontext-enclosing-tokens
                                  resolvecontext))))
    (unless (and (or (phpinspect-function-p last-encountered-token)
                 (phpinspect-class-p last-encountered-token))
             (phpinspect-block-p token))
        ;; When a class or function has been inserted already, its block
        ;; doesn't need to be added on top.
      (phpinspect--resolvecontext-push-enclosing-token resolvecontext token))

    (if (phpinspect-incomplete-token-p last-token)
        (phpinspect--get-resolvecontext last-token resolvecontext)
    ;; else
    (setf (phpinspect--resolvecontext-subject resolvecontext)
          (phpinspect--get-last-statement-in-token token))

    resolvecontext)))

(defun phpinspect--make-type-resolver-for-resolvecontext
    (resolvecontext)
  (let ((namespace-or-root
         (seq-find #'phpinspect-namespace-or-root-p
                   (phpinspect--resolvecontext-enclosing-tokens
                    resolvecontext)))
        (namespace-name))
    (when (phpinspect-namespace-p namespace-or-root)
      (setq namespace-name (cadadr namespace-or-root))
      (setq namespace-or-root (phpinspect-namespace-body namespace-or-root)))

      (phpinspect--make-type-resolver
       (phpinspect--uses-to-types
        (seq-filter #'phpinspect-use-p namespace-or-root))
       (seq-find #'phpinspect-class-p
                   (phpinspect--resolvecontext-enclosing-tokens
                    resolvecontext))
       namespace-name)))

(provide 'phpinspect-resolvecontext)
;;; phpinspect-resolvecontext.el ends here
