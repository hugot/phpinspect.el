;;; phpinspect-resolvecontext.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 2.1.0

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
(require 'phpinspect-cache)
(require 'phpinspect-project)
(require 'phpinspect-token-predicates)
(require 'phpinspect-type)
(require 'phpinspect-meta)
(require 'phpinspect-util)

(defsubst phpinspect-blocklike-p (token)
  (or (phpinspect-block-p token)
      (phpinspect-function-p token)
      (phpinspect-class-p token)
      (phpinspect-namespace-p token)))

(defsubst phpinspect-return-p (token)
  (and (phpinspect-word-p token)
       (string= "return" (cadr token))))

(define-inline phpinspect-statement-introduction-p (token)
  (inline-letevals (token)
    (inline-quote
     (or (phpinspect-return-p ,token)
         (phpinspect-end-of-statement-p ,token)
         (phpinspect-string-concatenator-p ,token)
	 (phpinspect-use-p ,token)
         (phpinspect-function-p ,token)))))

(cl-defstruct (phpinspect--resolvecontext
               (:constructor phpinspect--make-resolvecontext)
               (:copier phpinspect--copy-resolvecontext))
  (subject nil
           :type phpinspect--token
           :documentation
           "The statement we're trying to resolve the type of.")
  (project nil
           :type phpinspect-project
           :documentation
           "The project we're resolving types for.")
  (enclosing-metadata nil
                      :type list
                      :documentation
                      "Metadata of tokens that enclose the subject.")
  (enclosing-tokens nil
                    :type list
                    :documentation
                    "Tokens that enclose the subject."))

(defun phpinspect--repurpose-resolvecontext (rctx &optional enclosing-tokens subject)
  "Copy RCTX, optionally replacing the enclosing tokens and subject.

Note: if ENCLOSING-TOKENS are provided, the repurposed
resolvecontext will have it's enclosing token metadata unset as
it would no longer be valid for the new enclosing tokens."
  (let ((rctx (phpinspect--copy-resolvecontext rctx)))
    (when subject
      (setf (phpinspect--resolvecontext-subject rctx) subject))

    (when enclosing-tokens
      ;; Unset enclosing-metadata as it is no longer valid.
      (setf (phpinspect--resolvecontext-enclosing-metadata rctx) nil)
      (setf (phpinspect--resolvecontext-enclosing-tokens rctx) enclosing-tokens))

    rctx))

(cl-defmethod phpinspect--resolvecontext-push-enclosing-token
  ((resolvecontext phpinspect--resolvecontext) enclosing-token)
  "Add ENCLOSING-TOKEN to RESOLVECONTEXTs enclosing token stack."
  (push enclosing-token (phpinspect--resolvecontext-enclosing-tokens
                         resolvecontext)))

(defun phpinspect-find-statement-before-point (bmap meta point)
  (let ((children (reverse (phpinspect-meta-find-children-before meta point)))
        token previous-siblings)
    (catch 'return
      (dolist (child children)
        (setq token (phpinspect-meta-token child))

        (when (< (phpinspect-meta-start child) point)
          (if (and (not previous-siblings) (phpinspect-blocklike-p token))
              (progn
                (throw 'return (phpinspect-find-statement-before-point bmap child point)))
            (when (phpinspect-statement-introduction-p token)
              (throw 'return previous-siblings))
            (unless (phpinspect-comment-p token)
              (push child previous-siblings))))))
    previous-siblings))

(defun phpinspect--get-last-statement-in-token (token)
  (setq token (cond ((phpinspect-function-p token)
                     (phpinspect-function-block token))
                    ((phpinspect-namespace-p token)
                     (phpinspect-namespace-block token))
                    (t token)))
  (nreverse
   (seq-take-while
    (let ((keep-taking t) (last-test nil))
      (lambda (elt)
        (when last-test
          (setq keep-taking nil))
        (setq last-test (phpinspect-variable-p elt))
        (and keep-taking
             (not (phpinspect-end-of-statement-p elt))
             (listp elt))))
    (reverse token))))

(cl-defmethod phpinspect-get-resolvecontext
  ((project phpinspect-project) (bmap phpinspect-bmap) (point integer))
  "Construct resolvecontext for BMAP, orienting around POINT."
  (let* ((enclosing-tokens)
         ;; When there are no enclosing tokens, point is probably at the absolute
         ;; end of the buffer, so we find the last child before point.
         (subject (phpinspect-bmap-last-token-before-point bmap point))
         (subject-token))
    (phpinspect--log "Last token before point: %s, right siblings: %s, parent: %s"
                     (phpinspect-meta-string subject)
                     (mapcar #'phpinspect-meta-token (phpinspect-meta-right-siblings subject))
                     (phpinspect-meta-string (phpinspect-meta-parent subject)))

    (let ((next-sibling (car (phpinspect-meta-right-siblings subject))))
      ;; When the right sibling of the last ending token overlaps point, this is
      ;; our actual subject.
      (when (and next-sibling
                 (phpinspect-meta-overlaps-point next-sibling point))
        (setq subject next-sibling)))

    ;; Dig down through tokens that can contain statements
    (let (new-subject)
      (catch 'break
        (while (and subject
                    (phpinspect-enclosing-token-p (phpinspect-meta-token subject))
                    (cdr (phpinspect-meta-token subject)))
          (setq new-subject (phpinspect-meta-find-child-before subject point))
          (if new-subject
              (setq subject new-subject)
            (throw 'break nil)))))

    (phpinspect--log "Initial resolvecontext subject token: %s"
                     (phpinspect-meta-token subject))
    (when subject
      (setq subject-token
            (mapcar #'phpinspect-meta-token
                    (phpinspect-find-statement-before-point
                     bmap (phpinspect-meta-parent subject) point)))

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
              (push parent enclosing-tokens))
            (setq parent (phpinspect-meta-parent parent))))))

    (phpinspect--make-resolvecontext
     :subject (phpinspect--get-last-statement-in-token subject-token)
     :enclosing-tokens (nreverse (mapcar #'phpinspect-meta-token enclosing-tokens))
     :enclosing-metadata (nreverse enclosing-tokens)
     :project project)))

(defun phpinspect--get-resolvecontext (project token &optional resolvecontext)
  "Find the deepest nested incomplete token in TOKEN.
If RESOLVECONTEXT is nil, it is created.  Returns RESOLVECONTEXT
of type `phpinspect--resolvecontext' containing the last
statement of the innermost incomplete token as subject
accompanied by all of its enclosing tokens."
  (unless resolvecontext
    (setq resolvecontext (phpinspect--make-resolvecontext
                          :project project)))

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
        (phpinspect--get-resolvecontext project last-token resolvecontext)
    ;; else
    (setf (phpinspect--resolvecontext-subject resolvecontext)
          (phpinspect--get-last-statement-in-token token))

    resolvecontext)))

(defun phpinspect-rctx-get-typedef (rctx class-fqn &optional no-enqueue)
  (cl-assert (phpinspect--resolvecontext-p rctx))
  (let ((project (phpinspect--resolvecontext-project rctx)))
    (phpinspect-project-get-typedef-extra-or-create project class-fqn no-enqueue)))

(defun phpinspect-rctx-get-function-return-type (rctx function-name)
  (cl-assert (phpinspect--resolvecontext-p rctx))
  (let ((project (phpinspect--resolvecontext-project rctx)))
    (phpinspect-project-get-function-return-type project function-name)))

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

(cl-defmethod phpinspect--resolvecontext-pop-meta ((rctx phpinspect--resolvecontext))
  "Remove the first element of enclosing token metadata and
return it. Pops enclosing tokens to keep both in sync."
  (pop (phpinspect--resolvecontext-enclosing-tokens rctx))
  (pop (phpinspect--resolvecontext-enclosing-metadata rctx)))


(provide 'phpinspect-resolvecontext)
;;; phpinspect-resolvecontext.el ends here
