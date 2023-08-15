;;; phpinspect-type.el --- PHP parsing and completion package -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc

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
(require 'phpinspect-buffer)
(require 'phpinspect-resolvecontext)
(require 'phpinspect-suggest)

(defvar phpinspect--last-completion-list nil
  "Used internally to save metadata about completion options
  between company backend calls")

(cl-defstruct (phpinspect--completion
               (:constructor phpinspect--construct-completion))
  "Contains a possible completion value with all it's attributes."
  (value nil :type string)
  (meta nil :type string)
  (annotation nil :type string)
  (kind nil :type symbol))

(cl-defgeneric phpinspect--make-completion (completion-candidate)
  "Creates a `phpinspect--completion` for a possible completion
candidate. Candidates can be indexed functions and variables.")

(cl-defstruct (phpinspect--completion-list
               (:constructor phpinspect--make-completion-list))
  "Contains all data for a completion at point"
  (completion-start nil
                    :type integer)
  (completion-end nil
                  :type integer)
  (completions (obarray-make)
               :type obarray
               :documentation
               "A list of completion strings")
  (has-candidates nil))

(cl-defgeneric phpinspect--completion-list-add
    (comp-list completion)
  "Add a completion to a completion-list.")

(cl-defmethod phpinspect--completion-list-add
  ((comp-list phpinspect--completion-list) (completion phpinspect--completion))
  (setf (phpinspect--completion-list-has-candidates comp-list) t)
  (unless (intern-soft (phpinspect--completion-value completion)
                      (phpinspect--completion-list-completions comp-list))
    (set (intern (phpinspect--completion-value completion)
                 (phpinspect--completion-list-completions comp-list))
         completion)))

(cl-defmethod phpinspect--completion-list-get-metadata
  ((comp-list phpinspect--completion-list) (completion-name string))
  (let ((comp-sym (intern-soft completion-name
                               (phpinspect--completion-list-completions comp-list))))
    (when comp-sym
      (symbol-value comp-sym))))


(cl-defmethod phpinspect--completion-list-strings
  ((comp-list phpinspect--completion-list))
  (let ((strings))
    (obarray-map (lambda (sym) (push (symbol-name sym) strings))
                 (phpinspect--completion-list-completions comp-list))
    strings))

(cl-defstruct (phpinspect-completion-query (:constructor phpinspect-make-completion-query))
  (completion-point 0
                    :type integer
                    :documentation
                    "Position in the buffer from where the resolvecontext is determined.")
  (point 0
         :type integer
         :documentation "Position in buffer for which to provide completions")
  (buffer nil
          :type phpinspect-buffer))


(cl-defgeneric phpinspect-comp-strategy-supports (strategy query context)
  "Should return non-nil if STRATEGY should be deployed for QUERY
and CONTEXT. All strategies must implement this method.")

(cl-defgeneric phpinspect-comp-strategy-execute (strategy query context)
  "Should return a list of objects for which
`phpinspect--make-completion' is implemented.")

(cl-defstruct (phpinspect-comp-sigil (:constructor phpinspect-make-comp-sigil))
  "Completion strategy for the sigil ($) character.")

(defun phpinspect-completion-subject-at-point (buffer point predicate)
  (let ((subject (phpinspect-bmap-last-token-before-point
                  (phpinspect-buffer-parse-map buffer) point)))
    (and subject
         (funcall predicate (phpinspect-meta-token subject))
         (>= (phpinspect-meta-end subject) point)
         subject)))

(cl-defmethod phpinspect-comp-strategy-supports
  ((_strat phpinspect-comp-sigil) (q phpinspect-completion-query)
   (_rctx phpinspect--resolvecontext))
  (when-let ((subject (phpinspect-completion-subject-at-point
                       (phpinspect-completion-query-buffer q)
                       (phpinspect-completion-query-point q)
                       #'phpinspect-variable-p)))
    (list (+ (phpinspect-meta-start subject) 1) (phpinspect-meta-end subject))))


(cl-defmethod phpinspect-comp-strategy-execute
  ((_strat phpinspect-comp-sigil) (_q phpinspect-completion-query)
   (rctx phpinspect--resolvecontext))
  (phpinspect-suggest-variables-at-point rctx))

(define-inline phpinspect-attrib-start (attrib-meta)
  "The start position of the name of the attribute that is being referenced.

ATTRIB-META must be an instance of phpinspect-meta (see `phpinspect-make-meta'),
belonging to a token that conforms with `phpinspect-attrib-p'"
  (inline-letevals (attrib-meta)
    (inline-quote
     (- (phpinspect-meta-end ,attrib-meta)
        (length (cadadr (phpinspect-meta-token ,attrib-meta)))))))

(cl-defstruct (phpinspect-comp-attribute (:constructor phpinspect-make-comp-attribute))
  "Completion strategy for object attributes")

(cl-defmethod phpinspect-comp-strategy-supports
  ((_strat phpinspect-comp-attribute) (q phpinspect-completion-query)
   (_rctx phpinspect--resolvecontext))
  (when-let ((subject (phpinspect-completion-subject-at-point
                       (phpinspect-completion-query-buffer q)
                       (phpinspect-completion-query-point q)
                       #'phpinspect-object-attrib-p)))
    (list (phpinspect-attrib-start subject) (phpinspect-meta-end subject))))

(cl-defmethod phpinspect-comp-strategy-execute
  ((_strat phpinspect-comp-attribute) (_q phpinspect-completion-query)
   (rctx phpinspect--resolvecontext))
  (phpinspect-suggest-attributes-at-point rctx))

(cl-defstruct (phpinspect-comp-static-attribute (:constructor phpinspect-make-comp-static-attribute))
  "Completion strategy for static attributes")

(cl-defmethod phpinspect-comp-strategy-supports
  ((_strat phpinspect-comp-static-attribute) (q phpinspect-completion-query)
   (_rctx phpinspect--resolvecontext))
  (when-let ((subject (phpinspect-completion-subject-at-point
                       (phpinspect-completion-query-buffer q)
                       (phpinspect-completion-query-point q)
                       #'phpinspect-static-attrib-p)))
    (list (phpinspect-attrib-start subject) (phpinspect-meta-end subject))))

(cl-defmethod phpinspect-comp-strategy-execute
  ((_strat phpinspect-comp-static-attribute) (_q phpinspect-completion-query)
   (rctx phpinspect--resolvecontext))
  (phpinspect-suggest-attributes-at-point rctx 'static))

(cl-defstruct (phpinspect-comp-word (:constructor phpinspect-make-comp-word))
  "Comletion strategy for bare words")

(cl-defmethod phpinspect-comp-strategy-supports
  ((_strat phpinspect-comp-word) (q phpinspect-completion-query)
   (_rctx phpinspect--resolvecontext))
  (when-let ((subject (phpinspect-completion-subject-at-point
                       (phpinspect-completion-query-buffer q)
                       (phpinspect-completion-query-point q)
                       #'phpinspect-word-p)))
    (list (phpinspect-meta-start subject) (phpinspect-meta-end subject))))

(cl-defmethod phpinspect-comp-strategy-execute
  ((_strat phpinspect-comp-word) (_q phpinspect-completion-query)
   (rctx phpinspect--resolvecontext))
  (phpinspect-suggest-functions rctx))

(defvar phpinspect-completion-strategies (list (phpinspect-make-comp-attribute)
                                               (phpinspect-make-comp-sigil)
                                               (phpinspect-make-comp-word)
                                               (phpinspect-make-comp-static-attribute))
  "List of completion strategies that phpinspect can use.")

(defun phpinspect--get-completion-query ()
  (phpinspect-make-completion-query
   :buffer phpinspect-current-buffer
   :completion-point (phpinspect--determine-completion-point)
   :point (point)))

(cl-defmethod phpinspect-completion-query-execute ((query phpinspect-completion-query))
  "Execute QUERY.

Returns list of `phpinspect--completion'."
  (let* ((buffer (phpinspect-completion-query-buffer query))
         (point (phpinspect-completion-query-point query))
         (buffer-map (phpinspect-buffer-parse-map buffer))
         (rctx (phpinspect-get-resolvecontext buffer-map point))
         (completion-list (phpinspect--make-completion-list)))
    (phpinspect-buffer-update-project-index buffer)

    (dolist (strategy phpinspect-completion-strategies)
      (when-let (region (phpinspect-comp-strategy-supports strategy query rctx))
        (setf (phpinspect--completion-list-completion-start completion-list)
              (car region)
              (phpinspect--completion-list-completion-end completion-list)
              (cadr region))

        (phpinspect--log "Found matching completion strategy. Executing...")
        (dolist (candidate (phpinspect-comp-strategy-execute strategy query rctx))
          (phpinspect--completion-list-add
           completion-list (phpinspect--make-completion candidate)))))
    (setq phpinspect--last-completion-list completion-list)))

(cl-defmethod phpinspect--make-completion
  ((completion-candidate phpinspect--function))
  "Create a `phpinspect--completion` for COMPLETION-CANDIDATE."
  (phpinspect--construct-completion
   :value (phpinspect--function-name completion-candidate)
   :meta (concat "(" (mapconcat (lambda (arg)
                                  (concat "$" (if (> (length (car arg)) 8)
                                                  (truncate-string-to-width (car arg) 8 nil)
                                                (car arg))))
                                (phpinspect--function-arguments completion-candidate)
                                ", ")
                 ") "
                 (phpinspect--format-type-name (phpinspect--function-return-type completion-candidate)))
   :annotation (concat " "
                       (phpinspect--type-bare-name
                        (phpinspect--function-return-type completion-candidate)))
   :kind 'function))

(cl-defmethod phpinspect--make-completion
  ((completion-candidate phpinspect--variable))
  (phpinspect--construct-completion
   :value (phpinspect--variable-name completion-candidate)
   :meta (phpinspect--format-type-name
          (or (phpinspect--variable-type completion-candidate)
              phpinspect--null-type))
   :annotation (concat " "
                       (phpinspect--type-bare-name
                        (or (phpinspect--variable-type completion-candidate)
                            phpinspect--null-type)))
   :kind 'variable))

(define-inline phpinspect--prefix-for-completion (completion)
  (inline-letevals (completion)
    (inline-quote
     (pcase (phpinspect--completion-kind ,completion)
       ('function "<f> ")
       ('variable "<va> ")))))


(defun phpinspect-complete-at-point ()
  (let ((comp-list (phpinspect-completion-query-execute (phpinspect--get-completion-query)))
        strings)
    (obarray-map (lambda (sym) (push (symbol-name sym) strings)) (phpinspect--completion-list-completions comp-list))
    (and (phpinspect--completion-list-has-candidates comp-list)
         (list (phpinspect--completion-list-completion-start comp-list)
               (phpinspect--completion-list-completion-end comp-list)
               strings
               :affixation-function
               (lambda (completions)
                 (let (affixated completion)
                   (dolist (comp completions)
                     (setq completion (phpinspect--completion-list-get-metadata comp-list comp))
                     (push (list comp (phpinspect--prefix-for-completion completion)
                                 (phpinspect--completion-meta completion))
                           affixated))
                   (nreverse affixated)))
               :exit-function
               (lambda (comp-name state)
                 (when (and (eq 'finished state)
                            (eq 'function (phpinspect--completion-kind
                                           (phpinspect--completion-list-get-metadata
                                            phpinspect--last-completion-list
                                            comp-name))))
                   (insert "(")))
               :company-kind (lambda (comp-name)
                               (phpinspect--completion-kind
                                (phpinspect--completion-list-get-metadata
                                 phpinspect--last-completion-list
                                 comp-name)))))))


(provide 'phpinspect-completion)
