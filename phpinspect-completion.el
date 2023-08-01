;;; phpinspect-type.el --- PHP parsing and completion package -*- lexical-binding: t; -*-

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
  (completions (obarray-make)
               :type obarray
               :documentation
               "A list of completion strings"))

(cl-defgeneric phpinspect--completion-list-add
    (comp-list completion)
  "Add a completion to a completion-list.")

(cl-defmethod phpinspect--completion-list-add
  ((comp-list phpinspect--completion-list) (completion phpinspect--completion))
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

(cl-defmethod phpinspect-completion-query-execute ((query phpinspect-completion-query))
  "Execute QUERY.

Returns list of `phpinspect--completion'."
  (let* ((buffer (phpinspect-completion-query-buffer query))
         (point (phpinspect-completion-query-point query))
         (buffer-map (phpinspect-buffer-parse-map buffer))
         (rctx (phpinspect-get-resolvecontext buffer-map point))
         (completion-list (phpinspect--make-completion-list)))
    (dolist (strategy phpinspect-completion-strategies)
      (when (phpinspect-comp-strategy-supports strategy query rctx)
        (phpinspect--log "Found matching completion strategy. Executing...")
        (dolist (candidate (phpinspect-comp-strategy-execute strategy query rctx))
          (phpinspect--completion-list-add
           completion-list (phpinspect--make-completion candidate)))))
    completion-list))

(cl-defgeneric phpinspect-comp-strategy-supports (strategy (query phpinspect-completion-query) (context phpinspect--resolvecontext))
  "Should return non-nil if STRATEGY should be deployed for QUERY
and CONTEXT. All strategies must implement this method.")

(cl-defgeneric phpinspect-comp-strategy-execute (strategy (query phpinspect-completion-query) (context phpinspect--resolvecontext))
  "Should return a list of objects for which `phpinspect--make-completion' is implemented.")

(cl-defstruct (phpinspect-comp-sigil (:constructor phpinspect-make-comp-sigil))
  "Completion strategy for the sigil ($) character.")

(cl-defmethod phpinspect-comp-strategy-supports
  ((strat phpinspect-comp-sigil) (q phpinspect-completion-query)
   (rctx phpinspect--resolvecontext))
  (and (= (phpinspect-completion-query-completion-point q)
          (phpinspect-completion-query-point q))
       (phpinspect-variable-p
        (phpinspect-meta-token
         (phpinspect-bmap-last-token-before-point
          (phpinspect-buffer-parse-map (phpinspect-completion-query-buffer q))
          (phpinspect-completion-query-point q))))))

(cl-defmethod phpinspect-comp-strategy-execute
  ((strat phpinspect-comp-sigil) (q phpinspect-completion-query)
   (rctx phpinspect--resolvecontext))
  (phpinspect-suggest-variables-at-point rctx))

(cl-defstruct (phpinspect-comp-attribute (:constructor phpinspect-make-comp-attribute))
  "Completion strategy for object attributes")

(cl-defmethod phpinspect-comp-strategy-supports
  ((strat phpinspect-comp-attribute) (q phpinspect-completion-query)
   (rctx phpinspect--resolvecontext))
  (phpinspect-object-attrib-p (car (last (phpinspect--resolvecontext-subject rctx)))))

(cl-defmethod phpinspect-comp-strategy-execute
  ((strat phpinspect-comp-attribute) (q phpinspect-completion-query)
   (rctx phpinspect--resolvecontext))
  (phpinspect-suggest-attributes-at-point rctx))

(cl-defstruct (phpinspect-comp-static-attribute (:constructor phpinspect-make-comp-static-attribute))
  "Completion strategy for static attributes")

(cl-defmethod phpinspect-comp-strategy-supports
  ((strat phpinspect-comp-static-attribute) (q phpinspect-completion-query)
   (rctx phpinspect--resolvecontext))
  (phpinspect-static-attrib-p (car (last (phpinspect--resolvecontext-subject rctx)))))

(cl-defmethod phpinspect-comp-strategy-execute
  ((strat phpinspect-comp-static-attribute) (q phpinspect-completion-query)
   (rctx phpinspect--resolvecontext))
  (phpinspect-suggest-attributes-at-point rctx 'static))

(defvar phpinspect-completion-strategies (list (phpinspect-make-comp-attribute)
                                               (phpinspect-make-comp-sigil)
                                               (phpinspect-make-comp-static-attribute))
  "List of completion strategies that phpinspect can use.")

(cl-defmethod phpinspect--make-completion
  ((completion-candidate phpinspect--function))
  "Create a `phpinspect--completion` for COMPLETION-CANDIDATE."
  (phpinspect--construct-completion
   :value (phpinspect--function-name completion-candidate)
   :meta (concat "(" (mapconcat (lambda (arg)
                                  (concat (phpinspect--format-type-name (cadr arg)) " "
                                          "$" (if (> (length (car arg)) 8)
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

(provide 'phpinspect-completion)
