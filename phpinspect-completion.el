;;; phpinspect-type.el --- PHP parsing and completion package -*- lexical-binding: t; -*-

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

(require 'obarray)

(require 'phpinspect-bmap)
(require 'phpinspect-buffer)
(require 'phpinspect-resolvecontext)
(require 'phpinspect-suggest)

(phpinspect--declare-log-group 'completion)

(defvar phpinspect--last-completion-list nil
  "Used internally to save metadata about completion options
  between company backend calls")

(cl-defstruct (phpinspect--completion
               (:constructor phpinspect--construct-completion))
  "Contains a possible completion value with all it's attributes."
  (target nil
          :documentation
          "The object that this completion is aimed at/completing towards")
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
  ;; Ignore completions in an invalid state (nil values)
  (when (phpinspect--completion-value completion)
    (setf (phpinspect--completion-list-has-candidates comp-list) t)
    (unless (intern-soft (phpinspect--completion-value completion)
                         (phpinspect--completion-list-completions comp-list))
      (set (intern (phpinspect--completion-value completion)
                   (phpinspect--completion-list-completions comp-list))
           completion))))

(cl-defmethod phpinspect--completion-list-get-metadata
  ((comp-list phpinspect--completion-list) (completion-name string))
  (let ((comp-sym (intern-soft completion-name
                               (phpinspect--completion-list-completions comp-list))))
    (when comp-sym
      (symbol-value comp-sym))))


(cl-defmethod phpinspect--completion-list-strings
  ((comp-list phpinspect--completion-list))
  (let (strings)
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
    (phpinspect--log "Returning region for variable subject %s" (phpinspect-meta-string subject))
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
    (phpinspect--log "Returning region for attribute access subject %s" (phpinspect-meta-string subject))
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
    (phpinspect--log "[comp-static-attribute] Returning region for attribute access subject %s"
                     (phpinspect-meta-string subject))
    (list (phpinspect-attrib-start subject) (phpinspect-meta-end subject))))

(cl-defmethod phpinspect-comp-strategy-execute
  ((_strat phpinspect-comp-static-attribute) (_q phpinspect-completion-query)
   (rctx phpinspect--resolvecontext))
  (phpinspect-suggest-attributes-at-point rctx 'static))

(cl-defstruct (phpinspect-comp-word (:constructor phpinspect-make-comp-word))
  "Comletion strategy for bare words")

(defun phpinspect-keyword-body-but-not-function-p (token)
  (and (phpinspect-keyword-body-p token)
       (not (phpinspect-function-p token))))

(cl-defmethod phpinspect-comp-strategy-supports
  ((_strat phpinspect-comp-word) (q phpinspect-completion-query)
   (rctx phpinspect--resolvecontext))
  "Determine suitability of comp-word strategy to resolve Q.

Words can be type names, function names and keywords.  This
strategy is a bit special in the sense that it needs to carefully
consider whether or not it is useful to complete words at point.
Completing words in a comment for example, is usually not useful."
  ;; Don't complete words when editing comments
  (unless (seq-find (phpinspect-meta-token-predicate #'phpinspect-comment-p)
                    (phpinspect-buffer-tokens-enclosing-point
                     (phpinspect-completion-query-buffer q)
                     (phpinspect-completion-query-point q)))
    (or
     ;; Complete word being typed
     (when-let ((subject (phpinspect-completion-subject-at-point
                          (phpinspect-completion-query-buffer q)
                          (phpinspect-completion-query-point q)
                          #'phpinspect-word-p)))
       (list (phpinspect-meta-start subject) (phpinspect-meta-end subject)))

     ;; Point is right after a "new" token. Complete "newable" types.
     (when-let ((token-before (phpinspect-bmap-last-token-before-point
                               (phpinspect-buffer-map
                                (phpinspect-completion-query-buffer q))
                               (phpinspect-completion-query-point q)))
                ((phpinspect-new-p (phpinspect-meta-token token-before))))
       (list (phpinspect-completion-query-point q)
             (phpinspect-completion-query-point q)))

   ;; Point is after a scope, static, declaration, const or other keyword that
   ;; can precede a type or another word.
   (and (or
         (phpinspect-keyword-body-but-not-function-p
          (car (phpinspect--resolvecontext-enclosing-tokens rctx)))
         (phpinspect-keyword-body-but-not-function-p
          (car (phpinspect--resolvecontext-subject rctx))))

        (list (phpinspect-completion-query-point q)
              (phpinspect-completion-query-point q))))))

(cl-defmethod phpinspect-comp-strategy-execute
  ((_strat phpinspect-comp-word) (q phpinspect-completion-query)
   (rctx phpinspect--resolvecontext))
  (if-let ((subject (phpinspect-bmap-last-token-before-point
                     (phpinspect-buffer-map
                      (phpinspect-completion-query-buffer q))
                     (phpinspect-completion-query-point q)))
           (parent (car (phpinspect--resolvecontext-enclosing-metadata rctx)))
           ;; Parent is a declaration token
           ((phpinspect-declaration-p (phpinspect-meta-token parent)))
           (grandparent (phpinspect-meta-parent parent))
           ;; Grandparent is a function, so we're in a function declaration
           ((phpinspect-function-p (phpinspect-meta-token grandparent)))
           ;; We're in a function declaration, before the argument list. We're
           ;; probably editing a function name, so it is not helpful to complete
           ;; here.
           ((seq-find #'phpinspect-list-p
                      (mapcar #'phpinspect-meta-token
                              (phpinspect-meta-right-siblings subject)))))
      ;; Return nil, as completing words to name unnamed functions is not
      ;; useful.
      nil
    (phpinspect-suggest-words-at-point rctx)))

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

(defun phpinspect--find-atoms-start (point)
  "Determine the start of a sequence of atoms before POINT."
  (save-excursion
    (goto-char point)
    (if (looking-back (phpinspect--atom-regexp) nil t)
        (- point (length (match-string 0)))
      point)))

(defun phpinspect--completion-query-maybe-should-cache (last-query query)
  (and last-query
       (eq (phpinspect-completion-query-buffer last-query)
           (phpinspect-completion-query-buffer query))
       (let ((taints (phpinspect-edtrack-taint-pool
                      (phpinspect-buffer-edit-tracker
                       (phpinspect-completion-query-buffer query))))
             (atoms-start
              (phpinspect-with-current-buffer (phpinspect-completion-query-buffer query)
                (phpinspect--find-atoms-start (phpinspect-completion-query-point query)))))
         (or (length= taints 0)
             (and (length= taints 1)
                  (<= atoms-start
                      (phpinspect-completion-query-point last-query))
                  (>= (phpinspect-taint-end (car taints))
                      (phpinspect-completion-query-point last-query))
                  (>= 1 (abs (- (phpinspect-completion-query-point query)
                                (phpinspect-taint-end (car taints))))))))))

(cl-defstruct (phpinspect--completion-parameters
               (:constructor phpinspect--make-completion-parameters))
  "Parameters used for completion. Used to detect caching possibilities."
  (query nil)
  (subject nil)
  (strategy nil))

(defvar phpinspect--last-completion-parameters nil
  "Used internally to probe for opportunities to re-use the last
completion result.")

(cl-defmethod phpinspect-completion-query-execute ((query phpinspect-completion-query))
  "Execute QUERY.

Returns list of `phpinspect--completion'."
  (let* ((last-parameters phpinspect--last-completion-parameters)
         ;; Check if caching is at all possible, before parsing the buffer. This
         ;; needs to happen now, as tainted regions are removed from the taint
         ;; pool after a buffer parse. We need the tainted region to determine
         ;; if the only edit is one performed during completion.
         (maybe-cache? (and
                        last-parameters
                        (phpinspect--completion-query-maybe-should-cache
                         (phpinspect--completion-parameters-query last-parameters)
                         query))))

    (let* ((buffer (phpinspect-completion-query-buffer query))
           (point (phpinspect-completion-query-point query))
           (buffer-map (phpinspect-buffer-parse-map buffer))
           (rctx (phpinspect-get-resolvecontext
                  (phpinspect-buffer-project buffer) buffer-map point))
           (completion-list (phpinspect--make-completion-list)))

      (phpinspect-buffer-update-project-index buffer)

      (setq phpinspect--last-completion-list
            (catch 'phpinspect--return
              (dolist (strategy phpinspect-completion-strategies)
                (when-let (region (phpinspect-comp-strategy-supports strategy query rctx))
                  ;; Check if using the cached completion list is possible.
                  (if-let ((maybe-cache?)
                           ;; There is a previous list available
                           (last-list phpinspect--last-completion-list)
                           ;; The list had candidates in it
                           ((phpinspect--completion-list-has-candidates last-list))
                           ;; The subject of the last resolvecontext is the same
                           ;; (so likely to evaluate to the same results).
                           ((equal (butlast (phpinspect--resolvecontext-subject rctx))
                                   (butlast (phpinspect--completion-parameters-subject
                                             last-parameters))))
                           ;; The completion strategy is the same as the last
                           ;; one used.
                           ((eq strategy
                                (phpinspect--completion-parameters-strategy
                                 last-parameters))))
                      ;; We can safely use the cached list: All parameters used
                      ;; for the last completion seem to match the current one.
                      (progn
                        ;; Update the region, this is necessary for
                        ;; completion-at-point to determine what is being
                        ;; completed.
                        (setf (phpinspect--completion-list-completion-start last-list)
                              (car region)
                              (phpinspect--completion-list-completion-end last-list)
                              (cadr region))

                        (throw 'phpinspect--return last-list))

                    ;; We can't use the cached list, proceed executing the
                    ;; strategy.
                    (setf (phpinspect--completion-list-completion-start completion-list)
                          (car region)
                          (phpinspect--completion-list-completion-end completion-list)
                          (cadr region))

                    ;; update last used parameters
                    (setq phpinspect--last-completion-parameters
                          (phpinspect--make-completion-parameters
                           :subject (phpinspect--resolvecontext-subject rctx)
                           :strategy strategy
                           :query query))

                    (phpinspect--log "Found matching completion strategy. Executing...")
                    (dolist (candidate (phpinspect-comp-strategy-execute strategy query rctx))
                      (phpinspect--completion-list-add
                       completion-list (phpinspect--make-completion candidate)))

                    (throw 'phpinspect--return completion-list))))
                ;; return empty list
                completion-list))))

  (phpinspect--log "Returning completion list %s" phpinspect--last-completion-list)
  phpinspect--last-completion-list)


;; FIXME: completions should be stored in an LRU cache keyed with object
;; pointers of the things they represent (variables/functions). Re-creating them
;; for each completion causes hiccups due to the sheer number of completion in
;; some cases. This probably also causes the GC to kick in, making matters
;; worse.
;;;;
;; [2024-08-30 12:19] => A significant part of the performance problem seems to
;; be mitigated by the memoization of `phpinspect--type-format-display-name',
;; which is used to format/propertize return- and parameter types for
;; completions. If performance is still problematic at a later date, for example
;; on less powerful machines, consider implementing an LRU.
(defun phpinspect-make-fn-completion (completion-candidate)
  (phpinspect--construct-completion
   :value (phpi-fn-name completion-candidate)
   :meta (concat "(" (mapconcat (lambda (arg)
                                  (concat "$" (if (> (length (car arg)) 8)
                                                  (truncate-string-to-width (car arg) 8 nil)
                                                (car arg))))
                                (phpi-fn-arguments completion-candidate)
                                ", ")
                 ") "
                 (phpinspect--display-format-type-name (phpi-fn-return-type completion-candidate)))
   :annotation (concat " "
                       (phpinspect--type-bare-name
                        (phpi-fn-return-type completion-candidate)))
   :target completion-candidate
   :kind 'function))


(cl-defmethod phpinspect--make-completion
  ((completion-candidate phpinspect--function))
  "Create a `phpinspect--completion` for COMPLETION-CANDIDATE."
  (phpinspect-make-fn-completion completion-candidate))

(cl-defmethod phpinspect--make-completion
  ((completion-candidate phpinspect-method))
  "Create a `phpinspect--completion` for COMPLETION-CANDIDATE."
  (phpinspect-make-fn-completion completion-candidate))

(cl-defmethod phpinspect--make-completion ((completion-candidate phpinspect-property))
  (phpinspect--make-completion (phpi-prop-definition completion-candidate)))

(cl-defmethod phpinspect--make-completion ((type phpinspect--type))
  (phpinspect--construct-completion
   :value (propertize (phpinspect--type-bare-name type) 'face 'font-lock-type-face)
   :meta (phpinspect--format-type-name type)
   :target type
   :kind 'class))

(cl-defmethod phpinspect--make-completion
  ((completion-candidate phpinspect--variable))
  (phpinspect--construct-completion
   :value (phpinspect--variable-name completion-candidate)
   :meta (phpinspect--display-format-type-name
          (or (phpinspect--variable-type completion-candidate)
              phpinspect--null-type))
   :target completion-candidate
   :annotation (concat " "
                       (phpinspect--type-bare-name
                        (or (phpinspect--variable-type completion-candidate)
                            phpinspect--null-type)))
   :kind 'variable))

(cl-defmethod phpinspect--make-completion ((keyword phpinspect-suggest-keyword))
  (phpinspect--construct-completion
   :value (propertize (phpinspect-suggest-keyword-word keyword) 'face 'font-lock-keyword-face)
   :target keyword
   :kind 'keyword
   :meta "keyword"))

(define-inline phpinspect--prefix-for-completion (completion)
  (inline-letevals (completion)
    (inline-quote
     (pcase (phpinspect--completion-kind ,completion)
       ('function "<f> ")
       ('variable "<va> ")))))


(defun phpinspect-complete-at-point ()
  (catch 'phpinspect-parse-interrupted
    (let ((comp-list (phpinspect-completion-query-execute (phpinspect--get-completion-query)))
          strings)
      (phpinspect--log "Completion list: %s" comp-list)
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
                                   (concat " " (phpinspect--completion-meta completion)))
                             affixated))
                     (nreverse affixated)))
                 :exit-function
                 (lambda (comp-name state)
                   (let ((comp (phpinspect--completion-list-get-metadata
                                phpinspect--last-completion-list
                                comp-name)))
                     (when (and (eq 'finished state)
                                (eq 'function (phpinspect--completion-kind comp)))
                       (insert "(")
                       (when (= 0 (length (phpi-fn-arguments
                                           (phpinspect--completion-target comp))))
                         (insert ")")))))
                 :company-kind (lambda (comp-name)
                                 (let ((comp
                                        (phpinspect--completion-list-get-metadata
                                         phpinspect--last-completion-list
                                         comp-name)))
                                   (if comp
                                       (phpinspect--completion-kind comp)
                                     (phpinspect--log  "Unable to find matching completion for name %s" comp-name)
                                     nil))))))))

(provide 'phpinspect-completion)
