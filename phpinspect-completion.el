;;; phpinspect-type.el --- PHP parsing and completion package -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 3.0.1

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


(cl-defstruct (phpinspect--completion
               (:constructor phpinspect--construct-completion))
  "Contains a possible completion value with all it's attributes."
  (target nil
          :documentation
          "The object that this completion is aimed at/completing towards")
  (name nil :type string)
  (meta nil :type string)
  (annotation nil :type string)
  (kind nil :type symbol))

(cl-defgeneric phpinspect--make-completion (completion-candidate)
  "Creates a `phpinspect--completion` for a possible completion
candidate. Candidates can be indexed functions and variables.")


(cl-defstruct (phpi-comp-result
               (:constructor phpi-make-comp-result))
  "Contains all data for a completion at point"
  (completion-start nil
                    :type integer)
  (completion-end nil
                  :type integer)
  (query nil
         :type phpinspect-completion-query)
  (rctx nil
        :type phpinspect--resolvecontext)
  (strategy nil
            :type list)

  (-list nil
        :type phpinspect--completion-list))

(cl-defstruct (phpinspect--completion-list
               (:constructor phpinspect--make-completion-list))
  "Contains all data for a completion at point"
  (completions (make-hash-table :test #'equal :size 10000 :rehash-size 2)
               :type hash-table
               :documentation
               "A list of completion strings")
  (has-candidates nil))

(defun phpi-comp-result-list (result)
  (with-memoization (phpi-comp-result--list result)
    (let ((clist (phpinspect--make-completion-list)))
      (when (phpi-comp-result-strategy result)
        (dolist (candidate (phpinspect-comp-strategy-execute
                            (phpi-comp-result-strategy result)
                            (phpi-comp-result-query result)
                            (phpi-comp-result-rctx result)))
          (phpinspect--completion-list-add clist (phpinspect--make-completion candidate))))

      clist)))

(cl-defgeneric phpinspect--completion-list-add
    (comp-list completion)
  "Add a completion to a completion-list.")

(cl-defmethod phpinspect--completion-list-add
  ((comp-list phpinspect--completion-list) (completion phpinspect--completion))
  ;; Ignore completions in an invalid state (nil names)
  (when (phpinspect--completion-name completion)
    (setf (phpinspect--completion-list-has-candidates comp-list) t)
    (unless (gethash
             (phpinspect--completion-name completion)
             (phpinspect--completion-list-completions comp-list))
      (puthash (phpinspect--completion-name completion)
               completion
               (phpinspect--completion-list-completions comp-list)))))

(cl-defmethod phpinspect--completion-list-get-metadata
  ((comp-list phpinspect--completion-list) (completion-name string))
  (gethash completion-name (phpinspect--completion-list-completions comp-list)))

(cl-defmethod phpinspect--completion-list-strings
  ((comp-list phpinspect--completion-list))
  (hash-table-keys (phpinspect--completion-list-completions comp-list)))

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


(cl-defstruct (phpinspect--completion-parameters
               (:constructor phpinspect--make-completion-parameters))
  "Parameters used for completion. Used to detect caching possibilities."
  (query nil)
  (subject nil)
  (strategy nil))

(cl-defmethod phpinspect-completion-query-execute ((query phpinspect-completion-query))
  "Execute QUERY.

Returns list of `phpinspect--completion'."
  (let* (result thread)

    (setq thread
          (phpi-run-threaded "completion"
            (setq result
                  (let* ((buffer (phpinspect-completion-query-buffer query))
                         (point (phpinspect-completion-query-point query))
                         (buffer-map (phpinspect-buffer-parse-map buffer))
                         (rctx (phpinspect-get-resolvecontext
                                (phpinspect-buffer-project buffer) buffer-map point)))

                    (catch 'phpinspect--return
                      (dolist (strategy phpinspect-completion-strategies)
                        (when-let (region (phpinspect-comp-strategy-supports strategy query rctx))
                          (throw 'phpinspect--return
                                 (phpi-make-comp-result
                                  :completion-start (phpinspect-region-start region)
                                  :completion-end (phpinspect-region-end region)
                                  :rctx rctx
                                  :query query
                                  :strategy strategy)))))))))

    ;; This construction seems to be necessary while running as
    ;; completion-at-point function in the main thread. Using `sleep-for'
    ;; instead of waiting for a condition variable or joining the thread will
    ;; prevent scenarios in which emacs becomes unresponsive.
    (let ((cancelled t))
      (with-timeout (0.2)
        (while-no-input
          (while (thread-live-p thread)
            (sleep-for 0.01))
          (setq cancelled nil)))

      ;; If cancelled is non-nil, the completion query execution was interrupted
      ;; before it could be completed due to the time limit or user
      ;; input. Return an empty completion result.
      (if cancelled
          (phpi-make-comp-result
           :completion-start (point)
           :completion-end (point))
        result))))

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
   :name (phpi-fn-name completion-candidate)
   :meta (concat "(" (mapconcat (lambda (arg)
                                  (concat "$" (if (> (length (car arg)) 8)
                                                  (truncate-string-to-width (car arg) 8 nil)
                                                (car arg))))
                                (phpi-fn-arguments completion-candidate)
                                ", ")
                 ") "
                 (phpinspect--display-format-type-name (phpi-fn-return-type completion-candidate)))
   :annotation (concat " "
                       (phpinspect--type-base-name
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
   :name (propertize (phpinspect--type-base-name type) 'face 'font-lock-type-face)
   :meta (phpinspect--format-type-name type)
   :target type
   :kind 'class))

(cl-defmethod phpinspect--make-completion
  ((completion-candidate phpinspect--variable))
  (phpinspect--construct-completion
   :name (phpinspect--variable-name completion-candidate)
   :meta (phpinspect--display-format-type-name
          (or (phpinspect--variable-type completion-candidate)
              phpinspect--null-type))
   :target completion-candidate
   :annotation (concat " "
                       (phpinspect--type-base-name
                        (or (phpinspect--variable-type completion-candidate)
                            phpinspect--null-type)))
   :kind 'variable))

(cl-defmethod phpinspect--make-completion ((keyword phpinspect-suggest-keyword))
  (phpinspect--construct-completion
   :name (propertize (phpinspect-suggest-keyword-word keyword) 'face 'font-lock-keyword-face)
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
  (catch 'phpinspect-interrupted
    (when-let ((result (phpinspect-completion-query-execute (phpinspect--get-completion-query))))
      (list (phpi-comp-result-completion-start result)
            (phpi-comp-result-completion-end result)
                 (completion-table-dynamic (lambda (_)
                                             (phpinspect--completion-list-strings
                                              (phpi-comp-result-list result))))
                 :affixation-function
                 (lambda (completions)
                   (let (affixated completion)
                     (dolist (comp completions)
                       (setq completion (phpinspect--completion-list-get-metadata
                                         (phpi-comp-result-list result) comp))
                       (push (list comp (phpinspect--prefix-for-completion completion)
                                   (concat " " (phpinspect--completion-meta completion)))
                             affixated))
                     (nreverse affixated)))
                 :exit-function
                 (lambda (comp-name state)
                   (let ((comp (phpinspect--completion-list-get-metadata
                                (phpi-comp-result-list result)
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
                                         (phpi-comp-result-list result)
                                         comp-name)))
                                   (if comp
                                       (phpinspect--completion-kind comp)
                                     (phpinspect--log  "Unable to find matching completion for name %s" comp-name)
                                     nil)))

                 ;; Make non-exclusive if no strategy matched. A failure to
                 ;; match could be due to a timeout while waiting for the buffer
                 ;; to sync, in which case we don't want to have capf think that
                 ;; the function cannot complete at point at all.
                 :exclusive (if (phpi-comp-result-strategy result) 'yes 'no)))))

(provide 'phpinspect-completion)
