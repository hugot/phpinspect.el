;;; phpinspect-parse-context.el --- PHP parsing context module  -*- lexical-binding: t; -*-

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

;; A parse context is an object which is used during the lifetime of a parse
;; cycle. Several variables can be set through the parse context to influence
;; the behavior of the parser.

;; See also M-x cl-describe-type RET phpinspect-pctx for a more readable
;; overview of the slot documentation.

;;; Code:

(require 'phpinspect-util)
(require 'phpinspect-meta)
(require 'phpinspect-changeset)

(defvar phpinspect-parse-context nil
  "An instance of `phpinspect-pctx' that is used when
parsing. Usually used in combination with
`phpinspect-with-parse-context'")

(cl-defstruct (phpinspect-pctx (:constructor phpinspect-make-pctx))
  "Parser Context"
  (incremental
   nil
   :type boolean
   :documentation
   "A non-nil value enables incremental parsing.")
  (interrupt-threshold
   (time-convert '(0 0 2000 0) t)
   :documentation
   "After how much time `interrupt-predicate' should be consulted.
This is 2ms by default.")
  (-start-time
   nil
   :documentation "The time at which the currently active parse cycle started.
This slot is for private use and does not always have a value.")
  (change nil :type phpinspect-change)
  (interrupt-predicate
   nil
   :documentation
   "When non-nil, this should be a function. When the parse time
exceeds the configured interrupt-threshold, this function will be
called after each parsed token to make the final decision of
interrupting the parser. If this function returns a non-nil
value, the parse process is interrupted and the symbol
`phpinspect-parse-interrupted' is signaled.")
;;   (changesets
;;    nil
;;    :type list
;;    :documentation
;;    "Restore points for metadata changes executed during this
;; parse. Usually populated through `phpinspect-meta-with-changeset'.")
;;   (edtrack
;;    nil
;;    :type phpinspect-edtrack
;;    :documentation
;;    "When parsing incrementally, the edit tracker is used to determine
;; whether a token from a previous parse (in the buffer map that is
;; in the `previous-bmap' slot) can be recycled or is tainted/edited
;; and should not be recycled.")
  (bmap
   nil
   :type phpinspect-bmap
   :documentation
   "The new buffer map to register metadata objects with.")
  (previous-bmap
   nil
   :type phpinspect-bmap
   :documentation
   "If set, this should be a buffer map containing the metadata
gathered during the previous parse cycle of the
buffer-to-be-parsed. Eligible tokens will be removed from the old
metadata tree and recycled in the new buffer map (in the `bmap'
slot of this structure.)")
  (whitespace-before
   ""
   :type string
   :documentation
   "A slot that is used by the parser to store whitespace which is
encountered before each parsed token. Whitespace is not parsed as
a regular token to avoid pollution of the syntax tree with
useless metadata tokens."))

(define-inline phpinspect-pctx-whitespace-before-length (ctx)
  (inline-quote (length (phpinspect-pctx-whitespace-before ,ctx))))

(defmacro phpinspect-with-parse-context (ctx &rest body)
  "Set the currently active parce context to CTX and execute body.

If BODY signals an error, `phpinspect-pctx-cancel' is called in
an attempt to revert all changes made to the metadata tree while
parsing incrementally.

The error signal is not intercepted and will still need to be
handled by the code using this macro."
  (declare (indent 1))
  `(dlet ((phpinspect-parse-context ,ctx))
     (progn ,@body)))

(defmacro phpinspect-pctx-save-whitespace (pctx &rest body)
  (declare (indent 1))
  (let ((save-sym (gensym)))
    `(let ((,save-sym (phpinspect-pctx-whitespace-before ,pctx)))
       (unwind-protect
           (progn
             (setf (phpinspect-pctx-whitespace-before ,pctx) "")
             ,@body)
         (setf (phpinspect-pctx-whitespace-before ,pctx) ,save-sym)))))

;; (define-inline phpinspect-pctx-register-changeset (pctx changeset)
;;   (inline-quote
;;    (progn
;;      (push ,changeset (phpinspect-pctx-changesets ,pctx)))))

;; (define-inline phpinspect-meta-with-changeset (meta &rest body)
;;   "Perform mutations on META in BODY, saving changes.

;; Before BODY is executed, important slots of META are stored in a
;; changeset object and appended to the changesets slot of the
;; currently active parse context. The original state of META can be
;; restored by calling `phpinspect-pctx-cancel'."
;;   (declare (indent 1))
;;   (inline-letevals (meta)
;;     (push 'progn body)
;;     (inline-quote
;;      (progn
;;        (when phpinspect-parse-context
;;          (phpinspect-pctx-register-changeset
;;           phpinspect-parse-context (phpinspect-make-changeset ,meta)))
;;        ,body))))

(define-inline phpinspect-pctx-check-interrupt (pctx)
  "Signal `phpinspect-parse-interrupted' when conditions are met.

Parsing will be interrupted when the time passed since
`phpinspect--pctx-start-time' exceeds
`phpinspect-pctx-interrupt-threshold' and
`phpinspect-pctx-interrupt-predicate' returns non-nil.

When parsing is interrupted, any changes made to buffer token
metadata will be reverted in a call to `pphinspect-pctx-cancel'."
  (inline-letevals (pctx)
    (inline-quote
     (progn
       (unless (phpinspect-pctx--start-time ,pctx)
         (setf (phpinspect-pctx--start-time ,pctx) (time-convert nil t)))

       ;; Interrupt when blocking too long while input is pending.
       (when (and (time-less-p (phpinspect-pctx-interrupt-threshold ,pctx)
                               (time-since (phpinspect-pctx--start-time ,pctx)))
                  (funcall (phpinspect-pctx-interrupt-predicate ,pctx)))
         (throw 'phpinspect-parse-interrupted nil))))))

(define-inline phpinspect-pctx-register-whitespace (pctx whitespace)
  (inline-letevals (pctx)
    (inline-quote
     (setf (phpinspect-pctx-whitespace-before ,pctx) ,whitespace))))

(defsubst phpinspect-pctx-consume-whitespace (pctx)
  (let ((whitespace (phpinspect-pctx-whitespace-before pctx)))
    (setf (phpinspect-pctx-whitespace-before pctx) "")
    whitespace))

;; (defun phpinspect-pctx-cancel (pctx)
;;   "Cancel PCTX, revert all changes made during its lifetime.

;; Revert all changes made to the metadata tree while parsing
;; incrementally. This function is usually called by
;; `phpinspect-pctx-check-interrupt' when interrupt conditions are
;; met."
;;   (dolist (changeset (phpinspect-pctx-changesets pctx))
;;     (phpinspect-changeset-revert changeset))
;;   (setf (phpinspect-pctx-changesets pctx) nil))

(provide 'phpinspect-parse-context)
;;; phpinspect-parse-context.el ends here
