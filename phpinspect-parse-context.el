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
(require 'phpinspect-thread)

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
    :documentation
    "The time at which the currently active parse cycle started.")
  (collaborative
   nil
   :type boolean
   :documentation
   "When this is non-nil and the parser is not running in the main
thread, the thread will shortly yield before parsing a token.

Only use this when the content of the buffer to be parsed is
guaranteed not to change while parsing.")
  (change
   nil
   :type phpinspect-change
   :documentation
   "The change that happend after the parser produced
 `phpinspect-pctx-previous-bmap'")
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

(define-inline phpinspect-pctx-register-whitespace (pctx whitespace)
  (inline-letevals (pctx)
    (inline-quote
     (setf (phpinspect-pctx-whitespace-before ,pctx) ,whitespace))))

(defsubst phpinspect-pctx-consume-whitespace (pctx)
  (let ((whitespace (phpinspect-pctx-whitespace-before pctx)))
    (setf (phpinspect-pctx-whitespace-before pctx) "")
    whitespace))

(define-inline phpinspect-pctx-check-yield (pctx)
  "Yield the current thread before each parsed token.

If the current thread is the main thread, the thread will not be
yielded.

The parser will start yielding before each parsed token when
`phpinspect--pctx-start-time' exceeds
`phpinspect-pctx-interrupt-threshold'."
  (inline-letevals (pctx)
    (inline-quote
     (progn
       (unless (phpinspect-pctx--start-time ,pctx)
         (setf (phpinspect-pctx--start-time ,pctx) (time-convert nil t)))

       ;; Interrupt when blocking too long while input is pending.
       (when (time-less-p (phpinspect-pctx-interrupt-threshold ,pctx)
                          (time-since (phpinspect-pctx--start-time ,pctx)))
         (phpi-thread-yield))))))



(provide 'phpinspect-parse-context)
;;; phpinspect-parse-context.el ends here
