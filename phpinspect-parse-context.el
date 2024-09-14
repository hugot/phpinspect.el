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
  (incremental nil)
  (meta-iterator nil)
  (interrupt-threshold (time-convert '(0 0 2000 0) t)
                       :documentation
                       "After how much time `interrupt-predicate'
should be polled. This is 2ms by default.")
  (-start-time nil
               :documentation "The time at which the parse started.
This variable is for private use and not always set.")
  (interrupt-predicate nil
                      :documentation
                      "A function that is called in intervals during parsing when
set. If this function returns a non-nil value, the parse process
is interrupted and the symbol `phpinspect-parse-interrupted' is
thrown.")
  (changesets nil
              :type list
              :documentation "Metadata change sets executed during this parse")
  (edtrack nil
           :type phpinspect-edtrack)
  (bmap nil
        :type phpinspect-bmap)
  (previous-bmap nil
                 :type phpinspect-bmap)
  (whitespace-before ""
                     :type string))

(define-inline phpinspect-pctx-whitespace-before-length (ctx)
  (inline-quote (length (phpinspect-pctx-whitespace-before ,ctx))))

(defmacro phpinspect-with-parse-context (ctx &rest body)
  (declare (indent 1))
  (let ((old-ctx (gensym))
        (completed (gensym))
        (result (gensym)))
    `(let ((,old-ctx phpinspect-parse-context)
           (,result)
           (,completed))
       (unwind-protect
           (progn
             (setq phpinspect-parse-context ,ctx
                   ,result (progn ,@body)
                   ,completed t)
             ,result)
         (progn
           (unless ,completed (phpinspect-pctx-cancel ,ctx))
           (setq phpinspect-parse-context ,old-ctx))))))

(defmacro phpinspect-pctx-save-whitespace (pctx &rest body)
  (declare (indent 1))
  (let ((save-sym (gensym)))
    `(let ((,save-sym (phpinspect-pctx-whitespace-before ,pctx)))
       (unwind-protect
           (progn
             (setf (phpinspect-pctx-whitespace-before ,pctx) "")
             ,@body)
         (setf (phpinspect-pctx-whitespace-before ,pctx) ,save-sym)))))

(define-inline phpinspect-pctx-register-changeset (pctx changeset)
  (inline-quote
   (progn
     (push ,changeset (phpinspect-pctx-changesets ,pctx)))))

(define-inline phpinspect-meta-with-changeset (meta &rest body)
  (declare (indent 1))
  (inline-letevals (meta)
    (push 'progn body)
    (inline-quote
     (progn
       (when phpinspect-parse-context
         (phpinspect-pctx-register-changeset
          phpinspect-parse-context (phpinspect-make-changeset ,meta)))
       ,body))))


(define-inline phpinspect-pctx-check-interrupt (pctx)
  (inline-letevals (pctx)
    (inline-quote
     (progn
       (unless (phpinspect-pctx--start-time ,pctx)
         (setf (phpinspect-pctx--start-time ,pctx) (time-convert nil t)))

       ;; Interrupt when blocking too long while input is pending.
       (when (and (time-less-p (phpinspect-pctx-interrupt-threshold ,pctx)
                               (time-since (phpinspect-pctx--start-time ,pctx)))
                  (funcall (phpinspect-pctx-interrupt-predicate ,pctx)))
         (phpinspect-pctx-cancel ,pctx)
         (throw 'phpinspect-parse-interrupted nil))))))

(define-inline phpinspect-pctx-register-whitespace (pctx whitespace)
  (inline-letevals (pctx)
    (inline-quote
     (setf (phpinspect-pctx-whitespace-before ,pctx) ,whitespace))))

(defsubst phpinspect-pctx-consume-whitespace (pctx)
  (let ((whitespace (phpinspect-pctx-whitespace-before pctx)))
    (setf (phpinspect-pctx-whitespace-before pctx) "")
    whitespace))

(defun phpinspect-pctx-cancel (pctx)
  (phpinspect--log "Cancelling parse context")
  (dolist (changeset (phpinspect-pctx-changesets pctx))
    (phpinspect-changeset-revert changeset))
  (setf (phpinspect-pctx-changesets pctx) nil))

(provide 'phpinspect-parse-context)
;;; phpinspect-parse-context.el ends here
