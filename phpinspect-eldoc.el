;;; phpinspect-eldoc.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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



(cl-defstruct (phpinspect-eldoc-query (:constructor phpinspect-make-eldoc-query))
  (point 0
         :type integer
         :documentation "Position in buffer for which to provide hints")
  (buffer nil
          :type phpinspect-buffer))

(cl-defmethod phpinspect-eldoc-query-execute ((query phpinspect-eldoc-query))
  (let* ((buffer (phpinspect-eldoc-query-buffer query))
         (point (phpinspect-eldoc-query-point query))
         (buffer-map (phpinspect-buffer-parse-map buffer))
         (rctx (phpinspect-get-resolvecontext buffer-map point)))
    (catch 'matched
      (dolist (strategy phpinspect-eldoc-strategies)
        (when (phpinspect-eld-strategy-supports strategy query rctx)
          (throw 'matched (phpinspect-eld-strategy-execute strategy query rctx)))))))

(cl-defgeneric phpinspect-eld-strategy-supports (strategy (query phpinspect-eldoc-query) (context phpinspect--resolvecontext))
  "Should return non-nil if STRATEGY should be deployed for QUERY
and CONTEXT. All strategies must implement this method.")

(cl-defgeneric phpinspect-eld-strategy-execute (strategy (query phpinspect-eldoc-query) (context phpinspect--resolvecontext))
  "Should return an object for which `phpinspect-eldoc-string' is implemented.")

(cl-defgeneric phpinspect-eldoc-string (response)
  "Should return a string to be displayed by eldoc. This needs to
be implemented for return values of `phpinspect-eld-strategy-execute'")

(cl-defstruct (phpinspect-eld-attribute (:constructor phpinspect-make-eld-attribute))
  "Eldoc strategy for object attributes.")

(cl-defmethod phpinspect-eld-strategy-supports
  ((strat phpinspect-eld-attribute) (q phpinspect-eldoc-query) (rctx phpinspect--resolvecontext))
  (phpinspect-attrib-p (car (last (phpinspect--resolvecontext-subject rctx)))))

;; (cl-defmethod phpinspect-eld-strategy-execute
;;   ((strat phpinspect-eld-attribute) (q phpinspect-eldoc-query) (rctx phpinspect--resolvecontext))
;;   (let ((attrib (car (last (phpinspect--resolvecontext-subject rctx))))
;;         type-before)
;;     (setf (phpinspect--resolvecontext-subject rctx) (butlast (phpinspect--resolvecontext-subject rctx)))
;;     (setq type-before (phpinspect-resolve-type-from-context rctx))

;;     (when type-before
;;       (let ((class (phpinspect-project-get-class-create
;;                     (phpinspect--resolvecontext-project rctx)
;;                     type-before))
;;             attribute)
;;         (cond ((phpinspect-static-attrib-p attrib)
;;                (setq attribute (or (phpinspect--class-get-variable


(cl-defstruct (phpinspect-eld-function-args (:constructor phpinspect-make-eld-function-args))
  "Eldoc strategy for function arguments.")

(cl-defmethod phpinspect-eld-strategy-supports
  ((strat phpinspect-eld-function-args) (q phpinspect-eldoc-query) (rctx phpinspect--resolvecontext))
  (let ((parent-token (car (phpinspect--resolvecontext-enclosing-tokens rctx))))
    ;; When our subject is inside a list, it is probably an argument of a
    ;; function/method call, which is what this strategy provides information for.
    (or (phpinspect-list-p parent-token)

        ;; When our subject IS a list, we're probably in an empty/incomplete
        ;; argument list.
        (and (= 1 (length (phpinspect--resolvecontext-subject rctx)))
             (phpinspect-list-p
              (car (last (phpinspect--resolvecontext-subject rctx)))))

        ;; When the last token in our subject is an incomplete list, we're
        ;; probably at the end of the buffer in an unfinished argument list.
        (phpinspect-incomplete-list-p
         (car (last (phpinspect--resolvecontext-subject rctx)))))))

(cl-defmethod phpinspect-eld-strategy-execute
  ((strat phpinspect-eld-function-args) (q phpinspect-eldoc-query) (rctx phpinspect--resolvecontext))
  (let* ((token-map (phpinspect-buffer-parse-map (phpinspect-eldoc-query-buffer q)))
         (enclosing-token (cadr (phpinspect--resolvecontext-enclosing-tokens
                                 rctx)))
         (statement (phpinspect-find-statement-before-point
                     token-map (phpinspect-bmap-token-meta token-map enclosing-token)
                     (phpinspect-eldoc-query-point q)))
         match-result static arg-list arg-pos)

    (when enclosing-token
      (cond
       ;; Method call
       ((setq match-result (phpinspect--match-sequence (last statement 2)
                             :f #'phpinspect-attrib-p
                             :f #'phpinspect-list-p))

        (setq arg-list (car (last match-result))
              static (phpinspect-static-attrib-p (car match-result))
              arg-pos (seq-reduce
                       (lambda (count token)
                         (if (and (phpinspect-comma-p token)
                                  (> (phpinspect-eldoc-query-point q)
                                     (phpinspect-meta-end
                                      (phpinspect-bmap-token-meta token-map token))))
                             (+ count 1)
                           count))
                       arg-list 0))

        ;; Set resolvecontext subject to the statement minus the method
        ;; name. Point is likely to be at a location inside a method call like
        ;; "$a->b->doSomething(". The resulting subject should be "$a->b".
        (setf (phpinspect--resolvecontext-subject rctx) (butlast statement 2))

        (let* ((type-of-previous-statement
                (phpinspect-resolve-type-from-context rctx))
               (method-name-sym (phpinspect-intern-name (car (cdadar match-result))))
               (class (phpinspect-project-get-class-create
                       (phpinspect--resolvecontext-project rctx)
                       type-of-previous-statement))
               (method (when class
                         (if static
                             (phpinspect--class-get-static-method class method-name-sym)
                           (phpinspect--class-get-method class method-name-sym)))))
          (when method
            (phpinspect-make-function-doc :fn method :arg-pos arg-pos))))))))

(cl-defstruct (phpinspect-function-doc (:constructor phpinspect-make-function-doc))
  (fn nil
      :type phpinspect--function)
  (arg-pos nil))

(cl-defmethod phpinspect-eldoc-string ((doc phpinspect-function-doc))
  (let ((fn (phpinspect-function-doc-fn doc))
        (arg-pos (phpinspect-function-doc-arg-pos doc))
        (arg-count 0))
    (concat (truncate-string-to-width
             (phpinspect--function-name fn) phpinspect-eldoc-word-width) ": ("
             (mapconcat
              (lambda (arg)
                (let ((doc-string
                       (concat "$" (truncate-string-to-width
                                    (car arg) phpinspect-eldoc-word-width)
                               (if (cadr arg) " " "")
                               (phpinspect--format-type-name (or (cadr arg) "")))))
                  (when (and arg-pos (= arg-count arg-pos))
                    (setq doc-string
                          (propertize
                           doc-string 'face 'eldoc-highlight-function-argument)))
                  (setq arg-count (+ arg-count 1))
                  doc-string))
              (phpinspect--function-arguments fn)
              ", ")
             "): "
             (phpinspect--format-type-name
              (phpinspect--function-return-type fn)))))

(defvar phpinspect-eldoc-strategies (list ;;(phpinspect-make-eld-attribute)
                                          (phpinspect-make-eld-function-args))
  "The eldoc strategies that phpinspect is currently allowed to
employ. Strategies are queried in the order of this list. See
also `phpinspect-eldoc-query-execute'.")

(defun phpinspect-eldoc-function ()
  "An `eldoc-documentation-function` implementation for PHP files.

Ignores `eldoc-argument-case` and `eldoc-echo-area-use-multiline-p`.

TODO:
 - Respect `eldoc-echo-area-use-multiline-p`
"
  (catch 'phpinspect-parse-interrupted
    (let ((resp (phpinspect-eldoc-query-execute
                 (phpinspect-make-eldoc-query
                  :buffer phpinspect-current-buffer
                  :point (phpinspect--determine-completion-point)))))
      (when resp
        (phpinspect-eldoc-string resp)))))


(provide 'phpinspect-eldoc)