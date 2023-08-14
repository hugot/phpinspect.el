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
(require 'phpinspect-util)
(require 'phpinspect-meta)
(require 'phpinspect-parser)
(require 'phpinspect-resolve)

(defvar phpinspect-eldoc-word-width 14
  "The maximum width of words in eldoc strings.")

(cl-defstruct (phpinspect-eldoc-query (:constructor phpinspect-make-eldoc-query))
  (point 0
         :type integer
         :documentation "Position in buffer for which to provide hints")
  (buffer nil
          :type phpinspect-buffer))


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
  ((_strat phpinspect-eld-attribute) (_q phpinspect-eldoc-query) (rctx phpinspect--resolvecontext))
  (phpinspect-attrib-p (car (last (phpinspect--resolvecontext-subject rctx)))))

(cl-defmethod phpinspect-eld-strategy-execute
  ((_strat phpinspect-eld-attribute) (_q phpinspect-eldoc-query) (rctx phpinspect--resolvecontext))
  (let ((attrib (car (last (phpinspect--resolvecontext-subject rctx))))
        type-before)
    (setf (phpinspect--resolvecontext-subject rctx) (butlast (phpinspect--resolvecontext-subject rctx)))
    (setq type-before (phpinspect-resolve-type-from-context rctx))

    (when type-before
      (let ((class (phpinspect-project-get-class-create
                    (phpinspect--resolvecontext-project rctx)
                    type-before))
            (attribute-name (cadadr attrib))
            variable method result)
        (when attribute-name
          (cond ((phpinspect-static-attrib-p attrib)
                 (setq variable (phpinspect--class-get-variable class attribute-name))

                 (if (and variable
                          (or (phpinspect--variable-static-p variable)
                              (phpinspect--variable-const-p variable)))
                     (setq result variable)
                   (setq method (phpinspect--class-get-static-method
                                 class (phpinspect-intern-name attribute-name)))
                   (when method
                     (setq result (phpinspect-make-function-doc :fn method)))))
                ((phpinspect-object-attrib-p attrib)
                 (setq variable (phpinspect--class-get-variable class attribute-name))

                 (if (and variable
                          (phpinspect--variable-vanilla-p variable))
                     (setq result variable)
                   (setq method (phpinspect--class-get-method
                                 class (phpinspect-intern-name attribute-name)))
                   (when method
                     (setq result (phpinspect-make-function-doc :fn method))))))
          result)))))

(cl-defstruct (phpinspect-eld-function-args (:constructor phpinspect-make-eld-function-args))
  "Eldoc strategy for function arguments.")

(cl-defmethod phpinspect-eld-strategy-supports
  ((_strat phpinspect-eld-function-args) (_q phpinspect-eldoc-query) (rctx phpinspect--resolvecontext))
  (let ((parent-token (car (phpinspect--resolvecontext-enclosing-tokens rctx))))
    ;; When our subject is inside a list, it is probably an argument of a
    ;; function/method call, which is what this strategy provides information for.
    (or (phpinspect-list-p parent-token)
        ;; When the last token in our subject is a list, we're either at the end
        ;; of a buffer in an incomplete argument list (no closing paren), or in
        ;; an empty argument list of a function call.
        (phpinspect-list-p
         (car (last (phpinspect--resolvecontext-subject rctx)))))))

(cl-defmethod phpinspect-eld-strategy-execute
  ((_strat phpinspect-eld-function-args) (q phpinspect-eldoc-query) (rctx phpinspect--resolvecontext))
  (phpinspect--log "Executing `phpinspect-eld-function-args' strategy")
  (let* ((enclosing-token (car (phpinspect--resolvecontext-enclosing-metadata
                                 rctx)))
         (left-sibling )
         (statement )
         match-result static arg-list arg-pos)

    (cond
     ;; Subject is a statement
     ((and (phpinspect-list-p (car (last (phpinspect--resolvecontext-subject rctx))))
           enclosing-token)

      (setq left-sibling (phpinspect-meta-find-child-before-recursively
                          enclosing-token (phpinspect-eldoc-query-point q))))
    ;; Subject is inside an argument list
     ((and enclosing-token
                (phpinspect-list-p (phpinspect-meta-token enclosing-token)))
      (setq left-sibling (phpinspect-meta-find-left-sibling enclosing-token)
            statement (list enclosing-token))))

    (phpinspect--log "Left sibling: %s" (phpinspect-meta-string left-sibling))
    (phpinspect--log "Enclosing parent: %s" (phpinspect-meta-string (phpinspect-meta-parent enclosing-token)))


    (while (and left-sibling
                (not (phpinspect-statement-introduction-p (phpinspect-meta-token left-sibling))))
      (unless (phpinspect-comment-p (phpinspect-meta-token left-sibling))
        (push left-sibling statement))
      (setq left-sibling (phpinspect-meta-find-left-sibling left-sibling)))

    (phpinspect--log "Eldoc statement is:  %s" (mapcar #'phpinspect-meta-token statement))
    (phpinspect--log "Enclosing token was: %s" (phpinspect-meta-token enclosing-token))

    (when enclosing-token
      (cond
       ;; Method call
       ((setq match-result (phpinspect--match-sequence (last statement 2)
                             :f (phpinspect-meta-wrap-token-pred #'phpinspect-attrib-p)
                             :f (phpinspect-meta-wrap-token-pred #'phpinspect-list-p)))
        (phpinspect--log "Eldoc context is a method call")

        (setq arg-list (car (last match-result))
              static (phpinspect-static-attrib-p (phpinspect-meta-token (car match-result)))
              arg-pos (seq-reduce
                       (lambda (count meta)
                         (if (phpinspect-comma-p (phpinspect-meta-token meta))
                             (+ count 1)
                           count))
                       (phpinspect-meta-find-children-before arg-list (phpinspect-eldoc-query-point q)) 0))

        ;; Set resolvecontext subject to the statement minus the method
        ;; name. Point is likely to be at a location inside a method call like
        ;; "$a->b->doSomething(". The resulting subject should be "$a->b".
        (setf (phpinspect--resolvecontext-subject rctx)
              (mapcar #'phpinspect-meta-token (butlast statement 2)))

        (when-let* ((type-of-previous-statement
                     (phpinspect-resolve-type-from-context rctx))
                    (method-name-sym (phpinspect-intern-name (cadadr (phpinspect-meta-token (car match-result)))))
                    (class (phpinspect-project-get-class-create
                            (phpinspect--resolvecontext-project rctx)
                            type-of-previous-statement))
                    (method (if static
                                (phpinspect--class-get-static-method class method-name-sym)
                              (phpinspect--class-get-method class method-name-sym))))
          (when method
            (phpinspect-make-function-doc :fn method :arg-pos arg-pos))))
       ((setq match-result (phpinspect--match-sequence (last statement 2)
                             :f (phpinspect-meta-wrap-token-pred #'phpinspect-word-p)
                             :f (phpinspect-meta-wrap-token-pred #'phpinspect-list-p)))
        (phpinspect--log "Eldoc context is a function call")

        (setq arg-list (car (last match-result))
              arg-pos (seq-reduce
                       (lambda (count meta)
                         (if (phpinspect-comma-p (phpinspect-meta-token meta))
                             (+ count 1)
                           count))
                       (phpinspect-meta-find-children-before arg-list (phpinspect-eldoc-query-point q)) 0))

        (let ((func (phpinspect-project-get-function
                     (phpinspect--resolvecontext-project rctx)
                     (phpinspect-intern-name (cadr (phpinspect-meta-token (car match-result)))))))
          (phpinspect--log "Got past that")
          (when func
            (phpinspect-make-function-doc :fn func :arg-pos arg-pos))))))))

(cl-defmethod phpinspect-eldoc-string ((var phpinspect--variable))
  (concat (truncate-string-to-width
           (propertize (concat (if (phpinspect--variable-vanilla-p var) "$" "")
                               (phpinspect--variable-name var))
                       'face 'font-lock-variable-name-face)
           phpinspect-eldoc-word-width)
          ": "
          (propertize (phpinspect--format-type-name (phpinspect--variable-type var))
                      'face 'font-lock-type-face)))

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
             (propertize
              (phpinspect--format-type-name (phpinspect--function-return-type fn))
              'face 'font-lock-type-face))))

(defvar phpinspect-eldoc-strategies (list (phpinspect-make-eld-attribute)
                                          (phpinspect-make-eld-function-args))
  "The eldoc strategies that phpinspect is currently allowed to
employ. Strategies are queried in the order of this list. See
also `phpinspect-eldoc-query-execute'.")

(cl-defmethod phpinspect-eldoc-query-execute ((query phpinspect-eldoc-query))
  (let* ((buffer (phpinspect-eldoc-query-buffer query))
         (point (phpinspect-eldoc-query-point query))
         (buffer-map (phpinspect-buffer-parse-map buffer))
         (rctx (phpinspect-get-resolvecontext buffer-map point)))
    (phpinspect-buffer-update-project-index buffer)
    (catch 'matched
      (dolist (strategy phpinspect-eldoc-strategies)
        (when (phpinspect-eld-strategy-supports strategy query rctx)
          (phpinspect--log "Found matching eldoc strategy. Executing...")
          (throw 'matched (phpinspect-eld-strategy-execute strategy query rctx)))))))

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
