;;; phpinspect-buffer.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(require 'phpinspect-thread)
(require 'phpinspect-parser)
(require 'phpinspect-bmap)
(require 'phpinspect-edtrack)
(require 'phpinspect-index)
(require 'phpinspect-resolvecontext)
(require 'phpinspect-resolve)
(require 'phpinspect-util)
(require 'phpinspect-typedef)
(require 'phpinspect-token-predicates)
(require 'phpinspect-change)

(phpinspect--declare-log-group 'buffer)

(defvar-local phpinspect-current-buffer nil
  "An instance of `phpinspect-buffer' local to the active
buffer. This variable is only set for buffers where
`phpinspect-mode' is active. Also see `phpinspect-buffer'.")


(cl-defstruct (phpinspect-buffer (:constructor phpinspect-make-buffer))
  "An object containing phpinspect related metadata linked to an
emacs buffer."
  (buffer nil
          :type buffer
          :documentation "The associated emacs buffer")
  (shadow nil
          :type buffer)
  (tree nil
        :documentation
        "Parsed token tree that resulted from last parse")
  (map nil
       :type phpinspect-bmap)
  (-query-cache (make-hash-table :test 'equal :size 20 :rehash-size 2))
  (-tokens nil)
  (last-change nil :type phpinspect-change)
  (token-index (make-hash-table :test 'eq :size 100 :rehash-size 1.5))
  (-project nil
            :type phpinspect-project))

(defmacro phpinspect-buffer--query-with-cache (buffer label &rest body)
  (declare (indent 2))
  `(with-memoization (gethash ,label (phpinspect-buffer--query-cache ,buffer))
     ,@body))

(defun phpinspect-buffer--clear-query-cache (buffer)
  (setf (phpinspect-buffer--query-cache buffer)
        (make-hash-table :test 'equal :size 20 :rehash-size 2)))

(defun phpinspect-buffer-project (buffer)
  (or (phpinspect-buffer--project buffer)
      (with-current-buffer (phpinspect-buffer-buffer buffer)
        (phpinspect--cache-get-project-create (phpinspect--get-or-create-global-cache)
                                              (phpinspect-current-project-root)))))

(defun phpi-shadow-kill (shadow)
  (phpi-thread-kill (phpi-shadow-thread shadow))
  (kill-buffer (phpi-shadow-buffer shadow)))

(defun phpinspect-buffer-tainted-p (buffer)
  "Whether or not BUFFER's current tree needs updating to incorporate edits."
  (not (phpi-shadow-synced-p (phpinspect-buffer-shadow buffer))))

(defun phpinspect-buffer-needs-parse-p (buffer)
  "Whether or not BUFFER needs to be parsed for an updated tree to be present."
  (or (not (phpinspect-buffer-tree buffer))
      (phpinspect-buffer-tainted-p buffer)))

(defun phpinspect-buffer-fresh-p (buffer)
  "Whether or not BUFFER's metadata is fresh.

A buffer's metadata is fresh when the buffer's last parsed tree
was parsed from scratch and no edits have been applied
afterwards. An incrementally parsed tree that has incorporated
edits does not count as fresh (because incremental parsing has its flaws)."
  (not (or (phpinspect-buffer-needs-parse-p buffer)
           ;; If the buffer map has recycled tokens, the last parsed tree has
           ;; incorporated edits and is not fresh.
           (phpinspect-bmap-recycled-p (phpinspect-buffer-map buffer)))))

(defun phpinspect-buffer-reparse-if-not-fresh (buffer)
  "If BUFFER's tree is fresh, return it. Otherwise reparse the
 buffer and return the result."
  (phpi-shadow-await-synced (phpinspect-buffer-shadow buffer))
  (if (phpinspect-buffer-fresh-p buffer)
      (phpinspect-buffer-tree buffer)
    (phpinspect-buffer-reparse buffer)))

(cl-defmethod phpinspect-buffer-parse ((buffer phpinspect-buffer) &optional no-interrupt)
  "Parse the PHP code in the the emacs buffer that this object is
linked with."
  (phpi-shadow-await-synced (phpinspect-buffer-shadow buffer) (not no-interrupt))
  (unless (phpinspect-buffer-map buffer)
    (phpi-shadow-enqueue-task (phpinspect-buffer-shadow buffer) 'parse-fresh)
    (phpi-shadow-await-synced (phpinspect-buffer-shadow buffer) (not no-interrupt)))

  (phpinspect-buffer-tree buffer))

(cl-defmethod phpinspect-buffer-get-index-for-token ((buffer phpinspect-buffer) token)
  (gethash token (phpinspect-buffer-token-index buffer)))

(cl-defmethod phpinspect-buffer-set-index-reference-for-token ((buffer phpinspect-buffer) token index)
  (phpinspect--log "Setting index reference for token %s" token)
  (unless (phpinspect-probably-token-p token)
    (error "%s does not seem to be a token" token))
  (puthash token index (phpinspect-buffer-token-index buffer)))

(cl-defmethod phpinspect-buffer-update-index-reference-for-token ((buffer phpinspect-buffer) old new)
  (phpinspect--log "Updating index reference for old  token %s to new token %s" old new)
  (unless (and (phpinspect-probably-token-p old) (phpinspect-probably-token-p new))
    (when (and old new)
      (error "old and new parameters should be tokens")))

  (when-let ((index (gethash old (phpinspect-buffer-token-index buffer))))
    (remhash old (phpinspect-buffer-token-index buffer))
    (puthash new index (phpinspect-buffer-token-index buffer))))

(defun phpinspect-buffer-get-index-reference-for-token (buffer token)
  (gethash token (phpinspect-buffer-token-index buffer)))

(define-inline phpinspect--can-delete-buffer-index-for-token (token)
  (inline-quote
   (phpinspect-token-type-p
    ,token :class-declaration :function-declaration :const :class-variable :function)))

(cl-defmethod phpinspect-buffer-delete-index-for-token ((buffer phpinspect-buffer) token)
  "Delete index entities that were derived from TOKEN.

Index entities can be any object in the buffer's project cache:
classes, functions, variables, return types etc.

Normally, this function is used to delete index entities for
tokens that have been deleted from a buffer."
  (unless (phpinspect-probably-token-p token)
    (error "%s does not seem to be a token" token))

  (cond ((phpinspect-class-declaration-p token)
         (when-let ((typedef (gethash token (phpinspect-buffer-token-index buffer))))
           (if-let ((name (seq-find #'phpinspect-word-p token))
                    ;; See if class name survived the incremental parse
                    (name-token (gethash name (phpinspect-buffer--tokens buffer)))
                    (new-declaration (phpinspect-buffer-find-token-ancestor-matching
                                      buffer name-token #'phpinspect-class-declaration-p)))
               (progn
                 ;; Declaration has been replaced and class name is unchanged.
                 ;; Update existing typedef.
                 (if (equal (phpinspect-meta-token new-declaration)
                            token)
                     (progn
                       ;; tokens are equal, just update the index reference
                       (phpinspect-buffer-update-index-reference-for-token
                        buffer token (phpinspect-meta-token new-declaration)))

                   ;; Tokens are different, update class declaration
                   (phpinspect-buffer--index-class-declaration buffer new-declaration)
                   ;; Delete old index ref
                   (remhash token (phpinspect-buffer-token-index buffer))))
             (progn
               ;; Else: delete index ref AND associated typedef
               (remhash token (phpinspect-buffer-token-index buffer))
               (phpinspect-project-delete-typedef (phpinspect-buffer-project buffer) typedef)))))
        ((or (phpinspect-const-p token) (phpinspect-variable-p token))
         (when-let ((var (gethash token (phpinspect-buffer-token-index buffer))))
           (remhash token (phpinspect-buffer-token-index buffer))
           (when-let ((class (phpinspect-project-get-typedef
                              (phpinspect-buffer-project buffer)
                              (car var))))
             (if-let ((token-meta (phpinspect-buffer-token-meta buffer token)))
                 (phpi-typedef-delete-property-token-definition class (phpi-var-name (cdr var)) token-meta)
               (phpi-typedef-delete-property class (cdr var))))))
        ((or (phpinspect-this-p token) (phpinspect-attrib-p token))
         (phpinspect-buffer--delete-dynamic-prop-index-reference buffer token))
        ((phpinspect-function-p token)
         (phpinspect-buffer--delete-function-index-reference buffer token))
        (t (error "Cannot delete index for token %s" token))))

(defun phpinspect-buffer--delete-function-index-reference (buffer token)
  (when-let ((func (gethash token (phpinspect-buffer-token-index buffer))))
    (let ((arg-list (phpinspect-function-argument-list token)))
      (if-let ((arg-list-meta (phpinspect-buffer-token-meta buffer arg-list))
               ((eq (phpinspect-buffer-root-meta buffer)
                    (phpinspect-meta-find-root arg-list-meta)))
               ;; Arg-list has been adopted into current metadata tree, check
               ;; if declarations are equal.
               (new-declaration (phpinspect-meta-find-parent-matching-token
                                 arg-list-meta #'phpinspect-declaration-p))
               ((equal (phpinspect-meta-token new-declaration)
                       (phpinspect-function-declaration token)))
               ((thread-last (phpinspect-meta-parent new-declaration)
                             (phpinspect-meta-token)
                             (phpinspect-function-p))))
          (progn
            ;; Declaration is equal, update index reference
            (phpinspect-buffer-update-index-reference-for-token
             buffer token (phpinspect-meta-token (phpinspect-meta-parent new-declaration))))

        (progn
          ;; Declaration is not equal, delete index
          (remhash token (phpinspect-buffer-token-index buffer))
          (cond ((phpinspect-project-p (car func))
                 (phpinspect-project-delete-function (phpinspect-buffer-project buffer) (phpinspect--function-name (cdr func))))
                ((phpinspect--type-p (car func))
                 (when-let ((class (phpinspect-project-get-typedef
                                    (phpinspect-buffer-project buffer)
                                    (car func))))
                   (phpi-typedef-delete-method class (cdr func))))
                (t (phpinspect-message "Invalid index location, reindexing buffer")
                   (phpinspect-buffer-reindex buffer)
                   (error "invalid index location"))))))))

(defun phpinspect-buffer--delete-dynamic-prop-index-reference (buffer token)
  (when-let ((ref (phpinspect-buffer-get-index-reference-for-token buffer token))
             (typedef (phpinspect-project-get-typedef
                       (phpinspect-buffer-project buffer)
                       (car ref))))
    (phpi-typedef-delete-property-token-definition
     typedef (cadr ref) (phpinspect-meta-token (car (last ref))))))

(defun phpinspect-buffer-reset (buffer)
  "Clear all metadata stored in BUFFER."
  (interactive (list phpinspect-current-buffer))
  (phpinspect-buffer--clear-query-cache buffer)

  (phpi-shadow-kill (phpinspect-buffer-shadow buffer))
  (phpinspect-make-shadow buffer)

  (setf (phpinspect-buffer-tree buffer) nil
        (phpinspect-buffer--tokens buffer) nil
        (phpinspect-buffer-map buffer) nil

        ;; TODO: figure out what the desired behaviour is here
        ;; (phpinspect-buffer--additions buffer) nil
        ;; (phpinspect-buffer--deletions buffer) nil

        (phpinspect-buffer-token-index buffer)
        (make-hash-table :test 'eq :size 100 :rehash-size 1.5)))



(defun phpinspect-buffer-state (buffer)
  (interactive (list (or phpinspect-current-buffer
                         (error "Not a phpinspect buffer"))))

  (let ((shadow (phpinspect-buffer-shadow buffer)))
    (pop-to-buffer (generate-new-buffer "phpinspect-buffer-state"))
    (insert (format (concat "Buffer name: %s\nLast Shadow Error: %s\n"
                            "Shadow Thread Live: %s")
                    (phpinspect-with-current-buffer buffer (buffer-name))
                    (thread-last-error (phpi-shadow-thread shadow))
                    (phpi-shadow-thread-live-p shadow)))
    (read-only-mode)))

(defun phpinspect-buffer-reparse (buffer)
  "Discard BUFFER's current token tree and re-parse fully."
  (interactive (list (or phpinspect-current-buffer (error "Not a phpinspect buffer"))))
  (phpinspect-buffer-reset buffer)
  (phpi-shadow-enqueue-task (phpinspect-buffer-shadow buffer) 'parse-fresh)
  (phpi-shadow-await-synced (phpinspect-buffer-shadow buffer))
  (phpinspect-buffer-tree buffer))

(defun phpinspect-buffer-reindex (buffer)
  "Delete all existing index entities for tokens in BUFFER and re-index."
  (interactive (list (or phpinspect-current-buffer (error "Not a phpinspect buffer"))))
  (dolist (token (hash-table-keys (phpinspect-buffer-token-index buffer)))
    (phpinspect-buffer-delete-index-for-token buffer token))

  (phpinspect-buffer-reparse buffer)
  (phpinspect-buffer-update-project-index buffer))

(defun phpinspect-buffer-namespace-at-point (buffer point)
  (phpinspect-buffer--query-with-cache buffer `(namespace-at-point ,point)
    (seq-find #'phpinspect-namespace-p (phpinspect-buffer-tokens-enclosing-point buffer point))))

(defun phpinspect-buffer-find-token-children-matching (buffer token predicate)
  (phpinspect-buffer--query-with-cache buffer `(token-children ,(sxhash-eq token) ,(sxhash-eq predicate))
    (phpinspect-meta-find-children-matching-token token predicate)))

(defun phpinspect-buffer-find-token-ancestor-matching (buffer token predicate)
  (when token
    (phpinspect-buffer--query-with-cache buffer `(token-parent-matching ,(sxhash-eq token) ,(sxhash-eq predicate))
      (if (funcall predicate (phpinspect-meta-token token))
          token
        (phpinspect-buffer-find-token-ancestor-matching
         buffer (phpinspect-meta-parent token) predicate)))))

(defun phpinspect-buffer-get-index-context-for-token (buffer token)
  (let ((namespace (phpinspect-buffer-find-token-ancestor-matching
                    buffer token #'phpinspect-namespace-p))
        (imports (phpinspect-buffer-find-token-children-matching
                  buffer (phpinspect-buffer-root-meta buffer)
                  #'phpinspect-use-p))
        namespace-name)

    (phpinspect-buffer--query-with-cache buffer `(index-context ,(sxhash-eq namespace))
      (when namespace
        (setq namespace-name (thread-last
                               (phpinspect-meta-token namespace)
                               (phpinspect-namespace-name))
              imports (thread-last
                        (phpinspect-buffer-find-token-children-matching buffer namespace #'phpinspect-use-p)
                        (append imports))))

      (list (mapcar #'phpinspect-meta-token imports) namespace-name))))

(defun phpinspect-buffer-get-type-resolver-for-class (buffer class-token)
  (phpinspect-buffer--query-with-cache buffer `(type-resolver ,(sxhash-eq class-token))
    (pcase-let* ((`(,imports ,namespace-name)
                  (phpinspect-buffer-get-index-context-for-token buffer class-token)))
      (phpinspect--make-type-resolver
       imports
       (phpinspect-class-block (phpinspect-meta-token class-token))
       namespace-name))))

(defun phpinspect--buffer-update-type-declaration (buffer typedef declaration class-token imports namespace-name)
  (cl-assert (phpinspect-meta-p declaration))

  (phpi-typedef-update-declaration
   typedef (phpinspect-meta-token declaration) imports namespace-name
   (phpinspect-buffer--get-trait-configuration buffer class-token)
   (phpinspect-meta-token class-token)))

(defun phpinspect-buffer--get-trait-configuration (buffer class)
  (let ((uses (phpinspect-buffer-find-token-children-matching
               buffer (phpinspect-meta-last-child class) #'phpinspect-use-trait-p))
        (type-resolver (phpinspect-buffer-get-type-resolver-for-class buffer class))
        config)
    (dolist (use uses)
      (when-let ((part (thread-first (phpinspect-meta-token use)
                                     (phpinspect--index-trait-use type-resolver nil))))
        (setq config (nconc config part))))

    config))

(defun phpinspect-buffer--index-trait-use (buffer trait-use)
  (when-let ((class (phpinspect-buffer-find-token-ancestor-matching
                     buffer trait-use #'phpinspect-class-p)))
    ;; Only index trait use once per parse cycle
    (phpinspect-buffer--query-with-cache buffer `(trait-use ,(sxhash-eq class))
      (when-let ((declaration (phpinspect-meta-find-first-child-matching-token
                               class #'phpinspect-class-declaration-p)))
        (phpinspect-buffer--index-class-declaration buffer declaration 'force)))))

(defun phpinspect-buffer--index-class-declaration (buffer declaration &optional force)
  "Index DECLARATION in BUFFER.

DECLARATION must be an object of type `phpinspect-meta'."
  ;; Only index declaration when it hasn't been indexed before
  (unless (and (phpinspect-buffer-get-index-for-token buffer (phpinspect-meta-token declaration)) (not force))
    (let ((class (phpinspect-buffer-find-token-ancestor-matching
                  buffer declaration #'phpinspect-class-p))
          imports namespace-name class-name class-obj)
      (pcase-setq `(,imports ,namespace-name)
                  (phpinspect-buffer-get-index-context-for-token buffer declaration)
                  `(,class-name) (phpinspect--index-class-declaration
                                  (phpinspect-meta-token declaration)
                                  (phpinspect--make-type-resolver
                                   (phpinspect--uses-to-types imports)
                                   (phpinspect-class-block (phpinspect-meta-token class))
                                   namespace-name)
                                  (phpinspect-meta-token class)))
      (when class-name
        (setq  class-obj (phpinspect-project-get-typedef-create (phpinspect-buffer-project buffer) class-name 'no-enqueue))
        (phpinspect-buffer-set-index-reference-for-token buffer (phpinspect-meta-token declaration) class-obj)
        (phpinspect--buffer-update-type-declaration
         buffer class-obj declaration class imports namespace-name)

        ;; return typedef
        class-obj))))

(defun phpinspect-buffer-get-typedef-for-class-token (buffer class-token)
  (if-let ((declaration (phpinspect-meta-find-first-child-matching-token
                         class-token #'phpinspect-class-declaration-p)))
      (or (phpinspect-buffer-get-index-for-token buffer (phpinspect-meta-token declaration))
          (phpinspect-buffer--index-class-declaration buffer declaration))
    (error "Class token did not contain declaration")))

(defun phpinspect-buffer--index-method (buffer func class)
  (let ((parent (phpinspect-meta-parent func))
        comment-before static scope)
    (pcase (phpinspect-meta-token parent)
      ((pred phpinspect-static-p)
       (setq static (phpinspect-meta-parent func))
       (when (thread-last (phpinspect-meta-parent parent)
                          (phpinspect-meta-token)
                          (phpinspect-scope-p))
         ;; Create scope as expected by `phpinspect--index-function-from-scope'
         ;; (it does not handle "static" keywords).
         (setq scope `(,(car (phpinspect-meta-token (phpinspect-meta-parent static)))
                       ,(phpinspect-meta-token func))
               comment-before (phpinspect-meta-find-left-sibling (phpinspect-meta-parent static)))))
      ((pred phpinspect-scope-p)
       ;; Use scope that wraps 'func'
       (setq scope (phpinspect-meta-token parent)
             comment-before (phpinspect-meta-find-left-sibling parent))))

    ;; If func is not enclosed by a scope, it should default to "public"
    (setq scope (or scope `(:public ,(phpinspect-meta-token func)))

          ;; If no comment-before was found, make one last attempt to find it.
          comment-before (thread-last
                           (or comment-before (phpinspect-meta-find-left-sibling func))
                           (phpinspect-meta-token)))

    (unless (phpinspect-comment-p comment-before)
      (setq comment-before nil))

    (pcase-let* ((`(,imports ,namespace-name)
                  (phpinspect-buffer-get-index-context-for-token buffer class))
                 (type-resolver (phpinspect--make-type-resolver
                                 (phpinspect--uses-to-types imports)
                                 (phpinspect-meta-token class)
                                 namespace-name))
                 (typedef (phpinspect-buffer-get-typedef-for-class-token buffer class))
                 (indexed (phpinspect--index-function-from-scope
                           type-resolver scope comment-before)))

      (unless typedef (error "Unable to find typedef for class %s" (phpinspect-meta-string class)))

      ;; Add function to class
      (unless (phpinspect--function-anonymous-p indexed)
        (if static
            (phpi-typedef-set-static-method typedef indexed)
          (phpi-typedef-set-method typedef indexed)))

      (phpinspect-buffer-set-index-reference-for-token
       buffer (phpinspect-meta-token func)
       (cons (phpi-typedef-name typedef) indexed)))))

(defun phpinspect-buffer--index-function (buffer func)
  (if-let ((class (phpinspect-buffer-find-token-ancestor-matching buffer func #'phpinspect-class-p)))
      (phpinspect-buffer--index-method buffer func class)

    ;; Else: index normal function
    (pcase-let* ((`(,imports ,namespace-name) (phpinspect-buffer-get-index-context-for-token buffer func))
                 (comment-before (phpinspect-meta-find-left-sibling func))
                 (indexed (phpinspect--index-function-from-scope
                           (phpinspect--make-type-resolver
                            (phpinspect--uses-to-types imports) nil namespace-name)
                           `(:public ,(phpinspect-meta-token func))
                           (and (phpinspect-comment-p comment-before) comment-before)
                           nil namespace-name)))

      (phpinspect-project-set-function (phpinspect-buffer-project buffer) indexed)
      (phpinspect-buffer-set-index-reference-for-token
       buffer (phpinspect-meta-token func)
       (cons (phpinspect-buffer-project buffer) indexed)))))

(define-inline phpinspect-attrib-name (accessor)
  (inline-letevals (accessor)
    (inline-quote
     (progn
       (cl-assert (phpinspect-attrib-p ,accessor))
       (cadadr ,accessor)))))

(defun phpinspect-buffer--index-this (buffer this)
  "Extract metadata from usage of THIS ($this) in BUFFER."
  (when-let ((class (phpinspect-buffer-find-token-ancestor-matching
                     buffer this #'phpinspect-class-p))
             (accessor (phpinspect-meta-find-right-sibling this))
             ;; Right sibling is an accessor
             ((thread-last (phpinspect-meta-token accessor)
                           (phpinspect-attrib-p)))
             (accessor-name
              (thread-last (phpinspect-meta-token accessor)
                           (phpinspect-attrib-name)))
             (assignment (phpinspect-meta-find-right-sibling accessor))
             ;; Accessor is being assigned
             ((thread-last (phpinspect-meta-token assignment)
                           (phpinspect-assignment-p))))
    ;; Find end of assignment statement

    (let ((statement-end assignment))
      (while (and statement-end
                  (not (thread-last
                         (setq statement-end (phpinspect-meta-find-right-sibling statement-end))
                         (phpinspect-meta-token)
                         (phpinspect-statement-introduction-p)))))
      (when statement-end
        (when-let ((typedef (phpinspect-buffer-get-typedef-for-class-token buffer class))
                   (type (phpinspect-resolve-type-from-context
                     (phpinspect-buffer-get-resolvecontext
                      buffer (phpinspect-meta-start statement-end)))))

          (if-let ((prop (phpi-typedef-get-property typedef accessor-name)))
              (progn
                (setf (phpi-prop-type prop) type)
                (phpi-prop-add-definition-token prop accessor))

            ;; Property is dynamic
            (let ((prop (phpinspect-make-property
                         (phpi-typedef-name typedef)
                         (phpinspect--make-variable :name accessor-name
                                                    :type type
                                                    :scope '(:public)
                                                    :lifetime (when (phpinspect-static-attrib-p (phpinspect-meta-token accessor))
                                                                '(:static))))))
              (phpi-prop-add-definition-token prop accessor)
              (phpi-typedef-set-property typedef prop)))

          (let ((index-ref (list (phpi-typedef-name typedef) accessor-name this accessor)))
            (phpinspect-buffer-set-index-reference-for-token buffer (phpinspect-meta-token this) index-ref)
            (phpinspect-buffer-set-index-reference-for-token buffer (phpinspect-meta-token accessor) index-ref)))))))




(defun phpinspect-buffer--index-class-variable (buffer var)
  (let ((class (phpinspect-buffer-find-token-ancestor-matching buffer var #'phpinspect-class-p))
        scope static comment-before)
    (if (phpinspect-static-p (phpinspect-meta-token (phpinspect-meta-parent var)))
        ;; Variable is defined as [scope?] static [type?] $variable
        (progn
          (setq static (phpinspect-meta-parent var))
          (when (phpinspect-scope-p (phpinspect-meta-token (phpinspect-meta-parent static)))
            ;; Variable is defined as scope static [type?] $variable
            (setq scope `(,(car (phpinspect-meta-token (phpinspect-meta-parent static)))
                          ,@(phpinspect-meta-token-with-left-siblings var))
                  comment-before (phpinspect-meta-find-left-sibling (phpinspect-meta-parent static)))))
      (when (phpinspect-scope-p (phpinspect-meta-token (phpinspect-meta-parent var)))
        ;; Variable is defined as scope [type?] $variable
        (setq scope (phpinspect-meta-token (phpinspect-meta-parent var))
              comment-before (phpinspect-meta-find-left-sibling (phpinspect-meta-parent var)))))

    (unless scope
      (setq scope `(:public ,@(unless (phpinspect-const-p (phpinspect-meta-token var))
                                (mapcar #'phpinspect-meta-token (phpinspect-meta-left-siblings var)))
                            ,(phpinspect-meta-token var))))

    (unless comment-before
      (setq comment-before (phpinspect-meta-find-left-sibling var)))

    (setq comment-before (phpinspect-meta-token comment-before))

    (pcase-let* ((`(,imports ,namespace-name)
                  (phpinspect-buffer-get-index-context-for-token buffer var))
                 (type-resolver
                  (phpinspect--make-type-resolver
                   (phpinspect--uses-to-types imports)
                   (phpinspect-meta-token class)
                   namespace-name))
                 (typedef (phpinspect-buffer-get-typedef-for-class-token buffer class)))


      (when-let ((indexed
                  (phpinspect-make-property
                   (phpi-typedef-name typedef)
                   (if (phpinspect-const-p (phpinspect-meta-token var))
                       (phpinspect--index-const-from-scope scope)

                     (phpinspect--index-variable-from-scope
                      type-resolver
                      scope
                      (and (phpinspect-comment-p comment-before) comment-before)
                      static)))))

        (phpi-prop-add-definition-token indexed var)
        (phpi-typedef-set-property typedef indexed)

        (phpinspect-buffer-set-index-reference-for-token
         buffer (phpinspect-meta-token var)
         (cons (phpi-typedef-name typedef) indexed))))))

(cl-defmethod phpinspect-buffer-update-project-index ((buffer phpinspect-buffer))
  "Update project index using the last parsed token map of this
buffer. When `phpinspect-buffer-parse' has been executed before
and a map is available from the previous parse, this is used. If
none is available `phpinspect-buffer-parse' is called before
continuing execution."
  ;; Wait until project autoloader is ready, this is more efficient than waiting
  ;; for a shadow thread, which would be blocked while waiting for the
  ;; autoloader regardless.
  (phpinspect-project-await-autoload (phpinspect-buffer-project buffer))
  (phpi-shadow-await-index-synced (phpinspect-buffer-shadow buffer)))

(defun phpinspect-buffer-parse-map (buffer)
  (phpinspect-buffer-parse buffer)
  (phpinspect-buffer-map buffer))

(define-inline phpinspect--atom-regexp ()
  "A regular expression that matches (sequences of) atomic tokens."
  (inline-quote "\\(\\$\\|->\\|::\\)?[^][)(}{[:blank:]\n;'\"]+"))

(cl-defmethod phpinspect-buffer-register-edit
  ((buffer phpinspect-buffer) (start integer) (end integer) (pre-change-length integer))
  "Mark a region of the buffer as edited."

  ;; Take into account "atoms" (tokens without clear delimiters like words,
  ;; variables and object attributes. The meaning of these tokens will change as
  ;; they grow or shrink, so their full regions need to be marked for a reparse).
  (save-excursion
    (goto-char start)
    (when (looking-back (phpinspect--atom-regexp) nil t)
      (setq start (- start (length (match-string 0))))
      (setq pre-change-length (+ pre-change-length (length (match-string 0))))))

  (phpi-shadow-enqueue-task
   (phpinspect-buffer-shadow buffer)
   (phpi-change-create (phpinspect-buffer-buffer buffer)
                       start end pre-change-length)))

  ;; (phpinspect-edtrack-register-edit
  ;;  (phpinspect-buffer-edit-tracker buffer) start end pre-change-length))

(defun phpinspect-buffer-tokens-enclosing-point (buffer point)
  "Return token metadata objects for tokens enclosing POINT in BUFFER."
  (cl-assert (phpinspect-buffer-p buffer))
  (phpi-shadow-await-synced (phpinspect-buffer-shadow buffer))
  (phpinspect-buffer--query-with-cache buffer `(tokens-enclosing ,point)
    (phpinspect-bmap-tokens-overlapping (phpinspect-buffer-map buffer) point)))

(defun phpinspect-buffer-token-meta (buffer token)
  "Get metadata object for TOKEN.

TOKEN must be a token that was parsed in BUFFER."
  (gethash token (phpinspect-buffer--tokens buffer)))

(cl-defmethod phpinspect-buffer-location-resolver ((buffer phpinspect-buffer))
  "Derive location resolver from BUFFER's buffer map. Guarantees to
retrieve the lastest available map of BUFFER upon first
invocation, but subsequent invocations will not update the used
map afterwards, so don't keep the resolver around for long term
use."
  (lambda (token)
    (when-let ((meta (phpinspect-buffer-token-meta buffer token)))
      (phpinspect-make-region (phpinspect-meta-start meta)
                              (phpinspect-meta-end meta)))))

(defun phpinspect-buffer-root-meta (buffer)
  (phpi-shadow-await-synced (phpinspect-buffer-shadow buffer))
  (phpinspect-bmap-root-meta (phpinspect-buffer-map buffer)))

(defun phpinspect-display-buffer-tree ()
  (interactive)
  (when phpinspect-current-buffer
    (let ((buffer phpinspect-current-buffer))
      (pop-to-buffer (generate-new-buffer "phpinspect-buffer-tree"))
      (insert (pp-to-string (phpinspect-buffer-parse buffer 'no-interrupt)))
      (read-only-mode))))

(defun phpinspect-display-buffer-index ()
  (interactive)
  (when phpinspect-current-buffer
    (let ((buffer phpinspect-current-buffer))
      (pop-to-buffer (generate-new-buffer "phpinspect-buffer-index"))
      (insert (pp-to-string (phpinspect--index-tokens (phpinspect-buffer-parse buffer 'no-interrupt))))
      (read-only-mode))))

(defun phpinspect-after-change-function (start end pre-change-length)
  (when phpinspect-current-buffer
    (phpinspect-buffer-register-edit phpinspect-current-buffer start end pre-change-length)))

(define-inline phpinspect-with-current-buffer (buffer &rest body)
  (declare (indent 1))
  (inline-letevals (buffer)
    (push 'progn body)
    (inline-quote
     (with-current-buffer (phpinspect-buffer-buffer ,buffer)
       ,body))))

(defun phpinspect-buffer-get-resolvecontext (buffer point)
  (phpinspect-get-resolvecontext
   (phpinspect-buffer-project buffer) (phpinspect-buffer-parse-map buffer) point))

(defun phpinspect-buffer-kill ()
  (when phpinspect-current-buffer
    (phpi-shadow-kill (phpinspect-buffer-shadow phpinspect-current-buffer))))

(defun phpinspect-claim-buffer (buffer &optional project)
  "Setup an instance of `phpinspect-buffer' for BUFFER.

Like `phpinspect-make-buffer', but arguments are not a
plist. Sets up buffer-local variable `phpinspect-current-buffer'
and adds `phpinspect-after-change-function' to buffer-local
`after-change-functions'.

BUFFER must be a normal emacs buffer.
If provided, PROJECT must be an instance of `phpinspect-project'."
  (with-current-buffer buffer
    (let ((phpi-buffer
           (phpinspect-make-buffer :buffer buffer :-project project)))

      (phpinspect-make-shadow phpi-buffer)

      ;(message "Shadow: %s" (not (not (phpinspect-buffer-shadow phpi-buffer))))

      (setq-local phpinspect-current-buffer phpi-buffer)

      (add-hook 'after-change-functions #'phpinspect-after-change-function nil t)
      (add-hook 'kill-buffer-hook #'phpinspect-buffer-kill)

      phpinspect-current-buffer)))


;;;;;;;;;; SHADOWING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar phpinspect--shadow-counter 0)
(defvar phpinspect--shadow-run-sync nil)

(define-error 'phpinspect-wakeup-shadow
              "This error is used to wakeup the shadow thread.")

(cl-defstruct (phpinspect-shadow (:constructor phpinspect-make-shadow-generated)
                                 (:conc-name phpi-shadow-))
  (origin nil :type phpinspect-buffer)
  (buffer nil :type buffer)
  (queue nil :type phpinspect--queue)
  (thread nil :type thread)
  (id nil :type integer)
  (-last-change nil)
  (-synced-change nil)
  (-indexed-change nil)
  (-deletions nil :type list)
  (-additions (make-hash-table :test #'eq) :type hash-table))

(defun phpi--append-token-metadata-to-hash-table (table metadata)
  (dolist (meta metadata)
    (puthash (phpinspect-meta-token meta) meta table)))

(defun phpi-shadow--set-buffer-map (shadow bmap old-bmap)
  (let* ((buffer (phpi-shadow-origin shadow))
         (buffer-tokens (phpinspect-buffer--tokens buffer)))
    (setf (phpinspect-buffer-map buffer) bmap)

    (if buffer-tokens
        (let ((local-additions (with-memoization (phpi-shadow--additions shadow)
                                 (make-hash-table :test #'eq))))
            ;; Determine which tokens are new and which were already present in the
            ;; buffer
            (maphash
             (lambda (token meta) (unless (gethash token buffer-tokens)
                                    (puthash token meta buffer-tokens)
                                    (puthash token meta local-additions)))
             (phpinspect-bmap-meta bmap)))

      ;; There were no tokens registered, so we can adopt the map's token table
      (setf (phpinspect-buffer--tokens buffer) (phpinspect-bmap-meta bmap)
            ;; All tokens are new additions
            (phpi-shadow--additions shadow) (phpinspect-bmap-meta bmap)))

    (if-let ((old-bmap)
             (root-meta (phpinspect-bmap-root-meta old-bmap)))
        (progn
          (setf
           ;;Register deleted tokens
           (phpi-shadow--deletions shadow)
           (nconc (phpi-shadow--deletions shadow) (phpinspect-meta-flatten root-meta))))

      ;; There is no previous bmap, so there should also not be any previous additions
      (setf (phpi-shadow--additions shadow) (phpinspect-bmap-meta bmap)))

    ;; A new bmap was provided, so the structure of the token tree was
    ;; changed. All previous query results should be regarded as invalid.
    (phpinspect-buffer--clear-query-cache buffer)))

(defun phpi-shadow-wakeup-thread (shadow)
  (thread-signal (phpi-shadow-thread shadow) 'phpinspect-wakeup-shadow nil))

(defun phpi-shadow-make-queue-subscription (shadow)
  (lambda ()
    (phpi-shadow-wakeup-thread shadow)))

(defun phpi-shadow-process-change (shadow change)
  (with-current-buffer (phpi-shadow-buffer shadow)
    (phpi-change-apply change (current-buffer))

    (let* ((buffer (phpi-shadow-origin shadow))
           (pctx (phpinspect-make-pctx
                  :incremental t
                  :previous-bmap (phpinspect-buffer-map buffer)
                  :bmap (phpinspect-make-bmap)
                  :change change
                  :collaborative t)))

      (let (result)
        ;; Parse new content
        (with-current-buffer (phpi-shadow-buffer shadow)
          (phpinspect-with-parse-context pctx
            (setq result (phpinspect-parse-current-buffer))))

        (setf (phpinspect-buffer-tree buffer) result))

      (phpi-shadow--set-buffer-map
       shadow (phpinspect-pctx-bmap pctx) (phpinspect-pctx-previous-bmap pctx))

      (setf (phpi-shadow--synced-change shadow) change))))

(defun phpi-shadow-parse-fresh (shadow)
  (with-current-buffer (phpi-shadow-buffer shadow)
    (let* ((buffer (phpi-shadow-origin shadow))
           (pctx (phpinspect-make-pctx
                  :incremental t
                  :bmap (phpinspect-make-bmap)
                  :collaborative t)))

      (let (result)
        ;; Parse new content
        (with-current-buffer (phpi-shadow-buffer shadow)
          (phpinspect-with-parse-context pctx
            (setq result (phpinspect-parse-current-buffer))))

        (setf (phpinspect-buffer-tree buffer) result)

        (phpi-shadow--set-buffer-map
         shadow (phpinspect-pctx-bmap pctx) nil)

        (setf (phpi-shadow--synced-change shadow) 'parse-fresh)))))

(defun phpinspect-visit-shadow-buffer (buffer)
  (interactive (list (or phpinspect-current-buffer
                         (error "Not a phpinspect buffer"))))
  (pop-to-buffer (phpi-shadow-buffer (phpinspect-buffer-shadow buffer))))

(defun phpi-shadow-perform-task (shadow task)
  (pcase task
    ((pred phpinspect-change-p)
     (phpi-shadow-process-change shadow task))
    ('parse-fresh
     (phpi-shadow-parse-fresh shadow))
    ('update-project-index
     (phpi-shadow-update-project-index shadow))
    (_
     (phpinspect-message
      "Shadow thread received unsupported task type: %s"
      (type-of task)))))

(defun phpi-shadow-thread-live-p (shadow)
  (thread-live-p (phpi-shadow-thread shadow)))

(defun phpi-shadow-assert-live-p (shadow)
  (unless (phpi-shadow-thread-live-p shadow)
      (error "Shadow thread exited: %s"
             (thread-last-error (phpi-shadow-thread shadow)))))

(defun phpi-shadow-is-me-p (shadow)
  (eq (current-thread) (phpi-shadow-thread shadow)))

(defun phpi-shadow-await-predicate (shadow predicate allow-interrupt)
  (phpi-shadow-assert-live-p shadow)

  (unless (phpi-shadow-is-me-p shadow)

    (while (not (or (funcall predicate shadow) quit-flag))
      (when (and (phpinspect--input-pending-p t) allow-interrupt)
        (throw 'phpinspect-interrupted nil))

      (phpi-shadow-assert-live-p shadow)

      (thread-yield))))

(defun phpi-shadow-synced-p (shadow)
  (eq (phpi-shadow--synced-change shadow) (phpi-shadow--last-change shadow)))

(defun phpi-shadow-await-synced (shadow &optional allow-interrupt)
  (phpi-shadow-await-predicate shadow #'phpi-shadow-synced-p allow-interrupt))

(defun phpi-shadow-await-index-synced (shadow &optional allow-interrupt)
  (phpi-shadow-await-predicate shadow #'phpi-shadow-index-synced-p allow-interrupt))

(defun phpi--handle-use-trait-deletion (buffer deletion)
  (when-let ((class (seq-find (phpinspect-meta-token-predicate #'phpinspect-class-p)
                              (phpinspect-buffer-tokens-enclosing-point
                               buffer (phpinspect-meta-start deletion))))
             (declaration (phpinspect-meta-find-first-child-matching-token
                           class #'phpinspect-class-declaration-p)))
    (phpinspect-buffer--index-class-declaration buffer declaration 'force)))

(defun phpi-shadow-process-deletions (shadow)
  (let ((buffer (phpi-shadow-origin shadow))
        (additions (phpi-shadow--additions shadow)))
    ;; Process deleted tokens
    (dolist (deletion (phpi-shadow--deletions shadow))
      (phpi-progn
       (let ((token (phpinspect-meta-token deletion)))
         (if (gethash token additions)
             ;; Token was deleted before it could be indexed, remove and
             ;; continue
             (remhash token additions)

           (progn
             ;; Token was indexed but has been deleted, update index accordingly
             (pcase (phpinspect-meta-token deletion)
               ((pred phpinspect--can-delete-buffer-index-for-token)
                (phpinspect-buffer-delete-index-for-token
                 buffer (phpinspect-meta-token deletion)))

               ((pred phpinspect-use-trait-p)
                (phpi--handle-use-trait-deletion buffer deletion)))

             (remhash (phpinspect-meta-token deletion) (phpinspect-buffer--tokens buffer)))))))

    (setf (phpi-shadow--deletions shadow) nil)))

(defun phpi-shadow-process-additions (shadow)
  ;; Process newly parsed tokens
  (when-let ((additions (phpi-shadow--additions shadow))
             (buffer (phpi-shadow-origin shadow)))
    (maphash
     (lambda (_token addition)
       (phpi-progn
        (pcase (phpinspect-meta-token addition)
          ((pred phpinspect-class-declaration-p)
           (phpinspect-buffer--index-class-declaration buffer addition))
          ((pred phpinspect-function-p)
           (phpinspect-buffer--index-function buffer addition))
          ((pred phpinspect-use-trait-p)
           (phpinspect-buffer--index-trait-use buffer addition))
          ((pred phpinspect-this-p)
           (phpinspect-buffer--index-this buffer addition))
          ((or (pred phpinspect-class-variable-p)
               (pred phpinspect-const-p))
           (phpinspect-buffer--index-class-variable buffer addition)))))
     additions))

    (setf (phpi-shadow--additions shadow) nil))

(defun phpi-shadow-update-project-index (shadow)
  (let ((change (phpi-shadow--last-change shadow)))
    (when (phpinspect-buffer-project (phpi-shadow-origin shadow))
      (phpi-shadow-process-deletions shadow)
      (phpi-shadow-process-additions shadow)

      (setf (phpi-shadow--indexed-change shadow) change))))

(defun phpi-shadow--handle-job (shadow job)
  (phpi-progn
   (phpi-shadow-perform-task shadow job))

  (when (and (phpi-shadow-synced-p shadow)
             (not (phpi-shadow-index-synced-p shadow)))
    (phpi-shadow-enqueue-task shadow 'update-project-index)))

(defun phpi-shadow-index-synced-p (shadow)
  (and (phpi-shadow-synced-p shadow)
       (eq (phpi-shadow--synced-change shadow)
           (phpi-shadow--indexed-change shadow))))

(defun phpi-shadow-make-job-queue (shadow)
  ;; Make sure that the thread uses shadow buffer as its current buffer. This
  ;; prevents the user-edited buffer from becoming unkillable (buffers with
  ;; active threads cannot be killed).
  (with-current-buffer (phpi-shadow-buffer shadow)
    (phpi-start-job-queue  (format " **phpinspect-shadow-thread**<%d>" (phpi-shadow-id shadow))
      (lambda (job)
        (phpi-shadow--handle-job shadow job)))))

(defun phpinspect-make-shadow (origin)
  (cl-assert (phpinspect-buffer-p origin))

  (let* ((id (cl-incf phpinspect--shadow-counter))
         (shadow (phpinspect-make-shadow-generated
                  :origin origin
                  :buffer (generate-new-buffer
                           (format " **phpinspect-shadow**<%d>" id))
                  :id id)))

    (setf (phpinspect-buffer-shadow origin) shadow)

    ;; Copy buffer contents
    (with-current-buffer (phpi-shadow-buffer shadow)
      (insert (phpinspect-with-current-buffer origin (buffer-string))))

    (let ((job-queue (phpi-shadow-make-job-queue shadow)))

      (setf (phpi-shadow-queue shadow) job-queue
            (phpi-shadow-thread shadow) (phpi-job-queue-thread job-queue)))

    (phpi-shadow-enqueue-task shadow 'parse-fresh)

    shadow))

(defun phpi-shadow-enqueue-task (shadow task)
  (when (or (phpinspect-change-p task) (eq 'parse-fresh task))
    (setf (phpi-shadow--last-change shadow) task))

  (if phpinspect--shadow-run-sync
      (phpi-shadow--handle-job shadow task)
    (phpinspect-queue-enqueue (phpi-shadow-queue shadow) task)))

(provide 'phpinspect-buffer)
