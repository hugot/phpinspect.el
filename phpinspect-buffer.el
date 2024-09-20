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

(require 'phpinspect-parser)
(require 'phpinspect-bmap)
(require 'phpinspect-edtrack)
(require 'phpinspect-index)
(require 'phpinspect-toc)
(require 'phpinspect-resolvecontext)
(require 'phpinspect-resolve)
(require 'phpinspect-util)
(require 'phpinspect-typedef)
(require 'phpinspect-token-predicates)

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
  (tree nil
        :documentation
        "Parsed token tree that resulted from last parse")
  (map nil
       :type phpinspect-bmap)
  (-query-cache (make-hash-table :test 'equal :size 20 :rehash-size 2))
  (-last-indexed-bmap nil)
  (-deletions nil)
  (-additions nil)
  (-tokens nil)
  (token-index (make-hash-table :test 'eq :size 100 :rehash-size 1.5))
  (-project nil
            :type phpinspect-project)
  (edit-tracker (phpinspect-make-edtrack)
                :type phpinspect-edtrack))

(defmacro phpinspect-buffer--query-with-cache (buffer label &rest body)
  (declare (indent 2))
  `(with-memoization (gethash ,label (phpinspect-buffer--query-cache buffer))
     ,@body))

(defun phpinspect-buffer--clear-query-cache (buffer)
  (setf (phpinspect-buffer--query-cache buffer)
        (make-hash-table :test 'equal :size 20 :rehash-size 2)))

(defun phpinspect-buffer-project (buffer)
  (or (phpinspect-buffer--project buffer)
      (with-current-buffer (phpinspect-buffer-buffer buffer)
        (phpinspect--cache-get-project-create (phpinspect--get-or-create-global-cache)
                                              (phpinspect-current-project-root)))))

(defun phpinspect-buffer-tainted-p (buffer)
  "Whether or not BUFFER's current tree needs updating to incorporate edits."
  (and (phpinspect-edtrack-taint-pool (phpinspect-buffer-edit-tracker buffer)) t))

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
  (and (not (phpinspect-buffer-needs-parse-p buffer))
      ;; If the buffer map has overlays, the last parsed tree has incorporated
      ;; edits and is not fresh.
      (phpinspect-splayt-empty-p (phpinspect-bmap-overlays (phpinspect-buffer-map buffer)))))

(defun phpinspect-buffer-reparse-if-not-fresh (buffer)
  "If BUFFER's tree is fresh, return it. Otherwise reparse the
 buffer and return the result."
  (if (phpinspect-buffer-fresh-p buffer)
      (phpinspect-buffer-tree buffer)
    (phpinspect-buffer-reparse buffer)))

(defun phpinspect-buffer--set-map (buffer bmap old-bmap)
  (setf (phpinspect-buffer-map buffer) bmap)

  (let ((buffer-tokens (phpinspect-buffer--tokens buffer))
        additions)
    (if buffer-tokens
        ;; Determine which tokens are new and which were already present in the
        ;; buffer
        (maphash
         (lambda (token meta) (unless (gethash token buffer-tokens)
                                (puthash token meta buffer-tokens)
                                (push meta additions)))
         (phpinspect-bmap-meta bmap))
      ;; There were no tokens registered, so we can adopt the map's token table
      (setf (phpinspect-buffer--tokens buffer) (phpinspect-bmap-meta bmap))
      ;; All tokens are new additions
      (setq additions (hash-table-values (phpinspect-bmap-meta bmap))))

    (if-let ((old-bmap)
             (root-meta (phpinspect-bmap-root-meta old-bmap)))
        (progn
          ;; Register alterations for later processing/indexation
          (setf
           ;; Register new tokens
           (phpinspect-buffer--additions buffer)
           (nconc (phpinspect-buffer--additions buffer) additions)

           ;;Register deleted tokens
           (phpinspect-buffer--deletions buffer)
           (nconc (phpinspect-buffer--deletions buffer) (phpinspect-meta-flatten root-meta)))

          (dolist (deletion (phpinspect-buffer--deletions buffer))
            (remhash (phpinspect-meta-token deletion) buffer-tokens)))

      ;; There is no previous bmap, so there should also not be any previous additions
      (setf (phpinspect-buffer--additions buffer) additions))

    ;; A new bmap was provided, so the structure of the token tree was
    ;; changed. All previous query results should be regarded as invalid.
    (phpinspect-buffer--clear-query-cache buffer)))


(cl-defmethod phpinspect-buffer-parse ((buffer phpinspect-buffer) &optional no-interrupt)
  "Parse the PHP code in the the emacs buffer that this object is
linked with."
  (let (tree)
    (if (phpinspect-buffer-needs-parse-p buffer)
        (with-current-buffer (phpinspect-buffer-buffer buffer)
          (let* ((map (phpinspect-make-bmap))
                 (buffer-map (phpinspect-buffer-map buffer))
                 (ctx (phpinspect-make-pctx
                       :interrupt-predicate (unless no-interrupt #'phpinspect--input-pending-p)
                       :bmap map
                       :incremental t
                       :previous-bmap buffer-map
                       :edtrack (phpinspect-buffer-edit-tracker buffer))))
            (phpinspect-with-parse-context ctx
              (phpinspect--log "Parsing buffer")
              (let ((parsed (phpinspect-parse-current-buffer)))
                ;; Inhibit quitting to guarantee data integrity
                (let ((inhibit-quit t))
                  (setf (phpinspect-buffer-tree buffer) parsed)
                  (phpinspect-edtrack-clear (phpinspect-buffer-edit-tracker buffer))
                  (phpinspect-buffer--set-map buffer map buffer-map)

                  ;; set return value
                  (setq tree parsed))))))

      ;; Else: Just return last parse result
      (setq tree (phpinspect-buffer-tree buffer))
      tree)))

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

(define-inline phpinspect--can-delete-buffer-index-for-token (token)
  (inline-quote
   (phpinspect-token-type-p
    ,token :class-declaration :function-declaration :const :class-variable :function)))

(cl-defmethod phpinspect-buffer-delete-index-for-token ((buffer phpinspect-buffer) token)
  (unless (phpinspect-probably-token-p token)
    (error "%s does not seem to be a token" token))

  (phpinspect--log "Deleting index for token %s" token)
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
                 (phpinspect-buffer--index-class-declaration buffer new-declaration)
                 ;; Delete old index ref
                 (remhash token (phpinspect-buffer-token-index buffer)))
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
             (phpi-typedef-delete-property class (cdr var)))))
        ((phpinspect-function-p token)
         (when-let ((func (gethash token (phpinspect-buffer-token-index buffer))))
           (remhash token (phpinspect-buffer-token-index buffer))
           (cond ((phpinspect-project-p (car func))
                  (phpinspect-project-delete-function (phpinspect-buffer-project buffer) (phpinspect--function-name-symbol (cdr func))))
                 ((phpinspect--type-p (car func))
                  (when-let ((class (phpinspect-project-get-typedef
                                     (phpinspect-buffer-project buffer)
                                     (car func))))
                    (phpi-typedef-delete-method class (cdr func))))
                 (t (phpinspect-message "Invalid index location, reindexing buffer")
                    (phpinspect-buffer-reindex buffer)
                    (error "invalid index location")))))
        (t (error "Cannot delete index for token %s" token))))

;; (cl-defmethod phpinspect-buffer-namespace-at-point ((buffer phpinspect-buffer) (point integer))
;;   (let ((namespace (phpinspect-splayt-find-largest-before
;;                     (phpinspect-toc-tree (phpinspect-buffer-namespaces buffer))
;;                     point)))
;;     (and namespace (phpinspect-meta-overlaps-point namespace point) namespace)))

;; (defun phpinspect-buffer-get-type-resolver-for-class (buffer class-token)
;;   (pcase-let* ((`(,imports ,namespace-name)
;;                 (phpinspect-get-token-index-context
;;                  (phpinspect-buffer-namespaces buffer)
;;                  (phpinspect-buffer-imports buffer)
;;                  class-token)))
;;     (phpinspect--make-type-resolver
;;      imports
;;      (phpinspect-class-block (phpinspect-meta-token class-token))
;;      namespace-name)))



;; (cl-defmethod phpinspect-buffer-index-used-traits ((buffer phpinspect-buffer) (uses (head phpinspect-splayt)))
;;   (let ((update t))
;;     (if (phpinspect-buffer-used-traits buffer)
;;         (pcase-let ((`(,new ,deleted)
;;                      (phpinspect-toc-update
;;                       (phpinspect-buffer-used-traits buffer) uses)))
;;           (unless (or new deleted)
;;             ;; Nothing changed, don't update
;;             (setq update nil)))
;;       (setf (phpinspect-buffer-used-traits buffer) (phpinspect-make-toc uses)))

;;     (when update
;;       (let ((class-toc (phpinspect-buffer-classes buffer)))
;;         (phpinspect-splayt-traverse-lr (class (phpinspect-toc-tree class-toc))
;;           (let ((config (phpinspect-buffer-get-trait-configuration-between-points
;;                          buffer (phpinspect-meta-start class) (phpinspect-meta-end class)
;;                          (phpinspect-buffer-get-type-resolver-for-class buffer class))))

;;             (when-let ((typedef (phpinspect-buffer-get-index-for-token buffer (phpinspect-meta-token class))))
;;               (let ((new-extensions (seq-uniq (append
;;                                                (phpi-typedef-set-trait-config typedef config)
;;                                                (phpi-typedef-subscribed-to-types typedef))
;;                                               #'phpinspect--type=)))
;;                 (phpi-typedef-update-extensions typedef new-extensions)))))))))

;; (defun phpinspect-buffer-get-trait-configuration-between-points (buffer start end type-resolver)
;;   (let ((uses (phpinspect-toc-tokens-in-region (phpinspect-buffer-used-traits buffer) start end))
;;         config)
;;     (dolist (use uses)
;;       (setq config (nconc config (phpinspect--index-trait-use (phpinspect-meta-token use) type-resolver nil))))

;;     config))

;; (cl-defmethod phpinspect-buffer-index-imports ((buffer phpinspect-buffer) (imports (head phpinspect-splayt)))
;;   (let (to-be-indexed)
;;     (if (phpinspect-buffer-imports buffer)
;;         (pcase-let* ((`(,new) (phpinspect-toc-update
;;                                (phpinspect-buffer-imports buffer) imports)))
;;           (setq to-be-indexed new))
;;       (setq to-be-indexed (phpinspect-splayt-to-list imports))
;;       (setf (phpinspect-buffer-imports buffer) (phpinspect-make-toc imports)))

;;     (phpinspect-project-enqueue-imports
;;      (phpinspect-buffer-project buffer)
;;      (phpinspect--uses-to-types (mapcar #'phpinspect-meta-token to-be-indexed)))))

;; (cl-defmethod phpinspect-buffer-index-namespaces ((buffer phpinspect-buffer) (namespaces (head phpinspect-splayt)))
;;   (if (phpinspect-buffer-namespaces buffer)
;;       (phpinspect-toc-update (phpinspect-buffer-namespaces buffer) namespaces)
;;     (setf (phpinspect-buffer-namespaces buffer) (phpinspect-make-toc namespaces))))

;; (cl-defmethod phpinspect-buffer-index-declarations ((buffer phpinspect-buffer) (declarations (head phpinspect-splayt)))
;;   (if (phpinspect-buffer-declarations buffer)
;;       (phpinspect-toc-update (phpinspect-buffer-declarations buffer) declarations)
;;     (setf (phpinspect-buffer-declarations buffer) (phpinspect-make-toc declarations))))

;; (defun phpinspect-get-token-index-context (namespaces all-imports meta)
;;   (let ((namespace (phpinspect-toc-token-at-point namespaces (phpinspect-meta-start meta)))
;;         namespace-name imports)
;;     (if namespace
;;         (progn
;;           (setq namespace-name (phpinspect-namespace-name (phpinspect-meta-token namespace))
;;                 imports (mapcar #'phpinspect-meta-token
;;                                 (phpinspect-toc-tokens-in-region
;;                                  all-imports (phpinspect-meta-start namespace) (phpinspect-meta-start meta)))))
;;       (setq namespace-name nil
;;             imports (mapcar #'phpinspect-meta-token
;;                             (phpinspect-toc-tokens-in-region
;;                              all-imports 0 (phpinspect-meta-start meta)))))

;;     (list imports namespace-name)))

;; (defun phpinspect--buffer-update-type-declaration (buffer typedef declaration class-token imports namespace-name)
;;   (cl-assert (phpinspect-meta-p declaration))

;;   (phpi-typedef-update-declaration
;;    typedef (phpinspect-meta-token declaration) imports namespace-name
;;    (phpinspect-buffer-get-trait-configuration-between-points
;;     buffer (phpinspect-meta-start class-token) (phpinspect-meta-end class-token)
;;     (phpinspect--make-type-resolver
;;      imports (phpinspect-class-block (phpinspect-meta-token class-token)) namespace-name))
;;    (thread-last (phpinspect-meta-parent declaration)
;;                 (phpinspect-meta-token))))

;; (cl-defmethod phpinspect-buffer-index-classs ((buffer phpinspect-buffer) (classes (head phpinspect-splayt)))
;;   (let ((declarations (phpinspect-buffer-declarations buffer))
;;         (namespaces (phpinspect-buffer-namespaces buffer))
;;         (buffer-imports (phpinspect-buffer-imports buffer))
;;         (project (phpinspect-buffer-project buffer)))
;;     (if (phpinspect-buffer-classes buffer)
;;         (pcase-let* ((`(,new-classes ,deleted-classes) (phpinspect-toc-update
;;                                                         (phpinspect-buffer-classes buffer)
;;                                                         classes))
;;                      (new-declarations) (declaration) (replaced) (indexed) (class))
;;           ;; Collect declarations of new classes
;;           (dolist (class new-classes)
;;             (when (setq declaration (phpinspect-toc-token-at-or-after-point declarations (phpinspect-meta-start class)))
;;               (push (cons (phpinspect-meta-token declaration) class) new-declarations)))

;;           ;; Delete no longer existing classes from the index
;;           (dolist (deleted deleted-classes)
;;             (if (and (setq class (phpinspect-buffer-get-index-for-token
;;                                   buffer (phpinspect-meta-token deleted)))
;;                      ;; If we can find a declaration in the buffer that is equal
;;                      ;; to the deleted declaration, the class has just been
;;                      ;; edited in a way were the declaration ended up getting
;;                      ;; reparsed. No need to delete it from the index
;;                      (setq replaced (assoc (phpi-typedef-declaration class) new-declarations #'equal)))
;;                 (pcase-let ((`(,imports ,namespace-name) (phpinspect-get-token-index-context namespaces buffer-imports (cdr replaced))))
;;                   (phpinspect-buffer-update-index-reference-for-token
;;                    buffer (phpinspect-meta-token deleted) (phpinspect-meta-token (cdr replaced)))
;;                   (phpinspect--buffer-update-type-declaration
;;                    buffer class (car replaced) (cdr replaced) imports namespace-name)
;;                   (push (cdr replaced) indexed))
;;               (phpinspect-buffer-delete-index-for-token buffer (phpinspect-meta-token deleted))))

;;           (dolist (class new-declarations)
;;             (unless (memq (cdr class) indexed)
;;               (let (imports namespace-name class-name class-obj)
;;                 (pcase-setq `(,imports ,namespace-name)
;;                             (phpinspect-get-token-index-context namespaces buffer-imports (cdr class))
;;                             `(,class-name) (phpinspect--index-class-declaration
;;                                             (car class)
;;                                             (phpinspect--make-type-resolver
;;                                              (phpinspect--uses-to-types imports)
;;                                              (phpinspect-class-block (phpinspect-meta-token (cdr class)))
;;                                              namespace-name)
;;                                             (thread-last (phpinspect-meta-parent declaration)
;;                                                          (phpinspect-meta-token))))
;;                 (when class-name
;;                   (setq  class-obj (phpinspect-project-get-typedef-create project class-name 'no-enqueue))
;;                   (phpinspect-buffer-set-index-reference-for-token buffer (phpinspect-meta-token (cdr class)) class-obj)
;;                   (phpinspect--buffer-update-type-declaration
;;                    buffer class-obj (car class) (cdr class) imports namespace-name))))))
;;       ;; Else: Index all classes
;;       (setf (phpinspect-buffer-classes buffer) (phpinspect-make-toc classes))
;;       (phpinspect-splayt-traverse (class classes)
;;         (pcase-let* ((declaration (phpinspect-toc-token-at-or-after-point declarations (phpinspect-meta-start class)))
;;                      (`(,imports ,namespace-name) (phpinspect-get-token-index-context namespaces buffer-imports class))
;;                      (`(,class-name) (phpinspect--index-class-declaration
;;                                       (phpinspect-meta-token declaration)
;;                                       (phpinspect--make-type-resolver
;;                                        (phpinspect--uses-to-types imports)
;;                                        (phpinspect-class-block (phpinspect-meta-token class))
;;                                        namespace-name)
;;                                       (thread-last (phpinspect-meta-parent declaration)
;;                                                    (phpinspect-meta-token))))
;;                      (class-obj))
;;           (when class-name
;;             (setq class-obj (phpinspect-project-get-typedef-create project class-name 'no-enqueue))
;;             (phpinspect-buffer-set-index-reference-for-token buffer (phpinspect-meta-token class) class-obj)
;;             (phpinspect--buffer-update-type-declaration
;;              buffer class-obj declaration class imports namespace-name)))))))

;; (cl-defmethod phpinspect-buffer-index-functions ((buffer phpinspect-buffer) (functions (head phpinspect-splayt)))
;;   (let ((classes (phpinspect-buffer-classes buffer))
;;         (namespaces (phpinspect-buffer-namespaces buffer))
;;         (imports (phpinspect-buffer-imports buffer))
;;         to-be-indexed class-environments class indexed)
;;     (if (phpinspect-buffer-functions buffer)
;;         (pcase-let ((`(,new-funcs ,deleted-funcs)
;;                      (phpinspect-toc-update (phpinspect-buffer-functions buffer) functions)))
;;           (setq to-be-indexed new-funcs)

;;           (dolist (deleted deleted-funcs)
;;             (phpinspect-buffer-delete-index-for-token buffer (phpinspect-meta-token deleted))))
;;       (setq to-be-indexed (phpinspect-splayt-to-list functions))
;;       (setf (phpinspect-buffer-functions buffer) (phpinspect-make-toc functions)))

;;     (dolist (func to-be-indexed)
;;       (if (setq class (phpinspect-toc-token-at-point classes (phpinspect-meta-start func)))
;;           (let (scope static indexed index-env comment-before)
;;             (if (phpinspect-static-p (phpinspect-meta-token (phpinspect-meta-parent func)))
;;                 (progn
;;                   (setq static (phpinspect-meta-parent func))
;;                   (when (phpinspect-scope-p (phpinspect-meta-token (phpinspect-meta-parent static)))
;;                     (setq scope `(,(car (phpinspect-meta-token (phpinspect-meta-parent static)))
;;                                   ,(phpinspect-meta-token func))
;;                           comment-before (phpinspect-meta-find-left-sibling (phpinspect-meta-parent static)))))
;;               (when (phpinspect-scope-p (phpinspect-meta-token (phpinspect-meta-parent func)))
;;                 (setq scope (phpinspect-meta-token (phpinspect-meta-parent func))
;;                       comment-before (phpinspect-meta-find-left-sibling (phpinspect-meta-parent func)))))

;;             (unless scope (setq scope `(:public ,(phpinspect-meta-token func))))

;;             (unless (setq index-env (alist-get class class-environments nil nil #'eq))
;;               (setq index-env (phpinspect-get-token-index-context namespaces imports class))
;;               (setcar index-env (phpinspect--uses-to-types (car index-env)))
;;               (push (phpinspect--make-type-resolver (car index-env) (phpinspect-meta-token class) (cadr index-env))
;;                     index-env)
;;               (push (cons class index-env) class-environments))

;;             (unless comment-before
;;               (setq comment-before (phpinspect-meta-find-left-sibling func)))

;;             (setq comment-before (phpinspect-meta-token comment-before))

;;             (pcase-let ((`(,type-resolver) index-env)
;;                         (class-obj (phpinspect-buffer-get-index-for-token buffer (phpinspect-meta-token class))))
;;               (unless class-obj (error "Unable to find class obj for class %s" (phpinspect-meta-token class)))

;;               (setq indexed (phpinspect--index-function-from-scope
;;                              type-resolver
;;                              scope
;;                              (and (phpinspect-comment-p comment-before) comment-before)))
;;               (unless (phpinspect--function-anonymous-p indexed)
;;                 (if static
;;                     (phpi-typedef-set-static-method class-obj indexed)
;;                   (phpi-typedef-set-method class-obj indexed)))

;;               (phpinspect-buffer-set-index-reference-for-token
;;                buffer (phpinspect-meta-token func)
;;                (cons (phpi-typedef-name class-obj) indexed))))
;;         ;; Else: index function
;;         (pcase-let ((`(,imports ,namespace-name) (phpinspect-get-token-index-context namespaces imports func))
;;                     (comment-before (phpinspect-meta-find-left-sibling func)))
;;           (setq indexed (phpinspect--index-function-from-scope
;;                          (phpinspect--make-type-resolver
;;                           (phpinspect--uses-to-types imports) nil namespace-name)
;;                          `(:public ,(phpinspect-meta-token func))
;;                          (and (phpinspect-comment-p comment-before) comment-before)
;;                          nil namespace-name))
;;           (phpinspect-project-set-function (phpinspect-buffer-project buffer) indexed)
;;           (phpinspect-buffer-set-index-reference-for-token
;;            buffer (phpinspect-meta-token func)
;;            (cons (phpinspect-buffer-project buffer) indexed)))))))


;; (cl-defmethod phpinspect-buffer-index-class-variables ((buffer phpinspect-buffer) (class-variables (head phpinspect-splayt)))
;;   (let ((classes (phpinspect-buffer-classes buffer))
;;         (namespaces (phpinspect-buffer-namespaces buffer))
;;         (imports (phpinspect-buffer-imports buffer))
;;         to-be-indexed class-environments class class-obj)
;;     (if (phpinspect-buffer-class-variables buffer)
;;         (pcase-let ((`(,new-vars ,deleted-vars)
;;                      (phpinspect-toc-update
;;                       (phpinspect-buffer-class-variables buffer) class-variables)))
;;           (setq to-be-indexed new-vars)

;;           (dolist (deleted deleted-vars)
;;             (phpinspect-buffer-delete-index-for-token buffer (phpinspect-meta-token deleted))))

;;       (setq to-be-indexed (phpinspect-splayt-to-list class-variables))
;;       (setf (phpinspect-buffer-class-variables buffer) (phpinspect-make-toc class-variables)))

;;     (dolist (var to-be-indexed)
;;       ;; We have left the class we were previously indexing properties
;;       ;; for.
;;       (when (and class (> (phpinspect-meta-start var) (phpinspect-meta-end class)))
;;         (setq class nil))

;;       ;; Retrieve the class that the property belongs to. In a lot of cases only
;;       ;; one class will be in the current buffer. But PHP allows the definition
;;       ;; of multiple classes in the same file so this should be supported.
;;       (unless class
;;         (setq class (phpinspect-toc-token-at-point classes (phpinspect-meta-start var))))

;;       ;; Fetch the index for the current class
;;       (setq class-obj (phpinspect-buffer-get-index-for-token buffer (phpinspect-meta-token class)))

;;       (let (scope static indexed index-env comment-before)
;;         (if (phpinspect-static-p (phpinspect-meta-token (phpinspect-meta-parent var)))
;;             ;; Variable is defined as [scope?] static [type?] $variable
;;             (progn
;;               (setq static (phpinspect-meta-parent var))
;;               (when (phpinspect-scope-p (phpinspect-meta-token (phpinspect-meta-parent static)))
;;                 ;; Variable is defined as scope static [type?] $variable
;;                 (setq scope `(,(car (phpinspect-meta-token (phpinspect-meta-parent static)))
;;                               ,@(phpinspect-meta-token-with-left-siblings var))
;;                       comment-before (phpinspect-meta-find-left-sibling (phpinspect-meta-parent static)))))
;;           (when (phpinspect-scope-p (phpinspect-meta-token (phpinspect-meta-parent var)))
;;             ;; Variable is defined as scope [type?] $variable
;;             (setq scope (phpinspect-meta-token (phpinspect-meta-parent var))
;;                   comment-before (phpinspect-meta-find-left-sibling (phpinspect-meta-parent var)))))

;;         (unless scope
;;           (setq scope `(:public ,@(mapcar #'phpinspect-meta-token (phpinspect-meta-left-siblings var))
;;                                 ,(phpinspect-meta-token var))))

;;         (unless (setq index-env (alist-get class class-environments nil nil #'eq))
;;           (setq index-env (phpinspect-get-token-index-context namespaces imports class))
;;           (push (cons class index-env) class-environments))

;;         (unless comment-before
;;           (setq comment-before (phpinspect-meta-find-left-sibling var)))

;;         (setq comment-before (phpinspect-meta-token comment-before))

;;         (pcase-let* ((`(,imports ,namespace-name) index-env)
;;                      (type-resolver
;;                       (phpinspect--make-type-resolver
;;                        (phpinspect--uses-to-types imports)
;;                        (phpinspect-meta-token class)
;;                        namespace-name)))

;;           (setq indexed
;;                 (if (phpinspect-const-p (phpinspect-meta-token var))
;;                     (phpinspect--index-const-from-scope scope)
;;                   (phpinspect--index-variable-from-scope
;;                    type-resolver
;;                    scope
;;                    (and (phpinspect-comment-p comment-before) comment-before)
;;                    static)))

;;           (when (and (phpinspect-variable-p (phpinspect-meta-token var)) (not (phpinspect--variable-type indexed)))
;;             (setf (phpinspect--variable-type indexed)
;;                   (phpi-typedef-resolve-property-type
;;                    class-obj (phpinspect-buffer-project buffer)
;;                    (cadr (phpinspect-meta-token var)) type-resolver class)))

;;           (phpi-typedef-set-property class-obj indexed)

;;           (phpinspect-buffer-set-index-reference-for-token
;;            buffer (phpinspect-meta-token var)
;;            (cons (phpi-typedef-name class-obj) indexed)))))))

(cl-defmethod phpinspect-buffer-reset ((buffer phpinspect-buffer))
  "Clear all metadata stored in BUFFER."
  (phpinspect-buffer--clear-query-cache buffer)

  (setf (phpinspect-buffer-tree buffer) nil
        (phpinspect-buffer--tokens buffer) nil
        (phpinspect-buffer-map buffer) (phpinspect-make-bmap)
        (phpinspect-buffer--additions buffer) nil
        (phpinspect-buffer--deletions buffer) nil

        (phpinspect-buffer-token-index buffer)
        (make-hash-table :test 'eq :size 100 :rehash-size 1.5))

  (phpinspect-edtrack-clear (phpinspect-buffer-edit-tracker buffer)))

(cl-defmethod phpinspect-buffer-reparse ((buffer phpinspect-buffer))
  (phpinspect-buffer-reset buffer)
  (phpinspect-buffer-parse buffer 'no-interrupt))

(cl-defmethod phpinspect-buffer-reindex ((buffer phpinspect-buffer))
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
  (let ((class (phpinspect-buffer-find-token-ancestor-matching
                buffer token #'phpinspect-class-p))
        (namespace (phpinspect-buffer-find-token-ancestor-matching
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
      (setq config (nconc config
                          (thread-first (phpinspect-meta-token use)
                                        (phpinspect--index-trait-use type-resolver nil)))))
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

DECLARATION must be an object of type 'phpinspect-meta'."
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

(defun phpinspect-buffer--index-class-variable (buffer var)
  (let ((class (phpinspect-buffer-find-token-ancestor-matching buffer var #'phpinspect-class-p))
        scope static indexed index-env comment-before typedef)
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

    (setq index-env (phpinspect-buffer-get-index-context-for-token buffer var))

    (unless comment-before
      (setq comment-before (phpinspect-meta-find-left-sibling var)))

    (setq comment-before (phpinspect-meta-token comment-before))

    (pcase-let* ((`(,imports ,namespace-name) index-env)
                 (type-resolver
                  (phpinspect--make-type-resolver
                   (phpinspect--uses-to-types imports)
                   (phpinspect-meta-token class)
                   namespace-name))
                 (typedef (phpinspect-buffer-get-typedef-for-class-token buffer class)))

      (setq indexed
            (if (phpinspect-const-p (phpinspect-meta-token var))
                (phpinspect--index-const-from-scope scope)
              (phpinspect--index-variable-from-scope
               type-resolver
               scope
               (and (phpinspect-comment-p comment-before) comment-before)
               static)))

      (when (and (phpinspect-variable-p (phpinspect-meta-token var)) (not (phpinspect--variable-type indexed)))
        (setf (phpinspect--variable-type indexed)
              (phpi-typedef-resolve-property-type
               typedef (phpinspect-buffer-project buffer)
               (cadr (phpinspect-meta-token var)) type-resolver class)))

      (phpi-typedef-set-property typedef indexed)

      (phpinspect-buffer-set-index-reference-for-token
       buffer (phpinspect-meta-token var)
      (cons (phpi-typedef-name typedef) indexed)))))

(cl-defmethod phpinspect-buffer-update-project-index ((buffer phpinspect-buffer))
  "Update project index using the last parsed token map of this
buffer. When `phpinspect-buffer-parse' has been executed before
and a map is available from the previous parse, this is used. If
none is available `phpinspect-buffer-parse' is called before
continuing execution."

  ;; Parse buffer if no map is available.
  (unless (phpinspect-buffer-map buffer)
    (phpinspect-buffer-parse buffer))

  (phpinspect--log "Preparing to update project index")
  ;; Use inhibit-quit to prevent index corruption though partial index
  ;; application.
  (let ((inhibit-quit t))
    (when (phpinspect-buffer-project buffer)
      (let ((map (phpinspect-buffer-map buffer)))
        (unless (eq map (phpinspect-buffer--last-indexed-bmap buffer))
          (phpinspect--log "Updating project index")

          (dolist (deletion (phpinspect-buffer--deletions buffer))
            (pcase (phpinspect-meta-token deletion)
              ((pred phpinspect--can-delete-buffer-index-for-token)
               (phpinspect-buffer-delete-index-for-token buffer (phpinspect-meta-token deletion)))
              ((pred phpinspect-use-trait-p)
               (when-let ((class (seq-find (phpinspect-meta-token-predicate #'phpinspect-class-p)
                                           (phpinspect-buffer-tokens-enclosing-point
                                            buffer (phpinspect-meta-start deletion))))
                          (declaration (phpinspect-meta-find-first-child-matching-token
                                        class #'phpinspect-class-declaration-p)))
                 (phpinspect-buffer--index-class-declaration buffer declaration 'force)))))

          (dolist (addition (phpinspect-buffer--additions buffer))
            (pcase (phpinspect-meta-token addition)
              ((pred phpinspect-class-declaration-p)
               (phpinspect-buffer--index-class-declaration buffer addition))
              ((pred phpinspect-function-p)
               (phpinspect-buffer--index-function buffer addition))
              ((pred phpinspect-use-trait-p)
               (phpinspect-buffer--index-trait-use buffer addition))
              ((or (pred phpinspect-class-variable-p)
                   (pred phpinspect-const-p))
               (phpinspect-buffer--index-class-variable buffer addition))))

          (setf (phpinspect-buffer--additions buffer) nil
                (phpinspect-buffer--deletions buffer) nil))

          (setf (phpinspect-buffer--last-indexed-bmap buffer) map)))))

(defsubst phpinspect-buffer-parse-map (buffer)
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

  (phpinspect-edtrack-register-edit
   (phpinspect-buffer-edit-tracker buffer) start end pre-change-length))

(defun phpinspect-buffer-tokens-enclosing-point (buffer point)
  "Return token metadata objects for tokens enclosing POINT in BUFFER."
  (cl-assert (phpinspect-buffer-p buffer))
  (phpinspect-buffer--query-with-cache buffer `(tokens-enclosing ,point)
    (phpinspect-bmap-tokens-overlapping (phpinspect-buffer-map buffer) point)))

(cl-defmethod phpinspect-buffer-token-meta ((buffer phpinspect-buffer) token)
  (phpinspect-bmap-token-meta (phpinspect-buffer-map buffer) token))

(cl-defmethod phpinspect-buffer-location-resolver ((buffer phpinspect-buffer))
  "Derive location resolver from BUFFER's buffer map. Guarantees to
retrieve the lastest available map of BUFFER upon first
invocation, but subsequent invocations will not update the used
map afterwards, so don't keep the resolver around for long term
use."
  (let ((bmap-resolver))
    (lambda (token)
      (funcall (with-memoization bmap-resolver
                 (phpinspect-bmap-make-location-resolver (phpinspect-buffer-map buffer)))
               token))))

(defun phpinspect-buffer-root-meta (buffer)
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


(defun phpinspect-claim-buffer (buffer &optional project)
  "Setup an instance of `phpinspect-buffer' for BUFFER.

Like `phpinspect-make-buffer', but arguments are not a
plist. Sets up buffer-local variable `phpinspect-current-buffer'
and adds `phpinspect-after-change-function' to buffer-local
`after-change-functions'.

BUFFER must be a normal emacs buffer.
If provided, PROJECT must be an instance of `phpinspect-project'."
  (with-current-buffer buffer
    (setq-local phpinspect-current-buffer
		(phpinspect-make-buffer :buffer buffer :-project project))
    (add-hook 'after-change-functions #'phpinspect-after-change-function nil t)

    phpinspect-current-buffer))

(provide 'phpinspect-buffer)
