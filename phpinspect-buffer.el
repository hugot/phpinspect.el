;;; phpinspect-buffer.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(require 'phpinspect-parser)
(require 'phpinspect-bmap)
(require 'phpinspect-edtrack)
(require 'phpinspect-index)
(require 'phpinspect-toc)
(require 'phpinspect-resolvecontext)
(require 'phpinspect-resolve)
(require 'phpinspect-util)

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
  (-last-indexed-bmap nil)
  (imports nil
           :type phpinspect-toc)
  (namespaces nil
              :type phpinspect-toc)
  (classes nil
           :type phpinspect-toc)
  (class-variables nil
                   :type phpinspect-toc)
  (declarations nil
                :type phpinspect-toc)
  (functions nil
             :type phpinspect-toc)
  (token-index (make-hash-table :test 'eq :size 100 :rehash-size 1.5))
  (project nil
           :type phpinspect-project)
  (edit-tracker (phpinspect-make-edtrack)
                :type phpinspect-edtrack))

(cl-defmethod phpinspect-buffer-parse ((buffer phpinspect-buffer) &optional no-interrupt)
  "Parse the PHP code in the the emacs buffer that this object is
linked with."
  (let ((tree))
  (if (or (not (phpinspect-buffer-tree buffer))
          (phpinspect-edtrack-taint-pool (phpinspect-buffer-edit-tracker buffer)))
      (with-current-buffer (phpinspect-buffer-buffer buffer)
        (let* ((map (phpinspect-make-bmap))
               (buffer-map (phpinspect-buffer-map buffer))
               (ctx (phpinspect-make-pctx
                     :interrupt-predicate (unless no-interrupt #'input-pending-p)
                     :bmap map
                     :incremental t
                     :previous-bmap buffer-map
                     :edtrack (phpinspect-buffer-edit-tracker buffer))))
          (phpinspect-with-parse-context ctx
            (phpinspect--log "Parsing buffer")
            (let ((parsed (phpinspect-parse-current-buffer)))
              (setf (phpinspect-buffer-map buffer) map)
              (setf (phpinspect-buffer-tree buffer) parsed)
              (phpinspect-edtrack-clear (phpinspect-buffer-edit-tracker buffer))

              ;; set return value
              (setq tree parsed)))))
    ;; Else: Just return last parse result
    (setq tree (phpinspect-buffer-tree buffer)))
  tree))



(cl-defmethod phpinspect-buffer-get-index-for-token ((buffer phpinspect-buffer) token)
  (gethash token (phpinspect-buffer-token-index buffer)))

(cl-defmethod phpinspect-buffer-set-index-reference-for-token ((buffer phpinspect-buffer) token index)
  (unless (phpinspect-probably-token-p token)
    (error "%s does not seem to be a token" token))
  (puthash token index (phpinspect-buffer-token-index buffer)))

(cl-defmethod phpinspect-buffer-update-index-reference-for-token ((buffer phpinspect-buffer) old new)
  (unless (and (phpinspect-probably-token-p old) (phpinspect-probably-token-p new))
    (when (and old new)
      (error "old and new parameters should be tokens")))

  (when-let ((index (gethash old (phpinspect-buffer-token-index buffer))))
    (remhash old (phpinspect-buffer-token-index buffer))
    (puthash new index (phpinspect-buffer-token-index buffer))))

(cl-defmethod phpinspect-buffer-delete-index-for-token ((buffer phpinspect-buffer) token)
  (unless (phpinspect-probably-token-p token)
    (error "%s does not seem to be a token" token))

  (cond ((phpinspect-class-p token)
         (when-let ((class (gethash token (phpinspect-buffer-token-index buffer))))
           (remhash token (phpinspect-buffer-token-index buffer))
           (phpinspect-project-delete-class (phpinspect-buffer-project buffer) class)))
        ((or (phpinspect-const-p token) (phpinspect-variable-p token))
         (when-let ((var (gethash token (phpinspect-buffer-token-index buffer))))
           (remhash token (phpinspect-buffer-token-index buffer))
           (when-let ((class (phpinspect-project-get-class
                              (phpinspect-buffer-project buffer)
                              (car var))))
             (phpinspect--class-delete-variable class (cdr var)))))
        ((phpinspect-function-p token)
         (when-let ((func (gethash token (phpinspect-buffer-token-index buffer))))
           (remhash token (phpinspect-buffer-token-index buffer))
           (cond ((phpinspect-project-p (car func))
                  (phpinspect-project-delete-function (phpinspect-buffer-project buffer) (phpinspect--function-name-symbol (cdr func))))
                 ((phpinspect--type-p (car func))
                  (when-let ((class (phpinspect-project-get-class
                                     (phpinspect-buffer-project buffer)
                                     (car func))))
                    (phpinspect--class-delete-method class (cdr func))))
                 (t (error "Invalid index location")))))
        (t (error "Cannot delete index for token %s" token))))

(cl-defmethod phpinspect-buffer-namespace-at-point ((buffer phpinspect-buffer) (point integer))
  (let ((namespace (phpinspect-splayt-find-largest-before
                    (phpinspect-toc-tree (phpinspect-buffer-namespaces buffer))
                    point)))
    (and namespace (phpinspect-meta-overlaps-point namespace point) namespace)))

(cl-defmethod phpinspect-buffer-index-imports ((buffer phpinspect-buffer) (imports (head phpinspect-splayt)))
  (let (to-be-indexed)
    (if (phpinspect-buffer-imports buffer)
        (pcase-let* ((`(,new) (phpinspect-toc-update
                               (phpinspect-buffer-imports buffer) imports (phpinspect-buffer-root-meta buffer))))
          (setq to-be-indexed new))
      (setq to-be-indexed (phpinspect-splayt-to-list imports))
      (setf (phpinspect-buffer-imports buffer) (phpinspect-make-toc imports)))

    (phpinspect-project-enqueue-imports
     (phpinspect-buffer-project buffer)
     (phpinspect--uses-to-types (mapcar #'phpinspect-meta-token to-be-indexed)))))

(cl-defmethod phpinspect-buffer-index-namespaces ((buffer phpinspect-buffer) (namespaces (head phpinspect-splayt)))
  (if (phpinspect-buffer-namespaces buffer)
      (phpinspect-toc-update (phpinspect-buffer-namespaces buffer) namespaces (phpinspect-buffer-root-meta buffer))
    (setf (phpinspect-buffer-namespaces buffer) (phpinspect-make-toc namespaces))))

(cl-defmethod phpinspect-buffer-index-declarations ((buffer phpinspect-buffer) (declarations (head phpinspect-splayt)))
  (if (phpinspect-buffer-declarations buffer)
      (phpinspect-toc-update (phpinspect-buffer-declarations buffer) declarations (phpinspect-buffer-root-meta buffer))
    (setf (phpinspect-buffer-declarations buffer) (phpinspect-make-toc declarations))))

(defun phpinspect-get-token-index-context (namespaces all-imports meta)
  (let ((namespace (phpinspect-toc-token-at-point namespaces (phpinspect-meta-start meta)))
        namespace-name imports)
    (if namespace
        (progn
          (setq namespace-name (phpinspect-namespace-name (phpinspect-meta-token namespace))
                imports (mapcar #'phpinspect-meta-token
                                (phpinspect-toc-tokens-in-region
                                 all-imports (phpinspect-meta-start namespace) (phpinspect-meta-start meta)))))
      (setq namespace-name nil
            imports (mapcar #'phpinspect-meta-token
                            (phpinspect-toc-tokens-in-region
                             all-imports 0 (phpinspect-meta-start meta)))))

    (list imports namespace-name)))

(cl-defmethod phpinspect-buffer-index-classes ((buffer phpinspect-buffer) (classes (head phpinspect-splayt)))
  (let ((declarations (phpinspect-buffer-declarations buffer))
        (namespaces (phpinspect-buffer-namespaces buffer))
        (buffer-imports (phpinspect-buffer-imports buffer))
        (project (phpinspect-buffer-project buffer)))
    (if (phpinspect-buffer-classes buffer)
        (pcase-let* ((`(,new-classes ,deleted-classes) (phpinspect-toc-update
                                                        (phpinspect-buffer-classes buffer)
                                                        classes (phpinspect-buffer-root-meta buffer)))
                     (new-declarations) (declaration) (replaced) (indexed) (class))
          (dolist (class new-classes)
            (when (setq declaration (phpinspect-toc-token-at-or-after-point declarations (phpinspect-meta-start class)))
              (push (cons (phpinspect-meta-token declaration) class) new-declarations)))

          (dolist (deleted deleted-classes)
            (if (and (setq class (phpinspect-buffer-get-index-for-token
                                  buffer (phpinspect-meta-token deleted)))
                     (setq replaced (assoc (phpinspect--class-declaration class) new-declarations #'equal)))
                (pcase-let ((`(,imports ,namespace-name) (phpinspect-get-token-index-context namespaces buffer-imports (cdr replaced))))
                  (phpinspect-buffer-update-index-reference-for-token
                   buffer (phpinspect-meta-token deleted) (phpinspect-meta-token (cdr replaced)))
                  (phpinspect--class-update-declaration class (car replaced) imports namespace-name)
                  (push (cdr replaced) indexed))
              (phpinspect-buffer-delete-index-for-token buffer (phpinspect-meta-token deleted))))

          (dolist (class new-declarations)
            (unless (memq (cdr class) indexed)
              (let (imports namespace-name class-name class-obj)
                (pcase-setq `(,imports ,namespace-name) (phpinspect-get-token-index-context namespaces buffer-imports (cdr class))
                            `(,class-name) (phpinspect--index-class-declaration
                                            (car class)
                                            (phpinspect--make-type-resolver
                                             (phpinspect--uses-to-types imports)
                                             (phpinspect-class-block (phpinspect-meta-token (cdr class)))
                                             namespace-name)))
                (when class-name
                  (setq  class-obj (phpinspect-project-get-class-create project class-name 'no-enqueue))
                  (phpinspect-buffer-set-index-reference-for-token buffer (phpinspect-meta-token (cdr class)) class-obj)
                  (phpinspect--class-update-declaration class-obj (car class) imports namespace-name))))))
      ;; Else: Index all classes
      (setf (phpinspect-buffer-classes buffer) (phpinspect-make-toc classes))
      (phpinspect-splayt-traverse (class classes)
        (pcase-let* ((declaration (phpinspect-toc-token-at-or-after-point declarations (phpinspect-meta-start class)))
                     (`(,imports ,namespace-name) (phpinspect-get-token-index-context namespaces buffer-imports class))
                     (`(,class-name) (phpinspect--index-class-declaration
                                      (phpinspect-meta-token declaration)
                                      (phpinspect--make-type-resolver
                                       (phpinspect--uses-to-types imports)
                                       (phpinspect-class-block (phpinspect-meta-token class))
                                       namespace-name)))
                     (class-obj))
          (when class-name
            (setq class-obj (phpinspect-project-get-class-create project class-name 'no-enqueue))
            (phpinspect-buffer-set-index-reference-for-token buffer (phpinspect-meta-token class) class-obj)
            (phpinspect--class-update-declaration class-obj (phpinspect-meta-token declaration) imports namespace-name)))))))

(cl-defmethod phpinspect-buffer-index-functions ((buffer phpinspect-buffer) (functions (head phpinspect-splayt)))
  (let ((classes (phpinspect-buffer-classes buffer))
        (namespaces (phpinspect-buffer-namespaces buffer))
        (imports (phpinspect-buffer-imports buffer))
        to-be-indexed class-environments class indexed)
    (if (phpinspect-buffer-functions buffer)
        (pcase-let ((`(,new-funcs ,deleted-funcs)
                     (phpinspect-toc-update (phpinspect-buffer-functions buffer) functions (phpinspect-buffer-root-meta buffer))))
          (setq to-be-indexed new-funcs)

          (dolist (deleted deleted-funcs)
            (phpinspect-buffer-delete-index-for-token buffer (phpinspect-meta-token deleted))))
      (setq to-be-indexed (phpinspect-splayt-to-list functions))
      (setf (phpinspect-buffer-functions buffer) (phpinspect-make-toc functions)))

    (dolist (func to-be-indexed)
      (if (setq class (phpinspect-toc-token-at-point classes (phpinspect-meta-start func)))
          (let (scope static indexed index-env comment-before)
            (if (phpinspect-static-p (phpinspect-meta-token (phpinspect-meta-parent func)))
                (progn
                  (setq static (phpinspect-meta-parent func))
                  (when (phpinspect-scope-p (phpinspect-meta-token (phpinspect-meta-parent static)))
                    (setq scope `(,(car (phpinspect-meta-token (phpinspect-meta-parent static)))
                                  ,(phpinspect-meta-token func))
                          comment-before (phpinspect-meta-find-left-sibling (phpinspect-meta-parent static)))))
              (when (phpinspect-scope-p (phpinspect-meta-token (phpinspect-meta-parent func)))
                (setq scope (phpinspect-meta-token (phpinspect-meta-parent func))
                      comment-before (phpinspect-meta-find-left-sibling (phpinspect-meta-parent func)))))

            (unless scope (setq scope `(:public ,(phpinspect-meta-token func))))

            (unless (setq index-env (alist-get class class-environments nil nil #'eq))
              (setq index-env (phpinspect-get-token-index-context namespaces imports class))
              (setcar index-env (phpinspect--uses-to-types (car index-env)))
              (push (phpinspect--make-type-resolver (car index-env) (phpinspect-meta-token class) (cadr index-env))
                    index-env)
              (push (cons class index-env) class-environments))

            (unless comment-before
              (setq comment-before (phpinspect-meta-find-left-sibling func)))

            (setq comment-before (phpinspect-meta-token comment-before))

            (pcase-let ((`(,type-resolver) index-env)
                        (class-obj (phpinspect-buffer-get-index-for-token buffer (phpinspect-meta-token class))))
              (unless class-obj (error "Unable to find class obj for class %s" (phpinspect-meta-token class)))

              (setq indexed (phpinspect--index-function-from-scope
                             type-resolver
                             scope
                             (and (phpinspect-comment-p comment-before) comment-before)))
              (if static
                  (phpinspect--class-set-static-method class-obj indexed)
                (phpinspect--class-set-method class-obj indexed))

              (phpinspect-buffer-set-index-reference-for-token
               buffer (phpinspect-meta-token func)
               (cons (phpinspect--class-name class-obj) indexed))))
        ;; Else: index function
        (pcase-let ((`(,imports ,namespace-name) (phpinspect-get-token-index-context namespaces imports func))
                    (comment-before (phpinspect-meta-find-left-sibling func)))
          (setq indexed (phpinspect--index-function-from-scope
                         (phpinspect--make-type-resolver
                          (phpinspect--uses-to-types imports) nil namespace-name)
                         `(:public ,(phpinspect-meta-token func))
                         (and (phpinspect-comment-p comment-before) comment-before)
                         nil namespace-name))
          (phpinspect-project-set-function (phpinspect-buffer-project buffer) indexed)
          (phpinspect-buffer-set-index-reference-for-token
           buffer (phpinspect-meta-token func)
           (cons (phpinspect-buffer-project buffer) indexed)))))))


(cl-defmethod phpinspect-buffer-index-class-variables ((buffer phpinspect-buffer) (class-variables (head phpinspect-splayt)))
  (let ((classes (phpinspect-buffer-classes buffer))
        (namespaces (phpinspect-buffer-namespaces buffer))
        (imports (phpinspect-buffer-imports buffer))
        to-be-indexed class-environments class class-obj)
    (if (phpinspect-buffer-class-variables buffer)
        (pcase-let ((`(,new-vars ,deleted-vars)
                     (phpinspect-toc-update
                      (phpinspect-buffer-class-variables buffer) class-variables (phpinspect-buffer-root-meta buffer))))
          (setq to-be-indexed new-vars)

          (dolist (deleted deleted-vars)
            (phpinspect-buffer-delete-index-for-token buffer (phpinspect-meta-token deleted))))

      (setq to-be-indexed (phpinspect-splayt-to-list class-variables))
      (setf (phpinspect-buffer-class-variables buffer) (phpinspect-make-toc class-variables)))

    (dolist (var to-be-indexed)
      (when (and class (> (phpinspect-meta-start var) (phpinspect-meta-end class)))
        (setq class nil))

      (unless class
        (setq class (phpinspect-toc-token-at-point classes (phpinspect-meta-start var))))

      (setq class-obj (phpinspect-buffer-get-index-for-token buffer (phpinspect-meta-token class)))

      (let (scope static indexed index-env comment-before)
        (if (phpinspect-static-p (phpinspect-meta-token (phpinspect-meta-parent var)))
            (progn
              (setq static (phpinspect-meta-parent var))
              (when (phpinspect-scope-p (phpinspect-meta-token (phpinspect-meta-parent static)))
                (setq scope `(,(car (phpinspect-meta-token (phpinspect-meta-parent static)))
                              ,(phpinspect-meta-token var))
                      comment-before (phpinspect-meta-find-left-sibling (phpinspect-meta-parent static)))))
          (when (phpinspect-scope-p (phpinspect-meta-token (phpinspect-meta-parent var)))
            (setq scope (phpinspect-meta-token (phpinspect-meta-parent var))
                  comment-before (phpinspect-meta-find-left-sibling (phpinspect-meta-parent var)))))

        (unless scope (setq scope `(:public ,(phpinspect-meta-token var))))

        (unless (setq index-env (alist-get class class-environments nil nil #'eq))
          (setq index-env (phpinspect-get-token-index-context namespaces imports class))
          (push (cons class index-env) class-environments))

        (unless comment-before
          (setq comment-before (phpinspect-meta-find-left-sibling var)))

        (setq comment-before (phpinspect-meta-token comment-before))

        (pcase-let* ((`(,imports ,namespace-name) index-env)
                     (type-resolver
                      (phpinspect--make-type-resolver
                       (phpinspect--uses-to-types imports)
                       (phpinspect-meta-token class)
                       namespace-name)))

          (setq indexed
                (if (phpinspect-const-p (phpinspect-meta-token var))
                    (phpinspect--index-const-from-scope scope)
                  (phpinspect--index-variable-from-scope
                   type-resolver
                   scope
                   (and (phpinspect-comment-p comment-before) comment-before)
                   static)))

          (when (and (phpinspect-variable-p (phpinspect-meta-token var)) (not (phpinspect--variable-type indexed)))
            (when-let* ((constructor (phpinspect--class-get-method class-obj (phpinspect-intern-name "__construct")))
                        (rctx (phpinspect--make-resolvecontext :enclosing-tokens (list (phpinspect-meta-token class))
                                                               :enclosing-metadata (list class))))
              (setf (phpinspect--variable-type indexed)
                    (phpinspect-get-pattern-type-in-block
                     rctx (phpinspect--make-pattern :m `(:variable "this") :m `(:object-attrib (:word ,(cadr var))))
                     (phpinspect-function-block (phpinspect--function-token constructor))
                     type-resolver
                     (phpinspect-function-argument-list (phpinspect--function-token constructor))))))


          (phpinspect--class-set-variable class-obj indexed)

          (phpinspect-buffer-set-index-reference-for-token
           buffer (phpinspect-meta-token var)
           (cons (phpinspect--class-name class-obj) indexed)))))))

(cl-defmethod phpinspect-buffer-reparse ((buffer phpinspect-buffer))
  (setf (phpinspect-buffer-tree buffer) nil)
  (setf (phpinspect-buffer-map buffer) (phpinspect-make-bmap))
  (setf (phpinspect-buffer-declarations buffer) nil)
  (setf (phpinspect-buffer-imports buffer) nil)
  (phpinspect-edtrack-clear (phpinspect-buffer-edit-tracker buffer))
  (phpinspect-buffer-parse buffer 'no-interrupt))

(cl-defmethod phpinspect-buffer-update-project-index ((buffer phpinspect-buffer))
  (when (phpinspect-buffer-project buffer)
    (let ((map (phpinspect-buffer-map buffer)))
      (unless (eq map (phpinspect-buffer--last-indexed-bmap buffer))
        (phpinspect-buffer-index-imports buffer (phpinspect-bmap-imports map))
        (phpinspect-buffer-index-declarations buffer (phpinspect-bmap-declarations map))
        (phpinspect-buffer-index-namespaces buffer (phpinspect-bmap-namespaces map))
        (phpinspect-buffer-index-classes buffer (phpinspect-bmap-classes map))
        (phpinspect-buffer-index-functions buffer (phpinspect-bmap-functions map))
        (phpinspect-buffer-index-class-variables buffer (phpinspect-bmap-class-variables map))
        (setf (phpinspect-buffer--last-indexed-bmap buffer) map)))))

(defsubst phpinspect-buffer-parse-map (buffer)
  (phpinspect-buffer-parse buffer)
  (phpinspect-buffer-map buffer))

(cl-defmethod phpinspect-buffer-register-edit
  ((buffer phpinspect-buffer) (start integer) (end integer) (pre-change-length integer))
  "Mark a region of the buffer as edited."

  ;; Take into account "atoms" (tokens without clear delimiters like words,
  ;; variables and object attributes. The meaning of these tokens will change as
  ;; they grow or shrink, so their ful regions need to be marked for a reparse).
  (save-excursion
    (goto-char start)
    (when (looking-back "\\($|->|::\\)?[^][)(}{[:blank:]\n;'\"]+" nil t)
      (setq start (- start (length (match-string 0))))
      (setq pre-change-length (+ pre-change-length (length (match-string 0))))))

  (phpinspect-edtrack-register-edit
   (phpinspect-buffer-edit-tracker buffer) start end pre-change-length))

(cl-defmethod phpinspect-buffer-tokens-enclosing-point ((buffer phpinspect-buffer) point)
  (phpinspect-bmap-tokens-overlapping (phpinspect-buffer-map buffer) point))

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

(cl-defmethod phpinspect-buffer-root-meta ((buffer phpinspect-buffer))
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
      (pop-to-buffer (generate-new-buffer "phpinspect-buffer-tree"))
      (insert (pp-to-string (phpinspect--index-tokens (phpinspect-buffer-parse buffer 'no-interrupt))))
      (read-only-mode))))

(defun phpinspect-after-change-function (start end pre-change-length)
  (when phpinspect-current-buffer
    (phpinspect-buffer-register-edit phpinspect-current-buffer start end pre-change-length)))

(provide 'phpinspect-buffer)
