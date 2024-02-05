;;; phpinspect.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc

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

;; FIXME: Storage mechanism for abstract methods is missing (@abstract-method)

;;; Code:

(require 'phpinspect-project)
(require 'phpinspect-autoload)
(require 'phpinspect-worker)
(require 'inline)

(defcustom phpinspect-load-stubs t
  "If and when phpinspect should load code stubs."
  :type '(choice
          (const
           :tag
           "Load stubs on first mode init." t)
          (const
           :tag
           "Never load stubs." nil))
  :group 'phpinspect)

(defvar phpinspect-buffers (make-hash-table :test #'eq)
  "All buffers for which `phpinspect-mode' is currently active.

Hash table with buffer (native emacs buffer object, `bufferp') as
key, and a reset-function as value. The reset-function is called
without arguments when the cache is purged (see
`phpinspect-purge-cache'.")

(defun phpinspect-register-current-buffer (reset-func)
  (puthash (current-buffer) reset-func phpinspect-buffers))

(defun phpinspect-unregister-current-buffer ()
  (remhash (current-buffer) phpinspect-buffers))

(defvar phpinspect-stub-cache nil
  "An instance of `phpinspect--cache' containing an index of PHP
functions and classes which phpinspect preloads. This index is
not supposed to be mutated after initial creation.")

(defmacro phpinspect--cache-edit (cache &rest body)
  (declare (indent 1))
  `(unless (phpinspect--cache-read-only-p ,cache)
     ,@body))

(defvar phpinspect-cache nil
  "An object used to store and access metadata of PHP projects.")

(cl-defstruct (phpinspect--cache (:constructor phpinspect--make-cache))
  (read-only-p nil
               :type boolean
               :documentation
               "Whether this cache instance is read-only, meaning that it's data
should never be changed.

When the value of this slot is non-nil:

- Actions that would normally mutate it's data should become
no-ops.
- All projects that are retrieved from it should be marked as read-only as well.")
  (extra-class-retriever nil
                         :type lambda
                         :documentation
                         "A function that should accept a `phpinspect--type' and return
matching `phpinspect--class' instances or nil. Used to discover
classes that are defined outside of code that this cache knows about.")
  (extra-function-retriever nil
                            :type lambda
                            :documentation
                            "A function that should accept a `phpinspect-name' (see
`phpinspect-intern-name') and return matching
`phpinspect--function' instances or nil. Used to discover
functions that are defined outside of code that this cache knows
about.")
  (projects (make-hash-table :test 'equal :size 10)
            :type hash-table
            :documentation
            "A `hash-table` with the root directories of projects
as keys and project caches as values."))

(defun phpinspect--get-stub-class (fqn)
  (when phpinspect-stub-cache
    (phpinspect--log "Getting stub class")
    (catch 'return
      (maphash (lambda (_name project)
                 (when-let ((class (phpinspect-project-get-class project fqn)))
                   (throw 'return class)))
               (phpinspect--cache-projects phpinspect-stub-cache)))))

(defun phpinspect--get-stub-function (name)
  (when phpinspect-stub-cache
    (if name
        (catch 'return
          (phpinspect--log "Getting stub function by name %s" name)
          (maphash (lambda (_name project)
                     (when-let ((class (phpinspect-project-get-function project name)))
                       (throw 'return class)))
                   (phpinspect--cache-projects phpinspect-stub-cache)))
      (let* ((funcs (cons nil nil))
             (funcs-rear funcs))
        (phpinspect--log "Retrieving all stub functions for nil name")
        (maphash (lambda (_name project)
                   (setq funcs-rear (last (nconc funcs-rear (phpinspect-project-get-functions project)))))
                 (phpinspect--cache-projects phpinspect-stub-cache))
        (cdr funcs)))))

(defun phpinspect--get-or-create-global-cache ()
  "Get `phpinspect-cache'.
If its value is nil, it is created and then returned."
  (or phpinspect-cache
      (setq phpinspect-cache
            (phpinspect--make-cache
             :extra-class-retriever #'phpinspect--get-stub-class
             :extra-function-retriever #'phpinspect--get-stub-function))))

(defun phpinspect-purge-cache ()
  "Assign a fresh, empty cache object to `phpinspect-cache'.
This effectively purges any cached code information from all
currently opened projects."
  (interactive)
  (when phpinspect-cache
    ;; Allow currently known cached projects to cleanup after themselves
    (maphash (lambda (_ project)
               (phpinspect-project-purge project))
             (phpinspect--cache-projects phpinspect-cache)))


  (maphash (lambda (buffer reset-hook)
             (with-current-buffer buffer
               (funcall reset-hook)))
           phpinspect-buffers)

  ;; Assign a fresh cache object
  (setq phpinspect-cache (phpinspect--get-or-create-global-cache))
  (setq phpinspect-names (phpinspect-make-name-hash))
  (phpinspect-define-standard-types))

(cl-defmethod phpinspect--cache-get-project
  ((cache phpinspect--cache) (project-root string))
  (let ((project (gethash project-root (phpinspect--cache-projects cache))))
    (when (and project (phpinspect--cache-read-only-p cache)
               (not (phpinspect-project-read-only-p project)))
      (setf (phpinspect-project-read-only-p project) t))

    project))

(defun phpinspect-get-or-create-cached-project-class (project-root class-fqn)
  (when project-root
    (let ((project (phpinspect--cache-get-project-create
                    (phpinspect--get-or-create-global-cache)
                    project-root)))
      (phpinspect-project-get-class-extra-or-create project class-fqn))))

(cl-defmethod phpinspect--cache-get-project-create
  ((cache phpinspect--cache) (project-root string))
  "Get a project that is located in PROJECT-ROOT from CACHE.
If no such project exists in the cache yet, it is created and
then returned."
  (let ((project (phpinspect--cache-get-project cache project-root)))
    (unless project
      (phpinspect--cache-edit cache
        (setq project
              (puthash project-root
                       (phpinspect--make-project
                        :fs (phpinspect-make-fs)
                        :root project-root
                        :extra-class-retriever (phpinspect--cache-extra-class-retriever cache)
                        :extra-function-retriever (phpinspect--cache-extra-function-retriever cache)
                        :worker (phpinspect-make-dynamic-worker))
                       (phpinspect--cache-projects cache)))
        (let ((autoloader (phpinspect-make-autoloader
                           :fs (phpinspect-project-fs project)
                           :file-indexer (phpinspect-project-make-file-indexer project)
                           :project-root-resolver (phpinspect-project-make-root-resolver project))))
          (setf (phpinspect-project-autoload project) autoloader)
          (phpinspect-autoloader-refresh autoloader)
          (phpinspect-project-enqueue-include-dirs project))))
    project))

(defun phpinspect-project-enqueue-include-dirs (project)
  (interactive (list (phpinspect--cache-get-project-create
                      (phpinspect--get-or-create-global-cache)
                      (phpinspect-current-project-root))))
  (phpinspect-project-edit project
    (let ((dirs (alist-get 'include-dirs
                           (alist-get (phpinspect-project-root project)
                                      phpinspect-projects
                                      nil nil #'string=))))
      (dolist (dir dirs)
        (phpinspect-message "enqueueing dir %s" dir)
        (phpinspect-worker-enqueue
         (phpinspect-project-worker project)
         (phpinspect-make-index-dir-task :dir dir :project project))))))

(defun phpinspect-project-add-include-dir (dir)
  "Configure DIR as an include dir for the current project."
  (interactive (list (read-directory-name "Include Directory: ")))
  (custom-set-variables '(phpinspect-projects))
  (let ((existing
         (alist-get (phpinspect-current-project-root) phpinspect-projects nil #'string=)))
    (if existing
        (push dir (alist-get 'include-dirs existing))
      (push `(,(phpinspect-current-project-root) . ((include-dirs . (,dir)))) phpinspect-projects)))

  (customize-save-variable 'phpinspect-projects phpinspect-projects)

  (phpinspect-project-enqueue-include-dirs (phpinspect--cache-get-project-create
                                            (phpinspect--get-or-create-global-cache)
                                            (phpinspect-current-project-root))))

(defconst phpinspect-stub-directory
  (expand-file-name  "stubs" (file-name-directory (macroexp-file-name)))
  "Directory where PHP stub files are located.")

(defconst phpinspect-data-directory
  (expand-file-name  "data" (file-name-directory (macroexp-file-name)))
  "Directory for data distributed with phpinspect.")

(defconst phpinspect-stub-cache-file
  (expand-file-name "builtin-stubs.eld" phpinspect-data-directory)
  "")

(defconst phpinspect-builtin-index-file
  (expand-file-name (concat "builtin-stubs-index.eld" (if (zlib-available-p) ".gz" ""))
                    phpinspect-data-directory)
  "")

(defun phpinspect-build-stub-cache ()
  (let* ((cache (phpinspect--make-cache))
         (builtin-project (phpinspect--cache-get-project-create cache "builtins"))
         (phpinspect-worker 'nil-worker))
    (phpinspect-project-add-index builtin-project (phpinspect-build-stub-index))))

(defun phpinspect-build-stub-index ()
  (phpinspect--index-tokens
   (phpinspect-parse-file
    (expand-file-name "builtins.php" phpinspect-stub-directory))))

(defun phpinspect-dump-stub-index ()
  (interactive)
  (let* ((phpinspect-names (phpinspect-make-name-hash))
         (index (phpinspect-build-stub-index)))
    (with-temp-buffer
      (let ((print-length nil)
            (print-level nil)
            (print-circle t))

        (prin1 (list (cons 'names phpinspect-names)
                     (cons 'index index))
               (current-buffer))
        (write-file phpinspect-builtin-index-file)))))

(defun phpinspect-load-stub-index ()
  (interactive)
  (unless (file-exists-p phpinspect-builtin-index-file)
    (phpinspect-message "No stub index dump found, dumping stub index ...")
    (phpinspect-dump-stub-index))

  (let* ((data (with-temp-buffer
                 (insert-file-contents phpinspect-builtin-index-file)
                 (goto-char (point-min))
                 (read (current-buffer))))
         (project (phpinspect--make-project :worker 'nil-worker)))
    (phpinspect-purge-cache)
    (setq phpinspect-names (alist-get 'names data))
    (phpinspect-define-standard-types)
    (setq phpinspect-stub-cache (phpinspect--make-cache))
    (phpinspect-project-add-index project (alist-get 'index data))
    (puthash "builtins" project (phpinspect--cache-projects phpinspect-stub-cache))
    (setf (phpinspect--cache-read-only-p phpinspect-stub-cache) t)))

(cl-defstruct (phpinspect-bidi-graph (:constructor phpinspect-make-bidi-graph))
  "A bidirectional graph."
  (rel (make-hash-table :test #'eq :size 2000 :rehash-size 2.0))
  (rel-back (make-hash-table :test #'eq :size 2000 :rehash-size 2.0)))

(defun phpinspect-bidi-graph-link (graph obj1 obj2)
  (let ((existing-rel (gethash obj1 (phpinspect-bidi-graph-rel graph)))
        (existing-back-rel (gethash obj2 (phpinspect-bidi-graph-rel-back graph))))
    (if existing-rel
        (setcdr existing-rel (cons obj2 (cdr existing-rel)))
      (puthash obj1 (list obj2) (phpinspect-bidi-graph-rel graph)))

    (if existing-back-rel
        (setcdr existing-back-rel (cons obj1 (cdr existing-back-rel)))
      (puthash obj2 (list obj1) (phpinspect-bidi-graph-rel-back graph)))))

(defun phpinspect-bidi-graph-unlink-between (graph obj1 obj2)
  (when-let ((rel (gethash obj1 (phpinspect-bidi-graph-rel graph)))
             (back-rel (gethash obj2 (phpinspect-bidi-graph-rel-back graph))))
    (puthash obj1 (delq obj2 rel) (phpinspect-bidi-graph-rel graph))
    (puthash obj2 (delq obj1 back-rel) (phpinspect-bidi-graph-rel-back graph))))

(defun phpinspect-bidi-graph-unlink (graph obj)
  (when-let ((obj-link (gethash obj (phpinspect-bidi-graph-rel graph))))
    (dolist (back-rel obj-link)
      (when-let ((back-link (gethash back-rel (phpinspect-bidi-graph-rel-back graph))))
        (puthash back-rel (delq obj back-link) (phpinspect-bidi-graph-rel-back graph))))
    (remhash obj (phpinspect-bidi-graph-rel graph))))

(defun phpinspect-bidi-graph-get-linking-from (graph obj)
  (gethash obj (phpinspect-bidi-graph-rel graph)))

(defun phpinspect-bidi-graph-get-linking-to (graph obj)
  (gethash obj (phpinspect-bidi-graph-rel-back graph)))

(cl-defstruct (phpinspect-cache (:constructor phpinspect-make-cache))
  (groups (make-hash-table :test #'equal :size 2000 :rehash-size 1.2)))

(cl-defstruct (phpinspect-cache-type  (:constructor phpinspect-make-cache-type))
  (group nil)
  (category nil)
  (name nil)
  (methods nil)
  (variables nil))

(cl-defstruct (phpinspect-cache-namespace (:constructor phpinspect-make-cache-namespace))
  (name nil
        :type phpinspect-name)
  (group nil
         :type phpinspect-cache-group)
  (types nil)
  (functions nil))

(cl-defstruct (phpinspect-cache-group (:constructor phpinspect-make-cache-group))
  (autoloader nil
              :type phpinspect-autoloader)
  (spec nil)
  (namespaces (make-hash-table :test #'eq :size 2000 :rehash-size 2.0))
  (extends (phpinspect-make-bidi-graph))
  (implements (phpinspect-make-bidi-graph)))

(eval-and-compile
  (defconst phpinspect-cache-types '(@interface @trait @class @method @type
                                               @abstract-method @function @variable @namespace)
    "Types of entities that the cache can store.")

  (defconst phpinspect-cache-containing-types '(@class @trait @interface)
    "Types of entities that the cache can store members for.")

  (defconst phpinspect-cache-member-types '(@method @abstract-method @function @variable)
    "Types of entities that the cache can store as members."))

(defun phpinspect-cache-type-get-extends (type)
  (phpinspect-cache-group-get-extends
   (phpinspect-cache-type-group type)
   (phpinspect-cache-type-name type)))

(defun phpinspect-cache-type-get-implements (type)
  (phpinspect-cache-group-get-implements
   (phpinspect-cache-type-group type)
   (phpinspect-cache-type-name type)))

(defun phpinspect-cache-type-add-method (type method)
  (push (cons (phpinspect--function-short-name-symbol method)
              method)
        (phpinspect-cache-type-methods type)))

(defun phpinspect-cache-group-get-extends (group type-name)
  (phpinspect-bidi-graph-get-linking-from
   (phpinspect-cache-group-extends group)
   type-name))

(defun phpinspect-cache-group-get-implements (group type-name)
  (phpinspect-bidi-graph-get-linking-from
   (phpinspect-cache-group-implements group)
   type-name))

(defun phpinspect-cache-group-get-extending (group type-name)
  (phpinspect-bidi-graph-get-linking-to
   (phpinspect-cache-group-extends group)
   type-name))

(defun phpinspect-cache-group-get-implementing (group type-name)
  (phpinspect-bidi-graph-get-linking-to
   (phpinspect-cache-group-implements group)
   type-name))

(defun phpinspect-cache-group-get-namespace (group namespace)
  (gethash namespace (phpinspect-cache-group-namespaces group)))

(defun phpinspect-cache-group-get-namespace-create (group name)
  "Retrieve namespace by NAME from GROUP.

Name must be of type phpinspect-name (see `phpinspect-intern-name`)."
  (or (phpinspect-cache-group-get-namespace group name)
      (puthash name (phpinspect-make-cache-namespace :name name :group group)
               (phpinspect-cache-group-namespaces group))))

(defun phpinspect-cache-namespace-get-type-create (namespace type category group)
  (or (phpinspect-cache-namespace-get-type namespace type category)
      (cdar
       (push (cons (phpinspect--type-short-name type)
                   (phpinspect-make-cache-type
                    :name (phpinspect--type-name-symbol type)
                    :category category
                    :group group))
             (phpinspect-cache-namespace-types namespace)))))

(defun phpinspect-cache-namespace-get-type (namespace type category)
  "Retrieve cached type metadata for TYPE attributed to CATEGORY from NAMESPACE.

If an autoloader is available for the cache group that NAMESPACE
belongs to and no in-memory match is found, the autoloader is
queried and any resulting locations are indexed, after which the
resulting metadata is returned."
  (let* ((short-name (phpinspect--type-short-name type))
         (namespace-name (phpinspect-cache-namespace-name namespace))
         (autoloader (phpinspect-cache-group-autoloader
                      (phpinspect-cache-namespace-group namespace)))
         (entity (cdr (assq short-name
                            (phpinspect-cache-namespace-types namespace)))))

    ;; No entity was found in-memory, attempt to query autoloader when
    ;; available.
    (when (and (not entity) autoloader)
      (let ((names (phpinspect-autoloader-get-fqn-bag autoloader namespace-name)))
        (when (memq short-name names)
          ;; ... parse/index type
          (setq entity nil))))

    ;; Only return types of the requested category
    (and entity
         (or (eq '@type category)
             (eq category (phpinspect-cache-type-category entity)))
         entity)))

  ;; (when-let ((entity
  ;;             (cdr (assq (phpinspect--type-short-name type)
  ;;                        (phpinspect-cache-namespace-types namespace)))))
  ;;   (and (or (eq '@type category)
  ;;            (eq category (phpinspect-cache-type-category entity)))
  ;;        entity)))

(defun phpinspect-cache-namespace-delete-type (namespace type category)
  "Delete TYPE attributed to CATEGORY from the in-memory cache of NAMESPACE."
  (let ((types (phpinspect-cache-namespace-types namespace))
        (name (phpinspect--type-short-name type))
        cell-before)
    (catch 'break
      (while types
        (let ((type-cell (car types)))
          (when (eq (car type-cell) name)
            (when (or (eq '@type category)
                      (eq category
                          (phpinspect-cache-type-category (cdr type-cell))))
              (if cell-before
                  (setcdr cell-before (cdr types))
                (setf (phpinspect-cache-namespace-types namespace) (cdr types)))

              (throw 'break (cdr type-cell)))))

        (setq cell-before types
              types (cdr types))))))

(defun phpinspect-cache-namespace-delete-function (namespace function-name)
  (let ((functions (phpinspect-cache-namespace-functions namespace))
        (name (phpinspect-intern-name
               (phpinspect-type-name-short
                (phpinspect-name-string function-name))))
        cell-before)
    (catch 'break
      (while functions
        (let ((type-cell (car functions)))
          (when (eq (car type-cell) name)
            (if cell-before
                (setcdr cell-before (cdr functions))
              (setf (phpinspect-cache-namespace-functions namespace)
                    (cdr functions)))
            (throw 'break (cdr type-cell))))

        (setq cell-before functions
              functions (cdr functions))))))

(defun phpinspect-cache-group-unlink-type-dependencies (group name)
  (phpinspect-bidi-graph-unlink
   (phpinspect-cache-group-extends group) name)

  (phpinspect-bidi-graph-unlink
   (phpinspect-cache-group-implements group) name))

(defmacro phpinspect--inline-wildcard-param-p (param)
  `(let ((param ,param))
     (and (inline-const-p param) (eq '* (inline-const-val param)))))

(define-inline phpinspect-cache-query--do-delete-type
  (group param type _member namespace _implements _extends)
  (setq type (inline-const-val type))

  (inline-letevals (group param namespace)
    (if (phpinspect--inline-wildcard-param-p param)
        (if namespace
            (inline-quote
             (when-let ((namespace (phpinspect-cache-group-get-namespace
                                    ,group ,namespace)))
               (let (resultset)
                 (setf (phpinspect-cache-namespace-types namespace)
                       (seq-filter
                        (lambda (type-cell)
                          (if ,(if (eq type '@type)
                                   t
                                 `(eq (phpinspect-cache-type-category (cdr type-cell))
                                      (quote ,type)))
                              (progn
                                (push (cdr type-cell) resultset)
                                (phpinspect-cache-group-unlink-type-dependencies
                                 ,group (car type-cell))
                                     nil)
                            t))
                        (phpinspect-cache-namespace-types namespace)))
                 (cons 'phpinspect-cache-multiresult resultset))))
          (inline-quote
           (let (resultset)
             (dolist (namespace (hash-table-values
                                 (phpinspect-cache-group-namespaces ,group)))
               (let (new-types)
                 (dolist (type-cell (phpinspect-cache-namespace-types namespace))
                   (if ,(if (eq type '@type)
                            t
                          `(eq (phpinspect-cache-type-category
                                (cdr type-cell))
                               (quote ,type)))
                       (progn
                         (push (cdr type-cell) resultset)
                         (phpinspect-cache-group-unlink-type-dependencies
                          ,group (car type-cell))
                         nil)
                     (push type-cell new-types)))
                 (setf (phpinspect-cache-namespace-types namespace) new-types)))
             (cons 'phpinspect-cache-multiresult resultset))))
      (inline-quote
       (when-let* ((namespace (phpinspect-cache-group-get-namespace-create
                          ,group
                          (or ,namespace
                              (phpinspect--type-namespace ,param))))
                   (result (phpinspect-cache-namespace-delete-type
                            namespace ,param (quote ,type))))
         (phpinspect-cache-group-unlink-type-dependencies
          ,group (phpinspect-cache-type-name result))
         result)))))

(define-inline phpinspect-cache-query--do-delete-function
  (group param _type _member namespace _implements _extends)

  (inline-letevals (group param namespace)
    (if (phpinspect--inline-wildcard-param-p param)
        (if namespace
            (inline-quote
             (when-let ((namespace (phpinspect-cache-group-get-namespace
                                    ,group ,namespace)))
               (let ((resultset
                      (cons 'phpinspect-cache-multiresult
                            (mapcar #'cdr
                                    (phpinspect-cache-namespace-functions
                                     namespace)))))
                 (setf (phpinspect-cache-namespace-functions namespace) nil)
                 resultset)))
          (inline-quote
           (let (resultset)
             (dolist (namespace (hash-table-values
                                 (phpinspect-cache-group-namespaces ,group)))
               (setq resultset
                     (nconc
                      (mapcar #'cdr
                              (phpinspect-cache-namespace-functions
                               namespace))
                      resultset)))
             (cons 'phpinspect-cache-multiresult resultset))))
      (inline-quote
       (when-let ((namespace
                   (phpinspect-cache-group-get-namespace
                    ,group (or ,namespace
                               (phpinspect-intern-name
                                (phpinspect-type-name-namespace
                                 (phpinspect-name-string ,param)))))))
         (phpinspect-cache-namespace-delete-function
          namespace ,param))))))

(defun phpinspect--assq-delete (key alist)
  (let* ((result (cons nil nil))
         (result-rear result)
         deleted)
    (catch 'break
      (while alist
        (if (eq key (caar alist))
            (progn
              (setcdr result-rear (cdr alist))
              (setq deleted (car alist))
              (throw 'break nil))
          (setq result-rear (setcdr result-rear (cons (car alist) nil)))
          (setq alist (cdr alist)))))

    (list deleted (cdr result))))

;; OPTIMIZE: this is probably not optimal for large numbers of keys
(defun phpinspect--assq-delete-multi (keys alist)
  (let (all-deleted)
    (dolist (key keys)
      (pcase-let ((`(,deleted ,filtered) (phpinspect--assq-delete key alist)))
        (push deleted all-deleted)
        (setq alist filtered)))

    (list all-deleted alist)))

(defun phpinspect--alist-values (alist)
  (let* ((vals (cons nil nil))
         (vals-rear vals))
    (dolist (cell alist)
      (setq vals-rear (setcdr vals-rear (cons (cdr cell) nil))))

    (cdr vals)))

(defun phpinspect--alist-keys (alist)
  (let* ((keys (cons nil nil))
         (keys-rear keys))
    (dolist (cell alist)
      (setq keys-rear (setcdr keys-rear (cons (car cell) nil))))

    (cdr keys)))

(defun phpinspect-cache-query--do-delete-method (cache group param type member)
  (when-let ((member-type
              (car
               (phpinspect-cache-transact cache (list (phpinspect-cache-group-spec group))
                 :get member :as @type))))
    (cond ((eq '* param)
           (let ((deleted (phpinspect--alist-values
                           (phpinspect-cache-type-methods member-type))))
             (setf (phpinspect-cache-type-methods member-type) nil)
             (cons 'phpinspect-cache-multiresult deleted)))
          ((and (listp param) (not (phpinspect-name-p param)))
           (pcase-let ((`(,deleted ,filtered)
                        (phpinspect--assq-delete-multi
                         param (phpinspect-cache-type-methods member-type))))
             (setf (phpinspect-cache-type-methods member-type) filtered)
             (cons 'phpinspect-cache-multiresult deleted)))
          (t
           (pcase-let ((`(,deleted ,filtered)
                        (phpinspect--assq-delete
                         param (phpinspect-cache-type-methods member-type))))

             (setf (phpinspect-cache-type-methods member-type) filtered)
             deleted)))))

(defun phpinspect-cache-query--do-delete-variable (cache group param type member)
  (when-let ((member-type
              (car
               (phpinspect-cache-transact cache (list (phpinspect-cache-group-spec group))
                 :get member :as @type))))
    (cond ((eq '* param)
           (let ((deleted (phpinspect--alist-values
                           (phpinspect-cache-type-variables member-type))))
             (setf (phpinspect-cache-type-variables member-type) nil)
             (cons 'phpinspect-cache-multiresult deleted)))
          ((and (listp param) (not (phpinspect-name-p param)))
           (pcase-let ((`(,deleted ,filtered)
                        (phpinspect--assq-delete-multi
                         param (phpinspect-cache-type-variables member-type))))
             (setf (phpinspect-cache-type-variables member-type) filtered)
             (cons 'phpinspect-cache-multiresult deleted)))
          (t
           (pcase-let ((`(,deleted ,filtered)
                        (phpinspect--assq-delete
                         param (phpinspect-cache-type-variables member-type))))

             (setf (phpinspect-cache-type-variables member-type) filtered)
             deleted)))))

(define-inline phpinspect-cache-query--do-delete-member
  (cache group param type member namespace implements extends)
  (inline-letevals (cache group param member)
    (setq type (inline-const-val type))

    (pcase type
      ((or '@function '@method)
       (inline-quote
        (phpinspect-cache-query--do-delete-method ,cache ,group ,param (quote ,type) ,member)))
      ('@variable
       (inline-quote
        (phpinspect-cache-query--do-delete-variable ,cache ,group ,param (quote ,type) ,member)))
      (_ (error "Delete not supported for member type %s" type)))))

(defmacro phpinspect-cache-query--do-delete
  (cache group param type member namespace implements extends)
  (cl-assert (symbolp type))

  (cond
   ((memq type (cons '@type phpinspect-cache-containing-types))
    `(phpinspect-cache-query--do-delete-type
      ,group ,param (quote ,type) ,member ,namespace ,implements ,extends))
   ((and (eq '@function type) (not member))
    `(phpinspect-cache-query--do-delete-function
      ,group ,param (quote ,type) ,member ,namespace ,implements ,extends))
   ((and (memq type phpinspect-cache-member-types) member)
    `(phpinspect-cache-query--do-delete-member
      ,cache ,group ,param (quote ,type) ,member ,namespace ,implements ,extends))
   (t (error "Delete not supported for entity type %s" type))))

(defun phpinspect-cache-namespace-get-function (namespace func-name)
  (cdr (assq func-name (phpinspect-cache-namespace-functions namespace))))

(defun phpinspect-cache-group-register-extends (group type extends)
  (if (listp extends)
      (dolist (ext extends)
        (phpinspect-cache-group-register-extends type ext))
        (phpinspect-bidi-graph-link
     (phpinspect-cache-group-extends group)
     (phpinspect--type-name-symbol type)
     (phpinspect--type-name-symbol extends))))

(defun phpinspect-cache-group-register-implements (group type implements)
  (if (listp implements)
      (dolist (ext implements)
        (phpinspect-cache-group-register-implements type ext))
    (phpinspect-bidi-graph-link
     (phpinspect-cache-group-implements group)
     (phpinspect--type-name-symbol type)
     (phpinspect--type-name-symbol implements))))

(defun phpinspect-cache-type-add-variable (type variable)
  (push (cons (phpinspect--variable-name-symbol variable)
              variable)
        (phpinspect-cache-type-variables type)))

(define-inline phpinspect-cache-query--do-insert-method
  (cache group param type member namespace)
  (setq type (inline-const-val type))
  (inline-letevals (cache group param member namespace)
    (inline-quote
     (let ((member-type
            (car
             (phpinspect-cache-transact ,cache (list (phpinspect-cache-group-spec ,group))
               :get ,member
               :in ,namespace
               :as @type)))
           resultset)
       ,(pcase type
          ((or '@function '@method)
           `(if (listp ,param)
                (progn
                  (dolist (method ,param)
                    (push method resultset)
                    (phpinspect-cache-type-add-method member-type method))
                  (cons 'phpinspect-cache-multiresult resultset))
              (cdar (phpinspect-cache-type-add-method member-type ,param))))
          ('@variable
           `(if (listp ,param)
                (progn
                  (dolist (variable ,param)
                    (push variable resultset)
                    (phpinspect-cache-type-add-variable member-type variable))
                  (cons 'phpinspect-cache-multiresult resultset))
              (cdar (phpinspect-cache-type-add-variable member-type ,param))))
          (_ (error "Insert not supported for member type %s" type)))))))

(defun phpinspect-cache-query--do-insert-function (cache group param namespace)
  (let ((namespace
         (phpinspect-cache-group-get-namespace-create
          group (or namespace
                     (phpinspect--function-namespace param)))))
    (or (when-let ((existing (assq (phpinspect--function-name-symbol param)
                                   (phpinspect-cache-namespace-functions namespace))))
          (cdar (setcdr existing (cons param (cdr existing)))))
        (cdar (push (cons (phpinspect--function-short-name-symbol param) param)
                    (phpinspect-cache-namespace-functions namespace))))))

(define-inline phpinspect-cache-query--do-insert-member
    (cache group param type member namespace implements extends)
    (if member
        (inline-quote
         (phpinspect-cache-query--do-insert-method
          ,cache ,group ,param ,type ,member ,namespace))

      ;; Else: insert function in namespace
      (inline-quote
       (phpinspect-cache-query--do-insert-function ,cache ,group ,param ,namespace))))

(defmacro phpinspect-cache-query--do-insert
  (cache group param type member namespace implements extends)
  (cl-assert (symbolp type))

  (cond
   ((memq type (cons '@type phpinspect-cache-containing-types))
    `(let* ((namespace (phpinspect-cache-group-get-namespace-create
                        ,group
                        (or ,namespace (phpinspect--type-namespace ,param)))))
       ,(when extends
          `(phpinspect-cache-group-register-extends ,group ,param ,extends))

       ,(when implements
          `(phpinspect-cache-group-register-implements ,group ,param ,implements))

       (phpinspect-cache-namespace-get-type-create
        namespace ,param (quote ,type) ,group)))
   ((memq type phpinspect-cache-member-types)
    `(phpinspect-cache-query--do-insert-member
      ,cache ,group ,param (quote ,type) ,member ,namespace ,implements ,extends))
   (t (error "Insert not supported for entity type %s" type))))

(defun phpinspect-cache-query--do-get-type-wildcard (group param type namespace)
  "Retrieve all type metadata matching TYPE and NAMESPACE available
in the in-memory cache.

This function does not query an autoloader even if it is
available for the cache group."
  (let* ((all
          (if namespace
              (when-let ((namespace (phpinspect-cache-group-get-namespace group namespace)))

                (phpinspect--alist-values (phpinspect-cache-namespace-types namespace)))
             (mapcan
              (lambda (namespace)
                (mapcar #'cdr (phpinspect-cache-namespace-types namespace)))
              (hash-table-values (phpinspect-cache-group-namespaces group)))))
         filtered)

    (if (eq '@type type)
        (setq filtered all)
      (dolist (row all)
        (when (eq (phpinspect-cache-type-category row) type)
          (push row filtered))))

    (cons 'phpinspect-cache-multiresult filtered)))

(defun phpinspect-cache-insert-index (cache group-spec index)
  (cl-assert (eq 'phpinspect--root-index (car index)))

  (phpinspect-cache-transact cache group-spec
    :insert (alist-get 'functions index)
    :as @function)

  (dolist (class (alist-get 'classes index))
    (let ((class-name (alist-get 'class-name class)))
      (pcase (alist-get 'type class)
        ('@trait
         (phpinspect-cache-transact cache group-spec
           :insert class-name :as @trait))
        ('@interface
         (phpinspect-cache-transact cache group-spec
           :insert class-name :as @interface))
        ('@class
         (phpinspect-cache-transact cache group-spec
           :insert class-name
           :implementing (alist-get 'implements class)
           :extending (alist-get 'extending class)
           :as @class))
        (_ (error "Unexpected class type: %s" (alist-get 'type class))))


      (phpinspect-cache-transact cache group-spec
        :insert (append (alist-get 'methods class)
                        (alist-get 'static-methods class))
        :as @method
        :member-of class-name)

      (phpinspect-cache-transact cache group-spec
        :insert (append (alist-get 'variables class)
                        (alist-get 'constants class)
                        (alist-get 'static-variabes class))
        :as @variable
        :member-of class-name))))

(defun phpinspect-cache-query--do-get-type
  (group param type member namespace implements extends)
    (let ((result
           (if (eq '* param)
               (phpinspect-cache-query--do-get-type-wildcard group param type namespace)
             (when-let* ((namespace (phpinspect-cache-group-get-namespace
                                     group
                                     (or namespace
                                         (phpinspect--type-namespace param)))))
               (phpinspect-cache-namespace-get-type namespace param type)))))

      ;; OPTIMIZE: Performance of these filters won't be very good when using
      ;; wildcards without namespace parameter. Probably worth optimizing when
      ;; this becomes a frequent use case.
      (when implements
        (setq result
              (let ((implementing (phpinspect-cache-group-get-implementing group implements))
                    filtered)
                (when result
                  (if (and (consp result)
                           (eq 'phpinspect-cache-multiresult (car result)))
                      (progn
                        (dolist (res (cdr result))
                          (when (memq (phpinspect-cache-type-name res) implementing)
                            (push res filtered)))
                        (cons 'phpinspect-cache-multiresult filtered))
                    (when (memq (phpinspect-cache-type-name result) implementing)
                      result))))))

      (when extends
        (setq result
              (let ((extending (phpinspect-cache-group-get-extending group extends))
                    filtered)
                (when result
                  (if (and (consp result)
                           (eq 'phpinspect-cache-multiresult (car result)))
                      (progn
                        (dolist (res (cdr result))
                          (when (memq (phpinspect-cache-type-name res) extending)
                            (push res filtered)))
                        (cons 'phpinspect-cache-multiresult filtered))
                    (when (memq (phpinspect-cache-type-name result) extending)
                      result))))))
      result))

(define-inline phpinspect-cache-query--do-get-function
  (group param type member namespace implements extends)
  (inline-letevals (group param member namespace implements extends)
    (if (phpinspect--inline-wildcard-param-p param)
        (if namespace
            (inline-quote
             (when-let ((namespace (phpinspect-cache-group-get-namespace
                                    ,group ,namespace)))
               (cons 'phpinspect-cache-multiresult
                     (mapcar #'cdr
                             (phpinspect-cache-namespace-functions namespace)))))
          (inline-quote
           (let (resultset)
             (dolist (namespace (hash-table-values
                                 (phpinspect-cache-group-namespaces ,group)))
               (setq resultset
                     (nconc
                      (mapcar #'cdr
                              (phpinspect-cache-namespace-functions namespace))
                      resultset)))
             (cons 'phpinspect-cache-multiresult resultset))))
      (if namespace
          (inline-quote
           (when-let ((namespace (phpinspect-cache-group-get-namespace
                                  ,group ,namespace)))
             (phpinspect-cache-namespace-get-function namespace ,param)))
        (inline-quote
         (let* ((name-string (phpinspect-name-string ,param))
                (namespace-name (phpinspect-intern-name
                                 (phpinspect-type-name-namespace name-string)))
                (short-name (phpinspect-intern-name
                             (phpinspect-type-name-short name-string))))
           (if (string-match-p "^\\\\" (phpinspect-name-string ,param))
               (when-let ((namespace (phpinspect-cache-group-get-namespace
                                      ,group namespace-name)))
                 (phpinspect-cache-namespace-get-function namespace short-name))
             (let (resultset)
               (dolist (namespace (hash-table-values
                                   (phpinspect-cache-group-namespaces ,group)))
                 (when-let ((func (phpinspect-cache-namespace-get-function
                                   namespace short-name)))
                   (push func resultset)))
               (cons 'phpinspect-cache-multiresult resultset)))))))))

(defun phpinspect-cache-type-get-method (type method-name)
  (cdr (assq method-name (phpinspect-cache-type-methods type))))

(defun phpinspect-cache-type-get-variable (type method-name)
  (cdr (assq method-name (phpinspect-cache-type-variables type))))

(define-inline phpinspect-cache-query--do-get-member (cache group param type member namespace)
  (inline-letevals (cache group param member namespace)
    (setq type (inline-const-val type))

    (inline-quote
     (when-let ((member-type
                 (car
                  (phpinspect-cache-transact ,cache (list (phpinspect-cache-group-spec ,group))
                    :get ,member
                    :as @type
                    :in ,namespace))))

       ,(pcase type
          ((or '@function '@method)
           (if (phpinspect--inline-wildcard-param-p param)
               `(cons 'phpinspect-cache-multiresult
                      (phpinspect--alist-values (phpinspect-cache-type-methods member-type)))
             `(phpinspect-cache-type-get-method member-type ,param)))
          ('@variable
           (if (phpinspect--inline-wildcard-param-p param)
               `(cons 'phpinspect-cache-multiresult
                      (phpinspect--alist-values (phpinspect-cache-type-variables member-type)))
             `(phpinspect-cache-type-get-variable member-type ,param)))
          (_ (error "Get not supported for member entity type %s" type)))))))

(defmacro phpinspect-cache-query--do-get
  (cache group param type member namespace implements extends)
  (cl-assert (symbolp type))

  (cond
   ((memq type (cons '@type phpinspect-cache-containing-types))
    `(phpinspect-cache-query--do-get-type
      ,group ,param (quote ,type) ,member ,namespace ,implements ,extends))
   ((and (eq '@function type) (not member))
    `(phpinspect-cache-query--do-get-function
      ,group ,param ,type ,member ,namespace ,implements ,extends))
   ((and (memq type phpinspect-cache-member-types) member)
    `(phpinspect-cache-query--do-get-member
      ,cache ,group ,param (quote ,type) ,member ,namespace))
   (t (error "Get not supported for entity type %s" type))))

(defmacro phpinspect-cache-query--wrap-action
    (action cache group param type member namespace implements extends)
  (let* ((param-sym (gensym "param")))
    `(let ((,param-sym ,param))
       (if (and (sequencep ,param-sym) (not (phpinspect-name-p ,param-sym)))
           (let* ((result (cons 'phpinspect-cache-multiresult nil))
                  (result-rear result))
             (seq-doseq (p ,param-sym)
               (when-let ((action-result
                           (,action ,cache ,group p ,type ,member
                                    ,namespace ,implements ,extends)))
                 (setq result-rear
                       (setcdr result-rear
                               (cons action-result nil)))))
             result)
         (,action ,cache ,group ,param ,type ,member ,namespace ,implements ,extends)))))

(defun phpinspect-cache-query--compile-groups (cache group-specs intent)
  (cl-assert (phpinspect-cache-p cache))
  (let (groups)
    (if (phpinspect-cache-group-p cache)
        (list cache)
      (if group-specs
          (dolist (spec group-specs)
            (cl-assert (listp spec))
            (cl-assert (memq (car spec) '(project label))
                       t "Spec car must be the symbol `project' or `label'")
            (let ((group (gethash spec (phpinspect-cache-groups cache))))
              (when (and (eq :insert intent) (not group))
                (setq group (puthash spec (phpinspect-make-cache-group :spec spec)
                                     (phpinspect-cache-groups cache))))
              (push group groups)))
        (if (eq :insert intent)
            (error "Cannot insert without defining cache group")
          (setq groups (hash-table-values (phpinspect-cache-groups cache)))))

      groups)))

(defmacro phpinspect-cache-transact (cache group-specs &rest query)
  "Execute QUERY on CACHE, filtering by GROUP-SPECS.

CACHE must be an instance of `phpinspect-cache' or
`phpinspect-cache-group'. If CACHE is a cache group, the query is
only executed on this group and GROUP-SPECS is
ignored. Otherwise, the groups in CACHE are filtered by
GROUP-SPECS and each group in the resulting list of groups is
subject of QUERY."
  (declare (indent 2))
  (cl-assert (listp query))

  (let (type intent intent-param member namespace implements extends key value)
    (while (setq key (pop query))
      (cl-assert (keywordp key) t "Query keys must be keywords, %s provided" key)
      (setq value (pop query))

      ;; Namespace is allowed to be dynamic/nil
      (unless (eq :in key)
        (cl-assert value t "Key %s has no value" key))

      (pcase key
        ((or :insert :get :delete)
         (when intent
           (error "Defined duplicate intent: %s, %s" intent key))
         (setq intent key
               intent-param value))
        (:as (unless (and (symbolp value)
                          (memq value phpinspect-cache-types))
               (error ":type must be one of %s, %s provided"
                      phpinspect-cache-types value))
             (setq type value))
        (:member-of (setq member value))
        (:in (setq namespace value))
        (:implementing (setq implements value))
        (:extending (setq extends value))
        (_ (error "Unexpected query keyword %s" key))))

    ;; Query validation
    (unless type
      (error "Providing entity type with keyword :as is required."))

    (when (and member (not (memq type phpinspect-cache-member-types)))
      (error "Keyword :member-of can only be used for types %s"
             phpinspect-cache-member-types))

    (when (and extends (not (memq type '(@class @trait @interface @type))))
      (error "Keyword :extending cannot be used for types other than %s"
             '(class trait interface)))

    (when (eq :insert intent)
      (when (and (memq type '(@variable @method @abstract-method)) (not member))
        (error "Variable and methods must be member of %s."
               phpinspect-cache-containing-types))

      (when (eq '@type type)
        (error ":as @type cannot be used for insertions.")))

    (when (and intent (not intent-param))
      (error "Intent %s must have a parameter %s" intent))

    (let ((group-specs-sym (make-symbol "group-specs"))
          (intent-param-sym (make-symbol "intent-param"))
          (member-sym (make-symbol "member"))
          (namespace-sym (make-symbol "namespace"))
          (implements-sym (make-symbol "implements"))
          (extends-sym (make-symbol "extends"))
          (resultset-sym (make-symbol "resultset"))
          (groups-sym (make-symbol "groups")))
      (let* ((action-args
              `(,(if (eq '* intent-param) `(quote ,intent-param) intent-param-sym)
                ,type
                ,(if member member-sym nil)
                ,(if namespace namespace-sym nil)
                ,(if implements implements-sym nil)
                ,(if extends extends-sym nil)))
             (validate-args
              `(,(if (eq '* intent-param) `(quote ,intent-param) intent-param-sym)
                (quote ,type)
                ,(if member member-sym nil)
                ,(if namespace namespace-sym nil)
                ,(if implements implements-sym nil)
                ,(if extends extends-sym nil)))
             (action (pcase intent
                       (:insert 'phpinspect-cache-query--do-insert)
                       (:delete 'phpinspect-cache-query--do-delete)
                       (:get 'phpinspect-cache-query--do-get)
                       (_ (error "Invalid intent %s" intent)))))
        `(let* ,(seq-filter
                 (lambda (val) val)
                 `((,group-specs-sym ,group-specs)
                   ,(unless (eq '* intent-param) `(,intent-param-sym ,intent-param))
                   ,(when member `(,member-sym ,member))
                   ,(when namespace `(,namespace-sym ,namespace))
                   ,(when implements `(,implements-sym ,implements))
                   ,(when extends `(,extends-sym ,extends))
                   (,groups-sym (phpinspect-cache-query--compile-groups ,cache ,group-specs ,intent))
                   ,resultset-sym))
           ,(cons 'phpinspect-cache-query--validate (cons intent validate-args))
           (dolist (group ,groups-sym)
             (when-let ((result ,(cons 'phpinspect-cache-query--wrap-action
                                       (cons action (cons cache (cons 'group action-args))))))
               (if (and (listp result) (eq 'phpinspect-cache-multiresult (car result)))
                   (setq ,resultset-sym (nconc ,resultset-sym (cdr result)))
                 (push result ,resultset-sym))))
           ,resultset-sym)))))

(defun phpinspect-cache-query--validate
    (intent intent-param type member namespace implements extends)
  (and
   ;; Validate intent-param
   (cond
    ((phpinspect--type-p intent-param)
     (cond
      ((not (memq type '(@class @trait @interface @type)))
       (error "Cannot use intent-param of type phpinspect--type when querying for %s" type))
      ((and (phpinspect--type-fully-qualified intent-param)
            namespace)
       (error "Use of fully qualified type %s while specifying namespace %s in query"
              intent-param namespace)))
     t)
    ((phpinspect-name-p intent-param) t)
    ((listp intent-param)
     (if (memq type '(@class @trait @interface @type))
         (unless (seq-every-p #'phpinspect--type-p intent-param)
           (error "Each intent param must be of type `phpinspect--type'. Got: %s" intent-param))
       (pcase intent
         (:insert
          (unless (or (seq-every-p #'phpinspect--variable-p intent-param)
                      (seq-every-p #'phpinspect--function-p intent-param))
            (error "Each intent param must be an instance of `phpinspect--function' or `phpinspect--variable'")))
         (:get
          (unless (seq-every-p #'phpinspect-name-p intent-param)
            (error "Each intent param must be a name, got: %s" intent-param)))
         (:delete
          (unless (or (seq-every-p #'phpinspect-name-p intent-param))
            (error "Each intent param must be an instance of `phpinspect--function', `phpinspect--variable' or `phpinspect-name'")))))
     t)
    ((eq '* intent-param)
     (unless (memq intent '(:get :delete))
       (error "Wildcard '* cannot be used with intent %s" intent)))
    ((phpinspect--function-p intent-param)
     (cond
      ((not (memq intent '(:insert :delete)))
       (error "`phpinspect--function' can only be used as parameter for :insert and :delete intents."))
      ((not (memq type '(@function @method @abstract-method)))
       (error "Inserting/deleting `phpinspect--function' as type %s is not supported" type)))
     t)
    (t (error "Unsupported intent-param %s" intent-param)))
   ;; Validate member
   (cond
    ((phpinspect--type-p member) t)
    ((not member) t)
    (t (error "unsupported member type (allowed `phpinspect--type'): %s" member)))

   ;; Validate namespace
   (cond
    ((phpinspect-name-p namespace) t)
    ((not namespace) t)
    (t (error "unsupported namespace type (allowed `phpinspect-name'): %s" namespace)))

   ;; Validate implements
   (cond
    ((listp implements)
     (unless (seq-every-p #'phpinspect--type-p implements)
       (error "Each parameter of :implementing must be of type `phpinspect--type'. Got: %s" implements))
     t)
    ((phpinspect--type-p implements) t)
    ((not implements) t)
    (t (error "unsupported parameter for :implementing (allowed `phpinspect--type': %s" implements)))

   ;; Validate extends
   (cond
    ((listp extends)
     (unless (seq-every-p #'phpinspect--type-or-name-p extends)
       (error "Each parameter of :extending must be of type `phpinspect--type'. Got: %s" extends))
     t)
    ((phpinspect--type-p extends) t)
    ((not extends) t)
    (t (error "unsupported parameter for :extending (allowed `phpinspect--type': %s" extends)))))

;;; phpinspect.el ends here
(provide 'phpinspect-cache)
