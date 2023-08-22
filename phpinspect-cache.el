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

;;; Code:

(require 'phpinspect-project)
(require 'phpinspect-autoload)
(require 'phpinspect-worker)

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
  (phpinspect--index-tokens (phpinspect-parse-file (expand-file-name "builtins.php" phpinspect-stub-directory))))

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

(cl-defstruct (phpinspect-cache (:constructor phpinspect-make-cache))
  (groups (make-hash-table :test #'equal :size 2000 :rehash-size 1.2)))

(eval-and-compile
  (defconst phpinspect-cache-types '(interface trait class method
                                               abstract-method function variable namespace)
    "Types of entities that the cache can store.")

  (defconst phpinspect-cache-containing-types '(class trait interface)
    "Types of entities that the cache can store members for.")

  (defconst phpinspect-cache-member-types '(method abstract-method function variable)
    "Types of entities that the cache can store as members."))

(define-inline phpinspect-cache-query--do-delete (&rest args)
  (inline-quote
   (message ":delete Args: %s" ,(cons 'list args))))

(define-inline phpinspect-cache-query--do-insert (&rest args)
  (inline-quote
   (message ":insert Args: %s" ,(cons 'list args))))

(define-inline phpinspect-cache-query--do-get (&rest args)
  (inline-quote
   (message ":get Args: %s" ,(cons 'list args))))

(cl-defstruct (phpinspect-cache-group (:constructor phpinspect-make-cache-group)))

(defun phpinspect-cache-query--compile-groups (cache group-specs intent)
  (cl-assert (phpinspect-cache-p cache))
  (let (groups)

    (if group-specs
        (dolist (spec group-specs)
          (cl-assert (listp spec))
          (cl-assert (memq (car spec) '(project label))
                     t "Spec car must be the symbol `project' or `label'")
          (let ((group (gethash spec (phpinspect-cache-groups cache))))
            (when (and (eq :insert intent) (not group))
              (setq group (puthash spec (phpinspect-make-cache-group) (phpinspect-cache-groups cache))))
            (push group groups)))
      (if (eq :insert intent)
          (error "Cannot insert without defining cache group")
        (setq groups (hash-table-values (phpinspect-cache-groups cache)))))

    groups))

(define-inline phpinspect-cache-transact (cache group-specs &rest query)
  (declare (indent 2))
  (cl-assert (listp query))

  (let (type intent intent-param member namespace implements extends key value)
    (condition-case err
        (progn
          (while (setq key (pop query))
            (cl-assert (keywordp key) t "Query keys must be keywords, %s provided" key)
            (setq value (pop query))
            (cl-assert value t "Key %s has no value" key)

            (pcase key
              ((or :insert :get :delete)
               (when intent
                 (inline-error "Defined duplicate intent: %s, %s" intent key))
               (setq intent key
                     intent-param value))
              (:as (cl-assert (and (inline-const-p value)
                                   (memq (inline-const-val value) phpinspect-cache-types))
                              t ":type must be one of %s" phpinspect-cache-types)
                   (setq type (inline-const-val value)))
              (:member-of (setq member value))
              (:in (setq namespace value))
              (:implementing (setq implements value))
              (:extending (setq extends value))
              (_ (error "Unexpected query keyword %s" key))))

          ;; Query validation
          (unless type
            (error "Providing entity type with keyword :as is required."))

          (when (and member (not (memq type phpinspect-cache-member-types)))
            (error "Keyword :member-of can only be usd for types %s" phpinspect-cache-member-types))

          (when (and extends (not (memq type '(class trait interface))))
            (error "Keyword :extending cannot be used for types other than %s" '(class trait interface)))

          (when (and (eq 'variable type) (not member))
            (error "Variables outside of classes cannot be stored in the cache."))

          (when (and intent (not intent-param))
            (error "Intent %s must have a parameter %s" intent)))
      (t (inline-error "phpinspect-cache-transact: %s" err)))


    (inline-letevals (group-specs intent-param cache type member namespace implements extends)
      (let ((action-args `(,intent-param (quote ,type) ,member ,namespace ,implements ,extends))
            (action (pcase intent
                      (:insert 'phpinspect-cache-query--do-insert)
                      (:delete 'phpinspect-cache-query--do-delete)
                      (:get 'phpinspect-cache-query--do-get)
                      (_ (inline-error "Invalid intent %s" intent)))))

        (message "action: %s" action)

        (inline-quote
         (let ((groups (phpinspect-cache-query--compile-groups ,cache ,group-specs ,intent)))
           ,(cons 'phpinspect-cache-query--validate (cons intent action-args))
           (dolist (group groups)
             ,(cons action (cons 'group action-args)))))))))

(defun phpinspect-cache-query--validate (intent intent-param type member namespace implements extends)
  (and
   ;; Validate intent-param
   (cond
    ((phpinspect--type-p intent-param)
     (cond
      ((not (memq type '(class trait interface)))
       (error "Cannot use intent-param of type phpinspect--type when querying for %s" type))
      ((and (phpinspect--type-fully-qualified intent-param)
            namespace)
       (error "Use of fully qualified type %s while specifying namespace %s in query"
              intent-param namespace)))
     t)
    ((phpinspect-name-p intent-param) t)
    ((phpinspect--function-p intent-param)
     (cond
      ((not (memq intent '(:insert :delete)))
       (error "`phpinspect--function' can only be used as parameter for :insert and :delete intents."))
      ((not (memq type '(function method abstract-method)))
       (error "Inserting/deleting `phpinspect--function' as type %s is not supported" type)))
     t)
    (t (error "Unsupported intent-param %s" intent-param)))
   ;; Validate member
   (cond
    ((phpinspect-name-p member) t)
    ((phpinspect--type-p member) t)
    ((not member) t)
    (t (error "unsupported member type (allowed `phpinspect-name' and `phpinspect--type'): %s" member)))

   ;; Validate namespace
   (cond
    ((stringp namespace) t)
    ((phpinspect-name-p namespace) t)
    ((not namespace) t)
    (t (error "unsupported namespace type (allowed `phpinspect-name' and `stringp'): %s" namespace)))

   ;; Validate implements
   (cond
    ((listp implements)
     (unless (seq-every-p #'phpinspect--type-or-name-p implements)
       (error "Each parameter of :implementing must be of type `phpinspect--type' or `phpinspect-name'. Got: %s" implements))
     t)
    ((phpinspect-name-p implements) t)
    ((phpinspect--type-p implements) t)
    ((not implements) t)
    (t (error "unsupported parameter for :implementing (allowed `phpinspect--type' or `phpinspect-name': %s" implements)))

   ;; Validate extends
   (cond
    ((listp extends)
     (unless (seq-every-p #'phpinspect--type-or-name-p extends)
       (error "Each parameter of :extending must be of type `phpinspect--type' or `phpinspect-name'. Got: %s" extends))
     t)
    ((phpinspect-name-p extends) t)
    ((phpinspect--type-p extends) t)
    ((not extends) t)
    (t (error "unsupported parameter for :extending (allowed `phpinspect--type' or `phpinspect-name': %s" extends)))))


;;; phpinspect.el ends here
(provide 'phpinspect-cache)
