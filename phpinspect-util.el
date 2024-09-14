;;; phpinspect-util.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(defvar phpinspect-names (make-hash-table :test #'equal :size 5000 :rehash-size 1.2)
  "An hash-table containing cons cells representing encountered names in
PHP code. Used to optimize string comparison. See also `phpinspect-intern-name'")

(defun phpinspect-make-name-hash ()
  (make-hash-table :test #'equal :size 5000 :rehash-size 1.2))

(define-inline phpinspect-name-string (name)
  (inline-quote (cdr ,name)))

(define-inline phpinspect-name-p (name)
  (inline-quote (eq 'phpinspect-name (car ,name))))

(defvar phpinspect-project-root-file-list
  '("composer.json" "composer.lock" ".git" ".svn" ".hg")
  "List of files that could indicate a project root directory.")

(defvar phpinspect--debug nil
  "Enable debug logs for phpinspect by setting this variable to true")

(defun phpinspect-message (&rest args)
  (let ((format-string (car args))
        (args (cdr args)))
    (apply #'message `(,(concat "[phpinspect] " format-string) ,@args))))

(defun phpinspect-toggle-logging ()
  (interactive)
  (if (setq phpinspect--debug (not phpinspect--debug))
      (phpinspect-message "Enabled phpinspect logging.")
    (phpinspect-message "Disabled phpinspect logging.")))

(eval-and-compile
  (defvar phpinspect-log-groups nil)
  (defvar phpinspect-enabled-log-groups nil)
  (defvar-local phpinspect--current-log-group nil))

(define-inline phpinspect--declare-log-group (group)
  (unless (and (inline-const-p group) (symbolp (inline-const-val group)))
    (inline-error "Log group name should be a symbol"))

  (inline-quote
   (progn
     (add-to-list 'phpinspect-log-groups
                  (cons (macroexp-file-name) ,group)))))

(defun phpinspect-log-group-enabled-p (group)
  (seq-find (lambda (cons)
              (eq group (cdr cons)))
            phpinspect-enabled-log-groups))

(defmacro phpinspect--log (&rest args)
  (let ((log-group (alist-get (macroexp-file-name)
                              phpinspect-log-groups nil nil #'string=)))
    `(when (and phpinspect--debug
                (or (not phpinspect-enabled-log-groups)
                    ,(when log-group
                       `(member (quote ,log-group) phpinspect-enabled-log-groups))))
       (with-current-buffer (get-buffer-create "**phpinspect-logs**")
         (unless window-point-insertion-type
           (set (make-local-variable 'window-point-insertion-type) t))
         (goto-char (buffer-end 1))
         (insert (concat "[" (format-time-string "%H:%M:%S") "]: "
                         ,(if log-group (concat "(" (symbol-name log-group) ") ") "")
                         (format ,@args) "\n"))))))

(defun phpinspect-filter-logs (group-name)
  (interactive (list (completing-read "Log group: "
                                      (mapcar (lambda (g) (symbol-name (cdr g)))
                                              phpinspect-log-groups)
                                      nil t)))
  (add-to-list 'phpinspect-enabled-log-groups (intern group-name)))

(defun phpinspect-unfilter-logs ()
  (interactive)
  (setq phpinspect-enabled-log-groups nil))

(defun phpinspect--find-project-root (&optional start-file)
  "(Attempt to) Find the root directory of the visited PHP project.
If a found project root has a parent directory called \"vendor\",
the search continues upwards. See also
`phpinspect--locate-dominating-project-file'.

If START-FILE is provided, searching starts at the directory
level of START-FILE in stead of `default-directory`."
  (let ((project-file (phpinspect--locate-dominating-project-file
                       (or start-file default-directory))))
    (phpinspect--log "Checking for project root at  %s" project-file)
    (when project-file
      (let* ((directory (file-name-directory project-file))
             (directory-slugs (split-string (expand-file-name directory) "/")))
        (if (not (member "vendor" directory-slugs))
            (expand-file-name directory)
          ;; else. Only continue if the parent directory is not "/"
          (let ((parent-without-vendor
                 (string-join (seq-take-while (lambda (s) (not (string= s "vendor" )))
                                              directory-slugs)
                              "/")))
            (when (not (or (string= parent-without-vendor "/")
                           (string= parent-without-vendor "")))
              (phpinspect--find-project-root parent-without-vendor))))))))

(defun phpinspect-intern-name (name)
  (or (gethash name phpinspect-names)
      (puthash name (cons 'phpinspect-name name) phpinspect-names)))

(defun phpinspect-names-to-alist (names)
  (let ((alist))
    (dolist (name names)
      (push (cons (phpinspect-name-string name) name) alist))
    alist))

(defsubst phpinspect--wrap-plist-name-in-symbol (property-list)
  (let ((new-plist)
        (wrap-value))
    (dolist (item property-list)
      (when wrap-value
        (setq item `(phpinspect-intern-name ,item))
        (setq wrap-value nil))
      (when (eq item :name)
        (setq item :name-symbol)
        (setq wrap-value t))
      (push item new-plist))
    (nreverse new-plist)))

(cl-defstruct (phpinspect--pattern
               (:constructor phpinspect--make-pattern-generated))
  "An object that can be used to match lists to a given
pattern. See `phpinspect--match-sequence'."
  (matcher nil
           :type lambda
           :documentation "The function used to match sequences")
  (code nil
        :type list
        :documentation "The original code list used to create this pattern"))

(defmacro phpinspect--make-pattern (&rest pattern)
  `(phpinspect--make-pattern-generated
    :matcher (phpinspect--match-sequence-lambda ,@pattern)
    :code  (list ,@(mapcar (lambda (part) (if (eq '* part) `(quote ,part) part))
                           pattern))))

(defun phpinspect--pattern-length (pattern)
  (/ (length (phpinspect--pattern-code pattern)) 2))

(defmacro phpinspect--match-sequence-lambda (&rest pattern)
  (let ((sequence-sym (gensym)))
    `(lambda (,sequence-sym)
       (phpinspect--match-sequence ,sequence-sym ,@pattern))))

(cl-defmethod phpinspect--pattern-match ((pattern phpinspect--pattern) sequence)
  "Match SEQUENCE to PATTERN."
  (funcall (phpinspect--pattern-matcher pattern) sequence))

(defun phpinspect--list-all-equal (val sequence)
  (catch 'not-equal
    (dolist (item sequence)
      (unless (equal val item)
        (throw 'not-equal nil)))
    t))

(defmacro phpinspect--match-sequence (sequence &rest pattern)
  "Match SEQUENCE to positional matchers defined in PATTERN.

PATTERN is a plist with the allowed keys being :m and :f. Each
key-value pair in the plist defines a match operation that is
applied to the corresponding index of SEQUENCE (so for ex.: key 0
is applied to pos. 0 of SEQUENCE, key 1 to pos. 1, and so on).

Possible match operations:

:m - This key can be used to match a list element to the literal
value supplied for it, using the `equal' comparison function. For
example, providing `(\"foobar\") as value will result in the
comparison (equal (elt SEQUENCE pos) `(\"foobar\")). There is one
exception to this rule: using the symbol * as value for the :m
key will match anything, essentially skipping comparison for the
element at this position in SEQUENCE.

:f - This key can be used to match a list element by executing
the function provided as value. The function is executed with the
list element as argument, and will be considered as matching if
it evaluates to a non-nil value."
  (declare (indent 1))
  (let* ((pattern-length (length pattern))
         (sequence-length (/ pattern-length 2))
         (sequence-pos 0)
         (sequence-sym (gensym))
         (match-sym (gensym))
         (match-rear-sym (gensym))
         (checkers (cons nil nil))
         (checkers-rear checkers)
         rest key value)

    (while (setq key (pop pattern))
      (unless (keywordp key)
        (error "Invalid pattern argument, expected keyword, got: %s" key))

      (unless (setq value (pop pattern))
        (error "No value for key %s" key))

      (cond ((eq key :m)
             (unless (eq value '*)
               (setq checkers-rear
                     (setcdr checkers-rear
                             (cons `(equal ,value (elt ,sequence-sym ,sequence-pos)) nil)))))
            ((eq key :f)
             (setq checkers-rear
                   (setcdr
                    checkers-rear
                    (cons
                     (if (symbolp value)
                         `(,value (elt ,sequence-sym ,sequence-pos))
                       `(funcall ,value (elt ,sequence-sym ,sequence-pos)))
                     nil))))
            ((eq key :rest)
             (setq rest value))
            (t (error "Invalid keyword: %s" key)))

      (setq checkers-rear
            (setcdr checkers-rear
                    (cons `(setq ,match-rear-sym
                                 (setcdr ,match-rear-sym
                                         (cons (elt ,sequence-sym ,sequence-pos) nil)))
                          nil)))

      (setq sequence-pos (+ sequence-pos 1)))

    (setq checkers (cdr checkers))

    `(let* ((,sequence-sym ,sequence)
            (,match-sym (cons nil nil))
            (,match-rear-sym ,match-sym))
       ,(if rest
            `(and ,@checkers
                  (cdr ,match-sym)
                  ,(if (eq rest '*)
                       't
                     `(phpinspect--list-all-equal ,rest (nthcdr ,sequence-length ,sequence-sym))))
          `(and (= ,sequence-length (length ,sequence))
                ,@checkers
                (cdr ,match-sym))))))

(defun phpinspect--pattern-concat (pattern1 pattern2)
  (let* ((pattern1-sequence-length (/ (length (phpinspect--pattern-code pattern1)) 2)))
    (phpinspect--make-pattern-generated
     :matcher (lambda (sequence)
                (unless (< (length sequence) pattern1-sequence-length)
                  (and (phpinspect--pattern-match
                        pattern1
                        (butlast sequence (- (length sequence) pattern1-sequence-length)))
                       (phpinspect--pattern-match
                        pattern2
                        (last sequence (- (length sequence) pattern1-sequence-length))))))
     :code (append (phpinspect--pattern-code pattern1)
                   (phpinspect--pattern-code pattern2)))))

(defun phpinspect--locate-dominating-project-file (start-file)
  "Locate the first dominating file in `phpinspect-project-root-file-list`.
Starts looking at START-FILE and then recurses up the directory
hierarchy as long as no matching files are found.  See also
`locate-dominating-file'."
  (let ((dominating-file))
    (seq-find (lambda (file)
                (setq dominating-file (locate-dominating-file start-file file)))
              phpinspect-project-root-file-list)
    dominating-file))

(defun phpinspect--determine-completion-point ()
  "Find first point backwards that could contain any kind of
context for completion."
  (save-excursion
    (re-search-backward "[^[:blank:]\n]" nil t)
    (forward-char)
    (point)))

(defmacro phpinspect-json-preset (&rest body)
  "Default options to wrap around `json-read' and similar BODY."
  `(let ((json-object-type 'hash-table)
	     (json-array-type 'list)
	     (json-key-type 'string))
     ,@body))

(defun phpinspect--input-pending-p (&optional check-timers)
  (unless noninteractive
    (input-pending-p check-timers)))

(defun phpinspect-thread-pause (pause-time mx continue)
  "Pause current thread using MX and CONTINUE for PAUSE-TIME idle seconds.

PAUSE-TIME must be the idle time that the thread should pause for.
MX must be a mutex
CONTINUE must be a condition-variable"
  (phpinspect--log "Thread '%s' is paused for %d seconds" (thread-name (current-thread)) pause-time)
  (run-with-idle-timer
   pause-time
   nil
   (lambda () (with-mutex mx (condition-notify continue))))
  (with-mutex mx (condition-wait continue))
  (phpinspect--log "Thread '%s' continuing execution" (thread-name (current-thread))))

(provide 'phpinspect-util)
;;; phpinspect-util.el ends here
