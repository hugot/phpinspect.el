;;; phpinspect-util.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

(defvar phpinspect-name-obarray (obarray-make)
  "An obarray containing symbols for all encountered names in
PHP. Used to optimize string comparison.")

(defvar phpinspect--debug nil
  "Enable debug logs for phpinspect by setting this variable to true")

(defsubst phpinspect-intern-name (name)
  (intern name phpinspect-name-obarray))

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

(defun phpinspect-toggle-logging ()
  (interactive)
  (if (setq phpinspect--debug (not phpinspect--debug))
      (message "Enabled phpinspect logging.")
    (message "Disabled phpinspect logging.")))

(defsubst phpinspect--log (&rest args)
  (when phpinspect--debug
    (with-current-buffer (get-buffer-create "**phpinspect-logs**")
      (unless window-point-insertion-type
        (set (make-local-variable 'window-point-insertion-type) t))
      (goto-char (buffer-end 1))
      (insert (concat "[" (format-time-string "%H:%M:%S") "]: "
                      (apply #'format args) "\n")))))

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

(defsubst phpinspect--make-pattern (&rest pattern)
  (phpinspect--make-pattern-generated
   :matcher (apply #'phpinspect--match-sequence-lambda pattern)
   :code pattern))

(defun phpinspect--match-sequence-lambda (&rest pattern)
  (lambda (sequence)
    (apply #'phpinspect--match-sequence sequence pattern)))

(cl-defmethod phpinspect--pattern-match ((pattern phpinspect--pattern) sequence)
  "Match SEQUENCE to PATTERN."
  (funcall (phpinspect--pattern-matcher pattern) sequence))

(defun phpinspect--match-sequence (sequence &rest pattern)
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
  (let* ((pattern-length (length pattern))
         (count 0)
         (sequence-pos 0)
         (sequence-length (/ pattern-length 2)))

    (and (= sequence-length (length sequence))
         (catch 'found
           (while (< count pattern-length)
             (let ((key (elt pattern count))
                   (value (elt pattern (+ count 1))))
               (unless (keywordp key)
                 (error (format "Invalid, expected keyword, got %s" key)))

               (cond ((eq key :m)
                      (unless (eq value '*)
                        (unless (equal value (elt sequence sequence-pos))
                          (throw 'found nil))))
                     ((eq key :f)
                      (unless (funcall value (elt sequence sequence-pos))
                        (throw 'found nil)))
                     (t (error (format "Invalid keyword: %s" key))))
               (setq count (+ count 2)
                     sequence-pos (+ sequence-pos 1))))
           (throw 'found t)))))

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

(provide 'phpinspect-util)
;;; phpinspect-util.el ends here
