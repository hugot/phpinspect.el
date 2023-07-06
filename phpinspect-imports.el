; phpinspect-imports.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

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

;; See docstrings for documentation, starting with `phpinspect-mode'.

;;; Code:

(require 'phpinspect-parser)
(require 'phpinspect-index)
(require 'phpinspect-autoload)
(require 'phpinspect-buffer)

(defun phpinspect-insert-at-point (point data)
  (save-excursion
    (goto-char point)
    (insert data)))

(defun phpinspect-add-use (fqn buffer &optional namespace-token)
  "Add use statement for FQN to BUFFER.

If NAMESPACE-TOKEN is non-nil, it is assumed to be a token that
was parsed from BUFFER and its location will be used to find a
buffer position to insert the use statement at."
  (when (string-match "^\\\\" fqn)
    (setq fqn (string-trim-left fqn "\\\\")))

  (if namespace-token
      (let* ((meta (phpinspect-bmap-token-meta
                    (phpinspect-buffer-map buffer) namespace-token))
             (existing-use (seq-find #'phpinspect-use-p
                                     (phpinspect-namespace-body namespace-token)))
             (namespace-block (phpinspect-namespace-block namespace-token)))
        (if existing-use
            (phpinspect-insert-at-point
             (phpinspect-meta-start
              (phpinspect-buffer-token-meta buffer existing-use))
             (format "use %s;%c" fqn ?\n))
          (if namespace-block
              (phpinspect-insert-at-point
               (+ 1 (phpinspect-meta-start
                     (phpinspect-buffer-token-meta buffer namespace-block)))
               (format "%c%cuse %s;%c" ?\n ?\n fqn ?\n))
            (phpinspect-insert-at-point
             (phpinspect-meta-end
                   (phpinspect-buffer-token-meta
                    buffer (seq-find #'phpinspect-terminator-p namespace-token)))
             (format "%c%cuse %s;%c" ?\n ?\n fqn ?\n)))))
    ;; else
    (let ((existing-use (seq-find #'phpinspect-use-p
                                  (phpinspect-buffer-tree buffer))))
      (if existing-use
          (phpinspect-insert-at-point
           (phpinspect-meta-start
            (phpinspect-buffer-token-meta buffer existing-use))
           (format "use %s;%c" fqn ?\n))
        (let ((first-token (cadr (phpinspect-buffer-tree buffer))))
          (if (and (phpinspect-word-p first-token)
                   (string= "declare" (cadr first-token)))
              (phpinspect-insert-at-point
               (phpinspect-meta-end
                (phpinspect-buffer-token-meta
                 buffer (seq-find #'phpinspect-terminator-p (phpinspect-buffer-tree buffer))))
                (format "%c%cuse %s;%c" ?\n ?\n fqn ?\n))
            (phpinspect-insert-at-point
             (phpinspect-meta-start
              (phpinspect-buffer-token-meta buffer first-token))
             (format "%c%cuse %s;%c%c" ?\n ?\n fqn ?\n ?\n))))))))

(defun phpinspect-add-use-interactive (typename buffer project &optional namespace-token)
  (let* ((autoloader (phpinspect-project-autoload project))
         (fqn-bags (phpinspect-autoloader-type-name-fqn-bags autoloader)))

    (let ((fqns (gethash typename fqn-bags)))
      (cond ((= 1 (length fqns))
             (phpinspect-add-use (symbol-name (car fqns)) buffer namespace-token))
            ((> (length fqns) 1)
             (phpinspect-add-use (completing-read "Class: " fqns)
                                 buffer namespace-token))
            (t (message "No import found for type %s" typename))))))

(defun phpinspect-namespace-part-of-typename (typename)
  (string-trim-right typename "\\\\?[^\\\\]+"))

(defalias 'phpinspect-fix-uses-interactive #'phpinspect-fix-imports
  "Alias for backwards compatibility")

(defun phpinspect-fix-imports ()
  "Find types that are used in the current buffer and make sure
that there are import (\"use\") statements for them."
  (interactive)
  (if phpinspect-current-buffer
      (let* ((tree (phpinspect-buffer-parse phpinspect-current-buffer))
             (index (phpinspect--index-tokens
                     tree nil (phpinspect-buffer-location-resolver
                               phpinspect-current-buffer)))
             (classes (alist-get 'classes index))
             (imports (alist-get 'imports index))
             (project (phpinspect--cache-get-project-create
                       (phpinspect--get-or-create-global-cache)
                       (phpinspect-current-project-root))))
        (dolist (class classes)
          (let* ((class-imports (alist-get 'imports class))
                 (used-types (alist-get 'used-types class))
                 (class-name (alist-get 'class-name class))
                 (region))
            (dolist (type used-types)
              ;; Retrieve latest version of class location data changes with
              ;; each added use statement + reindex.
              (setq region
                    (alist-get 'location
                               (phpinspect-index-get-class
                                index class-name)))



              (let ((namespace
                     (seq-find #'phpinspect-namespace-p
                               (phpinspect-buffer-tokens-enclosing-point
                                phpinspect-current-buffer (phpinspect-meta-start meta)))))
                ;; Add use statements for types that aren't imported.

                (unless (or (or (alist-get type class-imports)
                                (alist-get type imports))
                            (gethash (phpinspect-intern-name
                                      (concat (phpinspect-namespace-part-of-typename
                                               (phpinspect--type-name (alist-get 'class-name class)))
                                              "\\"
                                              (symbol-name type)))
                                     (phpinspect-autoloader-types
                                      (phpinspect-project-autoload project))))
                  (phpinspect-add-use-interactive
                   type phpinspect-current-buffer project namespace)
                  ;; Buffer has been modified by adding type, update tree +
                  ;; location map. This is not optimal but will have to do until
                  ;; partial parsing is implemented.
                  ;;
                  ;; Note: this basically implements a bug where the locations
                  ;; of classes are no longer congruent with their location in
                  ;; the buffer's code. In files that contain multiple namespace
                  ;; blocks this could cause problems as a namespace may grow by
                  ;; added import statements and start envelopping the classes
                  ;; below it.
                  (setq index
                        (phpinspect--index-tokens
                         (phpinspect-buffer-parse phpinspect-current-buffer)
                         nil
                         (phpinspect-buffer-location-resolver
                              phpinspect-current-buffer)))))))))))

(provide 'phpinspect-imports)
