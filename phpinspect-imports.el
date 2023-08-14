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
(require 'phpinspect-cache)
(require 'phpinspect-util)

(defun phpinspect-insert-at-point (point data)
  (save-excursion
    (goto-char point)
    (insert data)))

(defun phpinspect-find-first-use (token-meta)
  (if (and (phpinspect-namespace-p (phpinspect-meta-token token-meta))
           (phpinspect-namespace-is-blocked-p (phpinspect-meta-token token-meta)))
      (phpinspect-find-first-use (phpinspect-meta-last-child token-meta))
    (phpinspect-meta-find-first-child-matching
     token-meta (phpinspect-meta-wrap-token-pred #'phpinspect-use-p))))

(defun phpinspect-add-use (fqn buffer &optional namespace-meta)
  "Add use statement for FQN to BUFFER.

If NAMESPACE-TOKEN is non-nil, it is assumed to be a token that
was parsed from BUFFER and its location will be used to find a
buffer position to insert the use statement at."
  (when (string-match "^\\\\" fqn)
    (setq fqn (string-trim-left fqn "\\\\")))

  (if namespace-meta
      (let* ((namespace-block (and (phpinspect-namespace-is-blocked-p
                                    (phpinspect-meta-token namespace-meta))
                                   (phpinspect-meta-last-child namespace-meta)))
             (existing-use (phpinspect-find-first-use namespace-meta)))
        (if existing-use
            (phpinspect-insert-at-point
             (phpinspect-meta-start existing-use) (format "use %s;%c" fqn ?\n))
          (if namespace-block
              (phpinspect-insert-at-point
               (+ 1 (phpinspect-meta-start namespace-block))
               (format "%c%cuse %s;%c" ?\n ?\n fqn ?\n))
            (phpinspect-insert-at-point
             (phpinspect-meta-end
              (phpinspect-meta-find-first-child-matching
               namespace-meta (phpinspect-meta-wrap-token-pred #'phpinspect-terminator-p)))
             (format "%c%cuse %s;%c" ?\n ?\n fqn ?\n)))))
    ;; else
    (let ((existing-use (phpinspect-meta-find-first-child-matching
                         (phpinspect-buffer-root-meta buffer)
                        (phpinspect-meta-wrap-token-pred #'phpinspect-use-p))))
      (if existing-use
          (phpinspect-insert-at-point
           (phpinspect-meta-start existing-use)
           (format "use %s;%c" fqn ?\n))
        (let* ((first-token (phpinspect-meta-first-child (phpinspect-buffer-root-meta buffer)))
               token-after)
          (message "First token %s" (phpinspect-meta-string first-token))
          (when (and (phpinspect-word-p (phpinspect-meta-token first-token))
                     (string= "declare" (cadr (phpinspect-meta-token first-token))))
            (progn
              (setq token-after first-token)
              (while (and token-after (not (phpinspect-terminator-p
                                            (phpinspect-meta-token token-after))))
                (setq token-after (phpinspect-meta-find-right-sibling token-after))
                (message "Token after: %s" (phpinspect-meta-string token-after)))))
          (if token-after
              (phpinspect-insert-at-point
               (phpinspect-meta-end token-after) (format "%c%cuse %s;%c" ?\n ?\n fqn ?\n))
            (phpinspect-insert-at-point
             (phpinspect-meta-start first-token)
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

(defun phpinspect-add-use-statements-for-missing-types (types buffer imports project parent-token)
  (let (namespace namespace-name)
    (dolist (type types)
      (setq namespace (phpinspect-meta-find-parent-matching-token
                       parent-token #'phpinspect-namespace-p)
            namespace-name (phpinspect-namespace-name namespace))
      ;; Add use statements for types that aren't imported or already referenced
      ;; with a fully qualified name.
      (unless (or (or (alist-get type imports))
                  (gethash (phpinspect-intern-name
                            (concat namespace-name "\\" (symbol-name type)))
                           (phpinspect-autoloader-types
                            (phpinspect-project-autoload project))))
        (phpinspect-add-use-interactive type buffer project namespace)
        (phpinspect-buffer-parse buffer 'no-interrupt)))))

(defun phpinspect-fix-imports ()
  "Find types that are used in the current buffer and make sure
that there are import (\"use\") statements for them."
  (interactive)
  (if phpinspect-current-buffer
      (let* ((buffer phpinspect-current-buffer)
             (tree (phpinspect-buffer-parse buffer))
             (index (phpinspect--index-tokens
                     tree nil (phpinspect-buffer-location-resolver buffer)))
             (classes (alist-get 'classes index))
             (imports (alist-get 'imports index))
             (project (phpinspect--cache-get-project-create
                       (phpinspect--get-or-create-global-cache)
                       (phpinspect-current-project-root)))
             (used-types (alist-get 'used-types index)))

        (phpinspect-add-use-statements-for-missing-types
         used-types buffer imports project (phpinspect-buffer-root-meta buffer))

        (dolist (class classes)
          (let* ((class-imports (alist-get 'imports class))
                 (used-types (alist-get 'used-types class))
                 (class-name (alist-get 'class-name class))
                 (region (alist-get 'location class))
                 token-meta)
            (message "Region: %s" region)
            (message "index: %s" index)
            (setq token-meta (phpinspect-meta-find-parent-matching-token
                              (phpinspect-bmap-last-token-before-point
                               (phpinspect-buffer-map buffer)
                               (+ (phpinspect-region-start region) 1))
                              #'phpinspect-class-p))
            (unless token-meta
              (error "Unable to find token for class %s" class-name))

            (phpinspect-add-use-statements-for-missing-types
             used-types buffer (append imports class-imports) project token-meta))))))

(provide 'phpinspect-imports)
