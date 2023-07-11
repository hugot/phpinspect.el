;; test-buffer.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Hugo Thunnissen <devel@hugot.nl>

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

;;

;;; Code:

(require 'ert)
(require 'phpinspect-parser)
(require 'phpinspect-buffer)

(ert-deftest phpinspect-buffer-region-lookups ()
  (let* ((parsed)
         (class))
    (with-temp-buffer
      (insert-file-contents (concat phpinspect-test-php-file-directory "/NamespacedClass.php"))
      (setq phpinspect-current-buffer
            (phpinspect-make-buffer :buffer (current-buffer)))
      (setq parsed (phpinspect-buffer-parse phpinspect-current-buffer))

      (let* ((class (seq-find #'phpinspect-class-p
                              (seq-find #'phpinspect-namespace-p parsed)))
             (classname (car (cddadr class))))

        ;; Root node should be the root parsed token
        (should (eq parsed (phpinspect-meta-token
                            (phpinspect-tree-value (phpinspect-buffer-tree
                                                    phpinspect-current-buffer)))))

        (let ((tokens (phpinspect-buffer-tokens-enclosing-point
                       phpinspect-current-buffer 617)))
          (should (eq classname
                      (phpinspect-meta-token (car tokens))))
          (should (phpinspect-declaration-p (phpinspect-meta-token (cadr tokens))))
          (should (eq class (phpinspect-meta-token (caddr tokens)))))))))


(ert-deftest phpinspect-parse-buffer-no-current ()
  "Confirm that the parser is still functional with
`phpinspect-current-buffer' unset."
  (let*((buffer)
        (parsed))
    (with-temp-buffer
      (should-not phpinspect-current-buffer)
      (insert-file-contents (concat phpinspect-test-php-file-directory "/NamespacedClass.php"))
      (setq parsed (phpinspect-parse-current-buffer)))

    (should (cdr parsed))))


(ert-deftest phpinspect-buffer-register-edit ()
  (let ((buffer (phpinspect-make-buffer)))
    (with-temp-buffer
      (insert-file-contents (concat phpinspect-test-php-file-directory "/NamespacedClass.php"))
      (setq phpinspect-current-buffer buffer)
      (setf (phpinspect-buffer-buffer buffer) (current-buffer))
      (phpinspect-buffer-parse buffer))

    ;; "Deletes" first curly brace of __construct function block
    (phpinspect-buffer-register-edit buffer 1036 1036 1)

    (let* ((region (phpinspect-make-region 1036 1037))
           (tainted
            (phpinspect-tree-find-smallest-overlapping-set
             (phpinspect-buffer-tree buffer) region)))
      (dolist (meta tainted)
        (should (phpinspect-meta-tainted meta))
        (phpinspect-tree-traverse (node (phpinspect-meta-tree meta))
          (when (phpinspect-tree-overlaps node region)
            (should (phpinspect-meta-tainted (phpinspect-tree-value node)))))))))

(cl-defstruct (phpinspect-document (:constructor phpinspect-make-document))
  (buffer (get-buffer-create
                  (generate-new-buffer-name " **phpinspect-document** shadow buffer") t)
                 :type buffer
                 :documentation
                 "A hidden buffer with a reference version of the document."))

(cl-defmethod phpinspect-document-apply-edit
  ((document phpinspect-document) start end delta contents)
  (with-current-buffer (phpinspect-document-buffer document)
      (goto-char start)
      (delete-region (point) (- end delta))
      (insert contents)))

(cl-defmethod phpinspect-document-set-contents
  ((document phpinspect-document) (contents string))
  (with-current-buffer (phpinspect-document-buffer document)
    (erase-buffer)
    (insert contents)))

(cl-defmethod phpinspect-document-contents ((document phpinspect-document))
  (with-current-buffer (phpinspect-document-buffer document)
    (buffer-string)))

(ert-deftest phpinspect-buffer-parse-incrementally ()
  (let* ((document (phpinspect-make-document))
         (buffer (phpinspect-make-buffer
                  :buffer (phpinspect-document-buffer document)))
         (parsed))
    ;; TODO: write tests for more complicated cases (multiple edits, etc.)
    (phpinspect-document-set-contents document "<?php function Hello() { echo 'Hello World!'; if ($name) { echo 'Hello ' . $name . '!';} }")

    (setq parsed (phpinspect-buffer-parse buffer))
    (should parsed)

    (let ((hello (car (phpinspect-buffer-tokens-enclosing-point buffer 18)))
          (hello1)
          (hello2))
      (should (equal '(:word "Hello") (phpinspect-meta-token hello)))
      (should parsed)

      ;; Delete function block opening brace
      (phpinspect-document-apply-edit document 24 24 -1 "")
      (should (string= "<?php function Hello()  echo 'Hello World!'; if ($name) { echo 'Hello ' . $name . '!';} }"
                       (phpinspect-document-contents document)))
      (phpinspect-buffer-register-edit buffer 24 24 1)
      (setq parsed (phpinspect-buffer-parse buffer))
      (should parsed)
      (setq hello1 (car (phpinspect-buffer-tokens-enclosing-point buffer 18)))
      (should (eq hello hello1))

      (phpinspect-document-apply-edit document 24 25 1 "{")
      (should (string= "<?php function Hello() { echo 'Hello World!'; if ($name) { echo 'Hello ' . $name . '!';} }"
                       (phpinspect-document-contents document)))
      (phpinspect-buffer-register-edit buffer 24 25 0)
      (setq parsed (phpinspect-buffer-parse buffer))
      (should parsed)
      (setq hello2 (car (phpinspect-buffer-tokens-enclosing-point buffer 18)))
      (should (eq hello hello2)))))
