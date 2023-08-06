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

(cl-defstruct (phpinspect-document (:constructor phpinspect-make-document))
  (buffer (get-buffer-create
                  (generate-new-buffer-name "**phpinspect-document** shadow buffer") t)
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

(defmacro phpinspect-document-setq-local (document &rest assignments)
  (declare (indent 1))
  `(with-current-buffer (phpinspect-document-buffer ,document)
     (setq-local ,@assignments)))

(defmacro phpinspect-with-document-buffer (document &rest body)
  (declare (indent 1))
  `(with-current-buffer (phpinspect-document-buffer ,document)
     ,@body))

(cl-defmethod phpinspect-document-contents ((document phpinspect-document))
  (with-current-buffer (phpinspect-document-buffer document)
    (buffer-string)))

(ert-deftest phpinspect-buffer-parse-incrementally ()
  (let* ((document (phpinspect-make-document))
         (buffer (phpinspect-make-buffer
                  :buffer (phpinspect-document-buffer document)))
         (parsed))
    ;; TODO: write tests for more complicated cases (multiple edits, etc.)
    (phpinspect-document-set-contents document "<?php function Bello() { echo 'Hello World!'; if ($name) { echo 'Hello ' . $name . '!';} }")

    (setq parsed (phpinspect-buffer-parse buffer))
    (should parsed)

    (let* ((enclosing-bello (phpinspect-buffer-tokens-enclosing-point buffer 18))
           (bello (car enclosing-bello))
           (enclosing-bello1)
           (bello1)
           (bello2))
      (should (equal '(:word "Bello") (phpinspect-meta-token bello)))
      (should parsed)

      ;; Delete function block opening brace
      (phpinspect-document-apply-edit document 24 24 -1 "")
      (should (string= "<?php function Bello()  echo 'Hello World!'; if ($name) { echo 'Hello ' . $name . '!';} }"
                       (phpinspect-document-contents document)))
      (phpinspect-buffer-register-edit buffer 24 24 1)
      (setq parsed (phpinspect-buffer-parse buffer))
      (should parsed)
      (setq enclosing-bello1 (phpinspect-buffer-tokens-enclosing-point buffer 18))
      (setq bello1 (car enclosing-bello1))
      (should (eq (phpinspect-meta-token bello) (phpinspect-meta-token bello1)))

      (should (phpinspect-declaration-p (phpinspect-meta-token (phpinspect-meta-parent bello))))
      (should (phpinspect-declaration-p (phpinspect-meta-token (phpinspect-meta-parent bello1))))

      (should (phpinspect-function-p (phpinspect-meta-token (phpinspect-meta-parent (phpinspect-meta-parent bello)))))
      (should (phpinspect-function-p (phpinspect-meta-token (phpinspect-meta-parent (phpinspect-meta-parent bello1)))))

      (let ((function (phpinspect-meta-token (phpinspect-meta-parent (phpinspect-meta-parent bello1)))))
        (should (= 2 (length function)))
        (should (phpinspect-declaration-p (cadr function)))
        (should (member '(:word "Bello") (cadr function)))
        (should (member '(:word "echo") (cadr function))))

      (phpinspect-document-apply-edit document 24 25 1 "{")
      (should (string= "<?php function Bello() { echo 'Hello World!'; if ($name) { echo 'Hello ' . $name . '!';} }"
                       (phpinspect-document-contents document)))
      (phpinspect-buffer-register-edit buffer 24 25 0)
      (setq parsed (phpinspect-buffer-parse buffer))
      (should parsed)
      (setq bello2 (car (phpinspect-buffer-tokens-enclosing-point buffer 18)))
      (should (eq (phpinspect-meta-token bello) (phpinspect-meta-token bello2))))))

(ert-deftest phpinspect-buffer-parse-incrementally-position-change ()
  (with-temp-buffer
    (let ((buffer (phpinspect-make-buffer :buffer (current-buffer))))
      (insert "<?php
declare(strict_types=1);

namespace App\\Controller\\Api\\V1;

class AccountStatisticsController {

    function __construct(){}
}")

      (setq-local phpinspect-test-buffer t)
      (add-to-list 'after-change-functions
                   (lambda (start end pre-change-length)
                     (when (boundp 'phpinspect-test-buffer)
                       (phpinspect-buffer-register-edit buffer start end pre-change-length))))

      (let* ((bmap (phpinspect-buffer-parse-map buffer))
             (class-location 67)
             (class (phpinspect-bmap-token-starting-at bmap class-location)))

        (should class)
        (should (phpinspect-class-p (phpinspect-meta-token class)))
        (should (= class-location (phpinspect-meta-start class)))

        (goto-char 65)
        (let ((edit-string "use Symfony\\Component\\HttpFoundation\\JsonResponse;\n")
              bmap class tokens-enclosing use-statement)
          (insert edit-string)

          (setq bmap (phpinspect-buffer-parse-map buffer)
                class (phpinspect-bmap-token-starting-at bmap (+ 67 (length edit-string))))

          (setq class-location (+ class-location (length edit-string)))
          (should class)
          (should (phpinspect-class-p (phpinspect-meta-token class)))
          (should (= class-location (phpinspect-meta-start class)))

          (setq tokens-enclosing (phpinspect-bmap-tokens-overlapping bmap class-location))
          (setq class (seq-find (lambda (meta) (phpinspect-class-p (phpinspect-meta-token meta)))
                                tokens-enclosing))
          (should  class)
          (should (= class-location (phpinspect-meta-start class)))
          (should (phpinspect-class-p (phpinspect-meta-token class)))

          (setq use-statement (phpinspect-bmap-token-starting-at bmap 65))
          (should use-statement)
          (should (phpinspect-use-p (phpinspect-meta-token use-statement)))
          (should (seq-find #'phpinspect-use-p (seq-find #'phpinspect-namespace-p (phpinspect-buffer-tree buffer))))

          (let ((second-use))
            (goto-char 65)
            (setq edit-string "use Another\\Use\\Statement;\n")
            (insert edit-string)

            (setq class-location (+ class-location (length edit-string)))
            (setq bmap (phpinspect-buffer-parse-map buffer)
                  class (phpinspect-bmap-token-starting-at bmap class-location))

            (should class)

            (setq second-use (phpinspect-bmap-token-starting-at bmap 65))
            (should second-use)

            (setq class (phpinspect-bmap-token-starting-at bmap class-location))
            (should  class)
            (should (= class-location (phpinspect-meta-start class)))
            (should (phpinspect-class-p (phpinspect-meta-token class)))))))))


(ert-deftest phpinspect-buffer-parse-incrementally-multiedit ()
  (let* ((document (phpinspect-make-document))
         (buffer (phpinspect-make-buffer
                  :buffer (phpinspect-document-buffer document)))
         parsed parsed-after current-tree)

    (phpinspect-document-set-contents
     document
     "<?php


namespace XXX;

use ZZZ\\zzz;





class YYY {

    function Foo() {
        if(bar()) {
            return $baz->bip->bop(bar($bim), $bom)
        }
    }
}")

    (phpinspect-document-setq-local document
      phpinspect-current-buffer buffer)
    (phpinspect-with-document-buffer document
      (setq buffer-undo-list nil)
      (add-hook 'after-change-functions #'phpinspect-after-change-function))

    (setq parsed (phpinspect-buffer-parse buffer 'no-interrupt))

    ;; Delete lines before class
    (phpinspect-with-document-buffer document
      (goto-char 40)
      (kill-line)
      (kill-line)
      (kill-line))

    (setq parsed-after (phpinspect-buffer-parse buffer 'no-interrupt))

    (should (equal parsed parsed-after))

    ;; Delete namespace declaration
    (phpinspect-with-document-buffer document
      (goto-char 9)
      (kill-line))

    (setq parsed-after (phpinspect-buffer-parse buffer 'no-interrupt))
    (setq current-tree (phpinspect-with-document-buffer document
                         (goto-char (point-min))
                         (phpinspect-parse-buffer-until-point (current-buffer) (point-max))))

    (should (equal current-tree parsed-after))

    ;;Bring back the namespace declaration
    (phpinspect-with-document-buffer document
      (undo-start)
      (undo-more 1))

    (setq parsed-after (phpinspect-buffer-parse buffer 'no-interrupt))

    (should (equal parsed parsed-after))))
