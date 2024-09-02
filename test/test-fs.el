;;; test-fs.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

(require 'phpinspect-fs)

(ert-deftest phpinspect-virtual-fs-file-exists-p ()
  (let ((fs (phpinspect-make-virtual-fs)))
    (phpinspect-virtual-fs-set-file fs "/test/test.txt" "contents")

    (should (phpinspect-fs-file-exists-p fs "/test/test.txt"))))

(ert-deftest phpinspect-virtual-fs-insert-file-contents ()
  (let ((fs (phpinspect-make-virtual-fs)))
    (phpinspect-virtual-fs-set-file fs "/test/test.txt" "contents")

    (with-temp-buffer
      (phpinspect-fs-insert-file-contents fs "/test/test.txt")
      (should (string= "contents" (buffer-string))))

    (with-temp-buffer
      (phpinspect-fs-insert-file-contents fs "/test/nonexistant")
      (should (string= "" (buffer-string))))))

(ert-deftest phpinspect-virtual-fs-directory-files-and-recursively ()
  (let ((fs (phpinspect-make-virtual-fs)))
    (puthash "/test/test.txt" "contents" (phpinspect-virtual-fs-files fs))
    (puthash "/a/b/c/dee.match" "contents" (phpinspect-virtual-fs-files fs))
    (puthash "/a/b/c/cee.match" "contents" (phpinspect-virtual-fs-files fs))
    (puthash "/a/b/c/aaa.match" "contents" (phpinspect-virtual-fs-files fs))
    (puthash "/a/b/c/nope.nomatch" "contents" (phpinspect-virtual-fs-files fs))
    (puthash "/a/b/d/jee.match" "contents" (phpinspect-virtual-fs-files fs))

    (let ((files (phpinspect-fs-directory-files fs "/test/")))
      (should (equal '("/test/test.txt") files)))

    (let ((files (phpinspect-fs-directory-files fs "/a/b/c")))
      (should (member "/a/b/c/dee.match" files))
      (should (member "/a/b/c/cee.match" files))
      (should (member "/a/b/c/aaa.match" files))
      (should (member "/a/b/c/nope.nomatch" files)))

    (let ((files (phpinspect-fs-directory-files fs "/a/b/c" "\\.match$")))
      (should (member "/a/b/c/dee.match" files))
      (should (member "/a/b/c/cee.match" files))
      (should (member "/a/b/c/aaa.match" files))
      (should (not (member "/a/b/c/nope.nomatch" files))))

    (let ((files (phpinspect-fs-directory-files fs "/a/b")))
      (should (member "/a/b/c" files))
      (should (member "/a/b/d" files))
      (should (= 2 (length files))))

    (let ((files (phpinspect-fs-directory-files-recursively fs "/a/b")))
      (should (member "/a/b/c/dee.match" files))
      (should (member "/a/b/c/cee.match" files))
      (should (member "/a/b/c/aaa.match" files))
      (should (member "/a/b/c/nope.nomatch" files))
      (should (member "/a/b/d/jee.match" files)))

    (let ((files (phpinspect-fs-directory-files-recursively fs "/a/b" "\\.match$")))
      (should (member "/a/b/c/dee.match" files))
      (should (member "/a/b/c/cee.match" files))
      (should (member "/a/b/c/aaa.match" files))
      (should (not (member "/a/b/c/nope.nomatch" files)))
      (should (member "/a/b/d/jee.match" files)))))
