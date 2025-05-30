;;; test-buffer-parsing.el --- tests for parsing of buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

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


(require 'phpinspect-buffer)
(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))

(ert-deftest phpinspect-parse-nested-function-incrementally ()
  (with-temp-buffer
    (let ((buffer (phpinspect-claim-buffer (current-buffer) (phpinspect--make-dummy-project))))
      (insert "class C { private ?\\DateTime $d; public function __construct() {


        return $foo()->bar->baz;
    }

    public function fooBar(RealTime $rt) {
        // stuff
    }
}")
      (should (equal (phpinspect-parse-string (buffer-string)) (phpinspect-buffer-parse buffer)))

      ;; goto first line of method block
      (goto-char 66)
      (insert "$foo")
      (insert " ")
      (insert "= ")
      (insert "function (): {\n\n}")
      (insert ";")

      (should (equal (phpinspect-parse-string (buffer-string)) (phpinspect-buffer-parse buffer)))

      (goto-char 86)
      (insert " ")
      (insert "use ")
      (insert "(")
      (should (equal (phpinspect-parse-string (buffer-string)) (phpinspect-buffer-parse buffer)))
      (insert "$d")
      (insert ")")
      (should (equal (phpinspect-parse-string (buffer-string)) (phpinspect-buffer-parse buffer))))))

(ert-deftest phpinspect-parse-nested-anonymous-functions-incrementally ()
  (with-temp-buffer
    (let ((buffer (phpinspect-claim-buffer (current-buffer) (phpinspect--make-dummy-project))))
      (insert "<?php
class ComplexParsing {
    private $callback;

    public function __construct() {
        $this->callback = function($x) {
            return function($y) use ($x) {
                return $x + $y;
            };
        };
    }

    public function executeCallback($value) {
        $inner = $this->callback($value);
        return $inner(10);
    }
}")
      (should (equal (phpinspect-parse-string (buffer-string)) (phpinspect-buffer-parse buffer)))

      ;; Insert a new method with nested anonymous functions
      (goto-char 175)
      (insert "\n\n    public function nestedAnonymous() {\n")
      (insert "        $nested = function($a) {\n")
      (insert "            return function($b) use ($a) {\n")
      (insert "                return function($c) use ($a, $b) {\n")
      (insert "                    return $a + $b + $c;\n")
      (insert "                };\n")
      (insert "            };\n")
      (insert "        };\n")
      (insert "        return $nested(1)(2)(3);\n")
      (insert "    }\n")

      (should (equal (phpinspect-parse-string (buffer-string)) (phpinspect-buffer-parse buffer)))

      ;; Modify the existing method to add another level of nesting
      (goto-char 130)
      (insert "            return function($z) use ($x, $y) {\n")
      (insert "                return $x + $y + $z;\n")
      (insert "            };\n")

      (should (equal (phpinspect-parse-string (buffer-string)) (phpinspect-buffer-parse buffer)))

      ;; Add a comment inside the nested anonymous function
      (goto-char 200)
      (insert " // This is a comment inside a nested anonymous function\n")

      (should (equal (phpinspect-parse-string (buffer-string)) (phpinspect-buffer-parse buffer))))))
