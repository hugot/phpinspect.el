;;; test-buffer-resolve.el --- Test resolving types in live edited buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation

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
(require 'phpinspect-buffer)
(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))

(ert-deftest phpinspect-buffer-test-resolve-self-referential-variable ()
  (with-temp-buffer
    (let ((buffer (phpinspect-claim-buffer
                   (current-buffer) (phpinspect--make-dummy-composer-project-with-code))))
      (insert "<?php

namespace App\\Testerino;

class Test
{
    public function test(bool $flipflop = false): void
    {
        $value = new \\App\\Bar();
        if (true) {
             $b = $flipflop;
             // for some reason this bug only triggers when there is at least
             // one line of code before the self-referential assignment
             $value = $value->foo();
        }

        $this->value = $value->
    }
}")

      (phpinspect-buffer-parse buffer)

      (backward-char 7)
      (insert "$value->")

      (let* ((rctx (phpinspect-buffer-get-resolvecontext buffer (point)))
             (type (phpinspect-resolve-type-from-context rctx)))

        (should type)
        (should (phpinspect--type= (phpinspect--make-type :name "\\App\\Foo" :fully-qualified t) type ))))))

(provide 'test-buffer-resolve)
;;; test-buffer-resolve.el ends here
