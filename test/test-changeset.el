;;; test-changeset.el --- Tests for phpinspect-changeset.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Hugo Thunnissen

;; Author: Hugo Thunnissen <hugo@libre-it.nl>

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

(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))

(require 'phpinspect-changeset)
(require 'phpinspect-meta)
(require 'phpinspect-parse-context)

(ert-deftest phpinspect-meta-with-changeset-revert-parent-relation ()
  (let ((parent (phpinspect-make-meta nil 1 20 "" 'parent))
        (child (phpinspect-make-meta nil 4 8 "" 'child))
        (pctx (phpinspect-make-pctx))
        (other-parent (phpinspect-make-meta nil 3 20 "" 'other-parent))
        parent-offset)

    (phpinspect-meta-set-parent child parent)
    (setq parent-offset (phpinspect-meta-parent-offset child))


    (phpinspect-with-parse-context pctx
      (phpinspect-meta-with-changeset child
        (phpinspect-meta-detach-parent child)
        (phpinspect-meta-set-parent child other-parent)))

    (phpinspect-changeset-revert (car (phpinspect-pctx-changesets pctx)))

    (should (eq parent (phpinspect-meta-parent child)))
    (let ((children (phpinspect-splayt-to-list
                     (phpinspect-meta-children parent))))
      (should (length= children 1))
      (should (eq 'child (phpinspect-meta-token (car children))))

      (should (= parent-offset (phpinspect-meta-parent-offset child)))
      (should (= 4 (phpinspect-meta-start child)))
      (should (= 8 (phpinspect-meta-end child)))
      (should (= 4 (phpinspect-meta-width child)))

      (should (eq child (phpinspect-splayt-find
                         (phpinspect-meta-children parent)
                         (phpinspect-meta-parent-offset child)))))))


;;; test-changeset.el ends here
