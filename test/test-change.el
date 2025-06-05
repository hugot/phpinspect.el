;;; test-change.el --- Tests for phpinspect-change.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords:

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

(require 'phpinspect-change)

(ert-deftest phpinspect-change-pre-position ()
  (let ((change (phpinspect-make-change
                 :start 1
                 :end 11
                 :prev-length 21)))
    (should (= 31 (phpi-change-pre-position change 20)))
    (should (= 26 (phpi-change-pre-position change 15)))
    (should (= 22 (phpi-change-pre-position change 11)))
    (should (= 10 (phpi-change-pre-position change 10)))

    (should (= 41 (phpi-change-pre-position change 30)))))

(ert-deftest phpinspect-change-pre-position-shift-right ()
  (let ((change (phpinspect-make-change
                 :start 1
                 :end 21
                 :prev-length 10)))
    (should (= 11 (phpi-change-pre-position change 20)))
    (should (= 11 (phpi-change-pre-position change 15)))
    (should (= 11 (phpi-change-pre-position change 11)))
    (should (= 10 (phpi-change-pre-position change 10)))

    (should (= 20 (phpi-change-pre-position change 30)))))


(provide 'test-change)
;;; test-change.el ends here
