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

(provide 'phpinspect-util)
;;; phpinspect-util.el ends here
