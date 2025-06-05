;;; parse-file.el --- Benchmarks of phpinspect parser in different configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: benchmark

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


(require 'profiler)
(require 'phpinspect-parser)
(require 'phpinspect-buffer)

(with-temp-buffer
  (let* ((here (file-name-directory (macroexp-file-name)))
         (benchmark-file (or (getenv "PHPINSPECT_BENCHMARK_FILE")
                             (expand-file-name "Response.php" here)))
         (buffer (phpinspect-claim-buffer (current-buffer) (phpinspect--make-project
                                                            :root "/dev/null"
                                                            :fs (phpinspect-make-virtual-fs)
                                                            :autoload (phpinspect-make-autoloader
                                                                       :fs (phpinspect-make-virtual-fs)
                                                                       :project-root-resolver (lambda () "/dev/null"))
                                                            :worker 'nil-worker)))
         result)

    (message "Incremental parse (warmup):")
    (setq result (benchmark-run 1 (progn
                                    (insert-file-contents benchmark-file)
                                    (phpinspect-buffer-parse buffer))))

    (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))

    (message "Incremental parse:")
    (setq result (benchmark-run 1 (phpinspect-buffer-parse buffer)))

    (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))

    (garbage-collect)
    (message "Incremental parse (no edits):")
    (setq result (benchmark-run 1 (phpinspect-buffer-parse buffer)))

    (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))
    (garbage-collect)

    (message "Incremental parse repeat (no edits):")
    (setq result (benchmark-run 1 (phpinspect-buffer-parse buffer)))
    (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))


    (message "Incremental parse after buffer edit:")

    (garbage-collect)

    (setq result (benchmark-run 1 (progn
                                    ;; Removes closing curly brace of __construct
                                    (goto-char 9062)
                                    (delete-char -1)
                                    (phpinspect-buffer-parse buffer))))

    (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))

    (garbage-collect)

    (message "Incremental parse after 2 more edits:")

    (setq result (benchmark-run 1 (progn
                                    (insert "{")
                                    (phpinspect-buffer-parse buffer))))


    (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))

    (with-temp-buffer
     (insert-file-contents benchmark-file)

     (garbage-collect)
     (message "Bare (no token reuse) parse (warmup):")
     (setq result (benchmark-run 1 (phpinspect-parse-current-buffer)))

     (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))

     (garbage-collect)
     (message "Bare (no token reuse) parse:")
     (setq result (benchmark-run 1 (phpinspect-parse-current-buffer))))

    (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))))
