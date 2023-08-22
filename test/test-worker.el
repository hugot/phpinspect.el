;;; test-worker.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'phpinspect-index)
(require 'phpinspect-worker)

(ert-deftest phpinspect-queue-enqueue ()
  (let ((queue (phpinspect-make-queue)))
    (phpinspect-queue-enqueue queue "one")
    (phpinspect-queue-enqueue queue "two")
    (phpinspect-queue-enqueue queue "three")

    (should (string= "one" (phpinspect-queue-dequeue queue)))
    (should (string= "two" (phpinspect-queue-dequeue queue)))
    (should (string= "three" (phpinspect-queue-dequeue queue)))
    (should-not (phpinspect-queue-dequeue queue))))

(ert-deftest phpinspect-queue-subscribe ()
  (let ((be-called nil))
    (let ((queue (phpinspect-make-queue (lambda () (setq be-called t)))))
      (phpinspect-queue-enqueue queue "one"))
    (should be-called)))

(ert-deftest phpinspect-queue-find ()
  (let ((queue (phpinspect-make-queue)))
    (phpinspect-queue-enqueue queue "one")
    (phpinspect-queue-enqueue queue "two")
    (phpinspect-queue-enqueue queue "three")

    (should (string= "one" (phpinspect-queue-find queue "one" 'string=)))
    (should (string= "two" (phpinspect-queue-find queue "two" 'string=)))
    (should (string= "three" (phpinspect-queue-find queue "three" 'string=)))))

(ert-deftest phpinspect-doqueue ()
  ;; Iterate over a populated queue
  (let ((queue (phpinspect-make-queue)))
    (phpinspect-queue-enqueue queue "one")
    (phpinspect-queue-enqueue queue "two")
    (phpinspect-queue-enqueue queue "three")
    (phpinspect-queue-enqueue queue "four")

    (let ((expected-things '("one" "two" "three" "four"))
          (things))
      (phpinspect-doqueue (thing queue)
        (push thing things))

      (should (equal expected-things (nreverse things)))))

  ;; attempt to iterate over an empty queue
  (let ((have-iterated nil))
    (phpinspect-doqueue (_thing (phpinspect-make-queue))
      (setq have-iterated t))

    (should-not have-iterated)))


(ert-deftest phpinspect-queue-enqueue-noduplicate ()
  (let ((queue (phpinspect-make-queue))
        (expected-things '("one" "two"))
        (things))

    (phpinspect-queue-enqueue-noduplicate queue "one" 'string=)
    (phpinspect-queue-enqueue-noduplicate queue "two" 'string=)
    (phpinspect-queue-enqueue-noduplicate queue "two" 'string=)
    (phpinspect-queue-enqueue-noduplicate queue "one" 'string=)

    (phpinspect-doqueue (thing queue)
      (push thing things))

    (should (equal expected-things (nreverse things)))))
