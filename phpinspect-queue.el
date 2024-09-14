;;; phpinspect-queue.el --- PHP parsing and completion package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc

;; Author: Hugo Thunnissen <devel@hugot.nl>
;; Keywords: php, languages, tools, convenience
;; Version: 2.1.0

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


(cl-defstruct (phpinspect-queue
               (:constructor phpinspect-make-queue-generated))
  (-first nil
         :type phpinspect-queue-item
         :documentation
         "The first item in the queue")
  (-last nil
        :type phpinspect-queue-item
        :documentation
        "The last item in the queue")
  (subscription  nil
                 :type function
                :documentation
                "A function that should be called when items are
                enqueued."))

(cl-defstruct (phpinspect-queue-item
               (:constructor phpinspect-make-queue-item))
  (next nil
        :type phpinspect-queue-item
        :documentation
        "The next item in the queue")
  (value nil
         :type any
         :documentation
         "The value stored in the queue")
  (previous nil
            :type phpinspect-queue-item
            :documentation
            "The previous item in the queue"))

(define-inline phpinspect-make-queue (&optional subscription)
  (inline-quote
   (progn
     (phpinspect-make-queue-generated :subscription ,subscription))))

(cl-defmethod phpinspect-queue-first ((queue phpinspect-queue))
  (phpinspect-queue--first queue))

(cl-defmethod phpinspect-queue-last ((queue phpinspect-queue))
  (or (phpinspect-queue--last queue) (phpinspect-queue--first queue)))

(cl-defmethod phpinspect-queue-enqueue ((queue phpinspect-queue) value &optional no-notify)
  "Add VALUE to the end of the queue that ITEM is part of."
  (let ((last (phpinspect-queue-last queue))
        (new-item (phpinspect-make-queue-item :value value)))
    (if (not last)
        (setf (phpinspect-queue--first queue) new-item)
      (setf (phpinspect-queue-item-next last) new-item)
      (setf (phpinspect-queue-item-previous new-item) last))
    (setf (phpinspect-queue--last queue) new-item))

  (when (and (not no-notify) (phpinspect-queue-subscription queue))
    (funcall (phpinspect-queue-subscription queue))))

(cl-defmethod phpinspect-queue-dequeue ((queue phpinspect-queue))
  "Remove the value at the front of the queue that ITEM is part of and return it."
  (let* ((first (phpinspect-queue-first queue))
         next value)
    (when first
      (setq next (phpinspect-queue-item-next first))
      (setq value (phpinspect-queue-item-value first)))
    (if next
        (setf (phpinspect-queue-item-previous next) nil)
      (setf (phpinspect-queue--last queue) nil))
    (setf (phpinspect-queue--first queue) next)
    value))

(defmacro phpinspect-doqueue (place-and-queue &rest body)
  "Loop over queue defined in PLACE-AND-QUEUE executing BODY.

PLACE-AND-QUEUE is a two-member list. The first item should be
the place that the current value in the queue should be assigned
to upon each iteration. The second item should be a queue-item
belonging to the queue that must be iterated over.

BODY can be any form."
  (declare (indent defun))
  (let ((item-sym (gensym))
        (place (car place-and-queue))
        (queue (cadr place-and-queue)))
    `(let* ((,item-sym (phpinspect-queue-first ,queue)))
       (while ,item-sym
         (let ((,place (phpinspect-queue-item-value ,item-sym)))
           ,@body
           (setq ,item-sym (phpinspect-queue-item-next ,item-sym)))))))

(cl-defmethod phpinspect-queue-find
  ((queue phpinspect-queue) value comparison-func)
  "Find VALUE in the queue that ITEM is part of using COMPARISON-FUNC."
  (catch 'found
    (phpinspect-doqueue (current-value queue)
      (when (funcall comparison-func current-value value)
        (throw 'found current-value)))))

(cl-defmethod phpinspect-queue-enqueue-noduplicate
  ((queue phpinspect-queue) value comparison-func)

  (when (not (phpinspect-queue-find queue value comparison-func))
    (phpinspect-queue-enqueue queue value)))

(provide 'phpinspect-queue)
;;; phpinspect-queue.el ends here
