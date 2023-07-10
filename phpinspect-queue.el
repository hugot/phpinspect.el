

(cl-defstruct (phpinspect-queue-item
               (:constructor phpinspect-make-queue-item))
  (next nil
        :type phpinspect-queue-item
        :documentation
        "The next item in the queue")
  (thing nil
         :type any
         :documentation
         "The thing stored in the queue")
  (previous nil
            :type phpinspect-queue-item
            :documentation
            "The previous item in the queue")
  (subscription nil
                :type function
                :read-only t
                :documentation
                "A function that should be called when items are
                enqueued."))

(defsubst phpinspect-make-queue (&optional subscription)
  (phpinspect-make-queue-item :subscription subscription))

;; Recursion causes max-eval-depth error here for long queues. Hence the loop
;; implementation for these two functions.
(cl-defmethod phpinspect-queue-last ((item phpinspect-queue-item))
  "Get the last item in the queue that ITEM is part of."
  (while (phpinspect-queue-item-next item)
    (setq item (phpinspect-queue-item-next item)))
  item)

(cl-defmethod phpinspect-queue-first ((item phpinspect-queue-item))
  "Get the first item in the queue that ITEM is part of."
  (while (phpinspect-queue-item-previous item)
      (setq item (phpinspect-queue-item-previous item)))
  item)

(cl-defmethod phpinspect-queue-enqueue ((item phpinspect-queue-item) thing)
  "Add THING to the end of the queue that ITEM is part of."
  (let ((last (phpinspect-queue-last item)))
    (if (not (phpinspect-queue-item-thing last))
        (setf (phpinspect-queue-item-thing last) thing)
      (setf (phpinspect-queue-item-next last)
            (phpinspect-make-queue-item
             :previous last
             :thing thing
             :subscription (phpinspect-queue-item-subscription item)))))
  (when (phpinspect-queue-item-subscription item)
    (funcall (phpinspect-queue-item-subscription item))))

(cl-defmethod phpinspect-queue-dequeue ((item phpinspect-queue-item))
  "Remove the thing at the front of the queue that ITEM is part of and return it."
  (let* ((first (phpinspect-queue-first item))
         (thing (phpinspect-queue-item-thing first))
         (next (phpinspect-queue-item-next first)))
    (when next (setf (phpinspect-queue-item-previous next) nil))
    (cond ((and (eq item first) (not next))
           (setf (phpinspect-queue-item-thing item)
                 nil))
          ((eq item first)
           (setf (phpinspect-queue-item-thing item)
                 (phpinspect-queue-item-thing next))
           (setf (phpinspect-queue-item-next item)
                 (phpinspect-queue-item-next next))))
    thing))

(defmacro phpinspect-doqueue (place-and-queue &rest body)
  "Loop over queue defined in PLACE-AND-QUEUE executing BODY.

PLACE-AND-QUEUE is a two-member list. The first item should be
the place that the current thing in the queue should be assigned
to upon each iteration. The second item should be a queue-item
belonging to the queue that must be iterated over.

BODY can be any form."
  (declare (indent defun))
  (let ((item-sym (gensym))
        (place (car place-and-queue))
        (queue (cadr place-and-queue)))
    `(let* ((,item-sym (phpinspect-queue-first ,queue))
            (,place (phpinspect-queue-item-thing ,item-sym)))
       (when ,place
         ,@body
         (while (setq ,item-sym (phpinspect-queue-item-next ,item-sym))
           (setq ,place (phpinspect-queue-item-thing ,item-sym))
           ,@body)))))

(cl-defmethod phpinspect-queue-find
  ((item phpinspect-queue-item) thing comparison-func)
  "Find THING in the queue that ITEM is part of using COMPARISON-FUNC."
  (catch 'found
    (phpinspect-doqueue (current-thing item)
      (when (funcall comparison-func current-thing thing)
        (throw 'found current-thing)))))

(cl-defmethod phpinspect-queue-enqueue-noduplicate
  ((item phpinspect-queue-item) thing comparison-func)

  (when (not (phpinspect-queue-find item thing comparison-func))
    (phpinspect-queue-enqueue item thing)))

(provide 'phpinspect-queue)
