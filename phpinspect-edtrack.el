

(cl-defstruct (phpinspect-edtrack (:constructor phpinspect-make-edtrack))
  (edits (phpinspect-make-ll)
         :documentation "Sorted list of edits in buffer")
  (taint-pool (phpinspect-make-tree :grow-root t)
              :documentation "Non overlapping pool of tainted buffer regions"))


(cl-defstruct (phpinspect-edit (:constructor phpinspect-make-edit))
  (list nil
        :type phpinspect-llnode)
  (original-start 0
                  :type integer)
  (local-delta 0
               :type integer)
  (length 0))

(defsubst phpinspect-edit-link (edit)
  (when (phpinspect-edit-list edit)
    (phpinspect-ll-link (phpinspect-edit-list edit) edit)))

(defsubst phpinspect-edit--left-delta (edit)
  (let* ((link (phpinspect-edit-link edit))
         (left (when link (phpinspect-llnode-left (phpinspect-edit-link edit)))))
    (if left (phpinspect-edit-delta (phpinspect-llnode-value left)) 0)))

(cl-defmethod phpinspect-edit-start ((edit phpinspect-edit))
  (+ (phpinspect-edit-original-start edit) (phpinspect-edit--left-delta edit)))

(cl-defmethod phpinspect-edit-delta ((edit phpinspect-edit))
  (+ (phpinspect-edit--left-delta edit) (phpinspect-edit-local-delta edit)))

(cl-defmethod phpinspect-edit-end ((edit phpinspect-edit))
  (let ((end (+ (phpinspect-edit-start edit)
                (+ (phpinspect-edit-length edit) (phpinspect-edit-local-delta edit)))))
    (if (> (phpinspect-edit-start edit) end)
        (phpinspect-edit-start edit)
      end)))

(cl-defmethod phpinspect-edit-overlaps-point ((edit phpinspect-edit) (point integer))
  (and (> (phpinspect-edit-end edit) point)
       (<= (phpinspect-edit-start edit) point)))

(cl-defmethod phpinspect-edit-before-point ((edit phpinspect-edit) (point integer))
  (<= (phpinspect-edit-end edit) point))

(cl-defmethod phpinspect-edit-before-original-point ((edit phpinspect-edit) (point integer))
  (< (phpinspect-edit-original-end edit) point))

(defsubst phpinspect-edit-overlaps (edit start end)
  (let ((region (phpinspect-make-region start end)))
    (or (phpinspect-edit-overlaps-point edit start)
        (phpinspect-edit-overlaps-point edit (- end 1))
        (phpinspect-region-overlaps-point region (phpinspect-edit-start edit))
        (phpinspect-region-overlaps-point region (- (phpinspect-edit-end edit) 1)))))

(defsubst phpinspect-edit-original-end (edit)
  (+ (phpinspect-edit-original-start edit)
     (+ (phpinspect-edit-length edit))))

(cl-defmethod phpinspect-edit-overlaps-original-point (edit point)
  (and (> (phpinspect-edit-original-end edit) point)
       (<= (phpinspect-edit-original-start edit) point)))

(defsubst phpinspect-edit-overlaps-original (edit start end)
  (let ((region (phpinspect-make-region start end)))
    (or (phpinspect-edit-overlaps-original-point edit start)
        (phpinspect-edit-overlaps-original-point edit (- end 1))
        (phpinspect-region-overlaps-point region (phpinspect-edit-original-start edit))
        (phpinspect-region-overlaps-point region (- (phpinspect-edit-original-end edit) 1)))))

(defsubst phpinspect-edtrack-clear-taints (tracker)
  (setf (phpinspect-edtrack-taint-pool tracker) (phpinspect-make-tree :grow-root t)))

(defsubst phpinspect-edtrack-clear (tracker)
  (phpinspect-edtrack-clear-taints tracker)
  (setf (phpinspect-edtrack-edits tracker) (phpinspect-make-ll)))

(defsubst phpinspect-edtrack-has-taints-p (tracker)
  (not (phpinspect-tree-empty-p (phpinspect-edtrack-taint-pool tracker))))

(defsubst phpinspect-edtrack-register-taint (tracker start end)
  (let* ((pool (phpinspect-edtrack-taint-pool tracker))
         (overlappers (phpinspect-tree-find-overlapping-children pool start end)))

    (when overlappers
      (seq-doseq (overlapper overlappers)
        (when (> (phpinspect-tree-end overlapper) end)
          (setq end (phpinspect-tree-end overlapper)))

        (when (< (phpinspect-tree-start overlapper) start)
          (setq start (phpinspect-tree-start overlapper))))
      (phpinspect-slice-detach overlappers))

    (phpinspect-tree-insert pool start end 'edited)))

(cl-defmethod phpinspect-edtrack-register-edit
  ((tracker phpinspect-edtrack) (start integer) (end integer) (pre-change-length integer))
  (let* ((edits (phpinspect-edtrack-edits tracker))
         (first-overlap)
         (last-overlap)
         (edit-before)
         (new-edit))

    (catch 'break
      (seq-doseq (edit edits)
        (cond
         ((phpinspect-edit-before-point edit start)
          (setq edit-before edit))
         (edit-before
          (throw 'break nil)))))

    (if edit-before
        (setq new-edit (phpinspect-make-edit
                        :original-start (- start (phpinspect-edit-delta edit-before))
                        :local-delta (- (- end start) pre-change-length)
                        :list edits
                        :length pre-change-length))
      (setq new-edit (phpinspect-make-edit
                      :original-start start
                      :local-delta (- (- end start) pre-change-length)
                      :list edits
                      :length pre-change-length)))

    (if edit-before
        (phpinspect-ll-insert-right (phpinspect-edit-link edit-before) new-edit)
      (phpinspect-ll-push new-edit edits))

    (phpinspect-edtrack-register-taint
     tracker (phpinspect-edit-original-start new-edit) (phpinspect-edit-original-end new-edit))

    ;; Return
    new-edit))

(defsubst phpinspect-edtrack--last-edit-before-point (edtrack point)
  (let ((found))
    (catch 'break
        (seq-doseq (edit (phpinspect-edtrack-edits edtrack))
          (if (phpinspect-edit-before-point edit point)
              (setq found edit)
            (when found
              (throw 'break nil)))))

    found))

(defsubst phpinspect-edtrack--last-edit-before-original-point (edtrack point)
  (let ((found))
    (catch 'break
        (seq-doseq (edit (phpinspect-edtrack-edits edtrack))
          (if (phpinspect-edit-before-original-point edit point)
              (setq found edit)
            (when found
              (throw 'break nil)))))
    found))

(cl-defmethod phpinspect-edtrack-original-position-at-point
  ((tracker phpinspect-edtrack) (point integer))
  (let ((edit-before (phpinspect-edtrack--last-edit-before-point tracker point)))
    (when edit-before
      (setq point (- point (phpinspect-edit-delta edit-before)))))
    point)

(cl-defmethod phpinspect-edtrack-current-position-at-point
  ((tracker phpinspect-edtrack) (point integer))
  (let ((edit-before (phpinspect-edtrack--last-edit-before-original-point tracker point)))
    (when edit-before
      (setq point (+ point (phpinspect-edit-delta edit-before)))))

  ;; Return
  point)


(defsubst phpinspect-edit-to-string (edit)
  (format "[original-start: %d, length: %d, local-delta: %d]"
          (phpinspect-edit-original-start edit)
          (phpinspect-edit-length edit)
          (phpinspect-edit-local-delta edit)))


(provide 'phpinspect-edtrack)
