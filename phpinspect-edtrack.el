

(cl-defstruct (phpinspect-edtrack (:constructor phpinspect-make-edtrack))
  (edits (phpinspect-make-ll)
         :documentation "Sorted list of edits in buffer"))


(cl-defstruct (phpinspect-edit (:constructor phpinspect-make-edit))
  (list nil
        :type phpinspect-llnode)
  (original-start 0
                  :type integer)
  (local-delta 0
               :type integer)
  (length 0))



;; (cl-defmethod phpinspect-edtrack-edit-in-original-region
;;   ((tracker phpinspect-edtrack) (start integer) (end integer))
;;   (condition-case found
;;       (seq-doseq (edit (phpinspect-edtrack-edits tracker))
;;         (when (phpinspect-edit-overlaps-original edit start end)
;;           (throw edit



  ;;        (first-overlapper (seq-find overlap-test edits))
  ;;        (overlappers (when first-overlapper
  ;;                       (seq-take-while overlap-test
  ;;                                       (phpinspect-ll-link edits first-overlapper)))))
  ;;   (when overlappers
  ;;     (phpinspect-edit-merge edit ,@overlappers)
  ;;     (let (
  ;;     (phpinspect-slice-detach

(defsubst phpinspect-edit-link (edit)
  (phpinspect-ll-link (phpinspect-edit-list edit) edit))

(defsubst phpinspect-edit--left-delta (edit)
  (let ((left (phpinspect-llnode-left (phpinspect-edit-link edit))))
    (if left (phpinspect-edit-delta (phpinspect-llnode-value left)) 0)))

(cl-defmethod phpinspect-edit-start ((edit phpinspect-edit))
  (+ (phpinspect-edit-original-start edit) (phpinspect-edit--left-delta edit)))

(cl-defmethod phpinspect-edit-delta ((edit phpinspect-edit))
  (+ (phpinspect-edit--left-delta edit) (phpinspect-edit-local-delta edit)))

(cl-defmethod phpinspect-edit-end ((edit phpinspect-edit))
  (+ (phpinspect-edit-start edit) (phpinspect-edit-length edit)))

(cl-defmethod phpinspect-edit-overlaps-point ((edit phpinspect-edit) (point integer))
  (and (> (phpinspect-edit-end edit) point)
       (<= (phpinspect-edit-start edit) point)))

(cl-defmethod phpinspect-edit-before-point ((edit phpinspect-edit) (point integer))
  (< (phpinspect-edit-end edit) point))

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
     (- (phpinspect-edit-length edit) (phpinspect-edit-local-delta edit))))

(cl-defmethod phpinspect-edit-overlaps-original-point (edit point)
  (and (> (phpinspect-edit-original-end edit) point)
       (<= (phpinspect-edit-original-start edit) point)))

(defsubst phpinspect-edit-overlaps-original (edit start end)
  (let ((region (phpinspect-make-region start end)))
    (or (phpinspect-edit-overlaps-original-point edit start)
        (phpinspect-edit-overlaps-original-point edit (- end 1))
        (phpinspect-region-overlaps-point region (phpinspect-edit-original-start edit))
        (phpinspect-region-overlaps-point region (- (phpinspect-edit-original-end edit) 1)))))

(cl-defmethod phpinspect-edit-merge ((edit phpinspect-edit) (other phpinspect-edit))
  (let* ((start (phpinspect-edit-original-start edit))
         (length (phpinspect-edit-length edit))
         (delta (phpinspect-edit-local-delta edit))
         (start-difference (- (phpinspect-edit-original-start other) start))
         (length-delta (- (+ (phpinspect-edit-length other) start-difference) length)))

    (when (< start-difference 0)
      (setq start (- start start-difference)))

    (when (> length-delta 0)
      (setq length (+ length length-delta)))

    (setq delta (+ delta (phpinspect-edit-local-delta other)))

    (setf (phpinspect-edit-local-delta edit) delta)
    (setf (phpinspect-edit-original-start edit) start)
    (setf (phpinspect-edit-length edit) length)))

(cl-defmethod phpinspect-edtrack-register-edit
  ((tracker phpinspect-edtrack) (start integer) (end integer) (pre-change-length integer))
  (let* ((overlap-test (lambda (edit) (phpinspect-edit-overlaps edit start end)))
         (edits (phpinspect-edtrack-edits tracker))
         (first-overlap)
         (last-overlap)
         (edit-before)
         (new-edit))
    (catch 'break
        (seq-doseq (edit edits)
          (cond
           ((phpinspect-edit-overlaps edit start end)
            (if first-overlap
                (setq last-overlap edit)
              (setq first-overlap edit)))
           ((phpinspect-edit-before-point edit start)
            (setq edit-before edit))
           (last-overlap
            (throw 'break)))))

    (if edit-before
        (setq new-edit (phpinspect-make-edit
                        :original-start (- start (phpinspect-edit-delta edit-before))
                        :local-delta (- (- end start) pre-change-length)
                        :list edits
                        :length (- end start)))
      (setq new-edit (phpinspect-make-edit
                      :original-start start
                      :local-delta (- (- end start) pre-change-length)
                      :list edits
                      :length (- end start))))

    (if first-overlap
      (if last-overlap
          (let ((overlappers (phpinspect-slice-detach
                              (phpinspect-make-slice
                               :start (phpinspect-edit-link first-overlap)
                               :end (phpinspect-edit-link last-overlap)))))
            (seq-doseq (overlap overlapper)
              (phpinspect-edit-merge new-edit overlapper))
            (if edit-before
                (phpinspect-ll-insert-right (phpinspect-edit-link edit-before) new-edit)
              (phpinspect-ll-push new-edit edits)))
        (let ((link (phpinspect-edit-link first-overlap)))
          (phpinspect-edit-merge new-edit first-overlap)
          (phpinspect-ll-relink link new-edit)))

      (if edit-before
          (phpinspect-ll-insert-right (phpinspect-edit-link edit-before) new-edit)
        (phpinspect-ll-push new-edit edits)))

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


(defsubst phpinspect-point-inside-edit-err-p (err)
  (and (listp err)
       (eq 'phpinspect-point-inside-edit-err (car err))))

(cl-defmethod phpinspect-edtrack-original-position-at-point
  ((tracker phpinspect-edtrack) (point integer))
  (let ((edit-before (phpinspect-edtrack--last-edit-before-point tracker point)))
    (when edit-before
      (setq point (- point (phpinspect-edit-delta edit-before)))
      (let ((next-edit-link (phpinspect-llnode-right (phpinspect-edit-link edit-before))))
        (when (and next-edit-link
                   (phpinspect-edit-overlaps-original-point
                    (phpinspect-llnode-value next-edit-link)
                    point))
          (throw 'phpinspect-point-inside-edit
                 `(phpinspect-point-inside-edit-err
                   "Point is inside an edited region, cannot accurately determine original location")))))
    point))

(cl-defmethod phpinspect-edtrack-current-position-at-point
  ((tracker phpinspect-edtrack) (point integer))
  (let ((edit-before (phpinspect-edtrack--last-edit-before-original-point tracker point)))
    (when edit-before
      (let ((next-edit-link (phpinspect-llnode-right (phpinspect-edit-link edit-before))))
        (when (and next-edit-link
                   (phpinspect-edit-overlaps-original-point
                    (phpinspect-llnode-value next-edit-link)
                    point))
          (throw 'phpinspect-point-inside-edit
                 `(phpinspect-point-inside-edit-err
                   "Point is inside an edited region, cannot accurately determine current location"))
          (setq point (+ point (phpinspect-edit-delta edit-before)))))))

  ;; Return
  point)


(defsubst phpinspect-edit-to-string (edit)
  (format "original-start: %d, length: %d, local-delta: %d"
          (phpinspect-edit-original-start edit)
          (phpinspect-edit-length edit)
          (phpinspect-edit-local-delta edit)))


(provide 'phpinspect-edtrack)
