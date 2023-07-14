(require 'ert)
(require 'phpinspect-edtrack)

(ert-deftest phpinspect-edit-end ()
  (let ((edit (list (cons 10 3) (cons 6 5) (cons 4 -2))))
    (should (= 13 (phpinspect-edit-end edit)))))

(ert-deftest phpinspect-edtrack-register-edit ()
  (let* ((edtrack (phpinspect-make-edtrack))
         (edit1 (phpinspect-edtrack-register-edit edtrack 5 10 10))
         (edit3 (phpinspect-edtrack-register-edit edtrack 100 200 150))
         (edit2 (phpinspect-edtrack-register-edit edtrack 15 22 7)))

    (should (equal `((255 . -50) (27 . 0) (15 . -5)) (phpinspect-edtrack-edits edtrack)))))
    ;; (pp (phpinspect-edtrack-edits edtrack))
    ;; (should (= 10 (phpinspect-edit-end edit1)))
    ;; (should (= 22 (phpinspect-edit-end edit2)))

    ;; (should (= 30 (phpinspect-edtrack-original-position-at-point edtrack 25)))
    ;; (should (= 4 (phpinspect-edtrack-original-position-at-point edtrack 4)))
    ;; (should (= 260 (phpinspect-edtrack-original-position-at-point edtrack 205)))))

(ert-deftest phpinsepct-edtrack-register-multi-edits ()
  (let ((track (phpinspect-make-edtrack)))
    (phpinspect-edtrack-register-edit track 10 20 5)
    (phpinspect-edtrack-register-edit track 25 30 0)
    (phpinspect-edtrack-register-edit track 13 20 0)

    (should (= 42 (phpinspect-edtrack-current-position-at-point track 25)))))

(ert-deftest phpinspect-edtrack-register-multi-edits-deletions ()
  (let ((track (phpinspect-make-edtrack)))
    (phpinspect-edtrack-register-edit track 10 20 5)
    (phpinspect-edtrack-register-edit track 25 30 20)
    (phpinspect-edtrack-register-edit track 13 20 0)

    (should (= 42 (phpinspect-edtrack-current-position-at-point track 45)))))

(ert-deftest phpinspect-edtrack-register-taint ()
  (let* ((track (phpinspect-make-edtrack)))
    (phpinspect-edtrack-register-taint track 0 5)
    (phpinspect-edtrack-register-taint track 10 20)
    (should (equal (list (cons 0 5) (cons 10 20)) (phpinspect-edtrack-taint-pool track)))

    (phpinspect-edtrack-register-taint track 3 20)

    (should (equal (list (cons 0 20)) (phpinspect-edtrack-taint-pool track)))))

    ;; (should-not (phpinspect-tree-empty-p (phpinspect-edtrack-taint-pool track)))))
