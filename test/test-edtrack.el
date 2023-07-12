(require 'ert)
(require 'phpinspect-edtrack)

(ert-deftest phpinspect-edtrack-register-edit ()
  (let* ((edtrack (phpinspect-make-edtrack))
         (edit1 (phpinspect-edtrack-register-edit edtrack 5 10 10))
         (edit2 (phpinspect-edtrack-register-edit edtrack 15 22 7))
         (edit3 (phpinspect-edtrack-register-edit edtrack 100 200 150)))

    (should (= 10 (phpinspect-edit-end edit1)))
    (should (= 22 (phpinspect-edit-end edit2)))

    (should (= 30 (phpinspect-edtrack-original-position-at-point edtrack 25)))
    (should (= 4 (phpinspect-edtrack-original-position-at-point edtrack 4)))
    (should (= 260 (phpinspect-edtrack-original-position-at-point edtrack 205)))))

(ert-deftest phpinsepct-edtrack-register-multi-edits ()
  (let* ((track (phpinspect-make-edtrack))
         (edit (phpinspect-edtrack-register-edit track 10 20 5))
         (edit1 (phpinspect-edtrack-register-edit track 25 30 0))
         (edit2 (phpinspect-edtrack-register-edit track 13 20 0)))
    (should (eq edit2 (seq-elt (phpinspect-edtrack-edits track) 0)))
    (should (eq edit (seq-elt (phpinspect-edtrack-edits track) 1)))
    (should (eq edit1 (seq-elt (phpinspect-edtrack-edits track) 2)))

    (should (= 42 (phpinspect-edtrack-current-position-at-point track 25)))))

(ert-deftest phpinspect-edtrack-register-multi-edits-deletions ()
  (let* ((track (phpinspect-make-edtrack))
         (edit (phpinspect-edtrack-register-edit track 10 20 5))
         (edit1 (phpinspect-edtrack-register-edit track 25 30 20))
         (edit2 (phpinspect-edtrack-register-edit track 13 20 0)))
    (should (eq edit2 (seq-elt (phpinspect-edtrack-edits track) 0)))
    (should (eq edit (seq-elt (phpinspect-edtrack-edits track) 1)))
    (should (eq edit1 (seq-elt (phpinspect-edtrack-edits track) 2)))

    (should (= 42 (phpinspect-edtrack-current-position-at-point track 45)))))

(ert-deftest phpinspect-edtrack-register-taint ()
  (let* ((track (phpinspect-make-edtrack)))
    (phpinspect-edtrack-register-taint track 0 5)

    (should-not (phpinspect-tree-empty-p (phpinspect-edtrack-taint-pool track)))))
