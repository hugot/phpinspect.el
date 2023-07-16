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

(ert-deftest phpinspect-edtrack-orginal-position-at-point ()
  (let ((track (phpinspect-make-edtrack)))
    (phpinspect-edtrack-register-edit track 10 20 0)
    (should (= 10 (phpinspect-edtrack-original-position-at-point track 20)))
    (should (= 10 (phpinspect-edtrack-original-position-at-point track 15)))
    (phpinspect-edtrack-register-edit track 30 40 5)
    (should (= 35 (phpinspect-edtrack-original-position-at-point track 50)))
    (should (= 25 (phpinspect-edtrack-original-position-at-point track 39)))))

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

(ert-deftest phpinspect-edtrack-taint-iterator ()
  (let ((track (phpinspect-make-edtrack))
        (iterator))
    (phpinspect-edtrack-register-taint track 120 150)
    (phpinspect-edtrack-register-taint track 5 30)
    (phpinspect-edtrack-register-taint track 25 50)
    (phpinspect-edtrack-register-taint track 70 100)

    (setq iterator (phpinspect-edtrack-make-taint-iterator track))

    (should-not (phpinspect-taint-iterator-token-is-tainted-p
                 iterator (phpinspect-make-meta nil 1 4 nil nil)))

    (should (phpinspect-taint-iterator-token-is-tainted-p
             iterator (phpinspect-make-meta nil 4 7 nil nil)))

    (should (phpinspect-taint-iterator-token-is-tainted-p
             iterator (phpinspect-make-meta nil 20 30 nil nil)))

    (should-not (phpinspect-taint-iterator-token-is-tainted-p
                 iterator (phpinspect-make-meta nil 51 55 nil nil)))

    (should (phpinspect-taint-iterator-token-is-tainted-p
             iterator (phpinspect-make-meta nil 65 73 nil nil)))

        (should (phpinspect-taint-iterator-token-is-tainted-p
             iterator (phpinspect-make-meta nil 100 130 nil nil)))))
