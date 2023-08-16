;;; test-edtrack.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'phpinspect-edtrack)
(require 'phpinspect-meta)

(ert-deftest phpinspect-edit-end ()
  (let ((edit (list (cons 10 3) (cons 6 5) (cons 4 -2))))
    (should (= 13 (phpinspect-edit-end edit)))))

(ert-deftest phpinspect-edtrack-register-edit ()
  (let* ((edtrack (phpinspect-make-edtrack)))
    (phpinspect-edtrack-register-edit edtrack 5 10 10)
    (phpinspect-edtrack-register-edit edtrack 100 200 150)
    (phpinspect-edtrack-register-edit edtrack 15 22 7)

    (should (equal `((255 . -50) (27 . 0) (15 . -5)) (phpinspect-edtrack-edits edtrack)))))

(ert-deftest phpinspect-edtrack-register-encroaching-edit ()
  (let* ((edtrack (phpinspect-make-edtrack)))
    (phpinspect-edtrack-register-edit edtrack 5 10 0)
    (phpinspect-edtrack-register-edit edtrack 100 150 25)

    ;; Encroaches on delta of edit before by 15 points ((125 + 25) - 135 = 15),
    ;; so the original end position should be calculated as 135 - (25 - 15) - 5 = 120
    ;; (see also `phpinspect-edtrack-original-position-at-point')
    (phpinspect-edtrack-register-edit edtrack 135 170 0)

    (should (equal `((120 . 35) (120 . 25) (5 . 5)) (phpinspect-edtrack-edits edtrack)))))


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

(ert-deftest phpinspect-edtrack-edit-derived-taint-iterator ()
  (let ((track (phpinspect-make-edtrack))
        iterator)
    (phpinspect-edtrack-register-edit track 10 20 5)
    (phpinspect-edtrack-register-edit track 15 30 0)
    (phpinspect-edtrack-register-edit track 20 25 10)

    (setq iterator (phpinspect-edtrack-make-taint-iterator track))

    (should (phpinspect-taint-iterator-region-is-tainted-p iterator 15 20))
    (should (phpinspect-taint-iterator-region-is-tainted-p iterator 25 30))
    (should-not (phpinspect-taint-iterator-region-is-tainted-p iterator 30 35))))

(ert-deftest phpinspect-edtrack-taint-overlapping-edits ()
  (let ((track (phpinspect-make-edtrack)))
    (phpinspect-edtrack-register-edit track 10 20 5)

    (should (equal (list (cons 10 15)) (phpinspect-edtrack-taint-pool track)))

    (phpinspect-edtrack-register-edit track 15 0 1)
    (should (equal (list (cons 10 16)) (phpinspect-edtrack-taint-pool track)))))

(ert-deftest phpinspect-edtrack-register-multi-edits-same-start ()
  (let ((track (phpinspect-make-edtrack)))
    (phpinspect-edtrack-register-edit track 10 11 0)
    (phpinspect-edtrack-register-edit track 10 10 1)

    (should (equal (list (cons 10 -1) (cons 10 1)) (phpinspect-edtrack-edits track)))))

(ert-deftest phpinspect-edtrack-undo ()
  (let ((track (phpinspect-make-edtrack)))
    (phpinspect-edtrack-register-edit track 10 10 10)
    (phpinspect-edtrack-register-edit track 10 10 10)
    (phpinspect-edtrack-register-edit track 10 30 0)

    (should (= 30 (phpinspect-edtrack-original-position-at-point track 30)))
    (should (= 20 (phpinspect-edtrack-original-position-at-point track 20)))
    (should (= 15 (phpinspect-edtrack-original-position-at-point track 15)))
    (should (= 35 (phpinspect-edtrack-original-position-at-point track 35)))
    (should (= 10 (phpinspect-edtrack-original-position-at-point track 10)))))
