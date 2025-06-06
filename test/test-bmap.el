;; -*- lexical-binding: t; -*-

(require 'phpinspect-bmap)

(ert-deftest phpinspect-bmap-overlay ()
  (let ((bmap (phpinspect-make-bmap)))
    (phpinspect-bmap-register bmap 1 50 '(:token))

    (phpinspect-bmap-recycle bmap (phpinspect-make-meta nil 52 70 "" '(:othertoken)) 3)
    (phpinspect-bmap-register bmap 1 200 '(:root))

    (should (phpinspect-bmap-token-starting-at bmap 55))))


(ert-deftest phpinspect-bmap-nest-parent ()
  (let ((bmap (phpinspect-make-bmap))
        (child '(:child))
        (parent '(:parent))
        (granny '(:granny)))
    (phpinspect-bmap-register bmap 10 20 child)
    (phpinspect-bmap-register bmap 5 25 parent)
    (phpinspect-bmap-register bmap 2 30 granny)

    (let ((child-meta (phpinspect-bmap-token-meta bmap child))
          (parent-meta (phpinspect-bmap-token-meta bmap parent)))
      (should (eq parent (phpinspect-meta-token
                          (phpinspect-meta-parent child-meta))))
      (should (eq granny (phpinspect-meta-token (phpinspect-meta-parent parent-meta)))))))


(ert-deftest phpinspect-bmap-tokens-overlapping ()
  (let ((bmap (phpinspect-make-bmap)))
    (phpinspect-bmap-register bmap 9  20 '(:node3))
    (phpinspect-bmap-register bmap 21  44 '(:node4))
    (phpinspect-bmap-register bmap 20  200 '(:node2))
    (phpinspect-bmap-register bmap 9 200 '(:node1))
    (phpinspect-bmap-register bmap 1  300 '(:root))

    (let ((result (phpinspect-bmap-tokens-overlapping bmap 22)))
      (should (equal '((:node4) (:node2) (:node1))
                     (mapcar #'phpinspect-meta-token result))))))

(ert-deftest phpinspect-bmap-register ()
  (let* ((bmap (phpinspect-make-bmap))
         (token1 `(:word "foo"))
         (token2 `(:word "bar"))
         (token3 `(:block ,token1 ,token2))
         (token4 `(:list ,token3)))
    (phpinspect-bmap-register bmap 10 20 token1)
    (phpinspect-bmap-register bmap 20 30 token2)
    (phpinspect-bmap-register bmap 9 31 token3)
    (phpinspect-bmap-register bmap 8 32 token4)

    (should (phpinspect-bmap-token-meta bmap token1))
    (should (phpinspect-bmap-token-meta bmap token2))
    (should (phpinspect-bmap-token-meta bmap token3))
    (should (phpinspect-bmap-token-meta bmap token4))))
