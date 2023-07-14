
(require 'phpinspect-bmap)

(ert-deftest phpinspect-bmap-overlay ()
  (let ((bmap (phpinspect-make-bmap))
        (bmap2 (phpinspect-make-bmap)))

    (phpinspect-bmap-register bmap 10 20 'token)
    (phpinspect-bmap-register bmap2 20 24 'token2)

    (should (phpinspect-bmap-token-starting-at bmap 10))

    (phpinspect-bmap-overlay
     bmap2 bmap (phpinspect-bmap-token-starting-at bmap 10) -3)

    (should (eq 'token2 (phpinspect--meta-token
                         (phpinspect-bmap-token-starting-at bmap2 20))))
    (should (eq 'token (phpinspect--meta-token
                        (phpinspect-bmap-token-starting-at bmap2 7))))

    (should (phpinspect-bmap-token-meta bmap 'token))
    (should (phpinspect-bmap-token-meta bmap2 'token2))
    (should (phpinspect-bmap-token-meta bmap2 'token))))

(ert-deftest phpinspect-bmap-nest-parent ()
  (let ((bmap (phpinspect-make-bmap)))
    (phpinspect-bmap-register bmap 10 20 'child)
    (phpinspect-bmap-register bmap 5 25 'parent)

    (let ((child (phpinspect-bmap-token-meta bmap 'child)))
      (should
       (eq 'parent (phpinspect--meta-token
                           (phpinspect--meta-parent child)))))))
