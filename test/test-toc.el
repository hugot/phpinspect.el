;;; test-edtrack.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

(require 'phpinspect-toc)
(require 'phpinspect-splayt)
(require 'phpinspect-meta)

(ert-deftest phpinspect-make-toc ()
  (let ((tokens (phpinspect-make-splayt))
        toc)
    (phpinspect-splayt-insert tokens 1 (phpinspect-make-meta nil 1 20 "" 'token1))
    (phpinspect-splayt-insert tokens 40 (phpinspect-make-meta nil 40 45 "" 'token2))
    (phpinspect-splayt-insert tokens 55 (phpinspect-make-meta nil 55 70 "" 'token3))

    (setq toc (phpinspect-make-toc tokens))

    (should (= 3 (hash-table-count (phpinspect-toc-table toc))))
    (should (= 3 (length (phpinspect-splayt-to-list (phpinspect-toc-tree toc)))))))

(ert-deftest phpinspect-update-toc ()
  (let ((tokens (phpinspect-make-splayt))
        (root (phpinspect-make-meta nil 1 200 "" 'root))
        (new-root (phpinspect-make-meta nil 1 400 "" 'root))
        (tok1 (phpinspect-make-meta nil 1 20 "" 'token1))
        (tok2 (phpinspect-make-meta nil 40 45 "" 'token2))
        (tok3 (phpinspect-make-meta nil 55 70 "" 'token3))
        (tok4 (phpinspect-make-meta nil 71 91 "" 'token4))
        new-tokens toc)

    (phpinspect-meta-set-parent tok1 root)
    (phpinspect-meta-set-parent tok2 root)
    (phpinspect-meta-set-parent tok3 root)

    (phpinspect-splayt-insert tokens 1 tok1)
    (phpinspect-splayt-insert tokens 40 tok2)
    (phpinspect-splayt-insert tokens 55 tok3)

    (setq toc (phpinspect-make-toc tokens))

    (phpinspect-meta-set-parent tok2 new-root)
    (phpinspect-meta-set-parent tok3 new-root)
    (phpinspect-meta-set-parent tok4 new-root)

    (setq new-tokens (phpinspect-make-splayt))
    (phpinspect-splayt-insert new-tokens 71 tok4)

    (pcase-let ((`(,result-new ,result-deleted) (phpinspect-toc-update toc new-tokens new-root)))
      (should (= 1 (length result-new)))
      (should (= 1 (length result-deleted)))

      (should (eq tok1 (car result-deleted)))
      (should (eq tok4 (car result-new))))

    (should (equal '(token2 token3)
                   (mapcar #'phpinspect-meta-token (phpinspect-toc-tokens-in-region toc 0 70))))

    (should (equal '(token2 token3 token4)
                   (mapcar #'phpinspect-meta-token (phpinspect-toc-tokens-in-region toc 0 91))))))
