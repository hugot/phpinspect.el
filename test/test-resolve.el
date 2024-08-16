
(require 'ert)
(require 'phpinspect-resolve)
(require 'phpinspect)
(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))

(ert-deftest phpinspect-get-variable-type-in-block-nested-array ()
  (let* ((code "class Foo { function a(\\Thing $baz) { $foo = [[$baz]]; foreach ($foo[0] as $bar) {$bar->")
         (bmap (phpinspect-parse-string-to-bmap code))
         (project (phpinspect--make-dummy-project))
         (context (phpinspect-get-resolvecontext project bmap (length code))))

    (let* ((function-token (seq-find #'phpinspect-function-p
                                     (phpinspect--resolvecontext-enclosing-tokens context)))
           (result (phpinspect-get-variable-type-in-block
                    context "bar"
                    (phpinspect-function-block function-token)
                    (phpinspect--make-type-resolver-for-resolvecontext context)
                    (phpinspect-function-argument-list function-token))))

      (should (phpinspect--type= (phpinspect--make-type :name "\\Thing")
                                 result)))))

(ert-deftest phpinspect-get-variable-type-in-block-array-access ()
  (let* ((code "class Foo { function a(\\Thing $baz) { $foo = []; $foo[] = $baz; $bar = $foo[0]; $bork = [$foo[0]]; $bark = $bork[0]; $borknest = [$bork]; $barknest = $borknest[0][0]; }}")
         (bmap (phpinspect-parse-string-to-bmap code))
         (project (phpinspect--make-dummy-project))
         (context (phpinspect-get-resolvecontext project bmap (length code))))

    (let* ((function-token (car (phpinspect--resolvecontext-enclosing-tokens context)))
           (result1 (phpinspect-get-variable-type-in-block
                     context "bar"
                     (phpinspect-function-block function-token)
                     (phpinspect--make-type-resolver-for-resolvecontext context)
                     (phpinspect-function-argument-list function-token)))
           (result2 (phpinspect-get-variable-type-in-block
                     context "bark"
                     (phpinspect-function-block function-token)
                     (phpinspect--make-type-resolver-for-resolvecontext context)
                     (phpinspect-function-argument-list function-token))))

      (should (phpinspect--type= (phpinspect--make-type :name "\\Thing")
                                 result1))
      (should (phpinspect--type= (phpinspect--make-type :name "\\Thing")
                                 result2)))))

(ert-deftest phpinspect-get-variable-type-in-block-array-foreach-self-referential ()
  (let* ((code "class Foo { function a(\\Thing $baz) { $foo = []; $foo[] = $baz; foreach ($foo as $bar) {$bar = $bar; $bar->")
         (bmap (phpinspect-parse-string-to-bmap code))
         (project (phpinspect--make-dummy-project))
         (context (phpinspect-get-resolvecontext project bmap (length code))))

    (let* ((function-token (seq-find #'phpinspect-function-p
                                     (phpinspect--resolvecontext-enclosing-tokens context)))
           (result (phpinspect-get-variable-type-in-block
                    context "bar"
                    (phpinspect-function-block function-token)
                    (phpinspect--make-type-resolver-for-resolvecontext context)
                    (phpinspect-function-argument-list function-token))))

      (should (phpinspect--type= (phpinspect--make-type :name "\\Thing")
                                 result)))))
