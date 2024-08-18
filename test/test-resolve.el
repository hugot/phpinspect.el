
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

(ert-deftest phpinspect-get-variable-type-in-block-wrapped-in-list ()
  (let* ((code "function () { $bar = (new Foo())->bar")
         (bmap (phpinspect-parse-string-to-bmap code))
         (project (phpinspect--make-dummy-project))
         (context (phpinspect-get-resolvecontext project bmap (length code)))
         (result (phpinspect-resolve-type-from-context context)))
    (phpinspect-project-add-index
     project
     (phpinspect--index-tokens
      (phpinspect-parse-string "class Foo { public string $bar; }")))

    (should (phpinspect--type= (phpinspect--make-type :name "\\string")
                               (phpinspect-resolve-type-from-context context)))))

(ert-deftest phpinspect-get-variable-type-in-block-assignment-wrapped-in-list ()
  (let ((base-code "function () { $bar = ($banana = new Foo())")
        (paths (list "->bar"
                     "; $banana->bar"
                     "; $bar->bar"))
        (project (phpinspect--make-dummy-project)))

    (phpinspect-project-add-index
     project
     (phpinspect--index-tokens
      (phpinspect-parse-string "class Foo { public string $bar; }")))

    (dolist (path paths)
      (let* ((code (concat base-code path))
             (bmap (phpinspect-parse-string-to-bmap code))
             (context (phpinspect-get-resolvecontext project bmap (length code)))
             (result (phpinspect-resolve-type-from-context context)))

        (should (phpinspect--type= (phpinspect--make-type :name "\\string")
                                   (phpinspect-resolve-type-from-context context)))))))

(ert-deftest phpinspect-get-variable-type-in-block-assignment-wrapped-in-if-condition ()
  (let ((base-code "function () { if ($bar = ($banana = new Foo())) {")
        (paths (list "$banana->bar"
                     "$bar->bar"
                     "$baz = new \\DateTime();} $banana->bar"
                     "if ($baz = $bar->bar) { $baz"))
        (project (phpinspect--make-dummy-project)))

    (phpinspect-project-add-index
     project
     (phpinspect--index-tokens
      (phpinspect-parse-string "class Foo { public string $bar; }")))

    (dolist (path paths)
      (let* ((code (concat base-code path))
             (bmap (phpinspect-parse-string-to-bmap code))
             (context (phpinspect-get-resolvecontext project bmap (length code)))
             (result (phpinspect-resolve-type-from-context context)))

        (should (phpinspect--type= (phpinspect--make-type :name "\\string")
                                   (phpinspect-resolve-type-from-context context)))))))


(ert-deftest phpinspect-get-variable-type-in-block-var-annotation ()
  (let ((base-code "/* @var \\Foo $banana  */ $banana = $undefined;")
        (paths (list "$banana->bar"
                     "if ($baz = $banana) { $bar = $banana; } $bar->bar"
                     "$baz = new \\DateTime(); /** @var \\DateTime $pear */} $banana->bar"
                     "if ($baz = $banana->bar) { $baz"))
        (project (phpinspect--make-dummy-project)))

    (phpinspect-project-add-index
     project
     (phpinspect--index-tokens
      (phpinspect-parse-string "class Foo { public string $bar; }")))

    (dolist (path paths)
      (let* ((code (concat base-code path))
             (bmap (phpinspect-parse-string-to-bmap code))
             (context (phpinspect-get-resolvecontext project bmap (length code)))
             (result (phpinspect-resolve-type-from-context context)))

        (should result)
        (should (phpinspect--type= (phpinspect--make-type :name "\\string")
                                   result))))))
