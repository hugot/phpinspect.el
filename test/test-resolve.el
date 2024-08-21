
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
                     "if ($baz = $banana->bar) { $baz"
                     "if ($baz = $banana->bar . 'a string') { $baz"))
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


(ert-deftest phpinspect-get-variable-type-in-block-typecast ()
  (let ((base-code "$foo = new \\DateTime();")
        (paths (list "$foo = (string) $foo; $foo"
                     "((Foo) $foo)->bar"
                     "$baz = (string) $foo; $baz"
                     "if ($baz = (string) $banana->bar) { $baz"))
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

(ert-deftest phpinspect-get-variable-type-in-block-function-return ()
  (let ((base-code "$bar = foo()")
        (paths (list (cons ";$bar" "Foo")
                     (cons ";$bar->baz" "string")
                     (cons "->baz" "string")
                     (cons "; curl('aaa' . $bar->baz" "string")))
        (project (phpinspect--make-dummy-project)))

    (phpinspect-project-add-index
     project
     (phpinspect--index-tokens
      (phpinspect-parse-string "class Foo { public string $baz; } function foo(): Foo { return 'bla'; }")))

    (dolist (path paths)
      (let* ((code (concat base-code (car path)))
             (bmap (phpinspect-parse-string-to-bmap code))
             (context (phpinspect-get-resolvecontext project bmap (length code)))
             (result (phpinspect-resolve-type-from-context context)))

        (should result)
        (should (phpinspect--type= (phpinspect--make-type :name (concat "\\" (cdr path)))
                                   result))))))

(ert-deftest phpinspect-resolve-type-from-context-static-method ()
  (with-temp-buffer
    (insert "
class Thing
{
    static function doThing(\\DateTime $moment, Thing $thing, $other): static
    {
        return $this;
    }

    function doStuff()
    {
        self::doThing()->")
  (let* ((bmap (phpinspect-make-bmap))
         (tokens (phpinspect-with-parse-context (phpinspect-make-pctx :incremental t
                                                                      :bmap bmap)
                   (phpinspect-parse-current-buffer)))
         (index (phpinspect--index-tokens tokens))
         (phpinspect-project-root-function (lambda () "phpinspect-test"))
         (phpinspect-eldoc-word-width 100)
         (project (phpinspect--make-dummy-project))
         (context (phpinspect-get-resolvecontext project bmap (point))))

    (phpinspect-purge-cache)
    (phpinspect-project-add-index project index)

    (should (phpinspect--type= (phpinspect--make-type :name "\\Thing")
                               (phpinspect-resolve-type-from-context
                                context
                                (phpinspect--make-type-resolver-for-resolvecontext
                                 context)))))))

(ert-deftest phpinspect-resolve-type-from-context-static-method-with-preceding-words ()
  (with-temp-buffer
    (insert "
class Thing
{
    static function doThing(\\DateTime $moment, Thing $thing, $other): static
    {
        return $this;
    }

    function doStuff()
    {
        if (true) {
            return self::doThing()->")
    (let* ((bmap (phpinspect-make-bmap))
           (tokens (phpinspect-with-parse-context (phpinspect-make-pctx :incremental t
                                                                        :bmap bmap)
                     (phpinspect-parse-current-buffer)))
           (index (phpinspect--index-tokens tokens))
           (phpinspect-eldoc-word-width 100)
           (project (phpinspect--make-dummy-project))
           (context (phpinspect-get-resolvecontext project bmap (point))))
      (phpinspect-purge-cache)
      (phpinspect-project-add-index project index)

      (should (phpinspect--type= (phpinspect--make-type :name "\\Thing")
                                 (phpinspect-resolve-type-from-context
                                  context
                                  (phpinspect--make-type-resolver-for-resolvecontext
                                   context)))))))
