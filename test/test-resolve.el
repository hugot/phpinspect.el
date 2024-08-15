
(require 'ert)
(require 'phpinspect-resolve)
(require 'phpinspect)
(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))


(ert-deftest phpinspect-get-variable-type-in-block-array-access ()
  (let* ((code "class Foo { function a(\\Thing $baz) { $foo = []; $foo[] = $baz; $bar = $foo[0]; $bork = [$foo[0]]; $bark = $bork[0]; $borknest = [$bork]; $barknest = $borknest[0][0]; }}")
         (tokens (phpinspect-parse-string-to-bmap code))
         (context (phpinspect-get-resolvecontext tokens (- (length code) 4)))
         (project-root "could never be a real project root")
         (phpinspect-project-root-function
          (lambda (&rest _ignored) project-root))
         (project (phpinspect--make-project
                              :fs (phpinspect-make-virtual-fs)
                              :root project-root
                              :worker (phpinspect-make-worker))))

    (puthash project-root project (phpinspect--cache-projects phpinspect-cache))

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
         (context (phpinspect-get-resolvecontext bmap (length code)))
         (project-root "could never be a real project root")
         (phpinspect-project-root-function
          (lambda (&rest _ignored) project-root))
         (project (phpinspect--make-project
                   :fs (phpinspect-make-virtual-fs)
                   :root project-root
                   :worker (phpinspect-make-worker))))

    (puthash project-root project (phpinspect--cache-projects phpinspect-cache))

    (let* ((function-token (seq-find #'phpinspect-function-p
                                     (phpinspect--resolvecontext-enclosing-tokens context)))
           (result (phpinspect-get-variable-type-in-block
                    context "bar"
                    (phpinspect-function-block function-token)
                    (phpinspect--make-type-resolver-for-resolvecontext context)
                    (phpinspect-function-argument-list function-token))))

      (should (phpinspect--type= (phpinspect--make-type :name "\\Thing")
                                 result)))))
