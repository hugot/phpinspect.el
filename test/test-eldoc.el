;;; test-eldoc.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

(require 'phpinspect)
(require 'phpinspect-eldoc)
(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))

(ert-deftest phpinspect-eld-method-call ()
  (setq phpinspect-load-stubs nil)
  (with-temp-buffer
    (phpinspect-purge-cache)

    (let* ((php-code "<?php
class Thing
{

    function getThis(\\DateTime $moment, Thing $thing, $other): static
    {
        return $this;
    }

    function doStuff()
    {
        $this->getThis(new \\DateTime(), bla)")
           (phpinspect-project-root-function (lambda () "phpinspect-test"))
           (phpinspect-eldoc-word-width 100)
           (project (phpinspect--make-project :autoload (phpinspect-make-autoloader) :worker 'nil-worker))
           (buffer (phpinspect-make-buffer :buffer (current-buffer) :-project project))
           second-arg-pos inside-nested-list-pos first-arg-pos)
      (setq-local phpinspect-current-buffer buffer)
      (insert php-code)

      (backward-char)
      (setq second-arg-pos (point))
      (backward-char 6)
      (setq inside-nested-list-pos (point))
      (backward-char 8)
      (setq first-arg-pos (point))

      (phpinspect-buffer-reindex buffer)

      ;; Strategy should not trigger inside constructor/function arguments
      (let ((query (phpinspect-make-eldoc-query :point inside-nested-list-pos :buffer buffer))
              (strat (phpinspect-make-eld-function-args))
              (rctx (phpinspect-get-resolvecontext project (phpinspect-buffer-parse-map buffer) inside-nested-list-pos)))
        (should-not (phpinspect-eld-strategy-execute strat query rctx)))

      (dolist (expected (list (cons second-arg-pos 1) (cons first-arg-pos 0)))
        ;; Test strat separately to ensure it is not erroneous
        (let ((query (phpinspect-make-eldoc-query :point (car expected) :buffer buffer))
              (strat (phpinspect-make-eld-function-args))
              (rctx (phpinspect-get-resolvecontext project (phpinspect-buffer-parse-map buffer) (car expected))))

          ;; Subject is correct
          (should (phpinspect-word-p (car (phpinspect--resolvecontext-subject rctx))))

          ;; Enclosing token is correct
          (should (phpinspect-list-p (car (phpinspect--resolvecontext-enclosing-tokens rctx))))

          ;; Statement is correctly determined
          (should (equal (cdr (phpinspect-parse-string "$this->getThis(new \\DateTime(), bla)"))
                         (mapcar #'phpinspect-meta-token
                                 (phpinspect--determine-function-call-statement
                                  rctx query (car (phpinspect--resolvecontext-enclosing-metadata
                                                   rctx))))))
          ;; Strategy correctly signals support
          (should (phpinspect-eld-strategy-supports strat query rctx))

          ;; Strategy correctly returns result
          (should (phpinspect-eld-strategy-execute strat query rctx)))

        ;; Test query execution
        (let ((result (seq-find #'phpinspect-function-doc-p
                                (phpinspect-eldoc-query-execute
                                 (phpinspect-make-eldoc-query :point (car expected) :buffer buffer)))))

          (should (phpinspect-function-doc-p result))
          (should (= (cdr expected) (phpinspect-function-doc-arg-pos result)))
          (should (string= "getThis" (phpi-fn-name (phpinspect-function-doc-fn result)))))))))


(ert-deftest phpinspect-eldoc-function-for-object-method ()
  (phpinspect-purge-cache)
  (let* ((php-code "
class Thing
{
    function getThis(\\DateTime $moment, Thing $thing, $other): static
    {
        return $this;
    }

    function doStuff()
    {
        $this->getThis(new \\DateTime(), bla)")
         (tokens (phpinspect-parse-string php-code))
         (index (phpinspect--index-tokens tokens))
         (project (phpinspect--make-dummy-project))
         (phpinspect-eldoc-word-width 100))
    (phpinspect-project-add-index project index)

    (should (string= "getThis: ($moment DateTime, $thing Thing, $other): Thing"
                   (with-temp-buffer
                     (insert php-code)
                     (backward-char)
                     (setq-local phpinspect-current-buffer
                                 (phpinspect-make-buffer :buffer (current-buffer) :-project project))
                     (phpinspect-buffer-parse phpinspect-current-buffer)
                     (phpinspect-eldoc-function))))))

(ert-deftest phpinspect-eldoc-function-for-static-method ()
  (phpinspect-purge-cache)
  (let* ((php-code "
class Thing
{
    static function doThing(\\DateTime $moment, Thing $thing, $other): static
    {
        return $this;
    }

    function doStuff()
    {
        self::doThing(")
         (tokens (phpinspect-parse-string php-code))
         (index (phpinspect--index-tokens tokens))
         (project (phpinspect--make-dummy-project))
         (phpinspect-eldoc-word-width 100))
    (phpinspect-project-add-index project index)

    (should (string= "doThing: ($moment DateTime, $thing Thing, $other): Thing"
                   (with-temp-buffer
                     (insert php-code)
                     (setq-local phpinspect-current-buffer
                                 (phpinspect-make-buffer :buffer (current-buffer)))
                     (phpinspect-eldoc-function))))))
