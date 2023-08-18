;;; test-eldoc.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

(require 'phpinspect)
(require 'phpinspect-eldoc)

(ert-deftest phpinspect-eld-method-call ()
  (with-temp-buffer
    (phpinspect-ensure-worker)
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
           (phpinspect-project-root-function (lambda () "phpinspect-test"))
           (phpinspect-eldoc-word-width 100)
           (buffer (phpinspect-make-buffer :buffer (current-buffer)))
           second-arg-pos inside-nested-list-pos first-arg-pos)
      (phpinspect-cache-project-class
       (phpinspect-current-project-root)
       (cdar (alist-get 'classes (cdr index))))

      (insert php-code)
      (backward-char)
      (setq second-arg-pos (point))
      (backward-char 6)
      (setq inside-nested-list-pos (point))
      (backward-char 8)
      (setq first-arg-pos (point))

      (let ((result (phpinspect-eldoc-query-execute
                     (phpinspect-make-eldoc-query :point second-arg-pos :buffer buffer))))
        (should (phpinspect-function-doc-p result))
        (should (= 1 (phpinspect-function-doc-arg-pos result)))
        (should (string= "getThis" (phpinspect--function-name (phpinspect-function-doc-fn result))))

        (setq result (phpinspect-eldoc-query-execute
                      (phpinspect-make-eldoc-query :point inside-nested-list-pos :buffer buffer)))
        (should-not result)

        (setq result (phpinspect-eldoc-query-execute
                      (phpinspect-make-eldoc-query :point first-arg-pos :buffer buffer)))
        (should (phpinspect-function-doc-p result))
        (should (= 0 (phpinspect-function-doc-arg-pos result)))
        (should (string= "getThis" (phpinspect--function-name (phpinspect-function-doc-fn result))))))))

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
         (phpinspect-project-root-function (lambda () "phpinspect-test"))
         (phpinspect-eldoc-word-width 100))
    (phpinspect-cache-project-class
     (phpinspect-current-project-root)
     (cdar (alist-get 'classes (cdr index))))

    (should (string= "getThis: ($moment DateTime, $thing Thing, $other): Thing"
                   (with-temp-buffer
                     (insert php-code)
                     (backward-char)
                     (setq-local phpinspect-current-buffer
                                 (phpinspect-make-buffer :buffer (current-buffer)))
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
         (phpinspect-project-root-function (lambda () "phpinspect-test"))
         (phpinspect-eldoc-word-width 100))
    (phpinspect-cache-project-class
     (phpinspect-current-project-root)
     (cdar (alist-get 'classes (cdr index))))

    (should (string= "doThing: ($moment DateTime, $thing Thing, $other): Thing"
                   (with-temp-buffer
                     (insert php-code)
                     (setq-local phpinspect-current-buffer
                                 (phpinspect-make-buffer :buffer (current-buffer)))
                     (phpinspect-eldoc-function))))))
