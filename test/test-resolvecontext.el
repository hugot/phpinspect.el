;; -*- lexical-binding: t; -*-

(require 'phpinspect-resolvecontext)
(require 'phpinspect)
(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))


(ert-deftest phinspect-get-resolvecontext ()
  (let* ((ctx (phpinspect-make-pctx :incremental t :bmap (phpinspect-make-bmap)))
         (code "
class TestClass {
    public function getCurrentStatisticAction(): JsonResponse
    {
        $statistic = $this->repository->getCurrentStatistic();
        if (!$this->authorization->isGranted(EntityAction::VIEW, $statistic)) {
            return $this->responder->respondUnauthorized();
        }

        $this->

        return $this->responder->respond($statistic);
    }
}")
         (bmap))
    (phpinspect-with-parse-context ctx
      (phpinspect-parse-string code))
    (setq bmap (phpinspect-pctx-bmap ctx))

    (let ((rctx (phpinspect-get-resolvecontext (phpinspect--make-dummy-project) bmap 317)))
      (should (phpinspect--resolvecontext-subject rctx))
      (should (phpinspect--resolvecontext-enclosing-tokens rctx)))))


(ert-deftest phpinspect-type-resolver-for-resolvecontext ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "IncompleteClass.php" phpinspect-test-php-file-directory))
    (let* ((bmap (phpinspect-parse-string-to-bmap (buffer-string)))
           (resolvecontext (phpinspect-get-resolvecontext (phpinspect--make-dummy-project) bmap (point-max)))
           (type-resolver (phpinspect--make-type-resolver-for-resolvecontext
                           resolvecontext)))

      (should (phpinspect--type= (phpinspect--make-type :name  "\\array")
                                 (funcall type-resolver
                                          (phpinspect--make-type :name "array"))))
      (should (phpinspect--type= (phpinspect--make-type :name  "\\array")
                                 (funcall type-resolver
                                          (phpinspect--make-type :name "\\array"))))
      (should (phpinspect--type= (phpinspect--make-type
                                  :name  "\\Symfony\\Component\\HttpFoundation\\Response")
                                 (funcall type-resolver (phpinspect--make-type :name "Response"))))
      (should (phpinspect--type= (phpinspect--make-type :name  "\\Response")
                                 (funcall type-resolver
                                          (phpinspect--make-type :name "\\Response"))))
      (should (phpinspect--type= (phpinspect--make-type :name  "\\App\\Controller\\GastonLagaffe")
                                 (funcall type-resolver
                                          (phpinspect--make-type :name "GastonLagaffe"))))
      (should (phpinspect--type=
               (phpinspect--make-type :name  "\\App\\Controller\\Dupuis\\GastonLagaffe")
               (funcall type-resolver
                        (phpinspect--make-type :name "Dupuis\\GastonLagaffe")))))))

(ert-deftest phpinspect-type-resolver-for-resolvecontext-namespace-block ()
  (with-temp-buffer
    (insert-file-contents (concat phpinspect-test-php-file-directory "/IncompleteClassBlockedNamespace.php"))
    (let* ((bmap (phpinspect-parse-string-to-bmap (buffer-string)))
           (resolvecontext (phpinspect-get-resolvecontext (phpinspect--make-dummy-project) bmap (point-max)))
           (type-resolver (phpinspect--make-type-resolver-for-resolvecontext
                           resolvecontext)))

      (should (phpinspect--type= (phpinspect--make-type :name "\\array")
                                 (funcall type-resolver (phpinspect--make-type :name "array"))))
      (should (phpinspect--type= (phpinspect--make-type :name  "\\array")
                                 (funcall type-resolver (phpinspect--make-type :name "\\array"))))
      (should (phpinspect--type= (phpinspect--make-type
                                  :name  "\\Symfony\\Component\\HttpFoundation\\Response")
                                 (funcall type-resolver (phpinspect--make-type :name "Response"))))
      (should (phpinspect--type= (phpinspect--make-type :name  "\\Response")
                                 (funcall type-resolver (phpinspect--make-type :name "\\Response"))))
      (should (phpinspect--type= (phpinspect--make-type :name  "\\App\\Controller\\GastonLagaffe")
                                 (funcall type-resolver (phpinspect--make-type
                                                         :name "GastonLagaffe"))))
      (should (phpinspect--type= (phpinspect--make-type
                                  :name  "\\App\\Controller\\Dupuis\\GastonLagaffe")
                                 (funcall type-resolver (phpinspect--make-type :name "Dupuis\\GastonLagaffe")))))))

(ert-deftest phpinspect-type-resolver-for-resolvecontext-multiple-namespace-blocks ()
  (let* ((resolvecontext (phpinspect--get-resolvecontext
                          (phpinspect-current-project)
                          (phpinspect-test-read-fixture-data
                           "IncompleteClassMultipleNamespaces")))
         (type-resolver (phpinspect--make-type-resolver-for-resolvecontext
                         resolvecontext)))

    (should (phpinspect--type= (phpinspect--make-type :name "\\array")
                               (funcall type-resolver
                                        (phpinspect--make-type :name "array"))))
    (should (phpinspect--type= (phpinspect--make-type :name  "\\array")
                               (funcall type-resolver
                                        (phpinspect--make-type :name "\\array"))))
    (should (phpinspect--type= (phpinspect--make-type
                                :name  "\\Symfony\\Component\\HttpFoundation\\Response")
                               (funcall type-resolver (phpinspect--make-type :name "Response"))))
    (should (phpinspect--type= (phpinspect--make-type :name  "\\Response")
                               (funcall type-resolver
                                        (phpinspect--make-type :name "\\Response"))))
    (should (phpinspect--type= (phpinspect--make-type :name  "\\App\\Controller\\GastonLagaffe")
                               (funcall type-resolver (phpinspect--make-type :name "GastonLagaffe"))))
    (should (phpinspect--type= (phpinspect--make-type
                                :name  "\\App\\Controller\\Dupuis\\GastonLagaffe")
                               (funcall type-resolver (phpinspect--make-type
                                                       :name "Dupuis\\GastonLagaffe"))))))
