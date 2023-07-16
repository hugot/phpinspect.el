
(require 'phpinspect-parser)

(ert-deftest phpinspect-parse-bmap ()
  (let* ((ctx (phpinspect-make-pctx :incremental t))
         (code "
class TestClass {
    public function getCurrentStatisticAction(): JsonResponse
    {
        $statistic = $this->repository->getCurrentStatistic();
        if (!$this->authorization->isGranted(EntityAction::VIEW, $statistic)) {
            return $this->responder->respondUnauthorized();
        }

        return $this->responder->respond($statistic);
    }
}")
         (bmap))
    (phpinspect-with-parse-context ctx
      (phpinspect-parse-string code))
    (setq bmap (phpinspect-pctx-bmap ctx))

    (let ((enclosing (phpinspect-bmap-tokens-overlapping bmap 350))
          (parent))
      (should enclosing)
      (should (phpinspect-variable-p (phpinspect-meta-token (car enclosing))))
      (should (string= "statistic" (cadr (phpinspect-meta-token (car enclosing)))))
      (should (phpinspect-meta-parent (car enclosing)))

      (setq parent (phpinspect-meta-parent (car enclosing)))
      (should (phpinspect-list-p (phpinspect-meta-token parent)))
      (should (phpinspect-block-p (phpinspect-meta-token (phpinspect-meta-parent parent)))))))
