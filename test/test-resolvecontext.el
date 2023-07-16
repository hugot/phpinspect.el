
(require 'phpinspect)

(ert-deftest phinspect-get-resolvecontext ()
  (let* ((ctx (phpinspect-make-pctx :incremental t))
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

    (let ((rctx (phpinspect-get-resolvecontext bmap 317)))
      (should (phpinspect--resolvecontext-subject rctx))
      (should (phpinspect--resolvecontext-enclosing-tokens rctx)))))
