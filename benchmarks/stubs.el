
(require 'phpinspect-cache)

(let (result)

  (message "Building and loading stub cache")
  (garbage-collect)
  (setq result
        (benchmark-run 1 (phpinspect-build-stub-cache)))
  (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))

  (message "Building stub cache")
  (garbage-collect)
  (setq result
        (benchmark-run 1 (phpinspect-build-stub-index)))
  (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))

  (message "Building and dumping stub cache")
  (garbage-collect)
  (setq result
        (benchmark-run 1 (phpinspect-dump-stub-index)))
  (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result))

  (message "Loading stub cache")
  (garbage-collect)
  (setq result
        (benchmark-run 1 (phpinspect-load-stub-index)))
  (message "Elapsed time: %f (%f in %d GC's)" (car result) (caddr result) (cadr result)))
