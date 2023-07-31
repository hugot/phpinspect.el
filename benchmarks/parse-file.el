
(require 'phpinspect-parser)

(defun phpinspect-parse-current-buffer ()
  (phpinspect-parse-buffer-until-point
   (current-buffer)
   (point-max)))

(let ((here (file-name-directory (or load-file-name buffer-file-name))))

  (with-temp-buffer
    (insert-file-contents (concat here "/Response.php"))

    (message "Incremental parse (warmup):")
    (phpinspect-with-parse-context (phpinspect-make-pctx :incremental t)
      (benchmark 1 '(phpinspect-parse-current-buffer)))

    (let ((bmap (phpinspect-make-bmap))
          (bmap2 (phpinspect-make-bmap)))
      (message "Incremental parse:")
      (phpinspect-with-parse-context (phpinspect-make-pctx :incremental t :bmap bmap)
        (benchmark 1 '(phpinspect-parse-current-buffer)))

      (garbage-collect)

      (message "Incremental parse (no edits):")
      (phpinspect-with-parse-context (phpinspect-make-pctx :incremental t :bmap bmap2 :previous-bmap bmap :edtrack (phpinspect-make-edtrack))
        (benchmark 1 '(phpinspect-parse-current-buffer)))

      (garbage-collect)

      (message "Incremental parse repeat (no edits):")
      (phpinspect-with-parse-context (phpinspect-make-pctx :incremental t :previous-bmap bmap2 :edtrack (phpinspect-make-edtrack))
        (benchmark 1 '(phpinspect-parse-current-buffer)))

      (garbage-collect)

      (let ((edtrack (phpinspect-make-edtrack))
            (bmap (phpinspect-make-bmap))
            (bmap-after (phpinspect-make-bmap)))
        ;; Fresh
        (phpinspect-with-parse-context (phpinspect-make-pctx :incremental t :bmap bmap)
          (phpinspect-parse-current-buffer))

        (message "Incremental parse after buffer edit:")
        ;; Removes closing curly brace of __construct
        (goto-char 9062)
        (delete-backward-char 1)

        (garbage-collect)

        (phpinspect-edtrack-register-edit edtrack 9061 9061 1)
        (phpinspect-with-parse-context (phpinspect-make-pctx :bmap bmap-after :incremental t :previous-bmap bmap :edtrack edtrack)
          (benchmark 1 '(phpinspect-parse-current-buffer)))

        (phpinspect-edtrack-clear edtrack)
        (insert "{")

        (phpinspect-edtrack-register-edit edtrack 9061 9062 0)
        ;; Mark region as edit without length deta
        (phpinspect-edtrack-register-edit edtrack 19552 19562 10)

        (garbage-collect)

        ;;(profiler-start 'cpu)
        (message "Incremental parse after 2 more edits:")
        (phpinspect-with-parse-context (phpinspect-make-pctx :incremental t :previous-bmap bmap-after :edtrack edtrack)
          (benchmark 1 '(phpinspect-parse-current-buffer)))

        ;; (save-current-buffer
        ;;   (profiler-stop)
        ;;   (profiler-report)
        ;;   (profiler-report-write-profile (concat here "/profile.txt")))
        )))

  (with-temp-buffer
    (insert-file-contents (concat here "/Response.php"))

    (garbage-collect)
    (message "Bare (no token reuse) parse (warmup):")
    (benchmark 1 '(phpinspect-parse-current-buffer))


    (garbage-collect)
    (message "Bare (no token reuse) parse:")
    (benchmark 1 '(phpinspect-parse-current-buffer))))
