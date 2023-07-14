
(require 'phpinspect)


(let ((here (file-name-directory (or load-file-name buffer-file-name))))

  (with-temp-buffer
    ;; (setq-local phpinspect-current-buffer (phpinspect-make-buffer :buffer (current-buffer)))

    (insert-file-contents (concat here "/Response.php"))

    (message "Bmap warmup parse:")
    (phpinspect-with-parse-context (phpinspect-make-pctx :incremental t)
      (benchmark 1 '(phpinspect-parse-current-buffer)))

    (let ((bmap (phpinspect-make-bmap))
          (bmap2 (phpinspect-make-bmap)))
      (message "Bmap parse:")
      (phpinspect-with-parse-context (phpinspect-make-pctx :incremental t :bmap bmap)
        (benchmark 1 '(phpinspect-parse-current-buffer)))

      (message "Bmap parse incremental:")
      (phpinspect-with-parse-context (phpinspect-make-pctx :incremental t :bmap bmap2 :previous-bmap bmap :edtrack (phpinspect-make-edtrack))
        (benchmark 1 '(phpinspect-parse-current-buffer)))

      (message "Bmap parse incremental repeat:")
      (phpinspect-with-parse-context (phpinspect-make-pctx :incremental t :previous-bmap bmap2 :edtrack (phpinspect-make-edtrack))
        (benchmark 1 '(phpinspect-parse-current-buffer))))

    ;; (message "Metadata parse:")
    ;; (benchmark 1 '(phpinspect-buffer-parse phpinspect-current-buffer))

    (message "Bare warmup parse:")
    (benchmark 1 '(phpinspect-parse-current-buffer))


    (message "Bare parse:")
    (benchmark 1 '(phpinspect-parse-current-buffer))))


    ;; (goto-char (floor (/ (point-max) 2 )))
    ;; (insert "abc")
    ;; (phpinspect-buffer-register-edit phpinspect-current-buffer (- (point) 3) (point) 0)

    ;; (message "Metadata parse incremental:")
    ;; (benchmark 1 '(phpinspect-buffer-parse phpinspect-current-buffer))))
