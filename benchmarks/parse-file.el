
(require 'phpinspect)


(let ((here (file-name-directory (or load-file-name buffer-file-name))))

  (with-temp-buffer
    (setq-local phpinspect-current-buffer (phpinspect-make-buffer :buffer (current-buffer)))

    (insert-file-contents (concat here "/Response.php"))

    (message "Metadata parse:")
    (benchmark 1 '(phpinspect-buffer-parse phpinspect-current-buffer))

    (message "Bare parse:")
    (benchmark 1 '(phpinspect-parse-current-buffer))

    (goto-char (floor (/ (point-max) 2 )))
    (insert "abc")
    (phpinspect-buffer-register-edit phpinspect-current-buffer (- (point) 3) (point) 0)

    (message "Metadata parse incremental:")
    (benchmark 1 '(phpinspect-buffer-parse phpinspect-current-buffer))))
