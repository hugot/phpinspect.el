
(require 'phpinspect)
(require 'phpinspect-index)
(require 'phpinspect-serialize)

(let ((here (file-name-directory (macroexp-file-name)))
      (print-length 1000)
      (print-level 1000))
  (dolist (file (directory-files (expand-file-name "../fixtures" here) t "\\.php\\'"))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (let ((result (phpinspect-parse-current-buffer)))
        (with-temp-buffer
          (insert (prin1-to-string result))
          (write-file (concat (string-remove-suffix ".php"  file) ".eld"))))))

  (dolist (class '("IndexClass1" "IndexClass2"))
    (let ((index-class
           (with-temp-buffer
             (insert-file-contents-literally (concat here "/../fixtures/" class ".eld"))
             (read (current-buffer)))))
      (with-temp-buffer
        (insert (prin1-to-string (phpinspect--serialize-root-index
                                  (phpinspect--index-tokens index-class))))
        (write-file (concat here "/../fixtures/" class "-indexed.eld"))))))
