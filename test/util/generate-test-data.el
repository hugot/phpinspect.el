
(require 'phpinspect)

(let ((here (file-name-directory
             (or load-file-name
                 buffer-file-name))))
  (dolist (file (directory-files (concat here "/../fixtures" ) t "\\.php$"))
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
        (insert (prin1-to-string (phpinspect--index-tokens index-class)))
        (write-file (concat here "/../fixtures/" class "-indexed.eld"))))))
