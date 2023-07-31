

(require 'phpinspect-splayt)

(let ((here (file-name-directory (or load-file-name buffer-file-name)))
      (tree (phpinspect-make-splayt)))
  (message "Splay tree 10000 insertions:")
  (garbage-collect)
  (benchmark
   1 '(dotimes (i 10000)
        (phpinspect-splayt-insert tree i 'value)))

  (message "Splay tree 10000 lookups:")
  (garbage-collect)
  (benchmark
   1 '(dotimes (i 10000)
        (phpinspect-splayt-find tree i))))


(let (map)
  (message "Hashtable 10000 insertions:")
  (garbage-collect)
  (benchmark
   1 '(progn
        (setq map (make-hash-table :test #'eq :size 10000 :rehash-size 1.5))
        (dotimes (i 10000)
        (puthash i 'value map))))

  (message "Hashtable 10000 lookups:")
  (garbage-collect)
  (benchmark
   1 '(dotimes (i 10000)
        (gethash i map))))
