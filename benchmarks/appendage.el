


(message "20000 appendages using nconc")
(garbage-collect)
(benchmark
 1 '(let (list)
      (dotimes (i 20000)
        (setq list (nconc list (list i))))

      list))

(message "20000 appendages using push + nreverse")
(garbage-collect)
(benchmark
 1 '(let (list)
      (dotimes (i 20000)
        (push i list))

      (nreverse list)))

(message "20000 appendages using rear pointer")
(garbage-collect)
(benchmark
 1 '(let* ((list (cons nil nil))
           (rear list))

      (dotimes (i 20000)
        (setq rear (setcdr rear (cons i nil))))

      (cdr list)))
