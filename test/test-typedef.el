

(require 'ert)
(require 'phpinspect-typedef)
(require 'phpinspect-index)
(require 'phpinspect-parser)
(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))

(ert-deftest phpinspect-typedef-set-index-simple ()
  (let* ((code "class A { function B(): C {} }")
         (index (phpinspect--index-tokens (phpinspect-parse-string code)))
         (class (cdar (alist-get 'classes index)))
         (typedef (phpinspect-make-typedef (alist-get 'class-name class))))
    (phpi-typedef-set-index typedef class)

    (should (phpi-typedef-get-methods typedef))
    (should (= 1 (length (phpi-typedef-get-methods typedef))))

    (should (eq (phpinspect-intern-name "B")
                (phpi-method-name (car (phpi-typedef-get-methods typedef)))))))

(ert-deftest phpinspect-typedef-subscribe ()
  (let* ((def1 (phpinspect-make-typedef (phpinspect--make-type :name "\\A")))
         (def2 (phpinspect-make-typedef (phpinspect--make-type :name "\\B")))
         (retriever (lambda (type)
                      (cond ((phpinspect--type= type (phpinspect--make-type :name "\\A"))
                             def1)
                            ((phpinspect--type= type (phpinspect--make-type :name "\\B"))
                             def2)))))

    (setf (phpi-typedef-retriever def1) retriever
          (phpi-typedef-retriever def2) retriever)
    (phpi-typedef-set-method def2 (phpinspect--make-function :name "test"))

    (phpi-typedef-update-extensions def1 (list (phpi-typedef-name def2)))

    (let ((method (phpi-typedef-get-method def1 (phpinspect-intern-name "test"))))
      (should method)
      (should-not (phpi-method-return-type method))

      (phpi-typedef-set-method def1 (phpinspect--make-function
                                     :name "test"
                                     :return-type (phpinspect--make-type :name "\\aaa")))

      (setq method (phpi-typedef-get-method def1 (phpinspect-intern-name "test")))
      (should method)
      (should (phpi-method-return-type method))

      (phpi-typedef-delete-method def1 (phpinspect-intern-name "test"))

      (setq method (phpi-typedef-get-method def1 (phpinspect-intern-name "test")))
      (should method)
      (should-not (phpi-method-return-type method))

      (phpi-typedef-update-extensions def1 nil)

      (setq method (phpi-typedef-get-method def1 (phpinspect-intern-name "test")))
      (should-not method))))

(ert-deftest phpinspect-typedef-variables ()
  (let ((def (phpinspect-make-typedef (phpinspect--make-type :name "\\test"))))

    (phpi-typedef-set-variable def (phpinspect--make-variable :name "test"))
    (phpi-typedef-set-variable def (phpinspect--make-variable :name "test2"))

    (let ((test1 (phpi-typedef-get-variable def "test"))
          (test2 (phpi-typedef-get-variable def "test2")))

      (should test1)
      (should (string= "test" (phpinspect--variable-name test1)))

      (should (phpi-typedef-get-variable def "test2"))
      (should (string= "test2" (phpinspect--variable-name test2))))))
