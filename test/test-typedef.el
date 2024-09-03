

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

(ert-deftest phpinspect-typedef-subscribe-static ()
  (let* ((def1 (phpinspect-make-typedef (phpinspect--make-type :name "\\A")))
         (def2 (phpinspect-make-typedef (phpinspect--make-type :name "\\B")))
         (retriever (lambda (type)
                      (cond ((phpinspect--type= type (phpinspect--make-type :name "\\A"))
                             def1)
                            ((phpinspect--type= type (phpinspect--make-type :name "\\B"))
                             def2)))))

    (setf (phpi-typedef-retriever def1) retriever
          (phpi-typedef-retriever def2) retriever)
    (phpi-typedef-set-static-method def2 (phpinspect--make-function :name "test"))

    (phpi-typedef-update-extensions def1 (list (phpi-typedef-name def2)))

    (let ((method (phpi-typedef-get-static-method def1 (phpinspect-intern-name "test"))))
      (should method)
      (should-not (phpi-method-return-type method))

      (phpi-typedef-set-static-method def1 (phpinspect--make-function
                                     :name "test"
                                     :return-type (phpinspect--make-type :name "\\aaa")))

      (setq method (phpi-typedef-get-static-method def1 (phpinspect-intern-name "test")))
      (should method)
      (should (phpi-method-return-type method))

      (phpi-typedef-delete-method def1 (phpinspect-intern-name "test"))

      (setq method (phpi-typedef-get-static-method def1 (phpinspect-intern-name "test")))
      (should method)
      (should-not (phpi-method-return-type method))

      (phpi-typedef-update-extensions def1 nil)

      (setq method (phpi-typedef-get-static-method def1 (phpinspect-intern-name "test")))
      (should-not method))))

(ert-deftest phpinspect-typedef-subscribe-multi-ancestor ()
  (let* ((def1 (phpinspect-make-typedef (phpinspect--make-type :name "\\A")))
         (def2 (phpinspect-make-typedef (phpinspect--make-type :name "\\B")))
         (def3 (phpinspect-make-typedef (phpinspect--make-type :name "\\C")))
         (retriever (lambda (type)
                      (cond ((phpinspect--type= type (phpi-typedef-name def1))
                             def1)
                            ((phpinspect--type= type (phpi-typedef-name def2))
                             def2)
                            ((phpinspect--type= type (phpi-typedef-name def3))
                             def3)))))

    (setf (phpi-typedef-retriever def1) retriever
          (phpi-typedef-retriever def2) retriever
          (phpi-typedef-retriever def3) retriever)


    (phpi-typedef-update-extensions def1 (list (phpi-typedef-name def2)))
    (phpi-typedef-update-extensions def2 (list (phpi-typedef-name def3)))

    (phpi-typedef-set-method def3 (phpinspect--make-function :name "test"))
    (phpi-typedef-trigger-subscriber-update def3)


    (let ((method (phpi-typedef-get-method def1 (phpinspect-intern-name "test"))))
      (should method)
      (should-not (phpi-method-return-type method)))))

(ert-deftest phpinspect-typedef-subscribe-multi-ancestor-no-manual-trigger ()
  (let* ((def1 (phpinspect-make-typedef (phpinspect--make-type :name "\\A")))
         (def2 (phpinspect-make-typedef (phpinspect--make-type :name "\\B")))
         (def3 (phpinspect-make-typedef (phpinspect--make-type :name "\\C")))
         (retriever (lambda (type)
                      (cond ((phpinspect--type= type (phpi-typedef-name def1))
                             def1)
                            ((phpinspect--type= type (phpi-typedef-name def2))
                             def2)
                            ((phpinspect--type= type (phpi-typedef-name def3))
                             def3)))))

    (setf (phpi-typedef-retriever def1) retriever
          (phpi-typedef-retriever def2) retriever
          (phpi-typedef-retriever def3) retriever)


    (phpi-typedef-update-extensions def1 (list (phpi-typedef-name def2)))
    (phpi-typedef-update-extensions def2 (list (phpi-typedef-name def3)))

    ;; Set method, don't trigger update (expect automatic single method propagation)
    (phpi-typedef-set-method def3 (phpinspect--make-function :name "test"))

    (let ((method (phpi-typedef-get-method def1 (phpinspect-intern-name "test"))))
      (should method)
      (should-not (phpi-method-return-type method))

      (phpi-typedef-delete-method def3 "test")

      (setq method (phpi-typedef-get-method def1 (phpinspect-intern-name "test")))

      (should-not method))))

(ert-deftest phpinspect-typedef-variables ()
  (let ((def (phpinspect-make-typedef (phpinspect--make-type :name "\\test"))))

    (phpi-typedef-set-property def (phpinspect--make-variable :name "test"))
    (phpi-typedef-set-property def (phpinspect--make-variable :name "test2"))

    (let ((test1 (phpi-typedef-get-property def "test"))
          (test2 (phpi-typedef-get-property def "test2")))

      (should test1)
      (should (string= "test" (phpi-var-name test1)))

      (should (phpi-typedef-get-property def "test2"))
      (should (string= "test2" (phpi-var-name test2))))))


(ert-deftest phpinspect-typedef-set-index-trait ()
  (let* ((code "class A { use \\AAA; function B(): C {} }")
         (index (phpinspect--index-tokens (phpinspect-parse-string code)))
         (class (cdar (alist-get 'classes index)))
         (def1 (phpinspect-make-typedef (alist-get 'class-name class)))
         (def2 (phpinspect-make-typedef (phpinspect--make-type :name "\\AAA")))
         (retriever (lambda (type)
                      (cond ((phpinspect--type= type (phpi-typedef-name def1))
                             def1)
                            ((phpinspect--type= type (phpi-typedef-name def2))
                             def2)))))

    (setf (phpi-typedef-retriever def1) retriever
          (phpi-typedef-retriever def2) retriever)

    (phpi-typedef-set-static-method def2 (phpinspect--make-function
                                          :name "test"
                                          :return-type (phpinspect--make-type :name "\\aaa")))

    (phpi-typedef-set-index def1 class)

    (should (phpi-typedef-get-methods def1))
    (should (= 1 (length (phpi-typedef-get-methods def1))))
    (should (= 1 (length (phpi-typedef-get-static-methods def1))))

    (should (eq (phpinspect-intern-name "B")
                (phpi-method-name (car (phpi-typedef-get-methods def1)))))

    (let ((static-method (phpi-typedef-get-static-method def1 (phpinspect-intern-name "test"))))
      (should static-method)
      (should (eq (phpinspect-intern-name "test")
                  (phpi-method-name static-method))))))

(ert-deftest phpinspect-typedef-set-index-trait-config ()
  (let* ((code "class A { use \\AAA; function B(): C {} }")
         (index (phpinspect--index-tokens (phpinspect-parse-string code)))
         (class (cdar (alist-get 'classes index)))
         (def1 (phpinspect-make-typedef (alist-get 'class-name class)))
         (def2 (phpinspect-make-typedef (phpinspect--make-type :name "\\AAA")))
         (retriever (lambda (type)
                      (cond ((phpinspect--type= type (phpi-typedef-name def1))
                             def1)
                            ((phpinspect--type= type (phpi-typedef-name def2))
                             def2)))))

    (setf (phpi-typedef-retriever def1) retriever
          (phpi-typedef-retriever def2) retriever)

    (phpi-typedef-set-index def1 class)
    (phpi-typedef-set-index
     def2 (cdar (alist-get 'classes (phpinspect--index-tokens
                                     (phpinspect-parse-string
                                      "class AAA { function b(): string {} }")))))


    (should (= 2 (length (phpi-typedef-get-methods def1))))

    (let ((method (phpi-typedef-get-method def1 "b")))
      (should (string= "b" (phpi-fn-name method)))
      (should (phpinspect--type= (phpinspect--make-type :name "\\string")
                                 (phpi-fn-return-type method))))

    (phpi-typedef-set-index
     def1 (cdar (alist-get 'classes (phpinspect--index-tokens
                                     (phpinspect-parse-string
                                      "class A { use \\AAA { \\AAA::b as boo } function B(): C {} }")))))

    (should-not (phpi-typedef-get-method def1 "b"))

    (let ((method (phpi-typedef-get-method def1 "boo")))
      (should (string= "boo" (phpi-fn-name method)))
      (should (phpinspect--type= (phpinspect--make-type :name "\\string")
                                 (phpi-fn-return-type method))))))

(ert-deftest phpinspect-typedef-inherited-properties ()
  (let* ((def1 (phpinspect-make-typedef (phpinspect--make-type :name "\\A")))
         (def2 (phpinspect-make-typedef (phpinspect--make-type :name "\\B")))
         (def3 (phpinspect-make-typedef (phpinspect--make-type :name "\\C")))
         (retriever (lambda (type)
                      (cond ((phpinspect--type= type (phpi-typedef-name def1))
                             def1)
                            ((phpinspect--type= type (phpi-typedef-name def2))
                             def2)
                            ((phpinspect--type= type (phpi-typedef-name def3))
                             def3)))))

    (setf (phpi-typedef-retriever def1) retriever
          (phpi-typedef-retriever def2) retriever
          (phpi-typedef-retriever def3) retriever)


    (phpi-typedef-update-extensions def1 (list (phpi-typedef-name def2)))
    (phpi-typedef-update-extensions def2 (list (phpi-typedef-name def3)))

    (phpi-typedef-set-property def3 (phpinspect--make-variable :name "testPublic" :scope '(:public)))
    (phpi-typedef-set-property def3 (phpinspect--make-variable :name "testProtected" :scope '(:protected)))
    (phpi-typedef-set-property def3 (phpinspect--make-variable :name "testPrivate" :scope '(:private)))
    (phpi-typedef-trigger-subscriber-update def3)

    (let ((prop (phpi-typedef-get-property def1 "testPublic")))
      (should prop)
      (should-not (phpi-prop-type prop)))

    (let ((prop (phpi-typedef-get-property def1 "testProtected")))
      (should prop)
      (should-not (phpi-prop-type prop)))

    (let ((prop (phpi-typedef-get-property def1 "testPrivate")))
      (should-not prop))))

(ert-deftest phpinspect-typedef-inherited-properties-no-manual-trigger ()
  (let* ((def1 (phpinspect-make-typedef (phpinspect--make-type :name "\\A")))
         (def2 (phpinspect-make-typedef (phpinspect--make-type :name "\\B")))
         (def3 (phpinspect-make-typedef (phpinspect--make-type :name "\\C")))
         (retriever (lambda (type)
                      (cond ((phpinspect--type= type (phpi-typedef-name def1))
                             def1)
                            ((phpinspect--type= type (phpi-typedef-name def2))
                             def2)
                            ((phpinspect--type= type (phpi-typedef-name def3))
                             def3)))))

    (setf (phpi-typedef-retriever def1) retriever
          (phpi-typedef-retriever def2) retriever
          (phpi-typedef-retriever def3) retriever)


    (phpi-typedef-update-extensions def1 (list (phpi-typedef-name def2)))
    (phpi-typedef-update-extensions def2 (list (phpi-typedef-name def3)))

    (phpi-typedef-set-property def3 (phpinspect--make-variable :name "testPublic" :scope '(:public)))
    (phpi-typedef-set-property def3 (phpinspect--make-variable :name "testProtected" :scope '(:protected)))
    (phpi-typedef-set-property def3 (phpinspect--make-variable :name "testPrivate" :scope '(:private)))

    ;; Don't manually trigger subscriber update

    (let ((prop (phpi-typedef-get-property def1 "testPublic")))
      (should prop)
      (should-not (phpi-prop-type prop)))

    (let ((prop (phpi-typedef-get-property def1 "testProtected")))
      (should prop)
      (should-not (phpi-prop-type prop)))

    (let ((prop (phpi-typedef-get-property def1 "testPrivate")))
      (should-not prop))

    (phpi-typedef-delete-property def3 (phpinspect-intern-name "testPublic"))

    (let ((prop (phpi-typedef-get-property def1 "testPublic")))
      (should-not prop))))

(ert-deftest phpinspect-typedef-get-dependencies-no-include-self ()
  "A typedef should never include its own type as a dependency."
  (let* ((type (phpinspect--make-type :name "\\A" :fully-qualified t))
	 (def (phpinspect-make-typedef type)))
    (phpi-typedef-set-method def (phpinspect--make-function :name "a" :return-type type))
    (phpi-typedef-set-method def (phpinspect--make-function :name "b" :return-type phpinspect--object-type))

    (let ((dependencies (phpi-typedef-get-dependencies def)))
      (should (length= dependencies 1))
      (should (phpinspect--type= phpinspect--object-type (car dependencies))))))
