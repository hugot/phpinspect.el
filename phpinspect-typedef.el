

(require 'phpinspect-method-cell)

(cl-defstruct (phpinspect-typedef
               (:constructor phpinspect-make-typedef-generated)
               (:conc-name phpi-typedef-))
  (read-only-p nil
               :type boolean
               :documentation
               "Whether this typedef is read-only, meaning that its data
should never be changed. Methods and functions that are meant to
manipulate typedef data should become no-ops when this slot has a
non-nil value.")
  (retriever nil
                   :type lambda
                   :documentaton
                   "A function that returns typedefs for types
(should accept object of type `phpinspect--type' as argument)")
  (methods nil
           :type phpinspect-method-collection)
  (static-methods nil
                  :type phpinspect-method-collection)
  (variables nil
             :type list)
  (initial-index nil
                 :type bool)
  (trait-config nil
                :documentation
                "The configuration of traits that are used in this type")
  (method-adder nil
                :type phpinspect-method-adder)
  (subscribed-types nil
                    :type list
                    :documentation "list of phpinspect--type structures.")
  (subscribed-to-types nil
                       :type list
                       :documentation "list of phpinspect--type structures")
  (name nil
        :type phpinspect--type)
  (declaration nil
               :type phpinspect-declaration-p))

(defun phpinspect-make-typedef (type &optional retriever)
  (cl-assert (phpinspect--type-p type))

  (let ((def (phpinspect-make-typedef-generated
              :name type
              :retriever retriever
              :methods (phpinspect-make-method-collection :home-type type)
              :static-methods (phpinspect-make-method-collection :home-type type)
              :method-adder (phpinspect-make-method-adder))))
    def))

(define-inline phpi--typedef-get-foreign-type (def type)
  (inline-letevals (def type)
    (inline-quote
     (when-let ((retriever (phpi-typedef-retriever ,def)))
       (funcall retriever ,type)))))

(defmacro phpi--typedef-edit (typedef &rest body)
  "Declare intent to edit TYPEDEF in BODY.

Conditionally executes BODY depending on
`phpi-typedef-read-only-p' value."
  (declare (indent 1))
  `(unless (phpi-typedef-read-only-p ,typedef)
     ,@body))

(cl-defstruct (phpinspect-method-adder
               (:constructor phpinspect-make-method-adder)
               (:conc-name phpi-ma-))
  (aliases nil
           :type alist)
  (overrides nil
             :type alist))

(defun phpi-ma-get-overrides (ma type)
  (alist-get type (phpi-ma-overrides ma) nil nil #'phpinspect--type=))

(defun phpi-ma-get-aliases (ma type)
  (alist-get type (phpi-ma-aliases ma) nil nil #'phpinspect--type=))

(defun phpi-ma-set-config (ma config)
  "Set type method import config for MA to CONFIG.

This function reconfigures MA to add methods according to the
rules specified in CONFIG. CONFIG is expected to be a list of the
structure returned by `phpinspect--index-trait-use'."
  (setf (phpi-ma-aliases ma) nil)
  (setf (phpi-ma-overrides ma) nil)

  (dolist (typeconf config)
    (let ((type (car typeconf))
          overrides aliases)
      (dolist (setting (cdr typeconf))
        (pcase (car setting)
          ('alias (push (cons (phpinspect-intern-name (cadr setting))
                              (phpinspect-intern-name (caddr setting)))
                        aliases))
          ('override (push (cons (phpinspect-intern-name (cadr setting))
                                 (caddr setting))
                           overrides))
          (_ (error (format "Invalid type configuration instruction %s" (car setting))))))

      (push (cons type aliases) (phpi-ma-aliases ma))
      (push (cons type overrides) (phpi-ma-overrides ma)))))

(defun phpi-ma-add (ma mcol method &optional overwrite)
  (if-let ((overrides (phpi-ma-get-overrides ma (phpi-method-origin-type method)))
           (aliases (phpi-ma-get-aliases ma (phpi-method-origin-type method))))
      (progn
        (if-let ((override (alist-get (phpi-method-name method) overrides)))
            ;; Override basically just means insert and overwrite without checking
            (phpi-mcol-add mcol method 'overwrite)
          ;; Can't alias if overriding, can't override if aliasing
          (when-let ((alias (alist-get (phpi-method-name method) aliases))
                     (copy (phpi-copy-method method)))
            ;; Rename method to alias, record original name in aliased-from slot
            (setf (phpi-method-aliased-from method) (phpi-method-name method)
                  (phpi-method-name method) alias))))

    ;; Not dealing with aliases or overrides, just add the methods (unless they
    ;; already exist)
    (phpi-mcol-add mcol method overwrite)))

(defun phpi-typedef-get-methods (def)
  (phpi-mcol-list-active (phpi-typedef-methods def)))

(defun phpi-typedef-get-static-methods (def)
  (phpi-mcol-list-active (phpi-typedef-static-methods def)))

(cl-defmethod phpi-typedef-get-method ((def phpinspect-typedef) (method-name (head phpinspect-name)))
  (phpi-mcol-get-active-method (phpi-typedef-methods def) method-name))

(cl-defmethod phpi-typedef-get-method ((def phpinspect-typedef) (method-name string))
  (phpi-typedef-get-method def (phpinspect-intern-name method-name)))

(cl-defmethod phpi-typedef-get-static-method ((def phpinspect-typedef) (method-name (head phpinspect-name)))
  (phpi-mcol-get-active-method (phpi-typedef-static-methods def) method-name))

(cl-defmethod phpi-typedef-get-static-method ((def phpinspect-typedef) (method-name string))
  (phpi-typedef-get-static-method def (phpinspect-intern-name method-name)))

(cl-defmethod phpi-typedef-set-method ((def phpinspect-typedef) (fn phpinspect--function))
  (let ((method (phpinspect-make-method (phpi-typedef-name def) fn)))
    (phpi-ma-add (phpi-typedef-method-adder def)
                 (phpi-typedef-methods def)
                 method 'overwrite)))

(cl-defmethod phpi-typedef-set-static-method ((def phpinspect-typedef) (fn phpinspect--function))
  (let ((method (phpinspect-make-method (phpi-typedef-name def) fn)))
    (phpi-ma-add (phpi-typedef-method-adder def)
                 (phpi-typedef-static-methods def)
                 method 'overwrite)))


(cl-defmethod phpi-typedef-delete-method ((def phpinspect-typedef) (name (head phpinspect-name)))
  (phpi-mcol-delete-for-type
   (phpi-typedef-methods def) (phpi-typedef-name def) name)

  (phpi-mcol-delete-for-type
   (phpi-typedef-static-methods def) (phpi-typedef-name def) name))

(cl-defmethod phpi-typedef-delete-method ((def phpinspect-typedef) (fn phpinspect--function))
  (phpi-typedef-delete-method def (phpinspect--function-name-symbol fn)))

(defun phpi-typedef-set-trait-config (def config)
  (phpi--typedef-edit def
    (let ((existing-config (phpi-typedef-trait-config def))
          types)
      (if existing-config
          ;; This can probably be made more sophisticated by only applying the
          ;; difference, but just deleting all incorporated methods will do for
          ;; an initial implementation.
          (dolist (use existing-config)
            (phpi-mcol-delete-for-type (phpi-typedef-methods def) (car use)))
        ;; Apply new config
        (progn
          (phpi-ma-set-config (phpi-typedef-method-adder def) config)
          (dolist (cons config)
            (push (car config) types))))
      ;; return all types that were in config
      types)))

(defun phpi-typedef-add-foreign-members (def foreign-def)
  "Add methods and properties to DEF from FOREIGN-DEF."
  (when foreign-def
    (dolist (method (phpi-typedef-get-methods foreign-def))
      (phpi-ma-add (phpi-typedef-method-adder def) (phpi-typedef-methods def) method))

    (dolist (method (phpi-typedef-get-static-methods foreign-def))
      (phpi-ma-add (phpi-typedef-method-adder def) (phpi-typedef-methods def) method))

    (dolist (var (phpi-typedef-variables foreign-def))
      (phpi-typedef-set-variable def var))))

(defun phpi-typedef-subscribe-to-foreign-typedef (def foreign-def)
  (push (phpi-typedef-name def) (phpi-typedef-subscribed-types foreign-def))
  (push (phpi-typedef-name foreign-def) (phpi-typedef-subscribed-to-types def)))

(defun phpi-typedef-unsubscribe-from-foreign-typedef (def foreign-def)
  "Unsubscribe DEF from changes in FOREIGN-DEF.

This undoes any links and data sharing between this type and any
extended classes, used traits or implemented interfaces."
  (setf (phpi-typedef-subscribed-types foreign-def)
        (cl-remove (phpi-typedef-name def) (phpi-typedef-subscribed-types foreign-def)
                   :test #'phpinspect--type=))

  (setf (phpi-typedef-subscribed-to-types def)
        (cl-remove (phpi-typedef-name foreign-def) (phpi-typedef-subscribed-types def)
                   :test #'phpinspect--type=))

  ;; Remove all members of foreign typedef
  (phpi-mcol-delete-for-type (phpi-typedef-methods def) (phpi-typedef-name foreign-def))
  (phpi-mcol-delete-for-type (phpi-typedef-static-methods def) (phpi-typedef-name foreign-def))
  (dolist (variable (phpi-typedef-variables foreign-def))
    (phpi-typedef-delete-variable def variable)))

(defun phpi-typedef-trigger-subscriber-update (def)
  "Incorporate DEF into subscribed typedefs."
  (dolist (type (phpi-typedef-subscribed-types def))
    (when-let ((foreign-def (phpi--typedef-get-foreign-type def type)))
      (phpi-typedef-add-foreign-members foreign-def def))))

(defun phpi-typedef-set-name (def type)
  "Set the TYPE name of DEF.

TYPE must be a structure of type `phpinspect--type'."
  (phpi--typedef-edit def
    (cl-assert (phpinspect--type-p type))

    (let ((existing (phpi-typedef-name def)))
      ;; Only update if type name is actually different
      (unless (phpinspect--type= existing type)
        (setf (phpi-typedef-name def) type)
        (phpi-mcol-set-home-type (phpi-typedef-methods def) type)))))

(defun phpi-typedef-update-declaration (def declaration imports namespace-name trait-config)
  (phpi--typedef-edit def
    (pcase-let ((`(,type ,extends ,implements ,_used-types)
                 (phpinspect--index-class-declaration
                  declaration (phpinspect--make-type-resolver
                               (phpinspect--uses-to-types imports) nil namespace-name))))
      (phpi-typedef-set-name def type)
      (setf (phpi-typedef-declaration def) declaration)
      (phpi-typedef-update-extensions
       def `(,@extends ,@implements ,@(phpi-typedef-set-trait-config def trait-config))))))

(defun phpi-typedef-update-extensions (def extensions)
  (phpi--typedef-edit def
    (when-let ((subscriptions (phpi-typedef-subscribed-to-types def)))
      (dolist (sub subscriptions)
        (unless (cl-member sub extensions :test #'phpinspect--type=)
          ;; No longer an extended type, unsubscribe
          (when-let ((fd (phpi--typedef-get-foreign-type def sub)))
            (phpi-typedef-unsubscribe-from-foreign-typedef def fd))))

      (dolist (ext extensions)
        (when (cl-member ext subscriptions :test #'phpinspect--type=)
            ;; Already subscribed, no refresh needed
          (delq ext extensions))))

    (when-let ((foreign-defs
                (seq-filter
                 #'phpinspect-typedef-p
                 (mapcar
                  (lambda (class-name)
                    (funcall (phpi-typedef-retriever def) class-name))
                  extensions))))

      (dolist (fd foreign-defs)
        (phpi-typedef-add-foreign-members def fd)
        (phpi-typedef-subscribe-to-foreign-typedef def fd)))))

(cl-defmethod phpi-typedef-set-index ((def phpinspect-typedef)
                                      (index (head phpinspect--indexed-class)))
  (phpi--typedef-edit def
    (setf (phpi-typedef-declaration def) (alist-get 'declaration index))



    (let ((ma (phpi-typedef-method-adder def))
          (mcol (phpi-typedef-methods def))
          (stmcol (phpi-typedef-static-methods def))
          (home-type (phpi-typedef-name def)))

    ;; Override methods when class seems syntactically correct (has balanced braces)
      (when (alist-get 'complete index)
        ;; Delete all known own methods
        (phpi-mcol-delete-for-type mcol home-type))

      (dolist (method (alist-get 'methods index))
        (phpi-ma-add ma mcol (phpinspect-make-method home-type method) 'overwrite))

      (dolist (method (alist-get 'static-methods index))
        (phpi-ma-add ma stmcol (phpinspect-make-method home-type method) 'overwrite))

      (setf (phpi-typedef-initial-index def) t)

      (setf (phpi-typedef-variables def)
            (append (alist-get 'variables index)
                    (alist-get 'constants index)
                    (alist-get 'static-variables index)))

      (phpi-typedef-update-extensions
       def `(,@(alist-get 'implements index)
             ,@(alist-get 'extends index)
             ,@(phpi-typedef-set-trait-config def (alist-get 'trait-usage index))))

      (phpi-typedef-trigger-subscriber-update def))))

(cl-defmethod phpi-typedef-get-method-return-type
  ((def phpinspect-typedef) (method-name (head phpinspect-name)) &optional tryhard)
  (phpi-mcol-get-return-type (phpi-typedef-methods def) method-name tryhard))

(cl-defmethod phpi-typedef-get-static-method-return-type
  ((def phpinspect-typedef) (method-name (head phpinspect-name)) &optional tryhard)
  (phpi-mcol-get-return-type (phpi-typedef-static-methods def) method-name tryhard))

(cl-defmethod phpi-typedef-set-variable ((def phpinspect-typedef)
                                         (var phpinspect--variable))
  (phpi--typedef-edit def
    (push var (phpi-typedef-variables def))))

(cl-defmethod phpi-typedef-delete-variable ((def phpinspect-typedef)
                                            (var phpinspect--variable))
  (phpi--typedef-edit def
    (let ((first-half (seq-take-while (lambda (clvar) (not (eq var clvar)))
                                      (phpi-typedef-variables def))))
      ;; Only remove the first occurrence. If a parent class or a trait has the
      ;; same variable, it will stick around.
      (setf (phpi-typedef-variables def)
            (append first-half (nthcdr (+ 1 (length first-half))
                                       (phpi-typedef-variables def)))))))

(cl-defmethod phpi-typedef-get-variables ((class phpinspect-typedef))
  (seq-filter #'phpinspect--variable-vanilla-p (phpi-typedef-variables class)))

(cl-defmethod phpi-typedef-get-static-variables ((class phpinspect-typedef))
  (seq-filter #'phpinspect--variable-static-p (phpi-typedef-variables class)))

(cl-defmethod phpi-typedef-get-constants ((class phpinspect-typedef))
  (seq-filter #'phpinspect--variable-const-p (phpi-typedef-variables class)))

(cl-defmethod phpi-typedef-get-variable
  ((def phpinspect-typedef) (variable-name string))
  (catch 'found
    (dolist (variable (phpi-typedef-variables def))
      (when (string= variable-name (phpinspect--variable-name variable))
        (throw 'found variable)))))

(provide 'phpinspect-typedef)
;;; phpinspect-typedef.el ends here
