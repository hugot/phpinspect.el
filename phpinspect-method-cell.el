

(cl-defstruct (phpinspect-method
               (:constructor phpinspect-make-method)
               (:conc-name phpi-method-))
  (name nil
        :type phpinspect-name)
  (origin-type nil
               :type phpinspect--type)
  (definition  nil
               :type phpinspect--function))

(cl-defstruct (phpinspect-method-cell
               (:constructor phpinspect-make-method-cell)
               (:conc-name phpi-mc-))
  (name nil
        :type phpinspect-name)
  (own nil
       :type phpinspect-method)
  (trait nil
         :type phpinspect-method)
  (inherited nil
             :type phpinspect-method)
  (interface nil
             :type phpinspect-method))

(cl-defstruct (phpinspect-method-collection
               (:constructor phpinspect-make-method-collection)
               (:conc-name phpi-mcol-))
  (home-type nil
             :type phpinspect--type)
  (cells nil
         :type alist
         :documentation "<phpinspect-name, phpinspect-method-cell>"))

(defun phpi-mcol-find-cell (mcol method-name &optional remove)
  (cl-assert (phpinspect-name-p method-name))

  (alist-get method-name (phpi-mcol-cells mcol) nil remove #'eq))

(defun phpi-mcol-find-cell-create (mcol method-name)
  (let ((cell (phpi-mcol-find-cell mcol method-name)))
    (unless cell
      (setq cell (phpinspect-make-method-cell
                  :name method-name))
      (push (cons method-name cell) (phpi-mcol-cells mcol)))

    cell))

(defun phpi-mcol-add (mcol method)
  (let ((cell (phpi-mcol-find-cell-create mcol (phpi-method-name method))))
    (phpi-mc-set cell (phpi-mcol-home-type mcol) (phpi-method-origin-type method) method)
    cell))


(defun phpi-mc-set (cell home-type origin-type method)
  (if (phpinspect--type= home-type
                         origin-type)
      ;; Method belongs to home type
      (setf (phpi-mc-own cell) method)
    ;; Method is from a trait, interface or inherited
    (pcase (phpinspect--type-category (phpi-method-origin-type method))
      ('trait (setf (phpi-mc-trait cell) method))
      ('interface (setf (phpi-mc-interface cell) method))
      ;; class or abstract class
      (_ (setf (phpi-mc-inherited cell) method)))))

(defun phpi-method-origin-type= (method type)
  (phpinspect--type= (phpi-method-origin-type method) type))

(defun phpi-mc-get-for-type (cell type)
  (catch 'phpinspect--break
    (dolist (method (list (phpi-mc-own cell) (phpi-mc-trait cell)
                          (phpi-mc-inherited cell) (phpi-mc-interface cell)))
      (when (and method (phpi-method-origin-type= method type))
        (throw 'phpinspect--break method)))
    nil))

(defun phpi-mcol-delete (mcol method-name)
  (phpi-mcol-find-cell mcol method-name 'remove))

(defun phpi-mc-emtpy-p (cell)
  "CELL is empty when it contains no methods."
  (not (phpi-mc-get-active cell)))

(defun phpi-mc-get-active (cell)
  "Get active method from CELL, according to PHP precendence order.

PHP's precendence order is described in the documentation for
traits, located at:
https://www.php.net/manual/en/language.oop5.traits.php"
  (or (phpi-mc-own cell)
      (phpi-mc-trait cell)
      (phpi-mc-inherited cell)
      (phpi-mc-interface cell)))

(defun phpi-mcol-delete-for-type (mcol type &optional name)
  "Delete from MCOL all methods that originate from TYPE.

When NAME is provided, only method with NAME is deleted."
  (if name
      ;; Name is provided, only delete method with NAME.
      (let ((cell (phpi-mcol-find-cell mcol name)))
        (when (phpi-mc-get-for-type cell type)
          (phpi-mc-set cell (phpi-mcol-home-type mcol) type nil))

        (when (phpi-mc-empty-p cell)
          (phpi-mcol-delete mcol name)))

    (let ((cells (phpi-mcol-cells mcol)))
      (dolist (cons cells)
        (let ((cell (cdr cons)))
          (when (phpi-mc-get-for-type cell type)
            (phpi-mc-set cell (phpi-mcol-home-type mcol) type nil))

          ;; mark cell for deletion by setting car and cdr of alist member to
          ;; nil
          (when (phpi-mc-empty-p cell)
            (setcar cons nil)
            (setcdr cons nil))))
      ;; Delete all empty cells
      (setf (phpi-mcol-cells mcol) (delete (cons nil nil) cells)))))
