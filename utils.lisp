(defun compose-fns (fns value)
   (if (= (length fns) 1)
       (funcall (first fns) value)
       (funcall (first fns) (compose-fns (rest fns) value))))

(defmacro compose (&rest args)
  (let ((value (first (last args)))
        (fns (butlast args)))
    `(compose-fns ',fns ,value)))

(defmacro alias (f-alias f-origin)
  `(defun ,f-alias (&rest args)
      (apply #',f-origin args)))
