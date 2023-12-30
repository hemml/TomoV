(in-package :tomo)

(def-local-macro-f sqr (x) `(* ,x ,x))

(defmacro lazy-slot (slot args &rest body)
  (let ((obj (caar args)))
    `(defmethod-f ,slot ,args
       (if (slot-boundp ,obj ',slot)
           (slot-value ,obj ',slot)
           (setf (slot-value ,obj ',slot)
                 (progn ,@body))))))

(def-local-macro-f with-self (var &rest code)
  `(let (,var)
     (setf ,var (progn ,@code))))
