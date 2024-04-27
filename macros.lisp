(in-package :tomo)

(def-local-macro-f sqr (x)
  (let ((xs (gensym)))
    `(let ((,xs ,x))
       (* ,xs ,xs))))

(def-local-macro-f xor (x y)
  `(not (eql (if ,x t nil) (if ,y  t nil))))

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
