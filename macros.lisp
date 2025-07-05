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

(defmacro destructuring-getf (vals var &rest code)
  `(destructuring-bind ,vals (list ,@(mapcar (lambda (v) `(getf ,var ,(intern (symbol-name v) :keyword))) vals))
     ,@code))

(defmacro defclass-conf (name super slots)
  `(progn
     (defclass-f ,name ,super
       ,(mapcar (lambda (s)
                  (cons (car s)
                        (loop for v on (cdr s) by #'cddr
                          when (not (position (car v) '(:desc :type :validator :variants)))
                            append (list (car v) (cadr v)))))
                slots))
     (defmethod-f render-config ((obj ,name))
       (append (call-next-method)
               (list ,@(loop for s in slots when (position :desc s) collect
                         (destructuring-getf (desc type variants validator) (cdr s)
                           (declare (ignorable variants))
                           `(create-element "div" :|style.marginTop| "1em"
                              :append-element
                                (create-element "span" :|innerHTML| (format nil "~A:" ,desc)
                                                       :|style.marginRight| "1em")
                              :append-element
                                ,(case type
                                   (:number `(render-widget (make-instance 'nulable-fld :value (let ((v (slot-value obj ',(car s))))
                                                                                                 (if (> (abs v) 1)
                                                                                                     (* (signum v) (/ (floor (+ 0.5 (* 1e6 (abs v)))) 1e6))
                                                                                                     v))
                                                              :ok (lambda (val)
                                                                    (let ((v (js-parse-float val)))
                                                                      (when (and v
                                                                                 (not (is-nan v))
                                                                                 ,@(if validator `((funcall ,validator v))))
                                                                        (setf (slot-value obj ',(car s)) v)
                                                                        val))))))
                                   (t (error (format nil "Invalid slot type: ~A" type))))))))))))
