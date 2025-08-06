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

(defvar config-roots (intern (symbol-name (gensym))))

(defmacro defclass-conf (name super slots)
  `(progn
     (defvar-f ,config-roots nil)
     (defclass-f ,name ,super
       ,(mapcar (lambda (s)
                  (cons (car s)
                        (loop for v on (cdr s) by #'cddr
                          when (not (position (car v) '(:desc :type :validator :variants :onchange)))
                            append (list (car v) (cadr v)))))
                slots))
     (defmethod-f redraw-config ((obj ,name))
       (when ,config-roots
         (let ((roots (gethash obj ,config-roots)))
           (when roots
             (map nil (lambda (old new)
                        ((jscl::oget (parent-element old) "insertBefore") new old)
                        (remove-element old))
                      roots
                      (render-config obj))))))
     (defmethod-f render-config ((obj ,name))
       (when (not ,config-roots)
         (setf ,config-roots (make-hash-table)))
       (setf (gethash obj ,config-roots)
             (append (when (next-method-p) (call-next-method))
                     (list ,@(loop for s in slots when (position :desc s) collect
                               (destructuring-getf (desc type variants validator onchange) (cdr s)
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
                                                                                       ,@(if validator `((funcall ,validator v
                                                                                                                       (setf (slot-value obj ',(car s)) v)
                                                                                                                       ,(when onchange
                                                                                                                          `(execute-after 0 (lambda () (funcall ,onchange obj))))
                                                                                                                       val))))))))))
                                         (:string `(render-widget (make-instance 'nulable-fld :value (slot-value obj ',(car s))
                                                                    :ok (lambda (val)
                                                                           (block nil
                                                                             ,(if validator
                                                                                  `(if (funcall ,validator val)
                                                                                       (setf (slot-value obj ',(car s)) val)
                                                                                       (return-from nil nil))
                                                                                  `(setf (slot-value obj ',(car s)) val))
                                                                             ,(when onchange
                                                                                `(funcall ,onchange obj))
                                                                             val)))))
                                         (:checkbox `(with-self box
                                                         (create-element "input" :|type| "checkbox"
                                                                                 :|checked| (slot-value obj ',(car s))
                                                           :|onclick| (lambda (ev)
                                                                        (block nil
                                                                          ,(if validator
                                                                               `(if (funcall ,validator (jscl::oget box "checked"))
                                                                                    (setf (slot-value obj ',(car s)) (jscl::oget box "checked"))
                                                                                    (return-from nil nil))
                                                                               `(setf (slot-value obj ',(car s)) (jscl::oget box "checked")))
                                                                          ,(when onchange
                                                                             `(funcall ,onchange obj))
                                                                          t)))))
                                         (:list `(let ((opts (list ,@(mapcar (lambda (v)
                                                                              `(cons ,(car s) (create-element "option" :|innerHTML| ,(cdr v)
                                                                                                 :|selected| (equal ,(car v) (slot-value obj ',(car s))))))
                                                                             variants))))
                                                   (with-self sel
                                                       (create-element "select"
                                                         :|onchange| (lambda (ev)
                                                                         (setf (slot-value obj ',(car s))
                                                                               (car (find-if (lambda (o) (jscl::oget "selected" (cdr o))) opts)))
                                                                         ,(when onchange
                                                                            `(funcall ,onchange obj))
                                                                         t)
                                                         :append-elements (mapcar #'cdr opts)))))

                                         (t (error (format nil "Invalid slot type: ~A" type)))))))))))))
