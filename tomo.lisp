(ql:quickload :omg)

(defpackage :tomo
  (:use cl omg omgui jscl omgdaemon omgwidgets))

(in-package :tomo)

(defparameter-f *noize-treshhold* 0.0)

(defun update-from-git ()
  (inferior-shell:run '(and (cd "/home/omg/")
                            (rm -fr "TomoV")
                            (git clone "https://github.com/hemml/TomoV.git")))
  (pushnew (make-pathname :directory '(:absolute "home/omg/TomoV")) asdf:*central-registry*)
  (asdf:load-system :tomo-v))

(defclass-f load-store-progress (modal-dialog-window)
  ((label :accessor label
          :initform nil
          :initarg :label)
   (direction :accessor direction
              :initform :in
              :initarg :direction)
   (buffer :accessor buffer
           :initarg :buffer
           :initform nil)
   (obj :accessor obj
        :initarg :obj)
   (size :accessor size
         :initform nil
         :initarg :size)
   (start :accessor start
          :initform 0
          :initarg :start)
   (respect-transfer :accessor respect-transfer
          :initform nil
          :initarg :respect-transfer)
   (final-cb :accessor final-cb
             :initarg :final-cb
             :initform #'identity)))

(defmethod-f render-widget :after ((w load-store-progress))
  (let ((pb (make-instance 'progress-bar))
        (cnt 0)
        (q 0.95))
    (append-element
      (create-element "div" :|style.border| "0.1em solid black"
                            :|style.border-radius| "0.5em"
                            :|style.padding| "0.5em"
                            :|style.background| "#fffff0"
        :append-elements
          (if (label w)
              (list (create-element "span" :|style.paddingRight| "1em"
                      :append-element (label w))))
        :append-element (create-element "span" :|style.width| "20em"
                          :append-element (render-widget pb)))
      (root w))
    (if (and (buffer w) (not (size w)))
        (setf (slot-value w 'size) (jscl::oget (buffer w) "byteLength")))
    (if (equal :in (direction w))
        (load-from-buffer (buffer w) :start (start w) :background 0.2
          :progress-cb (lambda (n)
                         (set-progress pb (/ n (size w))))
          :final-cb (lambda (obj n)
                      (funcall (final-cb w) obj n)
                      (close w)))
        (labels ((stb (buf fin &optional (cf 1) (ofs 0))
                   (store-to-buffer (obj w) buf :start (start w) :respect-transfer (respect-transfer w) :background 0.2
                     :progress-cb (lambda (n)
                                    (set-progress pb
                                      (+ ofs (* cf (if (size w)
                                                       (/ n (size w))
                                                       (- 1 (expt q (incf cnt))))))))
                     :final-cb (lambda (n)
                                 (funcall fin buf n)))))
          (if (buffer w)
              (stb (buffer w) (final-cb w))
              (stb (jscl::make-new (winref "ArrayBuffer") 1)
                   (lambda (buf siz)
                       (setf (slot-value w 'size) siz)
                       (stb (jscl::make-new (winref "ArrayBuffer") siz)
                            (lambda (buf siz)
                              (funcall (final-cb w) buf siz)
                              (close w))
                            (* 0.5 (1+ (expt q cnt)))
                            (* 0.5 (- 1 (expt q cnt)))))
                   0.5))))))
