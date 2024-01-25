(ql:quickload :omg)

(defpackage :tomo
  (:use cl omg omgui jscl omgdaemon omgwidgets))

(in-package :tomo)

(defparameter-f *noize-treshhold* 0.0)

(defun update-from-git ()
  (when (run '(and (cd "/home/omg/quicklisp/local-projects")
                   (rm -fr "TomoV")
                   (git clone "https://github.com/hemml/TomoV.git"))))
  (ql:quickload :tomo-v))
