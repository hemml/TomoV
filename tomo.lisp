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
