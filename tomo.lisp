(ql:quickload :omg)

(defpackage :tomo
  (:use cl omg omgui jscl omgdaemon omgwidgets))

(in-package :tomo)

(defparameter-f *noize-treshhold* 0.0)

(load "macros.lisp")
(load "settings.lisp")
(load "trail.lisp")
(load "profiles.lisp")
(load "art-solver.lisp")
(load "sample-src.lisp")
(load "real-src.lisp")
(load "app.lisp")
