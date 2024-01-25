(defsystem "tomo-v"
  :description "Doppler tomography with ART reconstruction method"
  :version "1.0.0"
  :author "Pavel Kaygorodov <pasha@inasan.ru>"
  :licence "MIT"
  :homepage "https://github.com/hemml/TomoV"
  :source-control "https://github.com/hemml/TomoV.git"
  :depends-on ("omg" "inferior-shell")
  :build-operation "static-program-op"
  :components ((:static-file "README.md")
               (:static-file "LICENSE")
               (:file "tomo")
               (:file "macros" :depends-on ("tomo"))
               (:file "settings" :depends-on ("tomo"))
               (:file "trail" :depends-on ("tomo"))
               (:file "profiles" :depends-on ("tomo"))
               (:file "art-solver" :depends-on ("tomo"))
               (:file "sample-src" :depends-on ("tomo"))
               (:file "real-src" :depends-on ("tomo"))
               (:file "app" :depends-on ("tomo"))))
