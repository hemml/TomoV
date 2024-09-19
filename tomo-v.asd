(defsystem "tomo-v"
  :description "Doppler tomography with ART reconstruction method"
  :version "1.0.0"
  :author "Pavel Kaygorodov <pasha@inasan.ru>"
  :licence "MIT"
  :homepage "https://github.com/hemml/TomoV"
  :source-control "https://github.com/hemml/TomoV.git"
  :depends-on ("omg" "inferior-shell" "omgdaemon")
  :build-operation "static-program-op"
  :components ((:static-file "README.md")
               (:static-file "LICENSE")
               (:file "tomo")
               (:file "macros" :depends-on ("tomo"))
               (:file "settings" :depends-on ("macros"))
               (:file "saveable-graph" :depends-on ("macros" "settings"))
               (:file "trail" :depends-on ("macros" "settings" "saveable-graph"))
               (:file "profiles" :depends-on ("macros" "settings" "trail" "tomo"))
               (:file "art-solver" :depends-on ("macros" "settings" "trail" "profiles" "saveable-graph"))
               (:file "sample-src" :depends-on ("macros" "settings" "trail" "profiles" "saveable-graph"))
               (:file "real-src" :depends-on ("macros" "settings" "trail" "profiles" "saveable-graph"))
               (:file "app" :depends-on ("macros" "settings" "profiles" "tomo"))))
