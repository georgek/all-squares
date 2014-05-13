(asdf:defsystem "gk-all-squares"
  :description "All-squares clustering stuff."
  :version "0.1"
  :author "George Kettleborough"
  :licence "GNU GPLv3"
  :depends-on ("gk-clusdiff")
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "heap")
               (:file "all-squares")))
