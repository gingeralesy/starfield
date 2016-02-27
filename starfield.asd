(in-package #:cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (push :verbose-no-init *features*)
  #+quicklisp (ql:quickload :verbose)
  #-quicklisp (asdf:load-system :verbose))

(asdf:defsystem #:starfield
  :defsystem-depends-on (:qtools)
  :depends-on (:qtools
               :qtcore
               :qtgui)
  :components ((:file "module")
               (:file "star")
               (:file "starfield")
               (:file "windowing")))
