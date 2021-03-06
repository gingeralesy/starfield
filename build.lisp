;; To run this first install Quicklisp from https://www.quicklisp.org/beta/
;; and then load this file with your Common Lisp implementation.

(defvar *ql-setup*
  (merge-pathnames
   #P"quicklisp/setup.lisp"
   *default-pathname-defaults*))

(unless (probe-file *ql-setup*)
  (setf *ql-setup*
        (merge-pathnames
         #P"quicklisp/setup.lisp"
         (user-homedir-pathname)))
  (unless (probe-file *ql-setup*)
    (setf *ql-setup*
          (merge-pathnames
           #P"AppData/Roaming/quicklisp/setup.lisp"
           (user-homedir-pathname)))
    (unless (probe-file *ql-setup*)
      (error "Could not find quicklisp!"))))

(load *ql-setup*)
(ql:quickload :starfield)
(asdf:operate :build-op :starfield)
(format T "Compilation finished!")
#+sbcl (sb-ext:quit)
#+clisp (ext:exit)
#+ccl (ccl:quit)
#+allegro (excl:exit)
#+ecl (ext:quit)
;(starfield:main)

