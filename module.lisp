(in-package #:cl-user)
(defpackage #:starfield
  (:use #:cl+qt)
  (:export #:main))
(in-package #:starfield)
(in-readtable :qtools)

(defvar *main-window* NIL)
(defparameter *fps* 1000/30)
(defparameter *title* "Starfield")

(defclass updatable () ())
(defclass paintable () ())

(defgeneric update (updatable))
(defgeneric paint (paintable target))

(defclass paintable ()
  ((visibility :initarg :visibility :accessor visibility))
  (:default-initargs :visibility 1.0))

(defun main (&key (blocking NIL))
  "Launches the game in debug mode."
  (unless *main-window*
    (setf v:*global-controller* (v:make-standard-global-controller))
    (with-main-window (window 'main :blocking blocking :name *title*))))

(defun noop (&rest args)
  "Do nothing."
  (declare (ignore args))
  (values))
