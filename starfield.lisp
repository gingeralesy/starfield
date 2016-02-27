(in-package #:starfield)
(in-readtable :qtools)

(defclass starfield (updatable paintable)
  ((stars :initarg :stars :accessor stars))
  (:default-initargs
   :stars NIL))

(defmethod initialize-instance :after ((starfield starfield) &key)
  (dotimes (i 100)
    (push (initialize-instance 'star
                               :id i
                               :location (list (random (q+:width *main-window*))
                                               (random (q+:height *main-window*)))
                               :brightness (+ (random 2)
                                              (cond ((< i 25) 1)
                                                    ((< i 50) 3)
                                                    (T 5))))
          (stars starfield))))

(defmethod update ((starfield starfield))
  (call-next-method)
  (loop for star in (stars starfield)
        do (update star)))

(defmethod paint ((starfield starfield) target)
  (call-next-method)
  (loop for star in (stars starfield)
        do (paint star target)))
