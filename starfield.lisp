(in-package #:starfield)
(in-readtable :qtools)

(defclass starfield (updatable paintable)
  ((stars :initarg :stars :initform NIL :accessor stars)))

(defmethod initialize-instance :after ((starfield starfield) &key (stars NIL))
  (setf (slot-value starfield 'stars) NIL)
  (if stars
      (setf (slot-value starfield 'stars) stars)
      (dotimes (i 100)
        (push (make-instance 'star
                             :id i
                             :location (list (random (q+:width *main-window*))
                                             (random (q+:height *main-window*)))
                             :brightness (+ (random 2)
                                            (cond ((< i 25) 1)
                                                  ((< i 50) 3)
                                                  (T 5))))
              (stars starfield)))))

(defmethod update ((starfield starfield))
  (call-next-method)
  (loop for star in (stars starfield)
        do (update star)))

(defmethod paint ((starfield starfield) target)
  (call-next-method)
  (loop for star in (stars starfield)
        do (paint star target)))
