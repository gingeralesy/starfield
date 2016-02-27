(in-package #:starfield)
(in-readtable :qtools)

(defclass star (updatable paintable)
  ((id :initarg :id :accessor od)
   (location :initarg :location :accessor location)
   (color :initarg :color :accessor color)
   (brightness :initarg :brightness :accessor brightness))
  (:default-initargs
   :id (error "Please give star an id.")
   :location (error "Please define coordinates.")
   :color (q+:make-qcolor 255 255 255)
   :brightness 5))

(defmethod update ((star star))
  (call-next-method))

(defmethod paint ((star star) target)
  (call-next-method)
  (let ((loc (location star)))
    (q+:fill-rect target (first loc) (second loc) 1 1 (color star))))
