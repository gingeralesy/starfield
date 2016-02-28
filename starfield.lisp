(in-package #:starfield)
(in-readtable :qtools)

;; === Starfield ===
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

;; === Star ===
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
