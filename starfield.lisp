(in-package #:starfield)
(in-readtable :qtools)

(defparameter *star-count* (+ 900 (random 200)))

;; === Starfield ===
(defclass starfield (updatable paintable)
  ((stars :initarg :stars :initform NIL :accessor stars)))

(defmethod initialize-instance :after ((starfield starfield) &key)
  (setf (slot-value starfield 'stars) NIL)
  (let ((colors `(,(q+:make-qcolor 255 255 255) ,(q+:make-qcolor 0 255 0)
                  ,(q+:make-qcolor 0 0 255) ,(q+:make-qcolor 0 255 255)
                  ,(q+:make-qcolor 255 0 0))))
    (dotimes (i *star-count*)
      (push (make-instance 'star
                           :id i
                           :location (list (random (q+:width *main-window*))
                                           (random (q+:height *main-window*)))
                           :brightness (+ (random 2)
                                          (cond ((< i (/ *star-count* 4)) 1)
                                                ((< i (/ *star-count* 2)) 3)
                                                (T 5)))
                           :color (nth (random (length colors)) colors))
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
   (brightness :initarg :brightness :accessor brightness)
   (color :initarg :color :accessor color))
  (:default-initargs
   :id (error "Please give star an id.")
   :location (error "Please define coordinates.")
   :brightness 5
   :color (q+:make-qcolor 255 255 255)))

(defmethod initialize-instance :after ((star star) &key)
  (when (< 10 (brightness star)) (setf (slot-value star 'brightness) 10))
  (when (< (brightness star) 1) (setf (slot-value star 'brightness) 1)))

(defmethod update ((star star))
  (call-next-method))

(defmethod paint ((star star) target)
  (call-next-method)
  (let ((loc (location star)))
    (q+:fill-rect target (first loc) (second loc) 1 1
                  (q+:darker (color star) (+ 100 (* 20 (brightness star)))))))
