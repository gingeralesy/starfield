(in-package #:starfield)
(in-readtable :qtools)

(defparameter *star-count* (+ 900 (random 200)))
(defparameter *star-colors* NIL)
(defparameter *twinkle-time* 15)
(defparameter *twinkle-chance* 2)

;; === Starfield ===
(defclass starfield (updatable paintable)
  ((stars :initarg :stars :initform NIL :accessor stars)))

(defmethod initialize-instance :after ((starfield starfield) &key)
  (setf (slot-value starfield 'stars) NIL)
  (let ((quarter-stars (floor (/ *star-count* 4)))
        (half-stars (floor (/ *star-count* 2))))
    (dotimes (i *star-count*)
      (push (make-instance 'star
                           :id i
                           :location (list (random (q+:width *main-window*))
                                           (random (q+:height *main-window*)))
                           :brightness (+ (random 2)
                                          (cond ((< i quarter-stars) 1)
                                                ((< i half-stars) 3)
                                                (T 5)))
                           :color (q+:make-qcolor
                                   (nth (random (length *star-colors*))
                                        *star-colors*)))
            (stars starfield)))))

(defmethod update ((starfield starfield))
  (call-next-method)
  (loop for star in (stars starfield)
        do (update star)))

(defmethod paint ((starfield starfield) target)
  (call-next-method)
  (loop for star in (stars starfield)
        do (paint star target)))

(defmethod finalize ((starfield starfield))
  (loop for star in (stars starfield)
        do (finalize star)))

;; === Star ===
(defclass star (updatable paintable)
  ((id :initarg :id :accessor id)
   (location :initarg :location :accessor location)
   (brightness :initarg :brightness :accessor brightness)
   (color :initarg :color :accessor color)
   (twinkle :initform NIL :accessor twinkle))
  (:default-initargs
   :id (error "Please give star an id.")
   :location (error "Please define coordinates.")
   :brightness 5
   :color (q+:make-qcolor 255 255 255)))

(defmethod initialize-instance :after ((star star) &key)
  (when (< 10 (brightness star)) (setf (slot-value star 'brightness) 10))
  (when (< (brightness star) 1) (setf (slot-value star 'brightness) 1)))

(defmethod update ((star star))
  (call-next-method)
  (let ((twinkle (twinkle star)))
    (if twinkle
        (when (< twinkle *cycle*)
          (setf (slot-value star 'twinkle) NIL))
        (unless (< *twinkle-chance* (random 100))
          (setf (slot-value star 'twinkle) (+ *cycle* *twinkle-time*))))))

(defmethod paint ((star star) target)
  (call-next-method)
  (let ((loc (location star)))
    (with-finalizing ((darker (q+:darker (color star) 200)))
      (with-finalizing
          ((pen (q+:make-qpen (if (twinkle star)
                                  darker
                                  (color star)))))
        (q+:set-pen target pen)
        (q+:draw-point target (first loc) (second loc))))))

(defmethod finalize ((star star))
  (finalize (color star)))
