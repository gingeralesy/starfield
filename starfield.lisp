(in-package #:starfield)
(in-readtable :qtools)

(defparameter *star-count* (+ 900 (random 200)))
(defparameter *twinkle-time* 15)
(defparameter *twinkle-chance* 500) ;; 500 means 1 in every 500 stars per cycle will twinkle

(defvar *star-colors* NIL)
(defvar *stars-rect* '(0 0))

;; === Starfield ===
(defclass starfield (updatable paintable)
  ((stars :initarg :stars :initform NIL :accessor stars)))

(defmethod initialize-instance :after ((starfield starfield) &key)
  (setf (slot-value starfield 'stars) NIL)
  (let ((quarter-stars (floor (/ *star-count* 4)))
        (half-stars (floor (/ *star-count* 2))))
    (dotimes (i *star-count*)
      (push (make-instance 'star
                           :location (list (random (first *stars-rect*))
                                           (random (second *stars-rect*)))
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
  ((location :initarg :location :accessor location)
   (brightness :initarg :brightness :accessor brightness)
   (color :initarg :color :accessor color)
   (darker :initform NIL :accessor darker)
   (twinkle :initform NIL :accessor twinkle)
   (light-pen :initform NIL :accessor light)
   (dark-pen :initform NIL :accessor dark))
  (:default-initargs
   :location (error "Please define coordinates.")
   :brightness 5
   :color (q+:make-qcolor 255 255 255)))

(defmethod initialize-instance :after ((star star) &key)
  (when (< 10 (brightness star)) (setf (slot-value star 'brightness) 10))
  (when (< (brightness star) 1) (setf (slot-value star 'brightness) 1))
  (setf (slot-value star 'darker) (q+:darker (color star) 200))
  (setf (slot-value star 'light-pen) (q+:make-qpen (color star)))
  (setf (slot-value star 'dark-pen) (q+:make-qpen (darker star))))

(defmethod update ((star star))
  (call-next-method)
  (when (and (<= (first (location star)) (q+:width *main-window*))
             (<= (second (location star)) (q+:height *main-window*)))
    (let ((twinkle (twinkle star)))
      (if twinkle
          (when (< twinkle *cycle*)
            (setf (slot-value star 'twinkle) NIL))
          (unless (twinkle-p)
            (setf (slot-value star 'twinkle) (+ *cycle* *twinkle-time*)))))))

(defmethod paint ((star star) target)
  (call-next-method)
  (let ((loc (location star)))
    (when (and (<= (first loc) (q+:width *main-window*))
               (<= (second loc) (q+:height *main-window*)))
      (q+:set-pen target (if (twinkle star) (dark star) (light star)))
      (q+:draw-point target (first loc) (second loc)))))

(defmethod finalize ((star star))
  (finalize (dark star))
  (finalize (light star))
  (finalize (darker star))
  (finalize (color star)))

(defun twinkle-p ()
  (< (floor (/ *star-count* *twinkle-chance*)) (random 100)))
