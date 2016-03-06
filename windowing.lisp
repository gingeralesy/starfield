(in-package #:starfield)
(in-readtable :qtools)

(define-widget main (QWidget)
  ((field :initform NIL :accessor field)))

(define-initializer (main setup)
  (setf *cycle* 0)
  (setf *main-window* main)
  (q+:resize main 1024 768)
  (setf (q+:window-title main) *title*)
  (setf *stars-rect* (list (q+:width main) (q+:height main)))
  (setf *star-colors*
        `(,(q+:make-qcolor 255 255 255)   ;; White
          ,(q+:make-qcolor 100 255 100)   ;; Green
          ,(q+:make-qcolor 100 100 255)   ;; Blue
          ,(q+:make-qcolor 100 255 255)   ;; Yellow
          ,(q+:make-qcolor 255 100 100))) ;; Red
  (setf (slot-value main 'field) (make-instance 'starfield :stars NIL)))

(define-finalizer (main teardown)
  (setf *main-window* NIL)
  (finalize (field main))
  (setf (slot-value main 'field) NIL)
  (loop for color in *star-colors*
        do (finalize color)))

(define-subwidget (main timer) (q+:make-qtimer main)
  (setf (q+:single-shot timer) T)
  (q+:start timer (round *fps*)))

(define-subwidget (main background) (q+:make-qcolor 0 0 0))

(define-slot (main update) ()
  (declare (connected timer (timeout)))
  (let ((start (get-internal-real-time)))
    (incf *cycle* 1)
    (with-simple-restart (abort "Abort the update and continue.")
      (update (field main)))
    (q+:repaint main)
    (q+:start timer 
              (round (max 0 (* (- *fps* (/ (- (get-internal-real-time) start)
                                           internal-time-units-per-second))
                               1000))))))

(define-override (main paint-event) (ev)
  (with-simple-restart (abort "Abort the drawing and continue.")
    (with-finalizing ((painter (q+:make-qpainter main))
                      (bgbrush (q+:make-qbrush background)))
      (setf (q+:render-hint painter) (q+:qpainter.antialiasing)
            (q+:render-hint painter) (q+:qpainter.text-antialiasing)
            (q+:render-hint painter) (q+:qpainter.smooth-pixmap-transform)
            (q+:render-hint painter) (q+:qpainter.high-quality-antialiasing)
            (q+:style (q+:background painter)) (q+:qt.solid-pattern)
            (q+:color (q+:background painter)) (q+:qt.black)
            (q+:style (q+:brush painter)) (q+:qt.solid-pattern))
      (q+:fill-rect painter (q+:rect main) bgbrush)
      (paint (field main) painter))))

(define-override (main resize-event) (ev)
  (let* ((size (q+:size ev))
         (width-d (- (q+:width size) (first *stars-rect*)))
         (height-d (- (q+:height size) (second *stars-rect*))))
    (when (< 0 width-d)
      (setf (first *stars-rect*) (q+:width size)))
    (when (< 0 height-d)
      (setf (second *stars-rect*) (q+:height size)))))
