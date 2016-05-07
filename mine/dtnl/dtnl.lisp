;;;; dtnl.lisp

(in-package #:dtnl)

;;; "dtnl" goes here. Hacks and glory await!

;; configure global variables
(defparameter *width* 640)
(defparameter *height* 480)
(defparameter *unit* 16)
(defparameter *bgm* "unknown-flower.wav")

;; useful functions
(defun units (n) (* *unit* n))
(defmacro mac (expression)
  `(pprint (macroexpand-1 ',expression)))

;; resources
(defresource "unknown-flower.wav" :volume 20)
(defresource "bip.wav" :volume 20)

(defparameter *play-ground-image* "play-ground.png")
(defparameter *eva-image* "eva.png")
(defparameter *tom-image* "tom.png")
(defparameter *bullet-image* "bullet.png")
(defparameter *tom-bullet-image* "tom-bullet.png")

;; game logic

;; class definitions
(defclass world (buffer)
  ((background-color :initform "black")
   (width :initform *width*)
   (height :initform *height*)
   (eva :initform (make-instance 'eva))))

(defclass tom (node)
  ( ;; (image :initform *tom-image*)
   (speed :initform 1)
   (width :initform 10)
   (height :initform 10)
   (heading :initform (direction-heading
                       (random-choose *directions*))
            :initarg :heading)))

(defclass bullet (node)
  ((image :initform *bullet-image*)
   (speed :initform 14)
   (width :initform 10)
   (height :initform 10)
   (heading :initform nil
            :initarg :heading)
   (frame-clock :initform 100)))

(defclass tom-bullet (node)
  ( ;; (image :initform *tom-bullet-image*)
   (speed :initform 7)
   (width :initform 10)
   (height :initform 10)
   (color :initform "white")
   (heading :initform (nth (random (length *directions*))
                           *directions*)
            :initarg :heading)))

(defclass eva (node)
  ( ;; (image :initform *eva-image*)
   (speed :initform 0)
   (width :initform 20)
   (height :initform 10)
   (color :initform "green")
   (heading :initform (direction-heading :down))))

(defclass wall (node)
  ((color :initform "gray50")))

;; ====================

(defun eva ()
  (slot-value (current-buffer) 'eva))

;; update method of classes.

(defmethod update ((eva eva))
  (with-slots (heading speed x y height width) eva
    (let ((head (find-direction)))
      (when head
        (setf speed 5)
        (if (or (equal head :right)
                (equal head :left))
            (progn (setf height 20)
                   (setf width 10))
            (progn (setf height 10)
                   (setf width 20)))
        (setf heading (direction-heading head))
        (move eva heading speed))))
  (fire-bullet eva))

(defmethod update :around ((eva eva))
  (with-slots (heading speed x y) eva
    (if (or (< x (units 1))
            (< y (units 1))
            ;; this works. however, i don't know why. ;-)
            (> (+ x (image-width *eva-image*)) (- *width* (units 2)))
            (> (+ y (image-height *eva-image*)) (- *height* (units 2))))
        (progn
          (move eva (opposite-heading heading) (* 2 speed))
          (aim eva (opposite-heading heading))
          (setf heading (opposite-heading heading)))
        (call-next-method))))

(defmethod update ((bullet bullet))
  (with-slots (heading speed frame-clock) bullet
    (move bullet heading speed)
    (decf frame-clock)
    (unless (plusp frame-clock)
      (remove-node (current-buffer) bullet)))
  (with-slots (x y) bullet
    (cond ((< x 0)
           (move-to bullet *width* y))
          ((> x *width*)
           (move-to bullet 0 y))
          ((< y 0)
           (move-to bullet x *height*))
          ((> y *height*)
           (move-to bullet x 0)))))

(defmethod update ((tom-bullet tom-bullet))
  (with-slots (heading speed) tom-bullet
    (move tom-bullet heading speed)))

(defmethod draw ((tom-bullet tom-bullet))
  (with-slots (x y width height heading) tom-bullet
    (draw-textured-rectangle-*
     x y 0 width height
     (find-texture "tom-bullet-down.png")
     ;; adjust angle to normalize for up-pointing sprites 
     :angle (+ 90 (heading-degrees heading)))))

(defmethod draw ((bullet bullet))
  (with-slots (x y width height heading) bullet
    (draw-textured-rectangle-*
     x y 0 width height
     (find-texture "tom-bullet-down.png")
     ;; adjust angle to normalize for up-pointing sprites 
     :angle (+ 90 (heading-degrees heading)))))

(defmethod update ((tom tom))
  (percent-of-time 1
                   (tom-fire-bullet tom))
  (with-slots (heading speed) tom
    (move tom heading speed)
    (percent-of-time 0.5
                     (setf heading
                           (direction-heading
                            (random-choose *directions*))))))

;; ====================

;; collide methods

(defmethod collide ((tom tom)
                    (bullet bullet))
  ;; (remove-node (current-buffer) tom)
  ;; (remove-node (current-buffer) bullet)
  (destroy tom)
  (destroy bullet)
  (play-sample "bip.wav")
  (let ((tom (make-instance 'tom)))
    (move-to tom
             (random (- *width* (units 2)))
             (random (- *height* (units 2))))
    (insert tom)))

(defmethod collide ((tom tom)
                    (wall wall))
  (with-slots (heading speed) tom
    (setf heading (opposite-heading heading))
    (move tom heading speed)))

(defmethod collide ((bullet1 bullet)
                    (bullet2 bullet))
  (play-sample "bip.wav")
  (destroy bullet1)
  (destroy bullet2))

(defmethod collide ((bullet bullet)
                    (eva eva))
  (play-sample "bip.wav")
  (destroy bullet))

(defmethod collide ((tom-bullet tom-bullet)
                    (eva eva))
  (play-sample "bip.wav")
  (destroy tom-bullet))

(defmethod collide ((tom tom)
                    (eva eva))
  (play-sample "bip.wav")
  (destroy tom))

;; (defmethod collide ((eva eva)
;;                     (bullet bullet))
;;   (with-slots (frame-clock) bullet
;;     (when (< frame-clock 90)
;;       (remove-node (current-buffer) eva)
;;       (remove-node (current-buffer) bullet)
;;       (play-sample "bip.wav"))))

;; (defmethod collide ((bullet bullet)
;;                     (wall wall))
;;   (with-slots (x y) bullet
;;     (cond ((< x 0)
;;            (move-to bullet *width* y))
;;           ((> x *width*)
;;            (move-to bullet 0 y))
;;           ((< y 0)
;;            (move-to bullet x *height*))
;;           ((> y *height*)
;;            (move-to bullet x 0))))
;;   ;; (remove-node (current-buffer) bullet)
;;   )

(defmethod collide :after ((tom tom)
                           (bullet bullet))
  (play-sample "bip.wav"))

(defmethod collide ((tom-bullet tom-bullet)
                    (wall wall))
  (destroy tom-bullet))

;; ====================

;; (defmethod collide ((eva eva)
;;                     (wall wall))
;;   (with-slots (heading speed x y) eva
;;     (move eva (opposite-heading heading) (* 2 speed))
;;     (aim eva (opposite-heading heading))
;;     (setf heading (opposite-heading heading))
;;     ;; (format t "~&x: ~S, y: ~S" x y)
;;     ))

(defmethod fire-bullet ((eva eva))
  (when (holding-lshift)
    (with-slots (x y heading) eva
      (choose-bullet-image heading)
      (add-node (current-buffer)
                (make-instance 'bullet :heading heading)
                x y))))

(defmethod tom-fire-bullet ((tom tom))
  (with-slots (x y) tom
    (let ((direction (nth (random (length *directions*))
                          *directions*)))
      ;; (choose-tom-bullet-image (direction-heading direction))
      (add-node (current-buffer)
                (make-instance 'tom-bullet
                               :heading (direction-heading direction))
                x y))))

(defmacro choose-image (var object heading)
  "var is the variable to set, object is the object name of
string type, heading is the object slot."
  `(setf ,var
         (direction-2-image-name (heading-direction ,heading)
                                 ,object)))

(defmacro direction-2-image-name (direction object)
  "direction is a keyword such as :up, :down,
object is an string, the name of the object"
  `(concatenate 'string ,object "-"
                (string-downcase (symbol-name ,direction))
                ".png"))

(defun choose-bullet-image (heading)
  (choose-image *bullet-image* "bullet" heading))

(defun choose-tom-bullet-image (heading)
  (choose-image *tom-bullet-image* "tom-bullet" heading))

(defun make-wall (x y width height)
  (let ((wall (make-instance 'wall)))
    (resize wall width height)
    (move-to wall x y)
    wall))

(defun make-border (x y width height)
  (let ((left x)
        (top y)
        (right (+ x width))
        (bottom (+ y height)))
    (with-new-buffer
        ;; top
        (insert (make-wall left top
                           (- right left) (units 0.1)))
      ;; bottom
      (insert (make-wall left (- bottom (units 0.1))
                         width (units 0.1)))
      ;; left
      (insert (make-wall left top (units 0.1) height))
      ;; right
      (insert (make-wall (- right (units 0.1)) top
                         (units 0.1) height))
      (current-buffer))))

(defun generate-tom ()
  (with-new-buffer
      (dotimes (i 5)
        (let ((tom (make-instance 'tom)))
          (move-to tom
                   (random (- *width* (units 2)))
                   (random (- *height* (units 2))))
          (insert tom)))
    (current-buffer)))

;; keyboard control functions.

(defun holding-lshift ()
  (keyboard-pressed-p :lshift))

(defmacro monitor-keyboard (&rest keys)
  `(and ,@(loop for key in keys collect `(keyboard-down-p ,key))))

(defun find-direction ()
  (cond ((monitor-keyboard :down :left) :downleft)
        ((monitor-keyboard :down :right):downright)
        ((monitor-keyboard :up :left) :upleft)
        ((monitor-keyboard :up :right) :upright)
        ((monitor-keyboard :up) :up)
        ((monitor-keyboard :down) :down)
        ((monitor-keyboard :left) :left)
        ((monitor-keyboard :right) :right)
        ((monitor-keyboard :escape) (quit))))

;; ====================

(defmethod start-game ((world world))
  (with-slots (eva) world
    (with-buffer world
      (insert eva)
      (move-to eva (/ *width* 2) (/ *height* 2))
      (paste world (make-border 0 0 *width* *height*))
      (paste world (generate-tom)))))

(defun dtnl ()
  (setf *screen-height* *height*)
  (setf *screen-width* *width*)
  (setf *resizable* t)
  (setf *scale-output-to-window* t)
  (with-session
    (open-project :dtnl)
    (index-all-images)
    (index-pending-resources)
    ;; (play-sample *bgm*)
    (let ((world (make-instance 'world)))
      (switch-to-buffer world)
      (start-game world))))
