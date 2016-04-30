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

;; resources
(defresource "unknown-flower.wav" :volume 20)
(defresource "bip.wav" :volume 20)

(defparameter *play-ground-image* "play-ground.png")
(defparameter *eva-image* "eva.png")
(defparameter *tom-image* "tom.png")
(defparameter *bullet-image* "bullet.png")

;; game logic
(defclass world (buffer)
  ((background-image :initform *play-ground-image*)
   (width :initform *width*)
   (height :initform *height*)
   (eva :initform (make-instance 'eva))))

(defclass eva (node)
  ((image :initform *eva-image*)
   (speed :initform 0)
   (heading :initform (direction-heading :down))))

(defun eva ()
  (slot-value (current-buffer) 'eva))

(defmethod update ((eva eva))
  (with-slots (heading speed) eva
    (let ((head (find-direction)))
      (when head
        (setf speed 5)
        (setf heading (direction-heading head))
        (move eva heading speed))))
  (fire-bullet eva))

(defclass bullet (node)
  ((image :initform *bullet-image*)
   (speed :initform 10)
   (heading :initform nil
            :initarg :heading)))

(defmethod update ((bullet bullet))
  (with-slots (heading speed) bullet
    (move bullet heading speed)))

(defclass tom (node)
  ((image :initform *tom-image*)
   (speed :initform 5)))

(defmethod collide ((tom tom)
                    (bullet bullet))
  (remove-node (current-buffer) tom)
  (remove-node (current-buffer) bullet)
  (play-sample "bip.wav"))

(defclass wall (node)
  ((color :initform "gray50")))

(defmethod collide ((eva eva)
                    (wall wall))
  (with-slots (heading speed) eva
    (move eva (opposite-heading heading) (* 2 speed))
    (aim eva (opposite-heading heading))
    (setf heading (opposite-heading heading))))

(defmethod collide ((bullet bullet)
                    (wall wall))
  (remove-node (current-buffer) bullet))

(defmethod collide :after ((eva eva)
                           (wall wall))
  (play-sample "bip.wav"))

(defmethod fire-bullet ((eva eva))
  (when (holding-lshift)
    (with-slots (x y heading) eva
      (choose-bullet-image heading)
      (add-node (current-buffer)
                (make-instance 'bullet :heading heading)
                x y))))

(defun choose-bullet-image (heading)
  (case (heading-direction heading)
    (:up (setf *bullet-image* "bullet-up.png"))
    (:down (setf *bullet-image* "bullet-down.png"))
    (:left (setf *bullet-image* "bullet-left.png"))
    (:right (setf *bullet-image* "bullet-right.png"))))

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
        (insert (make-wall left top (- right left) (units 0.5)))
      ;; bottom
      (insert (make-wall left (- bottom (units 0.5))
                         width (units 0.5)))
      ;; left
      (insert (make-wall left top (units 0.5) height))
      ;; right
      (insert (make-wall (- right (units 0.5)) top
                         (units 0.5) height))
      (current-buffer))))

(defun generate-tom ()
  (with-new-buffer
      (dotimes (i 5)
        (let ((tom (make-instance 'tom)))
          (move-to tom (random *width*) (random *height*))
          (insert tom)))
    (current-buffer)))

(defun holding-down-arraw ()
  (keyboard-down-p :down))

(defun holding-up-arraw ()
  (keyboard-down-p :up))

(defun holding-left-arraw ()
  (keyboard-down-p :left))

(defun holding-right-arraw ()
  (keyboard-down-p :right))

(defun holding-escape ()
  (keyboard-down-p :escape))

(defun holding-lshift ()
  (keyboard-pressed-p :lshift))

(defun find-direction ()
  (cond ((holding-up-arraw) :up)
        ((holding-down-arraw) :down)
        ((holding-left-arraw) :left)
        ((holding-right-arraw) :right)
        ((holding-escape) (quit))))

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
    (play-sample *bgm*)
    (let ((world (make-instance 'world)))
      (switch-to-buffer world)
      (start-game world))))
