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
  (
   ;; (background-image :initform *play-ground-image*)
   (background-color :initform "white")
   (width :initform *width*)
   (height :initform *height*)
   (eva :initform (make-instance 'eva))))

(defclass eva (node)
  ((image :initform *eva-image*)
   (speed :initform 0)
   (heading :initform (direction-heading :down))))

(defmethod update ((eva eva))
  (with-slots (heading speed) eva
    (let ((head (find-direction)))
      (when head
        (setf speed 5)
        (setf heading (direction-heading head)))
      (move eva heading speed))
    ))

(defclass wall (node)
  ((color :initform "gray50")))

(defmethod collide ((eva eva)
                    (wall wall))
  (with-slots (heading speed) eva
    (move eva (opposite-heading heading) speed)
    ;; (aim eva (direction-heading :down))
    (aim eva (opposite-heading heading))
    ;; (format t "~&heading: ~S" heading)
    ;; (setf heading (opposite-heading))
    ;; (setf speed (* speed 2))
    ))

(defmethod collide :after ((eva eva)
                           (wall wall))
  (play-sample "bip.wav"))

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
        ;; (insert (make-wall left (+ top (units 6)) width (units 1)))
        (insert (make-wall left top
                           (- right left) (units 1)))
        ;; bottom
        ;; (insert (make-wall left (- bottom (units 1)) width (units 1)))
        ;; left
        ;; (insert (make-wall left (+ top (units 1))
        ;;                    (units 1) (- height (units 2))))
        ;; right
        ;; (insert (make-wall (- right (units 1)) (+ top (units 1))
        ;;                    (units 1) (- height (units 2))))
        (current-buffer))))

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
      (paste world (make-border 0 0
                                (- *width* (units 1))
                                (- *height* (units 1)))))))

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
