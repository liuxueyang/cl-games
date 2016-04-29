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

(defmethod update ((eva eva))
  (with-slots (heading speed) eva
    (let ((head (find-direction)))
      (when head
        (setf speed 1)
        (setf heading (direction-heading head)))
      (move eva heading speed))))

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
      (insert eva))))

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
