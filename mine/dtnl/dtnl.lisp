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
(defparameter *tom-bullet-image* "tom-bullet.png")

;; game logic

;; class definitions
(defclass world (buffer)
  ((background-image :initform *play-ground-image*)
   (width :initform *width*)
   (height :initform *height*)
   (eva :initform (make-instance 'eva))))

(defclass tom (node)
  ((image :initform *tom-image*)
   (speed :initform 3)
   (heading :initform (direction-heading
                       (nth (random (length *directions*))
                            *directions*))
            :initarg :heading)))

(defclass bullet (node)
  ((image :initform *bullet-image*)
   (speed :initform 14)
   (heading :initform nil
            :initarg :heading)
   (frame-clock :initform 100)))

(defclass tom-bullet (node)
  ((image :initform *tom-bullet-image*)
   (speed :initform 10)
   (heading :initform (nth (random (length *directions*))
                           *directions*)
            :initarg :heading)))

(defclass eva (node)
  ((image :initform *eva-image*)
   (speed :initform 0)
   (heading :initform (direction-heading :down))))

(defclass wall (node)
  ((color :initform "gray50")))

;; ====================

(defun eva ()
  (slot-value (current-buffer) 'eva))

;; update method of classes.

(defmethod update ((eva eva))
  (with-slots (heading speed x y) eva
    (let ((head (find-direction)))
      (when head
        (setf speed 5)
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

(defmethod update ((tom tom))
  (percent-of-time 1
                   (tom-fire-bullet tom))
  (with-slots (heading speed) tom
    (move tom heading speed)))

;; ====================

;; collide methods

(defmethod collide ((tom tom)
                    (bullet bullet))
  ;; (remove-node (current-buffer) tom)
  ;; (remove-node (current-buffer) bullet)
  (destroy tom)
  (destroy bullet)
  (play-sample "bip.wav"))

(defmethod collide ((tom tom)
                    (wall wall))
  (with-slots (heading speed) tom
    (setf heading (opposite-heading heading))
    (move tom heading speed)
    (format t "~&heading : ~S" heading)))

(defmethod collide ((bullet1 bullet)
                    (bullet2 bullet))
  (play-sample "bip.wav")
  (destroy bullet1)
  (destroy bullet2)
  ;; (remove-node (current-buffer) bullet1)
  ;; (remove-node (current-buffer) bullet2)
  )

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
      (choose-tom-bullet-image (direction-heading direction))
      (add-node (current-buffer)
                (make-instance 'tom-bullet
                               :heading (direction-heading direction))
                x y))))

(defun choose-bullet-image (heading)
  (setf *bullet-image*
        (case (heading-direction heading)
          (:up "bullet-up.png")
          (:down "bullet-down.png")
          (:left "bullet-left.png")
          (:right "bullet-right.png")
          (:downright "bullet-downright.png")
          (:downleft "bullet-downleft.png")
          (:upright "bullet-upright.png")
          (:upleft "bullet-upleft.png"))))

(defun choose-tom-bullet-image (heading)
  (setf *tom-bullet-image*
        (case (heading-direction heading)
          (:up "tom-bullet-up.png")
          (:down "tom-bullet-down.png")
          (:left "tom-bullet-left.png")
          (:right "tom-bullet-right.png")
          (:downright "tom-bullet-downright.png")
          (:downleft "tom-bullet-downleft.png")
          (:upright "tom-bullet-upright.png")
          (:upleft "tom-bullet-upleft.png"))))

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

(defun holding-down-arraw ()
  (keyboard-down-p :down))

(defun holding-up-arraw ()
  (keyboard-down-p :up))

(defun holding-left-arraw ()
  (keyboard-down-p :left))

(defun holding-right-arraw ()
  (keyboard-down-p :right))

(defun holding-down-left-arraw ()
  (and (keyboard-down-p :down)
       (keyboard-down-p :left)))

(defun holding-down-right-arraw ()
  (and (keyboard-down-p :down)
       (keyboard-down-p :right)))

(defun holding-up-left-arraw ()
  (and (keyboard-down-p :up)
       (keyboard-down-p :left)))

(defun holding-up-right-arraw ()
  (and (keyboard-down-p :up)
       (keyboard-down-p :right)))

(defun holding-escape ()
  (keyboard-down-p :escape))

(defun holding-lshift ()
  (keyboard-pressed-p :lshift))

(defun find-direction ()
  (cond ((holding-down-left-arraw) :downleft)
        ((holding-down-right-arraw) :downright)
        ((holding-up-left-arraw) :upleft)
        ((holding-up-right-arraw) :upright)
        ((holding-up-arraw) :up)
        ((holding-down-arraw) :down)
        ((holding-left-arraw) :left)
        ((holding-right-arraw) :right)
        ((holding-escape) (quit))))
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
    (play-sample *bgm*)
    (let ((world (make-instance 'world)))
      (switch-to-buffer world)
      (start-game world))))
