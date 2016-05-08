;;;; bpmf.lisp

(in-package #:bpmf)

;;; "bpmf" goes here. Hacks and glory await!

;; configure global variables
(defparameter *width* 640)
(defparameter *height* 480)
(defparameter *unit* 16)
;; (defparameter *bgm* "unknown-flower.wav")

;; useful functions
(defun units (n) (* *unit* n))
(defmacro mac (expression)
  `(pprint (macroexpand-1 ',expression)))

;; resources
;; (defresource "unknown-flower.wav" :volume 20)
(defresource "bip.wav" :volume 20)

;; (defparameter *play-ground-image* "play-ground.png")
(defparameter *general-image* "general.png")
(defparameter *enemy-images* '("enemy-1.png"
                               "enemy-2.png"
                               "enemy-3.png"))
(defparameter *bullet-image* "bullet.png")

;; game logic

;; class definitions
(defclass world (buffer)
  ((background-color :initform "white")
   (width :initform *width*)
   (height :initform *height*)
   (general :initform (make-instance 'general))))

(defclass enemy (node)
  ((image :initform (random-choose *enemy-images*))
   (speed :initform 1)
   (heading :initform (direction-heading
                       :down
                       ;; (random-choose *directions*)
                       )
            :initarg :heading)))

(defmacro new-enemy ()
  (let ((enemy (gensym)))
    `(let ((,enemy (make-instance 'enemy)))
       (move-to ,enemy
                (random (- *width* (units 2)))
                (random (- *height* (units 2))))
       (insert ,enemy))))

(defclass bullet (node)
  ((image :initform *bullet-image*)
   (speed :initform 9)
   (heading :initform nil
            :initarg :heading)))

(defclass general (node)
  ((image :initform *general-image*)
   (speed :initform 0)
   (heading :initform (direction-heading :down))))

(defclass wall (node)
  ((color :initform "gray50")))

;; ====================

(defun general ()
  (slot-value (current-buffer) 'general))

;; update method of classes.

(defmethod update ((general general))
  (with-slots (heading speed x y) general
    (let ((head (find-direction)))
      (when head
        (setf speed 5)
        (setf heading (direction-heading head))
        (move general heading speed))))
  (fire-bullet general))

(defmethod update :around ((general general))
  (with-slots (heading speed x y) general
    (if (or (< x (units 1))
            (< y (units 1))
            ;; this works. however, i don't know why. ;-)
            (> (+
                x
                (image-width *general-image*))
               (- *width* (units 2)))
            (> (+
                y
                (image-height *general-image*))
               (- *height* (units 2))))
        (progn
          (move general (opposite-heading heading) (* 2 speed))
          (aim general (opposite-heading heading))
          (setf heading (opposite-heading heading)))
        (call-next-method))))

(defmethod update ((bullet bullet))
  (with-slots (heading speed frame-clock) bullet
    (move bullet heading speed)))

(defmethod update ((enemy enemy))
  (percent-of-time 1
                   (enemy-fire-bullet enemy))
  (with-slots (heading speed) enemy
    (move enemy heading speed)
    (percent-of-time 0.5
                     (setf heading
                           (direction-heading
                            :up
                            ;; (random-choose *directions*)
                            )))))

;; ====================

;; collide methods

(defmethod collide ((enemy enemy)
                    (bullet bullet))
  (destroy enemy)
  (destroy bullet)
  (format t "enemy and bullet collision.....~%")
  ;; (play-sample "bip.wav")
  (new-enemy))

(defmethod collide ((enemy enemy)
                    (wall wall))
  (destroy enemy)
  (new-enemy))

(defmethod collide ((bullet1 bullet)
                    (bullet2 bullet))
  ;; (play-sample "bip.wav")
  (destroy bullet1)
  (destroy bullet2))

(defmethod collide ((bullet bullet)
                    (general general))
  ;; (play-sample "bip.wav")
  (destroy bullet))

;; (defmethod collide ((enemy enemy)
;;                     (general general))
;;   ;; (play-sample "bip.wav")
;;   (destroy enemy))

(defmethod collide ((bullet bullet)
                    (wall wall))
  (destroy bullet))

;; ====================

(defmethod fire-bullet ((general general))
  (when (holding-lshift)
    (with-slots (x y heading) general
      (add-node (current-buffer)
                (make-instance 'bullet :heading heading)
                (+ x 7) (- y 2)))
    (play-sample "bip.wav")))

(defmethod enemy-fire-bullet ((enemy enemy))
  (with-slots (x y) enemy
    (let ((direction (random-choose *directions*)))
      (add-node
       (current-buffer)
       (make-instance
        'bullet
        :heading (direction-heading direction))
       x
       y))))

(defun make-wall (x y width height)
  (let ((wall (make-instance 'wall)))
    (resize wall width height)
    (move-to wall x y)
    wall))

(defun make-border (x y width height)
  (let ((left x)
        (top y)
        (right (+ x width))
        (botenemy (+ y height)))
    (with-new-buffer
        ;; top
        (insert (make-wall left top
                           (- right left) (units 1)))
      ;; botenemy
      (insert (make-wall left (- botenemy (units 1))
                         width (units 1)))
      ;; left
      (insert (make-wall left top (units 1) height))
      ;; right
      (insert (make-wall (- right (units 1)) top
                         (units 1) height))
      (current-buffer))))

(defun generate-enemy ()
  (with-new-buffer
      (dotimes (i 5)
        (let ((enemy (make-instance 'enemy)))
          (move-to enemy
                   (random (- *width* (units 2)))
                   ;; (units 4)
                   (random (- *height* (units 2))))
          (insert enemy)))
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
  (with-slots (general) world
    (with-buffer world
      (insert general)
      (move-to general (/ *width* 2) (/ *height* 2))
      (paste world (make-border 0 0 *width* *height*))
      (paste world (generate-enemy)))))

(defun bpmf ()
  (setf *screen-height* *height*)
  (setf *screen-width* *width*)
  (setf *resizable* t)
  (setf *scale-output-to-window* t)
  (with-session
      (open-project :bpmf)
    (index-all-images)
    (index-pending-resources)
    ;; (play-sample *bgm*)
    (let ((world (make-instance 'world)))
      (switch-to-buffer world)
      (start-game world))))

