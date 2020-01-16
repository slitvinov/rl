;-*- Mode: Lisp; Package: (g :use (common-lisp)) -*-

; Source Code for G, a low-level, device-independent, graphics language
; for Macintosh Common Lisp.  See http://envy.cs.umass.edu/People/sutton/G/g.html.

(defpackage :g
            (:use :common-lisp :ccl))

(in-package :g)


;;;; OBJECTS

(export '(*g-device* g-view g-window g-device
          g-get-parent g-set-parent g-get-children g-close-view))

(defvar *g-device*)                                           ;[Doc]

(defclass g-view (view)                                       ;[Doc]
  ((cs-left :initform 0.0s0 :accessor cs-left)
   (cs-bottom :initform 0.0s0 :accessor cs-bottom)
   (cs-right :initform  1.0s0 :accessor cs-right)
   (cs-top :initform  1.0s0 :accessor cs-top)
   (offsetx :accessor offsetx)
   (offsety :accessor offsety)
   (scalex :accessor scalex)
   (scaley :accessor scaley)))

(defmethod initialize-instance ((view g-view) &key parent)    ;[Doc]
  (without-event-processing
    (call-next-method)
    (unless (or (typep view 'g-device)
                (typep view 'window)
                (eq parent :none))
      (unless parent
        (setq parent (or (front-window :class 'g-window) (front-window))))
      (set-view-container view parent)
      (set-view-size view (view-size parent)))
    (g-update-normalization view)))

(defclass g-device (g-view)                                   ;[Doc]
  ((children :initform nil :accessor children))
  (:default-initargs
    :view-size (make-point *screen-width* *screen-height*)))

(defclass g-window (g-view window)                            ;[Doc]
  ((parent :initarg :parent)
   (last-color :initform nil))
  (:default-initargs
    :parent *g-device*
    :color-p t))

(defmethod initialize-instance :after                         ;[Doc]
           ((window g-window) &key g-viewport gd-viewport g-viewport-r gd-viewport-r)
  (without-event-processing
    (when g-viewport (apply #'g-set-viewport window g-viewport))
    (when gd-viewport (apply #'gd-set-viewport window gd-viewport))
    (when g-viewport-r (apply #'g-set-viewport-r window g-viewport-r))
    (when gd-viewport-r (apply #'gd-set-viewport-r window gd-viewport-r))
    (let ((device (g-get-parent window)))
      (push window (children device))))
  (when (typep (target) 'listener)
    (window-select (target))))

(defmethod g-close-view ((view view))                       ;[Doc]
  "Let the parent know this view is no longer in use"
  (remove-subviews (g-get-parent view) view))

(defmethod g-close-view ((window g-window))
  "Close the window and let the parent know this view is no longer in use"
  (setf (children (g-get-parent window))
        (remove window (children (g-get-parent window))))
  (window-close window))

(defmethod g-close-view ((window window))
  (window-close window))

(defmethod g-get-parent ((view simple-view))                       ;[Doc]
  (view-container view))

(defmethod g-get-parent ((window g-window))
  (slot-value window 'parent))

(defmethod g-get-parent ((window window))
  *g-device*)

(defmethod g-set-parent ((view view) new-parent)
  (let ((parent (g-get-parent view)))
    (when parent (remove-subviews parent view))
    (set-view-container view new-parent)))

(defmethod g-set-parent ((window window) new-parent)
  (with-slots (parent) window
    (when parent
      (setf (children parent)
            (remove window (children parent))))
    (setq parent new-parent)))

(defmethod g-get-children ((view g-view))                     ;[Doc]
  (subviews view))

(defmethod g-get-children ((device g-device))
  (if (eq device *g-device*)
    (union (children device) (windows))
    (children device)))



;;; VIEWS and COLORS and COORDINATE SYSTEMS

(export '(g-set-viewport g-get-viewport gd-set-viewport gd-get-viewport
          g-set-viewport-r g-get-viewport-r gd-set-viewport-r gd-get-viewport-r
          g-set-viewport-size gd-set-viewport-size
          g-set-coordinate-system g-get-coordinate-system gd-get-coordinate-system
          g-set-coordinate-system-r g-get-coordinate-system-r gd-get-coordinate-system-r
          g-set-coordinate-system-scale g-set-coordinate-system-scale-r
          g-set-cs-r g-get-cs-r gd-get-cs-r
          g-set-cs g-get-cs gd-get-cs
          g-set-cs-scale g-get-cs-scale
          g-accept-new-viewport-size
          g-accept-new-viewport-position
          g-coord-x g-coord-y gd-coord-x gd-coord-y
          g-offset-x g-offset-y gd-offset-x gd-offset-y
          g-color-rgb g-color-rgb-255
          g-color-bw
          g-color-pen
          g-color-user-pick
          g-color-black
          g-color-white
          g-color-pink
          g-color-red
          g-color-orange
          g-color-yellow
          g-color-green
          g-color-dark-green
          g-color-light-blue
          g-color-blue
          g-color-purple
          g-color-brown
          g-color-tan
          g-color-light-gray
          g-color-gray
          g-color-cyan
          g-color-magenta
          g-color-dark-gray
          g-color-flip
          g-color-invisible
          g-color-on
          g-color-off
          g-set-color
          set-view-size
          set-view-position
          g-accept-new-viewport-position
          g-update-normalization
          gd-convert-x
          gd-convert-y
          g-convert-x
          g-convert-y))


(defmethod set-view-size ((view g-view) h &optional v)
  (declare (ignore h v))
  (without-event-processing
    (call-next-method)
    (g-update-normalization view)
    (g-accept-new-viewport-size view)))

(defmethod set-view-position ((view g-view) x-pos &optional y-pos)
  (declare (ignore x-pos y-pos))
  (without-event-processing
    (call-next-method)
    (g-accept-new-viewport-position view)))

(defmethod g-accept-new-viewport-size ((view g-view))         ;[Doc]
  )

(defmethod g-accept-new-viewport-position ((view g-view))
  )


(defclass screen-coordinates (view) ())

(defmethod g-accept-new-viewport-position :before ((view screen-coordinates))
  (set-view-scroll-position
   view (local-to-global view (view-scroll-position view)))
  (loop for child in (g-get-children view)
        do (g-accept-new-viewport-position child)))


(defun gd-get-viewport (view)                                 ;[Doc]
  (let ((point1 (view-position view))
        (point2 (view-size view)))
    (setq point2 (add-points point1 point2))
    (values (point-h point1)
            (point-v point1)
            (1- (point-h point2))
            (1- (point-v point2)))))

(defun g-get-viewport (view)                                  ;[Doc]
  (let ((parent (g-get-parent view)))
    (multiple-value-bind (dx1 dy1 dx2 dy2) (gd-get-viewport view)
      (let ((x1 (g-coord-x parent dx1))
            (y1 (g-coord-y parent dy1))
            (x2 (g-coord-x parent dx2))
            (y2 (g-coord-y parent dy2)))
        (values (min x1 x2)
                (min y1 y2)
                (max x1 x2)
                (max y1 y2))))))

(defun gd-get-viewport-r (view)                               ;[Doc]
  (multiple-value-bind (x1 y1 x2 y2) (gd-get-viewport view)
    (values x1 y1 (- x2 x1) (- y2 y1))))

(defun g-get-viewport-r (view)                                ;[Doc]
  (multiple-value-bind (x1 y1 x2 y2) (g-get-viewport view)
    (values (min x1 x2)
            (min y1 y2)
            (abs (- x2 x1))
            (abs (- y2 y1)))))

(defun gd-set-viewport (view dx1 dy1 dx2 dy2)       ;[Doc]
  (set-view-position view (min dx1 dx2) (min dy1 dy2))
  (set-view-size view
                 (1+ (abs (- dx1 dx2)))
                 (1+ (abs (- dy1 dy2)))))

(defun g-set-viewport (view vpx1 vpy1 vpx2 vpy2)              ;[Doc]
  (let ((parent (g-get-parent view)))
    (gd-set-viewport view
                     (gd-coord-x parent vpx1)
                     (gd-coord-y parent vpy1)
                     (and vpx2 (gd-coord-x parent vpx2))
                     (and vpy2 (gd-coord-y parent vpy2)))))

                                                              ;[Doc]
(defun gd-set-viewport-r (view dx dy &optional delta-x delta-y)
  (when (null dx) (setq dx (nth-value 0 (gd-get-viewport view))))
  (when (null dy) (setq dy (nth-value 1 (gd-get-viewport view))))
  (when (null delta-x) (setq delta-x (nth-value 2 (gd-get-viewport-r view))))
  (when (null delta-y) (setq delta-y (nth-value 3 (gd-get-viewport-r view))))
  (gd-set-viewport view dx dy (+ dx delta-x) (+ dy delta-y)))

(defun g-set-viewport-r (view x y &optional delta-x delta-y)  ;[Doc]
  (when (null x) (setq x (nth-value 0 (g-get-viewport view))))
  (when (null y) (setq y (nth-value 1 (g-get-viewport view))))
  (when (null delta-x) (setq delta-x (nth-value 2 (g-get-viewport-r view))))
  (when (null delta-y) (setq delta-y (nth-value 3 (g-get-viewport-r view))))
  (g-set-viewport view x y (+ x delta-x) (+ y delta-y)))

(defun g-get-cs (view)
  (g-get-coordinate-system view))

(defun g-get-coordinate-system (view)                         ;[Doc]
  (with-slots (cs-left cs-right cs-bottom cs-top) view
    (values (min cs-left cs-right)
            (min cs-top cs-bottom)
            (max cs-right cs-left)
            (max cs-top cs-bottom)
            (if (<= cs-left cs-right)
              (if (<= cs-bottom cs-top)
                :lower-left
                :upper-left)
              (if (<= cs-bottom cs-top)
                :lower-right
                :upper-right)))))

(defun gd-get-cs (view)
  (gd-get-coordinate-system view))

(defun gd-get-coordinate-system (view)                        ;[Doc]      ;
  (let ((min-point (view-scroll-position view))
        (max-point (view-size view)))
    (setq max-point (add-points min-point max-point))
    (values (point-h min-point)
            (point-v min-point)
            (1- (point-h max-point))
            (1- (point-v max-point))
            :upper-left)))

(defun g-get-cs-r (view)
  (g-get-coordinate-system-r view))

                                                              ;[Doc]
(defun g-get-coordinate-system-r (view)
  (multiple-value-bind (x1 y1 x2 y2 corner) (g-get-coordinate-system view)
    (values x1 y1 (- x2 x1) (- y2 y1) corner)))

(defun gd-get-cs-r (view)
  (gd-get-coordinate-system-r view))

                                                              ;[Doc]
(defun gd-get-coordinate-system-r (view)
  (multiple-value-bind (x1 y1 x2 y2 corner) (gd-get-coordinate-system view)
    (values x1 y1 (- x2 x1) (- y2 y1) corner)))

                                                              ;[Doc]
(defun g-get-cs-scale (view)
  (g-get-coordinate-system-scale view))

                                                              ;[Doc]
(defun g-get-coordinate-system-scale (view)
  (multiple-value-bind (x1 y1 x2 y2 corner) (g-get-coordinate-system view)
    (declare (ignore x2 y2))
    (with-slots (scalex scaley) view
      (values x1 y1 scalex scaley corner))))

(defun g-set-cs (view x1 y1 x2 y2 &optional (corner :lower-left))
  (g-set-coordinate-system view x1 y1 x2 y2 corner))

                                                              ;[Doc]
(defun g-set-coordinate-system (view x1 y1 x2 y2 &optional (corner :lower-left))
  (setq x1 (coerce x1 'float)
        y1 (coerce y1 'float)
        x2 (coerce x2 'float)
        y2 (coerce y2 'float))
  (cond ((= x1 x2)
         (print "Attempt to set left and right of G coordinate system to same values.")
         (setq x2 (+ x1 1)))
        ((= y1 y2)
         (print "Attempt to set top and bottom of G coordinate system to same values.")
         (setq y2 (+ y1 1)))
        (t (with-slots (cs-left cs-right cs-top cs-bottom) view
    (setf cs-left (if (member corner '(:lower-left :upper-left)) x1 x2))
    (setf cs-bottom (if (member corner '(:lower-left :lower-right)) y1 y2))
    (setf cs-right (if (member corner '(:lower-left :upper-left)) x2 x1))
    (setf cs-top (if (member corner '(:lower-left :lower-right)) y2 y1)))))
  (g-update-normalization view))

(defun g-set-cs-r (view x y delta-x delta-y &optional (corner :lower-left))
  (g-set-coordinate-system-r view x y delta-x delta-y corner))

                                                              ;[Doc]
(defun g-set-coordinate-system-r (view x y delta-x delta-y &optional (corner :lower-left))
  (g-set-coordinate-system view x y (+ x delta-x) (+ y delta-y) corner))

                                                              ;[Doc]
(defun g-set-cs-scale (view x y x-scale &optional y-scale (corner :lower-left))
  (g-set-coordinate-system-scale view x y x-scale y-scale corner))

                                                              ;[Doc]
(defun g-set-coordinate-system-scale (view x y x-scale &optional y-scale (corner :lower-left))
  (when (not (numberp x-scale))
    (setq x-scale (ecase x-scale
                    (:inches 72)
                    (:centimeters 28.35)
                    (:pixels 1)
                    (:points 1))))
  (when (not (numberp y-scale))
    (setq y-scale (ecase y-scale
                    ('nil x-scale)
                    (:inches 72)
                    (:centimeters 28.35)
                    (:pixels 1)
                    (:points 1))))
  (multiple-value-bind (dx1 dy1 dx2 dy2) (gd-get-viewport view)
    (let ((x2 (+ x (/ (abs (- dx1 dx2)) (float x-scale))))
          (y2 (+ y (/ (abs (- dy1 dy2)) (float y-scale)))))
      (g-set-coordinate-system view x y x2 y2 corner))))

;(defun gd-within-coordinate-system-p (view dx dy)
;  (multiple-value-bind (min-x min-y max-x max-y) (gd-get-coordinate-system view)
;    (and (<= min-x dx max-x)
;         (<= min-y dy max-y))))

(defun g-update-normalization (view)
  "Updates state variables of normalized coordinate system"
  (with-slots (scalex scaley offsetx offsety cs-left cs-bottom cs-right cs-top) view
    (multiple-value-bind (x1 y1 x2 y2) (gd-get-coordinate-system view)
      (if (= cs-right cs-left)
        (error "Attempt to establish invalid (zero area) G coordinate system")
        (setf scalex (/ (- x2 x1) (- cs-right cs-left))))
      (setf offsetx (- x1 (* cs-left scalex)))
      (if (= cs-bottom cs-top)
        (error "Attempt to establish invalid (zero area) G coordinate system")
        (setf scaley (/ (- y2 y1) (- cs-bottom cs-top))))
      (setf offsety (- y1 (* cs-top scaley))))))

(defun gd-coord-x (view x)                                    ;[Doc]
  (with-slots (offsetx scalex) view
    (round (+ offsetx
              (* x scalex)))))

(defun gd-coord-y (view y)                                    ;[Doc]
  (with-slots (offsety scaley) view
    (round (+ offsety
              (* y scaley)))))

(defun gd-coords (view x y)
  (values (gd-coord-x view x) (gd-coord-y view y)))
                                                              ;[Doc]
(defun gd-offset-x (view x-offset)
  "Returns the length in device coords (pixels) of the x-distance in normal coords"
  (with-slots (scalex) view
    (round (* x-offset scalex))))
                                                              ;[Doc]
(defun gd-offset-y (view y-offset)
  "Returns the length in device coords (pixels) of the y-distance in normal coords"
  (with-slots (scaley) view
    (round (* y-offset scaley))))

(defun gd-offset (view x-offset y-offset)
  (values (gd-offset-x view x-offset) (gd-offset-y view y-offset)))

;;; Converting from device to normal coordinates

(defun g-coord-x (view dx)                                    ;[Doc]
  (with-slots (offsetx scalex) view
    (/ (- dx offsetx) scalex)))

(defun g-coord-y (view dy)                                    ;[Doc]
  (with-slots (offsety scaley) view
    (/ (- dy offsety) scaley)))

(defun g-coords (view dx dy)
  (values (g-coord-x view dx) (g-coord-y view dy)))

(defun g-offset-x (view dx-offset)                            ;[Doc]
  (with-slots (scalex) view
    (round (/ dx-offset scalex))))

(defun g-offset-y (view dy-offset)                            ;[Doc]
  (with-slots (scaley) view
    (round (/ dy-offset scaley))))

(defun g-offset (view dx-offset dy-offset)
  (values (g-offset-x view dx-offset) (g-offset-y view dy-offset)))

;;; Converting coordinates between views:

                                                              ;[Doc]
(defun gd-convert-x (from-view to-view dx)
  (point-h (global-to-local to-view (local-to-global from-view (make-point dx 0)))))

                                                              ;[Doc]
(defun gd-convert-y (from-view to-view dy)
  (point-v (global-to-local to-view (local-to-global from-view (make-point 0 dy)))))

                                                              ;[Doc]
(defun g-convert-x (from-view to-view x)
  (g-coord-x to-view (gd-convert-x from-view to-view (gd-coord-x from-view x))))

                                                              ;[Doc]
(defun g-convert-y (from-view to-view y)
  (g-coord-y to-view (gd-convert-y from-view to-view (gd-coord-y from-view y))))


;; Color Routines

(defvar *pens* (make-hash-table :test #'equal))
(defvar *colors* (make-hash-table :test #'equal))

(defun translate-pattern (keyword)
  (ecase keyword
    (:black-pattern *black-pattern*)
    (:white-pattern *white-pattern*)
    (:gray-pattern *gray-pattern*)
    (:light-gray-pattern *light-gray-pattern*)
    (:dark-gray-pattern *dark-gray-pattern*)))

                                                              ;[Doc]
(defun g-color-pen (view color pattern &optional mode x-size y-size)
  "Returns a new color with specified pen characteristics"
  (declare (ignore view))
  (let (pen)
    (setq pen (if (atom color)
                (list (or pattern :black-pattern)
                      (or mode :patCopy)
                      (make-point (or x-size 1) (or y-size 1)))
                (list (or pattern (second color))
                      (or mode (third color))
                      (make-point (or x-size (point-h (fourth color)))
                                  (or y-size (point-v (fourth color)))))))
    (setq pen (or (gethash pen *pens*)
                  (setf (gethash pen *pens*) pen)))
    (setq color (cons (if (atom color)
                        color (first color))
                      pen))
    (or (gethash color *colors*)
        (setf (gethash color *colors*) color))))

(defun g-color-pen-flip (view color)
  (g-color-pen view color :black-pattern :patXor))

(defun g-color-pen-invisible (view color)
  (g-color-pen view color :black-pattern :NotPatOr))

(defun g-color-size (view color x-size &optional y-size)
  (g-color-pen view color nil nil x-size y-size))

                                                              ;[Doc]
(defun g-color-black (view) (declare (ignore view)) *black-color*)
(defun g-color-white (view) (declare (ignore view)) *white-color*)
(defun g-color-pink (view) (declare (ignore view)) *pink-color*)
(defun g-color-red (view) (declare (ignore view)) *red-color*)
(defun g-color-orange (view) (declare (ignore view)) *orange-color*)
(defun g-color-yellow (view) (declare (ignore view)) *yellow-color*)
(defun g-color-green (view) (declare (ignore view)) *green-color*)
(defun g-color-dark-green (view) (declare (ignore view)) *dark-green-color*)
(defun g-color-light-blue (view) (declare (ignore view)) *light-blue-color*)
(defun g-color-blue (view) (declare (ignore view)) *blue-color*)
(defun g-color-purple (view) (declare (ignore view)) *purple-color*)
(defun g-color-brown (view) (declare (ignore view)) *brown-color*)
(defun g-color-tan (view) (declare (ignore view)) *tan-color*)
(defun g-color-light-gray (view) (declare (ignore view)) *light-gray-color*)
(defun g-color-gray (view) (declare (ignore view)) *gray-color*)
(defun g-color-dark-gray (view) (declare (ignore view)) *dark-gray-color*)
(defun g-color-on (view) (declare (ignore view)) *black-color*)
(defun g-color-off (view) (declare (ignore view)) *white-color*)
(defun g-color-cyan (view) (g-color-rgb view 0 1 1))
(defun g-color-magenta (view) (g-color-rgb view 1 0 1))

(defun g-color-flip (view)
  (g-color-pen-flip view *black-color*))

(defun g-color-invisible (view)
  (g-color-pen-invisible view *black-color*))

(defun g-color-rgb (view red green blue)                      ;[Doc]
  (declare (ignore view))
  (setq red (min 1.0 (max 0.0 red)))
  (setq green (min 1.0 (max 0.0 green)))
  (setq blue (min 1.0 (max 0.0 blue)))
  (make-color (floor (* 65535 red))
              (floor (* 65535 green))
              (floor (* 65535 blue))))

(defun g-color-rgb-255 (view red green blue)                  ;[Doc]
  (declare (ignore view))
  (setq red (min 255 (max 0 red)))
  (setq green (min 255 (max 0 green)))
  (setq blue (min 255 (max 0 blue)))
  (make-color (floor (* 257 red))
              (floor (* 257 green))
              (floor (* 257 blue))))

(defvar black *black-color*)
(defvar white *white-color*)
(defvar pink *pink-color*)
(defvar red *red-color*)
(defvar orange *orange-color*)
(defvar yellow *yellow-color*)
(defvar green *green-color*)
(defvar dark-green *dark-green-color*)
(defvar light-blue *light-blue-color*)
(defvar blue *blue-color*)
(defvar purple *purple-color*)
(defvar brown *brown-color*)
(defvar tan *tan-color*)
(defvar light-gray *light-gray-color*)
(defvar gray *gray-color*)
(defvar dark-gray *dark-gray-color*)
(defvar on *black-color*)
(defvar off *white-color*)
(defvar cyan (g-color-cyan t))
(defvar magenta (g-color-magenta t))
(defvar flip (g-color-flip t))
(defvar invisible (g-color-invisible t))

(defun g-color-bw (view intensity)                            ;[Doc]
;  (when (= 4 (ccl::screen-bits (ccl::window-screen (view-window view))))
;    (setq intensity (/ (round (* 15 intensity)) 15)))
  (setq intensity (- 1 intensity))
  (g-color-rgb view intensity intensity intensity))

(defun g-color-user-pick (view &rest args)                    ;[Doc]
  (declare (ignore view))
  (apply #'user-pick-color args))

(defun g-set-color (view color)                               ;[Doc]
  (set-color-if-needed (view-window view) color))

(defun set-color-if-needed (window color)
  "Sets the fore-color and pen of the window, if needed, to color"
  (if (not (typep window 'g-window))
    (if (atom color)                  ; not a g-window
      (set-fore-color window color)
      (progn (set-fore-color window (first color))
             (pen-normal window)
             (when (neq (second color) :black-pattern)
               (set-pen-pattern window (translate-pattern (second color))))
             (when (neq (third color) :patCopy)
               (set-pen-mode window (third color)))
             (when (neq (fourth color) #@(1 1))
               (set-pen-size window (fourth color)))))
    (with-slots (last-color) window   ; is a g-window
      (unless (eq color last-color)
        (if (atom color)
          (if (atom last-color)       ; color is atomic
            (set-fore-color window color)
            (progn (when (neq color (first last-color))
                     (set-fore-color window color))
                   (pen-normal window)))
          (if (atom last-color)       ; color is list
            (progn (when (neq (first color) last-color)       ; last-color atomic
                     (set-fore-color window (first color)))
                   (when (neq (second color) :black-pattern)
                     (set-pen-pattern window (translate-pattern (second color))))
                   (when (neq (third color) :patCopy)
                     (set-pen-mode window (third color)))
                   (when (neq (fourth color) #@(1 1))
                     (set-pen-size window (fourth color))))
            (progn (when (neq (first color) (first last-color))       ; both lists
                     (set-fore-color window (first color)))
                   (when (neq (second color) (second last-color))
                     (set-pen-pattern window (translate-pattern (second color))))
                   (when (neq (third color) (third last-color))
                     (set-pen-mode window (third color)))
                   (when (neq (fourth color) (fourth last-color))
                     (set-pen-size window (fourth color))))))
        (setq last-color color)))))


;;; GD-GRAPHICS

(export 'gd-draw-point)
(export 'gd-draw-line)
(export 'gd-draw-line-r)
(export 'gd-outline-rect)
(export 'gd-outline-rect-r)
(export 'gd-fill-rect)
(export 'gd-fill-rect-r)
(export 'gd-draw-circle)
(export 'gd-draw-disk)
(export 'gd-draw-arc)
(export 'gd-draw-text)
(export 'gd-draw-text-centered)
(export 'gd-text-width)
(export 'gd-text-height)
(export 'gd-get-cursor-position)

(defun gd-draw-point (view dx dy &optional color-code)        ;[Doc]
  (without-interrupts
;   (unless (eq *current-view* view) (focus-view view))
   (with-focused-view view
   (when color-code (set-color-if-needed (view-window view) color-code))
   (#_MoveTo :long (make-point dx dy))
   (#_Line :long #@(0 0)))))
                                                              ;[Doc]
(defun gd-draw-line (view dx1 dy1 dx2 dy2 &optional color-code)
  (without-interrupts
;   (unless (eq *current-view* view) (focus-view view))
   (with-focused-view view
   (when color-code (set-color-if-needed (view-window view) color-code))
   (#_MoveTo :long (make-point dx1 dy1))
   (#_LineTo :long (make-point dx2 dy2)))))

                                                              ;[Doc]
(defun gd-draw-line-r (view dx dy delta-x delta-y &optional color-code)
  (gd-draw-line view dx dy (+ dx delta-x) (+ dy delta-y) color-code))

                                                              ;[Doc]
(defun gd-outline-rect (view dx1 dy1 dx2 dy2 &optional color-code)
  (gd-outline-rect-r view dx1 dy1 (- dx2 dx1) (- dy2 dy1) color-code))

                                                              ;[Doc]
(defun gd-outline-rect-r (view dx dy delta-x delta-y &optional color-code)
  (incf delta-x dx)
  (incf delta-y dy)
  (rlet ((rect :rect))
    (points-to-rect (make-point dx dy)
                    (make-point delta-x delta-y)
                    rect)
    (incf (rref rect :rect.bottom))
    (incf (rref rect :rect.right))
    (without-interrupts
;   (unless (eq *current-view* view) (focus-view view))
   (with-focused-view view
     (when color-code (set-color-if-needed (view-window view) color-code))
     (#_FrameRect rect)))))

                                                              ;[Doc]
(defun gd-fill-rect (view dx1 dy1 dx2 dy2 &optional color-code)
  (gd-fill-rect-r view dx1 dy1 (- dx2 dx1) (- dy2 dy1) color-code))

                                                              ;[Doc]
(defun gd-fill-rect-r (view dx dy delta-x delta-y color-code)
  (incf delta-x dx)
  (incf delta-y dy)
  (rlet ((rect :rect))
    (points-to-rect (make-point dx dy)
                    (make-point delta-x delta-y)
                    rect)
    (incf (rref rect :rect.bottom))
    (incf (rref rect :rect.right))
    (without-interrupts
;   (unless (eq *current-view* view) (focus-view view))
   (with-focused-view view
     (when color-code (set-color-if-needed (view-window view) color-code))
     (#_PaintRect rect)))))

                                                              ;[Doc]
(defun gd-draw-circle (view dx dy dradius &optional color-code)
  (without-interrupts
;   (unless (eq *current-view* view) (focus-view view))
   (with-focused-view view
   (when color-code (set-color-if-needed (view-window view) color-code))
   (ccl::with-rectangle-arg
     (r (- dx dradius) (- dy dradius) (+ dx dradius) (+ dy dradius))
     (#_FrameOval r)))))

                                                              ;[Doc]
(defun gd-draw-arc (view dx dy dradius start-angle angle &optional color-code)
  (without-interrupts
;   (unless (eq *current-view* view) (focus-view view))
   (with-focused-view view
   (when color-code (set-color-if-needed (view-window view) color-code))
   (ccl::with-rectangle-arg
     (r (- dx dradius) (- dy dradius) (+ dx dradius) (+ dy dradius))
     (#_FrameArc r (- 90 start-angle) (- angle))))))

                                                              ;[Doc]
(defun gd-draw-disk (view dx dy dradius &optional color-code)
  (without-interrupts
;   (unless (eq *current-view* view) (focus-view view))
   (with-focused-view view
   (when color-code (set-color-if-needed (view-window view) color-code))
   (ccl::with-rectangle-arg
     (r (- dx dradius) (- dy dradius) (+ dx dradius) (+ dy dradius))
     (#_PaintOval r)))))

                                                              ;[Doc]
(defun gd-draw-text (view text font dx dy &optional color-code)
  (without-interrupts
   ;   (unless (eq *current-view* view) (focus-view view))
   (with-focused-view view
     (when color-code (set-color-if-needed (view-window view) color-code))
     (#_MoveTo :long (make-point dx dy))
     (let ((old-font (view-font view)))
       (unwind-protect
         (progn (set-view-font view font)
                (stream-write-string view text 0 (length text)))
         (set-view-font view old-font))))))

                                                              ;[Doc]
(defun gd-draw-text-centered (view string font dx dy &optional color-code)
  (let ((half-length (round (* .5 (gd-text-width view string font))))
        (half-height (round (* .5 (gd-text-height view string font)))))
   (gd-draw-text view string font (- dx half-length) (- dy half-height) color-code)))

(defun gd-text-width (view-ignore string character-style)     ;[Doc]
  (declare (ignore view-ignore))
  (string-width string character-style))

                                                              ;[Doc]
(defun gd-text-height (view-ignore text-ignore character-style)
  (declare (ignore view-ignore text-ignore))
  (font-info character-style))

; gd-read-cursor (calls view-mouse-position and converts coords)

                                                              ;[Doc]
(defun gd-get-cursor-position (view)
  "Returns the current cursor position in appropriate coordinates"
  (let* ((point (view-mouse-position view))
         (dx (point-h point))
         (dy (point-v point)))
      (values dx dy)))




;;; G-GRAPHICS

(export 'g-clear)
(export 'g-make-visible)
(export 'g-draw-point)
(export 'g-draw-line)
(export 'g-draw-line-r)
(export 'g-outline-rect)
(export 'g-outline-rect-r)
(export 'g-fill-rect)
(export 'g-fill-rect-r)
(export 'g-draw-circle)
(export 'g-draw-disk)
(export 'g-draw-arc)
(export 'g-draw-text)
(export 'g-draw-text-centered)
(export 'g-text-width)
(export 'g-text-height)
(export 'g-get-cursor-position)

                                                              ;[Doc]
(defun g-clear (view &optional (color (g-color-off view)))
  (multiple-value-bind (min-x min-y max-x max-y) (gd-get-coordinate-system view)
    (gd-fill-rect view min-x min-y max-x max-y color)))

(defun g-make-visible (view)                                  ;[Doc]
  (if (typep view 'window)
    (window-select view)
    (g-make-visible (view-container view))))

(defun g-draw-point (view x y &optional color)                ;[Doc]
  (let ((dx (gd-coord-x view x))
        (dy (gd-coord-y view y)))
     (gd-draw-point view dx dy color)))

(defun g-draw-line (view x1 y1 x2 y2 &optional color)         ;[Doc]
  (let ((dx1 (gd-coord-x view x1))
        (dy1 (gd-coord-y view y1))
        (dx2 (gd-coord-x view x2))
        (dy2 (gd-coord-y view y2)))
    (gd-draw-line view dx1 dy1 dx2 dy2 color)))
                                                              ;[Doc]
(defun g-draw-line-r (view x y delta-x delta-y &optional color)
  (g-draw-line view x y (+ x delta-x) (+ y delta-y) color))

(defun g-outline-rect (view x1 y1 x2 y2 &optional color)      ;[Doc]
  (let ((dx1 (gd-coord-x view x1))
        (dy1 (gd-coord-y view y1))
        (dx2 (gd-coord-x view x2))
        (dy2 (gd-coord-y view y2)))
     (gd-outline-rect view dx1 dy1 dx2 dy2 color)))
                                                              ;[Doc]
(defun g-outline-rect-r (view x y delta-x delta-y &optional color)
  (let ((dx1 (gd-coord-x view x))
        (dy1 (gd-coord-y view y))
        (dx2 (gd-coord-x view (+ x delta-x)))
        (dy2 (gd-coord-y view (+ y delta-y))))
    (gd-outline-rect view dx1 dy1 dx2 dy2 color)))

(defun g-fill-rect (view x1 y1 x2 y2 &optional color)         ;[Doc]
  (let ((dx1 (gd-coord-x view x1))
        (dy1 (gd-coord-y view y1))
        (dx2 (gd-coord-x view x2))
        (dy2 (gd-coord-y view y2)))
    (gd-fill-rect view dx1 dy1 dx2 dy2 color)))
                                                              ;[Doc]
(defun g-fill-rect-r (view x y delta-x delta-y &optional color)
  (let ((dx1 (gd-coord-x view x))
        (dy1 (gd-coord-y view y))
        (dx2 (gd-coord-x view (+ x delta-x)))
        (dy2 (gd-coord-y view (+ y delta-y))))
    (gd-fill-rect view dx1 dy1 dx2 dy2 color)))

(defun g-draw-circle (view x y radius &optional color)        ;[Doc]
  (let ((dx (gd-coord-x view x))
        (dy (gd-coord-y view y))
        (dradius (gd-offset-x view radius)))
    (gd-draw-circle view dx dy dradius color)))

(defun g-draw-disk (view x y radius &optional color)          ;[Doc]
  (let ((dx (gd-coord-x view x))
        (dy (gd-coord-y view y))
        (dradius (gd-offset-x view radius)))
    (gd-draw-disk view dx dy dradius color)))
                                                              ;[Doc]
(defun g-draw-arc (view x y radius start-angle angle &optional color)
  (let ((dx (gd-coord-x view x))
        (dy (gd-coord-y view y))
        (dradius (gd-offset-x view radius)))
    (gd-draw-arc view dx dy dradius start-angle angle color)))

(defun g-draw-text (view string font x y &optional color)     ;[Doc]
  (let ((dx (gd-coord-x view x))
        (dy (gd-coord-y view y)))
    (gd-draw-text view string font dx dy color)))
                                                              ;[Doc]
(defun g-draw-text-centered (view string font x y &optional color)
  (let ((half-length (* .5 (g-text-width view string font)))
        (half-height (* .5 (g-text-height view string font))))
   (g-draw-text view string font (- x half-length) (- y half-height) color)))

                                                              ;[Doc]
(defun g-text-width (view string character-style)
  (g-offset-x view (gd-text-width view string character-style)))

(defun g-text-height (view text character-style)              ;[Doc]
 (g-offset-y view (gd-text-height view text character-style)))

                                                              ;[Doc]
(defun g-get-cursor-position (view)
  "Returns the current cursor position in appropriate coordinates"
  (multiple-value-bind (dx dy) (gd-get-cursor-position view)
    (when dx
      (g-coords view dx dy))))



;;; EVENTS


; View-click-event-handler for a gus-window calls g-click-event-handler and
;  gd-click-event-handler for each gus context.
; Window-mouse-up-event-handler for a gus-window calls
;  g-mouse-up-event-handler for each gus-context.

(export '(gd-click-event-handler g-click-event-handler
          g-mouse-up-event-handler gd-cursor g-cursor
          g-draw-view view-draw-contents
          view-click-event-handler
          *grow-cursor* *cross-hair-cursor*))

(defmethod view-draw-contents ((view g-view))
  (g-draw-view view))

(defmethod g-draw-view ((view g-view))                        ;[Doc]
  (loop for child in (g-get-children view)
        do (view-draw-contents child)))

; To respond to a mouse click on a view you specialize a method
;    (gd-click-event-handler g-view dx dy)
; or (g-click-event-handler g-view x y)
; which will be called each time there is a mouse click in that view.

(defmethod view-click-event-handler :after ((view g-view) point-of-click)
  (let ((dx (point-h point-of-click))
        (dy (point-v point-of-click)))
    (gd-click-event-handler view dx dy)
    (g-click-event-handler view
                           (g-coord-x view dx)
                           (g-coord-y view dy))))

(defmethod gd-click-event-handler ((view g-view) dx dy)       ;[Doc]
  (declare (ignore dx dy)))
(defmethod g-click-event-handler ((view g-view) x y)          ;[Doc]
  (declare (ignore x y)))

(defmethod window-mouse-up-event-handler ((view g-view))
  (g-mouse-up-event-handler view))

(defmethod g-mouse-up-event-handler ((view g-view)) )

(defmethod view-cursor ((view g-view) point-of-click)
  (let ((dx (point-h point-of-click))
        (dy (point-v point-of-click)))
    (or (g-cursor view
                  (g-coord-x view dx)
                  (g-coord-y view dy))
        (gd-cursor view dx dy)
        *arrow-cursor*)))

(defmethod gd-cursor ((view g-view) dx dy)                    ;[Doc]
  (declare (ignore dx dy)))
(defmethod g-cursor ((view g-view) x y)                       ;[Doc]
  (declare (ignore x y)))

;;;;;;;;;;;;;;;;;;;;
;;
;; cursor hacking
;;

(defun make-cursor (data-string mask-string hotspot)
  (when (or (> (length (string data-string)) 64)
            (> (length (string mask-string)) 64))
    (error "data-string & mask-string must be < 64 chars long"))
  (rlet ((data :bits16)
         (mask :bits16))
    (with-pstrs ((data-str data-string)
                 (mask-str mask-string))
      (#_StuffHex :ptr data :ptr data-str)
      (#_StuffHex :ptr mask :ptr mask-str))
    (make-record :cursor
               :data data
               :mask mask
               :hotspot hotspot)))

(defun make-grow-cursor ()
  (make-cursor "00003FC02040204027F82448244824483FC80408040807F80000000000000000"
               "00003FC03FC03FC03FF83FF83FF83FF83FF807F807F807F80000000000000000"
               #@(2 3)))

(defvar *grow-cursor* (make-grow-cursor))

;the cross-hair-cursor
(defun make-cross-hair-cursor ()
  (make-cursor "04000400040004000400FFE00400040004000400040004000000000000000000"
               "0000000000000000000000000000000000000000000000000000000000000000"
               #@(5 5)))

(defvar *cross-hair-cursor* (make-cross-hair-cursor))



;;; ADDITIONAL

(export '(gd-draw-arrow gd-draw-arrowhead g-draw-arrow g-draw-arrowhead
          gd-draw-arrow-r gd-draw-arrowhead-r g-draw-arrow-r g-draw-arrowhead-r))

                                                              ;[Doc]
(defun gd-draw-arrow (view dx1 dy1 dx2 dy2 &optional color)
  "Draws an arrow starting at dx1,dy1 and ending at dx2,dy2 of color"
  (gd-draw-arrowhead view dx1 dy1 dx2 dy2 1.0 0.25 color))
                                                              ;[Doc]
(defun gd-draw-arrow-r (view dx dy delta-x delta-y &optional color)
  "Draws an arrow starting at dx,dy and ending at dx+delta-x,dy+delta-y of color"
  (gd-draw-arrowhead view dx dy (+ dx delta-x) (+ dy delta-y) 1.0 0.25 color))

(defvar angle-tangent 0.7)
;	Angle-tangent is the tangent of the angle between the base  main
;  part of the arrow and one of the two parts of the arrowhead.  I don't
;  know what happens if you make this parameter negative.  The default
;  value is 0.7.
                                                              ;[Doc]
(defun gd-draw-arrowhead (view dx1 dy1 dx2 dy2 body-size head-size &optional color)
  "Draws an arrowhead dx2,dy2 from dx1,dy1 of color and sizes"
  (let ((delta-x (* (- dx2 dx1) head-size))
        (delta-y (* (- dy2 dy1) head-size)))
    (unless (= 0 body-size)			;draw arrow body
      (gd-draw-line-r view dx2 dy2
                      (round (* body-size (- dx1 dx2)))
                      (round (* body-size (- dy1 dy2))) color))
    (gd-draw-line-r view dx2 dy2
                    (round (- (* (- delta-y) Angle-tangent) delta-x))
                    (round (- (* delta-x Angle-tangent) delta-y))
                    color)
    (gd-draw-line-r view dx2 dy2
                    (round (- (* delta-y Angle-tangent) delta-x))
                    (round (- (* (- delta-x) Angle-tangent) delta-y))
                    color)))
                                                              ;[Doc]
(defun gd-draw-arrowhead-r (view dx dy delta-x delta-y body-size head-size &optional color)
  "Draws an arrowhead starting at dx,dy and ending at dx+delta-x,dy+delta-y of color and sizes"
  (gd-draw-arrowhead view dx dy (+ dx delta-x) (+ dy delta-y) body-size head-size color))

(defun g-draw-arrow (view x1 y1 x2 y2 &optional color)        ;[Doc]
  (let ((dx1 (gd-coord-x view x1))
        (dy1 (gd-coord-y view y1))
        (dx2 (gd-coord-x view x2))
        (dy2 (gd-coord-y view y2)))
    (gd-draw-arrowhead view dx1 dy1 dx2 dy2 1.0 0.25 color)))

                                                              ;[Doc]
(defun g-draw-arrow-r (view x y delta-x delta-y &optional color)
  (g-draw-arrow view x y (+ x delta-x) (+ y delta-y) color))

                                                              ;[Doc]
(defun g-draw-arrowhead (view x1 y1 x2 y2 body-size head-size &optional color)
  (let ((dx1 (gd-coord-x view x1))
        (dy1 (gd-coord-y view y1))
        (dx2 (gd-coord-x view x2))
        (dy2 (gd-coord-y view y2)))
    (gd-draw-arrowhead view dx1 dy1 dx2 dy2 body-size head-size color)))

                                                              ;[Doc]
(defun g-draw-arrowhead-r (view x y delta-x delta-y body-size head-size &optional color)
  (g-draw-arrowhead view x y (+ x delta-x) (+ y delta-y) body-size head-size color))


(export 'maintain-g-viewports-of-children)
(defclass maintain-g-viewports-of-children (g-view) () )

(defmethod g-accept-new-viewport-size
           ((view maintain-g-viewports-of-children))
  (let* ((children (g-get-children view))
         (g-viewports (loop for child in children
                            collect (multiple-value-list
                                     (g-get-viewport child)))))
    (call-next-method view)
    (loop for child in children
          for g-viewport in g-viewports
          do (apply #'g-set-viewport child g-viewport))))





;;; FINAL INITIALIZATION

(unless (boundp '*g-device*)
  (setq *g-device* (make-instance 'g-device)))

;;; More things, on g

(export '(with-g-coordinate-system
           with-g-cs
           with-g-coordinate-system-r
           with-g-cs-r
           with-g-coordinate-system-scale
           with-g-cs-scale))

(defmacro with-g-coordinate-system ((view x1 y1 x2 y2 &optional corner) &body body)
  `(multiple-value-bind (oldx1 oldy1 oldx2 oldy2 old-corner) (g-get-cs ,view)
    (unwind-protect
      (progn (g-set-coordinate-system ,view ,x1 ,y1 ,x2 ,y2 ,corner)
             . ,body)
      (g-set-coordinate-system ,view oldx1 oldy1 oldx2 oldy2 old-corner))))

(defmacro with-g-cs ((view x1 y1 x2 y2 &optional corner) &body body)
  `(with-g-coordinate-system (,view ,x1 ,y1 ,x2 ,y2 ,corner) . ,body))

(defmacro with-g-coordinate-system-r ((view x y delta-x delta-y &optional corner) &body body)
  `(multiple-value-bind (oldx1 oldy1 oldx2 oldy2 old-corner) (g-get-cs ,view)
    (unwind-protect
      (progn (g-set-coordinate-system-r ,view ,x ,y ,delta-x ,delta-y ,corner)
             . ,body)
      (g-set-coordinate-system ,view oldx1 oldy1 oldx2 oldy2 old-corner))))

(defmacro with-g-cs-r ((view x1 y1 x2 y2 &optional corner) &body body)
  `(with-g-coordinate-system-r (,view ,x1 ,y1 ,x2 ,y2 ,corner) . ,body))

(defmacro with-g-coordinate-system-scale ((view x y xscale &optional yscale corner) &body body)
  `(multiple-value-bind (oldx1 oldy1 oldx2 oldy2 old-corner) (g-get-cs ,view)
    (unwind-protect
      (progn (g-set-coordinate-system-scale ,view ,x ,y ,xscale ,yscale ,corner)
             . ,body)
      (g-set-coordinate-system ,view oldx1 oldy1 oldx2 oldy2 old-corner))))

(defmacro with-g-cs-scale ((view x y xscale &optional yscale corner) &body body)
  `(with-g-coordinate-system-scale (,view ,x ,y ,xscale ,yscale ,corner) . ,body))

#|
(export 'basic-g-colors)                ; mix this with views to make color-slots
(export '(black white pink red orange yellow green dark-green light-blue blue purple
         brown tan light-gray gray dark-gray flip invisible on off))

(defclass basic-g-colors (view)
  (black white pink red orange yellow green dark-green light-blue blue purple
         brown tan light-gray gray dark-gray flip invisible on off))

(defmethod initialize-instance ((view basic-g-colors) &key)
  (without-event-processing
    (call-next-method)
    (with-slots (black white pink red orange yellow green dark-green light-blue blue purple
                       brown tan light-gray gray dark-gray flip invisible on off) view
      (setq black (g-color-black view))
      (setq white (g-color-white view))
      (setq pink (g-color-pink view))
      (setq red (g-color-red view))
      (setq orange (g-color-orange view))
      (setq yellow (g-color-yellow view))
      (setq green (g-color-green view))
      (setq dark-green (g-color-dark-green view))
      (setq light-blue (g-color-light-blue view))
      (setq blue (g-color-blue view))
      (setq purple (g-color-purple view))
      (setq brown (g-color-brown view))
      (setq tan (g-color-tan view))
      (setq light-gray (g-color-light-gray view))
      (setq gray (g-color-gray view))
      (setq dark-gray (g-color-dark-gray view))
      (setq flip (g-color-flip view))
      (setq invisible (g-color-invisible view))
      (setq on (g-color-on view))
      (setq off (g-color-off view)))))
|#

(export '(gd-within-viewport-p g-within-viewport-p))

(defun gd-within-viewport-p (view dx dy)
  "Is dx,dy within viewport of view, in the coordinate system of the parent of view?"
  (multiple-value-bind (x1 y1 x2 y2) (gd-get-viewport view)
    (and (< x1 dx x2)
         (< y1 dy y2))))

(defun g-within-viewport-p (view x y)
  "Is x,y within viewport of view, in the coordinate system of the parent of view?"
  (multiple-value-bind (x1 y1 x2 y2) (g-get-viewport view)
    (and (< x1 x x2)
         (< y1 y y2))))
