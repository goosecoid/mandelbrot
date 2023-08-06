(require :cl-raylib)
(defpackage mandelbrot
  (:use :cl))
(in-package :mandelbrot)

;; mandelbrot settings
(defparameter *x-bounds* (cons -2.00 0.47))
(defparameter *y-bounds* (cons -1.12 1.12))
(defparameter *max-iterations* 60)
(defparameter *escape-radius* 2)

(defun hsv-helper (hsv-lst)
  (destructuring-bind (h s v) hsv-lst
    `(:h ,h :s ,s :v ,v)))

;; draw settings
(defparameter *width* 600)
(defparameter *height* 600)
(defparameter *pixel-width* (/ (- (cdr *x-bounds*) (car *x-bounds*)) *width*))
(defparameter *pixel-height* (/ (- (cdr *y-bounds*) (car *y-bounds*)) *height*))
(defparameter *bittersweet-red* (hsv-helper '(5.0 0.603 0.929)))
(defparameter *atomic-tangerine* (hsv-helper '(20.0 0.471 1.0)))

;; Since h value in hsv represents an angle
;; we need to know if it's shorter to go ccw
;; or cw to interpolate the value
(defun lerp-hue (h1 h2 pct)
  (let* ((ccw (if (>= h1 h2)
                 (- h1 h2)
                 (+ 1 (- h1 h2))))
        (cw (if (>= h1 h2)
                (+ 1 (- h2 h1))
                (- h2 h1)))
        (h (if (<= cw ccw)
               (+ h1 (* cw pct))
               (- h1 (* ccw pct)))))
    (if (< h 0)
        (+ 1 h)
        (- h 1))))

;; return an extrapolated color (step) between dcolor
;; and lcolor giving a range (steps)
(defun lerp-color (dcolor lcolor step steps)
  (let* ((lh (getf lcolor :h))
         (ls (getf lcolor :s))
         (lv (getf lcolor :v))
         (dh (getf dcolor :h))
         (ds (getf dcolor :s))
         (dv (getf dcolor :v))
         (pct (/ step steps))
         (h (lerp-hue lh dh pct))
         (s (+ (* (- 1 pct) ls) (* pct ds)))
         (v (+ (* (- 1 pct) lv) (* pct dv))))
    `(:h ,h :s ,s :v ,v)))

(defun get-color-from-hsv-plist (color-plist)
  (let ((h (getf color-plist :h))
        (s (getf color-plist :s))
        (v (getf color-plist :v)))
    (cl-raylib:color-from-hsv h s v)))

;; calculate how many iterations it takes for z to escape
(defun calculate-iterations (cx cy)
  (loop with c = (complex cx cy)
        for iteration from 0 below *max-iterations*
        for z = c then (+ (* z z) c)
        while (< (abs z) *escape-radius*)
        finally (return iteration)))

;; plot the dots with the correct color
;; its shade is determined by the iteration value
(defun plot-mandelbrot ()
    (loop for y from 0 to *height*
          for cy from (car *y-bounds*) by *pixel-height*
          nconcing (loop for x from 0 to *width*
                         for cx from (car *x-bounds*) by *pixel-width*
                         for iteration = (calculate-iterations cx cy)
                         for color = (lerp-color *atomic-tangerine*
                                                 *bittersweet-red*
                                                 iteration
                                                 *max-iterations*)
                         collect (list color x y))))

(defun main ()
  (let ((screen-width *width*)
        (screen-height *height*))
    (cl-raylib:with-window (screen-width screen-height "Mandelbrot")
      (cl-raylib:set-target-fps 60)
      (loop
            until (cl-raylib:window-should-close)
            do (cl-raylib:with-drawing
                 (cl-raylib:clear-background cl-raylib:+white+)
                 (mapcar
                  (lambda (data)
                    (destructuring-bind (color x y) data
                      (cl-raylib:draw-circle x
                                             y
                                             1.0
                                             (get-color-from-hsv-plist color))))
                  (plot-mandelbrot)))))))

;; (main)
