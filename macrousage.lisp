#! /usr/local/bin/sbcl --script

(load
 (merge-pathnames
             "quicklisp/setup"
             (user-homedir-pathname)))

(let ((*standard-output* *error-output*))
  (ql:quickload :iterate)
  (ql:quickload :optima)
  (ql:quickload :alexandria)
  (ql:quickload :vecto))

(defpackage :macrousage
  (:use :cl :iterate :optima :alexandria :vecto)
  (:shadow :rotate))
(in-package :macrousage)

;; (barman p01 nf-fffd 3600 2000000 597 158815 233252 387 27.798)
;; (dom    prob solver maxt maxm   cost time   mem macrocost preprocess)

(defvar *dim* 350)
(defvar *max* 0)
(defvar scale 1)

(defun draw-point (x y)
  (arc x y (/ 1 scale) 0 (* 2 PI))
  (stroke))

(defun draw-grid (max d)
  (iter (for x from 0 to max by d)
        (draw-line x 0 x max)
        (draw-line 0 x max x)))

(defun draw-line (x1 y1 x2 y2)
  (move-to x1 y1)
  (line-to x2 y2)
  (stroke))

(defun myprint (threshold)
  (print threshold)
  (with-canvas (:width *dim* :height *dim*)
    (let (points)
      (handler-case
          (iter (match (read *standard-input*)
                  ((and x (list* _ _ _ _ _ cost time _ macrocost _))
                   (when (< time (* 1000 threshold)) ;; msec->sec
                     (when (plusp macrocost) ;; -1 in standard planners
                       (push (list cost macrocost) points)
                       (when (< cost macrocost)
                         (print x))
                       (setf *max* (max *max* macrocost cost)))))))
        (end-of-file (c)
          (let ((path (format nil "./macrousage-~a.png" threshold)))
            (print path)
            (setf scale (/ *dim* *max*))
            (format t "~&max: ~a scale: ~a" *max* scale)
            
            (draw-line 0 0 *dim* *dim*)
            (draw-grid *dim* (* (expt 10 (floor (log *max* 10))) scale))
            
            (scale scale scale)
            (set-line-width (/ 1 scale))
            (mapc (lambda (p) (apply #'draw-point p)) points)
            ;; (set-rgb-stroke 1 0 0)
            ;; (draw-point 150 150) ; testing
            (save-png path)))))))

(myprint (parse-integer (second sb-ext:*posix-argv*)))

(terpri)
