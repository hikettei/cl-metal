
(cl:defpackage :cl-metal.examples.mandelbrot
  (:use :cl :cl-metal)
  (:export

   ))

(cl:in-package :cl-metal.examples.mandelbrot)

(use-device 0)

(define-kernel (conv2d :thread-position-in-grid id)
    (void ((x* int :io)))
    (for (i 0) (< i 10) (incf i)
	 (setf (aref x id) (aref x i))))

(define-mfunc (aux :style :lisp :stream t)
    (float ((x float :in) (y float :in) (a float :in) (b float :in) (m int :in)))
    (if (< m 100)
	(let ((x1 (- (* x x) (* y y) a))
	      (y1 (- (* 2.0 x y) b)))
	  (if (> (+ (* x1 x1) (* y1 y1)) 4.0)
	      (return m)
	      (return (aux x1 y1 a b (+ m 1)))))
	(return 0.0)))

(define-kernel (mandelbrot
		:thread-position-in-grid id
		:using (aux)
		:stream t)
    (void ((x* float :io)))
    (let ((a (/ (- (mod id 2048) 512) 1024.0))
	  (b (/ (- (/   id 2048) 1024) 1024.0)))
      (setf (aref x id) (aux 0.0 0.0 a b 1))))

