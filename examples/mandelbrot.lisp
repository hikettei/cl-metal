
(cl:defpackage :cl-metal.examples.mandelbrot
  (:use :cl :cl-metal)
  (:export

   ))

(cl:in-package :cl-metal.examples.mandelbrot)

(use-device 0)

;; WIP
(define-kernel (mandelbrot :thread-position-in-grid id :mode :metal)
    (void ((uniform* float :in) (out* uchar4 :out)))
    "float width = uniform[0]"
    "float height = uniform[0]"
    "float2 c = 2.5 * (float2((id%int(width))/width - 0.5, 0.5 - (id/int(width))/height))"
    "c.x -= 0.7"
    "float2 z = c"
    "float done = 0.0, steps = 1.0, az = 0.0"
    )


(define-kernel (fused-relu :thread-position-in-grid id :mode :lisp)
    (void ((x* float :io)))
    (if (> (aref x id) 0.0)
	(setf (aref x id) 1.0)
	(setf (aref x id) 0.0)))


(defun test ()
  (multiple-value-bind (x storage) (numcl:uniform -1.0 1.0 `(10 10))
    ;;(print x)
    (time (fused-relu storage))
    nil))

(print (test))