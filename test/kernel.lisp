
(in-package :cl-metal.test)

(defun metal-sin ()
  ;; The metal code below is inlined:
  (make-metal
   "fsin"
   `((invector T :in) (outVector T :out))
   "
#include <metal_stdlib>

using namespace metal;

kernel void fsin(const device float *inVector [[ buffer(0) ]],
		device float *outVector [[ buffer(1) ]],
		uint id [[ thread_position_in_grid ]]) {
  outVector[id] = sin(inVector[id]);
}"))

(deftest running-simple-kernel
  (ok
   (eql
    t
    (progn
      (use-device 0)
      (%compile-metal-kernel
       "
#include <metal_stdlib>

using namespace metal;

kernel void fsin(const device float *inVector [[ buffer(0) ]],
		device float *outVector [[ buffer(1) ]],
		uint id [[ thread_position_in_grid ]]) {
  outVector[id] = sin(inVector[id]);
}
"
       "fsin"))))
  (ok
   (let* ((mtl (metal-sin))
	  (a   (make-array 10
			   :element-type 'single-float
			   :initial-element 1.0))
	  (b   (make-array 10
			   :element-type 'single-float
			   :initial-element 0.0)))
     (funcall-metal mtl b a)
     (every #'(lambda (x) (= x 0.841471)) b))))

#|
(define-kernel (metal-sin
		:thread-position-in-grid id
		:template t)
    (void ((a* T :in) (b* T :out)))
    "b[id] = sin(a[id])"
    "b[id] = sin(a[id])")

(define-kernel (metal-cos
		:thread-position-in-grid id
		:template t)
    (void ((x* T :in) (out* T :out)))
    (with-metalize
	(if (> 1 0)
	    (setf (aref out id) (aref x id)))))

(kernel-lambda () (void ((a* float :in) (b* float :out )))
	       "b[id] = a[id];")
|#

