
(in-package :cl-metal.test)

(define-kernel (metal-sin
		:thread-position-in-grid id
		:template t)
    (void ((a* T :in) (b* T :out)))
    "b[id] = sin(a[id]);")

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
   (let* ((a   (make-array 10
			   :element-type 'single-float
			   :initial-element 1.0))
	  (b   (make-array 10
			   :element-type 'single-float
			   :initial-element 0.0)))
     (metal-sin a b)
     (every #'(lambda (x) (= x 0.841471)) b))))
