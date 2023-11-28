
(in-package :cl-metal.test)

(deftest compiling-simple-metal-kernel
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
       "fsin")))))

