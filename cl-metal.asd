
(asdf:defsystem :cl-metal
  :description "A Common Lisp library for simple-array and metal kernels interoperation on macOS"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on (#:cffi
	       #:alexandria
	       #:mgl-pax
	       #:rove)
  :components
  ((:file "source/package")

   ))

