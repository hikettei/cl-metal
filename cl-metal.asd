
(asdf:defsystem :cl-metal
  :description "A Common Lisp library for simple-array and metal kernels interoperation on macOS"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on (#:cffi
	       #:alexandria
	       #:mgl-pax
	       #:rove
	       #:cl-ppcre
	       #:uiop
	       #:float-features
	       #:trivia #:trivia.ppcre)
  :components
  ((:file "source/package")
   (:file "source/utils")
   (:file "source/bindings")
   (:file "source/metallib")
   (:file "source/metalize" :depends-on ("source/defkernel"))
   (:file "source/defkernel")
   (:file "source/retcode")
   )

  :in-order-to
  ((test-op (asdf:test-op cl-metal/test))))    

(asdf:defsystem :cl-metal/test
  :description ""
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on (#:cl-metal #:rove)
  :components
  ((:file "test/package")
   (:file "test/device")
   (:file "test/kernel")
   (:file "test/metalize"))
  :perform
  (asdf:test-op (o s)
		(uiop:symbol-call (find-package :rove) :run :cl-metal/test)))

(asdf:defsystem :cl-metal/examples
  :description "Includes a demonstration of cl-metal"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on  (#:cl-metal #:numcl)
  :components
  ((:file "examples/mandelbrot")))

