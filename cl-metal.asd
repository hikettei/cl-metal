
(asdf:defsystem :cl-metal
  :description "A Common Lisp library for simple-array and metal kernels interoperation on macOS"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on (#:cffi
	       #:alexandria
	       #:mgl-pax
	       #:rove
	       #:uiop
	       #:float-features)
  :components
  ((:file "source/package")
   (:file "source/bindings")
   (:file "source/retcode")
   (:file "source/metallib")

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
   (:file "test/kernel"))
  :perform
  (asdf:test-op (o s)
		(uiop:symbol-call (find-package :rove) :run :cl-metal/test)))

