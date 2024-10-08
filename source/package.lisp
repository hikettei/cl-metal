
(mgl-pax:define-package :cl-metal
    (:documentation "

`:cl-metal` is added to the `cl:*features*` if Metal API is available in the computer.

")
  (:use :cl :cffi)
  (:import-from :float-features #:with-float-traps-masked)
  (:export
   #:metal-available-p
   #:apple-silicon-p
   #:apple-computer-p)
  (:export
   #:use-device
   #:get-n-device
   #:get-device)
  (:export
   #:make-metal
   #:%make-metal-inlined
   #:funcall-metal
   #:%funcall-metal
   #:%compile-metal-kernel
   #:get-kernel
   #:%load-from-metallib)
  (:export #:with-metalize)
  (:export
   #:define-kernel
   #:make-kernel
   #:define-mfunc
   #:make-mfunc))

(cl:in-package :cl-metal)

(mgl-pax:defsection @cl-metal-manual (:title "\\cl-metal, Common Lisp meets Metal on macOS")
  "

> cl-metal is still in the early development stage. Contributions are welcome!

An attempt to interoperate Common Lisp Array and Metal. This (should) brings the overwhelming performance of Common Lisp made possible by the use of Apple Silicon. As of this writing, this package is divided to three principle components: `:cl-metal` which provides a low-level binding for MetalKit and Common Lisp, `:cl-metal.mathkit` which provides basic mathematical operations, and `:cl-metal.linalg` providing optimized linear algebra ops. Its low-level bindings are strongly inspired from: [metalcompute](https://github.com/baldand/py-metal-compute).

The package would be dedicated to macOS with Apple Silicon. If you're looking for other devices, explore: [petalisp](https://github.com/marcoheisig/Petalisp), [cl-cuda](https://github.com/takagi/cl-cuda).
"
  (cl-metal asdf:system)
  (@cl-metal-device-manual section)
  ;; Other section follows ...
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; [FIXME] Needs to be improved...
  (defun apple-silicon-p ()
    "Returns T if the (machine-type) is ARM64"
    ;; The specification of machine-type depends on each system...
    ;; e.g.: ARM64 in SBCL but arm64 in ecl
    (or
     #+(or sbcl)(string= "ARM64" (machine-type))
     #+(or ecl)(string="arm64"   (machine-type))
     (progn
       (warn "cl-metal couldn't determine the architecture of cpu; proceeds anyway.")
       t)))    
  
  (defun apple-computer-p ()
    "Returns T if the (machine-version) starts with Apple"
    ;; as well as machine-version

    (or
     #+(or sbcl)(string= "Apple" (subseq (machine-version) 0 5))
     #+(or ecl)(null (machine-version))
     (progn
       (warn "cl-metal couldn't determine the version of the machine; proceeds anyway.")
       t)))

  (unless (find :cl-metal *features*)
    (if (apple-computer-p)
	(push :cl-metal *features*)
	(warn "cl-metal: Metal isn't available on the current computer: ~a" (machine-version))))

  (defun metal-available-p ()
    "Return T if the current computer provides Metal API otherwise returns nil.
The function is inlined before the compilation and the overhead can be ignored.

`:cl-metal` is added to the `cl:*features*` if Metal is available on the computer"
    #+cl-metal(progn t)
    #-cl-metal(progn nil))
  
  (when (not (metal-available-p)) (warn "Metal is not supported on your computer")))

(mgl-pax:defsection @cl-metal-device-manual (:title "Devices")
  (apple-silicon-p   function)
  (apple-computer-p  function)
  (metal-available-p function)
  (use-device        function)
  (get-device        function)
  (get-n-device      function))

(cffi:define-foreign-library libclmetal
  (t #.(let ((shared-library-pathname
	       (asdf:system-relative-pathname "cl-metal" "lib/.build/arm64-apple-macosx/release/libCLMetal.dylib")))
         (unless (probe-file shared-library-pathname)
	   (let ((cmd (uiop:strcat "cd "
				   (namestring (asdf:system-relative-pathname "cl-metal" ""))
				   " && make build")))
	     (warn "cl-metal: Running ~a. This may take a few seconds..." cmd)
             (uiop:run-program cmd :error-output t)))
	 shared-library-pathname)))
(load-foreign-library 'libclmetal)
