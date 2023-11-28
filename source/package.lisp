
(mgl-pax:define-package :cl-metal
    (:documentation "

`:cl-metal` is added to the `cl:*features*` if Metal API is available in the computer.

")
  (:use :cl :cffi)
  (:export
   #:use-device
   #:get-n-device
   )

  (:export
   #:%compile-metal-kernel)
  
  (:export
   #:metal-available-p
   #:apple-silicon-p
   #:apple-computer-p
   ))

(cl:in-package :cl-metal)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; [FIXME]
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
     #+(or sbcl)(string= "Apple" (machine-version) 0 5)
     #+(or ecl)(null (machine-version))
     (progn
       (warn "cl-metal couldn't determine the version of the machine; proceeds anyway.")
       t)))

  (unless (find :cl-metal *features*)
    (if (apple-computer-p)
	(push :cl-metal *features*)
	(warn "cl-metal: Metal isn't available on the current computer: ~a" (machine-version)))))

(defun metal-available-p ()
  "Return T if the current computer provides Metal API otherwise returns nil.
The function is inlined before the compilation and the overhead can be ignored."
  #+cl-metal(progn t)
  #-cl-metal(progn nil))

(mgl-pax:defsection @cl-metal-manual (:title "cl-metal manual")
  ""
  (apple-silicon-p   function)
  (apple-computer-p  function)
  (metal-available-p function)
  )

(defun load-cl-metal-library (&key
				(lib-path
				 (asdf:system-relative-pathname
				  "cl-metal"
				  "lib/.build/arm64-apple-macosx/release/libCLMetal.dylib")))
  (when (not (metal-available-p))
    (warn "cl-metal fails to load libCLMetal.dylib since the computer doesn't support Metal.")
    (return-from load-cl-metal-library nil))
  
  (restart-case
      (progn
	;;(reload-foreign-libraries)
	(load-foreign-library lib-path)
	t)
    (make-build-and-try-again ()
      :report "Ensure compiling the swift library and try loading it again"
      (let* ((cmd
	       (list
		"cd"
		(namestring
		 (asdf:system-relative-pathname "cl-metal" "./"))
		"&&"
		"make build"))
	     (process-info
	       (uiop:launch-program
		cmd
		:input :stream
		:error-output :stream))
	     (error-output (uiop:process-info-error-output process-info)))
        (unless (zerop (uiop:wait-process process-info))
	  (error "Failed to compile the swift library due to:~%~a"
		 (alexandria:read-stream-content-into-string error-output)))
	(warn "cl-metal tries loading ~a again ..." lib-path)
	(load-cl-metal-library)))))

;; Loading libCLMetal.dylib
(load-cl-metal-library)


