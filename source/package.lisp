
(mgl-pax:define-package :cl-metal
    (:documentation "

`:metal` is added to the `cl:*features*` if Metal API is available in the computer.

")
  (:use :cl :cffi)
  (:export
   #:metal-available-p
   ))

(cl:in-package :cl-metal)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun apple-silicon-p ()
    (string=
     "ARM64"
     (machine-type)))
  
  (defun apple-computer-p ()
    (string=
     "Apple"
     (subseq
      (machine-version)
      0 5)))

  (unless (find :metal *features*)
    (if (apple-computer-p)
	(push :metal *features*)
	(warn "cl-metal: Metal isn't available on the current computer: ~a" (machine-version))))

  (declaim (inline metal-available-p))
  (defun metal-available-p ()
    "Return T if the current computer provides Metal API otherwise returns nil.
The function is inlined before the compilation and the overhead can be ignored."
    #+metal(progn t)
    #-metal(progn nil)))

(mgl-pax:defsection @cl-metal-manual (:title "cl-metal manual")
  "")

(defun load-cl-metal-library (&key
				(pathname
				 (asdf:system-relative-pathname
				  "cl-metal"
				  "build/cl-metal.a")))
  (when (not (apple-computer-p))
    (warn "cl-metal fails to load cl-metal.dylib since the computer doesn't support Metal.")
    (return-from load-cl-metal-library nil))
  
  (restart-case
      (progn
	(load-foreign-library pathname)
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
	(warn "cl-metal tries loading ~a again..." pathname)
	(load-cl-metal-library)))))

(load-cl-metal-library)

