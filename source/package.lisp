
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
  (defun judge-metal-available-p ()
    "Judges whether the computer supports Metal Graphics or not depending on machine-version."
    (and
     (string=
      "Apple"
      (subseq
       (machine-version)
       0 5))
     (string=
      "ARM64"
      (machine-type))))

  (unless (find :metal *features*)
    (when (judge-metal-available-p)
      (push :metal *features*)))

  (declaim (inline metal-available-p))
  (defun metal-available-p ()
    "Return T if the current computer provides Metal API otherwise returns nil.
The function is inlined before the compilation and the overhead can be ignored."
    #+metal(progn t)
    #-metal(progn nil))
  )

(mgl-pax:defsection @cl-metal-manual (:title "cl-metal manual")
  "")

(defun load-cl-metal-extension (&key
				  (pathname
				   (asdf:system-relative-pathname
				    "cl-metal"
				    "build/cl-metal.dylib")))
  (handler-case
      (progn
	(load-foreign-library pathname)
	t)
    (error (c)
      (declare (ignore c))
      ;; [TODO]
      ;; Restarting, doing make build, and loading it again
      (warn "FAILED")
      nil)))

