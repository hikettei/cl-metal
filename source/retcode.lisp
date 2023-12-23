
(in-package :cl-metal)

(defun return-with-retcode (retcode
			    &key
			      (load-path ""))
  (declare (type fixnum retcode))
  ;; 0 -> Succeed, returning T
  ;; otherwise -> something went wrong; reporing the detail
  (case retcode
    (0
     T)
    (-1
     (error "CannotCreateDevice"))
    (-2
     (error "CannotCreateCommandQueue"))
    (-3
     (error "NotReadyToCompile
cl-metal don't know which gpu to use:
    - (use-device 0) and explict the gpu to use"))
    (-4
     (error "FailedToCompile
cl-metal failed to compile the given source:
    ~a"
	    (clm-get-compile-error)))
    (-5
     (error "FailedToFindFunction
The name of function described in the source must correspond with function-name"))
    (-6
     (error "NotReadyToCompute"))
    (-7
     (error "FailedToMakeInputBuffer"))
    (-8
     (error "NotReadyToRun"))
    (-9
     (error "CannotCreateCommandBuffer"))
    (-10
     (error "CannotCreateCommandEncoder"))
    (-11
     (error "CannotCreatePipelineState"))
    (-12
     (error "IncorrectOutputCount"))
    (-13
     (error "NotReadyToRetrieve"))
    (-14
     (error "UnsupportedInputFormat"))
    (-15
     (error "UnsupportedOutputFormat"))
    (-16
     (error "FailedToLoadLibrary: ~a" load-path))
    (-17
     (error "NotReadyToRun"))
    (T
     (error "cl-metal: failed with unknown RetCode ~a" retcode))))	   

(defun type2iformat (test-element)
  (if (typep test-element 'array)
      (or
       (alexandria:switch ((array-element-type test-element) :test #'equal)
	 ('double-float 10)
	 ('single-float 9)
	 ;; half-float 8
	 ('(signed-byte   64) 7)
	 ('(unsigned-byte 64) 6)
	 ('(signed-byte   32) 5)
	 ('(unsigned-byte 32) 4)
	 ('(signed-byte   16) 3)
	 ('(unsigned-byte 16) 2)
	 ('(signed-byte   8)  1)
	 ('(unsigned-byte 8)  0))
       (error "couldn't identity the type of this array: ~a" test-element))
      (etypecase test-element
	(double-float 10)
	(single-float 9)
	;; half-float 8
	((signed-byte   64) 7)
	((unsigned-byte 64) 6)
	((signed-byte   32) 5)
	((unsigned-byte 32) 4)
	((signed-byte   16) 3)
	((unsigned-byte 16) 2)
	((signed-byte   8)  1)
	((unsigned-byte 8)  0))))

