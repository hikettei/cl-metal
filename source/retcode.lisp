
(in-package :cl-metal)

(defun return-with-retcode (retcode)
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
     (error "FailedToLoadLibrary"))
    (-17
     (error "NotReadyToRun"))
    (T
     (error "cl-metal: failed with unknown RetCode ~a" retcode))))

