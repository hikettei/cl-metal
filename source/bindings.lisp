
(in-package :cl-metal)

(defmacro with-swift-float-mode (&body body)
  "(I don't completely understand why but) on SBCL, regardless of what functions to use,
calling a function declared in `cl-metal.swift` with MetalKIT API, always produces a floating-overflow.
Anyway mask traps on SBCL.
When calling foreign function using such apis, wrap the code with this macro.

(Could be) somethings to do with:
https://bugs.launchpad.net/sbcl/+bug/1519630"
  #+sbcl`(with-float-traps-masked t ,@body)
  #+(not sbcl)`(progn ,@body))

;; ~~~~~~~~~~~~~~~~~~~~~~~
;;  APIs for Environments
;; ~~~~~~~~~~~~~~~~~~~~~~~

(defcfun "clm_get_n_device" :int)
(defcfun "clm_set_device" :int
  (device-index :int))

(defun get-n-device ()
  "Counts the number of gpus[fixnum] installed on the device"
  (with-swift-float-mode
    (clm-get-n-device)))

(defun use-device (device-idx)
  "Explicts the gpu to use (get-n-device to know how many devices are installed)"
  (declare (type (unsigned-byte 64) device-idx))
  (with-swift-float-mode
    (return-with-retcode
     (clm-set-device device-idx))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  dynamically loading .metalib 
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defcfun "clm_compile_kernel" :int
  (metal :string)
  (fname :string))

(defun %compile-metal-kernel (source function-name)
  "(Dynamically) compiles the given source[string] and loads it as a MTLLibrary."
  (with-swift-float-mode
    (return-with-retcode
     (clm-compile-kernel source function-name))))

(defcfun "clm_load_from_metallib" :int
  (pathname :string)
  (fname    :string))

(defun %load-from-metallib (pathname fname)
  ""
  (with-swift-float-mode
    (return-with-retcode
     (clm-load-from-metallib pathname fname))))

(defcfun "clm_alloc" :int
  (nth    :int)     ;; nth buffer?
  (icount :int)     ;; a size of tensor
  (input  :pointer) ;; cffi:foreign-pointer
  (idtype :int))    ;; a fixnum indicating the dtype

(defcfun "clm_run" :int
  (global-width :int)
  (global-height :int)
  (global-depth :int)
  (local-width :int)
  (local-height :int)
  (local-depth :int))

(defcfun "clm_retrieve" :int
  (nth-buff :int)
  (obuff    :pointer))

(defcfun "clm_get_compile_error" :string)
(defcfun "clm_get_device" :string  (device-idx :int))

(defun get-device (idx)
  "Returns a string indicating the status of gpu[idx]"
  (with-swift-float-mode
    (clm-get-device idx)))

(defcfun "clm_reset_buffer" :int)
