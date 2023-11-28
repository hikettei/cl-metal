
(in-package :cl-metal)

(defmacro with-swift-float-mode (&body body)
  "(I don't completely understand why but) on SBCL, regardless of what functions to use,
calling a function declared in `cl-metal.swift` with MetalKIT API, always produces a floating-overflow.
Anyway mask traps on SBCL.
When calling foreign function using such apis, wrap the code with this macro."
  #+sbcl`(with-float-traps-masked t ,@body)
  #+(not sbcl)`(progn ,@body))

;; ~~~~~~~~~~~~~~~~~~~~~~~
;;  APIs for Environments
;; ~~~~~~~~~~~~~~~~~~~~~~~

(defcfun "clm_get_n_device" :int)
(defcfun "clm_set_device" :int
  (device-index :int))

;; [TODO] Docs, pax
(defun get-n-device ()
  "Counts the number of gpus[fixnum] installed on the device"
  (with-swift-float-mode
    (clm-get-n-device)))

(defun use-device (device-idx)
  "Explicts the gpu to use (get-n-device to know how many devices are installed)"
  (declare (type (unsigned-byte 64) device-idx))
  (with-swift-float-mode
    (let ((retcode (clm-set-device device-idx)))
      (return-with-retcode retcode))))

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
  (icount :int)
  (input  :pointer)
  (idtype :int)
  (ocount :int)
  (oformat :int))

(defcfun "clm_run" :int
  (kcount :int))

(defcfun "clm_retrieve" :int
  (ocount :int)
  (obuff  :pointer))

(defcfun "clm_get_compile_error" :string)

