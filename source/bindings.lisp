
(in-package :cl-metal)

(defcfun "clm_get_n_device" :int)
(defcfun "clm_set_device" :int64
  (device-index :int64))

(defun get-n-device ()
  (clm-get-n-device))

(defun use-device (device-idx)
  (declare (type (unsigned-byte 64) device-idx))
  (let ((retcode (clm-set-device device-idx)))
    retcode))

