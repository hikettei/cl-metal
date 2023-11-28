
(in-package :cl-metal)

;; Switching devices
(defcfun "clm_set_device" :int
  (device-idx :int))

