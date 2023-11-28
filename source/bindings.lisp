
(in-package :cl-metal)

;; Switching devices
(defcfun "clm_sw_init" :int
  (device-idx :int))

