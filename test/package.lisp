
(cl:in-package :cl-user)

(defpackage :cl-metal.test
  (:use :cl :rove :cl-metal))

(in-package :cl-metal.test)

(deftest load-mps-device
  (ok (= 0 (clm-set-device 0))))

