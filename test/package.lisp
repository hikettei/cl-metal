
(cl:in-package :cl-user)

(defpackage :cl-metal.test
  (:use :cl :rove :cl-metal))

(in-package :cl-metal.test)

(deftest get-the-number-of-devices
  (ok (print (get-n-device))))

(deftest initialize-mps-device
  (ok (= 0 (use-device 0))))

