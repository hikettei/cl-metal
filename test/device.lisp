
(in-package :cl-metal.test)

(deftest getting-the-number-of-devices
  (ok
   (progn
    (format t "The number of gpus: ~a" (get-n-device))
    t)))

(deftest initialize-mps-device
  (ok (eql t (use-device 0))))

