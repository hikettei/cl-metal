
(in-package :cl-metal.test)

(deftest getting-the-number-of-devices
  (ok
   (progn
     (format t "The number of gpus: ~a" (get-n-device))
     t))
  (ok
   (progn
     (dotimes (i (get-n-device))
       (format t "~a" (get-device i)))
     t)))

(deftest initialize-mps-device
  (ok (eql t (use-device 0))))


