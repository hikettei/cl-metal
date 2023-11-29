
(in-package :cl-metal)

(defun range (upfrom below &optional (by 1))
  (loop for i upfrom upfrom below below by by collect i))

(define-compiler-macro range (upfrom below &optional (by 1))
  `(loop for i upfrom ,upfrom below ,below by ,by collect i))

