
(in-package :cl-metal.test)

(defun equivalent-to (scalar)
  #'(lambda (x)
      (= x scalar)))

(define-kernel (metal-sin
		:thread-position-in-grid id)
    (void ((a* float :in) (b* float :out)))
    "b[id] = sin(a[id]);")

(define-kernel (metal-cos
		:thread-position-in-grid id)
    (void ((a* float :in) (b* float :out)))
    (with-metalize
	(setf (aref b id) (cos (aref a id)))))

(deftest running-simple-kernel
  (ok
   (progn
     (let* ((a   (make-array 10
			     :element-type 'single-float
			     :initial-element 1.0))
	    (b   (make-array 10
			     :element-type 'single-float
			     :initial-element 0.0)))
       (time (metal-sin a b))
       ;; With fastMath enabled, the result is approximated
       (every (equivalent-to 0.841471) b))))
  (ok
   (progn
     (let* ((a   (make-array 12
			     :element-type 'single-float
			     :initial-element 1.0))
	    (b   (make-array 12
			     :element-type 'single-float
			     :initial-element 0.0)))
       (time (metal-cos a b))
       ;; With fastMath enabled, the result is approximated
       (every (equivalent-to 0.5403023) b)))))


(define-kernel (test-scalar-input :thread-position-in-grid id :mode :lisp)
    (void ((x* float :io) (eff float :in)))
    (setf (aref x id) (+ (aref x id) eff)))

(deftest running-scalar-input
  (ok
   (let ((a (make-array 10 :element-type 'single-float :initial-element 1.0)))
     (test-scalar-input a 2.0)
     (every (equivalent-to 3.0) a))))


