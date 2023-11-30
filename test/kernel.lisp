
(in-package :cl-metal.test)

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
       ;; With fastMath enabled:
       (every #'(lambda (x) (= x 0.841471)) b))))
  (ok
   (progn
     (let* ((a   (make-array 12
			     :element-type 'single-float
			     :initial-element 1.0))
	    (b   (make-array 12
			     :element-type 'single-float
			     :initial-element 0.0)))
       (time (metal-cos a b))
       ;; With fastMath enabled:
       (every #'(lambda (x) (= x 0.5403023)) b)))))


