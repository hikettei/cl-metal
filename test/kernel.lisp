
(in-package :cl-metal.test)

(defun equivalent-to (scalar)
  #'(lambda (x)
      (= x scalar)))

(define-kernel (metal-sin
		:thread-position-in-grid id
		:style :metal)
    (void ((a* float :in) (b* float :out)))
    "b[id.x] = sin(a[id.x]);")

(define-kernel (metal-cos
		:thread-position-in-grid id)
    (void ((a* float :in) (b* float :out)))
    (setf (aref b id.x) (cos (aref a id.x))))

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


(define-kernel (test-scalar-input :thread-position-in-grid id :style :lisp)
    (void ((x* float :io) (eff float :in)))
    (setf (aref x id.x) (+ (aref x id.x) eff)))

(deftest running-scalar-input
  (ok
   (let ((a (make-array 10 :element-type 'single-float :initial-element 1.0)))
     (test-scalar-input a 2.0)
     (every (equivalent-to 3.0) a))))

(deftest test-dtype
  (macrolet ((def (fname type lisp one)
	       `(testing ,(format nil "Testing w/ ~a" type)
		  (define-kernel (,fname :thread-position-in-grid id)
		      (void ((a* ,type :in) (b* ,type :out)))
		      (incf (aref b id.x) (aref a id.x)))
		  (let* ((a   (make-array 12
			     :element-type ,lisp
			     :initial-element ,one))
			 (b   (make-array 12
			     :element-type ,lisp
			     :initial-element ,one)))
		    (ok (every (equivalent-to 2) (,fname a b)))))))
    (def test-uint8 uint8-t '(unsigned-byte 8) 1)
    (def test-bfloat16 bfloat 'single-float 1.0)))

