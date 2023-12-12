
(cl:defpackage :cl-metal.examples.benchmark
  (:use
   :cl
   :cl-metal)
  (:export

   ))

(cl:in-package :cl-metal.examples.benchmark)

(use-device 0)

;; (ql:quickload :cl-waffe2)
;; (ql:quickload :numcl)

(defmacro measure-time (&body body &aux (id (gensym)))
  `(let ((,id (get-internal-run-time)))
     (progn ,@body)
     (/
      (- (get-internal-run-time) ,id)
      internal-time-units-per-second)))

(defun benchmark (name
		  subject-functions
		  target-names
		  generators
		  &key
		    (scales `(2 4 8 16 32 64 128 256 512 1024 2048))
		    (N 100)
		    (stream t))
  (flet ((benchmark-helper (subject-f gen-f scale)
	   (declare (type function subject-f))
	   (let ((arr (funcall gen-f scale)))
	     (measure-time
	      (dotimes (i N)
		(funcall subject-f arr))))))
    (format stream "~%Benchmarking on: ~a (N=~a):~%" name N)
    (loop for scale in scales
	  do (let ((results
		     (map 'list
			  #'(lambda (f name gen-f)
			      `(,name ,(float (benchmark-helper f gen-f scale))))
			  subject-functions target-names generators)))
	       (format
		stream
		"scale=~a | ~a~%"
		scale
		(with-output-to-string (out)
		  (loop for (device . result) in results
			do (format out "~a -> ~a | " device result))))))))

(defun make-wf2-tester (f)
  (let ((model
	  (wf/t:build
	   (funcall f (wf/t:make-input `(A B) :in))
	   :inputs `(:in)
	   :compile-mode :fastest)))
    #'(lambda (x)
        (wf/nodes:forward model x))))

(defun fused-relu-cpu (x &aux (n (array-total-size x)))
  (declare (optimize (speed 3))
	   (type (simple-array single-float (*)) x))
  (dotimes (id n)
    (if (> (aref x id) 0.0)
	(setf (aref x id) (aref x id))
	(setf (aref x id) 0.0))))

(define-kernel (fused-relu-metal :thread-position-in-grid id :mode :lisp)
    (void ((x* float :io)))
    (if (> (aref x id) 0.0)
	(setf (aref x id) (aref x id))
	(setf (aref x id) 0.0)))

(defun gelu-cpu (x &aux (n (array-total-size x)))
  (declare (optimize (speed 3))
	   (type (simple-array single-float (*)) x))
  (dotimes (id n)
    (let ((xi (aref x id)))
      (setf
       (aref x id)
       (* 0.5 xi
	  (+
	   1
	   (tanh
	    (*
	     #.(coerce (sqrt (/ 2.0 pi)) 'single-float)
	     (+ xi
		(* 0.044715 (expt xi 3)))))))))))

(define-kernel (gelu-metal :thread-position-in-grid id :mode :lisp)
    (void ((x* float :io)))
    (let ((xi (aref x id)))
      (setf
       (aref x id)
       (* 0.5 xi
	  (+
	   1
	   (tanh
	    (*
	     #.(coerce (sqrt (/ 2.0 pi)) 'single-float)
	     (+ xi
		(* 0.044715 (expt xi 3.0))))))))))

;; cl-waffe2 is (still) not yet ready for providing hardwafe-level apple silicon support! T_T.
(defun !gelu-revisit (x)
  (wf:!* 0.5 x
	 (wf:!+ 1
		(wf:!tanh
		 (wf:!* #.(coerce (sqrt (/ 2.0 pi)) 'single-float)
			(wf:!+ x
			       (wf:!* 0.044715 x x x)))))))

(define-kernel (sigmoid-metal :thread-position-in-grid id :mode :lisp)
    (void ((x* float :io)))
    (setf (aref x id) (/ 1 (+ 1 (exp (- (aref x id)))))))

(defun sigmoid-cpu (x &aux (n (array-total-size x)))
  (declare (optimize (speed 3))
	   (type (simple-array single-float (*)) x))
  (dotimes (id n)
    (setf (aref x id) (/ 1 (+ 1 (exp (- (aref x id))))))))

(defun uniform-numcl (scale)
  (multiple-value-bind (x storage) (numcl:uniform -1.0 1.0 `(,scale ,scale))
    (declare (ignore x))
    storage))

(defun uniform-wf2 (scale)
  (wf/d:uniform-random `(,scale ,scale) -1.0 1.0))

(benchmark
 "sin"
 (list #'sin-cpu #'sin-metal (make-wf2-tester #'wf:!sin))
 (list "SBCL(CPU)" "Metal" "cl-waffe2 (CPU+VM)")
 (list #'uniform-numcl #'uniform-numcl #'uniform-wf2))

(benchmark
 "ReLU"
 (list #'fused-relu-cpu #'fused-relu-metal (make-wf2-tester #'wf/nn:!relu))
 (list "SBCL(CPU)" "Metal" "cl-waffe2 (CPU+VM)")
 (list #'uniform-numcl #'uniform-numcl #'uniform-wf2))

(benchmark
 "Sigmoid"
 (list #'sigmoid-cpu #'sigmoid-metal (make-wf2-tester #'wf/nn:!sigmoid))
 (list "SBCL(CPU)" "Metal" "cl-waffe2 (CPU+VM)")
 (list #'uniform-numcl #'uniform-numcl #'uniform-wf2))

(benchmark
 "GeLU"
 (list #'gelu-cpu #'gelu-metal (make-wf2-tester #'!gelu-revisit))
 (list "SBCL(CPU)" "Metal" "cl-waffe2 (CPU+VM)")
 (list #'uniform-numcl #'uniform-numcl #'uniform-wf2))


