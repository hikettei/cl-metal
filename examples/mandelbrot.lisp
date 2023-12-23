
(cl:defpackage :cl-metal.examples.mandelbrot
  (:use :cl :cl-metal)
  (:export

   ))

(cl:in-package :cl-metal.examples.mandelbrot)

(use-device 0)

(defun mandelbrot-cpu (xs)
  (declare (type (simple-array (unsigned-byte 8) (*)) xs)
	   (optimize (speed 3)))
  ;; optmize
  (dotimes (i #.(* 2048 2048))
    (labels ((aux (x y a b m)
	       (declare (type single-float x y a b)
			(type (unsigned-byte 8) m))
               (if (< m 100)
                   (let ((x1 (- (* x x) (* y y) a))
			 (y1 (- (* 2.0 x y) b)))
                     (if (> (+ (* x1 x1) (* y1 y1)) 4.0)
			 m
			 (aux x1 y1 a b (+ m 1))))
                   0)))
      (let ((a (/ (- (mod i 2048) 512) 1024.0))
            (b (/ (- (/ i 2048.0) 1024) 1024.0)))
	(setf (aref xs i) (aux 0.0 0.0 a b 1))))))

(define-mfunc (aux :stream t)
    (uint8-t ((x float :in) (y float :in) (a float :in) (b float :in) (m uint8-t :in)))
    (if (< m 100)
	(let ((x1 (- (* x x) (* y y) a))
	      (y1 (- (* 2.0 x y)     b)))
	  (if (> (+ (* x1 x1) (* y1 y1)) 4.0)
	      m
	      (aux x1 y1 a b (+ m 1))))
	0))

(define-kernel (mandelbrot
		:thread-position-in-grid id
		:using (aux)
		:stream t)
    (void ((x* uint8-t :out)))
    (let ((a (/ (- (mod id 2048) 512.0) 1024.0))
	  (b (/ (- (/   id 2048) 1024.0) 1024.0)))
      (setf (aref x id) (aux 0.0 0.0 a b 1))))

(defun draw-mandelbrot (pathname xs)
  (with-open-file (out pathname :direction :output
                                :if-does-not-exist :create
                                :if-exists :supersede)
    (write-line "P2" out)
    (write-line "2048 2048" out)
    (write-line "255" out)
    (dotimes (i (* 2048 2048))
      (princ (min 255 (* 8 (aref xs i))) out)
      (terpri out))))

(defun main ()
  (let ((xs (make-array (* 2048 2048)
			:initial-element 0
			:element-type '(unsigned-byte 8))))
    (time (mandelbrot-cpu xs))
    (time (mandelbrot xs))
    (draw-mandelbrot #P"./mandelbrot.pgm" xs)))

