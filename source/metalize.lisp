
(in-package :cl-metal)

;; with-metalize: -> S-expression -> Metal Kernel

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cSymbol (symbol)
    (case symbol
      (setf '=)
      (incf '+=)
      (decf '-=)
      (mulcf '*=)
      (divcf '/=)
      (= '==)
      (T symbol)))
  
  (defun metalize-form (form)
    (trivia:ematch form
      ;; arithmetic
      ((list* (or '+ '- '* '/ '> '>= '< '<= '=) _)
       (flet ((helper (prev b)
		(format nil "(~a ~a ~a)"
			(if (stringp prev)
			    prev
			    (metalize-form prev))
			(cSymbol (car form))
			(metalize-form b))))
	 (format nil "~a" (reduce #'helper (cdr form)))))
      ;; A = B, A+=B, A-=B, A*=B, A/=B
      ((list (or 'incf 'decf 'setf 'mulcf 'divcf) form1 form2)
       (format nil
	       "~a ~a ~a"
	       (metalize-form form1)
	       (cSymbol (car form))
	       (metalize-form form2)))
      ;; (if exp then &optional else)
      ((list* 'if exp _)
       (flet ((helper (form)
		(let ((mform (metalize-form form)))
		  (if (eql #\{ (aref mform 0))		      
		      mform
		      (format nil "{~a;}" mform)))))
	 (format nil "if ~a ~a~a"		   
		 (metalize-form exp)
		 (helper (third form))
		 (if (fourth form)
		     (format nil " else ~a" (helper (fourth form)))
		     ""))))
      ;; progn -> { forms }
      ((list* 'progn _)
       (format nil "{~%~a}" (map-split1 #.(format nil ";~%") #'metalize-form (cdr form))))
      ;; aref -> A[idx]
      ((list* 'aref (type symbol) _)
       (format nil "~a[~a]"
	       (metalize-form (second form))
	       (map-split ", " #'metalize-form (cddr form))))
      ;; funcall
      ((list* (type symbol) _)
       (format nil "~a(~a)"
	       (metalize-form (car form))
	       (map-split ", " #'metalize-form (cdr form))))
      ;; number
      ((type number)
       (format nil "~a" form))
      ;; variable
      ((type symbol)
       (cName form))
      ;;string
      ((type string) (format nil "\"~a\"" form))
      (_
       (warn "metalize-form: Can't deal with this form: ~a" form)
       (format nil "~a" form)))))


;; [TODO]
;;  - Testing
;;  - Complete all specs
;;  - docs
;;  - compatible with cl-cuda

(defmacro with-metalize (&rest forms)
  "[Experimental] Translates the given S-expressions into a metal expression"
  (let ((metalized-form
	  (cl-ppcre:regex-replace-all
	   "};"
	   (map-split1 #.(format nil ";~%") #'metalize-form forms)
	   "}")))
    metalized-form))

(print
 (with-metalize 
     (setf (aref a idx) (+ (aref a idx) (sin (aref b idx 0))))
   (+ 1 (* 2 (- 3 1 a)))
   (+ 1 (* 2 (- 3 1 a)))
   (sin 1)
   (= a 1)
   (*= a b)
   "A"
   (> 2 3)
   (progn
     (print 1)
     (print 2))
   (if (> (aref a i) (aref b i))
       (progn
	 (setf (aref a idx) (aref b idx))))))

