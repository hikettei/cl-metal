
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
  
  (defun metalize-form (form
			&aux
			  (form
			   ;; Interning all symbols
			   (let ((*package* (find-package :cl-metal)))
			     (read-from-string
			      (format nil "~a" form)))))
    (trivia:ematch form
      ;; (- a) -> -a
      ((list* '- form)
       (format nil "-~a" (metalize-form (car form))))
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
      ;; (expt a b) -> a^b
      ((list 'expt a b)
       (format nil "pow(~a, ~a)"
	       (metalize-form a)
	       (metalize-form b)))
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
      ((list* (or 'let 'let*) _)
       (let ((bindings (second form))
	     (body     (cddr   form)))
	 (with-output-to-string (tmp)
	   (loop for bind in bindings do
	     (format tmp "auto ~a = ~a;~%"
		     (metalize-form (car bind))
		     (metalize-form (second bind))))
	   (format tmp "~a" (metalize-form `(progn ,@body))))))
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
       (warn "metalize-form: Cannot deal with this form: ~a" form)
       (format nil "~a" form)))))


;; [TODO]
;;  - Testing
;;  - Complete all specs
;;  - docstrings
;;  - prepare compatible specifications with cl-cuda

(defmacro with-metalize (&rest forms)
  "[Experimental] Translates the given S-expressions into a metal expression"
  (let ((metalized-form
	  (cl-ppcre:regex-replace-all
	   "};"
	   (map-split1 #.(format nil ";~%") #'metalize-form forms)
	   "}")))
    metalized-form))

