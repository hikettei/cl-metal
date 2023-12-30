
(in-package :cl-metal)

(defun range (upfrom below &optional (by 1))
  (loop for i upfrom upfrom below below by by collect i))

(define-compiler-macro range (upfrom below &optional (by 1))
  `(loop for i upfrom ,upfrom below ,below by ,by collect i))

(defun map-split (split-with function &rest sequences)
  "A helper function merging A B C -> A, B, C"
  (apply
   #'concatenate
   'string
   (butlast
    (alexandria:flatten
     (apply
      #'map
      'list
      #'(lambda (&rest args)
	  (list
	   (apply function args)
	   split-with))
      sequences)))))

(defun map-split1 (split-with function &rest sequences)
  "A helper function merging A B C -> A, B, C,
this is the equivalent to map-split ignoring , is added to the last sequence"
  (apply
   #'concatenate
   'string
   (alexandria:flatten
    (apply
     #'map
     'list
     #'(lambda (&rest args)
	 (list
	  (apply function args)
	  split-with))
     sequences))))

