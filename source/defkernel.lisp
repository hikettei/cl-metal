
(in-package :cl-metal)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cName (base-char)
    (cl-ppcre:regex-replace-all
     "-"
     (format nil "~(~a~)" base-char)
     "_"))

  (defun true-type-name (string)
    "Trimming * from the given base string"
    (cl-ppcre:regex-replace-all
     "\\*"
     (cName string)
     ""))

  (defun pointer-p (string)
    "Return T if base-string contains *"
    (if (find #\* (coerce (cName string) 'list)) t nil))
  
  (defstruct (MArg
	      (:constructor
		  make-marg (var-name
			     type
			     state
			     nBuffer
			     &key
			       (pointer-p   nil)
			       (description nil))))
    (varName (cName var-name)      :type string)
    (dType   type                  :type string)
    (state   state                 :type (or null (and keyword (member :in :out :io))))
    (nBuffer nBuffer               :type (or null fixnum))
    (pointer-p pointer-p           :type boolean)
    (description description       :type (or null string)))

  (trivia:defpattern IOkeyword
      ()
      `(and (type keyword)
	    (or
	     (eql :in)
	     (eql :out)
	     (eql :io))))
  
  (defun parse-marg (form &optional (buffer-n nil) (template nil))
    "the form is given as one of:
- (varName dType io)
- (varName* dtype io)
- (bind    thread-position-in-grid)"
    (trivia:ematch form
      ((list (type symbol) (type symbol) (IOKeyword))
       (make-marg
	(true-type-name (first form))
	(if (eql template (second form))
	    (string-upcase (cName (second form)))
	    (cName (second form)))
	(third form)
	(or buffer-n
	    (error "parse-margs: the position of buffer needs to be explicted for ~a" form))
	:pointer-p (pointer-p (car form))))
      ((list (type symbol) (type symbol))
       (make-marg (first form)
		  "uint" nil nil
		  :description
		  (alexandria:switch ((cName (second form)) :test #'equal)
		    ("thread_position_in_grid"
		     "thread_position_in_grid")
		    (T
		     (warn "Unrecognised keyword: ~a" (second form))
		     (cName (second form))))))))

  (defun marg-as-string (marg)
    (declare (type Marg marg))
    (trivia:ematch marg
      ((marg :state (IOKeyword) :nBuffer (type fixnum) :description (eql nil))
       (format nil
	       "~adevice ~a ~a~a [[ buffer(~a) ]]"
	       (ecase (marg-state marg)
		 (:in  "const ")
		 (:out "")
		 (:io  ""))
	       (marg-dtype marg)
	       (if (marg-pointer-p marg) "*" "&")
	       (marg-varName marg)
	       (marg-nBuffer marg)))
      ((marg :description (type string))
       (format nil
	       "~a ~a [[ ~a ]]"
	       (marg-dtype marg)
	       (marg-varName marg)
	       (marg-description marg)))))

  (defun %define-kernel-helper (function-name
				&key
				  (utils "")
				  (template)
				  (includes)
				  (using-namespaces)
				  (return-type)
				  (args)
				  (metalized-form))
    (format nil "~a~%~a~%~a~%~a~%kernel ~a ~a(~a) {~%~a~%}"
	    (with-output-to-string (out)
	      (dolist (include includes)
		(format out "#include ~a~%" include)))
	    (with-output-to-string (out)
	      (dolist (namespace using-namespaces)
		(format out "using namespace ~a;~%" namespace)))
	    utils
	    (if template
		(format nil "template <typename ~a>" (string-upcase (cName template)))
		"")
	    (cName return-type)
	    (cName function-name)
	    (map-split ", " #'marg-as-string args)
	    metalized-form))

  (defun eval-metal-form (metal-forms metalize-p)
    (let ((results
	    (loop for form in metal-forms
		  for evaluated-form = (eval (if metalize-p
						 `(with-metalize ,form)
						 form))
		  if (= (length metal-forms) 1)
		    collect
		  evaluated-form
		  else
		    append
		    (list
		     evaluated-form
		     #.(format nil ";~%")))))
      (apply #'concatenate 'string results))))

(defmacro define-kernel
    ((function-name
      &key
	(thread-position-in-grid 'id)
	(utils "")
	(template nil)
	(includes         `("<metal_stdlib>"))
	(using-namespaces `("metal"))
	(mode :metal))
     (return-type (&rest args))
     &rest metalized-form)
  "Defines an inlined metal kernel from a given string:

Args = (varName Type IO) | (varName thread-position-in-grid)

Example:
```lisp
(define-kernel (metal-sin
		:thread-position-in-grid id
		:template t)
    (void ((a* t :in) (b* t :out)))
    \"b[id] = sin(a[id])\")
```
If metalized-form is multiple, each result is concatenated with merging newline+;
(TODO Docs)
"
  (declare (type (member :metal :lisp) mode))
  (let* ((margs (loop for nth upfrom 0
		      for arg in args
		      collect
		      (parse-marg arg nth template)))
	 (metal-form
	   (%define-kernel-helper
	    function-name
	    :utils utils
	    :template template
	    :includes includes
	    :using-namespaces using-namespaces
	    :return-type return-type
	    :args `(,@margs ,(parse-marg `(,thread-position-in-grid thread-position-in-grid)))
	    :metalized-form (eval-metal-form metalized-form (eql mode :lisp)))))
    `(defun ,function-name (,@(map 'list #'car args))
       (funcall-metal
	,(%make-metal-inlined
	  (cName function-name)
	  args
	  metal-form)
	,@(map 'list #'car args)))))

(defmacro kernel-lambda
    ((&key
	(thread-position-in-grid 'id)
	(utils "")
	(template nil)
	(includes        `("<metal_stdlib>"))
	(using-namespaces `("metal"))
	(mode :metal))
     (return-type (&rest args))
     &rest metalized-form)
  "docs"
  (declare (type (member :metal :lisp) mode))
  (let* ((margs (loop for nth upfrom 0
		      for arg in args
		      collect
		      (parse-marg arg nth template)))
	 (fname (cName (symbol-name (gensym "LAMBDA"))))
	 (metal-form
	   (%define-kernel-helper
	    fname
	    :utils utils
	    :template template
	    :includes includes
	    :using-namespaces using-namespaces
	    :return-type return-type
	    :args `(,@margs ,(parse-marg `(,thread-position-in-grid thread-position-in-grid)))
	    :metalized-form (eval-metal-form metalized-form (eql mode :lisp)))))
    (%make-metal-inlined
     fname
     args
     metal-form)))

