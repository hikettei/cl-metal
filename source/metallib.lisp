
(in-package :cl-metal)

;; Dynamically Generated .metalib
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct Metallib
    "Stores the cached function of metal"
    (load-state :not-yet-compiled :type (member :not-yet-compiled :compiled-metallib))
    (args     nil :type list)
    (fname    ""  :type string)
    (pathname nil :type (or null string))
    (source   ""  :type string))

  (defun make-cached-metallib (fname source)
    (let* ((base-path   (format nil "./.cl_metal_tmp/cached_~a" fname))
	   (metal-path  (format nil "~a.metal"    base-path))
	   (air-path    (format nil "~a.air"      base-path))
	   (lib-path    (format nil "~a.metallib" base-path)))
      (ensure-directories-exist (pathname "./.cl_metal_tmp/"))
      (with-open-file (stream (pathname metal-path)
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	(format stream "~a~%" source))

      (let* ((base-cmd `("xcrun" "-sdk" "macosx"))
	     (cmd1
	       (append
		base-cmd
		(list
		 "metal" "-c" metal-path "-o" air-path)))
	     (cmd2
	       (append
		base-cmd
		(list
		 "metallib" air-path "-o" lib-path))))
	(flet ((run-cmd (cmd)
		 (let* ((process-info
			  (uiop:launch-program
			   cmd
			   :error-output :stream))
			(error-output
			  (uiop:process-info-error-output process-info)))
		   (unless (zerop (uiop:wait-process process-info))
		     (error "cl-metal: Failed to create an inlined metallib cache due to compiling error:~%~a"
			    (alexandria:read-stream-content-into-string error-output))))))
	  (run-cmd cmd1)
	  (run-cmd cmd2))
	lib-path)))

  (defun %make-metal-inlined (fname args source)
    (let ((lib-path (make-cached-metallib fname source)))
      `(make-metallib
	:load-state :compiled-metallib
	:args     ',args
	:fname    ,fname
	:source   ,source
	:pathname ,lib-path))))

(defun make-metal (fname args source)
  "Embeds a metal code in Common Lisp. If the function can be reached in the toplevel, the compilation is inlined.
Return -> Metallib"
  (declare (type string fname source))
  ;; TODO
  ;; Appending #include ... namespace metal ...
  ;; LISP-like DSL -> Metal
  (make-metallib
   :load-state :not-yet-compiled
   :args   (map 'list #'parse-marg args args (range 0 (length args)))
   :fname  fname
   :source source))

;; Inlined make-metal compilation
;;(define-compiler-macro make-metal (fname args source)
;;  (%make-metal-inlined fname (map 'list #'parse-marg args (range 0 (length args))) source))

(defun funcall-metal (metallib &rest args)
  "Invokes the kernel described in metal"
  (declare (type Metallib metallib))

  ;; Ensures the function can be loaded?
  (with-swift-float-mode
    (ecase (metallib-load-state metallib)
      (:not-yet-compiled
       (%compile-metal-kernel
	(metallib-source metallib)
	(metallib-fname  metallib)))
      (:compiled-metallib
       (if (probe-file (metallib-pathname metallib)) ;; Does the cached .metalib still exist?
	   ;; Loading a cache stored in .cl_metal_cache
	   (clm-load-from-metallib
	    (metallib-pathname metallib)
	    (metallib-fname    metallib))
	   ;; Compiling again
	   (%compile-metal-kernel
	    (metallib-source metallib)
	    (metallib-fname  metallib)))))

    (assert (= (length args)
	       (length (metallib-args metallib)))
	    ()
	    "funcall-metal: The number of arguments is invaild: got ~a expected ~a"
	    (length args)
	    (length (metallib-args metallib)))
    
    ;; 1. Initialize all buffers
    (clm-reset-buffer)
    ;; 2. Send all pointers and sync with buffers
    (labels ((send (count rest-arr buff-list)
	       (if (null rest-arr)
		   (progn
		     ;; 3. Finally (after confirmed all buffers are sent)
		     ;; Commits the function
		     (clm-run 1)

		     ;; 4. Retreive all results
		     ;; metal-funcall returns a list of tensors whose state = :out or :io
		     (apply
		      #'values
		      (loop for arg in (metallib-args metallib)
			    for buf in buff-list
			    for arr in args
			    for nth upfrom 0
			    if (or (eql (marg-state arg) :out)
				   (eql (marg-state arg) :io))
			      collect (and
				       (clm-retrieve nth buf)
				       arr))))
		   
		   ;; the function send seems a little weird:
		   ;;  is there cffi:with-pointer-to-vector-data but a function version??		    
		   (with-pointer-to-vector-data
		       (bind* (car rest-arr))
		     (clm-alloc
		      count
		      (array-total-size (aref (car rest-arr) 0))
		      bind*
		      (type2iformat (aref (car rest-arr) 0)))
		     (send
		      (1+ count)
		      (cdr rest-arr)
		      `(,@buff-list ,bind*))))))
      (send 0 args nil))))


