
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
   :args   args
   :fname  fname
   :source source))

;; Inlined make-metal compilation
(define-compiler-macro make-metal (fname args source)
  (%make-metal-inlined fname args source))

;; [TODO] Multiple Arguments
(defun funcall-metal (metal out-buffer in-buffer)
  "Invokes the kernel described in metal"
  (declare (type Metallib metal))

  ;; Ensures the function can be loaded?
  (with-swift-float-mode
    (ecase (metallib-load-state metal)
      (:not-yet-compiled
       (%compile-metal-kernel
	(metallib-source metal)
	(metallib-fname  metal)))
      (:compiled-metallib
       (if (probe-file (metallib-pathname metal)) ;; Does the cached .metalib still exist?
	   ;; Loading a cache stored in .cl_metal_cache
	   (clm-load-from-metallib
	    (metallib-pathname metal)
	    (metallib-fname    metal))
	   ;; Compiling again
	   (%compile-metal-kernel
	    (metallib-source metal)
	    (metallib-fname  metal)))))

    ;; Commits the function
    (with-pointer-to-vector-data (in* in-buffer)
      (with-pointer-to-vector-data (out* out-buffer)      
	(clm-alloc (array-total-size in-buffer)
		   in*
		   (type2iformat (aref in-buffer 0))
		   (array-total-size out-buffer)
		   (type2iformat (aref out-buffer 0)))
	(clm-run 1)
	(clm-retrieve (array-total-size out-buffer) out*)))
    out-buffer))

