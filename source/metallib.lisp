
(in-package :cl-metal)

;; Dynamically Generated .metalib
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct Metallib
    "Stores the cached function of metal"
    (pathname (symbol-name (gensym "METALLIB")) :type string)
    (source  "" :type string))

  ;; to-metalib
  ;; from-metalib
  )

(defun metal (source)
  ;; Creates a cache ./.cl-metal-cache/gensym.metallib
  )

;; (define-compiler-macro metal (source) )

