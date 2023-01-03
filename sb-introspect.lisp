(in-package :sb-introspect)

(defun find-definition-source (object)
  (typecase object
    ((or sb-pcl::condition-class sb-pcl::structure-class)
     (let ((classoid (sb-pcl::class-classoid object)))
       (when classoid
	 (translate-source-location
	  (sb-kernel::classoid-source-location classoid)))))
    (method-combination
     (car
      (find-definition-sources-by-name
       (sb-pcl::method-combination-type-name object) :method-combination)))
    (package
     (translate-source-location (sb-impl::package-source-location object)))
    ((or class sb-mop:slot-definition)
     (translate-source-location (sb-pcl::definition-source object)))
    ;; Use the PCL definition location information instead of the function
    ;; debug-info for methods and generic functions. Sometimes the
    ;; debug-info would point into PCL internals instead of the proper
    ;; location.
    (generic-function
     (let ((source (translate-source-location
		    (sb-pcl::definition-source object))))
       (when source
	 (setf (definition-source-description source)
	       (list (sb-mop:generic-function-lambda-list object))))
       source))
    (method
     (let ((source (translate-source-location
		    (sb-pcl::definition-source object))))
       (when source
	 (setf (definition-source-description source)
	       (append (method-qualifiers object)
		       (if (sb-mop:method-generic-function object)
			 (sb-pcl::unparse-specializers
			  (sb-mop:method-generic-function object)
			  (sb-mop:method-specializers object))
			 (sb-mop:method-specializers object)))))
       source))
    (interpreted-function
     #+sb-eval
     (let ((source (translate-source-location
		    (sb-eval:interpreted-function-source-location object))))
       source)
     #+sb-fasteval
     (translate-source-location (sb-interpreter:fun-source-location object)))
    (function
     (find-function-definition-source object))
    ((or condition standard-object structure-object)
     (find-definition-source (class-of object)))
    (t
     (error "Don't know how to retrieve source location for a ~S"
	    (type-of object)))))

(defun method-combination-lambda-list (method-combination)
  "Return the lambda-list of METHOD-COMBINATION designator.
METHOD-COMBINATION can be a method combination object,
or a method combination name."
  (let* ((name (etypecase method-combination
		 (symbol method-combination)
		 (method-combination
		  (sb-pcl::method-combination-type-name method-combination))))
	 (type (or (gethash name sb-pcl::**method-combination-types**)
		   (error "~S: no such method combination." name))))
    (sb-pcl::method-combination-type-lambda-list type)))
