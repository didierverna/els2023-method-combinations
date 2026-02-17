(in-package :els2023-method-combinations)


;; A better protocol to access method combination objects. This is merely a
;; duplication of my patched SBCL's code for FIND-METHOD-COMBINATION. There's
;; no point in implementing a SETF method here since those objects are handled
;; internally and automatically.
(defun find-method-combination*
    (name &optional options (errorp t)
	  &aux (type (find-method-combination-type name errorp)))
  "Find a method combination object for NAME and OPTIONS.
If ERRORP (the default), throw an error if no NAMEd method combination type is
found. Otherwise, return NIL. Note that when a NAMEd method combination type
exists, asking for a new set of (conformant) OPTIONS will always instantiate
the combination again, regardless of the value of ERRORP."
  (when type
    (or (gethash options
		 #+sbcl (sb-pcl::method-combination-type-%cache type)
		 #+ecl (clos::method-combination-type-%instances type))
	(setf (gethash options
		       #+sbcl (sb-pcl::method-combination-type-%cache type)
		       #+ecl (clos::method-combination-type-%instances type))
	      (funcall #+sbcl (sb-pcl::method-combination-%constructor type)
		       #+ecl (clos::method-combination-%constructor type)
		options)))))

(defmacro change-method-combination (function &rest combination)
  "Change generic FUNCTION to a new method COMBINATION.
- FUNCTION is a generic function designator.
- COMBINATION is a method combination type name, potentially followed by
arguments."
  (when (symbolp function) (setq function `(function ,function)))
  `(reinitialize-instance ,function
     :method-combination
     (find-method-combination* ',(car combination) ',(cdr combination))))
