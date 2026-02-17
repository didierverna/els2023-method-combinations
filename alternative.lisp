(in-package :els2023-method-combinations)



;; ==========================
;; Extended Generic Functions
;; ==========================

(defclass generic-function! (standard-generic-function)
  ((%functions :documentation "The discriminating functions cache.
This is a hash table mapping method combinations to discriminating functions."
	      :initform (make-hash-table)
	      :reader %functions))
  (:metaclass funcallable-standard-class)
  (:documentation "Meta-class for extended generic functions."))

(defun generic-function!-p (object)
  "Return T if OBJECT is an extended generic function."
  (typep object 'generic-function!))

(defun process-defgeneric!-options (options)
  "Process DEFGENERIC! options before calling DEFGENERIC.
Currently, this only means to provide a :generic-function-class option if
missing."
  (unless (assoc :generic-function-class options)
    (push '(:generic-function-class generic-function!) options))
  options)

(defmacro defgeneric! (name lambda-list &body options)
  "Wrapper around DEFGENERIC for creating extended generic functions."
  `(defgeneric ,name ,lambda-list ,@(process-defgeneric!-options options)))


;; -----------------------------
;; Generic function modification
;; -----------------------------

(defmethod add-method :after
    ((function generic-function!) method)
  "Invalidate all cached discriminating functions."
  (clrhash (%functions function)))

(defmethod remove-method :after
    ((function generic-function!) method)
  "Invalidate all cached discriminating functions."
  (clrhash (%functions function)))


;; -------------------------
;; Method combination change
;; -------------------------

;; #### NOTE: normally, we would like to do this in a before method, but SBCL
;; defines an around method on generic functions, notably with two assertions
;; checking that a generic function is not referenced by two different method
;; combinations at the same time, which would break for alternative method
;; combinations. Hence, we need to do the cleanup below first, in an around
;; method of our own. The reason SBCL does this in an around method is that it
;; compares the previous and the next method combinations, which couldn't be
;; done in before/after methods, as they would miss half of the required
;; information.
(defmethod reinitialize-instance :around
    ((function generic-function!)
     &key (method-combination nil method-combination-p) &allow-other-keys)
  "Remove potential trace of a new method combination as an alternative one."
  (when (and method-combination-p
	     (not (eq method-combination
		      (generic-function-method-combination function))))
    (remhash method-combination (%functions function))
    #+sbcl (sb-pcl::remove-from-weak-hashset
	    function
	    (method-combination-%generic-functions method-combination))
    #+ecl (setf (method-combination-%generic-functions method-combination)
		(remove function
			(method-combination-%generic-functions
			 method-combination))))
  (call-next-method))


;; ----------------------------------------
;; Method combination redefinition handling
;; ----------------------------------------

(defmethod update-generic-function-for-redefined-method-combination
    ((function generic-function!) previous current)
  "Either fall back to the default behavior when CURRENT combination is
FUNCTION's regular method combination, or invalidate the corresponding cached
discriminating function."
    (if (eq current (generic-function-method-combination function))
      (call-next-method)
      ;; #### FIXME: do we need this or not ? It doesn't seem to affect the
      ;; tests and I don't currently understand SBCL's effective method
      ;; caches.
      ;; (sb-pcl::flush-effective-method-cache function)
      (remhash current (%functions function))))




;; ===============================
;; Alternative Method Combinations
;; ===============================

(defun call-with-combination
    (combination function
     &rest arguments
     &aux (default-combination (generic-function-method-combination function)))
  "Call generic FUNCTION on ARGUMENTS with alternative method COMBINATION."
  (if (eq combination default-combination)
    (apply function arguments)
    (let ((alternative (gethash combination (%functions function))))
      (if alternative
	(apply alternative arguments)
	(let (values)
	  ;; #### TODO: by digging a bit deeper into the implementation, it
	  ;; may be possible to do better (and faster) than using
	  ;; REINITIALIZE-INSTANCE twice like that. The potential gain may not
	  ;; be worth it however.
	  (reinitialize-instance function :method-combination combination)
	  (setq values (multiple-value-list (apply function arguments)))
	  #+sbcl (setq alternative (sb-kernel::%funcallable-instance-fun function))
	  (reinitialize-instance function
	    :method-combination default-combination)
	  (setf (gethash combination (%functions function)) alternative)
	  #+sbcl (sb-pcl::add-to-weak-hashset
		  function
		  (method-combination-%generic-functions combination))
	  #+ecl (push function
		      (method-combination-%generic-functions combination))
	  (values-list values))))))

(defmacro call/cb (combination function &rest arguments)
  "Call generic FUNCTION on ARGUMENTS with alternative method COMBINATION.
- COMBINATION is a method combination type name or a list of a method
  combination type name, potentially followed by arguments.
- FUNCTION is a generic function designator."
  (unless (consp combination) (setq combination (list combination)))
  (when (symbolp function) (setq function `(function ,function)))
  `(call-with-combination
    (find-method-combination* ',(car combination) ',(cdr combination))
    ,function
    ,@arguments))

(defun install-#!-reader-macro ()
  "Install a #! reader-macro for CALL/CB in the current readtable.
The new syntax is #!combination(function arguments...)."
  (set-dispatch-macro-character #\# #\!
    (lambda (stream subchar arg
	     &aux (combination (read stream t nil t))
		  (function-call (read stream t nil t)))
      (declare (ignore subchar arg))
      `(call/cb ,combination ,@function-call))))
