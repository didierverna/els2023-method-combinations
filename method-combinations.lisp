(defpackage :els2023-method-combinations
  (:use :cl)
  (:import-from :sb-mop
    :find-method-combination-type
    :short-method-combination :long-method-combination
    :funcallable-standard-class
    :generic-function-method-combination
    :update-generic-function-for-redefined-method-combination)
  (:export :find-method-combination* :change-method-combination
	   :define-long-short-method-combination
	   :generic-function! :generic-function!-p :defgeneric!
	   :call-with-combination :call/cb :install-#!-reader-macro))

(in-package :els2023-method-combinations)


;; =============================
;; Method Combinations Utilities
;; =============================

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
    (or (gethash options (sb-pcl::method-combination-type-%cache type))
	(setf (gethash options (sb-pcl::method-combination-type-%cache type))
	      (funcall (sb-pcl::method-combination-%constructor type)
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

(defmacro define-long-short-method-combination
    (name &key documentation (operator name) identity-with-one-argument
	       (method-combination-class 'long-method-combination))
  "Define NAME as a long-short method combination.
A long-short method combination resembles a short one, with the following
differences:
- the primary methods must not be qualified,
- :before and :after methods are available."
  (let ((documentation (when documentation (list documentation)))
	(single-method-call (if identity-with-one-argument
			      '`(call-method ,(first primary))
			      ``(,',operator (call-method ,(first primary))))))
    `(define-method-combination ,name (&optional (order :most-specific-first))
       ((around (:around))
	(before (:before))
	(primary () :order order :required t)
	(after (:after)))
       (:method-combination-class ,method-combination-class)
       ,@documentation
       (flet ((call-methods (methods)
		(mapcar (lambda (method) `(call-method ,method)) methods)))
	 (let* ((primary-form (if (rest primary)
				`(,',operator ,@(call-methods primary))
				,single-method-call))
		(form (if (or before after)
			`(multiple-value-prog1
			     (progn ,@(call-methods before) ,primary-form)
			   ,@(call-methods (reverse after)))
			primary-form)))
	   (if around
	     `(call-method
	       ,(first around) (,@(rest around) (make-method ,form)))
	     form))))))



;; =======================================
;; Build-In Long-Short Method Combinations
;; =======================================

(defmacro define-built-in-long-short-method-combinations ()
  "Define all built-in long-short method combinations.
This defines :+ :* :max :min :list :append :nconc :progn :and and :or.
Note that the standard doesn't define a * method combination, but we do."
  `(progn
     ,@(mapcar
	(lambda (name)
	  `(define-long-short-method-combination
	       ,(intern (symbol-name name) :keyword)
	     :documentation
	     ,(format nil "The ~A built-in long-short method combination."
		name)
	     :operator ,name
	     :identity-with-one-argument t))
	 '(+ * max min nconc progn and or))
     ,@(mapcar
	(lambda (name)
	  `(define-long-short-method-combination
	       ,(intern (symbol-name name) :keyword)
	     :documentation
	     ,(format nil "The ~A built-in long-short method combination."
		name)
	     :operator ,name))
	'(list append))))

(define-built-in-long-short-method-combinations)



;; ==========================
;; Extended Generic Functions
;; ==========================

(defclass generic-function! (standard-generic-function)
  ((functions :documentation "The discriminating functions cache.
This is a hash table mapping method combinations to discriminating functions."
	      :initform (make-hash-table)
	      :reader functions))
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
    (remhash method-combination (functions function))
    (sb-pcl::remove-from-weak-hashset
     function
     (sb-pcl::method-combination-%generic-functions method-combination)))
  (call-next-method))


;; ----------------------------------------
;; Method combination redefinition handling
;; ----------------------------------------

(defmethod update-generic-function-for-redefined-method-combination
    ((function generic-function!) combination)
  "Either fall back to the default behavior when COMBINATION is FUNCTION's
regular method combination, or invalidate the corresponding cached
discriminating function."
    (if (eq combination (generic-function-method-combination function))
      (call-next-method)
      ;; #### FIXME: do we need this or not ? It doesn't seem to affect the
      ;; tests and I don't currently understand SBCL's effective method
      ;; caches.
      ;; (sb-pcl::flush-effective-method-cache function)
      (remhash combination (functions function))))


;; ------------------------
;; Alternative combinations
;; ------------------------

(defmethod add-method :after
    ((function generic-function!) method)
  "Invalidate all cached discriminating functions."
  (clrhash (functions function)))

(defmethod remove-method :after
    ((function generic-function!) method)
  "Invalidate all cached discriminating functions."
  (clrhash (functions function)))

(defun call-with-combination
    (combination function
     &rest arguments
     &aux (default-combination (generic-function-method-combination function)))
  "Call generic FUNCTION on ARGUMENTS with alternative method COMBINATION."
  (if (eq combination default-combination)
    (apply function arguments)
    (let ((alternative (gethash combination (functions function))))
      (if alternative
	(apply alternative arguments)
	(let (values)
	  ;; #### TODO: by digging a bit deeper into the implementation, it
	  ;; may be possible to do better (and faster) than using
	  ;; REINITIALIZE-INSTANCE twice like that. The potential gain may not
	  ;; be worth it however.
	  (reinitialize-instance function :method-combination combination)
	  (setq values (multiple-value-list (apply function arguments))
		alternative (sb-kernel::%funcallable-instance-fun function))
	  (reinitialize-instance function
	    :method-combination default-combination)
	  (setf (gethash combination (functions function)) alternative)
	  (sb-pcl::add-to-weak-hashset
	   function
	   (sb-pcl::method-combination-%generic-functions combination))
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
