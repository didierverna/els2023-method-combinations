(defpackage :els2023-method-combinations
  (:use :cl)
  (:import-from :sb-mop
    :funcallable-standard-class
    :generic-function-method-combination)
  (:export :find-method-combination!
	   :define-long-short-method-combination
	   :+! :*! :max! :min! :nconc! :progn! :and! :or! :list! :append!
	   :generic-function! :generic-function!-p :defgeneric!
	   :call-with-combination :call/cb :install-#!-reader-macro))

(in-package :els2023-method-combinations)


;; =========================
;; Method Combination Access
;; =========================

(defun find-method-combination!
    (name &optional options (errorp t)
	  &aux (info (gethash name sb-pcl::**method-combinations**)))
  "Find a method combination object for NAME and OPTIONS.
If ERRORP (the default), throw an error if no NAMEd method combination type is
found. Otherwise, return NIL. Note that when a NAMEd method combination type
exists, asking for a new set of (conformant) OPTIONS will always instantiate
the combination again, regardless of the value of ERRORP."
  (or (when info
	(or (cdr (assoc options (sb-pcl::method-combination-info-cache info)
			:test #'equal))
	    (cdar (push
		   (cons options
			 (funcall
			     (sb-pcl::method-combination-info-constructor info)
			   options))
		   (sb-pcl::method-combination-info-cache info)))))
      (and errorp (error "No method combination named ~A." name))))


;; =======================================
;; Build-In Long-Short Method Combinations
;; =======================================

(defmacro define-long-short-method-combination
    (name &key documentation (operator name) identity-with-one-argument)
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

(defmacro define-built-in-long-short-method-combinations ()
  "Define all built-in long-short method combinations.
This defines +! *! max! min! list! append! nconc! progn! and! and or!.
Note that the standard doesn't define a * method combination, but we do."
  `(progn
     ,@(mapcar
	(lambda (name)
	  `(define-long-short-method-combination
	       ,(intern (concatenate 'string (symbol-name name) "!"))
	     :documentation
	     ,(format nil "The ~A built-in long-short method combination."
		name)
	     :operator ,name
	     :identity-with-one-argument t))
	 '(+ * max min nconc progn and or))
     ,@(mapcar
	(lambda (name)
	  `(define-long-short-method-combination
	       ,(intern (concatenate 'string (symbol-name name) "!"))
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

;; ------------------------
;; Alternative combinations
;; ------------------------

(defmethod add-method :after
    ((generic-function generic-function!) method)
  "Invalidate all cached discriminating functions."
  (clrhash (functions generic-function)))

(defmethod remove-method :after
    ((generic-function generic-function!) method)
  "Invalidate all cached discriminating functions."
  (clrhash (functions generic-function)))

(defun call-with-combination
    (combination generic-function
     &rest arguments
     &aux (default-combination
	   (generic-function-method-combination generic-function)))
  "Call GENERIC-FUNCTION on ARGUMENTS with alternative method COMBINATION."
  (if (eq combination default-combination)
    (apply generic-function arguments)
    (let ((function (gethash combination (functions generic-function))))
      (if function
	(apply function arguments)
	(let (values)
	  ;; #### TODO: by digging a bit deeper into the implementation, it
	  ;; may be possible to do better (and faster) than using
	  ;; REINITIALIZE-INSTANCE twice like that. The potential gain may not
	  ;; be worth it however.
	  (reinitialize-instance generic-function
	    :method-combination combination)
	  (setq values
		(multiple-value-list (apply generic-function arguments))
		function
		(sb-kernel::%funcallable-instance-fun generic-function))
	  (reinitialize-instance generic-function
	    :method-combination default-combination)
	  (setf (gethash combination (functions generic-function))
		function)
	  (values-list values))))))

(defmacro call/cb (combination generic-function &rest arguments)
  "Call GENERIC-FUNCTION on ARGUMENTS with alternative method COMBINATION.
- GENERIC-FUNCTION is a generic function name.
- COMBINATION is a method combination type name or a list of a method
  combination type name, potentially followed by arguments."
  (unless (consp combination) (setq combination (list combination)))
  `(call-with-combination
    (find-method-combination! ',(car combination) ',(cdr combination))
    (function ,generic-function)
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
