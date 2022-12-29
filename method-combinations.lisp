(defpackage :els2023-method-combinations
  (:use :cl)
  (:import-from :sb-mop
    :funcallable-standard-class
    :generic-function-method-combination)
  (:export :find-method-combination!
	   :define-long-short-method-combination
	   :+! :*! :max! :min! :nconc! :progn! :and! :or! :list! :append!
	   :generic-function! :generic-function!-p :defgeneric!
	   :change-method-combination
	   :call-with-combination :call/cb :install-#!-reader-macro))

(in-package :els2023-method-combinations)


;; ===================
;; Method Combinations
;; ===================

;; Post ELS 2018, we don't need method combinators anymore, as SBCL correctly
;; handles generic functions and method combinations updates. The current
;; implementation still leaves room for improvement however. See comment
;; below.

;; -------------------------
;; Method combination access
;; -------------------------

;; A better protocol to access method combination objects. This is merely a
;; duplication of SBCL's code for the original FIND-METHOD-COMBINATION.
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

;; The wording of the standard suggests that method combinations thingies
;; (that is, what's defined by DEFINE-METHOD-COMBINATION) behave like
;; parametric types. SBCL doesn't have an object-oriented representation for
;; those (merely a METHOD-COMBINATION-INFO structure), but it would make sense
;; to turn that into an OO hierarchy, in which case SBCL's method combination
;; classes would need to be refined (for example, the OPTIONS slot in
;; STANDARD-METHOD-COMBINATION is not nice). A new method combination type
;; would become a subclass of SHORT/LONG-METHOD-COMBINATION, and specific uses
;; with specific sets of arguments would become instances of those.

;; In any case, in the current state of things, FIND-METHOD-COMBINATION[!] is
;; closer to FIND-METHOD than to FIND-CLASS for instance, in that a name is
;; not associated with a single method combination, but with a set of those
;; (a METHOD-COMBINATION-INFO structure). So it doesn't make sense to
;; implement a SETF method for it.


;; ----------------------------
;; Method combinations updating
;; ----------------------------

;; SBCL has an UPDATE-MCS function which it calls whenever a method
;; combination type is created or redefined. In case of a redefinition, this
;; function transfers the old cache to the new info structure, and then
;; updates the existing method combination instances by calling CHANGE-CLASS
;; on them (as I did in the ELS 2018 version), and invalidates all associated
;; generic functions. This is essentially what the "Clients management"
;; section of my previous work did.

;; One improvement we can make here is move the generic functions invalidation
;; code to an UPDATE-INSTANCE-FOR-DIFFERENT-CLASS method on method
;; combinations.

(defmethod update-instance-for-different-class :after
  ((previous sb-pcl::standard-method-combination)
   (current sb-pcl::standard-method-combination)
   &key &allow-other-keys)
  (maphash (lambda (gf ignore)
             (declare (ignore ignore))
	     (update-generic-function-for-redefined-method-combination
	      gf current))
	   (sb-pcl::method-combination-%generic-functions current)))

(sb-ext:with-unlocked-packages (sb-pcl)
  (defun sb-pcl::update-mcs (name new old frobmc)
    (setf (gethash name sb-pcl::**method-combinations**) new)
    ;; for correctness' sake we should probably lock **METHOD-COMBINATIONS**
    ;; while we're updating things, to defend against defining gfs in one
    ;; thread while redefining the method combination in another thread.
    (when old
      (setf (sb-pcl::method-combination-info-cache new)
	    (sb-pcl::method-combination-info-cache old))
      (setf (sb-pcl::method-combination-info-cache old) nil)
      (dolist (entry (sb-pcl::method-combination-info-cache new))
	(funcall frobmc (cdr entry))))))

;; Also, method combination objects are low-level and should only be
;; manipulated by SBCL's internals, so it's not worth implementing anything
;; special for REINITIALIZE-INSTANCE.



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


;; -------------------------
;; Method combination change
;; -------------------------

;; #### FIXME: this is not enough. We need to check extended generic functions
;; for cached discriminating functions related to the change.

(defmacro change-method-combination (function &rest combination)
  "Change generic FUNCTION to a new method COMBINATION.
- FUNCTION is a generic function designator.
- COMBINATION is a method combination type name, potentially followed by
arguments."
  (when (symbolp function) (setq function `(function ,function)))
  `(reinitialize-instance ,function
     :method-combination
     (find-method-combination! ',(car combination) ',(cdr combination))))


;; ----------------------------------------
;; Method combination redefinition handling
;; ----------------------------------------

(defgeneric update-generic-function-for-redefined-method-combination
    (function combination)
  (:documentation
   "Inform generic FUNCTION that method COMBINATION was redefined.")
  (:method ((function generic-function) combination)
    "Flush the effective method cache and reinitialize FUNCTION."
    ;; This is just what SBCL does.
    (sb-pcl::flush-effective-method-cache function)
    (reinitialize-instance function))
  (:method ((function generic-function!) combination)
    "Either fall back to the default behavior when COMBINATION is FUNCTION's
regular method combination, or invalidate the corresponding cached
discriminating function." 
    (if (eq combination (generic-function-method-combination function))
      (call-next-method)
      ;; #### FIXME: do we need this or not ? It doesn't seem to affect the
      ;; tests and I don't currently understand SBCL's effective method
      ;; caches.
      ;; (sb-pcl::flush-effective-method-cache function)
      (remhash combination (functions function)))))


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
	  (values-list values))))))

(defmacro call/cb (combination function &rest arguments)
  "Call generic FUNCTION on ARGUMENTS with alternative method COMBINATION.
- COMBINATION is a method combination type name or a list of a method
  combination type name, potentially followed by arguments.
- FUNCTION is a generic function designator."
  (unless (consp combination) (setq combination (list combination)))
  (when (symbolp function) (setq function `(function ,function)))
  `(call-with-combination
    (find-method-combination! ',(car combination) ',(cdr combination))
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
