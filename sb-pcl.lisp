;;; sb-pcl.lisp --- Method combination infrastructure reimplementation

;; #### WARNING: this code is meant to be loaded in a specific version of SBCL
;; in which some standard and implementation-specific things related to method
;; combinations have been renamed to get out of the way. This is necessary
;; because the MOP forbids the redefinition of some classes of class
;; meta-objects, which is exactly what we want to do here.

;; Post ELS 2018, we don't need method combinators anymore, as SBCL correctly
;; handles generic functions and method combinations updates. The current
;; implementation still leaves room for improvement, however, which is the
;; purpose of this code.

;; The wording of the standard suggests that method combinations thingies
;; (that is, what's defined by DEFINE-METHOD-COMBINATION) behave like
;; parametric types. SBCL doesn't have an object-oriented representation for
;; those (merely a METHOD-COMBINATION-INFO structure). The code below makes
;; this happen. It implements a METHOD-COMBINATION-TYPE hierarchy replacing
;; SBCL's INFO structure. Each new method combination type creates a subclass
;; in the method combinations hierarchy, with a method combination type as its
;; meta-class. This allows a clean separation between information belonging to
;; the method combination type, and information belonging to one specific use
;; (instance) by a generic function.

(in-package :sb-pcl)


(defgeneric update-generic-function-for-redefined-method-combination
    (function combination)
  (:documentation
   "Inform generic FUNCTION that its method COMBINATION was redefined.")
  (:method ((function generic-function) combination)
    "Flush the effective method cache and reinitialize FUNCTION."
    ;; This is just what SBCL does.
    (flush-effective-method-cache function)
    (reinitialize-instance function)))


;; =========================
;; Infrastructure Definition
;; =========================

;; Replacement for **method-combinations**.
(defvar **method-combination-types** (make-hash-table :test 'eq)
  "The global method combination types hash table.
This hash table maps names to method combination types.")

;; Analog to find class.
;; #### NOTE: I'm not defining a SETF method on it here because we wouldn't
;; want it to be public.
(defun find-method-combination-type (name &optional (errorp t))
  "Find a NAMEd method combination type.
If ERRORP (the default), throw an error if no such method combination type is
found. Otherwise, return NIL."
  (or (gethash name **method-combination-types**)
      (when errorp
	(error "There is no method combination type named ~A." name))))


;; ------------------------
;; Method combination types
;; ------------------------

;; Replacements for the method-combination-info structure.

;; #### WARNING: trying to hijack the NAME slot in anonymous class metaobjects
;; (that is, classes that are not meant to be registered globally) is
;; dangerous, especially during bootstrap. I've seen very strange errors
;; occurring when trying to do so. Hence the TYPE-NAME slot below.
(defclass method-combination-type (standard-class)
  ((type-name :initarg :type-name :reader method-combination-type-name)
   (lambda-list :initform nil :initarg :lambda-list
		:reader method-combination-type-lambda-list)
   ;; A reader without "type" in the name seems more readable to me.
   (%constructor :reader method-combination-%constructor)
   (%cache :initform (make-hash-table :test #'equal)
	   :reader method-combination-type-%cache))
  (:documentation "Meta-class for method combination types.
It is the base class for short and long method combination types meta-classes.
This only class directly implemented as this class is the standard method
combination class."))

(defmethod print-object ((type method-combination-type) stream)
  (print-unreadable-object (type stream :type t :identity t)
    (format stream "~S ~:S"
      (slot-value-for-printing type 'type-name)
      (slot-value-for-printing type 'lambda-list))))

;; #### NOTE: (DOCUMENTATION OBJECT T) is automatically supported on method
;; #### combination types because we set the documentation class slot when we
;; #### create them in LOAD-*-DEFCOMBIN.
(defmethod documentation
    ((type method-combination-type) (doctype (eql 'method-combination)))
  (documentation type t))

(defmethod (setf documentation)
    (value (type method-combination-type) (doctype (eql 'method-combination)))
  (setf (documentation type t) value))

;; Validate the creation of subclasses of METHOD-COMBINATION implemented as
;; METHOD-COMBINATION-TYPE.
(defmethod validate-superclass
    ((class method-combination-type) (superclass standard-class))
  t)

(defun load-defcombin
    (name new documentation &aux (old (find-method-combination-type name nil)))
  (when old
    (setf (slot-value new '%cache) (method-combination-type-%cache old))
    (maphash (lambda (options combination)
	       (declare (ignore options))
	       (change-class combination new))
	     (method-combination-type-%cache new)))
  (setf (gethash name **method-combination-types**) new)
  (setf (random-documentation name 'method-combination) documentation)
  name)


(defclass short-method-combination-type (method-combination-type)
  ((lambda-list :initform '(&optional (order :most-specific-first)))
   (operator :initarg :operator
	     :reader short-method-combination-type-operator)
   (identity-with-one-argument
    :initarg :identity-with-one-argument
    :reader short-method-combination-type-identity-with-one-argument))
  (:documentation "Meta-class for short method combination types."))


(defclass long-method-combination-type (method-combination-type)
  ((args-lambda-list :initform nil :initarg :args-lambda-list
		     :reader long-method-combination-type-args-lambda-list)
   ;; #### NOTE: in original SBCL, the FUNCTION slot in long method
   ;; combination objects is unused. This one is, and serves as a replacement
   ;; for the global variable *long-method-combination-functions*.
   (%function :initarg :function
	      :reader long-method-combination-type-%function))
  (:documentation "Meta-class for long method combination types."))


;; -------------------
;; Method combinations
;; -------------------

(with-unlocked-packages (cl)
  (defclass method-combination (metaobject)
    ()
    (:documentation "Base class for method combinations.")))

(let ((classes (mapcar #'find-class
		 '(class eql-specializer class-eq-specializer
		   ;; #### FIXME: what about METHOD-COMBINATION-TYPE here ?
		   method method-combination))))
  (dolist (class classes)
    (dolist (other-class classes)
      (unless (eq class other-class)
	(pushnew other-class (class-incompatible-superclass-list class)
		 :test #'eq)))))

(defmethod documentation ((combination method-combination) (doctype (eql t)))
  (documentation (class-of combination) t))

(defmethod documentation
    ((combination method-combination) (doctype (eql 'method-combination)))
  (documentation (class-of combination) t))

(defmethod (setf documentation)
    (value (combination method-combination) (doctype (eql t)))
  (setf (documentation (class-of combination) t) value))

(defmethod (setf documentation)
    (value
     (combination method-combination)
     (doctype (eql 'method-combination)))
  (setf (documentation (class-of combination) t) value))

;; #### NOTE: SBCL normally has the equivalent of this class instantiated
;; once, and also serving as a base class for other method combinations, but I
;; don't like that. I prefer to make the standard method combination behave as
;; the other ones, in other words, to have its own class implemented as a
;; method-combination-type.
(defclass standard-method-combination (method-combination)
  ((options :initform nil
	    :initarg :options
	    :reader method-combination-options)
   (%generic-functions :initform (make-gf-hashset)
		       :reader method-combination-%generic-functions))
  (:documentation "Base class for standard method combinations.
It is the base class for short and long method combinations.
The only concrete subclass of this class is the standard (standard) method
combination class."))

(defmethod method-combination-type-name
    ((combination standard-method-combination))
  ;; #### NOTE: every concrete method combination class is supposed to be
  ;; implemented as a method combination type.
  (method-combination-type-name (class-of combination)))

(defmethod print-object ((combination standard-method-combination) stream)
  (print-unreadable-object (combination stream :type t :identity t)
    (format stream "~S ~:S"
      ;; #### NOTE: every concrete method combination class is supposed to be
      ;; implemented as a method combination type.
      (slot-value-for-printing (class-of combination) 'type-name)
      (slot-value-for-printing combination 'options))))

(defmethod update-instance-for-different-class :after
    ((previous standard-method-combination)
     (current standard-method-combination)
     &key &allow-other-keys)
  (maphash (lambda (gf ignore)
	     (declare (ignore ignore))
	     (update-generic-function-for-redefined-method-combination
	      gf current))
	   (method-combination-%generic-functions current)))


;; Short method combinations
;; -------------------------

;; #### NOTE: in principle, short method combinations would only have at most
;; two different instances, because the only possible choice for options is
;; :MOST-SPECIFIC-FIRST or :MOST-SPECIFIC-LAST. However, the caches keep track
;; of the options provided by the programmer (it's nice because it's
;; informative). As a result, if the method combination is used without any
;; argument, or explicitly with :MOST-SPECIFIC-FIRST, we will end up with 2
;; different yet identical instances (so possibly 3 in total if
;; :MOST-SPECIFIC-LAST appears as well). Not such a big deal.
(defclass short-method-combination (standard-method-combination)
  ()
  (:documentation "Base class for short method combinations."))

;; #### NOTE: in SBCL, accessor names are inconsistent. Short ones are called
;; SHORT-COMBINATION-... whereas long ones are called
;; LONG-METHOD-COMBINATION-...
(defmethod short-combination-operator ((combination short-method-combination))
  (short-method-combination-type-operator (class-of combination)))

;; #### NOTE: in SBCL, accessor names are inconsistent. Short ones are called
;; SHORT-COMBINATION-... whereas long ones are called
;; LONG-METHOD-COMBINATION-...
(defmethod short-combination-identity-with-one-argument
    ((combination short-method-combination))
  (short-method-combination-type-identity-with-one-argument
   (class-of combination)))

(defmethod compute-effective-method
    ((function generic-function)
     (combination short-method-combination)
     applicable-methods)
  (short-compute-effective-method function combination applicable-methods))

(defmethod initialize-instance :before
    ((instance short-method-combination)
     &key options &allow-other-keys
     &aux (name (method-combination-type-name instance)))
  (when (cdr options)
    (method-combination-error
     "Illegal options to the ~S short method combination.~%~
      Short method combinations accept a single ORDER argument."
     name))
  (unless (member (car options) '(:most-specific-first :most-specific-last))
    (method-combination-error
     "Illegal ORDER option to the ~S short method combination.~%~
      ORDER must be either :MOST-SPECIFIC-FIRST or :MOST-SPECIFIC-LAST."
     name)))

(defmethod invalid-qualifiers ((gf generic-function)
			       (combin short-method-combination)
			       method)
  (let* ((qualifiers (method-qualifiers method))
	 (qualifier (first qualifiers))
	 (type-name (method-combination-type-name combin))
	 (why (cond
		((null qualifiers)
		 "has no qualifiers")
		((cdr qualifiers)
		 "has too many qualifiers")
		(t
		 (aver (not (short-method-combination-qualifier-p
			     type-name qualifier)))
		 "has an invalid qualifier"))))
    (invalid-method-error
     method
     "~@<The method ~S on ~S ~A.~
      ~@:_~@:_~
      The method combination type ~S was defined with the short form ~
      of DEFINE-METHOD-COMBINATION and so requires all methods have ~
      either ~{the single qualifier ~S~^ or ~}.~@:>"
     method gf why type-name (short-method-combination-qualifiers type-name))))

;; Overridden to allow a :method-combination-class argument.
(defun expand-short-defcombin (whole)
  (let* ((canary (cons nil nil))
         (type-name (cadr whole))
         (documentation (getf (cddr whole) :documentation canary))
         (ioa (getf (cddr whole) :identity-with-one-argument nil))
         (operator
           (getf (cddr whole) :operator type-name))
	 (class (getf (cddr whole) :method-combination-class
		      'short-method-combination)))
    (unless (or (eq documentation canary)
                (stringp documentation))
      (%program-error
       "~@<~S argument to the short form of ~S must be a string.~:@>"
       :documentation 'define-method-combination))
    `(load-short-defcombin
      ',type-name ',operator ',ioa
      ',(and (neq documentation canary) documentation)
      ',class
      (sb-c:source-location))))

;; Replacement for both load-short-defcombin and short-combine-methods.
(defun load-short-defcombin
    (name operator identity-with-one-argument documentation class
     source-location
     &aux (class (find-class class)))
  (unless (subtypep class 'short-method-combination)
    (method-combination-error
     "Invalid method combination class: ~A.~%~
      When defining a method combination type in short form, the provided~%~
      method combination class must be a subclass of SHORT-METHOD-COMBINATION."
     class))
  ;; #### NOTE: we can't change-class class metaobjects, so we need to
  ;; recreate a brand new one.
  (let ((new (make-instance 'short-method-combination-type
	       'source source-location
	       :direct-superclasses (list class)
	       :documentation documentation
	       :type-name name
	       :operator operator
	       :identity-with-one-argument identity-with-one-argument)))
    (setf (slot-value new '%constructor)
	  (lambda (options)
	    (funcall #'make-instance
	      new :options (or options '(:most-specific-first)))))
    (load-defcombin name new documentation)))


;; Long method combinations
;; ------------------------

(defclass long-method-combination (standard-method-combination)
  ()
  (:documentation "Base class for long method combinations."))

;; #### NOTE: in SBCL, accessor names are inconsistent. Short ones are called
;; SHORT-COMBINATION-... whereas long ones are called
;; LONG-METHOD-COMBINATION-...
(defmethod long-method-combination-args-lambda-list
    ((combination long-method-combination))
  (long-method-combination-type-args-lambda-list (class-of combination)))

(defmethod compute-effective-method
    ((function generic-function)
     (combination long-method-combination)
     applicable-methods)
  (funcall (long-method-combination-type-%function (class-of combination))
    function combination applicable-methods))

;; Overridden to allow a :method-combination-class argument.
(defun expand-long-defcombin (form)
  (let ((type-name (cadr form))
        (lambda-list (caddr form))
        (method-group-specifiers-presentp (cdddr form))
        (method-group-specifiers (cadddr form))
        (body (cddddr form))
        (args-option ())
        (gf-var nil)
	(mc-class 'long-method-combination))
    (unless method-group-specifiers-presentp
      (%program-error
       "~@<The long form of ~S requires a list of method group specifiers.~:@>"
       'define-method-combination))
    (when (and (consp (car body)) (eq (caar body) :arguments))
      (setq args-option (cdr (pop body))))
    (when (and (consp (car body)) (eq (caar body) :generic-function))
      (unless (and (cdar body) (symbolp (cadar body)) (null (cddar body)))
        (%program-error
	 "~@<The argument to the ~S option of ~S must be a single symbol.~:@>"
         :generic-function 'define-method-combination))
      (setq gf-var (cadr (pop body))))
    (when (and (consp (car body)) (eq (caar body) :method-combination-class))
      (unless (and (cdar body) (symbolp (cadar body)) (null (cddar body)))
        (%program-error
	 "~@<The argument to the ~S option of ~S must be a single symbol.~:@>"
         :method-combination-class 'define-method-combination))
      (setq mc-class (cadr (pop body))))
    (multiple-value-bind (documentation function)
        (make-long-method-combination-function
         type-name lambda-list method-group-specifiers args-option gf-var
         body)
      `(load-long-defcombin
	',type-name ',documentation #',function ',lambda-list
	',args-option ',mc-class (sb-c:source-location)))))

(defun load-long-defcombin
    (name documentation function lambda-list args-lambda-list class
     source-location
     &aux (class (find-class class)))
  (unless (subtypep class 'long-method-combination)
    (method-combination-error
     "Invalid method combination class: ~A.~%~
      When defining a method combination type in long form, the provided~%~
      method combination class must be a subclass of LONG-METHOD-COMBINATION."
     class))
  ;; #### NOTE: we can't change-class class metaobjects, so we need to
  ;; recreate a brand new one.
  (let ((new (make-instance 'long-method-combination-type
	       'source source-location
	       :direct-superclasses (list class)
	       :documentation documentation
	       :type-name name
	       :lambda-list lambda-list
	       :args-lambda-list args-lambda-list
	       :function function)))
    (setf (slot-value new '%constructor)
	  (lambda (options) (funcall #'make-instance new :options options)))
    (load-defcombin name new documentation)))



;; =========================
;; Infrastructure Population
;; =========================

;; #### NOTE: from SBCL's original METHOD-COMBINATION-INFO structure, it's
;; impossible (or extremely difficult to figure out whether a method
;; combination would be short or long: the cache could tell us, but it may be
;; empty; the lambda-list could hint us, but even a long method combination
;; could use a lambda-list looking like a short one... Consequently, we will
;; simply re-create the standard and built-in ones here (which is what would
;; happen anyway if this was actually part of SBCL). This is why this code
;; must be loaded as early as possible.

;; We will, however, scan all these combinations and recreate the caches.


;; ---------------------------
;; Standard method combination
;; ---------------------------

;; #### WARNING: we work with CLOS layer 1 (the macro level) below because
;; it's much simpler to create the specialization of COMPUTE-EFFECTIVE-METHOD
;; this way. The unfortunate side effect of that is that the class below is
;; defined globally, which I don't really want (all other concrete method
;; combination classes are anonymous; even the built-in short ones).
(defclass standard-standard-method-combination (standard-method-combination)
  ()
  (:metaclass method-combination-type)
  (:documentation "The standard (standard) method combination class.
This class is a singleton class: the only instance is that of the standard
method combination."))

(let* ((class (find-class 'standard-standard-method-combination))
       (instance (make-instance class)))
  (setf (slot-value class 'type-name) 'standard)
  (setf (slot-value class '%constructor)
	(lambda (options)
	  (when options
	    (method-combination-error
	     "The standard method combination accepts no options."))
	  instance))
  (setf (gethash nil (method-combination-type-%cache class)) instance)
  (setf (gethash 'standard **method-combination-types**) class))

(defmethod compute-effective-method
    ((function generic-function)
     (combination standard-standard-method-combination)
     applicable-methods)
  (standard-compute-effective-method function combination applicable-methods))


;; ------------------------------------
;; Built-in (short) method combinations
;; ------------------------------------

(with-unlocked-packages (cl)
  (define-method-combination +      :identity-with-one-argument t)
  (define-method-combination and    :identity-with-one-argument t)
  (define-method-combination append :identity-with-one-argument nil)
  (define-method-combination list   :identity-with-one-argument nil)
  (define-method-combination max    :identity-with-one-argument t)
  (define-method-combination min    :identity-with-one-argument t)
  (define-method-combination nconc  :identity-with-one-argument t)
  (define-method-combination or     :identity-with-one-argument t)
  (define-method-combination progn  :identity-with-one-argument t))



;; ========================
;; Infrastructure Injection
;; ========================

;; At that point, we have recreated everything, and the new infrastructure
;; exists in parallel with the old one which is still in use. We now need to
;; switch to the new infrastructure, which means populating caches and
;; updating all the existing generic functions. This is extremely dangerous
;; because the code doing it involves generic calls, which in turn could
;; trigger the computation of effective methods, which in turn would run the
;; method combination infrastructure that we're precisely in the process of
;; transferring...


;; -------------------------------------
;; Caches and generic functions updating
;; -------------------------------------

(defun transfer-cache-and-update-generic-functions (new old)
  (setf (slot-value new '%generic-functions)
	;; This is still the old architecture, so the old method! Also, the
	;; global variable below still contains the old value so it's quicker
	;; to access it like that than going through the info structure's
	;; cache.
	(method-combination-%generic-functions old))
  (maphash (lambda (gf ignore)
	     (declare (ignore ignore))
	     (setf (generic-function-method-combination gf) new))
	   ;; This, on the other hand, is the new method.
	   (method-combination-%generic-functions new)))


;; Standard method combination
;; Remember that we have already created the only instance of the standard
;; method combination. So we just need to repopulate its cache and update the
;; concerned generic functions.
(let ((combination
	(gethash nil (method-combination-type-%cache
		      (find-method-combination-type 'standard)))))
  (transfer-cache-and-update-generic-functions
   combination *standard-method-combination*)
  (setq *standard-method-combination* combination))

;; If we reach that point, we're mostly safe.

;; Built-in method combinations
(dolist (name '(+ and append list max min nconc or progn))
  (let ((type (find-method-combination-type name)))
    (dolist (entry (method-combination-info-cache
		    (gethash name **method-combinations**)))
      (let ((combination
	      (funcall (method-combination-%constructor type) (car entry))))
	(setf (gethash (car entry) (method-combination-type-%cache type))
	      combination)
	(transfer-cache-and-update-generic-functions
	 combination (cdr entry))))))


;; ------------------
;; Late redefinitions
;; ------------------

;; At that point, the new infrastructure is in place. We still have a number
;; of redefinitions to perform.

(defun method-combination-p (object)
  (typep object 'method-combination))

(defun short-method-combination-p (object)
  (typep object 'short-method-combination))

(defun long-method-combination-p (object)
  (typep object 'long-method-combination))

(defmethod find-method-combination
    ((generic-function generic-function) name options)
  (let ((type (find-method-combination-type name nil)))
    (when type
      (or (gethash options (method-combination-type-%cache type))
	  (setf (gethash options (method-combination-type-%cache type))
		(funcall (method-combination-%constructor type)
		  options))))))

;; This comes from early generic functions fixups, only because
;; NORMALIZE-OPTIONS specialized on EARLY-METHOD-COMBINATION. It restores
;; proper use of ENSURE-GENERIC-FUNCTION when a method combination object is
;; passed. Otherwise, specifying a method combination name / options (as done
;; by DEFGENERIC) didn't break.
(labels ((resolve-class (context class-or-name environment)
	   (cond ((symbolp class-or-name)
		  (find-class class-or-name t environment))
		 ((classp class-or-name)
		  class-or-name)
		 (t
		  (error "~@<The ~A (~S) was neither a class nor a ~
			  symbol that names a class.~@:>"
			 context class-or-name))))
	 (resolve-and-finalize-class (class-or-name environment)
	   (let ((class (resolve-class ":GENERIC-FUNCTION-CLASS argument"
				       class-or-name environment)))
	     (if (class-has-a-forward-referenced-superclass-p class)
		 ;; FIXME: reference MOP documentation -- this is an
		 ;; additional requirement on our users
		 (error "~@<The generic function class ~A is not ~
			 finalizeable~@:>"
			class)
		 (ensure-class-finalized class))))
	 (normalize-options (&rest options &key
				   environment
				   (lambda-list nil lambda-list-p)
				   (generic-function-class 'standard-generic-function)
				   &allow-other-keys)
	   (let ((class (resolve-and-finalize-class
			 generic-function-class environment)))
	     (collect ((initargs))
	       (doplist (key value) options
		 (case key
		   ((:environment :generic-function-class))
		   (:method-combination
		    (initargs
		     key
		     (etypecase value
		       (cons
			(destructuring-bind (type . options) value
			  (find-method-combination
			   (class-prototype class) type options)))
		       (method-combination
			value))))
		   (:method-class
		    (initargs key (resolve-class ":METHOD-CLASS argument"
						 value environment)))
		   (t
		    (initargs key value))))
	       (values class lambda-list lambda-list-p (initargs))))))

  (defmethod ensure-generic-function-using-class
      ((existing generic-function) fun-name
       &rest options &key &allow-other-keys)
    (multiple-value-bind
	  (generic-function-class lambda-list lambda-list-p initargs)
	(apply #'normalize-options options)
      (unless (eq (class-of existing) generic-function-class)
	(change-class existing generic-function-class))
      (prog1
	  (apply #'reinitialize-instance existing initargs)
	(note-gf-signature fun-name lambda-list-p lambda-list))))

  (defmethod ensure-generic-function-using-class
      ((existing null) fun-name &rest options &key &allow-other-keys)
    (declare (ignore existing))
    (multiple-value-bind
	  (generic-function-class lambda-list lambda-list-p initargs)
	(apply #'normalize-options options)
      (prog1
	  (setf (gdefinition fun-name)
		(apply #'make-instance generic-function-class
		       :name fun-name initargs))
	(note-gf-signature fun-name lambda-list-p lambda-list)))))

(sb-ext:with-unlocked-packages (sb-mop)
  (let ((sb-mop (find-package :sb-mop))
	(symbols '(update-generic-function-for-redefined-method-combination
		   find-method-combination-type
		   short-method-combination long-method-combination)))
    (import symbols sb-mop)
    (export symbols sb-mop)))

;;; sb-pcl.lisp ends here
