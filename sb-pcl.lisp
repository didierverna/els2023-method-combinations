;;; sb-pcl.lisp --- Method combination infrastructure reimplementation

;; #### WARNING: this code is meant to be loaded in a specific version of SBCL
;; in which some standard and implementation-specific things related to method
;; combinations have been renamed to get out of the way. This is necessary
;; because the MOP forbids the redefinition of some classes of class
;; meta-objects, which is exactly what we want to do here.

(in-package :sb-pcl)


(defgeneric update-generic-function-for-redefined-method-combination
    (function combination)
  (:documentation
   "Inform generic FUNCTION that its method COMBINATION was redefined.")
  (:method ((function generic-function) combination)
    "Flush the effective method cache and reinitialize FUNCTION."
    ;; This is just what SBCL does.
    (sb-pcl::flush-effective-method-cache function)
    (reinitialize-instance function)))


;; =========================
;; Infrastructure Definition
;; =========================

;; Replacement for **method-combinations**.
(defvar **method-combination-types** (make-hash-table :test 'eq)
  "The global method combination types hash table.
This hash table maps names to method combination types.")


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
   (lambda-list :initform nil :initarg :lambda-list)
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

;; Validate the creation of subclasses of METHOD-COMBINATION implemented as
;; METHOD-COMBINATION-TYPE.
(defmethod validate-superclass
    ((class method-combination-type) (superclass standard-class))
  t)


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

;; #### NOTE: SBCL normally has the equivalent of this class instantiated
;; once, and also serving as a base class for other method combinations, but I
;; don't like that. I prefer to make the standard method combination behave as
;; the other ones, in other words, to have its own class implemented as a
;; method-combination-type.
(defclass standard-method-combination (method-combination)
  ((options :initform nil
	    :initarg :options
	    :reader method-combination-options)
   (%generic-functions :initform (sb-pcl::make-gf-hashset)
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

;; Replacement for both load-short-defcombin and short-combine-methods.
(defun load-short-defcombin
    (name operator identity-with-one-argument documentation source-location)
  ;; #### NOTE: we can't change-class class metaobjects, so we need to
  ;; recreate a brand new one.
  (let ((old (gethash name **method-combination-types**))
	(new (make-instance 'short-method-combination-type
	       'source source-location
	       :direct-superclasses
	       (list (find-class 'short-method-combination))
	       :documentation documentation
	       :type-name name
	       :operator operator
	       :identity-with-one-argument identity-with-one-argument)))
    (setf (slot-value new '%constructor)
	  (lambda (options)
	    (funcall #'make-instance
	      new :options (or options '(:most-specific-first)))))
    (when old
      (setf (slot-value new '%cache) (method-combination-type-%cache old))
      (maphash (lambda (options combination)
		 (declare (ignore options))
		 (change-class combination new))
	       (method-combination-type-%cache new)))
    (setf (gethash name **method-combination-types**) new))
  (setf (random-documentation name 'method-combination) documentation)
  name)


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

(defun load-long-defcombin
    (name documentation function lambda-list args-lambda-list source-location)
  ;; #### NOTE: we can't change-class class metaobjects, so we need to
  ;; recreate a brand new one.
  (let ((old (gethash name **method-combination-types**))
	(new (make-instance 'long-method-combination-type
	       'source source-location
	       :direct-superclasses
	       (list (find-class 'long-method-combination))
	       :documentation documentation
	       :type-name name
	       :lambda-list lambda-list
	       :args-lambda-list args-lambda-list
	       :function function)))
    (setf (slot-value new '%constructor)
	  (lambda (options) (funcall #'make-instance new :options options)))
    (when old
      (setf (slot-value new '%cache) (method-combination-type-%cache old))
      (maphash (lambda (options combination)
		 (declare (ignore options))
		 (change-class combination new))
	       (method-combination-type-%cache new)))
    (setf (gethash name **method-combination-types**) new))
  (setf (random-documentation name 'method-combination) documentation)
  name)



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
		      (gethash 'standard **method-combination-types**)))))
  (transfer-cache-and-update-generic-functions
   combination *standard-method-combination*)
  (setq *standard-method-combination* combination))

;; If we reach that point, we're mostly safe.

;; Built-in method combinations
(dolist (name '(+ and append list max min nconc or progn))
  (let ((type (gethash name **method-combination-types**)))
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
  (let ((type (gethash name **method-combination-types**)))
    (when type
      (or (gethash options (method-combination-type-%cache type))
	  (setf (gethash options (method-combination-type-%cache type))
		(funcall (method-combination-%constructor type)
		  options))))))

;;; sb-pcl.lisp ends here
