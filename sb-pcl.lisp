;;; sb-pcl.lisp --- Method combination infrastructure reimplementation

;; #### WARNING: this code is meant to be loaded in a specific version of SBCL
;; in which some standard and implementation-specific things related to method
;; combinations have been renamed to get out of the way. This is necessary
;; because the MOP forbids the redefinition of some classes of class
;; meta-objects, which is exactly what we want to do here.

(in-package :sb-pcl)


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

(defclass method-combination-type (standard-class)
  ((lambda-list :initform nil)
   ;; A reader without "type" in the name seems more readable to me.
   (%constructor :initarg :constructor :reader method-combination-%constructor)
   (%cache :initform (make-hash-table :test #'equal)
	   :reader method-combination-type-%cache))
  (:documentation "Meta-class for method combination types.
It is the base class for short and long method combination types meta-classes.
This only class directly implemented as this class is the standard method
combination class."))

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


;; -------------------
;; Method combinations
;; -------------------

(with-unlocked-packages (cl)
  (defclass method-combination (metaobject)
    ()
    (:documentation "Base class for method combinations.")))

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
  (class-name (class-of combination)))  

(defmethod print-object ((combination standard-method-combination) stream)
  (print-unreadable-object (combination stream :type t :identity t)
    (format stream "~S ~:S"
      (slot-value-for-printing (class-of combination) 'name)
      (slot-value-for-printing combination 'options))))


;; Short method combinations
;; -------------------------

(defclass short-method-combination (standard-method-combination)
  ()
  (:documentation "Base class for short method combinations.
Short method combinations can only have two instances: one for each method
order."))

(defmethod initialize-instance :before
    ((instance short-method-combination)
     &key options &allow-other-keys
     &aux (name (class-name (class-of (class-of instance)))))
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

(defun load-short-defcombin
    (name operator identity-with-one-argument documentation source-location)
  (let ((class (gethash name **method-combination-types**)))
    (cond (class ) ;; #### FIXME: implement redefinition.
	  (t
	   (setq class
		 (make-instance 'short-method-combination-type
		   'source source-location
		   :name name
		   :direct-superclasses
		   (list (find-class 'short-method-combination))
		   :documentation documentation
		   :constructor
		   (lambda (options)
		     (funcall #'make-instance
		       class :options (or options '(:most-specific-first))))
		   :operator operator
		   :identity-with-one-argument identity-with-one-argument))
	   (setf (gethash name **method-combination-types**) class))))
  (setf (random-documentation name 'method-combination) documentation)
  name)



;; =========================
;; Infrastructure Population
;; =========================

;; ---------------------------
;; Standard method combination
;; ---------------------------

;; This one is created by hand and globally registered because we need to
;; specialize methods on it.
(defclass standard-standard-method-combination (standard-method-combination)
  ()
  (:metaclass method-combination-type)
  (:documentation "The standard (standard) method combination class.
This class is a singleton class: the only instance is that of the standard
method combination."))

(let* ((class (find-class 'standard-standard-method-combination))
       (instance (make-instance class)))
  (setf (slot-value class 'name) 'standard)
  (setf (slot-value class '%constructor)
	(lambda (options)
	  (when options
	    (method-combination-error
	     "The standard method combination accepts no options."))
	  *standard-method-combination*))
  (setf (gethash nil (method-combination-type-%cache class)) instance)
  (setf (gethash 'standard **method-combination-types**) class))

(defmethod compute-effective-method
  ((generic-function generic-function)
   (method-combination standard-standard-method-combination)
   applicable-methods)
  (standard-compute-effective-method
   generic-function method-combination applicable-methods))




;; ============================
;; New Infrastructure Injection
;; ============================

;; ---------------------------
;; Standard method combination
;; ---------------------------

(let ((instance (gethash nil
			 (method-combination-type-%cache
			  (gethash 'standard **method-combination-types**)))))
  (setf (slot-value instance '%generic-functions)
	;; This is still the old architecture, so the old method !
	(method-combination-%generic-functions *standard-method-combination*))
  (maphash (lambda (gf ignore)
	     (declare (ignore ignore))
	     (setf (generic-function-method-combination gf) instance))
	   ;; This, on the other hand, is the new method.
	   (method-combination-%generic-functions instance))
  (setq *standard-method-combination* instance))

;; #### WARNING: until now, we were still running on the old infrastructure,
;; which was ok for adding new classes and methods and stuff. Now we still
;; have a couple of things to rewrite.

(defun method-combination-p (object) (typep object 'method-combination))



#+()(defmethod find-method-combination
    ((generic-function generic-function) name options)
  (let ((type (gethash name **method-combination-types**)))
    (when type
      (or (gethash options (method-combination-type-cache type))
	  (setf (gethash options (method-combination-type-cache type))
		(funcall (method-combination-type-constructor type)
		  options))))))

#+()(defmethod initialize-instance :before
    ((instance short-method-combination) &key order &allow-other-keys)
  (unless (member order '(:most-specific-first :most-specific-last))
    (sb-pcl::method-combination-error
     "Illegal ORDER option to the ~S short method combination.~%~
      ORDER must be either :MOST-SPECIFIC-FIRST or :MOST-SPECIFIC-LAST."
     (method-combination-type-name (class-of (class-of instance))))))

;;; sb-pcl.lisp ends here
