(in-package :els2023-method-combinations)


;; ==============================
;; Medium Method Combination Type
;; ==============================

(defclass medium-method-combination-type (long-method-combination-type)
  ((operator :initarg :operator :reader operator)
   (identity-with-one-argument :initarg :identity-with-one-argument
			       :reader identity-with-one-argument))
  (:documentation "The Medium Method Combination Type class.
A medium method combination resembles a short one, with the following
differences:
- the primary methods must not be qualified,
- :before and :after methods are available."))

(defmethod print-object ((type medium-method-combination-type) stream)
  (print-unreadable-object (type stream :type t :identity t)
    (format stream "~S (~S ~S)"
      (method-combination-type-name type)
      (operator type)
      (identity-with-one-argument type))))

(defmacro define-medium-method-combination-type
    (name &key documentation (operator name) identity-with-one-argument)
  "Define NAME as a medium method combination type."
  (let ((documentation (when documentation (list documentation)))
	(single-method-call (if identity-with-one-argument
			      '`(call-method ,(first primary))
			      ``(,',operator (call-method ,(first primary))))))
    `(define-method-combination ,name (&optional (order :most-specific-first))
       ((around (:around))
	(before (:before))
	(primary () :order order :required t)
	(after (:after)))
       (:method-combination-type-class medium-method-combination-type
	:operator ,operator
	:identity-with-one-argument ,identity-with-one-argument)
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




;; =========================================
;; Build-In Medium Method Combinations Types
;; =========================================

(defmacro define-built-in-medium-method-combinations ()
  "Define all built-in medium method combinations.
This defines :+ :* :max :min :list :append :nconc :progn :and and :or.
Note that the standard doesn't define a * method combination, but we do."
  `(progn
     ,@(mapcar
	(lambda (name)
	  `(define-medium-method-combination-type
	       ,(intern (symbol-name name) :keyword)
	     :documentation
	     ,(format nil "The ~A built-in medium method combination."
		name)
	     :operator ,name
	     :identity-with-one-argument t))
	 '(+ * max min nconc progn and or))
     ,@(mapcar
	(lambda (name)
	  `(define-medium-method-combination-type
	       ,(intern (symbol-name name) :keyword)
	     :documentation
	     ,(format nil "The ~A built-in medium method combination."
		name)
	     :operator ,name))
	'(list append))))

(define-built-in-medium-method-combinations)
