(defgeneric short-gf (a)
  (:method-combination progn)
  (:method progn ((a number)) (print 'number))
  (:method progn ((a integer)) (print 'integer))
  (:method progn ((a fixnum)) (print 'fixnum))
  (:method progn ((a float)) (print 'float)))

(let ((method-combination
	(sb-mop:generic-function-method-combination #'short-gf))
      (applicable-methods
	(list (find-method #'short-gf '(progn) (list (find-class 'number)))
	      (find-method #'short-gf '(progn) (list (find-class 'integer)))
	      (find-method #'short-gf '(progn) (list (find-class 'fixnum))))))
  (time (loop :repeat 10000000
	      :do (sb-mop:compute-effective-method
		   #'short-gf method-combination applicable-methods))))


(define-method-combination long-progn (&optional (order :most-specific-first))
  ((around (:around))
   (before (:before))
   (primary (progn) :order order :required t)
   (after (:after)))
  (flet ((call-methods (methods)
	   (mapcar (lambda (method) `(call-method ,method)) methods)))
    (let* ((primary-form (if (rest primary)
			   `(progn ,@(call-methods primary))
			   `(call-method ,(first primary))))
	   (form (if (or before after)
		   `(multiple-value-prog1
			(progn ,@(call-methods before) ,primary-form)
		      ,@(call-methods (reverse after)))
		   primary-form)))
      (if around
	`(call-method
	  ,(first around) (,@(rest around) (make-method ,form)))
	form))))

(defgeneric long-gf (a)
  (:method-combination long-progn)
  (:method progn ((a number)) (print 'number))
  (:method progn ((a integer)) (print 'integer))
  (:method progn ((a fixnum)) (print 'fixnum))
  (:method progn ((a float)) (print 'float)))

(let ((method-combination
	(sb-mop:generic-function-method-combination #'long-gf))
      (applicable-methods
	(list (find-method #'long-gf '(progn) (list (find-class 'number)))
	      (find-method #'long-gf '(progn) (list (find-class 'integer)))
	      (find-method #'long-gf '(progn) (list (find-class 'fixnum))))))
  (time (loop :repeat 10000000
	      :do (sb-mop:compute-effective-method
		   #'long-gf method-combination applicable-methods))))
