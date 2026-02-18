(defpackage :els2023-method-combinations/cem-bench
  (:use :cl #+sbcl :sb-mop #+sbcl :sb-pcl #+ecl :clos)
  (:export :cem-bench))

(in-package :els2023-method-combinations/cem-bench)


(defgeneric short-gf (a)
  (:method-combination progn)
  (:method progn (a) (print t))
  (:method progn ((a number)) (print 'number))
  (:method progn ((a real)) (print 'real))
  (:method progn ((a rational)) (print 'rational))
  (:method progn ((a integer)) (print 'integer))
  (:method progn ((a fixnum)) (print 'fixnum)))

(defun cem-short-bench ()
  (let ((method-combination (generic-function-method-combination #'short-gf))
	(am1
	  (list (find-method #'short-gf '(progn) (list (find-class t)))))
	(am3
	  (list (find-method #'short-gf '(progn) (list (find-class t)))
		(find-method #'short-gf '(progn) (list (find-class 'number)))
		(find-method #'short-gf '(progn) (list (find-class 'real)))))
	(am6
	  (list (find-method #'short-gf '(progn) (list (find-class t)))
		(find-method #'short-gf '(progn) (list (find-class 'number)))
		(find-method #'short-gf '(progn) (list (find-class 'real)))
		(find-method #'short-gf '(progn) (list (find-class 'rational)))
		(find-method #'short-gf '(progn) (list (find-class 'integer)))
		(find-method #'short-gf '(progn) (list (find-class 'fixnum))))))
    (format t "** Short method combination~%")
    (format t "*** 1 applicable method~%")
    (time
     (loop :repeat 10000000
	   :do (compute-effective-method #'short-gf method-combination am1)))
    (format t "*** 3 applicable method~%")
    (time
     (loop :repeat 10000000
	   :do (compute-effective-method #'short-gf method-combination am3)))
    (format t "*** 6 applicable method~%")
    (time
     (loop :repeat 10000000
	   :do (compute-effective-method #'short-gf method-combination am6)))))


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
  (:method progn (a) (print t))
  (:method progn ((a number)) (print 'number))
  (:method progn ((a real)) (print 'real))
  (:method progn ((a rational)) (print 'rational))
  (:method progn ((a integer)) (print 'integer))
  (:method progn ((a fixnum)) (print 'fixnum)))

(defun cem-long-bench ()
  (let ((method-combination (generic-function-method-combination #'long-gf))
	(am1
	  (list (find-method #'long-gf '(progn) (list (find-class t)))))
	(am3
	  (list (find-method #'long-gf '(progn) (list (find-class t)))
		(find-method #'long-gf '(progn) (list (find-class 'number)))
		(find-method #'long-gf '(progn) (list (find-class 'real)))))
	(am6
	  (list (find-method #'long-gf '(progn) (list (find-class t)))
		(find-method #'long-gf '(progn) (list (find-class 'number)))
		(find-method #'long-gf '(progn) (list (find-class 'real)))
		(find-method #'long-gf '(progn) (list (find-class 'rational)))
		(find-method #'long-gf '(progn) (list (find-class 'integer)))
		(find-method #'long-gf '(progn) (list (find-class 'fixnum))))))
    (format t "** Long method combination~%")
    (format t "*** 1 applicable method~%")
    (time
     (loop :repeat 10000000
	   :do (compute-effective-method #'long-gf method-combination am1)))
    (format t "*** 3 applicable method~%")
    (time
     (loop :repeat 10000000
	   :do (compute-effective-method #'long-gf method-combination am3)))
    (format t "*** 6 applicable method~%")
    (time
     (loop :repeat 10000000
	   :do (compute-effective-method #'long-gf method-combination am6)))))


(defun cem-bench ()
  (cem-short-bench)
  (cem-long-bench))
