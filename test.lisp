(defpackage :els2023-method-combinations/test
  (:use :cl #+sbcl :sb-mop #+sbcl :sb-pcl #+ecl :clos #+abcl :mop
	:els2023-method-combinations :lisp-unit)
  (:import-from #+sbcl :sb-pcl #+ecl :clos #+abcl :mop
		:method-combination-%generic-functions)
  (:export :test))

(in-package :els2023-method-combinations/test)

;; #### NOTE: calls to FMAKUNBOUND are here because consecutive calls to
;; DEFGENERIC[!] may preserve some state (e.g. updated method combinations,
;; added methods, etc.).

(define-test standard

  (assert-true (find-method-combination-type 'standard))
  (assert-true (find-method-combination-instance 'standard))
  (assert-error 'error
    (find-method-combination-instance 'standard '(:dummy) nil))
  (assert-true (typep (find-method-combination-instance 'standard)
		      'standard-method-combination))

  (assert-true (find-method-combination-type 'or))
  (assert-true (find-method-combination-instance 'or))
  (assert-true (find-method-combination-instance 'or '(:most-specific-first)))
  (assert-true (find-method-combination-instance 'or '(:most-specific-last)))
  ;; #### NOTE: SBCL checks the validity of the arguments. ECL doesn't.
  ;; (assert-error 'error (find-method-combination-instance 'or '(:dummy)))
  (assert-true (typep (find-method-combination-instance 'or)
		      'short-method-combination))

  (assert-error 'error
    (define-method-combination dummy
      :method-combination-class long-method-combination))
  (assert-error 'error
    (define-method-combination dummy () ()
      (:method-combination-class short-method-combination))))


(define-test basic
  (fmakunbound 'gf)
  (defgeneric! gf (i)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  (assert-eql  6 (gf 5))
  (assert-eql 11 (call/cb :+ gf 5))
  (assert-eql 30 (call/cb :* gf 5))
  ;; Make sure previous calls are undisturbed.
  (assert-eql 11 (call/cb :+ gf 5))
  (assert-eql  6 (gf 5))

  (assert-eql 5.0 (gf 5.0))
  (assert-eql 5.0 (call/cb :+ gf 5.0))
  (assert-eql 5.0 (call/cb :* gf 5.0))
  ;; Make sure previous calls are undisturbed.
  (assert-eql 5.0 (call/cb :+ gf 5.0))
  (assert-eql 5.0 (gf 5.0)))

(define-test cache
  (fmakunbound 'gf)
  (defgeneric! gf (i)
    (:method-combination :or)
    (:method (i) i))
  (defparameter mbr
    #+sbcl #'sb-pcl::weak-hashset-memberp
    #-sbcl #'member)

  (assert-true (funcall mbr #'gf (method-combination-%generic-functions
				  (find-method-combination-instance :or))))

  (change-method-combination gf :and)

  (assert-true (funcall mbr #'gf (method-combination-%generic-functions
				  (find-method-combination-instance :and))))
  (assert-false (funcall mbr #'gf (method-combination-%generic-functions
				   (find-method-combination-instance :or))))

  (call/cb :progn gf 1)

  (assert-true (funcall mbr #'gf (method-combination-%generic-functions
				  (find-method-combination-instance :and))))
  (assert-true (funcall mbr #'gf (method-combination-%generic-functions
				  (find-method-combination-instance :progn))))

  (change-method-combination gf :progn)

  (assert-true (funcall mbr #'gf (method-combination-%generic-functions
				  (find-method-combination-instance :progn))))
  (assert-false (funcall mbr #'gf (method-combination-%generic-functions
				   (find-method-combination-instance :and)))))

(define-test change-main-combination
  (fmakunbound 'gf)
  (defgeneric! gf (i)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  ;; Make sure the generic function is called a couple of times.
  (assert-eql  6   (gf 5))
  (assert-eql  5.0 (gf 5.0))
  (assert-eql 11   (call/cb :+ gf 5))
  (assert-eql  5.0 (call/cb :+ gf 5.0))

  (change-method-combination gf :min)
  (assert-eql 5 (gf 5))
  (assert-eql 5.0 (gf 5.0))

  (change-method-combination gf :max)
  (assert-eql 6 (gf 5))
  (assert-eql 5.0 (gf 5.0))

  ;; Make sure previous calls are undisturbed.
  (assert-eql 11   (call/cb :+ gf 5))
  (assert-eql  5.0 (call/cb :+ gf 5.0)))


(define-test update-main-combination
  (define-medium-method-combination-type -! :operator -)

  (fmakunbound 'gf1)
  (defgeneric! gf1 (i)
    (:method-combination -!)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  (fmakunbound 'gf2)
  (defgeneric! gf2 (i)
    (:method-combination -! :most-specific-last)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  (assert-eql  1   (gf1 5))
  (assert-eql -5.0 (gf1 5.0))
  (assert-eql -1   (gf2 5))
  (assert-eql -5.0 (gf2 5.0))

  (assert-eql 11   (call/cb :+ gf1 5))
  (assert-eql  5.0 (call/cb :+ gf1 5.0))
  (assert-eql 11   (call/cb :+ gf2 5))
  (assert-eql  5.0 (call/cb :+ gf2 5.0))

  (define-medium-method-combination-type -!
    :operator - :identity-with-one-argument t)

  (assert-eql  1   (gf1 5))
  (assert-eql  5.0 (gf1 5.0))
  (assert-eql -1   (gf2 5))
  (assert-eql  5.0 (gf2 5.0))

  ;; Make sure previous calls are undisturbed.
  (assert-eql 11   (call/cb :+ gf1 5))
  (assert-eql  5.0 (call/cb :+ gf1 5.0))
  (assert-eql 11   (call/cb :+ gf2 5))
  (assert-eql  5.0 (call/cb :+ gf2 5.0)))


(define-test update-alternative-combination
  (define-medium-method-combination-type -! :operator -)

  (fmakunbound 'gf)
  (defgeneric! gf (i)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  ;; Make sure the generic function is called a couple of times.
  (assert-eql 6 (gf 5))
  (assert-eql 5.0 (gf 5.0))

  (assert-eql  1   (call/cb -! gf 5))
  (assert-eql -5.0 (call/cb -! gf 5.0))
  (assert-eql -1   (call/cb (-! :most-specific-last) gf 5))
  (assert-eql -5.0 (call/cb (-! :most-specific-last) gf 5.0))

  ;; Make sure previous calls are undisturbed.
  (assert-eql 6 (gf 5))
  (assert-eql 5.0 (gf 5.0))

  (define-medium-method-combination-type -!
    :operator - :identity-with-one-argument t)

  (assert-eql  1   (call/cb -! gf 5))
  (assert-eql  5.0 (call/cb -! gf 5.0))
  (assert-eql -1   (call/cb (-! :most-specific-last) gf 5))
  (assert-eql  5.0 (call/cb (-! :most-specific-last) gf 5.0))

  ;; Make sure previous calls are still undisturbed.
  (assert-eql 6   (gf 5))
  (assert-eql 5.0 (gf 5.0)))


(define-test add-method
  (fmakunbound 'gf)
  (defgeneric! gf (i)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  ;; Make sure the generic function is called a couple of times.
  (assert-eql  6   (gf 5))
  (assert-eql  5.0 (gf 5.0))
  (assert-eql 11   (call/cb :+ gf 5))
  (assert-eql  5.0 (call/cb :+ gf 5.0))

  (defmethod gf ((i fixnum)) (+ i 2))

  (assert-eql  7   (gf 5))
  (assert-eql  5.0 (gf 5.0))
  (assert-eql 18   (call/cb :+ gf 5))
  (assert-eql  5.0 (call/cb :+ gf 5.0)))


(define-test remove-method
  (fmakunbound 'gf)
  (defgeneric! gf (i)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  ;; Make sure the generic function is called a couple of times.
  (assert-eql  6   (gf 5))
  (assert-eql  5.0 (gf 5.0))
  (assert-eql 11   (call/cb :+ gf 5))
  (assert-eql  5.0 (call/cb :+ gf 5.0))

  (remove-method #'gf (find-method #'gf nil '(integer)))

  (assert-eql 5   (gf 5))
  (assert-eql 5.0 (gf 5.0))
  (assert-eql 5   (call/cb :+ gf 5))
  (assert-eql 5.0 (call/cb :+ gf 5.0)))

(define-test documentation
  (define-method-combination smct
    :operator progn :documentation "SMCT")
  (define-medium-method-combination-type mmct
    :operator progn :documentation "MMCT")

  (defparameter *smct* (find-method-combination-type 'smct))
  (defparameter *mmct* (find-method-combination-type 'mmct))
  (defparameter *an-smct*
    (find-method-combination-instance 'smct '(:most-specific-last)))
  (defparameter *an-mmct*
    (find-method-combination-instance 'mmct '(:most-specific-last)))

  (assert-equal (documentation 'smct 'method-combination) "SMCT")
  (assert-equal (documentation 'mmct 'method-combination) "MMCT")
  (assert-equal (documentation *smct* t) "SMCT")
  (assert-equal (documentation *mmct* t) "MMCT")
  (assert-equal (documentation *smct* 'method-combination) "SMCT")
  (assert-equal (documentation *mmct* 'method-combination) "MMCT")
  (assert-equal (documentation *an-smct* t) "SMCT")
  (assert-equal (documentation *an-mmct* t) "MMCT")
  (assert-equal (documentation *an-smct* 'method-combination) "SMCT")
  (assert-equal (documentation *an-mmct* 'method-combination) "MMCT")

  (setf (documentation 'smct 'method-combination) "SMCT usage")
  (setf (documentation 'mmct 'method-combination) "MMCT usage")

  (assert-equal (documentation 'smct 'method-combination) "SMCT usage")
  (assert-equal (documentation 'mmct 'method-combination) "MMCT usage")
  (assert-equal (documentation *smct* t) "SMCT")
  (assert-equal (documentation *mmct* t) "MMCT")
  (assert-equal (documentation *smct* 'method-combination) "SMCT usage")
  (assert-equal (documentation *mmct* 'method-combination) "MMCT usage")
  (assert-equal (documentation *an-smct* t) "SMCT")
  (assert-equal (documentation *an-mmct* t) "MMCT")
  (assert-equal (documentation *an-smct* 'method-combination) "SMCT usage")
  (assert-equal (documentation *an-mmct* 'method-combination) "MMCT usage")

  (setf (documentation *smct* t) "SMCT implementation")
  (setf (documentation *mmct* 'method-combination) "MMCT usage 1")

  (assert-equal (documentation 'smct 'method-combination) "SMCT usage")
  (assert-equal (documentation 'mmct 'method-combination) "MMCT usage 1")
  (assert-equal (documentation *smct* t) "SMCT implementation")
  (assert-equal (documentation *mmct* t) "MMCT")
  (assert-equal (documentation *smct* 'method-combination) "SMCT usage")
  (assert-equal (documentation *mmct* 'method-combination) "MMCT usage 1")
  (assert-equal (documentation *an-smct* t) "SMCT implementation")
  (assert-equal (documentation *an-mmct* t) "MMCT")
  (assert-equal (documentation *an-smct* 'method-combination) "SMCT usage")
  (assert-equal (documentation *an-mmct* 'method-combination) "MMCT usage 1")

  (setf (documentation *an-smct* t) "SMCT implementation 1")
  (setf (documentation *an-mmct* 'method-combination) "MMCT usage 2")

  (assert-equal (documentation 'smct 'method-combination) "SMCT usage")
  (assert-equal (documentation 'mmct 'method-combination) "MMCT usage 2")
  (assert-equal (documentation *smct* t) "SMCT implementation 1")
  (assert-equal (documentation *mmct* t) "MMCT")
  (assert-equal (documentation *smct* 'method-combination) "SMCT usage")
  (assert-equal (documentation *mmct* 'method-combination) "MMCT usage 2")
  (assert-equal (documentation *an-smct* t) "SMCT implementation 1")
  (assert-equal (documentation *an-mmct* t) "MMCT")
  (assert-equal (documentation *an-smct* 'method-combination) "SMCT usage")
  (assert-equal (documentation *an-mmct* 'method-combination) "MMCT usage 2")

  (setf (documentation (find-method-combination-instance 'smct
			 '(:most-specific-first))
		       t)
	"SMCT implementation 2")
  (setf (documentation (find-method-combination-instance 'mmct
			 '(:most-specific-first))
		       'method-combination)
	"MMCT usage 3")

  (assert-equal (documentation 'smct 'method-combination) "SMCT usage")
  (assert-equal (documentation 'mmct 'method-combination) "MMCT usage 3")
  (assert-equal (documentation *smct* t) "SMCT implementation 2")
  (assert-equal (documentation *mmct* t) "MMCT")
  (assert-equal (documentation *smct* 'method-combination) "SMCT usage")
  (assert-equal (documentation *mmct* 'method-combination) "MMCT usage 3")
  (assert-equal (documentation *an-smct* t) "SMCT implementation 2")
  (assert-equal (documentation *an-mmct* t) "MMCT")
  (assert-equal (documentation *an-smct* 'method-combination) "SMCT usage")
  (assert-equal (documentation *an-mmct* 'method-combination) "MMCT usage 3")


  (define-method-combination smct
    :operator progn :documentation "New SMCT")
  (define-medium-method-combination-type mmct
    :operator progn :documentation "New MMCT")

  (setq *smct* (find-method-combination-type 'smct))
  (setq *mmct* (find-method-combination-type 'mmct))
  (setq *an-smct*
	(find-method-combination-instance 'smct '(:most-specific-last)))
  (setq *an-mmct*
    (find-method-combination-instance 'mmct '(:most-specific-last)))

  (assert-equal (documentation 'smct 'method-combination) "New SMCT")
  (assert-equal (documentation 'mmct 'method-combination) "New MMCT")
  (assert-equal (documentation *smct* t) "New SMCT")
  (assert-equal (documentation *mmct* t) "New MMCT")
  (assert-equal (documentation *smct* 'method-combination) "New SMCT")
  (assert-equal (documentation *mmct* 'method-combination) "New MMCT")
  (assert-equal (documentation *an-smct* t) "New SMCT")
  (assert-equal (documentation *an-mmct* t) "New MMCT")
  (assert-equal (documentation *an-smct* 'method-combination) "New SMCT")
  (assert-equal (documentation *an-mmct* 'method-combination) "New MMCT"))

(defun test ()
  (let ((lisp-unit:*print-failures* t))
    (lisp-unit:run-tests :all :els2023-method-combinations/test)))
