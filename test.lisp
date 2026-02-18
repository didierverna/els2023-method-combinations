(defpackage :els2023-method-combinations/test
  (:use :cl #+sbcl :sb-mop #+sbcl :sb-pcl #+ecl :clos
	:els2023-method-combinations :lisp-unit)
  (:export :test))

(in-package :els2023-method-combinations/test)

;; #### NOTE: calls to FMAKUNBOUND are here because consecutive calls to
;; DEFGENERIC[!] may preserve some state (e.g. updated method combinations,
;; added methods, etc.).

(define-test standard

  (assert-true (find-method-combination-type 'standard))
  (assert-true (find-method-combination* 'standard))
  (assert-error 'error (find-method-combination* 'standard '(:dummy) nil))
  (assert-true (typep (find-method-combination* 'standard)
		      'standard-method-combination))

  (assert-true (find-method-combination-type 'or))
  (assert-true (find-method-combination* 'or))
  (assert-true (find-method-combination* 'or '(:most-specific-first)))
  (assert-true (find-method-combination* 'or '(:most-specific-last)))
  ;; #### NOTE: SBCL checks the validity of the arguments. ECL doesn't.
  ;; (assert-error 'error (find-method-combination* 'or '(:dummy)))
  (assert-true (typep (find-method-combination* 'or)
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


(defun test ()
  (let ((lisp-unit:*print-failures* t))
    (lisp-unit:run-tests :all :els2023-method-combinations/test)))
