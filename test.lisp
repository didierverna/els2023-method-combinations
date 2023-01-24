(defpackage :els2023-method-combinations/test
  (:use :cl :els2023-method-combinations :lisp-unit)
  (:export :test))

(in-package :els2023-method-combinations/test)

;; #### NOTE: calls to FMAKUNBOUND are here because consecutive calls to
;; DEFGENERIC[!] may preserve some state (e.g. updated method combinations,
;; added methods, etc.).


(define-test basic
  (fmakunbound 'gf)
  (defgeneric! gf (i)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  (assert-equal  6 (gf 5))
  (assert-equal 11 (call/cb :+ gf 5))
  (assert-equal 30 (call/cb :* gf 5))
  ;; Make sure previous calls are undisturbed.
  (assert-equal 11 (call/cb :+ gf 5))
  (assert-equal  6 (gf 5))

  (assert-equal 5.0 (gf 5.0))
  (assert-equal 5.0 (call/cb :+ gf 5.0))
  (assert-equal 5.0 (call/cb :* gf 5.0))
  ;; Make sure previous calls are undisturbed.
  (assert-equal 5.0 (call/cb :+ gf 5.0))
  (assert-equal 5.0 (gf 5.0)))


(define-test change-main-combination
  (fmakunbound 'gf)
  (defgeneric! gf (i)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  ;; Make sure the generic function is called a couple of times.
  (assert-equal  6   (gf 5))
  (assert-equal  5.0 (gf 5.0))
  (assert-equal 11   (call/cb :+ gf 5))
  (assert-equal  5.0 (call/cb :+ gf 5.0))

  (change-method-combination gf :min)
  (assert-equal 5 (gf 5))
  (assert-equal 5.0 (gf 5.0))

  (change-method-combination gf :max)
  (assert-equal 6 (gf 5))
  (assert-equal 5.0 (gf 5.0))

  ;; Make sure previous calls are undisturbed.
  (assert-equal 11   (call/cb :+ gf 5))
  (assert-equal  5.0 (call/cb :+ gf 5.0)))


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

  (assert-equal  1   (gf1 5))
  (assert-equal -5.0 (gf1 5.0))
  (assert-equal -1   (gf2 5))
  (assert-equal -5.0 (gf2 5.0))

  (assert-equal 11   (call/cb :+ gf1 5))
  (assert-equal  5.0 (call/cb :+ gf1 5.0))
  (assert-equal 11   (call/cb :+ gf2 5))
  (assert-equal  5.0 (call/cb :+ gf2 5.0))

  (define-medium-method-combination-type -!
    :operator - :identity-with-one-argument t)

  (assert-equal  1   (gf1 5))
  (assert-equal  5.0 (gf1 5.0))
  (assert-equal -1   (gf2 5))
  (assert-equal  5.0 (gf2 5.0))

  ;; Make sure previous calls are undisturbed.
  (assert-equal 11   (call/cb :+ gf1 5))
  (assert-equal  5.0 (call/cb :+ gf1 5.0))
  (assert-equal 11   (call/cb :+ gf2 5))
  (assert-equal  5.0 (call/cb :+ gf2 5.0)))


(define-test update-alternative-combination
  (define-medium-method-combination-type -! :operator -)

  (fmakunbound 'gf)
  (defgeneric! gf (i)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  ;; Make sure the generic function is called a couple of times.
  (assert-equal 6 (gf 5))
  (assert-equal 5.0 (gf 5.0))

  (assert-equal  1   (call/cb -! gf 5))
  (assert-equal -5.0 (call/cb -! gf 5.0))
  (assert-equal -1   (call/cb (-! :most-specific-last) gf 5))
  (assert-equal -5.0 (call/cb (-! :most-specific-last) gf 5.0))

  ;; Make sure previous calls are undisturbed.
  (assert-equal 6 (gf 5))
  (assert-equal 5.0 (gf 5.0))

  (define-medium-method-combination-type -!
    :operator - :identity-with-one-argument t)

  (assert-equal  1   (call/cb -! gf 5))
  (assert-equal  5.0 (call/cb -! gf 5.0))
  (assert-equal -1   (call/cb (-! :most-specific-last) gf 5))
  (assert-equal  5.0 (call/cb (-! :most-specific-last) gf 5.0))

  ;; Make sure previous calls are still undisturbed.
  (assert-equal 6   (gf 5))
  (assert-equal 5.0 (gf 5.0)))


(define-test add-method
  (fmakunbound 'gf)
  (defgeneric! gf (i)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  ;; Make sure the generic function is called a couple of times.
  (assert-equal  6   (gf 5))
  (assert-equal  5.0 (gf 5.0))
  (assert-equal 11   (call/cb :+ gf 5))
  (assert-equal  5.0 (call/cb :+ gf 5.0))

  (defmethod gf ((i fixnum)) (+ i 2))

  (assert-equal  7   (gf 5))
  (assert-equal  5.0 (gf 5.0))
  (assert-equal 18   (call/cb :+ gf 5))
  (assert-equal  5.0 (call/cb :+ gf 5.0)))


(define-test remove-method
  (fmakunbound 'gf)
  (defgeneric! gf (i)
    (:method ((i number)) i)
    (:method ((i integer)) (1+ i)))

  ;; Make sure the generic function is called a couple of times.
  (assert-equal  6   (gf 5))
  (assert-equal  5.0 (gf 5.0))
  (assert-equal 11   (call/cb :+ gf 5))
  (assert-equal  5.0 (call/cb :+ gf 5.0))

  (remove-method #'gf (find-method #'gf nil '(integer)))

  (assert-equal 5   (gf 5))
  (assert-equal 5.0 (gf 5.0))
  (assert-equal 5   (call/cb :+ gf 5))
  (assert-equal 5.0 (call/cb :+ gf 5.0)))


(defun test ()
  (let ((lisp-unit:*print-failures* t))
    (lisp-unit:run-tests :all :els2023-method-combinations/test)))
