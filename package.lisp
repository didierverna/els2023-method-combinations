(defpackage :els2023-method-combinations
  (:use :cl)
  (:import-from #+sbcl :sb-mop #+ecl :clos
    :funcallable-standard-class
    :generic-function-method-combination)
  (:import-from #+sbcl :sb-pcl #+ecl :clos
    :standard-method-combination
    :short-method-combination
    :find-method-combination-type
    :long-method-combination
    :long-method-combination-type
    :method-combination-type-name
    :method-combination-%constructor
    :method-combination-%generic-functions
    :method-combination-type-%instances
    :update-generic-function-for-redefined-method-combination)
  (:export :standard-method-combination
	   :short-method-combination
	   :find-method-combination-type
	   :find-method-combination* :change-method-combination
	   :define-medium-method-combination-type
	   :generic-function! :generic-function!-p :defgeneric!
	   :call-with-combination :call/cb :install-#!-reader-macro))
