(defpackage :els2023-method-combinations
  (:use :cl)
  (:import-from :sb-mop
    :funcallable-standard-class
    :generic-function-method-combination)
  (:import-from :sb-pcl
    :find-method-combination-type
    :long-method-combination
    :long-method-combination-type
    :update-generic-function-for-redefined-method-combination)
  (:export :find-method-combination* :change-method-combination
	   :define-medium-method-combination-type
	   :generic-function! :generic-function!-p :defgeneric!
	   :call-with-combination :call/cb :install-#!-reader-macro))
