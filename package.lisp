(defpackage :els2023-method-combinations
  (:use :cl #+sbcl :sb-mop #+sbcl :sb-pcl #+ecl :clos #+abcl :mop)
  (:import-from #+sbcl :sb-pcl #+ecl :clos #+abcl :mop
		:find-method-combination-instance
		:find-method-combination-type
		:method-combination-%constructor
		:method-combination-%generic-functions
		:method-combination-type-%instances)
  (:export :find-method-combination-instance :change-method-combination
	   :define-medium-method-combination-type
	   :generic-function! :generic-function!-p :defgeneric!
	   :call-with-combination :call/cb :install-#!-reader-macro))
