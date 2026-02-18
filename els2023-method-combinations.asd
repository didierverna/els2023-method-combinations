(asdf:defsystem :els2023-method-combinations
  :long-name "ELS 2023 Method Combination Experimentations"
  :description "Companion code for my ELS 2023 paper."
  :long-description "\
This code accompanies my ELS 2023 paper about method combination types.
Among other things, it re-implements my ELS 2018 alternative combinators
idea."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :source-control "https://github.com/didierverna/els2023-method-combinations"
  :if-feature (:or :sbcl :ecl)
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "medium")
	       (:file "alternative")))

(asdf:defsystem :els2023-method-combinations/test
  :depends-on (:els2023-method-combinations :lisp-unit)
  :components ((:file "test")))

(asdf:defsystem :els2023-method-combinations/funcall-bench
  :depends-on (:els2023-method-combinations)
  :components ((:file "funcall-bench")))

(asdf:defsystem :els2023-method-combinations/cem-bench
  :components ((:file "cem-bench")))
