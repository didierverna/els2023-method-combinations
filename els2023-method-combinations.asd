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
  :if-feature :sbcl
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "medium")
	       (:file "alternative")))
