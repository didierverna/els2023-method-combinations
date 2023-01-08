(dolist (file '("sb-pcl" "sb-introspect" "method-combinations"))
  (load (concatenate 'string file ".lisp")))
