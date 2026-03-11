(in-package :els2023-method-combinations)

(defmacro change-method-combination (function &rest combination)
  "Change generic FUNCTION to a new method COMBINATION.
- FUNCTION is a generic function designator.
- COMBINATION is a method combination type name, potentially followed by
arguments."
  (when (symbolp function) (setq function `(function ,function)))
  `(reinitialize-instance ,function
     :method-combination
     (find-method-combination-instance
      ',(car combination) ',(cdr combination))))
