(in-package :cl-symbolic-math)

;;;; Top level operations and wrappers

(defun execute-expression (math-expression &optional (variable-bindings nil))
  (let ((s-variable-bindings (if (and variable-bindings
				      (not (consp (car variable-bindings))))
				 (plist-alist variable-bindings)
				 variable-bindings)))
    (funcall (symbolic-compile math-expression) s-variable-bindings)))