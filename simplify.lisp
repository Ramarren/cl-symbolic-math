(in-package :cl-symbolic-math)

;;;; Simplify symbolic math trees.

(defun unfold-variates (math-tree)
  (if (listp math-tree)
      (if (and (member (car math-tree) *variate-operators*)
	       (> (length math-tree) 3))
	  (list (car math-tree)
		(unfold-variates (cadr math-tree))
		(unfold-variates (cons (car math-tree) (cddr math-tree))))
	  (cons (car math-tree)
		(mapcar #'unfold-variates (cdr math-tree))))
      math-tree))

(defun fold-variates (math-tree)
  (match math-tree
    ((_sym-op . _args)
     (where (member _sym-op *variate-operators*))
     (cons _sym-op
	   (mapcan #'(lambda (math-subtree)
		       (if (and (listp math-subtree)
				(eql (car math-subtree) _sym-op))
			   (cdr math-subtree)
			   (list math-subtree)))
		   (mapcar #'fold-variates _args))))
    ((_sym-op . _args)
     (cons _sym-op (mapcar #'fold-variates _args)))
    (_x _x)))

(defun eliminate-trivial-operations (math-tree &optional (cleanup-p nil))
  (match math-tree
    ;;group identities
    ((+ _x 0)
     _x)
    ((+ 0 _x)
     _x)
    ((- 0 _x)
     (- _x))
    ((- _x 0)
     _x)
    ((* 1 _x)
     _x)
    ((* _x 1)
     _x)
    ;;group zeroes
    ((* _ 0)
     0)
    ((* 0 _)
     0)
    ((/ 0 _)
     0)
    ((_sym-op . _args)
     (where-not cleanup-p)
     (eliminate-trivial-operations (cons _sym-op
					 (mapcar #'eliminate-trivial-operations _args))
				   t))
    (_x _x)))

(defparameter *unwrap-constants-when-performing* nil)

(defun perform-purely-numeric-expressions (math-tree)
  (match math-tree
    ((_sym-op . _args)
     (let ((p-args (mapcar #'perform-purely-numeric-expressions _args)))
       (if (every #'numberp p-args)
	   (execute-expression (cons _sym-op p-args))
	   (eliminate-trivial-operations (cons _sym-op p-args)))))
    (_x (where (and (member _x *constants*)
		    *unwrap-constants-when-performing*))
	(execute-expression _x))
    (_x _x)))
