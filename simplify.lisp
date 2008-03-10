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