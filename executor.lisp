(in-package :symbolic)

;;;; Execute symbolic math trees. Or even better, compile them to closures.

(def! symbolic-compile nil
  (constantly nil))

;;; terminals

(def symbolic-compile _x
  (where (symbolp _x))
  (lambda (b)
    (assoc _x b)))

(def symbolic-compile _n
  (where (numberp _n))
  (constantly _n))

;;; basic arithmetic
(defmacro define-binary-operator (symbolic-op real-op)
  (with-unique-names (b l r)
   `(def symbolic-compile (,symbolic-op _a _b)
      (let ((,l (symbolic-compile _a))
	    (,r (symbolic-compile _b)))
       (lambda (,b)
	 (,real-op (funcall ,l ,b)
		   (funcall ,r ,b)))))))

(def symbolic-compile (+ _a _b)
  (lambda (b)
    (+ (funcall (symbolic-compile _a) b)
       (funcall (symbolic-compile _b) b))))

(def symbolic-compile (- _a _b)
  (lambda (b)
    (- (funcall (symbolic-compile _a) b)
       (funcall (symbolic-compile _b) b))))