(in-package :cl-symbolic-math)

;;;; Execute symbolic math trees. Or even better, compile them to closures.

;;; keep track of special symbols/operators
(defparameter *constants* nil)
(defparameter *variate-operators* nil)

(def! symbolic-compile nil
  (constantly nil))

;;; constants

(defmacro define-symbolic-constant (symbolic-constant value)
  `(progn
     (push ',symbolic-constant *constants*)
     (def symbolic-compile ,symbolic-constant
       (constantly ,value))))

(define-symbolic-constant pi pi)

(define-symbolic-constant e (exp 1))

;;; terminals

(def symbolic-compile _x
  (where (symbolp _x))
  (lambda (b)
    (if-let (bg (assoc _x b))
	    (cdr bg)
	    (error "Binding ~a not found." _x))))

(def symbolic-compile _n
  (where (numberp _n))
  (constantly _n))

;;; basic arithmetic
(defmacro define-unary-operator (symbolic-op real-op)
  "Define mapping from symbolic-op unary operator to real-op unary operator."
  (with-unique-names (b a)
    `(def symbolic-compile (,symbolic-op _a)
       (let ((,a (symbolic-compile _a)))
	 (lambda (,b)
	   (,real-op (funcall ,a ,b)))))))

(defmacro define-binary-operator (symbolic-op real-op)
  "Define mapping from symbolic-op binary operator to real-op binary operator."
  (with-unique-names (b l r)
   `(def symbolic-compile (,symbolic-op _a _b)
      (let ((,l (symbolic-compile _a))
	    (,r (symbolic-compile _b)))
       (lambda (,b)
	 (,real-op (funcall ,l ,b)
		   (funcall ,r ,b)))))))

(defmacro define-variate-operator (symbolic-op real-op)
  "Define expander for multivariate operator to nested binary operators"
  (with-unique-names (b l r)
    `(progn
       (push ',symbolic-op *variate-operators*)
       (def symbolic-compile (,symbolic-op _a . _rest)
	(where (> (length _rest) 1))
	(let ((,l (symbolic-compile _a))
	      (,r (symbolic-compile (cons ',symbolic-op _rest))))
	  (lambda (,b)
	    (,real-op (funcall ,l ,b)
		      (funcall ,r ,b))))))))

(define-unary-operator - -)

(define-binary-operator + +)
(define-binary-operator - -)
(define-binary-operator * *)
(define-binary-operator / /)

(define-variate-operator + +)
(define-variate-operator - -)
(define-variate-operator * *)
(define-variate-operator / /)

;;; some basic functions

(defmacro define-unary-function (symbolic-op unary-function)
  "Map symbolic-op to arbitrary unary function"
  (with-unique-names (b a f)
    `(def symbolic-compile (,symbolic-op _a)
       (let ((,a (symbolic-compile _a))
	     (,f ,unary-function))
	 (lambda (,b)
	   (funcall ,f
		    (funcall ,a ,b)))))))

(define-unary-operator sin sin)
(define-unary-operator cos cos)
(define-unary-operator tan tan)
(define-unary-function ctg (lambda (x)
			     (/ 1 (tan x))))

(define-unary-operator asin asin)
(define-unary-operator acos acos)
(define-unary-operator atan atan)
(define-unary-function actg (lambda (x)
			      (atan (/ 1 x))))

(define-binary-operator ^ expt)
(define-unary-operator exp exp)

;;; failure

(def symbolic-compile _
  (error "Unknown symbolic math operation."))
