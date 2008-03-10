(in-package :cl-symbolic-math)

;;;; Perform symbolic derivation on math trees.

(defparameter *derivation-variable* nil)

(def! symbolic-derive nil
  nil)

(def symbolic-derive _x
  (where (symbolp _x))
  (if (eql *derivation-variable* _x)
      1
      _x))

(def symbolic-derive _x
  (where (or (numberp _x)
	     (member _x *constants*)))
  0)

(def symbolic-derive (+ _a _b)
  (let ((a (symbolic-derive)))))