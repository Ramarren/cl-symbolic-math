(asdf:defsystem cl-symbolic-math
  :version "0"
  :description "My very own symbolic math system"
  :maintainer " <ramarren@cignet.higersbergernet>"
  :author " <ramarren@cignet.higersbergernet>"
  :licence "GPLv3"
  :depends-on (#:alexandria #:bpm)
  :components ((:file "package")
	       (:file "executor" :depends-on ("package"))
	       (:file "operations" :depends-on ("package" "executor"))
	       (:file "simplify" :depends-on ("package" "operations"))
	       (:file "derivate" :depends-on ("package")))
  :long-description "System for simple symbolic math manipulation, for when one does not need an entire Maxima.")

