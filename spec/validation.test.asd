; vim: ft=lisp et
(in-package :asdf)
(defsystem :validation.test
  :version "0.0.1"
  :depends-on
  (:jingoh "validation")
  :components
  ((:file "validation"))
  :perform
  (test-op (o c) (declare (special args))
   (let (*compile-verbose* *compile-print*)
     (apply #'symbol-call :jingoh :examine :validation args))))
