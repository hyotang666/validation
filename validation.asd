; vim: ft=lisp et
(in-package :asdf)
(defsystem "validation"
  :version "1.0.2"
  :depends-on
  (
   "closer-mop" ; meta object protocols
   "alexandria" ; public domain utilities
   )
  :pathname
  "src/"
  :components
  ((:file "validation")))

;; These two methods below are added by JINGOH.GENERATOR.
(in-package :asdf)
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "validation"))))
  (append (call-next-method) '((test-op "validation.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "validation"))) &rest keys
                        &key ((:compile-print *compile-print*))
                        ((:compile-verbose *compile-verbose*))
                        &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
