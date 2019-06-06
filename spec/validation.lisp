(defpackage :validation.spec
  (:use :cl :jingoh :validation))
(in-package :validation.spec)
(setup :validation)

(requirements-about VALIDATE)
;; setup.
#?(defmethod validate validate ((o symbol)&key)
    (if(equal "" (symbol-name o))
      (values o `((symbol-name . "is empty")))
      (values o nil)))
:be-the method
,:before (loop :for specializer :in '(symbol string t vector)
	       :for method = (find-method #'validate '(validate)(list (find-class specializer))nil)
	       :when method :do (remove-method #'validate method))

#?(defmethod validate validate ((s string)&key length)
    (if(eql (length s)length)
      (values s nil)
      (values s `((string-length . ,(format nil "is not ~S"length))))))
:be-the method

;;;; Description:
; Validate object.

#+syntax
(VALIDATE object &key target-slots test) ; => result

;;;; Argument Precedence Order:
; object

;;;; Method signature:
; none

;;;; Arguments and Values:

; object := T

; result := (values object errors)
; errors := list.
#?(validate :hoge) :values (:hoge nil)
#?(validate :||) :values (:|| ((symbol-name . "is empty")))

;;;; Affected By:
; Should be none.

;;;; Side-Effects:
; OBJECT may be destructively modified.

;;;; Notes:
; superclass validation is alway called.
#?(defmethod validate validate(o &key)
    (values o '((object . "the default"))))
:be-the method

#?(validate :hoge) :values (:hoge ((object . "the default")))
#?(validate :||) :values (:|| ((symbol-name . "is empty")(object . "the default")))
#?(validate "hoge" :length 4) :values ("hoge" ((object . "the default")))

;;;; Exceptional-Situations:
; Each method must return two values, otherwise runtime error may occur.
#?(defmethod validate validate((vector vector)&key)
    (if (= 0 (length vector))
      (values vector `((vector . "is empty")))
      vector))
:be-the method

#?(validate "" :length 1) :values ("" ((string-length . "is not 1")
				       (vector . "is empty")
				       (object . "the default")))
#?(validate "hoge") :signals error

; When other method combination is specified, runtime error is occur
#?(defmethod validate and ((object symbol)&key)
    (values object nil))
:be-the method

#?(validate :hoge) :signals error
,:lazy t
,:ignore-signals warning

; In such case, you need to remove-method.
#?(remove-method #'validate (find-method #'validate '(and) (list (find-class 'symbol))))
:be-the generic-function

; After remove-method, generic function varidate works fine.
#?(validate :hoge) :values (:hoge ((object . "the default")))

; When DEFMETHOD with no specifiers, runtime error is occur.
#?(defmethod validate ((object symbol)&key)
    (values object nil))
:be-the method

#?(validatioin :hoge) :signals error

; Same with above, you need to remove-method.
#?(remove-method #'validate (find-method #'validate () (list (find-class 'symbol))))
:be-the generic-function

; After remove-method, generic-function validate works fine.
#?(validate :hoge) :values (:hoge ((object . "the default")))

; TIPS - If you feel annoying to remove-method, you can (fmakunbound 'validate) then reload system with :force t.

(requirements-about WITH-OBJECT-VALIDATION)

;;;; Description:
; DSL macro for writing VALIDATE method.

; setup
#?(defclass super ()
    ((number :initarg :number :accessor number-of)
     (string :initarg :string :accessor string-of)))
:be-the standard-class

#+syntax
(WITH-OBJECT-VALIDATION (object-generate-form &key target-slots
                         ((:test optional-clause))) (&rest assertions)) ; => result

; Minimum example.
#?(with-object-validation(0)())
:expanded-to (let((*errors*)(object 0))
	       (values object (reverse *errors*)))

;;;; Arguments and Values:

; object-generate-form := A form which generates standard-object, otherwise unspecified.
#?(with-object-validation((make-condition 'error))()) => unspecified

; target-slots := List, otherwise signals an error.
#?(with-object-validation((make-instance 'super) :target-slots "not-list")())
:signals type-error

; Each element must be symbol which names slot, otherwise signals an error.
#?(with-object-validation((make-instance 'super) :target-slots '(no-such-slot))())
:signals error
#?(with-object-validation((make-instance 'super) :target-slots '("NUMBER"))())
:signals error

; When specified, only such slots are validated.
#?(with-object-validation((make-instance 'super)) ; <--- not specified.
    ((number (:require t))
     (string (:require t))))
:multiple-value-satisfies #`(& (typep $1 'super)
			       (not(slot-boundp $1 'number))
			       (not(slot-boundp $1 'string))
			       ;; Every slots are validated.
			       (equal $2 '((number . "is required")
					   (string . "is required"))))

#?(with-object-validation((make-instance 'super) :target-slots '(number)) ; <--- Specify only NUMBER.
    ((number (:require t))
     (string (:require t))))
:multiple-value-satisfies #`(& (typep $1 'super)
			       (not(slot-boundp $1 'number))
			       (not(slot-boundp $1 'string))
			       ;; Only number slot is validated.
			       (equal $2 '((number . "is required"))))

; test := List, otherwise error.
#?(with-object-validation((make-instance 'super):test "not list")())
:signals error

; Each element must be (function-designator subject message).
#?(with-object-validation((make-instance 'super):test '(not-list))())
:signals error
#?(with-object-validation((make-instance 'super):test '((less elt)))())
:signals error
#?(with-object-validation((make-instance 'super):test '((too much long elt)))())
:signals error

; function-designator := as (function()generalized-boolean), otherwise error.
#?(with-object-validation((make-instance 'super):test '((not-function :dummy :dummy)))())
:signals error

; subject := Unspecified. Expected symbol currently, but not limited.

; message := Unspecified. Expected string currently, but not limited.

; When predicate fails, pair of subject and mesage are included as ERRORS.
#?(with-object-validation((make-instance 'super):test '((list subject "message")))())
:multiple-value-satisfies (lambda(object errors)
			    (& (typep object 'super)
			       (equal errors '((subject . "message")))))

; assertion := (slot-specifier [ require-assert | type-assert | key-assert | assert-assert ]*)
; slot-specifier := [ slot-name | (alias slot-name) ]
; alias := symbol
#?(with-object-validation((make-instance 'super :number "not integer"))
    (((n number)(:key #'parse-integer "~S is not integer" n))))
:multiple-value-satisfies (lambda(object errors)
			    (& (typep object 'super)
			       (equal "not integer" (number-of object))
			       (equal errors '((number . "\"not integer\" is not integer")))))

; require-assert := (:require boolean . format-args)
; When specified true, such slot must bound.
#?(with-object-validation((make-instance 'super :number nil))
    ((number (:require t))
     (string (:require t))))
:multiple-value-satisfies (lambda(object errors)
			    (& (typep object 'super)
			       (equal errors '((string . "is required")))))

; format-args := as (apply #'format nil format-args).
#?(with-object-validation((make-instance 'super))
    ((number (:require t "altered message"))
     (string (:require t "Example of using parameter ~D" 10))))
:multiple-value-satisfies (lambda(object errors)
			    (& (typep object 'super)
			       (equal errors '((number . "altered message")
					       (string . "Example of using parameter 10")))))

; Example of minimum :require assertion.
#?(with-object-validation(0)
    ((slot-name (:require t))))
:expanded-to (let((*errors*)(object 0))
	       (if (slot-boundp object 'slot-name)
		 nil
		 (push (cons 'slot-name "is required")
		       *errors*))
	       (values object (reverse *errors*)))

; type-assert := (:type type-specifier . format-args)
; type-specifier := Acceptable value as (typep t type-specifier)
; When type of slot value is not specified type, stack errors.
#?(with-object-validation((make-instance 'super :number nil))
    ((number (:type integer))))
:multiple-value-satisfies (lambda(object errors)
			    (& (typep object 'super)
			       (slot-boundp object 'number)
			       (null(slot-value object 'number))
			       (equal errors '((number . "is not type-of INTEGER")))))

; Example of minimum :type assertion.
#?(with-object-validation(0)
    ((slot-name (:type integer))))
:expanded-to (let((*errors*)(object 0))
	       (if(slot-boundp object 'slot-name)
		 (let((slot-name (slot-value object 'slot-name)))
		   (if (typep slot-name 'integer)
		     nil
		     (push (cons 'slot-name (format nil "is not type-of ~S" 'integer))
			   *errors*)))
		 nil)
	       (values object (reverse *errors*)))

; key-assert := (:key key-function . format-args)
; key-function := as (function(t))
; This function must signal an error when failed, (e.g. CL:PARSE-INTEGER).
; Otherwise slot value is SETFed with return value.
; Case when success.
#?(with-object-validation((make-instance 'super :number "1"))
    ((number (:key #'parse-integer))))
:multiple-value-satisfies (lambda(object errors)
			    (& (typep object 'super)
			       (eql 1 (number-of object)) ; <--- Slot value is updated.
			       (null errors)))
; Case when fail, with signaling.
#?(with-object-validation((make-instance 'super :number "not number"))
    ((number (:key #'parse-integer "is not able to be parsed"))))
:multiple-value-satisfies (lambda(object errors)
			    (& (typep object 'super)
			       (equal "not number" (number-of object)) ; <--- Not updated.
			       (equal errors '((number . "is not able to be parsed")))))
; Bad example
#?(with-object-validation((make-instance 'super :number "not number"))
    ((number (:key #'integerp))))
:multiple-value-satisfies (lambda(object errors)
			    (& (typep object 'super)
			       (null (number-of object)) ; <--- Updated.
			       (null errors)))

; Example of minimum :key assertion.
#?(with-object-validation(0)
    ((slot-name (:key #'integerp))))
:expanded-to (let((*errors*)(object 0))
	       (if(slot-boundp object 'slot-name)
		 (let((slot-name (slot-value object 'slot-name)))
		   (handler-case(let((canonicalized(funcall #'integerp slot-name)))
				  (setf (slot-value object 'slot-name) canonicalized
					slot-name canonicalized)
				  nil)
		     (error(condition)
		       (push (cons 'slot-name (princ-to-string condition))
			     *errors*))))
		 nil)
	       (values object (reverse *errors*)))

; assert-assert := (:assert assert-expression . format-args)
; assert-expression := Form which generates generalized boolean.
; In this form, slot can be refer.
#?(with-object-validation((make-instance 'super :number 3))
    ((number (:assert (< 0 number 5)))))
:multiple-value-satisfies (lambda(object errors)
			    (& (typep object 'super)
			       (eql 3 (number-of object))
			       (null errors)))

#?(with-object-validation((make-instance 'super :number 30))
    ((number (:assert (< 0 number 5) "is out of range"))))
:multiple-value-satisfies (lambda(object errors)
			    (& (typep object 'super)
			       (eql 30 (number-of object))
			       (equal errors '((number . "is out of range")))))

; Example of minimum :assert assertion.
#?(with-object-validation(0)
    ((slot-name (:assert (< slot-name 10)))))
:expanded-to (let((*errors*)(object 0))
	       (if(slot-boundp object 'slot-name)
		 (let((slot-name (slot-value object 'slot-name)))
		   (if (< slot-name 10)
		     nil
		     (push (cons 'slot-name (format nil "must satisfies ~S but ~S"
						    '(< slot-name 10)
						    slot-name))
			   *errors*)))
		 nil)
	       (values object (reverse *errors*)))

; result := (values object errors)
; errors := alist of (subject . message)

;;;; Affected By:
; none

;;;; Side-Effects:
; OBJECT may be destructively modified.

;;;; Notes:
; assertions are checked from left to right. When first assertion is failed assertions are totally failed.
#?(with-object-validation((make-instance 'super :number 5))
    ((number (:type string)
	     (:key #'print)
	     (:assert (print(< number 10))))))
:multiple-value-satisfies (lambda(object errors)
			    (& (typep object 'super)
			       (eql 5 (number-of object))
			       (equal errors '((number . "is not type-of STRING")))))
,:lazy t
,:ignore-signals warning
,:stream nil

;;;; Exceptional-Situations:

(requirements-about UPDATE-INSTANCE)

;;;; Description:
; Update object with its keyword parameter.

#?(update-instance (make-instance 'super :number 5) ; <--- Make with 5.
		   :number 10)                      ; <--- Update with 10.
:satisfies (lambda(object)
	     (& (typep object 'super)
		(eql 10 (number-of object))))

; Unspecified keyword parameter is remain.
#?(update-instance (make-instance 'super :number 5 :string "hoge")
		   :number 10) ; <--- Specify only number.
:satisfies (lambda(object)
	     (& (typep object 'super)
		(eql 10 (number-of object)) ; <--- Updated.
		(equal "hoge" (string-of object)))) ; <--- Remain.

#+syntax
(UPDATE-INSTANCE object &rest args) ; => result

;;;; Argument Precedence Order:
; object

;;;; Method signature:
#+signature(UPDATE-INSTANCE (ARGS ARGS) &REST PARAMS)
#+signature(UPDATE-INSTANCE (OBJECT STANDARD-OBJECT) &REST ARGS)

;;;; Arguments and Values:

; object := t

; args := keyword parameters

; result := object.

;;;; Affected By:
; none

;;;; Side-Effects:
; Object may be destructively modified.

;;;; Notes:
; When keywords are duplicated, first one is used.
#?(update-instance (make-instance 'super :number 1)
		   :number 2
		   :number 3)
:satisfies (lambda(object)
	     (& (typep object 'super)
		(eql 2 (number-of object))))

; Other keys are ignored, aka &allow-other-keys
#?(update-instance (make-instance 'super)
		   :no-such-initarg '#:dummy)
:be-the super

;;;; Exceptional-Situations:

(requirements-about DEFASSERT)

;;;; Description:
; DSL to define new assertion.
; Defined methods are called in macroexpand time of WITH-OBJECT-VALIDATION to make validation form.

#?(defassert :dummy (1) 2)
:expanded-to (defmethod validation::assert-form ((k (eql ':dummy)) 1)
	       2)

#+syntax
(DEFASSERT key (args) &body body) ; => result

;;;; Arguments and Values:

; key := symbol, otherwise error.
#?(defassert "not symbol"(dummy))
:signals error
; Not evaluated.
#?(defassert (intern "not evaluated")(dummy))
:signals error

; args := Symbol as variable, otherwise error.
#?(defassert #:dummy ("not symbol"))
:signals error
#?(defassert #:dummy (:keyword-symbol-is-not-able-to-use-as-variable))
:signals error

; body := Implicit PROGN.

; result := Form, which test assertion.

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify global environment, aka define new method.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about NEXT-ASSERTION)

;;;; Description:
; Like CL:CALL-NEXT-METHOD, reccursively call next assertion.

#?(next-assertion (validation::make-args))
=> NIL

#?(next-assertion (validation::make-args :rest-assertions '((:require t))))
=> (if (slot-boundp nil 'nil)
     nil
     (push (cons 'nil "is required") *errors*))
,:test equal

#+syntax
(NEXT-ASSERTION args) ; => result

;;;; Arguments and Values:

; args := validation::args object, otherwise error.
#?(next-assertion "not args") :signals type-error

; result := Form which will be embed in macro WITH-OBJECT-VALIDATION.

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about *ERRORS*)

;;;; Description:
; Special variable to stack error messages for macro WITH-OBJECT-VALIDATION.

;;;; Value type is UNBOUND
#?(boundp '*errors*) => NIL

; Initial value is :UNBOUND

;;;; Affected By:
; none

;;;; Notes:
; Referable in WITH-OBJECT-VALIDATION only.
; See WITH-OBJECT-VALIDATION.

(requirements-about OBJECT)

;;;; Description:
; Slot accessor for object validation::args which is passed to assertion method.
; This slot is bound by variable which will be bound by object.

#+syntax
(OBJECT sb-kernel:instance) ; => result

#+setf
(SETF (OBJECT SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := validation::args object, otherwise error.
#?(object "not args object")
:signals error
,:lazy t

; result := Symbol as variable.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SLOT-NAME)

;;;; Description:
; Slot accessor for object validation::args which is passed to assertion method.
; This slot is bound by variable which will be bound by slot-name.

#+syntax
(SLOT-NAME sb-kernel:instance) ; => result

#+setf
(SETF (SLOT-NAME SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := validation::args object, otherwise error.
#?(slot-name "not args object")
:signals error
,:lazy t

; result := Symbol as slot-name.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about LOCAL-VAR)

;;;; Description:
; Slot accessor for object validation::args which is passed to assertion method.
; This slot is bound by variable which will be bound by slot value.

#+syntax
(LOCAL-VAR sb-kernel:instance) ; => result

#+setf
(SETF (LOCAL-VAR SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := validation::args object, otherwise error.
#?(local-var "not args object")
:signals error
,:lazy t

; result := Symbol as local variable.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about VALUE)

;;;; Description:
; Slot accessor for object validation::args which is passed to assertion method.
; This slot is bound by form which is specified as assertion value.
; E.g. T from (:require t).

#+syntax
(VALUE sb-kernel:instance) ; => result

#+setf
(SETF (VALUE SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := validation::args object, otherwise error.
#?(value "not args object")
:signals error
,:lazy t

; result := Form.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FORMAT-ARGS)

;;;; Description:
; Slot accessor for object validation::args which is passed to assertion method.
; This slot is bound by list which will be as (apply #'format nil format-args)

#+syntax
(FORMAT-ARGS sb-kernel:instance) ; => result

#+setf
(SETF (FORMAT-ARGS SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := validation::args object, otherwise error.
#?(format-args "not args object")
:signals error
,:lazy t

; result := List.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

