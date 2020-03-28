(in-package :cl-user)
(defpackage :validation
  (:use :cl)
  (:export
    ;; Main apis for light users.
    #:validate ; Generic-function method-combination
    #:with-object-validation ; Helper macro to write VALIDATE method.
    ;; Miscellaneous
    #:update-instance ; Like CL:MAKE-INSTANCE but destructively modify the first argument.

    ;; DSL for extension.
    #:defassert ; Macro to define new assertion.
    #:next-assertion ; Must called inside of ASSERT-FORM.
    #:*errors* ; To build error stack.
    ;; slot-accessors for second argument of ASSERT-FORM
    #:object ; To access variable which bound by object.
    #:slot-name ; To access slot-name symbol.
    #:local-var ; To access variable which bound by run-time slot value.
    #:value ; To access second element of assertion, e.g. INTEGER of the assertion (:type integer).
    #:format-args ; To access 2ndcdr sublist of assertion,
                  ; e.g. ("~A must integer" slot-name) of the assertion (:type integer "~A must integer" slot-name)
    ))
(in-package :validation)

;;;; METHOD COMBINATION
;;; TODO?
;;; Currently around method is not supported.
(define-method-combination validate()
			   ((primary (validate) :required t))
  (if (null(cdr primary))
    `(call-method ,(car primary) nil)
    `(multiple-value-call (lambda(&rest results)
			    (values (car results)
				    (loop :for errors :in (cdr results) :by #'cddr
					  :append errors)))
       ,@(mapcar (lambda(method)
		   `(call-method ,method nil))
		 primary))))

;;;; VALIDATE
(defgeneric validate(object &key)
  (:method-combination validate))

;;;; UPDATE-INSTANCE
(defgeneric update-instance(object &rest args))

(defmethod update-instance((object standard-object)&rest args)
  (loop :with initargs = (loop :for key :in args :by #'cddr :collect key)
	:for slot :in (c2mop:class-slots (class-of object))
	:for keys = (intersection (c2mop:slot-definition-initargs slot) initargs)
	:when (and keys
		   (or (null (cdr keys))
		       (error "Dupcated initargs ~S"keys)))
	:do (let((value(getf args(car keys) #0='#:not-supplied)))
	      (unless(eq value #0#)
		(setf (slot-value object (c2mop:slot-definition-name slot))
		    value))))
  object)

;;;; HELPERS
(defun canonicalize-assertions(assertions)
  (mapcar (lambda(assert)
	    (etypecase(car assert)
	      (SYMBOL
		(cons (list (car assert)(car assert))(cdr assert)))
	      ((CONS (AND SYMBOL (NOT (OR KEYWORD BOOLEAN)))
		     (CONS (AND SYMBOL (NOT (OR KEYWORD BOOLEAN)))
			   NULL))
	       (assert(every (lambda(clause)
			       (keywordp (car clause)))
			     (cdr assert)))
	       assert)))
	  assertions))

(defun <slot-asserts>(clause g.targets? g.obj)
  (destructuring-bind((local-var slot-name) . assertions)clause
    (if g.targets?
      `(when (or (find ',slot-name ,g.targets?)
		 (null ,g.targets?))
	 ,#0=(next-assertion (make-args :object g.obj
					:slot-name slot-name
					:local-var local-var
					:rest-assertions (parse-assertions assertions))))
      #0#)))

(defun parse-assertions(assertions)
  (if(typep (car assertions)'(CONS (EQL :REQUIRE) *))
    assertions
    (cons '(:require nil) assertions)))

(defstruct(args (:conc-name nil))
  "Intermediate object"
  object slot-name local-var value format-args rest-assertions)

(defmethod update-instance((args args)&rest params)
  (setf args (copy-args args))
  (loop :for slot :in (c2mop:class-slots(class-of args))
	:for name = (c2mop:slot-definition-name slot)
	:for v = (getf params (intern (symbol-name name)
				      :keyword)
		       #0='#:not-found)
	:unless (eq v #0#)
	:do (setf (slot-value args name) v))
  args)

(defgeneric assert-form(key args))

(defmacro defassert(key (args)&body body)
  (check-type key symbol)
  (let((k(gensym"KEY")))
    `(defmethod assert-form ((,k (eql ',key)),args)
       ,@body)))

(set-pprint-dispatch '(cons (member defassert)) (pprint-dispatch '(defun)))

(defassert :require (args)
  (let((rest(next-assertion args))
       (required?(value args)))
    (when(or rest required?)
      `(IF (SLOT-BOUNDP ,(object args) ',(slot-name args))
	   ,(if rest
	      `(LET((,(local-var args)(SLOT-VALUE ,(object args) ',(slot-name args))))
		 ,rest)
	      nil)
	   ,(when required?
	      `(PUSH (CONS ',(slot-name args)
			   ,(let((format-args(format-args args)))
			      (if format-args
				`(FORMAT NIL ,@(format-args args))
				"is required")))
		     *errors*))))))

(defassert :type (args)
  `(IF (TYPEP ,(local-var args) ',(value args))
     ,(next-assertion args)
     (PUSH (CONS ',(slot-name args)
		 (FORMAT NIL ,@(or (format-args args)
				   `("is not type-of ~S" ',(value args)))))
	    *ERRORS*)))

(defassert :key (args)
  (let((v(gensym"CANONICALIZED"))
       (c(gensym"CONDITION")))
    `(HANDLER-CASE(LET((,v (FUNCALL ,(value args) ,(local-var args))))
		    (SETF (SLOT-VALUE ,(object args) ',(slot-name args)) ,v
			  ,(local-var args) ,v)
		    ,(next-assertion args))
       ,(if (format-args args)
	  `(ERROR ()(PUSH (CONS ',(slot-name args)(FORMAT NIL ,@(format-args args)))*ERRORS*))
	  `(ERROR (,c)(PUSH(CONS ',(slot-name args)(PRINC-TO-STRING ,c))*ERRORS*))))))

(defassert :assert (args)
  `(IF ,(value args)
       ,(next-assertion args)
       (PUSH (CONS ',(slot-name args)
		   (FORMAT NIL ,@(or (format-args args)
				     `("must satisfies ~S but ~S"',(value args),(local-var args)))))
	     *ERRORS*)))

(defmethod no-applicable-method ((gf (eql #'assert-form)) &rest args)
  (destructuring-bind(key args)args
    (declare(ignore args))
    (let((keys(loop :for method :in (c2mop:generic-function-methods gf)
		    :for specializer = (car (c2mop:method-specializers method))
		    :if (typep specializer 'c2mop:eql-specializer)
		    :collect (c2mop:eql-specializer-object specializer)
		    :else
		    :do (restart-bind((remove-method-before-abort
					(lambda()(remove-method gf method)
					  (invoke-restart(find-restart 'abort)))))
			  (error "~S first argument must be specialized by eql-specializer, but ~S"
				 (c2mop:generic-function-name gf)
				 specializer)))))
      (if(not(find key keys))
	(error "Unknown key ~S, supported are ~S." key keys)
	(call-next-method)))))

(defun next-assertion(args)
  (let((rest-assertions(rest-assertions args)))
    (unless(endp rest-assertions)
      (destructuring-bind(key value . format-args)(car rest-assertions)
	(assert-form key (update-instance args :value value :format-args format-args
					  :rest-assertions (cdr rest-assertions)))))))

(defun check-targets(object slots)
  (let((names(mapcar #'c2mop:slot-definition-name (c2mop:class-slots (class-of object)))))
    (dolist(slot slots)
      (unless(find slot names)
	(error "Unknown slot ~S, class ~S have only ~S" slot (type-of object) names)))))

#| bnf
(with-object-validation(object-generate-form target-slots &optional optional-pred*)&rest clause*)
object-generate-form := form which generate object.
target-slots := form which generate list which icludes slot-names.
clause := (var-spec &rest assertion*)
var-spec := [ slot-name | (local-var slot-name) ]
slot-name := symbol
local-var := symbol
assertion := [ require-assertion | type-assertion | key-assertion | assert-assertion ]
require-assertion := (:require boolean)
type-assertion := (:type type-spec &rest format-control)
type-spec := form which acceptable for TYPEP second arguments.
key-assertion := (:key key-function &rest format-control)
key-function := as (function * *), but must signal an error when failed.
assert-assertion := (:assert assert-form &rest format-control)
assert-form := form, but must return nil when failed. In this form local-var is able to be refer.
format-control := which acceptable (apply #'format nil format-control). In this form local-var is able to be refer.
optional-pred := (pred-form subject document)
pred-form := form which generate nullary function.
subject := symbol
document := string
|#

(defvar *errors*)

(defmacro with-object-validation((object-generate-form &key target-slots ((:test optional-clause)))(&rest assertions))
  (setf assertions (canonicalize-assertions assertions))
  (alexandria:with-unique-names(var targets clause)
    ;; body
    `(LET((*errors*)
	  (,var ,object-generate-form)
	  ,@(when target-slots
	      `((,targets ,target-slots))))
       ,@(when target-slots
	   `((CHECK-TYPE ,targets LIST)
	     (CHECK-TARGETS ,var ,targets)))
       ,@(loop :for assert :in assertions
	       :collect (<slot-asserts> assert (and target-slots targets) var))
       ,@(when optional-clause
	   (alexandria:with-unique-names(test subject doc-string)
	     `((dolist(,clause ,optional-clause)
		 (destructuring-bind(,test ,subject ,doc-string),clause
		   (unless(funcall ,test)
		     (push (cons ,subject ,doc-string) *errors*)))))))
       (values ,var (reverse *errors*)))))
