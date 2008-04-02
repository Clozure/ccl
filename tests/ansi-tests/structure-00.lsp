;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May  9 11:21:25 1998
;;;; Contains: Common code for creating structure tests

(in-package :cl-test)
(declaim (optimize (safety 3)))

(defun make-struct-test-name (structure-name n)
  ;; (declare (type (or string symbol character) structure-name)
  ;;  (type fixnum n))
  (assert (typep structure-name '(or string symbol character)))
  ;; (assert (typep n 'fixnum))
  (setf structure-name (string structure-name))
  (intern (concatenate 'string
	    structure-name
	    "/"
	    (princ-to-string n))))

(defun make-struct-p-fn (structure-name)
  (assert (typep structure-name '(or string symbol character)))
  (setf structure-name (string structure-name))
  (intern (concatenate 'string
	    structure-name
	    (string '#:-p))))

(defun make-struct-copy-fn (structure-name)
  (assert (typep structure-name '(or string symbol character)))
  (setf structure-name (string structure-name))
  (intern (concatenate 'string
		       (string '#:copy-)
		       structure-name)))

(defun make-struct-field-fn (conc-name field-name)
  "Make field accessor for a field in a structure"
  (cond
   ((null conc-name) field-name)
   (t
    (assert (typep conc-name '(or string symbol character)))
    (assert (typep field-name '(or string symbol character)))
    (setf conc-name (string conc-name))
    (setf field-name (string field-name))
    (intern (concatenate 'string conc-name field-name)))))

(defun make-struct-make-fn (structure-name)
  "Make the make- function for a structure"
  (assert (typep structure-name '(or string symbol character)))
  (setf structure-name (string structure-name))
  (intern (concatenate 'string
	    (string '#:make-) structure-name)))

(defun create-instance-of-type (type)
  "Return an instance of a type.  Signal an error if
  it can't figure out a value for the type."
  (cond
   ((eqt type t)  ;; anything
    'a)
   ((eqt type 'symbol)
    'b)
   ((eqt type 'null) nil)
   ((eqt type 'boolean) t)
   ((eqt type 'keyword) :foo)
   ((eqt type nil) (error "Cannot obtain element of type ~S~%" type))
   ((eqt type 'cons) (cons 'a 'b))
   ((eqt type 'list) (list 1 2 3))
   ((eqt type 'fixnum) 17)
   ((eqt type 'bignum)
    (let ((x 1))
      (loop until (typep x 'bignum)
	  do (setq x (* 2 x)))
      x))
   ((and (symbolp type)
	 (typep type 'structure-class))
    (let ((make-fn
	   (intern (concatenate 'string (string '#:make-) (symbol-name type))
		   (symbol-package type))))
      (eval (list make-fn))))
   ((eqt type 'character) #\w)
   ((eqt type 'base-char) #\z)
   ((member type '(integer unsigned-byte signed-byte)) 35)
   ((eqt type 'bit) 1)
   ((and (consp type)
	 (consp (cdr type))
	 (consp (cddr type))
	 (null (cdddr type))
	 (eqt (car type) 'integer)
	 (integerp (second type)))
    (second type))
   ((member type '(float single-float long-float double-float short-float))
    0.0)
   ((and (consp type)
	 (eqt (car type) 'member)
	 (consp (cdr type)))
    (second type))
   ((and (consp type)
	 (eqt (car type) 'or)
	 (consp (second type)))
    (create-instance-of-type (second type)))
   (t (error "Cannot generate element for type ~S~%" type))))

(defun find-option (option-list option &optional default)
  (loop for opt in option-list
	when (or (eq opt option)
		 (and (consp opt)
		      (eq (car opt) option)))
	return opt
	finally (return default)))

(defvar *defstruct-with-tests-names* nil
  "Names of structure types defined with DEFSRUCT-WITH-TESTS.")

#|
(defvar *subtypep-works-with-classes* t
  "Becomes NIL if SUBTYPEP doesn't work with classes.  We test this first to avoid
   repeated test failures that cause GCL to bomb.")

(deftest subtypep-works-with-classes
  (let ((c1 (find-class 'vector)))
    ;; (setq *subtypep-works-with-classes* nil)
    (subtypep c1 'vector)
    (subtypep 'vector c1)
    ;; (setq *subtypep-works-with-classes* t))
  t)

(defvar *typep-works-with-classes* t
  "Becomes NIL if TYPEP doesn't work with classes.  We test this first to avoid
   repeated test failures that cause GCL to bomb.")

(deftest typep-works-with-classes
  (let ((c1 (find-class 'vector)))
    ;; (setq *typep-works-with-classes* nil)
    (typep #(0 0) c1)
    ;; (setq *typep-works-with-classes* t))
  t)
|#

;;
;; There are a number of standardized tests for
;; structures.  The following macro generates the
;; structure definition and the tests.
;;

(defmacro defstruct-with-tests
    (name-and-options &body slot-descriptions-and-documentation)
"Construct standardized tests for a defstruct, and also
do the defstruct."
  (defstruct-with-tests-fun name-and-options
    slot-descriptions-and-documentation))

(defun defstruct-with-tests-fun (name-and-options
				 slot-descriptions-and-documentation)
  ;; Function called from macro defstruct-with-tests
  (let* (
	 ;; Either NIL or the documentation string for the structure
	 (doc-string
	  (when (and (consp slot-descriptions-and-documentation)
		     (stringp (car slot-descriptions-and-documentation)))
	    (car slot-descriptions-and-documentation)))

	 ;; The list of slot descriptions that follows either the
	 ;; name and options or the doc string
	 (slot-descriptions
	  (if doc-string (cdr slot-descriptions-and-documentation)
	    slot-descriptions-and-documentation))

	 ;; The name of the structure (should be a symbol)
	 (name (if (consp name-and-options)
		   (car name-and-options)
		 name-and-options))

	 ;; The options list, or NIL if there were no options
	 (options (if (consp name-and-options)
		      (cdr name-and-options)
		    nil))

	 ;; List of symbols that are the names of the slots
	 (slot-names
	  (loop
	   for x in slot-descriptions collect
	   (if (consp x) (car x) x)))

	 ;; List of slot types, if any
	 (slot-types
	  (loop
	   for x in slot-descriptions collect
	   (if (consp x)
	       (getf (cddr x) :type :none)
	     :none)))

	 ;; read-only flags for slots
	 (slot-read-only
	  (loop
	   for x in slot-descriptions collect
	   (and (consp x)
		(getf (cddr x) :read-only))))

	 ;; Symbol obtained by prepending MAKE- to the name symbol
	 (make-fn (make-struct-make-fn name))

	 ;; The type option, if specified
	 (type-option (find-option options :type))
	 (struct-type (second type-option))

	 (named-option (find-option options :named))
	 (include-option (find-option options :include))

	 ;; The :predicate option entry from OPTIONS, or NIL if none
	 (predicate-option (find-option options :predicate))

	 ;; The name of the -P function, either the default or the
	 ;; one specified in the :predicate option
	 (p-fn-default (make-struct-p-fn name))
	 (p-fn (cond
		((and type-option (not named-option)) nil)
		((or (eq predicate-option :predicate)
		     (null (cdr predicate-option)))
		 p-fn-default)
		((cadr predicate-option) (cadr predicate-option))
		(t nil)))

	 ;; The :copier option, or NIL if no such option specified
	 (copier-option (find-option options :copier))
	 ;; The name of the copier function, either the default or
	 ;; one speciefied in the :copier option
	 (copy-fn-default (make-struct-copy-fn name))
	 (copy-fn (cond
		   ((or (eq copier-option :copier)
			(null (cdr copier-option)))
		    copy-fn-default)
		   ((cadr copier-option) (cadr copier-option))
		   (t nil)))

	 ;; The :conc-name option, or NIL if none specified
	 (conc-option (find-option options :conc-name))
	 ;; String to be prepended to slot names to get the
	 ;; slot accessor function
	 (conc-prefix-default (concatenate 'string (string name) "-"))
	 (conc-prefix (cond
		       ((null conc-option)
			conc-prefix-default)
		       ((or (eq conc-option :conc-name)
			    (null (cadr conc-option)))
			nil)
		       (t (string (cadr conc-option)))))

	 (initial-offset-option (find-option options :initial-offset))
	 (initial-offset (second initial-offset-option))
	 
	 ;; Accessor names
	 (field-fns
	  (loop for slot-name in slot-names
		collect (make-struct-field-fn conc-prefix slot-name)))

	 ;; a list of initial values
	 (initial-value-alist
	  (loop
	   for slot-desc in slot-descriptions
	   for slot-name in slot-names
	   for type      in slot-types
	   for i from 1
	   collect (if (not (eq type :none))
		       (cons slot-name (create-instance-of-type type))
		     (cons slot-name (defstruct-maketemp name "SLOTTEMP" i)))))
	 )
    (declare (ignorable initial-offset))
    ;; Build the tests in an eval-when form
    `(eval-when (:load-toplevel :compile-toplevel :execute)

       (report-and-ignore-errors
	(eval '(defstruct ,name-and-options
		 ,@slot-descriptions-and-documentation))
	,(unless (or type-option include-option)
	   `(pushnew ',name *defstruct-with-tests-names*))
	nil)

       ;; Test that structure is of the correct type
       (deftest ,(make-struct-test-name name 1)
	 (and (fboundp (quote ,make-fn))
	      (functionp (function ,make-fn))
	      (symbol-function (quote ,make-fn))
	      (typep (,make-fn) (quote ,(if type-option struct-type
					  name)))
	      t)
	 t)

       ;; Test that the predicate exists
       ,@(when p-fn
	   `((deftest ,(make-struct-test-name name 2)
	       (let ((s (,make-fn)))
		 (and (fboundp (quote ,p-fn))
		      (functionp (function ,p-fn))
		      (symbol-function (quote ,p-fn))
		      (notnot (funcall #',p-fn s))
		      (notnot-mv (,p-fn s))
		      ))
	       t)
	     (deftest ,(make-struct-test-name name "ERROR.1")
	       (signals-error (,p-fn) program-error)
	       t)
	     (deftest ,(make-struct-test-name name "ERROR.2")
	       (signals-error (,p-fn (,make-fn) nil) program-error)
	       t)
	     ))

       ;; Test that the elements of *universe* are not
       ;; of this type
       ,@(when p-fn
	   `((deftest ,(make-struct-test-name name 3)
	       (count-if (function ,p-fn) *universe*)
	       0)))
       ,@(unless type-option
	   `((deftest ,(make-struct-test-name name 4)
	       (count-if (function (lambda (x) (typep x (quote ,name))))
			 *universe*)
	       0)))

       ;; Check that the fields can be read after being initialized
       (deftest ,(make-struct-test-name name 5)
	 ,(let ((inits nil)
		(tests nil)
		(var (defstruct-maketemp name "TEMP-5")))
	    (loop
	     for (slot-name . initval) in initial-value-alist
	     for field-fn in field-fns
	     do
	     (setf inits
		   (list* (intern (string slot-name) "KEYWORD")
			  (list 'quote initval)
			  inits))
	     (push `(and 
		     (eqlt (quote ,initval)
			   (,field-fn ,var))
		     (eqlt (quote ,initval)
			   (funcall #',field-fn ,var)))
		   tests))
	    `(let ((,var (,make-fn . ,inits)))
	       (and ,@tests t)))
	 t)

       (deftest ,(make-struct-test-name name "ERROR.3")
	 (remove nil
		 (list
		  ,@(loop
		     for (slot-name . initval) in initial-value-alist
		     for field-fn in field-fns
		     collect
		     `(multiple-value-bind
			  (x val)
			  (signals-error (,field-fn) program-error)
			(unless x
			  (list ',slot-name ',field-fn val))))))
	 nil)

       (deftest ,(make-struct-test-name name "ERROR.4")
	 (remove nil
		 (list
		  ,@(loop
		     for (slot-name . initval) in initial-value-alist
		     for field-fn in field-fns
		     collect
		     `(multiple-value-bind
			  (x val)
			  (signals-error (,field-fn (,make-fn) nil)
					 program-error)
			(unless x
			  (list ',slot-name ',field-fn val))))))
	 nil)

       ;; Check that two invocations return different structures
       (deftest ,(make-struct-test-name name 6)
	 (eqt (,make-fn) (,make-fn))
	 nil)

       ;; Check that we can setf the fields
       (deftest ,(make-struct-test-name name 7)
	 ,(let* ((var (defstruct-maketemp name "TEMP-7-1"))
		 (var2 (defstruct-maketemp name "TEMP-7-2"))
		 (tests
		  (loop
		   for (slot-name . initval) in initial-value-alist
		   for read-only-p in slot-read-only
		   for slot-desc in slot-descriptions
		   for field-fn in field-fns
		   unless read-only-p
		   collect
		   `(let ((,var2 (quote ,initval)))
		      (setf (,field-fn ,var) ,var2)
		      (eqlt (,field-fn ,var) ,var2)))))
	    `(let ((,var (,make-fn)))
	       (and ,@tests t)))
	 t)

       ;; Check that the copy function exists
       ,@(when copy-fn
	   `((deftest ,(make-struct-test-name name 8)
	       (and (fboundp (quote ,copy-fn))
		    (functionp (function ,copy-fn))
		    (symbol-function (quote ,copy-fn))
		    t)
	       t)
	     (deftest ,(make-struct-test-name name "ERROR.5")
	       (signals-error (,copy-fn) program-error)
	       t)
	     (deftest ,(make-struct-test-name name "ERROR.6")
	       (signals-error (,copy-fn (,make-fn) nil) program-error)
	       t)
	     ))	     

       ;; Check that the copy function properly copies fields
       ,@(when copy-fn
	   `((deftest ,(make-struct-test-name name 9)
	       ,(let* ((var 'XTEMP-9)
		       (var2 'YTEMP-9)
		       (var3 'ZTEMP-9))	       
		  `(let ((,var (,make-fn
				,@(loop
				   for (slot-name . initval)
				   in initial-value-alist
				   nconc (list (intern (string slot-name)
						       "KEYWORD")
					       `(quote ,initval))))))
		     (let ((,var2 (,copy-fn ,var))
			   (,var3 (funcall #',copy-fn ,var)))
		       (and
			(not (eqlt ,var ,var2))
			(not (eqlt ,var ,var3))
			(not (eqlt ,var2 ,var3))
			,@(loop
			   for (slot-name . nil) in initial-value-alist
			   for fn in field-fns
			   collect
			   `(and (eqlt (,fn ,var) (,fn ,var2))
				 (eqlt (,fn ,var) (,fn ,var3))))
			t))))
	       t)))

       ;; When the predicate is not the default, check
       ;; that the default is not defined.  Tests should
       ;; be designed so that this function name doesn't
       ;; collide with anything else.
       ,@(unless (eq p-fn p-fn-default)
	   `((deftest ,(make-struct-test-name name 10)
	       (fboundp (quote ,p-fn-default))
	       nil)))

       ;; When the copy function name is not the default, check
       ;; that the default function is not defined.  Tests should
       ;; be designed so that this name is not accidently defined
       ;; for something else.
       ,@(unless (eq copy-fn copy-fn-default)
	   `((deftest ,(make-struct-test-name name 11)
	       (fboundp (quote ,copy-fn-default))
	       nil)))

       ;; When there are read-only slots, test that the SETF
       ;; form for them is not FBOUNDP
       ,@(when (loop for x in slot-read-only thereis x)
	   `((deftest ,(make-struct-test-name name 12)
	       (and
		,@(loop for slot-name in slot-names
			for read-only in slot-read-only
			for field-fn in field-fns
			when read-only
			collect `(not-mv (fboundp '(setf ,field-fn))))
		t)
	       t)))

       ;; When the structure is a true structure type, check that
       ;; the various class relationships hold
       ,@(unless type-option
	   `(
	     (deftest ,(make-struct-test-name name 13)
	       (notnot-mv (typep (,make-fn) (find-class (quote ,name))))
	       t)
	     (deftest ,(make-struct-test-name name 14)
	       (let ((class (find-class (quote ,name))))
		 (notnot-mv (typep class 'structure-class)))
	       t)
	     (deftest ,(make-struct-test-name name 15)
	       (notnot-mv (typep (,make-fn) 'structure-object))
	       t)
	     (deftest ,(make-struct-test-name name 16)
	       (loop for type in *disjoint-types-list*
		     unless (and
			     (equalt (multiple-value-list
				      (subtypep* type (quote ,name)))
				     '(nil t))
			     (equalt (multiple-value-list
				      (subtypep* (quote ,name) type))
				     '(nil t)))				    
		     collect type)
	       nil)
	     (deftest ,(make-struct-test-name name 17)
	       (let ((class (find-class (quote ,name))))
		 (loop for type in *disjoint-types-list*
		       unless (and
			       (equalt (multiple-value-list
					(subtypep* type class))
				       '(nil t))
			       (equalt (multiple-value-list
					(subtypep* class type))
				       '(nil t)))
		       collect type))
	       nil)
	     (deftest ,(make-struct-test-name name "15A")
	       (let ((class (find-class (quote ,name))))
		 (notnot-mv (subtypep class 'structure-object)))
	       t t)
	     (deftest ,(make-struct-test-name name "15B")
	       (notnot-mv (subtypep (quote ,name) 'structure-object))
	       t t)

	     ))

       ;;; Documentation tests

       ,(when doc-string
	  `(deftest ,(make-struct-test-name name 18)
	     (let ((doc (documentation ',name 'structure)))
	       (or (null doc) (equalt doc ',doc-string)))
	     t))

       ,(when (and doc-string (not type-option))
	  `(deftest ,(make-struct-test-name name 19)
	     (let ((doc (documentation ',name 'type)))
	       (or (null doc) (equalt doc ',doc-string)))
	     t))

       ;; Test that COPY-STRUCTURE works, if this is a structure
       ;; type
       ,@(unless type-option
	   `((deftest ,(make-struct-test-name name 20)
	       ,(let* ((var 'XTEMP-20)
		       (var2 'YTEMP-20))
		  `(let ((,var (,make-fn
				,@(loop
				   for (slot-name . initval)
				   in initial-value-alist
				   nconc (list (intern (string slot-name)
						       "KEYWORD")
					       `(quote ,initval))))))
		     (let ((,var2 (copy-structure ,var)))
		       (and
			(not (eqlt ,var ,var2))
			,@(loop
			   for (slot-name . nil) in initial-value-alist
			   for fn in field-fns
			   collect
			   `(eqlt (,fn ,var) (,fn ,var2)))
			t))))
	       t)))
       nil
       )))

(defun defstruct-maketemp (stem suffix1 &optional suffix2)
  "Make a temporary variable for DEFSTRUCT-WITH-TESTS."
  (intern (if suffix2 (format nil "~A-~A-~A" stem suffix1 suffix2)
	    (format nil "~A-~A" stem suffix1))))
