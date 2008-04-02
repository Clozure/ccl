;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Mar 24 03:40:24 2003
;;;; Contains: Auxiliary functions for testing CLOS

(in-package :cl-test)

(defun make-defclass-test-name (&rest args)
  (intern (apply #'concatenate 'string (mapcar #'string args))
	  (find-package :cl-test)))

(defparameter *defclass-slot-readers* nil)
(defparameter *defclass-slot-writers* nil)
(defparameter *defclass-slot-accessors* nil)

(defstruct my-class
  (name nil :type symbol)
  (direct-superclass-names nil :type list)
  (slots nil :type list)
  (default-initargs nil :type list)
  (metaclass 'standard-class :type symbol)
  (documentation nil :type (or null string))
  ;; Internal fields
  (preds nil :type list)
  (succs nil :type list)
  (count 0 :type integer)
  (index nil)
  (min-pred-index 1000000)
  )

(defstruct my-slot
  (name nil :type symbol)
  (has-initform nil :type boolean)
  initform
  (initargs nil :type list)
  (documentation nil :type (or null string))
  (readers nil :type list)
  (writers nil :type list)
  (accessors nil :type list)
  (allocation :instance :type (member :instance :class))
  (type t)
  )

(defparameter *my-classes* (make-hash-table)
  "Hash table mapping names of classes defined using DEFCLASS-WITH-TESTS
   to their my-class objects.")

(defun find-my-class (class-name)
  (gethash class-name *my-classes*))

;;; This macro will assume that all the superclasses have already
;;; been defined.  Tests will be written with defclass itself
;;; to test forward referenced superclasses

(defmacro defclass-with-tests
  (&whole args
	  class-name superclasses slot-specifiers
	  &rest class-options)

  (assert (typep class-name '(and (not null) symbol)))
  (assert (listp superclasses))
  (assert (every #'(lambda (x) (typep x '(and (not null) symbol)))
		 superclasses))
  (assert (listp slot-specifiers))
  (assert (every #'(lambda (s)
		     (or (symbolp s) (and (consp s) (symbolp (car s)))))
		 slot-specifiers))
  (assert (every #'(lambda (x)
		     (and (consp x)
			  (member (car x) '(:default-initargs
					    :documentation
					    :metaclass))))
		 class-options))
  (assert (eql (length class-options)
	       (length (remove-duplicates class-options))))

  (let* ((default-initargs (rest (assoc :default-initargs class-options)))
	 (metaclass (or (second (assoc :metaclass class-options))
			'standard-class))
	 (doc (second (assoc :documentation class-options)))
	 (slot-names
	  (loop for slot-spec in slot-specifiers
		collect (cond
			 ((symbolp slot-spec) slot-spec)
			 (t (assert (consp slot-spec))
			    (assert (symbolp (car slot-spec)))
			    (car slot-spec)))))
	 (slot-options
	  (loop for slot-spec in slot-specifiers
		collect (if (consp slot-spec)
			    (cdr slot-spec)
			  nil)))
	 (readers 
	  (loop for slot-option in slot-options
		append (collect-properties slot-option :reader)))
	 (writers
	  (loop for slot-option in slot-options
		append (collect-properties slot-option :writer)))
	 (accessors
	  (loop for slot-option in slot-options
		append (collect-properties slot-option :accessor)))
	 (allocations
	  (loop for slot-option in slot-options
		collect (or (get slot-option :allocation)
			    :instance)))
	 (initargs
	  (loop for slot-option in slot-options
		collect (collect-properties slot-option :initarg)))
	 (types
	  (loop for slot-option in slot-options
		collect (collect-properties slot-option :type)))
	 (initforms
	  (loop for slot-option in slot-options
		collect (collect-properties slot-option :initform)))
	 (class-var-name
	  (intern (concatenate 'string "*CLASS-" (symbol-name class-name)
			       "-RETURNED-BY-DEFCLASS*")
		  (find-package :cl-test)))
	 )

    (declare (ignorable readers writers accessors allocations
			initargs types initforms default-initargs
			doc))

    (assert (loop for e in types always (< (length e) 2)))
    (assert (loop for e in initforms always (< (length e) 2)))

    (setf *defclass-slot-readers* (append readers *defclass-slot-readers*))
    (setf *defclass-slot-writers* (append writers *defclass-slot-writers*))
    (setf *defclass-slot-accessors*
	  (append accessors *defclass-slot-accessors*))

    ;;; Store away information about the class and its slots
    ;;; in a my-class object and associated my-slot objects.
    
    (let* ((my-slots
	   (loop for name in slot-names
		 for slot-option in slot-options
		 for readers = (collect-properties slot-option :reader)
		 for writers = (collect-properties slot-option :writer)
		 for accessors = (collect-properties slot-option :accessor)
		 for documentation = (getf slot-option :documentation)
		 for initarg-list in initargs
		 for type-list in types
		 for initform-list in initforms
		 for allocation in allocations
		 collect
		 (make-my-slot
		  :name name
		  :has-initform (notnot initform-list)
		  :initform (first initform-list)
		  :documentation documentation
		  :readers readers
		  :writers writers
		  :accessors accessors
		  :type (if type-list (first type-list) t)
		  )))
	  (my-class-obj
	   (make-my-class :name class-name
			  :direct-superclass-names superclasses
			  :default-initargs default-initargs
			  :documentation doc
			  :metaclass metaclass
			  :slots my-slots)))
      (setf (gethash class-name *my-classes*) my-class-obj))

    `(progn
       (declaim (special ,class-var-name))
       
       (report-and-ignore-errors (setq ,class-var-name
				       (defclass ,@(cdr args))))

       (deftest ,(make-defclass-test-name class-name "-DEFCLASS-RETURNS-CLASS")
	 (eqt (find-class ',class-name) ,class-var-name)
	 t)

       (deftest ,(make-defclass-test-name class-name
					  "-IS-IN-ITS-METACLASS")
	 (notnot-mv (typep (find-class ',class-name) ',metaclass))
	 t)

       ,@(when (eq metaclass 'standard-class)
	   `((deftest ,(make-defclass-test-name class-name
						"S-ARE-STANDARD-OBJECTS")
	       (subtypep* ',class-name 'standard-object)
	       t t)))

       ,@(loop for slot-name in slot-names
	       collect
	       `(deftest ,(make-defclass-test-name class-name
						   "-HAS-SLOT-NAMED-"
						   slot-name)
		  (notnot-mv (slot-exists-p (make-instance ',class-name)
					    ',slot-name))
		  t))

       (deftest ,(make-defclass-test-name class-name
					  "-ALLOCATE-INSTANCE")
	 (defclass-allocate-instance-test ',class-name ',slot-names)
	 nil)

       )))

(defun defclass-allocate-instance-test (class-name slot-names)
  (let* ((class (find-class class-name))
	 (instance (allocate-instance class)))
    (append
     (unless (eql (class-of instance) class)
       (list (list 'not-instance-of class-name)))
     (loop for slot in slot-names
	   when (slot-boundp instance slot)
	   collect (list 'is-bound slot))
     (loop for slot in slot-names
	   unless (equal (multiple-value-list
			   (notnot-mv (slot-exists-p instance slot)))
			  '(t))
	   collect (list 'does-not-exist slot))
     (let ((bad-slot '#:foo))
       (when (slot-exists-p instance bad-slot)
	 (list (list 'should-not-exist bad-slot))))
     )))
  
(defmacro generate-slot-tests ()
  "Generate generic tests from the read/writer/accessor functions
   for slots from defclass-with-tests."
  (let ((funs (remove-duplicates
	       (append *defclass-slot-readers*
		       *defclass-slot-writers*
		       *defclass-slot-accessors*))))
    `(progn
       (deftest class-readers/writers/accessors-are-generic-functions
	 (loop for sym in ',funs
	       unless (typep (symbol-function sym) 'generic-function)
	       collect sym)
	 nil)
       
       (deftest class-accessors-have-generic-setf-functions
	 (append
	  ,@(loop for sym in *defclass-slot-accessors*
		  collect
		  `(and (not (typep (function (setf ,sym))
				    'generic-function))
			'(,sym))))
	 nil))))

(defun my-compute-class-precedence-list (class-name)
  "Compute the class precdence list for classes defined using
   DEFCLASS-WITH-TESTS."
  (let ((class-names nil)
	(class-names-to-consider (list class-name))
	classes)
    ;; Find all classes
    (loop
     while class-names-to-consider
     do (let ((name (pop class-names-to-consider)))
	  (unless (member name class-names)
	    (push name class-names)
	    (let ((my-class (find-my-class name)))
	      (assert my-class)
	      (setq class-names-to-consider
		    (append (my-class-direct-superclass-names my-class)
			    class-names-to-consider))))))
    (setq class-names (reverse class-names))
    (assert (eq class-name (first class-names)))
    ;; class-names now contains class-name (which occurs first) and
    ;; the names of all its superclasses except T
    (setq classes (mapcar #'find-my-class class-names))
    ;; Walk the classes and set the predecessor links in the
    ;; class precedence DAG
    (loop for c in classes
	  for dsns = (my-class-direct-superclass-names c)
	  do (let ((pred c))
	       (loop for superclass-name in dsns
		     for superclass = (find-my-class superclass-name)
		     do (push pred (my-class-preds superclass))
		     do (pushnew superclass (my-class-succs pred))
		     do (incf (my-class-count superclass))
		     do (setq pred superclass))))
    ;; The list candidates will contain all the classes
    ;; for which the count is zero.  These are the candidates
    ;; for selection as the next class in the class precedence list
    (let ((candidates (loop for c in classes
			    when (zerop (my-class-count c))
			    collect c))
	  (n 0)
	  (result nil))
      (assert (equal candidates (list (first classes))))
      (loop
       while candidates
       do (let* ((next (first candidates))
		 (min-pred-index (my-class-min-pred-index next)))
	    (loop
	     for c in (rest candidates)
	     for c-min-pred-index = (my-class-min-pred-index c)
	     do
	     (cond
	      ((< c-min-pred-index min-pred-index)
	       (setq next c
		     min-pred-index c-min-pred-index))
	      (t (assert (not (= c-min-pred-index min-pred-index))))))
	    (setq candidates (remove next candidates))
	    (setf (my-class-index next) (incf n))
	    (push next result)
	    (loop
	     for succ in (my-class-succs next)
	     do (decf (my-class-count succ))
	     do (setf (my-class-min-pred-index succ)
		      (min (my-class-min-pred-index succ)
			   n))
	     do (when (zerop (my-class-count succ))
		  (push succ candidates)))))
      (assert (eql (length result) (length classes)))
      (setq result (reverse result))
      (mapcar #'my-class-name result))))
