;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jun  6 18:15:50 2003
;;;; Contains: Generator class and associated generic function definitions

(in-package :rctest)

(compile-and-load "rctest-util.lsp")

(defvar *prototype-class-table* (make-hash-table)
  "Contains a map from names of classes to prototype instances
   for those classes.")

(defgeneric prototype (class)
  ;; Map a class to a prototype instance of the class.  Cache using
  ;; *prototype-class-table*.
  (:method ((class standard-class) &aux (name (class-name class)))
	   (or (gethash name *prototype-class-table*)
	       (setf (gethash name *prototype-class-table*)
		     (make-instance class))))
  (:method ((class symbol))
	   (prototype (find-class class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generators are objects that are used to create random instances.

(defclass generator () ())

(defclass composite-generator (generator)
  ((subgenerators :type array :initform (make-array '(10)
						    :adjustable t
						    :fill-pointer 0))
   (cumulative-weights :type array
		       :initform (make-array '(10)
					     :fill-pointer 0
					     :adjustable t
					     :element-type 'single-float
					     :initial-element 0.0f0))
   ))

(defclass simple-generator (generator) ())

(defgeneric generate (gen size &rest ctxt &key &allow-other-keys)
  (:method
   ((gen composite-generator) (size real) &rest ctxt)
   (let* ((subgens (slot-value gen 'subgenerators))
	  (n (fill-pointer subgens)))
     (when (<= n 0) (return-from generate (values nil nil)))
     (let* ((cum-weights (slot-value gen 'cumulative-weights))
	    (total-weight (aref cum-weights (1- n)))
	    (random-weight (random total-weight))
	    ;; Replace POSITION call with a binary search if necessary
	    (index (position random-weight cum-weights :test #'>=)))
       (loop for i from 1 to 10
	     do (multiple-value-bind (val success?)
		    (apply #'generate (aref subgens index) size ctxt)
		  (when success? (return (values val t))))
	     finally (return (values nil nil))))))
  )

(defmethod generate ((gen symbol) size &rest ctxt &key &allow-other-keys)
  (apply #'generate (prototype gen) size ctxt))

(defgeneric add-subgenerator (gen subgen weight)
  (:method
   ((gen composite-generator) (subgen generator) weight)
   (let* ((subgens (slot-value gen 'subgenerators))
	  (n (fill-pointer subgens))
	  (cum-weights (slot-value gen 'cumulative-weights))
	  (total-weight (if (> n 0) (aref cum-weights (1- n)) 0.0f0)))
     (vector-push-extend gen subgens n)
     (vector-push-extend (+ total-weight weight) cum-weights n)
     (values))))

(defclass iterative-generator (generator)
  ((subgenerator :initarg :sub)))

(defclass random-iterative-generator (iterative-generator) ())

(defmethod generate ((gen random-iterative-generator) size &rest ctxt)
  (if (<= size 1)
      nil
    (let ((subgen (slot-value gen 'subgenerator))
	  (subsizes (randomly-partition (1- size) (min (isqrt size) 10))))
      (loop for subsize in subsizes
	    for (element success) = (multiple-value-list
				     (apply #'generate subgen subsize ctxt))
	    when success collect element))))

;;; Macro for defining simple generator objects
;;; BODY is the body of the method with arguments (gen ctxt size)
;;; for computing the result.  Inside the body the function FAIL causes
;;; the generator to return (nil nil).

(defmacro defgenerator (name &key
			     keys
			     body
			     (superclass 'simple-generator)
			     slots)
  (let ((rtag (gensym)))
    (unless (listp keys) (setf keys (list keys)))
    `(progn
       (defclass ,name (,superclass) ,slots)
       (defmethod generate ((gen ,name) (size real) &rest ctxt &key ,@keys)
	 (declare (ignorable gen size ctxt))
	 (block ,rtag
	   (flet ((fail () (return-from ,rtag (values nil nil))))
	     ,body))))))
