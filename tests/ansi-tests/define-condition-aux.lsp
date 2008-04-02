;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar  9 05:40:13 2003
;;;; Contains: Auxiliary functions for testing DEFINE-CONDITION

(in-package :cl-test)

(defun make-def-cond-name (name &rest suffixes)
  (intern (apply #'concatenate 'string (string name) "/"
		 (mapcar #'string suffixes))
	  :cl-test))

(defmacro define-condition-with-tests (name-symbol
				       parents slot-specs &rest options)

  "Create a condition and some associated tests."

  (assert (symbolp name-symbol))
  (dolist (parent parents) (assert (symbolp parent)))
  
  (let ((name (symbol-name name-symbol)))
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (report-and-ignore-errors (eval '(define-condition ,name-symbol ,parents
				     ,slot-specs ,@options)))
     ,@(loop for parent in (adjoin 'condition parents)
	     collect
	     `(deftest ,(make-def-cond-name name "IS-SUBTYPE-OF/" parent)
		(subtypep* ',name-symbol ',parent)
		t t))
     ,@(loop for parent in (adjoin 'condition parents)
	     collect
	     `(deftest ,(make-def-cond-name name "IS-SUBTYPE-OF-2/" parent)
		(check-all-subtypep ',name-symbol ',parent)
		nil))
     ,@(loop for parent in (adjoin 'condition parents)
	     collect
	     `(deftest ,(make-def-cond-name name
					    "IS-NOT-SUPERTYPE-OF/" parent)
		(subtypep* ',parent ',name-symbol)
		nil t))
     ,@(loop for parent in (adjoin 'condition parents)
	     collect
	     `(deftest ,(make-def-cond-name name "IS-A/" parent)
		(let ((c (make-condition ',name-symbol)))
		  (notnot-mv (typep c ',parent)))
		t))
     ,@(loop for parent in (adjoin 'condition parents)
	     collect
	     `(deftest ,(make-def-cond-name name "IS-SUBCLASS-OF/" parent)
		(subtypep* (find-class ',name-symbol)
			   (find-class ',parent))
		t t))
     ,@(loop for parent in (adjoin 'condition parents)
	     collect
	     `(deftest ,(make-def-cond-name name
					    "IS-NOT-SUPERCLASS-OF/" parent)
		(subtypep* (find-class ',parent)
			   (find-class ',name-symbol))
		nil t))
     ,@(loop for parent in (adjoin 'condition parents)
	     collect
	     `(deftest ,(make-def-cond-name name "IS-A-MEMBER-OF-CLASS/"
					    parent)
		(let ((c (make-condition ',name-symbol)))
		  (notnot-mv (typep c (find-class ',parent))))
		t))
     (deftest ,(make-def-cond-name name "HANDLER-CASE-1")
       (let ((c (make-condition ',name-symbol)))
	 (handler-case (normally (signal c))
		       (,name-symbol (c1) (eqt c c1))))
       t)
     (deftest ,(make-def-cond-name name "HANDLER-CASE-2")
       (let ((c (make-condition ',name-symbol)))
	 (handler-case (normally (signal c))
		       (condition (c1) (eqt c c1))))
       t)
     ,@(unless (some #'(lambda (ct) (subtypep ct 'error)) parents)
	 `((deftest ,(make-def-cond-name name "HANDLER-CASE-3")
	     (let ((c (make-condition ',name-symbol)))
	       (handler-case (normally (signal c))
			     (error () nil)
			     (,name-symbol (c2) (eqt c c2))))
	     t)))
     )))
