;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jun 23 11:54:10 2005
;;;; Contains: Tests of MAKE-CONDITION

(in-package :cl-test)


(deftest make-condition.1
  (loop for tp in *cl-condition-type-symbols*
	for c = (make-condition tp)
	unless (and (typep c tp)
		    (typep c 'condition))
	collect (list tp c))
  nil)

(deftest make-condition.2
  (loop for tp in *cl-condition-type-symbols*
	for class = (find-class tp)
	for c = (and class (make-condition class))
	unless (or (not class)
		   (and (typep c tp)
			(typep c class)
			(typep c 'condition)))
	collect (list tp c))
  nil)

(deftest make-condition.3
  :notes (:make-condition-with-compound-name :ansi-spec-problem)
  (let* ((tp '(or program-error type-error))
	 (c (make-condition tp)))
    (or (not (and (subtypep tp 'condition)
		  (or (subtypep 'program-error tp)
		      (subtypep 'type-error tp))))
	(notnot-mv (typep c tp))))
  t)

(deftest make-condition.4
  :notes (:make-condition-with-compound-name :ansi-spec-problem)
  (let* ((tp '(and simple-error type-error))
	 (c (make-condition tp)))
    (or (not (and (subtypep 'simple-error tp)
		  (subtypep 'type-error tp)
		  (subtypep tp 'condition)))
	(notnot-mv (typep c tp))))
  t)

;;; Error tests

(deftest make-condition.error.1
  (signals-error (make-condition) program-error)
  t)
