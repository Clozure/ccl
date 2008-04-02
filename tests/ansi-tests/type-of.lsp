;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jun  4 21:15:05 2003
;;;; Contains: Tests of TYPE-OF

(in-package :cl-test)

;;;  It turns out I left out an important test of type-of:
;;;  (type-of x) must be a recognizable subtype of every builtin type
;;;  of which x is a member.

(deftest type-of.1
  :notes :type-of/strict-builtins
  (loop for x in *universe*
	for tp = (type-of x)
	for failures = (loop for tp2 in *cl-all-type-symbols*
			     when (and (typep x tp2)
				       (not (subtypep tp tp2)))
			     collect tp2)
	when failures collect (list x failures))
  nil)

;;; Some have objected to that (in type-of.1) interpretation
;;; of req. 1.a in the TYPE-OF page, saying that it need hold
;;; for only *one* builtin type that the object is an element of.
;;; This test tests the relaxed requirement.

(deftest type-of.1-relaxed
  (loop for x in *universe*
	for builtins = (remove x *cl-all-type-symbols*
			       :test (complement #'typep))
	for tp = (type-of x)
	when (and builtins
		  (not (loop for tp2 in builtins
			     thereis (subtypep tp tp2))))
	collect x)
  nil)

;;; 1. For any object that is an element of some built-in type:
;;;  b. the type returned does not involve and, eql, member, not,
;;;     or, satisfies, or values.
;;;
;;; Since every object is an element of the built-in type T, this
;;; applies universally.

(deftest type-of.2
  (loop for x in *universe*
	for tp = (type-of x)
	when (and (consp tp)
		  (member (car tp) '(and eql member not or satisfies values
					 function)))
	collect x)
  nil)

(deftest type-of.3
  (loop for x in *universe*
	unless (typep x (type-of x))
	collect x)
  nil)

(deftest type-of.4
  (loop for x in *universe*
	for tp = (type-of x)
	for class = (class-of x)
	unless (equal (multiple-value-list (subtypep* tp class)) '(t t))
	collect x)
  nil)

(deftest type-of.5
  (loop for x in *cl-condition-type-symbols*
	for cnd = (make-condition x)
	for tp = (type-of cnd)
	unless (eq x tp)
	collect x)
  nil)

(defstruct type-of.example-struct a b c)

(deftest type-of.6
  (type-of (make-type-of.example-struct))
  type-of.example-struct)

(defclass type-of.example-class () ())

(deftest type-of.7
  (type-of (make-instance 'type-of.example-class))
  type-of.example-class)

(deftest type-of.8
  (let ((class (eval '(defclass type-of.example-class-2 () ((a) (b) (c))))))
    (setf (class-name class) nil)
    (eqt (type-of (make-instance class)) class))
  t)

(deftest type-of.9
  (let ((class (eval '(defclass type-of.example-class-3 () ((a) (b) (c))))))
    (setf (find-class 'type-of.example-class-3) nil)
    (eqt (type-of (make-instance class)) class))
  t)

(deftest type-of.10
  (let* ((class (eval '(defclass type-of.example-class-4 () ((a) (b) (c)))))
	 (obj (make-instance class)))
    (setf (class-name class) nil)
    (notnot-mv (typep obj class)))
  t)

(deftest type-of.11
  (let* ((c #c(-1 1/2))
	 (type (type-of c)))
    (notnot (typep c type)))
  t)

;;; Error tests

(deftest type-of.error.1
  (signals-error (type-of) program-error)
  t)

(deftest type-of.error.2
  (signals-error (type-of nil nil) program-error)
  t)  

