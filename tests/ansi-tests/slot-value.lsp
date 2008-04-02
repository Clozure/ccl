;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 10 16:16:59 2003
;;;; Contains: Tests of SLOT-VALUE

(in-package :cl-test)

;;; SLOT-VALUE is used extensively elsewhere.

(defclass slot-value-class-01 ()
  (a
   (b :type t)
   (c :type fixnum)
   (d :type float)
   (e :type symbol)
   (f :type short-float)
   (g :type single-float)
   (h :type double-float)
   (i :type long-float)
   (j :type rational)
   (k :type ratio)
   (l :type cons)
   (m :type string)
   (n :type vector)
   (o :type bit)
   ))

(defparameter *slot-value-test-slot-names*
  '(a b c d e f g h i j k l m n o))

(defparameter *slot-value-test-slot-values*
  '(t nil 10 4.0 a 1.0s0 2.0f0 3.0d0 4.0l0
      5/4 2/3 (a . b) "abcd" #(1 2 3 4) 1))

(deftest slot-value.1
  (let ((obj (make-instance 'slot-value-class-01))
	(slot-names *slot-value-test-slot-names*)
	(slot-values *slot-value-test-slot-values*))
    (loop for name in slot-names
	  for val in slot-values
	  unless (and (equal (multiple-value-list
			      (setf (slot-value obj name) val))
			     (list val))
		      (equal (multiple-value-list
			      (slot-value obj name))
			     (list val)))
	  collect name))
  nil)

(defclass slot-value-class-02 (slot-value-class-01)
  ((a :allocation :class)
   (b :allocation :class)
   (c :allocation :class)
   (d :allocation :class)
   (e :allocation :class)
   (f :allocation :class)
   (g :allocation :class)
   (h :allocation :class)
   (i :allocation :class)
   (j :allocation :class)
   (k :allocation :class)
   (l :allocation :class)
   (m :allocation :class)
   (n :allocation :class)
   (o :allocation :class)))

(deftest slot-value.2
  (let ((obj (make-instance 'slot-value-class-02))
	(slot-names *slot-value-test-slot-names*)
	(slot-values *slot-value-test-slot-values*))
    (loop for name in slot-names
	  for val in slot-values
	  unless (and (equal (multiple-value-list
			      (setf (slot-value obj name) val))
			     (list val))
		      (equal (multiple-value-list
			      (slot-value obj name))
			     (list val)))
	  collect name))
  nil)

;;; Order of evaluation test(s)

(deftest slot-value.order.1
  (let ((obj (make-instance 'slot-value-class-01))
	(i 0) x y)
    (values
     (setf (slot-value obj 'a) t)
     (slot-value (progn (setf x (incf i)) obj)
		 (progn (setf y (incf i)) 'a))
     i x y))
  t t 2 1 2)

(deftest slot-value.order.2
  (let ((obj (make-instance 'slot-value-class-01))
	(i 0) x y)
    (values
     (setf (slot-value (progn (setf x (incf i)) obj)
		       (progn (setf y (incf i)) 'b))
	   t)
     (slot-value obj 'b)
     i x y))
  t t 2 1 2)

;;; Error tests

(deftest slot-value.error.1
  (signals-error (slot-value) program-error)
  t)

(deftest slot-value.error.2
  (signals-error (slot-value (make-instance 'slot-value-class-01))
		 program-error)
  t)

(deftest slot-value.error.3
  (signals-error
   (let ((obj (make-instance 'slot-value-class-01)))
     (setf (slot-value obj 'a) t)
     (slot-value obj 'a nil))
   program-error)
  t)

(deftest slot-value.error.4
  (handler-case
   (progn (slot-value (make-instance 'slot-value-class-01) (gensym))
	  :bad)
   (error () :good))
  :good)

(deftest slot-value.error.5
  (let ((built-in-class (find-class 'built-in-class))
	(slot-name (gensym)))
    (check-predicate
     #'(lambda (e)
	 (let ((class (class-of e)))
	   (or (not (eq (class-of class) built-in-class))
	       (handler-case (progn (slot-value e slot-name) nil)
			     (error () t)))))))
  nil)

(deftest slot-value.error.6
  (let ((built-in-class (find-class 'built-in-class))
	(slot-name (gensym)))
    (check-predicate
     #'(lambda (e)
	 (let ((class (class-of e)))
	   (or (not (eq (class-of class) built-in-class))
	       (handler-case (setf (slot-value e slot-name) nil)
				  (error () t)))))))
  nil)
