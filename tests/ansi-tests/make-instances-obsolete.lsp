;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 17 08:12:35 2003
;;;; Contains: Tests of MAKE-INSTANCES-OBSOLETE

(in-package :cl-test)

(defclass make-instances-obsolete-class-01 ()
  ((a :initarg :a)
   (b :initarg :b :allocation :class)
   (c :initarg :c :initform 'abc)
   (d :initarg :d :type fixnum :initform 0)))

(deftest make-instances-obsolete.1
  (let* ((class-designator 'make-instances-obsolete-class-01)
	 (class (find-class class-designator))
	 (obj (make-instance class :a 'x :b 'y :c 'z :d 17)))
    (values
     (eqt (class-of obj) class)
     (map-slot-value obj '(a b c d))
     (let ((val (make-instances-obsolete class)))
       (or (eqt val class-designator)
	   (eqt val class)))
     (map-slot-value obj '(a b c d))))
  t (x y z 17) t (x y z 17))

(deftest make-instances-obsolete.2
  (let* ((class-designator 'make-instances-obsolete-class-01)
	 (class (find-class class-designator))
	 (obj (make-instance class :a 'x :b 'y :c 'z :d 17)))
    (values
     (eqt (class-of obj) class)
     (map-slot-value obj '(a b c d))
     (let ((val (make-instances-obsolete class-designator)))
       (or (eqt val class-designator)
	   (eqt val class)))
     (map-slot-value obj '(a b c d))))
  t (x y z 17) t (x y z 17))

;;; Error cases

(deftest make-instances-obsolete.error.1
  (signals-error (make-instances-obsolete) program-error)
  t)

(deftest make-instances-obsolete.error.2
  (signals-error (make-instances-obsolete
		   (find-class 'make-instances-obsolete-class-01)
		   nil)
		 program-error)
  t)
