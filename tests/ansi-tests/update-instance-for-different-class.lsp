;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon May  5 19:32:56 2003
;;;; Contains: Tests for UPDATE-INSTANCE-FOR-DIFFERENT-CLASS

(in-package :cl-test)

(defclass uifdc-class-01a () ((a :initarg :a) (b :initarg :b)))
(defclass uifdc-class-01b () (a b))

(declaim (special *uifdc-01-obj*))

(defmethod update-instance-for-different-class
  ((from-obj uifdc-class-01a)
   (to-obj uifdc-class-01b)
   &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (assert (not (eq *uifdc-01-obj* from-obj)))
  (assert (eq *uifdc-01-obj* to-obj))
  (if (slot-boundp from-obj 'a)
    (setf (slot-value to-obj 'b)
	  (slot-value from-obj 'a))
    (slot-makunbound to-obj 'b))
  (if (slot-boundp from-obj 'b)
    (setf (slot-value to-obj 'a)
	  (slot-value from-obj 'b))
    (slot-makunbound to-obj 'a))
  to-obj)

(deftest update-instance-for-different-class.1
  (let* ((obj (make-instance 'uifdc-class-01a))
	 (new-class (find-class 'uifdc-class-01b))
	 (*uifdc-01-obj* obj))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj new-class)
     (map-slot-boundp* obj '(a b))))
  (nil nil)
  t t
  (nil nil))

(deftest update-instance-for-different-class.2
  (let* ((obj (make-instance 'uifdc-class-01a :a 1))
	 (new-class (find-class 'uifdc-class-01b))
	 (*uifdc-01-obj* obj))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj new-class)
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'b)))
  (t nil)
  t t
  (nil t)
  1)

(deftest update-instance-for-different-class.3
  (let* ((obj (make-instance 'uifdc-class-01a :b 1))
	 (new-class (find-class 'uifdc-class-01b))
	 (*uifdc-01-obj* obj))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj new-class)
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)))
  (nil t)
  t t
  (t nil)
  1)

(deftest update-instance-for-different-class.4
  (let* ((obj (make-instance 'uifdc-class-01a :a 1 :b 2))
	 (new-class (find-class 'uifdc-class-01b))
	 (*uifdc-01-obj* obj))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj new-class)
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (t t)
  t t
  (t t)
  2 1)


;;; after method

(defclass uifdc-class-02 () ((a :initform 'x :initarg :a)
			      (b :initarg :b)))

(defmethod update-instance-for-different-class :after
  ((from-obj uifdc-class-01a)
   (to-obj uifdc-class-02)
   &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value to-obj 'a) 100)
  to-obj)

(deftest update-instance-for-different-class.5
  (let* ((obj (make-instance 'uifdc-class-01a))
	 (class (find-class 'uifdc-class-02)))
    (values
     (eqt obj (change-class obj class))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)))
  t (t nil) 100)

(deftest update-instance-for-different-class.6
  (let* ((obj (make-instance 'uifdc-class-01a :a 1))
	 (class (find-class 'uifdc-class-02)))
    (values
     (eqt obj (change-class obj class))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)))
  t (t nil) 100)

(deftest update-instance-for-different-class.7
  (let* ((obj (make-instance 'uifdc-class-01a :b 17))
	 (class (find-class 'uifdc-class-02)))
    (values
     (eqt obj (change-class obj class))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  t (t t) 100 17)

(deftest update-instance-for-different-class.8
  (let* ((obj (make-instance 'uifdc-class-01a :b 17 :a 4))
	 (class (find-class 'uifdc-class-02)))
    (values
     (eqt obj (change-class obj class))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  t (t t) 100 17)




