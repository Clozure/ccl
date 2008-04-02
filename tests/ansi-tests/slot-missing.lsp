;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jun 15 06:03:58 2003
;;;; Contains: Tests of SLOT-MISSING

(in-package :cl-test)

(defparameter *slot-missing-class-01-var* nil)

(defclass slot-missing-class-01 () (a b c))

(defmethod slot-missing ((class t) (obj slot-missing-class-01)
			 (slot-name t) (operation t)
			 &optional (new-value nil new-value-p))
  (setf *slot-missing-class-01-var*
	(list slot-name operation new-value (notnot new-value-p))))

(deftest slot-missing.1
  (let ((obj (make-instance 'slot-missing-class-01)))
    (values
     (slot-value obj 'foo)
     *slot-missing-class-01-var*))
  (foo slot-value nil nil)
  (foo slot-value nil nil))

(deftest slot-missing.2
  (let ((obj (make-instance 'slot-missing-class-01)))
    (values
     (setf (slot-value obj 'foo) 'bar)
     *slot-missing-class-01-var*))
  bar
  (foo setf bar t))

(deftest slot-missing.3
  (let ((obj (make-instance 'slot-missing-class-01)))
    (values
     (eqt obj (slot-makunbound obj 'xyz))
     *slot-missing-class-01-var*))
  t
  (xyz slot-makunbound nil nil))

(deftest slot-missing.4
  (let ((obj (make-instance 'slot-missing-class-01)))
    (values
     (notnot (slot-boundp obj 'abc))
     *slot-missing-class-01-var*))
  t
  (abc slot-boundp nil nil))

(deftest slot-missing.5
  (let ((obj (make-instance 'slot-missing-class-01)))
    (slot-value obj 'd))
  (d slot-value nil nil))

(deftest slot-missing.6
  (let ((obj (make-instance 'slot-missing-class-01)))
    (setf (slot-value obj 'd) 'bar))
  bar)

(deftest slot-missing.7
  (let* ((obj (make-instance 'slot-missing-class-01))
	 (val (slot-makunbound obj 'd)))
    (if (eq val obj)
	:good
      val))
  :good)

(defmethod slot-missing ((class t) (obj slot-missing-class-01)
			 (slot-name (eql 'not-there))
			 (operation (eql 'slot-boundp))
			 &optional new-value)
  (declare (ignore new-value))
  (values nil :ignore-this))

(deftest slot-missing.8
  (let* ((obj (make-instance 'slot-missing-class-01)))
    (slot-boundp obj 'not-there))
  nil)
