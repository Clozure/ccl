;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jun 15 06:57:23 2003
;;;; Contains: Tests for SLOT-UNBOUND

(in-package :cl-test)

(defclass slot-unbound-class-01 ()
  ((a :reader sunb-a)
   (b :accessor sunb-b)
   (c :writer sunb-c)
   (e :reader sunb-e)
   (f :reader sunb-f)))

(defmethod slot-unbound ((class t) (obj slot-unbound-class-01) (slot-name t))
  (list (class-name class) slot-name))

(deftest slot-unbound.1
  (let ((obj (make-instance 'slot-unbound-class-01)))
    (values
     (slot-value obj 'a)
     (slot-value obj 'b)
     (slot-value obj 'c)))
  (slot-unbound-class-01 a)
  (slot-unbound-class-01 b)
  (slot-unbound-class-01 c))

(deftest slot-unbound.2
  (let ((obj (make-instance 'slot-unbound-class-01)))
    (values
     (sunb-a obj)
     (sunb-b obj)))
  (slot-unbound-class-01 a)
  (slot-unbound-class-01 b))

(defmethod slot-unbound ((class t) (obj slot-unbound-class-01)
			 (slot-name (eql 'e)))
  (values))

(defmethod slot-unbound ((class t) (obj slot-unbound-class-01)
			 (slot-name (eql 'f)))
  (values 1 2 3))

(deftest slot-unbound.3
  (slot-value (make-instance 'slot-unbound-class-01) 'e)
  nil)

(deftest slot-unbound.4
  (slot-value (make-instance 'slot-unbound-class-01) 'f)
  1)

(deftest slot-unbound.5
  (sunb-e (make-instance 'slot-unbound-class-01))
  nil)

(deftest slot-unbound.6
  (sunb-f (make-instance 'slot-unbound-class-01))
  1)
