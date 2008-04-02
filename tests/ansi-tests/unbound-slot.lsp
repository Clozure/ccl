;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jun  4 20:14:26 2003
;;;; Contains: Tests for UNBOUND-SLOT, UNBOUND-SLOT-INSTANCE

(in-package :cl-test)

(defclass ubs-class-01 ()
  ((a :initarg :a)))

(deftest unbound-slot.1
  (let ((obj (make-instance 'ubs-class-01)))
    (handler-case
     (slot-value obj 'a)
     (unbound-slot (c)
		   (values
		    (typep* c 'cell-error)
		    (eqt (unbound-slot-instance c) obj)
		    (cell-error-name c)))))
  t t a)

(defclass ubs-class-02 ()
  ((b :allocation :class)))

(deftest unbound-slot.2
  (let ((obj (make-instance 'ubs-class-02)))
    (handler-case
     (slot-value obj 'b)
     (unbound-slot (c)
		   (values
		    (typep* c 'cell-error)
		    (eqt (unbound-slot-instance c) obj)
		    (cell-error-name c)))))
  t t b)


