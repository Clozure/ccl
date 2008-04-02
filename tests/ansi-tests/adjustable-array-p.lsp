;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 20 21:25:22 2003
;;;; Contains: Tests for ADJUSTABLE-ARRAY-P

(in-package :cl-test)

(deftest adjustable-array-p.1
  (notnot (adjustable-array-p (make-array '(5) :adjustable t)))
  t)

(deftest adjustable-array-p.2
  (notnot (adjustable-array-p (make-array nil :adjustable t)))
  t)

(deftest adjustable-array-p.3
  (notnot (adjustable-array-p (make-array '(2 3) :adjustable t)))
  t)

(deftest adjustable-array-p.4
  (notnot (adjustable-array-p (make-array '(2 2 2) :adjustable t)))
  t)

(deftest adjustable-array-p.5
  (notnot (adjustable-array-p (make-array '(2 2 2 2) :adjustable t)))
  t)

(deftest adjustable-array-p.6
  (macrolet ((%m (z) z))
	    (let ((a (make-array '(5) :adjustable t)))
	      (notnot (adjustable-array-p (expand-in-current-env (%m a))))))
  t)

(deftest adjustable-array-p.order.1
  (let ((i 0) x)
    (values
     (notnot (adjustable-array-p (progn (setf x (incf i))
					(make-array '(5) :adjustable t))))
     i x))
  t 1 1)

;;; Error tests

(deftest adjustable-array-p.error.1
  (signals-error (adjustable-array-p) program-error)
  t)

(deftest adjustable-array-p.error.2
  (signals-error (adjustable-array-p "aaa" nil) program-error)
  t)

(deftest adjustable-array-p.error.3
  (signals-type-error x 10 (adjustable-array-p x))
  t)

(deftest adjustable-array-p.error.4
  (check-type-error #'adjustable-array-p #'arrayp)
  nil)

(deftest adjustable-array-p.error.5
  (signals-error (locally (adjustable-array-p 10)) type-error)
  t)

(deftest adjustable-array-p.error.6
  (signals-error (let ((x 10))
		    (locally (declare (optimize (safety 3)))
			   (adjustable-array-p x)))
		 type-error)
  t)
