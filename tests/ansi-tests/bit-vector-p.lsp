;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 20:16:50 2003
;;;; Contains: Tests of BIT-VECTOR-P

(in-package :cl-test)

(deftest bit-vector-p.2
  (notnot-mv (bit-vector-p #*))
  t)
  
(deftest bit-vector-p.3
  (notnot-mv (bit-vector-p #*00101))
  t)

(deftest bit-vector-p.4
  (bit-vector-p #(0 1 1 1 0 0))
  nil)

(deftest bit-vector-p.5
  (bit-vector-p "011100")
  nil)

(deftest bit-vector-p.6
  (bit-vector-p 0)
  nil)

(deftest bit-vector-p.7
  (bit-vector-p 1)
  nil)

(deftest bit-vector-p.8
  (bit-vector-p nil)
  nil)

(deftest bit-vector-p.9
  (bit-vector-p 'x)
  nil)

(deftest bit-vector-p.10
  (bit-vector-p '(0 1 1 0))
  nil)

(deftest bit-vector-p.11
  (bit-vector-p (make-array '(2 2) :element-type 'bit
			    :initial-element 0))
  nil)

(deftest bit-vector-p.12
  (check-type-predicate #'bit-vector-p 'bit-vector)
  nil)

(deftest bit-vector-p.13
  (macrolet
   ((%m (z) z))
   (values (notnot (bit-vector-p (expand-in-current-env (%m #*110101))))
	   (bit-vector-p (expand-in-current-env (%m nil)))))
  t nil)

(deftest bit-vector-p.order.1
  (let ((i 0) x)
    (values
     (notnot (bit-vector-p (progn (setf x (incf i)) #*0010)))
     i x))
  t 1 1)

(deftest bit-vector-p.order.2
  (let ((i 0) x)
    (values
     (bit-vector-p (progn (setf x (incf i)) 'a))
     i x))
  nil 1 1)


(deftest bit-vector-p.error.1
  (signals-error (bit-vector-p) program-error)
  t)

(deftest bit-vector-p.error.2
  (signals-error (bit-vector-p #* #*) program-error)
  t)
