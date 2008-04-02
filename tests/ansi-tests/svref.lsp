;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 22 21:39:30 2003
;;;; Contains: Tests of SVREF

(in-package :cl-test)

(deftest svref.1
  (let ((a (vector 1 2 3 4)))
    (loop for i below 4 collect (svref a i)))
  (1 2 3 4))

(deftest svref.2
  (let ((a (vector 1 2 3 4)))
    (values
     (loop for i below 4
	   collect (setf (svref a i) (+ i 10)))
     a))
  (10 11 12 13)
  #(10 11 12 13))

(deftest svref.order.1
  (let ((v (vector 'a 'b 'c 'd))
	(i 0) a b)
    (values
     (svref (progn (setf a (incf i)) v)
	    (progn (setf b (incf i)) 2))
     i a b))
  c 2 1 2)

(deftest svref.order.2
  (let ((v (vector 'a 'b 'c 'd))
	(i 0) a b c)
    (values
     (setf
      (svref (progn (setf a (incf i)) v)
	     (progn (setf b (incf i)) 2))
      (progn (setf c (incf i)) 'w))
     v i a b c))
  w #(a b w d) 3 1 2 3)


;;; Error tests

(deftest svref.error.1
  (signals-error (svref) program-error)
  t)

(deftest svref.error.2
  (signals-error (svref (vector 1)) program-error)
  t)

(deftest svref.error.3
  (signals-error (svref (vector 1) 0 0) program-error)
  t)

(deftest svref.error.4
  (signals-error (svref (vector 1) 0 nil) program-error)
  t)
