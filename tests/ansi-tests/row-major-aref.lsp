;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 22 20:16:38 2003
;;;; Contains: Tests of ROW-MAJOR-AREF

(in-package :cl-test)

;;; ROW-MAJOR-AREF is also used by equalp-with-case (see rt/rt.lsp)

(deftest row-major-aref.1
  (loop for i from 0 to 5 collect (row-major-aref #(a b c d e f) i))
  (a b c d e f))

(deftest row-major-aref.2
  (loop for i from 0 to 5 collect (row-major-aref #2a((a b c d)(e f g h)) i))
  (a b c d e f))

(deftest row-major-aref.3
  (row-major-aref #0a100 0)
  100)

(deftest row-major-aref.4
  (loop for i from 0 to 5 collect (row-major-aref #*011100 i))
  (0 1 1 1 0 0))

(deftest row-major-aref.5
  (loop for i from 0 to 5 collect (row-major-aref "abcdef" i))
  (#\a #\b #\c #\d #\e #\f))

(deftest row-major-aref.6
  (let ((a (make-array nil :initial-element 'x)))
    (values
     (aref a)
     (setf (row-major-aref a 0) 'y)
     (aref a)
     a))
  x y y #0ay)

(deftest row-major-aref.7
  (let ((a (make-array '(4) :initial-element 'x)))
    (values
     (aref a 0)
     (aref a 1)
     (aref a 2)
     (aref a 3)
     (setf (row-major-aref a 0) 'a)
     (setf (row-major-aref a 1) 'b)
     (setf (row-major-aref a 2) 'c)
     a))
  x x x x a b c #(a b c x))

(deftest row-major-aref.8
  (let ((a (make-array '(4) :element-type 'base-char
		       :initial-element #\x)))
    (values
     (aref a 0)
     (aref a 1)
     (aref a 2)
     (aref a 3)
     (setf (row-major-aref a 0) #\a)
     (setf (row-major-aref a 1) #\b)
     (setf (row-major-aref a 2) #\c)
     a))
  #\x #\x #\x #\x #\a #\b #\c "abcx")

(deftest row-major-aref.9
  (let ((a (make-array '(4) :initial-element 0
		       :element-type 'bit)))
    (values
     (aref a 0)
     (aref a 1)
     (aref a 2)
     (aref a 3)
     (setf (row-major-aref a 0) 1)
     (setf (row-major-aref a 1) 1)
     (setf (row-major-aref a 3) 1)
     a))
  0 0 0 0 1 1 1 #*1101)

(deftest row-major-aref.10
  (let ((a (make-array '(2 3 4)
		       :initial-contents '(((a b c d)(e f g h)(i j k l))
					   ((m n o p)(q r s t)(u v w x))))))
    (loop for i from 0 to 23 collect (row-major-aref a i)))
  (a b c d e f g h i j k l m n o p q r s t u v w x))

(deftest row-major-aref.order.1
  (let ((i 0) x y)
    (values
     (row-major-aref
      (progn (setf x (incf i)) #(a b c d e f))
      (progn (setf y (incf i)) 2))
     i x y))
  c 2 1 2)

(deftest row-major-aref.order.2
  (let ((i 0) x y z
	(a (copy-seq #(a b c d e f))))
    (values
     (setf
      (row-major-aref
       (progn (setf x (incf i)) a)
       (progn (setf y (incf i)) 2))
      (progn (setf z (incf i)) 'w))
     a i x y z))
  w #(a b w d e f) 3 1 2 3)

;;; Error tests

(deftest row-major-aref.error.1
  (signals-error (row-major-aref) program-error)
  t)
