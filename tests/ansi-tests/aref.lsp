;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Feb 11 17:33:24 2003
;;;; Contains: Tests for AREF

(in-package :cl-test)

;;; AREF is also tested in many other places

(deftest aref.1
  (aref #0aT)
  T)

(deftest aref.2
  (aref #(1 2 3 4) 2)
  3)

(deftest aref.3
  (aref #2a((a b c d)(e f g h))  1 2)
  g)

(deftest aref.4
  (loop for i from 0 below 6 collect (aref "abcdef" i))
  (#\a #\b #\c #\d #\e #\f))

(deftest aref.5
  (let ((a (make-array '(2 3) :element-type 'base-char
		       :initial-contents '("abc" "def"))))
    (loop for i below 2
	  collect (loop for j below 3
			collect (aref a i j))))
  ((#\a #\b #\c)
   (#\d #\e #\f)))

(deftest aref.6
  (loop for i below 10 collect (aref #*1101100010 i))
  (1 1 0 1 1 0 0 0 1 0))

(deftest aref.7
  (let ((a (make-array '(2 5) :element-type 'bit
		       :initial-contents '((1 1 0 0 1)
					   (0 1 0 1 0)))))
    (loop for i below 2
	  collect (loop for j below 5
			collect (aref a i j))))
  ((1 1 0 0 1)
   (0 1 0 1 0)))

;;; Order of argument evaluation

(deftest aref.order.1
  (let ((i 0) x y (a #(a b c d)))
    (values
     (aref (progn (setf x (incf i)) a)
	   (progn (setf y (incf i)) 2))
     i x y))
  c 2 1 2)

(deftest aref.order.2
  (let ((i 0) x y z (a #2a((a b c)(d e f))))
    (values
     (aref (progn (setf x (incf i)) a)
	   (progn (setf y (incf i)) 1)
	   (progn (setf z (incf i)) 2))
     i x y z))
  f 3 1 2 3)


;;; Setf of aref

(deftest setf-aref.1
  (let ((a (copy-seq #(1 2 3 4))))
    (values
     (setf (aref a 2) 'z)
     a))
  z
  #(1 2 z 4))

(deftest setf-aref.2
  (let ((a (make-array nil :initial-element 1)))
    (values
     (setf (aref a) 'z)
     a))
  z #0az)

(deftest setf-aref.3
  (let ((a (make-array '(2 3) :initial-element 'a)))
    (values
     (setf (aref a 0 1) 'z)
     a))
  z
  #2a((a z a)(a a a)))

(deftest setf-aref.4
  (let ((a (copy-seq "abcd")))
    (values
     (setf (aref a 0) #\z)
     a))
  #\z
  "zbcd")

(deftest setf-aref.5
  (let ((a (copy-seq #*0011)))
    (values
     (setf (aref a 0) 1)
     a))
  1
  #*1011)

(deftest setf-aref.6
  (let ((a (make-array '(2 3) :initial-element #\a :element-type 'base-char)))
    (values
     (setf (aref a 0 1) #\z)
     a))
  #\z
  #2a((#\a #\z #\a)(#\a #\a #\a)))

(deftest setf-aref.7
  (let ((a (make-array '(2 3) :initial-element 1 :element-type 'bit)))
    (values
     (setf (aref a 0 1) 0)
     a))
  0
  #2a((1 0 1)(1 1 1)))

(deftest setf-aref.order.1
  (let ((i 0) x y z (a (copy-seq #(a b c d))))
    (values
     (setf (aref (progn (setf x (incf i)) a)
		 (progn (setf y (incf i)) 2))
	   (progn (setf z (incf i)) 'z))
     a
     i x y z))
  z #(a b z d) 3 1 2 3)

;;; To add: aref on displaced arrays, arrays with fill pointers, etc.

(deftest aref.special-integer.1
  (do-special-integer-vectors
   (v #(1 1 0 1 0 1) nil)
   (assert (= (aref v 0) 1))
   (assert (= (aref v 1) 1))
   (assert (= (aref v 2) 0))
   (assert (= (aref v 3) 1))
   (assert (= (aref v 4) 0))
   (assert (= (aref v 5) 1)))
  nil)

(deftest aref.special-strings.1
  (do-special-strings
   (s "ABCDE" nil)
   (assert (eql (aref s 0) #\A))
   (assert (eql (aref s 1) #\B))
   (assert (eql (aref s 2) #\C))
   (assert (eql (aref s 3) #\D))
   (assert (eql (aref s 4) #\E)))
  nil)

;;; Error tests

(deftest aref.error.1
  (signals-error (aref) program-error)
  t)

(deftest aref.error.2
  (signals-error (funcall #'aref) program-error)
  t)

