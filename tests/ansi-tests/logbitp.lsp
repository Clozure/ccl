;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Sep  9 07:02:00 2003
;;;; Contains: Tests of LOGBITP

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

;;; Error tests

(deftest logbitp.error.1
  (signals-error (logbitp) program-error)
  t)

(deftest logbitp.error.2
  (signals-error (logbitp 0) program-error)
  t)

(deftest logbitp.error.3
  (signals-error (logbitp 0 0 0) program-error)
  t)

(deftest logbitp.error.4
  (check-type-error #'(lambda (x) (logbitp x 0)) (typef 'unsigned-byte))
  nil)

(deftest logbitp.error.5
  (check-type-error #'(lambda (x) (logbitp 0 x)) #'integerp)
  nil)

;;; Non-error tests

(deftest logbitp.1
  (loop for x in *integers*
	unless (if (logbitp 0 x) (oddp x) (evenp x))
	collect x)
  nil)

(deftest logbitp.2
  (loop for len from 0 to 300
	for i = (ash 1 len)
	always (and (logbitp len i)
		    (loop for j from 0 to 300
			  always (or (eql j len)
				     (not (logbitp j i))))))
  t)

(deftest logbitp.3
  (logbitp most-positive-fixnum 0)
  nil)
	
(deftest logbitp.4
  (notnot-mv (logbitp most-positive-fixnum -1))
  t)

(deftest logbitp.5
  (logbitp (1+ most-positive-fixnum) 0)
  nil)
	
(deftest logbitp.6
  (notnot-mv (logbitp (1+ most-positive-fixnum) -1))
  t)

(deftest logbitp.7
  (loop for len = (random 100)
	for i = (random-from-interval (ash 1 len))
	for k = (random (1+ len))
	repeat 1000
	unless (if (ldb-test (byte 1 k) i)
		   (logbitp k i)
		 (not (logbitp k i)))
	collect (list i k))
  nil)

(deftest logbitp.8
  (loop for k from 1 to 1000
	always (logbitp k -1))
  t)

(deftest logbitp.order.1
  (let ((i 0) a b)
    (values
     (logbitp (progn (setf a (incf i)) 2)
	      (progn (setf b (incf i)) #b111010))
     i a b))
  nil 2 1 2)




	
	
