;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan  1 18:43:46 2005
;;;; Contains: Tests of READTABLE-CASE

(in-package :cl-test)

(deftest readtable-case.1
  (with-standard-io-syntax
   (readtable-case *readtable*))
  :upcase)

(deftest readtable-case.2
  (with-standard-io-syntax
   (let ((rt (copy-readtable)))
     (readtable-case rt)))
  :upcase)

(deftest readtable-case.3
  (let ((rt (copy-readtable)))
    (values
     (setf (readtable-case rt) :upcase)
     (readtable-case rt)))
  :upcase :upcase)

(deftest readtable-case.4
  (let ((rt (copy-readtable)))
    (values
     (setf (readtable-case rt) :downcase)
     (readtable-case rt)))
  :downcase :downcase)

(deftest readtable-case.5
  (let ((rt (copy-readtable)))
    (values
     (setf (readtable-case rt) :preserve)
     (readtable-case rt)))
  :preserve :preserve)

(deftest readtable-case.6
  (let ((rt (copy-readtable)))
    (values
     (setf (readtable-case rt) :invert)
     (readtable-case rt)))
  :invert :invert)

(deftest readtable-case.7
  (let ((rt (copy-readtable)))
    (loop for rtc in '(:upcase :downcase :preserve :invert)
	  do (setf (readtable-case rt) rtc)
	  nconc (let ((rt2 (copy-readtable rt)))
		  (unless (eq (readtable-case rt2) rtc)
		    (list rtc rt2)))))
  nil)

;;; Error cases

(deftest readtable-case.error.1
  (signals-error (readtable-case) program-error)
  t)

(deftest readtable-case.error.2
  (signals-error (readtable-case *readtable* nil) program-error)
  t)

(deftest readtable-case.error.3
  (check-type-error #'readtable-case (typef 'readtable))
  nil)

(deftest readtable-case.error.4
  (check-type-error #'(lambda (x)
			(let ((rt (copy-readtable)))
			  (setf (readtable-case rt) x)))
		    (typef '(member :upcase :downcase :preserve :invert)))
  nil)

(deftest readtable-case.error.5
  (check-type-error #'(lambda (x) (setf (readtable-case x) :upcase))
		    (typef 'readtable))
  nil)

