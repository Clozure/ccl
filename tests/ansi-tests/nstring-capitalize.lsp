;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct  3 21:38:49 2002
;;;; Contains: Tests for NSTRING-CAPITALIZE

(in-package :cl-test)

(deftest nstring-capitalize.1
  (let* ((s (copy-seq "abCd"))
	 (s2 (nstring-capitalize s)))
    (values (eqt s s2) s))
  t "Abcd")

(deftest nstring-capitalize.2
  (let* ((s (copy-seq "0adA2Cdd3wXy"))
	 (s2 (nstring-capitalize s)))
    (values (eqt s s2) s))
  t "0ada2cdd3wxy")

(deftest nstring-capitalize.3
  (let* ((s (copy-seq "1a"))
	 (s2 (nstring-capitalize s)))
    (values (eqt s s2) s))
  t "1a")

(deftest nstring-capitalize.4
  (let* ((s (copy-seq "a1a"))
	 (s2 (nstring-capitalize s)))
    (values (eqt s s2) s))
  t "A1a")

(deftest nstring-capitalize.7
  (let ((s "ABCDEF"))
     (loop for i from 0 to 5
	   collect (nstring-capitalize (copy-seq s) :start i)))
  ("Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF"))

(deftest nstring-capitalize.8
  (let ((s "ABCDEF"))
     (loop for i from 0 to 5
	   collect (nstring-capitalize (copy-seq s) :start i :end nil)))
  ("Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF"))

(deftest nstring-capitalize.9
  (let ((s "ABCDEF"))
     (loop for i from 0 to 6
	   collect (nstring-capitalize (copy-seq s) :end i)))
  ("ABCDEF" "ABCDEF" "AbCDEF" "AbcDEF" "AbcdEF" "AbcdeF" "Abcdef"))

(deftest nstring-capitalize.10
  (let ((s "ABCDEF"))
    (loop for i from 0 to 5
	  collect (loop for j from i to 6
			collect (nstring-capitalize (copy-seq s)
						    :start i :end j))))
  (("ABCDEF" "ABCDEF" "AbCDEF" "AbcDEF" "AbcdEF" "AbcdeF" "Abcdef")
   ("ABCDEF" "ABCDEF" "ABcDEF" "ABcdEF" "ABcdeF" "ABcdef")
   ("ABCDEF" "ABCDEF" "ABCdEF" "ABCdeF" "ABCdef")
   ("ABCDEF" "ABCDEF" "ABCDeF" "ABCDef")
   ("ABCDEF" "ABCDEF" "ABCDEf")
   ("ABCDEF" "ABCDEF")))

(deftest nstring-capitalize.11
  (nstring-capitalize "")
  "")

(deftest nstring-capitalize.12
  :notes (:nil-vectors-are-strings)
  (nstring-capitalize (make-array '(0) :element-type nil))
  "")

(deftest nstring-capitalize.13
  (loop for type in '(standard-char base-char character)
	for s = (make-array '(10) :element-type type
			    :fill-pointer 5
			    :initial-contents "aB0cDefGHi")
	collect (list (copy-seq s)
		      (copy-seq (nstring-capitalize s))
		      (copy-seq s)
		      (progn (setf (fill-pointer s) 10) (copy-seq s))
		      ))
  (("aB0cD" "Ab0cd" "Ab0cd" "Ab0cdefGHi")
   ("aB0cD" "Ab0cd" "Ab0cd" "Ab0cdefGHi")
   ("aB0cD" "Ab0cd" "Ab0cd" "Ab0cdefGHi")))

(deftest nstring-capitalize.14
  (loop for type in '(standard-char base-char character)
	for s0 = (make-array '(10) :element-type type
			     :initial-contents "zZaB0cDefG")
	for s = (make-array '(5) :element-type type
			    :displaced-to s0
			    :displaced-index-offset 2)
	collect (list (copy-seq s)
		      (nstring-capitalize s)
		      (copy-seq s)
		      s0))
  (("aB0cD" "Ab0cd" "Ab0cd" "zZAb0cdefG")
   ("aB0cD" "Ab0cd" "Ab0cd" "zZAb0cdefG")
   ("aB0cD" "Ab0cd" "Ab0cd" "zZAb0cdefG")))

(deftest nstring-capitalize.15
  (loop for type in '(standard-char base-char character)
	for s = (make-array '(5) :element-type type
			    :adjustable t
			    :initial-contents "aB0cD")
	collect (list (copy-seq s)
		      (nstring-capitalize s)
		      (copy-seq s)))
  (("aB0cD" "Ab0cd" "Ab0cd")
   ("aB0cD" "Ab0cd" "Ab0cd")
   ("aB0cD" "Ab0cd" "Ab0cd")))

;;; Order of evaluation tests


(deftest nstring-capitalize.order.1
  (let ((i 0) a b c (s (copy-seq "abcdef")))
    (values
     (nstring-capitalize
      (progn (setf a (incf i)) s)
      :start (progn (setf b (incf i)) 1)
      :end   (progn (setf c (incf i)) 4))
     i a b c))
  "aBcdef" 3 1 2 3)

(deftest nstring-capitalize.order.2
  (let ((i 0) a b c (s (copy-seq "abcdef")))
    (values
     (nstring-capitalize
      (progn (setf a (incf i)) s)
      :end   (progn (setf b (incf i)) 4)
      :start (progn (setf c (incf i)) 1))
     i a b c))
  "aBcdef" 3 1 2 3)

;;; Error cases

(deftest nstring-capitalize.error.1
  (signals-error (nstring-capitalize) program-error)
  t)

(deftest nstring-capitalize.error.2
  (signals-error (nstring-capitalize (copy-seq "abc") :bad t) program-error)
  t)

(deftest nstring-capitalize.error.3
  (signals-error (nstring-capitalize (copy-seq "abc") :start) program-error)
  t)

(deftest nstring-capitalize.error.4
  (signals-error (nstring-capitalize (copy-seq "abc") :bad t
				      :allow-other-keys nil)
		 program-error)
  t)

(deftest nstring-capitalize.error.5
  (signals-error (nstring-capitalize (copy-seq "abc") :end) program-error)
  t)

(deftest nstring-capitalize.error.6
  (signals-error (nstring-capitalize (copy-seq "abc") 1 2) program-error)
  t)
