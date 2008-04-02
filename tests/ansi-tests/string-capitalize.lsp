;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct  3 20:08:26 2002
;;;; Contains: Tests for STRING-CAPITALIZE

(in-package :cl-test)

(deftest string-capitalize.1
  (let ((s "abCd"))
    (values (string-capitalize s) s))
  "Abcd"
  "abCd")


(deftest string-capitalize.2
  (let ((s "0adA2Cdd3wXy"))
    (values (string-capitalize s) s))
  "0ada2cdd3wxy"
  "0adA2Cdd3wXy")

(deftest string-capitalize.3
  (let ((s "1a"))
    (values (string-capitalize s) s))
  "1a"
  "1a")

(deftest string-capitalize.4
  (let ((s "a1a"))
    (values (string-capitalize s) s))
  "A1a"
  "a1a")

(deftest string-capitalize.5
  (let ((s #\a))
    (values (string-capitalize s) s))
  "A"
  #\a)

(deftest string-capitalize.6
  (let ((s '|abcDe|))
    (values (string-capitalize s) (symbol-name s)))
  "Abcde"
  "abcDe")

(deftest string-capitalize.7
  (let ((s "ABCDEF"))
    (values
     (loop for i from 0 to 5
	   collect (string-capitalize s :start i))
     s))
  ("Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF")
  "ABCDEF")

(deftest string-capitalize.8
  (let ((s "ABCDEF"))
    (values
     (loop for i from 0 to 5
	   collect (string-capitalize s :start i :end nil))
     s))
  ("Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF")
  "ABCDEF")

(deftest string-capitalize.9
  (let ((s "ABCDEF"))
    (values
     (loop for i from 0 to 6
	   collect (string-capitalize s :end i))
     s))
  ("ABCDEF" "ABCDEF" "AbCDEF" "AbcDEF" "AbcdEF" "AbcdeF" "Abcdef")
  "ABCDEF")

(deftest string-capitalize.10
  (let ((s "ABCDEF"))
    (values
     (loop for i from 0 to 5
	   collect (loop for j from i to 6
			 collect (string-capitalize s :start i :end j)))
     s))
  (("ABCDEF" "ABCDEF" "AbCDEF" "AbcDEF" "AbcdEF" "AbcdeF" "Abcdef")
   ("ABCDEF" "ABCDEF" "ABcDEF" "ABcdEF" "ABcdeF" "ABcdef")
   ("ABCDEF" "ABCDEF" "ABCdEF" "ABCdeF" "ABCdef")
   ("ABCDEF" "ABCDEF" "ABCDeF" "ABCDef")
   ("ABCDEF" "ABCDEF" "ABCDEf")
   ("ABCDEF" "ABCDEF"))
  "ABCDEF")

(deftest string-capitalize.11
  :notes (:nil-vectors-are-strings)
  (string-capitalize (make-array '(0) :element-type nil))
  "")

(deftest string-capitalize.12
  (loop for type in '(standard-char base-char character)
	for s = (make-array '(10) :element-type type
			    :fill-pointer 5
			    :initial-contents "aB0cDefGHi")
	collect (list s (string-capitalize s)))
  (("aB0cD" "Ab0cd") ("aB0cD" "Ab0cd") ("aB0cD" "Ab0cd")))


(deftest string-capitalize.13
  (loop for type in '(standard-char base-char character)
	for s0 = (make-array '(10) :element-type type
			     :initial-contents "zZaB0cDefG")
	for s = (make-array '(5) :element-type type
			    :displaced-to s0
			    :displaced-index-offset 2)
	collect (list s (string-capitalize s)))
  (("aB0cD" "Ab0cd") ("aB0cD" "Ab0cd") ("aB0cD" "Ab0cd")))

(deftest string-capitalize.14
  (loop for type in '(standard-char base-char character)
	for s = (make-array '(5) :element-type type
			    :adjustable t
			    :initial-contents "aB0cD")
	collect (list s (string-capitalize s)))
  (("aB0cD" "Ab0cd") ("aB0cD" "Ab0cd") ("aB0cD" "Ab0cd")))

;;; Order of evaluation tests

(deftest string-capitalize.order.1
  (let ((i 0) a b c (s (copy-seq "abcdef")))
    (values
     (string-capitalize
      (progn (setf a (incf i)) s)
      :start (progn (setf b (incf i)) 1)
      :end   (progn (setf c (incf i)) 4))
     i a b c))
  "aBcdef" 3 1 2 3)

(deftest string-capitalize.order.2
  (let ((i 0) a b c (s (copy-seq "abcdef")))
    (values
     (string-capitalize
      (progn (setf a (incf i)) s)
      :end   (progn (setf b (incf i)) 4)
      :start (progn (setf c (incf i)) 1))
     i a b c))
  "aBcdef" 3 1 2 3)

(def-fold-test string-capitalize.fold.1 (string-capitalize "ABCDE"))

;;; Error cases

(deftest string-capitalize.error.1
  (signals-error (string-capitalize) program-error)
  t)

(deftest string-capitalize.error.2
  (signals-error (string-capitalize (copy-seq "abc") :bad t) program-error)
  t)

(deftest string-capitalize.error.3
  (signals-error (string-capitalize (copy-seq "abc") :start) program-error)
  t)

(deftest string-capitalize.error.4
  (signals-error (string-capitalize (copy-seq "abc") :bad t
				      :allow-other-keys nil) program-error)
  t)

(deftest string-capitalize.error.5
  (signals-error (string-capitalize (copy-seq "abc") :end) program-error)
  t)

(deftest string-capitalize.error.6
  (signals-error (string-capitalize (copy-seq "abc") 1 2) program-error)
  t)
