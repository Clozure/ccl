;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep 30 21:41:59 2002
;;;; Contains: Tests for STRING-DOWNCASE

(in-package :cl-test)

(deftest string-downcase.1
  (let ((s "A"))
    (values (string-downcase s) s))
  "a" "A")

(deftest string-downcase.2
  (let ((s "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
    (values (string-downcase s) s))
  "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(deftest string-downcase.3
  (let ((s "0123456789!@#$%^&*()_-+=|\\{}[]:\";'<>?,./ "))
    (values (string-downcase s) s))
  "0123456789!@#$%^&*()_-+=|\\{}[]:\";'<>?,./ "
  "0123456789!@#$%^&*()_-+=|\\{}[]:\";'<>?,./ ")

(deftest string-downcase.4
  (string-downcase #\A)
  "a")

(deftest string-downcase.5
  (let ((sym '|A|))
    (values (string-downcase sym) sym))
  "a" |A|)

(deftest string-downcase.6
  (let ((s (make-array 6 :element-type 'character
		       :initial-contents '(#\A #\B #\C #\D #\E #\F))))
    (values (string-downcase s) s))
  "abcdef"
  "ABCDEF")

(deftest string-downcase.7
  (let ((s (make-array 6 :element-type 'standard-char
		       :initial-contents '(#\A #\B #\7 #\D #\E #\F))))
    (values (string-downcase s) s))
  "ab7def"
  "AB7DEF")

;; Tests with :start, :end

(deftest string-downcase.8
  (let ((s "ABCDEF"))
    (values
     (loop for i from 0 to 6
	   collect (string-downcase s :start i))
     s))
  ("abcdef" "Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF")
  "ABCDEF")

(deftest string-downcase.9
  (let ((s "ABCDEF"))
    (values
     (loop for i from 0 to 6
	   collect (string-downcase s :start i :end nil))
     s))
  ("abcdef" "Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF")
  "ABCDEF")

(deftest string-downcase.10
  (let ((s "ABCDE"))
    (values
     (loop for i from 0 to 4
	   collect (loop for j from i to 5
			 collect (string-invertcase
				  (string-downcase s :start i :end j))))
     s))
  (("abcde" "Abcde" "ABcde" "ABCde" "ABCDe" "ABCDE")
   ("abcde" "aBcde" "aBCde" "aBCDe" "aBCDE")
   ("abcde" "abCde" "abCDe" "abCDE")
   ("abcde" "abcDe" "abcDE")
   ("abcde" "abcdE"))
  "ABCDE")

(deftest string-downcase.11
  :notes (:nil-vectors-are-strings)
  (string-downcase (make-array '(0) :element-type nil))
  "")

(deftest string-downcase.12
  (loop for type in '(standard-char base-char character)
	for s = (make-array '(10) :element-type type
			    :fill-pointer 5
			    :initial-contents "aB0cDefGHi")
	collect (list s (string-downcase s)))
  (("aB0cD" "ab0cd") ("aB0cD" "ab0cd") ("aB0cD" "ab0cd")))


(deftest string-downcase.13
  (loop for type in '(standard-char base-char character)
	for s0 = (make-array '(10) :element-type type
			     :initial-contents "zZaB0cDefG")
	for s = (make-array '(5) :element-type type
			    :displaced-to s0
			    :displaced-index-offset 2)
	collect (list s (string-downcase s)))
  (("aB0cD" "ab0cd") ("aB0cD" "ab0cd") ("aB0cD" "ab0cd")))

(deftest string-downcase.14
  (loop for type in '(standard-char base-char character)
	for s = (make-array '(5) :element-type type
			    :adjustable t
			    :initial-contents "aB0cD")
	collect (list s (string-downcase s)))
  (("aB0cD" "ab0cd") ("aB0cD" "ab0cd") ("aB0cD" "ab0cd")))

;;; Order of evaluation tests

(deftest string-downcase.order.1
  (let ((i 0) a b c (s (copy-seq "ABCDEF")))
    (values
     (string-downcase
      (progn (setf a (incf i)) s)
      :start (progn (setf b (incf i)) 1)
      :end   (progn (setf c (incf i)) 4))
     i a b c))
  "AbcdEF" 3 1 2 3)

(deftest string-downcase.order.2
  (let ((i 0) a b c (s (copy-seq "ABCDEF")))
    (values
     (string-downcase
      (progn (setf a (incf i)) s)
      :end   (progn (setf b (incf i)) 4)
      :start (progn (setf c (incf i)) 1))
     i a b c))
  "AbcdEF" 3 1 2 3)

(def-fold-test string-downcase.fold.1 (string-downcase "ABCDE"))

;;; Error cases

(deftest string-downcase.error.1
  (signals-error (string-downcase) program-error)
  t)

(deftest string-downcase.error.2
  (signals-error (string-downcase (copy-seq "abc") :bad t) program-error)
  t)

(deftest string-downcase.error.3
  (signals-error (string-downcase (copy-seq "abc") :start) program-error)
  t)

(deftest string-downcase.error.4
  (signals-error (string-downcase (copy-seq "abc") :bad t
				      :allow-other-keys nil) program-error)
  t)

(deftest string-downcase.error.5
  (signals-error (string-downcase (copy-seq "abc") :end) program-error)
  t)

(deftest string-downcase.error.6
  (signals-error (string-downcase (copy-seq "abc") 1 2) program-error)
  t)
