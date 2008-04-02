;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct  3 21:33:16 2002
;;;; Contains: Tests for NSTRING-DOWNCASE

(in-package :cl-test)

(deftest nstring-downcase.1
  (let* ((s (copy-seq "A"))
	 (s2 (nstring-downcase s)))
    (values (eqt s s2) s))
  t "a")

(deftest nstring-downcase.2
  (let* ((s (copy-seq "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
	 (s2 (nstring-downcase s)))
    (values (eqt s s2) s))
  t
  "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")

(deftest nstring-downcase.3
  (let* ((s (copy-seq "0123456789!@#$%^&*()_-+=|\\{}[]:\";'<>?,./ "))
	 (s2 (nstring-downcase s)))
    (values (eqt s s2) s))
  t
  "0123456789!@#$%^&*()_-+=|\\{}[]:\";'<>?,./ ")

(deftest nstring-downcase.6
  (let* ((s (make-array 6 :element-type 'character
			:initial-contents '(#\A #\B #\C #\D #\E #\F)))
	 (s2 (nstring-downcase s)))
    (values (eqt s s2) s))
  t "abcdef")

(deftest nstring-downcase.7
  (let* ((s (make-array 6 :element-type 'standard-char
			:initial-contents '(#\A #\B #\7 #\D #\E #\F)))
	 (s2 (nstring-downcase s)))
    (values (eqt s s2) s))
  t
  "ab7def")

;; Tests with :start, :end

(deftest nstring-downcase.8
  (let ((s "ABCDEF"))
     (loop for i from 0 to 6
	   collect (nstring-downcase (copy-seq s) :start i)))
  ("abcdef" "Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF"))

(deftest nstring-downcase.9
  (let ((s "ABCDEF"))
     (loop for i from 0 to 6
	   collect (nstring-downcase (copy-seq s) :start i :end nil)))
  ("abcdef" "Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF"))

(deftest nstring-downcase.10
  (let ((s "ABCDE"))
    (loop for i from 0 to 4
	  collect (loop for j from i to 5
			collect (string-invertcase
				 (nstring-downcase (copy-seq s)
						   :start i :end j)))))
  (("abcde" "Abcde" "ABcde" "ABCde" "ABCDe" "ABCDE")
   ("abcde" "aBcde" "aBCde" "aBCDe" "aBCDE")
   ("abcde" "abCde" "abCDe" "abCDE")
   ("abcde" "abcDe" "abcDE")
   ("abcde" "abcdE")))

(deftest nstring-downcase.11
  :notes (:nil-vectors-are-strings)
  (nstring-downcase (make-array '(0) :element-type nil))
  "")

(deftest nstring-downcase.12
  (loop for type in '(standard-char base-char character)
	for s = (make-array '(10) :element-type type
			    :fill-pointer 5
			    :initial-contents "aB0cDefGHi")
	collect (list (copy-seq s)
		      (copy-seq (nstring-downcase s))
		      (copy-seq s)
		      (progn (setf (fill-pointer s) 10) (copy-seq s))
		      ))
  (("aB0cD" "ab0cd" "ab0cd" "ab0cdefGHi")
   ("aB0cD" "ab0cd" "ab0cd" "ab0cdefGHi")
   ("aB0cD" "ab0cd" "ab0cd" "ab0cdefGHi")))

(deftest nstring-downcase.13
  (loop for type in '(standard-char base-char character)
	for s0 = (make-array '(10) :element-type type
			     :initial-contents "zZaB0cDefG")
	for s = (make-array '(5) :element-type type
			    :displaced-to s0
			    :displaced-index-offset 2)
	collect (list (copy-seq s)
		      (nstring-downcase s)
		      (copy-seq s)
		      s0))
  (("aB0cD" "ab0cd" "ab0cd" "zZab0cdefG")
   ("aB0cD" "ab0cd" "ab0cd" "zZab0cdefG")
   ("aB0cD" "ab0cd" "ab0cd" "zZab0cdefG")))

(deftest nstring-downcase.14
  (loop for type in '(standard-char base-char character)
	for s = (make-array '(5) :element-type type
			    :adjustable t
			    :initial-contents "aB0cD")
	collect (list (copy-seq s)
		      (nstring-downcase s)
		      (copy-seq s)))
  (("aB0cD" "ab0cd" "ab0cd")
   ("aB0cD" "ab0cd" "ab0cd")
   ("aB0cD" "ab0cd" "ab0cd")))

;;; Order of evaluation tests

(deftest nstring-downcase.order.1
  (let ((i 0) a b c (s (copy-seq "ABCDEF")))
    (values
     (nstring-downcase
      (progn (setf a (incf i)) s)
      :start (progn (setf b (incf i)) 1)
      :end   (progn (setf c (incf i)) 4))
     i a b c))
  "AbcdEF" 3 1 2 3)

(deftest nstring-downcase.order.2
  (let ((i 0) a b c (s (copy-seq "ABCDEF")))
    (values
     (nstring-downcase
      (progn (setf a (incf i)) s)
      :end   (progn (setf b (incf i)) 4)
      :start (progn (setf c (incf i)) 1))
     i a b c))
  "AbcdEF" 3 1 2 3)

;;; Error cases

(deftest nstring-downcase.error.1
  (signals-error (nstring-downcase) program-error)
  t)

(deftest nstring-downcase.error.2
  (signals-error (nstring-downcase (copy-seq "abc") :bad t) program-error)
  t)

(deftest nstring-downcase.error.3
  (signals-error (nstring-downcase (copy-seq "abc") :start) program-error)
  t)

(deftest nstring-downcase.error.4
  (signals-error (nstring-downcase (copy-seq "abc") :bad t
				      :allow-other-keys nil)
		 program-error)
  t)

(deftest nstring-downcase.error.5
  (signals-error (nstring-downcase (copy-seq "abc") :end) program-error)
  t)

(deftest nstring-downcase.error.6
  (signals-error (nstring-downcase (copy-seq "abc") 1 2) program-error)
  t)




