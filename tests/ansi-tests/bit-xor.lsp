;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 19:35:46 2003
;;;; Contains: Tests of BIT-XOR

(in-package :cl-test)

(compile-and-load "bit-aux.lsp")

(deftest bit-xor.1
  (let* ((s1 (make-array nil :initial-element 0 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit)))
    (values (bit-xor s1 s2) s1 s2))
  #0a0
  #0a0
  #0a0)

(deftest bit-xor.2
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit)))
    (values (bit-xor s1 s2) s1 s2))
  #0a1
  #0a1
  #0a0)

(deftest bit-xor.3
  (let* ((s1 (make-array nil :initial-element 0 :element-type 'bit))
	 (s2 (make-array nil :initial-element 1 :element-type 'bit)))
    (values (bit-xor s1 s2) s1 s2))
  #0a1
  #0a0
  #0a1)

(deftest bit-xor.4
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 1 :element-type 'bit)))
    (values (bit-xor s1 s2) s1 s2))
  #0a0
  #0a1
  #0a1)

(deftest bit-xor.5
  (let* ((s1 (make-array nil :initial-element 0 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit))
	 (s3 (make-array nil :initial-element 1 :element-type 'bit))
	 (result (bit-xor s1 s2 s3)))
    (values s1 s2 s3 result (eqt s3 result)))
  #0a0
  #0a0
  #0a0
  #0a0
  t)

(deftest bit-xor.6
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 1 :element-type 'bit))
	 (s3 (make-array nil :initial-element 1 :element-type 'bit))
	 (result (bit-xor s1 s2 s3)))
    (values s1 s2 s3 result (eqt s3 result)))
  #0a1
  #0a1
  #0a0
  #0a0
  t)

(deftest bit-xor.7
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit))
	 (result (bit-xor s1 s2 t)))
    (values s1 s2 result (eqt s1 result)))
  #0a1
  #0a0
  #0a1
  t)


;;; Tests on bit vectors

(deftest bit-xor.8
  (let ((a1 (copy-seq #*0011))
	(a2 (copy-seq #*0101)))
    (values (check-values (bit-xor a1 a2)) a1 a2))
  #*0110 #*0011 #*0101)

(deftest bit-xor.9
  (let* ((a1 (copy-seq #*0011))
	 (a2 (copy-seq #*0101))
	 (result (check-values (bit-xor a1 a2 t))))
    (values result a1 a2 (eqt result a1)))
  #*0110 #*0110 #*0101 t)

(deftest bit-xor.10
  (let* ((a1 (copy-seq #*0011))
	 (a2 (copy-seq #*0101))
	 (a3 (copy-seq #*1110))
	 (result (check-values (bit-xor a1 a2 a3))))
    (values result a1 a2 a3 (eqt result a3)))
  #*0110 #*0011 #*0101 #*0110 t)

(deftest bit-xor.11
  (let ((a1 (copy-seq #*0011))
	(a2 (copy-seq #*0101)))
    (values (check-values (bit-xor a1 a2 nil)) a1 a2))
  #*0110 #*0011 #*0101)

;;; Tests on bit arrays

(deftest bit-xor.12
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (result (bit-xor a1 a2)))
    (values a1 a2 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 0)))

(deftest bit-xor.13
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (result (bit-xor a1 a2 t)))
    (values a1 a2 result))
  #2a((0 1)(1 0))
  #2a((0 0)(1 1))
  #2a((0 1)(1 0)))

(deftest bit-xor.14
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (result (bit-xor a1 a2 nil)))
    (values a1 a2 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 0)))

(deftest bit-xor.15
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (a3 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(0 0))))
	 (result (bit-xor a1 a2 a3)))
    (values a1 a2 a3 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 0))
  #2a((0 1)(1 0)))

;;; Adjustable arrays

(deftest bit-xor.16
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))
			 :adjustable t))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))
			 :adjustable t))
	 (result (bit-xor a1 a2)))
    (values a1 a2 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 0)))

;;; Displaced arrays

(deftest bit-xor.17
  (let* ((a0 (make-array '(8) :element-type 'bit
			 :initial-contents '(0 1 0 1 0 0 1 1)))
	 (a1 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 0))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 4))
	 (result (bit-xor a1 a2)))
    (values a0 a1 a2 result))
  #*01010011
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 0)))

(deftest bit-xor.18
  (let* ((a0 (make-array '(8) :element-type 'bit
			 :initial-contents '(0 1 0 1 0 0 1 1)))
	 (a1 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 0))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 4))
	 (result (bit-xor a1 a2 t)))
    (values a0 a1 a2 result))
  #*01100011
  #2a((0 1)(1 0))
  #2a((0 0)(1 1))
  #2a((0 1)(1 0)))

(deftest bit-xor.19
  (let* ((a0 (make-array '(12) :element-type 'bit
			 :initial-contents '(0 1 0 1 0 0 1 1 1 1 1 0)))
	 (a1 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 0))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 4))
	 (a3 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 8))
	 (result (bit-xor a1 a2 a3)))
    (values a0 a1 a2 result))
  #*010100110110
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 1)(1 0)))

(deftest bit-xor.20
  (macrolet ((%m (z) z)) (bit-xor (expand-in-current-env (%m #*0011)) #*0101))
  #*0110)

(deftest bit-xor.21
  (macrolet ((%m (z) z)) (bit-xor #*1010 (expand-in-current-env (%m #*1100))))
  #*0110)

(deftest bit-xor.22
  (macrolet ((%m (z) z)) (bit-xor #*10100011 #*01101010
				  (expand-in-current-env (%m nil))))
  #*11001001)

(deftest bit-xor.order.1
  (let* ((s1 (make-array 1 :initial-element 0 :element-type 'bit))
	 (s2 (make-array 1 :initial-element 0 :element-type 'bit))
	 (x 0) y z)
    (values
     (bit-xor (progn (setf y (incf x)) s1)
	      (progn (setf z (incf x)) s2))
     x y z))
  #*0 2 1 2)

(def-fold-test bit-xor.fold.1 (bit-xor #*00101 #*10100))

;;; Random tests

(deftest bit-xor.random.1
  (bit-random-test-fn #'bit-xor #'logxor)
  nil)

;;; Error tests

(deftest bit-xor.error.1
  (signals-error (bit-xor) program-error)
  t)

(deftest bit-xor.error.2
  (signals-error (bit-xor #*000) program-error)
  t)

(deftest bit-xor.error.3
  (signals-error (bit-xor #*000 #*0100 nil nil)
		 program-error)
  t)
