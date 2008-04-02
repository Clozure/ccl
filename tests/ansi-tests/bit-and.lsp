;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 18:18:47 2003
;;;; Contains: Tests of BIT-AND

(in-package :cl-test)

(compile-and-load "bit-aux.lsp")

(deftest bit-and.1
  (let* ((s1 (make-array nil :initial-element 0 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit)))
    (values (bit-and s1 s2) s1 s2))
  #0a0
  #0a0
  #0a0)

(deftest bit-and.2
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit)))
    (values (bit-and s1 s2) s1 s2))
  #0a0
  #0a1
  #0a0)

(deftest bit-and.3
  (let* ((s1 (make-array nil :initial-element 0 :element-type 'bit))
	 (s2 (make-array nil :initial-element 1 :element-type 'bit)))
    (values (bit-and s1 s2) s1 s2))
  #0a0
  #0a0
  #0a1)

(deftest bit-and.4
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 1 :element-type 'bit)))
    (values (bit-and s1 s2) s1 s2))
  #0a1
  #0a1
  #0a1)

(deftest bit-and.5
  (let* ((s1 (make-array nil :initial-element 0 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit))
	 (s3 (make-array nil :initial-element 1 :element-type 'bit))
	 (result (bit-and s1 s2 s3)))
    (values s1 s2 s3 result (eqt s3 result)))
  #0a0
  #0a0
  #0a0
  #0a0
  t)

(deftest bit-and.6
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 1 :element-type 'bit))
	 (s3 (make-array nil :initial-element 0 :element-type 'bit))
	 (result (bit-and s1 s2 s3)))
    (values s1 s2 s3 result (eqt s3 result)))
  #0a1
  #0a1
  #0a1
  #0a1
  t)

(deftest bit-and.7
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit))
	 (result (bit-and s1 s2 t)))
    (values s1 s2 result (eqt s1 result)))
  #0a0
  #0a0
  #0a0
  t)


;;; Tests on bit vectors

(deftest bit-and.8
  (let ((a1 (copy-seq #*0011))
	(a2 (copy-seq #*0101)))
    (values (check-values (bit-and a1 a2)) a1 a2))
  #*0001 #*0011 #*0101)

(deftest bit-and.9
  (let* ((a1 (copy-seq #*0011))
	 (a2 (copy-seq #*0101))
	 (result (check-values (bit-and a1 a2 t))))
    (values result a1 a2 (eqt result a1)))
  #*0001 #*0001 #*0101 t)

(deftest bit-and.10
  (let* ((a1 (copy-seq #*0011))
	 (a2 (copy-seq #*0101))
	 (a3 (copy-seq #*1110))
	 (result (check-values (bit-and a1 a2 a3))))
    (values result a1 a2 a3 (eqt result a3)))
  #*0001 #*0011 #*0101 #*0001 t)

(deftest bit-and.11
  (let ((a1 (copy-seq #*0011))
	(a2 (copy-seq #*0101)))
    (values (check-values (bit-and a1 a2 nil)) a1 a2))
  #*0001 #*0011 #*0101)

;;; Tests on bit arrays

(deftest bit-and.12
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (result (bit-and a1 a2)))
    (values a1 a2 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 0)(0 1)))

(deftest bit-and.13
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (result (bit-and a1 a2 t)))
    (values a1 a2 result))
  #2a((0 0)(0 1))
  #2a((0 0)(1 1))
  #2a((0 0)(0 1)))

(deftest bit-and.14
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (result (bit-and a1 a2 nil)))
    (values a1 a2 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 0)(0 1)))

(deftest bit-and.15
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (a3 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(0 0))))
	 (result (bit-and a1 a2 a3)))
    (values a1 a2 a3 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 0)(0 1))
  #2a((0 0)(0 1)))

;;; Adjustable arrays

(deftest bit-and.16
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))
			 :adjustable t))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))
			 :adjustable t))
	 (result (bit-and a1 a2)))
    (values a1 a2 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 0)(0 1)))

;;; Displaced arrays

(deftest bit-and.17
  (let* ((a0 (make-array '(8) :element-type 'bit
			 :initial-contents '(0 1 0 1 0 0 1 1)))
	 (a1 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 0))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 4))
	 (result (bit-and a1 a2)))
    (values a0 a1 a2 result))
  #*01010011
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 0)(0 1)))

(deftest bit-and.18
  (let* ((a0 (make-array '(8) :element-type 'bit
			 :initial-contents '(0 1 0 1 0 0 1 1)))
	 (a1 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 0))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 4))
	 (result (bit-and a1 a2 t)))
    (values a0 a1 a2 result))
  #*00010011
  #2a((0 0)(0 1))
  #2a((0 0)(1 1))
  #2a((0 0)(0 1)))

(deftest bit-and.19
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
	 (result (bit-and a1 a2 a3)))
    (values a0 a1 a2 result))
  #*010100110001
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((0 0)(0 1)))

(deftest bit-and.20
  (macrolet ((%m (z) z)) (bit-and (expand-in-current-env (%m #*0011)) #*0101))
  #*0001)

(deftest bit-and.21
  (macrolet ((%m (z) z)) (bit-and #*1010 (expand-in-current-env (%m #*1100))))
  #*1000)

(deftest bit-and.22
  (macrolet ((%m (z) z)) (bit-and #*10100011 #*01101010
				  (expand-in-current-env (%m nil))))
  #*00100010)

(deftest bit-and.order.1
  (let* ((s1 (make-array 1 :initial-element 0 :element-type 'bit))
	 (s2 (make-array 1 :initial-element 0 :element-type 'bit))
	 (x 0) y z)
    (values
     (bit-and (progn (setf y (incf x)) s1)
	      (progn (setf z (incf x)) s2))
     x y z))
  #*0 2 1 2)

(def-fold-test bit-and.fold.1 (bit-and #*01101 #*01011))

;;; Randomized tests

(deftest bit-and.random.1
  (bit-random-test-fn #'bit-and #'logand)
  nil)

;;; Error tests

(deftest bit-and.error.1
  (signals-error (bit-and) program-error)
  t)

(deftest bit-and.error.2
  (signals-error (bit-and #*000) program-error)
  t)

(deftest bit-and.error.3
  (signals-error (bit-and #*000 #*0100 nil nil)
		 program-error)
  t)

