;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 19:31:35 2003
;;;; Contains: Tests of BIT-ORC2

(in-package :cl-test)

(compile-and-load "bit-aux.lsp")

(deftest bit-orc2.1
  (let* ((s1 (make-array nil :initial-element 0 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit)))
    (values (bit-orc2 s1 s2) s1 s2))
  #0a1
  #0a0
  #0a0)

(deftest bit-orc2.2
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit)))
    (values (bit-orc2 s1 s2) s1 s2))
  #0a1
  #0a1
  #0a0)

(deftest bit-orc2.3
  (let* ((s1 (make-array nil :initial-element 0 :element-type 'bit))
	 (s2 (make-array nil :initial-element 1 :element-type 'bit)))
    (values (bit-orc2 s1 s2) s1 s2))
  #0a0
  #0a0
  #0a1)

(deftest bit-orc2.4
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 1 :element-type 'bit)))
    (values (bit-orc2 s1 s2) s1 s2))
  #0a1
  #0a1
  #0a1)

(deftest bit-orc2.5
  (let* ((s1 (make-array nil :initial-element 0 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit))
	 (s3 (make-array nil :initial-element 0 :element-type 'bit))
	 (result (bit-orc2 s1 s2 s3)))
    (values s1 s2 s3 result (eqt s3 result)))
  #0a0
  #0a0
  #0a1
  #0a1
  t)

(deftest bit-orc2.6
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 1 :element-type 'bit))
	 (s3 (make-array nil :initial-element 0 :element-type 'bit))
	 (result (bit-orc2 s1 s2 s3)))
    (values s1 s2 s3 result (eqt s3 result)))
  #0a1
  #0a1
  #0a1
  #0a1
  t)

(deftest bit-orc2.7
  (let* ((s1 (make-array nil :initial-element 1 :element-type 'bit))
	 (s2 (make-array nil :initial-element 0 :element-type 'bit))
	 (result (bit-orc2 s1 s2 t)))
    (values s1 s2 result (eqt s1 result)))
  #0a1
  #0a0
  #0a1
  t)


;;; Tests on bit vectors

(deftest bit-orc2.8
  (let ((a1 (copy-seq #*0011))
	(a2 (copy-seq #*0101)))
    (values (check-values (bit-orc2 a1 a2)) a1 a2))
  #*1011 #*0011 #*0101)

(deftest bit-orc2.9
  (let* ((a1 (copy-seq #*0011))
	 (a2 (copy-seq #*0101))
	 (result (check-values (bit-orc2 a1 a2 t))))
    (values result a1 a2 (eqt result a1)))
  #*1011 #*1011 #*0101 t)

(deftest bit-orc2.10
  (let* ((a1 (copy-seq #*0011))
	 (a2 (copy-seq #*0101))
	 (a3 (copy-seq #*1110))
	 (result (check-values (bit-orc2 a1 a2 a3))))
    (values result a1 a2 a3 (eqt result a3)))
  #*1011 #*0011 #*0101 #*1011 t)

(deftest bit-orc2.11
  (let ((a1 (copy-seq #*0011))
	(a2 (copy-seq #*0101)))
    (values (check-values (bit-orc2 a1 a2 nil)) a1 a2))
  #*1011 #*0011 #*0101)

;;; Tests on bit arrays

(deftest bit-orc2.12
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (result (bit-orc2 a1 a2)))
    (values a1 a2 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((1 1)(0 1)))

(deftest bit-orc2.13
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (result (bit-orc2 a1 a2 t)))
    (values a1 a2 result))
  #2a((1 1)(0 1))
  #2a((0 0)(1 1))
  #2a((1 1)(0 1)))

(deftest bit-orc2.14
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (result (bit-orc2 a1 a2 nil)))
    (values a1 a2 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((1 1)(0 1)))

(deftest bit-orc2.15
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))))
	 (a3 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(0 0))))
	 (result (bit-orc2 a1 a2 a3)))
    (values a1 a2 a3 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((1 1)(0 1))
  #2a((1 1)(0 1)))

;;; Adjustable arrays

(deftest bit-orc2.16
  (let* ((a1 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 1)(0 1))
			 :adjustable t))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :initial-contents '((0 0)(1 1))
			 :adjustable t))
	 (result (bit-orc2 a1 a2)))
    (values a1 a2 result))
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((1 1)(0 1)))

;;; Displaced arrays

(deftest bit-orc2.17
  (let* ((a0 (make-array '(8) :element-type 'bit
			 :initial-contents '(0 1 0 1 0 0 1 1)))
	 (a1 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 0))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 4))
	 (result (bit-orc2 a1 a2)))
    (values a0 a1 a2 result))
  #*01010011
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((1 1)(0 1)))

(deftest bit-orc2.18
  (let* ((a0 (make-array '(8) :element-type 'bit
			 :initial-contents '(0 1 0 1 0 0 1 1)))
	 (a1 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 0))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 4))
	 (result (bit-orc2 a1 a2 t)))
    (values a0 a1 a2 result))
  #*11010011
  #2a((1 1)(0 1))
  #2a((0 0)(1 1))
  #2a((1 1)(0 1)))

(deftest bit-orc2.19
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
	 (result (bit-orc2 a1 a2 a3)))
    (values a0 a1 a2 result))
  #*010100111101
  #2a((0 1)(0 1))
  #2a((0 0)(1 1))
  #2a((1 1)(0 1)))

(deftest bit-orc2.20
  (macrolet ((%m (z) z)) (bit-orc2 (expand-in-current-env (%m #*0011)) #*0101))
  #*1011)

(deftest bit-orc2.21
  (macrolet ((%m (z) z)) (bit-orc2 #*1010 (expand-in-current-env (%m #*1100))))
  #*1011)

(deftest bit-orc2.22
  (macrolet ((%m (z) z)) (bit-orc2 #*10100011 #*01101010
				   (expand-in-current-env (%m nil))))
  #*10110111)

(deftest bit-orc2.order.1
  (let* ((s1 (make-array 1 :initial-element 0 :element-type 'bit))
	 (s2 (make-array 1 :initial-element 0 :element-type 'bit))
	 (x 0) y z)
    (values
     (bit-orc2 (progn (setf y (incf x)) s1)
	       (progn (setf z (incf x)) s2))
     x y z))
  #*1 2 1 2)

(deftest bit-orc2.fold.1
  (flet ((%f () (declare (optimize speed (safety 0) (space 0)))
	     (bit-orc2 #*00101 #*01011)))
    (values (%f) (let ((bv (%f))) (setf (elt bv 0) 0) bv) (%f)))
  #*10101 #*00101 #*10101)

;;; Random tests

(deftest bit-orc2.random.1
  (bit-random-test-fn #'bit-orc2 #'logorc2)
  nil)

;;; Error tests

(deftest bit-orc2.error.1
  (signals-error (bit-orc2) program-error)
  t)

(deftest bit-orc2.error.2
  (signals-error (bit-orc2 #*000) program-error)
  t)

(deftest bit-orc2.error.3
  (signals-error (bit-orc2 #*000 #*0100 nil nil)
		 program-error)
  t)


