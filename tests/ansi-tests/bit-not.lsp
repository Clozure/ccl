;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 19:40:13 2003
;;;; Contains: Tests of BIT-NOT

(in-package :cl-test)

(deftest bit-not.1
  (let ((a1 (make-array nil :element-type 'bit :initial-element 0)))
    (values (bit-not a1) a1))
  #0a1 #0a0)
     
(deftest bit-not.2
  (let ((a1 (make-array nil :element-type 'bit :initial-element 1)))
    (values (bit-not a1) a1))
  #0a0 #0a1)

(deftest bit-not.3
  (let ((a1 (make-array nil :element-type 'bit :initial-element 0)))
    (values (bit-not a1 t) a1))
  #0a1 #0a1)
     
(deftest bit-not.4
  (let ((a1 (make-array nil :element-type 'bit :initial-element 1)))
    (values (bit-not a1 t) a1))
  #0a0 #0a0)

(deftest bit-not.5
  (let* ((a1 (make-array nil :element-type 'bit :initial-element 1))
	 (a2 (make-array nil :element-type 'bit :initial-element 1))
	 (result (bit-not a1 a2)))
    (values a1 a2 (eqt a2 result)))
  #0a1 #0a0 t)

(deftest bit-not.6
  (let ((a1 (make-array nil :element-type 'bit :initial-element 0)))
    (values (bit-not a1 nil) a1))
  #0a1 #0a0)

;;; Tests on bit vectors

(deftest bit-not.7
  (let ((a1 (copy-seq #*0011010110)))
    (values (bit-not a1) a1))
  #*1100101001
  #*0011010110)

(deftest bit-not.8
  (let ((a1 (copy-seq #*0011010110)))
    (values (bit-not a1 t) a1))
  #*1100101001
  #*1100101001)

(deftest bit-not.9
  (let ((a1 (copy-seq #*0011010110))
	(a2 (copy-seq #*0000000000)))
    (values (bit-not a1 a2) a1 a2))
  #*1100101001
  #*0011010110
  #*1100101001)

;;; Arrays

(deftest bit-not.10
  (let ((a1 (make-array '(2 2) :element-type 'bit
			:initial-contents '((0 1)(1 0)))))
    (values (bit-not a1) a1))
  #2a((1 0)(0 1))
  #2a((0 1)(1 0)))

(deftest bit-not.11
  (let ((a1 (make-array '(2 2) :element-type 'bit
			:initial-contents '((0 1)(1 0)))))
    (values (bit-not a1 nil) a1))
  #2a((1 0)(0 1))
  #2a((0 1)(1 0)))

(deftest bit-not.12
  (let ((a1 (make-array '(2 2) :element-type 'bit
			:initial-contents '((0 1)(1 0)))))
    (values (bit-not a1 t) a1))
  #2a((1 0)(0 1))
  #2a((1 0)(0 1)))

(deftest bit-not.13
  (let ((a1 (make-array '(2 2) :element-type 'bit
			:initial-contents '((0 1)(1 0))))
	(a2 (make-array '(2 2) :element-type 'bit
			:initial-element 0)))
    (values (bit-not a1 a2) a1 a2))
  #2a((1 0)(0 1))
  #2a((0 1)(1 0))
  #2a((1 0)(0 1)))

;;; Adjustable array

(deftest bit-not.14
  (let ((a1 (make-array '(2 2) :element-type 'bit
			:adjustable t
			:initial-contents '((0 1)(1 0)))))
    (values (bit-not a1) a1))
  #2a((1 0)(0 1))
  #2a((0 1)(1 0)))

;;; Displaced arrays

(deftest bit-not.15
  (let* ((a0 (make-array '(12) :element-type 'bit
			 :initial-contents '(0 0 0 1 1 0 0 0 0 0 0 0)))
	 (a1 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 2))
	 (a2 (make-array '(2 2) :element-type 'bit
			 :displaced-to a0
			 :displaced-index-offset 6)))
    (values (bit-not a1 a2) a0 a1 a2))
  #2a((1 0)(0 1))
  #*000110100100
  #2a((0 1)(1 0))
  #2a((1 0)(0 1)))

;;; Macro env tests

(deftest bit-not.16
  (macrolet
   ((%m (z) z))
   (bit-not (expand-in-current-env (%m #*10010011))))
  #*01101100)

(deftest bit-not.17
  (macrolet
   ((%m (z) z))
   (bit-not #*1101011010 (expand-in-current-env (%m nil))))
  #*0010100101)

;;;

(deftest bit-not.order.1
  (let ((a (copy-seq #*001101))
	(i 0) x)
    (values
     (bit-not (progn (setf x (incf i)) a))
     i x))
  #*110010 1 1)

(def-fold-test bit-not.fold.1 (bit-not #*00101))

;;; Error tests

(deftest bit-not.error.1
  (signals-error (bit-not) program-error)
  t)

(deftest bit-not.error.2
  (signals-error (bit-not #*000 nil nil) program-error)
  t)
