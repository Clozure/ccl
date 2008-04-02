;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 19:57:29 2003
;;;; Contains: Tests for ARRAY-IN-BOUNDS-P

(in-package :cl-test)

(deftest array-in-bounds-p.1
  (array-in-bounds-p #() 0)
  nil)

(deftest array-in-bounds-p.2
  (array-in-bounds-p #() -1)
  nil)

(deftest array-in-bounds-p.3
  (let ((a #(a b c d)))
    (loop for i from 0 to 4 collect (notnot (array-in-bounds-p a i))))
  (t t t t nil))

(deftest array-in-bounds-p.4
  (notnot (array-in-bounds-p #0aNIL))
  t)

(deftest array-in-bounds-p.5
  (array-in-bounds-p "" 0)
  nil)

(deftest array-in-bounds-p.6
  (array-in-bounds-p "" -1)
  nil)

(deftest array-in-bounds-p.7
  (let ((a "abcd"))
    (loop for i from 0 to 4 collect (notnot (array-in-bounds-p a i))))
  (t t t t nil))

(deftest array-in-bounds-p.8
  (array-in-bounds-p #* 0)
  nil)

(deftest array-in-bounds-p.9
  (array-in-bounds-p #* -1)
  nil)

(deftest array-in-bounds-p.10
  (let ((a #*0110))
    (loop for i from 0 to 4 collect (notnot (array-in-bounds-p a i))))
  (t t t t nil))

;; Fill pointer tests

(deftest array-in-bounds-p.11
  (let ((a (make-array '(10) :fill-pointer 5)))
    (loop for i from -1 to 10 collect (notnot (array-in-bounds-p a i))))
  (nil t t t t t t t t t t nil))

(deftest array-in-bounds-p.12
  (let ((a (make-array '(10) :fill-pointer 5 :element-type 'bit :initial-element 0)))
    (loop for i from -1 to 10 collect (notnot (array-in-bounds-p a i))))
  (nil t t t t t t t t t t nil))

(deftest array-in-bounds-p.13
  (let ((a (make-array '(10) :fill-pointer 5 :element-type 'base-char :initial-element #\x)))
    (loop for i from -1 to 10 collect (notnot (array-in-bounds-p a i))))
  (nil t t t t t t t t t t nil))

(deftest array-in-bounds-p.14
  (let ((a (make-array '(10) :fill-pointer 5 :element-type 'character :initial-element #\x)))
    (loop for i from -1 to 10 collect (notnot (array-in-bounds-p a i))))
  (nil t t t t t t t t t t nil))

;;; Displaced arrays

(deftest array-in-bounds-p.15
  (let* ((a1 (make-array '(20)))
	 (a2 (make-array '(10) :displaced-to a1)))
    (loop for i from -1 to 10 collect (notnot (array-in-bounds-p a2 i))))
  (nil t t t t t t t t t t nil))

(deftest array-in-bounds-p.16
  (let* ((a1 (make-array '(20) :element-type 'bit :initial-element 0))
	 (a2 (make-array '(10) :displaced-to a1 :element-type 'bit)))
    (loop for i from -1 to 10 collect (notnot (array-in-bounds-p a2 i))))
  (nil t t t t t t t t t t nil))

(deftest array-in-bounds-p.17
  (let* ((a1 (make-array '(20) :element-type 'character :initial-element #\x))
	 (a2 (make-array '(10) :displaced-to a1 :element-type 'character)))
    (loop for i from -1 to 10 collect (notnot (array-in-bounds-p a2 i))))
  (nil t t t t t t t t t t nil))

;;; Multidimensional arrays

(deftest array-in-bounds-p.18
  (let ((a (make-array '(3 4))))
    (loop for i from -1 to 3 collect
	  (loop for j from -1 to 4 collect
		(notnot (array-in-bounds-p a i j)))))
  ((nil nil nil nil nil nil)
   (nil t   t   t   t   nil)
   (nil t   t   t   t   nil)
   (nil t   t   t   t   nil)
   (nil nil nil nil nil nil)))

(deftest array-in-bounds-p.19
  (let ((a (make-array '(1 3 4) :adjustable t)))
    (loop for i from -1 to 3 collect
	  (loop for j from -1 to 4 collect
		(notnot (array-in-bounds-p a 0 i j)))))
  ((nil nil nil nil nil nil)
   (nil t   t   t   t   nil)
   (nil t   t   t   t   nil)
   (nil t   t   t   t   nil)
   (nil nil nil nil nil nil)))

;;; Very large indices

(deftest array-in-bounds-p.20
  (array-in-bounds-p #(a b c) (1+ most-positive-fixnum))
  nil)

(deftest array-in-bounds-p.21
  (array-in-bounds-p #(a b c) (1- most-negative-fixnum))
  nil)

(deftest array-in-bounds-p.22
  (array-in-bounds-p #(a b c) 1000000000000000000)
  nil)

(deftest array-in-bounds-p.23
  (array-in-bounds-p #(a b c) -1000000000000000000)
  nil)

;;; Macro expansion

(deftest array-in-bounds-p.24
  (macrolet ((%m (z) z)) (array-in-bounds-p (expand-in-current-env (%m #(a b))) 3))
  nil)

(deftest array-in-bounds-p.25
  (macrolet ((%m (z) z))
	    (array-in-bounds-p #(a b) (expand-in-current-env (%m 2))))
  nil)

;;; Order of evaluation tests

(deftest array-in-bounds-p.order.1
  (let ((x 0) y z)
    (values
     (array-in-bounds-p (progn (setf y (incf x))
			       #())
			(progn (setf z (incf x))
			       10))
     x y z))
  nil 2 1 2)

;;; Error tests

(deftest array-in-bounds-p.error.1
  (signals-error (array-in-bounds-p) program-error)
  t)
