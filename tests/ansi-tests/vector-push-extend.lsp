;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan 25 08:04:35 2003
;;;; Contains: Tests for VECTOR-PUSH-EXTEND

(in-package :cl-test)

(deftest vector-push-extend.1
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(a b c d e)))
	(i 0) x y)
    (values
     (fill-pointer a)
     (vector-push-extend (progn (setf x (incf i)) 'x)
			 (progn (setf y (incf i)) a))
     (fill-pointer a)
     a
     i x y))
  2 2 3 #(a b x) 2 1 2)

(deftest vector-push-extend.2
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(a b c d e))))
    (values
     (fill-pointer a)
     (vector-push-extend 'x a)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(a b c d e x))

(deftest vector-push-extend.3
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents "abcde"
		       :element-type 'base-char)))
    (values
     (fill-pointer a)
     (vector-push-extend #\x a)
     (fill-pointer a)
     a))
  2 2 3 "abx")

(deftest vector-push-extend.4
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents "abcde"
		       :element-type 'base-char))
	(i 0) x y z)
    (values
     (fill-pointer a)
     (vector-push-extend (progn (setf x (incf i)) #\x)
			 (progn (setf y (incf i)) a)
			 (progn (setf z (incf i)) 1))
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a
     i x y z))
  5 5 6 nil "abcdex" 3 1 2 3)

(deftest vector-push-extend.5
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents "abcde"
		       :element-type 'character)))
    (values
     (fill-pointer a)
     (vector-push-extend #\x a)
     (fill-pointer a)
     a))
  2 2 3 "abx")

(deftest vector-push-extend.6
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents "abcde"
		       :element-type 'character)))
    (values
     (fill-pointer a)
     (vector-push-extend #\x a 10)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil "abcdex")

(deftest vector-push-extend.7
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(0 1 1 0 0)
		       :element-type 'bit)))
    (values
     (fill-pointer a)
     (vector-push-extend 0 a)
     (fill-pointer a)
     a))
  2 2 3 #*010)

(deftest vector-push-extend.8
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(0 0 0 0 0)
		       :element-type 'bit)))
    (values
     (fill-pointer a)
     (vector-push-extend 1 a 100)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #*000001)

(deftest vector-push-extend.9
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1 2 3 4 5)
		       :element-type 'fixnum)))
    (values
     (fill-pointer a)
     (vector-push-extend 0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1 2 0))

(deftest vector-push-extend.10
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(1 2 3 4 5)
		       :element-type 'fixnum)))
    (values
     (fill-pointer a)
     (vector-push-extend 0 a 1)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(1 2 3 4 5 0))

(deftest vector-push-extend.11
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1 2 3 4 5)
		       :element-type '(integer 0 (256)))))
    (values
     (fill-pointer a)
     (vector-push-extend 0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1 2 0))

(deftest vector-push-extend.12
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(1 2 3 4 5)
		       :element-type '(integer 0 (256)))))
    (values
     (fill-pointer a)
     (vector-push-extend 0 a 1)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(1 2 3 4 5 0))

(deftest vector-push-extend.13
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1.0s0 2.0s0 3.0s0 4.0s0 5.0s0)
		       :element-type 'short-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0s0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1.0s0 2.0s0 0.0s0))

(deftest vector-push-extend.14
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(1.0s0 2.0s0 3.0s0 4.0s0 5.0s0)
		       :element-type 'short-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0s0 a 1)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(1.0s0 2.0s0 3.0s0 4.0s0 5.0s0 0.0s0))

(deftest vector-push-extend.15
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0)
		       :element-type 'single-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0f0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1.0f0 2.0f0 0.0f0))

(deftest vector-push-extend.16
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0)
		       :element-type 'single-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0f0 a 1)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0 0.0f0))


(deftest vector-push-extend.17
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0)
		       :element-type 'double-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0d0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1.0d0 2.0d0 0.0d0))

(deftest vector-push-extend.18
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0)
		       :element-type 'double-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0d0 a 1)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0 0.0d0))

(deftest vector-push-extend.19
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1.0l0 2.0l0 3.0l0 4.0l0 5.0l0)
		       :element-type 'long-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0l0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1.0l0 2.0l0 0.0l0))

(deftest vector-push-extend.20
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(1.0l0 2.0l0 3.0l0 4.0l0 5.0l0)
		       :element-type 'long-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0l0 a 1)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(1.0l0 2.0l0 3.0l0 4.0l0 5.0l0 0.0l0))


;;; Tests on displaced arrays

(deftest vector-push-extend.21
  (let* ((a1 (make-array 10 :initial-element nil))
	 (a2 (make-array 6 :displaced-to a1
			 :displaced-index-offset 2
			 :fill-pointer 0)))
    (values
     (fill-pointer a2)
     (map 'list #'identity a2)
     (vector-push-extend 'foo a2)
     (fill-pointer a2)
     (map 'list #'identity a2)
     (map 'list #'identity a1)))
  0
  ()
  0
  1
  (foo)
  (nil nil foo nil nil nil nil nil nil nil))

(deftest vector-push-extend.22
  (let* ((a1 (make-array 6 :initial-element nil))
	 (a2 (make-array 0 :displaced-to a1
			 :displaced-index-offset 2
			 :adjustable t
			 :fill-pointer 0)))
    (values
     (fill-pointer a2)
     (map 'list #'identity a2)
     (vector-push-extend 'foo a2)
     (fill-pointer a2)
     (map 'list #'identity a2)
     (map 'list #'identity a1)
     (notnot (adjustable-array-p a2))
     (multiple-value-list (array-displacement a2))
     ))
  0
  ()
  0
  1
  (foo)
  (nil nil nil nil nil nil)
  t
  (nil 0))

(deftest vector-push-extend.23
  (let* ((a1 (make-array 10 :initial-element nil))
	 (a2 (make-array 6 :displaced-to a1
			 :displaced-index-offset 2
			 :adjustable t
			 :fill-pointer 1)))
    (values
     (fill-pointer a2)
     (map 'list #'identity a2)
     (vector-push-extend 'foo a2)
     (fill-pointer a2)
     (map 'list #'identity a2)
     (map 'list #'identity a1)
     (notnot (adjustable-array-p a2))
     (eqt (array-displacement a2) a1)
     (nth-value 1 (array-displacement a2))
     ))
  1
  (nil)
  1
  2
  (nil foo)
  (nil nil nil foo nil nil nil nil nil nil)
  t
  t
  2)

(deftest vector-push-extend.24
  (let* ((a1 (make-array 4 :initial-element nil))
	 (a2 (make-array 2 :displaced-to a1
			 :displaced-index-offset 2
			 :adjustable t
			 :fill-pointer 2)))
    (values
     (map 'list #'identity a1)
     (map 'list #'identity a2)
     (vector-push-extend 'foo a2 7)
     (fill-pointer a2)
     (map 'list #'identity a1)
     (map 'list #'identity a2)
     (array-dimension a2 0)
     (notnot (adjustable-array-p a2))
     (multiple-value-list (array-displacement a2))))
  (nil nil nil nil)
  (nil nil)
  2
  3
  (nil nil nil nil)
  (nil nil foo)
  9
  t
  (nil 0))

;;; Integer vectors

(deftest vector-push-extend.25
  (loop for adj in '(nil t)
	nconc
	(loop for bits from 1 to 64
	      for etype = `(unsigned-byte ,bits)
	      for a1 = (make-array 10 :initial-element 0
				   :element-type etype)
	      for a2 =(make-array 6
				  :element-type etype
				  :displaced-to a1
				  :displaced-index-offset 2
				  :adjustable adj
				  :fill-pointer 0)
	      for result = (list (fill-pointer a2)
				 (map 'list #'identity a2)
				 (vector-push-extend 1 a2)
				 (fill-pointer a2)
				 (map 'list #'identity a2)
				 (map 'list #'identity a1))
	      unless (equal result '(0 () 0 1 (1) (0 0 1 0 0 0 0 0 0 0)))
	      collect (list etype adj result)))
  nil)

(deftest vector-push-extend.26
  (loop for bits from 1 to 64
	for etype = `(unsigned-byte ,bits)
	for a1 = (make-array 8 :initial-element 0
			     :element-type etype)
	for a2 = (make-array 6
			     :element-type etype
			     :displaced-to a1
			     :displaced-index-offset 2
			     :adjustable t
			     :fill-pointer 6)
	for result = (list (fill-pointer a2)
			   (map 'list #'identity a2)
			   (vector-push-extend 1 a2)
			   (fill-pointer a2)
			   (map 'list #'identity a2)
			   (map 'list #'identity a1)
			   (notnot (adjustable-array-p a2))
			   (multiple-value-list (array-displacement a1)))
	unless (equal result '(6 (0 0 0 0 0 0) 6 7 (0 0 0 0 0 0 1)
				 (0 0 0 0 0 0 0 0) t (nil 0)))
	collect (list etype result))
  nil)

;;; strings

(deftest vector-push-extend.27
  (loop for adj in '(nil t)
	nconc
	(loop for etype in '(character base-char standard-char)
	      for a1 = (make-array 10 :initial-element #\a
				   :element-type etype)
	      for a2 =(make-array 6
				  :element-type etype
				  :displaced-to a1
				  :displaced-index-offset 2
				  :adjustable adj
				  :fill-pointer 0)
	      for result = (list (fill-pointer a2)
				 (map 'list #'identity a2)
				 (vector-push-extend #\b a2)
				 (fill-pointer a2)
				 (map 'list #'identity a2)
				 (map 'list #'identity a1))
	      unless (equal result '(0 () 0 1 (#\b) (#\a #\a #\b #\a #\a #\a #\a #\a #\a #\a)))
	      collect (list etype adj result)))
  nil)

(deftest vector-push-extend.28
  (loop for etype in '(character base-char standard-char)
	for a1 = (make-array 8 :initial-element #\a
			     :element-type etype)
	for a2 = (make-array 6
			     :element-type etype
			     :displaced-to a1
			     :displaced-index-offset 2
			     :adjustable t
			     :fill-pointer 6)
	for result = (list (fill-pointer a2)
			   (map 'list #'identity a2)
			   (vector-push-extend #\b a2)
			   (fill-pointer a2)
			   (map 'list #'identity a2)
			   (map 'list #'identity a1)
			   (notnot (adjustable-array-p a2))
			   (multiple-value-list (array-displacement a1)))
	unless (equal result '(6 #.(coerce "aaaaaa" 'list)
				 6 7
				 #.(coerce "aaaaaab" 'list)
				 #.(coerce "aaaaaaaa" 'list)
				 t (nil 0)))
	collect (list etype result))
  nil)

;;; float tests

(deftest vector-push-extend.29
  (loop for adj in '(nil t)
	nconc
	(loop for etype in '(short-float single-float double-float long-float)
	      for zero in '(0.0s0 0.0f0 0.0d0 0.0l0)
	      for one in '(1.0s0 1.0f0 1.0d0 1.0l0)
	      for a1 = (make-array 10 :initial-element zero
				   :element-type etype)
	      for a2 =(make-array 6
				  :element-type etype
				  :displaced-to a1
				  :displaced-index-offset 2
				  :adjustable adj
				  :fill-pointer 0)
	      for result = (list (fill-pointer a2)
				 (map 'list #'identity a2)
				 (vector-push-extend one a2)
				 (fill-pointer a2)
				 (map 'list #'identity a2)
				 (map 'list #'identity a1))
	      unless (equal result `(0 () 0 1 (,one) (,zero ,zero ,one ,zero ,zero ,zero ,zero ,zero ,zero ,zero)))
	      collect (list etype adj result)))
  nil)

(deftest vector-push-extend.30
  (loop for etype in '(short-float single-float double-float long-float)
	for zero in '(0.0s0 0.0f0 0.0d0 0.0l0)
	for one in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for a1 = (make-array 8 :initial-element zero
			     :element-type etype)
	for a2 = (make-array 6
			     :element-type etype
			     :displaced-to a1
			     :displaced-index-offset 2
			     :adjustable t
			     :fill-pointer 6)
	for result = (list (fill-pointer a2)
			   (map 'list #'identity a2)
			   (vector-push-extend one a2)
			   (fill-pointer a2)
			   (map 'list #'identity a2)
			   (map 'list #'identity a1)
			   (notnot (adjustable-array-p a2))
			   (multiple-value-list (array-displacement a1)))
	unless (equal result `(6 (,zero ,zero ,zero ,zero ,zero ,zero)
				 6 7
				 (,zero ,zero ,zero ,zero ,zero ,zero ,one)
				 (,zero ,zero ,zero ,zero ,zero ,zero ,zero ,zero)
				 t (nil 0)))
	collect (list etype result))
  nil)


;;; Error tests

(defun vector-push-extend-error-test (seq val)
  (declare (optimize (safety 3)))
  (handler-case
   (eval `(let ((a (copy-seq ,seq)))
	    (declare (optimize (safety 3)))
	    (or (notnot (array-has-fill-pointer-p a))
		(vector-push-extend ',val a 1))))
   (error () t)))

(deftest vector-push-extend.error.1
  (vector-push-extend-error-test #(a b c d) 'x)
  t)

(deftest vector-push-extend.error.2
  (vector-push-extend-error-test #*00000 1)
  t)

(deftest vector-push-extend.error.3
  (vector-push-extend-error-test "abcde" #\x)
  t)

(deftest vector-push-extend.error.4
  (vector-push-extend-error-test #() 'x)
  t)

(deftest vector-push-extend.error.5
  (vector-push-extend-error-test #* 1)
  t)

(deftest vector-push-extend.error.6
  (vector-push-extend-error-test "" #\x)
  t)

(deftest vector-push-extend.error.7
  (vector-push-extend-error-test (make-array '5 :element-type 'base-char
				      :initial-element #\a)
			  #\x)
  t)

(deftest vector-push-extend.error.8
  (vector-push-extend-error-test (make-array '5 :element-type '(integer 0 (256))
				      :initial-element 0)
			  17)
  t)

(deftest vector-push-extend.error.9
  (vector-push-extend-error-test (make-array '5 :element-type 'float
				      :initial-element 1.0)
			  2.0)
  t)

(deftest vector-push-extend.error.10
  (vector-push-extend-error-test (make-array '5 :element-type 'short-float
				      :initial-element 1.0s0)
			  2.0s0)
  t)

(deftest vector-push-extend.error.11
  (vector-push-extend-error-test (make-array '5 :element-type 'long-float
				      :initial-element 1.0l0)
			  2.0l0)
  t)

(deftest vector-push-extend.error.12
  (vector-push-extend-error-test (make-array '5 :element-type 'single-float
				      :initial-element 1.0f0)
			  2.0f0)
  t)

(deftest vector-push-extend.error.13
  (vector-push-extend-error-test (make-array '5 :element-type 'double-float
				      :initial-element 1.0d0)
			  2.0d0)
  t)

(deftest vector-push-extend.error.14
  (signals-error (vector-push-extend) program-error)
  t)

(deftest vector-push-extend.error.15
  (signals-error (vector-push-extend (vector 1 2 3)) program-error)
  t)

(deftest vector-push-extend.error.16
  (signals-error (vector-push-extend (vector 1 2 3) 4 1 nil) program-error)
  t)

(deftest vector-push-extend.error.17
  (handler-case
   (eval
    `(locally
      (declare (optimize (safety 3)))
      (let ((a (make-array '5 :fill-pointer t :adjustable nil
			   :initial-element nil)))
	(or (notnot (adjustable-array-p a))  ; It's actually adjustable, or...
	    (vector-push-extend a 'x)        ; ... this fails
	    ))))
   (error () t))
  t)
