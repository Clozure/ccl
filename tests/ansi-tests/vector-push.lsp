;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan 25 00:55:43 2003
;;;; Contains: Tests for VECTOR-PUSH

(in-package :cl-test)

(deftest vector-push.1
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(a b c d e)))
	(i 0) x y)
    (values
     (fill-pointer a)
     (vector-push (progn (setf x (incf i)) 'x)
		  (progn (setf y (incf i)) a))
     (fill-pointer a)
     a i x y))
  2 2 3 #(a b x) 2 1 2)


(deftest vector-push.2
  (let ((a (make-array '(5) :fill-pointer 5
		       :initial-contents '(a b c d e))))
    (values
     (fill-pointer a)
     (vector-push 'x a)
     (fill-pointer a)
     a))
  5 nil 5 #(a b c d e))

(deftest vector-push.3
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents "abcde"
		       :element-type 'base-char)))
    (values
     (fill-pointer a)
     (vector-push #\x a)
     (fill-pointer a)
     a))
  2 2 3 "abx")

(deftest vector-push.4
  (let ((a (make-array '(5) :fill-pointer 5
		       :initial-contents "abcde"
		       :element-type 'base-char)))
    (values
     (fill-pointer a)
     (vector-push #\x a)
     (fill-pointer a)
     a))
  5 nil 5 "abcde")

(deftest vector-push.5
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents "abcde"
		       :element-type 'character)))
    (values
     (fill-pointer a)
     (vector-push #\x a)
     (fill-pointer a)
     a))
  2 2 3 "abx")

(deftest vector-push.6
  (let ((a (make-array '(5) :fill-pointer 5
		       :initial-contents "abcde"
		       :element-type 'character)))
    (values
     (fill-pointer a)
     (vector-push #\x a)
     (fill-pointer a)
     a))
  5 nil 5 "abcde")

(deftest vector-push.7
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(0 1 1 0 0)
		       :element-type 'bit)))
    (values
     (fill-pointer a)
     (vector-push 0 a)
     (fill-pointer a)
     a))
  2 2 3 #*010)

(deftest vector-push.8
  (let ((a (make-array '(5) :fill-pointer 5
		       :initial-contents '(0 0 0 0 0)
		       :element-type 'bit)))
    (values
     (fill-pointer a)
     (vector-push 1 a)
     (fill-pointer a)
     a))
  5 nil 5 #*00000)

(deftest vector-push.9
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1 2 3 4 5)
		       :element-type 'fixnum)))
    (values
     (fill-pointer a)
     (vector-push 0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1 2 0))

(deftest vector-push.10
  (let ((a (make-array '(5) :fill-pointer 5
		       :initial-contents '(1 2 3 4 5)
		       :element-type 'fixnum)))
    (values
     (fill-pointer a)
     (vector-push 0 a)
     (fill-pointer a)
     a))
  5 nil 5 #(1 2 3 4 5))

(deftest vector-push.11
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1 2 3 4 5)
		       :element-type '(integer 0 (256)))))
    (values
     (fill-pointer a)
     (vector-push 0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1 2 0))

(deftest vector-push.12
  (let ((a (make-array '(5) :fill-pointer 5
		       :initial-contents '(1 2 3 4 5)
		       :element-type '(integer 0 (256)))))
    (values
     (fill-pointer a)
     (vector-push 0 a)
     (fill-pointer a)
     a))
  5 nil 5 #(1 2 3 4 5))

(deftest vector-push.13
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1.0s0 2.0s0 3.0s0 4.0s0 5.0s0)
		       :element-type 'short-float)))
    (values
     (fill-pointer a)
     (vector-push 0.0s0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1.0s0 2.0s0 0.0s0))

(deftest vector-push.14
  (let ((a (make-array '(5) :fill-pointer 5
		       :initial-contents '(1.0s0 2.0s0 3.0s0 4.0s0 5.0s0)
		       :element-type 'short-float)))
    (values
     (fill-pointer a)
     (vector-push 0.0s0 a)
     (fill-pointer a)
     a))
  5 nil 5 #(1.0s0 2.0s0 3.0s0 4.0s0 5.0s0))

(deftest vector-push.15
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0)
		       :element-type 'single-float)))
    (values
     (fill-pointer a)
     (vector-push 0.0f0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1.0f0 2.0f0 0.0f0))

(deftest vector-push.16
  (let ((a (make-array '(5) :fill-pointer 5
		       :initial-contents '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0)
		       :element-type 'single-float)))
    (values
     (fill-pointer a)
     (vector-push 0.0f0 a)
     (fill-pointer a)
     a))
  5 nil 5 #(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0))


(deftest vector-push.17
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0)
		       :element-type 'double-float)))
    (values
     (fill-pointer a)
     (vector-push 0.0d0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1.0d0 2.0d0 0.0d0))

(deftest vector-push.18
  (let ((a (make-array '(5) :fill-pointer 5
		       :initial-contents '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0)
		       :element-type 'double-float)))
    (values
     (fill-pointer a)
     (vector-push 0.0d0 a)
     (fill-pointer a)
     a))
  5 nil 5 #(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0))

(deftest vector-push.19
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1.0l0 2.0l0 3.0l0 4.0l0 5.0l0)
		       :element-type 'long-float)))
    (values
     (fill-pointer a)
     (vector-push 0.0l0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1.0l0 2.0l0 0.0l0))

(deftest vector-push.20
  (let ((a (make-array '(5) :fill-pointer 5
		       :initial-contents '(1.0l0 2.0l0 3.0l0 4.0l0 5.0l0)
		       :element-type 'long-float)))
    (values
     (fill-pointer a)
     (vector-push 0.0l0 a)
     (fill-pointer a)
     a))
  5 nil 5 #(1.0l0 2.0l0 3.0l0 4.0l0 5.0l0))



;;; Error tests

(defun vector-push-error-test (seq val)
  (declare (optimize (safety 3)))
  (handler-case
   (eval `(let ((a (copy-seq ,seq)))
	    (declare (optimize (safety 3)))
	    (or (notnot (array-has-fill-pointer-p a))
		(vector-push ',val a))))
   (error () t)))

(deftest vector-push.error.1
  (vector-push-error-test #(a b c d) 'x)
  t)

(deftest vector-push.error.2
  (vector-push-error-test #*00000 1)
  t)

(deftest vector-push.error.3
  (vector-push-error-test "abcde" #\x)
  t)

(deftest vector-push.error.4
  (vector-push-error-test #() 'x)
  t)

(deftest vector-push.error.5
  (vector-push-error-test #* 1)
  t)

(deftest vector-push.error.6
  (vector-push-error-test "" #\x)
  t)

(deftest vector-push.error.7
  (vector-push-error-test (make-array '5 :element-type 'base-char
				      :initial-element #\a)
			  #\x)
  t)

(deftest vector-push.error.8
  (vector-push-error-test (make-array '5 :element-type '(integer 0 (256))
				      :initial-element 0)
			  17)
  t)

(deftest vector-push.error.9
  (vector-push-error-test (make-array '5 :element-type 'float
				      :initial-element 1.0)
			  2.0)
  t)

(deftest vector-push.error.10
  (vector-push-error-test (make-array '5 :element-type 'short-float
				      :initial-element 1.0s0)
			  2.0s0)
  t)

(deftest vector-push.error.11
  (vector-push-error-test (make-array '5 :element-type 'long-float
				      :initial-element 1.0l0)
			  2.0l0)
  t)

(deftest vector-push.error.12
  (vector-push-error-test (make-array '5 :element-type 'single-float
				      :initial-element 1.0f0)
			  2.0f0)
  t)

(deftest vector-push.error.13
  (vector-push-error-test (make-array '5 :element-type 'double-float
				      :initial-element 1.0d0)
			  2.0d0)
  t)

(deftest vector-push.error.14
  (signals-error (vector-push) program-error)
  t)

(deftest vector-push.error.15
  (signals-error (vector-push (vector 1 2 3)) program-error)
  t)

(deftest vector-push.error.16
  (signals-error (vector-push (vector 1 2 3) 4 nil) program-error)
  t)
