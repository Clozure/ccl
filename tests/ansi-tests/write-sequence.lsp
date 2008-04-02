;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 21 04:07:58 2004
;;;; Contains: Tests of WRITE-SEQUENCE

(in-package :cl-test)

(defmacro def-write-sequence-test (name input args &rest expected)
  `(deftest ,name
     (let ((s ,input))
       (with-output-to-string
	 (os)
	 (assert (eq (write-sequence s os ,@args) s))))
     ,@expected))

;;; on strings

(def-write-sequence-test write-sequence.string.1 "abcde" () "abcde")
(def-write-sequence-test write-sequence.string.2 "abcde" (:start 1) "bcde")
(def-write-sequence-test write-sequence.string.3 "abcde" (:end 3) "abc")
(def-write-sequence-test write-sequence.string.4 "abcde"
  (:start 1 :end 4) "bcd")
(def-write-sequence-test write-sequence.string.5 "abcde" (:end nil) "abcde")
(def-write-sequence-test write-sequence.string.6 "abcde" (:start 3 :end 3) "")
(def-write-sequence-test write-sequence.string.7 "abcde"
  (:end nil :start 1) "bcde")
(def-write-sequence-test write-sequence.string.8 "abcde"
  (:allow-other-keys nil) "abcde")
(def-write-sequence-test write-sequence.string.9 "abcde"
  (:allow-other-keys t :foo nil) "abcde")
(def-write-sequence-test write-sequence.string.10 "abcde"
  (:allow-other-keys t :allow-other-keys nil :foo nil) "abcde")
(def-write-sequence-test write-sequence.string.11 "abcde"
  (:bar 'x :allow-other-keys t) "abcde")
(def-write-sequence-test write-sequence.string.12 "abcde"
  (:start 1 :end 4 :start 2 :end 3) "bcd")
(def-write-sequence-test write-sequence.string.13 "" () "")

(defmacro def-write-sequence-special-test (name string args expected)
  `(deftest ,name
     (let ((str ,string)
	   (expected ,expected))
       (do-special-strings
	(s str nil)
	(let ((out (with-output-to-string
		     (os)
		     (assert (eq (write-sequence s os ,@args) s)))))
	  (assert (equal out expected)))))
     nil))

(def-write-sequence-special-test write-sequence.string.14 "12345" () "12345")
(def-write-sequence-special-test write-sequence.string.15 "12345" (:start 1 :end 3) "23")

;;; on lists

(def-write-sequence-test write-sequence.list.1 (coerce "abcde" 'list)
  () "abcde")
(def-write-sequence-test write-sequence.list.2 (coerce "abcde" 'list)
  (:start 1) "bcde")
(def-write-sequence-test write-sequence.list.3 (coerce "abcde" 'list)
  (:end 3) "abc")
(def-write-sequence-test write-sequence.list.4 (coerce "abcde" 'list)
  (:start 1 :end 4) "bcd")
(def-write-sequence-test write-sequence.list.5 (coerce "abcde" 'list)
  (:end nil) "abcde")
(def-write-sequence-test write-sequence.list.6 (coerce "abcde" 'list)
  (:start 3 :end 3) "")
(def-write-sequence-test write-sequence.list.7 (coerce "abcde" 'list)
  (:end nil :start 1) "bcde")
(def-write-sequence-test write-sequence.list.8 () () "")


;;; on vectors

(def-write-sequence-test write-sequence.simple-vector.1
  (coerce "abcde" 'simple-vector) () "abcde")
(def-write-sequence-test write-sequence.simple-vector.2
  (coerce "abcde" 'simple-vector) (:start 1) "bcde")
(def-write-sequence-test write-sequence.simple-vector.3
  (coerce "abcde" 'simple-vector) (:end 3) "abc")
(def-write-sequence-test write-sequence.simple-vector.4
  (coerce "abcde" 'simple-vector) (:start 1 :end 4) "bcd")
(def-write-sequence-test write-sequence.simple-vector.5
  (coerce "abcde" 'simple-vector) (:end nil) "abcde")
(def-write-sequence-test write-sequence.simple-vector.6
  (coerce "abcde" 'simple-vector) (:start 3 :end 3) "")
(def-write-sequence-test write-sequence.simple-vector.7
  (coerce "abcde" 'simple-vector) (:end nil :start 1) "bcde")
(def-write-sequence-test write-sequence.simple-vector.8 #() () "")

;;; on vectors with fill pointers

(def-write-sequence-test write-sequence.fill-vector.1
  (make-array 10 :initial-contents "abcde     " :fill-pointer 5) () "abcde")
(def-write-sequence-test write-sequence.fill-vector.2
  (make-array 10 :initial-contents "abcde     " :fill-pointer 5)
  (:start 1) "bcde")
(def-write-sequence-test write-sequence.fill-vector.3
  (make-array 10 :initial-contents "abcde     " :fill-pointer 5)
  (:end 3) "abc")
(def-write-sequence-test write-sequence.fill-vector.4
  (make-array 10 :initial-contents "abcde     " :fill-pointer 5)
  (:start 1 :end 4) "bcd")
(def-write-sequence-test write-sequence.fill-vector.5
  (make-array 10 :initial-contents "abcde     " :fill-pointer 5)
  (:end nil) "abcde")
(def-write-sequence-test write-sequence.fill-vector.6
  (make-array 10 :initial-contents "abcde     " :fill-pointer 5)
  (:start 3 :end 3) "")
(def-write-sequence-test write-sequence.fill-vector.7
  (make-array 10 :initial-contents "abcde     " :fill-pointer 5)
  (:end nil :start 1) "bcde")

;;; on bit vectors

(defmacro def-write-sequence-bv-test (name input args expected)
  `(deftest ,name
     (let ((s ,input)
	   (expected ,expected))
       (with-open-file
	(os "tmp.dat" :direction :output
	    :element-type '(unsigned-byte 8)
	    :if-exists :supersede)
	 (assert (eq (write-sequence s os ,@args) s)))
       (with-open-file
	(is "tmp.dat" :direction :input
	    :element-type '(unsigned-byte 8))
	 (loop for i from 0 below (length expected)
	       for e = (elt expected i)
	       always (eql (read-byte is) e))))
     t))

(def-write-sequence-bv-test write-sequence.bv.1 #*00111010
  () #*00111010)
(def-write-sequence-bv-test write-sequence.bv.2 #*00111010
  (:start 1) #*0111010)
(def-write-sequence-bv-test write-sequence.bv.3 #*00111010
  (:end 5) #*00111)
(def-write-sequence-bv-test write-sequence.bv.4 #*00111010
  (:start 1 :end 6) #*01110)
(def-write-sequence-bv-test write-sequence.bv.5 #*00111010
  (:start 1 :end nil) #*0111010)
(def-write-sequence-bv-test write-sequence.bv.6 #*00111010
  (:start 1 :end nil :end 4) #*0111010)


;;; Error tests

(deftest write-sequence.error.1
  (signals-error (write-sequence) program-error)
  t)

(deftest write-sequence.error.2
  (signals-error (write-sequence "abcde") program-error)
  t)

(deftest write-sequence.error.3
  (signals-error (write-sequence '(#\a . #\b) *standard-output*) type-error)
  t)

(deftest write-sequence.error.4
  (signals-error (write-sequence #\a *standard-output*) type-error)
  t)

(deftest write-sequence.error.5
  (signals-error (write-sequence "ABC" *standard-output* :start -1) type-error)
  t)

(deftest write-sequence.error.6
  (signals-error (write-sequence "ABC" *standard-output* :start 'x) type-error)
  t)

(deftest write-sequence.error.7
  (signals-error (write-sequence "ABC" *standard-output* :start 0.0)
		 type-error)
  t)

(deftest write-sequence.error.8
  (signals-error (write-sequence "ABC" *standard-output* :end -1)
		 type-error)
  t)

(deftest write-sequence.error.9
  (signals-error (write-sequence "ABC" *standard-output* :end 'x)
		 type-error)
  t)

(deftest write-sequence.error.10
  (signals-error (write-sequence "ABC" *standard-output* :end 2.0)
		 type-error)
  t)

(deftest write-sequence.error.11
  (signals-error (write-sequence "abcde" *standard-output*
				 :foo nil) program-error)
  t)
	 
(deftest write-sequence.error.12
  (signals-error (write-sequence "abcde" *standard-output*
				 :allow-other-keys nil :foo t)
		 program-error)
  t)

(deftest write-sequence.error.13
  (signals-error (write-sequence "abcde" *standard-output* :start)
		 program-error)
  t)

(deftest write-sequence.error.14
  (check-type-error #'(lambda (x) (write-sequence x *standard-output*))
		    #'sequencep)
  nil)

(deftest write-sequence.error.15
  (check-type-error #'(lambda (x) (write-sequence "abcde" *standard-output*
						  :start x))
		    (typef 'unsigned-byte))
  nil)

(deftest write-sequence.error.16
  (check-type-error #'(lambda (x) (write-sequence "abcde" *standard-output*
						  :end x))
		    (typef '(or null unsigned-byte)))
  nil)

