;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep 29 21:04:44 2002
;;;; Contains: Tests of CHAR and SCHAR accessors

(in-package :cl-test)

(deftest char.1
  (let ((s "abcd"))
    (values (char s 0) (char s 1) (char s 2) (char s 3)))
  #\a #\b #\c #\d)

(deftest char.2
  (let ((s0 (copy-seq "abcd"))
	(s1 (copy-seq "abcd"))
	(s2 (copy-seq "abcd"))
	(s3 (copy-seq "abcd")))
    (setf (char s0 0) #\X)
    (setf (char s1 1) #\X)
    (setf (char s2 2) #\X)
    (setf (char s3 3) #\X)
    (values s0 s1 s2 s3))
  "Xbcd" "aXcd" "abXd" "abcX")

(deftest char.3
  (let ((s (make-array 6 :element-type 'character
		       :initial-contents '(#\a #\b #\c #\d #\e #\f))))
    (setf (char s 3) #\X)
    s)
  "abcXef")

(deftest char.4
  (let ((s (make-array 6 :element-type 'character
		       :initial-contents '(#\a #\b #\c #\d #\e #\f)
		       :fill-pointer 4)))
    (setf (char s 3) #\X)
    s)
  "abcX")

(deftest char.5
  (let ((s (make-string 5 :initial-element #\a)))
    (setf (char s 3) #\X)
    s)
  "aaaXa")

(deftest char.6
  (let ((s (make-string 5 :initial-element #\a :element-type 'base-char)))
    (setf (char s 3) #\X)
    s)
  "aaaXa")

(deftest char.7
  (let ((s (make-string 5 :initial-element #\a :element-type 'character)))
    (setf (char s 3) #\X)
    s)
  "aaaXa")

(deftest char.8
  (let ((s (make-array 6 :element-type 'character
		       :initial-contents '(#\a #\b #\c #\d #\e #\f)
		       :fill-pointer 4)))
    (setf (char s 5) #\X)
    (setf (fill-pointer s) 6)
    s)
  "abcdeX")

(deftest char.9
  (let ((s (make-string 5 :initial-element #\a
			:element-type 'base-char)))
    (setf (char s 3) #\X)
    s)
  "aaaXa")

(deftest char.10
  (let ((s (make-string 5 :initial-element #\a
			:element-type 'standard-char)))
    (setf (char s 3) #\X)
    s)
  "aaaXa")

(deftest char.order.1
  (let ((i 0) a b)
    (values
     (char (progn (setf a (incf i)) "abc")
	   (progn (setf b (incf i)) 1))
     i a b))
  #\b 2 1 2)

(deftest char.order.2
  (let ((i 0) a b c (s (make-string 5 :initial-element #\z)))
    (values
     (setf
      (char (progn (setf a (incf i)) s)
	    (progn (setf b (incf i)) 1))
      (progn (setf c (incf i)) #\a))
     s i a b c))
  #\a "zazzz" 3 1 2 3)

;;; Error tests

(deftest char.error.1
  (signals-error (char) program-error)
  t)

(deftest char.error.2
  (signals-error (char "abc") program-error)
  t)

(deftest char.error.3
  (signals-error (char "abc" 1 nil) program-error)
  t)

;;; Tests of schar

(deftest schar.1
  (let ((s "abcd")) (values (schar s 0) (schar s 1) (schar s 2) (schar s 3)))
  #\a #\b #\c #\d)

(deftest schar.2
  (let ((s0 (copy-seq "abcd"))
	(s1 (copy-seq "abcd"))
	(s2 (copy-seq "abcd"))
	(s3 (copy-seq "abcd")))
    (setf (schar s0 0) #\X)
    (setf (schar s1 1) #\X)
    (setf (schar s2 2) #\X)
    (setf (schar s3 3) #\X)
    (values s0 s1 s2 s3))
  "Xbcd" "aXcd" "abXd" "abcX")

(deftest schar.3
  (let ((s (make-string 6 :initial-element #\x)))
    (setf (schar s 2) #\X)
    s)
  "xxXxxx")

(deftest schar.4
  (let ((s (make-string 6 :initial-element #\x :element-type 'character)))
    (setf (schar s 2) #\X)
    s)
  "xxXxxx")

(deftest schar.5
  (let ((s (make-string 6 :initial-element #\x :element-type 'standard-char)))
    (setf (schar s 2) #\X)
    s)
  "xxXxxx")

(deftest schar.6
  (let ((s (make-string 6 :initial-element #\x :element-type 'base-char)))
    (setf (schar s 2) #\X)
    s)
  "xxXxxx")

(deftest schar.7
  (let ((s (make-string 6 :initial-element #\x
			:element-type 'standard-char)))
    (setf (schar s 2) #\X)
    s)
  "xxXxxx")

(deftest schar.order.1
  (let ((i 0) a b)
    (values
     (schar (progn (setf a (incf i)) "abc")
	    (progn (setf b (incf i)) 1))
     i a b))
  #\b 2 1 2)

(deftest schar.order.2
  (let ((i 0) a b c (s (copy-seq "zzzzz")))
    (values
     (setf
      (schar (progn (setf a (incf i)) s)
	     (progn (setf b (incf i)) 1))
      (progn (setf c (incf i)) #\a))
     s i a b c))
  #\a "zazzz" 3 1 2 3)

;;; Error tests

(deftest schar.error.1
  (signals-error (schar) program-error)
  t)

(deftest schar.error.2
  (signals-error (schar "abc") program-error)
  t)

(deftest schar.error.3
  (signals-error (schar "abc" 1 nil) program-error)
  t)

