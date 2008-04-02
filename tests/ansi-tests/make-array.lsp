;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Sep 20 06:47:37 2002
;;;; Contains: Tests for MAKE-ARRAY

(in-package :cl-test)

(compile-and-load "array-aux.lsp")

(deftest make-array.1
  (let ((a (make-array-with-checks 10)))
    (and (symbolp a) a))
  nil)

(deftest make-array.1a
  (let ((a (make-array-with-checks '(10))))
    (and (symbolp a) a))
  nil)

(deftest make-array.2
  (make-array-with-checks 3 :initial-element 'z)
  #(z z z))

(deftest make-array.2a
  (make-array-with-checks 3 :initial-contents '(a b c))
  #(a b c))

(deftest make-array.2b
  (make-array-with-checks 3 :initial-contents #(a b c))
  #(a b c))

(deftest make-array.2c
  (make-array-with-checks 3 :initial-contents "abc")
  #(#\a #\b #\c))

(deftest make-array.2d
  (make-array-with-checks 3 :initial-contents #*010)
  #(0 1 0))

(deftest make-array.3
  (let ((a (make-array-with-checks 5 :element-type 'bit)))
    (and (symbolp a) a))
  nil)

(deftest make-array.4
  (make-array-with-checks 5 :element-type 'bit :initial-element 1)
  #*11111)

(deftest make-array.4a
  (make-array-with-checks 5 :element-type 'bit :initial-contents '(1 0 0 1 0))
  #*10010)

(deftest make-array.4b
  (make-array-with-checks 5 :element-type 'bit :initial-contents #(1 0 0 1 0))
  #*10010)

(deftest make-array.4c
  (make-array-with-checks 5 :element-type 'bit :initial-contents #*10010)
  #*10010)

(deftest make-array.5
  (let ((a (make-array-with-checks 4 :element-type 'character)))
    (and (symbolp a) a))
  nil)

(deftest make-array.5a
  (let ((a (make-array-with-checks '(4) :element-type 'character)))
    (and (symbolp a) a))
  nil)

(deftest make-array.6
  (make-array-with-checks 4 :element-type 'character
			  :initial-element #\x)
  "xxxx")

(deftest make-array.6a
  (make-array-with-checks 4 :element-type 'character
			  :initial-contents '(#\a #\b #\c #\d))
  "abcd")

(deftest make-array.6b
  (make-array-with-checks 4 :element-type 'character
			  :initial-contents "abcd")
  "abcd")

(deftest make-array.7
  (make-array-with-checks 5 :element-type 'symbol
			  :initial-element 'a)
  #(a a a a a))

(deftest make-array.7a
  (make-array-with-checks 5 :element-type 'symbol
			  :initial-contents '(a b c d e))
  #(a b c d e))

(deftest make-array.7b
  (make-array-with-checks '(5) :element-type 'symbol
			  :initial-contents '(a b c d e))
  #(a b c d e))

(deftest make-array.8
  (let ((a (make-array-with-checks 8 :element-type '(integer 0 (256)))))
    ;; Should return a symbol only in error situations
    (and (symbolp a) a))
  nil)

(deftest make-array.8a
  (make-array-with-checks 8 :element-type '(integer 0 (256))
			  :initial-element 9)
  #(9 9 9 9 9 9 9 9))

(deftest make-array.8b
  (make-array-with-checks '(8) :element-type '(integer 0 (256))
			  :initial-contents '(4 3 2 1 9 8 7 6))
  #(4 3 2 1 9 8 7 6))

(deftest make-array.8c
  (loop for i from 1 to 32
	for tp = `(unsigned-byte ,i)
	for a = (make-array 5 :fill-pointer 3 :element-type tp :initial-contents '(1 1 0 0 1))
	when (symbolp a)
	collect (list i tp a))
  nil)

(deftest make-array.8d
  (loop for i from 2 to 32
	for tp = `(signed-byte ,i)
	for a = (make-array 5 :fill-pointer 3 :element-type tp :initial-contents '(1 1 0 0 1))
	when (symbolp a)
	collect (list i tp a))
  nil)

(deftest make-array.8e
  (loop for tp in '(short-float single-float double-float long-float)
	for v in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for a = (make-array 5 :fill-pointer 3 :element-type tp :initial-element v)
	when (symbolp a)
	collect (list tp v a))
  nil)

(deftest make-array.8f
  (loop for tp in '(short-float single-float double-float long-float)
	for v in '(1.0s0 1.0f0 1.0d0 1.0l0)
	for a = (make-array 5 :fill-pointer 3 :element-type `(complex ,tp)
			    :initial-element (complex v))
	when (symbolp a)
	collect (list tp v a))
  nil)

;;; Zero dimensional arrays

(deftest make-array.9
  (let ((a (make-array-with-checks nil)))
    (and (symbolp a) a))
  nil)

(deftest make-array.10
  (make-array-with-checks nil :initial-element 1)
  #0a1)

(deftest make-array.11
  (make-array-with-checks nil :initial-contents 2)
  #0a2)

(deftest make-array.12
  (make-array-with-checks nil :element-type 'bit :initial-contents 1)
  #0a1)

(deftest make-array.12a
  (make-array-with-checks 10 :element-type 'bit :initial-contents '(1 0 0 1 1 0 0 1 0 0)
			  :fill-pointer 6)
  #*100110)

(deftest make-array.12b
  (make-array-with-checks 10 :element-type 'character
			  :initial-contents "abcdefghij"
			  :fill-pointer 8)
  "abcdefgh")

(deftest make-array.12c
  (make-array-with-checks 10 :element-type 'base-char
			  :initial-contents "abcdefghij"
			  :fill-pointer 8)
  "abcdefgh")

(deftest make-array.13
  (make-array-with-checks nil :element-type t :initial-contents 'a)
  #0aa)

;;; Higher dimensional arrays

(deftest make-array.14
  (let ((a (make-array-with-checks '(2 3))))
    (and (symbolp a) a))
  nil)

(deftest make-array.15
  (make-array-with-checks '(2 3) :initial-element 'x)
  #2a((x x x) (x x x)))

(deftest make-array.16
  (equalpt (make-array-with-checks '(0 0))
	   (read-from-string "#2a()"))
  t)

(deftest make-array.17
  (make-array-with-checks '(2 3) :initial-contents '((a b c) (d e f)))
  #2a((a b c) (d e f)))

(deftest make-array.18
  (make-array-with-checks '(2 3) :initial-contents '(#(a b c) #(d e f)))
  #2a((a b c) (d e f)))

(deftest make-array.19
  (make-array-with-checks '(4) :initial-contents
			  (make-array '(10) :initial-element 1
				      :fill-pointer 4))
  #(1 1 1 1))

(deftest make-array.20
  (let ((a (make-array '(10) :initial-element 1
		       :fill-pointer 4)))
    (make-array-with-checks '(3 4) :initial-contents
			    (list a a a)))
  #2a((1 1 1 1) (1 1 1 1) (1 1 1 1)))

(deftest make-array.21
  (make-array-with-checks '(3 4) :initial-contents
			  (make-array '(10) :initial-element '(1 2 3 4)
				      :fill-pointer 3))
  #2a((1 2 3 4) (1 2 3 4) (1 2 3 4)))

(deftest make-array.22
  (loop for i from 3 below (min array-rank-limit 128)
	always
	(equalpt (make-array-with-checks (make-list i :initial-element 0))
		 (read-from-string (format nil "#~Aa()" i))))
  t)

(deftest make-array.23
  (let ((len (1- array-rank-limit)))
    (equalpt (make-array-with-checks (make-list len :initial-element 0))
	     (read-from-string (format nil "#~Aa()" len))))
  t)

;;; (deftest make-array.24
;;;  (make-array-with-checks '(5) :initial-element 'a :displaced-to nil)
;;;  #(a a a a a))

(deftest make-array.25
  (make-array '(4) :initial-element 'x :nonsense-argument t
	      :allow-other-keys t)
  #(x x x x))

(deftest make-array.26
  (make-array '(4) :initial-element 'x
	      :allow-other-keys nil)
  #(x x x x))

(deftest make-array.27
  (make-array '(4) :initial-element 'x
	      :allow-other-keys t
	      :allow-other-keys nil
	      :nonsense-argument t)
  #(x x x x))

(deftest make-array.28
  (let ((*package* (find-package :cl-test)))
    (let ((len (1- (min 10000 array-rank-limit))))
      (equalpt (make-array (make-list len :initial-element 1) :initial-element 'x)
	       (read-from-string (concatenate
				  'string
				  (format nil "#~dA" len)
				  (make-string len :initial-element #\()
				  "x"
				  (make-string len :initial-element #\)))))))
  t)

(deftest make-array.29
  (make-array-with-checks '(5) :element-type '(integer 0 (256))
			  :initial-contents '(0 5 255 119 57))
  #(0 5 255 119 57))

(deftest make-array.30
  (make-array-with-checks '(5) :element-type '(integer -128 127)
			  :initial-contents '(-10 5 -128 86 127))
  #(-10 5 -128 86 127))

(deftest make-array.31
  (make-array-with-checks '(5) :element-type '(integer 0 (65536))
			  :initial-contents '(0 100 65535 7623 13))
  #(0 100 65535 7623 13))

(deftest make-array.32
  (make-array-with-checks '(5) :element-type 'fixnum
			  :initial-contents '(1 2 3 4 5))
  #(1 2 3 4 5))

(deftest make-array.33
  (make-array-with-checks '(5) :element-type 'short-float
			  :initial-contents '(1.0s0 2.0s0 3.0s0 4.0s0 5.0s0))
  #(1.0s0 2.0s0 3.0s0 4.0s0 5.0s0))

(deftest make-array.34
  (make-array-with-checks '(5) :element-type 'single-float
			  :initial-contents '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0))
  #(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0))

(deftest make-array.35
  (make-array-with-checks '(5) :element-type 'double-float
			  :initial-contents '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0))
  #(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0))

(deftest make-array.36
  (make-array-with-checks '(5) :element-type 'long-float
			  :initial-contents '(1.0l0 2.0l0 3.0l0 4.0l0 5.0l0))
  #(1.0l0 2.0l0 3.0l0 4.0l0 5.0l0))


;;; Adjustable arrays

(deftest make-array.adjustable.1
  (let ((a (make-array-with-checks '(10) :adjustable t)))
    (and (symbolp a) a))
  nil)

(deftest make-array.adjustable.2
 (make-array-with-checks '(4) :adjustable t
			 :initial-element 6)
 #(6 6 6 6))

(deftest make-array.adjustable.3
  (make-array-with-checks nil :adjustable t :initial-element 7)
  #0a7)

(deftest make-array.adjustable.4
  (make-array-with-checks '(2 3) :adjustable t :initial-element 7)
  #2a((7 7 7) (7 7 7)))

(deftest make-array.adjustable.5
  (make-array-with-checks '(2 3) :adjustable t
			  :initial-contents '((1 2 3) "abc"))
  #2a((1 2 3) (#\a #\b #\c)))

(deftest make-array.adjustable.6
 (make-array-with-checks '(4) :adjustable t
			 :initial-contents '(a b c d))
 #(a b c d))

(deftest make-array.adjustable.7
 (make-array-with-checks '(4) :adjustable t
			 :fill-pointer t
			 :initial-contents '(a b c d))
 #(a b c d))

(deftest make-array.adjustable.7a
 (make-array-with-checks '(4) :adjustable t
			 :element-type 'bit
			 :fill-pointer t
			 :initial-contents '(1 0 0 1))
 #(1 0 0 1))

(deftest make-array.adjustable.7b
 (make-array-with-checks '(4) :adjustable t
			 :element-type 'base-char
			 :fill-pointer t
			 :initial-contents "abcd")
 "abcd")

(deftest make-array.adjustable.7c
 (make-array-with-checks '(4) :adjustable t
			 :element-type 'character
			 :fill-pointer t
			 :initial-contents "abcd")
 "abcd")

(deftest make-array.adjustable.8
 (make-array-with-checks '(4) :adjustable t
			 :element-type '(integer 0 (256))
			 :initial-contents '(1 4 7 9))
 #(1 4 7 9))

(deftest make-array.adjustable.9
 (make-array-with-checks '(4) :adjustable t
			 :element-type 'base-char
			 :initial-contents "abcd")
 "abcd")

(deftest make-array.adjustable.10
 (make-array-with-checks '(4) :adjustable t
			 :element-type 'bit
			 :initial-contents '(0 1 1 0))
 #*0110)

(deftest make-array.adjustable.11
 (make-array-with-checks '(4) :adjustable t
			 :element-type 'symbol
			 :initial-contents '(a b c d))
 #(a b c d))

;;; Displaced arrays

(deftest make-array.displaced.1
  (let ((a (make-array '(10) :initial-contents '(a b c d e f g h i j))))
    (make-array-with-checks '(5) :displaced-to a))
  #(a b c d e))

(deftest make-array.displaced.2
  (let ((a (make-array '(10) :initial-contents '(a b c d e f g h i j))))
    (make-array-with-checks '(5) :displaced-to a
			    :displaced-index-offset 3))
  #(d e f g h))

(deftest make-array.displaced.3
  (let ((a (make-array '(10) :initial-contents '(a b c d e f g h i j))))
    (make-array-with-checks '(5) :displaced-to a
			    :displaced-index-offset 5))
  #(f g h i j))

(deftest make-array.displaced.4
  (let ((a (make-array '(10) :initial-contents '(a b c d e f g h i j))))
    (make-array-with-checks '(0) :displaced-to a
			    :displaced-index-offset 10))
  #())

(deftest make-array.displaced.5
  (let ((a (make-array '(10) :element-type '(integer 0 (256))
		       :initial-contents '(1 3 5 7 9 11 13 15 17 19))))
    (make-array-with-checks '(5) :element-type '(integer 0 (256))
			    :displaced-to a))
  #(1 3 5 7 9))

(deftest make-array.displaced.6
  (let ((a (make-array '(10) :element-type '(integer 0 (256))
		       :initial-contents '(1 3 5 7 9 11 13 15 17 19))))
    (loop for i from 0 to 5 collect
	  (make-array-with-checks '(5) :element-type '(integer 0 (256))
				  :displaced-to a
				  :displaced-index-offset i)))
  (#(1 3 5 7 9)
   #(3 5 7 9 11)
   #(5 7 9 11 13)
   #(7 9 11 13 15)
   #(9 11 13 15 17)
   #(11 13 15 17 19)))

(deftest make-array.displaced.7
  (let ((a (make-array '(10) :element-type '(integer 0 (256))
		       :initial-contents '(1 3 5 7 9 11 13 15 17 19))))
    (make-array-with-checks '(0) :element-type '(integer 0 (256))
			    :displaced-to a
			    :displaced-index-offset 10))
  #())

(deftest make-array.displaced.8
  (let ((a (make-array '(10) :element-type 'bit
		       :initial-contents '(0 1 1 0 1 1 1 0 1 0))))
    (make-array-with-checks '(5) :element-type 'bit
			    :displaced-to a))
  #*01101)

(deftest make-array.displaced.9
  (let ((a (make-array '(10) :element-type 'bit
		       :initial-contents '(0 1 1 0 1 1 1 0 1 0))))
    (loop for i from 0 to 5 collect
	  (make-array-with-checks '(5) :element-type 'bit
				  :displaced-to a
				  :displaced-index-offset i)))
  (#*01101 #*11011 #*10111 #*01110 #*11101 #*11010))

(deftest make-array.displaced.10
  (let ((a (make-array '(10) :element-type 'bit
		       :initial-contents '(0 1 1 0 1 1 1 0 1 0))))
    (make-array-with-checks '(0) :element-type 'bit
			    :displaced-to a
			    :displaced-index-offset 10))
  #*)

(deftest make-array.displaced.11
  (let ((a (make-array '(10) :element-type 'base-char
		       :initial-contents "abcdefghij")))
    (make-array-with-checks '(5) :element-type 'base-char
			    :displaced-to a))
  "abcde")

(deftest make-array.displaced.12
  (let ((a (make-array '(10) :element-type 'base-char
		       :initial-contents "abcdefghij")))
    (loop for i from 0 to 5 collect
	  (make-array-with-checks '(5) :element-type 'base-char
				  :displaced-to a
				  :displaced-index-offset i)))
  ("abcde"
   "bcdef"
   "cdefg"
   "defgh"
   "efghi"
   "fghij"))

(deftest make-array.displaced.13
  (let ((a (make-array '(10) :element-type 'base-char
		       :initial-contents "abcdefghij")))
    (make-array-with-checks '(0) :element-type 'base-char
			    :displaced-to a
			    :displaced-index-offset 10))
  "")

(deftest make-array.displaced.14
  (let ((a (make-array '(10) :element-type 'character
		       :initial-contents "abcdefghij")))
    (make-array-with-checks '(5) :element-type 'character
			    :displaced-to a))
  "abcde")

(deftest make-array.displaced.15
  (let ((a (make-array '(10) :element-type 'character
		       :initial-contents "abcdefghij")))
    (loop for i from 0 to 5 collect
	  (make-array-with-checks '(5) :element-type 'character
				  :displaced-to a
				  :displaced-index-offset i)))
  ("abcde"
   "bcdef"
   "cdefg"
   "defgh"
   "efghi"
   "fghij"))

(deftest make-array.displaced.16
  (let ((a (make-array '(10) :element-type 'character
		       :initial-contents "abcdefghij")))
    (make-array-with-checks '(0) :element-type 'character
			    :displaced-to a
			    :displaced-index-offset 10))
  "")

;;; Multidimensional displaced arrays

(deftest make-array.displaced.17
  (let ((a (make-array '(3 4) :initial-contents '((1 2 3 4) (5 6 7 8)
						  (9 10 11 12)))))
    (make-array-with-checks '(8) :displaced-to a))
  #(1 2 3 4 5 6 7 8))

(deftest make-array.displaced.18
  (let ((a (make-array '(3 4) :initial-contents '((1 2 3 4) (5 6 7 8)
						  (9 10 11 12)))))
    (make-array-with-checks '(8) :displaced-to a
			    :displaced-index-offset 3))
  #(4 5 6 7 8 9 10 11))

(deftest make-array.displaced.19
  (let ((a (make-array '(3 4) :initial-contents '((1 2 3 4) (5 6 7 8)
						  (9 10 11 12)))))
    (make-array-with-checks '(2 4) :displaced-to a
			    :displaced-index-offset 4))
  #2a((5 6 7 8) (9 10 11 12)))

(deftest make-array.displaced.20
  (let ((a (make-array '(2 3 4)
		       :initial-contents '(((a b c d) (e f g h) (i j k l))
					   ((m n o p) (q r s t) (u v w x))))))
    (make-array-with-checks '(24) :displaced-to a))
  #(a b c d e f g h i j k l m n o p q r s t u v w x))

(deftest make-array.displaced.21
  (let ((a (make-array '(2 3 4)
		       :initial-contents '(((a b c d) (e f g h) (i j k l))
					   ((m n o p) (q r s t) (u v w x))))))
    (make-array-with-checks '(3 8) :displaced-to a))
  #2a((a b c d e f g h) (i j k l m n o p) (q r s t u v w x)))

(deftest make-array.displaced.22
  (let ((a (make-array '(2 3 4)
		       :initial-contents '(((a b c d) (e f g h) (i j k l))
					   ((m n o p) (q r s t) (u v w x))))))
    (make-array-with-checks '(10) :displaced-to a
			    :displaced-index-offset 5))
  #(f g h i j k l m n o))

(deftest make-array.displaced.23
  (let ((a (make-array '(2 3 4)
		       :initial-contents '(((a b c d) (e f g h) (i j k l))
					   ((m n o p) (q r s t) (u v w x))))))
    (make-array-with-checks '(10) :displaced-to a
			    :displaced-index-offset 5
			    :fill-pointer t))
  #(f g h i j k l m n o))

(deftest make-array.displaced.24
  (let ((a (make-array '(2 3 4)
		       :initial-contents '(((a b c d) (e f g h) (i j k l))
					   ((m n o p) (q r s t) (u v w x))))))
    (make-array-with-checks '(10) :displaced-to a
			    :displaced-index-offset 5
			    :fill-pointer 5))
  #(f g h i j))

(deftest make-array.displaced.25
  (let ((a (make-array '(2 3 4)
		       :initial-contents '(((a b c d) (e f g h) (i j k l))
					   ((m n o p) (q r s t) (u v w x))))))
    (make-array-with-checks '(10) :displaced-to a
			    :displaced-index-offset 5
			    :adjustable t))
  #(f g h i j k l m n o))

(deftest make-array.displaced.26
  (let ((a (make-array '(2 3 4)
		       :initial-contents '(((a b c d) (e f g h) (i j k l))
					   ((m n o p) (q r s t) (u v w x))))))
    (make-array-with-checks '(10) :displaced-to a
			    :displaced-index-offset 5
			    :fill-pointer 8
			    :adjustable t))
  #(f g h i j k l m))

(deftest make-array.displaced.27
  (let ((a (make-array '(10)
		       :initial-contents '(1 2 3 4 5 6 7 8 9 10)
		       :fill-pointer t)))
    (make-array-with-checks '(2 4) :displaced-to a))
  #2a((1 2 3 4) (5 6 7 8)))

(deftest make-array.displaced.28
  (let ((a (make-array '(10)
		       :initial-contents '(1 2 3 4 5 6 7 8 9 10)
		       :fill-pointer 4)))
    (make-array-with-checks '(2 4) :displaced-to a))
  #2a((1 2 3 4) (5 6 7 8)))

(deftest make-array.displaced.29
  (let ((a (make-array '(10) :initial-element 0)))
    (prog1
	(make-array-with-checks '(2 4) :displaced-to a)
      (loop for i below 10 do (setf (aref a i) (1+ i)))))
  #2a((1 2 3 4) (5 6 7 8)))

(deftest make-array.displaced.30
  (let* ((a1 (make-array '(10) :initial-element 0))
	 (a2 (make-array '(10) :displaced-to a1)))
    (prog1
	(make-array-with-checks '(2 4) :displaced-to a2)
      (loop for i below 10 do (setf (aref a2 i) (1+ i)))))
  #2a((1 2 3 4) (5 6 7 8)))

(deftest make-array.displaced.31
  (let* ((a1 (make-array '(10) :initial-element 0))
	 (a2 (make-array '(10) :displaced-to a1)))
    (prog1
	(make-array-with-checks '(2 4) :displaced-to a2)
      (loop for i below 10 do (setf (aref a1 i) (1+ i)))))
  #2a((1 2 3 4) (5 6 7 8)))


;;; Keywords tests

(deftest make-array.allow-other-keys.1
  (make-array '(5) :initial-element 'a :allow-other-keys t)
  #(a a a a a))

(deftest make-array.allow-other-keys.2
  (make-array '(5) :initial-element 'a :allow-other-keys nil)
  #(a a a a a))

(deftest make-array.allow-other-keys.3
  (make-array '(5) :initial-element 'a :allow-other-keys t '#:bad t)
  #(a a a a a))

(deftest make-array.allow-other-keys.4
  (make-array '(5) :initial-element 'a :bad t :allow-other-keys t)
  #(a a a a a))

(deftest make-array.allow-other-keys.5
  (make-array '(5) :bad t :initial-element 'a :allow-other-keys t)
  #(a a a a a))

(deftest make-array.allow-other-keys.6
  (make-array '(5) :bad t :initial-element 'a :allow-other-keys t
	      :allow-other-keys nil :also-bad nil)
  #(a a a a a))

(deftest make-array.allow-other-keys.7
  (make-array '(5) :allow-other-keys t :initial-element 'a)
  #(a a a a a))

(deftest make-array.keywords.8.
  (make-array '(5) :initial-element 'x :initial-element 'a)
  #(x x x x x))

;;; Error tests

(deftest make-array.error.1
  (signals-error (make-array) program-error)
  t)

(deftest make-array.error.2
  (signals-error (make-array '(10) :bad t) program-error)
  t)

(deftest make-array.error.3
  (signals-error (make-array '(10) :allow-other-keys nil :bad t)
		 program-error)
  t)

(deftest make-array.error.4
  (signals-error (make-array '(10) :allow-other-keys nil
			      :allow-other-keys t :bad t)
		 program-error)
  t)

(deftest make-array.error.5
  (signals-error (make-array '(10) :bad) program-error)
  t)

(deftest make-array.error.6
  (signals-error (make-array '(10) 1 2) program-error)
  t)

;;; Order of evaluation tests

(deftest make-array.order.1
  (let ((i 0) a b c e)
    (values
     (make-array (progn (setf a (incf i)) 5)
		 :initial-element (progn (setf b (incf i)) 'a)
		 :fill-pointer (progn (setf c (incf i)) nil)
		 ;; :displaced-to (progn (setf d (incf i)) nil)
		 :element-type (progn (setf e (incf i)) t)
		 )
     i a b c e))
  #(a a a a a) 4 1 2 3 4)

(deftest make-array.order.2
  (let ((i 0) a b d e)
    (values
     (make-array (progn (setf a (incf i)) 5)
		 :element-type (progn (setf b (incf i)) t)
		 ;; :displaced-to (progn (setf c (incf i)) nil)
		 :fill-pointer (progn (setf d (incf i)) nil)
		 :initial-element (progn (setf e (incf i)) 'a)
		 )
     i a b d e))
  #(a a a a a) 4 1 2 3 4)

;; Must add back order tests for :displaced-to and :displaced-index-offset

