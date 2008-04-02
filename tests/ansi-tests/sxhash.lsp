;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 21:18:12 2003
;;;; Contains: Tests of SXHASH

(in-package :cl-test)

(deftest sxhash.1
  (check-predicate #'(lambda (x) (typep (sxhash x) '(and unsigned-byte fixnum))))
  nil)

(deftest sxhash.2
  (loop for i from 0 below 256
	for c = (code-char i)
	when (and c
		  (not (= (sxhash (string c))
			  (sxhash (string c)))))
	collect c)
  nil)

(deftest sxhash.3
  (=t (sxhash "") (sxhash (copy-seq "")))
  t)

(deftest sxhash.4
  (loop for bv1 in '(#* #*0 #*1 #*01 #*00 #*10 #*11
			#*1100101101100 #*110010101011001011010000111001011)
	for bv2 = (copy-seq bv1)
	for sx1 = (sxhash bv1)
	for sx2 = (sxhash bv2)
	always (and (not (eq bv1 bv2))
		    (equal bv1 bv2)
		    (typep sx1 '(and unsigned-byte fixnum))
		    (typep sx2 '(and unsigned-byte fixnum))
		    (= sx1 sx2)))
  t)

(deftest sxhash.5
  (let ((s1 "abcd")
	(s2 (make-array 10 :element-type 'character
			:initial-contents "abcdefghij"
			:fill-pointer 4)))
    (and (equalt s1 s2)
	 (=t (sxhash s1) (sxhash s2))))
  t)

(deftest sxhash.6
  (let ((s1 #*01101)
	(s2 (make-array 10 :element-type 'bit
			:initial-contents #*0110111101
			:fill-pointer 5)))
    (and (equalt s1 s2)
	 (=t (sxhash s1) (sxhash s2))))
  t)

(deftest sxhash.7
  (let* ((a (make-array 10 :initial-element nil))
	 (sx1 (sxhash a)))
    (setf (aref a 4) 'x)
    (let ((sx2 (sxhash a)))
      (and (typep sx1 '(and unsigned-byte fixnum))
	   (eqlt sx1 sx2))))
  t)

(deftest sxhash.8
  :notes (:nil-vectors-are-strings)
  (eqlt (sxhash (make-array 0 :element-type nil))
	(sxhash ""))
  t)

(deftest sxhash.9
  (let ((s1 (make-array 5 :element-type 'base-char :initial-contents "abcde"))
	(s2 (copy-seq "abcde")))
    (eqlt (sxhash s1) (sxhash s2)))
  t)

(deftest sxhash.10
  (let ((s1 "abcd")
	(s2 (make-array 10 :element-type 'base-char
			:initial-contents "abcdefghij"
			:fill-pointer 4)))
    (and (equalt s1 s2)
	 (=t (sxhash s1) (sxhash s2))))
  t)

(deftest sxhash.11
  (let* ((x (cons 'a 'b))
	 (sx1 (sxhash x))
	 (sx2 (sxhash '(a . b))))
    (setf (car x) 'c)
    (let* ((sx3 (sxhash x))
	   (sx4 (sxhash '(c . b))))
      (and (=t sx1 sx2)
	   (=t sx3 sx4))))
  t)

(deftest sxhash.12
  (let ((x (1+ most-positive-fixnum))
	(y (1+ most-positive-fixnum)))
    (=t (sxhash x) (sxhash y)))
  t)

(deftest sxhash.13
  (let ((sx1 (sxhash (make-symbol "FOO")))
	(sx2 (sxhash (make-symbol "FOO"))))		      
    (and (typep sx1 '(and unsigned-byte fixnum))
	 (eqlt sx1 sx2)))
  t)

;; (deftest sxhash.14
;;  (let ((sx1 (sxhash :foo))
;;	(sx2 (sxhash '#:foo)))
;;    (and (typep sx1 '(and unsigned-byte fixnum))
;;	 (eqlt sx1 sx2)))
;;  t)

(deftest sxhash.15
  (let* ((package-name
	  (loop for i from 0
		for name = (format nil "PACKAGE-~A" i)
		for package = (find-package name)
		unless package do (return name)))
	 (sx1
	  (let* ((package (make-package package-name :nicknames nil :use nil))
		 (symbol (intern "FOO" package)))
	    (prog1
	       (sxhash symbol)
	      (delete-package package))))
	 (sx2
	  (let* ((package (make-package package-name :nicknames nil :use nil))
		 (symbol (intern "FOO" package)))
	    (prog1
	       (sxhash symbol)
	      (delete-package package)))))
    (assert (typep sx1 '(and unsigned-byte fixnum)))
    (if (= sx1 sx2) :good (list sx1 sx2)))
  :good)

(deftest sxhash.16
  (let ((c1 (list 'a))
	(c2 (list 'a)))
    (setf (cdr c1) c1)
    (setf (cdr c2) c2)
    (let ((sx1 (sxhash c1))
	  (sx2 (sxhash c2)))
      (or (eqlt sx1 sx2) (list sx1 sx2))))
  t)

;;; Since similarity of numbers is 'same type and same mathematical value',
;;; and since sxhash must produce the same value for similar numeric arguments,
;;; (sxhash 0.0) and (sxhash -0.0) must be eql for all float types.
;;; This may be a spec bug, so I've added a note.

(deftest sxhash.17
  :notes (:negative-zero-is-similar-to-positive-zero)
  (loop for c1 in '(0.0s0 0.0f0 0.0d0 0.0l0)
	for c2 in '(-0.0s0 -0.0f0 -0.0d0 -0.0l0)
	for t1 = (type-of c1)
	for t2 = (type-of c2)
	for sx1 = (sxhash c1)
	for sx2 = (sxhash c2)
	unless (or (not (subtypep t1 t2))
		   (not (subtypep t2 t1))
		   (eql sx1 sx2))
	collect (list c1 c2 sx1 sx2))
  nil)

(deftest sxhash.18
  :notes (:negative-zero-is-similar-to-positive-zero)
  (loop for r1 in '(0.0s0 0.0f0 0.0d0 0.0l0)
	for c1 = (complex r1)
	for r2 in '(-0.0s0 -0.0f0 -0.0d0 -0.0l0)
	for c2 = (complex r2)
	for t1 = (type-of c1)
	for t2 = (type-of c2)
	for sx1 = (sxhash c1)
	for sx2 = (sxhash c2)
	unless (or (not (subtypep t1 t2))
		   (not (subtypep t2 t1))
		   (eql sx1 sx2))
	collect (list c1 c2 sx1 sx2))
  nil)

(deftest sxhash.19
  :notes (:negative-zero-is-similar-to-positive-zero)
  (loop for r1 in '(0.0s0 0.0f0 0.0d0 0.0l0)
	for c1 = (complex 0 r1)
	for r2 in '(-0.0s0 -0.0f0 -0.0d0 -0.0l0)
	for c2 = (complex 0 r2)
	for t1 = (type-of c1)
	for t2 = (type-of c2)
	for sx1 = (sxhash c1)
	for sx2 = (sxhash c2)
	unless (or (not (subtypep t1 t2))
		   (not (subtypep t2 t1))
		   (eql sx1 sx2))
	collect (list c1 c2 sx1 sx2))
  nil)

;;; Similar pathnames have the same hash
(deftest sxhash.20
  (let* ((pathspec "sxhash.lsp")
	 (sx1 (sxhash (pathname (copy-seq pathspec))))
	 (sx2 (sxhash (pathname (copy-seq pathspec)))))
    (if (and (typep sx1 '(and fixnum unsigned-byte))
	     (eql sx1 sx2))
	:good
      (list sx1 sx2)))
  :good)

;;; Similarity for strings
(deftest sxhash.21
  (let* ((s1 "abc")
	 (s2 (make-array '(3) :element-type 'character
			 :initial-contents s1))
	 (s3 (make-array '(3) :element-type 'base-char
			 :initial-contents s1))
	 (s4 (make-array '(3) :element-type 'standard-char
			 :initial-contents s1))
	 (s5 (make-array '(3) :element-type 'character
			 :adjustable t
			 :initial-contents "abc"))
	 (s6 (make-array '(5) :element-type 'character
			 :fill-pointer 3
			 :initial-contents "abcde"))
	 (s7 (make-array '(3) :element-type 'character
			 :displaced-to s2
			 :displaced-index-offset 0))
	 (s8 (make-array '(3) :element-type 'character
			 :displaced-to (make-array '(7) :element-type 'character
						   :initial-contents "xxabcyy")
			 :displaced-index-offset 2))
	 (strings (list s1 s2 s3 s4 s5 s6 s7 s8))
	 (hashes (mapcar #'sxhash strings)))
    (if (and (every #'(lambda (h) (typep h '(and unsigned-byte fixnum))) hashes)
	     (not (position (car hashes) hashes :test #'/=)))
	:good
      hashes))
  :good)

;;; Similarity for bit vectors
(deftest sxhash.22
  (let* ((bv1 #*010)
	 (bv2 (make-array '(3) :element-type 'bit
			 :initial-contents bv1))
	 (bv5 (make-array '(3) :element-type 'bit
			 :adjustable t
			 :initial-contents bv1))
	 (bv6 (make-array '(5) :element-type 'bit
			 :fill-pointer 3
			 :initial-contents #*01010))
	 (bv7 (make-array '(3) :element-type 'bit
			 :displaced-to bv2
			 :displaced-index-offset 0))
	 (bv8 (make-array '(3) :element-type 'bit
			 :displaced-to (make-array '(7) :element-type 'bit
						   :initial-contents #*1101001)
			 :displaced-index-offset 2))
	 (bit-vectors (list bv1 bv2 bv5 bv6 bv7 bv8))
	 (hashes (mapcar #'sxhash bit-vectors)))
    (if (and (every #'(lambda (h) (typep h '(and unsigned-byte fixnum))) hashes)
	     (not (position (car hashes) hashes :test #'/=)))
	:good
      hashes))
  :good)

;;; The hash of a symbol does not change when its package changes
(deftest sxhash.23
  (progn
    (safely-delete-package "A")
    (defpackage "A" (:use))
    (let* ((pkg (find-package "A"))
	   (sym (intern "FOO" pkg))
	   (hash (sxhash sym)))
      (unintern sym pkg)
      (let ((hash2 (sxhash sym)))
	(if (eql hash hash2) nil (list hash hash2)))))
  nil)      

;;; Error cases

(deftest sxhash.error.1
  (signals-error (sxhash) program-error)
  t)

(deftest sxhash.error.2
  (signals-error (sxhash nil nil) program-error)
  t)
