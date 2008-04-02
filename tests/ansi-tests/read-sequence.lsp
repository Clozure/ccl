;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 19 06:55:04 2004
;;;; Contains: Tests of READ-SEQUENCE

(in-package :cl-test)

;;; Read into a string

(defmacro def-read-sequence-test (name init args input &rest expected)
  `(deftest ,name
     (let ((s ,init))
       (with-input-from-string
	(is ,input)
	(values
	 (read-sequence s is ,@args)
	 s)))
     ,@expected))

(def-read-sequence-test read-sequence.string.1 (copy-seq "     ")
  () "abcdefghijk" 5 "abcde")

(def-read-sequence-test read-sequence.string.2 (copy-seq "     ")
  () "abc" 3 "abc  ")

(def-read-sequence-test read-sequence.string.3 (copy-seq "     ")
  (:start 1) "abcdefghijk" 5 " abcd")

(def-read-sequence-test read-sequence.string.4 (copy-seq "     ")
  (:end 3) "abcdefghijk" 3 "abc  ")

(def-read-sequence-test read-sequence.string.5 (copy-seq "     ")
  (:start 1 :end 4) "abcdefghijk" 4 " abc ")

(def-read-sequence-test read-sequence.string.6 (copy-seq "     ")
  (:start 0 :end 0) "abcdefghijk" 0 "     ")

(def-read-sequence-test read-sequence.string.7 (copy-seq "     ")
  (:end nil) "abcdefghijk" 5 "abcde")

(def-read-sequence-test read-sequence.string.8 (copy-seq "     ")
  (:allow-other-keys nil) "abcdefghijk" 5 "abcde")

(def-read-sequence-test read-sequence.string.9 (copy-seq "     ")
  (:allow-other-keys t :foo 'bar) "abcdefghijk" 5 "abcde")

(def-read-sequence-test read-sequence.string.10 (copy-seq "     ")
  (:foo 'bar :allow-other-keys 'x) "abcdefghijk" 5 "abcde")

(def-read-sequence-test read-sequence.string.11 (copy-seq "     ")
  (:foo 'bar :allow-other-keys 'x :allow-other-keys nil)
  "abcdefghijk" 5 "abcde")

(def-read-sequence-test read-sequence.string.12 (copy-seq "     ")
  (:end 5 :end 3 :start 0 :start 1) "abcdefghijk" 5 "abcde")

;;; Read into a base string

(def-read-sequence-test read-sequence.base-string.1
  (make-array 5 :element-type 'base-char)
  () "abcdefghijk" 5 "abcde")

(def-read-sequence-test read-sequence.base-string.2
  (make-array 5 :element-type 'base-char :initial-element #\Space)
  () "abc" 3 "abc  ")

(def-read-sequence-test read-sequence.base-string.3
  (make-array 5 :element-type 'base-char :initial-element #\Space)
  (:start 1) "abcdefghijk" 5 " abcd")

(def-read-sequence-test read-sequence.base-string.4
  (make-array 5 :element-type 'base-char :initial-element #\Space)
  (:end 3) "abcdefghijk" 3 "abc  ")

(def-read-sequence-test read-sequence.base-string.5
  (make-array 5 :element-type 'base-char :initial-element #\Space)
  (:start 1 :end 4) "abcdefghijk" 4 " abc ")

(def-read-sequence-test read-sequence.base-string.6
  (make-array 5 :element-type 'base-char :initial-element #\Space)
  (:start 0 :end 0) "abcdefghijk" 0 "     ")

(def-read-sequence-test read-sequence.base-string.7
  (make-array 5 :element-type 'base-char :initial-element #\Space)
  (:end nil) "abcdefghijk" 5 "abcde")

;;; Read into a list

(def-read-sequence-test read-sequence.list.1 (make-list 5)
  () "abcdefghijk" 5 (#\a #\b #\c #\d #\e))

(def-read-sequence-test read-sequence.list.2 (make-list 5)
  () "abc" 3 (#\a #\b #\c nil nil))

(def-read-sequence-test read-sequence.list.3 (make-list 5)
  (:start 1) "abcdefghijk" 5 (nil #\a #\b #\c #\d))

(def-read-sequence-test read-sequence.list.4 (make-list 5)
  (:end 3) "abcdefghijk" 3 (#\a #\b #\c nil nil))

(def-read-sequence-test read-sequence.list.5 (make-list 5)
  (:end 4 :start 1) "abcdefghijk" 4 (nil #\a #\b #\c nil))

(def-read-sequence-test read-sequence.list.6 (make-list 5)
  (:start 0 :end 0) "abcdefghijk" 0 (nil nil nil nil nil))

(def-read-sequence-test read-sequence.list.7 (make-list 5)
  (:end nil) "abcdefghijk" 5 (#\a #\b #\c #\d #\e))

;;; Read into a vector

(def-read-sequence-test read-sequence.vector.1
  (vector nil nil nil nil nil)
  () "abcdefghijk" 5 #(#\a #\b #\c #\d #\e))

(def-read-sequence-test read-sequence.vector.2
  (vector nil nil nil nil nil)
  () "abc" 3 #(#\a #\b #\c nil nil))

(def-read-sequence-test read-sequence.vector.3
  (vector nil nil nil nil nil)
  (:start 2) "abcdefghijk" 5 #(nil nil #\a #\b #\c))

(def-read-sequence-test read-sequence.vector.4
  (vector nil nil nil nil nil)
  (:start 1 :end 4) "abcdefghijk" 4 #(nil #\a #\b #\c nil))

(def-read-sequence-test read-sequence.vector.5
  (vector nil nil nil nil nil)
  (:end 2) "abcdefghijk" 2 #(#\a #\b nil nil nil))

(def-read-sequence-test read-sequence.vector.6
  (vector nil nil nil nil nil)
  (:end 0 :start 0) "abcdefghijk" 0 #(nil nil nil nil nil))

(def-read-sequence-test read-sequence.vector.7
  (vector nil nil nil nil nil)
  (:end nil) "abcdefghijk" 5 #(#\a #\b #\c #\d #\e))

;;; Read into a vector with a fill pointer

(def-read-sequence-test read-sequence.fill-vector.1
  (make-array 10 :initial-element nil :fill-pointer 5)
  () "abcdefghijk" 5 #(#\a #\b #\c #\d #\e))

(def-read-sequence-test read-sequence.fill-vector.2
  (make-array 10 :initial-element nil :fill-pointer 5)
  () "ab" 2 #(#\a #\b nil nil nil))

(def-read-sequence-test read-sequence.fill-vector.3
  (make-array 10 :initial-element nil :fill-pointer 5)
  () "" 0 #(nil nil nil nil nil))

(def-read-sequence-test read-sequence.fill-vector.4
  (make-array 10 :initial-element nil :fill-pointer 5)
  (:start 2) "abcdefghijk" 5 #(nil nil #\a #\b #\c))

(def-read-sequence-test read-sequence.fill-vector.5
  (make-array 10 :initial-element nil :fill-pointer 5)
  (:start 1 :end 4) "abcdefghijk" 4 #(nil #\a #\b #\c nil))

(def-read-sequence-test read-sequence.fill-vector.6
  (make-array 10 :initial-element nil :fill-pointer 5)
  (:end 2) "abcdefghijk" 2 #(#\a #\b nil nil nil))

(def-read-sequence-test read-sequence.fill-vector.7
  (make-array 10 :initial-element nil :fill-pointer 5)
  (:end 0 :start 0) "abcdefghijk" 0 #(nil nil nil nil nil))

(def-read-sequence-test read-sequence.fill-vector.8
  (make-array 10 :initial-element nil :fill-pointer 5)
  (:end nil) "abcdefghijk" 5 #(#\a #\b #\c #\d #\e))

;;; Nil vectors

(deftest read-sequence.nil-vector.1
  :notes (:nil-vectors-are-strings)
  (let ((s (make-array 0 :element-type nil)))
    (with-input-from-string
     (is "abcde")
     (values
      (read-sequence s is)
      s)))
  0 "")

;;; Read into a bit vector

(defmacro def-read-sequence-bv-test (name init args &rest expected)
  `(deftest ,name
     ;; Create output file
     (progn
       (let (os)
	 (unwind-protect
	     (progn
	       (setq os (open "temp.dat" :direction :output
			      :element-type '(unsigned-byte 8)
			      :if-exists :supersede))
	       (loop for i in '(0 1 1 0 0 1 1 0 1 0 1 1 1 0)
		     do (write-byte i os)))
	   (when os (close os))))
       (let (is (bv (copy-seq ,init)))
	 (unwind-protect
	     (progn
	       (setq is (open "temp.dat" :direction :input
			      :element-type '(unsigned-byte 8)))
	       (values
		(read-sequence bv is ,@args)
		bv))
	   (when is (close is)))))
     ,@expected))
     
(def-read-sequence-bv-test read-sequence.bv.1 #*00000000000000 ()
  14 #*01100110101110)
  
(def-read-sequence-bv-test read-sequence.bv.2 #*00000000000000 (:start 0)
  14 #*01100110101110)
  
(def-read-sequence-bv-test read-sequence.bv.3 #*00000000000000 (:end 14)
  14 #*01100110101110)
  
(def-read-sequence-bv-test read-sequence.bv.4 #*00000000000000 (:end nil)
  14 #*01100110101110)
  
(def-read-sequence-bv-test read-sequence.bv.5 #*00000000000000 (:start 2)
  14 #*00011001101011)
  
(def-read-sequence-bv-test read-sequence.bv.6 #*00000000000000
  (:start 2 :end 13)
  13 #*00011001101010)

(def-read-sequence-bv-test read-sequence.bv.7 #*00000000000000 (:end 6)
  6 #*01100100000000)

;;; Error cases

(deftest read-sequence.error.1
  (signals-error (read-sequence) program-error)
  t)

(deftest read-sequence.error.2
  (signals-error (read-sequence (make-string 10)) program-error)
  t)

(deftest read-sequence.error.3
  (signals-error
   (read-sequence (make-string 5) (make-string-input-stream "abc") :start)
   program-error)
  t)

(deftest read-sequence.error.4
  (signals-error
   (read-sequence (make-string 5) (make-string-input-stream "abc") :foo 1)
   program-error)
  t)

(deftest read-sequence.error.5
  (signals-error
   (read-sequence (make-string 5) (make-string-input-stream "abc")
		  :allow-other-keys nil :bar 2)
   program-error)
  t)

(deftest read-sequence.error.6
  (check-type-error #'(lambda (x) (read-sequence x (make-string-input-stream "abc")))
		    #'sequencep)
  nil)

(deftest read-sequence.error.7
  (signals-error
   (read-sequence (cons 'a 'b) (make-string-input-stream "abc"))
   type-error)
  t)

;;; This test appears to cause Allegro CL to crash
(deftest read-sequence.error.8
  (signals-type-error x -1
		      (read-sequence (make-string 3)
				     (make-string-input-stream "abc")
				     :start x))
  t)

(deftest read-sequence.error.9
  (check-type-error #'(lambda (s)
			(read-sequence (make-string 3) (make-string-input-stream "abc")
				       :start s))
		    (typef 'unsigned-byte))
  nil)

(deftest read-sequence.error.10
  (signals-type-error x -1
		      (read-sequence (make-string 3) (make-string-input-stream "abc")
				     :end x))
  t)

(deftest read-sequence.error.11
  (check-type-error #'(lambda (e)
			(read-sequence (make-string 3) (make-string-input-stream "abc")
				       :end e))
		    (typef '(or unsigned-byte null)))
  nil)
