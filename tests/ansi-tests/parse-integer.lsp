;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 10:24:13 2003
;;;; Contains: Tests of PARSE-INTEGER

(in-package :cl-test)

(deftest parse-integer.error.1
  (signals-error (parse-integer) program-error)
  t)

(deftest parse-integer.error.2
  (signals-error (parse-integer "123" :bogus) program-error)
  t)

(deftest parse-integer.error.3
  (signals-error (parse-integer "123" :bogus 'foo) program-error)
  t)

(deftest parse-integer.error.4
  (signals-error (parse-integer "") parse-error)
  t)

(deftest parse-integer.error.5
  (loop for x across +standard-chars+
	unless (or (digit-char-p x)
		   (eval `(signals-error (parse-integer ,(string x))
					 parse-error)))
	collect x)
  nil)

(deftest parse-integer.error.5a
  (signals-error (parse-integer "") parse-error)
  t)

(deftest parse-integer.error.6
  (signals-error (parse-integer "1234a") parse-error)
  t)

(deftest parse-integer.error.7
  (signals-error (parse-integer "-") parse-error)
  t)

(deftest parse-integer.error.8
  (signals-error (parse-integer "+") parse-error)
  t)

(deftest parse-integer.error.9
  (signals-error (parse-integer "--10") parse-error)
  t)

(deftest parse-integer.error.10
  (signals-error (parse-integer "++10") parse-error)
  t)

(deftest parse-integer.error.11
  (signals-error (parse-integer "10.") parse-error)
  t)

(deftest parse-integer.error.12
  (signals-error (parse-integer "#O123") parse-error)
  t)

(deftest parse-integer.error.13
  (signals-error (parse-integer "#B0100") parse-error)
  t)

(deftest parse-integer.error.14
  (signals-error (parse-integer "#X0100") parse-error)
  t)

(deftest parse-integer.error.15
  (signals-error (parse-integer "#3R0100") parse-error)
  t)

;;;

(deftest parse-integer.1
  (parse-integer "123")
  123 3)

(deftest parse-integer.2
  (parse-integer " 123")
  123 4)

(deftest parse-integer.3
  (parse-integer "    12345678901234567890   ")
  12345678901234567890 27)

(deftest parse-integer.4
  (parse-integer (concatenate 'string (string #\Newline) "17"
			      (string #\Newline)))
  17 4)

(deftest parse-integer.5
  (let ((c (name-char "Tab")))
    (if c
	(parse-integer (concatenate 'string (string c) "6381" (string c)))
      (values 6381 6)))
  6381 6)

(deftest parse-integer.6
  (let ((c (name-char "Linefeed")))
    (if c
	(parse-integer (concatenate 'string (string c) "-123712" (string c)))
      (values -123712 9)))
  -123712 9)

(deftest parse-integer.7
  (let ((c (name-char "Page")))
    (if c
	(parse-integer (concatenate 'string (string c) "0" (string c)))
      (values 0 3)))
  0 3)

(deftest parse-integer.8
  (let ((c (name-char "Return")))
    (if c
	(parse-integer (concatenate 'string (string c) "999" (string c)))
      (values 999 5)))
  999 5)

(deftest parse-integer.9
  (parse-integer "-0")
  0 2)

(deftest parse-integer.10
  (parse-integer "+0")
  0 2)

(deftest parse-integer.11
  (parse-integer "-00")
  0 3)

(deftest parse-integer.12
  (parse-integer "+000")
  0 4)

(deftest parse-integer.13
  (parse-integer "00010")
  10 5)

(deftest parse-integer.14
  (parse-integer "10110" :radix 2)
  22 5)

(deftest parse-integer.15
  (parse-integer "1021" :radix 3)
  34 4)

(deftest parse-integer.16
  (loop for radix from 2 to 36
	for c across "123456789abcdefghijklmnopqrstuvwxyz"
	for s = (concatenate 'string (string c) "0")
	for vals = (multiple-value-list (parse-integer s :radix radix))
	for (val pos) = vals
	always (and (= (length vals) 2)
		    (= pos 2)
		    (= val (* radix (1- radix)))))
  t)

(deftest parse-integer.17
  (parse-integer "10A" :junk-allowed t)
  10 2)

(deftest parse-integer.18
  (parse-integer "10" :junk-allowed t)
  10 2)

(deftest parse-integer.19
  (parse-integer "ABCDE" :junk-allowed t)
  nil 0)

(deftest parse-integer.20
  (parse-integer "" :junk-allowed t)
  nil 0)

(deftest parse-integer.21
  :notes (:nil-vectors-are-strings)
  (parse-integer (make-array 0 :element-type nil) :junk-allowed t)
  nil 0)

(deftest parse-integer.22
  (parse-integer "a1234b" :start 2 :end 4)
  23 4)

(deftest parse-integer.23
  (parse-integer "a1234b" :start 2 :end 4 :end nil)
  23 4)

(deftest parse-integer.24
  (parse-integer "a1234b" :start 2 :end 4 :start 1)
  23 4)


(deftest parse-integer.25
  (parse-integer "a1234b" :start 2 :end 4 :allow-other-keys nil)
  23 4)

(deftest parse-integer.26
  (parse-integer "a1234b" :start 2 :end 4 :allow-other-keys t :foo nil)
  23 4)

(deftest parse-integer.27
  (parse-integer "a1234b" :start 2 :end 4 :allow-other-keys t
		 :allow-other-keys nil :foo nil)
  23 4)

(deftest parse-integer.28
  (let* ((s (make-array 5 :initial-contents "a123b" :element-type 'base-char))
	 (s2 (make-array 3 :displaced-to s :displaced-index-offset 1
			 :element-type 'base-char)))
    (values
     s2
     (length s2)
     (equalpt "123" s2)
     (multiple-value-list (parse-integer s2))))
  "123" 3 t (123 3))

(deftest parse-integer.28a
  (let* ((s (make-array 5 :initial-contents "a123b" :element-type 'character))
	 (s2 (make-array 3 :displaced-to s :displaced-index-offset 1
			 :element-type 'character)))
    (values
     s2
     (length s2)
     (equalpt "123" s2)
     (multiple-value-list (parse-integer s2))))
  "123" 3 t (123 3))

(deftest parse-integer.29
  (let ((s (make-array 10 :initial-contents "1234567890"
		       :fill-pointer 3
		       :element-type 'base-char)))
    (values
     (length s)
     (multiple-value-list (parse-integer s))))
  3 (123 3))

(deftest parse-integer.29a
  (let ((s (make-array 10 :initial-contents "1234567890"
		       :fill-pointer 3
		       :element-type 'character)))
    (values
     (length s)
     (multiple-value-list (parse-integer s))))
  3 (123 3))

(deftest parse-integer.30
  (let ((s (make-array 10 :initial-contents "1234567890"
		       :adjustable t
		       :element-type 'base-char)))
    (values
     (length s)
     (multiple-value-list (parse-integer s))
     (progn
       (adjust-array s 3 :element-type 'base-char)
       (multiple-value-list (parse-integer s)))))
  10
  (1234567890 10)
  (123 3))

(deftest parse-integer.30a
  (let ((s (make-array 10 :initial-contents "1234567890"
		       :adjustable t
		       :element-type 'character)))
    (values
     (length s)
     (multiple-value-list (parse-integer s))
     (progn
       (adjust-array s 3 :element-type 'character)
       (multiple-value-list (parse-integer s)))))
  10
  (1234567890 10)
  (123 3))

(deftest parse-integer.31
  (parse-integer "1234" :start 1)
  234 4)

(deftest parse-integer.32
  (parse-integer "1234" :start 1 :end nil)
  234 4)

(deftest parse-integer.33
  (let* ((s (make-array 5 :initial-contents "a123b" :element-type 'base-char))
	 (s2 (make-array 3 :displaced-to s :displaced-index-offset 1
			 :element-type 'base-char))
	 (s3 (make-array 2 :displaced-to s2 :displaced-index-offset 1
			 :element-type 'base-char)))
    (values
     s3
     (length s3)
     (equalpt "23" s3)
     (multiple-value-list (parse-integer s3))))
  "23" 2 t (23 2))

(deftest parse-integer.34
  (parse-integer "1234" :end 3)
  123 3)

(deftest parse-integer.35
  (parse-integer "1234" :end 3 :end 1)
  123 3)

(deftest parse-integer.36
  (parse-integer "1234" :end nil :end 3)
  1234 4)

;;; Order of evaluation tests

(deftest parse-integer.order.1
  (let ((i 0) a b c d e)
    (values
     (multiple-value-list
      (parse-integer (progn (setf a (incf i)) "10001")
		     :radix (progn (setf b (incf i)) 2)
		     :start (progn (setf c (incf i)) 0)
		     :end (progn (setf d (incf i)) 5)
		     :junk-allowed (progn (setf e (incf i)) nil)))
     i a b c d e))
  (17 5) 5 1 2 3 4 5)
