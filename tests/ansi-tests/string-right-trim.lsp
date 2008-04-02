;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct  4 04:59:46 2002
;;;; Contains: Tests of STRING-RIGHT-TRIM

(in-package :cl-test)

(deftest string-right-trim.1
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.2
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim '(#\a #\b) s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.3
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim #(#\a #\b) s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.4
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim (make-array 2 :initial-contents '(#\a #\b))
			  s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.5
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'character)
			  s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.6
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'standard-char)
			  s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.7
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'base-char)
			  s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.8
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim (make-array 4 :initial-contents '(#\a #\b #\c #\d)
				      :element-type 'character
				      :fill-pointer 2)
			  s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.9
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'character
			))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.10
  (let* ((s (make-array 9 :initial-contents "abcdabadd"
			:element-type 'character
			:fill-pointer 7))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.10a
  (let* ((s (make-array 9 :initial-contents "abcdabadd"
			:element-type 'base-char
			:fill-pointer 7))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.10b
  (let* ((s (make-array 9 :initial-contents "abcdabadd"
			:element-type 'base-char
			:adjustable t
			:fill-pointer 7))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.11
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'standard-char
			))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.12
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'base-char
			))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

;;; Test that trimming is case sensitive
(deftest string-right-trim.13
  (let* ((s (copy-seq "Aa"))
	 (s2 (string-right-trim "a" s)))
    (values s s2))
  "Aa" "A")

(deftest string-right-trim.14
  (let* ((s '|abcdaba|)
	 (s2 (string-right-trim "ab" s)))
    (values (symbol-name s) s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.15
  (string-right-trim "abc" "")
  "")

(deftest string-right-trim.16
  (string-right-trim "a" #\a)
  "")

(deftest string-right-trim.17
  (string-right-trim "b" #\a)
  "a")

(deftest string-right-trim.18
  (string-right-trim "" (copy-seq "abcde"))
  "abcde")

(deftest string-right-trim.19
  (string-right-trim "abc" (copy-seq "abcabcabc"))
  "")

(deftest string-right-trim.20
  :notes (:nil-vectors-are-strings)
  (string-right-trim "abcd" (make-array '(0) :element-type nil))
  "")

(deftest string-right-trim.21
  :notes (:nil-vectors-are-strings)
  (string-right-trim (make-array '(0) :element-type nil) "abcd")
  "abcd")

(deftest string-right-trim.22
  (let ((s (make-array '(6) :initial-contents "abcaeb"
		       :element-type 'base-char
		       :adjustable t)))
    (values (string-right-trim "ab" s) s))
  "abcae" "abcaeb")

(deftest string-right-trim.23
  (let ((s (make-array '(6) :initial-contents "abcaeb"
		       :element-type 'character
		       :adjustable t)))
    (values (string-right-trim "ab" s) s))
  "abcae" "abcaeb")

(deftest string-right-trim.24
  (let* ((etype 'base-char)
	 (s0 (make-array '(6) :initial-contents "abcaeb"
			 :element-type etype))
	 (s (make-array '(3) :element-type etype
			:displaced-to s0
			:displaced-index-offset 1)))
    (values (string-right-trim "ab" s) s s0))
  "bc" "bca" "abcaeb")

(deftest string-right-trim.25
  (let* ((etype 'character)
	 (s0 (make-array '(6) :initial-contents "abcaeb"
			 :element-type etype))
	 (s (make-array '(3) :element-type etype
			:displaced-to s0
			:displaced-index-offset 1)))
    (values (string-right-trim "ab" s) s s0))
  "bc" "bca" "abcaeb")

(deftest string-right-trim.order.1
  (let ((i 0) x y)
    (values
     (string-right-trim (progn (setf x (incf i)) " ")
		       (progn (setf y (incf i))
			      (copy-seq "   abc d e f  ")))
     i x y))
  "   abc d e f" 2 1 2)

(def-fold-test string-right-trim.fold.1 (string-right-trim " " "abcd "))

;;; Error cases

(deftest string-right-trim.error.1
  (signals-error (string-right-trim) program-error)
  t)

(deftest string-right-trim.error.2
  (signals-error (string-right-trim "abc") program-error)
  t)

(deftest string-right-trim.error.3
  (signals-error (string-right-trim "abc" "abcdddabc" nil) program-error)
  t)
