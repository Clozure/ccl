;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct  3 21:53:38 2002
;;;; Contains: Tests for STRING-TRIM

(in-package :cl-test)

(deftest string-trim.1
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.2
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim '(#\a #\b) s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.3
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim #(#\a #\b) s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.4
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim (make-array 2 :initial-contents '(#\a #\b))
			  s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.5
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'character)
			  s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.6
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'standard-char)
			  s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.7
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'base-char)
			  s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.8
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim (make-array 4 :initial-contents '(#\a #\b #\c #\d)
				      :element-type 'character
				      :fill-pointer 2)
			  s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.8a
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim (make-array 4 :initial-contents '(#\a #\b #\c #\d)
				      :element-type 'base-char
				      :fill-pointer 2)
			  s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.9
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'character
			))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.10
  (let* ((s (make-array 9 :initial-contents "abcdabadd"
			:element-type 'character
			:fill-pointer 7))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.10a
  (let* ((s (make-array 9 :initial-contents "abcdabadd"
			:element-type 'base-char
			:adjustable t
			:fill-pointer 7))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.11
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'standard-char
			))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.12
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'base-char
			))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

;;; Test that trimming is case sensitive
(deftest string-trim.13
  (let* ((s (copy-seq "Aa"))
	 (s2 (string-trim "a" s)))
    (values s s2))
  "Aa" "A")

(deftest string-trim.14
  (let* ((s '|abcdaba|)
	 (s2 (string-trim "ab" s)))
    (values (symbol-name s) s2))
  "abcdaba"
  "cd")

(deftest string-trim.15
  (string-trim "abc" "")
  "")

(deftest string-trim.16
  (string-trim "a" #\a)
  "")

(deftest string-trim.17
  (string-trim "b" #\a)
  "a")

(deftest string-trim.18
  (string-trim "" (copy-seq "abcde"))
  "abcde")

(deftest string-trim.19
  (string-trim "abc" (copy-seq "abcabcabc"))
  "")

(deftest string-trim.20
  :notes (:nil-vectors-are-strings)
  (string-trim "abcd" (make-array '(0) :element-type nil))
  "")

(deftest string-trim.21
  :notes (:nil-vectors-are-strings)
  (string-trim (make-array '(0) :element-type nil) "abcd")
  "abcd")

(deftest string-trim.22
  (let ((s (make-array '(6) :initial-contents "abcaeb"
		       :element-type 'base-char
		       :adjustable t)))
    (values (string-trim "ab" s) s))
  "cae" "abcaeb")

(deftest string-trim.23
  (let ((s (make-array '(6) :initial-contents "abcaeb"
		       :element-type 'character
		       :adjustable t)))
    (values (string-trim "ab" s) s))
  "cae" "abcaeb")

(deftest string-trim.24
  (let* ((etype 'base-char)
	 (s0 (make-array '(6) :initial-contents "abcaeb"
			 :element-type etype))
	 (s (make-array '(3) :element-type etype
			:displaced-to s0
			:displaced-index-offset 1)))
    (values (string-trim "ab" s) s s0))
  "c" "bca" "abcaeb")

(deftest string-trim.25
  (let* ((etype 'character)
	 (s0 (make-array '(6) :initial-contents "abcaeb"
			 :element-type etype))
	 (s (make-array '(3) :element-type etype
			:displaced-to s0
			:displaced-index-offset 1)))
    (values (string-trim "ab" s) s s0))
  "c" "bca" "abcaeb")

(deftest string-trim.order.1
  (let ((i 0) x y)
    (values
     (string-trim (progn (setf x (incf i)) " ")
		  (progn (setf y (incf i))
			 (copy-seq "   abc d e f  ")))
     i x y))
  "abc d e f" 2 1 2)

(def-fold-test string-trim.fold.1 (string-trim " " " abcd "))

;;; Error cases

(deftest string-trim.error.1
  (signals-error (string-trim) program-error)
  t)

(deftest string-trim.error.2
  (signals-error (string-trim "abc") program-error)
  t)

(deftest string-trim.error.3
  (signals-error (string-trim "abc" "abcdddabc" nil) program-error)
  t)
