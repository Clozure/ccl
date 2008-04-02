;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct  4 04:57:41 2002
;;;; Contains: Tests for STRING-LEFT-TRIM

(in-package :cl-test)

(deftest string-left-trim.1
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.2
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim '(#\a #\b) s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.3
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim #(#\a #\b) s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.4
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim (make-array 2 :initial-contents '(#\a #\b))
			  s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.5
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'character)
			  s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.6
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'standard-char)
			  s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.7
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'base-char)
			  s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.8
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim (make-array 4 :initial-contents '(#\a #\b #\c #\d)
				      :element-type 'character
				      :fill-pointer 2)
			  s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.9
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'character
			))
	 (s2 (string-left-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.10
  (let* ((s (make-array 9 :initial-contents "abcdabadd"
			:element-type 'character
			:fill-pointer 7))
	 (s2 (string-left-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.10a
  (let* ((s (make-array 9 :initial-contents "abcdabadd"
			:element-type 'base-char
			:fill-pointer 7))
	 (s2 (string-left-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.10b
  (let* ((s (make-array 9 :initial-contents "abcdabadd"
			:element-type 'base-char
			:adjustable t
			:fill-pointer 7))
	 (s2 (string-left-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.11
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'standard-char
			))
	 (s2 (string-left-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.12
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'base-char
			))
	 (s2 (string-left-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cdaba")

;;; Test that trimming is case sensitive
(deftest string-left-trim.13
  (let* ((s (copy-seq "aA"))
	 (s2 (string-left-trim "a" s)))
    (values s s2))
  "aA" "A")

(deftest string-left-trim.14
  (let* ((s '|abcdaba|)
	 (s2 (string-left-trim "ab" s)))
    (values (symbol-name s) s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.15
  (string-left-trim "abc" "")
  "")

(deftest string-left-trim.16
  (string-left-trim "a" #\a)
  "")

(deftest string-left-trim.17
  (string-left-trim "b" #\a)
  "a")

(deftest string-left-trim.18
  (string-left-trim "" (copy-seq "abcde"))
  "abcde")

(deftest string-left-trim.19
  (string-left-trim "abc" (copy-seq "abcabcabc"))
  "")

(deftest string-left-trim.20
  :notes (:nil-vectors-are-strings)
  (string-left-trim "abcd" (make-array '(0) :element-type nil))
  "")

(deftest string-left-trim.21
  :notes (:nil-vectors-are-strings)
  (string-left-trim (make-array '(0) :element-type nil) "abcd")
  "abcd")

(deftest string-left-trim.22
  (let ((s (make-array '(6) :initial-contents "abcaeb"
		       :element-type 'base-char
		       :adjustable t)))
    (values (string-left-trim "ab" s) s))
  "caeb" "abcaeb")

(deftest string-left-trim.23
  (let ((s (make-array '(6) :initial-contents "abcaeb"
		       :element-type 'character
		       :adjustable t)))
    (values (string-left-trim "ab" s) s))
  "caeb" "abcaeb")

(deftest string-left-trim.24
  (let* ((etype 'base-char)
	 (s0 (make-array '(6) :initial-contents "abcaeb"
			 :element-type etype))
	 (s (make-array '(3) :element-type etype
			:displaced-to s0
			:displaced-index-offset 1)))
    (values (string-left-trim "ab" s) s s0))
  "ca" "bca" "abcaeb")

(deftest string-left-trim.25
  (let* ((etype 'character)
	 (s0 (make-array '(6) :initial-contents "abcaeb"
			 :element-type etype))
	 (s (make-array '(3) :element-type etype
			:displaced-to s0
			:displaced-index-offset 1)))
    (values (string-left-trim "ab" s) s s0))
  "ca" "bca" "abcaeb")


(deftest string-left-trim.order.1
  (let ((i 0) x y)
    (values
     (string-left-trim (progn (setf x (incf i)) " ")
		       (progn (setf y (incf i))
			      (copy-seq "   abc d e f  ")))
     i x y))
  "abc d e f  " 2 1 2)

(def-fold-test string-left-trim.fold.1 (string-left-trim " " " abcd"))

;;; Error cases

(deftest string-left-trim.error.1
  (signals-error (string-left-trim) program-error)
  t)

(deftest string-left-trim.error.2
  (signals-error (string-left-trim "abc") program-error)
  t)

(deftest string-left-trim.error.3
  (signals-error (string-left-trim "abc" "abcdddabc" nil) program-error)
  t)
