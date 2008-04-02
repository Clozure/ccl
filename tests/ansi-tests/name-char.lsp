;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 29 17:14:03 2004
;;;; Contains: Tests of NAME-CHAR

(in-package :cl-test)

(compile-and-load "char-aux.lsp")

(deftest name-char.1
  (name-char.1.body)
  t)

(deftest name-char.2
  (loop for s in '("RubOut" "PAGe" "BacKspace" "RetUrn" "Tab" "LineFeed"
		   "SpaCE" "NewLine")
	always
	(let ((c1 (name-char (string-upcase s)))
	      (c2 (name-char (string-downcase s)))
	      (c3 (name-char (string-capitalize s)))
	      (c4 (name-char s)))
	  (and (eqlt c1 c2) (eqlt c2 c3) (eqlt c3 c4))))
  t)

(deftest name-char.order.1
  (let ((i 0))
    (values
     (name-char (progn (incf i) "Space"))
     i))
  #\Space 1)

;;; Specialized sequence tests

(deftest name-char.specialized.1
  (loop for etype in '(standard-char base-char character)
	append
	(loop for s in '("Rubout" "Page" "Backspace" "Return" "Tab" "Linefeed"
			 "Space" "Newline")
	      for s2 = (make-array (length s) :element-type 'base-char
				   :initial-contents s)
	      unless (eql (name-char s) (name-char s2))
	      collect (list s s2)))
  nil)

(deftest name-char.specialized.2
  (loop for etype in '(standard-char base-char character)
	append
	(loop for s in '("Rubout" "Page" "Backspace" "Return" "Tab" "Linefeed"
			 "Space" "Newline")
	      for s2 = (make-array (length s) :element-type etype
				   :adjustable t
				   :initial-contents s)
	      unless (eql (name-char s) (name-char s2))
	      collect (list etype s s2)))
  nil)

(deftest name-char.specialized.3
  (loop for etype in '(standard-char base-char character)
	append
	(loop for s in '("Rubout" "Page" "Backspace" "Return" "Tab" "Linefeed"
			 "Space" "Newline")
	      for s2 = (make-array (+ 3 (length s)) :element-type etype
				   :fill-pointer (length s)
				   :initial-contents (concatenate 'string s "   "))
	      unless (eql (name-char s) (name-char s2))
	      collect (list etype s s2)))
  nil)

(deftest name-char.specialized.4
  (loop for etype in '(standard-char base-char character)
	append
	(loop for s in '("Rubout" "Page" "Backspace" "Return" "Tab" "Linefeed"
			 "Space" "Newline")
	      for s1 = (make-array (+ 4 (length s)) :element-type etype
				   :initial-contents (concatenate 'string "  " s "  "))
	      for s2 = (make-array (length s) :element-type etype
				   :displaced-to s1 :displaced-index-offset 2)
	      unless (eql (name-char s) (name-char s2))
	      collect (list etype s s2)))
  nil)

;;; Error tests

(deftest name-char.error.1
  (signals-error (name-char) program-error)
  t)

(deftest name-char.error.2
  (signals-error (name-char "space" "space") program-error)
  t)
