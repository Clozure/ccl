;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Mar  5 07:12:20 2004
;;;; Contains: Tests for printing of characters

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

;;; See CLtS section 22.1.3.2, "Printing Characters"

(deftest print.char.1
  (with-standard-io-syntax
    (loop for c across +standard-chars+
	  unless (equal (string c)
			(with-output-to-string (s)
			  (princ c s)))
	  collect c))
  nil)

(deftest print.char.2
  (with-standard-io-syntax
    (loop for c across +code-chars+
	  unless (equal (string c)
			(with-output-to-string (s)
			  (princ c s)))
	  collect c))
  nil)

(deftest print.char.3
  (with-standard-io-syntax
    (let ((*print-readably* nil))
      (loop for c across +base-chars+
	    unless (or (eql c #\Space)
		       (equal (format nil "#\\~C" c)
			      (with-output-to-string (s)
				(prin1 c s))))
	    collect c)))
  nil)

(deftest print.char.4
  (with-standard-io-syntax
    (let ((*print-readably* nil))
      (with-output-to-string (s)
	(prin1 #\Space s))))
  "#\\ ")

(deftest print.char.5
  (with-standard-io-syntax
    (let ((*print-readably* nil))
      (with-output-to-string (s)
	(prin1 #\Newline s))))
  "#\\Newline")

(deftest print.char.6
  (with-standard-io-syntax
    (let ((*print-readably* nil))
      (with-output-to-string (s)
	(princ #\Newline s))))
  #.(string #\Newline))

(deftest print.char.7
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (loop for c across +code-chars+
	   for str = (with-output-to-string (s) (prin1 c s))
	   for len = (length str)
	   unless (and (>= len 3)
		       (equal (subseq str 0 2) "#\\")
		       (or (= len 3)
			   (let ((name (subseq str 2)))
			     (eql c (name-char name)))))
	   collect c)))
  nil)

(deftest print.char.8
  (loop for i = (random (min char-code-limit (ash 1 16)))
	for c = (code-char i)
	repeat 1000
	unless (null c)
	nconc (let ((result (randomly-check-readability c)))
		(and result (list (cons i (first result))))))
  nil)

(deftest print.char.9
  (loop for i = (random (min char-code-limit (ash 1 32)))
	for c = (code-char i)
	repeat 1000
	unless (null c)
	nconc (let ((result (randomly-check-readability c)))
		(and result (list (cons i (first result))))))
  nil)

(deftest print.char.10
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (loop for c across +standard-chars+
	   for str = (with-output-to-string (s) (prin1 c s))
	   unless (or (eql c #\Newline)
		      (equal str (concatenate 'string "#\\" (string c))))
	   collect (list c str))))
  nil)

(deftest print.char.11
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (let ((names '("Newline" "Tab" "Rubout" "Linefeed" "Page"
		    "Backspace" "Return")))
       (loop for name in names
	     for c = (name-char name)
	     for str = (with-output-to-string (s) (prin1 c s))
	     unless (or (null c)
			(and (>= (length str) 3)
			     (equal (subseq str 0 2) "#\\")
			     (member (subseq str 2) names
				     :test #'equal)))
	     collect (list c str)))))
  nil)