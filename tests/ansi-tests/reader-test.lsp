;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Apr  8 20:03:45 1998
;;;; Contains: Tests on readtables (just started, very incomplete)

(in-package :cl-test)

(compile-and-load "reader-aux.lsp")

(def-syntax-test read-symbol.1
  (read-from-string "a")
  a 1)

(def-syntax-test read-symbol.2
  (read-from-string "|a|")
  |a| 3)

(def-syntax-test read-symbol.3
  (multiple-value-bind (s n)
      (read-from-string "#:abc")
    (not
     (and (symbolp s)
	  (eql n 5)
	  (not (symbol-package s))
	  (string-equal (symbol-name s) "abc"))))
  nil)

(def-syntax-test read-symbol.4
  (multiple-value-bind (s n)
      (read-from-string "#:|abc|")
    (not
     (and (symbolp s)
	  (eql n 7)
	  (not (symbol-package s))
	  (string= (symbol-name s) "abc"))))
  nil)

(def-syntax-test read-symbol.5
  (multiple-value-bind (s n)
      (read-from-string "#:||")
    (if (not (symbolp s))
	s
      (not (not
	    (and (eql n 4)
		 (not (symbol-package s))
		 (string= (symbol-name s) ""))))))
  t)

(def-syntax-test read-symbol.6
  (let ((str "cl-test::abcd0123"))
    (multiple-value-bind (s n)
	(read-from-string str)
      (if (not (symbolp s))
	  s
	(not (not
	      (and (eql n (length str))
		   (eqt (symbol-package s) (find-package :cl-test))
		   (string-equal (symbol-name s)
				 "abcd0123")))))))
  t)

(def-syntax-test read-symbol.7
  (multiple-value-bind (s n)
      (read-from-string ":ABCD")
    (if (not (symbolp s))
	s
      (not (not
	    (and (eql n 5)
		 (eqt (symbol-package s) (find-package "KEYWORD"))
		 (string-equal (symbol-name s)
			       "ABCD"))))))
  t)
	     
(defun read-symbol.9-body (natoms maxlen &optional (chars +standard-chars+))
  (loop
   repeat natoms
   count
   (let* ((len (random (1+ maxlen)))
	  (actual-len 0)
	  (s (make-string (+ 2 (* 2 len))))
	  (s2 (make-string len)))
     (loop for j from 0 to (1- len) do
	   (let ((c (random-from-seq chars)))
	     (when (member c '(#\| #\\))
	       (setf (elt s actual-len) #\\)
	       (incf actual-len))
	     (setf (elt s actual-len) c)
	     (setf (elt s2 j) c)
	     (incf actual-len)))
     (let ((actual-string (subseq s 0 actual-len)))
       (multiple-value-bind (sym nread)
	   (read-from-string
	    (concatenate 'string "#:|" actual-string "|"))
	 (unless (and (symbolp sym)
		      (eql nread (+ 4 actual-len))
		      (string-equal s2 (symbol-name sym)))
	   (let ((*print-readably* t))
	     (format t "Symbol read failed: ~S (~S) read as ~S~%"
		     actual-string s2 sym))
	   t))))))

(def-syntax-test read-symbol.9
  (read-symbol.9-body 1000 100)
  0)

(def-syntax-test read-symbol.9a
  (let ((chars (coerce (loop for i below (min 256 char-code-limit)
			     for c = (code-char i)
			     when c collect c)
		       'string)))
    (if (> (length chars) 0)
	(read-symbol.9-body 1000 100)
      0))
  0)

(def-syntax-test read-symbol.9b
  (let ((chars (coerce (loop for i below (min 65536 char-code-limit)
			     for c = (code-char i)
			     when c collect c)
		       'string)))
    (if (> (length chars) 0)
	(read-symbol.9-body 1000 100)
      0))
  0)

(def-syntax-test read-symbol.10
  (equalt (symbol-name
	   (read-from-string
	    (with-output-to-string (s)
				   (write (make-symbol ":")
					  :readably t
					  :stream s))))
	  ":")
  t)

(def-syntax-test read-symbol.11
  (loop for c across +standard-chars+
	for str = (make-array 2 :element-type 'character :initial-contents (list #\\ c))
	for sym = (read-from-string str)
	unless (and (symbolp sym)
		    (eql sym (find-symbol (string c)))
		    (equal (symbol-name sym) (string c)))
	collect (list c str sym))
  nil)

(def-syntax-test read-symbol.12
  (loop for c across +standard-chars+
	for str = (make-array 2 :element-type 'base-char :initial-contents (list #\\ c))
	for sym = (read-from-string str)
	unless (and (symbolp sym)
		    (eql sym (find-symbol (string c)))
		    (equal (symbol-name sym) (string c)))
	collect (list c str sym))
  nil)

(def-syntax-test read-symbol.13
  (loop for i below (min 65536 char-code-limit)
	for c = (code-char i)
	for str = (and c (make-array 2 :element-type 'character :initial-contents (list #\\ c)))
	for sym = (and c (read-from-string str))
	unless (or (not c)
		   (and (symbolp sym)
			(eql sym (find-symbol (string c)))
			(equal (symbol-name sym) (string c))))
	collect (list c str sym))
  nil)

(def-syntax-test read-symbol.14
  (loop for i = (random (min (ash 1 24) char-code-limit))
	for c = (code-char i)
	for str = (and c (make-array 2 :element-type 'character :initial-contents (list #\\ c)))
	for sym = (and c (read-from-string str))
	repeat 1000
	unless (or (not c)
		   (and (symbolp sym)
			(eql sym (find-symbol (string c)))
			(equal (symbol-name sym) (string c))))
	collect (list c str sym))
  nil)

(def-syntax-test read-symbol.15
  (loop for c across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@$%^&*_-+={}[]<>?/~"
	for str = (string c)
	for sym = (read-from-string str)
	unless (eql sym (find-symbol (string (char-upcase c))))
	collect (list c str sym))
  nil)

(def-syntax-test read-symbol.16
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :downcase)
    (loop for c across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@$%^&*_-+={}[]<>?/~"
	  for str = (string c)
	  for sym = (read-from-string str)
	  unless (eql sym (find-symbol (string (char-downcase c))))
	  collect (list c str sym)))
  nil)

(def-syntax-test read-symbol.17
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :preserve)
    (loop for c across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@$%^&*_-+={}[]<>?/~"
	  for str = (string c)
	  for sym = (read-from-string str)
	  unless (eql sym (find-symbol str))
	  collect (list c str sym)))
  nil)

(def-syntax-test read-symbol.18
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :invert)
    (loop for c across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@$%^&*_-+={}[]<>?/~"
	  for str = (string c)
	  for sym = (read-from-string str)
	  for c2 = (cond ((upper-case-p c) (char-downcase c))
			 ((lower-case-p c) (char-upcase c))
			 (t c))
	  unless (eql sym (find-symbol (string c2)))
	  collect (list c c2 str sym)))
  nil)

(def-syntax-test read-symbol.19
  (read-from-string "123||")
  |123| 5)

(def-syntax-test read-symbol.20
  (read-from-string "123\\4")
  |1234| 5)

(def-syntax-test read-symbol.21
  (read-from-string "\\:1234")
  |:1234| 6)

(def-syntax-test read-symbol.22
  (read-from-string "||")
  #.(intern "" (find-package "CL-TEST")) 2)

(def-syntax-test read-symbol.23
  (loop for c across +standard-chars+
	for s = (concatenate 'string (string c) ".")
	for sym = (intern (string-upcase s))
	when (alpha-char-p c)
	nconc
	(let ((sym2 (let ((*read-base* 36))
		      (read-from-string s))))
	  (if (eq sym sym2)
	      nil
	    (list c s sym sym2))))
  nil)

(def-syntax-test read-symbol.24
  (loop for c1 = (random-from-seq +alpha-chars+)
	for c2 = (random-from-seq +alpha-chars+)
	for d1 = (loop repeat (random 4) collect (random-from-seq +digit-chars+))
	for d2 = (loop repeat (random 4) collect (random-from-seq +digit-chars+))
	for s = (concatenate 'string d1 (list c1 c2) d2)
	for sym = (intern (string-upcase s))
	repeat 1000
	nconc
	(let ((sym2 (read-from-string s)))
	  (if (eq sym sym2)
	      nil
	    (list c1 c2 d1 d2 s sym sym2))))
  nil)

(def-syntax-test read-symbol.25
  (let ((potential-chars "01234567890123456789+-esdlf_^/")
	(*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :preserve)
    (loop for d1 = (loop repeat (random 6)
			 collect (random-from-seq potential-chars))
	  for c = (random-from-seq potential-chars)
	  for d2 = (loop repeat (random 6)
			 collect (random-from-seq potential-chars))
	  for s1 = (concatenate 'string d1 (list c) d2)
	  for sym1 = (intern s1)
	  for s2 = (concatenate 'string d1 (list #\\ c) d2)
	  for sym2 = (read-from-string s2)
	  repeat 1000
	  unless (eql sym1 sym2)
	  collect (list d1 c d2 s1 sym1 s2 sym2)))
  nil)

(deftest read-float.1
  (eqlt -0.0 (- 0.0))
  t)
