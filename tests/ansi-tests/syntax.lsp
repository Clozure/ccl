;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan  2 08:12:51 2005
;;;; Contains: Tests of standard syntax

(in-package :cl-test)

(compile-and-load "reader-aux.lsp")

(def-syntax-test syntax.whitespace.1
  ;; Check that various standard or semistandard characters are whitespace[2]
  (let ((names '("Tab" "Newline" "Linefeed" "Space" "Return" "Page")))
    (loop for name in names
	  for c = (name-char name)
	  nconc
	  (when c
	    (let* ((s (concatenate 'string (string c) "123"))
		   (val (read-from-string s)))
	      (unless (eql val 123)
		(list (list name c s val)))))))
  nil)

(def-syntax-test syntax.constituent.1
  ;; Tests of various characters that they are constituent characters,
  ;; and parse to symbols
  (let ((chars (concatenate
		'string
		"!$%&*<=>?@[]^_-{}+/"
		"abcdefghijklmnopqrstuvwxyz"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (loop for c across chars
	  for s = (string c)
	  for sym = (read-from-string s)
	  unless (string= (symbol-name sym) (string-upcase s))
	  collect (list c sym)))
  nil)

;;; Backspace is an invalid constituent character

(def-syntax-test syntax.backspace.invalid
  (let ((c (name-char "Backspace")))
    (if (not c) t
      (eval `(signals-error (read-from-string (string ,c)) reader-error))))
  t)

;;; Rubout is an invalid constituent character

(def-syntax-test syntax.rubout.invalid
  (let ((c (name-char "Rubout")))
    (if (not c) t
      (eval `(signals-error (read-from-string (string ,c)) reader-error))))
  t)

;;; Digits are alphabetic if >= the read base

(def-syntax-test syntax.digits.alphabetic.1
  (loop for base from 2 to 9
	nconc
	(let ((*read-base* base))
	  (loop for digit-val from base to 9
		for c = (elt "0123456789" digit-val)
		for s = (string c)
		for val = (read-from-string s)
		unless (and (symbolp val)
			    (string= s (symbol-name val)))
		collect (list base digit-val c s val))))
  nil)

;;; Reading escaped characters

(def-syntax-test syntax.escaped.1
  (loop for c across +standard-chars+
	for s0 = (string c)
	for s = (concatenate 'string "\\" s0)
	for sym = (read-from-string s)
	unless (and (symbolp sym)
		    (string= (symbol-name sym) s0))
	collect (list c s0 s sym))
  nil)

(def-syntax-test syntax.escaped.2
  (let ((count 0))
    (loop for i from 0 below (min 65536 char-code-limit)
	  for c = (code-char i)
	  for s0 = (and c (string c))
	  for s = (and c (concatenate 'string "\\" s0))
	  for sym = (and c (read-from-string s))
	  unless (or (not c)
		     (and (symbolp sym)
			  (string= (symbol-name sym) s0)))
	  collect (progn
		    (when (> (incf count) 100) (loop-finish))
		    (list i c s0 s sym))))
  nil)

(def-syntax-test syntax.escaped.3
  (loop for i = (random (min char-code-limit (ash 1 24)))
	for c = (code-char i)
	for s0 = (and c (string c))
	for s = (and c (concatenate 'string "\\" s0))
	for sym = (and c (read-from-string s))
	repeat 1000
	unless (or (not c)
		   (and (symbolp sym)
			(string= (symbol-name sym) s0)))
	collect (list i c s0 s sym))
  nil)

(def-syntax-test syntax.escaped.4
  (loop for c across +standard-chars+
	for bad = (find c "\\|")
	for s0 = (string c)
	for s = (concatenate 'string "|" s0 "|")
	for sym = (and (not bad) (read-from-string s))
	unless (or bad
		   (and (symbolp sym)
			(string= (symbol-name sym) s0)))
	collect (list c s0 s sym))
  nil)

(def-syntax-test syntax.escaped.5
  (let ((count 0))
    (loop for i from 0 below (min 65536 char-code-limit)
	  for c = (code-char i)
	  for bad = (or (not c) (find c "\\|"))
	  for s0 = (and c (string c))
	  for s = (and c (concatenate 'string "|" s0 "|"))
	  for sym = (and c (not bad) (read-from-string s))
	  unless (or bad
		     (and (symbolp sym)
			  (string= (symbol-name sym) s0)))
	  collect (progn
		    (when (> (incf count) 100) (loop-finish))
		    (list c s0 s sym))))
  nil)

(def-syntax-test syntax.escaped.6
  (loop for i = (random (min char-code-limit (ash 1 24)))
	for c = (code-char i)
	for bad = (or (not c) (find c "\\|"))
	for s0 = (and c (string c))
	for s = (and c (concatenate 'string "|" s0 "|"))
	for sym = (and (not bad) (read-from-string s))
	repeat 1000
	unless (or bad
		   (and (symbolp sym)
			(string= (symbol-name sym) s0)))
	collect (list c s0 s sym))
  nil)

(def-syntax-test syntax.escape.whitespace.1
  (let ((names '("Tab" "Newline" "Linefeed" "Space" "Return" "Page"
		 "Rubout" "Backspace")))
    (loop for name in names
	  for c = (name-char name)
	  nconc
	  (when c
	    (let* ((s (concatenate 'string "\\" (string c)))
		   (val (read-from-string s)))
	      (unless (eql val (intern (string c)))
		(list (list name c s val)))))))
  nil)

;;;
;;; CLtS appears to be inconsistent on the next test.
;;; Compare the definition of 'invalid' with the specification
;;; of the token reading algorithm.
;;;
(def-syntax-test syntax.escape.whitespace.2
  (let ((names '("Tab" "Newline" "Linefeed" "Space" "Return" "Page")))
    (loop for name in names
	  for c = (name-char name)
	  nconc
	  (when c
	    (let* ((s (concatenate 'string "|" (string c) "|"))
		   (val (read-from-string s)))
	      (unless (eql val (intern (string c)))
		(list (list name c s val)))))))
  nil)

#|
(def-syntax-test syntax.multiple-escape.invalid.backspace
  (let ((c (name-char "Backspace")))
    (or (not c)
	(let ((s (concatenate 'string "|" (string c) "|")))
	  (eval `(signals-error (read-from-string ',s) reader-error)))))
  t)

(def-syntax-test syntax.multiple-escape.invalid.rubout
  (let ((c (name-char "Rubout")))
    (or (not c)
	(let ((s (concatenate 'string "|" (string c) "|")))
	  (eval `(signals-error (read-from-string ',s) reader-error)))))
  t)
|#


;;; Tests of #\

(def-syntax-test syntax.sharp-backslash.1
  (loop for c across +standard-chars+
	for s = (concatenate 'string "#\\" (string c))
	for c2 = (read-from-string s)
	unless (eql c c2)
	collect (list c s c2))
  nil)

(def-syntax-test syntax.sharp-backslash.2
  (let ((count 0))
    (loop for i below (min 65536 char-code-limit)
	  for c = (code-char i)
	  for s = (and c (concatenate 'string "#\\" (string c)))
	  for c2 = (and c (read-from-string s))
	  unless (eql c c2)
	  collect (progn (when (> (incf count) 100) (loop-finish))
			 (list c s c2))))
  nil)

(def-syntax-test syntax.sharp-backslash.3
  (loop for i = (random (min (ash 1 24) char-code-limit))
	for c = (code-char i)
	for s = (and c (concatenate 'string "#\\" (string c)))
	for c2 = (and c (read-from-string s))
	repeat 1000
	unless (eql c c2)
	collect (list i c s c2))
  nil)

(def-syntax-test syntax.sharp-backslash.4
  (flet ((%f (s) (read-from-string (concatenate 'string "#\\" s))))
    (loop for s in '("SPACE" "NEWLINE" "TAB" "RUBOUT" "BACKSPACE" "PAGE" "LINEFEED" "RETURN")
	  for c = (name-char s)
	  unless (or (null c)
		     (and (eql (%f s) c)
			  (eql (%f (string-downcase s)) c)
			  (eql (%f (string-capitalize s)) c)))
	  collect (list s c)))
  nil)

(def-syntax-test syntax.sharp-backslash.5
  (flet ((%f (s) (read-from-string (concatenate 'string "#\\" s))))
    (let ((good-chars (concatenate 'string +alphanumeric-chars+
				   "<,.>\"':/?[]{}~`!@#$%^&*_-+=")))
      (loop for c across +standard-chars+
	    for name = (char-name c)
	    unless (or (null name)
		       (string/= "" (string-trim good-chars name))
		       (and (eql (%f name) c)
			    (eql (%f (string-downcase name)) c)
			    (eql (%f (string-upcase name)) c)
			    (eql (%f (string-capitalize name)) c)))
	    collect (list c name))))
  nil)

(def-syntax-test syntax.sharp-backslash.6
  (flet ((%f (s) (read-from-string (concatenate 'string "#\\" s))))
    (let ((good-chars (concatenate 'string +alphanumeric-chars+
				   "<,.>\"':/?[]{}~`!@#$%^&*_-+=")))
      (loop for i below (min 65536 char-code-limit)
	    for c = (code-char i)
	    for name = (and c (char-name c))
	    unless (or (null name)
		       (string/= "" (string-trim good-chars name))
		       (and (eql (%f name) c)
			    (eql (%f (string-downcase name)) c)
			    (eql (%f (string-upcase name)) c)
			    (eql (%f (string-capitalize name)) c)))
	    collect (list i c name))))
  nil)

(def-syntax-test syntax.sharp-backslash.7
  (flet ((%f (s) (read-from-string (concatenate 'string "#\\" s))))
    (let ((good-chars (concatenate 'string +alphanumeric-chars+
				   "<,.>\"':/?[]{}~`!@#$%^&*_-+=")))
      (loop for i = (random (min (ash 1 24) char-code-limit))
	    for c = (code-char i)
	    for name = (and c (char-name c))
	    repeat 1000
	    unless (or (null name)
		       (string/= "" (string-trim good-chars name))
		       (and (eql (%f name) c)
			    (eql (%f (string-downcase name)) c)
			    (eql (%f (string-upcase name)) c)
			    (eql (%f (string-capitalize name)) c)))
	    collect (list i c name))))
  nil)


;;; Tests of #'

(def-syntax-test syntax.sharp-quote.1
  (read-from-string "#'X")
  (function |X|) 3)

(def-syntax-test syntax.sharp-quote.2
  (read-from-string "#':X")
  (function :|X|) 4)

(def-syntax-test syntax.sharp-quote.3
  (read-from-string "#'17")
  (function 17) 4)

(def-syntax-test syntax.sharp-quote.error.1
  (signals-error (read-from-string "#'") end-of-file)
  t)

(def-syntax-test syntax.sharp-quote.error.2
  (signals-error (read-from-string "(#'" nil nil) end-of-file)
  t)

;;; Tess of #(...)

(def-syntax-vector-test syntax.sharp-left-paren.1
  "#()")

(def-syntax-vector-test syntax.sharp-left-paren.2
  "#0()")

(def-syntax-vector-test syntax.sharp-left-paren.3
  "#(a)" a)

(def-syntax-vector-test syntax.sharp-left-paren.4
  "#(a b c)" a b c)

(def-syntax-vector-test syntax.sharp-left-paren.5
  "#2(a)" a a)

(def-syntax-vector-test syntax.sharp-left-paren.6
  "#5(a b)" a b b b b)

(def-syntax-vector-test syntax.sharp-left-paren.7
  "#5(a b c d e)" a b c d e)

(def-syntax-vector-test syntax.sharp-left-paren.8
  "#9(a b c d e)" a b c d e e e e e)

(def-syntax-test syntax.sharp-left-paren.9
  (let ((*read-base* 2))
    (read-from-string "#10(a)"))
  #(a a a a a a a a a a)
  6)

(def-syntax-test syntax.sharp-left-paren.error.1
  (signals-error (read-from-string "#(") end-of-file)
  t)

(def-syntax-test syntax.sharp-left-paren.error.2
  (signals-error (read-from-string "(#(" nil nil) end-of-file)
  t)

;;; Tests of #*

(def-syntax-bit-vector-test syntax.sharp-asterisk.1
  "#*")

(def-syntax-bit-vector-test syntax.sharp-asterisk.2
  "#0*")

(def-syntax-bit-vector-test syntax.sharp-asterisk.3
  "#1*0" 0)

(def-syntax-bit-vector-test syntax.sharp-asterisk.4
  "#1*1" 1)

(def-syntax-bit-vector-test syntax.sharp-asterisk.5
  "#2*1" 1 1)

(def-syntax-bit-vector-test syntax.sharp-asterisk.6
  "#2*0" 0 0)

(def-syntax-bit-vector-test syntax.sharp-asterisk.7
  "#5*010" 0 1 0 0 0)

(def-syntax-bit-vector-test syntax.sharp-asterisk.8
  "#7*0101" 0 1 0 1 1 1 1)

(def-syntax-bit-vector-test syntax.sharp-asterisk.9
  "#10*01010" 0 1 0 1 0 0 0 0 0 0)

(def-syntax-test syntax.sharp-asterisk.10
  (let ((*read-base* 3))
    (read-from-string "#10*01"))
  #*0111111111
  6)

(def-syntax-test syntax.sharp-asterisk.11
  (let ((*read-suppress* t))
    (values (read-from-string "#1* ")))
  nil)

(def-syntax-test syntax.sharp-asterisk.12
  (let ((*read-suppress* t))
    (values (read-from-string "#1*00")))
  nil)

(def-syntax-test syntax.sharp-asterisk.13
  (let ((*read-suppress* t))
    (values (read-from-string "#*012")))
  nil)

(def-syntax-test syntax.sharp-asterisk.error.1
  (signals-error (read-from-string "#1* X") reader-error)
  t)

(def-syntax-test syntax.sharp-asterisk.error.2
  (signals-error (read-from-string "#2*011") reader-error)
  t)

(def-syntax-test syntax.sharp-asterisk.error.3
  (signals-error (read-from-string "#*012") reader-error)
  t)

;;; Tests of #: ...

; (def-syntax-unintern-test syntax.sharp-colon.1 "")
; (def-syntax-unintern-test syntax.sharp-colon.2 "#")
(def-syntax-unintern-test syntax.sharp-colon.3 "a")
(def-syntax-unintern-test syntax.sharp-colon.4 "A")
(def-syntax-unintern-test syntax.sharp-colon.5 "NIL")
(def-syntax-unintern-test syntax.sharp-colon.6 "T")
(def-syntax-unintern-test syntax.sharp-colon.7 ".")


;;; Tests of #.

(def-syntax-test syntax.sharp-dot.1
  (read-from-string "#.(+ 1 2)")
  3 9)

(def-syntax-test syntax.sharp-dot.2
  (read-from-string "#.'X")
  X 4)

(def-syntax-test syntax.sharp-dot.error.1
  (signals-error (read-from-string "#.") end-of-file)
  t)

(def-syntax-test syntax.sharp-dot.error.2
  (signals-error (read-from-string "(#." nil nil) end-of-file)
  t)

(def-syntax-test syntax.sharp-dot.error.3
  (signals-error (let ((*read-eval* nil)) (read-from-string "#.1")) reader-error)
  t)

;;; Tests of #B

(def-syntax-test syntax.sharp-b.1
  (read-from-string "#b0")
  0 3)

(def-syntax-test syntax.sharp-b.2
  (read-from-string "#B1")
  1 3)

(def-syntax-test syntax.sharp-b.3
  (read-from-string "#b101101")
  45 8)

(def-syntax-test syntax.sharp-b.4
  (read-from-string "#B101101")
  45 8)

(def-syntax-test syntax.sharp-b.5
  (read-from-string "#b010001/100")
  17/4 12)

(def-syntax-test syntax.sharp-b.6
  (read-from-string "#b-10011")
  -19 8)

(def-syntax-test syntax.sharp-b.7
  (read-from-string "#B-1/10")
  -1/2 7)

(def-syntax-test syntax.sharp-b.8
  (read-from-string "#B-0/10")
  0 7)

(def-syntax-test syntax.sharp-b.9
  (read-from-string "#b0/111")
  0 7)

(def-syntax-test syntax.sharp-b.10
  (let ((*read-eval* nil))
    (read-from-string "#b-10/11"))
  -2/3 8)

;;; Tests of #O

(def-syntax-test syntax.sharp-o.1
  (read-from-string "#o0")
  0 3)

(def-syntax-test syntax.sharp-o.2
  (read-from-string "#O7")
  7 3)

(def-syntax-test syntax.sharp-o.3
  (read-from-string "#o10")
  8 4)

(def-syntax-test syntax.sharp-o.4
  (read-from-string "#O011")
  9 5)

(def-syntax-test syntax.sharp-o.5
  (read-from-string "#o-0")
  0 4)

(def-syntax-test syntax.sharp-o.6
  (read-from-string "#O-1")
  -1 4)

(def-syntax-test syntax.sharp-o.7
  (read-from-string "#O11/10")
  9/8 7)

(def-syntax-test syntax.sharp-o.8
  (read-from-string "#o-1/10")
  -1/8 7)

(def-syntax-test syntax.sharp-o.9
  (read-from-string "#O0/10")
  0 6)

(def-syntax-test syntax.sharp-o.10
  (let ((*read-eval* nil))
    (read-from-string "#o-10/11"))
  -8/9 8)

;;; Tests of #X

(def-syntax-test syntax.sharp-x.1
  (read-from-string "#x0")
  0 3)

(def-syntax-test syntax.sharp-x.2
  (read-from-string "#X1")
  1 3)

(def-syntax-test syntax.sharp-x.3
  (read-from-string "#xa")
  10 3)

(def-syntax-test syntax.sharp-x.4
  (read-from-string "#Xb")
  11 3)

(def-syntax-test syntax.sharp-x.5
  (read-from-string "#XC")
  12 3)

(def-syntax-test syntax.sharp-x.6
  (read-from-string "#xD")
  13 3)

(def-syntax-test syntax.sharp-x.7
  (read-from-string "#xe")
  14 3)

(def-syntax-test syntax.sharp-x.8
  (read-from-string "#Xf")
  15 3)

(def-syntax-test syntax.sharp-x.9
  (read-from-string "#x10")
  16 4)

(def-syntax-test syntax.sharp-x.10
  (read-from-string "#X1ab")
  427 5)

(def-syntax-test syntax.sharp-x.11
  (read-from-string "#x-1")
  -1 4)

(def-syntax-test syntax.sharp-x.12
  (read-from-string "#X-0")
  0 4)

(def-syntax-test syntax.sharp-x.13
  (read-from-string "#xa/B")
  10/11 5)

(def-syntax-test syntax.sharp-x.14
  (read-from-string "#X-1/1c")
  -1/28 7)

(def-syntax-test syntax.sharp-x.15
  (let ((*read-eval* nil))
    (read-from-string "#x-10/11"))
  -16/17 8)

;;; Tests of #nR

(def-syntax-test syntax.sharp-r.1
  (loop for i = (random (ash 1 (+ 2 (random 32))))
	for base = (+ 2 (random 35))
	for s = (write-to-string i :radix nil :base base :readably nil)
	for c = (random-from-seq "rR")
	for s2 = (format nil "#~d~c~a" base c s)
	for s3 = (rcase (1 (string-upcase s2))
			(1 (string-downcase s2))
			(1 (string-capitalize s2))
			(1 s2))
	for base2 = (+ 2 (random 35))
	for vals = (let ((*read-base* base2))
		     (multiple-value-list
		      (read-from-string s3)))
	repeat 1000
	unless (equal vals (list i (length s3) ))
	collect (list i base s c s2 s3 base2 vals))
  nil)

(def-syntax-test syntax.sharp-r.2
  (read-from-string "#2r0")
  0 4)

(def-syntax-test syntax.sharp-r.3
  (read-from-string "#36r0")
  0 5)

(def-syntax-test syntax.sharp-r.4
  (read-from-string "#29R-0")
  0 6)

(def-syntax-test syntax.sharp-r.5
  (read-from-string "#23r-1")
  -1 6)

(def-syntax-test syntax.sharp-r.6
  (read-from-string "#17r11")
  18 6)

(def-syntax-test syntax.sharp-t.7
  (read-from-string "#3r10/11")
  3/4 8)

(def-syntax-test syntax.sharp-t.8
  (read-from-string "#5R-10/11")
  -5/6 9)

;;; Tests of #c

(def-syntax-test syntax.sharp-c.1
  (read-from-string "#c(1 1)")
  #.(complex 1 1) 7)

(def-syntax-test syntax.sharp-c.2
  (read-from-string "#C(1 0)")
  1 7)

(def-syntax-test syntax.sharp-c.3
  (read-from-string "#c(0 1)")
  #.(complex 0 1) 7)

(def-syntax-test syntax.sharp-c.4
  (read-from-string "#c(-1/2 1)")
  #.(complex -1/2 1) 10)

(def-syntax-test syntax.sharp-c.5
  (read-from-string "#c (1 1)")
  #.(complex 1 1) 8)

(def-syntax-test syntax.sharp-c.6
  (loop for format in '(short-float single-float double-float long-float)
	for c = (let ((*read-default-float-format* format))
		  (read-from-string "#c(1.0 0.0)"))
	unless (eql c (complex (coerce 1 format)
			       (coerce 0 format)))
	collect (list format c))
  nil)

(def-syntax-test syntax.sharp-c.7
  (loop for format in '(short-float single-float double-float long-float)
	for c = (let ((*read-default-float-format* format))
		  (read-from-string "#C(0.0 1.0)"))
	unless (eql c (complex (coerce 0 format)
			       (coerce 1 format)))
	collect (list format c))
  nil)

;;; Tests of #a

(def-syntax-array-test syntax.sharp-a.1
  "#0anil"
  (make-array nil :initial-element nil))

(def-syntax-array-test syntax.sharp-a.2
  "#0a1"
  (make-array nil :initial-element 1))

(def-syntax-array-test syntax.sharp-a.3
  "#1a(1 2 3 5)"
  (make-array '(4) :initial-contents '(1 2 3 5)))

(def-syntax-array-test syntax.sharp-a.4
  "#1a\"abcd\""
  (make-array '(4) :initial-contents '(#\a #\b #\c #\d)))

(def-syntax-array-test syntax.sharp-a.5
  "#1a#1a(:a :b :c)"
  (make-array '(3) :initial-contents '(:a :b :c)))

(def-syntax-array-test syntax.sharp-a.6
  "#1a#.(coerce \"abcd\" 'simple-base-string)"
  (make-array '(4) :initial-contents '(#\a #\b #\c #\d)))

(def-syntax-array-test syntax.sharp-a.7
  "#1a#*000110"
  (make-array '(6) :initial-contents '(0 0 0 1 1 0)))

(def-syntax-array-test syntax.sharp-a.8
  "#1a#.(make-array 4 :element-type '(unsigned-byte 8)
                      :initial-contents '(1 2 3 5))"
  (make-array '(4) :initial-contents '(1 2 3 5)))

(def-syntax-array-test syntax.sharp-a.9
  "#1a#.(make-array 4 :element-type '(unsigned-byte 4)
                      :initial-contents '(1 2 3 5))"
  (make-array '(4) :initial-contents '(1 2 3 5)))

(def-syntax-array-test syntax.sharp-a.10
  "#1a#.(make-array 4 :element-type '(signed-byte 4)
                      :initial-contents '(1 2 3 5))"
  (make-array '(4) :initial-contents '(1 2 3 5)))

(def-syntax-array-test syntax.sharp-a.11
  "#1a#.(make-array 4 :element-type '(signed-byte 8)
                      :initial-contents '(1 2 3 5))"
  (make-array '(4) :initial-contents '(1 2 3 5)))

(def-syntax-array-test syntax.sharp-a.12
  "#1a#.(make-array 4 :element-type '(unsigned-byte 16)
                      :initial-contents '(1 2 3 5))"
  (make-array '(4) :initial-contents '(1 2 3 5)))

(def-syntax-array-test syntax.sharp-a.13
  "#1a#.(make-array 4 :element-type '(signed-byte 16)
                      :initial-contents '(1 2 3 5))"
  (make-array '(4) :initial-contents '(1 2 3 5)))

(def-syntax-array-test syntax.sharp-a.14
  "#1a#.(make-array 4 :element-type '(unsigned-byte 32)
                      :initial-contents '(1 2 3 5))"
  (make-array '(4) :initial-contents '(1 2 3 5)))

(def-syntax-array-test syntax.sharp-a.15
  "#1a#.(make-array 4 :element-type '(signed-byte 32)
                      :initial-contents '(1 2 3 5))"
  (make-array '(4) :initial-contents '(1 2 3 5)))

(def-syntax-array-test syntax.sharp-a.16
  "#1a#.(make-array 4 :element-type 'fixnum
                      :initial-contents '(1 2 3 5))"
  (make-array '(4) :initial-contents '(1 2 3 5)))

(def-syntax-array-test syntax.sharp-a.17
  "#1anil"
  (make-array '(0)))

(def-syntax-array-test syntax.sharp-a.18
  "#2anil"
  (make-array '(0 0)))

(def-syntax-array-test syntax.sharp-a.19
  "#2a((2))"
  (make-array '(1 1) :initial-element 2))

(def-syntax-array-test syntax.sharp-a.20
  "#2a((1 2 3)(4 5 6))"
  (make-array '(2 3) :initial-contents #(#(1 2 3) #(4 5 6))))

(def-syntax-array-test syntax.sharp-a.21
  "#2a#(#(1 2 3)#(4 5 6))"
  (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6))))

(def-syntax-array-test syntax.sharp-a.22
  "#2a\"\""
  (make-array '(0 0)))

(def-syntax-array-test syntax.sharp-a.23
  "#2a#*"
  (make-array '(0 0)))

(def-syntax-array-test syntax.sharp-a.24
  "#1a#.(make-array '(10) :fill-pointer 5 :initial-element 17)"
  (make-array '(5) :initial-contents '(17 17 17 17 17)))

(def-syntax-array-test syntax.sharp-a.25
  "#1a#.(make-array '(5) :adjustable t :initial-element 17)"
  (make-array '(5) :initial-contents '(17 17 17 17 17)))

(def-syntax-array-test syntax.sharp-a.26
  "#1A#.(let ((x (make-array '(10) :adjustable t
                      :initial-contents '(1 2 3 4 5 6 7 8 9 10))))
           (make-array '(5) :displaced-to x :displaced-index-offset 2))"
  (make-array '(5) :initial-contents '(3 4 5 6 7)))

;;; Tests of #S

(unless (find-class 'syntax-test-struct-1 nil)
  (defstruct syntax-test-struct-1
    a b c))

(def-syntax-test syntax.sharp-s.1
  (let ((v (read-from-string "#s(syntax-test-struct-1)")))
    (values
     (notnot (typep v 'syntax-test-struct-1))
     (syntax-test-struct-1-a v)
     (syntax-test-struct-1-b v)
     (syntax-test-struct-1-c v)))
  t nil nil nil)

(def-syntax-test syntax.sharp-s.2
  (let ((v (read-from-string "#S(syntax-test-struct-1 :a x :c y :b z)")))
    (values
     (notnot (typep v 'syntax-test-struct-1))
     (syntax-test-struct-1-a v)
     (syntax-test-struct-1-b v)
     (syntax-test-struct-1-c v)))
  t x z y)

(def-syntax-test syntax.sharp-s.3
  (let ((v (read-from-string "#s(syntax-test-struct-1 \"A\" x)")))
    (values
     (notnot (typep v 'syntax-test-struct-1))
     (syntax-test-struct-1-a v)
     (syntax-test-struct-1-b v)
     (syntax-test-struct-1-c v)))
  t x nil nil)

(def-syntax-test syntax.sharp-s.4
  (let ((v (read-from-string "#S(syntax-test-struct-1 #\\A x)")))
    (values
     (notnot (typep v 'syntax-test-struct-1))
     (syntax-test-struct-1-a v)
     (syntax-test-struct-1-b v)
     (syntax-test-struct-1-c v)))
  t x nil nil)

(def-syntax-test syntax.sharp-s.5
  (let ((v (read-from-string "#s(syntax-test-struct-1 :a x :a y)")))
    (values
     (notnot (typep v 'syntax-test-struct-1))
     (syntax-test-struct-1-a v)
     (syntax-test-struct-1-b v)
     (syntax-test-struct-1-c v)))
  t x nil nil)

(def-syntax-test syntax.sharp-s.6
  (let ((v (read-from-string "#S(syntax-test-struct-1 :a x :allow-other-keys 1)")))
    (values
     (notnot (typep v 'syntax-test-struct-1))
     (syntax-test-struct-1-a v)
     (syntax-test-struct-1-b v)
     (syntax-test-struct-1-c v)))
  t x nil nil)

(def-syntax-test syntax.sharp-s.7
  (let ((v (read-from-string "#s(syntax-test-struct-1 :b z :allow-other-keys nil)")))
    (values
     (notnot (typep v 'syntax-test-struct-1))
     (syntax-test-struct-1-a v)
     (syntax-test-struct-1-b v)
     (syntax-test-struct-1-c v)))
  t nil z nil)


(def-syntax-test syntax.sharp-s.8
  (let ((v (read-from-string "#S(syntax-test-struct-1 :b z :allow-other-keys t :a x :foo bar)")))
    (values
     (notnot (typep v 'syntax-test-struct-1))
     (syntax-test-struct-1-a v)
     (syntax-test-struct-1-b v)
     (syntax-test-struct-1-c v)))
  t x z nil)

(def-syntax-test syntax.sharp-s.9
  (let ((v (read-from-string "#s(syntax-test-struct-1 a x c y b z :a :bad :b bad2 :c bad3)")))
    (values
     (notnot (typep v 'syntax-test-struct-1))
     (syntax-test-struct-1-a v)
     (syntax-test-struct-1-b v)
     (syntax-test-struct-1-c v)))
  t x z y)

(def-syntax-test syntax.sharp-s.10
  (let ((v (read-from-string "#S(syntax-test-struct-1 #:a x #:c y #:b z)")))
    (values
     (notnot (typep v 'syntax-test-struct-1))
     (syntax-test-struct-1-a v)
     (syntax-test-struct-1-b v)
     (syntax-test-struct-1-c v)))
  t x z y)

;; (Put more tests of this in the structure tests)

;;; Tests of #P

(def-syntax-test syntax.sharp-p.1
  (read-from-string "#p\"\"")
  #.(parse-namestring "") 4)

(def-syntax-test syntax.sharp-p.2
  (read-from-string "#P\"syntax.lsp\"")
  #.(parse-namestring "syntax.lsp") 14)

(def-syntax-test syntax.sharp-p.3
  (read-from-string "#P \"syntax.lsp\"")
  #.(parse-namestring "syntax.lsp") 15)

(def-syntax-test syntax.sharp-p.4
  (let ((*read-eval* nil))
    (read-from-string "#p\"syntax.lsp\""))
  #.(parse-namestring "syntax.lsp") 14)

(def-syntax-test syntax.sharp-p.5
  (read-from-string "#P#.(make-array '(10) :initial-contents \"syntax.lsp\" :element-type 'base-char)")
  #.(parse-namestring "syntax.lsp") 78)

;;; #<number># and #<number>=

(def-syntax-test syntax.sharp-circle.1
  (let ((x (read-from-string "(#1=(17) #1#)")))
    (assert (eq (car x) (cadr x)))
    x)
  ((17) (17)))

(def-syntax-test syntax.sharp-circle.2
  (let ((x (read-from-string "(#0=(17) #0#)")))
    (assert (eq (car x) (cadr x)))
    x)
  ((17) (17)))

(def-syntax-test syntax.sharp-circle.3
  (let ((x (read-from-string "(#123456789123456789=(17) #123456789123456789#)")))
    (assert (eq (car x) (cadr x)))
    x)
  ((17) (17)))

(def-syntax-test syntax.sharp-circle.4
  (let ((x (read-from-string "#1=(A B . #1#)")))
    (assert (eq (cddr x) x))
    (values (car x) (cadr x)))
  a b)

(def-syntax-test syntax.sharp-circle.5
  (let ((x (read-from-string "#1=#(A B #1#)")))
    (assert (typep x '(simple-vector 3)))
    (assert (eq (elt x 2) x))
    (values (elt x 0) (elt x 1)))
  a b)

(def-syntax-test syntax.sharp-circle.6
  (let ((x (read-from-string "((#1=(17)) #1#)")))
    (assert (eq (caar x) (cadr x)))
    x)
  (((17)) (17)))

(def-syntax-test syntax.sharp-circle.7
  (let ((x (read-from-string "((#1=#2=(:x)) #1# #2#)")))
    (assert (eq (caar x) (cadr x)))
    (assert (eq (caar x) (caddr x)))
    x)
  (((:x)) (:x) (:x)))

;;; #+

(def-syntax-test syntax.sharp-plus.1
  (let ((*features* nil))
    (read-from-string "#+X :bad :good"))
  :good 14)

(def-syntax-test syntax.sharp-plus.2
  (let ((*features* '(:a :x :b)))
    (read-from-string "#+X :good :bad"))
  :good 10)

(def-syntax-test syntax.sharp-plus.3
  (let ((*features* '(:a :x :b)))
    (read-from-string "#+:x :good :bad"))
  :good 11)

(def-syntax-test syntax.sharp-plus.4
  (let ((*features* '(:a :x :b)))
    (read-from-string "#+(and):good :bad"))
  :good 13)

(def-syntax-test syntax.sharp-plus.5
  (let ((*features* '(:a :x :b)))
    (read-from-string "#+(:and):good :bad"))
  :good 14)

(def-syntax-test syntax.sharp-plus.6
  (let ((*features* '(:a :x :b)))
    (read-from-string "#+(or) :bad :good"))
  :good 17)

(def-syntax-test syntax.sharp-plus.7
  (let ((*features* '(:a :x :b)))
    (read-from-string "#+(:or) :bad :good"))
  :good 18)

(def-syntax-test syntax.sharp-plus.8
  (let ((*features* '(x)))
    (read-from-string "#+X :bad :good"))
  :good 14)

(def-syntax-test syntax.sharp-plus.9
  (let ((*features* '(x)))
    (read-from-string "#+CL-TEST::X :good :bad"))
  :good 19)

(def-syntax-test syntax.sharp-plus.10
  (let ((*features* nil))
    (read-from-string "#+(not x) :good :bad"))
  :good 16)

(def-syntax-test syntax.sharp-plus.11
  (let ((*features* '(:x)))
    (read-from-string "#+(not x) :bad :good"))
  :good 20)

(def-syntax-test syntax.sharp-plus.12
  (let ((*features* nil))
    (read-from-string "#+(:not :x) :good :bad"))
  :good 18)

(def-syntax-test syntax.sharp-plus.13
  (let ((*features* '(:a :x :b)))
    (read-from-string "#+(and a b) :good :bad"))
  :good 18)

(def-syntax-test syntax.sharp-plus.14
  (let ((*features* '(:a :x :b)))
    (read-from-string "#+(and a c) :bad :good"))
  :good 22)

(def-syntax-test syntax.sharp-plus.15
  (let ((*features* '(:a :x :b)))
    (read-from-string "#+(or c b) :good :bad"))
  :good 17)

(def-syntax-test syntax.sharp-plus.16
  (let ((*features* '(:a :x :b)))
    (read-from-string "#+(or c d) :bad :good"))
  :good 21)

;;; Tests of #| |#

(def-syntax-test syntax.sharp-bar.1
  (read-from-string "#||#1")
  1 5)

(def-syntax-test syntax.sharp-bar.2
  (read-from-string "1#||#2")
  |1##2| 6)

(def-syntax-test syntax.sharp-bar.3
  (read-from-string "#| #| |# |#1")
  1 12)

(def-syntax-test syntax.sharp-bar.4
  (read-from-string "#| ; |#1")
  1 8)

(def-syntax-test syntax.sharp-bar.5
  (read-from-string "#| ( |#1")
  1 8)

(def-syntax-test syntax.sharp-bar.6
  (read-from-string "#| # |#1")
  1 8)

(def-syntax-test syntax.sharp-bar.7
  (read-from-string "#| .. |#1")
  1 9)

(def-syntax-test syntax.sharp-bar.8
  (loop for c across +standard-chars+
	for s = (concatenate 'string "\#| " (string c) " |\#1")
	for vals = (multiple-value-list (read-from-string s))
	unless (equal vals '(1 8))
	collect (list c s vals))
  nil)

(def-syntax-test syntax.sharp-bar.9
  (loop for i below (min (ash 1 16) char-code-limit)
	for c = (code-char i)
	for s = (and c (concatenate 'string "\#| " (string c) " |\#1"))
	for vals = (and c (multiple-value-list (read-from-string s)))
	unless (or (not c) (equal vals '(1 8)))
	collect (list i c s vals))
  nil)

(def-syntax-test syntax.sharp-bar.10
  (loop for i = (random (min (ash 1 24) char-code-limit))
	for c = (code-char i)
	for s = (and c (concatenate 'string "\#| " (string c) " |\#1"))
	for vals = (and c (multiple-value-list (read-from-string s)))
	repeat 1000
	unless (or (not c) (equal vals '(1 8)))
	collect (list i c s vals))
  nil)

;;;; Various error cases

(def-syntax-test syntax.sharp-whitespace.1
  (let ((names '("Tab" "Newline" "Linefeed" "Space" "Return" "Page")))
    (loop for name in names
	  for c = (name-char name)
	  when c
	  nconc
	  (let* ((form `(signals-error
			 (read-from-string ,(concatenate 'string "#" (string c)))
			 reader-error))
		 (vals (multiple-value-list (eval form))))
	    (unless (equal vals '(t))
	      (list (list name c form vals))))))
  nil)

(def-syntax-test syntax.sharp-less-than.1
  (signals-error (read-from-string "#<" nil nil) reader-error)
  t)


(def-syntax-test syntax.sharp-close-paren.1
  (signals-error (read-from-string "#)" nil nil) reader-error)
  t)

(def-syntax-test syntax.single-escape-eof.1
  (signals-error (read-from-string "\\") end-of-file)
  t)

(def-syntax-test syntax.single-escape-eof.2
  (signals-error (read-from-string "\\" nil nil) end-of-file)
  t)

(def-syntax-test syntax.multiple-escape-eof.1
  (signals-error (read-from-string "|") end-of-file)
  t)

(def-syntax-test syntax.multiple-escape-eof.2
  (signals-error (read-from-string "|" nil nil) end-of-file)
  t)

