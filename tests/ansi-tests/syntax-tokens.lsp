;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jan 14 07:43:24 2005
;;;; Contains: Tests of reading of tokens

(in-package :cl-test)

(compile-and-load "reader-aux.lsp")

;; Erroneous numbers

(def-syntax-test syntax.number-token.error.1
  (signals-error (read-from-string "1/0") reader-error)
  t)

#|
(def-syntax-test syntax.number-token.error.2
  (loop for f in (list most-positive-short-float
		       most-positive-single-float
		       most-positive-double-float
		       most-positive-long-float)
	for c across "sfdl"
	for r = (float-radix f)
	for x = (nth-value 1 (decode-float f))
	for n = (1+ (ceiling (* (log r 10) x)))
	for s = (format nil "1.0~C~D" c n)
	for vals = (multiple-value-list
		    (eval `(signals-error (read-from-string ,s)
					  reader-error)))
	unless (equal vals '(t))
	collect (list f c r x n s vals))
  nil)
|#

(def-syntax-test syntax.number-token.3
  (loop for tp in '(short-float single-float double-float long-float)
	for c across "sfdl"
	for s = (concatenate 'string "1.0"
			     (make-string 1000 :initial-element #\0)
			     "1" (string c) "0")
	for n = (read-from-string s)
	unless (and (typep n tp)
		    (<= 1 n)
		    (< n 2))
	collect (list c tp s n))
  nil)

(def-syntax-test syntax.number-token.4
  (loop for type in '(short-float single-float double-float long-float)
	nconc
	(let* ((*read-default-float-format* type)
	       (s (concatenate 'string
			       "1." (make-string 1000 :initial-element #\0)
			       "1"))
	       (n (read-from-string s)))
	  (unless (and (typep n type)
		       (<= 1 n)
		       (< n 2))
	    (list (list type s n)))))
  nil)

	    
;;; Dot tokens

(def-syntax-test syntax.dot-token.1
  (read-from-string "\\.")
  |.| 2)

(def-syntax-test syntax.dot-token.2
  (read-from-string ".\\.")
  |..| 3)

(def-syntax-test syntax.dot-token.3
  (read-from-string "\\..")
  |..| 3)

(def-syntax-test syntax.dot-token.4
  (read-from-string "..\\.")
  |...| 4)

(def-syntax-test syntax.dot-token.5
  (read-from-string ".\\..")
  |...| 4)

(def-syntax-test syntax.dot-token.6
  (read-from-string "\\...")
  |...| 4)

(def-syntax-test syntax.dot-token.7
  (read-from-string ".||")
  |.| 3)

(def-syntax-test syntax.dot-token.8
  (read-from-string "..||")
  |..| 4)

(def-syntax-test syntax.dot-error.1
  (signals-error (read-from-string ".") reader-error)
  t)

(def-syntax-test syntax.dot-error.2
  (signals-error (read-from-string "..") reader-error)
  t)

(def-syntax-test syntax.dot-error.3
  (signals-error (read-from-string "...") reader-error)
  t)

(def-syntax-test syntax.dot-error.4
  (signals-error (read-from-string "( . 1)") reader-error)
  t)

(def-syntax-test syntax.dot-error.5
  (signals-error (read-from-string "(1 ..)") reader-error)
  t)

(def-syntax-test syntax.dot-error.6
  (signals-error (read-from-string "(1 .. 2)") reader-error)
  t)

(def-syntax-test syntax.dot-error.7
  (signals-error (read-from-string "#(1 . 2)") reader-error)
  t)

;;; right paren

(def-syntax-test syntax.right-paren-error.1
  (signals-error (read-from-string ")") reader-error)
  t)

(def-syntax-test syntax.comma-error.1
  (signals-error (read-from-string ",") reader-error)
  t)

(def-syntax-test syntax.comma-error.2
  (signals-error (read-from-string ",1") reader-error)
  t)

