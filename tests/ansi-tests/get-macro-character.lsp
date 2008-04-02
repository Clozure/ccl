;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan  2 15:54:27 2005
;;;; Contains: Tests of GET-MACRO-CHARACTER

(in-package :cl-test)

(compile-and-load "reader-aux.lsp")

(def-syntax-test get-macro-character.1
  (loop for c across "()';\"`,#"
	collect
	(let ((vals (multiple-value-list (get-macro-character c))))
	  (list
	   (=t (length vals) 2)
	   (or (notnot (functionp (car vals)))
	       (and (symbolp (car vals))
		    (notnot (fboundp (car vals)))))
	   (notnot (cadr vals)))))
  ((t t nil) (t t nil) (t t nil) (t t nil)
   (t t nil) (t t nil) (t t nil) (t t t)))

(def-syntax-test get-macro-character.2
  (loop for c across (concatenate
		      'string
		      "abcdefghijklmnopqrstuvwxyz"
		      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		      "1234567890!@$%^&*_-+={[}]<>?/~")
	for (fn non-term-p) = (multiple-value-list
			       (get-macro-character c))
	unless (or (null fn) non-term-p)
	collect (list c fn non-term-p))
  nil)

(def-syntax-test get-macro-character.3
  (loop for rt in (list nil *readtable* (copy-readtable))
	collect
	(loop for c across "()';\"`,#"
	      collect
	      (let ((vals (multiple-value-list (get-macro-character c rt))))
		(list
		 (=t (length vals) 2)
		 (or (notnot (functionp (car vals)))
		     (and (symbolp (car vals))
			  (notnot (fboundp (car vals)))))
		 (notnot (cadr vals))))))
  (((t t nil) (t t nil) (t t nil) (t t nil)
    (t t nil) (t t nil) (t t nil) (t t t))
   ((t t nil) (t t nil) (t t nil) (t t nil)
    (t t nil) (t t nil) (t t nil) (t t t))
   ((t t nil) (t t nil) (t t nil) (t t nil)
    (t t nil) (t t nil) (t t nil) (t t t))))

(def-syntax-test get-macro-character.4
  (loop for rt in (list nil *readtable* (copy-readtable))
	nconc
	(loop for c across (concatenate
			    'string
			    "abcdefghijklmnopqrstuvwxyz"
			    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			    "1234567890!@$%^&*_-+={[}]<>?/~")
	      for (fn non-term-p) = (multiple-value-list
				     (get-macro-character c rt))
	      unless (or (null fn) non-term-p)
	      collect (list rt c fn non-term-p)))
  nil)

;;; Copying a readtable preserves the reader macros

(def-syntax-test get-macro-character.5
  (let ((rt (copy-readtable)))
    (loop for c across +standard-chars+
	  for (fn1 ntp1) = (multiple-value-list (get-macro-character c))
	  for (fn2 ntp2) = (multiple-value-list (get-macro-character c rt))
	  unless (and (or (not (symbolp fn1))
			  (not (symbolp fn2))
			  (eql fn1 fn2))
		      (if ntp1 ntp2 (not ntp2)))
	  collect (list c fn1 ntp1 fn2 ntp2)))
  nil)

(def-syntax-test get-macro-character.6
  (let ((rt (copy-readtable)))
    (loop for i below (min 65536 char-code-limit)
	  for c = (code-char i)
	  for (fn1 ntp1) = (if c (multiple-value-list (get-macro-character c))
			     '(nil nil))
	  for (fn2 ntp2) = (if c (multiple-value-list (get-macro-character c rt))
			     '(nil nil))
	  unless (and (or (not (symbolp fn1))
			  (not (symbolp fn2))
			  (eql fn1 fn2))
		      (if ntp1 ntp2 (not ntp2)))
	  collect (list c fn1 ntp1 fn2 ntp2)))
  nil)

(def-syntax-test get-macro-character.7
  (let ((rt (copy-readtable)))
    (loop for i = (random (min char-code-limit (ash 1 24)))
	  for c = (code-char i)
	  for (fn1 ntp1) = (if c (multiple-value-list (get-macro-character c))
			     '(nil nil))
	  for (fn2 ntp2) = (if c (multiple-value-list (get-macro-character c rt))
			     '(nil nil))
	  repeat 10000
	  unless (and (or (not (symbolp fn1))
			  (not (symbolp fn2))
			  (eql fn1 fn2))
		      (if ntp1 ntp2 (not ntp2)))
	  collect (list c fn1 ntp1 fn2 ntp2)))
  nil)


;;; Error tests

(deftest get-macro-character.error.1
  (signals-error (get-macro-character) program-error)
  t)

(deftest get-macro-character.error.2
  (signals-error (get-macro-character #\; (copy-readtable) nil) program-error)
  t)


