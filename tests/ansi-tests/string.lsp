;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep 30 19:16:59 2002
;;;; Contains: Tests for string related functions and classes

(in-package :cl-test)

(deftest string.1
  (subtypep* 'string 'array)
  t t)

(deftest string.2
  (subtypep* 'string 'vector)
  t t)

(deftest string.3
  (subtypep* 'string 'sequence)
  t t)

(deftest string.4
  (let ((s (string #\a)))
    (values (notnot (stringp s)) s))
  t "a")

(deftest string.5
  (let ((s (string "")))
    (values (notnot (stringp s)) s))
  t "")

(deftest string.6
  (let ((s (string '|FOO|)))
    (values (notnot (stringp s)) s))
  t "FOO")

(deftest string.7
  (check-predicate
   #'(lambda (x)
       (handler-case (stringp (string x))
		     (type-error () :caught))))
  nil)

(deftest string.8 
  :notes (:allow-nil-arrays :nil-vectors-are-strings)
  (subtypep* '(array nil (*)) 'string)
  t t)

(deftest string.9
  :notes (:allow-nil-arrays :nil-vectors-are-strings)
  (subtypep* '(array nil 1) 'string)
  t t)

(deftest string.10
  :notes (:allow-nil-arrays :nil-vectors-are-strings)
  (string (make-array '(0) :element-type nil))
  "")

(deftest string.11
  (typep* "abcd" 'string)
  t)

(deftest string.12
  :notes (:allow-nil-arrays :nil-vectors-are-strings)
  (typep* (make-array '(17) :element-type nil) 'string)
  t)

(deftest string.13
  :notes (:allow-nil-arrays :nil-vectors-are-strings)
  (typep* (make-array '(0) :element-type nil) 'string)
  t)

(deftest string.14
  (let ((count 0))
    (loop for i below (min char-code-limit 65536)
	  for c = (code-char i)
	  for s = (and c (string c))
	  when (and c
		    (or (not (stringp s))
			(not (= (length s) 1))
			(not (eql c (char s 0)))))
	  collect (progn (incf count) (list i c s))
	  until (>= count 100)))
  nil)

(deftest string.15
  (when (> char-code-limit 65536)
    (loop for i = (random char-code-limit)
	  for c = (code-char i)
	  for s = (and c (string c))
	  repeat 2000
	  when (and c
		    (or (not (stringp s))
			(not (= (length s) 1))
			(not (eql c (char s 0)))))
	  collect (list i c s)))
  nil)

(deftest string.16
  (check-predicate #'(lambda (s) (or (not (stringp s)) (eq s (string s)))))
  nil)

(deftest string.17
  (typep* "abc" '(string))
  t)

(deftest string.18
  (typep* "abc" '(string *))
  t)

(deftest string.19
  (typep* "abc" '(string 3))
  t)

(deftest string.20
  (typep* "abc" '(string 2))
  nil)

(deftest string.21
  (typep* "abc" '(string 4))
  nil)

(deftest string.22
  (do-special-strings (s "X") (assert (typep s 'string)))
  nil)
		     
(deftest string.23
  (do-special-strings (s "X") (assert (typep s '(string))))
  nil)
		     
(deftest string.24
  (do-special-strings (s "X") (assert (typep s '(string *))))
  nil)
		     
(deftest string.25
  (do-special-strings (s "X")
		      (or (array-has-fill-pointer-p s)
			  (assert (typep s '(string 1)))))
  nil)

(deftest string.26
  (let ((i 0))
    (values (string (progn (incf i) "")) i))
  "" 1)

(def-fold-test string.fold.1 (string #\A))

;;; Error tests

(deftest string.error.1
  (signals-error (string) program-error)
  t)

(deftest string.error.2
  (signals-error (string nil nil) program-error)
  t)
