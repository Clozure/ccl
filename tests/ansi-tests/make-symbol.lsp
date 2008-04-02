;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun 14 05:45:21 2003
;;;; Contains: Tests of MAKE-SYMBOL

(in-package :cl-test)

(deftest make-symbol.1
  (notnot-mv (symbolp (make-symbol "FOO")))
  t)

(deftest make-symbol.2
  (symbol-package (make-symbol "BAR"))
  nil)

(deftest make-symbol.3
  (symbol-package (make-symbol "CL::FOO"))
  nil)

(deftest make-symbol.4
  (symbol-package (make-symbol "CL:FOO"))
  nil)

(deftest make-symbol.5
  (symbol-name (make-symbol "xyz"))
  "xyz")

(deftest make-symbol.6
    (eqt (make-symbol "A")
	(make-symbol "A"))
  nil)

(deftest make-symbol.7
  (boundp (make-symbol "B"))
  nil)

(deftest make-symbol.8
  (symbol-plist (make-symbol "C"))
  nil)

(deftest make-symbol.9
  (fboundp (make-symbol "D"))
  nil)

(deftest make-symbol.10
  (symbol-name (make-symbol ""))
  "")

(deftest make-symbol.11
  :notes (:nil-vectors-are-strings)
  (symbol-name (make-symbol (make-array '(0) :element-type nil)))
  "")

(deftest make-symbol.12
  (let* ((name (make-array '(4) :initial-contents '(#\A #\B #\C #\D)
			   :element-type 'base-char))
	 (s (make-symbol name))
	 (name2 (symbol-name s)))
    (values
     (symbol-package s)
     (string=t name2 "ABCD")))
  nil t)

(deftest make-symbol.13
  (let* ((name (make-array '(6) :initial-contents '(#\A #\B #\C #\D #\E #\F)
			   :element-type 'character
			   :fill-pointer 4))
	 (s (make-symbol name))
	 (name2 (symbol-name s)))
    (values
     (symbol-package s)
     (string=t name2 "ABCD")))
  nil t)

(deftest make-symbol.14
  (let* ((name (make-array '(4) :initial-contents '(#\A #\B #\C #\D)
			   :adjustable t
			   :element-type 'character))
	 (s (make-symbol name))
	 (name2 (symbol-name s)))
    (values
     (symbol-package s)
     (string=t name2 "ABCD")))
  nil t)

(deftest make-symbol.15
  (let* ((name0 (make-array '(6) :initial-contents '(#\0 #\A #\B #\C #\D #\E)
			    :element-type 'character))
	 (name (make-array '(4) :element-type 'character
			   :displaced-to name0
			   :displaced-index-offset 1))
	 (s (make-symbol name))
	 (name2 (symbol-name s)))
    (values
     (symbol-package s)
     (string=t name2 "ABCD")))
  nil t)

(deftest make-symbol.16
  (let* ((name0 (make-array '(6) :initial-contents '(#\0 #\A #\B #\C #\D #\E)
			    :element-type 'base-char))
	 (name (make-array '(4) :element-type 'base-char
			   :displaced-to name0
			   :displaced-index-offset 1))
	 (s (make-symbol name))
	 (name2 (symbol-name s)))
    (values
     (symbol-package s)
     (string=t name2 "ABCD")))
  nil t)


(deftest make-symbol.order.1
  (let ((i 0))
    (values
     (symbol-name (make-symbol (progn (incf i) "ABC")))
     i))
  "ABC" 1)

(deftest make-symbol.error.1
  (check-type-error #'make-symbol #'stringp)
  nil)

(deftest make-symbol.error.9
  (signals-error (make-symbol) program-error)
  t)

(deftest make-symbol.error.10
  (signals-error (make-symbol "a" "a") program-error)
  t)

(deftest make-symbol.error.11
  (signals-type-error x '(#\a #\b #\c) (make-symbol x))
  t)
