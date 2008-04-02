;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun 14 05:44:41 2003
;;;; Contains: Tests of COPY-SYMBOL

(in-package :cl-test)

(deftest copy-symbol.1
  (notnot-mv
   (every
    #'(lambda (x)
	(let ((y (copy-symbol x)))
	  (and (null (symbol-plist y))
	       (symbolp y)
	       (not (boundp y))
	       (not (fboundp y))
	       (null (symbol-package y))
	       (string= (symbol-name x) (symbol-name y))
	       (symbolp (copy-symbol y))
	       )))
    '(nil t a b |a| |123|)))
  t)

(deftest copy-symbol.2
  (progn
    (setf (symbol-plist '|foo|) '(a b c d))
    (makunbound '|foo|)
    (notnot-mv
     (every
      #'(lambda (x)
	  (let ((y (copy-symbol x t)))
	    (and
	     (equal (symbol-plist y) (symbol-plist x))
	     (symbolp y)
	     (if (boundp x)
		 (boundp y)
	       (not (boundp y)))
	     (if (fboundp x) (fboundp y) (not (fboundp y)))
	     (null (symbol-package y))
	     (string= (symbol-name x) (symbol-name y))
	     )))
      '(nil t a b |foo| |a| |123|))))
  t)

(deftest copy-symbol.3
  (progn
    (setf (symbol-plist '|foo|) '(a b c d))
    (setf (symbol-value '|a|) 12345)
    (notnot-mv
     (every
      #'(lambda (x)
	  (let ((y (copy-symbol x t)))
	    (and
	     (eql (length (symbol-plist y))
		  (length (symbol-plist x)))
	     ;; Is a list copy
	     (every #'eq (symbol-plist y) (symbol-plist x))
	     (symbolp y)
	     (if (boundp x)
		 (eqt (symbol-value x)
		      (symbol-value y))
	       (not (boundp y)))
	     (if (fboundp x) (fboundp y) (not (fboundp y)))
	     (null (symbol-package y))
	     (string= (symbol-name x) (symbol-name y))
	     (eql (length (symbol-plist x))
		  (length (symbol-plist y)))
	     )))
      '(nil t a b |foo| |a| |123|))))
  t)

(deftest copy-symbol.4
  (eqt (copy-symbol 'a) (copy-symbol 'a))
  nil)

(deftest copy-symbol.5
  (let ((i 0) x y (s '#:|x|))
    (let ((s2 (copy-symbol
	       (progn (setf x (incf i)) s)
	       (progn (setf y (incf i)) nil))))
      (values
       (symbol-name s2)
       (eq s s2)
       i x y)))
  "x" nil 2 1 2)

;;; Error tests

(deftest copy-symbol.error.1
  (signals-error (copy-symbol) program-error)
  t)

(deftest copy-symbol.error.2
  (signals-error (copy-symbol 'a t 'foo) program-error)
  t)
