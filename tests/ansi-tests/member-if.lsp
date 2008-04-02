;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:51:56 2003
;;;; Contains: Tests of MEMBER-IF

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest member-if.1
  (member-if #'listp nil)
  nil)

(deftest member-if.2
  (member-if #'(lambda (x) (eqt x 'a)) '(1 2 a 3 4))
  (a 3 4))

(deftest member-if.3
  (member-if #'(lambda (x) (eql x 12)) '(4 12 11 73 11) :key #'1+)
  (11 73 11))

(deftest member-if.4
  (let ((test-inputs
	 `(1 a 11.3121 11.31s3 1.123f5 -1 0
	     13.13122d34 581.131e-10
	     (a b c . d)
	     ,(make-array '(10))
	     "ancadas"  #\w)))
    (notnot-mv
     (every
      #'(lambda (x)
	  (let ((result (catch-type-error (member-if #'listp x))))
	    (or (eqt result 'type-error)
		(progn
		  (format t "~%On ~S: returned ~%~S" x result)
		  nil))))
      test-inputs)))
  t)

(deftest member-if.5
  (member-if #'identity '(1 2 3 4 5) :key #'evenp)
  (2 3 4 5))

;;; Order of argument tests

(deftest member-if.order.1
  (let ((i 0) x y)
    (values
     (member-if (progn (setf x (incf i))
		       #'identity)
		(progn (setf y (incf i))
		       '(nil nil a b nil c d)))
     i x y))
  (a b nil c d) 2 1 2)

(deftest member-if.order.2
  (let ((i 0) x y z w)
    (values
     (member-if (progn (setf x (incf i))
		       #'identity)
		(progn (setf y (incf i))
		       '(nil nil a b nil c d))
		:key (progn (setf z (incf i)) #'identity)
		:key (progn (setf w (incf i)) #'not))
			    
     i x y z w))
  (a b nil c d) 4 1 2 3 4)

;;; Keyword tests

(deftest member-if.keywords.1
  (member-if #'identity '(1 2 3 4 5) :key #'evenp :key #'oddp)
  (2 3 4 5))

(deftest member-if.allow-other-keys.2
  (member-if #'identity '(nil 2 3 4 5) :allow-other-keys t :bad t)
  (2 3 4 5))

(deftest member-if.allow-other-keys.3
  (member-if #'identity '(nil 2 3 4 5) :bad t :allow-other-keys t)
  (2 3 4 5))

(deftest member-if.allow-other-keys.4
  (member-if #'identity '(nil 2 3 4 5) :allow-other-keys t)
  (2 3 4 5))

(deftest member-if.allow-other-keys.5
  (member-if #'identity '(nil 2 3 4 5) :allow-other-keys nil)
  (2 3 4 5))

(deftest member-if.allow-other-keys.6
  (member-if #'identity '(nil 2 3 4 5) :allow-other-keys t
	     :allow-other-keys nil)
  (2 3 4 5))

(deftest member-if.allow-other-keys.7
  (member-if #'identity '(nil 2 3 4 5) :allow-other-keys t
	     :allow-other-keys nil :key #'identity :key #'null)
  (2 3 4 5))

;;; Error cases

(deftest member-if.error.1
  (check-type-error #'(lambda (x) (member-if #'identity x)) #'listp)
  nil)
  
(deftest member-if.error.2
  (signals-error (member-if) program-error)
  t)
  
(deftest member-if.error.3
  (signals-error (member-if #'null) program-error)
  t)
  
(deftest member-if.error.4
  (signals-error (member-if #'null '(a b c) :bad t) program-error)
  t)
  
(deftest member-if.error.5
  (signals-error (member-if #'null '(a b c) :bad t :allow-other-keys nil)
		 program-error)
  t)
  
(deftest member-if.error.6
  (signals-error (member-if #'null '(a b c) :key) program-error)
  t)
  
(deftest member-if.error.7
  (signals-error (member-if #'null '(a b c) 1 2) program-error)
  t)

(deftest member-if.error.8
  (signals-error (locally (member-if #'identity 'a) t) type-error)
  t)

(deftest member-if.error.9
  (signals-error (member-if #'cons '(a b c)) program-error)
  t)

(deftest member-if.error.10
  (signals-error (member-if #'identity '(a b c) :key #'cons) program-error)
  t)
