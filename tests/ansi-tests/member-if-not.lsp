;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:39:29 1998
;;;; Contains: Tests of MEMBER-IF-NOT

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest member-if-not.1
  (member-if-not #'listp nil)
  nil)

(deftest member-if-not.2
  (member-if-not #'(lambda (x) (eqt x 'a)) '(a 1 2 a 3 4))
  (1 2 a 3 4))

(deftest member-if-not.3
  (member-if-not #'(lambda (x) (not (eql x 12))) '(4 12 11 73 11) :key #'1+)
  (11 73 11))

(deftest member-if-not.4
  (let ((test-inputs
	 `(1 a 11.3121 11.31s3 1.123f5 -1 0
	     13.13122d34 581.131e-10
	     ((a) (b) (c) . d)
	     ,(make-array '(10))
	     "ancadas"  #\w)))
    (not (every
	  #'(lambda (x)
	      (let ((result (catch-type-error (member-if-not #'listp x))))
		(or (eqt result 'type-error)
		    (progn
		      (format t "~%On x = ~S, returns: ~%~S" x result)
		      nil))))
	  test-inputs)))
  nil)

(deftest member-if-not.5
  (member-if-not #'not '(1 2 3 4 5) :key #'evenp)
  (2 3 4 5))

;;; Order of evaluation tests

(deftest member-if-not.order.1
  (let ((i 0) x y)
    (values
     (member-if-not (progn (setf x (incf i))
			   #'not)
		    (progn (setf y (incf i))
			   '(nil nil a b nil c d)))
     i x y))
  (a b nil c d) 2 1 2)

(deftest member-if-not.order.2
  (let ((i 0) x y z w)
    (values
     (member-if-not (progn (setf x (incf i))
			   #'not)
		    (progn (setf y (incf i))
			   '(nil nil a b nil c d))
		    :key (progn (setf z (incf i)) #'identity)
		    :key (progn (setf w (incf i)) #'not))
			    
     i x y z w))
  (a b nil c d) 4 1 2 3 4)

;;; Keyword tests

(deftest member-if-not.keywords.1
  (member-if-not #'not '(1 2 3 4 5) :key #'evenp :key #'oddp)
  (2 3 4 5))

(deftest member-if-not.allow-other-keys.2
  (member-if-not #'not '(nil 2 3 4 5) :allow-other-keys t :bad t)
  (2 3 4 5))

(deftest member-if-not.allow-other-keys.3
  (member-if-not #'not '(nil 2 3 4 5) :bad t :allow-other-keys t)
  (2 3 4 5))

(deftest member-if-not.allow-other-keys.4
  (member-if-not #'not '(nil 2 3 4 5) :allow-other-keys t)
  (2 3 4 5))

(deftest member-if-not.allow-other-keys.5
  (member-if-not #'not '(nil 2 3 4 5) :allow-other-keys nil)
  (2 3 4 5))

(deftest member-if-not.allow-other-keys.6
  (member-if-not #'not '(nil 2 3 4 5) :allow-other-keys t
		 :allow-other-keys nil :key #'identity :key #'null)
  (2 3 4 5))

;;; Error tests

(deftest member-if-not.error.1
  (check-type-error #'(lambda (x) (member-if-not #'identity x)) #'listp)
  nil)
  
(deftest member-if-not.error.2
  (signals-error (member-if-not) program-error)
  t)
  
(deftest member-if-not.error.3
  (signals-error (member-if-not #'null) program-error)
  t)
  
(deftest member-if-not.error.4
  (signals-error (member-if-not #'null '(a b c) :bad t) program-error)
  t)
  
(deftest member-if-not.error.5
  (signals-error (member-if-not #'null '(a b c) :bad t :allow-other-keys nil)
		 program-error)
  t)
  
(deftest member-if-not.error.6
  (signals-error (member-if-not #'null '(a b c) :key) program-error)
  t)
  
(deftest member-if-not.error.7
  (signals-error (member-if-not #'null '(a b c) 1 2) program-error)
  t)

(deftest member-if-not.error.8
  (signals-error (locally (member-if-not #'identity 'a) t)
		 type-error)
  t)

(deftest member-if-not.error.9
  (signals-error (member-if-not #'cons '(a b c)) program-error)
  t)

(deftest member-if-not.error.10
  (signals-error (member-if-not #'identity '(a b c) :key #'cons)
		 program-error)
  t)
