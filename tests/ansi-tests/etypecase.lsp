;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 23:02:23 2002
;;;; Contains: Tests of ETYPECASE

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

(deftest etypecase.1
  (etypecase 1 (integer 'a) (t 'b))
  a)

(deftest etypecase.2
  (signals-type-error x 1 (etypecase x (symbol 'a)))
  t)

(deftest etypecase.3
  (etypecase 1 (symbol 'a) (t 'b))
  b)

(deftest etypecase.4
  (etypecase 1 (t (values))))

(deftest etypecase.5
  (etypecase 1 (integer (values)) (t 'a)))

(deftest etypecase.6
  (etypecase 1 (bit 'a) (integer 'b))
  a)

(deftest etypecase.7
  (etypecase 1 (t 'a))
  a)

(deftest etypecase.8
  (etypecase 1 (t (values 'a 'b 'c)))
  a b c)

(deftest etypecase.9
  (etypecase 1 (integer (values 'a 'b 'c)) (t nil))
  a b c)

(deftest etypecase.10
  (let ((x 0))
    (values
     (etypecase 1
       (bit     (incf x)   'a)
       (integer (incf x 2) 'b)
       (t       (incf x 4) 'c))
     x))
  a 1)

(deftest etypecase.11
  (etypecase 1 (integer) (t 'a))
  nil)

(deftest etypecase.12
  (etypecase 'a
    (number 'bad)
    (#.(find-class 'symbol nil) 'good))
  good)

(deftest etypecase.13
  (block nil
    (tagbody
     (let ((x 'a))
       (etypecase x (symbol (go 10)
			    10
			    (return 'bad))))
     10
     (return 'good)))
  good)

(deftest etypecase.14
  (loop
   for x in '(1 a 1.3 "")
   collect
   (etypecase x (t :good) (integer :bad) (symbol :bad)
	      (float :bad) (string :bad)))
  (:good :good :good :good))

(deftest etypecase.15
  (let* ((u (coerce *universe* 'vector))
	 (len1 (length u))
	 (types (coerce *cl-all-type-symbols* 'vector))
	 (len2 (length types)))
    (loop
     for n = (random 10)
     for my-types = (loop repeat n collect (elt types (random len2)))
     for val = (elt u (random len1))
     for i = (position val my-types :test #'typep)
     for form = `(function
		  (lambda (x)
		    (handler-case
		     (etypecase x
		       ,@(loop for i from 0 for type in my-types collect `(,type ,i)))
		     (type-error (c)
				 (assert (eql x (type-error-datum c)))
				 (let* ((expected (type-error-expected-type c)))
				   (let ((equiv (check-equivalence expected
								   ',(cons 'or my-types))))
				     (assert (null equiv) () "EQUIV = ~A" EQUIV)))
				 nil))))
     for j = (funcall (eval form) val)
     repeat 200
     unless (eql i j)
     collect (list n my-types val i form j)))
  nil)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest etypecase.16
  (macrolet
   ((%m (z) z))
   (etypecase
    (expand-in-current-env (%m :foo))
    (integer :bad1)
    (keyword :good)
    (symbol :bad2)))
  :good)

(deftest etypecase.17
  (macrolet
   ((%m (z) z))
   (etypecase :foo
    (integer (expand-in-current-env (%m :bad1)))
    (keyword (expand-in-current-env (%m :good)))
    (symbol (expand-in-current-env (%m :bad2)))))
  :good)

;;; Error cases

(deftest etypecase.error.1
  (signals-error (funcall (macro-function 'etypecase))
		 program-error)
  t)

(deftest etypecase.error.2
  (signals-error (funcall (macro-function 'etypecase)
			   '(etypecase t))
		 program-error)
  t)

(deftest etypecase.error.3
  (signals-error (funcall (macro-function 'etypecase)
			   '(etypecase t) nil nil)
		 program-error)
  t)
