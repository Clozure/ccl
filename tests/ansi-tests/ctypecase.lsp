;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 23:05:10 2002
;;;; Contains: Tests of CTYPECASE

(in-package :cl-test)

(deftest ctypecase.1
  (let ((x 1))
    (ctypecase x (integer 'a) (t 'b)))
  a)

(deftest ctypecase.2
  (check-type-error #'(lambda (x) (ctypecase x (symbol 'a))) #'symbolp)
  nil)

(deftest ctypecase.3
  (let ((x 1))
    (ctypecase x (symbol 'a) (t 'b)))
  b)

(deftest ctypecase.4
  (let ((x 1))
    (ctypecase x (t (values)))))

(deftest ctypecase.5
  (let ((x 1))
    (ctypecase x (integer (values)) (t 'a))))

(deftest ctypecase.6
  (let ((x 1))
    (ctypecase x (bit 'a) (integer 'b)))
  a)

(deftest ctypecase.7
  (let ((x 1))
    (ctypecase x (t 'a)))
  a)

(deftest ctypecase.8
  (let ((x 1))
    (ctypecase x (t (values 'a 'b 'c))))
  a b c)

(deftest ctypecase.9
  (let ((x 1))
    (ctypecase x (integer (values 'a 'b 'c)) (t nil)))
  a b c)

(deftest ctypecase.10
  (let ((x 0) (y 1))
    (values
     (ctypecase y
       (bit     (incf x)   'a)
       (integer (incf x 2) 'b)
       (t       (incf x 4) 'c))
     x))
  a 1)

(deftest ctypecase.11
  (let ((x 1))
    (ctypecase x (integer) (t 'a)))
  nil)

(deftest ctypecase.12
  (let ((x 1))
    (values
     (handler-bind
      ((type-error #'(lambda (c)
		       (assert (eql (type-error-datum c) 1))
		       (assert (not (typep 1 (type-error-expected-type c))))
		       (store-value 'a c))))
      (ctypecase x
       (symbol :good)
       (float :bad)))
     x))
  :good a)

;;; (deftest ctypecase.error.1
;;;  (signals-error (ctypecase) program-error)
;;;  t)


(deftest ctypecase.13
  (let ((x 'a))
    (ctypecase x
	       (number 'bad)
	       (#.(find-class 'symbol nil) 'good)))
  good)

(deftest ctypecase.14
  (block done
    (tagbody
     (let ((x 'a))
       (ctypecase x (symbol (go 10)
			    10
			    (return-from done 'bad))))
     10
     (return-from done 'good)))
  good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest ctypecase.15
  (macrolet
   ((%m (z) z))
   (ctypecase
    (expand-in-current-env (%m :foo))
    (integer :bad1)
    (keyword :good)
    (symbol :bad2)))
  :good)

(deftest ctypecase.16
  (macrolet
   ((%m (z) z))
   (ctypecase :foo
    (integer (expand-in-current-env (%m :bad1)))
    (keyword (expand-in-current-env (%m :good)))
    (symbol (expand-in-current-env (%m :bad2)))))
  :good)

(deftest ctypecase.error.1
  (signals-error (funcall (macro-function 'ctypecase))
		 program-error)
  t)

(deftest ctypecase.error.2
  (signals-error (funcall (macro-function 'ctypecase)
			   '(ctypecase t))
		 program-error)
  t)

(deftest ctypecase.error.3
  (signals-error (funcall (macro-function 'ctypecase)
			   '(ctypecase t)
			   nil nil)
		 program-error)
  t)
