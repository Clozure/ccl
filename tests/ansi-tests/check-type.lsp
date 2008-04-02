;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 20:12:04 2003
;;;; Contains: Tests of CHECK-TYPE

(in-package :cl-test)

(deftest check-type.1
  (let ((x 'a))
    (values (check-type x symbol) x))
  nil a)

(deftest check-type.2
  (signals-type-error x 'a (check-type x integer))
  t)

(deftest check-type.3
  (let ((x 'a))
    (handler-bind
     ((type-error #'(lambda (c)
		      (assert (eql (type-error-datum c) x))
		      (assert (not (typep x (type-error-expected-type c))))
		      ;; Can we assume the expected-type is NUMBER?
		      (store-value 15 c))))
     (values (check-type x number) x)))
  nil 15)

(deftest check-type.4
  (let ((x 'a))
    (values (check-type x symbol "a symbol") x))
  nil a)

(deftest check-type.5
  (let ((x 'a))
    (handler-bind
     ((type-error #'(lambda (c)
		      (assert (eql (type-error-datum c) x))
		      (assert (not (typep x (type-error-expected-type c))))
		      ;; Can we assume the expected-type is STRING?
		      (store-value "abc" c))))
     (values (check-type x string "a string") x)))
  nil "abc")

(deftest check-type.6
  (let ((x 'a))
    (handler-bind
     ((type-error #'(lambda (c)
		      (assert (eql (type-error-datum c) x))
		      (assert (not (typep x (type-error-expected-type c))))
		      ;; Can we assume the expected-type is NUMBER?
		      (store-value 15 nil))))
     (values (check-type x number) x)))
  nil 15)

(deftest check-type.7
  (let ((x 'a))
    (handler-bind
     ((type-error #'(lambda (c)
		      (assert (eql (type-error-datum c) x))
		      (assert (not (typep x (type-error-expected-type c))))
		      ;; Can we assume the expected-type is NUMBER?
		      (store-value 15))))
     (values (check-type x number) x)))
  nil 15)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest check-type.8
  (let ((x 10))
    (macrolet
     ((%m (z) z))
     (check-type (expand-in-current-env (%m x))
		 (integer 8 13))))
  nil)

(deftest check-type.9
  (let ((x 10))
    (macrolet
     ((%m (z) z))
     (check-type x (integer 9 12) (expand-in-current-env (%m "Foo!")))))
  nil)

