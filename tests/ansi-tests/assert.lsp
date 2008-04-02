;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 28 06:48:19 2003
;;;; Contains: Tests of ASSERT

(in-package :cl-test)

(deftest assert.1
  (assert t)
  nil)

(deftest assert.2
  (assert t ())
  nil)

;;; I am assuming that when no places are given to ASSERT,
;;; it doesn't invoke any interactive handler.

(deftest assert.3
  (let ((x nil))
    (handler-bind
     ((error #'(lambda (c)
		 (setq x 17)
		 (let ((r (find-restart 'continue c)))
		   (when r (invoke-restart r))))))
     (assert x)
     x))
  17)

(deftest assert.3a
  (let ((x nil))
    (handler-bind
     ((error #'(lambda (c)
		 (setq x 17)
		 (continue c))))
     (assert x)
     x))
  17)


;;; I don't yet know how to test the interactive version of ASSERT
;;; that is normally invoked when places are given.

;;; Tests of the syntax (at least)

(deftest assert.4
  (let (x)
    (assert t (x)))
  nil)

(deftest assert.5
  (let ((x (cons 'a 'b)))
    (assert t ((car x) (cdr x))))
  nil)

(deftest assert.6
  (let ((x (vector 'a 'b 'c)))
    (assert t ((aref x 0) (aref x 1) (aref x 2))
	    "Vector x has value: ~A." x))
  nil)

(deftest assert.7
  (let ((x nil))
    (handler-bind
     ((simple-error #'(lambda (c)
			(setq x 17)
			(continue c))))
     (assert x () 'simple-error)
     x))
  17)

(deftest assert.8
  (let ((x 0))
    (handler-bind
     ((type-error #'(lambda (c)
			(incf x)
			(continue c))))
     (assert (> x 5) () 'type-error)
     x))
  6)

(deftest assert.9
  (let ((x 0))
    (handler-bind
     ((type-error #'(lambda (c) (declare (ignore c))
			(incf x)
			(continue))))
     (assert (> x 5) () 'type-error)
     x))
  6)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest assert.10
  (macrolet
   ((%m (z) z))
   (assert (expand-in-current-env (%m t))))
  nil)

(deftest assert.11
  (macrolet
   ((%m (z) z))
   (assert (expand-in-current-env (%m t)) () "Foo!"))
  nil)
