;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 30 07:36:26 2005
;;;; Contains: Tests of WITH-COMPILATION-UNIT

;;; WITH-COMPILATION-UNIT doesn't have much in the way of standardized
;;; semantics, so there's not much to test.

(in-package :cl-test)

(deftest with-compilation-unit.1
  (with-compilation-unit ())
  nil)

(deftest with-compilation-unit.2
  (with-compilation-unit () t)
  t)

(deftest with-compilation-unit.3
  (with-compilation-unit () (values)))

(deftest with-compilation-unit.4
  (with-compilation-unit () (values 1 2 3 4 5))
  1 2 3 4 5)

(deftest with-compilation-unit.5
  (with-compilation-unit (:override nil) :foo)
  :foo)
 
(deftest with-compilation-unit.6
  (with-compilation-unit (:override t) (values 10 17))
  10 17)

(deftest with-compilation-unit.7
  (let ((x nil))
    (values
     (block done
       (with-compilation-unit
	(:override nil)
	(setq x 1)
	(return-from done 2)
	(setq x 2)))
     x))
  2 1)

;;; Add a test that (1) checks if the compiler normally delays
;;; warnings until the end of a file and, if so, (2) checks that
;;; with-compilation-unit delays the warnings for more than one
;;; file compilation until the end of the unit.
