;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 06:20:51 2003
;;;; Contains: Tests for ARRAY-DISPLACEMENT

(in-package :cl-test)

;;; The tests in make-array.lsp also test array-displacement

;;; The standard is contradictory about whether arrays created with
;;; :displaced-to NIL should return NIL as their primary value or
;;; not.  I will assume (as per Kent Pitman's comment on comp.lang.lisp)
;;; that an implementation is free to implement all arrays as actually
;;; displaced.  Therefore, I've omitted all the tests of not-expressly
;;; displaced arrays.

;;; Behavior on expressly displaced arrays

(deftest array-displacement.7
  (let* ((a (make-array '(10)))
	 (b (make-array '(10) :displaced-to a)))
    (multiple-value-bind* (dt disp)
	(array-displacement b)
      (and (eqt a dt)
	   (eqlt disp 0))))
  t)

(deftest array-displacement.8
  (let* ((a (make-array '(10)))
	 (b (make-array '(5) :displaced-to a :displaced-index-offset 2)))
    (multiple-value-bind* (dt disp)
	(array-displacement b)
      (and (eqt a dt)
	   (eqlt disp 2))))
  t)

(deftest array-displacement.9
  (let* ((a (make-array '(10) :element-type 'base-char))
	 (b (make-array '(5) :displaced-to a :displaced-index-offset 2
			:element-type 'base-char)))
    (multiple-value-bind* (dt disp)
	(array-displacement b)
      (and (eqt a dt)
	   (eqlt disp 2))))
  t)

(deftest array-displacement.10
  (let* ((a (make-array '(10) :element-type 'base-char))
	 (b (make-array '(5) :displaced-to a
			:element-type 'base-char)))
    (multiple-value-bind* (dt disp)
	(array-displacement b)
      (and (eqt a dt)
	   (eqlt disp 0))))
  t)

(deftest array-displacement.11
  (let* ((a (make-array '(10) :element-type 'bit))
	 (b (make-array '(5) :displaced-to a :displaced-index-offset 2
			:element-type 'bit)))
    (multiple-value-bind* (dt disp)
	(array-displacement b)
      (and (eqt a dt)
	   (eqlt disp 2))))
  t)

(deftest array-displacement.12
  (let* ((a (make-array '(10) :element-type 'bit))
	 (b (make-array '(5) :displaced-to a
			:element-type 'bit)))
    (multiple-value-bind* (dt disp)
	(array-displacement b)
      (and (eqt a dt)
	   (eqlt disp 0))))
  t)

(deftest array-displacement.13
  (let* ((a (make-array '(10) :element-type '(integer 0 255)))
	 (b (make-array '(5) :displaced-to a :displaced-index-offset 2
			:element-type '(integer 0 255))))
    (multiple-value-bind* (dt disp)
	(array-displacement b)
      (and (eqt a dt)
	   (eqlt disp 2))))
  t)

(deftest array-displacement.14
  (let* ((a (make-array '(10) :element-type '(integer 0 255)))
	 (b (make-array '(5) :displaced-to a
			:element-type '(integer 0 255))))
    (multiple-value-bind* (dt disp)
	(array-displacement b)
      (and (eqt a dt)
	   (eqlt disp 0))))
  t)

(deftest array-displacement.15
  (let* ((a (make-array '(10) :initial-contents '(a b c d e f g h i j)))
	 (b (make-array '(5) :displaced-to a :displaced-index-offset 2)))
    (macrolet
     ((%m (z) z))
     (multiple-value-bind
      (x y)
      (array-displacement (expand-in-current-env (%m b)))
      (values (eqlt x a) y))))
  t 2)

;;; FIXME: Add tests for other kinds of specialized arrays
;;;  (character, other integer types, float types, complex types)

(deftest array-displacement.order.1
  (let* ((a (make-array '(10)))
	 (b (make-array '(10) :displaced-to a))
	 (i 0))
    (multiple-value-bind* (dt disp)
	(array-displacement (progn (incf i) b))
      (and (eql i 1)
	   (eqt a dt)
	   (eqlt disp 0))))
  t)

;;; Error tests

(deftest array-displacement.error.1
  (signals-error (array-displacement) program-error)
  t)

(deftest array-displacement.error.2
  (signals-error (array-displacement #(a b c) nil) program-error)
  t)

(deftest array-displacement.error.3
  (check-type-error #'array-displacement #'arrayp)
  nil)

(deftest array-displacement.error.4
  (signals-type-error x nil (array-displacement x))
  t)
