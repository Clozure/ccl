;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May  2 21:45:32 1998
;;;; Contains: Test code for structures, part 01

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;; Tests for structures
;;;
;;; The CL Spec leaves undefined just what will happen when a structure is
;;; redefined.  These tests don't redefine structures, but reloading a file
;;; with structure definition will do so.  I assume that this leaves the
;;; structure type unchanged.

;; Test simple defstruct (fields, no options)

(defstruct s-1
  foo bar)

;; Test that make-s-1 produces objects
;; of the correct type
(deftest structure-1-1
  (notnot-mv (typep (make-s-1) 's-1))
  t)

;; Test that the -p predicate exists
(deftest structure-1-2
  (notnot-mv (s-1-p (make-s-1)))
  t)

;; Test that all the objects in the universe are
;; not of this type
(deftest structure-1-3
  (count-if #'s-1-p *universe*)
  0)

(deftest structure-1-4
  (count-if #'(lambda (x) (typep x 's-1)) *universe*)
  0)

;; Check that the fields can be read after being initialized
(deftest structure-1-5
  (s-1-foo (make-s-1 :foo 'a))
  a)

(deftest structure-1-6
  (s-1-bar (make-s-1 :bar 'b))
  b)

(deftest structure-1-7
  (let ((s (make-s-1 :foo 'c :bar 'd)))
    (list (s-1-foo s) (s-1-bar s)))
  (c d))

;; Can setf the fields
(deftest structure-1-8
  (let ((s (make-s-1)))
    (setf (s-1-foo s) 'e)
    (setf (s-1-bar s) 'f)
    (list (s-1-foo s) (s-1-bar s)))
  (e f))

(deftest structure-1-9
  (let ((s (make-s-1 :foo 'a :bar 'b)))
    (setf (s-1-foo s) 'e)
    (setf (s-1-bar s) 'f)
    (list (s-1-foo s) (s-1-bar s)))
  (e f))

;; copier function defined
(deftest structure-1-10
  (let ((s (make-s-1 :foo 'a :bar 'b)))
    (let ((s2 (copy-s-1 s)))
      (setf (s-1-foo s) nil)
      (setf (s-1-bar s) nil)
      (list (s-1-foo s2)
	    (s-1-bar s2))))
  (a b))

;; Make produces unique items
(deftest structure-1-11
  (eqt (make-s-1) (make-s-1))
  nil)

(deftest structure-1-12
  (eqt (make-s-1 :foo 'a :bar 'b)
       (make-s-1 :foo 'a :bar 'b))
  nil)

;; More type and class checks

(deftest structure-1-13
  (notnot-mv (typep (class-of (make-s-1)) 'structure-class))
  t)

(deftest structure-1-14
  (notnot-mv (typep (make-s-1) 'structure-object))
  t)

(deftest structure-1-15
  (subtypep* 's-1 'structure-object)
  t t)
