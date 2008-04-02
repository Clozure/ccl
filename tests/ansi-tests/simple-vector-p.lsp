;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 22 21:23:45 2003
;;;; Contains: Tests for SIMPLE-VECTOR-P

(in-package :cl-test)

;;; More tests for this are in make-array.lsp

(deftest simple-vector-p.1
  (check-type-predicate #'simple-vector-p 'simple-vector)
  nil)

(deftest simple-vector-p.2
  (notnot-mv (simple-vector-p (make-array '(10))))
  t)

;; (deftest simple-vector-p.3
;;  (simple-vector-p (make-array '(5) :fill-pointer t))
;;  nil)

(deftest simple-vector-p.4
  (notnot-mv (simple-vector-p (vector 'a 'b 'c)))
  t)

;;; (deftest simple-vector-p.5
;;;  (simple-vector-p (make-array '(5) :adjustable t))
;;;  nil)

;;; (deftest simple-vector-p.6
;;;  (let ((a #(a b c d e g h)))
;;;    (simple-vector-p (make-array '(5) :displaced-to a)))
;;;   nil)

(deftest simple-vector-p.7
  (simple-vector-p #*001101)
  nil)

(deftest simple-vector-p.8
  (simple-vector-p "abcdef")
  nil)

(deftest simple-vector-p.9
  (simple-vector-p (make-array nil))
  nil)

(deftest simple-vector-p.10
  (simple-vector-p (make-array '(10) :element-type 'base-char))
  nil)

(deftest simple-vector-p.11
  (simple-vector-p (make-array '(10) :element-type 'character))
  nil)

(deftest simple-vector-p.12
  (simple-vector-p (make-array '(10) :element-type 'bit))
  nil)

;;; Error tests

(deftest simple-vector-p.error.1
  (signals-error (simple-vector-p) program-error)
  t)

(deftest simple-vector-p.error.2
  (signals-error (simple-vector-p #(a b) nil) program-error)
  t)
