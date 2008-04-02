;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jul 12 18:25:53 2004
;;;; Contains: Tests for REMPROP

(in-package :cl-test)

(deftest remprop.1
  (let ((sym (gensym)))
    (values
     (symbol-plist sym)
     (multiple-value-list (remprop sym :foo))
     (symbol-plist sym)))
  nil (nil) nil)

(deftest remprop.2
  (let ((sym (gensym)))
    (values
     (symbol-plist sym)
     (copy-list (setf (symbol-plist sym) '(:foo 0)))
     (multiple-value-list (notnot-mv (remprop sym :foo)))
     (symbol-plist sym)))
  nil (:foo 0) (t) nil)

(deftest remprop.3
  (let ((sym (gensym)))
    (values
     (symbol-plist sym)
     (copy-list (setf (symbol-plist sym) (list :bar 1 :foo 0 :baz 2)))
     (multiple-value-list (notnot-mv (remprop sym :foo)))
     (copy-list (symbol-plist sym))
     (multiple-value-list (notnot-mv (remprop sym :foo)))
     (symbol-plist sym)))     
  nil
  (:bar 1 :foo 0 :baz 2)
  (t)
  (:bar 1 :baz 2)
  (nil)
  (:bar 1 :baz 2))

(deftest remprop.4
  (let ((sym (gensym)))
    (values
     (symbol-plist sym)
     (copy-list (setf (symbol-plist sym) (list :bar 1 :foo 0 :baz 2 :foo 3)))
     (multiple-value-list (notnot-mv (remprop sym :foo)))
     (copy-list (symbol-plist sym))
     (multiple-value-list (notnot-mv (remprop sym :foo)))
     (symbol-plist sym)))
  nil
  (:bar 1 :foo 0 :baz 2 :foo 3)
  (t)
  (:bar 1 :baz 2 :foo 3)
  (t)
  (:bar 1 :baz 2))

;;; Error tests

(deftest remprop.error.1
  (signals-error (remprop) program-error)
  t)

(deftest remprop.error.2
  (signals-error (remprop (gensym)) program-error)
  t)

(deftest remprop.error.3
  (signals-error (remprop (gensym) nil nil) program-error)
  t)

(deftest remprop.error.4
  (check-type-error #'(lambda (x) (remprop x nil)) #'symbolp)
  nil)
