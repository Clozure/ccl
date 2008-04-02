;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Contains: Tests of the function ARRAY-HAS-FILL-POINTER-P

(in-package :cl-test)

;;; Many tests are in other files, incidental to testing of
;;; other things

(deftest array-has-fill-pointer-p.1
  (array-has-fill-pointer-p #0a1)
  nil)

(deftest array-has-fill-pointer-p.2
  (array-has-fill-pointer-p #2a((a b)(c d)))
  nil)

(deftest array-has-fill-pointer-p.3
  (array-has-fill-pointer-p #3a(((a))))
  nil)

(deftest array-has-fill-pointer-p.4
  (array-has-fill-pointer-p #4a((((a)))))
  nil)

(deftest array-has-fill-pointer-p.5
  (macrolet
   ((%m (z) z))
   (array-has-fill-pointer-p (expand-in-current-env (%m #2a((a b)(c d))))))
  nil)

(deftest array-has-fill-pointer-p.order.1
  (let ((i 0))
    (array-has-fill-pointer-p (progn (incf i) #(a b c)))
    i)
  1)

;;; Error tests

(deftest array-has-fill-pointer-p.error.1
  (signals-error (array-has-fill-pointer-p) program-error)
  t)

(deftest array-has-fill-pointer-p.error.2
  (signals-error (array-has-fill-pointer-p #(a b c) nil) program-error)
  t)

(deftest array-has-fill-pointer-p.error.3
  (check-type-error #'array-has-fill-pointer-p #'arrayp)
  nil)

(deftest array-has-fill-pointer-p.error.4
  (signals-type-error x nil (array-has-fill-pointer-p x))
  t)


