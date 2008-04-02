;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:03:01 2003
;;;; Contains: Tests of LIST-LENGTH

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest list-length-nil
  (list-length nil)
  0)

(deftest list-length-list
  (list-length '(a b c d e f))
  6)

;; check that list-length returns nil
;; on a circular list

(deftest list-length-circular-list
  (let ((x (cons nil nil)))
    (let ((y (list* 1 2 3 4 5 6 7 8 9 x)))
      (setf (cdr x) y)
      (let ((z (list* 'a 'b 'c 'd 'e y)))
	(list-length z))))
  nil)

(deftest list-length.order.1
  (let ((i 0))
    (values (list-length (progn (incf i) '(a b c))) i))
  3 1)


(deftest list-length.4
  (list-length (copy-tree '(a b c)))
  3)

;; Check that list-length produces a type-error
;; on arguments that are not proper lists or circular lists

(deftest list-length.error.1
  (loop
   for x in (list 'a 1 1.0 #\w (make-array '(10))
		  '(a b . c) (symbol-package 'cons))
   count (not (eval `(signals-type-error x ',x (list-length x)))))
  0)

(deftest list-length.error.2
  (signals-error (list-length) program-error)
  t)

(deftest list-length.error.3
  (signals-error (list-length nil nil) program-error)
  t)

(deftest list-length.error.4
  (signals-error (list-length 'a) type-error)
  t)

(deftest list-length.error.5
  (signals-error (locally (list-length 'a) t) type-error)
  t)

(deftest list-length-symbol
  (signals-error (list-length 'a) type-error)
  t)

(deftest list-length-dotted-list
  (signals-error (list-length (copy-tree '(a b c d . e))) type-error)
  t)
