;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:04:27 2003
;;;; Contains: Tests of MAKE-LIST

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest make-list-empty.1
  (make-list 0)
  nil)

(deftest make-list-empty.2
  (make-list 0 :initial-element 'a)
  nil)

(deftest make-list-no-initial-element
  (make-list 6)
  (nil nil nil nil nil nil))

(deftest make-list-with-initial-element
  (make-list 6 :initial-element 'a)
  (a a a a a a))

(deftest make-list.allow-other-keys.1
  (make-list 5 :allow-other-keys t :foo 'a)
  (nil nil nil nil nil))

(deftest make-list.allow-other-keys.2
  (make-list 5 :bar nil :allow-other-keys t)
  (nil nil nil nil nil))

(deftest make-list.allow-other-keys.3
  (make-list 5 :allow-other-keys nil)
  (nil nil nil nil nil))

(deftest make-list.allow-other-keys.4
  (make-list 5 :allow-other-keys t :allow-other-keys nil 'bad t)
  (nil nil nil nil nil))

(deftest make-list.allow-other-keys.5
  (make-list 5 :allow-other-keys t)
  (nil nil nil nil nil))

(deftest make-list-repeated-keyword
  (make-list 5 :initial-element 'a :initial-element 'b)
  (a a a a a))

(deftest make-list.order.1
  (let ((i 0) x y)
    (values
     (make-list (progn (setf x (incf i)) 5)
		:initial-element
		(progn (setf y (incf i)) 'a))
     i x y))
  (a a a a a)
  2 1 2)

(deftest make-list.order.2
  (let ((i 0) x y z)
    (values
     (make-list (progn (setf x (incf i)) 5)
		:initial-element
		(progn (setf y (incf i)) 'a)
		:initial-element
		(progn (setf z (incf i)) 'b))
     i x y z))
  (a a a a a)
  3 1 2 3)

(def-fold-test make-list.fold.1 (make-list 1))
(def-fold-test make-list.fold.2 (make-list 10 :initial-element 'x))

;;; Error tests

(deftest make-list.error.1
  (check-type-error #'make-list (typef 'unsigned-byte))
  nil)

(deftest make-list.error.3
  (signals-error (make-list) program-error)
  t)

(deftest make-list.error.4
  (signals-error (make-list 5 :bad t) program-error)
  t)

(deftest make-list.error.5
  (signals-error (make-list 5 :initial-element) program-error)
  t)

(deftest make-list.error.6
  (signals-error (make-list 5 1 2) program-error)
  t)

(deftest make-list.error.7
  (signals-error (make-list 5 :bad t :allow-other-keys nil)
		 program-error)
  t)

(deftest make-list.error.8
  (signals-error (locally (make-list 'a) t) type-error)
  t)
