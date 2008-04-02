;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:56:04 2003
;;;; Contains: Tests of LIST, LIST*

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest list.1
  (list 'a 'b 'c)
  (a b c))

(deftest list.2
  (list)
  nil)

(deftest list.order.1
  (let ((i 0))
    (list (incf i) (incf i) (incf i) (incf i)))
  (1 2 3 4))

(deftest list.order.2
  (let ((i 0))
    (list (incf i) (incf i) (incf i) (incf i)
	  (incf i) (incf i) (incf i) (incf i)))
  (1 2 3 4 5 6 7 8))

(deftest list.order.3
  (let ((i 0))
    (list (incf i) (incf i) (incf i) (incf i)
	  (incf i) (incf i) (incf i) (incf i)
	  (incf i) (incf i) (incf i) (incf i)
	  (incf i) (incf i) (incf i) (incf i)))
  (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))

(def-fold-test list.fold.1 (list 'a))
(def-fold-test list.fold.2 (list 'a 'b))
(def-fold-test list.fold.3 (list 'a 'b 'c 'd 'e 'f))

;;; LIST* tests

(deftest list*.1
  (list* 1 2 3)
  (1 2 . 3))

(deftest list*.2
  (list* 'a)
  a)

(deftest list-list*.1
  (list* 'a 'b 'c (list 'd 'e 'f))
  (a b c d e f))

(deftest list*.3
  (list* 1)
  1)

(deftest list*.order.1
  (let ((i 0))
    (list* (incf i) (incf i) (incf i) (incf i)))
  (1 2 3 . 4))

(deftest list*.order.2
  (let ((i 0))
    (list* (incf i) (incf i) (incf i) (incf i)
	   (incf i) (incf i) (incf i) (incf i)
	   (incf i) (incf i) (incf i) (incf i)
	   (incf i) (incf i) (incf i) (incf i)))
  (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 . 16))

(def-fold-test list*.fold.1 (list* 'a 'b))
(def-fold-test list*.fold.2 (list* 'a 'b 'c))
(def-fold-test list*.fold.3 (list* 'a 'b 'c 'd 'e 'f))
