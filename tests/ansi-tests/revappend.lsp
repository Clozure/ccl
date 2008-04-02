;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:37:43 2003
;;;; Contains: Tests of REVAPPEND

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest revappend.1
    (let* ((x (list 'a 'b 'c))
	   (y (list 'd 'e 'f))
	   (xcopy (make-scaffold-copy x))
	   (ycopy (make-scaffold-copy y))
	   )
      (let ((result (revappend x y)))
	(and
	 (check-scaffold-copy x xcopy)
	 (check-scaffold-copy y ycopy)
	 (eqt (cdddr result) y)
	 result)))
  (c b a d e f))

(deftest revappend.2
    (revappend (copy-tree '(a b c d e)) 10)
  (e d c b a . 10))

(deftest revappend.3
    (revappend nil 'a)
  a)

(deftest revappend.4
    (revappend (copy-tree '(a (b c) d)) nil)
  (d (b c) a))

(deftest revappend.order.1
  (let ((i 0) x y)
    (values
     (revappend (progn (setf x (incf i)) (copy-list '(a b c)))
		(progn (setf y (incf i)) (copy-list '(d e f))))
     i x y))
  (c b a d e f) 2 1 2)

(def-fold-test revappend.fold.1 (revappend '(x) nil))
(def-fold-test revappend.fold.2 (revappend '(x y z) nil))

;;; Error tests

(deftest revappend.error.1
  (signals-error (revappend) program-error)
  t)

(deftest revappend.error.2
  (signals-error (revappend nil) program-error)
  t)

(deftest revappend.error.3
  (signals-error (revappend nil nil nil) program-error)
  t)

(deftest revappend.error.4
  (signals-error (revappend '(a . b) '(z)) type-error)
  t)
