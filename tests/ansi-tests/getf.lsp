;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:37:41 2003
;;;; Contains: Tests of GETF

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest getf.1
  (getf nil 'a)
  nil)

(deftest getf.2
  (getf nil 'a 'b)
  b)

(deftest getf.3
  (getf '(a b) 'a)
  b)

(deftest getf.4
  (getf '(a b) 'a 'c)
  b)

(deftest getf.5
  (let ((x 0))
    (values
     (getf '(a b) 'a (incf x))
     x))
  b 1)

(deftest getf.order.1
  (let ((i 0) x y)
    (values
     (getf (progn (setf x (incf i)) '(a b))
	   (progn (setf y (incf i)) 'a))
     i x y))
  b 2 1 2)
		  
(deftest getf.order.2
  (let ((i 0) x y z)
    (values
     (getf (progn (setf x (incf i)) '(a b))
	   (progn (setf y (incf i)) 'a)
	   (setf z (incf i)))
     i x y z))
  b 3 1 2 3)

(deftest setf-getf.1
  (let ((p (copy-list '(a 1 b 2))))
    (setf (getf p 'c) 3)
    ;; Must check that only a, b, c have properties
    (and
     (eqlt (getf p 'a) 1)
     (eqlt (getf p 'b) 2)
     (eqlt (getf p 'c) 3)
     (eqlt
      (loop
       for ptr on p by #'cddr count
       (not (member (car ptr) '(a b c))))
      0)
     t))
  t)

(deftest setf-getf.2
  (let ((p (copy-list '(a 1 b 2))))
    (setf (getf p 'a) 3)
    ;; Must check that only a, b have properties
    (and
     (eqlt (getf p 'a) 3)
     (eqlt (getf p 'b) 2)
     (eqlt
      (loop
       for ptr on p by #'cddr count
       (not (member (car ptr) '(a b))))
      0)
     t))
  t)    

(deftest setf-getf.3
  (let ((p (copy-list '(a 1 b 2))))
    (setf (getf p 'c 17) 3)
    ;; Must check that only a, b, c have properties
    (and
     (eqlt (getf p 'a) 1)
     (eqlt (getf p 'b) 2)
     (eqlt (getf p 'c) 3)
     (eqlt
      (loop
       for ptr on p by #'cddr count
       (not (member (car ptr) '(a b c))))
      0)
     t))
  t)

(deftest setf-getf.4
  (let ((p (copy-list '(a 1 b 2))))
    (setf (getf p 'a 17) 3)
    ;; Must check that only a, b have properties
    (and
     (eqlt (getf p 'a) 3)
     (eqlt (getf p 'b) 2)
     (eqlt
      (loop
       for ptr on p by #'cddr count
       (not (member (car ptr) '(a b))))
      0)
     t))
  t)

(deftest setf-getf.5
  (let ((p (copy-list '(a 1 b 2)))
	(foo nil))
    (setf (getf p 'a (progn (setf foo t) 0)) 3)
    ;; Must check that only a, b have properties
    (and
     (eqlt (getf p 'a) 3)
     (eqlt (getf p 'b) 2)
     (eqlt
      (loop
       for ptr on p by #'cddr count
       (not (member (car ptr) '(a b))))
      0)
     foo))
  t)

(deftest setf-getf.order.1
  (let ((p (list (copy-list '(a 1 b 2))))
        (cnt1 0) (cnt2 0) (cnt3 0))
    (setf (getf (car (progn (incf cnt1) p)) 'c (incf cnt3))
          (progn (incf cnt2) 3))
    ;; Must check that only a, b, c have properties
    (values
     cnt1 ; (eqlt cnt1 1)
     cnt2 ; (eqlt cnt2 1)
     cnt3 ; (eqlt cnt3 1)
     (getf (car p) 'a)
     (getf (car p) 'b)
     (getf (car p) 'c)
     (loop
        for ptr on (car p) by #'cddr count
          (not (member (car ptr) '(a b c))))))
  1 1 1
  1 2 3
  0)

(deftest setf-getf.order.2
  (let ((p (list (copy-list '(a 1 b 2))))
	(i 0) x y z w)
    (setf (getf (car (progn (setf x (incf i)) p))
		(progn (setf y (incf i)) 'c)
		(setf z (incf i)))
	  (progn (setf w (incf i)) 3))
    ;; Must check that only a, b, c have properties
    (values
     i x y z w
     (getf (car p) 'a)
     (getf (car p) 'b)
     (getf (car p) 'c)
     (loop for ptr on (car p) by #'cddr count
	  (not (member (car ptr) '(a b c))))))
  4 1 2 3 4 1 2 3 0)

(deftest incf-getf.1
  (let ((p (copy-list '(a 1 b 2))))
    (incf (getf p 'b))
    ;; Must check that only a, b have properties
    (and
     (eqlt (getf p 'a) 1)
     (eqlt (getf p 'b) 3)
     (eqlt
      (loop
       for ptr on p by #'cddr count
       (not (member (car ptr) '(a b))))
      0)
     t))
  t)

(deftest incf-getf.2
  (let ((p (copy-list '(a 1 b 2))))
    (incf (getf p 'c 19))
    ;; Must check that only a, b have properties
    (and
     (eqlt (getf p 'a) 1)
     (eqlt (getf p 'b) 2)
     (eqlt (getf p 'c) 20)
     (eqlt
	(loop
	 for ptr on p by #'cddr count
	 (not (member (car ptr) '(a b c))))
	0)
     t))
  t)

(deftest push-getf.1
  (let ((p nil))
    (values
     (push 'x (getf p 'a))
     p))
  (x) (a (x)))

;;; Error tests

(deftest getf.error.1
  (signals-error (getf) program-error)
  t)

(deftest getf.error.2
  (signals-error (getf nil) program-error)
  t)

(deftest getf.error.3
  (signals-error (getf nil nil nil nil) program-error)
  t)

(deftest getf.error.4
  (signals-error (getf '(a . b) 'c) type-error)
  t)

(deftest getf.error.5
  (signals-error (getf '(a 10 . b) 'c) type-error)
  t)
