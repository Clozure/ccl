;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 10:00:50 2002
;;;; Contains: Tests for PROGV

(in-package :cl-test)

(deftest progv.1
  (progv () () t)
  t)

(deftest progv.2
  (progv '(x) '(1) (not (not (boundp 'x))))
  t)

(deftest progv.3
  (progv '(x) '(1) (symbol-value 'x))
  1)

(deftest progv.4
  (progv '(x) '(1)
    (locally (declare (special x))
	     x))
  1)

(deftest progv.5
  (let ((x 0))
    (progv '(x) '(1) x))
  0)

(deftest progv.6
  (let ((x 0))
    (declare (special x))
    (progv '(x) ()
      (boundp 'x)))
  nil)

(deftest progv.6a
  (let ((x 0))
    (declare (special x))
    (progv '(x) () (setq x 1))
    x)
  0)

(deftest progv.7
  (progv '(x y z) '(1 2 3)
    (locally (declare (special x y z))
	     (values x y z)))
  1 2 3)

(deftest progv.8
  (progv '(x y z) '(1 2 3 4 5 6 7 8)
    (locally (declare (special x y z))
	     (values x y z)))
  1 2 3)

(deftest progv.9
  (let ((x 0))
    (declare (special x))
    (progv '(x y z w) '(1)
      (values (not (not (boundp 'x)))
	      (boundp 'y)
	      (boundp 'z)
	      (boundp 'w))))
  t nil nil nil)

;; forms are evaluated in order

(deftest progv.10
  (let ((x 0) (y 0) (c 0))
    (progv
	(progn (setf x (incf c)) nil)
	(progn (setf y (incf c)) nil)
      (values x y c)))
  1 2 2)

;;; No tagbody

(deftest progv.11
  (block nil
    (tagbody
     (progv nil nil (go 10) 10 (return 'bad))
     10
     (return 'good)))
  good)

;;; Variables that are not bound don't have any type constraints

(deftest progv.12
  (progv '(x y) '(1)
    (locally (declare  (special x y) (type nil y))
	     (values
	      x
	      (boundp 'y))))
  1 nil)

;;; Macros are expanded in the appropriate environment

(deftest progv.13
  (macrolet
   ((%m (z) z))
   (progv (expand-in-current-env (%m '(x)))
	  '(:good)
	  (locally (declare (special x)) x)))
  :good)

(deftest progv.14
  (macrolet
   ((%m (z) z))
   (progv (list (expand-in-current-env (%m 'x)))
	  '(:good)
	  (locally (declare (special x)) x)))
  :good)

(deftest progv.15
  (macrolet
   ((%m (z) z))
   (progv '(x)
	  (expand-in-current-env (%m '(:good)))
	  (locally (declare (special x)) x)))
  :good)

(deftest progv.16
  (macrolet
   ((%m (z) z))
   (progv '(x)
	  (list (expand-in-current-env (%m :good)))
	  (locally (declare (special x)) x)))
  :good)

(deftest progv.17
  (macrolet
   ((%m (z) z))
   (progv nil nil (expand-in-current-env (%m :good))))
  :good)
