;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan  8 06:02:47 2005
;;;; Contains: Tests of LOCALLY

(in-package :cl-test)

(deftest locally.1
  (locally)
  nil)

(deftest locally.2
  (locally (values)))

(deftest locally.3
  (locally (values 1 2 3 4))
  1 2 3 4)

(deftest locally.4
  (locally (declare) t)
  t)

(deftest locally.5
  (locally (declare) (declare) (declare) t)
  t)

(deftest locally.6
  (let ((x 'a))
    (declare (special x))
    (let ((x 'b))
      (values
       x
       (locally (declare (special x)) x)
       x)))
  b a b)

(deftest locally.7
  (locally (declare))
  nil)

;;; Macros are expanded in the appropriate environment

(deftest locally.8
  (macrolet ((%m (z) z))
	    (locally (expand-in-current-env (%m :good))))
  :good)
