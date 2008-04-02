;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan  8 05:58:53 2005
;;;; Contains: Tests of SYMBOL-MACROLET

(in-package :cl-test)

(deftest symbol-macrolet.1
  (loop for s in *cl-non-variable-constant-symbols*
	for form = `(ignore-errors (symbol-macrolet ((,s 17)) ,s))
	unless (eql (eval form) 17)
	collect s)
  nil)

(deftest symbol-macrolet.2
  (symbol-macrolet ())
  nil)

(deftest symbol-macrolet.3
  (symbol-macrolet () (declare (optimize)))
  nil)

(deftest symbol-macrolet.4
  (symbol-macrolet ((x 1))
    (symbol-macrolet ((x 2))
      x))
  2)

(deftest symbol-macrolet.5
  (let ((x 10))
     (symbol-macrolet ((y x))
       (list x
	     y
	     (let ((x 20)) x)
	     (let ((y 30)) x)
	     (let ((y 50)) y)
	     x
	     y)))
  (10 10 20 10 50 10 10))

(deftest symbol-macrolet.6
  (symbol-macrolet () (values)))
		       
(deftest symbol-macrolet.7
  (symbol-macrolet () (values 'a 'b 'c 'd 'e))
  a b c d e)

(deftest symbol-macrolet.8
  (let ((x :good))
    (declare (special x))
    (let ((x :bad))
      (symbol-macrolet () (declare (special x)) x)))
  :good)

;;; Error tests		       

(deftest symbol-macrolet.error.1
  (signals-error
   (symbol-macrolet ((x 10))
     (declare (special x))
     20)
   program-error)
  t)

(defconstant constant-for-symbol-macrolet.error.2 nil)

(deftest symbol-macrolet.error.2
  (signals-error (symbol-macrolet ((constant-for-symbol-macrolet.error.2 'a))
		   constant-for-symbol-macrolet.error.2)
		 program-error)
  t)

(deftest symbol-macrolet.error.3
  (signals-error (symbol-macrolet ((*pathnames* 19)) *pathnames*)
		 program-error)
  t)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest symbol-macrolet.9
  (macrolet
   ((%m (z) z))
   (symbol-macrolet () (expand-in-current-env (%m :good))))
  :good)
