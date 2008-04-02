;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  7 19:20:17 2002
;;;; Contains: Tests of various kinds of places (section 5.1)

(in-package :cl-test)

;;; Section 5.1.1.1

(deftest setf.order.1
  (let ((x (vector nil nil nil nil))
	(i 0))
    (setf (aref x (incf i)) (incf i))
    (values x i))
  #(nil 2 nil nil) 2)

(deftest setf.order.2
  (let ((x (vector nil nil nil nil))
	(i 0))
    (setf (aref x (incf i)) (incf i)
	  (aref x (incf i)) (incf i 10))
    (values x i))
  #(nil 2 nil 13) 13)

(deftest incf.order.1
  (let ((x (copy-seq #(0 0 0 0 0)))
	(i 1))
    (values
     (incf (aref x (incf i)) (incf i))
     x i))
  3 #(0 0 3 0 0) 3)

(deftest decf.order.1
  (let ((x (copy-seq #(0 0 0 0 0)))
	(i 1))
    (values
     (decf (aref x (incf i)) (incf i))
     x i))
  -3 #(0 0 -3 0 0) 3)

   
;;; Section 5.1.2.1
(deftest setf-var
  (let ((x nil))
    (setf x 'a)
    x)
  a)

;;; Section 5.1.2.2
;;; See SETF forms at various accessor functions

;;; Section 5.1.2.3
(deftest setf-values.1
  (let ((x nil) (y nil) (z nil))
    (setf (values x y z) (values 1 2 3)))
  1 2 3)

(deftest setf-values.2
  (let ((x nil) (y nil) (z nil))
    (setf (values x y z) (values 1 2 3))
    (values z y x))
  3 2 1)

(deftest setf-values.3
  (let ((x nil) (y nil) (z nil))
    (setf (values x x x) (values 1 2 3))
    x)
  3)

;;; Test that the subplaces of a VALUES place can be
;;; complex, and that the various places' subforms are
;;; evaluated in the correct (left-to-right) order.

(deftest setf-values.4
  (let ((x (list 'a 'b)))
    (setf (values (car x) (cadr x)) (values 1 2))
    x)
  (1 2))

(deftest setf-values.5
  (let ((a (vector nil nil))
	(i 0)
	x y z)
    (setf (values (aref a (progn (setf x (incf i)) 0))
		  (aref a (progn (setf y (incf i)) 1)))
	  (progn
	    (setf z (incf i))
	    (values 'foo 'bar)))
    (values a i x y z))
  #(foo bar) 3 1 2 3)

(deftest setf-values.6
  (setf (values) (values)))

;;; Section 5.1.2.4
(deftest setf-the.1
  (let ((x 1))
    (setf (the integer x) 2)
    x)
  2)

(deftest setf-the.2
  (let ((x (list 'a)))
    (values
     (setf (the symbol (car x)) 'b)
     x))
  b (b))

;;; Section 5.1.2.5
(deftest setf-apply.1
  (let ((x (vector 0 1 2 3 4 5)))
    (setf (apply #'aref x '(0)) 10)
    x)
  #(10 1 2 3 4 5))

(deftest setf-apply.2
  (let ((a (make-array '(2 2) :initial-contents '((0 0)(0 0)))))
    (setf (apply #'aref a 1 1 nil) 'a)
    (equalp a (make-array '(2 2) :initial-contents '((0 0)(0 a)))))
  t)

(deftest setf-apply.3
  (let ((bv (copy-seq #*0000000000)))
    (setf (apply #'bit bv 4 nil) 1)
    bv)
  #*0000100000)

(deftest setf-apply.4
  (let ((bv (copy-seq #*0000000000)))
    (setf (apply #'sbit bv 4 nil) 1)
    bv)
  #*0000100000)

;;; Section 5.1.2.6
(defun accessor-5-1-2-6-update-fn (x y)
  (setf (car x) y)
  y)

(defsetf accessor-5-1-2-6 accessor-5-1-2-6-update-fn)

(deftest setf-expander.1
  (let ((x (list 1)))
    (values (setf (accessor-5-1-2-6 x) 2)
	    (1+ (car x))))
  2 3)

;;; Section 5.1.2.7

(defmacro accessor-5-1-2-7 (x) `(car ,x))
(deftest setf-macro.1
  (let ((x (list 1)))
    (values (setf (accessor-5-1-2-7 x) 2)
	    (1+ (car x))))
  2 3)

(defun accessor-5-1-2-7a-update-fn (x y)
  (declare (special *x*))
  (setf (car x) y)
  (setf *x* 'boo)
  y)

(defmacro accessor-5-1-2-7a (x) `(car ,x))
(defsetf accessor-5-1-2-7a accessor-5-1-2-7a-update-fn)
;; Test that the defsetf override the macro expansion
(deftest setf-macro.2
  (let ((x (list 1))
	(*x* nil))
     (declare (special *x*))
    (values (setf (accessor-5-1-2-7a x) 2)
	    *x*
	    (1+ (car x))))
  2 boo 3)

(defmacro accessor-5-1-2-7b (x) `(accessor-5-1-2-7 ,x))
;; Test that the macroexpansion occurs more than once
(deftest setf-macro.3
  (let ((x (list 1)))
    (values (setf (accessor-5-1-2-7b x) 2)
	    (1+ (car x))))
  2 3)

;; Macroexpansion from a macrolet
(deftest setf-macro.4
  (macrolet ((%m (y) `(car ,y)))
    (let ((x (list 1)))
      (values (setf (%m x) 2)
	      (1+ (car x)))))
  2 3)

;;; section 5.1.2.8 -- symbol macros
(deftest setf-symbol-macro.1
  (symbol-macrolet ((x y))
    (let ((y nil))
      (values (setf x 1) x y)))
  1 1 1)

;;; Symbol macros in SETQs are treated as if the form were a SETF
(deftest setf-symbol-macro.2
  (symbol-macrolet ((x y))
    (let ((y nil))
      (values (setq x 1) x y)))
  1 1 1)

;;; Tests that, being treated like SETF, this causes multiple values
;;; to be assigned to (values y z)
(deftest setf-symbol-macro.3
  (symbol-macrolet ((x (values y z)))
    (let ((y nil) (z nil))
      (values (setq x (values 1 2)) x y z)))
  1 1 1 2)

(deftest setq.1
  (setq)
  nil)

(deftest setq.2
  (let ((x 0) (y 0))
    (values (setq x 1 y 2) x y))
  2 1 2)

(deftest setq.3
  (let ((x 0) (y 0))
    (values (setq x (values 1 3) y (values 2 4)) x y))
  2 1 2)

(deftest setq.4
  (let (x) (setq x (values 1 2)))
  1)

(deftest setq.5
  (let ((*x* 0))
    (declare (special *x*))
    (values *x* (setq *x* 1) *x*))
  0 1 1)

(deftest setq.6
  (let ((*x* 0))
    (declare (special *x*))
    (setq *x* 1))
  1)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest setq.7
  (macrolet
   ((%m (z) z))
   (let ((x nil))
     (values (setq x (expand-in-current-env (%m :good)))
	     x)))
  :good :good)

;;; Tests of SETF    

(deftest setf.1
  (setf)
  nil)

(deftest setf.2
  (let ((x 0) (y 0))
    (values (setf x 1 y 2) x y))
  2 1 2)

(deftest setf.3
  (let ((x 0) (y 0))
    (values (setf x (values 1 3) y (values 2 4)) x y))
  2 1 2)

(deftest setf.4
  (let (x) (setf x (values 1 2)))
  1)

(deftest setf.5
  (let ((*x* 0))
    (declare (special *x*))
    (values *x* (setf *x* 1) *x*))
  0 1 1)

(deftest setf.6
  (let ((*x* 0))
    (declare (special *x*))
    (setf *x* 1))
  1)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest setf.7
  (macrolet
   ((%m (z) z))
   (let ((x nil))
     (values x (setf (expand-in-current-env (%m x)) t) x)))
  nil t t)

(deftest setf.8
  (macrolet
   ((%m (z) z))
   (let ((x nil))
     (values x (setf x (expand-in-current-env (%m t))) x)))
  nil t t)
