;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Oct  9 21:45:07 2002
;;;; Contains: Tests of FUNCALL

(in-package :cl-test)

(deftest funcall.1
  (let ((fn #'cons))
    (funcall fn 'a 'b))
  (a . b))

(deftest funcall.2
  (funcall (symbol-function 'cons) 'a 'b)
  (a . b))

(deftest funcall.3
  (let ((fn 'cons))
    (funcall fn 'a 'b))
  (a . b))

(deftest funcall.4
  (funcall 'cons 'a 'b)
  (a . b))

(deftest funcall.5
  (let ((fn #'+))
    (funcall fn 1 2 3 4))
  10)

(deftest funcall.6
  (funcall #'(lambda (x y) (cons x y)) 'a 'b)
  (a . b))

(defun xcons (x y) (cons x y))

(deftest funcall.7
  (flet ((xcons (x y) (list y x)))
    (values (funcall 'xcons 1 2)
	    (funcall #'xcons 1 2)))
  (1 . 2)
  (2 1))

(deftest funcall.8
  (flet ((foo (x y z) (values x y z)))
    (funcall #'foo 1 2 3))
  1 2 3)

(deftest funcall.9
  (flet ((foo () (values)))
    (funcall #'foo))
  )

(deftest funcall.order.1
  (let ((i 0) a b)
    (values
     (funcall (progn (setf a (incf i)) #'car)
	      (progn (setf b (incf i)) '(x . y)))
     i a b))
  x 2 1 2)

(deftest funcall.order.2
  (let ((i 0) a b c)
    (values
     (funcall (progn (setf a (incf i)) #'cons)
	      (progn (setf b (incf i)) 'x)
	      (progn (setf c (incf i)) 'y))
     i a b c))
  (x . y) 3 1 2 3)


;;; FUNCALL should throw an UNDEFINED-FUNCTION condition when
;;; called on a symbol with a global definition as a special
;;; operator
(deftest funcall.error.1
  (signals-error (funcall 'quote 1) undefined-function :name quote)
  t)

(deftest funcall.error.2
  (signals-error (funcall 'progn 1) undefined-function :name progn)
  t)

;;; FUNCALL should throw an UNDEFINED-FUNCTION condition when
;;; called on a symbol with a global definition as a macro
(deftest funcall.error.3
  (signals-error (funcall 'defconstant '(defconstant x 10))
		 undefined-function
		 :name defconstant)
  t)

(deftest funcall.error.4
  (signals-error (funcall) program-error)
  t)

(deftest funcall.error.5
  (signals-error (funcall #'cons) program-error)
  t)

(deftest funcall.error.6
  (signals-error (funcall #'cons 1) program-error)
  t)

(deftest funcall.error.7
  (signals-type-error x 'a (funcall #'car x))
  t)
