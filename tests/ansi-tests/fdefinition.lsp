;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 13 15:27:51 2003
;;;; Contains: Tests for FDEFINITION

(in-package :cl-test)

;;; Error cases

(deftest fdefinition.error.1
  (signals-error (fdefinition) program-error)
  t)

(deftest fdefinition.error.2
  (signals-error (fdefinition 'cons nil) program-error)
  t)

(deftest fdefinition.error.3
  (let ((v (gensym)))
    (eval `(signals-error (fdefinition ',v) undefined-function
			  :name ,v)))
  t)

(deftest fdefinition.error.4
  (check-type-error #'fdefinition #'(lambda (x) (typep x '(or symbol (cons (eql setf) (cons symbol null))))))
  nil)

;;; (deftest fdefinition.error.5
;;;  (let ((fn `(setf ,(gensym))))
;;;    (eval `(signals-error (fdefinition ',fn) undefined-function
;;;			  :name ,fn)))
;;;  t)

(deftest fdefinition.error.6
  (signals-error (locally (fdefinition 10) t) type-error)
  t)

(deftest fdefinition.error.7
  (check-type-error #'fdefinition (constantly nil) '((setf) (setf . foo) (setf foo . bar) (setf foo bar)))
  nil)

(deftest fdefinition.error.8
  (loop for x in *mini-universe*
	unless (symbolp x)
	nconc
	(handler-case
	 (list x (fdefinition `(setf ,x)))
	 (type-error (c)
		     (assert (not (typep (type-error-datum c)
					 (type-error-expected-type c))))
		     nil)
	 (error (c) (list (list x c)))))
  nil)

;;; Non-error cases

(deftest fdefinition.1
  (let ((fun (fdefinition 'cons)))
    (funcall fun 'a 'b))
  (a . b))

(deftest fdefinition.2
  (progn
    (fdefinition 'cond)
    :good)
  :good)

(deftest fdefinition.3
  (progn
    (fdefinition 'setq)
    :good)
  :good)

(deftest fdefinition.4
  (let ((sym (gensym)))
    (values
     (fboundp sym)
     (progn
       (setf (fdefinition sym) (fdefinition 'cons))
       (funcall (symbol-function sym) 'a 'b))
     (notnot (fboundp sym))))
  nil
  (a . b)
  t)

(deftest fdefinition.5
  (let* ((sym (gensym))
	 (fname (list 'setf sym)))
    (values
     (fboundp fname)
     (progn
       (setf (fdefinition fname) (fdefinition 'cons))
       (eval `(setf (,sym 'a) 'b)))
     (notnot (fboundp fname))))
  nil
  (b . a)
  t)

(deftest fdefinition.order.1
  (let ((i 0))
    (fdefinition (progn (incf i) 'setq))
    i)
  1)

