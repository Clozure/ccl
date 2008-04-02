;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  7 22:37:22 2002
;;;; Contains: Tests of FBOUNDP

(in-package :cl-test)

(deftest fboundp.1
  (not-mv (fboundp 'car))
  nil)

(deftest fboundp.2
  (not-mv (fboundp 'cdr))
  nil)

(deftest fboundp.3
  (not-mv (fboundp 'defun))  ; a macro
  nil)

(deftest fboundp.4
  ;; fresh symbols are not fbound
  (let ((g (gensym))) (fboundp g))
  nil)

(defun fboundp-5-fn (x) x)
(deftest fboundp.5
  (not-mv (fboundp 'fboundp-5-fn))
  nil)

(report-and-ignore-errors
 (defun (setf fboundp-6-accessor) (y x) (setf (car x) y)))

(deftest fboundp.6
  (not-mv (fboundp '(setf fboundp-6-accessor)))
  nil)

(deftest fboundp.7
  (let ((g (gensym))) (fboundp (list 'setf g)))
  nil)

;;; See 11.1.2.1.1
(deftest fboundp.8
  (loop for x in *cl-non-function-macro-special-operator-symbols*
	when (and (fboundp x) (not (eq x 'ed)))
	collect x)
  nil)

(deftest fboundp.order.1
  (let ((i 0))
    (values (notnot (fboundp (progn (incf i) 'car))) i))
  t 1)

(deftest fboundp.error.1
  (check-type-error #'fboundp #'(lambda (x) (typep x '(or symbol (cons (eql setf) (cons symbol null))))))
  nil)

(deftest fboundp.error.2
  (signals-type-error x '(x) (fboundp x))
  t)

(deftest fboundp.error.3
  (signals-type-error x '(setf) (fboundp x))
  t)

(deftest fboundp.error.4
  (signals-type-error x '(setf foo . bar) (fboundp x))
  t)

(deftest fboundp.error.5
  (signals-type-error x '(setf foo bar) (fboundp x))
  t)

(deftest fboundp.error.6
  (signals-error (fboundp) program-error)
  t)

(deftest fboundp.error.7
  (signals-error (fboundp 'cons nil) program-error)
  t)

(deftest fboundp.error.8
  (signals-error (locally (fboundp 1) t) type-error)
  t)

(deftest fboundp.error.9
  (signals-type-error x '(setf . foo) (fboundp x))
  t)

(deftest fboundp.error.10
  (loop for x in *mini-universe*
	unless (symbolp x)
	nconc
	(handler-case
	 (list x (fboundp `(setf ,x)))
	 (type-error (c)
		     (assert (not (typep (type-error-datum c)
					 (type-error-expected-type c))))
		     nil)
	 (error (c) (list (list x c)))))
  nil)

