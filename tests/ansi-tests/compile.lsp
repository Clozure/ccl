;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 10 20:54:20 2002
;;;; Contains: Tests for COMPILE, COMPILED-FUNCTION-P, COMPILED-FUNCTION

(in-package :cl-test)

(deftest compile.1
  (progn
    (fmakunbound 'compile.1-fn)
    (values
     (eval '(defun compile.1-fn (x) x))
     (compiled-function-p 'compile.1-fn)
     (let ((x (compile 'compile.1-fn)))
       (or (eqt x 'compile.1-fn)
	   (notnot (compiled-function-p x))))
     (compiled-function-p 'compile.1-fn)
     (not (compiled-function-p #'compile.1-fn))
     (fmakunbound 'compile.1-fn)))
  compile.1-fn
  nil
  t
  nil
  nil
  compile.1-fn)


;;; COMPILE returns three values (function, warnings-p, failure-p)
(deftest compile.2
  (let* ((results (multiple-value-list
		   (compile nil '(lambda (x y) (cons y x)))))
	 (fn (car results)))
    (values (length results)
	    (funcall fn 'a 'b)
	    (second results)
	    (third results)))
  3
  (b . a)
  nil
  nil)

;;; Compile does not coalesce literal constants
(deftest compile.3
  (let ((x (list 'a 'b))
	(y (list 'a 'b)))
    (and (not (eqt x y))
	 (funcall (compile nil `(lambda () (eqt ',x ',y))))))
  nil)

(deftest compile.4
  (let ((x (copy-seq "abc"))
	(y (copy-seq "abc")))
    (and (not (eqt x y))
	 (funcall (compile nil `(lambda () (eqt ,x ,y))))))
  nil)

(deftest compile.5
  (let ((x (copy-seq "abc")))
    (funcall (compile nil `(lambda () (eqt ,x ,x)))))
  t)

(deftest compile.6
  (let ((x (copy-seq "abc")))
    (funcall (compile nil `(lambda () (eqt ',x ',x)))))
  t)

(deftest compile.7
  (let ((x (copy-seq "abc")))
    (eqt x (funcall (compile nil `(lambda () ,x)))))
  t)

(deftest compile.8
  (let ((x (list 'a 'b)))
    (eqt x (funcall (compile nil `(lambda () ',x)))))
  t)

(deftest compile.9
  (let ((i 0) a b)
    (values
     (funcall (compile (progn (setf a (incf i)) nil)
		       (progn (setf b (incf i)) '(lambda () 'z))))
     i a b))
  z 2 1 2)

;;; Error tests

(deftest compile.error.1
  (signals-error (compile) program-error)
  t)

(deftest compile.error.2
  (signals-error (compile nil '(lambda () nil) 'garbage)
		 program-error)
  t)
