;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May 18 20:47:58 2003
;;;; Contains: Tests of DISASSEMBLE

(in-package :cl-test)

(defun disassemble-it (fn)
  (let (val)
    (values
     (notnot
      (stringp
       (with-output-to-string (*standard-output*)
			      (setf val (disassemble fn)))))
     val)))

(deftest disassemble.1
  (disassemble-it 'car)
  t nil)

(deftest disassemble.2
  (disassemble-it (symbol-function 'car))
  t nil)

(deftest disassemble.3
  (disassemble-it '(lambda (x y) (cons y x)))
  t nil)

(deftest disassemble.4
  (disassemble-it (eval '(function (lambda (x y) (cons x y)))))
  t nil)

(deftest disassemble.5
  (disassemble-it
   (funcall (compile nil '(lambda () (let ((x 0)) #'(lambda () (incf x)))))))
  t nil)

(deftest disassemble.6
  (let ((name 'disassemble.fn.1))
    (fmakunbound name)
    (eval `(defun ,name (x) x))
    (disassemble-it name))
  t nil)

(deftest disassemble.7
  (let ((name 'disassemble.fn.2))
    (fmakunbound name)
    (eval `(defun ,name (x) x))
    (compile name)
    (disassemble-it name))
  t nil)

(deftest disassemble.8
  (progn
    (eval '(defun (setf disassemble-example-fn) (val arg)
	     (setf (car arg) val)))
    (disassemble-it '(setf disassemble-example-fn)))
  t nil)

(deftest disassemble.9
  (progn
    (eval '(defgeneric disassemble-example-fn2 (x y z)))
    (disassemble-it 'disassemble-example-fn2))
  t nil)

(deftest disassemble.10
  (progn
    (eval '(defgeneric disassemble-example-fn3 (x y z)))
    (eval '(defmethod disassemble-example-fn3 ((x t)(y t)(z t)) (list x y z)))
    (disassemble-it 'disassemble-example-fn3))
  t nil)

(deftest disassemble.11
  (let ((fn 'disassemble-example-fn4))
    (when (fboundp fn) (fmakunbound fn))
    (eval `(defun ,fn (x) x))
    (let ((is-compiled? (typep (symbol-function fn) 'compiled-function)))
      (multiple-value-call
       #'values
       (disassemble-it fn)
       (if is-compiled? (notnot (typep (symbol-function fn) 'compiled-function))
	 (not (typep (symbol-function fn) 'compiled-function))))))
  t nil t)

;;; Error tests

(deftest disassemble.error.1
  (signals-error (disassemble) program-error)
  t)

(deftest disassemble.error.2
  (signals-error (disassemble 'car nil) program-error)
  t)

(deftest disassemble.error.3
  (check-type-error #'disassemble
		    (typef '(or function symbol (cons (eql setf) (cons symbol null)))))
  nil)

