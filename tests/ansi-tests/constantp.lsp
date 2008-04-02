;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 13 19:12:17 2003
;;;; Contains: Tests for CONSTANTP

;;; See also defconstant.lsp

(in-package :cl-test)

;;; Error tests

(deftest constantp.error.1
  (signals-error (constantp) program-error)
  t)

(deftest constantp.error.2
  (signals-error (constantp nil nil nil) program-error)
  t)

;;; Non-error tests

(deftest constantp.1
  (check-predicate #'(lambda (e) (or (symbolp e) (consp e) (constantp e))))
  nil)

(deftest constantp.2
  (notnot-mv (constantp t))
  t)
  
(deftest constantp.3
  (notnot-mv (constantp nil))
  t)

(deftest constantp.4
  (notnot-mv (constantp :foo))
  t)

(deftest constantp.5
  (constantp (gensym))
  nil)

(defconstant constantp-test-symbol 1)

(defmacro constantp-macro (form &environment env)
  (notnot-mv (constantp form env)))

(deftest constantp.6
  (constantp-macro constantp-test-symbol)
  t)
  
(deftest constantp.7
  (constantp '(incf x))
  nil)

(deftest constantp.8
  (notnot-mv (constantp 1 nil))
  t)

(deftest constantp.9
  (notnot-mv (constantp ''(((foo)))))
  t)

(deftest constantp.10
  (notnot-mv (constantp 'pi))
  t)

(defmacro macro-for-constantp.11 (x) x)

(deftest constantp.11
  (macrolet ((macro-for-constantp.11 (y)
	       (declare (ignore y))
	       '*standard-input*))
    (macrolet ((%m (&environment env)
		 (if (constantp '(macro-for-constantp.11 0) env)
		     :bad
		     :good)))
      (%m)))
  :good)
    

(deftest constantp.order.1
  (let ((i 0))
    (values
     (notnot (constantp (progn (incf i) 1)))
     i))
  t 1)

(deftest constantp.order.2
  (let ((i 0) x y)
    (values
     (notnot (constantp (progn (setf x (incf i)) 1)
			(progn (setf y (incf i)) nil)))
     i x y))
  t 2 1 2)

  

