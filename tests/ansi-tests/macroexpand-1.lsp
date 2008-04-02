;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 28 13:47:32 2005
;;;; Contains: Tests of MACROEXPAND-1

(in-package :cl-test)

(deftest macroexpand-1.error.1
  (signals-error (macroexpand-1) program-error)
  t)

(deftest macroexpand-1.error.2
  (signals-error (macroexpand-1 'x nil nil) program-error)
  t)

;;; Non-error tests

(deftest macroexpand-1.1
  (check-predicate
   #'(lambda (x)
       (or (symbolp x) (consp x)
	   (let ((vals (multiple-value-list (macroexpand-1 x))))
	     (and (= (length vals) 2)
		  (eql (car vals) x)
		  (null (cadr vals)))))))
  nil)

(deftest macroexpand-1.2
  (check-predicate
   #'(lambda (x)
       (or (symbolp x) (consp x)
	   (let ((vals (multiple-value-list (macroexpand-1 x nil))))
	     (and (= (length vals) 2)
		  (eql (car vals) x)
		  (null (cadr vals)))))))
  nil)

(deftest macroexpand-1.3
  (macrolet
      ((%m (&environment env)
	   `(quote
	     ,(check-predicate
	       #'(lambda (x)
		   (or (symbolp x) (consp x)
		       (let ((vals (multiple-value-list (macroexpand-1 x env))))
			 (and (= (length vals) 2)
			      (eql (car vals) x)
			      (null (cadr vals))))))))))
    (%m))
  nil)

(deftest macroexpand-1.4
  (macrolet ((%m () ''foo))
    (macrolet ((%m2 (&environment env)
		    (macroexpand-1 '(%m) env)))
      (%m2)))
  foo)

(deftest macroexpand-1.5
  (let ((form (list (gensym)))
	(i 0))
    (values
     (equalt (macroexpand-1 (progn (incf i) form)) form)
     i))
  t 1)

(deftest macroexpand-1.6
  (let ((form (list (gensym)))
	(i 0) a b)
    (values
     (equalt (macroexpand-1 (progn (setf a (incf i)) form)
			    (progn (setf b (incf i)) nil))
	     form)
     i a b))
  t 2 1 2)
