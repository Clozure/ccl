;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 28 13:43:00 2005
;;;; Contains: Tests of MACROEXPAND

(in-package :cl-test)

(deftest macroexpand.error.1
  (signals-error (macroexpand) program-error)
  t)

(deftest macroexpand.error.2
  (signals-error (macroexpand 'x nil nil) program-error)
  t)

;;; Non-error tests

(deftest macroexpand.1
  (check-predicate
   #'(lambda (x)
       (or (symbolp x) (consp x)
	   (let ((vals (multiple-value-list (macroexpand x))))
	     (and (= (length vals) 2)
		  (eql (car vals) x)
		  (null (cadr vals)))))))
  nil)

(deftest macroexpand.2
  (check-predicate
   #'(lambda (x)
       (or (symbolp x) (consp x)
	   (let ((vals (multiple-value-list (macroexpand x nil))))
	     (and (= (length vals) 2)
		  (eql (car vals) x)
		  (null (cadr vals)))))))
  nil)

(deftest macroexpand.3
  (macrolet
      ((%m (&environment env)
	   `(quote
	     ,(check-predicate
	       #'(lambda (x) (or (symbolp x) (consp x)
				 (let ((vals (multiple-value-list (macroexpand x env))))
				   (and (= (length vals) 2)
					(eql (car vals) x)
					(null (cadr vals))))))))))
    (%m))
  nil)

(deftest macroexpand.4
  (macrolet ((%m () ''foo))
    (macrolet ((%m2 (&environment env)
		    (macroexpand '(%m) env)))
      (%m2)))
  foo)

(deftest macroexpand.5
  (let ((form (list (gensym)))
	(i 0))
    (values
     (equalt (macroexpand (progn (incf i) form)) form)
     i))
  t 1)

(deftest macroexpand.6
  (let ((form (list (gensym)))
	(i 0) a b)
    (values
     (equalt (macroexpand (progn (setf a (incf i)) form)
			  (progn (setf b (incf i)) nil))
	     form)
     i a b))
  t 2 1 2)
