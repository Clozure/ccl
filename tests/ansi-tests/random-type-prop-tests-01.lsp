;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar  6 20:36:56 2005
;;;; Contains: Test that invoke the random type prop infrastructure, part 1

(in-package :cl-test)

(def-type-prop-test special-operator-p 'special-operator-p '(symbol) 1)
(def-type-prop-test type-of 'type-of '(t) 1)
(def-type-prop-test typep.1 '(lambda (x y) (typep x (type-of y))) '(t t) 2)
(def-type-prop-test typep.2 'typep
  (list t #'(lambda (x)
	      (let ((type (make-random-type-containing x)))
		`(eql ,type))))
  2)
(def-type-prop-test subtypep
  '(lambda (x y) (subtypep (type-of x) (type-of y))) '(t t) 2)
(def-type-prop-test fboundp.1 'fboundp '(symbol) 1)
(def-type-prop-test fboundp.2 'fboundp '((cons (eql setf) (cons symbol null))) 1)
(def-type-prop-test functionp 'functionp '(t) 1)
(def-type-prop-test compiled-function-p 'compiled-function-p '(t) 1)
(def-type-prop-test not 'not '(t) 1)
(def-type-prop-test eq 'eq (list
			    '(and t (not number) (not character))
			    #'(lambda (x) (rcase
					   (1 `(eql ,x))
					   (1 '(and t (not number) (not character))))))
  2)
(def-type-prop-test eql.1 'eql '(t t) 2)
(def-type-prop-test eql.2 'eql (list t #'(lambda (x) `(eql ,x))) 2)
(def-type-prop-test equal.1 'equal '(t t) 2)
(def-type-prop-test equal.2 'equal (list t #'(lambda (x) `(eql ,x))) 2)
(def-type-prop-test equalp.1 'equalp '(t t) 2)
(def-type-prop-test equalp.2 'equalp (list t #'(lambda (x) `(eql ,x))) 2)
(def-type-prop-test identity 'identity '(t) 1)
(def-type-prop-test complement
 '(lambda (f y) (funcall (complement f) y)) (list `(eql ,#'symbolp) t) 2)
(def-type-prop-test constantly
  '(lambda (x) (funcall (constantly x))) '(t) 1)
(def-type-prop-test and.1 'and '(t) 1)
(def-type-prop-test and.2 'and '((or null t) t) 2)
(def-type-prop-test and.3 'and '((or null t) (or null t) t) 3)
(def-type-prop-test if.1 'if '(boolean t) 2)
(def-type-prop-test if.2 'if '(boolean t t) 3)
(def-type-prop-test if.3 '(lambda (p q x y z) (if p (if q x y) z))
  '(boolean boolean t t t) 5)
(def-type-prop-test if.4 '(lambda (p q x y z) (if p x (if q y z)))
  '(boolean boolean t t t) 5)
(def-type-prop-test if.5 '(lambda (p q x y) (if (or p q) x y))
  '(boolean boolean t t) 4)
(def-type-prop-test if.6 '(lambda (p q x y) (if (and p q) x y))
  '(boolean boolean t t) 4)
(def-type-prop-test cond.1 '(lambda (p x y) (cond (p x) (t y))) '(boolean t t) 3)
(def-type-prop-test cond.2 '(lambda (p x y) (cond (p x) (t y))) '((or null t) t t) 3)
(def-type-prop-test or.1 'or '(t) 1)
(def-type-prop-test or.2 'or '((or null t) t) 2)
(def-type-prop-test or.3 'or '((or null null t) (or null t) t) 3)
(def-type-prop-test when 'when '((or null t) t) 2)
(def-type-prop-test unless 'unless '((or null t) t) 2)
(def-type-prop-test slot-exists-p 'slot-exists-p '(t symbol) 2)
(def-type-prop-test find-class 'find-class '(symbol null) 2)
(def-type-prop-test class-of 'class-of '(t) 1)
(def-type-prop-test find-restart 'find-restart '((and symbol (not null))) 1)
(def-type-prop-test symbolp 'symbolp '(t) 1)
(def-type-prop-test keywordp 'keywordp '(t) 1)
(def-type-prop-test make-symbol 'make-symbol '(string) 1
  :test #'(lambda (x y) (string= (symbol-name x) (symbol-name y))))
(def-type-prop-test symbol-name 'symbol-name '(symbol) 1)
(def-type-prop-test symbol-package 'symbol-package '(symbol) 1)
(def-type-prop-test boundp 'boundp '(symbol) 1)
(def-type-prop-test find-symbol 'find-symbol '(string) 1)
(def-type-prop-test find-package 'find-package '((or string symbol character)) 1)

