;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jun 24 20:53:36 2005
;;;; Contains: Tests for LET*

(in-package :cl-test)

(deftest let*.1
  (let* ((x 0)) x)
  0)

(deftest let*.2
  (let* ((x 0) (y 1)) (values x y))
  0 1)

(deftest let*.3
  (let* ((x 0) (y 1)) (declare (special x y)) (values x y))
  0 1)

(deftest let*.4
  (let* ((x 0))
    (let* ((x 1))
      x))
  1)

(deftest let*.5
  (let* ((x 0))
    (let* ((#:x 1))
      x))
  0)

(deftest let*.6
  (let* ((x 0))
    (declare (special x))
    (let* ((x 1))
      (values x (locally (declare (special x)) x))))
  1 0)

(deftest let*.7
  (let* ((x '(a b c)))
    (declare (dynamic-extent x))
    x)
  (a b c))

(deftest let*.8
  (let* ((x 0) (x 1)) x)
  1)

(deftest let*.9
  (let* (x y z) (values x y z))
  nil nil nil)

(deftest let*.10
  (let* ((x 1) x) x)
  nil)

(deftest let*.11
  (let* ((x 1))
    (list x
	  (let* (x x x)
	    (declare (special x))
	    x)
	  x))
  (1 nil 1))

(deftest let*.12
  (let* ((x 1)
	 (y (1+ x))
	 (x (1+ y))
	 (z (+ x y)))
    (values x y z))
  3 2 5)

;;; (deftest let*.13
;;;  (flet ((%f () (declare (special x)) x))
;;;    (let* ((x 1)
;;;	   (x (1+ (%f))))
;;;      (declare (special x))
;;;      x))
;;;  2)

;;; Tests of large number of LET* variables
(deftest let*.14
  (let* ((n 100)
	 (vars (mapcar #'gensym (make-list n :initial-element "G")))
	 (expr `(let* ,(let ((i 0))
			 (mapcar #'(lambda (v) (list v (incf i))) vars))
		  ,(let ((sumexpr 0))
		     (dolist (v vars)
		       (setq sumexpr `(+ ,v ,sumexpr)))
		     sumexpr)))
	 (val (eval expr)))
    (or (eqlt val (/ (* n (1+ n)) 2)) (list val)))
  t)

;;; Test that all non-variables exported from COMMON-LISP can be bound
;;; in LET* forms.
(deftest let*.15
  (loop for s in *cl-non-variable-constant-symbols*
	for form = `(ignore-errors (let* ((,s 17)) ,s))
	unless (eql (eval form) 17)
	collect s)
  nil)

;;; Check that LET* does not have a tagbody
(deftest let*.16
  (block done
    (tagbody
     (let () (go 10) 10 (return-from done 'bad))
     10
     (return-from done 'good)))
  good)

;;; Check that free declarations do not apply to the init forms

(deftest let*.17
  (let ((x :bad))
    (declare (special x))
    (let ((x :good)) ;; lexical binding
      (let* ((y x))
	(declare (special x)) ;; free declaration
	y)))
  :good)

(deftest let*.17a
  (funcall
   (compile
    nil
    '(lambda ()
       (let ((x :bad))
	 (declare (special x))
	 (let ((x :good)) ;; lexical binding
	   (let* ((y x))
	     (declare (special x)) ;; free declaration
	     y))))))
  :good)

(deftest let*.18
  (let ((x :bad1)
	(z :bad2))
    (declare (special x z))
    (let ((x :good)
	  (z :good)) ;; lexical bindings
      (let* ((y x)
	     (w z))
	(declare (special x)) ;; free declaration
	(values y w))))
  :good
  :good)

(deftest let*.19
  (let ((foo 'special))
    (declare (special foo))
    (let* ((foo 'lexical))
      (locally (declare (special foo)))
      foo))
  lexical)

(deftest let*.20
  (loop for k in lambda-list-keywords
	unless (eql (eval `(let* ((,k :foo)) ,k)) :foo)
	collect k)
  nil)

;;; Macros are expanded in the appropriate environment

(deftest let*.21
  (macrolet ((%m (z) z))
	    (let* () (expand-in-current-env (%m :good))))
  :good)

(deftest let*.22
  (macrolet ((%m (z) z))
	    (let* ((x (expand-in-current-env (%m 1)))) (+ x x x)))
  3)

(deftest let*.23
  (macrolet ((%m (z) z))
	    (let* ((x (expand-in-current-env (%m 1)))
		   (y (expand-in-current-env (%m 2))))
	      (+ x y)))
  3)



