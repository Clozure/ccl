;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:28:38 2003
;;;; Contains: Tests of C*R functions

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

;; Tests of car, cdr and compound forms
(deftest cons.23
  (car '(a))
  a)

(deftest cons.24
  (cdr '(a . b))
  b)

(deftest cons.25
  (caar '((a)))
  a)

(deftest cons.26
  (cdar '((a . b)))
  b)

(deftest cons.27
  (cadr '(a b))
  b)

(deftest cons.28
  (cddr '(a b . c))
  c)

(deftest cons.29
  (caaar '(((a))))
  a)

(deftest cons.30
  (cdaar '(((a . b))))
  b)

(deftest cons.31
  (cadar (cons (cons 'a (cons 'b 'c)) 'd))
  b)

(deftest cons.32
  (cddar (cons (cons 'a (cons 'b 'c)) 'd))
  c)

(deftest cons.33
  (caadr (cons 'a (cons (cons 'b 'c) 'd)))
  b)

(deftest cons.34
  (caddr (cons 'a (cons 'b (cons 'c 'd))))
  c)

(deftest cons.36
  (cdadr (cons 'a (cons (cons 'b 'c) 'd)))
  c)

(deftest cons.37
  (cdddr (cons 'a (cons 'b (cons 'c 'd))))
  d)

(defvar *cons-test-4*
  (cons (cons (cons (cons 'a 'b)
		    (cons 'c 'd))
	      (cons (cons 'e 'f)
		    (cons 'g 'h)))
	(cons (cons (cons 'i 'j)
		    (cons 'k 'l))
	      (cons (cons 'm 'n)
		    (cons 'o 'p)))))

(deftest cons.38
  (caaaar *cons-test-4*)
  a)

(deftest cons.39
  (cdaaar *cons-test-4*)
  b)

(deftest cons.40
  (cadaar *cons-test-4*)
  c)

(deftest cons.41
  (cddaar *cons-test-4*)
  d)

(deftest cons.42
  (caadar *cons-test-4*)
  e)

(deftest cons.43
  (cdadar *cons-test-4*)
  f)

(deftest cons.44
  (caddar *cons-test-4*)
  g)

(deftest cons.45
  (cdddar *cons-test-4*)
  h)

;;;

(deftest cons.46
  (caaadr *cons-test-4*)
  i)

(deftest cons.47
  (cdaadr *cons-test-4*)
  j)

(deftest cons.48
  (cadadr *cons-test-4*)
  k)

(deftest cons.49
  (cddadr *cons-test-4*)
  l)

(deftest cons.50
  (caaddr *cons-test-4*)
  m)

(deftest cons.51
  (cdaddr *cons-test-4*)
  n)

(deftest cons.52
  (cadddr *cons-test-4*)
  o)

(deftest cons.53
  (cddddr *cons-test-4*)
  p)

(deftest car.1
  (car '(a))
  a)

(deftest car-nil
  (car nil)
  nil)

(deftest car.error.1
  (check-type-error #'car #'listp)
  nil)

(deftest car.error.2
  (signals-error (locally (car 'a) t) type-error)
  t)

(deftest car.order.1
  (let ((i 0))
    (values (car (progn (incf i) '(a b))) i))
  a 1)

(deftest cdr.1
  (cdr '(a b))
  (b))

(deftest cdr-nil
  (cdr ())
  nil)

(deftest cdr.order.1
  (let ((i 0))
    (values (cdr (progn (incf i) '(a b))) i))
  (b) 1)

(deftest cdr.error.1
  (check-type-error #'cdr #'listp)
  nil)

(deftest cdr.error.2
  (signals-error (locally (cdr 'a) t) type-error)
  t)

;;; Error checking of c*r functions

(deftest caar.error.1
  (signals-error (caar 'a) type-error)
  t)

(deftest caar.error.2
  (signals-error (caar '(a)) type-error)
  t)

(deftest cadr.error.1
  (signals-error (cadr 'a) type-error)
  t)

(deftest cadr.error.2
  (signals-error (cadr '(a . b)) type-error)
  t)

(deftest cdar.error.1
  (signals-error (cdar 'a) type-error)
  t)

(deftest cdar.error.2
  (signals-error (cdar '(a . b)) type-error)
  t)

(deftest cddr.error.1
  (signals-error (cddr 'a) type-error)
  t)

(deftest cddr.error.2
  (signals-error (cddr '(a . b)) type-error)
  t)

(deftest caaar.error.1
  (signals-error (caaar 'a) type-error)
  t)

(deftest caaar.error.2
  (signals-error (caaar '(a)) type-error)
  t)

(deftest caaar.error.3
  (signals-error (caaar '((a))) type-error)
  t)

(deftest caadr.error.1
  (signals-error (caadr 'a) type-error)
  t)

(deftest caadr.error.2
  (signals-error (caadr '(a . b)) type-error)
  t)

(deftest caadr.error.3
  (signals-error (caadr '(a . (b))) type-error)
  t)

(deftest cadar.error.1
  (signals-error (cadar 'a) type-error)
  t)

(deftest cadar.error.2
  (signals-error (cadar '(a . b)) type-error)
  t)

(deftest cadar.error.3
  (signals-error (cadar '((a . c) . b)) type-error)
  t)

(deftest caddr.error.1
  (signals-error (caddr 'a) type-error)
  t)

(deftest caddr.error.2
  (signals-error (caddr '(a . b)) type-error)
  t)

(deftest caddr.error.3
  (signals-error (caddr '(a c . b)) type-error)
  t)

(deftest cdaar.error.1
  (signals-error (cdaar 'a) type-error)
  t)

(deftest cdaar.error.2
  (signals-error (cdaar '(a)) type-error)
  t)

(deftest cdaar.error.3
  (signals-error (cdaar '((a . b))) type-error)
  t)

(deftest cdadr.error.1
  (signals-error (cdadr 'a) type-error)
  t)

(deftest cdadr.error.2
  (signals-error (cdadr '(a . b)) type-error)
  t)

(deftest cdadr.error.3
  (signals-error (cdadr '(a b . c)) type-error)
  t)

(deftest cddar.error.1
  (signals-error (cddar 'a) type-error)
  t)

(deftest cddar.error.2
  (signals-error (cddar '(a . b)) type-error)
  t)

(deftest cddar.error.3
  (signals-error (cddar '((a . b) . b)) type-error)
  t)

(deftest cdddr.error.1
  (signals-error (cdddr 'a) type-error)
  t)

(deftest cdddr.error.2
  (signals-error (cdddr '(a . b)) type-error)
  t)

(deftest cdddr.error.3
  (signals-error (cdddr '(a c . b)) type-error)
  t)

;;

(deftest caaaar.error.1
  (signals-error (caaaar 'a) type-error)
  t)

(deftest caaaar.error.2
  (signals-error (caaaar '(a)) type-error)
  t)

(deftest caaaar.error.3
  (signals-error (caaaar '((a))) type-error)
  t)

(deftest caaaar.error.4
  (signals-error (caaaar '(((a)))) type-error)
  t)

(deftest caaadr.error.1
  (signals-error (caaadr 'a) type-error)
  t)

(deftest caaadr.error.2
  (signals-error (caaadr '(a . b)) type-error)
  t)

(deftest caaadr.error.3
  (signals-error (caaadr '(a . (b))) type-error)
  t)

(deftest caaadr.error.4
  (signals-error (caaadr '(a . ((b)))) type-error)
  t)

(deftest caadar.error.1
  (signals-error (caadar 'a) type-error)
  t)

(deftest caadar.error.2
  (signals-error (caadar '(a . b)) type-error)
  t)

(deftest caadar.error.3
  (signals-error (caadar '((a . c) . b)) type-error)
  t)

(deftest caadar.error.4
  (signals-error (caadar '((a . (c)) . b)) type-error)
  t)

(deftest caaddr.error.1
  (signals-error (caaddr 'a) type-error)
  t)

(deftest caaddr.error.2
  (signals-error (caaddr '(a . b)) type-error)
  t)

(deftest caaddr.error.3
  (signals-error (caaddr '(a c . b)) type-error)
  t)

(deftest caaddr.error.4
  (signals-error (caaddr '(a c . (b))) type-error)
  t)

(deftest cadaar.error.1
  (signals-error (cadaar 'a) type-error)
  t)

(deftest cadaar.error.2
  (signals-error (cadaar '(a)) type-error)
  t)

(deftest cadaar.error.3
  (signals-error (cadaar '((a . b))) type-error)
  t)

(deftest cadaar.error.4
  (signals-error (cadaar '((a . (b)))) type-error)
  t)

(deftest cadadr.error.1
  (signals-error (cadadr 'a) type-error)
  t)

(deftest cadadr.error.2
  (signals-error (cadadr '(a . b)) type-error)
  t)

(deftest cadadr.error.3
  (signals-error (cadadr '(a b . c)) type-error)
  t)

(deftest cadadr.error.4
  (signals-error (cadadr '(a (b . e) . c)) type-error)
  t)

(deftest caddar.error.1
  (signals-error (caddar 'a) type-error)
  t)

(deftest caddar.error.2
  (signals-error (caddar '(a . b)) type-error)
  t)

(deftest caddar.error.3
  (signals-error (caddar '((a . b) . b)) type-error)
  t)

(deftest caddar.error.4
  (signals-error (caddar '((a  b . c) . b)) type-error)
  t)

(deftest cadddr.error.1
  (signals-error (cadddr 'a) type-error)
  t)

(deftest cadddr.error.2
  (signals-error (cadddr '(a . b)) type-error)
  t)

(deftest cadddr.error.3
  (signals-error (cadddr '(a c . b)) type-error)
  t)

(deftest cadddr.error.4
  (signals-error (cadddr '(a c e . b)) type-error)
  t)

(deftest cdaaar.error.1
  (signals-error (cdaaar 'a) type-error)
  t)

(deftest cdaaar.error.2
  (signals-error (cdaaar '(a)) type-error)
  t)

(deftest cdaaar.error.3
  (signals-error (cdaaar '((a))) type-error)
  t)

(deftest cdaaar.error.4
  (signals-error (cdaaar '(((a . b)))) type-error)
  t)

(deftest cdaadr.error.1
  (signals-error (cdaadr 'a) type-error)
  t)

(deftest cdaadr.error.2
  (signals-error (cdaadr '(a . b)) type-error)
  t)

(deftest cdaadr.error.3
  (signals-error (cdaadr '(a . (b))) type-error)
  t)

(deftest cdaadr.error.4
  (signals-error (cdaadr '(a . ((b . c)))) type-error)
  t)

(deftest cdadar.error.1
  (signals-error (cdadar 'a) type-error)
  t)

(deftest cdadar.error.2
  (signals-error (cdadar '(a . b)) type-error)
  t)

(deftest cdadar.error.3
  (signals-error (cdadar '((a . c) . b)) type-error)
  t)

(deftest cdadar.error.4
  (signals-error (cdadar '((a . (c . d)) . b)) type-error)
  t)

(deftest cdaddr.error.1
  (signals-error (cdaddr 'a) type-error)
  t)

(deftest cdaddr.error.2
  (signals-error (cdaddr '(a . b)) type-error)
  t)

(deftest cdaddr.error.3
  (signals-error (cdaddr '(a c . b)) type-error)
  t)

(deftest cdaddr.error.4
  (signals-error (cdaddr '(a c b . d)) type-error)
  t)

(deftest cddaar.error.1
  (signals-error (cddaar 'a) type-error)
  t)

(deftest cddaar.error.2
  (signals-error (cddaar '(a)) type-error)
  t)

(deftest cddaar.error.3
  (signals-error (cddaar '((a . b))) type-error)
  t)

(deftest cddaar.error.4
  (signals-error (cddaar '((a . (b)))) type-error)
  t)

(deftest cddadr.error.1
  (signals-error (cddadr 'a) type-error)
  t)

(deftest cddadr.error.2
  (signals-error (cddadr '(a . b)) type-error)
  t)

(deftest cddadr.error.3
  (signals-error (cddadr '(a b . c)) type-error)
  t)

(deftest cddadr.error.4
  (signals-error (cddadr '(a (b . e) . c)) type-error)
  t)

(deftest cdddar.error.1
  (signals-error (cdddar 'a) type-error)
  t)

(deftest cdddar.error.2
  (signals-error (cdddar '(a . b)) type-error)
  t)

(deftest cdddar.error.3
  (signals-error (cdddar '((a . b) . b)) type-error)
  t)

(deftest cdddar.error.4
  (signals-error (cdddar '((a  b . c) . b)) type-error)
  t)

(deftest cddddr.error.1
  (signals-error (cddddr 'a) type-error)
  t)

(deftest cddddr.error.2
  (signals-error (cddddr '(a . b)) type-error)
  t)

(deftest cddddr.error.3
  (signals-error (cddddr '(a c . b)) type-error)
  t)

(deftest cddddr.error.4
  (signals-error (cddddr '(a c e . b)) type-error)
  t)

;;; Need to add 'locally' wrapped forms of these

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setting of C*R accessors

(loop
    for fn in '(car cdr caar cadr cdar cddr
		caaar caadr cadar caddr cdaar cdadr cddar cdddr
		caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
		cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
    do
      (let ((level (- (length (symbol-name fn)) 2)))
	(eval `(deftest ,(intern
			  (concatenate 'string
			    (symbol-name fn)
			    "-SET-ALT")
			  :cl-test)
		   (let ((x (create-c*r-test ,level)))
		     (and
		      (setf (,fn x) 'a)
		      (eql (,fn x) 'a)
		      (setf (,fn x) 'none)
		      (equalt x (create-c*r-test ,level))
		      ))
		 t))))

(loop
    for (fn len) in '((first 1) (second 2) (third 3) (fourth 4)
		      (fifth 5) (sixth 6) (seventh 7) (eighth 8)
		      (ninth 9) (tenth 10))
    do
      (eval
       `(deftest ,(intern
		   (concatenate 'string
		     (symbol-name fn)
		     "-SET-ALT")
		   :cl-test)
	    (let ((x (make-list 20 :initial-element nil)))
	      (and
	       (setf (,fn x) 'a)
	       (loop
		   for i from 1 to 20
		   do (when (and (not (eql i ,len))
				 (nth (1- i) x))
			(return nil))
		   finally (return t))
	       (eql (,fn x) 'a)
	       (nth ,(1- len) x)))
	  a)))
