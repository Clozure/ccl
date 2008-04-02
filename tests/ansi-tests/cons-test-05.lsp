;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:34:08 1998
;;;; Contains:  Testing of CL Features related to "CONS", part 5

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(defparameter *cons-accessors*
  '(first second third fourth fifth sixth seventh eighth ninth tenth
    car cdr caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; first, ..., tenth

(deftest first-etc-1
  (let ((x (loop for i from 1 to 20 collect i)))
    (list (first x)
	  (second x)
	  (third x)
	  (fourth x)
	  (fifth x)
	  (sixth x)
	  (seventh x)
	  (eighth x)
	  (ninth x)
	  (tenth x)))
  (1 2 3 4 5 6 7 8 9 10))

(deftest first-etc-2
  (let ((x (make-list 15 :initial-element 'a)))
    (and
     (eql (setf (first x) 1) 1)
     (eql (setf (second x) 2) 2)
     (eql (setf (third x) 3) 3)
     (eql (setf (fourth x) 4) 4)
     (eql (setf (fifth x) 5) 5)
     (eql (setf (sixth x) 6) 6)
     (eql (setf (seventh x) 7) 7)
     (eql (setf (eighth x) 8) 8)
     (eql (setf (ninth x) 9) 9)
     (eql (setf (tenth x) 10) 10)
     x))
  (1 2 3 4 5 6 7 8 9 10 a a a a a))

(deftest rest-set-1
  (let ((x (list 'a 'b 'c)))
    (and
     (eqt (setf (rest x) 'd) 'd)
     x))
  (a . d))

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
				  "-SET")
		     :cl-test)
	    (let ((x (create-c*r-test ,level))
		  (y (list (create-c*r-test ,level)))
		  (i 0))
	      (and
	       (setf (,fn (progn (incf i) x)) 'a)
	       (eqlt (,fn x) 'a)
	       (eqlt i 1)
	       (setf (,fn x) 'none)
	       (equalt x (create-c*r-test ,level))
	       (setf (,fn (progn (incf i) (car y))) 'a)
	       (eqlt (,fn (car y)) 'a)
	       (eqlt i 2)
	       (setf (,fn (car y)) 'none)
	       (null (cdr y))
	       (equalt (car y) (create-c*r-test ,level))
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
			   "-SET")
	      :cl-test)
     (let* ((x (make-list 20 :initial-element nil))
	    (y (list (copy-list x)))
	    (cnt 0))
       (and
	(setf (,fn (progn (incf cnt) x)) 'a)
	(eqlt cnt 1)
	(loop
	 for i from 1 to 20
	 do (when (and (not (eql i ,len))
		       (nth (1- i) x))
	      (return nil))
	 finally (return t))
	(setf (,fn (car y)) 'a)
	(loop
	 for i from 1 to 20
	 do (when (and (not (eql i ,len))
		       (nth (1- i) (car y)))
	      (return nil))
	 finally (return t))
	(eqlt (,fn x) 'a)
	(eqlt (nth ,(1- len) x) 'a)
	(eqlt (,fn (car y)) 'a)
	(nth ,(1- len) (car y))))
     a)))

;; set up program-error tests

(loop for name in *cons-accessors*
      do (eval
	  `(deftest ,(intern (concatenate 'string (symbol-name name)
					  ".ERROR.NO-ARGS")
			     :cl-test)
	     (signals-error (,name) program-error)
	     t))
      do (eval
	  `(deftest ,(intern (concatenate 'string (symbol-name name)
					  ".ERROR.EXCESS-ARGS")
			     :cl-test)
	     (signals-error (,name nil nil) program-error)
	     t)))
