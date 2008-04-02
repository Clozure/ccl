;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jun 15 10:49:39 2003
;;;; Contains: Tests of DEFINE-METHOD-COMBINATION

(in-package :cl-test)

(defclass dmc-class-01a () ())
(defclass dmc-class-01b (dmc-class-01a) ())
(defclass dmc-class-01c (dmc-class-01a) ())
(defclass dmc-class-01d (dmc-class-01b dmc-class-01c) ())
(defclass dmc-class-01e (dmc-class-01c dmc-class-01b) ())
(defclass dmc-class-01f (dmc-class-01d) ())
(defclass dmc-class-01g (dmc-class-01a) ())
(defclass dmc-class-01h (dmc-class-01f dmc-class-01g) ())

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defvar *dmc-times*
     (define-method-combination times
       :documentation "Multiplicative method combination, version 1"
       :operator *))
   
   (defgeneric dmc-gf-01 (x) (:method-combination times))
   
   (defmethod dmc-gf-01 times ((x integer)) 2)
   (defmethod dmc-gf-01 times ((x rational)) 3)
   (defmethod dmc-gf-01 times ((x real)) 5)
   (defmethod dmc-gf-01 times ((x number)) 7)
   (defmethod dmc-gf-01 times ((x complex)) 11)
   ))

(deftest define-method-combination-01.1
  (values
   (dmc-gf-01 1)
   (dmc-gf-01 1/2)
   (dmc-gf-01 1.0)
   (dmc-gf-01 #c(1 2)))
  210 105 35 77)

(deftest define-method-combination-01.2
  (handler-case
   (eval '(locally (declare (optimize (safety 3)))
		   (dmc-gf-01 'x)))
   (error () :good))
  :good)

(deftest define-method-combination-01.3
  *dmc-times*
  times)

(deftest define-method-combination-01.4
  (let ((doc (documentation *dmc-times* 'method-combination)))
    (or (null doc)
	(equalt doc "Multiplicative method combination, version 1")))
  t)		

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defgeneric dmc-gf-02 (x) (:method-combination times))
   
   (defmethod dmc-gf-02 times ((x integer)) 2)
   (defmethod dmc-gf-02 :around ((x rational)) (1- (call-next-method)))
   (defmethod dmc-gf-02 times ((x real)) 3)
   (defmethod dmc-gf-02 times ((x number)) 5)
   (defmethod dmc-gf-02 :around ((x (eql 1.0s0))) 1)
   ))

(deftest define-method-combination-02.1
  (values
   (dmc-gf-02 1)
   (dmc-gf-02 1/3)
   (dmc-gf-02 1.0s0)
   (dmc-gf-02 13.0)
   (dmc-gf-02 #c(1 2)))
  29 14 1 15 5)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defgeneric dmc-gf-03 (x) (:method-combination times))))

(deftest define-method-combination-03.1
  (prog1
      (handler-case
       (progn
	 (eval '(defmethod dmc-gf-03 ((x integer)) t))
	 (eval '(dmc-gf-03 1))
	 :bad)
       (error () :good))
    (dolist (meth (compute-applicable-methods #'dmc-gf-03 (list 1)))
      (remove-method #'dmc-gf-03 meth)))
  :good)

(deftest define-method-combination-03.2
  (prog1
      (handler-case
       (progn
	 (eval '(defmethod dmc-gf-03 :before ((x cons)) t))
	 (eval '(dmc-gf-03 (cons 'a 'b)))
	 :bad)
       (error () :good))
    (dolist (meth (compute-applicable-methods #'dmc-gf-03 (list '(a))))
      (remove-method #'dmc-gf-03 meth)))
  :good)

(deftest define-method-combination-03.3
  (prog1
      (handler-case
       (progn
	 (eval '(defmethod dmc-gf-03 :after ((x symbol)) t))
	 (eval '(dmc-gf-03 'a))
	 :bad)
       (error () :good))
    (dolist (meth (compute-applicable-methods #'dmc-gf-03 (list 'a)))
      (remove-method #'dmc-gf-03 meth)))
  :good)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (define-method-combination times2
     :operator *
     :identity-with-one-argument t)
   
   (defgeneric dmc-gf-04 (x) (:method-combination times2))
   
   (defmethod dmc-gf-04 times2 ((x dmc-class-01b)) 2)
   (defmethod dmc-gf-04 times2 ((x dmc-class-01c)) 3)
   (defmethod dmc-gf-04 times2 ((x dmc-class-01d)) 5)
   (defmethod dmc-gf-04 times2 ((x symbol)) nil)
   ))

(deftest define-method-combination-04.1
  (dmc-gf-04 (make-instance 'dmc-class-01h))
  30)

(deftest define-method-combination-04.2
  (dmc-gf-04 (make-instance 'dmc-class-01e))
  6)

(deftest define-method-combination-04.3
  (dmc-gf-04 'a)
  nil)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defvar *dmc-times-5*
     (define-method-combination times-5 :operator *))))

(deftest define-method-combination-05.1
  (let* ((doc1 (setf (documentation *dmc-times-5* 'method-combination)
		     "foo"))
	 (doc2 (documentation *dmc-times-5* 'method-combination)))
    (values
     doc1
     (or (null doc2)
	 (equalt doc2 "foo"))))
  "foo" t)

;; Operator name defaults to the method combination name.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun times-7 (&rest args) (apply #'* args))
  (report-and-ignore-errors
   (defvar *dmc-times-7*
     (define-method-combination times-7))
   (defgeneric dmc-gf-07 (x) (:method-combination times))

   (defmethod dmc-gf-07 times ((x integer)) 2)
   (defmethod dmc-gf-07 times ((x rational)) 3)
   (defmethod dmc-gf-07 times ((x real)) 5)
   (defmethod dmc-gf-07 times ((x number)) 7)
   (defmethod dmc-gf-07 times ((x complex)) 11)
   ))

(deftest define-method-combination-07.1
  (values
   (dmc-gf-07 1)
   (dmc-gf-07 1/2)
   (dmc-gf-07 1.0)
   (dmc-gf-07 #c(1 2)))
  210 105 35 77)
