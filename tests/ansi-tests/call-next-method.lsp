;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 31 11:18:15 2003
;;;; Contains: Tests of CALL-NEXT-METHOD

(in-package :cl-test)

;;; Tests where there is no next method are in no-next-method.lsp

(defgeneric cnm-gf-01 (x)
  (:method ((x integer)) (cons 'a (call-next-method)))
  (:method ((x rational)) (cons 'b (call-next-method)))
  (:method ((x real)) (cons 'c (call-next-method)))
  (:method ((x number)) (cons 'd (call-next-method)))
  (:method ((x t)) nil))

(deftest call-next-method.1
  (mapcar #'cnm-gf-01 '(0 2/3 1.3 #c(1 1) a))
  ((a b c d) (b c d) (c d) (d) nil))

;; Check that call-next-method passes along multiple values correctly

(defgeneric cnm-gf-02 (x)
  (:method ((x integer)) (call-next-method))
  (:method ((x number)) (values))
  (:method ((x (eql 'a))) (call-next-method))
  (:method ((x symbol)) (values 1 2 3 4 5 6)))

(deftest call-next-method.2
  (cnm-gf-02 0))

(deftest call-next-method.3
  (cnm-gf-02 'a)
  1 2 3 4 5 6)

;;; Call next method has indefinite extent

(defgeneric cnm-gf-03 (x)
  (:method ((x integer)) #'call-next-method)
  (:method ((x t)) t))

(deftest call-next-method.4
  (funcall (cnm-gf-03 0))
  t)

;;; The arguments to c-n-m can be changed

(defgeneric cnm-gf-04 (x)
  (:method ((x integer)) (call-next-method (+ x 10)))
  (:method ((x number)) (1+ x)))

(deftest call-next-method.5
  (mapcar #'cnm-gf-04 '(0 1 2 5/3 9/2 1.0 #c(1 1)))
  (11 12 13 8/3 11/2 2.0 #c(2 1)))

;;; call-next-method goes up the list of applicable methods
;;; which may be to a method with specializers incomparable to
;;; the current method

(defgeneric cnm-gf-05 (x y)
  (:method ((x integer) (y integer)) (cons 'a (call-next-method)))
  (:method ((x integer) (y t))  (cons 'b (call-next-method)))
  (:method ((x t) (y integer))  (cons 'c (call-next-method)))
  (:method ((x t) (y t)) (list 'd)))

(deftest call-next-method.6
  (mapcar #'cnm-gf-05 '(0 0 t t) '(0 t 0 t))
  ((a b c d)
   (b d)
   (c d)
   (d)))

(defclass cnm-class-01a () ())
(defclass cnm-class-01b (cnm-class-01a) ())
(defclass cnm-class-01c (cnm-class-01a) ())
(defclass cnm-class-01d (cnm-class-01c cnm-class-01b) ())

(defgeneric cnm-gf-06 (x)
  (:method ((x cnm-class-01d)) (cons 1 (call-next-method)))
  (:method ((x cnm-class-01c)) (cons 2 (call-next-method)))
  (:method ((x cnm-class-01b)) (cons 3 (call-next-method)))
  (:method ((x cnm-class-01a)) (cons 4 (call-next-method)))
  (:method ((x t)) nil))

(deftest call-next-method.7
  (values
   (cnm-gf-06 (make-instance 'cnm-class-01d))
   (cnm-gf-06 (make-instance 'cnm-class-01c))
   (cnm-gf-06 (make-instance 'cnm-class-01b))
   (cnm-gf-06 (make-instance 'cnm-class-01a))
   (cnm-gf-06 nil))
  (1 2 3 4)
  (2 4)
  (3 4)
  (4)
  nil)

;;; Neither rebinding nor setq affects the arguments passed by
;;; (call-next-method)

(defgeneric cnm-gf-07 (x)
  (:method ((x integer)) (list (incf x) (call-next-method)))
  (:method ((x symbol)) (list (setq x 'a) x (call-next-method)))
  (:method ((x cons)) (list x (let ((x :bad))
				(declare (ignorable x))
				(call-next-method))))
  (:method ((x t)) x))

(deftest call-next-method.8
  (mapcar #'cnm-gf-07 '(0 z (x) #\a))
  ((1 0) (a a z) ((x) (x)) #\a))

;; Nor does argument defaulting

(defgeneric cnm-gf-08 (x &optional y)
  (:method ((x integer) &optional y) (list* x y (call-next-method)))
  (:method ((x t) &optional y) (list x y)))

(deftest call-next-method.9
  (values
   (cnm-gf-08 0)
   (cnm-gf-08 0 t)
   (cnm-gf-08 'a)
   (cnm-gf-08 'a 'b))
  (0 nil 0 nil)
  (0 t 0 t)
  (a nil)
  (a b))

;;; When c-n-m is called with arguments but omits optionals, those
;;; optionals are defaulted

(defgeneric cnm-gf-09 (x &optional y)
  (:method ((x integer) &optional y) (list* x y (call-next-method (1+ x))))
  (:method ((x t) &optional y) (list x y)))

(deftest call-next-method.10
  (values
   (cnm-gf-09 5)
   (cnm-gf-09 8 'a)
   (cnm-gf-09 'x)
   (cnm-gf-09 'x 'y))
  (5 nil 6 nil)
  (8 a 9 nil)
  (x nil)
  (x y))

(defgeneric cnm-gf-10 (x &optional y z)
  (:method ((x integer) &optional (y 'a y-p) (z 'b z-p))
	   (list* x y (notnot y-p) z (notnot z-p) (call-next-method (1+ x))))
  (:method ((x t) &optional (y 'c y-p) (z 'd z-p))
	   (list x y (notnot y-p) z (notnot z-p))))

(deftest call-next-method.11
  (values
   (cnm-gf-10 5)
   (cnm-gf-10 8 'p)
   (cnm-gf-10 8 'p 'q)
   (cnm-gf-10 'x)
   (cnm-gf-10 'x 'u)
   (cnm-gf-10 'x 'u 'v))
  (5 a nil b nil 6 c nil d nil)
  (8 p t b nil 9 c nil d nil)
  (8 p t q t 9 c nil d nil)
  (x c nil d nil)
  (x u t d nil)
  (x u t v t))

;;; "When providing arguments to call-next-method, the following
;;;  rule must be satisfied or an error of type error should be signaled:
;;;  the ordered set of applicable methods for a changed set of arguments
;;;  for call-next-method must be the same as the ordered set of applicable
;;;  methods for the original arguments to the generic function."

(defgeneric cnm-order-error-gf-01 (x)
  (declare (optimize (safety 3)))
  (:method ((x (eql 0)))
	   (declare (optimize (safety 3)))
	   (call-next-method 1))  ;; no longer EQL to 0
  (:method ((x t)) nil))

(deftest call-next-method.error.1
  (locally
   (declare (optimize (safety 3)))
   (handler-case 
    (eval '(locally (declare (optimize (safety 3)))
		    (cnm-order-error-gf-01 0)))
    (error () :error)))
  :error)

(defgeneric cnm-order-error-gf-02 (x)
  (declare (optimize (safety 3)))
  (:method ((x integer))
	   (declare (optimize (safety 3)))
	   (call-next-method :bad))
  (:method ((x t)) x))

(deftest call-next-method.error.2
  (locally
   (declare (optimize (safety 3)))
   (handler-case 
    (eval '(locally (declare (optimize (safety 3)))
		    (cnm-order-error-gf-02 0)))
    (error () :error)))
  :error)



	   


  

