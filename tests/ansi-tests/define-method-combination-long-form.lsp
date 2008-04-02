;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jul 13 08:26:41 2003
;;;; Contains: Tests of DEFINE-METHOD-COMBINATION (long form)

(in-package :cl-test)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defparameter *dmc-long-01*
     (define-method-combination mc-long-01 nil nil)))
  (report-and-ignore-errors
   (defgeneric dmc-long-gf-01 (x y) (:method-combination mc-long-01)))
  )

(deftest define-method-combination-long.01.1
  (eqt *dmc-long-01* 'mc-long-01)
  t)

;;; The list of method groups specifiers for this method combination
;;; is empty, so no methods are valid.
(deftest define-method-combination-long.01.2
  (progn
    (eval '(defmethod dmc-long-gf-01 ((x t) (y t)) :foo))
    (handler-case
     (eval '(dmc-long-gf-01 'a 'b))
     (error () :caught)))
  :caught)

;;; A single method group with the * method group specifier

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defparameter *dmc-long-02*
     (define-method-combination mc-long-02 nil ((method-list *))
       `(vector ,@(mapcar #'(lambda (m) `(call-method ,m)) method-list)))))
  (report-and-ignore-errors
   (defgeneric dmc-long-gf-02 (x y) (:method-combination mc-long-02)))
  )

(deftest define-method-combination-long.02.1
  (eqt *dmc-long-02* 'mc-long-02)
  t)

(deftest define-method-combination-long.02.2
  (progn
    (eval '(defmethod dmc-long-gf-02 ((x (eql 1)) (y integer)) 'a))
    (eval '(defmethod dmc-long-gf-02 ((x integer) (y (eql 2))) 'b))
    (eval '(defmethod dmc-long-gf-02 ((x integer) (y integer)) 'z))
    (values
     (dmc-long-gf-02 0 0)
     (dmc-long-gf-02 1 0)
     (dmc-long-gf-02 0 2)
     (dmc-long-gf-02 1 2)))
  #(z) #(a z) #(b z) #(a b z))

(deftest define-method-combination-long.02.3
  (signals-error (dmc-long-gf-02 nil nil) error)
  t)

;;; Same, but with :order parameter.
;;; Also, :description with a format string

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defparameter *dmc-long-03*
     (define-method-combination mc-long-03 nil ((method-list * :order :most-specific-first
							     :description "This method has qualifiers ~A"
							     ))
       `(vector ,@(mapcar #'(lambda (m) `(call-method ,m)) method-list)))))
  (report-and-ignore-errors
   (defgeneric dmc-long-gf-03 (x y) (:method-combination mc-long-03)))
  )

(deftest define-method-combination-long.03.1
  (eqt *dmc-long-03* 'mc-long-03)
  t)

(deftest define-method-combination-long.03.2
  (progn
    (eval '(defmethod dmc-long-gf-03 ((x (eql 1)) (y integer)) 'a))
    (eval '(defmethod dmc-long-gf-03 ((x integer) (y (eql 2))) 'b))
    (eval '(defmethod dmc-long-gf-03 ((x integer) (y integer)) 'z))
    (values
     (dmc-long-gf-03 0 0)
     (dmc-long-gf-03 1 0)
     (dmc-long-gf-03 0 2)
     (dmc-long-gf-03 1 2)))
  #(z) #(a z) #(b z) #(a b z))

(deftest define-method-combination-long.03.3
  (signals-error (dmc-long-gf-03 nil nil) error)
  t)

;;; Same, but with :order parameter :most-specific-last
;;; (and testing that the :order parameter is evaluated)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defparameter *dmc-long-04*
     (let ((order :most-specific-last))
       (define-method-combination mc-long-04 nil ((method-list * :order order))
	 `(vector ,@(mapcar #'(lambda (m) `(call-method ,m)) method-list))))))
  (report-and-ignore-errors
   (defgeneric dmc-long-gf-04 (x y) (:method-combination mc-long-04)))
  )

(deftest define-method-combination-long.04.1
  (eqt *dmc-long-04* 'mc-long-04)
  t)

(deftest define-method-combination-long.04.2
  (progn
    (eval '(defmethod dmc-long-gf-04 ((x (eql 1)) (y integer)) 'a))
    (eval '(defmethod dmc-long-gf-04 ((x integer) (y (eql 2))) 'b))
    (eval '(defmethod dmc-long-gf-04 ((x integer) (y integer)) 'z))
    (values
     (dmc-long-gf-04 0 0)
     (dmc-long-gf-04 1 0)
     (dmc-long-gf-04 0 2)
     (dmc-long-gf-04 1 2)))
  #(z) #(z a) #(z b) #(z b a))

(deftest define-method-combination-long.04.3
  (signals-error (dmc-long-gf-04 nil nil) error)
  t)

;;; Empty qualifier list
  
(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defparameter *dmc-long-05*
     (define-method-combination mc-long-05 nil ((method-list nil)
						(ignored-methods *))
       (declare (ignorable ignored-methods))
       `(vector ,@(mapcar #'(lambda (m) `(call-method ,m)) method-list)))))
  (report-and-ignore-errors
   (defgeneric dmc-long-gf-05 (x y) (:method-combination mc-long-05)))
  )

(deftest define-method-combination-long.05.1
  (eqt *dmc-long-05* 'mc-long-05)
  t)

(deftest define-method-combination-long.05.2
  (progn
    (eval '(defmethod dmc-long-gf-05 ((x (eql 1)) (y integer)) 'a))
    (eval '(defmethod dmc-long-gf-05 ((x integer) (y (eql 2))) 'b))
    (eval '(defmethod dmc-long-gf-05 ((x integer) (y integer)) 'z))
    (eval '(defmethod dmc-long-gf-05 foo ((x t) (y t)) 'bad))
    (values
     (dmc-long-gf-05 nil nil)
     (dmc-long-gf-05 0 0)
     (dmc-long-gf-05 1 0)
     (dmc-long-gf-05 0 2)
     (dmc-long-gf-05 1 2)))
  #() #(z) #(a z) #(b z) #(a b z))

;;; :required
  
(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defparameter *dmc-long-06*
     (define-method-combination mc-long-06 nil ((method-list nil :required t)
						(ignored-methods *))
       (declare (ignorable ignored-methods))
       `(vector ,@(mapcar #'(lambda (m) `(call-method ,m)) method-list)))))
  (report-and-ignore-errors
   (defgeneric dmc-long-gf-06 (x y) (:method-combination mc-long-06)))
  )

(deftest define-method-combination-long.06.1
  (eqt *dmc-long-06* 'mc-long-06)
  t)

(deftest define-method-combination-long.06.2
  (progn
    (eval '(defmethod dmc-long-gf-06 ((x (eql 1)) (y integer)) 'a))
    (eval '(defmethod dmc-long-gf-06 ((x integer) (y (eql 2))) 'b))
    (eval '(defmethod dmc-long-gf-06 ((x integer) (y integer)) 'z))
    (eval '(defmethod dmc-long-gf-06 foo ((x t) (y t)) 'bad))
    (values
     (dmc-long-gf-06 0 0)
     (dmc-long-gf-06 1 0)
     (dmc-long-gf-06 0 2)
     (dmc-long-gf-06 1 2)))
  #(z) #(a z) #(b z) #(a b z))

(deftest define-method-combination-long.06.3
  (signals-error-always (dmc-long-gf-06 nil nil) error)
  t t)


;;; Non-empty lambda lists

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defparameter *dmc-long-07*
     (define-method-combination mc-long-07 (p1 p2) ((method-list *))
       `(vector ',p1 ',p2 ,@(mapcar #'(lambda (m) `(call-method ,m)) method-list)))))
  (report-and-ignore-errors
   (defgeneric dmc-long-gf-07 (x y) (:method-combination mc-long-07 1 2)))
  )

(deftest define-method-combination-long.07.1
  (eqt *dmc-long-07* 'mc-long-07)
  t)

(deftest define-method-combination-long.07.2
  (progn
    (eval '(defmethod dmc-long-gf-07 ((x (eql 1)) (y integer)) 'a))
    (eval '(defmethod dmc-long-gf-07 ((x integer) (y (eql 2))) 'b))
    (eval '(defmethod dmc-long-gf-07 ((x integer) (y integer)) 'z))
    (values
     (dmc-long-gf-07 0 0)
     (dmc-long-gf-07 1 0)
     (dmc-long-gf-07 0 2)
     (dmc-long-gf-07 1 2)))
  #(1 2 z) #(1 2 a z) #(1 2 b z) #(1 2 a b z))

(deftest define-method-combination-long.07.3
  (signals-error (dmc-long-gf-07 nil) error)
  t)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defparameter *dmc-long-08*
     (define-method-combination mc-long-08 (p1 &optional p2 p3) ((method-list *))
       `(vector ',p1 ',p2 ',p3 ,@(mapcar #'(lambda (m) `(call-method ,m)) method-list)))))
  (report-and-ignore-errors
   (defgeneric dmc-long-gf-08 (x y) (:method-combination mc-long-08 1 2)))
  )

(deftest define-method-combination-long.08.1
  (eqt *dmc-long-08* 'mc-long-08)
  t)

(deftest define-method-combination-long.08.2
  (progn
    (eval '(defmethod dmc-long-gf-08 ((x (eql 1)) (y integer)) 'a))
    (eval '(defmethod dmc-long-gf-08 ((x integer) (y (eql 2))) 'b))
    (eval '(defmethod dmc-long-gf-08 ((x integer) (y integer)) 'z))
    (values
     (dmc-long-gf-08 0 0)
     (dmc-long-gf-08 1 0)
     (dmc-long-gf-08 0 2)
     (dmc-long-gf-08 1 2)))
  #(1 2 nil z) #(1 2 nil a z) #(1 2 nil b z) #(1 2 nil a b z))

(deftest define-method-combination-long.08.3
  (signals-error (dmc-long-gf-08 nil) error)
  t)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defparameter *dmc-long-09*
     (define-method-combination mc-long-09 (p1 &key p2 p3) ((method-list *))
       `(vector ',p1 ',p2 ',p3 ,@(mapcar #'(lambda (m) `(call-method ,m)) method-list)))))
  (report-and-ignore-errors
   (defgeneric dmc-long-gf-09 (x y) (:method-combination mc-long-09 1 :p3 3)))
  )

(deftest define-method-combination-long.09.1
  (eqt *dmc-long-09* 'mc-long-09)
  t)

(deftest define-method-combination-long.09.2
  (progn
    (eval '(defmethod dmc-long-gf-09 ((x (eql 1)) (y integer)) 'a))
    (eval '(defmethod dmc-long-gf-09 ((x integer) (y (eql 2))) 'b))
    (eval '(defmethod dmc-long-gf-09 ((x integer) (y integer)) 'z))
    (values
     (dmc-long-gf-09 0 0)
     (dmc-long-gf-09 1 0)
     (dmc-long-gf-09 0 2)
     (dmc-long-gf-09 1 2)))
  #(1 nil 3 z) #(1 nil 3 a z) #(1 nil 3 b z) #(1 nil 3 a b z))

(deftest define-method-combination-long.09.3
  (signals-error (dmc-long-gf-09 nil) error)
  t)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defparameter *dmc-long-10*
     (define-method-combination mc-long-10 (p1 &rest p2) ((method-list *))
       `(vector ',p1 ',p2 ,@(mapcar #'(lambda (m) `(call-method ,m)) method-list)))))
  (report-and-ignore-errors
   (defgeneric dmc-long-gf-10 (x y) (:method-combination mc-long-10 1 2 3 4)))
  )

(deftest define-method-combination-long.10.1
  (eqt *dmc-long-10* 'mc-long-10)
  t)

(deftest define-method-combination-long.10.2
  (progn
    (eval '(defmethod dmc-long-gf-10 ((x (eql 1)) (y integer)) 'a))
    (eval '(defmethod dmc-long-gf-10 ((x integer) (y (eql 2))) 'b))
    (eval '(defmethod dmc-long-gf-10 ((x integer) (y integer)) 'z))
    (values
     (dmc-long-gf-10 0 0)
     (dmc-long-gf-10 1 0)
     (dmc-long-gf-10 0 2)
     (dmc-long-gf-10 1 2)))
  #(1 (2 3 4)  z) #(1 (2 3 4) a z) #(1 (2 3 4) b z) #(1 (2 3 4) a b z))

(deftest define-method-combination-long.10.3
  (signals-error (dmc-long-gf-10 nil) error)
  t)

