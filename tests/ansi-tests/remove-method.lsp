;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May 11 19:53:37 2003
;;;; Contains: Tests of REMOVE-METHOD

(in-package :cl-test)

(defparameter *remove-meth-gf-01*
  (defgeneric remove-meth-gf-01 (x)))

(defparameter *remove-meth-gf-01-method-t*
  (defmethod remove-meth-gf-01 ((x t)) x))

(defparameter *remove-meth-gf-02*
  (defgeneric remove-meth-gf-02 (x)))

(defparameter *remove-meth-gf-02-method-t*
  (defmethod remove-meth-gf-02 ((x t)) x))

;;; remove method must not signal an error if the method
;;; does not belong to the generic function

(deftest remove-method.1
  (and
   (eqt (remove-method *remove-meth-gf-01* *remove-meth-gf-02-method-t*)
	*remove-meth-gf-01*)
   (remove-meth-gf-01 :good))
  :good)

;;; Add, then remove, a method

(deftest remove-method.2
  (let (meth)
    (values
     (remove-meth-gf-01 10)
     (progn (setf meth (eval '(defmethod remove-meth-gf-01 ((x integer))
				(1+ x))))
	    nil)
     (remove-meth-gf-01 10)
     (eqt *remove-meth-gf-01*
	  (remove-method *remove-meth-gf-01* meth))
     (remove-meth-gf-01 10)))
  10 nil 11 t 10)

;;; Add two disjoint methods, then remove

(deftest remove-method.3
  (let (meth1 meth2)
    (values
     (mapcar #'remove-meth-gf-01 '(19 a))
     (progn
       (setf meth1 (eval '(defmethod remove-meth-gf-01 ((x symbol))
			    (list x))))

       (mapcar #'remove-meth-gf-01 '(19 a)))
     (progn
       (setf meth2 (eval '(defmethod remove-meth-gf-01 ((x number))
			    (1+ x))))

       (mapcar #'remove-meth-gf-01 '(19 a)))
     (eqt *remove-meth-gf-01* (remove-method *remove-meth-gf-01* meth1))
     (mapcar #'remove-meth-gf-01 '(19 a))
     (eqt *remove-meth-gf-01* (remove-method *remove-meth-gf-01* meth2))
     (mapcar #'remove-meth-gf-01 '(19 a))))
  (19 a) (19 (a)) (20 (a)) t (20 a) t (19 a))

;;; Remove in the other order

(deftest remove-method.4
  (let (meth1 meth2)
    (values
     (mapcar #'remove-meth-gf-01 '(19 a))
     (progn
       (setf meth1 (eval '(defmethod remove-meth-gf-01 ((x symbol))
			    (list x))))

       (mapcar #'remove-meth-gf-01 '(19 a)))
     (progn
       (setf meth2 (eval '(defmethod remove-meth-gf-01 ((x number))
			    (1+ x))))

       (mapcar #'remove-meth-gf-01 '(19 a)))
     (eqt *remove-meth-gf-01* (remove-method *remove-meth-gf-01* meth2))
     (mapcar #'remove-meth-gf-01 '(19 a))
     (eqt *remove-meth-gf-01* (remove-method *remove-meth-gf-01* meth1))
     (mapcar #'remove-meth-gf-01 '(19 a))))
  (19 a) (19 (a)) (20 (a)) t (19 (a)) t (19 a))

;;; Now methods that shadow one another

(deftest remove-method.5
  (let (meth1 meth2)
    (values
     (mapcar #'remove-meth-gf-01 '(10 20.0))
     (progn
       (setf meth1 (eval '(defmethod remove-meth-gf-01 ((x integer))
			    (1- x))))

       (mapcar #'remove-meth-gf-01 '(10 20.0)))
     (progn
       (setf meth2 (eval '(defmethod remove-meth-gf-01 ((x number))
			    (1+ x))))

       (mapcar #'remove-meth-gf-01 '(10 20.0)))
     (eqt *remove-meth-gf-01* (remove-method *remove-meth-gf-01* meth1))
     (mapcar #'remove-meth-gf-01 '(10 20.0))
     (eqt *remove-meth-gf-01* (remove-method *remove-meth-gf-01* meth2))
     (mapcar #'remove-meth-gf-01 '(10 20.0))))
  (10 20.0) (9 20.0) (9 21.0) t (11 21.0) t (10 20.0))

(deftest remove-method.6
  (let (meth1 meth2)
    (values
     (mapcar #'remove-meth-gf-01 '(10 20.0))
     (progn
       (setf meth1 (eval '(defmethod remove-meth-gf-01 ((x integer))
			    (1- x))))

       (mapcar #'remove-meth-gf-01 '(10 20.0)))
     (progn
       (setf meth2 (eval '(defmethod remove-meth-gf-01 ((x number))
			    (1+ x))))

       (mapcar #'remove-meth-gf-01 '(10 20.0)))
     (eqt *remove-meth-gf-01* (remove-method *remove-meth-gf-01* meth2))
     (mapcar #'remove-meth-gf-01 '(10 20.0))
     (eqt *remove-meth-gf-01* (remove-method *remove-meth-gf-01* meth1))
     (mapcar #'remove-meth-gf-01 '(10 20.0))))
  (10 20.0) (9 20.0) (9 21.0) t (9 20.0) t (10 20.0))

(deftest remove-method.7
  (let (meth1 meth2)
    (values
     (mapcar #'remove-meth-gf-01 '(10 20.0))
     (progn
       (setf meth1 (eval '(defmethod remove-meth-gf-01 ((x number))
			    (1+ x))))

       (mapcar #'remove-meth-gf-01 '(10 20.0)))
     (progn
       (setf meth2 (eval '(defmethod remove-meth-gf-01 ((x integer))
			    (1- x))))

       (mapcar #'remove-meth-gf-01 '(10 20.0)))
     (eqt *remove-meth-gf-01* (remove-method *remove-meth-gf-01* meth1))
     (mapcar #'remove-meth-gf-01 '(10 20.0))
     (eqt *remove-meth-gf-01* (remove-method *remove-meth-gf-01* meth2))
     (mapcar #'remove-meth-gf-01 '(10 20.0))))
  (10 20.0) (11 21.0) (9 21.0) t (9 20.0) t (10 20.0))

(deftest remove-method.8
  (let (meth1 meth2)
    (values
     (mapcar #'remove-meth-gf-01 '(10 20.0))
     (progn
       (setf meth1 (eval '(defmethod remove-meth-gf-01 ((x number))
			    (1+ x))))

       (mapcar #'remove-meth-gf-01 '(10 20.0)))
     (progn
       (setf meth2 (eval '(defmethod remove-meth-gf-01 ((x integer))
			    (1- x))))

       (mapcar #'remove-meth-gf-01 '(10 20.0)))
     (eqt *remove-meth-gf-01* (remove-method *remove-meth-gf-01* meth2))
     (mapcar #'remove-meth-gf-01 '(10 20.0))
     (eqt *remove-meth-gf-01* (remove-method *remove-meth-gf-01* meth1))
     (mapcar #'remove-meth-gf-01 '(10 20.0))))
  (10 20.0) (11 21.0) (9 21.0) t (11 21.0) t (10 20.0))

;;; Adding and removing auxiliary methods

(declaim (special *rmgf-03-var*))

(defparameter *remove-meth-gf-03*
  (defgeneric remove-meth-gf-03 (x)))

(defparameter *remove-meth-gf-03-method-t*
  (defmethod remove-meth-gf-03 ((x t)) (list *rmgf-03-var* x)))

(deftest remove-method.9
  (let (meth (*rmgf-03-var* 0))
    (values
     (mapcar #'remove-meth-gf-03 '(5 a))
     (progn
       (setf meth (eval '(defmethod remove-meth-gf-03 :before ((x number))
			   (incf *rmgf-03-var*))))
       (mapcar #'remove-meth-gf-03 '(5 a)))
     (eqt *remove-meth-gf-03* (remove-method *remove-meth-gf-03* meth))
     (mapcar #'remove-meth-gf-03 '(5 a))))
  ((0 5) (0 a))
  ((1 5) (1 a))
  t
  ((1 5) (1 a)))

(deftest remove-method.10
  (let (meth (*rmgf-03-var* 0))
    (values
     (mapcar #'remove-meth-gf-03 '(5 a))
     (progn
       (setf meth (eval '(defmethod remove-meth-gf-03 :after ((x number))
			   (incf *rmgf-03-var*))))
       (mapcar #'remove-meth-gf-03 '(5 a)))
     (eqt *remove-meth-gf-03* (remove-method *remove-meth-gf-03* meth))
     (mapcar #'remove-meth-gf-03 '(5 a))))
  ((0 5) (0 a))
  ((0 5) (1 a))
  t
  ((1 5) (1 a)))

(deftest remove-method.11
  (let (meth (*rmgf-03-var* 0))
    (values
     (mapcar #'remove-meth-gf-03 '(5 a))
     (progn
       (setf meth (eval '(defmethod remove-meth-gf-03 :around ((x number))
			   (incf *rmgf-03-var*)
			   (prog1 (call-next-method)
			     (decf *rmgf-03-var*)))))
       (mapcar #'remove-meth-gf-03 '(5 a)))
     (eqt *remove-meth-gf-03* (remove-method *remove-meth-gf-03* meth))
     (mapcar #'remove-meth-gf-03 '(5 a))))
  ((0 5) (0 a))
  ((1 5) (0 a))
  t
  ((0 5) (0 a)))

;;; Must add tests for nonstandard method combinations
