;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu May 29 07:15:06 2003
;;;; Contains: Tests of FIND-CLASS

;; find-class is also tested in numerous other places.

(in-package :cl-test)

(deftest find-class.1
  (loop for name in *cl-types-that-are-classes-symbols*
	unless (eq (find-class name) (find-class name))
	collect name)
  nil)

(deftest find-class.2
  (loop for name in *cl-types-that-are-classes-symbols*
	unless (eq (find-class name t) (find-class name))
	collect name)
  nil)

(deftest find-class.3
  (loop for name in *cl-types-that-are-classes-symbols*
	unless (eq (find-class name nil) (find-class name))
	collect name)
  nil)

(deftest find-class.4
  (handler-case
   (progn (eval '(find-class (gensym))) :bad)
   (error () :good))
  :good)

(deftest find-class.5
  (handler-case
   (progn (eval '(find-class (gensym) t)) :bad)
   (error () :good))
  :good)

(deftest find-class.6
  (find-class (gensym) nil)
  nil)

(deftest find-class.7
  (loop for name in *cl-types-that-are-classes-symbols*
	unless (eq (find-class name t nil) (find-class name))
	collect name)
  nil)

(deftest find-class.8
  (loop for name in *cl-types-that-are-classes-symbols*
	unless (eq (find-class name nil nil) (find-class name))
	collect name)
  nil)

(deftest find-class.9
  (macrolet
      ((%m (&environment env)
	   (let ((result
		  (loop for name in *cl-types-that-are-classes-symbols*
			unless (eq (find-class name nil env)
				   (find-class name))
			collect name)))
	     `',result)))
    (%m))
  nil)

(deftest find-class.10
  (macrolet
      ((%m (&environment env)
	   (let ((result
		  (loop for name in *cl-types-that-are-classes-symbols*
			unless (eq (find-class name t env)
				   (find-class name))
			collect name)))
	     `',result)))
    (%m))
  nil)

(deftest find-class.11
  (handler-case
   (progn (eval '(find-class (gensym) 'a nil)) :bad)
   (error () :good))
  :good)

(deftest find-class.12
  (find-class (gensym) nil nil)
  nil)

(deftest find-class.13
  (macrolet
      ((%m (&environment env)
	   `',(find-class (gensym) nil env)))
    (%m))
  nil)

(deftest find-class.14
  (handler-case
   (progn
     (eval '(macrolet
		((%m (&environment env)
		     `',(find-class (gensym) 17 env)))
	      (%m)))
     :bad)
   (error () :good))
  :good)

;;; Need tests of assignment to (FIND-CLASS ...)
;;; Add tests of:
;;;   Setting class to itself
;;;   Changing class to a different class
;;;   Changing to NIL (and that the class object stays around)
;;;   Check that find-class is affected by the assignment, and
;;;    class-name is not.

(deftest find-class.15
  (progn
    (setf (find-class 'find-class-class-01) nil)
    (let* ((class  (eval '(defclass find-class-class-01 () ())))
	   (class1 (find-class 'find-class-class-01))
	   (class2 (setf (find-class 'find-class-class-01) class1)))
      (values
       (eqt class class1)
       (eqt class class2)
       (class-name class)
       )))
  t t find-class-class-01)

(deftest find-class.16
  (progn
    (setf (find-class 'find-class-class-01 nil) nil)
    (setf (find-class 'find-class-class-01 t) nil) ;; should not throw error
    (let* ((i 0)
	   (class  (eval '(defclass find-class-class-01 () ())))
	   (class1 (find-class 'find-class-class-01))
	   (class2 (setf (find-class 'find-class-class-01 (incf i)) class1)))
      (values
       i
       (eqt class class1)
       (eqt class class2))))
  1 t t)

(deftest find-class.17
  (macrolet
      ((%m (&environment env)
	   `',(progn
		(setf (find-class 'find-class-class-01) nil)
		(let*
		    ((i 0)
		     x y z
		     (class  (eval '(defclass find-class-class-01 () ())))
		     (class1 (find-class (progn (setf x (incf i))
						'find-class-class-01)
					 (setf y (incf i))
					 (progn (setf z (incf i)) env)))
		     (class2 (setf (find-class 'find-class-class-01) class1)))
		  (list
		   (eqt class class1)
		   (eqt class class2)
		   i x y z
		   )))))
    (%m))
  (t t 3 1 2 3))

(deftest find-class.18
  (progn
    (setf (find-class 'find-class-class-01) nil)
    (let* ((class  (eval '(defclass find-class-class-01 () ())))
	   (class1 (find-class 'find-class-class-01))
	   (class2 (setf (find-class 'find-class-class-01) nil))
	   (class3 (find-class 'find-class-class-01 nil)))
      (values
       (eqt class class1)
       (eqt class class2)
       class2
       (class-name class)
       class3)))
  t nil nil find-class-class-01 nil)

(deftest find-class.19
  (progn
    (setf (find-class 'find-class-class-01 nil) nil)
    (setf (find-class 'find-class-class-01 t) nil) ;; should not throw error
    (let* ((class  (eval '(defclass find-class-class-01 () ())))
	   (class1 (find-class 'find-class-class-01))
	   (class2 (setf (find-class 'find-class-class-01 t nil)
			 class1)))
      (values
       (eqt class class1)
       (eqt class class2))))
  t t)

;; Change to a different class

(deftest find-class.20
  (progn
    (setf (find-class 'find-class-class-01) nil)
    (setf (find-class 'find-class-class-02) nil)
    (let* ((class1 (eval '(defclass find-class-class-01 () ())))
	   (class2 (eval '(defclass find-class-class-02 () ()))))
      (setf (find-class 'find-class-class-01) class2)
      (let* ((new-class1 (find-class 'find-class-class-01 nil))
	     (new-class2 (find-class 'find-class-class-02)))
	(values
	 (eqt class1 class2)
	 (eqt class2 new-class1)
	 (eqt class2 new-class2)
	 (class-name class2)))))
  nil t t find-class-class-02)

(deftest find-class.21
  (progn
    (setf (find-class 'find-class-class-01) nil)
    (setf (find-class 'find-class-class-02) nil)
    (let* ((class1 (eval '(defclass find-class-class-01 () ())))
	   (class2 (eval '(defclass find-class-class-02 () ()))))
      (psetf (find-class 'find-class-class-01) class2
	     (find-class 'find-class-class-02) class1)
      (let* ((new-class1 (find-class 'find-class-class-01 nil))
	     (new-class2 (find-class 'find-class-class-02)))
	(values
	 (eqt class1 class2)
	 (eqt class2 new-class1)
	 (eqt class1 new-class2)
	 (class-name new-class1)
	 (class-name new-class2)
	 ))))
  nil t t find-class-class-02 find-class-class-01)

;;; Effect on method dispatch

(deftest find-class.22
  (progn
    (setf (find-class 'find-class-class-01) nil)
    (let* ((class1 (eval
		    '(defclass find-class-class-01 () ())))
	   (fn (eval '(defgeneric find-class-gf-01 (x)
			(:method ((x find-class-class-01)) :good)
			(:method ((x t)) nil))))
	   (obj (make-instance class1)))
      (assert (typep fn 'function))
      (locally
       (declare (type function fn))
       (values
	(funcall fn nil)
	(funcall fn obj)
	(setf (find-class 'find-class-class-01) nil)
	(funcall fn nil)
	(funcall fn obj)))))
  nil :good nil nil :good)

(deftest find-class.23
  (progn
    (setf (find-class 'find-class-class-01) nil)
    (setf (find-class 'find-class-class-02) nil)
    (let* ((class1 (eval '(defclass find-class-class-01 () ())))
	   (class2 (eval '(defclass find-class-class-02
			    (find-class-class-01) ())))
	   (fn (eval '(defgeneric find-class-gf-02 (x)
			(:method ((x find-class-class-01)) 1)
			(:method ((x find-class-class-02)) 2)
			(:method ((x t)) t))))
	   (obj1 (make-instance class1))
	   (obj2 (make-instance class2)))
      (assert (typep fn 'function))
      (locally
       (declare (type function fn))
       (values
	(funcall fn nil)
	(funcall fn obj1)
	(funcall fn obj2)
	(setf (find-class 'find-class-class-01) nil)
	(funcall fn nil)
	(funcall fn obj1)
	(funcall fn obj2)))))
  t 1 2 nil t 1 2)

;;; Error tests

(deftest find-class.error.1
  (signals-error (find-class) program-error)
  t)

(deftest find-class.error.2
  (signals-error (find-class 'symbol nil nil nil) program-error)
  t)
