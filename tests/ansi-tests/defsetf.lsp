;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 17:18:01 2003
;;;; Contains: Tests of DEFSETF

(in-package :cl-test)

;;; Need to add non-error tests

(def-macro-test defsetf.error.1 (defsetf nonexistent-access-fn
				  nonexistent-update-fn))

;;; Short form

(defun defsetf.1-accessor (x)
  (cadr x))

(defun defsetf.1-accessor-settor (x val)
  (setf (cadr x) val))

(deftest defsetf.1
  (progn
    (let ((vals (multiple-value-list
		 (defsetf defsetf.1-accessor defsetf.1-accessor-settor))))
      (assert (equal vals '(defsetf.1-accessor))
	      ()
	      "Return values are ~A~%" vals))
    (eval
     '(let ((x (list 1 2 3)))
	(values
	 (setf (defsetf.1-accessor x) 4)
	 x))))
  4
  (1 4 3))

;;; Use a macro instead of a function for updatefn

(defun defsetf.2-accessor (x)
  (cadr x))

(defmacro defsetf.2-accessor-settor (x val)
  `(setf (cadr ,x) ,val))

(defparameter *defsetf.2-vals*
  (multiple-value-list
   (defsetf defsetf.2-accessor defsetf.2-accessor-settor)))

(deftest defsetf.2a
  *defsetf.2-vals*
  (defsetf.2-accessor))

(deftest defsetf.2b
  (let ((x (list 1 2 3)))
    (values
     (setf (defsetf.2-accessor x) 4)
     x))
  4
  (1 4 3))

;;; Documentation string

(defun defsetf.3-accessor (x)
  (cadr x))

(defun defsetf.3-accessor-settor (x val)
  (setf (cadr x) val))

(defparameter *defsetf.3-vals*
  (multiple-value-list
   (defsetf defsetf.3-accessor defsetf.3-accessor-settor
     "A doc string")))

(deftest defsetf.3a
  *defsetf.3-vals*
  (defsetf.3-accessor))

(deftest defsetf.3b
  (let ((doc (documentation 'defsetf.3-accessor 'setf)))
    (or (null doc) (equalt doc "A doc string")))
  t)

(deftest defsetf.3c
  (let ((x (list 1 2 3)))
    (values
     (setf (defsetf.3-accessor x) 4)
     x))
  4
  (1 4 3))

;;; Long form of defsetf

(defun defsetf.4-accessor (n seq)
  (elt seq n))

(defparameter *defsetf.4-vals*
  (multiple-value-list
   (defsetf defsetf.4-accessor (n seq) (val) 
     (declare)
     "Doc string for defsetf.4-accessor setf"
     `(setf (elt ,seq ,n) ,val))))

(deftest defsetf.4a
  *defsetf.4-vals*
  (defsetf.4-accessor))

(deftest defsetf.4b
  (let ((doc (documentation 'defsetf.4-accessor 'setf)))
    (or (null doc) (equalt doc "Doc string for defsetf.4-accessor setf")))
  t)

(deftest defsetf.4c
  (let ((x (list 1 2 3 4))
	(i 0)
	(j nil)
	(k nil))
    (values
     (setf (defsetf.4-accessor
	     (progn (setf j (incf i))
		    2)
	     (progn (setf k (incf i)) x))
	   (progn (incf i) 'a))
     x
     i j k))
  a
  (1 2 a 4)
  3 1 2)

;;; Test that there's a block around the forms in long form defsetf

(defun defsetf.5-accessor (x) (car x))

(defsetf defsetf.5-accessor (y) (val)
  (return-from defsetf.5-accessor `(setf (car ,y) ,val)))

(deftest defsetf.5a
  (let ((x (cons 'a 'b)))
    (values
     (setf (defsetf.5-accessor x) 'c)
     x))
  c (c . b))

;;; Test that the defsetf expansion function is defined in the same
;;; lexical environment that the defsetf appears in

(defun defsetf.6-accessor (x) (car x))

(let ((z 'car))
  (defsetf defsetf.6-accessor (y) (val)
    `(setf (,z ,y) ,val)))

(deftest defsetf.6a
  (let ((x (cons 'a 'b)))
    (values
     (setf (defsetf.6-accessor x) 'c)
     x))
  c (c . b))
