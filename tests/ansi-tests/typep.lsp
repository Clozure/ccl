;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon May 23 07:13:32 2005
;;;; Contains: Tests of TYPEP

(in-package :cl-test)

(deftest typep.error.1
  (signals-error (typep) program-error)
  t)

(deftest typep.error.2
  (signals-error (typep nil) program-error)
  t)

(deftest typep.error.3
  (signals-error (typep nil t nil nil) program-error)
  t)

(deftest typep.error.4
  (signals-error-always (typep nil 'values) error)
  t t)

(deftest typep.error.5
  (signals-error-always (typep nil '(values)) error)
  t t)

(deftest typep.error.6
  (signals-error-always (typep nil '(values t t t t)) error)
  t t)

(deftest typep.error.7
  (signals-error-always (typep nil '(function () t)) error)
  t t)

;;; Non-error tests
;;; Many more tests use typep when testing other functions

(deftest typep-nil-null
  (notnot-mv (typep nil 'null))
  t)

(deftest typep-t-null
  (typep t 'null)
  nil)

;;; Tests of env arguments to typep

(deftest typep.env.1
  (notnot-mv (typep 0 'bit nil))
  t)

(deftest typep.env.2
  (macrolet ((%foo (&environment env)
		   (notnot-mv (typep 0 'bit env))))
    (%foo))
  t)

(deftest typep.env.3
  (macrolet ((%foo (&environment env)
		   (notnot-mv (typep env (type-of env)))))
    (%foo))
  t)

;;; Other typep tests

(deftest typep.1
  (notnot-mv (typep 'a '(eql a)))
  t)

(deftest typep.2
  (notnot-mv (typep 'a '(and (eql a))))
  t)

(deftest typep.3
  (notnot-mv (typep 'a '(or (eql a))))
  t)

(deftest typep.4
  (typep 'a '(eql b))
  nil)

(deftest typep.5
  (typep 'a '(and (eql b)))
  nil)

(deftest typep.6
  (typep 'a '(or (eql b)))
  nil)

(deftest typep.7
  (notnot-mv (typep 'a '(satisfies symbolp)))
  t)

(deftest typep.8
  (typep 10 '(satisfies symbolp))
  nil)

(deftest typep.9
  (let ((class (find-class 'symbol)))
    (notnot-mv (typep 'a class)))
  t)

(deftest typep.10
  (let ((class (find-class 'symbol)))
    (notnot-mv (typep 'a `(and ,class))))
  t)

(deftest typep.11
  (let ((class (find-class 'symbol)))
    (typep 10 class))
  nil)

(deftest typep.12
  (let ((class (find-class 'symbol)))
    (typep 10 `(and ,class)))
  nil)

(deftest typep.13
  (typep 'a '(and symbol integer))
  nil)

(deftest typep.14
  (notnot-mv (typep 'a '(or symbol integer)))
  t)

(deftest typep.15
  (notnot-mv (typep 'a '(or integer symbol)))
  t)

(deftest typep.16
  (let ((c1 (find-class 'number))
	(c2 (find-class 'symbol)))
    (notnot-mv (typep 'a `(or ,c1 ,c2))))
  t)

(deftest typep.17
  (let ((c1 (find-class 'number))
	(c2 (find-class 'symbol)))
    (notnot-mv (typep 'a `(or ,c2 ,c1))))
  t)

(deftest typep.18
  (let ((i 0))
    (values
     (notnot (typep (incf i) '(and (integer 0 10) (integer -5 6))))
     i))
  t 1)

(defun typep.19-fn (reps &optional (prob .5))
  (let* ((vec "abcdefghijklmnopqrstuvwxyz"))
    (flet ((%make-random-type
	    ()
	    `(and character (member ,@(loop for e across vec
					    when (< (random 1.0) prob)
					    collect e)))))
      (loop 
       for t1 = (%make-random-type)
       for t2 = (%make-random-type)
       for t3 = `(and ,t1 ,t2)
       for result1 = (loop for e across vec
			   when (if (typep e t3)
				    (or (not (typep e t1)) (not (typep e t2)))
				  (and (typep e t1) (typep e t2)))
			   collect e)
       repeat reps
       when result1
       nconc (list result1 t1 t2 t3)))))

(eval-when (:load-toplevel) (compile 'typep.19-fn))

(deftest typep.19 (typep.19-fn 1000) nil)

	    
			     
