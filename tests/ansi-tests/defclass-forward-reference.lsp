;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Apr  2 22:53:27 2003
;;;; Contains: Tests for definitions of classes with forward references

(in-package :cl-test)

(deftest defclass.forward-ref.1
  (let ((c1 (gensym))
	(c2 (gensym)))
    (let ((class1 (eval `(defclass ,c1 (,c2) nil))))
      (if (not (typep class1 'class))
	  1
	(let ((class2 (eval `(defclass ,c2 nil nil))))
	  (if (not (typep class2 'class))
	      2
	    (let ((i1 (make-instance c1))
		  (i2 (make-instance c2)))
	      (cond
	       ((not (typep i1 c1))     3)
	       ((not (typep i1 class1)) 4)
	       ((not (typep i1 c2))     5)
	       ((not (typep i1 class2)) 6)
	       ((typep i2 c1)           7)
	       ((typep i2 class1)       8)
	       ((not (typep i2 c2))     9)
	       ((not (typep i2 class2)) 10)
	       (t 'good))))))))
  good)

(deftest defclass.forward-ref.2
  (let ((c1 (gensym))
	(c2 (gensym))
	(c3 (gensym)))
    (let ((class1 (eval `(defclass ,c1 (,c2 ,c3) nil))))
      (if (not (typep class1 'class))
	  1
	(let ((class2 (eval `(defclass ,c2 nil nil))))
	  (if (not (typep class2 'class))
	      2
	    (let ((class3 (eval `(defclass ,c3 nil nil))))
	      (if (not (typep class3 'class))
		  3
		(let ((i1 (make-instance c1))
		      (i2 (make-instance c2))
		      (i3 (make-instance c3)))
		  (cond
		   ((not (typep i1 c1))     4)
		   ((not (typep i1 class1)) 5)
		   ((not (typep i1 c2))     6)
		   ((not (typep i1 class2)) 7)
		   ((not (typep i1 c3))     8)
		   ((not (typep i1 class3)) 9)
		   ((typep i2 c1)           10)
		   ((typep i2 class1)       11)
		   ((typep i3 c1)           12)
		   ((typep i3 class1)       13)
		   ((not (typep i2 c2))     14)
		   ((not (typep i2 class2)) 15)
		   ((not (typep i3 c3))     16)
		   ((not (typep i3 class3)) 17)
		   ((typep i2 c3)           18)
		   ((typep i2 class3)       19)
		   ((typep i3 c2)           20)
		   ((typep i3 class2)       21)
		   (t 'good))))))))))
  good)

(deftest defclass.forward-ref.3
  (let ((c1 (gensym))
	(c2 (gensym))
	(c3 (gensym)))
    (let ((class1 (eval `(defclass ,c1 (,c2) nil))))
      (if (not (typep class1 'class))
	  1
	(let ((class2 (eval `(defclass ,c2 (,c3) nil))))
	  (if (not (typep class2 'class))
	      2
	    (let ((class3 (eval `(defclass ,c3 nil nil))))
	      (if (not (typep class3 'class))
		  3
		(let ((i1 (make-instance c1))
		      (i2 (make-instance c2))
		      (i3 (make-instance c3)))
		  (cond
		   ((not (typep i1 c1))     4)
		   ((not (typep i1 class1)) 5)
		   ((not (typep i1 c2))     6)
		   ((not (typep i1 class2)) 7)
		   ((not (typep i1 c3))     8)
		   ((not (typep i1 class3)) 9)
		   ((typep i2 c1)           10)
		   ((typep i2 class1)       11)
		   ((typep i3 c1)           12)
		   ((typep i3 class1)       13)
		   ((not (typep i2 c2))     14)
		   ((not (typep i2 class2)) 15)
		   ((not (typep i3 c3))     16)
		   ((not (typep i3 class3)) 17)
		   ((not (typep i2 c3))     18)
		   ((not (typep i2 class3)) 19)
		   ((typep i3 c2)           20)
		   ((typep i3 class2)       21)
		   (t 'good))))))))))
  good)

(deftest defclass.forward-ref.4
  (block nil
    (let ((c1 (gensym))
	  (c2 (gensym))
	  (c3 (gensym))
	  (c4 (gensym))
	  (c5 (gensym)))
      (unless (typep (eval `(defclass ,c4 nil nil)) 'class)
	(return 1))
      (unless (typep (eval `(defclass ,c5 nil nil)) 'class)
	(return 2))
      (unless (typep (eval `(defclass ,c1 (,c2 ,c3) nil)) 'class)
	(return 3))
      (unless (typep (eval `(defclass ,c2 (,c4 ,c5) nil)) 'class)
	(return 4))
      (handler-case
       (eval `(progn
		(defclass ,c3 (,c5 ,c4) nil)
		(make-instance ',c1)))
       (error () :good))))
  :good)
