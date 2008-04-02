;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 22:51:25 2002
;;;; Contains: Tests for TYPECASE

(in-package :cl-test)

(deftest typecase.1
  (typecase 1 (integer 'a) (t 'b))
  a)

(deftest typecase.2
  (typecase 1 (symbol 'a))
  nil)

(deftest typecase.3
  (typecase 1 (symbol 'a) (t 'b))
  b)

(deftest typecase.4
  (typecase 1 (t (values))))

(deftest typecase.5
  (typecase 1 (integer (values)) (t 'a)))

(deftest typecase.6
  (typecase 1 (bit 'a) (integer 'b))
  a)

(deftest typecase.7
  (typecase 1 (otherwise 'a))
  a)

(deftest typecase.8
   (typecase 1 (t (values 'a 'b 'c)))
   a b c)

(deftest typecase.9
   (typecase 1 (integer (values 'a 'b 'c)) (t nil))
   a b c)

(deftest typecase.10
  (let ((x 0))
    (values
     (typecase 1
       (bit     (incf x)   'a)
       (integer (incf x 2) 'b)
       (t       (incf x 4) 'c))
     x))
  a 1)

(deftest typecase.11
   (typecase 1 (otherwise 'a))
   a)

(deftest typecase.12
  (typecase 1 (integer) (t 'a))
  nil)

(deftest typecase.13
  (typecase 1 (symbol 'a) (t))
  nil)

(deftest typecase.14
  (typecase 1 (symbol 'a) (otherwise))
  nil)

(deftest typecase.15
  (typecase 'a
    (number 'bad)
    (#.(find-class 'symbol nil) 'good))
  good)

(deftest typecase.16
  (block done
    (tagbody
     (typecase 'a (symbol (go 10)
			  10
			  (return-from done 'bad)))
     10
     (return-from done 'good)))
  good)

(deftest typecase.17
  (block done
    (tagbody
     (typecase 'a
       (integer 'bad)
       (t (go 10)
	  10
	  (return-from done 'bad)))
     10
     (return-from done 'good)))
  good)

(deftest typecase.18
  (loop for x in '(a 1 1.4 "c")
	collect (typecase x
		  (t :good)
		  (otherwise :bad)))
  (:good :good :good :good))

;;; A randomized test

(deftest typecase.19
  (let* ((u (coerce *universe* 'vector))
	 (len1 (length u))
	 (types (coerce *cl-all-type-symbols* 'vector))
	 (len2 (length types)))
    (loop
     for n = (random 10)
     for my-types = (loop repeat n collect (elt types (random len2)))
     for val = (elt u (random len1))
     for i = (position val my-types :test #'typep)
     for form = `(typecase ',val
		   ,@(loop for i from 0 for type in my-types collect `(,type ,i))
		   (otherwise nil))
     for j = (eval form)
     repeat 1000
     unless (eql i j)
     collect (list n my-types val i form j)))
  nil)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest typecase.20
  (macrolet
   ((%m (z) z))
   (typecase (expand-in-current-env (%m 2))
	     ((integer 0 1) :bad1)
	     ((integer 2 10) :good)
	     (t :bad2)))
  :good)

(deftest typecase.21
  (macrolet
   ((%m (z) z))
   (typecase 2
	     ((integer 0 1) (expand-in-current-env (%m :bad1)))
	     ((integer 2 10) (expand-in-current-env (%m :good)))
	     (t (expand-in-current-env (%m :bad2)))))
  :good)

;;; Error cases

(deftest typecase.error.1
  (signals-error (funcall (macro-function 'typecase)) program-error)
  t)

(deftest typecase.error.2
  (signals-error (funcall (macro-function 'typecase)
			   '(typecase t)) program-error)
  t)

(deftest typecase.error.3
  (signals-error (funcall (macro-function 'typecase)
			   '(typecase t)
			   nil nil) program-error)
  t)



