;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 13:27:22 2002
;;;; Contains: Tests of TAGBODY

(in-package :cl-test)

(deftest tagbody.1
  (tagbody)
  nil)

(deftest tagbody.2
  (tagbody 'a)
  nil)

(deftest tagbody.3
  (tagbody (values))
  nil)

(deftest tagbody.4
  (tagbody (values 1 2 3 4 5))
  nil)

(deftest tagbody.5
  (let ((x 0))
    (values
     (tagbody
      (setq x 1)
      (go a)
      (setq x 2)
      a)
     x))
  nil 1)

(deftest tagbody.6
  (let ((x 0))
    (tagbody
     (setq x 1)
     (go a)
     b
     (setq x 2)
     (go c)
     a
     (setq x 3)
     (go b)
     c)
    x)
  2)

;;; Macroexpansion occurs after tag determination
(deftest tagbody.7
  (let ((x 0))
    (macrolet ((%m () 'a))
      (tagbody
       (tagbody
	(go a)
	(%m)
	(setq x 1))
       a ))
    x)
  0)

(deftest tagbody.8
  (let ((x 0))
    (tagbody
     (flet ((%f (y) (setq x y) (go a)))
       (%f 10))
     (setq x 1)
     a)
    x)
  10)

;;; Tag names are in their own name space
(deftest tagbody.9
  (let (result)
    (tagbody
     (flet ((a (x) x))
       (setq result (a 10))
       (go a))
     a)
    result)
  10)

(deftest tagbody.10
  (let (result)
    (tagbody
     (block a
       (setq result 10)
       (go a))
     (setq result 20)
     a)
    result)
  10)

(deftest tagbody.11
  (let (result)
    (tagbody
     (catch 'a
       (setq result 10)
       (go a))
     (setq result 20)
     a)
    result)
  10)

(deftest tagbody.12
  (let (result)
    (tagbody
     (block a
       (setq result 10)
       (return-from a nil))
     (setq result 20)
     a)
    result)
  20)

;;; Test that integers are accepted as go tags

(deftest tagbody.13
  (block done
    (tagbody
     (go around)
     10
     (return-from done 'good)
     around
     (go 10)))
  good)

(deftest tagbody.14
  (block done
    (tagbody
     (go around)
     -10
     (return-from done 'good)
     around
     (go -10)))
  good)

(deftest tagbody.15
  (block done
    (tagbody
     (go around)
     #.(1+ most-positive-fixnum)
     (return-from done 'good)
     around
     (go #.(1+ most-positive-fixnum))))
  good)

(deftest tagbody.16
  (let* ((t1 (1+ most-positive-fixnum))
	 (t2 (1+ most-positive-fixnum))
	 (form `(block done
		  (tagbody
		   (go around)
		   ,t1
		   (return-from done 'good)
		   around
		   (go ,t2)))))
    (eval form))
  good)

;;; Check that macros are not expanded before finding tags
;;; Test for issue TAGBODY-TAG-EXPANSION

(deftest tagbody.17
  (block done
    (tagbody
     (macrolet ((foo () 'tag))
       (let (tag)
	 (tagbody
	  (go tag)
	  (foo)
	  (return-from done :bad))))
     tag
     (return-from done :good)))
  :good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest tagbody.18
  (macrolet ((%m (z) z))
    (tagbody
      (expand-in-current-env (%m :foo))))
  nil)
