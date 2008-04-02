;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 13:04:02 2002
;;;; Contains: Tests of CATCH and THROW

(in-package :cl-test)

(deftest catch.1
  (catch 'foo)
  nil)

(deftest catch.2
  (catch 'foo 'a)
  a)

(deftest catch.3
  (catch 'foo (values)))

(deftest catch.4
  (catch 'foo (values 1 2 3))
  1 2 3)

(deftest catch.5
  (catch 'foo 'a (throw 'foo 'b) 'c)
  b)

(deftest catch.6
  (let ((tag1 (1+ most-positive-fixnum))
	(tag2 (1+ most-positive-fixnum)))
    (if (eqt tag1 tag2)
	'good
      (catch tag1
	(catch tag2 (throw tag1 'good))
	'bad)))
  good)

(deftest catch.7
  (catch 'foo 'a (throw 'foo (values)) 'c))

(deftest catch.8
  (catch 'foo 'a (throw 'foo (values 1 2 3)) 'c)
  1 2 3)

(deftest catch.9
  (let ((i 0))
    (catch (progn (incf i) 'foo)
      (assert (eql i 1))
      (throw (progn (incf i 2) 'foo) i)))
  3)

(deftest catch.10
  (flet ((%f (x) (throw 'foo x)))
    (catch 'foo
      (%f 'good)
      'bad))
  good)

(defun catch.11-fn (x) (throw 'foo x))

(deftest catch.11
  (catch 'foo
    (catch.11-fn 'good)
    'bad)
  good)

(deftest catch.12
  (labels ((%f (x) (throw 'foo x)))
    (catch 'foo
      (%f 'good)
      'bad))
  good)

;;; No implicit tagbody
(deftest catch.13
  (block done
    (tagbody
     (catch 'foo
       (go 10)
       10
       (return-from done 'bad))
     10
     (return-from done 'good)))
  good)

;;; Macros are expanded in the appropriate environment

(deftest catch.14
  (macrolet ((%m (z) z))
	    (catch 'foo (expand-in-current-env (%m :good))))
  :good)

(deftest catch.15
  (macrolet ((%m (z) z))
	    (catch 'foo (throw (expand-in-current-env (%m 'foo)) :good) :bad))
  :good)

(deftest catch.16
  (macrolet ((%m (z) z))
	    (catch 'foo (throw 'foo (expand-in-current-env (%m :good))) :bad))
  :good)

(deftest throw-error
  (signals-error (throw (gensym) nil) control-error)
  t)
