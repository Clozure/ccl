;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan  8 07:26:48 2005
;;;; Contains: Tests of DOLIST

(in-package :cl-test)

(deftest dolist.1
  (let ((count 0))
    (dolist (x '(a b nil d)) (incf count))
    count)
  4)

(deftest dolist.2
  (let ((count 0))
    (dolist (x '(a nil c d) count) (incf count)))
  4)

(deftest dolist.3
  (let ((count 0))
    (dolist (x nil count) (incf count)))
  0)

(deftest dolist.4
  (let ((y nil))
    (flet ((%f () (locally (declare (special e))
			   (push e y))))
      (dolist (e '(a b c) (reverse y))
	(declare (special e))
	(%f))))
  (a b c))

;;; Tests that it's a tagbody
(deftest dolist.5
  (let ((even nil)
	(odd nil))
    (dolist (i '(1 2 3 4 5 6 7 8) (values (reverse even)
					  (reverse odd)))
      (when (evenp i) (go even))
      (push i odd)
      (go done)
      even
      (push i even)
      done))
  (2 4 6 8)
  (1 3 5 7))

;;; Test that bindings are not normally special
(deftest dolist.6
  (let ((i 0) (y nil))
    (declare (special i))
    (flet ((%f () i))
      (dolist (i '(1 2 3 4))
	(push (%f) y)))
    y)
  (0 0 0 0))

;;; Test multiple return values

(deftest dolist.7
  (dolist (x '(a b) (values))))

(deftest dolist.8
  (let ((count 0))
    (dolist (x '(a b c) (values count count))
      (incf count)))
  3 3)

;;; Test ability to return, and the scope of the implicit
;;; nil block
(deftest dolist.9
  (block nil
    (eqlt (dolist (x '(a b c))
	    (return 1))
	  1))
  t)

(deftest dolist.10
  (block nil
    (eqlt (dolist (x '(a b c))
	    (return-from nil 1))
	  1))
  t)

(deftest dolist.11
  (block nil
    (dolist (x (return 1)))
    2)
  2)

(deftest dolist.12
  (block nil
    (dolist (x '(a b) (return 1)))
    2)
  2)

;;; Check that binding of element var is visible in the result form
(deftest dolist.13
  (dolist (e '(a b c) e))
  nil)

(deftest dolist.14
  (let ((e 1))
    (dolist (e '(a b c) (setf e 2)))
    e)
  1)

(deftest dolist.15
  (let ((x nil))
    (dolist (e '(a b c d e f))
      (push e x)
      (when (eq e 'c) (return x))))
  (c b a))

;;; Scope of free declarations

(deftest dolist.16
  (block done
    (let ((x :bad))
      (declare (special x))
      (let ((x :good))
	(dolist (e (return-from done x))
	  (declare (special x))))))
  :good)

(deftest dolist.17
  (let ((x :good))
    (declare (special x))
    (let ((x :bad))
      (dolist (e nil x)
	(declare (special x)))))
  :good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest dolist.18
  (let ((result nil))
    (macrolet
     ((%m (z) z))
     (dolist (x (expand-in-current-env (%m '(a b c))) result)
       (push x result))))
  (c b a))

(deftest dolist.19
  (let ((result nil))
    (macrolet
     ((%m (z) z))
     (dolist (x '(a b c) (expand-in-current-env (%m result)))
       (push x result))))
  (c b a))

;;; Error tests

(def-macro-test dolist.error.1
  (dolist (x nil)))

