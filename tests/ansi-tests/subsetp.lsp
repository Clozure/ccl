;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Apr  1 22:10:54 1998
;;;; Contains: Tests of SUBSETP

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(defvar cons-test-24-var '(78 "z" (8 9)))

(deftest subsetp.1
  (subsetp-with-check (copy-tree '(78)) cons-test-24-var)
  t)

(deftest subsetp.2
  (subsetp-with-check (copy-tree '((8 9))) cons-test-24-var)
  nil)

(deftest subsetp.3
  (subsetp-with-check (copy-tree '((8 9)))
		      cons-test-24-var :test 'equal)
  t)

(deftest subsetp.4
  (subsetp-with-check (list 78 (copy-seq "Z")) cons-test-24-var
		      :test #'equalp)
  t)

(deftest subsetp.5
  (subsetp-with-check (list 1) (list 0 2 3 4)
		      :key #'(lambda (i) (floor (/ i 2))))
  t)

(deftest subsetp.6
  (subsetp-with-check (list 1 6) (list 0 2 3 4)
		      :key #'(lambda (i) (floor (/ i 2))))
  nil)

(deftest subsetp.7
  (subsetp-with-check (list '(a . 10) '(b . 20) '(c . 30))
		      (list '(z . c) '(a . y) '(b . 100) '(e . f)
			    '(c . foo))
		      :key #'car)
  t)

(deftest subsetp.8
  (subsetp-with-check (copy-tree '((a . 10) (b . 20) (c . 30)))
		      (copy-tree '((z . c) (a . y) (b . 100) (e . f)
				   (c . foo)))
		      :key 'car)
  t)

(deftest subsetp.9
  (subsetp-with-check (list 'a 'b 'c)
		      (copy-tree
		       (list '(z . c) '(a . y) '(b . 100) '(e . f)
			     '(c . foo)))
		      :test #'(lambda (e1 e2)
				(eqt e1 (car e2))))
  t)

(deftest subsetp.10
  (subsetp-with-check (list 'a 'b 'c)
		      (copy-tree
		       (list '(z . c) '(a . y) '(b . 100) '(e . f)
			     '(c . foo)))
		      :test #'(lambda (e1 e2)
				(eqt e1 (car e2)))
		      :key nil)
  t)

(deftest subsetp.11
  (subsetp-with-check (list 'a 'b 'c)
		      (copy-tree
		       (list '(z . c) '(a . y) '(b . 100) '(e . f)
			     '(c . foo)))
		      :test-not  #'(lambda (e1 e2)
				     (not (eqt e1 (car e2)))))
  t)

;; Check that it maintains order of arguments

(deftest subsetp.12
  (block fail
    (subsetp-with-check
     (list 1 2 3)
     (list 4 5 6)
     :test #'(lambda (x y)
	       (when (< y x) (return-from fail 'fail))
	       t)))
  t)

(deftest subsetp.13
  (block fail
    (subsetp-with-check
     (list 1 2 3)
     (list 4 5 6)
     :key #'identity
     :test #'(lambda (x y)
	       (when (< y x) (return-from fail 'fail))
	       t)))
  t)

(deftest subsetp.14
  (block fail
    (subsetp-with-check
     (list 1 2 3)
     (list 4 5 6)
     :test-not #'(lambda (x y)
		   (when (< y x) (return-from fail 'fail))
		   nil)))
  t)

(deftest subsetp.15
  (block fail
    (subsetp-with-check
     (list 1 2 3)
     (list 4 5 6)
     :key #'identity
     :test-not #'(lambda (x y)
		   (when (< y x) (return-from fail 'fail))
		   nil)))
  t)

(defharmless subsetp.test-and-test-not.1
  (subsetp '(a b c) '(a g c e b) :test #'eql :test-not #'eql))

(defharmless subsetp.test-and-test-not.3
  (subsetp '(a b c) '(a g c e b) :test-not #'eql :test #'eql))

;;; Order of argument evaluation tests

(deftest subsetp.order.1
  (let ((i 0) x y)
    (values
     (notnot (subsetp (progn (setf x (incf i))
			     '(a b c))
		      (progn (setf y (incf i))
			     '(a b c d))))
     i x y))
  t 2 1 2)

(deftest subsetp.order.2
  (let ((i 0) x y z w)
    (values
     (notnot (subsetp (progn (setf x (incf i))
			     '(a b c))
		      (progn (setf y (incf i))
			     '(a b c d))
		      :test (progn (setf z (incf i)) #'eql)
		      :key  (progn (setf w (incf i)) nil)))
     i x y z w))
  t 4 1 2 3 4)

(deftest subsetp.order.3
  (let ((i 0) x y z w)
    (values
     (notnot (subsetp (progn (setf x (incf i))
			     '(a b c))
		      (progn (setf y (incf i))
			     '(a b c d))
		      :key  (progn (setf z (incf i)) nil)
		      :test (progn (setf w (incf i)) #'eql)))
     i x y z w))
  t 4 1 2 3 4)

;;; Keyword tests

(deftest subsetp.allow-other-keys.1
  (notnot-mv (subsetp '(1 2 3 4) '(0 1 2 3 4 5) :bad t :allow-other-keys 67))
  t)

(deftest subsetp.allow-other-keys.2
  (notnot-mv (subsetp '(1 2 3 4) '(0 1 2 3 4 5)
		   :allow-other-keys #'cons :bad t))
  t)

(deftest subsetp.allow-other-keys.3
  (notnot-mv (subsetp '(1 2 3 4) '(0 1 2 3 4)
		   :allow-other-keys (make-hash-table)
		   :bad t
		   :test #'(lambda (x y) (= (1+ x) y))))
  nil)

(deftest subsetp.allow-other-keys.4
  (notnot-mv (subsetp '(1 2 3 4) '(0 1 2 3 4 5) :allow-other-keys t))
  t)

(deftest subsetp.allow-other-keys.5
  (notnot-mv (subsetp '(1 2 3 4) '(0 1 2 3 4 5) :allow-other-keys nil))
  t)

(deftest subsetp.allow-other-keys.6
  (notnot-mv (subsetp '(1 2 3 4) '(0 1 2 3 4 5)
		   :allow-other-keys t :bad1 t
		   :allow-other-keys nil :bad2 t))
  t)

(deftest subsetp.keywords.7
  (notnot-mv (subsetp '(1 2 3 4) '(0 1 2 3 4)
		   :test #'(lambda (x y) (= (1+ x) y))
		   :test #'eql))
  nil)

(deftest subsetp.keywords.8
  (notnot-mv (subsetp '(1 2 3 4 10) '(0 1 2 3 4)
		   :key nil
		   :key #'(lambda (x) (mod x 2))))
  nil)


;;; Error tests

(deftest subsetp.error.1
  (signals-error (subsetp) program-error)
  t)

(deftest subsetp.error.2
  (signals-error (subsetp nil) program-error)
  t)

(deftest subsetp.error.3
  (signals-error (subsetp nil nil :bad t) program-error)
  t)

(deftest subsetp.error.4
  (signals-error (subsetp nil nil :key) program-error)
  t)

(deftest subsetp.error.5
  (signals-error (subsetp nil nil 1 2) program-error)
  t)

(deftest subsetp.error.6
  (signals-error (subsetp nil nil :bad t :allow-other-keys nil) program-error)
  t)

(deftest subsetp.error.7
  (signals-error (subsetp (list 1 2) (list 3 4) :test #'identity) program-error)
  t)

(deftest subsetp.error.8
  (signals-error (subsetp (list 1 2) (list 3 4) :test-not #'identity) program-error)
  t)

(deftest subsetp.error.9
  (signals-error (subsetp (list 1 2) (list 3 4) :key #'cons) program-error)
  t)

(deftest subsetp.error.10
  (signals-error (subsetp (list 1 2) (list 3 4) :key #'car) type-error)
  t)

(deftest subsetp.error.11
  (signals-error (subsetp (list 1 2 3) (list* 4 5 6)) type-error)
  t)

(deftest subsetp.error.12
  (signals-error (subsetp (list* 1 2 3) (list 1 2 3 4 5 6)) type-error)
  t)

;;; The next two tests previously compared against NIL, but arguably
;;; a conforming implementation is not required to signal an error
;;; in these cases, since it doesn't have to traverse the other list.

(deftest subsetp.error.13
  (check-type-error #'(lambda (x) (subsetp x '(a b))) #'listp)
  nil)

(deftest subsetp.error.14
  (check-type-error #'(lambda (x) (subsetp '(a b) x)) #'listp)
  nil)

