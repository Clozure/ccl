;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:32:50 2003
;;;; Contains: Tests of SUBLIS

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest sublis.1
  (check-sublis '((a b) g (d e 10 g h) 15 . g)
		'((e . e2) (g . 17)))
  ((a b) 17 (d e2 10 17 h) 15 . 17))

(deftest sublis.2
  (check-sublis '(f6 10 (f4 (f3 (f1 a b) (f1 a p)) (f2 a b)))
		'(((f1 a b) . (f2 a b)) ((f2 a b) . (f1 a b)))
		:test #'equal)
  (f6 10 (f4 (f3 (f2 a b) (f1 a p)) (f1 a b))))

(deftest sublis.3
  (check-sublis '(10 ((10 20 (a b c) 30)) (((10 20 30 40))))
		'((30 . "foo")))
  (10 ((10 20 (a b c) "foo")) (((10 20 "foo" 40)))))

(deftest sublis.4
  (check-sublis (sublis
		 (copy-tree '((a . 2) (b . 4) (c . 1)))
		 (copy-tree '(a b c d e (a b c a d b) f)))
		'((t . "yes"))
		:key #'(lambda (x) (and (typep x 'integer)
					(evenp x))))
  ("yes" "yes" 1 d e ("yes" "yes" 1 "yes" d "yes") f))

(deftest sublis.5
  (check-sublis '("fee" (("fee" "Fie" "foo"))
		  fie ("fee" "fie"))
		`((,(copy-seq "fie") . #\f)))
  ("fee" (("fee" "Fie" "foo")) fie ("fee" "fie")))

(deftest sublis.6
  (check-sublis '("fee" fie (("fee" "Fie" "foo") 1)
		  ("fee" "fie"))
		`((,(copy-seq "fie") . #\f))
		:test 'equal)
  ("fee" fie (("fee" "Fie" "foo") 1) ("fee" #\f)))

(deftest sublis.7
  (check-sublis '(("aa" a b)
		  (z "bb" d)
		  ((x . "aa")))
		`((,(copy-seq "aa") . 1)
		  (,(copy-seq "bb") . 2))
		:test 'equal
		:key #'(lambda (x) (if (consp x) (car x)
				     '*not-present*)))
  (1 (z . 2) ((x . "aa"))))

;; Check that a null key arg is ignored.

(deftest sublis.8
  (check-sublis 
   '(1 2 a b)
   '((1 . 2) (a . b))
   :key nil)
  (2 2 b b))

(deftest sublis.9
  (check-sublis  (list 0 3 8 20)
		 '((1 . x) (5 . y) (10 . z))
		:test #'(lambda (x y) (and (realp x) (realp y) (< x y))))
  (x y z 20))

(deftest sublis.10
  (check-sublis  (list 0 3 8 20)
		 '((1 . x) (5 . y) (10 . z))
		:test-not
		#'(lambda (x y) (not (and (realp x) (realp y) (< x y)))))
  (x y z 20))

(defharmless sublis.test-and-test-not.1
  (sublis '((a . 1) (b . 2)) (list 'a 'b 'c 'd)
	   :test #'eql :test-not #'eql))

(defharmless sublis.test-and-test-not.2
  (sublis '((a . 1) (b . 2)) (list 'a 'b 'c 'd)
	   :test-not #'eql :test #'eql))

;;; Order of argument evaluation
(deftest sublis.order.1
  (let ((i 0) w x y z)
    (values
     (sublis
      (progn (setf w (incf i))
	     '((a . z)))
      (progn (setf x (incf i))
	     (copy-tree '(a b c d)))
      :test (progn (setf y (incf i)) #'eql)
      :key (progn (setf z (incf i)) #'identity))
     i w x y z))
  (z b c d)
  4 1 2 3 4)

(deftest sublis.order.2
  (let ((i 0) w x y z)
    (values
     (sublis
      (progn (setf w (incf i))
	     '((a . z)))
      (progn (setf x (incf i))
	     (copy-tree '(a b c d)))
      :key (progn (setf y (incf i)) #'identity)
      :test-not (progn (setf z (incf i)) (complement #'eql))
      )
     i w x y z))
  (z b c d)
  4 1 2 3 4)

;;; Const fold tests

(def-fold-test sublis.fold.1 (sublis '((a . b)) '(a x y . a)))


;;; Keyword tests

(deftest sublis.allow-other-keys.1
  (sublis nil 'a :bad t :allow-other-keys t)
  a)

(deftest sublis.allow-other-keys.2
  (sublis nil 'a :allow-other-keys t :bad t)
  a)

(deftest sublis.allow-other-keys.3
  (sublis nil 'a :allow-other-keys t)
  a)

(deftest sublis.allow-other-keys.4
  (sublis nil 'a :allow-other-keys nil)
  a)

(deftest sublis.allow-other-keys.5
  (sublis nil 'a :allow-other-keys t :allow-other-keys t :bad t)
  a)

(deftest sublis.keywords.6
  (sublis '((1 . a)) (list 0 1 2) :key #'(lambda (x) (if (numberp x) (1+ x) x))
	  :key #'identity)
  (a 1 2))


;; Argument error cases

(deftest sublis.error.1
  (signals-error (sublis) program-error)
  t)

(deftest sublis.error.2
  (signals-error (sublis nil) program-error)
  t)

(deftest sublis.error.3
  (signals-error (sublis nil 'a :test) program-error)
  t)

(deftest sublis.error.4
  (signals-error (sublis nil 'a :bad-keyword t) program-error)
  t)

(deftest sublis.error.5
  (signals-error (sublis '((a . 1) (b . 2))
			  (list 'a 'b 'c 'd)
			  :test #'identity)
		 program-error)
  t)

(deftest sublis.error.6
  (signals-error (sublis '((a . 1) (b . 2))
			  (list 'a 'b 'c 'd)
			  :key #'cons)
		 program-error)
  t)

(deftest sublis.error.7
  (signals-error (sublis '((a . 1) (b . 2))
			  (list 'a 'b 'c 'd)
			  :test-not #'identity)
		 program-error)
  t)

(deftest sublis.error.8
  (signals-error (sublis '((a . 1) . bad)
			  (list 'a 'b 'c 'd))
		 type-error)
  t)

(deftest sublis.shared
  (let* ((shared-piece (list 'a 'b))
	 (a (list shared-piece shared-piece)))
    (check-sublis a '((a . b) (b . a))))
  ((b a) (b a)))
