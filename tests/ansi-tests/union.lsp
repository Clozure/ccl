;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:41:24 2003
;;;; Contains: Tests of UNION

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest union.1
  (union nil nil)
  nil)

(deftest union.2
  (union-with-check (list 'a) nil)
  (a))

(deftest union.3
  (union-with-check (list 'a) (list 'a))
  (a))

(deftest union-4
  (union-with-check (list 1) (list 1))
  (1))

(deftest union.5
  (let ((x (list 'a 'b)))
    (union-with-check (list x) (list x)))
  ((a b)))

(deftest union.6
  (let ((x (copy-list '(a b c d e f)))
	(y (copy-list '(z c y a v b))))
    (let ((result (union-with-check x y)))
      (check-union x y result)))
  t)

(deftest union.6-a
  (let ((x (copy-list '(a b c d e f)))
	(y (copy-list '(z c y a v b))))
    (let ((result (union-with-check x y :test #'eq)))
      (check-union x y result)))
  t)

(deftest union.7
  (let ((x (copy-list '(a b c d e f)))
	(y (copy-list '(z c y a v b))))
    (let ((result (union-with-check x y :test #'eql)))
      (check-union x y result)))
  t)

(deftest union.8
  (let ((x (copy-list '(a b c d e f)))
	(y (copy-list '(z c y a v b))))
    (let ((result (union-with-check x y :test #'equal)))
      (check-union x y result)))
  t)

(deftest union.9
  (let ((x (copy-list '(a b c d e f)))
	(y (copy-list '(z c y a v b))))
    (let ((result (union-with-check x y :test-not (complement #'eql))))
      (check-union x y result)))
  t)

(deftest union.10
  (let ((x (copy-list '(a b c d e f)))
	(y (copy-list '(z c y a v b))))
    (let ((result (union-with-check x y :test-not (complement #'equal))))
      (check-union x y result)))
  t)

(deftest union.11
  (let ((x (copy-list '(a b c d e f)))
	(y (copy-list '(z c y a v b))))
    (let ((result (union-with-check x y :test-not (complement #'eq))))
      (check-union x y result)))
  t)

(deftest union.12
  (let ((x (copy-list '(1 2 3 4 5 6 7)))
	(y (copy-list '(10 19 5 3 17 1001 2))))
    (let ((result (union-with-check x y)))
      (check-union x y result)))
  t)

(deftest union.13
  (let ((x (copy-list '(1 2 3 4 5 6 7)))
	(y (copy-list '(10 19 5 3 17 1001 2))))
    (let ((result (union-with-check x y :test #'equal)))
      (check-union x y result)))
  t)

(deftest union.14
  (let ((x (copy-list '(1 2 3 4 5 6 7)))
	(y (copy-list '(10 19 5 3 17 1001 2))))
    (let ((result (union-with-check x y :test #'eql)))
      (check-union x y result)))
  t)

(deftest union.15
  (let ((x (copy-list '(1 2 3 4 5 6 7)))
	(y (copy-list '(10 19 5 3 17 1001 2))))
    (let ((result (union-with-check x y :test-not (complement #'equal))))
      (check-union x y result)))
  t)

(deftest union.16
  (let ((x (copy-list '(1 2 3 4 5 6 7)))
	(y (copy-list '(10 19 5 3 17 1001 2))))
    (let ((result (union-with-check x y :test-not (complement  #'eql))))
      (check-union x y result)))
  t)

(deftest union.17
  (let ((x (copy-list '(1 2 3 4 5 6 7)))
	(y (copy-list '(10 19 5 3 17 1001 2))))
    (let ((result (union-with-check-and-key x y #'1+)))
      (check-union x y result)))
  t)

(deftest union.18
  (let ((x (copy-list '(1 2 3 4 5 6 7)))
	(y (copy-list '(10 19 5 3 17 1001 2))))
    (let ((result (union-with-check-and-key x y #'1+ :test #'equal)))
      (check-union x y result)))
  t)

(deftest union.19
  (let ((x (copy-list '(1 2 3 4 5 6 7)))
	(y (copy-list '(10 19 5 3 17 1001 2))))
    (let ((result (union-with-check-and-key x y #'1+ :test #'eql)))
      (check-union x y result)))
  t)

(deftest union.20
  (let ((x (copy-list '(1 2 3 4 5 6 7)))
	(y (copy-list '(10 19 5 3 17 1001 2))))
    (let ((result (union-with-check-and-key x y #'1+
					    :test-not (complement #'equal))))
      (check-union x y result)))
  t)

(deftest union.21
  (let ((x (copy-list '(1 2 3 4 5 6 7)))
	(y (copy-list '(10 19 5 3 17 1001 2))))
    (let ((result (union-with-check-and-key x y #'1+
					    :test-not (complement #'equal))))
      (check-union x y result)))
  t)

(deftest union.22
  (let ((x (copy-list '(1 2 3 4 5 6 7)))
	(y (copy-list '(10 19 5 3 17 1001 2))))
    (let ((result (union-with-check-and-key x y nil)))
      (check-union x y result)))
  t)

(deftest union.23
  (let ((x (copy-list '(1 2 3 4 5 6 7)))
	(y (copy-list '(10 19 5 3 17 1001 2))))
    (let ((result (union-with-check-and-key x y '1+)))
      (check-union x y result)))
  t)

;; Do large numbers of random units

(deftest union.24
  (do-random-unions 100 100 200)
  nil)

(deftest union.25
  (let ((x (shuffle '(1 4 6 10 45 101)))
	(y (copy-list '(102 5 2 11 44 6))))
    (let ((result (union-with-check x y
				    :test #'(lambda (a b)
						    (<= (abs (- a b)) 1)))))
      (and
       (not (eqt result 'failed))
       (sort
	(sublis
	 '((2 . 1) (5 . 4) (11 . 10) (45 . 44) (102 . 101))
	 (copy-list result))
	#'<))))
  (1 4 6 10 44 101))

;;; Check that union uses eql, not equal or eq

(deftest union.26
  (let ((x 1000)
	(y 1000))
    (loop
     while (not (typep x 'bignum))
     do (progn
	  (setf x (* x x))
	  (setf y (* y y))))
    (notnot-mv
     (or
      (eqt x y)  ;; if bignums are eq, the test is worthless
      (eql (length
	    (union-with-check
	     (list x) (list x)))
	   1))))
  t)

(deftest union.27
  (union-with-check (list (copy-seq "aa"))
		    (list (copy-seq "aa")))
  ("aa" "aa"))

;; Check that union does not reverse the arguments to :test, :test-not

(deftest union.28
  (block fail
    (sort
     (union-with-check
      (list 1 2 3)
      (list 4 5 6)
      :test #'(lambda (x y)
		(when (< y x) (return-from fail 'fail))
		(eql x y)))
     #'<))
  (1 2 3 4 5 6))

(deftest union.29
  (block fail
    (sort
     (union-with-check-and-key
      (list 1 2 3)
      (list 4 5 6)
      #'identity
      :test #'(lambda (x y)
		(when (< y x) (return-from fail 'fail))
		(eql x y)))
     #'<))
  (1 2 3 4 5 6))

(deftest union.30
  (block fail
    (sort
     (union-with-check
      (list 1 2 3)
      (list 4 5 6)
      :test-not
      #'(lambda (x y)
	  (when (< y x) (return-from fail 'fail))
	  (not (eql x y))))
     #'<))
  (1 2 3 4 5 6))

(deftest union.31
  (block fail
    (sort
     (union-with-check-and-key
      (list 1 2 3)
      (list 4 5 6)
      #'identity
      :test-not #'(lambda (x y)
		    (when (< y x) (return-from fail 'fail))
		    (not (eql x y))))
     #'<))
  (1 2 3 4 5 6))

(defharmless union.test-and-test-not.1
  (union (list 1 4 8 10) (list 1 2 3 9 10 13) :test #'eql :test-not #'eql))

(defharmless union.test-and-test-not.2
  (union (list 1 4 8 10) (list 1 2 3 9 10 13) :test-not #'eql :test #'eql))


;;; Order of evaluation tests

(deftest union.order.1
  (let ((i 0) x y)
    (values
     (sort
      (union (progn (setf x (incf i)) (copy-list '(1 3 5)))
	     (progn (setf y (incf i)) (copy-list '(2 5 8))))
      #'<)
     i x y))
  (1 2 3 5 8)
  2 1 2)

(deftest union.order.2
  (let ((i 0) x y z w)
    (values
     (sort
      (union (progn (setf x (incf i)) (copy-list '(1 3 5)))
	     (progn (setf y (incf i)) (copy-list '(2 5 8)))
	     :test (progn (setf z (incf i)) #'eql)
	     :key (progn (setf w (incf i)) #'identity))
      #'<)
     i x y z w))
  (1 2 3 5 8)
  4 1 2 3 4)


(deftest union.order.3
  (let ((i 0) x y z w)
    (values
     (sort
      (union (progn (setf x (incf i)) (copy-list '(1 3 5)))
	     (progn (setf y (incf i)) (copy-list '(2 5 8)))
	     :key (progn (setf z (incf i)) #'identity)
	     :test (progn (setf w (incf i)) #'eql))
      #'<)
     i x y z w))
  (1 2 3 5 8)
  4 1 2 3 4)

;;; Keyword tests

(deftest union.allow-other-keys.1
  (sort (union (list 7 9 1 5) (list 10 11 9 20 1 2) :bad t
	       :allow-other-keys "yes")
	#'<)
  (1 2 5 7 9 10 11 20))

(deftest union.allow-other-keys.2
  (sort (union (list 7 9 1 5) (list 10 11 9 20 1 2)
	       :allow-other-keys t :also-bad t)
	#'<)
  (1 2 5 7 9 10 11 20))

(deftest union.allow-other-keys.3
  (sort (union (list 1 2 3) (list 1 2 3)
	       :allow-other-keys t :also-bad t
	       :test #'(lambda (x y) (= x (+ y 100))))
	#'<)
  (1 1 2 2 3 3))

(deftest union.allow-other-keys.4
  (sort (union (list 7 9 1 5) (list 10 11 9 20 1 2)
	       :allow-other-keys t)
	#'<)
  (1 2 5 7 9 10 11 20))

(deftest union.allow-other-keys.5
  (sort (union (list 7 9 1 5) (list 10 11 9 20 1 2)
	       :allow-other-keys nil)
	#'<)
  (1 2 5 7 9 10 11 20))

(deftest union.allow-other-keys.6
  (sort (union (list 7 9 1 5) (list 10 11 9 20 1 2)
	       :allow-other-keys t
	       :allow-other-keys nil)
	#'<)
  (1 2 5 7 9 10 11 20))

(deftest union.allow-other-keys.7
  (sort (union (list 7 9 1 5) (list 10 11 9 20 1 2)
	       :allow-other-keys t
	       :allow-other-keys nil
	       '#:x 1)
	#'<)
  (1 2 5 7 9 10 11 20))

(deftest union.keywords.9
  (sort (union (list 1 2 3) (list 1 2 3)
	       :test #'(lambda (x y) (= x (+ y 100)))
	       :test #'eql)
	#'<)
  (1 1 2 2 3 3))

(def-fold-test union.fold.1 (union '(a b c d e) '(d x y a w c)))

;;; Error tests

(deftest union.error.1
  (signals-error (union) program-error)
  t)

(deftest union.error.2
  (signals-error (union nil) program-error)
  t)

(deftest union.error.3
  (signals-error (union nil nil :bad t) program-error)
  t)

(deftest union.error.4
  (signals-error (union nil nil :key) program-error)
  t)

(deftest union.error.5
  (signals-error (union nil nil 1 2) program-error)
  t)

(deftest union.error.6
  (signals-error (union nil nil :bad t :allow-other-keys nil) program-error)
  t)

(deftest union.error.7
  (signals-error (union (list 1 2) (list 3 4) :test #'identity) program-error)
  t)

(deftest union.error.8
  (signals-error (union (list 1 2) (list 3 4) :test-not #'identity) program-error)
  t)

(deftest union.error.9
  (signals-error (union (list 1 2) (list 3 4) :key #'cons) program-error)
  t)

(deftest union.error.10
  (signals-error (union (list 1 2) (list 3 4) :key #'car) type-error)
  t)

(deftest union.error.11
  (signals-error (union (list 1 2 3) (list* 4 5 6)) type-error)
  t)

(deftest union.error.12
  (signals-error (union (list* 1 2 3) (list 4 5 6)) type-error)
  t)

;;; The next two tests used to check for union with NIL, but arguably
;;; that goes beyond the 'be prepared to signal an error' requirement,
;;; since a union algorithm doesn't have to traverse one argument
;;; if the other is the empty list.

(deftest union.error.13
  (check-type-error #'(lambda (x) (union x '(1 2))) #'listp)
  nil)

(deftest union.error.14
  (check-type-error #'(lambda (x) (union '(1 2) x)) #'listp)
  nil)
