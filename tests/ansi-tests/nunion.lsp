;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:42:35 2003
;;;; Contains: Tests of NUNION

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest nunion.1
    (nunion nil nil)
  nil)

(deftest nunion.2
    (nunion-with-copy (list 'a) nil)
  (a))

(deftest nunion.3
    (nunion-with-copy (list 'a) (list 'a))
  (a))

(deftest nunion.4
    (nunion-with-copy (list 1) (list 1))
  (1))

(deftest nunion.5
    (let ((x (list 'a 'b)))
      (nunion-with-copy (list x) (list x)))
  ((a b)))

(deftest nunion.6
    (let ((x '(a b c d e f))
	  (y '(z c y a v b)))
      (let ((result (nunion-with-copy x y)))
	(check-union x y result)))
  t)

(deftest nunion.6-a
    (let ((x '(a b c d e f))
	  (y '(z c y a v b)))
      (let ((result (nunion-with-copy x y :test #'eq)))
	(check-union x y result)))
  t)

(deftest nunion.7
    (let ((x '(a b c d e f))
	  (y '(z c y a v b)))
      (let ((result (nunion-with-copy x y :test #'eql)))
	(check-union x y result)))
  t)

(deftest nunion.8
    (let ((x '(a b c d e f))
	  (y '(z c y a v b)))
      (let ((result (nunion-with-copy x y :test #'equal)))
	(check-union x y result)))
  t)

(deftest nunion.9
    (let ((x  '(a b c d e f))
	  (y  '(z c y a v b)))
      (let ((result (nunion-with-copy x y :test-not (complement #'eql))))
	(check-union x y result)))
  t)

(deftest nunion.10
    (let ((x '(a b c d e f))
	  (y '(z c y a v b)))
      (let ((result (nunion-with-copy x y :test-not (complement #'equal))))
	(check-union x y result)))
  t)

(deftest nunion.11
    (let ((x '(a b c d e f))
	  (y '(z c y a v b)))
      (let ((result (nunion-with-copy x y :test-not (complement #'eq))))
	(check-union x y result)))
  t)

(deftest nunion.12
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy x y)))
	(check-union x y result)))
  t)

(deftest nunion.13
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy x y :test #'equal)))
	(check-union x y result)))
  t)

(deftest nunion.14
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy x y :test #'eql)))
	(check-union x y result)))
  t)

(deftest nunion.15
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy x y :test-not (complement #'equal))))
	(check-union x y result)))
  t)

(deftest nunion.16
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy x y :test-not (complement  #'eql))))
	(check-union x y result)))
  t)

(deftest nunion.17
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy-and-key x y #'1+)))
	(check-union x y result)))
  t)

(deftest nunion.18
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy-and-key x y #'1+ :test #'equal)))
	(check-union x y result)))
  t)

(deftest nunion.19
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy-and-key x y #'1+ :test #'eql)))
	(check-union x y result)))
  t)

(deftest nunion.20
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy-and-key x y #'1+
					      :test-not (complement #'equal))))
	(check-union x y result)))
  t)

(deftest nunion.21
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy-and-key x y #'1+
					      :test-not (complement #'equal))))
	(check-union x y result)))
  t)

(deftest nunion.22
  (let ((x '(1 2 3 4 5 6 7))
	(y '(10 19 5 3 17 1001 2)))
    (let ((result (nunion-with-copy-and-key x y nil)))
      (check-union x y result)))
  t)

(deftest nunion.23
  (let ((x '(1 2 3 4 5 6 7))
	(y '(10 19 5 3 17 1001 2)))
    (let ((result (nunion-with-copy-and-key x y '1+)))
      (check-union x y result)))
  t)

;; Do large numbers of random nunions

(deftest nunion.24
  (do-random-nunions 100 100 200)
  nil)

(deftest nunion.25
  (let ((x (shuffle '(1 4 6 10 45 101)))
	(y '(102 5 2 11 44 6)))
    (let ((result (nunion-with-copy x y
				    :test #'(lambda (a b)
					      (<= (abs (- a b)) 1)))))
      (sort
       (sublis
	'((2 . 1) (5 . 4) (11 . 10) (45 . 44) (102 . 101))
	(copy-list result))
       #'<)))
  (1 4 6 10 44 101))

;; Check that nunion uses eql, not equal or eq

(deftest nunion.26
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
	    (nunion-with-copy (list x) (list x)))
	   1))))
  t)

(deftest nunion.27
  (nunion-with-copy (list (copy-seq "aa"))
		    (list (copy-seq "aa")))
  ("aa" "aa"))

(defharmless nunion.test-and-test-not.1
  (nunion (list 1 4 8 10) (list 1 2 3 9 10 13) :test #'eql :test-not #'eql))

(defharmless nunion.test-and-test-not.2
  (nunion (list 1 4 8 10) (list 1 2 3 9 10 13) :test-not #'eql :test #'eql))

;; Check that nunion does not reverse the arguments to :test, :test-not

(deftest nunion.28
    (block fail
      (sort
       (nunion-with-copy
	'(1 2 3)
	'(4 5 6)
	:test #'(lambda (x y)
		  (when (< y x) (return-from fail 'fail))
		  (eql x y)))
       #'<))
  (1 2 3 4 5 6))

(deftest nunion.29
    (block fail
      (sort
       (nunion-with-copy-and-key
	'(1 2 3)
	'(4 5 6)
	#'identity
	:test #'(lambda (x y)
		  (when (< y x) (return-from fail 'fail))
		  (eql x y)))
       #'<))
  (1 2 3 4 5 6))

(deftest nunion.30
    (block fail
      (sort
       (nunion-with-copy
	'(1 2 3)
	'(4 5 6)
	:test-not
	#'(lambda (x y)
	    (when (< y x) (return-from fail 'fail))
	    (not (eql x y))))
       #'<))
  (1 2 3 4 5 6))

(deftest nunion.31
    (block fail
      (sort
       (nunion-with-copy-and-key
	'(1 2 3)
	'(4 5 6)
	#'identity
	:test-not #'(lambda (x y)
		      (when (< y x) (return-from fail 'fail))
		      (not (eql x y))))
       #'<))
  (1 2 3 4 5 6))

;;; Order of evaluation tests

(deftest nunion.order.1
  (let ((i 0) x y)
    (values
     (sort
      (nunion (progn (setf x (incf i)) (copy-list '(1 3 5)))
	      (progn (setf y (incf i)) (copy-list '(2 5 8))))
      #'<)
     i x y))
  (1 2 3 5 8)
  2 1 2)

(deftest nunion.order.2
  (let ((i 0) x y z w)
    (values
     (sort
      (nunion (progn (setf x (incf i)) (copy-list '(1 3 5)))
	      (progn (setf y (incf i)) (copy-list '(2 5 8)))
	      :test (progn (setf z (incf i)) #'eql)
	      :key (progn (setf w (incf i)) #'identity))
      #'<)
     i x y z w))
  (1 2 3 5 8)
  4 1 2 3 4)


(deftest nunion.order.3
  (let ((i 0) x y z w)
    (values
     (sort
      (nunion (progn (setf x (incf i)) (copy-list '(1 3 5)))
	      (progn (setf y (incf i)) (copy-list '(2 5 8)))
	      :key (progn (setf z (incf i)) #'identity)
	      :test (progn (setf w (incf i)) #'eql))
      #'<)
     i x y z w))
  (1 2 3 5 8)
  4 1 2 3 4)

;;; Keyword tests

(deftest nunion.allow-other-keys.1
  (sort (nunion (list 7 9 1 5) (list 10 11 9 20 1 2) :bad t
	       :allow-other-keys "yes")
	#'<)
  (1 2 5 7 9 10 11 20))

(deftest nunion.allow-other-keys.2
  (sort (nunion (list 7 9 1 5) (list 10 11 9 20 1 2)
	       :allow-other-keys t :also-bad t)
	#'<)
  (1 2 5 7 9 10 11 20))

(deftest nunion.allow-other-keys.3
  (sort (nunion (list 1 2 3) (list 1 2 3)
	       :allow-other-keys t :also-bad t
	       :test #'(lambda (x y) (= x (+ y 100))))
	#'<)
  (1 1 2 2 3 3))

(deftest nunion.allow-other-keys.4
  (sort (nunion (list 7 9 1 5) (list 10 11 9 20 1 2)
	       :allow-other-keys t)
	#'<)
  (1 2 5 7 9 10 11 20))

(deftest nunion.allow-other-keys.5
  (sort (nunion (list 7 9 1 5) (list 10 11 9 20 1 2)
	       :allow-other-keys nil)
	#'<)
  (1 2 5 7 9 10 11 20))

(deftest nunion.allow-other-keys.6
  (sort (nunion (list 7 9 1 5) (list 10 11 9 20 1 2)
	       :allow-other-keys t
	       :allow-other-keys nil)
	#'<)
  (1 2 5 7 9 10 11 20))

(deftest nunion.allow-other-keys.7
  (sort (nunion (list 7 9 1 5) (list 10 11 9 20 1 2)
	       :allow-other-keys t
	       :allow-other-keys nil
	       '#:x 1)
	#'<)
  (1 2 5 7 9 10 11 20))

(deftest nunion.keywords.9
  (sort (nunion (list 1 2 3) (list 1 2 3)
	       :test #'(lambda (x y) (= x (+ y 100)))
	       :test #'eql)
	#'<)
  (1 1 2 2 3 3))

;;; Error tests

(deftest nunion.error.1
  (signals-error (nunion) program-error)
  t)

(deftest nunion.error.2
  (signals-error (nunion nil) program-error)
  t)

(deftest nunion.error.3
  (signals-error (nunion nil nil :bad t) program-error)
  t)

(deftest nunion.error.4
  (signals-error (nunion nil nil :key) program-error)
  t)

(deftest nunion.error.5
  (signals-error (nunion nil nil 1 2) program-error)
  t)

(deftest nunion.error.6
  (signals-error (nunion nil nil :bad t :allow-other-keys nil) program-error)
  t)

(deftest nunion.error.7
  (signals-error (nunion (list 1 2) (list 3 4) :test #'identity) program-error)
  t)

(deftest nunion.error.8
  (signals-error (nunion (list 1 2) (list 3 4) :test-not #'identity) program-error)
  t)

(deftest nunion.error.9
  (signals-error (nunion (list 1 2) (list 3 4) :key #'cons) program-error)
  t)

(deftest nunion.error.10
  (signals-error (nunion (list 1 2) (list 3 4) :key #'car) type-error)
  t)

(deftest nunion.error.11
  (signals-error (nunion (list 1 2 3) (list* 4 5 6)) type-error)
  t)

(deftest nunion.error.12
  (signals-error (nunion (list* 1 2 3) (list 4 5 6)) type-error)
  t)

(deftest nunion.error.13
  (check-type-error #'(lambda (x) (nunion x (list 1 2 3))) #'listp)
  nil)

(deftest nunion.error.14
  (check-type-error #'(lambda (x) (nunion (list 1 2 3) x)) #'listp)
  nil)
