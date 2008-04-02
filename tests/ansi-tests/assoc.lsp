;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:27:20 2003
;;;; Contains: Tests of ASSOC

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest assoc.1
    (assoc nil nil)
  nil)

(deftest assoc.2
    (assoc nil '(nil))
  nil)

(deftest assoc.3
    (assoc nil '(nil (nil . 2) (a . b)))
  (nil . 2))

(deftest assoc.4
    (assoc nil '((a . b) (c . d)))
  nil)

(deftest assoc.5
    (assoc 'a '((a . b)))
  (a . b))

(deftest assoc.6
    (assoc 'a '((:a . b) (#:a . c) (a . d) (a . e) (z . f)))
  (a . d))

(deftest assoc.7
    (let* ((x (copy-tree '((a . b) (b . c) (c . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (assoc 'b x)))
      (and
       (eqt result (second x))
       (check-scaffold-copy x xcopy)))
  t)

(deftest assoc.8
    (assoc 1 '((0 . a) (1 . b) (2 . c)))
  (1 . b))

(deftest assoc.9
    (assoc (copy-seq "abc")
	   '((abc . 1) ("abc" . 2) ("abc" . 3)))
  nil)

(deftest assoc.10
    (assoc (copy-list '(a)) (copy-tree '(((a) b) ((a) (c)))))
  nil)

(deftest assoc.11
    (let ((x (list 'a 'b)))
      (assoc x `(((a b) c) (,x . d) (,x . e) ((a b) 1))))
  ((a b) . d))


(deftest assoc.12
    (assoc #\e '(("abefd" . 1) ("aevgd" . 2) ("edada" . 3))
	   :key #'(lambda (x) (schar x 1)))
  ("aevgd" . 2))

(deftest assoc.13
    (assoc nil '(((a) . b) ( nil . c ) ((nil) . d))
	   :key #'car)
  (nil . c))

(deftest assoc.14
    (assoc (copy-seq "abc")
	   '((abc . 1) ("abc" . 2) ("abc" . 3))
	   :test #'equal)
  ("abc" . 2))

(deftest assoc.15
    (assoc (copy-seq "abc")
	   '((abc . 1) ("abc" . 2) ("abc" . 3))
	   :test #'equalp)
  ("abc" . 2))

(deftest assoc.16
    (assoc (copy-list '(a)) (copy-tree '(((a) b) ((a) (c))))
	   :test #'equal)
  ((a) b))

(deftest assoc.17
    (assoc (copy-seq "abc")
	   '((abc . 1) (a . a) (b . b) ("abc" . 2) ("abc" . 3))
	   :test-not (complement #'equalp))
  ("abc" . 2))

(deftest assoc.18
    (assoc 'a '((a . d)(b . c)) :test-not #'eq)
  (b . c))
     
(deftest assoc.19
    (assoc 'a '((a . d)(b . c)) :test (complement #'eq))
  (b . c))

(deftest assoc.20
    (assoc "a" '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))
	   :key #'(lambda (x) (and (stringp x) (string-downcase x)))
	   :test #'equal)
  ("A" . 6))

(deftest assoc.21
    (assoc "a" '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))
	   :key #'(lambda (x) (and (stringp x) x))
	   :test #'equal)
  ("a" . 3))

(deftest assoc.22
    (assoc "a" '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))
	   :key #'(lambda (x) (and (stringp x) (string-downcase x)))
	   :test-not (complement #'equal))
  ("A" . 6))

(deftest assoc.23
    (assoc "a" '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))
	   :key #'(lambda (x) (and (stringp x) x))
	   :test-not (complement #'equal))
  ("a" . 3))

;; Check that it works when test returns a true value
;; other than T

(deftest assoc.24
    (assoc 'a '((b . 1) (a . 2) (c . 3))
	   :test #'(lambda (x y) (and (eqt x y) 'matched)))
  (a . 2))

;; Check that the order of the arguments to test is correct

(deftest assoc.25
    (block fail
      (assoc 'a '((b . 1) (c . 2) (a . 3))
	     :test #'(lambda (x y)
		       (unless (eqt x 'a) (return-from fail 'fail))
		       (eqt x y))))
  (a . 3))

;;; Order of test arguments

(deftest assoc.26
  (assoc 10 '((1 a) (5 b) (8 c) (11 d) (12 e)) :test #'<)
  (11 d))

(deftest assoc.27
  (assoc 10 '((1 a) (5 b) (8 c) (11 d) (12 e)) :test-not #'>=)
  (11 d))

;;; Special cases: the nil key does not match the nil pair

(deftest assoc.30
  (let () (assoc nil '((a . b) nil (c . d) (nil . e) (nil . f) nil (g . h))))
  (nil . e))

(deftest assoc.31
  (let () (assoc nil '((a . b) nil (c . d) (nil . e) (nil . f) nil (g . h))
		 :test #'eq))
  (nil . e))

;;; :test & :test-not together are harmless

(defharmless assoc.test-and-test-not.1
  (assoc 'a '((a . 1) (b . 2)) :test #'eql :test-not #'eql))

(defharmless assoc.test-and-test-not.2
  (assoc 'a '((a . 1) (b . 2)) :test-not #'eql :test #'eql))

;;; Order of argument evaluation

(deftest assoc.order.1
  (let ((i 0) x y)
    (values
     (assoc (progn (setf x (incf i)) 'c)
	    (progn (setf y (incf i)) '((a . 1) (b . 2) (c . 3) (d . 4))))
     i x y))
  (c . 3) 2 1 2)

(deftest assoc.order.2
  (let ((i 0) x y z)
    (values
     (assoc (progn (setf x (incf i)) 'c)
	    (progn (setf y (incf i)) '((a . 1) (b . 2) (c . 3) (d . 4)))
	    :test (progn (setf z (incf i)) #'eq))
     i x y z))
  (c . 3) 3 1 2 3)

(deftest assoc.order.3
  (let ((i 0) x y)
    (values
     (assoc (progn (setf x (incf i)) 'c)
	    (progn (setf y (incf i)) '((a . 1) (b . 2) (c . 3) (d . 4)))
	    :test #'eq)
     i x y))
  (c . 3) 2 1 2)

(deftest assoc.order.4
  (let ((i 0) x y z w)
    (values
     (assoc (progn (setf x (incf i)) 'c)
	    (progn (setf y (incf i)) '((a . 1) (b . 2) (c . 3) (d . 4)))
	    :key (progn (setf z (incf i)) #'identity)
	    :key (progn (setf w (incf i)) #'not))
     i x y z w))
  (c . 3) 4 1 2 3 4)

;;; Keyword tests

(deftest assoc.allow-other-keys.1
  (assoc 'b '((a . 1) (b . 2) (c . 3)) :bad t :allow-other-keys t)
  (b . 2))

(deftest assoc.allow-other-keys.2
  (assoc 'b '((a . 1) (b . 2) (c . 3)) :allow-other-keys t :also-bad t)
  (b . 2))

(deftest assoc.allow-other-keys.3
  (assoc 'b '((a . 1) (b . 2) (c . 3)) :allow-other-keys t :also-bad t
	 :test-not #'eql)
  (a . 1))

(deftest assoc.allow-other-keys.4
  (assoc 'b '((a . 1) (b . 2) (c . 3)) :allow-other-keys t)
  (b . 2))

(deftest assoc.allow-other-keys.5
  (assoc 'b '((a . 1) (b . 2) (c . 3)) :allow-other-keys nil)
  (b . 2))

(deftest assoc.keywords.6
  (assoc 'b '((a . 1) (b . 2) (c . 3)) :key #'identity :key #'null)
  (b . 2))

(deftest assoc.keywords.7
  (assoc 'b '((a . 1) (b . 2) (c . 3)) :key nil :key #'null)
  (b . 2))


(deftest assoc.error.1
  (signals-error (assoc) program-error)
  t)

(deftest assoc.error.2
  (signals-error (assoc nil) program-error)
  t)

(deftest assoc.error.3
  (signals-error (assoc nil nil :bad t) program-error)
  t)

(deftest assoc.error.4
  (signals-error (assoc nil nil :key) program-error)
  t)

(deftest assoc.error.5
  (signals-error (assoc nil nil 1 1) program-error)
  t)

(deftest assoc.error.6
  (signals-error (assoc nil nil :bad t :allow-other-keys nil)
		 program-error)
  t)

(deftest assoc.error.7
  (signals-error (assoc 'a '((a . b)) :test #'identity)
		 program-error)
  t)

(deftest assoc.error.8
  (signals-error (assoc 'a '((a . b)) :test-not #'identity)
		 program-error)
  t)

(deftest assoc.error.9
  (signals-error (assoc 'a '((a . b)) :key #'cons)
		 program-error)
  t)

(deftest assoc.error.10
  (signals-error (assoc 'z '((a . b) . c))
		 type-error)
  t)

(deftest assoc.error.11
  (signals-error (assoc 'z '((a . b) :bad (c . d)))
		 type-error)
  t)

(deftest assoc.error.12
  (signals-type-error x 'y (assoc 'x x))
  t)

