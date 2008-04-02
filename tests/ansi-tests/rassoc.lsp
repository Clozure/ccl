;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:33:49 2003
;;;; Contains: Tests of RASSOC

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest rassoc.1
  (rassoc nil nil)
  nil)

(deftest rassoc.2
  (rassoc nil '(nil))
  nil)

(deftest rassoc.3
  (rassoc nil (rev-assoc-list '(nil (nil . 2) (a . b))))
  (2 . nil))

(deftest rassoc.4
  (rassoc nil '((a . b) (c . d)))
  nil)

(deftest rassoc.5
  (rassoc 'a '((b . a)))
  (b . a))

(deftest rassoc.6
  (rassoc 'a (rev-assoc-list '((:a . b) (#:a . c) (a . d) (a . e) (z . f))))
  (d . a))

(deftest rassoc.7
  (let* ((x (copy-tree (rev-assoc-list '((a . b) (b . c) (c . d)))))
	 (xcopy (make-scaffold-copy x))
	 (result (rassoc 'b x)))
    (and
     (eqt result (second x))
     (check-scaffold-copy x xcopy)))
  t)

(deftest rassoc.8
  (rassoc 1 (rev-assoc-list '((0 . a) (1 . b) (2 . c))))
  (b . 1))

(deftest rassoc.9
  (rassoc (copy-seq "abc")
	  (rev-assoc-list '((abc . 1) ("abc" . 2) ("abc" . 3))))
  nil)

(deftest rassoc.10
  (rassoc (copy-list '(a))
	  (copy-tree (rev-assoc-list '(((a) b) ((a) (c))))))
  nil)

(deftest rassoc.11
  (let ((x (list 'a 'b)))
    (rassoc x
	    (rev-assoc-list `(((a b) c) (,x . d) (,x . e) ((a b) 1)))))
  (d a b))


(deftest rassoc.12
  (rassoc #\e
	  (copy-tree
	   (rev-assoc-list '(("abefd" . 1) ("aevgd" . 2) ("edada" . 3))))
	  :key #'(lambda (x) (schar x 1)))
  (2 . "aevgd"))

(deftest rassoc.13
  (rassoc nil
	  (copy-tree
	   (rev-assoc-list
	    '(((a) . b) ( nil . c ) ((nil) . d))))
	  :key #'car)
  (c))

(deftest rassoc.14
  (rassoc (copy-seq "abc")
	  (copy-tree
	   (rev-assoc-list
	    '((abc . 1) ("abc" . 2) ("abc" . 3))))
	  :test #'equal)
  (2 . "abc"))

(deftest rassoc.15
  (rassoc (copy-seq "abc")
	  (copy-tree
	   (rev-assoc-list
	    '((abc . 1) ("abc" . 2) ("abc" . 3))))
	  :test #'equalp)
  (2 . "abc"))

(deftest rassoc.16
  (rassoc (copy-list '(a))
	  (copy-tree
	   (rev-assoc-list '(((a) b) ((a) (c)))))
	  :test #'equal)
  ((b) a))

(deftest rassoc.17
  (rassoc (copy-seq "abc")
	  (copy-tree
	   (rev-assoc-list
	    '((abc . 1) (a . a) (b . b) ("abc" . 2) ("abc" . 3))))
	  :test-not (complement #'equalp))
  (2 . "abc"))

(deftest rassoc.18
  (rassoc 'a 
	  (copy-tree
	   (rev-assoc-list
	    '((a . d)(b . c))))
	  :test-not #'eq)
  (c . b))

(deftest rassoc.19
  (rassoc 'a
	  (copy-tree
	   (rev-assoc-list
	    '((a . d)(b . c))))
	  :test (complement #'eq))
  (c . b))

(deftest rassoc.20
  (rassoc "a"
	  (copy-tree
	   (rev-assoc-list
	    '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))))
	  :key #'(lambda (x) (and (stringp x) (string-downcase x)))
	  :test #'equal)
  (6 . "A"))

(deftest rassoc.21
  (rassoc "a"
	  (copy-tree
	   (rev-assoc-list
	    '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))))
	  :key #'(lambda (x) (and (stringp x) x))
	  :test #'equal)
  (3 . "a"))

(deftest rassoc.22
  (rassoc "a"
	  (copy-tree
	   (rev-assoc-list
	    '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))))
	  :key #'(lambda (x) (and (stringp x) (string-downcase x)))
	  :test-not (complement #'equal))
  (6 . "A"))

(deftest rassoc.23
  (rassoc "a"
	  (copy-tree
	   (rev-assoc-list
	    '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))))
	  :key #'(lambda (x) (and (stringp x) x))
	  :test-not (complement #'equal))
  (3 . "a"))

;; Check that it works when test returns a true value
;; other than T

(deftest rassoc.24
  (rassoc 'a
	  (copy-tree
	   (rev-assoc-list
	    '((b . 1) (a . 2) (c . 3))))
	  :test #'(lambda (x y) (and (eqt x y) 'matched)))
  (2 . a))

;; Check that the order of the arguments to :test is correct

(deftest rassoc.25
  (block fail
    (rassoc 'a '((1 . b) (2 . c) (3 . a))
	    :test #'(lambda (x y)
		      (unless (eqt x 'a) (return-from fail 'fail))
		      (eqt x y))))
  (3 . a))

(deftest rassoc.26
  (rassoc 10 '((a . 1) (b . 5) (c . 10) (d . 15) (e . 40))
	  :test #'<)
  (d . 15))

(deftest rassoc.27
  (rassoc 10 '((a . 1) (b . 5) (c . 10) (d . 15) (e . 40))
	  :test-not #'>=)
  (d . 15))

(defharmless rassoc.test-and-test-not.1
  (rassoc 'a '((x . b) (y . a) (z . c)) :test #'eql :test-not #'eql))

(defharmless rassoc.test-and-test-not.2
  (rassoc 'a '((x . b) (y . a) (z . c)) :test-not #'eql :test #'eql))

;;; Order of argument evaluation

(deftest rassoc.order.1
  (let ((i 0) x y)
    (values
     (rassoc (progn (setf x (incf i)) 'c)
	     (progn (setf y (incf i)) '((1 . a) (2 . b) (3 . c) (4 . c))))
     i x y))
  (3 . c) 2 1 2)

(deftest rassoc.order.2
  (let ((i 0) x y z)
    (values
     (rassoc (progn (setf x (incf i)) 'c)
	     (progn (setf y (incf i)) '((1 . a) (2 . b) (3 . c) (4 . c)))
	     :test (progn (setf z (incf i)) #'eql))
     i x y z))
  (3 . c) 3 1 2 3)

(deftest rassoc.order.3
  (let ((i 0) x y)
    (values
     (rassoc (progn (setf x (incf i)) 'c)
	    (progn (setf y (incf i)) '((1 . a) (2 . b) (3 . c) (4 . c)))
	    :test #'eql)
     i x y))
  (3 . c) 2 1 2)

(deftest rassoc.order.4
  (let ((i 0) x y z w)
    (values
     (rassoc (progn (setf x (incf i)) 'c)
	    (progn (setf y (incf i)) '((1 . a) (2 . b) (3 . c) (4 . c)))
	    :key (progn (setf z (incf i)) #'identity)
	    :key (progn (setf w (incf i)) #'not))
     i x y z w))
  (3 . c) 4 1 2 3 4)

;;; Keyword tests

(deftest rassoc.allow-other-keys.1
  (rassoc 'b '((1 . a) (2 . b) (3 . c)) :bad t :allow-other-keys t)
  (2 . b))

(deftest rassoc.allow-other-keys.2
  (rassoc 'b '((1 . a) (2 . b) (3 . c)) :allow-other-keys t :bad t)
  (2 . b))

(deftest rassoc.allow-other-keys.3
  (rassoc 'a '((1 . a) (2 . b) (3 . c)) :allow-other-keys t :bad t
	  :test-not #'eql)
  (2 . b))

(deftest rassoc.allow-other-keys.4
  (rassoc 'b '((1 . a) (2 . b) (3 . c)) :allow-other-keys t)
  (2 . b))

(deftest rassoc.allow-other-keys.5
  (rassoc 'b '((1 . a) (2 . b) (3 . c)) :allow-other-keys nil)
  (2 . b))

(deftest rassoc.keywords.6
  (rassoc 'b '((1 . a) (2 . b) (3 . c))
	  :test #'eql :test (complement #'eql))
  (2 . b))

;;; Error tests

(deftest rassoc.error.1
  (signals-error (rassoc) program-error)
  t)

(deftest rassoc.error.2
  (signals-error (rassoc nil) program-error)
  t)

(deftest rassoc.error.3
  (signals-error (rassoc nil nil :bad t) program-error)
  t)

(deftest rassoc.error.4
  (signals-error (rassoc nil nil :key) program-error)
  t)

(deftest rassoc.error.5
  (signals-error (rassoc nil nil 1 1) program-error)
  t)

(deftest rassoc.error.6
  (signals-error (rassoc nil nil :bad t :allow-other-keys nil) program-error)
  t)

(deftest rassoc.error.7
  (signals-error (rassoc 'a '((b . a)(c . d)) :test #'identity) program-error)
  t)

(deftest rassoc.error.8
  (signals-error (rassoc 'a '((b . a)(c . d)) :test-not #'identity) program-error)
  t)

(deftest rassoc.error.9
  (signals-error (rassoc 'a '((b . a)(c . d)) :key #'cons) program-error)
  t)

(deftest rassoc.error.10
  (signals-error (rassoc 'z '((a . b) . c)) type-error)
  t)

(deftest rassoci.error.11
  (check-type-error #'(lambda (x) (rassoc 'a x)) #'listp)
  nil)
