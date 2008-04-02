;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:40:02 2003
;;;; Contains: Tests of NINTERSECTION

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest nintersection.1
  (nintersection nil nil)
  nil)

(deftest nintersection.2
  (nintersection (loop for i from 1 to 100 collect i) nil)
  nil)

(deftest nintersection.3
  (nintersection-with-check nil (loop for i from 1 to 100 collect i))
  nil)

(deftest nintersection.4
  (let* ((x (copy-list '(a 1 c 7 b 4 3 z)))
	 (xc (copy-list x))
	 (y (copy-list '(3 y c q z a 18)))
	 (result (nintersection-with-check xc y)))
    (and
     (not (eqt result 'failed))
     (+
      (loop for e in x count
	    (and (member e y)
		 (not (member e result))))
      (loop for e in result count
	    (or (not (member e x)) (not (member e y))))
      (loop for hd on result count
	    (and (consp hd)
		 (member (car hd) (cdr hd)))))))
  0)

(deftest nintersection.5
  (let* ((x (copy-list '(a a a)))
	 (y (copy-list '(a a a b b b)))
	 (result (nintersection-with-check x y)))
    (and
     (not (eqt result 'failed))
     (member 'a result)
     (not (member 'b result))))
  t)

(deftest nintersection.6
  (nintersection-with-check
   (list 1000000000000 'a 'b 'c)
   (list (1+ 999999999999) 'd 'e 'f))
  (1000000000000))

(deftest nintersection.7
  (nintersection-with-check (list 'a 10 'b 17)
			    (list 'c 'd 4 'e 'f 10 1 13 'z))
  (10))

(deftest nintersection.8
  (nintersection-with-check
   (list 'a (copy-seq "aaa") 'b)
   (list 'd (copy-seq "aaa") 'e))
  nil)

(deftest nintersection.9
  (nintersection-with-check
   (list 'a (copy-seq "aaa") 'b)
   (list 'd (copy-seq "aaa") 'e)
   :test #'equal)
  ("aaa"))

(deftest nintersection.9-a
  (nintersection-with-check
   (list 'a (copy-seq "aaa") 'b)
   (list 'd (copy-seq "aaa") 'e)
   :test 'equal)
  ("aaa"))

(deftest nintersection.9-b
  (nintersection
   (list 'a (copy-seq "aaa") 'b)
   (list 'd (copy-seq "aaa") 'e)
   :test-not #'(lambda (p q) (not (equal p q))))
  ("aaa"))

(deftest nintersection.10
  (equalt
   (sort
    (let ((result
	   (nintersection-with-check
	    (loop for i from 0 to 1000 by 3 collect i)
	    (loop for i from 0 to 1000 by 7 collect i))))
      (if (eqt result 'failed) () result))
    #'<)
   (loop for i from 0 to 1000 by 21 collect i))
  t)

(deftest nintersection.11
  (equalt
   (sort
    (let ((result
	   (nintersection-with-check
	    (loop for i from 0 to 999 by 5 collect i)
	    (loop for i from 0 to 999 by 7 collect i)
	    :test #'(lambda (a b)
		      (and (eql a b)
			   (= (mod a 3) 0))))))
      (if (eqt result 'failed) () result))
    #'<)
   (loop
    for i from 0 to 999 by (* 3 5 7) collect i))
  t)

(deftest nintersection.12
  (nintersection-12-body 100 100)
  nil)

;; Key argument

(deftest nintersection.13
  (let ((x '(0 5 8 13 31 42))
	(y (copy-list '(3 5 42 0 7 100 312 33))))
    (equalt
     (sort (copy-list (nintersection
		       (copy-list x) y)) #'<)
     (sort (copy-list (nintersection
		       (copy-list x) y :key #'1+)) #'<)))
  t)

;; Check that a nil key argument is ignored

(deftest nintersection.14
  (let
      ((result (nintersection
		(copy-list '(a b c d))
		(copy-list '(e c f b g))
		:key nil)))
    (and
     (member 'b result)
     (member 'c result)
     (every #'(lambda (x) (member x '(b c))) result)
     t))
  t) 

;; Test that nintersection preserves the order of arguments to :test, :test-not

(deftest nintersection.15
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (block fail
      (nintersection
       list1 list2
       :test
       #'(lambda (x y)
	   (when (< y x) (return-from fail 'fail))
	   (eql x y)))))
  (4))

(deftest nintersection.16
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (block fail
      (nintersection
       list1 list2
       :key #'identity
       :test
       #'(lambda (x y)
	   (when (< y x) (return-from fail 'fail))
	   (eql x y)))))
  (4))

(deftest nintersection.17
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (block fail
      (nintersection
       list1 list2
       :test-not
       #'(lambda (x y)
	   (when (< y x) (return-from fail 'fail))
	   (not (eql x y))))))
  (4))

(deftest nintersection.18
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (block fail
      (nintersection
       list1 list2
       :key #'identity
       :test-not
       #'(lambda (x y)
	   (when (< y x) (return-from fail 'fail))
	   (not (eql x y))))))
  (4))

(defharmless nintersection.test-and-test-not.1
  (nintersection (list 'a 'b 'c) (list 'a 'c 'e) :test #'eql :test-not #'eql))

(defharmless nintersection.test-and-test-not.2
  (nintersection (list 'a 'b 'c) (list 'a 'c 'e) :test-not #'eql :test #'eql))

;;; Order of argument evaluation tests

(deftest nintersection.order.1
  (let ((i 0) x y)
    (values
     (nintersection (progn (setf x (incf i)) (list 'a 'b))
		   (progn (setf y (incf i)) (list 'c 'd)))
     i x y))
  nil 2 1 2)

(deftest nintersection.order.2
  (let ((i 0) x y)
    (values
     (nintersection (progn (setf x (incf i)) (list 'a 'b))
		   (progn (setf y (incf i)) (list 'c 'd))
		   :test #'eq)
     i x y))
  nil 2 1 2)

(deftest nintersection.order.3
  (let ((i 0) x y z w)
    (values
     (nintersection (progn (setf x (incf i)) (list 'a 'b))
		   (progn (setf y (incf i)) (list 'c 'd))
		   :test (progn (setf z (incf i)) #'eq)
		   :test (progn (setf w (incf i))
				(complement #'eq)))
     i x y z w))
  nil 4 1 2 3 4)

(deftest nintersection.order.4
  (let ((i 0) x y z w)
    (values
     (nintersection (progn (setf x (incf i)) (list 'a 'b))
		   (progn (setf y (incf i)) (list 'c 'd))
		   :test (progn (setf z (incf i)) #'eq)
		   :key (progn (setf w (incf i)) #'identity))
     i x y z w))
  nil 4 1 2 3 4)

(deftest nintersection.order.5
  (let ((i 0) x y z w)
    (values
     (nintersection (progn (setf x (incf i)) (list 'a 'b))
		   (progn (setf y (incf i)) (list 'c 'd))
		   :key (progn (setf z (incf i)) #'identity)
		   :test (progn (setf w (incf i)) #'eq))
     i x y z w))
  nil 4 1 2 3 4)

;;; Keyword tests

(deftest nintersection.allow-other-keys.1
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (nintersection list1 list2 :bad t :allow-other-keys 1))
  (4))

(deftest nintersection.allow-other-keys.2
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (nintersection list1 list2 :allow-other-keys :foo :also-bad t))
  (4))

(deftest nintersection.allow-other-keys.3
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (nintersection list1 list2 :allow-other-keys :foo :also-bad t
		  :test #'(lambda (x y) (= x (1+ y)))))
  nil)

(deftest nintersection.allow-other-keys.4
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (nintersection list1 list2 :allow-other-keys t))
  (4))

(deftest nintersection.allow-other-keys.5
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (nintersection list1 list2 :allow-other-keys nil))
  (4))

(deftest nintersection.allow-other-keys.6
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (nintersection list1 list2 :allow-other-keys t
		  :allow-other-keys nil :bad t))
  (4))

(deftest nintersection.allow-other-keys.7
  (sort
   (let ((list1 (list 1 2 3 4))
	 (list2 (list 4 5 6 7)))
     (nintersection list1 list2 :allow-other-keys t
		   :allow-other-keys nil
		   :test #'(lambda (x y) (eql x (1- y)))))
   #'<)
  (3 4))

(deftest nintersection.keywords.8
  (sort
   (let ((list1 (list 1 2 3 4))
	 (list2 (list 4 5 6 7)))
     (nintersection list1 list2
		   :test #'(lambda (x y) (eql x (1- y)))
		   :test #'eql))
   #'<)
  (3 4))

(deftest nintersection.allow-other-keys.9
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (nintersection list1 list2 :allow-other-keys :foo :also-bad t
		  :test #'(lambda (x y) (= x (1+ y)))))
  nil)

(deftest nintersection.error.1
  (signals-error (nintersection) program-error)
  t)

(deftest nintersection.error.2
  (signals-error (nintersection nil) program-error)
  t)

(deftest nintersection.error.3
  (signals-error (nintersection nil nil :bad t) program-error)
  t)

(deftest nintersection.error.4
  (signals-error (nintersection nil nil :key) program-error)
  t)

(deftest nintersection.error.5
  (signals-error (nintersection nil nil 1 2) program-error)
  t)

(deftest nintersection.error.6
  (signals-error (nintersection nil nil :bad t :allow-other-keys nil) program-error)
  t)

(deftest nintersection.error.7
  (signals-error (nintersection (list 1 2 3) (list 4 5 6) :test #'identity) program-error)
  t)

(deftest nintersection.error.8
  (signals-error (nintersection (list 1 2 3) (list 4 5 6) :test-not #'identity) program-error)
  t)

(deftest nintersection.error.9
  (signals-error (nintersection (list 1 2 3) (list 4 5 6) :key #'cons) program-error)
  t)

(deftest nintersection.error.10
  (signals-error (nintersection (list 1 2 3) (list 4 5 6) :key #'car) type-error)
  t)

(deftest nintersection.error.11
  (signals-error (nintersection (list 1 2 3) (list* 4 5 6 7)) type-error)
  t)

(deftest nintersection.error.12
  (signals-error (nintersection (list* 1 2 3) (list 4 5 6)) type-error)
  t)

(deftest nintersection.error.13
  (check-type-error #'(lambda (x) (nintersection x (copy-seq '(a b c)))) #'listp)
  nil)

(deftest nintersection.error.14
  (check-type-error #'(lambda (x) (nintersection (copy-seq '(a b c)) x)) #'listp)
  nil)