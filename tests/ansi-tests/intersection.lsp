;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:39:19 2003
;;;; Contains: Tests of INTERSECTION

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest intersection.1
  (intersection nil nil)
  nil)

(deftest intersection.2
  (intersection (loop for i from 1 to 100 collect i) nil)
  nil)

(deftest intersection.3
  (intersection nil (loop for i from 1 to 100 collect i))
  nil)

(deftest intersection.4
  (let* ((x (copy-list '(a 1 c 7 b 4 3 z)))
	 (xcopy (make-scaffold-copy x))
	 (y (copy-list '(3 y c q z a 18)))
	 (ycopy (make-scaffold-copy y))
	 (result (intersection x y)))
    (and
     (check-scaffold-copy x xcopy)
     (check-scaffold-copy y ycopy)
     (+
      (loop
       for e in x count
       (and (member e y)
	    (not (member e result))))
      (loop
       for e in result count
       (or (not (member e x))
	   (not (member e y))))
      (loop
       for hd on result count
       (and (consp hd)
	    (member (car hd) (cdr hd)))))))
  0)

(deftest intersection.5
  (let* ((x (copy-list '(a a a)))
	 (xcopy (make-scaffold-copy x))
	 (y (copy-list '(a a a b b b)))
	 (ycopy (make-scaffold-copy y))
	 (result (intersection x y)))
    (and
     (check-scaffold-copy x xcopy)
     (check-scaffold-copy y ycopy)
     (member 'a result)
     (not (member 'b result))))
  t)

(deftest intersection.6
  (intersection (list 1000000000000 'a 'b 'c)
		(list (1+ 999999999999) 'd 'e 'f))
  (1000000000000))

(deftest intersection.7
  (intersection (list 'a 10 'b 17)
		(list 'c 'd 4 'e 'f 10 1 13 'z))
  (10))

(deftest intersection.8
  (intersection (list 'a (copy-seq "aaa") 'b)
		(list 'd (copy-seq "aaa") 'e))
  nil)

(deftest intersection.9
  (intersection (list 'a (copy-seq "aaa") 'b)
		(list 'd (copy-seq "aaa") 'e)
		:test #'equal)
  ("aaa"))

;; Same as 9, but with a symbol function designator for :test
(deftest intersection.9-a
  (intersection (list 'a (copy-seq "aaa") 'b)
		(list 'd (copy-seq "aaa") 'e)
		:test 'equal)
  ("aaa"))

(deftest intersection.9-b
  (intersection (list 'a (copy-seq "aaa") 'b)
		(list 'd (copy-seq "aaa") 'e)
		:test-not #'(lambda (p q) (not (equal p q))))
  ("aaa"))

(deftest intersection.10
  (equalt
   (sort
    (intersection (loop
		   for i from 0 to 1000 by 3
		   collect i)
		  (loop
		   for i from 0 to 1000 by 7
		   collect i))
    #'<)
   (loop for i from 0 to 1000 by 21 collect i))
  t)

(deftest intersection.11
  (equalt
   (sort
    (intersection (loop
		   for i from 0 to 999 by 5
		   collect i)
		  (loop
		   for i from 0 to 999 by 7
		   collect i)
		  :test #'(lambda (a b)
			    (and (eql a b)
				 (= (mod a 3) 0))))
    #'<)
   (loop for i from 0 to 999 by (* 3 5 7) collect i))
  t)

(deftest intersection.11-a
  (equalt
   (sort
    (intersection (loop
		   for i from 0 to 999 by 5
		   collect i)
		  (loop
		   for i from 0 to 999 by 7
		   collect i)
		  :test-not
		  #'(lambda (a b)
		      (not (and (eql a b)
				(= (mod a 3) 0)))))
    #'<)
   (loop for i from 0 to 999 by (* 3 5 7) collect i))
  t)

;;
;; Do large numbers of random intersection tests
;;

(deftest intersection.12
  (intersection-12-body 100 100)
  nil)


;;
;; :key argument
;;

(deftest intersection.13
  (let ((x (copy-list '(0 5 8 13 31 42)))
	(y (copy-list '(3 5 42 0 7 100 312 33))))
    (equalt
     (sort (copy-list (intersection x y)) #'<)
     (sort (copy-list (intersection x y :key #'1+)) #'<)))
  t)

;; Same as 13, but with a symbol function designator for :key
(deftest intersection.13-a
  (let ((x (copy-list '(0 5 8 13 31 42)))
	(y (copy-list '(3 5 42 0 7 100 312 33))))
    (equalt
     (sort (copy-list (intersection x y)) #'<)
     (sort (copy-list (intersection x y :key '1+)) #'<)))
  t)

;; Test that a nil key argument is ignored

(deftest intersection.14
  (let
      ((result (intersection (copy-list '(a b c d))
			     (copy-list '(e c f b g))
			     :key nil)))
    (and
     (member 'b result)
     (member 'c result)
     (every #'(lambda (x) (member x '(b c))) result)
     t))
  t)

;; Test that intersection preserves the order of arguments to :test, :test-not

(deftest intersection.15
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (block fail
      (intersection
       list1 list2
       :test
       #'(lambda (x y)
	   (when (< y x) (return-from fail 'fail))
	   (eql x y)))))
  (4))

(deftest intersection.16
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (block fail
      (intersection
       list1 list2
       :key #'identity
       :test
       #'(lambda (x y)
	   (when (< y x) (return-from fail 'fail))
	   (eql x y)))))
  (4))

(deftest intersection.17
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (block fail
      (intersection
       list1 list2
       :test-not
       #'(lambda (x y)
	   (when (< y x) (return-from fail 'fail))
	   (not (eql x y))))))
  (4))

(deftest intersection.18
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (block fail
      (intersection
       list1 list2
       :key #'identity
       :test-not
       #'(lambda (x y)
	   (when (< y x) (return-from fail 'fail))
	   (not (eql x y))))))
  (4))

(defharmless intersection.test-and-test-not.1
  (intersection '(a b c) '(a c e) :test #'eql :test-not #'eql))

(defharmless intersection.test-and-test-not.2
  (intersection '(a b c) '(a c e) :test-not #'eql :test #'eql))

;;; Order of argument evaluation tests

(deftest intersection.order.1
  (let ((i 0) x y)
    (values
     (intersection (progn (setf x (incf i)) (list 'a 'b))
		   (progn (setf y (incf i)) (list 'c 'd)))
     i x y))
  nil 2 1 2)

(deftest intersection.order.2
  (let ((i 0) x y)
    (values
     (intersection (progn (setf x (incf i)) (list 'a 'b))
		   (progn (setf y (incf i)) (list 'c 'd))
		   :test #'eq)
     i x y))
  nil 2 1 2)

(deftest intersection.order.3
  (let ((i 0) x y z w)
    (values
     (intersection (progn (setf x (incf i)) (list 'a 'b))
		   (progn (setf y (incf i)) (list 'c 'd))
		   :test (progn (setf z (incf i)) #'eq)
		   :test (progn (setf w (incf i))
				(complement #'eq)))
     i x y z w))
  nil 4 1 2 3 4)

(deftest intersection.order.4
  (let ((i 0) x y z w)
    (values
     (intersection (progn (setf x (incf i)) (list 'a 'b))
		   (progn (setf y (incf i)) (list 'c 'd))
		   :test (progn (setf z (incf i)) #'eq)
		   :key (progn (setf w (incf i)) #'identity))
     i x y z w))
  nil 4 1 2 3 4)

(deftest intersection.order.5
  (let ((i 0) x y z w)
    (values
     (intersection (progn (setf x (incf i)) (list 'a 'b))
		   (progn (setf y (incf i)) (list 'c 'd))
		   :key (progn (setf z (incf i)) #'identity)
		   :test (progn (setf w (incf i)) #'eq))
     i x y z w))
  nil 4 1 2 3 4)


;;; Keyword tests

(deftest intersection.allow-other-keys.1
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (intersection list1 list2 :bad t :allow-other-keys 1))
  (4))

(deftest intersection.allow-other-keys.2
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (intersection list1 list2 :allow-other-keys :foo :also-bad t))
  (4))

(deftest intersectionallow-other-keys.3
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (intersection list1 list2 :allow-other-keys :foo :also-bad t
		  :test #'(lambda (x y) (= x (1+ y)))))
  nil)

(deftest intersection.allow-other-keys.4
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (intersection list1 list2 :allow-other-keys t))
  (4))

(deftest intersection.allow-other-keys.5
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (intersection list1 list2 :allow-other-keys nil))
  (4))

(deftest intersection.allow-other-keys.6
  (let ((list1 (list 1 2 3 4))
	(list2 (list 4 5 6 7)))
    (intersection list1 list2 :allow-other-keys t
		  :allow-other-keys nil :bad t))
  (4))

(deftest intersection.allow-other-keys.7
  (sort
   (let ((list1 (list 1 2 3 4))
	 (list2 (list 4 5 6 7)))
     (intersection list1 list2 :allow-other-keys t
		   :allow-other-keys nil
		   :test #'(lambda (x y) (eql x (1- y)))))
   #'<)
  (3 4))

(deftest intersection.keywords.8
  (sort
   (let ((list1 (list 1 2 3 4))
	 (list2 (list 4 5 6 7)))
     (intersection list1 list2
		   :test #'(lambda (x y) (eql x (1- y)))
		   :test #'eql))
   #'<)
  (3 4))

(def-fold-test intersection.fold.1 (intersection '(a b c d e f) '(d w a x b y)))

;;; Error tests

(deftest intersection.error.1
  (signals-error (intersection) program-error)
  t)

(deftest intersection.error.2
  (signals-error (intersection nil) program-error)
  t)

(deftest intersection.error.3
  (signals-error (intersection nil nil :bad t) program-error)
  t)

(deftest intersection.error.4
  (signals-error (intersection nil nil :key) program-error)
  t)

(deftest intersection.error.5
  (signals-error (intersection nil nil 1 2) program-error)
  t)

(deftest intersection.error.6
  (signals-error (intersection nil nil :bad t :allow-other-keys nil)
		 program-error)
  t)

(deftest intersection.error.7
  (signals-error (intersection '(a b c) '(d e f) :test #'identity)
		 program-error)
  t)

(deftest intersection.error.8
  (signals-error (intersection '(a b c) '(d e f) :test-not #'identity)
		 program-error)
  t)

(deftest intersection.error.9
  (signals-error (intersection '(a b c) '(d e f) :key #'cons)
		 program-error)
  t)

(deftest intersection.error.10
  (signals-error (intersection '(a b c) '(d e f) :key #'car)
		 type-error)
  t)

(deftest intersection.error.11
  (signals-error (intersection '(a b c) '(d e f . g))
		 type-error)
  t)

(deftest intersection.error.12
  (signals-error (intersection '(a b . c) '(d e f))
		 type-error)
  t)

(deftest intersection.error.13
  (check-type-error #'(lambda (x) (intersection x '(a b c))) #'listp)
  nil)

(deftest intersection.error.14
  (check-type-error #'(lambda (x) (intersection '(a b c) x)) #'listp)
  nil)