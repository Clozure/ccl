;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:45:46 2003
;;;; Contains: Tests of SET-EXCLUSIVE-OR

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest set-exclusive-or.1
  (set-exclusive-or nil nil)
  nil)

(deftest set-exclusive-or.2
  (let ((result
	 (set-exclusive-or-with-check '(a b c) nil)))
    (check-set-exclusive-or '(a b c) nil result))
  t)

(deftest set-exclusive-or.3
  (let ((result
	 (set-exclusive-or-with-check '(a b c d e f) '(f b d))))
    (check-set-exclusive-or '(a b c d e f) '(f b d) result))
  t)

(deftest set-exclusive-or.4
  (sort
   (copy-list
    (set-exclusive-or-with-check (shuffle '(1 2 3 4 5 6 7 8))
				 '(10 101 4 74 2 1391 7 17831)))
   #'<)
  (1 3 5 6 8 10 74 101 1391 17831))

(deftest set-exclusive-or.5
  (check-set-exclusive-or
   nil
   '(a b c d e f g h)
   (set-exclusive-or-with-check nil '(a b c d e f g h)))
  t)

(deftest set-exclusive-or.6
  (set-exclusive-or-with-check '(a b c d e) '(d a b e)
			       :key nil)
  (c))

(deftest set-exclusive-or.7
  (set-exclusive-or-with-check '(a b c d e) '(d a b e) :test #'eq)
  (c))

(deftest set-exclusive-or.7-a
  (set-exclusive-or-with-check '(d a b e) '(a b c d e) :test #'eq)
  (c))

(deftest set-exclusive-or.8
  (set-exclusive-or-with-check '(a b c d e) '(d a b e) :test #'eql)
  (c))

(deftest set-exclusive-or.8-a
  (set-exclusive-or-with-check '(e d b a) '(a b c d e) :test #'eql)
  (c))

(deftest set-exclusive-or.8-b
  (set-exclusive-or-with-check '(a b c d e) '(d a b e)
			       :test-not (complement #'eql))
  (c))

(deftest set-exclusive-or.9
  (set-exclusive-or-with-check '(a b c d e) '(d a b e) :test #'equal)
  (c))

(deftest set-exclusive-or.10
  (set-exclusive-or-with-check '(a b c d e) '(d a b e)
			       :test 'eq)
  (c))

(deftest set-exclusive-or.11
  (set-exclusive-or-with-check '(a b c d e) '(d a b e)
			       :test 'eql)
  (c))

(deftest set-exclusive-or.12
  (set-exclusive-or-with-check '(a b c d e) '(d a b e)
			       :test 'equal)
  (c))

;;; (deftest set-exclusive-or.13
;;;    (do-random-set-exclusive-ors 100 100)
;;;  nil)

(deftest set-exclusive-or.14
  (set-exclusive-or-with-check '((a . 1) (b . 2) (c . 3012))
			       '((a . 10) (c . 3))
			       :key 'car)
  ((b . 2)))

(deftest set-exclusive-or.15
  (set-exclusive-or-with-check '((a . xx) (b . 2) (c . 3))
			       '((a . 1) (c . 3313))
			       :key #'car)
  ((b . 2)))

(deftest set-exclusive-or.16
  (set-exclusive-or-with-check '((a . xx) (b . 2) (c . 3))
			       '((a . 1) (c . 3313))
			       :key #'car
			       :test-not (complement #'eql))
  ((b . 2)))

;;
;; Check that set-exclusive-or does not invert
;; the order of the arguments to the test function
;;
(deftest set-exclusive-or.17
  (let ((list1 '(a b c d))
	(list2 '(e f g h)))
    (block fail
      (notnot-mv
       (set-exclusive-or-with-check
	list1 list2
	:test #'(lambda (s1 s2)
		  (when (or (member s1 list2)
			    (member s2 list1))
		    (return-from fail 'failed)))))))
  t)

(deftest set-exclusive-or.17-a
  (let ((list1 '(a b c d))
	(list2 '(e f g h)))
    (block fail
      (notnot-mv
       (set-exclusive-or-with-check
	list1 list2
	:key #'identity
	:test #'(lambda (s1 s2)
		  (when (or (member s1 list2)
			    (member s2 list1))
		    (return-from fail 'failed)))))))
  t)

(deftest set-exclusive-or.18
  (let ((list1 '(a b c d))
	(list2 '(e f g h)))
    (block fail
      (notnot-mv
       (set-exclusive-or-with-check
	list1 list2
	:test-not
	#'(lambda (s1 s2)
	    (when (or (member s1 list2)
		      (member s2 list1))
	      (return-from fail 'failed))
	    t)))))
  t)

(deftest set-exclusive-or.18-a
  (let ((list1 '(a b c d))
	(list2 '(e f g h)))
    (block fail
      (notnot-mv
       (set-exclusive-or-with-check
	list1 list2
	:key #'identity
	:test-not
	#'(lambda (s1 s2)
	    (when (or (member s1 list2)
		      (member s2 list1))
	      (return-from fail 'failed))
	    t)))))
  t)

(defharmless set-exclusive-or.test-and-test-not.1
  (set-exclusive-or (list 1 2 3 4) (list 1 7 3 8)
		     :test #'eql :test-not #'eql))

(defharmless set-exclusive-or.test-and-test-not.2
  (set-exclusive-or (list 1 2 3 4) (list 1 7 3 8)
		     :test-not #'eql :test #'eql))


;;; Order of argument evaluation tests

(deftest set-exclusive-or.order.1
  (let ((i 0) x y)
    (values
     (sort
      (set-exclusive-or (progn (setf x (incf i))
			       (list 1 2 3 4))
			(progn (setf y (incf i))
			       (list 1 3 6 10)))
      #'<)
     i x y))
  (2 4 6 10) 2 1 2)

(deftest set-exclusive-or.order.2
  (let ((i 0) x y z)
    (values
     (sort
      (set-exclusive-or (progn (setf x (incf i))
			       (list 1 2 3 4))
			(progn (setf y (incf i))
			       (list 1 3 6 10))
			:test (progn (setf z (incf i))
				     #'eql))
      #'<)
     i x y z))
  (2 4 6 10) 3 1 2 3)

(deftest set-exclusive-or.order.3
  (let ((i 0) x y z w)
    (values
     (sort
      (set-exclusive-or (progn (setf x (incf i))
			       (list 1 2 3 4))
			(progn (setf y (incf i))
			       (list 1 3 6 10))
			:test (progn (setf z (incf i))
				     #'eql)
			:key (progn (setf w (incf i)) nil))
      #'<)
     i x y z w))
  (2 4 6 10) 4 1 2 3 4)

(deftest set-exclusive-or.order.4
  (let ((i 0) x y z w)
    (values
     (sort
      (set-exclusive-or (progn (setf x (incf i))
			       (list 1 2 3 4))
			(progn (setf y (incf i))
			       (list 1 3 6 10))
			:key (progn (setf z (incf i)) nil)
			:test (progn (setf w (incf i))
				     #'eql))
      #'<)
     i x y z w))
  (2 4 6 10) 4 1 2 3 4)

(deftest set-exclusive-or.order.5
  (let ((i 0) x y z w)
    (values
     (sort
      (set-exclusive-or (progn (setf x (incf i))
			       (list 1 2 3 4))
			(progn (setf y (incf i))
			       (list 1 3 6 10))
			:key (progn (setf z (incf i)) nil)
			:key (progn (setf w (incf i))
				    (complement #'eql)))
      #'<)
     i x y z w))
  (2 4 6 10) 4 1 2 3 4)


;;; Keyword tests

(deftest set-exclusive.allow-other-keys.1
  (sort (set-exclusive-or (list 1 2 3 4) (list 3 4 5 6)
			  :bad t :allow-other-keys t)
	#'<)
  (1 2 5 6))

(deftest set-exclusive.allow-other-keys.2
  (sort (set-exclusive-or (list 1 2 3 4) (list 3 4 5 6)
			  :allow-other-keys t :bad t)
	#'<)
  (1 2 5 6))

(deftest set-exclusive.allow-other-keys.3
  (sort (set-exclusive-or (list 1 2 3 4) (list 3 4 5 6)
			  :allow-other-keys t :bad t
			  :test #'(lambda (x y) (= x (1- y))))
	#'<)
  (1 6))

(deftest set-exclusive.allow-other-keys.4
  (sort (set-exclusive-or (list 1 2 3 4) (list 3 4 5 6)
			  :allow-other-keys t)
	#'<)
  (1 2 5 6))

(deftest set-exclusive.allow-other-keys.5
  (sort (set-exclusive-or (list 1 2 3 4) (list 3 4 5 6)
			  :allow-other-keys nil)
	#'<)
  (1 2 5 6))

(deftest set-exclusive.allow-other-keys.6
  (sort (set-exclusive-or (list 1 2 3 4) (list 3 4 5 6)
			  :allow-other-keys t
			  :allow-other-keys nil)
	#'<)
  (1 2 5 6))

(deftest set-exclusive.allow-other-keys.7
  (sort (set-exclusive-or (list 1 2 3 4) (list 3 4 5 6)
			  :allow-other-keys t
			  :allow-other-keys nil
			  '#:x 1)
	#'<)
  (1 2 5 6))

(deftest set-exclusive.keywords.8
  (sort (set-exclusive-or (list 1 2 3 4) (list 3 4 5 6)
			  :test #'eql
			  :test #'/=)
	#'<)
  (1 2 5 6))

(deftest set-exclusive.keywords.9
  (sort (set-exclusive-or (list 1 2 3 4) (list 3 4 5 6)
			  :test #'/=
			  :test #'eql)
	#'<)
  nil)

(def-fold-test set-exclusive-or.fold.1 (set-exclusive-or '(a b c d e f) '(b x e y z c)))

;;; Error tests

(deftest set-exclusive-or.error.1
  (signals-error (set-exclusive-or) program-error)
  t)

(deftest set-exclusive-or.error.2
  (signals-error (set-exclusive-or nil) program-error)
  t)

(deftest set-exclusive-or.error.3
  (signals-error (set-exclusive-or nil nil :bad t) program-error)
  t)

(deftest set-exclusive-or.error.4
  (signals-error (set-exclusive-or nil nil :key) program-error)
  t)

(deftest set-exclusive-or.error.5
  (signals-error (set-exclusive-or nil nil 1 2) program-error)
  t)

(deftest set-exclusive-or.error.6
  (signals-error (set-exclusive-or nil nil :bad t :allow-other-keys nil) program-error)
  t)

(deftest set-exclusive-or.error.7
  (signals-error (set-exclusive-or (list 1 2) (list 3 4) :test #'identity) program-error)
  t)

(deftest set-exclusive-or.error.8
  (signals-error (set-exclusive-or (list 1 2) (list 3 4) :test-not #'identity) program-error)
  t)

(deftest set-exclusive-or.error.9
  (signals-error (set-exclusive-or (list 1 2) (list 3 4) :key #'cons) program-error)
  t)

(deftest set-exclusive-or.error.10
  (signals-error (set-exclusive-or (list 1 2) (list 3 4) :key #'car) type-error)
  t)

(deftest set-exclusive-or.error.11
  (signals-error (set-exclusive-or (list 1 2 3) (list* 4 5 6)) type-error)
  t)

(deftest set-exclusive-or.error.12
  (signals-error (set-exclusive-or (list* 1 2 3) (list 4 5 6)) type-error)
  t)

(deftest set-exclusive-or.error.13
  (check-type-error #'(lambda (x) (set-exclusive-or x '(a b c))) #'listp)
  nil)

(deftest set-exclusive-or.error.14
  (check-type-error #'(lambda (x) (set-exclusive-or '(a b c) x)) #'listp)
  nil)


;;; Randomized test

(deftest random-set-exclusive-or
  (random-set-exclusive-or-test 10 100)
  nil)

