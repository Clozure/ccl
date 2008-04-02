;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 09:36:58 2003
;;;; Contains: Test of MAPHASH

(in-package :cl-test)

(deftest maphash.1
  (let ((table (make-hash-table)))
    (loop for i from 1 to 1000 do (setf (gethash i table) (+ i i)))
    (let ((s1 0) (s2 0))
      (values
       (multiple-value-list
	(maphash #'(lambda (k v) (incf s1 k) (incf s2 v)) table))
       s1 s2)))
  (nil) #.(* 500 1001) #.(* 1000 1001))

(deftest maphash.2
  (let ((table (make-hash-table :test 'equal)))
    (loop for i from 1 to 1000 do (setf (gethash i table) (+ i i)))
    (let ((s1 0) (s2 0))
      (values
       (multiple-value-list
	(maphash #'(lambda (k v) (incf s1 k) (incf s2 v)) table))
       s1 s2)))
  (nil) #.(* 500 1001) #.(* 1000 1001))

(deftest maphash.3
  (let ((table (make-hash-table :test 'equalp)))
    (loop for i from 1 to 1000 do (setf (gethash i table) (+ i i)))
    (let ((s1 0) (s2 0))
      (values
       (multiple-value-list
	(maphash #'(lambda (k v) (incf s1 k) (incf s2 v)) table))
       s1 s2)))
  (nil) #.(* 500 1001) #.(* 1000 1001))

;;; Test that REMHASH on the key being traversed is allowed

(deftest maphash.4
  (let ((table (make-hash-table)))
    (loop for i from 1 to 1000 do (setf (gethash i table) (+ i i)))
    (let ((s1 0) (s2 0))
      (values
       (multiple-value-list
	(maphash #'(lambda (k v)
		     (incf s1 k) (incf s2 v)
		     (remhash k table))
		 table))
       s1 s2 (hash-table-count table))))
  (nil) #.(* 500 1001) #.(* 1000 1001) 0)

(deftest maphash.5
  (let ((table (make-hash-table :test 'equal)))
    (loop for i from 1 to 1000 do (setf (gethash i table) (+ i i)))
    (let ((s1 0) (s2 0))
      (values
       (multiple-value-list
	(maphash #'(lambda (k v)
		     (incf s1 k) (incf s2 v)
		     (remhash k table))
		 table))
       s1 s2 (hash-table-count table))))
  (nil) #.(* 500 1001) #.(* 1000 1001) 0)

(deftest maphash.6
  (let ((table (make-hash-table :test 'equalp)))
    (loop for i from 1 to 1000 do (setf (gethash i table) (+ i i)))
    (let ((s1 0) (s2 0))
      (values
       (multiple-value-list
	(maphash #'(lambda (k v)
		     (incf s1 k) (incf s2 v)
		     (remhash k table))
		 table))
       s1 s2 (hash-table-count table))))
  (nil) #.(* 500 1001) #.(* 1000 1001) 0)


;;; EQ hash tables

(deftest maphash.7
  (let ((symbols '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
	(table (make-hash-table :test #'eq)))
    (loop for sym in symbols
	  for i from 1
	  do (setf (gethash sym table) i))
    (let ((sum 0))
      (values
       (multiple-value-list
	(maphash #'(lambda (k v)
		     (assert (eq (elt symbols (1- v)) k))
		     (incf sum v))
		 table))
       sum)))
  (nil) #.(* 13 27))

(deftest maphash.8
  (let ((symbols '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
	(table (make-hash-table :test #'eq)))
    (loop for sym in symbols
	  for i from 1
	  do (setf (gethash sym table) i))
    (let ((sum 0))
      (values
       (multiple-value-list
	(maphash #'(lambda (k v)
		     (assert (eq (elt symbols (1- v)) k))
		     (remhash k table)
		     (incf sum v))
		 table))
       sum
       (hash-table-count table))))
  (nil) #.(* 13 27) 0)

;;; Need to add tests where things are setf'd during traversal

(deftest maphash.order.1
  (let ((i 0) x y dummy
	(table (make-hash-table)))
    (values
     (multiple-value-list
      (maphash (progn (setf x (incf i))
		      #'(lambda (k v) (setf dummy (list k v))))
	       (progn (setf y (incf i))
		      table)))
     i x y dummy))
  (nil) 2 1 2 nil)
    

;;; Error tests

(deftest maphash.error.1
  (signals-error (maphash) program-error)
  t)

(deftest maphash.error.2
  (signals-error (maphash #'list) program-error)
  t)

(deftest maphash.error.3
  (signals-error (maphash #'list (make-hash-table) nil) program-error)
  t)
