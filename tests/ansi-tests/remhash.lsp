;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 08:58:06 2003
;;;; Contains: Tests of REMHASH

(in-package :cl-test)

(deftest remhash.1
  (let ((table (make-hash-table)))
    (values (gethash 'a table)
	    (remhash 'a table)
	    (setf (gethash 'a table) 'b)
	    (gethash 'a table)
	    (notnot (remhash 'a table))
	    (gethash 'a table)))
  nil nil b b t nil)

(deftest remhash.2
  (let ((table (make-hash-table :test 'eq)))
    (values (gethash 'a table)
	    (remhash 'a table)
	    (setf (gethash 'a table) 'b)
	    (gethash 'a table)
	    (notnot (remhash 'a table))
	    (gethash 'a table)))
  nil nil b b t nil)

(deftest remhash.3
  (let ((table (make-hash-table :test 'equal)))
    (values (gethash 'a table)
	    (remhash 'a table)
	    (setf (gethash 'a table) 'b)
	    (gethash 'a table)
	    (notnot (remhash 'a table))
	    (gethash 'a table)))
  nil nil b b t nil)

(deftest remhash.4
  (let ((table (make-hash-table :test 'equalp)))
    (values (gethash 'a table)
	    (remhash 'a table)
	    (setf (gethash 'a table) 'b)
	    (gethash 'a table)
	    (notnot (remhash 'a table))
	    (gethash 'a table)))
  nil nil b b t nil)

(deftest remhash.5
  (remhash 'a (make-hash-table))
  nil)

(deftest remhash.6
  (notnot-mv (remhash nil (let ((table (make-hash-table)))
			    (setf (gethash nil table) t)
			    table)))
  t)

(deftest remhash.order.1
  (let ((i 0) x y)
    (values
     (remhash (progn (setf x (incf i)) 'a)
	      (progn (setf y (incf i)) (make-hash-table)))
     i x y))
  nil 2 1 2)

;;; Error tests

(deftest remhash.error.1
  (signals-error (remhash) program-error)
  t)

(deftest remhash.error.2
  (signals-error (remhash 'a) program-error)
  t)

(deftest remhash.error.3
  (signals-error (remhash 'a (make-hash-table) nil) program-error)
  t)







	    
