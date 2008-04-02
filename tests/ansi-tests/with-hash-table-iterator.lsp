;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 20:08:43 2003
;;;; Contains: Tests of WITH-HASH-TABLE-ITERATOR

(in-package :cl-test)

(deftest with-hash-table-iterator.1
  (with-hash-table-iterator (x (make-hash-table)))
  nil)

(deftest with-hash-table-iterator.2
  (with-hash-table-iterator (x (make-hash-table)) (values)))

(deftest with-hash-table-iterator.3
  (with-hash-table-iterator (x (make-hash-table)) (values 'a 'b 'c 'd))
  a b c d)

(deftest with-hash-table-iterator.4
  (with-hash-table-iterator
   (%x (make-hash-table))
   (%x))
  nil)

(deftest with-hash-table-iterator.5
  (let ((table (make-hash-table)))
    (setf (gethash 'a table) 'b)
    (with-hash-table-iterator
     (%x table)
     (multiple-value-bind (success-p key val)
	 (%x)
       (values (notnot success-p) key val))))
  t a b)

(deftest with-hash-table-iterator.6
  (let ((table (make-hash-table)))
    (setf (gethash 'a table) 'b)
    (with-hash-table-iterator
     (%x table)
     (length (multiple-value-list (%x)))))
  3)

(deftest with-hash-table-iterator.7
  (let ((keys '("a" "b" "c" "d" "e")))
    (loop for test in '(eq eql equal equalp)
	  for test-fn of-type function = (symbol-function test)
	  collect
	  (let ((table (make-hash-table :test test)))
	    (loop for k in keys
		  for i from 0
		  do (setf (gethash k table) i))
	    (let ((count 0) (found-keys))
	      (with-hash-table-iterator
	       (%x table)
	       (block done
		 (loop
		  (multiple-value-bind (success key val)
		      (%x)
		    (unless success (return-from done nil))
		    (incf count)
		    (push key found-keys)
		    (assert (= val (position key keys :test test-fn))))))
	       (and (= count (length keys))
		    (every test-fn
			   (sort (remove-duplicates found-keys :test test)
				 #'string<)
			   keys)
		    t))))))
  (t t t t))

(deftest with-hash-table-iterator.8
  (with-hash-table-iterator
   (%x (make-hash-table))
   (declare (optimize)))
  nil)

(deftest with-hash-table-iterator.8a
  (with-hash-table-iterator
   (%x (make-hash-table))
   (declare (optimize))
   (declare (optimize)))
  nil)

(deftest with-hash-table-iterator.9
  (with-hash-table-iterator
   (%x (make-hash-table))
   (macrolet
       ((expand-%x
	 (&environment env)
	 (let ((expanded-form (macroexpand '(%x) env)))
	   (if (equal expanded-form '(%x)) nil t))))
     (expand-%x)))
  t)

(deftest with-hash-table-iterator.10
  (let ((table (make-hash-table)))
    (loop for key from 1 to 100
	  for val from 101 to 200
	  do (setf (gethash key table) val))
    (let ((pairs nil))
      (with-hash-table-iterator
       (%x table)
       (loop
	(multiple-value-bind (success key val)
	    (%x)
	  (unless success (return nil))
	  (remhash key table)
	  (push (cons key val) pairs))))
      (assert (eql (length pairs) 100))
      (setq pairs (sort pairs #'(lambda (p1 p2) (< (car p1) (car p2)))))
      (values
       (hash-table-count table)
       (loop
	for (key . val) in pairs
	for expected-key from 1
	for expected-val from 101
	always (and (eql key expected-key)
		    (eql val expected-val))))))
  0 t)

(deftest with-hash-table-iterator.11
  (let ((table (make-hash-table)))
    (loop for key from 1 to 100
	  for val from 101 to 200
	  do (setf (gethash key table) val))
    (let ((pairs nil))
      (with-hash-table-iterator
       (%x table)
       (loop
	(multiple-value-bind (success key val)
	    (%x)
	  (unless success (return nil))
	  (setf (gethash key table) (+ 1000 val))
	  (push (cons key val) pairs))))
      (assert (eql (length pairs) 100))
      (setq pairs (sort pairs #'(lambda (p1 p2) (< (car p1) (car p2)))))
      (values
       (hash-table-count table)
       (loop
	for (key . val) in pairs
	for expected-key from 1
	for expected-val from 101
	always (and (eql key expected-key)
		    (eql val expected-val)
		    (eql (gethash key table) (+ 1000 val))
		    )))))
  100 t)

;;; Free declaration scope

(deftest with-hash-table-iterator.12
  (block done
    (let ((x :bad))
      (declare (special x))
      (let ((x :good))
	(with-hash-table-iterator (m (return-from done x))
				  (declare (special x))))))
  :good)
