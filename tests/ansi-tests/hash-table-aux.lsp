;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct  4 09:24:24 2003
;;;; Contains: Aux. functions for testing hash tables

(in-package :cl-test)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (compile-and-load "random-aux.lsp"))

(defparameter *hash-table-test-iters* 1000)

(defun test-hash-table-1 (&rest args)
  (let ((table (apply #'make-hash-table args))
	(test (or (getf args :test) 'eql)))
    (assert (member test '(eq eql equal equalp)))
    (assert (hash-table-p table))
    (assert (typep table 'hash-table))
    ;; Build a hash table using the arguments in ARGS.
    ;; Perform *hash-table-test-iters* iterations of
    ;; random hash table operations
    (let* ((universe-vec (coerce *universe* 'vector))
	   ;; (universe-size (length universe-vec))
	   (mapping nil)
	   (count 0))

      (loop
       for i from 0 below *hash-table-test-iters*
       do (assert (eql (hash-table-count table) count))
       do (assert (let ((size (hash-table-size table)))
		    (and (integerp size) (>= size 0))))
       do
       (flet ((%remove-pair
	       (rpair)
	       (decf count)
	       (let ((key (car rpair))
		     (expected-value (cdr rpair)))
		 (multiple-value-bind (value present-p)
		     (gethash key table)
		   (assert present-p)
		   (assert (eql expected-value value))
		   (setf mapping
			 (remove rpair mapping :count 1 :test 'eq)))
		 (assert (remhash key table))
		 (multiple-value-bind (value present-p)
		     (gethash key table)
		   (assert (not present-p))
		   (assert (null value))
		   ))))
			    
	 (rcase
	  (1 ;; Insert
	   (let* ((new-elem (random-from-seq universe-vec))
		  (pair (assoc new-elem mapping :test test)))
	     (cond
	      (pair
	       (multiple-value-bind
		   (value present-p)
		   (gethash new-elem table)
		 (assert present-p)
		 (assert (eql (cdr pair) value))
		 (setf (cdr pair) i
		       (gethash new-elem table) i)))
	      (t
	       (assert
		(equal (multiple-value-list (gethash new-elem table))
		       '(nil nil)))
	       (incf count)
	       (push (cons new-elem i) mapping)
	       (setf (gethash new-elem table) i)))))
	  (1 ;; Delete element in the set
	   (when mapping
	     (%remove-pair (random-from-seq mapping))))
	  (1 ;; Delete random element from universe
	   (let* ((key (random-from-seq universe-vec))
		  (pair (assoc key mapping :test test)))
	     (cond
	      (pair (%remove-pair pair))
	      (t
	       ;; Not present -- check that this is true
	       (assert (equal (multiple-value-list (gethash key table))
			      '(nil nil)))
	       (assert (not (remhash key table)))
	       (assert (equal (multiple-value-list (gethash key table))
			      '(nil nil)))))
	     ))
	  ))))))

	      
		  
	    
	    
		
