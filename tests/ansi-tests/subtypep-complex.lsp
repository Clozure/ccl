;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 23 07:12:38 2005
;;;; Contains: Tests of SUBTYPEP on complex types

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

(deftest subtypep-complex.1
  (subtypep* 'complex 'number)
  t t)

(deftest subtypep-complex.2
  (subtypep* 'number 'complex)
  nil t)

(defun check-not-complex-type (type)
  (let ((result1 (multiple-value-list (subtypep* type 'complex)))
	(result2 (multiple-value-list (subtypep* 'complex type))))
    (if (and (equal result1 '(nil t))
	     (equal result2 '(nil t)))
	nil
      (list (list type result1 result2)))))

(deftest subtypep-complex.3
  (mapcan #'check-not-complex-type
	  '(bit unsigned-byte integer rational ratio real float short-float
		single-float double-float long-float fixnum bignum))
  nil)

(deftest subtypep-complex.4
  (loop for i from 1 to 100
	nconc (check-not-complex-type `(unsigned-byte ,i)))
  nil)
	
(deftest subtypep-complex.5
  (loop for i from 1 to 100
	nconc (check-not-complex-type `(signed-byte ,i)))
  nil)

(deftest subtypep-complex.7
  (let ((types '(complex (complex) (complex *))))
    (loop for tp1 in types
	  nconc (loop for tp2 in types
		      for result = (multiple-value-list (subtypep* tp1 tp2))
		      unless (equal result '(t t))
		      collect (list tp1 tp2 result))))
  nil)

(defun check-complex-upgrading (t1 t2)
  (let* ((ucpt1 (upgraded-complex-part-type t1))
	 (ucpt2 (upgraded-complex-part-type t2))
	 (result (multiple-value-list
		  (subtypep* `(complex ,t1) `(complex ,t2)))))
    (cond
     ((or (equal ucpt1 ucpt2)
	  (subtypep t1 t2))
      (unless (equal result '(t t))
	(list (list :case1 t1 t2 ucpt1 ucpt2 result))))
     (t
      (multiple-value-bind
	  (ucpt-sub1? good1?)
	  (subtypep* ucpt1 ucpt2)
	(multiple-value-bind
	    (ucpt-sub2? good2?)
	    (subtypep* ucpt2 ucpt1)
	  (cond
	   ;; the second is not a subtype of the first
	   ((and good2? ucpt-sub1? (not ucpt-sub2?))
	    (assert good1?)
	    (unless (equal result '(nil t))
	      (list (list :case2 t1 t2 ucpt1 ucpt2 result))))
	   ;; the first is not a subtype of the second
	   ((and good1? (not ucpt-sub1?) ucpt-sub2?)
	    (assert good2?)
	    (unless (equal result '(nil t))
	      (list (list :case3 t1 t2 ucpt1 ucpt2 result))))
	   ;; they are both subtypes of each other, and so represent
	   ;; the same set of objects
	   ((and ucpt-sub1? ucpt-sub2?)
	    (assert good1?)
	    (assert good2?)
	    (unless (equal result '(t t))
	      (list (list :case4 t1 t2 ucpt1 ucpt2 result)))))))))))

(deftest subtypep-complex.8
  (let ((types (reverse
		'(bit fixnum bignum integer unsigned-byte rational ratio
		      short-float single-float double-float long-float
		      float real)))
	(float-types
	 (remove-duplicates '(short-float single-float double-float long-float)
			    :test #'(lambda (t1 t2)
				      (eql (coerce 0 t1) (coerce 0 t2))))))
    (loop for i in '(1 2 3 4 6 8 13 16 17 28 29 31 32 48 64)
	  do (push `(unsigned-byte ,i) types)
	  do (push `(signed-byte ,i) types)
	  do (loop for ftp in float-types
		   do (push `(,ftp ,(coerce 0 ftp)
				   ,(coerce i ftp))
			    types)
		   do (push `(,ftp (,(coerce (- i) ftp))
				   ,(coerce i ftp))
			    types))
	  do (push `(float ,(coerce 0 'single-float)
			   ,(coerce i 'single-float))
		   types))
    (setq types (reverse types))
    (let ((results
	   (mapcan #'(lambda (t1)
		       (mapcan #'(lambda (t2) (check-complex-upgrading t1 t2))
			       types))
		   types)))
      (subseq results 0 (min 100 (length results)))))
  nil)
