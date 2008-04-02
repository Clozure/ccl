;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  6 05:04:45 2003
;;;; Contains: Generating random types and testing relationships on them

(in-package :cl-test)

(compile-and-load "types-aux.lsp")
(compile-and-load "random-aux.lsp")
(compile-and-load "random-int-form.lsp")

(defparameter *random-types* nil)

(defun make-random-type (size)
  (if (<= size 1)
      (rcase
       (1 nil)
       (1 t)
       (1 `(eql ,(let ((r (ash 1 (random 45))))
		   (random-from-interval r (- r)))))
       (1 (random-from-seq #(integer unsigned-byte ratio rational real float
			     short-float single-float double-float
			     long-float complex symbol cons function)))
       (1
	(let* ((len (random *maximum-random-int-bits*))
	       (r1 (ash 1 len))
	       (r2 (+ r1 r1))
	       (x (- (random r2) r1))
	       (y (- (random r2) r1))
	       (lo (min x y))
	       (hi (max x y)))
	  `(integer ,lo ,hi)))
       (1 (make-random-real-type))
       ;; (1 (make-random-complex-type))
       )
    (rcase
     (2 (let* ((op (random-from-seq #(cons cons and or)))
	       (nargs (if (eq op 'cons) 2
			(1+ (random (min size 4)))))
	       (sizes (random-partition (1- size) nargs)))
	  `(,op ,@(mapcar #'make-random-type sizes))))
     (1 `(not ,(make-random-type (1- size))))
     ; (1 (make-random-function-type size))
     )))

(defun make-random-real-type ()
  (rcase
   (1 (random-from-seq '(integer unsigned-byte short-float single-float
				 double-float long-float rational real)))
   (1 (destructuring-bind (lo hi)
	  (make-random-integer-range)
	(rcase
	 (4 `(integer ,lo ,hi))
	 (1 `(integer ,lo))
	 (1 `(integer ,lo *))
	 (2 `(integer * ,hi)))))
   (1 (let ((r1 (random-real))
	    (r2 (random-real)))
	`(real ,(min r1 r2) ,(max r2 r2))))
   ;;; Add more cases here
   ))

(defun make-random-complex-type ()
  `(complex ,(make-random-real-type)))

(defun make-random-function-type (size)
  (let* ((sizes (random-partition (1- size) 2))
	 (types (mapcar #'make-random-type sizes)))
    `(function (,(car types)) ,(cadr types))))

(defun size-of-type (type)
  (if (consp type)
      (case (car type)
	(complex (1+ (size-of-type (cadr type))))
	((array simple-array) (1+ (size-of-type (cadr type))))
	(vector (1+ (size-of-type (cadr type))))
	(complex (1+ (size-of-type (cadr type))))
	((cons or and not) (reduce #'+ (cdr type) :initial-value 1
				   :key #'size-of-type))
	(t 1))
      1))

(defun mutate-type (type)
  (let* ((size (size-of-type type))
	 (r (random size)))
    (flet ((%f ()
	     (rcase
	      (6 (make-random-type (random (1+ size))))
	      (2 `(not ,type))
	      (1 `(and ,(make-random-type 1) ,type))
	      (1 `(and ,type ,(make-random-type 1)))
	      (1 `(or ,(make-random-type 1) ,type))
	      (1 `(or ,type ,(make-random-type 1)))))
	   (%random-int ()
	     (let ((bits (1+ (min (random 20) (random 20)))))
	       (- (ash 1 bits) (random (ash 1 (1+ bits)))))))
      (if (or (and (= r 0) (coin)) (not (consp type)))
	  (%f)
	  (case (car type)
	    ((and or not cons complex)
	     (let ((sizes (mapcar #'size-of-type (cdr type))))
	       (loop with sum = 0
		  for e on sizes
		  for ctype in (cdr type)
		  for i from 0
		  do (setf sum (incf (car e) sum))
		  when (>= sum r)
		  return (rcase
			  (1 ctype) ;; replace with component type
			  (1 (cons (car type)
				   (append (subseq (cdr type) 0 i)
					   (list (mutate-type ctype))
					   (subseq (cdr type) (1+ i)))))))))
	    ((array simple-array vector)
	     (let ((ctype (if (cdr type) (cadr type) t)))
	       (rcase
		(1 (if (eql ctype *) t ctype))
		(1 (cons (car type)
			 (cons (mutate-type ctype)
			       (cddr type)))))))
	    ((unsigned-byte)
	     (if (integerp (cadr type))
		 (rcase
		  (1 'unsigned-byte)
		  (1 `(unsigned-byte (+ (cadr type) (- 10 (random 20))))))
		 (%f)))

	    ((integer)
	     (let ((lo-delta (%random-int))
		   (hi-delta (%random-int))
		   (old-lo (or (cadr type) '*))
		   (old-hi (or (caddr type) '*)))
	       (flet ((%inc (old delta)
			(if (or (coin) (not (integerp old)))
			    delta
			    (+ old delta))))
		 (rcase
		  (1 `(integer ,old-lo *))
		  (1 `(integer * ,old-hi))
		  (1 (let ((new-lo (%inc old-lo lo-delta)))
		       (if (or (null (cdr type))
			       (null (cddr type))
			       (not (integerp old-hi)))
			   `(integer ,new-lo ,@(cddr type))
			   ;; caddr is integer
			   (if (<= new-lo old-hi)
			       `(integer ,new-lo ,old-hi)
			       `(integer ,old-hi ,new-lo)))))
		  (1 (let ((new-hi (%inc old-hi hi-delta)))
		       (if (or (null (cdr type))
			       (null (cddr type))
			       (not (integerp old-lo)))
			   `(integer ,old-lo ,new-hi)
			   (if (<= old-lo new-hi)
			       `(integer ,old-lo ,new-hi)
			       `(integer ,new-hi ,old-lo)))))
		  (1 (let ((new-lo (%inc old-lo lo-delta))
			   (new-hi (%inc old-hi hi-delta)))
		       (if (<= new-lo new-hi)
			   `(integer ,new-lo ,new-hi)
			   `(integer ,new-hi ,new-lo))))))))
		       
	    (t (%f)))))))

(defun test-random-types (n size)
  (loop for t1 = (make-random-type size)
	for t2 = (make-random-type size)
	for i from 0 below n
	;; do (print (list t1 t2))
        do (setf *random-types* (list t1 t2))
	do (when (and (= (mod i 100) 0) (> i 0))
	     (format t "~A " i) (finish-output *standard-output*))
	when (test-types t1 t2)
	collect (list t1 t2)
	finally (terpri)))

(defun test-random-mutated-types (n size &key (reps 1))
  (loop for t1 = (make-random-type size)
	for t2 = (let ((x t1)) (loop repeat reps
				     do (setq x (mutate-type x))) x)
	for i from 0 below n
	;; do (print (list t1 t2))
        do (setf *random-types* (list t1 t2))
	do (when (and (= (mod i 100) 0) (> i 0))
	     (format t "~A " i) (finish-output *standard-output*))
	when (test-types t1 t2)
	collect (list t1 t2)
	finally (terpri)))

(defun test-types (t1 t2)
  (multiple-value-bind (sub success)
      (subtypep t1 t2)
    (when success
      (if sub
	  (check-all-subtypep t1 t2)
	(let ((nt1 `(not ,t1))
	      (nt2 `(not ,t2)))
	  (subtypep nt2 nt1))))))

(defun prune-type (tp try-fn)
  (declare (type function try-fn))
  (flet ((try (x) (funcall try-fn x)))
    (cond
     ((member tp '(nil t)))
     ((symbolp tp)
      (try nil)
      (try t))
     ((consp tp)
      (try nil)
      (try t)
      (let ((op (first tp))
	    (args (rest tp)))
	(case op
	  ((cons)
	   (try 'cons)
	   (prune-list args
		       #'prune-type
		       #'(lambda (args) (try `(cons ,@args)))))
	  ((integer)
	   (try op)
	   (try '(eql 0))
	   (when (= (length args) 2)
	     (let ((arg1 (first args))
		   (arg2 (second args)))
	       (when (and (integerp arg1) (integerp arg2))
		 (try `(eql ,arg1))
		 (try `(eql ,arg2))
		 (when (and (< arg1 0) (<= 0 arg2))
		   (try `(integer 0 ,arg2)))
		 (when (and (<= arg1 0) (< 0 arg2))
		   (try `(integer ,arg1 0)))
		 (when (> (- arg2 arg1) 1)
		   (try `(integer ,(+ arg1 (floor (- arg2 arg1) 2)) ,arg2))
		   (try `(integer ,arg1 ,(- arg2 (floor (- arg2 arg1) 2)))))))))
	    
	  ((real float ratio single-float double-float short-float long-float)
	   (try op))
	   
	  ((or and)
	   (mapc try-fn args)
	   (loop for i from 0 below (length args)
		 do (try `(,op ,@(subseq args 0 i)
			       ,@(subseq args (1+ i)))))
	   (prune-list args
		       #'prune-type
		       #'(lambda (args) (try (cons op args)))))
	  ((not)
	   (let ((arg (first args)))
	     (try arg)
	     (when (and (consp arg)
			(eq (car arg) 'not))
	       (try (second arg)))
	     (prune-type arg #'(lambda (arg) (try `(not ,arg))))))
	  
	  ((member)
	   (dolist (arg (cdr tp))
	     (try `(eql ,arg)))
	   (when (cddr tp)
	   (try `(member ,@(cddr tp)))))

	  ((eql)
	   (assert (= (length args) 1))
	   (let ((arg (first args)))
	     (unless (= arg 0)
	       (try `(eql 0))
	       (cond
		((< arg -1)
		 (try `(eql ,(ceiling arg 2))))
		((> arg 1)
		 (try `(eql ,(floor arg 2))))))))		 
	  
	  )))))
  (values))

(defun prune-type-pair (pair &optional (fn #'test-types))
  (declare (type function fn))
  (let ((t1 (first pair))
	(t2 (second pair))
	changed)
    (loop
     do (flet ((%try2 (new-tp)
		      (when (funcall fn t1 new-tp)
			(print "Success in first loop")
			(print new-tp)
			(setq t2 new-tp
			      changed t)
			(throw 'success nil))))
	  (catch 'success
	    (prune-type t2 #'%try2)))
     do (flet ((%try1 (new-tp)
		      (when (funcall fn new-tp t2)
			(print "Success in second loop")
			(print new-tp)
			(setq t1 new-tp
			      changed t)
			(throw 'success nil))))
	  (catch 'success
	    (prune-type t1 #'%try1)))
     while changed
     do (setq changed nil))
    (list t1 t2)))

(defun test-type-triple (t1 t2 t3)
  ;; Returns non-nil if a problem is found
  (catch 'problem
    (multiple-value-bind (sub1 success1)
	(subtypep t1 t2)
      (when success1
	(if sub1
	    (append
	     (check-all-subtypep t1 `(or ,t2 ,t3))
	     (check-all-subtypep `(and ,t1 ,t3) t2))
	    (or (subtypep `(or ,t1 ,t3) t2)
		(subtypep t1 `(and ,t2 ,t3))))))))

(defun test-random-types3 (n size)
  (loop for t1 = (make-random-type (1+ (random size)))
	for t2 = (make-random-type (1+ (random size)))
	for t3 = (make-random-type (1+ (random size)))
	for i from 1 to n
	;; do (print (list t1 t2))
        do (setf *random-types* (list t1 t2 t3))
	do (when (and (= (mod i 100) 0) (> i 0))
	     (format t "~A " i) (finish-output *standard-output*))
	when (test-type-triple t1 t2 t3)
	collect (list t1 t2 t3)
	finally (terpri)))

(defun prune-type-triple (pair &optional (fn #'test-type-triple))
  (declare (type function fn))
  (let ((t1 (first pair))
	(t2 (second pair))
	(t3 (third pair))
	changed)
    (loop
     do (flet ((%try2 (new-tp)
		      (when (funcall fn t1 new-tp t3)
			(print "Success in first loop")
			(print new-tp)
			(setq t2 new-tp
			      changed t)
			(throw 'success nil))))
	  (catch 'success
	    (prune-type t2 #'%try2)))
     do (flet ((%try1 (new-tp)
		      (when (funcall fn new-tp t2 t3)
			(print "Success in second loop")
			(print new-tp)
			(setq t1 new-tp
			      changed t)
			(throw 'success nil))))
	  (catch 'success
	    (prune-type t1 #'%try1)))
     do (flet ((%try3 (new-tp)
		      (when (funcall fn t1 t2 new-tp)
			(print "Success in second loop")
			(print new-tp)
			(setq t3 new-tp
			      changed t)
			(throw 'success nil))))
	  (catch 'success
	    (prune-type t3 #'%try3)))
     while changed
     do (setq changed nil))
    (list t1 t2 t3)))



  