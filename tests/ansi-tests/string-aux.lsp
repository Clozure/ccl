;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct  4 06:51:32 2002
;;;; Contains: Auxiliary functions for string testing

(in-package :cl-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (compile-and-load "random-aux.lsp"))

(defun my-string-compare (string1 string2 comparison
				  &key (start1 0) end1 (start2 0) end2 case
				  &aux
				  (len1 (progn (assert (stringp string1))
					       (length string1)))
				  (len2 (progn (assert (stringp string2))
					       (length string2)))
				  (compare-fn
				   (case comparison
				     (< (if case #'char-lessp #'char<))
				     (<= (if case #'char-not-greaterp
					   #'char<=))
				     (= (if case #'char-equal #'char=))
				     (/= (if case #'char-not-equal #'char/=))
				     (> (if case #'char-greaterp #'char>))
				     (>= (if case #'char-not-lessp #'char>=))
				     (t (error "Bad comparison arg: ~A~%"
					       comparison))))
				  (equal-fn (if case #'char-equal #'char=)))

  (assert (integerp start1))
  (assert (integerp start2))
  (unless end1 (setq end1 len1))
  (unless end2 (setq end2 len2))
  (assert (<= 0 start1 end1))
  (assert (<= 0 start2 end2))
  (loop
   for i1 from start1
   for i2 from start2
   do
   (cond
    ((= i1 end1)
     (return
      (cond
       ((= i2 end2)
	;; Both ended -- equality case
	(if (member comparison '(= <= >=))
	    end1
	  nil))
       (t ;; string2 still extending
	(if (member comparison '(/= < <=))
	    end1
	  nil)))))
    ((= i2 end2)
     ;; string1 still extending
     (return
      (if (member comparison '(/= > >=))
	  i1
	nil)))
    (t
     (let ((c1 (my-aref string1 i1))
	   (c2 (my-aref string2 i2)))
       (cond
	((funcall equal-fn c1 c2))
	(t ;; mismatch found -- what kind?
	 (return
	  (if (funcall compare-fn c1 c2)
	      i1
	    nil)))))))))

(defun make-random-string-compare-test (n)
  (let* ((len (random n))
	 ;; Maximum lengths of the two strings
	 (len1 (if (or (coin) (= len 0)) len (+ len (random len))))
	 (len2 (if (or (coin) (= len 0)) len (+ len (random len))))
	 (s1 (make-random-string len1))
	 (s2 (make-random-string len2))
	 ;; Actual lengths of the strings
	 (len1 (length s1))
	 (len2 (length s2))
	 ;; Lengths of the parts of the strings to be matched
	 (sublen1 (if (or (coin) (= len1 0)) (min len1 len2) (random len1)))
	 (sublen2 (if (or (coin) (= len2 0)) (min len2 sublen1) (random len2)))
	 ;; Start and end of the substring of the first string
	 (start1 (if (coin 3) 0
		   (max 0 (min (1- len1) (random (- len1 sublen1 -1))))))
	 (end1 (+ start1 sublen1))
	 ;; Start and end of the substring of the second string
	 (start2 (if (coin 3) 0
		   (max 0 (min (1- len2) (random (- len2 sublen2 -1))))))
	 (end2 (+ start2 sublen2))
	 )
    #|
    (format t "len = ~A, len1 = ~A, len2 = ~A, sublen1 = ~A, sublen2 = ~A~%"
	    len len1 len2 sublen1 sublen2)
    (format t "start1 = ~A, end1 = ~A, start2 = ~A, end2 = ~A~%"
	    start1 end1 start2 end2)
    (format t "s1 = ~S, s2 = ~S~%" s1 s2)
    |#
    ;; Sometimes we want them to have a common prefix
    (when (and (coin)
	       (equal (array-element-type s1)
		      (array-element-type s2)))
      (if (<= sublen1 sublen2)
	  (setf (subseq s2 start2 (+ start2 sublen1))
		(subseq s1 start1 (+ start1 sublen1)))
	(setf (subseq s1 start1 (+ start1 sublen2))
	      (subseq s2 start2 (+ start2 sublen2)))))
    (values
     s1
     s2
     (reduce #'nconc
	     (random-permute
	      (list
	       (if (and (= start1 0) (coin))
		   nil
		 (list :start1 start1))
	       (if (and (= end1 len1) (coin))
		   nil
		 (list :end1 end1))
	       (if (and (= start2 0) (coin))
		   nil
		 (list :start2 start2))
	       (if (and (= end2 len2) (coin))
		   nil
		 (list :end2 end2))))))))

(defun random-string-compare-test (n comparison case &optional (iterations 1))
  (loop for i from 1 to iterations
	count
	(multiple-value-bind (s1 s2 args)
	    (make-random-string-compare-test n)
	  ;; (format t "Strings: ~s ~s - Args = ~S~%" s1 s2 args)
	  (let ((x (apply (case comparison
			    (< (if case #'string-lessp #'string<))
			    (<= (if case #'string-not-greaterp
				  #'string<=))
			    (= (if case #'string-equal #'string=))
			    (/= (if case #'string-not-equal #'string/=))
			    (> (if case #'string-greaterp #'string>))
			    (>= (if case #'string-not-lessp #'string>=))
			    (t (error "Bad comparison arg: ~A~%" comparison)))
			  s1 s2 args))
		(y (apply #'my-string-compare s1 s2 comparison :case case args)))
	    (not
	     (or (eql x y)
		 (and x y (eqt comparison '=))))))))

(defun string-all-the-same (s)
  (let ((len (length s)))
    (or (= len 0)
	(let ((c (my-aref s 0)))
	  (loop for i below len
		for d = (my-aref s i)
		always (eql c d))))))
