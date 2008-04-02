;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Mar  6 17:45:42 2003
;;;; Contains: Auxiliary functions for cons-related tests

(in-package :cl-test)

;;;
;;; A scaffold is a structure that is used to remember the object
;;; identities of the cons cells in a (noncircular) data structure.
;;; This lets us check if the data structure has been changed by
;;; an operation.
;;;

(defstruct scaffold
  node
  car
  cdr)

(defun make-scaffold-copy (x)
  "Make a tree that will be used to check if a tree has been changed."
  (if
      (consp x)
      (make-scaffold :node x
		     :car (make-scaffold-copy (car x))
		     :cdr (make-scaffold-copy (cdr x)))
    (make-scaffold :node x
		   :car nil
		   :cdr nil)))

(defun check-scaffold-copy (x xcopy)
  "Return t if xcopy were produced from x by make-scaffold-copy,
   and none of the cons cells in the tree rooted at x have been
   changed."

  (and (eq x (scaffold-node xcopy))
       (or
	(not (consp x))
	(and
	 (check-scaffold-copy (car x) (scaffold-car xcopy))
	 (check-scaffold-copy (cdr x) (scaffold-cdr xcopy))))))

(defun create-c*r-test (n)
  (cond
   ((<= n 0) 'none)
   (t
    (cons (create-c*r-test (1- n))
	  (create-c*r-test (1- n))))))

(defun nth-1-body (x)
  (loop
      for e in x
       and i from 0
       count (not (eqt e (nth i x)))))

(defun check-cons-copy (x y)
  "Check that the tree x is a copy of the tree y,
   returning t if it is, nil if not."
  (cond
   ((consp x)
    (and (consp y)
	 (not (eqt x y))
	 (check-cons-copy (car x) (car y))
	 (check-cons-copy (cdr x) (cdr y))))
   ((eqt x y) t)
   (t nil)))

(defun check-sublis (a al &key (key 'no-key) test test-not)
  "Apply sublis al a with various keys.  Check that
   the arguments are not themselves changed.  Return nil
   if the arguments do get changed."
  (setf a (copy-tree a))
  (setf al (copy-tree al))
  (let ((acopy (make-scaffold-copy a))
	(alcopy (make-scaffold-copy al)))
    (let ((as
	   (apply #'sublis al a
		  `(,@(when test `(:test ,test))
		    ,@(when test-not `(:test-not ,test-not))
		    ,@(unless (eqt key 'no-key) `(:key ,key))))))
      (and
       (check-scaffold-copy a acopy)
       (check-scaffold-copy al alcopy)
       as))))

(defun check-nsublis (a al &key (key 'no-key) test test-not)
  "Apply nsublis al a, copying these arguments first."
  (setf a (copy-tree a))
  (setf al (copy-tree al))
  (let ((as
	 (apply #'sublis (copy-tree al) (copy-tree a)
		`(,@(when test `(:test ,test))
		    ,@(when test-not `(:test-not ,test-not))
		    ,@(unless (eqt key 'no-key) `(:key ,key))))))
    as))

(defun check-subst (new old tree &key (key 'no-key) test test-not)
  "Call subst new old tree, with keyword arguments if present.
   Check that the arguments are not changed."
  (setf new (copy-tree new))
  (setf old (copy-tree old))
  (setf tree (copy-tree tree))
  (let ((newcopy (make-scaffold-copy new))
	(oldcopy (make-scaffold-copy old))
	(treecopy (make-scaffold-copy tree)))
    (let ((result
	   (apply #'subst new old tree
		  `(,@(unless (eqt key 'no-key) `(:key ,key))
		    ,@(when test `(:test ,test))
		    ,@(when test-not `(:test-not ,test-not))))))
      (and (check-scaffold-copy new newcopy)
	   (check-scaffold-copy old oldcopy)
	   (check-scaffold-copy tree treecopy)
	   result))))


(defun check-subst-if (new pred tree &key (key 'no-key))
  "Call subst-if new pred tree, with various keyword arguments
   if present.  Check that the arguments are not changed."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (let ((newcopy (make-scaffold-copy new))
	(predcopy (make-scaffold-copy pred))
	(treecopy (make-scaffold-copy tree)))
    (let ((result
	   (apply #'subst-if new pred tree
		  (unless (eqt key 'no-key) `(:key ,key)))))
      (and (check-scaffold-copy new newcopy)
	   (check-scaffold-copy pred predcopy)
	   (check-scaffold-copy tree treecopy)
	   result))))

(defun check-subst-if-not (new pred tree &key (key 'no-key))
  "Call subst-if-not new pred tree, with various keyword arguments
   if present.  Check that the arguments are not changed."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (let ((newcopy (make-scaffold-copy new))
	(predcopy (make-scaffold-copy pred))
	(treecopy (make-scaffold-copy tree)))
    (let ((result
	   (apply #'subst-if-not new pred tree
		  (unless (eqt key 'no-key) `(:key ,key)))))
      (and (check-scaffold-copy new newcopy)
	   (check-scaffold-copy pred predcopy)
	   (check-scaffold-copy tree treecopy)
	   result))))

(defun check-nsubst (new old tree &key (key 'no-key) test test-not)
  "Call nsubst new old tree, with keyword arguments if present."
  (setf new (copy-tree new))
  (setf old (copy-tree old))
  (setf tree (copy-tree tree))
  (apply #'nsubst new old tree
	 `(,@(unless (eqt key 'no-key) `(:key ,key))
	     ,@(when test `(:test ,test))
	     ,@(when test-not `(:test-not ,test-not)))))

(defun check-nsubst-if (new pred tree &key (key 'no-key))
  "Call nsubst-if new pred tree, with keyword arguments if present."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (apply #'nsubst-if new pred tree
	 (unless (eqt key 'no-key) `(:key ,key))))

(defun check-nsubst-if-not (new pred tree &key (key 'no-key))
  "Call nsubst-if-not new pred tree, with keyword arguments if present."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (apply #'nsubst-if-not new pred tree
		  (unless (eqt key 'no-key) `(:key ,key))))

(defun check-copy-list-copy (x y)
  "Check that y is a copy of the list x."
  (if
      (consp x)
      (and (consp y)
	   (not (eqt x y))
	   (eqt (car x) (car y))
	   (check-copy-list-copy (cdr x) (cdr y)))
    (and (eqt x y) t)))

(defun check-copy-list (x)
  "Apply copy-list, checking that it properly copies,
   and checking that it does not change its argument."
  (let ((xcopy (make-scaffold-copy x)))
    (let ((y (copy-list x)))
      (and
       (check-scaffold-copy x xcopy)
       (check-copy-list-copy x y)
       y))))

(defun append-6-body ()
  (let* ((cal (min 2048 call-arguments-limit))
	 (step (max 1 (floor (/ cal) 64))))
    (loop
     for n from 0
     below cal
     by step
     count
     (not
      (equal
       (apply #'append (loop for i from 1 to n
			     collect '(a)))
       (make-list n :initial-element 'a))))))

(defun is-intersection (x y z)
  "Check that z is the intersection of x and y."
  (and
   (listp x)
   (listp y)
   (listp z)
   (loop for e in x
	 always (or (not (member e y))
		    (member e z)))
   (loop for e in y
	 always (or (not (member e x))
		    (member e z)))
   (loop for e in z
	 always (and (member e x) (member e y)))
   t))

(defun shuffle (x)
  (cond
   ((null x) nil)
   ((null (cdr x)) x)
   (t
    (multiple-value-bind
	(y z)
	(split-list x)
      (append (shuffle y) (shuffle z))))))

(defun split-list (x)
  (cond
   ((null x) (values nil nil))
   ((null (cdr x)) (values x nil))
   (t
    (multiple-value-bind
	(y z)
	(split-list (cddr x))
      (values (cons (car x) y) (cons (cadr x) z))))))

(defun intersection-12-body (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size
			     collect (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size
			     collect (random maxelem state)))))
       (let ((z (intersection x y)))
	 (let ((is-good (is-intersection x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))

(defun nintersection-with-check (x y &key test)
  (let ((ycopy (make-scaffold-copy y)))
    (let ((result (if test
		      (nintersection x y :test test)
		    (nintersection x y))))
      (if (check-scaffold-copy y ycopy)
	  result
	'failed))))

(defun nintersection-12-body (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state t)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size
			     collect (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size
			     collect (random maxelem state)))))
       (let ((z (nintersection-with-check (copy-list x) y)))
	 (when (eqt z 'failed) (return (values x y z)))
	 (let ((is-good (is-intersection x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))


(defun union-with-check (x y &key test test-not)
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result (cond
		   (test (union x y :test test))
		   (test-not (union x y :test-not test-not))
		   (t (union x y)))))
      (if (and (check-scaffold-copy x xcopy)
	       (check-scaffold-copy y ycopy))
	  result
	'failed))))

(defun union-with-check-and-key (x y key &key test test-not)
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result  (cond
		   (test (union x y :key key :test test))
		   (test-not (union x y :key key :test-not test-not))
		   (t (union x y :key key)))))
      (if (and (check-scaffold-copy x xcopy)
	       (check-scaffold-copy y ycopy))
	  result
	'failed))))

(defun check-union (x y z)
  (and (listp x)
       (listp y)
       (listp z)
       (loop for e in z always (or (member e x) (member e y)))
       (loop for e in x always (member e z))
       (loop for e in y always (member e z))
       t))

(defun do-random-unions (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
       for i from 1 to niters do
	  (let ((x (shuffle (loop for j from 1 to size collect
				  (random maxelem state))))
		(y (shuffle (loop for j from 1 to size collect
				  (random maxelem state)))))
	    (let ((z (union x y)))
	      (let ((is-good (check-union x y z)))
		(unless is-good (return (values x y z)))))))
    nil))

(defun nunion-with-copy (x y &key test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (cond
   (test (nunion x y :test test))
   (test-not (nunion x y :test-not test-not))
   (t (nunion x y))))

(defun nunion-with-copy-and-key (x y key &key test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (cond
   (test (nunion x y :key key :test test))
   (test-not (nunion x y :key key :test-not test-not))
   (t (nunion x y :key key))))

(defun do-random-nunions (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
       for i from 1 to niters do
	  (let ((x (shuffle (loop for j from 1 to size collect
				  (random maxelem state))))
		(y (shuffle (loop for j from 1 to size collect
				  (random maxelem state)))))
	    (let ((z (nunion-with-copy x y)))
	      (let ((is-good (check-union x y z)))
		(unless is-good (return (values x y z)))))))
    nil))

(defun set-difference-with-check (x y &key (key 'no-key)
					   test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result (apply #'set-difference
			 x y
			 `(,@(unless (eqt key 'no-key) `(:key ,key))
			   ,@(when test `(:test ,test))
			   ,@(when test-not `(:test-not ,test-not))))))  
      (cond
       ((and (check-scaffold-copy x xcopy)
	     (check-scaffold-copy y ycopy))
	result)
       (t
	'failed)))))

(defun check-set-difference (x y z &key (key #'identity)
					(test #'eql))
  (and
   ;; (not (eqt 'failed z))
   (listp x)
   (listp y)
   (listp z)
   (loop for e in z always (member e x :key key :test test))
   (loop for e in x always (or (member e y :key key :test test)
			       (member e z :key key :test test)))
   (loop for e in y never  (member e z :key key :test test))
   t))

(defun do-random-set-differences (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
			     (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size collect
			     (random maxelem state)))))
       (let ((z (set-difference-with-check x y)))
	 (let ((is-good (check-set-difference x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))
(defun nset-difference-with-check (x y &key (key 'no-key)
				     test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (apply #'nset-difference
	 x y
	 `(,@(unless (eqt key 'no-key) `(:key ,key))
	     ,@(when test `(:test ,test))
	     ,@(when test-not `(:test-not ,test-not)))))

(defun check-nset-difference (x y z &key (key #'identity)
				(test #'eql))
  (and
   (listp x)
   (listp y)
   (listp z)
   (loop for e in z always (member e x :key key :test test))
   (loop for e in x always (or (member e y :key key :test test)
			       (member e z :key key :test test)))
   (loop for e in y never  (member e z :key key :test test))
   t))

(defun do-random-nset-differences (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
			     (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size collect
			     (random maxelem state)))))
       (let ((z (nset-difference-with-check x y)))
	 (let ((is-good (check-nset-difference x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))

(defun set-exclusive-or-with-check (x y &key (key 'no-key)
				      test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result (apply #'set-exclusive-or
			 x y
			 `(,@(unless (eqt key 'no-key) `(:key ,key))
			     ,@(when test `(:test ,test))
			     ,@(when test-not `(:test-not ,test-not))))))  
      (cond
       ((and (check-scaffold-copy x xcopy)
	     (check-scaffold-copy y ycopy))
	result)
       (t
	'failed)))))

(defun check-set-exclusive-or (x y z &key (key #'identity)
				 (test #'eql))
  (and
   ;; (not (eqt 'failed z))
   (listp x)
   (listp y)
   (listp z)
   (loop for e in z always (or (member e x :key key :test test)
			       (member e y :key key :test test)))
   (loop for e in x always (if (member e y :key key :test test)
			       (not (member e z :key key :test test))
			     (member e z :key key :test test)))
   (loop for e in y always (if (member e x :key key :test test)
			       (not (member e z :key key :test test))
			     (member e z :key key :test test)))
   t))

#|
(defun do-random-set-exclusive-ors (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
			     (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size collect
			     (random maxelem state)))))
       (let ((z (set-exclusive-or-with-check x y)))
	 (let ((is-good (check-set-exclusive-or x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))
|#

(defun nset-exclusive-or-with-check (x y &key (key 'no-key)
				       test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (apply #'nset-exclusive-or
	 x y
	 `(,@(unless (eqt key 'no-key) `(:key ,key))
	     ,@(when test `(:test ,test))
	     ,@(when test-not `(:test-not ,test-not)))))

#|
(defun do-random-nset-exclusive-ors (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
			     (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size collect
			     (random maxelem state)))))
       (let ((z (nset-exclusive-or-with-check x y)))
	 (let ((is-good (check-set-exclusive-or x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))
|#

(defun subsetp-with-check (x y &key (key 'no-key) test test-not)
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result
	   (apply #'subsetp x y
		  `(,@(unless (eqt key 'no-key)
			`(:key ,key))
		      ,@(when test `(:test ,test))
		      ,@(when test-not `(:test-not ,test-not))))))
      (cond
       ((and (check-scaffold-copy x xcopy)
	     (check-scaffold-copy y ycopy))
	(not (not result)))
       (t 'failed)))))

(defun my-set-exclusive-or (set1 set2 &key key test test-not)

  (assert (not (and test test-not)))

  (cond
   (test-not (when (symbolp test-not)
	       (setq test-not (symbol-function test-not)))
	     (setq test (complement test-not)))
   ((not test) (setq test #'eql)))

  ;;; (when (symbolp test) (setq test (symbol-function test)))
  (etypecase test
    (symbol (setq test (symbol-function test)))
    (function nil))

  (etypecase key
    (null nil)
    (symbol (setq key (symbol-function key)))
    (function nil))

  (let* ((keys1 (if key (mapcar (the function key) set1) set1))
	 (keys2 (if key (mapcar (the function key) set2) set2))
	 (mask1 (make-array (length set1) :element-type 'bit
			    :initial-element 0))
	 (mask2 (make-array (length set2) :element-type 'bit
			    :initial-element 0)))
    (loop for i1 from 0
	  for k1 in keys1
	  do
	  (loop for i2 from 0
		for k2 in keys2
		when (funcall (the function test) k1 k2)
		do (setf (sbit mask1 i1) 1
			 (sbit mask2 i2) 1)))
    (nconc
     (loop for e in set1
	   for i across mask1
	   when (= i 0)
	   collect e)
     (loop for e in set2
	   for i across mask2
	   when (= i 0)
	   collect e))))

(defun make-random-set-exclusive-or-input (n)
  (let* ((set1 (loop for i from 1 to n collect (random n)))
	 (set2 (loop for i from 1 to n collect (random n)))
	 (test-args
	  (random-case nil nil nil
		       (list :test 'eql)
		       (list :test #'eql)
		       (list :test (complement #'eql))))
	 (test-not-args
	  (and (not test-args)
	       (random-case nil nil (list :test-not 'eql)
			    (list :test-not #'eql)
			    (list :test-not (complement #'eql)))))
	 (key-args
	  (random-case nil nil nil nil
		       (list :key nil)
		       (list :key 'identity)
		       (list :key 'not))))
    (list* set1 set2
	  (reduce #'append (random-permute
			    (list test-args test-not-args key-args))))))

(defun random-set-exclusive-or-test (n reps &optional (fn 'set-exclusive-or))
  (let ((actual-fn (etypecase fn
		     (symbol (symbol-function fn))
		     (function fn))))
    (declare (type function actual-fn))
    (loop for i below reps
	  for args = (make-random-set-exclusive-or-input n)
	  for set1 = (car args)
	  for set2 = (cadr args)
	  for result1 = (apply #'remove-duplicates
			       (sort (copy-list (apply #'my-set-exclusive-or args))
				     #'<)
			       (cddr args))
	  for result2 = (apply #'remove-duplicates
			       (sort (copy-list (apply actual-fn
						       (copy-list set1)
						       (copy-list set2)
						       (cddr args)))
				     #'<)
			       (cddr args))
	  unless (equal result1 result2)
	  return (list (list 'remove-duplicates (list 'sort (cons fn args) '<) "...")
		       "actual: " result2 "should be: " result1))))

(defun rev-assoc-list (x)
  (cond
   ((null x) nil)
   ((null (car x))
    (cons nil (rev-assoc-list (cdr x))))
   (t
    (acons (cdar x) (caar x) (rev-assoc-list (cdr x))))))

(defvar *mapc.6-var* nil)
(defun mapc.6-fun (x)
  (push x *mapc.6-var*)
  x)
