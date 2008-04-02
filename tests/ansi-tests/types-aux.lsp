;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jun 21 20:14:38 2004
;;;; Contains: Aux. functions for types tests

(in-package :cl-test)

(defun classes-are-disjoint (c1 c2)
  "If either c1 or c2 is a builtin class or the name of a builtin
   class, then check for disjointness.  Return a non-NIL list
   of failed subtypep relationships, if any."
  (and (or (is-builtin-class c1)
	   (is-builtin-class c2))
       (check-disjointness c1 c2)))

(declaim (special *subtype-table*))

(defun types.6-body ()
  (loop
      for p in *subtype-table*
      for tp = (car p)
      append
      (and (not (member tp '(sequence cons list t)))
	   (let ((message (check-subtypep tp 'atom t t)))
	     (if message (list message))))))

(defparameter *type-list* nil)
(defparameter *supertype-table* nil)

(defun types.9-body ()
  (let ((tp-list (append '(keyword atom list)
			 (loop for p in *subtype-table* collect (car p))))
	(result-list))
    (setf tp-list (remove-duplicates tp-list))
    ;; TP-LIST is now a list of unique CL type names
    ;; Store it in *TYPE-LIST* so we can inspect it later if this test
    ;; fails.  The variable is also used in test TYPES.9A
    (setf *type-list* tp-list)
    ;; Compute all pairwise SUBTYPEP relationships among
    ;; the elements of *TYPE-LIST*.
    (let ((subs (make-hash-table :test #'eq))
	  (sups (make-hash-table :test #'eq)))
      (loop
	  for x in tp-list do
	    (loop
		for y in tp-list do
		  (multiple-value-bind (result good)
		      (subtypep* x y)
		    (declare (ignore good))
		    (when result
		      (pushnew x (gethash y subs))
		      (pushnew y (gethash x sups))))))
      ;; Store the supertype relations for later inspection
      ;; and use in test TYPES.9A
      (setf *supertype-table* sups)
      ;; Check that the relation we just computed is transitive.
      ;; Return a list of triples on which transitivity fails.
      (loop
	  for x in tp-list do
	    (let ((sub-list (gethash x subs))
		  (sup-list (gethash x sups)))
	      (loop
		  for t1 in sub-list do
		    (loop
			for t2 in sup-list do
			  (multiple-value-bind (result good)
			      (subtypep* t1 t2)
			    (when (and good (not result))
			      (pushnew (list t1 x t2) result-list
				       :test #'equal)))))))
      
      result-list)))

;;; TYPES.9-BODY returns a list of triples (T1 T2 T3)
;;; where (AND (SUBTYPEP T1 T2) (SUBTYPEP T2 T3) (NOT (SUBTYPEP T1 T3)))
;;;  (and where SUBTYPEP succeeds in each case, returning true as its
;;;   second return value.)

(defun types.9a-body ()
  (cond
   ((not (and *type-list* *supertype-table*))
    (format nil "Run test type.9 first~%")
    nil)
   (t
    (loop
     for tp in *type-list*
     sum
     (let ((sups (gethash tp *supertype-table*)))
       (loop
	for x in *universe*
	sum
	(handler-case
	 (cond
	  ((not (typep x tp)) 0)
	  (t
	   (loop
	    for tp2 in sups
	    count
	    (handler-case
	     (and (not (typep x tp2))
		  (progn
		    (format t "Found element of ~S not in ~S: ~S~%"
			    tp tp2 x)
		    t))
	     (condition (c) (format t "Error ~S occured: ~S~%"
				    c tp2)
			t)))))
	 (condition (c) (format t "Error ~S occured: ~S~%" c tp)
		    1))))))))

(defun check-subtypep (type1 type2 is-sub &optional should-be-valid)
  (multiple-value-bind
      (sub valid)
      (subtypep type1 type2)
    (unless (constantp type1) (setq type1 (list 'quote type1)))
    (unless (constantp type2) (setq type2 (list 'quote type2)))
    (if (or (and valid sub (not is-sub))
	    (and valid (not sub) is-sub)
	    (and (not valid) should-be-valid))
	`(((SUBTYPEP ,type1 ,type2) :==> ,sub ,valid))
      nil)))

;;; Check that the subtype relationships implied
;;; by disjointness are not contradicted.  Return NIL
;;; if ok, or a list of error messages if not.

;;; Assumes the types are nonempty.

(defun check-disjointness (type1 type2)
  (append
   (check-subtypep type1 type2 nil)
   (check-subtypep type2 type1 nil)
   (check-subtypep type1 `(not ,type2) t)
   (check-subtypep type2 `(not ,type1) t)
   (check-subtypep `(and ,type1 ,type2) nil t)
   (check-subtypep `(and ,type2 ,type1) nil t)
   (check-subtypep `(and ,type1 (not ,type2)) type1 t)
   (check-subtypep `(and (not ,type2) ,type1) type1 t)
   (check-subtypep `(and ,type2 (not ,type1)) type2 t)
   (check-subtypep `(and (not ,type1) ,type2) type2 t)
;;;   (check-subtypep type1 `(or ,type1 (not ,type2)) t)
;;;   (check-subtypep type1 `(or (not ,type2) ,type1) t)
;;;   (check-subtypep type2 `(or ,type2 (not ,type1)) t)
;;;   (check-subtypep type2 `(or (not ,type1) ,type2) t)
   (check-subtypep t `(or (not ,type1) (not ,type2)) t)
   (check-subtypep t `(or (not ,type2) (not ,type1)) t)
   ))

(defun check-equivalence (type1 type2)
  (append
   (check-subtypep type1 type2 t)
   (check-subtypep type2 type1 t)
   (check-subtypep `(not ,type1) `(not ,type2) t)
   (check-subtypep `(not ,type2) `(not ,type1) t)
   (check-subtypep `(and ,type1 (not ,type2)) nil t)
   (check-subtypep `(and ,type2 (not ,type1)) nil t)
   (check-subtypep `(and (not ,type2) ,type1) nil t)
   (check-subtypep `(and (not ,type1) ,type2) nil t)
   (check-subtypep t `(or ,type1 (not ,type2)) t)
   (check-subtypep t `(or ,type2 (not ,type1)) t)
   (check-subtypep t `(or (not ,type2) ,type1) t)
   (check-subtypep t `(or (not ,type1) ,type2) t)))

(defun check-all-subtypep (type1 type2)
  (append
   (check-subtypep type1 type2 t)
   (check-subtypep `(not ,type2) `(not ,type1) t)
   (check-subtypep `(and ,type1 (not ,type2)) nil t)
   (check-subtypep t `(or (not ,type1) ,type2) t)))

(defun check-all-not-subtypep (type1 type2)
  (append
   (check-subtypep type1 type2 nil)
   (check-subtypep `(not ,type2) `(not ,type1) nil)))

(defun subtypep-and-contrapositive-are-consistent (t1 t2)
  (multiple-value-bind (sub1 success1)
      (subtypep* t1 t2)
    (multiple-value-bind (sub2 success2)
	(subtypep* `(not ,t2) `(not ,t1))
      (or (not success1)
	  (not success2)
	  (eqlt sub1 sub2)))))

;;; For use in deftype tests
(deftype even-array (&optional type size)
  `(and (array ,type ,size)
	(satisfies even-size-p)))
