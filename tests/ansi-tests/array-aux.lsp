;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 05:11:31 2003
;;;; Contains: Auxiliary functions for array tests

(in-package :cl-test)

(defun make-array-check-upgrading (type)
  (subtypep* type (array-element-type (make-array 0 :element-type type))))

(defun subtypep-or-unknown (subtype supertype)
  (multiple-value-bind* (is-subtype is-known)
      (subtypep subtype supertype)
    (or (not is-known) (notnot is-subtype))))

(defun make-array-with-checks (dimensions
			       &rest options
			       &key
			       (element-type t element-type-p)
			       (initial-contents nil initial-contents-p)
			       (initial-element nil initial-element-p)
			       (adjustable nil)
			       (fill-pointer nil)
			       (displaced-to nil)
			       (displaced-index-offset 0 dio-p)
			       &aux
			       (dimensions-list (if (listp dimensions)
						    dimensions
						  (list dimensions))))
  "Call MAKE-ARRAY and do sanity tests on the output."
  (declare (ignore element-type-p initial-contents initial-contents-p
		   initial-element initial-element-p dio-p))
  (let ((a (check-values (apply #'make-array dimensions options)))
	(rank (length dimensions-list)))
    (cond

     ((not (typep a 'array))
      :fail-not-array)
     ((not (typep a (find-class 'array)))
      :fail-not-array-class)
     ((not (typep a '(array *)))
      :fail-not-array2)
     ((not (typep a `(array * ,dimensions-list)))
      :fail-not-array3)
     ((not (typep a `(array * *)))
      :fail-not-array4)
     ((not (typep a `(array ,element-type)))
      :fail-not-array5)
     ((not (typep a `(array ,element-type *)))
      :fail-not-array6)
     
;     #-gcl
     ((not (typep a `(array ,element-type ,rank)))
      :fail-not-array7)

     ((not (typep a `(array ,element-type ,dimensions-list)))
      :fail-not-array8)

     ((not (typep a `(array ,element-type ,(mapcar (constantly '*)
						   dimensions-list))))
      :fail-not-array9)

     ((loop for i from 0 below (min 10 rank)
	    thereis
	    (let ((x (append (subseq dimensions-list 0 i)
			     (list '*)
			     (subseq dimensions-list (1+ i)))))
	      (or (not (typep a `(array * ,x)))
		  (not (typep a `(array ,element-type ,x))))))
      :fail-not-array10)

     ((not (check-values (arrayp a))) :fail-not-arrayp)

     ((and ;; (eq t element-type)
	   (not adjustable)
	   (not fill-pointer)
	   (not displaced-to)
	   (cond
	    ((not (typep a 'simple-array))
	     :fail-not-simple-array)
	    ((not (typep a '(simple-array *)))
	     :fail-not-simple-array2)
	    ((not (typep a `(simple-array * ,dimensions-list)))
	     :fail-not-simple-array3)
	    ((not (typep a `(simple-array * *)))
	     :fail-not-simple-array4)
	    ((not (typep a `(simple-array ,element-type)))
	     :fail-not-simple-array5)
	    ((not (typep a `(simple-array ,element-type *)))
	     :fail-not-simple-array6)
	    #-gcl
	    ((not (typep a `(simple-array ,element-type
					  ,rank)))
	     :fail-not-array7)
	    ((not (typep a `(simple-array ,element-type ,dimensions-list)))
	     :fail-not-simple-array8)
	    ((not (typep a `(simple-array ,element-type
					  ,(mapcar (constantly '*)
						   dimensions-list))))
	     :fail-not-simple-array9)
	    )))

     ;; If the array is a vector, check that...
     ((and (eql rank 1) 
	   (cond
	    ;; ...It's in type vector
	    ((not (typep a 'vector))
	     :fail-not-vector)
	    ;; ...If the element type is a subtype of BIT, then it's a
	    ;; bit vector...
	    ((and (subtypep 'bit element-type)
		  (subtypep element-type 'bit)
		  (or (not (bit-vector-p a))
		      (not (typep a 'bit-vector))))
	     :fail-not-bit-vector)
	    ;; ...If not adjustable, fill pointered, or displaced,
	    ;; then it's a simple vector or simple bit vector
	    ;; (if the element-type is appropriate)
	    ((and (not adjustable)
		  (not fill-pointer)
		  (not displaced-to)
		  (cond
		   ((and (eq t element-type)
			 (or (not (simple-vector-p a))
			     (not (typep a 'simple-vector))))
		    :fail-not-simple-vector)
		   ((and (subtypep 'bit element-type)
			 (subtypep element-type 'bit)
			 (or (not (simple-bit-vector-p a))
			     (not (typep a 'simple-bit-vector))))
		    :fail-not-simple-bit-vector) ))) )))

     ;; The dimensions of the array must be initialized properly
     ((not (equal (array-dimensions a) dimensions-list))
      :fail-array-dimensions)

     ;; The rank of the array must equal the number of dimensions
     ((not (equal (array-rank a) rank))
      :fail-array-rank)

     ;; Arrays other than vectors cannot have fill pointers
     ((and (not (equal (array-rank a) 1))
	   (array-has-fill-pointer-p a))
      :fail-non-vector-fill-pointer)

     ;; The actual element type must be a supertype of the element-type
     ;; argument
     ((not (subtypep-or-unknown element-type (array-element-type a)))
      :failed-array-element-type)

     ;; If :adjustable is given, the array must be adjustable.
     ((and adjustable
	   (not (check-values (adjustable-array-p a)))
	   :fail-adjustable))

     ;; If :fill-pointer is given, the array must have a fill pointer
     ((and fill-pointer
	   (not (check-values (array-has-fill-pointer-p a)))
	   :fail-has-fill-pointer))

     ;; If the fill pointer is given as an integer, it must be the value
     ;; of the fill pointer of the new array
     ((and (check-values (integerp fill-pointer))
	   (not (eql fill-pointer (check-values (fill-pointer a))))
	   :fail-fill-pointer-1))

     ;; If the fill-pointer argument is t, the fill pointer must be
     ;; set to the vector size.
     ((and (eq fill-pointer t)
	   (not (eql (first dimensions-list) (fill-pointer a)))
	   :fail-fill-pointer-2))

     ;; If displaced-to another array, check that this is proper
     ((and
       displaced-to
       (multiple-value-bind* (actual-dt actual-dio)
	   (array-displacement a)
	 (cond
	  ((not (eq actual-dt displaced-to))
	   :fail-displacement-1)
	  ((not (eql actual-dio displaced-index-offset))
	   :fail-displaced-index-offset)))))

     ;; Test of array-total-size
     ((not (eql (check-values (array-total-size a))
		(reduce #'* dimensions-list :initial-value 1)))
      :fail-array-total-size)

     ;; Test array-row-major-index on all zeros
     ((and (> (array-total-size a) 0)
	   (not (eql (check-values
		      (apply #'array-row-major-index
			     a (make-list (array-rank a) :initial-element 0)))
		     0)))
      :fail-array-row-major-index-0)

     ;; For the last entry
     ((and (> (array-total-size a) 0)
	   (not (eql (apply #'array-row-major-index
			    a (mapcar #'1- dimensions-list))
		     (1- (reduce #'* dimensions-list :initial-value 1)))))
      :fail-array-row-major-index-last)

     ;; No problems -- return the array
     (t a))))
