; -*- Mode:Lisp; Package:CCL; -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

(defun bit (bit-array &rest subscripts)
  "Return the bit from the BIT-ARRAY at the specified SUBSCRIPTS."
  (declare (dynamic-extent subscripts))
  (unless (eq (array-element-type bit-array) 'bit)
    (report-bad-arg bit-array '(array bit)))
  (apply #'aref bit-array subscripts))

(defun %bitset (bit-array &rest stuff)
  (declare (dynamic-extent stuff))
  (unless (eq (array-element-type bit-array) 'bit)
    (report-bad-arg bit-array '(array bit)))
  (apply #'aset bit-array stuff))

(defun sbit (v &optional (sub0 nil sub0-p) &rest others)
  "Return the bit from SIMPLE-BIT-ARRAY at the specified SUBSCRIPTS."
  (declare (dynamic-extent others))
  (if sub0-p
    (if others
      (apply #'bit v sub0 others)
      ( sbit (require-type v 'simple-bit-vector) sub0))
    (bit v)))

(defun %sbitset (v sub0 &optional (newval nil newval-p) &rest newval-was-really-sub1)
  (declare (dynamic-extent newval-was-really-sub1))
  (if newval-p
    (if newval-was-really-sub1
      (apply #'%bitset v sub0 newval newval-was-really-sub1)
      (progn
        (unless (typep v 'simple-bit-vector)
          (report-bad-arg v 'simple-bit-vector))
        (uvset v sub0 newval)))
    (%bitset v sub0)))

(defun bit-and (bit-array1 bit-array2 &optional result-bit-array)
  "Perform a bit-wise LOGAND on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
  putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
  BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
  created. All the arrays must have the same rank and dimensions."
   (bit-boole boole-and bit-array1 bit-array2 result-bit-array))

(defun bit-ior (bit-array1 bit-array2 &optional result-bit-array)
  "Perform a bit-wise LOGIOR on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
  putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
  BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
  created. All the arrays must have the same rank and dimensions."
  (bit-boole  boole-ior bit-array1 bit-array2 result-bit-array))

(defun bit-xor (bit-array1 bit-array2 &optional result-bit-array)
  "Perform a bit-wise LOGXOR on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
  putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
  BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
  created. All the arrays must have the same rank and dimensions."
   (bit-boole  boole-xor bit-array1 bit-array2 result-bit-array))

(defun bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
  "Perform a bit-wise LOGEQV on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
  putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
  BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
  created. All the arrays must have the same rank and dimensions."
  (bit-boole boole-eqv bit-array1 bit-array2 result-bit-array))

(defun bit-nand (bit-array1 bit-array2 &optional result-bit-array)
  "Perform a bit-wise LOGNAND on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
  putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
  BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
  created. All the arrays must have the same rank and dimensions."
  (bit-boole boole-nand bit-array1 bit-array2 result-bit-array))

(defun bit-nor (bit-array1 bit-array2 &optional result-bit-array)
  "Perform a bit-wise LOGNOR on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
  putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
  BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
  created. All the arrays must have the same rank and dimensions."
  (bit-boole boole-nor bit-array1 bit-array2 result-bit-array))

(defun bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Perform a bit-wise LOGANDC1 on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
  putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
  BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
  created. All the arrays must have the same rank and dimensions."
  (bit-boole boole-andc1 bit-array1 bit-array2 result-bit-array))

(defun bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Perform a bit-wise LOGANDC2 on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
  putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
  BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
  created. All the arrays must have the same rank and dimensions."
  (bit-boole boole-andc2 bit-array1 bit-array2 result-bit-array))

(defun bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Perform a bit-wise LOGORC1 on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
  putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
  BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
  created. All the arrays must have the same rank and dimensions."
  (bit-boole boole-orc1 bit-array1 bit-array2 result-bit-array))

(defun bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Perform a bit-wise LOGORC2 on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
  putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
  BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
  created. All the arrays must have the same rank and dimensions."
  (bit-boole boole-orc2 bit-array1 bit-array2 result-bit-array))

(defun bit-not (bit-array &optional result-bit-array)
  "Performs a bit-wise logical NOT on the elements of BIT-ARRAY,
  putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
  BIT-ARRAY is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
  created. Both arrays must have the same rank and dimensions."
  (bit-boole boole-nor bit-array bit-array result-bit-array))

(defun result-bit-array (bit-array-1 bit-array-2 result)
  ; Check that the two bit-array args are bit-arrays with
  ; compatible dimensions.  If "result" is specified as T,
  ; return bit-array-1.  If result is unspecified, return
  ; a new bit-array of the same dimensions as bit-array-2.
  ; Otherwise, make sure that result is a bit-array of the
  ; same dimensions as the other two arguments and return
  ; it.
  (let* ((typecode-1 (typecode bit-array-1))
         (typecode-2 (typecode bit-array-2)))
    (declare (fixnum typecode-1 typecode-2))
    (flet ((bit-array-dimensions (bit-array typecode)
             (declare (fixnum typecode))
             (if (= typecode target::subtag-bit-vector)
               (uvsize bit-array)
               (let* ((array-p (= typecode target::subtag-arrayH))
                      (vector-p (= typecode target::subtag-vectorH)))
                 (if (and (or array-p vector-p) 
                          (= (the fixnum (%array-header-subtype bit-array)) target::subtag-bit-vector))
                   (if vector-p
                     (array-dimension bit-array 0)
                     (array-dimensions bit-array))
                   (report-bad-arg bit-array '(array bit))))))
           (check-matching-dimensions (a1 d1 a2 d2)
             (unless (equal d1 d2)
               (error "~s and ~s have different dimensions." a1 a2))
             a2))
      (let* ((dims-1 (bit-array-dimensions bit-array-1 typecode-1))
             (dims-2 (bit-array-dimensions bit-array-2 typecode-2)))
        (check-matching-dimensions bit-array-1 dims-1 bit-array-2 dims-2)
        (if result
          (if (eq result t)
            bit-array-1
            (check-matching-dimensions bit-array-2 dims-2 result (bit-array-dimensions result (typecode result))))
          (make-array dims-2 :element-type 'bit :initial-element 0))))))




  
(defun bit-boole (opcode array1 array2 result-array)
  (unless (eql opcode (logand 15 opcode))
    (setq opcode (require-type opcode '(mod 16))))
  (let* ((result (result-bit-array array1 array2 result-array)))
    (if (and (typep array1 'simple-bit-vector)
             (typep array2 'simple-bit-vector)
             (typep result 'simple-bit-vector))
      (%simple-bit-boole opcode array1 array2 result)
      (multiple-value-bind (v1 i1) (array-data-and-offset array1)
        (declare (simple-bit-vector v1) (fixnum i1))
        (multiple-value-bind (v2 i2) (array-data-and-offset array2)
          (declare (simple-bit-vector v2) (fixnum i2))
          (multiple-value-bind (v3 i3) (array-data-and-offset result)
            (declare (simple-bit-vector v3) (fixnum i3))
            (let* ((e3 (+ i3 (the fixnum (array-total-size result)))))
              (declare (fixnum e3))
              (do* ( )
                   ((= i3 e3) result)
                (setf (sbit v3 i3) 
                      (logand (boole opcode (sbit v1 i1) (sbit v2 i2)) 1))
                (incf i1)
                (incf i2)
                (incf i3)))))))))


          
          




; shrink-vector is called only in sequences-2. None of the calls depend on
; the side affect of setting the passed-in symbol to the [possibly new]
; returned vector
; Since there hasn't been such a thing as sequences-2 in about 7 years,
; this is especially puzzling.
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defmacro shrink-vector (vector to-size)
    `(setq ,vector (%shrink-vector ,vector ,to-size)))
  )


; new and faulty def
(defun %shrink-vector (vector to-size)
  (cond ((eq (length vector) to-size)
         vector)
        ((array-has-fill-pointer-p vector)
         (setf (fill-pointer vector) to-size)
         vector)
        (t (subseq vector 0 to-size))))


; this could be put into print-db as it was in ccl-pr-4.2
; Or it (and print-db) could just be flushed ... tough one.
(defun multi-dimension-array-to-list (array)
  "Produces a nested list of the elements in array."
  (mdal-aux array (array-dimensions array) nil 
            (array-dimensions array)))

(defun mdal-aux (array all-dimensions use-dimensions 
                       remaining-dimensions)
  (if (= (length all-dimensions) (length use-dimensions))
    (apply 'aref array use-dimensions)
    (do ((index 0 (1+ index))
         (d-length (car remaining-dimensions))
         (result nil))
        ((= d-length index) result)
      (setq result 
            (append result (list (mdal-aux array all-dimensions
                                           (append use-dimensions 
                                                   (list index))
                                           (cdr remaining-dimensions))))))))

(defun adjust-array (array dims
			   &key (element-type nil element-type-p)
			   (initial-element nil initial-element-p)
			   (initial-contents nil initial-contents-p)
			   (fill-pointer nil fill-pointer-p)
			   displaced-to
			   displaced-index-offset
			   &aux (subtype (array-element-subtype array)))
  "Adjust ARRAY's dimensions to the given DIMENSIONS and stuff."
  (when (and element-type-p
             (neq (element-type-subtype element-type) subtype))
    (error "~S is not of element type ~S" array element-type))
  (when (integerp dims)(setq dims (list dims))) ; because %displace-array wants the list
  (if (neq (list-length dims)(array-rank array))
    (error "~S has wrong rank for adjusting to dimensions ~S" array dims))
  (let ((size 1)
        (explicitp nil))
    (dolist (dim dims)
      (when (< dim 0)(report-bad-arg dims '(integer 0 *)))
      (setq size (* size dim)))
    (when (and (neq fill-pointer t)
               (array-has-fill-pointer-p array)
               (< size (or fill-pointer (fill-pointer array))))
      (error "Cannot adjust array ~S to size less than fill pointer ~S"
             array (or fill-pointer (fill-pointer array))))
    (when (and fill-pointer (not (array-has-fill-pointer-p array)))
      (error "~S does not have a fill pointer" array))
    (when (and displaced-index-offset (null displaced-to))
      (error "Cannot specify ~S without ~S" :displaced-index-offset :displaced-to))
    (when (and initial-element-p initial-contents-p)
      (error "Cannot specify both ~S and ~S" :initial-element :initial-contents))
    (cond 
      ((not (adjustable-array-p array))
       (let ((new-array (make-array-1  dims 
                                       (array-element-type array) T
                                       displaced-to
                                       displaced-index-offset
                                       nil
                                       (or fill-pointer
                                           (and (array-has-fill-pointer-p array)
                                                (fill-pointer array)))
                                       initial-element initial-element-p
                                       initial-contents initial-contents-p
                                       size)))
                     
	 (when (and (null initial-contents-p)
		    (null displaced-to))
	   (multiple-value-bind (array-data offs) (array-data-and-offset array)
	     (let ((new-array-data (array-data-and-offset new-array))) 
	       (cond ((null dims)
		      (uvset new-array-data 0 (uvref array-data offs)))
		     (T
		      (init-array-data array-data offs (array-dimensions array) 
				       new-array-data 0 dims))))))
	 (setq array new-array)))
      (T (cond 
	   (displaced-to
	    (if (and displaced-index-offset 
		     (or (not (fixnump displaced-index-offset))
			 (< displaced-index-offset 0)))
	      (report-bad-arg displaced-index-offset '(integer 0 #.most-positive-fixnum)))
	    (when (or initial-element-p initial-contents-p)
	      (error "Cannot specify initial values for displaced arrays"))
	    (unless (eq subtype (array-element-subtype displaced-to))
	      (error "~S is not of element type ~S"
		     displaced-to (array-element-type array)))
	    (do* ((vec displaced-to (displaced-array-p vec)))
		 ((null vec) ())
	      (when (eq vec array)
		(error "Array cannot be displaced to itself.")))
	    (setq explicitp t))
	   (T
	    (setq displaced-to (%alloc-misc size subtype))
	    (cond (initial-element-p
		   (dotimes (i (the fixnum size)) (uvset displaced-to i initial-element)))
		  (initial-contents-p
		   (if (null dims) (uvset displaced-to 0 initial-contents)
                     (init-uvector-contents displaced-to 0 dims initial-contents))))
	    (cond ((null dims)
		   (uvset displaced-to 0 (aref array)))
		  ((not initial-contents-p)
		   (multiple-value-bind (vec offs) (array-data-and-offset array)
		     (init-array-data vec offs (array-dimensions array) displaced-to 0 dims))))))
	 (%displace-array array dims size displaced-to (or displaced-index-offset 0) explicitp)))
    (when fill-pointer-p
      (cond
        ((eq fill-pointer t)
         (set-fill-pointer array size))
        (fill-pointer
         (set-fill-pointer array fill-pointer))))
    array))

(defun array-dims-sizes (dims)
   (if (or (atom dims) (null (%cdr dims))) dims
     (let ((ndims (array-dims-sizes (%cdr dims))))
       (cons (* (%car dims) (%car ndims)) ndims))))

(defun init-array-data (vec off dims nvec noff ndims)
   (init-array-data-aux vec off dims (array-dims-sizes (cdr dims))
                        nvec noff ndims (array-dims-sizes (cdr ndims))))

(defun init-array-data-aux (vec off dims siz nvec noff ndims nsiz)
   (when (null siz)
      (return-from init-array-data-aux
         (init-vector-data vec off (car dims) nvec noff (car ndims))))
   (let ((count (pop dims))
         (size (pop siz))
         (ncount (pop ndims))
         (nsize (pop nsiz)))
     (dotimes (i (if (%i< count ncount) count ncount))
        (declare (fixnum i))
        (init-array-data-aux vec off dims siz nvec noff ndims nsiz)
        (setq off (%i+ off size) noff (%i+ noff nsize)))))

(defun init-vector-data (vec off len nvec noff nlen)
  (dotimes (i (if (%i< len nlen) len nlen))
     (declare (fixnum i))
     (uvset nvec noff (uvref vec off))
     (setq off (%i+ off 1) noff (%i+ noff 1))))

;;; only caller is adjust-array

(defun %displace-array (array dims size data offset explicitp)
  (let* ((typecode (typecode array))
         (array-p (eql typecode target::subtag-arrayH))
         (vector-p (eql typecode target::subtag-vectorH)))
    (unless (or array-p vector-p)
      (error "Array ~S cannot be displaced" array))
    (unless (fixnump offset) (report-bad-arg offset '(integer 0 #.most-positive-fixnum)))
    (unless (adjustable-array-p data)
      (multiple-value-bind (ndata noffset) (displaced-array-p data)
        (if ndata (setq data ndata offset (%i+ offset noffset)))))
    (unless (and (fixnump size) (%i<= (%i+ offset size) (array-total-size data)))
      (error "Offset ~S + size ~S must be less than size of array displaced-to" offset size))
    (let* ((flags (%svref array target::vectorH.flags-cell)))
      (declare (fixnum flags))
      (setf (%svref array target::vectorH.flags-cell)
            (if (> (the fixnum (typecode data)) target::subtag-vectorH)
              (bitclr $arh_disp_bit flags)
              (bitset $arh_disp_bit flags)))
      (setf (%svref array target::vectorH.flags-cell)
            (if explicitp
              (bitset $arh_exp_disp_bit flags)
              (bitclr $arh_exp_disp_bit flags)))
      (setf (%svref array target::arrayH.data-vector-cell) data)
      (if array-p
        (progn
          (do ((i target::arrayH.dim0-cell (1+ i)))
              ((null dims))
            (declare (fixnum i))
            (setf (%svref array i) (pop dims)))
          (setf (%svref array target::arrayH.physsize-cell) size)
          (setf (%svref array target::arrayH.displacement-cell) offset))
        (progn
          (if (or (not (logbitp $arh_fill_bit flags))
                  (> (the fixnum (%svref array target::vectorH.logsize-cell)) size))
            (setf (%svref array target::vectorH.logsize-cell) size))
          (setf (%svref array target::vectorH.physsize-cell) size)
          (setf (%svref array target::vectorH.displacement-cell) offset)))
      array)))



(defun array-row-major-index (array &lexpr subscripts)
  (let ((rank  (array-rank array))
        (nsubs (%lexpr-count subscripts))
        (sum 0))
    (declare (fixnum sum rank))
    (unless (eql rank nsubs)
      (%err-disp $xndims array nsubs))    
      (if (eql 0 rank)
        0
        (do* ((i (1- rank) (1- i))
              (dim (array-dimension array i) (array-dimension array i))
              (last-size 1 size)
              (size dim (* dim size)))
             (nil)
          (declare (fixnum i last-size size))
          (let ((s (%lexpr-ref subscripts nsubs i)))
            (unless (fixnump s)
              (setq s (require-type s 'fixnum)))
            (when (or (< s 0) (>= s dim))
              (%err-disp $XARROOB (%apply-lexpr 'list subscripts) array))
            (incf sum (the fixnum (* s last-size)))
            (when (eql i 0) (return sum)))))))

(defun array-in-bounds-p (array &lexpr subscripts)
  "Return T if the SUBSCIPTS are in bounds for the ARRAY, NIL otherwise."
  (let ((rank  (array-rank array))
        (nsubs (%lexpr-count subscripts)))
    (declare (fixnum nsubs rank))    
    (if (not (eql nsubs rank))
      (%err-disp $xndims array nsubs)
      (if (eql 0 rank)
        0
        (do* ((i (1- rank) (1- i))
              (dim (array-dimension array i) (array-dimension array i)))
             (nil)
          (declare (fixnum i dim))
          (let ((s  (%lexpr-ref subscripts nsubs i)))
	    (if (typep s 'fixnum)
	      (locally (declare (fixnum s))
		(if (or (< s 0)(>= s dim)) (return nil)))
	      (if (typep s 'bignum)
		(return nil)
		(report-bad-arg s 'integer)))
            (when (eql i 0) (return t))))))))

(defun row-major-aref (array index)
  "Return the element of array corressponding to the row-major index. This is
   SETF'able."
  (multiple-value-bind (displaced-to offset) (displaced-array-p array)
    (aref (or displaced-to array) (+ index offset))))

(defun row-major-aset (array index new)
  (multiple-value-bind (displaced-to offset) (displaced-array-p array)
    (setf (aref (or displaced-to array) (+ index offset)) new)))

(defsetf row-major-aref row-major-aset)
             


; end
