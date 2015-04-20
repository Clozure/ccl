;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")




; Return T if array or vector header, NIL if (simple-array * *), else
; error.

(defun %array-is-header (array)
  (if (typep array 'array)
    (let* ((typecode (typecode array)))
      (declare (fixnum typecode))
      (or (= typecode target::subtag-arrayH)
          (= typecode target::subtag-vectorH)))
    (report-bad-arg array 'array)))

(defun %set-fill-pointer (vectorh new)
  (setf (%svref vectorh target::vectorh.logsize-cell) new))

(defun %array-header-subtype (header)
  (the fixnum 
    (ldb target::arrayH.flags-cell-subtag-byte (the fixnum (%svref header target::arrayH.flags-cell)))))

(defun array-element-subtype (array)
  (if (%array-is-header array)
    (%array-header-subtype array)
    (typecode array)))
  
#+ppc32-target
(defconstant ppc32::*immheader-array-types*
  '#(short-float
     (unsigned-byte 32)
     (signed-byte 32)
     fixnum
     character
     (unsigned-byte 8)
     (signed-byte 8)
     (unsigned-byte 16)
     (signed-byte 16)
     double-float
     (complex single-float)
     (complex double-float)
     bit))

#+ppc64-target
(defconstant ppc64::*immheader-array-types*
  '#(unused
     unused
     unused
     unused
     (signed-byte 8)
     (signed-byte 16)
     (signed-byte 32)
     (signed-byte 64)
     (unsigned-byte 8)
     (unsigned-byte 16)
     (unsigned-byte 32)
     (unsigned-byte 64)
     unused
     (complex double-float)
     short-float
     fixnum
     unused
     unused
     unused
     double-float
     unused
     unused
     character
     (complex single-float)
     unused
     unused
     unused
     unused
     unused
     bit
     unused
     unused))

#+x8632-target
(defconstant x8632::*immheader-array-types*
  '#(short-float
     (unsigned-byte 32)
     (signed-byte 32)
     fixnum
     character
     (unsigned-byte 8)
     (signed-byte 8)
     (unsigned-byte 16)
     (signed-byte 16)
     double-float
     (complex single-float)
     (complex double-float)
     bit))

#+x8664-target
(progn
(defconstant x8664::*immheader-0-array-types*
  ;; ivector-class-other-bit
  #(unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    (complex double-float)
    (signed-byte 16)
    (unsigned-byte 16)
    character
    (signed-byte 8)
    (unsigned-byte 8)
    bit
    ))

(defconstant x8664::*immheader-1-array-types*
    ;; ivector-class-32-bit
  #(
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    character
    (signed-byte 32)
    (unsigned-byte 32)
    single-float))

(defconstant x8664::*immheader-2-array-types*
  ;; ivector-class-64-bit
  #(
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    (complex single-float)
    fixnum
    (signed-byte 64)
    (unsigned-byte 64)
    double-float))
    
)

#+arm-target
(defconstant arm::*immheader-array-types*
  '#(short-float
     (unsigned-byte 32)
     (signed-byte 32)
     fixnum
     character
     (unsigned-byte 8)
     (signed-byte 8)
     (unsigned-byte 16)
     (signed-byte 16)
     double-float
     (complex single-float)
     (complex double-float)
     bit))


(defun array-element-type (array)
  "Return the type of the elements of the array"
  (let* ((subtag (if (%array-is-header array)
                   (%array-header-subtype array)
                   (typecode array))))
    (declare (fixnum subtag))
    (if (= subtag target::subtag-simple-vector)
      t                                 ; only node CL array type
      #+ppc-target
      (svref target::*immheader-array-types*
             #+ppc32-target
             (ash (the fixnum (- subtag ppc32::min-cl-ivector-subtag)) -3)
             #+ppc64-target
             (ash (the fixnum (logand subtag #x7f)) (- ppc64::nlowtagbits)))
      #+x8632-target
      (svref x8632::*immheader-array-types*
	     (ash (the fixnum (- subtag x8632::min-cl-ivector-subtag))
		  (- x8632::ntagbits)))
      #+x8664-target
      (let* ((class (logand subtag x8664::fulltagmask))
             (idx (ash subtag (- x8664::ntagbits))))
        (declare (fixnum class idx))
        (cond ((= class x8664::ivector-class-64-bit)
               (%svref x8664::*immheader-2-array-types* idx))
              ((= class x8664::ivector-class-32-bit)
               (%svref x8664::*immheader-1-array-types* idx))
              (t
               (%svref x8664::*immheader-0-array-types* idx))))
      #+arm-target
      (svref arm::*immheader-array-types*
             (ash (the fixnum (- subtag arm::min-cl-ivector-subtag)) -3))
      )))



(defun adjustable-array-p (array)
  "Return T if (ADJUST-ARRAY ARRAY...) would return an array identical
   to the argument, this happens for complex arrays."
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (or (>= (the (unsigned-byte 8) (gvector-typecode-p typecode))
                target::subtag-arrayH)
            (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                target::min-cl-ivector-subtag))
      (if (or (= typecode target::subtag-arrayH)
              (= typecode target::subtag-vectorH))
        (logbitp $arh_adjp_bit (the fixnum (%svref array target::arrayH.flags-cell))))
      (report-bad-arg array 'array))))

(defun array-displacement (array)
  "Return the values of :DISPLACED-TO and :DISPLACED-INDEX-offset
   options to MAKE-ARRAY, or NIL and 0 if not a displaced array."
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (and (or (= typecode target::subtag-arrayH)
                 (= typecode target::subtag-vectorH))
             (logbitp $arh_exp_disp_bit
                      (the fixnum (%svref array target::arrayH.flags-cell))))
      (values (%svref array target::arrayH.data-vector-cell)
              (%svref array target::arrayH.displacement-cell))
      (if (array-typecode-p typecode)
        (values nil 0)
        (report-bad-arg array 'array)))))

(defun array-data-and-offset (array)
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (or (= typecode target::subtag-arrayH)
            (= typecode target::subtag-vectorH))
      (%array-header-data-and-offset array)
      (if (or (= typecode target::subtag-simple-vector)
              (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                  target::min-cl-ivector-subtag))
        (values array 0)
        (report-bad-arg array 'array)))))

(defun array-data-offset-subtype (array)
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (or (= typecode target::subtag-vectorH)
            (= typecode target::subtag-arrayH))
      (do* ((header array data)
            (offset (%svref header target::arrayH.displacement-cell)
                    (+ offset 
                       (the fixnum 
                         (%svref header target::arrayH.displacement-cell))))
            (data (%svref header target::arrayH.data-vector-cell)
                  (%svref header target::arrayH.data-vector-cell)))
           ((> (the fixnum (typecode data)) target::subtag-vectorH)
            (values data offset (typecode data)))
        (declare (fixnum offset)))
      (if (or (= typecode target::subtag-simple-vector)
              (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                  target::min-cl-ivector-subtag))
        (values array 0 typecode)
        (report-bad-arg array 'array)))))
  

(defun array-has-fill-pointer-p (array)
  "Return T if the given ARRAY has a fill pointer, or NIL otherwise."
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (= typecode target::subtag-vectorH)
      (logbitp $arh_fill_bit (the fixnum (%svref array target::vectorH.flags-cell)))
      (unless (array-typecode-p typecode)
        (report-bad-arg array 'array)))))


(defun fill-pointer (array)
  "Return the FILL-POINTER of the given VECTOR."
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (and (= typecode target::subtag-vectorH)
             (logbitp $arh_fill_bit (the fixnum (%svref array target::vectorH.flags-cell))))
      (%svref array target::vectorH.logsize-cell)
      (report-bad-arg array '(and array (satisfies array-has-fill-pointer-p))))))

(defun set-fill-pointer (array value)
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (and (= typecode target::subtag-vectorH)
             (logbitp $arh_fill_bit (the fixnum (%svref array target::vectorH.flags-cell))))
      (let* ((vlen (%svref array target::vectorH.physsize-cell)))
        (declare (fixnum vlen))
        (if (eq value t)
          (setq value vlen)
          (unless (and (fixnump value)
                     (>= (the fixnum value) 0)
                     (<= (the fixnum value) vlen))
            (%err-disp $XARROOB value array)))
        (setf (%svref array target::vectorH.logsize-cell) value))
      (%err-disp $XNOFILLPTR array))))

(eval-when (:compile-toplevel)
  (assert (eql target::vectorH.physsize-cell target::arrayH.physsize-cell)))

(defun array-total-size (array)
  "Return the total number of elements in the Array."
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (or (= typecode target::subtag-arrayH)
              (= typecode target::subtag-vectorH))
        (%svref array target::vectorH.physsize-cell)
      (if (or (= typecode target::subtag-simple-vector)
            (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                target::min-cl-ivector-subtag))
        (uvsize array)
        (report-bad-arg array 'array)))))

      

(defun array-dimension (array axis-number)
  "Return the length of dimension AXIS-NUMBER of ARRAY."
  (unless (typep axis-number 'fixnum) (report-bad-arg axis-number 'fixnum))
  (locally
    (declare (fixnum axis-number))
    (let* ((typecode (typecode array)))
      (declare (fixnum typecode))
      (if (array-typecode-p typecode)
        (if (= typecode target::subtag-arrayH)
          (let* ((rank (%svref array target::arrayH.rank-cell)))
            (declare (fixnum rank))
            (unless (and (>= axis-number 0)
                         (< axis-number rank))
              (%err-disp $XNDIMS array axis-number))
            (%svref array (the fixnum (+ target::arrayH.dim0-cell axis-number))))
          (if (neq axis-number 0)
            (%err-disp $XNDIMS array axis-number)
            (if (= typecode target::subtag-vectorH)
              (%svref array target::vectorH.physsize-cell)
              (uvsize array))))
        (report-bad-arg array 'array)))))

(defun array-dimensions (array)
  "Return a list whose elements are the dimensions of the array"
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (array-typecode-p typecode)
      (if (= typecode target::subtag-arrayH)
        (let* ((rank (%svref array target::arrayH.rank-cell))
               (dims ()))
          (declare (fixnum rank))        
          (do* ((i (1- rank) (1- i)))
               ((< i 0) dims)
            (declare (fixnum i))
            (push (%svref array (the fixnum (+ target::arrayH.dim0-cell i))) dims)))
        (list (if (= typecode target::subtag-vectorH)
                (%svref array target::vectorH.physsize-cell)
                (uvsize array))))
      (report-bad-arg array 'array))))


(defun array-rank (array)
  "Return the number of dimensions of ARRAY."
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (array-typecode-p typecode)
      (if (= typecode target::subtag-arrayH)
        (%svref array target::arrayH.rank-cell)
        1)
      (report-bad-arg array 'array))))

(defun vector-push (elt vector)
  "Attempt to set the element of ARRAY designated by its fill pointer
   to NEW-EL, and increment the fill pointer by one. If the fill pointer is
   too large, NIL is returned, otherwise the index of the pushed element is
   returned."
  (let* ((fill (fill-pointer vector))
         (len (%svref vector target::vectorH.physsize-cell)))
    (declare (fixnum fill len))
    (when (< fill len)
      (multiple-value-bind (data offset) (%array-header-data-and-offset vector)
        (declare (fixnum offset))
        (setf (%svref vector target::vectorH.logsize-cell) (the fixnum (1+ fill))
              (uvref data (the fixnum (+ fill offset))) elt)
        fill))))

;;; Implement some of the guts of REPLACE, where the source and target
;;; sequence have the same type (and we might be able to BLT things
;;; around more quickly because of that.)
;;; Both TARGET and SOURCE are (SIMPLE-ARRAY (*) *), and all of the
;;; indices are fixnums and in bounds.
;;; (Actually, we allow some internal uvector types as well as CL vectors.)
(defun %uvector-replace (target target-start source source-start n typecode)
  (declare (fixnum target-start n source-start n typecode)
           (optimize (speed 3) (safety 0)))
  (if (gvectorp target)
    (if (and (eq source target)
             (> target-start source-start))
      (do* ((i 0 (1+ i))
            (source-pos (1- (the fixnum (+ source-start n)))
                        (1- source-pos))
            (target-pos (1- (the fixnum (+ target-start n)))
                        (1- target-pos)))
           ((= i n))
        (declare (fixnum i source-pos target-pos))
        (setf (%svref target target-pos) (%svref source source-pos)))
      (dotimes (i n)
        (setf (%svref target target-start) (%svref source source-start))
        (incf target-start)
        (incf source-start)))
    (ecase typecode
      (#.target::subtag-bit-vector
       (if (and (eq source target)
                (> target-start source-start))
         (do* ((i 0 (1+ i))
               (source-pos (1- (the fixnum (+ source-start n)))
                           (1- source-pos))
               (target-pos (1- (the fixnum (+ target-start n)))
                           (1- target-pos)))
              ((= i n))
           (declare (fixnum i source-pos target-pos))
           (setf (sbit target target-pos) (sbit source source-pos)))
         (dotimes (i n)
           (setf (sbit target target-start) (sbit source source-start))
           (incf target-start)
           (incf source-start))))
      ;; All other cases can be handled with %COPY-IVECTOR-TO-IVECTOR,
      ;; which knows how to handle overlap
      ((#.target::subtag-s8-vector
        #.target::subtag-u8-vector)
       (%copy-ivector-to-ivector source
                                 source-start
                                 target
                                 target-start
                                 n))
      ((#.target::subtag-s16-vector
        #.target::subtag-u16-vector)
       (%copy-ivector-to-ivector source
                                 (the fixnum (* source-start 2))
                                 target
                                 (the fixnum (* target-start 2))
                                 (the fixnum (* n 2))))
      ((#.target::subtag-s32-vector
        #.target::subtag-u32-vector
        #.target::subtag-single-float-vector
        #.target::subtag-simple-base-string
        #.target::subtag-bignum
        #.target::subtag-single-float
        #.target::subtag-double-float
        #+32-bit-target #.target::subtag-fixnum-vector)
       (%copy-ivector-to-ivector source
                                 (the fixnum (* source-start 4))
                                 target
                                 (the fixnum (* target-start 4))
                                 (the fixnum (* n 4))))
      ((#.target::subtag-double-float-vector
        #+64-bit-target #.target::subtag-s64-vector
        #+64-bit-target #.target::subtag-u64-vector
        #+64-bit-target #.target::subtag-fixnum-vector
        #.target::subtag-complex-single-float-vector)
       (%copy-ivector-to-ivector source
                                 (the fixnum
                                   (+ (the fixnum (- target::misc-dfloat-offset
                                                     target::misc-data-offset))
                                      (the fixnum (* source-start 8))))
                                 target
                                 (the fixnum
                                   (+ (the fixnum (- target::misc-dfloat-offset
                                                     target::misc-data-offset))
                                      (the fixnum (* target-start 8))))
                                 (the fixnum (* n 8))))
      (#.target::complex-double-float-vector
        (%copy-ivector-to-ivector source
                                 (the fixnum
                                   (+ (the fixnum (- target::misc-dfloat-offset
                                                     target::misc-data-offset))
                                      (the fixnum (* source-start 16))))
                                 target
                                 (the fixnum
                                   (+ (the fixnum (- target::misc-dfloat-offset
                                                     target::misc-data-offset))
                                      (the fixnum (* target-start 16))))
                                 (the fixnum (* n 16)))))
  target)

(defun vector-push-extend (elt vector &optional (extension nil extp))
  "Attempt to set the element of VECTOR designated by its fill pointer
to ELT, and increment the fill pointer by one. If the fill pointer is
too large, VECTOR is extended using adjust-array.  EXTENSION is the
minimum number of elements to add if it must be extended."
  (when extp
    (unless (and (typep extension 'fixnum)
                 (> (the fixnum extension) 0))
      (setq extension (require-type extension 'unsigned-byte))))
  (let* ((fill (fill-pointer vector))
         (len (%svref vector target::vectorH.physsize-cell)))
    (declare (fixnum fill len))
    (multiple-value-bind (data offset) (%array-header-data-and-offset vector)
      (declare (fixnum offset))
      (if (= fill len)
        (let* ((flags (%svref vector target::arrayH.flags-cell)))
          (declare (fixnum flags))
          (unless (logbitp $arh_adjp_bit flags)
            (%err-disp $XMALADJUST vector))
          (let* ((new-size (max
                            (+ len (the fixnum (or extension
                                                  len)))
                            4))
                 (typecode (typecode data))
                 (new-vector (%alloc-misc new-size typecode)))
            (%uvector-replace new-vector 0 data offset fill typecode)
            (setf (%svref vector target::vectorH.data-vector-cell) new-vector
                  (%svref vector target::vectorH.displacement-cell) 0
                  (%svref vector target::vectorH.physsize-cell) new-size
                  (%svref vector target::vectorH.flags-cell) (bitclr $arh_exp_disp_bit flags)
                  (uvref new-vector fill) elt)))
        (setf (uvref data (the fixnum (+ offset fill))) elt))
      (setf (%svref vector target::vectorH.logsize-cell) (the fixnum (1+ fill))))
    fill))

;;; Could avoid potential memoization somehow
(defun vector (&lexpr vals)
  "Construct a SIMPLE-VECTOR from the given objects."
  (let* ((n (%lexpr-count vals))
         (v (allocate-typed-vector :simple-vector n)))
    (declare (fixnum n))
    (dotimes (i n v) (setf (%svref v i) (%lexpr-ref vals n i)))))

;;; CALL-ARGUMENTS-LIMIT.
(defun list-to-vector (elts)
  (let* ((n (length elts)))
    (declare (fixnum n))
    (if (< n (floor #x8000 target::node-size))
      (apply #'vector elts)
      (make-array n :initial-contents elts))))

             
    
(defun %gvector (subtag &lexpr vals)
  (let* ((n (%lexpr-count vals))
         (v (%alloc-misc n subtag)))
    (declare (fixnum n))
    (dotimes (i n v) (setf (%svref v i) (%lexpr-ref vals n i)))))

(defun %aref1 (v i)
  (let* ((typecode (typecode v)))
    (declare (type (unsigned-byte 8)  typecode))
    (if (or (= typecode target::subtag-simple-vector)
            (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                target::min-cl-ivector-subtag))
      (uvref v i)
      (if (= typecode target::subtag-vectorH)
        (multiple-value-bind (data offset)
                             (%array-header-data-and-offset v)
          (unless (typep i 'fixnum)
            (report-bad-arg i 'fixnum))
          (unless (and (typep i 'fixnum)
                       (>= (the fixnum i) 0)
                       (< (the fixnum i) (the fixnum (%svref v target::vectorH.physsize-cell))))
            (if (not (typep i 'fixnum))
              (report-bad-arg i 'fixnum)
              (%err-disp $XARROOB i v)))
          (uvref data (+ offset i)))
        (if (= typecode target::subtag-arrayH)
          (%err-disp $XNDIMS v 1)
          (report-bad-arg v 'array))))))

(defun %aset1 (v i new)
  (let* ((typecode (typecode v)))
    (declare (type (unsigned-byte 8) typecode))
    (if (or (= typecode target::subtag-simple-vector)
            (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                target::min-cl-ivector-subtag))
      (setf (uvref v i) new)
      (if (= typecode target::subtag-vectorH)
        (multiple-value-bind (data offset)
                             (%array-header-data-and-offset v)
          (unless (and (typep i 'fixnum)
                       (>= (the fixnum i) 0)
                       (< (the fixnum i) (the fixnum (%svref v target::vectorH.physsize-cell))))
            (if (not (typep i 'fixnum))
              (report-bad-arg i 'fixnum)
              (%err-disp $XARROOB i v)))
          (setf (uvref data (+ offset i)) new))
        (if (= typecode target::subtag-arrayH)
          (%err-disp $XNDIMS v 1)
          (report-bad-arg v 'array))))))

;;; Validate the N indices in the lexpr L against the
;;; array-dimensions of L.  If anything's out-of-bounds,
;;; error out (unless NO-ERROR is true, in which case
;;; return NIL.)
;;; If everything's OK, return the "row-major-index" of the array.
;;; We know that A's an array-header of rank N.

(defun %array-index (a l n &optional no-error)
  (declare (fixnum n))
  (let* ((count (%lexpr-count l)))
    (declare (fixnum count))
    (do* ((axis (1- n) (1- axis))
          (chunk-size 1)
          (result 0))
         ((< axis 0) result)
      (declare (fixnum result axis chunk-size))
      (let* ((index (%lexpr-ref l count axis))
             (dim (%svref a (the fixnum (+ target::arrayH.dim0-cell axis)))))
        (declare (fixnum dim))
        (unless (and (typep index 'fixnum)
                     (>= (the fixnum index) 0)
                     (< (the fixnum index) dim))
          (if no-error
            (return-from %array-index nil)
            (error "Index value ~d is out of bounds for axis ~d of ~s."
                   index axis a)))
        (incf result (the fixnum (* chunk-size (the fixnum index))))
        (setq chunk-size (* chunk-size dim))))))

(defun aref (a &lexpr subs)
  "Return the element of the ARRAY specified by the SUBSCRIPTS."
  (let* ((n (%lexpr-count subs)))
    (declare (fixnum n))
    (if (= n 1)
      (%aref1 a (%lexpr-ref subs n 0))
      (if (= n 2)
        (%aref2 a (%lexpr-ref subs n 0) (%lexpr-ref subs n 1))
        (if (= n 3)
          (%aref3 a (%lexpr-ref subs n 0) (%lexpr-ref subs n 1) (%lexpr-ref subs n 2))
          (let* ((typecode (typecode a)))
            (declare (fixnum typecode))
            (if (or (>= (the (unsigned-byte 8) (gvector-typecode-p typecode))
                        target::subtag-vectorH)
                    (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                        target::min-cl-ivector-subtag))
              (%err-disp $XNDIMS a n)
              (if (/= typecode target::subtag-arrayH)
                (report-bad-arg a 'array)
                ;;  This typecode is Just Right ...
                (progn
                  (unless (= (the fixnum (%svref a target::arrayH.rank-cell)) n)
                    (%err-disp $XNDIMS a n))
                  (let* ((rmi (%array-index a subs n)))
                    (declare (fixnum rmi))
                    (multiple-value-bind (data offset) (%array-header-data-and-offset a)
                      (declare (fixnum offset))
                      (uvref data (the fixnum (+ offset rmi))))))))))))))





(defun aset (a &lexpr subs&val)
  (let* ((count (%lexpr-count subs&val))
         (nsubs (1- count)))
    (declare (fixnum nsubs count))
    (if (eql count 0)
      (%err-disp $xneinps)
      (let* ((val (%lexpr-ref subs&val count nsubs)))
        (if (= nsubs 1)
          (%aset1 a (%lexpr-ref subs&val count 0) val)
          (if (= nsubs 2)
            (%aset2 a (%lexpr-ref subs&val count 0) (%lexpr-ref subs&val count 1) val)
            (if (= nsubs 3)
              (%aset3 a (%lexpr-ref subs&val count 0) (%lexpr-ref subs&val count 1) (%lexpr-ref subs&val count 2) val)
              (let* ((typecode (typecode a)))
                (declare (fixnum typecode))
                (if (or (>= (the (unsigned-byte 8) (gvector-typecode-p typecode))
                        target::subtag-vectorH)
                    (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                        target::min-cl-ivector-subtag))
                  (%err-disp $XNDIMS a nsubs)
                  (if (/= typecode target::subtag-arrayH)
                    (report-bad-arg a 'array)
                    ;;  This typecode is Just Right ...
                    (progn
                      (unless (= (the fixnum (%svref a target::arrayH.rank-cell)) nsubs)
                        (%err-disp $XNDIMS a nsubs))
                      (let* ((rmi (%array-index a subs&val nsubs)))
                        (declare (fixnum rmi))
                        (multiple-value-bind (data offset) (%array-header-data-and-offset a)
                          (setf (uvref data (the fixnum (+ offset rmi))) val))))))))))))))



(defun schar (s i)
  "SCHAR returns the character object at an indexed position in a string
   just as CHAR does, except the string must be a simple-string."
  (let* ((typecode (typecode s)))
    (declare (fixnum typecode))
    (if (= typecode target::subtag-simple-base-string)
      (aref (the simple-string s) i)
      (report-bad-arg s 'simple-string))))


(defun %scharcode (s i)
  (let* ((typecode (typecode s)))
    (declare (fixnum typecode))
    (if (= typecode target::subtag-simple-base-string)
      (locally
        (declare (optimize (speed 3) (safety 0)))
        (aref (the (simple-array (unsigned-byte 32) (*)) s) i))
        (report-bad-arg s 'simple-string))))


(defun set-schar (s i v)
  (let* ((typecode (typecode s)))
    (declare (fixnum typecode))
    (if (= typecode target::subtag-simple-base-string)
      (setf (aref (the simple-string s) i) v)
        (report-bad-arg s 'simple-string))))

 
(defun %set-scharcode (s i v)
  (let* ((typecode (typecode s)))
    (declare (fixnum typecode))
    (if (= typecode target::subtag-simple-base-string)
      (locally
        (declare (optimize (speed 3) (safety 0)))
        (setf (aref (the simple-string s) i) v))
        (report-bad-arg s 'simple-string))))
  

; Strings are simple-strings, start & end values are sane.
(defun %simple-string= (str1 str2 start1 start2 end1 end2)
  (declare (fixnum start1 start2 end1 end2))
  (when (= (the fixnum (- end1 start1))
           (the fixnum (- end2 start2)))
    (locally (declare (type simple-base-string str1 str2))
            (do* ((i1 start1 (1+ i1))
                  (i2 start2 (1+ i2)))
                 ((= i1 end1) t)
              (declare (fixnum i1 i2))
              (unless (eq (schar str1 i1) (schar str2 i2))
                (return))))))

(defun copy-uvector (src)
  (%extend-vector 0 src (uvsize src)))

#+(or ppc32-target arm-target)
(defun subtag-bytes (subtag element-count)
  (declare (fixnum subtag element-count))
  (unless (= #.target::fulltag-immheader (logand subtag #.target::fulltagmask))
    (error "Not an ivector subtag: ~s" subtag))
  (let* ((element-bit-shift
          (if (<= subtag target::max-32-bit-ivector-subtag)
            5
            (if (<= subtag target::max-8-bit-ivector-subtag)
              3
              (if (<= subtag target::max-16-bit-ivector-subtag)
                4
                (if (= subtag target::subtag-double-float-vector)
                  6
                  0)))))
         (total-bits (ash element-count element-bit-shift)))
    (ash (+ 7 total-bits) -3)))

#+ppc64-target
(defun subtag-bytes (subtag element-count)
  (declare (fixnum subtag element-count))
  (unless (= ppc64::lowtag-immheader (logand subtag ppc64::lowtagmask))
    (error "Not an ivector subtag: ~s" subtag))
  (let* ((ivector-class (logand subtag ppc64::fulltagmask))
         (element-bit-shift
          (if (= ivector-class ppc64::ivector-class-32-bit)
            5
            (if (= ivector-class ppc64::ivector-class-8-bit)
              3
              (if (= ivector-class ppc64::ivector-class-64-bit)
                6
                (if (= subtag ppc64::subtag-bit-vector)
                  0
                  4)))))
         (total-bits (ash element-count element-bit-shift)))
    (declare (fixnum ivector-class element-bit-shift total-bits))
    (ash (the fixnum (+ 7 total-bits)) -3)))

#+x8632-target
(defun subtag-bytes (subtag element-count)
  (declare (fixnum subtag element-count))
  (unless (= #.x8632::fulltag-immheader (logand subtag #.x8632::fulltagmask))
    (error "Not an ivector subtag: ~s" subtag))
  (let* ((element-bit-shift
          (if (<= subtag x8632::max-32-bit-ivector-subtag)
            5
            (if (<= subtag x8632::max-8-bit-ivector-subtag)
              3
              (if (<= subtag x8632::max-16-bit-ivector-subtag)
                4
                (if (= subtag x8632::subtag-double-float-vector)
                  6
                  0)))))
         (total-bits (ash element-count element-bit-shift)))
    (ash (+ 7 total-bits) -3)))

#+x8664-target
(defun subtag-bytes (subtag element-count)
  (declare (fixnum subtag element-count))
  (unless (logbitp (the (mod 16) (logand subtag x8664::fulltagmask))
                   (logior (ash 1 x8664::fulltag-immheader-0)
                           (ash 1 x8664::fulltag-immheader-1)
                           (ash 1 x8664::fulltag-immheader-2)))
    (error "Not an ivector subtag: ~s" subtag))
  (let* ((ivector-class (logand subtag x8664::fulltagmask))
         (element-bit-shift
          (if (= ivector-class x8664::ivector-class-32-bit)
            5
            (if (= ivector-class x8664::ivector-class-64-bit)
                6
                (if (= subtag x8664::subtag-bit-vector)
                  0
                  (if (>= subtag x8664::min-8-bit-ivector-subtag)
                    3
                    4)))))
         (total-bits (ash element-count element-bit-shift)))
    (declare (fixnum ivector-class element-bit-shift total-bits))
    (ash (the fixnum (+ 7 total-bits)) -3)))

(defun element-type-subtype (type)
  "Convert element type specifier to internal array subtype code"
  (ctype-subtype (specifier-type type)))

(defun ctype-subtype (ctype)
  (typecase ctype
    (class-ctype
     (if (or (eq (class-ctype-class ctype) *character-class*)
	     (eq (class-ctype-class ctype) *base-char-class*)
             (eq (class-ctype-class ctype) *standard-char-class*))
       target::subtag-simple-base-string
       target::subtag-simple-vector))
    (numeric-ctype
     (if (eq (numeric-ctype-complexp ctype) :complex)
       (case (numeric-ctype-format ctype)
         (single-float target::subtag-complex-single-float-vector)
         (double-float target::subtag-complex-double-float-vector)
         (t target::subtag-simple-vector))
       (case (numeric-ctype-class ctype)
	 (integer
	  (let* ((low (numeric-ctype-low ctype))
		 (high (numeric-ctype-high ctype)))
	    (cond ((or (null low) (null high)) target::subtag-simple-vector)
		  ((and (>= low 0) (<= high 1)) target::subtag-bit-vector)
		  ((and (>= low 0) (<= high 255))
                   target::subtag-u8-vector)
		  ((and (>= low 0) (<= high 65535))
                   target::subtag-u16-vector)
		  ((and (>= low 0) (<= high #xffffffff))
                   target::subtag-u32-vector)
                  #+64-bit-target
                  ((and (>= low 0) (<= high (1- (ash 1 64))))
                   target::subtag-u64-vector)
		  ((and (>= low -128) (<= high 127)) target::subtag-s8-vector)
		  ((and (>= low -32768) (<= high 32767)) target::subtag-s16-vector)
                  #+32-bit-target
                  ((and (>= low target::target-most-negative-fixnum)
                        (<= high target::target-most-positive-fixnum))
                   target::subtag-fixnum-vector)
		  ((and (>= low (ash -1 31)) (<= high (1- (ash 1 31))))
		   target::subtag-s32-vector)
                  #+64-bit-target
                  ((and (>= low target::target-most-negative-fixnum)
                        (<= high target::target-most-positive-fixnum))
                   target::subtag-fixnum-vector)                  
                  #+64-bit-target
                  ((and (>= low (ash -1 63)) (<= high (1- (ash 1 63))))
                   target::subtag-s64-vector)
		  (t target::subtag-simple-vector))))
	 (float
	  (case (numeric-ctype-format ctype)
	    ((double-float long-float) target::subtag-double-float-vector)
	    ((single-float short-float) target::subtag-single-float-vector)
	    (t target::subtag-simple-vector)))
	 (t target::subtag-simple-vector))))
    (named-ctype ; *, T, etc.
     target::subtag-simple-vector)
    (t
     (harder-ctype-subtype ctype))))

(defun %set-simple-array-p (array)
  (setf (%svref array  target::arrayh.flags-cell)
        (bitset  $arh_simple_bit (%svref array target::arrayh.flags-cell))))

(defun  %array-header-simple-p (array)
  (logbitp $arh_simple_bit (%svref array target::arrayh.flags-cell)))

(defun %misc-ref (v i)
  (%misc-ref v i))

(defun %misc-set (v i new)
  (%misc-set v i new))

#-ppc-target
(defun %extend-vector (start oldv newsize)
  (declare (fixnum start))
  (let* ((typecode (typecode oldv))
         (new (%alloc-misc newsize typecode))
         (oldsize (uvsize oldv)))
    (declare (fixnum oldsize) (type (unsigned-byte 8) typecode))
    (%uvector-replace  new start oldv 0 oldsize typecode)))



; end of l0-array.lisp
