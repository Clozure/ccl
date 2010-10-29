;;-*-Mode: LISP; Package: CCL -*-
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

;;
;; utility functions
;;
;;  these probably want to be in-line

(defun make-sequence-like (sequence length)
  (seq-dispatch 
   sequence
   (make-list length)
   (make-array length :element-type (array-element-type sequence))))

(defun adjust-test-args (item test test-not)
  ;; after running this "test" is the real test, a null test means "eq"
  ;; and "test-not" is used as a flag
  (when test-not
    (if test 
      (error "Both ~s and ~s keywords supplied" :test :test-not)
      (setq test test-not)))
  (if test
    (if (or (eq test #'eq)
            (eq test 'eq)
            (and (or (eq test #'equal) (eq test 'equal))
                 (or (fixnump item) (symbolp item))))
      (setq test nil)
      (if (eq test #'funcall)
        (setq test 'funcall)))
    (if (or (macptrp item) (and (not (fixnump item)) (numberp item)))
      (setq test #'eql)))
  (values test test-not))

(defun adjust-key (key)
  (and (neq key 'identity) 
       (neq key #'identity)
       key))

(defun matchp2 (item elt test test-not key)
  (if key
    (setq elt (funcall key elt)))
  (let ((res (if test
               (if (eq test 'funcall)
                 (funcall item elt)
                 (funcall test item elt))
               (eq item elt))))
    (if test-not
      (not res)
      res)))

;;; CTYPE is a recognizable subtype of VECTOR, which means that it's either
;;; a) an ARRAY-CTYPE
;;; b) a UNION-CTYPE whose leaves are ARRAY-CTYPE
;;; c) the NIL type, which is trivially a subtype of VECTOR but isn't really
;;;    worth considering here
;;; d) a MEMBER-CTYPE whose members are all vectors and which therefore have
;;;    corresponding ARRAY-CTYPEs.
;;; Try to find the interesection of all ARRAY-CTYPEs referenced in CTYPE and
;;;  return it.
;;; Note that this intersection may be the null type.
(defun simplify-vector-ctype (ctype)
  (typecase ctype
    (array-ctype
     (make-array-ctype :complexp nil
                       :element-type (array-ctype-element-type ctype)
                       :specialized-element-type (array-ctype-specialized-element-type ctype)
                       :dimensions '(*)))
                                      
    (named-ctype ctype)
    (member-ctype
     (apply #'type-intersection (mapcar #'(lambda (x)
                                            (simplify-vector-ctype
                                             (ctype-of x)))
                                        (member-ctype-members ctype))))
    (union-ctype
     (apply #'type-intersection (mapcar #'simplify-vector-ctype (union-ctype-types ctype))))))
    
(defun make-sequence (type length &key (initial-element nil initial-element-p))
  "Return a sequence of the given TYPE and LENGTH, with elements initialized
  to INITIAL-ELEMENT."
  (setq length (require-type length 'fixnum))
  (let* ((ctype (specifier-type type)))
    (declare (fixnum length))
    (if (< length 0) (report-bad-arg length '(and fixnum unsigned-byte)))
    (let ((tlength (array-ctype-length ctype)))
      (if (and tlength (neq tlength length))
        (error 'invalid-subtype-error
               :datum type
               :expected-type `(vector ,(type-specifier (array-ctype-element-type ctype)) ,length))))
    (cond 
          ((csubtypep ctype (specifier-type 'base-string))
           (if initial-element-p
             (make-string length 
                          :element-type 'base-char
                          :initial-element initial-element)
             (make-string length
                          :element-type 'base-char)))
          ((csubtypep ctype (specifier-type 'vector))
           (let* ((atype (simplify-vector-ctype ctype)))
             (unless (typep atype 'array-ctype)
               (error "Can't determine vector element-type of ~s" (type-specifier ctype)))
             (let* ((element-type (type-specifier (array-ctype-element-type atype))))
               (if (eq element-type '*) (setq element-type t))
               (if initial-element-p
                 (make-array (the fixnum length)
                             :element-type element-type
                             :initial-element initial-element)
                 (make-array (the fixnum length)
                             :element-type element-type)))))
          ((csubtypep ctype (specifier-type 'null))
           (unless (zerop length)
             (error 'invalid-subtype-error :datum type :expected-type 'cons)))
          ((csubtypep ctype (specifier-type 'cons))
           (if (zerop length)
             (error 'invalid-subtype-error :datum type :expected-type 'null)
             (make-list length :initial-element initial-element)))
          ((csubtypep ctype (specifier-type 'list))
           (make-list length :initial-element initial-element))
          (t (error 'invalid-subtype-error :datum  type
                    :expected-type 'sequence)))))



;;; Subseq:

;;; SRC is a (SIMPLE-ARRAY * (*)), TYPECODE is its ... typecode,
;;; START and END are fixnums and sanity-checked.
(defun simple-1d-array-subseq (src typecode start end)
  (declare (fixnum start end typecode))
  (let* ((n (- end start))
	 (dest (%alloc-misc n typecode)))
    (declare (fixnum n))
    (if (= typecode target::subtag-simple-vector)
      (%copy-gvector-to-gvector src start dest 0 n)
      (ecase typecode
	((#.target::subtag-s8-vector
	  #.target::subtag-u8-vector)
	 (%copy-ivector-to-ivector src start dest 0 n))
	((#.target::subtag-s16-vector
	  #.target::subtag-u16-vector)
	 (%copy-ivector-to-ivector src
				   (the fixnum (+ start start))
				   dest
				   0
				   (the fixnum (+ n n))))
	((#.target::subtag-s32-vector
	  #.target::subtag-u32-vector
	  #.target::subtag-single-float-vector
          #+32-bit-target #.target::subtag-fixnum-vector
          #.target::subtag-simple-base-string)
	 (%copy-ivector-to-ivector src
				   (the fixnum (ash start 2))
				   dest
				   0
				   (the fixnum (ash n 2))))
	;; DOUBLE-FLOAT vectors have extra alignment padding on ppc32/x8632.
	#+32-bit-target
	(#.target::subtag-double-float-vector
	 (%copy-ivector-to-ivector src
				   (the fixnum (+ (the fixnum (ash start 3))
						  (- target::misc-dfloat-offset
						     target::misc-data-offset)))
				   dest
				   (- target::misc-dfloat-offset
						     target::misc-data-offset)
				   (the fixnum (ash n 3))))
	#+64-bit-target
	((#.target::subtag-double-float-vector
	  #.target::subtag-s64-vector
	  #.target::subtag-u64-vector
          #.target::subtag-fixnum-vector)
	 (%copy-ivector-to-ivector src
				   (the fixnum (ash start 3))
				   dest
				   0
				   (the fixnum (ash n 3))))
	(#.target::subtag-bit-vector
	 ;; We can probably do a byte at a time if (not (logtest start 7))
	 (if (not (logtest start 7))
	   (%copy-ivector-to-ivector src
				     (the fixnum (ash (the fixnum (+ start 7))
						      -3))
				     dest
				     0
				     (the fixnum (ash (the fixnum (+ n 7))
						      -3)))
	   ;; Harder to optimize this case.
	   (locally  (declare (simple-bit-vector src dest)
			      (optimize (speed 3) (safety 0)))
	     (do* ((i start (1+ i))
		   (j 0 (1+ j)))
		  ((= i end) dest)
	       (declare (fixnum i j))
	       (setf (sbit dest j) (sbit src i))))))))))


(defun nthcdr-error (index list &aux (copy list))
 "If index > length, error"
 (dotimes (i index copy)
   (declare (fixnum i))
   (if copy
     (setq copy (cdr copy))
     (%err-disp $XACCESSNTH index list))))

; slisp didn't error if end > length, or if start > end.
(defun list-subseq* (sequence start end)
  (declare (fixnum start end))
  (if (= start end)
    nil
    (let* ((groveled (nthcdr-error start sequence))
           (result (list (car groveled))))
      (when groveled
        (do ((list (cdr groveled) (cdr list))
             (splice result (cdr (rplacd splice (list (car list)))))
             (index (1+ start) (1+ index)))
             ((= index end) result)
          (declare (fixnum index))
           ())))))

; This ensures that start & end will be non-negative FIXNUMS ...
; This implies that the address space is < 2^31 bytes, i.e., no list
; can have a length > most-positive fixnum.  Let them report it as a
; bug ...

(defun subseq (sequence start &optional end)
  "Return a copy of a subsequence of SEQUENCE starting with element number
   START and continuing to the end of SEQUENCE or the optional END."
  (setq end (check-sequence-bounds sequence start end))
  (locally 
      (declare (fixnum start end))
      (seq-dispatch 
       sequence
       (list-subseq* sequence start end)
       (let* ((typecode (typecode sequence)))
	 (declare (fixnum typecode))
	 (when (= typecode target::subtag-vectorH)
	   (multiple-value-bind (data offset)
	       (array-data-and-offset sequence)
	     (declare (fixnum offset))
	     (incf start offset)
	     (incf end offset)
	     (setq sequence data typecode (typecode data))))
	 (simple-1d-array-subseq sequence typecode start end)))))
	 

;;; Copy-seq:

(defun copy-seq (sequence)
  "Return a copy of SEQUENCE which is EQUAL to SEQUENCE but not EQ."
  (seq-dispatch 
   sequence
   (copy-list sequence)
   (let* ((length (length sequence))
          (subtype (element-type-subtype (array-element-type sequence)))
          (result  (%alloc-misc length subtype))
          )
     (multiple-value-bind (src offset) (array-data-and-offset sequence)
       (declare (fixnum offset))                          
       (dotimes (i length result)
         (declare (fixnum i))
         (setf (uvref result i) (uvref src offset))
         (incf offset))))))



;;; Fill:

(defun fill (sequence item &key (start 0) end)
  "Replace the specified elements of SEQUENCE with ITEM.
   !$ could be sped up by calling iv-fill, sv-fill to avoid aref overhead."
  (setq end (check-sequence-bounds sequence start end))
  (seq-dispatch 
   sequence
   (do* ((current (nthcdr start sequence) (cdr (the list current)))
         (index start (1+ index)))
        ((or (atom current) (= index end)) sequence)
     (rplaca (the cons current) item))
   (if (and (typep sequence 'ivector)
            (eql start 0)
            (eql end (uvsize sequence)))
     (%init-misc item sequence)
     (do ((index start (1+ index)))
         ((= index end) sequence)
       (aset sequence index item)))))

;;; Replace:

(defun replace (target-sequence source-sequence &key
                                ((:start1 target-start) 0)
                                ((:end1 target-end))
                                ((:start2 source-start) 0)
                                ((:end2 source-end)))
  "The target sequence is destructively modified by copying successive
   elements into it from the source sequence."
  (setq target-end (check-sequence-bounds target-sequence target-start
                                          target-end))
  (setq source-end (check-sequence-bounds source-sequence source-start
                                          source-end))
  (locally (declare (fixnum target-start target-end source-start source-end))
    (seq-dispatch 
     target-sequence
     (seq-dispatch 
      source-sequence
      (if (and (eq target-sequence source-sequence) 
               (> target-start source-start))
        (let ((new-elts (subseq source-sequence source-start
                                (+ source-start
                                   (min (- target-end target-start)
                                        (- source-end source-start))))))
          (do ((n new-elts (cdr n))
               (o (nthcdr target-start target-sequence) (cdr o)))
              ((null n) target-sequence)
            (rplaca o (car n))))
        (do ((target-index target-start (1+ target-index))
             (source-index source-start (1+ source-index))
             (target-sequence-ref (nthcdr target-start target-sequence)
                                  (cdr target-sequence-ref))
             (source-sequence-ref (nthcdr source-start source-sequence)
                                  (cdr source-sequence-ref)))
            ((or (= target-index target-end) (= source-index source-end)
                 (null target-sequence-ref) (null source-sequence-ref))
             target-sequence)
          (declare (fixnum target-index source-index))
          (rplaca target-sequence-ref (car source-sequence-ref))))
      (do ((target-index target-start (1+ target-index))
           (source-index source-start (1+ source-index))
           (target-sequence-ref (nthcdr target-start target-sequence)
                                (cdr target-sequence-ref)))
          ((or (= target-index target-end) (= source-index source-end)
               (null target-sequence-ref))
           target-sequence)
        (declare (fixnum target-index source-index))
        (rplaca target-sequence-ref (aref source-sequence source-index))))
     (seq-dispatch 
      source-sequence
      (do ((target-index target-start (1+ target-index))
           (source-index source-start (1+ source-index))
           (source-sequence (nthcdr source-start source-sequence)
                            (cdr source-sequence)))
          ((or (= target-index target-end) (= source-index source-end)
               (null source-sequence))
           target-sequence)
        (declare (fixnum target-index source-index))
        (aset target-sequence target-index (car source-sequence)))
      ;; If we are copying around in the same vector, be careful not
      ;; to copy the same elements over repeatedly.  We do this by
      ;; copying backwards.
      (if (and (eq target-sequence source-sequence) 
               (> target-start source-start))
        (let ((nelts (min (- target-end target-start) 
                          (- source-end source-start))))
          (do ((target-index (+ target-start nelts -1) (1- target-index))
               (source-index (+ source-start nelts -1) (1- source-index)))
              ((= target-index (1- target-start)) target-sequence)
            (aset target-sequence target-index
                  (aref source-sequence source-index))))
        (do ((target-index target-start (1+ target-index))
             (source-index source-start (1+ source-index)))
            ((or (= target-index target-end) (= source-index source-end))
             target-sequence)
          (declare (fixnum target-index source-index))
          (aset target-sequence target-index
                (aref source-sequence source-index))))))))

;;; Concatenate:


(defun concatenate (output-type-spec &rest sequences)
  "Return a new sequence of all the argument sequences concatenated together
  which shares no structure with the original argument sequences of the
  specified OUTPUT-TYPE-SPEC."
  (declare (dynamic-extent sequences))
  (if (memq output-type-spec '(string simple-string))
    (setq output-type-spec 'base-string)
    (unless (memq output-type-spec '(string simple-string base-string list vector
                                     simple-base-string
                                     bit-vector simple-bit-vector))
      (setq output-type-spec (type-expand output-type-spec))))
  (case (if (atom output-type-spec) output-type-spec (car output-type-spec))
    (list (apply #'concat-to-list* sequences))
    ((simple-vector simple-string simple-base-string base-string vector string array
                    bit-vector simple-bit-vector)
     (apply #'concat-to-simple* output-type-spec sequences))
    (t
     (if (subtypep output-type-spec 'vector)
       (apply #'concat-to-simple* output-type-spec sequences)
       (if (subtypep output-type-spec 'list)
         (apply #'concat-to-list* sequences)
         (error "~S: invalid output type specification." output-type-spec))))))

;;; Internal Frobs:

(defun concat-to-list* (&rest sequences)
  (declare (dynamic-extent sequences))
  (let* ((result (list nil))
         (splice result))
    (dolist (sequence sequences (%cdr result))
      (seq-dispatch
       sequence
       (dolist (item sequence)
         (setq splice (%cdr (%rplacd splice (list item)))))
       (dotimes (i (length sequence))
         (setq splice (%cdr (%rplacd splice (list (aref sequence i))))))))))
             

(defun concat-to-simple* (output-type-spec &rest arg-sequences)
  (declare (dynamic-extent arg-sequences))
  (do ((seqs arg-sequences (cdr seqs))
        (total-length 0)
        ;(lengths ())
        )
      ((null seqs)
       (do ((sequences arg-sequences (cdr sequences))
            ;(lengths lengths (cdr lengths))
            (index 0)
            (result (make-sequence output-type-spec total-length)))
           ((= index total-length) result)
         (let ((sequence (car sequences)))
           (seq-dispatch
            sequence
            (do ((sequence sequence (cdr sequence)))
                ((atom sequence))
              (aset result index (car sequence))
              (setq index (1+ index)))
            (let ((len (length sequence)))
              (do ((jndex 0 (1+ jndex)))
                  ((= jndex len))
                (aset result index (aref sequence jndex))
                (setq index (1+ index))))))))
     (let ((length (length (car seqs))))
       ;(setq lengths (nconc lengths (list length))) ; if itsa list, we dont care about its length, if itsan array, length twice is cheap
       (setq total-length (+ total-length length)))))

(defun concat-to-string (&rest sequences)
  (declare (dynamic-extent sequences))
  (let* ((size 0))
    (declare (fixnum size))
    (dolist (seq sequences)
      (setq size (+ size (the fixnum (length seq)))))
    (let* ((result (make-string size))
           (out 0))
      (declare (simple-string result) (fixnum out))
      (dolist (seq sequences result)
        (etypecase seq
          (simple-string
           (let* ((n (length seq)))
             (declare (fixnum n))
             (%copy-ivector-to-ivector seq
                                       0
                                       result
                                       (the fixnum (ash out 2))
                                       (the fixnum (ash n 2)))
             (incf out n)))
          (string
           (let* ((n (length seq)))
             (declare (fixnum n))
             (multiple-value-bind (data offset) (array-data-and-offset seq)
               (declare (fixnum offset))
               (%copy-ivector-to-ivector data
                                         (the fixnum (ash offset 2))
                                         result
                                         (the fixnum (ash out 2))
                                         (the fixnum (ash n 2)))
               (incf out n))))
          (vector
           (dotimes (i (length seq))
             (setf (schar result out) (aref seq i))
             (incf out)))
          (list
           (dolist (elt seq)
             (setf (schar result out) elt)
             (incf out))))))))

;This one doesn't choke on circular lists, doesn't cons as much, and is
;about 1/8K smaller to boot.
(defun map (type function sequence &rest more-sequences)
  (declare (dynamic-extent more-sequences))
  (let* ((sequences (cons sequence more-sequences))
         (arglist (make-list (length sequences)))
         (index 0)
         args seq p (ans ()))
    (declare (dynamic-extent sequences arglist))
    (unless (or (null type)
                (eq type 'list)
                (memq (if (consp type) (%car type) type)
                      '(simple-vector simple-string vector string array
                        simple-array bit-vector simple-bit-vector))
                (subtypep type 'sequence))
      (report-bad-arg type 'sequence))
    (loop
      (setq p sequences args arglist)
      (while p
        (cond ((null (setq seq (%car p))) (return))
              ((consp seq)
               (%rplaca p (%cdr seq))
               (%rplaca args (%car seq)))
              ((eq index (length seq)) (return))
              (t (%rplaca args (elt seq index))))
        (setq args (%cdr args) p (%cdr p)))
      (setq p (apply function arglist))
      (if type (push p ans))
      (setq index (%i+ index 1)))
    (when type
      (setq ans (nreverse ans))
      (if (eq type 'list) ans (coerce ans type)))))

;;;;;;;;;;;;;;;;;
;;
;; some, every, notevery, notany
;;
;; these all call SOME-XX-MULTI or SOME-XX-ONE
;; SOME-XX-MULTI should probably be coded in lap
;;
;; these should be transformed at compile time
;;
;; we may want to consider open-coding when
;; the predicate is a lambda
;; 

(eval-when (:execute :compile-toplevel)
  (defmacro negating-quantifier-p (quantifier-constant)
    `(%i> ,quantifier-constant $notany))
  )

; Vector is guaranteed to be simple; new-size is guaranteed <= (length vector).
; Return vector with its size adjusted and extra doublewords zeroed out.
; Should only be called on freshly consed vectors...

    
    
(defun some (predicate one-seq &rest sequences)
  "Apply PREDICATE to the 0-indexed elements of the sequences, then 
   possibly to those with index 1, and so on. Return the first 
   non-NIL value encountered, or NIL if the end of any sequence is reached."
  (declare (dynamic-extent sequences))
  (if sequences
      (some-xx-multi $some nil predicate one-seq sequences)
      (some-xx-one $some nil predicate one-seq)))

(defun notany (predicate one-seq &rest sequences)
  "Apply PREDICATE to the 0-indexed elements of the sequences, then 
   possibly to those with index 1, and so on. Return NIL as soon
   as any invocation of PREDICATE returns a non-NIL value, or T if the end
   of any sequence is reached."
  (declare (dynamic-extent sequences))
  (if sequences
      (some-xx-multi $notany t predicate one-seq sequences)
      (some-xx-one $notany t predicate one-seq)))

(defun every (predicate one-seq &rest sequences)
  "Apply PREDICATE to the 0-indexed elements of the sequences, then
   possibly to those with index 1, and so on. Return NIL as soon
   as any invocation of PREDICATE returns NIL, or T if every invocation
   is non-NIL."
  (declare (dynamic-extent sequences))
  (if sequences
      (some-xx-multi $every t predicate one-seq sequences)
      (some-xx-one $every t predicate one-seq)))

(defun notevery (predicate one-seq &rest sequences)
  "Apply PREDICATE to 0-indexed elements of the sequences, then
   possibly to those with index 1, and so on. Return T as soon
   as any invocation of PREDICATE returns NIL, or NIL if every invocation
   is non-NIL."
  (declare (dynamic-extent sequences))
  (if sequences
      (some-xx-multi $notevery nil predicate one-seq sequences)
      (some-xx-one $notevery nil predicate one-seq)))

(defun some-xx-multi (caller at-end predicate first-seq sequences)
  (let* ((sequences (cons first-seq sequences))
         (min-vector-length target::target-most-positive-fixnum)
         (arg-slice (make-list (list-length sequences)))
         (cur-slice arg-slice)
         (not-result (negating-quantifier-p caller))
         result)
  (declare (fixnum min-vector-length)
           (list sequences arg-slice cur-slice)
           (dynamic-extent sequences arg-slice))
  (dolist (seq sequences)
    (seq-dispatch seq
                  nil
                  (setq min-vector-length (min min-vector-length
                                               (length seq)))))
  (dotimes (index min-vector-length)
    (dolist (one-seq sequences)
      (%rplaca cur-slice
               (if (vectorp one-seq)
                   (aref one-seq index)
                   (if one-seq
                       (progn
                         (%rplaca (memq one-seq sequences) (cdr one-seq))
                         (%car one-seq))
                       (return-from some-xx-multi at-end))))
      (setq cur-slice (%cdr cur-slice)))
    (setq result (apply predicate arg-slice)
          cur-slice arg-slice)
    (if not-result
        (when (not result)
          (return-from some-xx-multi
                       (if (eq caller $every) nil t)))
        (when result
          (return-from some-xx-multi
                       (if (eq caller $some) result nil)))))
  at-end))


(defun some-xx-one (caller at-end predicate seq
                           &aux (not-result (negating-quantifier-p caller))
                           result)
  (if (vectorp seq)
      (if (simple-vector-p seq)
        (locally (declare (type simple-vector seq))
          (dovector (element seq)
            (setq result (funcall predicate element))
            (if not-result
              (when (not result)
                (return-from some-xx-one
                  (if (eq caller $every) nil t)))
              (when result
                (return-from some-xx-one
                  (if (eq caller $some ) result nil))))))
        (dovector (element seq)
          (setq result (funcall predicate element))
          (if not-result
            (when (not result)
              (return-from some-xx-one
                (if (eq caller $every) nil t)))
            (when result
              (return-from some-xx-one
                (if (eq caller $some ) result nil))))))
      (dolist (element seq)
        (setq result (funcall predicate element))
        (if not-result
            (when (not result)
              (return-from some-xx-one
                           (if (eq caller $every) nil t)))
            (when result
              (return-from some-xx-one
                           (if (eq caller $some ) result nil))))))
      at-end)

;;; simple positional versions of find, position

(defun find-positional-test-key (item sequence test key)
  (if sequence
    (seq-dispatch
     sequence
     (let ((cons (member item sequence :test test :key key)))
       (and cons (%car cons)))
     (let ((pos (vector-position-1 item sequence nil test nil 0 nil key)))
       (and pos (aref sequence pos))))))

(defun find-positional-test-not-key (item sequence test-not key)
  (if sequence
    (seq-dispatch
     sequence
     (let ((cons (member item sequence :test-not test-not :key key)))
       (and cons (%car cons)))
     (let ((pos (vector-position-1 item sequence nil nil test-not 0 nil key)))
       (and pos (aref sequence pos))))))

(defun position-positional-test-key (item sequence test key)
  (if sequence
    (seq-dispatch
     sequence
     (progn
       (setq key (adjust-key key))
       (setq test
             (adjust-test-args item test nil))
       (if (or test key)
         (list-position/find-complex nil item sequence 0 nil test nil key)
         (list-position/find-simple nil item sequence 0 nil)))
     (vector-position-1 item sequence nil test nil 0 nil key))))

(defun position-positional-test-not-key (item sequence test-not key)
  (if sequence
    (seq-dispatch
     sequence
     (progn
       (setq key (adjust-key key))
       (multiple-value-bind (test test-not)
                            (adjust-test-args item nil test-not)
         (list-position/find-complex nil item sequence 0 nil test test-not key)))
     (vector-position-1 item sequence nil nil test-not 0 nil key))))


;;; Reduce:

(eval-when (:execute :compile-toplevel)
  
  (defmacro list-reduce (function sequence start end initial-value ivp key)
    (let ((what `(if ,key (funcall ,key (car sequence)) (car sequence))))
      `(let ((sequence (nthcdr ,start ,sequence)))
         (do ((count (if ,ivp ,start (1+ ,start)) (1+ count))
              (sequence (if ,ivp sequence (cdr sequence))
                        (cdr sequence))
              (value (if ,ivp ,initial-value ,what)
                     (funcall ,function value ,what)))
             ((= count ,end) value)))))
  
  (defmacro list-reduce-from-end (function sequence start end 
                                           initial-value ivp key)
    (let ((what `(if ,key (funcall ,key (car sequence)) (car sequence))))
      `(let ((sequence (nthcdr (- (length ,sequence) ,end) (reverse ,sequence))))
         (do ((count (if ,ivp ,start (1+ ,start)) (1+ count))
              (sequence (if ,ivp sequence (cdr sequence))
                        (cdr sequence))
              (value (if ,ivp ,initial-value ,what)
                     (funcall ,function ,what value)))
             ((= count ,end) value)))))
  
  ) ;; end eval-when

(defun reduce (function sequence &key from-end (start 0)
                        end (initial-value nil ivp) key)
  "The specified Sequence is ``reduced'' using the given Function.
  See manual for details."
  (unless end (setq end (length sequence)))
  (if (= end start)
    (if ivp initial-value (funcall function))
    (seq-dispatch
     sequence
     (if from-end
       (list-reduce-from-end  function sequence start end initial-value ivp key)
       (list-reduce function sequence start end initial-value ivp key))
     (let* ((disp (if from-end -1 1))
            (index (if from-end (1- end) start))
            (terminus (if from-end (1- start) end))
            (value (if ivp initial-value
                       (let ((elt (aref sequence index)))
                         (setq index (+ index disp))
                         (if key (funcall key elt) elt))))
            (element nil))
       (do* ()
            ((= index terminus) value)
         (setq element (aref sequence index)
               index (+ index disp)
               element (if key (funcall key element) element)
               value (funcall function (if from-end element value) (if from-end value element))))))))

(defun map-into (result-sequence function &rest sequences)
  (declare (dynamic-extent sequences))
  (let* ((nargs (list-length sequences))
         (temp (make-list (length sequences)))
         (maxcnt (seq-dispatch result-sequence (length result-sequence) (array-total-size result-sequence)))
         (rseq result-sequence))
    (declare (fixnum nargs maxcnt))
    (declare (dynamic-extent temp))
    ; this declaration is maybe bogus
    (dolist (seq sequences)
      (let ((len (length seq)))
        (declare (fixnum len))
        (if (< len maxcnt)(setq maxcnt len))))
    (dotimes (cnt maxcnt)
      (let ((args temp)(seqs sequences))
        (dotimes (i nargs)
          (let ((seq (%car seqs)))
            (cond ((listp seq)
                   (%rplaca seqs (%cdr seq))
                   (%rplaca args (%car seq)))
                  (t (%rplaca args (aref seq cnt)))))
          (setq args (%cdr args))
          (setq seqs (%cdr seqs))))
      (let ((res (apply function temp)))
        (cond ((consp rseq)
               (%rplaca rseq res)
               (setq rseq (%cdr rseq)))
              (t (setf (aref result-sequence cnt) res)))))
    (when (and (not (listp result-sequence))
               (array-has-fill-pointer-p result-sequence))
      (setf (fill-pointer result-sequence) maxcnt))
    result-sequence))
          
    
;;; Coerce:

#|
; don't know if this is always right
; It's almost never right: the "type-spec" could be something
; defined with DEFTYPE, whose last element (if it has one) has
; nothing to do with the "length" of the specified type.
(defun specifier-length (type-spec)
  (if (consp type-spec)
    (let ((len? (car (last type-spec))))
      (if (fixnump len?) len?))))
|#


(defun array-ctype-length (ctype)
  (if (typep ctype 'array-ctype)
    (let* ((dims (array-ctype-dimensions ctype)))
      (if (listp dims)
        (if (null (cdr dims))
          (let* ((dim0 (car dims)))
            (unless (eq dim0 '*) dim0)))))))




; from optimizer - just return object if type is OK


;If you change this, remember to change the transform.
(defun coerce (object output-type-spec)
  "Coerce the Object to an object of type Output-Type-Spec."
  (let* ((type (specifier-type output-type-spec)))
    (if (%typep object type)
      object
      (cond
        ((csubtypep type (specifier-type 'character))
         (character object))
        ((eq output-type-spec 'standard-char)
         (let ((char (character object)))
           (unless (standard-char-p char) (%err-disp $xcoerce object 'standard-char))
           char))
        ((eq output-type-spec 'compiled-function)
         (coerce-to-compiled-function object))
        ((csubtypep type (specifier-type 'function))
         (coerce-to-function-1 object))
        ((csubtypep type (specifier-type 'cons))
         (if object
           (coerce-to-list object)
           (report-bad-arg object 'cons)))
        ((csubtypep type (specifier-type 'list))
         (coerce-to-list object))
        ((csubtypep type (specifier-type 'string))
         (let ((length (array-ctype-length type)))
           (if (and length (neq length (length object)))
             (report-bad-arg (make-string length) `(string ,(length object)))))
         (coerce-to-uarray object #.(type-keyword-code :simple-string)
                           t))
        ((csubtypep type (specifier-type 'vector))
         (let ((length (array-ctype-length type)))
           (if (and length (neq length (length object)))
             (error 'invalid-subtype-error
                    :datum output-type-spec
                    :expected-type `(vector * ,(length object)))))
         (let* ((atype (simplify-vector-ctype type)))
           (unless (typep atype 'array-ctype)
             (error "Can't determine vector type of ~s" output-type-spec))
           (let* ((element-type (type-specifier (array-ctype-element-type atype))))
             (let ((length (array-ctype-length atype)))
               (if (and length (neq length (length object)))
                 (report-bad-arg (make-array length :element-type element-type)
                                 `(vector ,element-type ,(length object))))
               (coerce-to-uarray object (element-type-subtype element-type) t)))))
        ((csubtypep type (specifier-type 'array))
         (let* ((dims (array-ctype-dimensions type)))
           (when (consp dims)
             (when (not (null (cdr dims)))(error "~s is not a sequence type." output-type-spec))))
         (let ((length (array-ctype-length type)))
           (if (and length (neq length (length object)))
             (error "Length of ~s is not ~s." object length)))
         (coerce-to-uarray object (element-type-subtype (type-specifier 
                                                         (array-ctype-element-type type))) t))
        ((numberp object)
         (let ((res
                (cond
                  ((csubtypep type (specifier-type 'double-float))
                   (float object 1.0d0))
                  ((csubtypep type (specifier-type 'float))
                   (float object 1.0s0))                		
                  ((csubtypep type (specifier-type 'complex))
                   (coerce-to-complex object  output-type-spec)))))
           (unless res                  ;(and res (%typep res type))
             (error "~S can't be coerced to type ~S." object output-type-spec))
           res))
        (t (error "~S can't be coerced to type ~S." object output-type-spec))))))

(defun %coerce-to-string (seq)
   (let* ((len (length seq))
          (string (make-string len)))
     (declare (fixnum len) (simple-base-string string))
     (if (typep seq 'list)
       (do* ((l seq (cdr l))
             (i 0 (1+ i)))
            ((null l) string)
         (declare (list l) ; we know that it's a proper list because LENGTH won
                  (fixnum i))
         (setf (schar string i) (car l)))
       (dotimes (i len string)
         (setf (schar string i) (aref seq i))))))

(defun %coerce-to-vector (seq subtype)
   (let* ((len (length seq))
          (vector (%alloc-misc len subtype)))
     (declare (fixnum len) (type (simple-array * (*)) vector))
     (if (typep seq 'list)
       (do* ((l seq (cdr l))
             (i 0 (1+ i)))
            ((null l) vector)
         (declare (list l) ; we know that it's a proper list because LENGTH won
                  (fixnum i))
         (setf (uvref vector i) (car l)))
       (dotimes (i len vector)
         (setf (uvref vector i) (aref seq i))))))

(defun %coerce-to-list (seq)
  (if (typep seq 'list)
    seq
    (collect ((result))
      (dotimes (i (length seq) (result))
        (result (aref seq i))))))




(defun coerce-to-complex (object  output-type-spec)
  (if (consp output-type-spec)
      (let ((type2 (cadr output-type-spec)))     
        (if (complexp object)
	    (complex (coerce (realpart object) type2)(coerce (imagpart object) type2))
	    (complex (coerce object type2) 0)))
      (complex object)))
        

(defun coerce-to-function-1 (thing)
  (if (functionp thing)
    thing
    (if (symbolp thing)
      (%function thing)
      (if (lambda-expression-p thing)
        (%make-function nil thing nil)
        (%err-disp $xcoerce thing 'function)))))

;;; Internal Frobs:
;(coerce object '<array-type>)
(defun coerce-to-uarray (object subtype simple-p)
  (if (typep object 'array)
    (if (and (or (not simple-p) (typep object 'simple-array))
             (or (null subtype) (eq (array-element-subtype object) subtype)))
      object
      ;Make an array of the same shape as object but different subtype..
      (%copy-array subtype object))
    (if (typep object 'list)
      (%list-to-uvector subtype object)
      (%err-disp $xcoerce object 'array))))

;(coerce object 'list)
(defun coerce-to-list (object)
  (seq-dispatch 
   object
   object
   (let* ((n (length object)))
     (declare (fixnum n))
     (multiple-value-bind (data offset) (array-data-and-offset object)
       (let* ((head (cons nil nil))
              (tail head))
         (declare (dynamic-extent head)
                  (cons head tail))
         (do* ((i 0 (1+ i))
               (j offset (1+ j)))
              ((= i n) (cdr head))
           (declare (fixnum i j))
           (setq tail (cdr (rplacd tail (cons (uvref data j) nil))))))))))
 

(defun %copy-array (new-subtype array)
  ;To be rewritten once make-array disentangled (so have a subtype-based entry
  ;point)
  (make-array (if (eql 1 (array-rank array))
                (length array)
                (array-dimensions array))
              :element-type (element-subtype-type new-subtype)
              :initial-contents array ;***** WRONG *****
              ))

(defun check-count (c)
  (if c
    (min (max (require-type c 'integer) 0) target::target-most-positive-fixnum)
    target::target-most-positive-fixnum))

;;; Delete:

(defun list-delete-1 (item list from-end test test-not start end count key 
                           &aux (temp list)  revp)
  (unless end (setq end target::target-most-positive-fixnum))
  (when (and from-end count)
    (let ((len (length temp)))
      (if (not (%i< start len))
        (return-from list-delete-1 temp))
      (setq temp (nreverse temp) revp t)
      (psetq end (%i- len start)
             start (%i- len (%imin len end)))))
  (setq key (adjust-key key))
  (multiple-value-setq (test test-not)
                       (adjust-test-args item test test-not))
  (setq temp
        (if (or test key test-not)
          (list-delete-moderately-complex item temp start end count test test-not key)
          (list-delete-very-simple item temp start end count)))
   (if revp
    (nreverse temp)
    temp))


(defun list-delete-very-simple (item list start end count)
  (unless start (setq start 0))
  (unless end (setq end target::target-most-positive-fixnum))
  (setq count (check-count count))
  (do* ((handle (cons nil list))
        (splice handle)
        (numdeleted 0)
        (i 0 (1+ i)))
       ((or (eq i end) (null (%cdr splice)) (eq numdeleted count))
        (%cdr handle))
    (declare (fixnum i start end count numdeleted)  ; declare-type-free !!
             (dynamic-extent handle) 
             (list splice handle))
    (if (and (%i>= i start) (eq item (car (%cdr splice))))
      (progn
        (%rplacd splice (%cddr splice))
        (setq numdeleted (%i+ numdeleted 1)))
      (setq splice (%cdr splice)))))

(defun list-delete-moderately-complex (item list start end count test test-not key)
  (unless start (setq start 0))
  (unless end (setq end target::target-most-positive-fixnum))
  (setq count (check-count count))
  (do* ((handle (cons nil list))
        (splice handle)
        (numdeleted 0)
        (i 0 (1+ i)))
       ((or (= i end) (null (cdr splice)) (= numdeleted count))
        (cdr handle))
    (declare (fixnum i start end count numdeleted)
             (dynamic-extent handle)
             (list splice))
    (if (and (>= i start) (matchp2 item (cadr splice) test test-not key))
      (progn
        (rplacd splice (cddr splice))
        (setq numdeleted (1+ numdeleted)))
      (setq splice (cdr splice)))))

(defun list-delete (item list &key from-end test test-not (start 0)
                         end count key 
                         &aux (temp list)  revp)
  (unless end (setq end target::target-most-positive-fixnum))
  (when (and from-end count)
    (let ((len (length temp)))
      (if (not (%i< start len))
        (return-from list-delete temp))
      (setq temp (nreverse temp) revp t)
      (psetq end (%i- len start)
             start (%i- len (%imin len end)))))
  (setq key (adjust-key key))
  (multiple-value-setq (test test-not)
                       (adjust-test-args item test test-not))
  (setq temp
        (if (or test key test-not)
          (list-delete-moderately-complex item temp start end count test test-not key)
          (list-delete-very-simple item temp start end count)))
   (if revp
    (nreverse temp)
    temp))

; The vector will be freshly consed & nothing is displaced to it,
; so it's legit to destructively truncate it.
; Likewise, it's ok to access its components with UVREF.

(defun simple-vector-delete (item vector test test-not key start end inc count
                                  &aux (length (length vector)) 
                                  subtype pos fill)
  (setq key (adjust-key key))
  (multiple-value-setq (test test-not) (adjust-test-args item test test-not))
  (setq end (check-sequence-bounds vector start end))
  (setq fill start)
  (if (%i< inc 0) (psetq start (%i- end 1) end (%i- start 1)))
  (let* ((bv (make-array (the fixnum (length vector)) :element-type 'bit :Initial-element 0))
         offset)    
    (declare (dynamic-extent bv)
             (type (simple-array bit (*)) bv))
    (multiple-value-setq (vector offset)(array-data-and-offset vector))
    (setq subtype (typecode vector))
    (setq pos start)
    (loop
      (when (or (eq count 0) (eq pos end))
        (unless (eq pos end)
          (incf fill (abs (- pos end))))
        (return))
      (if (matchp2 item (uvref  vector (%i+ pos offset))
                   test test-not key)
        (progn (setf (aref bv pos) 1)
               (setq count (%i- count 1)))
        (setq fill (%i+ fill 1)))
      (setq pos (%i+ pos inc)))
    (when (%i< inc 0)
      (psetq start (%i+ end 1) end (%i+ start 1)))
    (let* ((tail (- length end))
           (size (+ fill tail))
           (new-vect (%alloc-misc size subtype))
           (fill-end fill))
      (declare (fixnum tail size))
      (when (neq 0 start)
        (dotimes (i start)
          (setf (uvref new-vect i) (uvref  vector (%i+ offset i)))
          ))
      (setq fill start)
      (setq pos start)
      (loop
        (if (eq fill fill-end) (return))
        (if (neq 1 (aref bv pos))
          (progn
            (setf (uvref new-vect fill) (uvref vector (%i+ offset pos)))
            (setq fill (%i+ fill 1))))
        (setq pos (%i+ pos 1)))
      (setq pos end)
      (loop
        (when (eq fill size) (return))
          (setf (uvref  new-vect fill) (uvref  vector (%i+ offset pos)))
          (setq fill (%i+ fill 1)
                pos (%i+ pos 1)))
      new-vect)))


; When a vector has a fill pointer & it can be "destructively modified" by adjusting
; that fill pointer.
(defun vector-delete (item vector test test-not key start end inc count
                           &aux (length (length vector)) pos fill val)
  (setq key (adjust-key key))
  (multiple-value-setq (test test-not) (adjust-test-args item test test-not))
  (setq end (check-sequence-bounds vector start end))
  (if (%i< inc 0) (psetq start (%i- end 1) end (%i- start 1)))
  (setq fill (setq pos start))
  (loop
    (if (or (eq count 0) (eq pos end)) (return))
    (if (matchp2 item (setq val (aref vector pos)) test test-not key)
      (setq count (%i- count 1))
      (progn
        (if (neq fill pos) (setf (aref vector fill) val))
        (setq fill (%i+ fill inc))))
    (setq pos (%i+ pos inc)))
  (if (%i> fill pos) (psetq fill (%i+ pos 1) pos (%i+ fill 1)))
  (loop
    (if (eq pos length) (return))
    (setf (aref vector fill) (aref vector pos))
    (setq fill (%i+ fill 1) pos (%i+ pos 1)))
  (when (eq t (array-element-type vector))
    (let ((old-fill (fill-pointer vector))
          (i fill))
      (declare (fixnum i old-fill))
      (loop
        (when (>= i old-fill) (return))
        (setf (aref vector i) nil)
        (incf i))))
  (setf (fill-pointer vector) fill)
  vector)

(defun delete (item sequence &key from-end test test-not (start 0)
                    end count key)
  "Return a sequence formed by destructively removing the specified ITEM from
  the given SEQUENCE."
  (setq count (check-count count))
  (if sequence
    (seq-dispatch
     sequence
     (list-delete-1 item 
                  sequence 
                  from-end
                  test 
                  test-not
                  start 
                  end 
                  count
                  key)
     (if (array-has-fill-pointer-p sequence)
       (vector-delete item sequence test test-not key start end (if from-end -1 1) count)
       (simple-vector-delete item
                            sequence
                             test test-not key start end (if from-end -1 1) count)))))

(defun delete-if (test sequence &key from-end (start 0)                       
                       end count key)
  "Return a sequence formed by destructively removing the elements satisfying
  the specified PREDICATE from the given SEQUENCE."
  (delete test sequence
          :test #'funcall
          :from-end from-end 
          :start start 
          :end end 
          :count count 
          :key key))

(defun delete-if-not (test sequence &key from-end (start 0) end count key)
  "Return a sequence formed by destructively removing the elements not
  satisfying the specified PREDICATE from the given SEQUENCE."
  (delete test sequence 
          :test-not #'funcall 
          :from-end from-end 
          :start start 
          :end end 
          :count count 
          :key key))



;;; Remove:



(defun remove (item sequence &key from-end test test-not (start 0)
                    end count key)
  "Return a copy of SEQUENCE with elements satisfying the test (default is
   EQL) with ITEM removed."
  (setq count (check-count count))
  (seq-dispatch
   sequence
   (list-delete-1 item 
                (copy-list sequence)
                from-end
                test 
                test-not
                start 
                end 
                count
                key)
   (simple-vector-delete item
                         sequence
                         test
                         test-not
                         key
                         start
                         end
                         (if from-end -1 1)
                         count)))




(defun remove-if (test sequence &key from-end (start 0)
                         end count key)
  "Return a copy of sequence with elements such that predicate(element)
   is non-null removed"
  (setq count (check-count count))
  (remove test sequence
          :test #'funcall
          :from-end from-end
          :start start
          :end end
          :count count
          :key key))

(defun remove-if-not (test sequence &key from-end (start 0)
                         end count key)
  "Return a copy of sequence with elements such that predicate(element)
   is null removed"
  (setq count (check-count count))
  (remove test sequence
          :test-not #'funcall
          :from-end from-end
          :start start
          :end end
          :count count
          :key key))

;;; Remove-Duplicates:

;;; Remove duplicates from a list. If from-end, remove the later duplicates,
;;; not the earlier ones. Thus if we check from-end we don't copy an item
;;; if we look into the already copied structure (from after :start) and see
;;; the item. If we check from beginning we check into the rest of the 
;;; original list up to the :end marker (this we have to do by running a
;;; do loop down the list that far and using our test.
; test-not is typically NIL, but member doesn't like getting passed NIL
; for its test-not fn, so I special cased the call to member. --- cfry

(defun remove-duplicates (sequence &key (test #'eql) test-not (start 0) 
      from-end end key)
  "The elements of SEQUENCE are compared pairwise, and if any two match,
   the one occurring earlier is discarded, unless FROM-END is true, in
   which case the one later in the sequence is discarded. The resulting
   sequence is returned.

   The :TEST-NOT argument is deprecated."
  (setq end (check-sequence-bounds sequence start end))
  (delete-duplicates (copy-seq sequence) :from-end from-end :test test
                     :test-not test-not :start start :end end :key key))

;;; Delete-Duplicates:

(defparameter *delete-duplicates-hash-threshold*  200)

(defun list-delete-duplicates* (list test test-not key from-end start end)
  ;;(%print "test:" test "test-not:" test-not "key:" key)
  (let* ((len (- end start))
	 (handle (cons nil list))
	 (previous (nthcdr start handle)))
    (declare (dynamic-extent handle))
    (if (and (> len *delete-duplicates-hash-threshold*)
	     (or (eq test 'eq) (eq test 'eql) (eq test 'equal) (eq test 'equalp)
		 (eq test #'eq) (eq test #'eql) (eq test #'equal) (eq test #'equalp)))
      (let ((hash (make-hash-table :size len :test test :shared nil)))
        (loop for i from start below end as obj in (cdr previous)
          do (incf (gethash (funcall key obj) hash 0)))
        (loop for i from start below end while (cdr previous)
          do (let* ((current (cdr previous))
                    (obj (car current))
                    (obj-key (funcall key obj)))
               (if (if from-end
                     ;; Keep first ref
                     (prog1 (gethash obj-key hash) (setf (gethash obj-key hash) nil))
                     ;; Keep last ref
                     (eql (decf (gethash obj-key hash)) 0))
                 (setq previous current)
                 (rplacd previous (cdr current))))))
      (do ((current (cdr previous) (cdr current))
           (index start (1+ index)))
          ((or (= index end) (null current)))
        ;;(%print "outer loop top current:" current "previous:" previous)
        (if (do ((x (if from-end 
                      (nthcdr (1+ start) handle)
                      (cdr current))
                    (cdr x))
                 (i (1+ index) (1+ i)))
                ((or (null x) 
                     (and (not from-end) (= i end)) 
                     (eq x current)) 
                 nil)
              ;;(%print "inner loop top x:" x "i:" i)
              (if (list-delete-duplicates*-aux current x test test-not key)
                (return t)))
          (rplacd previous (cdr current))
          (setq previous (cdr previous)))))
    (cdr handle)))

(defun list-delete-duplicates*-aux (current x test test-not key)
  (if test-not
    (not (funcall test-not 
                  (funcall key (car current))
                  (funcall key (car x))))
    (funcall test 
             (funcall key (car current)) 
             (funcall key (car x)))))


(defun vector-delete-duplicates* (vector test test-not key from-end start end 
					 &optional (length (length vector)))
  (declare (vector vector))
  (let* ((len (- end start))
	 (index start)
	 (jndex start))
    (if (and (not test-not)
	     (> len *delete-duplicates-hash-threshold*)
	     (or (eq test 'eq) (eq test 'eql) (eq test 'equal) (eq test 'equalp)
		 (eq test #'eq) (eq test #'eql) (eq test #'equal) (eq test #'equalp)))
	(let ((hash (make-hash-table :size len :test test :shared nil)))
	  (loop for i from start below end as obj = (aref vector i)
	     do (incf (gethash (funcall key obj) hash 0)))
	  (loop while (< index end) as obj = (aref vector index) as obj-key = (funcall key obj)
	     do (incf index)
	     do (when (if from-end
			  (prog1 (gethash obj-key hash) (setf (gethash obj-key hash) nil))
			  (eql (decf (gethash obj-key hash)) 0))
		  (aset vector jndex obj)
		  (incf jndex))))
	(loop while (< index end) as obj = (aref vector index)
	   do (incf index)
	   do (unless (position (funcall key obj) vector :key key
				:start (if from-end start index) :test test
				:end (if from-end jndex end) :test-not test-not)
		(aset vector jndex obj)
		(incf jndex))))
    (do ((index index (1+ index))	; copy the rest of the vector
	 (jndex jndex (1+ jndex)))
	((= index length)
	 (setq vector (shrink-vector vector jndex)))
      (aset vector jndex (aref vector index)))))


(defun delete-duplicates (sequence &key (test #'eql) test-not (start 0) from-end end key)
  "The elements of SEQUENCE are examined, and if any two match, one is
   discarded.  The resulting sequence, which may be formed by destroying the
   given sequence, is returned.
   Sequences of type STR have a NEW str returned."
  (setq end (check-sequence-bounds sequence start end))
  (unless key (setq key #'identity))
  (seq-dispatch sequence
    (if sequence
      (list-delete-duplicates* sequence test test-not key from-end start end))
    (vector-delete-duplicates* sequence test test-not key from-end start end)))

(defun list-substitute* (pred new list start end count key 
                              test test-not old)
  ;(print-db pred new list start end count key test test-not old)
  (let* ((result (list nil))
         elt
         (splice result)
         (list list))           ; Get a local list for a stepper.
    (do ((index 0 (1+ index)))
        ((= index start))
      (setq splice (cdr (rplacd splice (list (car list)))))
      (setq list (cdr list)))
    (do ((index start (1+ index)))
        ((or (and end (= index end)) (null list) (= count 0)))
      (setq elt (car list))
      (setq splice
            (cdr (rplacd splice
                         (list
                          (cond ((case pred
                                   (normal
                                    (if test-not
                                      (not (funcall test-not  old
                                                    ;fry mod to slisp, which had arg order of OLD and ELT reversed.
                                                    (funcall key elt)))
                                      (funcall test old
                                               (funcall key elt))))
                                   (if (funcall test (funcall key elt)))
                                   (if-not (not (funcall test 
                                                         (funcall key elt)))))
                                 (setq count (1- count))
                                 new)
                                (t elt))))))
      (setq list (cdr list)))
    (do ()
        ((null list))
      (setq splice (cdr (rplacd splice (list (car list)))))
      (setq list (cdr list)))
    (cdr result)))

;;; Replace old with new in sequence moving from left to right by incrementer
;;; on each pass through the loop. Called by all three substitute functions.
(defun vector-substitute* (pred new sequence incrementer left right length
                                start end count key test test-not old)
  (let ((result (make-sequence-like sequence length))
        (index left))
    (do ()
        ((= index start))
      (aset result index (aref sequence index))
      (setq index (+ index incrementer)))
    (do ((elt))
        ((or (= index end) (= count 0)))
      (setq elt (aref sequence index))
      (aset result index 
            (cond ((case pred
                     (normal
                      (if test-not
                        (not (funcall test-not old (funcall key elt))) ;cfry mod
                        (funcall test old (funcall key elt)))) ;cfry mod
                     (if (funcall test (funcall key elt)))
                     (if-not (not (funcall test (funcall key elt)))))
                   (setq count (1- count))
                   new)
                  (t elt)))
      (setq index (+ index incrementer)))
    (do ()
        ((= index right))
      (aset result index (aref sequence index))
      (setq index (+ index incrementer)))
    result))

;;; Substitute:

(defun substitute (new old sequence &key from-end (test #'eql) test-not
                       (start 0) count
                       end (key #'identity))
  "Return a sequence of the same kind as SEQUENCE with the same elements,
  except that all elements equal to OLD are replaced with NEW. See manual
  for details."
  (setq count (check-count count))
  (let ((length (length sequence))        )
    (setq end (check-sequence-bounds sequence start end))
    (seq-dispatch 
     sequence
     (if from-end
       (nreverse (list-substitute* 'normal new (reverse sequence) (- length end)
                                   (- length start) count key test test-not old))
       (list-substitute* 'normal new sequence start end count key test test-not
                         old))
     (if from-end
       (vector-substitute* 'normal new sequence -1 (1- length) -1 length 
                           (1- end) (1- start) count key test test-not old)
       (vector-substitute* 'normal new sequence 1 0 length length
                           start end count key test test-not old)))))


(defun substitute-if (new test sequence &key from-end (start 0)
                          (end (length sequence))
                          count (key #'identity))
  "Return a sequence of the same kind as SEQUENCE with the same elements
  except that all elements satisfying the PRED are replaced with NEW. See
  manual for details."
  (substitute new test sequence
              :from-end from-end
              :test #'funcall
              :start start
              :end end
              :from-end from-end
              :count count
              :key key))

(defun substitute-if-not (new test sequence &key from-end (start 0)
                              (end (length sequence))
                              count (key #'identity))
  "Return a sequence of the same kind as SEQUENCE with the same elements
  except that all elements not satisfying the PRED are replaced with NEW.
  See manual for details."
  (substitute new test sequence
              :from-end from-end
              :test-not #'funcall
              :start start
              :end end
              :from-end from-end
              :count count
              :key key))

;;; NSubstitute:

(defun nsubstitute (new old sequence &key from-end (test #'eql) test-not 
                        end 
                        (count target::target-most-positive-fixnum) (key #'identity) (start 0))
  "Return a sequence of the same kind as SEQUENCE with the same elements
  except that all elements equal to OLD are replaced with NEW. The SEQUENCE
  may be destructively modified. See manual for details."
  (setq count (check-count count))
  (let ((incrementer 1)
	(length (length sequence)))
    (setq end (check-sequence-bounds sequence start end))
    (seq-dispatch
     sequence
      (if from-end
        (nreverse (nlist-substitute*
                   new old (nreverse (the list sequence))
                   test test-not 
                   (- length end) 
                   (- length start)
                   count key))
        (nlist-substitute* new old sequence
                           test test-not start end count key))
      (progn 
        (if from-end
          (psetq start (1- end)
                 end (1- start)
                 incrementer -1))
        (nvector-substitute* new old sequence incrementer
                             test test-not start end count key)))))

(defun nlist-substitute* (new old sequence test test-not start end count key)
  (do ((list (nthcdr start sequence) (cdr list))
       (index start (1+ index)))
      ((or (and end (= index end)) (null list) (= count 0)) sequence)
    (when (if test-not
            (not (funcall test-not  old (funcall key (car list)))) ;cfry mod
            (funcall test  old (funcall key (car list)))) ;cfry mod
      (rplaca list new)
      (setq count (1- count)))))

(defun nvector-substitute* (new old sequence incrementer
                                test test-not start end count key)
  (do ((index start (+ index incrementer)))
      ((or (= index end) (= count 0)) sequence)
    (when (if test-not
            (not (funcall test-not  old (funcall key (aref sequence index))))
            ;above cfry mod. both order of argss to test-not and paren error
            ; between the funcall key and the funcall test-not
            (funcall test old (funcall key (aref sequence index)))) ;cfry mod
      (aset sequence index new)
      (setq count (1- count)))))

;;; NSubstitute-If:

(defun nsubstitute-if (new test sequence &key from-end (start 0)
                           end  
                           (count target::target-most-positive-fixnum) (key #'identity))
  "Return a sequence of the same kind as SEQUENCE with the same elements
   except that all elements satisfying the PRED are replaced with NEW. 
   SEQUENCE may be destructively modified. See manual for details."
  (nsubstitute new test sequence
               :from-end from-end
               :test #'funcall
               :start start
               :end end
               :count count
               :key key))


;;; NSubstitute-If-Not:

(defun nsubstitute-if-not (new test sequence &key from-end (start 0)
                               end (count target::target-most-positive-fixnum) (key #'identity))
  "Return a sequence of the same kind as SEQUENCE with the same elements
   except that all elements not satisfying the TEST are replaced with NEW.
   SEQUENCE may be destructively modified. See manual for details."
  (nsubstitute new test sequence
                 :from-end from-end
                 :test-not #'funcall
                 :start start
                 :end end
                 :count count
                 :key key))


;;; Position:

(defun list-position/find-1 (eltp item list from-end test test-not start end key &aux hard)
  ;;if eltp is true, return element, otherwise return position
  (setq key (adjust-key key))
  (multiple-value-setq (test test-not)
                       (adjust-test-args item test test-not))
  (setq end (check-sequence-bounds list start end)
        hard (or test key test-not))
  (if from-end
    (if hard
      (list-position/find-from-end-complex eltp item list start end test test-not key)
      (list-position/find-from-end-simple eltp item list start end))
    (if hard
      (list-position/find-complex eltp item list start end test test-not key)
      (list-position/find-simple eltp item list start end))))

(defun position (item sequence &key from-end test test-not (start 0) end key)
  (if sequence
    (seq-dispatch 
     sequence
     (list-position/find-1 nil item sequence from-end test test-not start end key)
     (vector-position-1 item sequence from-end test test-not start end key))))

;Is it really necessary for these internal functions to take keyword args?
(defun list-position/find (eltp item list &key from-end test test-not (start 0) end key &aux hard)
  ;;if eltp is true, return element, otherwise return position
  (setq key (adjust-key key))
  (multiple-value-setq (test test-not)
                       (adjust-test-args item test test-not))
  (setq end (check-sequence-bounds list start end)
        hard (or test key test-not))
  (if from-end
    (if hard
      (list-position/find-from-end-complex eltp item list start end test test-not key)
      (list-position/find-from-end-simple eltp item list start end))
    (if hard
      (list-position/find-complex eltp item list start end test test-not key)
      (list-position/find-simple eltp item list start end))))

;;; make these things positional



;;; add a simple-vector case

(defun vector-position-1 (item vector from-end test test-not start end key
                        &aux (inc (if from-end -1 1)) pos)
  (setq end (check-sequence-bounds vector start end))
  (setq key (adjust-key key))
  (multiple-value-setq (test test-not) (adjust-test-args item test test-not))
  (if from-end (psetq start (%i- end 1) end (%i- start 1)))
  (setq pos start)
  (if (simple-vector-p vector)
    (locally (declare (type simple-vector vector)
                      (optimize (speed 3) (safety 0)))
      (loop
        (if (eq pos end) (return))
        (if (matchp2 item (aref vector pos) test test-not key) (return pos))
        (setq pos (%i+ pos inc))))
    (loop
      (if (eq pos end) (return))
      (if (matchp2 item (aref vector pos) test test-not key) (return pos))
      (setq pos (%i+ pos inc)))))

(defun list-position/find-simple (eltp item list start end &aux (pos 0))
  (loop
    (if (or (eq pos start) (null list))
      (return)
      (setq list (cdr list) pos (%i+ pos 1))))
  (loop
    (if (and list (neq end pos))
      (if (eq item (car list))
        (return (if eltp item pos))
        (setq list (%cdr list) pos (%i+ pos 1)))
      (return))))

(defun list-position/find-complex (eltp item list start end test test-not key &aux (pos 0))
  (loop
    (if (or (eq pos start) (null list))
      (return)
      (setq list (cdr list) pos (%i+ pos 1))))
  (loop
    (if (and list (neq end pos))
      (progn
        (if (matchp2 item (car list) test test-not key)
          (return (if eltp (%car list) pos))
          (setq list (%cdr list) pos (%i+ pos 1))))
      (return))))

(defun list-position/find-from-end-simple (eltp item list start end &aux (pos 0) ret)
  (loop
    (if (or (eq pos start) (null list))
      (return)
      (setq list (cdr list) pos (%i+ pos 1))))
  (loop
    (if (and list (neq end pos))
      (progn
        (if (eq item (car list)) (setq ret pos))
        (setq list (%cdr list) pos (%i+ pos 1)))
      (return (if eltp (if ret item) ret)))))

(defun list-position/find-from-end-complex (eltp item list start end test test-not key 
                                            &aux (pos 0) ret val)
  (loop
    (if (or (eq pos start) (null list))
      (return)
      (setq list (cdr list) pos (%i+ pos 1))))
  (loop
    (if (and list (neq end pos))
      (progn
        (if (matchp2 item (setq val (car list)) test test-not key)
          (setq ret (if eltp val pos)))
        (setq list (%cdr list) pos (%i+ pos 1)))
      (return ret))))

(defun vector-position (item vector &key from-end test test-not (start 0) end key
                        &aux (inc (if from-end -1 1)) pos)
  (setq end (check-sequence-bounds vector start end))
  (setq key (adjust-key key))
  (multiple-value-setq (test test-not) (adjust-test-args item test test-not))
  (if from-end (psetq start (%i- end 1) end (%i- start 1)))
  (setq pos start)
  (loop
    (if (eq pos end) (return))
    (if (matchp2 item (aref vector pos) test test-not key) (return pos))
    (setq pos (%i+ pos inc))))

;;; Position-if:

(defun position-if (test sequence &key from-end (start 0) end key)
  (position test sequence
            :test #'funcall
            :from-end from-end
            :start start
            :end end
            :key key))


;;; Position-if-not:

(defun position-if-not (test sequence &key from-end (start 0) end key)
  (position test sequence
            :test-not #'funcall
            :from-end from-end
            :start start
            :end end
            :key key))

;;; Count:

(defun vector-count-from-start (test item sequence start end key)
  (declare (fixnum start end))
  (do* ((index start (1+ index))
        (count 0))
       ((= index end) count)
    (declare (fixnum index count))
    (when (funcall test item  (funcall key (aref sequence index)))
      (incf count))))

(defun vector-count-from-end (test item sequence start end key)
  (declare (fixnum start end))
  (do* ((index (1- end) (1- index))
        (count 0)
        (limit (1- start)))
       ((= index limit) count)
    (declare (fixnum index count limit))
    (when (funcall test item (funcall key (aref sequence index)))
      (incf count))))

(defun vector-count-not-p-from-start (test-not item sequence start end key)
  (declare (fixnum start end))
  (do* ((index start (1+ index))
        (count 0))
       ((= index end) count)
    (declare (fixnum index count))
    (unless (funcall test-not item (funcall key (aref sequence index)))
      (incf count))))

(defun vector-count-not-p-from-end (test-not item sequence start end key)
  (declare (fixnum start end))
  (do* ((index (1- end) (1- index))
        (count 0)
        (limit (1- start)))
       ((= index limit) count)
    (declare (fixnum index count limit))
    (unless (funcall test-not item (funcall key (aref sequence index)))
      (incf count))))

(defun list-count-from-start (test item sequence start end key)
  (declare (fixnum start end) (list sequence))
  (do* ((seq (nthcdr start sequence) (cdr seq))
        (element (car seq) (car seq))
        (index start (1+ index))
        (count 0))
       ((or (= index end) (null seq)) count)
    (declare (fixnum index count) (list seq))
    (when (funcall test item (funcall key element))
      (incf count))))

(defun list-count-from-end (test item sequence start end key)
  (declare (fixnum start end))
  (let* ((len (length sequence)))
    (declare (fixnum len))
    (list-count-from-start test item (reverse sequence) (- len end) (- len start) key)))

(defun list-count-not-p-from-start (test-not item sequence start end key)
  (declare (fixnum start end) (list sequence))
  (do* ((seq (nthcdr start sequence) (cdr seq))
        (element (car seq) (car seq))
        (index start (1+ index))
        (count 0))
       ((or (= index end) (null seq)) count)
    (declare (fixnum index count) (list seq))
    (unless (funcall test-not item  (funcall key element))
      (incf count))))

(defun list-count-not-p-from-end (test-not item sequence start end key)
  (declare (fixnum start end))
  (let* ((len (length sequence)))
    (declare (fixnum len))
    (list-count-not-p-from-start test-not item (reverse sequence) (- len end) (- len start) key)))

(defun count (item sequence &key from-end (test #'eql testp)
                   (test-not nil notp) (start 0) end key)
  "Return the number of elements in SEQUENCE satisfying a test with ITEM,
   which defaults to EQL."
  (if (and testp notp)
    (test-not-error test test-not))
  (unless key
    (setq key #'identity))
  (setq end (check-sequence-bounds sequence start end))
  (if sequence
    (seq-dispatch
     sequence
     (if notp
       (if from-end
         (list-count-not-p-from-end test-not item  sequence start end key)
         (list-count-not-p-from-start test-not item sequence start end key))
       (if from-end
         (list-count-from-end test item sequence start end key)
         (list-count-from-start test item sequence start end key)))
     (if notp
       (if from-end
         (vector-count-not-p-from-end test-not item sequence start end key)
         (vector-count-not-p-from-start test-not item sequence start end key))
       (if from-end
         (vector-count-from-end test item sequence start end key)
         (vector-count-from-start test item sequence start end key))))
    0))


;;; Count-if:

(defun count-if (test sequence &key from-end (start 0) end key)
  "Return the number of elements in SEQUENCE satisfying PRED(el)."
  (count test sequence
         :test #'funcall
         :from-end from-end
         :start start
         :end end
         :key key))

;;; Count-if-not:

(defun count-if-not (test sequence &key from-end (start 0) end key)
  "Return the number of elements in SEQUENCE not satisfying TEST(el)."
  (count test sequence
         :test-not #'funcall
         :from-end from-end
         :start start
         :end end
         :key key))


;;; Find:

(defun find (item sequence &key from-end test test-not (start 0) end key &aux temp)
  (if sequence
    (seq-dispatch
     sequence
     (list-position/find-1 t item sequence from-end test test-not start end key)
     (if (setq temp (vector-position-1 item sequence from-end test test-not start end key))
       (aref sequence temp)))))

(defun find-if (test sequence &key from-end (start 0) end key)
  (find test sequence
        :test #'funcall
        :from-end from-end
        :start start
        :end end
        :key key))

(defun find-if-not (test sequence &key from-end (start 0) end key)
  (find test sequence
        :test-not #'funcall
        :from-end from-end
        :start start
        :end end
        :key key))


;;; Mismatch:

(defun mismatch (seq1 seq2 &key (from-end nil)
                                  (test #'eql)
                                  (test-not nil)
                                  (key #'identity)
                                  (start1 0)
                                  (start2 0)
                                  (end1 nil)
                                  (end2 nil)
                             &aux (length1 (length seq1))
                                  (length2 (length seq2))
                                  (vectorp1 (vectorp seq1))
                                  (vectorp2 (vectorp seq2)))
  "The specified subsequences of SEQUENCE1 and SEQUENCE2 are compared
   element-wise. If they are of equal length and match in every element, the
   result is NIL. Otherwise, the result is a non-negative integer, the index
   within SEQUENCE1 of the leftmost position at which they fail to match; or,
   if one is shorter than and a matching prefix of the other, the index within
   SEQUENCE1 beyond the last position tested is returned. If a non-NIL
   :FROM-END argument is given, then one plus the index of the rightmost
   position in which the sequences differ is returned."
  ;seq type-checking is done by length
  ;start/end type-cheking is done by <= (below)
  ;test/key type-checking is done by funcall
  ;no check for both test and test-not
  (or end1 (setq end1 length1))
  (or end2 (setq end2 length2))
  (unless (and (<= start1 end1 length1)
               (<= start2 end2 length2))
    (error "Sequence arg out of range"))
  (unless vectorp1
    (setq seq1 (nthcdr start1 seq1))
    (if from-end
      (do* ((s1 ())
            (i start1 (1+ i)))
           ((= i end1) (setq seq1 s1))
        (push (pop seq1) s1))))
  (unless vectorp2
    (setq seq2 (nthcdr start2 seq2))
    (if from-end
      (do* ((s2 ())
            (i start2 (1+ i)))
           ((= i end2) (setq seq2 s2))
        (push (pop seq2) s2))))
  (when test-not (setq test test-not))
  (if from-end
      ;from-end
      (let* ((count1 end1)
             (count2 end2)
             (elt1)
             (elt2))
        (loop
          (if (or (eq count1 start1)
                  (eq count2 start2))
              (return-from mismatch
                           (if (and (eq count1 start1)
                                    (eq count2 start2))
                               nil
                               count1)))
          
          (setq count1 (%i- count1 1)
                count2 (%i- count2 1))

          (setq elt1 (funcall key (if vectorp1
                                      (aref seq1 count1)
                                      (prog1
                                        (%car seq1)
                                        (setq seq1 (%cdr seq1)))))
                elt2 (funcall key (if vectorp2
                                      (aref seq2 count2)
                                      (prog1
                                        (%car seq2)
                                        (setq seq2 (%cdr seq2))))))

          (when (if test-not
                    (funcall test elt1 elt2)
                    (not (funcall test elt1 elt2)))
            (return-from mismatch (%i+ count1 1)))))
      ;from-start
      (let* ((count1 start1)
             (count2 start2)
             (elt1)
             (elt2))
        (loop
          (if (or (eq count1 end1)
                  (eq count2 end2))
              (return-from mismatch
                           (if (and (eq count1 end1)
                                    (eq count2 end2))
                               nil
                               count1)))
          (setq elt1 (funcall key (if vectorp1
                                      (aref seq1 count1)
                                      (prog1
                                        (%car seq1)
                                        (setq seq1 (%cdr seq1)))))
                elt2 (funcall key (if vectorp2
                                      (aref seq2 count2)
                                      (prog1
                                        (%car seq2)
                                        (setq seq2 (%cdr seq2))))))
          
          (when (if test-not
                    (funcall test elt1 elt2)
                    (not (funcall test elt1 elt2)))
            (return-from mismatch count1)) 
          (setq count1 (%i+ count1 1)
                count2 (%i+ count2 1))
          
          ))))


;;; Search comparison functions:

(eval-when (:execute :compile-toplevel)
  
  ;;; Compare two elements
  
  (defmacro xcompare-elements (elt1 elt2)
    `(if (not key)
       (if test-not
         (not (funcall test-not ,elt1 ,elt2))
         (funcall test ,elt1 ,elt2))
       (let* ((e1 (funcall key ,elt1))
              (e2 (funcall key ,elt2)))
         (if test-not
           (not (funcall test-not  e1 e2))
           (funcall test e1 e2)))))  
  
  (defmacro vector-vector-search (sub main)
    `(let ((first-elt (aref ,sub start1))
           (last-one nil))
       (do* ((index2 start2 (1+ index2))
             (terminus (%i- end2 (%i- end1 start1))))
            ((> index2 terminus))
         (declare (fixnum index2 terminus))
         (if (xcompare-elements first-elt (aref ,main index2))
           (if (do* ((subi1 (1+ start1)(1+ subi1))
                     (subi2 (1+ index2) (1+ subi2)))
                    ((eq subi1 end1) t)
                 (declare (fixnum subi1 subi2))
                 (if (not (xcompare-elements (aref ,sub subi1) (aref ,main subi2)))
                   (return nil)))
             (if from-end
               (setq last-one index2)
               (return-from search index2)))))
       last-one))

  (defmacro list-list-search (sub main)
    `(let* ((sub-sub (nthcdr start1 ,sub))
            (first-elt (%car sub-sub))
            (last-one nil))
       (do* ((index2 start2 (1+ index2))
             (sub-main (nthcdr start2 ,main) (%cdr sub-main))
             (terminus (%i- end2 (%i- end1 start1))))
            ((> index2 terminus))
         (declare (fixnum index2 terminus))
         (if (xcompare-elements first-elt (car sub-main))
           (if (do* ((ss (%cdr sub-sub) (%cdr ss))
		     (pos (1+ start1) (1+ pos))
                     (sm (%cdr sub-main) (cdr sm)))
                    ((or (null ss) (= pos end1))  t)
		 (declare (fixnum pos))
                 (if (not (xcompare-elements (%car ss) (%car sm)))
                     (return nil)))
              (if from-end
               (setq last-one index2)
               (return-from search index2)))))
       last-one))
  
  (defmacro list-vector-search (sub main)
    `(let* ((sub-sub (nthcdr start1 ,sub))
              (first-elt (%car sub-sub))
              (last-one nil))
         (do* ((index2 start2 (1+ index2))
               (terminus (%i- end2 (%i- end1 start1))))
              ((> index2 terminus))
           (declare (fixnum index2 terminus))
           (if (xcompare-elements first-elt (aref ,main index2))
             (if (do* ((ss (%cdr sub-sub) (%cdr ss))
		       (pos (1+ start1) (1+ pos))
                       (subi2 (1+ index2) (1+ subi2)))
                      ((or (null ss) (= pos end1))  t)
                   (declare (fixnum subi2 pos))
                   (if (not (xcompare-elements (%car ss) (aref ,main subi2)))
                     (return nil)))
               (if from-end
                 (setq last-one index2)
                 (return-from search index2)))))
         last-one))

  (defmacro vector-list-search (sub main)
    `(let ((first-elt (aref ,sub start1))
           (last-one nil))
       (do* ((index2 start2 (1+ index2))
             (sub-main (nthcdr start2 ,main) (%cdr sub-main))
             (terminus (%i- end2 (%i- end1 start1))))
            ((> index2 terminus))
         (declare (fixnum index2 terminus))
         (if (xcompare-elements first-elt (car sub-main))
           (if (do* ((subi1 (1+ start1)(1+ subi1))
                     (sm (%cdr sub-main) (cdr sm)))
                    ((eq subi1 end1) t)
                 (declare (fixnum subi1))
                 (if (not (xcompare-elements (aref ,sub subi1) (car sm)))
                   (return nil)))
             (if from-end
               (setq last-one index2)
               (return-from search index2)))))
       last-one))
                 
    
  )



(defun search (sequence1 sequence2 &key from-end (test #'eql) test-not 
                          (start1 0) end1 (start2 0) end2 (key #'identity))
  (setq end1 (check-sequence-bounds sequence1 start1 end1))
  (setq end2 (check-sequence-bounds sequence2 start2 end2))
  (setq key (adjust-key key))
  (locally (declare (fixnum start1 end1 start2 end2))
    (if (eq 0 (%i- end1 start1))(if from-end end2 start2)
    (seq-dispatch sequence1
                  (seq-dispatch sequence2
                                (list-list-search sequence1 sequence2)
                                (list-vector-search sequence1 sequence2))
                  (seq-dispatch sequence2
                                (vector-list-search sequence1 sequence2)
                                (vector-vector-search sequence1 sequence2))))))

(defun make-string (size &key (initial-element () initial-element-p) (element-type 'character element-type-p))
  "Given a character count and an optional fill character, makes and returns
   a new string COUNT long filled with the fill character."
  (declare (optimize (speed 1) (safety 1)))
  (when (and initial-element-p (not (typep initial-element 'character)))
    (report-bad-arg initial-element 'character))
  (when (and element-type-p
             (not (or (member element-type '(character base-char standard-char))
                      (subtypep element-type 'character))))
    (error ":element-type ~S is not a subtype of CHARACTER" element-type))
  (if initial-element-p
      (make-string size :element-type 'base-char :initial-element initial-element)
      (make-string size :element-type 'base-char)))
