;;;-*-Mode: LISP; Package: CCL -*-
;;;
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

;;; Sort

;;; sorts a sequence destructively using a predicate which must be a
;;;  of two arguments which returns non-() only if the first argument is
;;;  strictly less than the second.  The keyfun (if present) must be a
;;;  function of one argument.  The predicate is applied to keyfun of the
;;;  sequence elements, or directly to the elements if the keyfun is not
;;;  given.

;;; Sort dispatches to type specific sorting routines.

(in-package "CCL")

(defun sort (sequence predicate &key key)
  "Returns SEQUENCE, which has been modified to be in order.
   If sequence is a displaced array, sorts just that portion of the
   data-array which is part of SEQUENCE."
  (if (< (length sequence) 2)
    sequence
    (if (listp sequence)
      (sort-list sequence predicate key)
      (quick-sort-vector sequence predicate key))))

(defun stable-sort (sequence predicate &key key)
  "Returns SEQUENCE, which has been modified to be in order.
   If sequence is a displaced array, sorts just that portion of the
   data-array which is part of SEQUENCE."
  (if (< (length sequence) 2)
    sequence
    (if (listp sequence)
      (sort-list sequence predicate key)
      (merge-sort-vector sequence predicate key))))


;;; Vector sorting.
;;; The quick-sort is a little slower than the merge-sort, but it doesn't cons.
;;; The merge-sort is stable.
;;; Note that there are three version of each:
;;;   AREF for non-simple (eventually) vectors.
;;;   %SVREF with a key.
;;;   %SVREF without a key.
;;; Other than that, the three versions are duplicates.
(defun merge-sort-vector (vector pred key)
  (canonicalize-pred-and-key)
  (let* ((end (length vector)))
    (when (> end 1)
      (multiple-value-bind (real-vector start) 
                           (array-data-and-offset vector)
        (incf end start)
        (unless (fixnump end)
          (error "Sorry, can't sort vectors larger than ~d." most-positive-fixnum))
        (let* ((temp-array (make-array (the fixnum end))))
          (declare (dynamic-extent temp-array))
          (if (simple-vector-p real-vector)
            (if key
              (%merge-sort-simple-vector
               real-vector start end pred key temp-array nil)
              (%merge-sort-simple-vector-no-key
               real-vector start end pred temp-array nil))
            (%merge-sort-vector real-vector start end pred key temp-array nil))))))
  vector)

(defun quick-sort-vector (vector pred key)
  (canonicalize-pred-and-key)
  (let ((end (length vector)))
    (when (> end 1)
      (multiple-value-bind (real-vector start) 
                           (array-data-and-offset vector)
        (incf end (%i- start 1))
; No vector should have a length that's not  a fixnum.
        '(unless (fixnump end)
          (error "Sorry, can't sort vectors larger than ~d." most-positive-fixnum))
        (if (simple-vector-p real-vector)
          (if key
            (%quick-sort-simple-vector real-vector start end pred key)
            (%quick-sort-simple-vector-no-key real-vector start end pred))
          (%quick-sort-vector
           real-vector start end pred (or key #'identity))))))
  vector)

;;; merge-sort internals

(defun %merge-sort-vector (vector start end pred key
                                  temp-vec res-temp?)
  ;; If somebody wanted to do it, half of these arefs can be %svrefs,
  ;; but you'd need two loops in the merge code
  ;; (temp-vec is simple if res-temp? is false).
  ;; But who sorts non-svref'able vectors anyway?
  (let* ((mid (%ilsr 1 (%i+ start end))))
    (if (%i<= (%i- mid 1) start)
      (unless res-temp?
        (setf (aref temp-vec start) (aref vector start)))
      (%merge-sort-vector
       vector start mid pred key temp-vec (not res-temp?)))
    (if (%i>= (%i+ mid 1) end)
      (unless res-temp?
        (setf (aref temp-vec mid) (aref vector mid)))
      (%merge-sort-vector 
       vector mid end pred key temp-vec (not res-temp?)))
    
    (unless res-temp?
      (psetq vector temp-vec temp-vec vector))
    
    (%merge-vectors vector start mid vector mid end temp-vec start pred key)))
    
(defun %merge-sort-simple-vector (vector start end pred key
                                         temp-vec res-temp?)
  (let* ((mid (%ilsr 1 (%i+ start end))))
    (if (%i<= (%i- mid 1) start)
      (unless res-temp?
        (setf (%svref temp-vec start) (%svref vector start)))
      (%merge-sort-simple-vector
       vector start mid pred key temp-vec (not res-temp?)))
    (if (%i>= (%i+ mid 1) end)
      (unless res-temp?
        (setf (%svref temp-vec mid) (%svref vector mid)))
      (%merge-sort-simple-vector 
       vector mid end pred key temp-vec (not res-temp?)))
    
    (unless res-temp?
      (psetq vector temp-vec temp-vec vector))
    
    (%merge-simple-vectors
     vector start mid vector mid end temp-vec start pred key)))

(defun %merge-sort-simple-vector-no-key (vector start end pred
                                                temp-vec res-temp?)
  (let* ((mid (%ilsr 1 (%i+ start end))))
    (if (%i<= (%i- mid 1) start)
      (unless res-temp?
        (setf (%svref temp-vec start) (%svref vector start)))
      (%merge-sort-simple-vector-no-key
       vector start mid pred temp-vec (not res-temp?)))
    (if (%i>= (%i+ mid 1) end)
      (unless res-temp?
        (setf (%svref temp-vec mid) (%svref vector mid)))
      (%merge-sort-simple-vector-no-key
       vector mid end pred temp-vec (not res-temp?)))
    
    (unless res-temp?
      (psetq vector temp-vec temp-vec vector))
    
    (%merge-simple-vectors-no-key
     vector start mid vector mid end temp-vec start pred)))

(defun %merge-vectors (a1 start1 end1 a2 start2 end2
                          out start-out pred key)
  (let* ((i1 start1)
         (i2 start2)
         (i-out start-out)
         v1 v2 k1 k2)
    (cond ((eq start1 end1)
           (when (eq start2 end2)
             (return-from %merge-vectors out))
           (setq i1 start2
                 end1 end2
                 a1 a2
                 v1 (aref a1 i1)))
          ((eq start2 end2)
           (setq i1 start1
                 v1 (aref a1 i1)))
          (t
           (setq v1 (aref a1 i1)
                 v2 (aref a2 i2)
                 k1 (if key (funcall key v1) v1)
                 k2 (if key (funcall key v2) v2))
           (loop (if (funcall pred k2 k1)
                   (progn (setf (aref out i-out) v2
                                i-out (%i+ i-out 1)
                                i2 (%i+ i2 1))
                          (when (eq i2 end2)
                            (return))
                          (setq v2 (aref a2 i2)
                                k2 (if key (funcall key v2) v2)))
                   (progn (setf (aref out i-out) v1
                                i-out (%i+ i-out 1)
                                i1 (%i+ i1 1))
                          (when (eq i1 end1)
                            (setq a1 a2 i1 i2 end1 end2 v1 v2)
                            (return))
                          (setq v1 (aref a1 i1)
                                k1 (if key (funcall key v1) v1)))))))
    (loop
      (setf (aref out i-out) v1
            i1 (%i+ i1 1))
      (if (eq i1 end1) 
        (return out))
      (setq v1 (aref a1 i1)
            i-out (%i+ i-out 1)))))

(defun %merge-simple-vectors (a1 start1 end1 a2 start2 end2
                                 out start-out pred key)
  (let* ((i1 start1)
         (i2 start2)
         (i-out start-out)
         v1 v2 k1 k2)
    (cond ((eq start1 end1)
           (when (eq start2 end2)
             (return-from %merge-simple-vectors out))
           (setq i1 start2
                 end1 end2
                 a1 a2
                 v1 (%svref a1 i1)))
          ((eq start2 end2)
           (setq i1 start1
                 v1 (%svref a1 i1)))
          (t
           (setq v1 (%svref a1 i1)
                 v2 (%svref a2 i2)
                 k1 (if key (funcall key v1) v1)
                 k2 (if key (funcall key v2) v2))
           (loop (if (funcall pred k2 k1)
                   (progn (setf (%svref out i-out) v2
                                i-out (%i+ i-out 1)
                                i2 (%i+ i2 1))
                          (when (eq i2 end2)
                            (return))
                          (setq v2 (%svref a2 i2)
                                k2 (funcall key v2)))
                   (progn (setf (%svref out i-out) v1
                                i-out (%i+ i-out 1)
                                i1 (%i+ i1 1))
                          (when (eq i1 end1)
                            (setq a1 a2 i1 i2 end1 end2 v1 v2)
                            (return))
                          (setq v1 (%svref a1 i1)
                                k1 (funcall key v1)))))))
    (loop
      (setf (%svref out i-out) v1
            i1 (%i+ i1 1))
      (if (eq i1 end1) 
        (return out))
      (setq v1 (%svref a1 i1)
            i-out (%i+ i-out 1)))))

(defun %merge-simple-vectors-no-key (a1 start1 end1 a2 start2 end2
                                        out start-out pred)
  (let* ((i1 start1)
         (i2 start2)
         (i-out start-out)
         v1 v2)
    (cond ((eq start1 end1)
           (when (eq start2 end2)
             (return-from %merge-simple-vectors-no-key out))
           (setq i1 start2
                 end1 end2
                 a1 a2
                 v1 (%svref a1 i1)))
          ((eq start2 end2)
           (setq i1 start1
                 v1 (%svref a1 i1)))
          (t
           (setq v1 (%svref a1 i1)
                 v2 (%svref a2 i2))
           (loop (if (funcall pred v2 v1)
                   (progn (setf (%svref out i-out) v2
                                i-out (%i+ i-out 1)
                                i2 (%i+ i2 1))
                          (when (eq i2 end2)
                            (return))
                          (setq v2 (%svref a2 i2)))
                   (progn (setf (%svref out i-out) v1
                                i-out (%i+ i-out 1)
                                i1 (%i+ i1 1))
                          (when (eq i1 end1)
                            (setq a1 a2 i1 i2 end1 end2 v1 v2)
                            (return))
                          (setq v1 (%svref a1 i1)))))))
    (loop
      (setf (%svref out i-out) v1
            i1 (%i+ i1 1))
      (if (eq i1 end1) 
        (return out))
      (setq v1 (%svref a1 i1)
            i-out (%i+ i-out 1)))))


;;; Quick sort internals
(defun %quick-sort-vector (vector start end pred key)
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum start end))
  (if (< start end)
    (let* ((p (the fixnum (+ start (the fixnum (ash (the fixnum (- end start)) -1)))))
           (Ai (aref vector p))
           (x (funcall key Ai))
           (pivot Ai)
           (i start)
           (j (the fixnum (1+ end)))
           Aj)
      (declare (fixnum p i j))
      (setf (aref vector p) (aref vector start)
            (aref vector start) Ai)
      (block partition
        (loop
          (loop (unless (> (decf j) i) (return-from partition))
                (unless (funcall pred
                                 x
                                 (funcall key (setq Aj (aref vector j))))
                  (return)))
          (loop (unless (< (incf i) j) (return-from partition))
                (unless (funcall pred
                                 (funcall key (setq Ai (aref vector i)))
                                 x)
                  (return)))
          (setf (aref vector i) Aj
                (aref vector j) Ai)))
      (setf (aref vector start) (aref vector j)
            (aref vector j) pivot)
      ; This compare is important.  It limits stack depth to log(end-start)
      (if (< (the fixnum (- j start)) (the fixnum (- end j)))
        (progn
          (%quick-sort-vector vector start (the fixnum (1- j)) pred key)
          (%quick-sort-vector vector (the fixnum (1+ j)) end pred key))
        (progn
          (%quick-sort-vector vector (the fixnum (1+ j)) end pred key)
          (%quick-sort-vector vector start (the fixnum (1- j)) pred key))))
    vector))

(defun %quick-sort-simple-vector (vector start end pred key)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type simple-vector vector)
           (fixnum start end))
  (if (< start end)
    (let* ((p (the fixnum (+ start (the fixnum (ash (the fixnum (- end start)) -1)))))
           (Ai (svref vector p))
           (pivot Ai)
           (x (funcall key Ai))
           (i start)
           (j (the fixnum (1+ end)))
           Aj)
      (declare (fixnum p i j))
      (setf (svref vector p) (svref vector start)
            (svref vector start) Ai)
      (block partition
        (loop
          (loop (unless (> (decf j) i) (return-from partition))
                (unless (funcall pred
                                 x
                                 (funcall key (setq Aj (svref vector j))))
                  (return)))
          (loop (unless (< (incf i) j) (return-from partition))
                (unless (funcall pred
                                 (funcall key (setq Ai (svref vector i)))
                                 x)
                  (return)))
          (setf (aref vector i) Aj
                (aref vector j) Ai)))
      (setf (svref vector start) (svref vector j)
            (svref vector j) pivot)
      (if (< (the fixnum (- j start)) (the fixnum (- end j)))
        (progn
          (%quick-sort-simple-vector vector start (the fixnum (1- j)) pred key)
          (%quick-sort-simple-vector vector (the fixnum (1+ j)) end pred key))
        (progn
          (%quick-sort-simple-vector vector (the fixnum (1+ j)) end pred key)
          (%quick-sort-simple-vector vector start (the fixnum (1- j)) pred key))))
    vector))

(defun %quick-sort-simple-vector-no-key (vector start end pred)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type simple-vector vector)
           (fixnum start end))
  (if (< start end)
    (let* ((p (the fixnum (+ start (the fixnum (ash (the fixnum (- end start)) -1)))))
           (x (svref vector p))
           (i start)
           (j (the fixnum (1+ end)))
           Ai Aj)
      (declare (fixnum p i j))
      (setf (svref vector p) (svref vector start)
            (svref vector start) x)
      (block partition
        (loop
          (loop (unless (> (decf j) i) (return-from partition))
                (unless (funcall pred
                                 x
                                 (setq Aj (svref vector j)))
                  (return)))
          (loop (unless (< (incf i) j) (return-from partition))
                (unless (funcall pred
                                 (setq Ai (svref vector i))
                                 x)
                  (return)))
          (setf (aref vector i) Aj
                (aref vector j) Ai)))
      (setf (svref vector start) (svref vector j)
            (svref vector j) x)
      (if (< (the fixnum (- j start)) (the fixnum (- end j)))
        (progn
          (%quick-sort-simple-vector-no-key vector start (the fixnum (1- j)) pred)
          (%quick-sort-simple-vector-no-key vector (the fixnum (1+ j)) end pred))
        (progn
          (%quick-sort-simple-vector-no-key vector (the fixnum (1+ j)) end pred)
          (%quick-sort-simple-vector-no-key vector start (the fixnum (1- j)) pred))))
    vector))



;; This conses like crazy if you merge lists into vectors or vice-versa, but
;; I don't want to write 6 more merging routines.  Fry's coerce's
;; will have to stand for now.
;; Only difficulty here is parsing the result-type for vectors.
(defun merge (result-type sequence1 sequence2 predicate &key key)
  "Merge the sequences SEQUENCE1 and SEQUENCE2 destructively into a
   sequence of type RESULT-TYPE using PREDICATE to order the elements.
   If result-type specifies an array, the returned array will not be
   a complex array. Usually, result-type is either LIST, ARRAY or STRING."
  (let* ((result-len (+ (length sequence1) (length sequence2)))
         (result-ctype (specifier-type result-type)))
    (cond ((csubtypep result-ctype (specifier-type 'null))
           (unless (zerop result-len)
             (error 'invalid-subtype-error :datum result-type
                    :expected-type 'cons)))
          ((csubtypep result-ctype (specifier-type 'list))
           (canonicalize-pred-and-key predicate key)
           (values                      ; For the terminally pedantic.
            (merge-lists* (if (listp sequence1)
                            sequence1
                            (coerce sequence1 'list))
                          (if (listp sequence2)
                            sequence2
                            (coerce sequence2 'list))
                          predicate key)))
          ((csubtypep result-ctype (specifier-type 'vector))
           (merge-vectors (if (listp sequence1)
                            (coerce sequence1 'vector)
                            sequence1)
                          (if (listp sequence2)
                            (coerce sequence2 'vector)
                            sequence2)
                          predicate key
                          result-type))
          (t (error 'invalid-subtype-error
                    :datum result-type
                    :expected-type 'sequence)))))

(defun merge-vectors (vector-1 vector-2 pred key 
                               &optional (result-type 'vector))
  "Internal function.  Use MERGE instead."
  (canonicalize-pred-and-key)
  (let* ((length-1 (length vector-1))
         (length-2 (length vector-2))
         (result-length (+ length-1 length-2))
         (result (make-merge-vectors-result
                  result-type result-length vector-1 vector-2))
         real-vector-1 start-1 real-vector-2 start-2)
    (multiple-value-setq (real-vector-1 start-1)
                         (array-data-and-offset vector-1))
    (multiple-value-setq (real-vector-2 start-2)
                         (array-data-and-offset vector-2))
    (incf length-1 start-1)
    (incf length-2 start-2)
    (if (and (simple-vector-p real-vector-1) (simple-vector-p real-vector-2)
             (simple-vector-p result))
      (if key
        (%merge-simple-vectors real-vector-1 start-1 length-1
                               real-vector-2 start-2 length-2
                               result 0 pred key)
        (%merge-simple-vectors-no-key real-vector-1 start-1 length-1
                                      real-vector-2 start-2 length-2
                                      result 0 pred))
      (%merge-vectors real-vector-1 start-1 length-1
                      real-vector-2 start-2 length-2
                      result 0 pred key))))

;; OK, here goes the type parsing...
(defun make-merge-vectors-result (result-type result-length vector-1 vector-2)
  (let* ((ctype (specifier-type result-type)))
    (let* ((size (array-ctype-length ctype))
           (elt-type (array-or-union-ctype-element-type ctype)))
      (if (eq elt-type '*)
        (let ((et1 (array-element-type vector-1))
              (et2 (array-element-type vector-2)))
          (setq elt-type (if (eq et1 et2) et1 `(or ,et1 ,et2)))))
      (if (and size (not (eq size result-length)))
        (error 'invalid-subtype-error
               :datum result-type
               :expected-type `(vector ,elt-type ,result-length))
        (make-array (the fixnum (or size result-length))
                    :element-type elt-type)))))
        

;; Gee, that wasn't so bad after all.
;; Well, when you're building on the shoulders of giants,
;; your little effort can seem great.


;; "If I haven't seen as far as others, it's because giants were standing on my shoulders."
