;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2005 Clozure Associates
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

(in-package "CL-USER")

;;; A sample HONS (hash-consing) implementation, based on the
;;; primitives defined in "ccl:library;hash-cons.lisp".

(defun largest-prime-less-than-or-equal-to (n)
  (flet ((primep (n)
           (or (eql n 1)
               (eql n 2)
               (and (oddp n)
                    (let* ((max (isqrt n)))
                      (do* ((i 3 (+ i 2)))
                           ((> i max) t)
                        (when (zerop (mod n i))
                          (return nil))))))))
    (if (primep n)
      n
      (do* ((m (if (oddp n) (- n 2) (- n 1)) (- m 2)))
           ((primep m) m)))))


;;; A "hons-table" just represents a range of indices in a
;;; static memory region dedicated to hash-consing (and
;;; a little information about the contents of that region.)
(defstruct (hons-table (:constructor %make-hons-table))
  start-index                           ;lower inclusive index
  end-index                             ;upper exclusive index
  size                                  ;(<= size (- end-index start-index))
  max                                   ;maximum "full" point
  (used 0)                              ;current number of used pairs
  )

(defmethod print-object ((ht hons-table) stream)
  (print-unreadable-object (ht stream :type t :identity t)
    (format stream "indices ~d-~d, used ~d/~d"
            (hons-table-start-index ht)
            (hons-table-end-index ht)
            (hons-table-used ht)
            (hons-table-max ht))))

;;; The "active" HONS table is the CAR of this list.
(defparameter *all-hons-tables* ()
  "A list of all hons tables, maintained in reverse order of creation (e.g., the CAR of this list is the most recently created.)")

(defparameter *hons-table-max-full-ratio* .85
  "Controls how full a hons table can get.")

;;; Try to allocate a new HONS table, which describes a newly
;;; allocated range of indices in HONS space.  If successful,
;;; that new table gets pushed onto the front of *ALL-HONS-TABLES*
;;; and returned.
(defun make-hons-table (size &optional (max-full-ratio
                                        *hons-table-max-full-ratio*))
  (check-type size (and fixnum unsigned-byte))
  (setq size (largest-prime-less-than-or-equal-to size))
  (let* ((current (openmcl-hons:hons-space-size))
         (new (setf (openmcl-hons:hons-space-size)
                    (+ current (the fixnum size)))))
    (declare (fixnum current new))
    (if (>= (- new current) size)
      (let* ((table (%make-hons-table :start-index current
                                      :end-index new
                                      :size size
                                      :max (floor (* size max-full-ratio)))))
        (push table *all-hons-tables*)
        table)
      ;; As of 12/30/05, there's a slight possibility that
      ;; #'(setf opencl-hons:hons-space-size) can fail
      ;; even though address-space/memory are available.
      ;; (The problem has to do with the way that CCL:WITHOUT-GCING
      ;; works; if the GC is disabled, we can't move things around,
      ;; but there isn't currently an easy way to detect that.)
      (error "Couldn't increase hons space size by ~d pairs" size))))

(defun hons-hash-string (s)
  (let* ((h 0))
    (declare (fixnum h))
    (dotimes (i (length s) (logand h most-positive-fixnum))
      (setq h (+ (the fixnum (* 4999 h)) (the fixnum (ccl::%scharcode s i)))))))

;;; Exactly what types of objects can go in the CAR or CDR of
;;; a HONS table is application dependent, but it's reasonable
;;; to insist that all CONSes are HONSes.


(defun hash-pair-for-honsing (car cdr)
  ;; This often calls CCL::%%EQLHASH, which is (as one might
  ;; assume) a primitive used with EQL hash tables.  It tries
  ;; to "scramble the bits" a little, so that "related" keys
  ;; (like numerically adjacent integers) hash to unrelated
  ;; values.
  (flet ((hash-for-honsing (thing)
           (logand
            (the fixnum
              (etypecase thing
                (cons (let* ((idx (openmcl-hons::honsp thing)))
                        (if idx
                          (ccl::%%eqlhash idx)
                          (error "~s is not HONSP." thing))))
                (fixnum (ccl::%%eqlhash thing))
                ((or bignum single-float double-float)
                 (ccl::%%eqlhash thing))
                (null target::nil-value)
                (symbol (hons-hash-string (symbol-name thing)))
                (simple-string (hons-hash-string thing))
                ((complex rational) (ccl::%%eqlhash thing))))
            most-positive-fixnum)))
     (the fixnum
       (+ (the fixnum (* 37 (the fixnum (hash-for-honsing car))))
          (the fixnum (* 33 (the fixnum (hash-for-honsing cdr))))))))

(defparameter *hons-probes* 0)
(defparameter *hons-secondary-probes* 0)


(defun hons-table-get (ht hash car cdr)
  "Tries to find a HONS with matching (EQL) CAR and CDR in the hash table HT.
Returns a CONS if a match is found, a fixnum index otherwise."
  (declare (fixnum hash) (optimize (speed 3)))
  (incf *hons-probes*)
  (do* ((size (hons-table-size ht))
        (start (hons-table-start-index ht))
        (end (+ start size))
        (idx (+ start (the fixnum (ccl::fast-mod hash size))) (+ idx 1))
        (first-deleted-index nil))
       ()
    (declare (fixnum start end size idx))
    (if (>= idx end)
      (decf idx size))
    (let* ((hcar (openmcl-hons:hons-space-ref-car idx))
           (hcdr (openmcl-hons:hons-space-ref-cdr idx)))
      (cond ((and  (eql hcar car) (eql hcdr cdr))
             (return (openmcl-hons:hons-from-index idx)))
            (t
             (if (eq hcar (openmcl-hons:hons-space-deleted-marker))
               (unless first-deleted-index
                 (setq first-deleted-index idx))
               (if (eq hcar (openmcl-hons:hons-space-free-marker))
                 (return (or first-deleted-index idx))))))
      (incf *hons-secondary-probes*))))


;;; These values are entirely arbitrary.

(defparameter *initial-hons-table-size* (ash 100 20)
  "The number of pairs to allocate in the initially allocated hons table.")

(defparameter *secondary-hons-table-size* (ash 25 20)
  "The number of pairs to allocate in subsequently allocated hons tables.")

;;; Find HONS (a statically allocated CONS cell) with matching CAR and
;;; CDR, or create a new one.
(defun hons (car cdr)
  (let* ((tables *all-hons-tables*)
         (active-table (if tables
                         (car tables)
                         (make-hons-table *initial-hons-table-size*)))
         (hash (hash-pair-for-honsing car cdr))
         (h (hons-table-get active-table hash car cdr)))
    (declare (fixnum hash))
    (cond ((consp h) h)
          ((< (hons-table-used active-table)
              (hons-table-max active-table))
           (incf (hons-table-used active-table))
           (openmcl-hons:hons-space-cons h car cdr))
          (t (error "Active hons table is full.")))))



;;; Some utilities.

(defun discard-active-hons-table ()
  (let* ((table (pop *all-hons-tables*)))
    (when table
      (setf (openmcl-hons:hons-space-size) (hons-table-start-index table)
            (hons-table-start-index table) nil)
      t)))
      
           
(defun discard-all-hons-tables ()
  (dolist (table *all-hons-tables*)
    ;; Invalidate the table.
    (setf (hons-table-start-index table) nil))
  (setq *all-hons-tables* nil)
  (setf (openmcl-hons:hons-space-size) 0)
  t)

#||

(defvar *test-var*)

(defun test (n)
  (setq *test-var* nil)
  (loop for i from 1 to n do
        (print i)
        (loop for i from 1 to 1000000 do
              (setq *test-var* (hons i *test-var*)))))


||#




               
