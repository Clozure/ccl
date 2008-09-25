;;;-*- Mode: Lisp; Package: CCL -*-
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

;;;;;;;;;;;;;
;;
;; See hash.lisp for documentation


(eval-when (:compile-toplevel :execute)
  (require "HASHENV" "ccl:xdump;hashenv")
  (require :number-case-macro)
  (define-symbol-macro deleted-hash-key-marker (%slot-unbound-marker))
  (define-symbol-macro free-hash-marker (%unbound-marker))
  (define-symbol-macro rehashing-value-marker (%slot-unbound-marker))
  (declaim (inline nhash.vector-size))
  (declaim (inline mixup-hash-code))
  (declaim (inline hash-table-p))
  (declaim (inline %%eqhash))
  (declaim (inline index->vector-index vector-index->index swap))
  (declaim (inline %already-rehashed-p %set-already-rehashed-p))
  (declaim (inline need-use-eql))
  (declaim (inline %needs-rehashing-p))
  (declaim (inline compute-hash-code))
  (declaim (inline eq-hash-find eq-hash-find-for-put))
  (declaim (inline read-lock-hash-table write-lock-hash-table  unlock-hash-table))
  (declaim (inline %hash-symbol))
  (declaim (inline hash-mod))
  (declaim (inline set-hash-key-conditional set-hash-value-conditional))
  (declaim (inline hash-lock-free-p lock-free-gethash)))



(defun %cons-hash-table (keytrans-function compare-function vector
                         threshold rehash-ratio rehash-size find find-new owner &optional lock-free-p)
  (%istruct
   'HASH-TABLE                          ; type
   keytrans-function                    ; nhash.keytransF
   compare-function                     ; nhash.compareF
   nil                                  ; nhash.rehash-bits
   vector                               ; nhash.vector
   (if lock-free-p $nhash.lock-free 0)  ; nhash.lock
   owner                                ; nhash.owner 
   threshold                            ; nhash.grow-threshold
   rehash-ratio                         ; nhash.rehash-ratio
   rehash-size                          ; nhash.rehash-size
   0                                    ; nhash.puthash-count
   (if lock-free-p
     (make-lock)
     (unless owner (make-read-write-lock))) ; nhash.exclusion-lock
   find                                 ; nhash.find
   find-new                             ; nhash.find-new
   nil                                  ; nhash.read-only
   ))

(defun nhash.vector-size (vector)
  (nhash.vector.size vector))

(defun hash-mod (hash entries vector)
  (fast-mod-3 hash entries (nhash.vector.size-reciprocal vector)))

;; For lock-free hash tables
(defun set-hash-key-conditional (index vector old new)
  (%set-hash-table-vector-key-conditional (%i+ target::misc-data-offset
                                               (ash (the fixnum index) target::word-shift))
                                          vector
                                          old
                                          new))

(defun set-hash-value-conditional (index vector old new)
  (store-gvector-conditional (%i+ index 1) vector old new))

(defun hash-lock-free-p (hash)
  (logtest $nhash.lock-free (the fixnum (nhash.lock hash))))
 
;;; Is KEY something which can be EQL to something it's not EQ to ?
;;; (e.g., is it a number or macptr ?)
;;; This can be more general than necessary but shouldn't be less so.
(defun need-use-eql (key)
  (let* ((typecode (typecode key)))
    (declare (fixnum typecode))
    (or (= typecode target::subtag-macptr)
        #+(or ppc32-target x8632-target)
        (and (>= typecode target::min-numeric-subtag)
             (<= typecode target::max-numeric-subtag))
        #+64-bit-target
        (or (= typecode target::subtag-bignum)
            (= typecode target::subtag-double-float)
            (= typecode target::subtag-ratio)
            (= typecode target::subtag-complex)))))

;;; Don't rehash at all, unless some key is address-based (directly or
;;; indirectly.)
(defun %needs-rehashing-p (vector)
  (let* ((flags (nhash.vector.flags vector)))
    (declare (fixnum flags))
    (if (logbitp $nhash_track_keys_bit flags)
      ;; GC is tracking key movement
      (logbitp $nhash_key_moved_bit flags)
      ;; GC is not tracking key movement
      (if (logbitp $nhash_component_address_bit flags)
        (not (eql (the fixnum (%get-gc-count)) (the fixnum (nhash.vector.gc-count vector))))))))

(defun %set-does-not-need-rehashing (hash)
  (let* ((vector (nhash.vector hash))
         (flags (nhash.vector.flags vector)))
    (declare (fixnum flags))
    (setf (nhash.vector.gc-count vector) (%get-gc-count))
    (when (logbitp $nhash_track_keys_bit flags)
      (setf (nhash.vector.flags vector)
            (logand (lognot (ash 1 $nhash_key_moved_bit)) flags)))))


;;; Tempting though it may be to remove this, a hash table loaded from
;;; a fasl file likely needs to be rehashed, and the MAKE-LOAD-FORM
;;; for hash tables needs to be able to call this or something similar.
(defun %set-needs-rehashing (hash)
  (let* ((vector (nhash.vector hash))
         (flags (nhash.vector.flags vector)))
    (declare (fixnum flags))
    (setf (nhash.vector.gc-count vector) (the fixnum (1- (the fixnum (%get-gc-count)))))
    (when (logbitp $nhash_track_keys_bit flags)
      (setf (nhash.vector.flags vector) (logior (ash 1 $nhash_key_moved_bit) flags)))))

#+32-bit-target
(defun mixup-hash-code (fixnum)
  (declare (fixnum fixnum))
  (the fixnum
    (+ fixnum
       (the fixnum (%ilsl (- 32 8)
                          (logand (1- (ash 1 (- 8 3))) fixnum))))))

#+64-bit-target
(defun mixup-hash-code (fixnum)
  (declare (fixnum fixnum))
  (the fixnum
    (+ fixnum
       (the fixnum (%ilsl 50
                          (logand (1- (ash 1 (- 8 3))) fixnum))))))


(defun rotate-hash-code (fixnum)
  (declare (fixnum fixnum))
  (let* ((low-3 (logand 7 fixnum))
         (but-low-3 (%ilsr 3 fixnum))
         (low-3*64K (%ilsl 13 low-3))
         (low-3-in-high-3 (%ilsl (- 32 3 3) low-3)))
    (declare (fixnum low-3 but-low-3 low-3*64K low-3-in-high-3))
    (the fixnum (+ low-3-in-high-3
                   (the fixnum (logxor low-3*64K but-low-3))))))




(defconstant $nhash-track-keys-mask
  #.(- (ash 1 $nhash_track_keys_bit)))

(defconstant $nhash-clear-key-bits-mask #xfffff)


(defun %hash-symbol (sym)
  (if sym    
    (let* ((vector (%symptr->symvector sym))
           (cell (%svref vector target::symbol.plist-cell)))
      (or (car cell)
          (let* ((pname (%svref vector target::symbol.pname-cell))
                 (hash (mixup-hash-code (%pname-hash pname (uvsize pname)))))
            (declare (type (simple-string pname)))
            (if cell
              (setf (car cell) hash)
              (progn
                (setf (%svref vector target::symbol.plist-cell)
                      (cons hash nil))
                hash)))))
    +nil-hash+))
              
;;; Hash on address, or at least on some persistent, immutable
;;; attribute of the key.  If all keys are fixnums or immediates (or if
;;; that attribute exists), rehashing won't ever be necessary.
(defun %%eqhash (key)
  (let* ((typecode (typecode key)))
    (if (eq typecode target::tag-fixnum)
      (values (mixup-hash-code key) nil)
      (if (eq typecode target::subtag-instance)
        (values (mixup-hash-code (instance.hash key)) nil)
        (if (symbolp key)
          (values (%hash-symbol key) nil)
          (let ((hash (mixup-hash-code (strip-tag-to-fixnum key))))
            (if (immediate-p-macro key)
              (values hash nil)
              (values hash :key ))))))))


#+32-bit-target
(defun swap (num)
  (declare (fixnum num))
  (the fixnum (+ (the fixnum (%ilsl 16 num))(the fixnum (%ilsr 13 num)))))

#+64-bit-target
(defun swap (num)
  (declare (fixnum num))
  (the fixnum (+ (the fixnum (%ilsl 32 num))(the fixnum (%ilsr 29 num)))))

;;; teeny bit faster when nothing to do
(defun %%eqlhash-internal (key)
  (number-case key
    (fixnum (mixup-hash-code key)) ; added this 
    (double-float (%dfloat-hash key))
    (short-float (%sfloat-hash key))
    (bignum (%bignum-hash key))
    (ratio (logxor (swap (%%eqlhash-internal (numerator key)))
                   (%%eqlhash-internal (denominator key))))
    (complex
     (logxor (swap (%%eqlhash-internal (realpart key)))
             (%%eqlhash-internal (imagpart key))))
    (t (cond ((macptrp key)
              (%macptr-hash key))
             (t key)))))

               


;;; new function

(defun %%eqlhash (key)
  ;; if key is a macptr, float, bignum, ratio, or complex, convert it
  ;; to a fixnum
  (if (hashed-by-identity key)
    (%%eqhash key)
    (let ((primary  (%%eqlhash-internal key)))
      (if (eq primary key)
        (%%eqhash key)
        (mixup-hash-code (strip-tag-to-fixnum primary))))))


(defun %%equalhash (key)
  (let* ((id-p (hashed-by-identity key))
         (hash (if (and key (not id-p)) (%%eqlhash-internal key)))
         addressp)
    (cond ((null key) (mixup-hash-code 17))
          #+64-bit-target
          ((and (typep key 'single-float)
                (zerop (the single-float key)))
           0)
          ((immediate-p-macro key) (mixup-hash-code (strip-tag-to-fixnum key)))
          ((and hash (neq hash key)) hash)  ; eql stuff
          (t (typecase key
                (simple-string (%pname-hash key (length key)))
                (string
                 (let ((length (length key)))
                   (multiple-value-bind (data offset) (array-data-and-offset key)
                     (%string-hash offset data length))))
                (bit-vector (bit-vector-hash key))
                (cons
                 (let ((hash 0))
                   (do* ((i 0 (1+ i))
                         (list key (cdr list)))
                        ((or (not (consp list)) (> i 11))) ; who figured 11?
                     (declare (fixnum i))
                     (multiple-value-bind (h1 a1) (%%equalhash (%car list))
                       (when a1 (setq addressp t))
                       ; fix the case of lists of same stuff in different order
                       ;(setq hash (%ilogxor (fixnum-rotate h1 i) hash))
                       (setq hash (%i+ (rotate-hash-code hash) h1))
                       ))
                   (values hash addressp)))
                (pathname (%%equalphash key))
                (t (%%eqlhash key)))))))

(defun update-hash-flags (hash vector addressp)
  (when addressp
    (flet ((new-flags (flags addressp)
             (declare (fixnum flags))
             (if (eq :key addressp)
               ;; hash code depended on key's address
               (if (logbitp $nhash_component_address_bit flags)
                 flags
                 (logior $nhash-track-keys-mask
                         (if (logbitp $nhash_track_keys_bit flags)
                           flags
                           (bitclr $nhash_key_moved_bit flags))))
               ;; hash code depended on component address
               (bitset $nhash_component_address_bit
                       (logand (lognot $nhash-track-keys-mask) flags)))))
      (declare (inline new-flags))
      (if (hash-lock-free-p hash)
        (loop
          (let* ((flags (nhash.vector.flags vector))
                 (new-flags (new-flags flags addressp)))
            (when (or (eq flags new-flags)
                      (store-gvector-conditional nhash.vector.flags vector flags new-flags))
              (return))))
        (setf (nhash.vector.flags vector)
              (new-flags (nhash.vector.flags vector) addressp))))))

(defun compute-hash-code (hash key update-hash-flags &optional
                               (vector (nhash.vector hash))) ; vectorp))
  (let ((keytransF (nhash.keytransF hash))
        primary addressp)
    (if (not (fixnump keytransF))
      ;; not EQ or EQL hash table
      (progn
        (multiple-value-setq (primary addressp) (funcall keytransF key))
        (let ((immediate-p (immediate-p-macro primary)))
          (setq primary (strip-tag-to-fixnum primary))
          (unless immediate-p
            (setq primary (mixup-hash-code primary))
            (setq addressp :key))))
      ;; EQ or EQL hash table
      (if (and (not (eql keytransF 0))
	       (need-use-eql key))
	;; EQL hash table
	(setq primary (%%eqlhash-internal key))
	;; EQ hash table - or something eql doesn't do
	(multiple-value-setq (primary addressp) (%%eqhash key))))
    (when update-hash-flags
      (when addressp
        (update-hash-flags hash vector addressp)))
    (let* ((entries (nhash.vector-size vector)))
      (declare (fixnum entries))
      (values primary
              (hash-mod primary entries vector)
              entries))))

(defun %already-rehashed-p (primary rehash-bits)
  (declare (optimize (speed 3)(safety 0)))
  (declare (type (simple-array bit (*)) rehash-bits))
  (eql 1 (sbit rehash-bits primary)))

(defun %set-already-rehashed-p (primary rehash-bits)
  (declare (optimize (speed 3)(safety 0)))
  (declare (type (simple-array bit (*)) rehash-bits))
  (setf (sbit rehash-bits primary) 1))


(defun hash-table-p (hash)
  (istruct-typep hash 'hash-table))

(defun %normalize-hash-table-count (hash)
  (let* ((vector (nhash.vector hash))
	 (weak-deletions-count (nhash.vector.weak-deletions-count vector)))
    (declare (fixnum weak-deletions-count))
    (unless (eql 0 weak-deletions-count)
      (setf (nhash.vector.weak-deletions-count vector) 0)
      ;; lock-free hash tables don't maintain deleted-count, since would need to
      ;; lock and it's not worth it.
      (unless (hash-lock-free-p hash)
	(let ((deleted-count (the fixnum
			       (+ (the fixnum (nhash.vector.deleted-count vector))
				  weak-deletions-count)))
	      (count (the fixnum (- (the fixnum (nhash.vector.count vector)) weak-deletions-count))))
          (setf (nhash.vector.deleted-count vector) deleted-count
                (nhash.vector.count vector) count))))))


(defparameter *shared-hash-table-default* t
  "Be sure that you understand the implications of changing this
before doing so.")

(defparameter *lock-free-hash-table-default* :shared
  "If NIL, hash tables default to using the standard algorithms, with locks for shared tables.
   If :SHARED, shared hash tables default to using the \"lock-free\" algorithm,
   which is faster for typical access but slower for rehashing or growing the table.
   Otherwise, all hash tables default to the lock-free algorithm")

(defun make-hash-table (&key (test 'eql)
                             (size 60)
                             (rehash-size 1.5)
                             (rehash-threshold .85)
                             (hash-function nil)
                             (weak nil)
                             (finalizeable nil)
                             (address-based t)  ;; Ignored
                             (lock-free *lock-free-hash-table-default*)
                             (shared *shared-hash-table-default*))
  "Create and return a new hash table. The keywords are as follows:
     :TEST -- Indicates what kind of test to use.
     :SIZE -- A hint as to how many elements will be put in this hash
       table.
     :REHASH-SIZE -- Indicates how to expand the table when it fills up.
       If an integer, add space for that many elements. If a floating
       point number (which must be greater than 1.0), multiply the size
       by that amount.
     :REHASH-THRESHOLD -- Indicates how dense the table can become before
       forcing a rehash. Can be any positive number <=1, with density
       approaching zero as the threshold approaches 0. Density 1 means an
       average of one entry per bucket."
  (declare (ignore address-based)) ;; TODO: could reinterpret as "warn if becomes address-based"
  (unless (and test (or (functionp test) (symbolp test)))
    (report-bad-arg test '(and (not null) (or symbol function))))
  (unless (or (functionp hash-function) (symbolp hash-function))
    (report-bad-arg hash-function '(or symbol function)))
  (unless (and (realp rehash-threshold) (<= 0.0 rehash-threshold) (<= rehash-threshold 1.0))
    (report-bad-arg rehash-threshold '(real 0 1)))
  (unless (or (fixnump rehash-size) (and (realp rehash-size) (< 1.0 rehash-size)))
    (report-bad-arg rehash-size '(or fixnum (real 1 *))))
  (unless (fixnump size) (report-bad-arg size 'fixnum))
  (when (and (eq lock-free :shared) (not shared))
    (setq lock-free nil))
  (setq rehash-threshold (/ 1.0 (max 0.01 rehash-threshold)))
  (let* ((default-hash-function
             (cond ((or (eq test 'eq) (eq test #'eq)) 
                    (setq test 0))
                   ((or (eq test 'eql) (eq test #'eql)) 
                    (setq test -1))
                   ((or (eq test 'equal) (eq test #'equal))
                    (setq test #'equal) #'%%equalhash)
                   ((or (eq test 'equalp) (eq test #'equalp))
                    (setq test #'equalp) #'%%equalphash)
                   (t (setq test (require-type test 'symbol))
                      (or hash-function 
                          (error "non-standard test specified without hash-function")))))
         (find-function
          (case test
            (0 #'eq-hash-find)
            (-1 #'eql-hash-find)
            (t #'general-hash-find)))
         (find-put-function
          (case test
            (0 #'eq-hash-find-for-put)
            (-1 #'eql-hash-find-for-put)
            (t #'general-hash-find-for-put))))
    (setq hash-function
          (if hash-function
            (require-type hash-function 'symbol)
            default-hash-function))
    (when (and weak (neq weak :value) (neq test 0))
      (error "Only EQ hash tables can be weak."))
    (when (and finalizeable (not weak))
      (error "Only weak hash tables can be finalizeable."))
    (when (and (eq lock-free :shared) (not shared))
      (setq lock-free nil))
    (multiple-value-bind (grow-threshold total-size)
        (compute-hash-size (1- size) 1 rehash-threshold)
      (let* ((flags (+ (if weak (ash 1 $nhash_weak_bit) 0)
                       (ecase weak
                         ((t nil :key) 0)
                         (:value (ash 1 $nhash_weak_value_bit)))
                       (if finalizeable (ash 1 $nhash_finalizeable_bit) 0)
                       (if lock-free (ash 1 $nhash_keys_frozen_bit) 0)))
             (hash (%cons-hash-table 
                    hash-function test
                    (%cons-nhash-vector total-size flags)
                    grow-threshold rehash-threshold rehash-size
                    find-function find-put-function
                    (unless shared *current-process*)
                    lock-free)))
        (setf (nhash.vector.hash (nhash.vector hash)) hash)
        hash))))

(defun compute-hash-size (size rehash-size rehash-ratio)
  (let* ((new-size size))
    (declare (fixnum size new-size))
    (setq new-size (max 30 (if (fixnump rehash-size)
                             (%i+ size rehash-size)
                             (ceiling (* size rehash-size)))))
    (if (<= new-size size)
      (setq new-size (1+ size)))        ; God save you if you make this happen
    
    (let ((vector-size (%hash-size (max (+ new-size 2) (ceiling (* new-size rehash-ratio))))))
      ; TODO: perhaps allow more entries, based on actual size:
      ;  (values (min (floor vector-size rehash-ratio) (%i- vector-size 2)) vector-size))
      (values new-size vector-size)
      )))

;;;  Suggested size is a fixnum: number of pairs.  Return a fixnum >=
;;;  that size that is relatively prime to all secondary keys.
(defun %hash-size (suggestion)
  (declare (fixnum suggestion))
  (declare (optimize (speed 3)(safety 0)))
  (if (<= suggestion #.(aref secondary-keys 7))
    (setq suggestion (+ 2 #.(aref secondary-keys 7)))
     (setq suggestion (logior 1 suggestion)))
  (loop
    (dovector (key secondary-keys (return-from %hash-size suggestion))
      (when (eql 0 (fast-mod suggestion key))
        (return)))
    (incf suggestion 2)))


(defvar *continue-from-readonly-hashtable-lock-error* t)

(defun signal-read-only-hash-table-error (hash)
  (cond (*continue-from-readonly-hashtable-lock-error*
         (cerror "Make the hash-table writable. DANGEROUS! This could damage your lisp if another thread is acccessing this table. CONTINUE ONLY IF YOU KNOW WHAT YOU'RE DOING!"
                 "Hash-table ~s is readonly" hash)
         (assert-hash-table-writeable hash)
         (write-lock-hash-table hash))
        (t (error "Hash-table ~s is readonly" hash))))

(defun read-lock-hash-table (hash)
  (if (nhash.read-only hash)
    :readonly
    (let* ((lock (nhash.exclusion-lock hash)))
      (if lock
        (read-lock-rwlock lock)
        (unless (eq (nhash.owner hash) *current-process*)
          (error "Not owner of hash table ~s" hash))))))

(defun write-lock-hash-table (hash)
  (if (nhash.read-only hash)
    (signal-read-only-hash-table-error hash)
    (let* ((lock (nhash.exclusion-lock hash)))
      (if lock
        (write-lock-rwlock lock)
        (unless (eq (nhash.owner hash) *current-process*)
          (error "Not owner of hash table ~s" hash))))))


(defun unlock-hash-table (hash was-readonly)
  (unless was-readonly
    (let* ((lock (nhash.exclusion-lock hash)))
      (if lock
        (unlock-rwlock lock)))))

(defun index->vector-index (index)
  (declare (fixnum index))
  (the fixnum (+ $nhash.vector_overhead (the fixnum (+ index index)))))

(defun vector-index->index (index)
  (declare (fixnum index))
  (the fixnum (ash (the fixnum (- index $nhash.vector_overhead)) -1)))

(defun hash-table-count (hash)
  "Return the number of entries in the given HASH-TABLE."
  (setq hash (require-type hash 'hash-table))
  (when (hash-lock-free-p hash)
    ;; We don't try to maintain a running total, so just count.
    (return-from hash-table-count (lock-free-count-entries hash)))
  (%normalize-hash-table-count hash)
  (the fixnum (nhash.vector.count (nhash.vector hash))))

(defun hash-table-rehash-size (hash)
  "Return the rehash-size HASH-TABLE was created with."
  (nhash.rehash-size (require-type hash 'hash-table)))

(defun hash-table-rehash-threshold (hash)
  "Return the rehash-threshold HASH-TABLE was created with."
  (/ 1.0 (nhash.rehash-ratio (require-type hash 'hash-table))))

(defun hash-table-size (hash)
  "Return a size that can be used with MAKE-HASH-TABLE to create a hash
   table that can hold however many entries HASH-TABLE can hold without
   having to be grown."
  (let* ((hash (require-type hash 'hash-table))
         (vector (nhash.vector hash)))
    (values (floor (nhash.vector.size vector) (nhash.rehash-ratio hash)))))

(defun hash-table-test (hash)
  "Return the test HASH-TABLE was created with."
  (let ((f (nhash.compareF (require-type hash 'hash-table))))
    (if (fixnump f)
      (if (eql 0 f) 'eq 'eql)
      (let ((name (if (symbolp f) f (function-name f))))
        (if (memq name '(equal equalp)) name f)))))

;;; sometimes you'd rather have the function than the symbol.
(defun hash-table-test-function (hash)
  (let ((f (nhash.compareF (require-type hash 'hash-table))))
    (if (fixnump f)
      (if (eql 0 f) #'eq #'eql)
      f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; nearly-lock-free hash tables
;;
;; A modification of the lock-free hash table algorithm described by Cliff Click Jr.  in
;; http://blogs.azulsystems.com/cliff/2007/03/a_nonblocking_h.html.
;;
;; The modifications have to do with the fact that our goal is just to minimize the
;; performance impact of thread-safety, by eliminating the need for locking on every
;; read.  I don't bother with aspects of his algorithm that aren't relevant to that goal.
;;
;; The main difference from Click's algorithm is that I don't try to do rehashing
;; concurrently.  Instead, rehashing grabs a lock, so that only one thread can be
;; rehashing at any given time, and readers/writers will block waiting for the rehashing
;; to finish.
;;
;; In addition, I don't have a separate state for partially inserted key, I reuse the
;; DELETED state for that.  So in our implementation the following are the possible states
;; of a hash table entry (where "object" means any object other than the special markers):
;;
;; State      Key               Value
;; DELETED1   object            free-hash-marker
;; DELETED2   deleted-marker    free-hash-marker
;; IN-USE     object            object
;; FREE       free-hash-marker  free-hash-marker
;; REHASHING  object            rehashing-value-marker
;; REHASHING  free-hash-marker  rehashing-value-marker
;; REHASHING  deleted-marker    rehashing-value-marker
;;
;; No other states are allowed - at no point in time can a hash table entry be in any
;; other state.   In addition, the only transitions allowed on the key slot are
;; free-hash-marker -> object/deleted-marker -> deleted-marker.  Once a key slot
;; is claimed, it must never change to free or another key value (even after the hash
;; vector has been discarded after rehashing, because there some process might still
;; be looking at it).
;; In particular, rehashing in place is not an option.  All rehashing creates a new
;; vector and copies into it.  This means it's kinda risky to use lock-free hash
;; tables with address-based keys, because they will thrash in low-memory situations,
;; but we don't disallow it because a particular use might not have this problem.
;;
;; The following operations may take place:
;;
;; * gethash: find matching key - if no match, return not found.  Else fetch value,
;;   if value is rehashing-value-marker then maybe-rehash and try again;
;;   if value is free-hash-marker, return not found, else return found value.
;;
;; * puthash: find matching key or FREE slot.
;;   ** If found key, fetch value.
;;      if value is rehashing-value-marker then maybe-rehash and try again;
;;      else store-conditional the value -> new value, if fails try again.
;;   ** Else have FREE slot, store-key-conditional free-hash-marker -> key,
;;      and if that succeeds, store-conditional free-hash-marker -> new value,
;;      if either fails, maybe-rehash and try again.
;;
;; * remhash: find matching key - if no match, done.  Else fetch value,
;;   if value is rehashing-value-marker then maybe-rehash and try again;
;;   else store-conditional the value -> free-hash-marker, if fails try again.
;;
;; * rehash: grab a lock, estimate number of entries, make a new vector.  loop over
;; old vector, at each entry fetch the old value with atomic swap of
;; rehashing-value-marker.  This prevents any further state changes involving the
;; value.  It doesn't prevent state changes involving the key, but the only ones that
;; can happen is FREE -> DELETED, and DELETED1 <-> DELETED2, all of which are
;; equivalent from the point of view of rehashing.  Anyway, if the old value was
;; rehashing-value-marker then bug (because we have a lock).  If the old value is
;; free-hash-marker then do nothing, else get the entry key and rehash into the new
;; vector -- if no more room, start over.  When done, store the new vector in the
;; hash table and release lock.
;;
;; * gc: for weak tables, gc may convert IN-USE states to DELETED2 states.
;;   Even for non-weak tables, gc could convert DELETED1 states to DELETED2.


(defun lock-free-rehash (hash)
  ;;(break "We think we need to rehash ~s" (nhash.vector hash))
  (with-lock-context
    (without-interrupts ;; not re-entrant
      (let ((lock (nhash.exclusion-lock hash)))
        (%lock-recursive-lock-object lock)
        ;; TODO: might also want to rehash if deleted entries are a large percentage
        ;; of all entries, more or less.
        (when (or (%i<= (nhash.grow-threshold hash) 0) ;; no room
                  (%needs-rehashing-p (nhash.vector hash))) ;; or keys moved
          (%lock-free-rehash hash))
        (%unlock-recursive-lock-object lock)))))


;; TODO: This is silly.  We're implementing atomic swap using store-conditional,
;; but internally store-conditional is probably implemented using some kind of
;; an atomic swap!!
(defun atomic-swap-gvector (index gvector value)
  (loop
    (let ((old-value (%svref gvector index)))
      (when (store-gvector-conditional index gvector old-value value)
        (return old-value)))))

;; Interrupts are disabled and caller has the hash lock on the table, blocking other
;; threads attempting a rehash.
;; Other threads might be reading/writing/deleting individual entries, but they
;; will block if they see a value = rehashing-value-marker.
;; GC may run, updating the needs-rehashing flags and deleting weak entries in both
;; old and new vectors.
(defun %lock-free-rehash (hash)
  ;; Prevent puthash from adding new entries.  Note this doesn't keep it from undeleting
  ;; existing entries, so we might still lose, but this makes the odds much smaller.
  (setf (nhash.grow-threshold hash) 0)
  (let* ((old-vector (nhash.vector hash))
         (inherited-flags (logand $nhash_weak_flags_mask (nhash.vector.flags old-vector)))
         count new-vector grow-threshold vector-size)
    (tagbody
     RESTART
     (setq count (lock-free-count-entries hash))
     (multiple-value-setq (grow-threshold vector-size)
       (compute-hash-size count (nhash.rehash-size hash) (nhash.rehash-ratio hash)))
     (setq new-vector (%cons-nhash-vector vector-size inherited-flags))
     REHASH
     (loop for i from $nhash.vector_overhead below (uvsize old-vector) by 2
       do (let ((value (atomic-swap-gvector (%i+ i 1) old-vector rehashing-value-marker)))
            (when (eq value rehashing-value-marker) (error "Who else is doing this?"))
            (unless (eq value free-hash-marker)
              (let* ((key (%svref old-vector i))
                     (new-index (%growhash-probe new-vector hash key))
                     (new-vector-index (index->vector-index new-index)))
                (setf (%svref new-vector new-vector-index) key)
                (setf (%svref new-vector (%i+ new-vector-index 1)) value)
                (when (%i<= (decf grow-threshold) 0)
                  ;; Too many entries got undeleted while we were rehashing!
                  (go RESTART))))))
     (when (%needs-rehashing-p new-vector) ;; keys moved, but at least can use the same new-vector.
       (%init-misc free-hash-marker new-vector)
       (%init-nhash-vector new-vector inherited-flags)
       (go REHASH)))
    (setf (nhash.vector.hash new-vector) hash)
    (setf (nhash.grow-threshold hash) grow-threshold)
    ;; At this point, another thread might decrement the threshold while they're looking at the old
    ;; vector. That's ok, just means it will be too small and we'll rehash sooner than planned,
    ;; no big deal.
    (setf (nhash.vector hash) new-vector)))


(defun lock-free-gethash (key hash default)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop
    (let* ((vector (nhash.vector hash))
           (vector-index (funcall (the function (nhash.find hash)) hash key)))
      (declare (fixnum vector-index))
      ;; Need to punt if vector changed because no way to know whether nhash.find was
      ;; using old or new vector.
      (when (eq vector (nhash.vector hash))
        (cond ((eql vector-index -1)
               (unless (%needs-rehashing-p vector)
                 (return-from lock-free-gethash (values default nil))))
              (t (let ((value (%svref vector (%i+ vector-index 1))))
                   (unless (eq value rehashing-value-marker)
                     (if (eq value free-hash-marker)
                       (return-from lock-free-gethash (values default nil))
                       (return-from lock-free-gethash (values value t)))))))))
    ;; We're here because the table needs rehashing or it was getting rehashed while we
    ;; were searching. Take care of it and try again.
    (lock-free-rehash hash)))

(defun lock-free-remhash (key hash)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop
    (let* ((vector (nhash.vector hash))
           (vector-index (funcall (the function (nhash.find hash)) hash key)))
      (declare (fixnum vector-index))
      ;; Need to punt if vector changed because no way to know whether nhash.find was
      ;; using old or new vector.
      (when (eq vector (nhash.vector hash))
        (cond ((eql vector-index -1)
               (unless (%needs-rehashing-p vector)
                 (return-from lock-free-remhash nil)))
              (t (let ((old-value (%svref vector (%i+ vector-index 1))))
                   (unless (eq old-value rehashing-value-marker)
                     (when (eq old-value free-hash-marker)
                       (return-from lock-free-remhash nil))
                     (when (set-hash-value-conditional vector-index vector old-value free-hash-marker)
                       ;; We just use this as a flag - tell gc to scan the vector for deleted keys.
                       ;; It's just a hint, so don't worry about sync'ing
                       (setf (nhash.vector.deleted-count vector) 1)
                       (return-from lock-free-remhash t)))))))
      ;; We're here because the table needs rehashing or it was getting rehashed while we
      ;; were searching.  Take care of it and try again.
      (lock-free-rehash hash))))

(defun lock-free-clrhash (hash)
  (with-lock-context
    (without-interrupts
     (let ((lock (nhash.exclusion-lock hash)))
       (%lock-recursive-lock-object lock) ;; disallow rehashing.
       (loop
         with vector = (nhash.vector hash)
         for i1 fixnum from (%i+ $nhash.vector_overhead 1) below (uvsize vector) by 2
         do (setf (%svref vector i1) free-hash-marker)
         ;; We just use this as a flag - tell gc to scan the vector for deleted keys.
         ;; It's just a hint, so don't worry about sync'ing
         finally (setf (nhash.vector.deleted-count vector) 1))
       (%unlock-recursive-lock-object lock))))
  hash)

(defun lock-free-puthash (key hash value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (or (eq value rehashing-value-marker)
            (eq value free-hash-marker))
    (error "Illegal value ~s for storing in a hash table" value))
  (loop
    (let* ((vector (nhash.vector  hash))
           (vector-index (funcall (nhash.find-new hash) hash key)))
      ;; Need to punt if vector changed because no way to know whether nhash.find-new was
      ;; using old or new vector.
      (when (eq vector (nhash.vector hash))
        (cond ((or (eql vector-index -1)
                   (eq (%svref vector vector-index) free-hash-marker))
               (unless (or (%needs-rehashing-p vector)
                           (%i<= (nhash.grow-threshold hash) 0))
                 ;; Note if the puthash fails, grow-threshold will end up too small. This
                 ;; just means we might rehash sooner than absolutely necessary, no real
                 ;; harm done (the most likely cause of failing is that somebody is
                 ;; already rehashing anyway).  DON'T try to incf it back on failure --
                 ;; that risks grow-threshold ending up too big (e.g. if somebody rehashes
                 ;; before the incf), which _could_ be harmful.
                 (atomic-decf (nhash.grow-threshold hash))
                 (if (set-hash-key-conditional vector-index vector free-hash-marker key)
                   (when (set-hash-value-conditional vector-index vector free-hash-marker value)
                     (return-from lock-free-puthash value)))))
              (t (let ((old-value (%svref vector (%i+ vector-index 1))))
                   (unless (eq old-value rehashing-value-marker)
                     (when (set-hash-value-conditional vector-index vector old-value value)
                       (return-from lock-free-puthash value))))))))
    ;; We're here because the table needs rehashing or it was getting rehashed while we
    ;; were searching, or no room for new entry, or somebody else claimed the key from
    ;; under us (that last case doesn't need to retry, but it's unlikely enough that
    ;; it's not worth checking for).  Take care of it and try again.
    (lock-free-rehash hash)))


(defun lock-free-count-entries (hash)
  ;; Other threads could be adding/removing entries while we count, some of
  ;; which will be included in the count (i.e. will be treated as if they
  ;; happened after counting) and some won't (i.e. will be treated as if
  ;; they happened before counting), but not necessarily in correlation
  ;; with their temporal relationship.
  (loop
    with vector = (nhash.vector hash)
    for i fixnum from $nhash.vector_overhead below (uvsize vector) by 2
    count (and (neq (%svref vector i) free-hash-marker)
               (let ((value (%svref vector (%i+ i 1))))
                 (when (eq value rehashing-value-marker)
                   ;; This table is being rehashed.  Wait for it to be
                   ;; done and try again.
                   (lock-free-rehash hash)
                   (return-from lock-free-count-entries (lock-free-count-entries hash)))
                 (neq value free-hash-marker)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gethash (key hash &optional default)
  "Finds the entry in HASH-TABLE whose key is KEY and returns the associated
   value and T as multiple values, or returns DEFAULT and NIL if there is no
   such entry. Entries can be added using SETF."
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (when (or (eq key free-hash-marker)
            (eq key deleted-hash-key-marker))
    (return-from gethash (values default nil)))
  (when (hash-lock-free-p hash)
    (return-from gethash (lock-free-gethash key hash default)))
  (let* ((value nil)
         (gc-locked nil)
         (readonly nil)
         (foundp nil))
    (with-lock-context
      (without-interrupts
        (setq readonly (eq (read-lock-hash-table hash) :readonly))
        (let* ((vector (nhash.vector hash)))
          (if (and (eq key (nhash.vector.cache-key vector))
                   ;; Check twice: the GC might nuke the cached key/value pair
                   (progn (setq value (nhash.vector.cache-value vector))
                          (eq key (nhash.vector.cache-key vector))))
            (setq foundp t)
            (loop
              (let* ((vector-index (funcall (nhash.find hash) hash key)))
                (declare (fixnum vector-index))
                (cond ((setq foundp (not (eql vector-index -1)))
                       ;; Referencing both key and value here - and referencing
                       ;; value first - is an attempt to compensate for the
                       ;; possibility that the GC deletes a weak-on-key pair.
                       (setq value (%svref vector (%i+ vector-index 1)))
                       (when (nhash.owner hash)
                         (setf (nhash.vector.cache-key vector)
                               (%svref vector vector-index)
                               (nhash.vector.cache-value vector)
                               value
                               (nhash.vector.cache-idx vector)
                               (vector-index->index (the fixnum vector-index))))
                       (return))
                      ((%needs-rehashing-p vector)
                       (%lock-gc-lock)
                       (setq gc-locked t)
                       (unless readonly
                         (let* ((lock (nhash.exclusion-lock hash)))
                           (when lock (%promote-rwlock lock))))
                       (when (%needs-rehashing-p vector)
                         (%rehash hash)))
                      (t (return)))))))
        (when gc-locked (%unlock-gc-lock))
        (unlock-hash-table hash readonly)))
    (if foundp
      (values value t)
      (values default nil))))

(defun remhash (key hash)
  "Remove the entry in HASH-TABLE associated with KEY. Return T if there
   was such an entry, or NIL if not."
  (unless (typep hash 'hash-table)
    (setq hash (require-type hash 'hash-table)))
  (when (hash-lock-free-p hash)
    (return-from remhash (lock-free-remhash key hash)))
  (let* ((foundp nil))
    (with-lock-context
      (without-interrupts
       (write-lock-hash-table hash)
       (%lock-gc-lock)
       (let* ((vector (nhash.vector hash)))
         (when (%needs-rehashing-p vector)
           (%rehash hash))
         (if (eq key (nhash.vector.cache-key vector))
           (progn
             (setf (nhash.vector.cache-key vector) free-hash-marker
                   (nhash.vector.cache-value vector) nil)
             (let ((vidx (index->vector-index (nhash.vector.cache-idx vector))))
               (setf (%svref vector vidx) deleted-hash-key-marker)
               (setf (%svref vector (the fixnum (1+ vidx))) nil))
             (incf (the fixnum (nhash.vector.deleted-count vector)))
             (decf (the fixnum (nhash.vector.count vector)))
             (setq foundp t))
           (let* ((vector-index (funcall (nhash.find hash) hash key)))
             (declare (fixnum vector-index))
             (unless (eql vector-index -1)
               ;; always clear the cache cause I'm too lazy to call the
               ;; comparison function and don't want to keep a possibly
               ;; deleted key from being GC'd
               (setf (nhash.vector.cache-key vector) free-hash-marker
                     (nhash.vector.cache-value vector) nil)
               ;; Update the count
               (incf (the fixnum (nhash.vector.deleted-count vector)))
               (decf (the fixnum (nhash.vector.count vector)))
               ;; Delete the value from the table.
               (setf (%svref vector vector-index) deleted-hash-key-marker
                     (%svref vector (the fixnum (1+ vector-index))) nil)
               (setq foundp t))))
         (when (and foundp
                    (zerop (the fixnum (nhash.vector.count vector))))
           (do* ((i $nhash.vector_overhead (1+ i))
                 (n (uvsize vector)))
                ((= i n))
             (declare (fixnum i n))
             (setf (%svref vector i) free-hash-marker))
           (setf (nhash.grow-threshold hash)
                 (+ (nhash.vector.deleted-count vector)
                    (nhash.vector.weak-deletions-count vector)
                    (nhash.grow-threshold hash))
                 (nhash.vector.deleted-count vector) 0
                 (nhash.vector.weak-deletions-count vector) 0)))
       ;; Return T if we deleted something
       (%unlock-gc-lock)
       (unlock-hash-table hash nil)))
    foundp))

;;; what if somebody is mapping, growing, rehashing? 
(defun clrhash (hash)
  "This removes all the entries from HASH-TABLE and returns the hash table
   itself."
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (when (hash-lock-free-p hash)
    (return-from clrhash (lock-free-clrhash hash)))
  (with-lock-context
    (without-interrupts
     (write-lock-hash-table hash)
     (let* ((vector (nhash.vector hash))
            (size (nhash.vector-size vector))
            (count (+ size size))
            (index $nhash.vector_overhead))
       (declare (fixnum size count index))
       (dotimes (i count)
         (setf (%svref vector index) free-hash-marker)
         (incf index))
       (incf (the fixnum (nhash.grow-threshold hash))
             (the fixnum (+ (the fixnum (nhash.vector.count vector))
                            (the fixnum (nhash.vector.deleted-count vector)))))
       (setf (nhash.vector.count vector) 0
             (nhash.vector.cache-key vector) free-hash-marker
             (nhash.vector.cache-value vector) nil
             (nhash.vector.finalization-alist vector) nil
             (nhash.vector.free-alist vector) nil
             (nhash.vector.weak-deletions-count vector) 0
             (nhash.vector.deleted-count vector) 0
             (nhash.vector.flags vector) (logand $nhash_weak_flags_mask
                                                 (nhash.vector.flags vector))))
     (unlock-hash-table hash nil)
     hash)))


(defun puthash (key hash default &optional (value default))
  (declare (optimize (speed 3) (space 0)))
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (when (or (eq key free-hash-marker)
            (eq key deleted-hash-key-marker))
    (error "Can't use ~s as a hash-table key" key))
  (when (hash-lock-free-p hash)
    (return-from puthash (lock-free-puthash key hash value)))
  (with-lock-context
    (without-interrupts
     (block protected
       (tagbody
          (write-lock-hash-table hash)
        AGAIN
          (%lock-gc-lock)
          (let ((vector (nhash.vector hash)))
            (when (%needs-rehashing-p vector)
              (%rehash hash))
            (when (eq key (nhash.vector.cache-key vector))
              (let* ((idx (nhash.vector.cache-idx vector)))
                (declare (fixnum idx))
                (setf (%svref vector (the fixnum (1+ (the fixnum (index->vector-index idx)))))
                      value)
                (setf (nhash.vector.cache-value vector) value)
                (return-from protected)))               
            (let* ((vector-index (funcall (nhash.find-new hash) hash key))
                   (old-value (%svref vector vector-index)))
              (declare (fixnum vector-index))

              (cond ((eq old-value deleted-hash-key-marker)
                     (%set-hash-table-vector-key vector vector-index key)
                     (setf (%svref vector (the fixnum (1+ vector-index))) value)
                     (incf (the fixnum (nhash.vector.count vector)))
                     ;; Adjust deleted-count
                     (when (> 0 (the fixnum
                                  (decf (the fixnum
                                          (nhash.vector.deleted-count vector)))))
                       (%normalize-hash-table-count hash)))
                    ((eq old-value free-hash-marker)
                     (when (eql 0 (nhash.grow-threshold hash))
                       (%unlock-gc-lock)
                       (%grow-hash-table hash)
                       (go AGAIN))
                     (%set-hash-table-vector-key vector vector-index key)
                     (setf (%svref vector (the fixnum (1+ vector-index))) value)
                     (decf (the fixnum (nhash.grow-threshold hash)))
                     (incf (the fixnum (nhash.vector.count vector))))
                    (t
                     ;; Key was already there, update value.
                     (setf (%svref vector (the fixnum (1+ vector-index))) value)))
              (setf (nhash.vector.cache-idx vector) (vector-index->index vector-index)
                    (nhash.vector.cache-key vector) key
                    (nhash.vector.cache-value vector) value)))))
     (%unlock-gc-lock)
     (unlock-hash-table hash nil)))
  value)


(defun count-entries (hash)
  (if (hash-lock-free-p hash)
    (lock-free-count-entries hash)
    (let* ((vector (nhash.vector hash))
           (size (uvsize vector))
           (idx $nhash.vector_overhead)
           (count 0))
      (loop
        (when (neq (%svref vector idx) free-hash-marker)
          (incf count))
        (when (>= (setq idx (+ idx 2)) size)
          (return count))))))





     

(defun grow-hash-table (hash)
  (unless (typep hash 'hash-table)
    (setq hash (require-type hash 'hash-table)))
  (%grow-hash-table hash))

;;; Interrupts are disabled, and the caller has an exclusive
;;; lock on the hash table.
(defun %grow-hash-table (hash)
  (block grow-hash-table
    (%normalize-hash-table-count hash)
    (let* ((old-vector (nhash.vector hash))
           (old-size (nhash.vector.count old-vector))
           (old-total-size (nhash.vector.size old-vector))
           (flags 0)
           (flags-sans-weak 0)
           (weak-flags nil))
      (declare (fixnum old-total-size flags flags-sans-weak))
      (when (> (nhash.vector.deleted-count old-vector) 0)
        ;; There are enough deleted entries. Rehash to get rid of them
        (%rehash hash)
        (return-from grow-hash-table))
      (multiple-value-bind (size total-size)
                           (compute-hash-size 
                            old-size (nhash.rehash-size hash) (nhash.rehash-ratio hash))
        (unless (eql 0 (nhash.grow-threshold hash))       ; maybe it's done already - shouldnt happen                
          (return-from grow-hash-table ))
        (progn
          (unwind-protect
            (let ((gc-count (%get-gc-count))
                  vector)
              (setq flags (nhash.vector.flags old-vector)
                    flags-sans-weak (logand flags (logxor -1 $nhash_weak_flags_mask))
                    weak-flags (logand flags $nhash_weak_flags_mask))
              (setf (nhash.vector.flags old-vector) flags-sans-weak)      ; disable GC weak stuff
              (%normalize-hash-table-count hash)
              (when (> (nhash.vector.deleted-count old-vector) 0)
                (setf (nhash.vector.flags old-vector) flags)
                (setq weak-flags nil)
                (return-from grow-hash-table (%rehash hash)))
              (setq vector (%cons-nhash-vector total-size 0))
              (do* ((index 0 (1+ index))
                    (vector-index (index->vector-index 0) (+ vector-index 2)))
                   ((>= index old-total-size))
                (declare (fixnum index vector-index))
                
                 (let ((key (%svref old-vector vector-index)))
                   (unless (or (eq key free-hash-marker)
                               (eq key deleted-hash-key-marker))
                     (let* ((new-index (%growhash-probe vector hash key))
                            (new-vector-index (index->vector-index new-index)))
                       (setf (%svref vector new-vector-index) key)
                       (setf (%svref vector (the fixnum (1+ new-vector-index)))
                             (%svref old-vector (the fixnum (1+ vector-index))))))))
              (progn
               (setf (nhash.vector.finalization-alist vector)
                     (nhash.vector.finalization-alist old-vector)
                     (nhash.vector.free-alist vector)
                     (nhash.vector.free-alist old-vector)
                     (nhash.vector.count vector) old-size
                     (nhash.vector.flags vector)
                     (logior (the fixnum weak-flags)
                             (the fixnum (nhash.vector.flags vector))))
               (setf (nhash.rehash-bits hash) nil
                     (nhash.vector hash) vector
                     (nhash.vector.hash vector) hash
                     (nhash.vector.cache-key vector) free-hash-marker
                     (nhash.vector.cache-value vector) nil
                     (nhash.vector.gc-count vector) gc-count
                     (nhash.grow-threshold hash) (- size old-size))
               (setq weak-flags nil)       ; tell clean-up form we finished the loop
               ;; If the old vector's in some static heap, zero it
               ;; so that less garbage is retained.
	       (%init-misc 0 old-vector)))
            (when weak-flags
              (setf (nhash.vector.flags old-vector)
                    (logior (the fixnum weak-flags)
                            (the fixnum (nhash.vector.flags old-vector)))))))))))



(defun general-hash-find (hash key)
  (%hash-probe hash key nil))

(defun general-hash-find-for-put (hash key)
  (%hash-probe hash key (if (hash-lock-free-p hash) :free :reuse)))

;;; returns a single value:
;;;   index - the index in the vector for key (where it was or where
;;;           to insert if the current key at that index is deleted-hash-key-marker
;;;           or free-hash-marker)



(defun %hash-probe (hash key for-put-p)
  (declare (optimize (speed 3) (space 0)))
  (multiple-value-bind (hash-code index entries)
                       (compute-hash-code hash key for-put-p)
    (locally (declare (fixnum hash-code index entries))
      (let* ((compareF (nhash.compareF hash))
             (vector (nhash.vector hash))
             (vector-index 0)
             table-key
             (first-deleted-index nil))
        (declare (fixnum vector-index))
        (macrolet ((return-it (form)
                     `(return-from %hash-probe ,form)))
          (macrolet ((test-it (predicate)
                       (unless (listp predicate) (setq predicate (list predicate)))
                       `(progn
                          (setq vector-index (index->vector-index index)
                                table-key (%svref vector vector-index))
                          (cond ((eq table-key free-hash-marker)
                                 (return-it (if for-put-p
                                              (or first-deleted-index
                                                  vector-index)
                                              -1)))
                                ((eq table-key deleted-hash-key-marker)
                                 (when (and (eq for-put-p :reuse)
                                            (null first-deleted-index))
                                   (setq first-deleted-index vector-index)))
                                ((,@predicate key table-key)
                                 (return-it vector-index))))))
            (macrolet ((do-it (predicate)
                         `(progn
                            (test-it ,predicate)
                            ; First probe failed. Iterate on secondary key
                            (let ((initial-index index)
                                  (secondary-hash (%svref secondary-keys (logand 7 hash-code))))
                              (declare (fixnum secondary-hash initial-index))
                              (loop
                                (incf index secondary-hash)
                                (when (>= index entries)
                                  (decf index entries))
                                (when (eql index initial-index)
                                  (return-it (if for-put-p
                                               (or first-deleted-index
                                                   (error "Bug: no room in table"))
                                               -1)))
                                (test-it ,predicate))))))
              (if (fixnump comparef)
                ;; EQ or EQL hash table
                (if (or (eql 0 comparef)
                        (immediate-p-macro key)
                        (not (need-use-eql key)))
                  ;; EQ hash table or EQL == EQ for KEY
                  (do-it eq)
                  (do-it eql))
                ;; general compare function
                (do-it (funcall comparef))))))))))

(defun eq-hash-find (hash key)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((vector (nhash.vector hash))
         (hash-code
          (let* ((typecode (typecode key)))
            (if (eq typecode target::tag-fixnum)
              (mixup-hash-code key)
              (if (eq typecode target::subtag-instance)
                (mixup-hash-code (instance.hash key))
                (if (symbolp key)
                  (%hash-symbol key)
                  (mixup-hash-code (strip-tag-to-fixnum key)))))))
         (entries (nhash.vector-size vector))
         (vector-index (index->vector-index (hash-mod hash-code entries vector)))
         (table-key (%svref vector vector-index)))
    (declare (fixnum hash-code  entries vector-index))
    (if (eq table-key key)
      vector-index
      (if (eq table-key free-hash-marker)
        -1
        (let* ((secondary-hash (%svref secondary-keys-*-2
                                       (logand 7 hash-code)))
               (initial-index vector-index)             
               (count (+ entries entries))
               (length (+ count $nhash.vector_overhead)))
          (declare (fixnum secondary-hash initial-index count length))
          (loop
            (incf vector-index secondary-hash)
            (when (>= vector-index length)
              (decf vector-index count))
            (setq table-key (%svref vector vector-index))
            (when (= vector-index initial-index)
              (return -1))
            (if (eq table-key key)
              (return vector-index)
              (when (eq table-key free-hash-marker)
                (return -1)))))))))

;;; As above, but note whether the key is in some way address-based
;;; and update the hash-vector's flags word if so.
;;; This only needs to be done by PUTHASH, and it only really needs
;;; to be done if we're adding a new key.
(defun eq-hash-find-for-put (hash key)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((vector (nhash.vector hash))
         (hash-code
          (let* ((typecode (typecode key)))
            (if (eq typecode target::tag-fixnum)
              (mixup-hash-code key)
              (if (eq typecode target::subtag-instance)
                (mixup-hash-code (instance.hash key))
                (if (symbolp key)
                  (%hash-symbol key)
                  (progn
                    (unless (immediate-p-macro key)
                      (update-hash-flags hash vector :key))
                    (mixup-hash-code (strip-tag-to-fixnum key))))))))
         (entries (nhash.vector-size vector))
         (vector-index (index->vector-index (hash-mod hash-code entries vector)))
         (table-key (%svref vector vector-index))
         (reuse (not (hash-lock-free-p hash))))
    (declare (fixnum hash-code vector-index))
    (if (or (eq key table-key)
            (eq table-key free-hash-marker))
      vector-index
      (let* ((secondary-hash (%svref secondary-keys-*-2
                                     (logand 7 hash-code)))
             (initial-index vector-index)             
             (first-deleted-index (and reuse
                                       (eq table-key deleted-hash-key-marker)
                                       vector-index))
             (count (+ entries entries))
             (length (+ count $nhash.vector_overhead)))
        (declare (fixnum secondary-hash initial-index count length))
        (loop
          (incf vector-index secondary-hash)
          (when (>= vector-index length)
            (decf vector-index count))
          (setq table-key (%svref vector vector-index))
          (when (= vector-index initial-index)
            (return (or first-deleted-index
                        (error "Bug: no room in table"))))
          (if (eq table-key key)
            (return vector-index)
            (if (eq table-key free-hash-marker)
              (return (or first-deleted-index vector-index))
              (if (and reuse
                       (null first-deleted-index)
                       (eq table-key deleted-hash-key-marker))
                (setq first-deleted-index vector-index)))))))))

(defun eql-hash-find (hash key)
  (declare (optimize (speed 3) (safety 0)))
  (if (need-use-eql key)
    (let* ((vector (nhash.vector hash))
           (hash-code (%%eqlhash-internal key))
           (entries (nhash.vector-size vector))
           (vector-index (index->vector-index (hash-mod hash-code entries vector)))
           (table-key (%svref vector vector-index)))
      (declare (fixnum hash-code entries vector-index))
      (if (eql key table-key)
        vector-index
        (if (eq table-key free-hash-marker)
          -1
          (let* ((secondary-hash (%svref secondary-keys-*-2
                                         (logand 7 hash-code)))
                 (initial-index vector-index)
                 (count (+ entries entries))
                 (length (+ count $nhash.vector_overhead)))
            (declare (fixnum secondary-hash initial-index count length))
            (loop
              (incf vector-index secondary-hash)
              (when (>= vector-index length)
                (decf vector-index count))
              (setq table-key (%svref vector vector-index))
              (when (= vector-index initial-index)
                (return -1))
              (if (eql table-key key)
                (return vector-index)
                (when (eq table-key free-hash-marker)
                  (return -1))))))))
    (eq-hash-find hash key)))

(defun eql-hash-find-for-put (hash key)
  (declare (optimize (speed 3) (safety 0)))
  (if (need-use-eql key)
    (let* ((vector (nhash.vector hash))
           (hash-code (%%eqlhash-internal key))
           (entries (nhash.vector-size vector))
           (vector-index (index->vector-index (hash-mod hash-code entries vector)))
           (table-key (%svref vector vector-index))
           (reuse (not (hash-lock-free-p hash))))
      (declare (fixnum hash-code entries vector-index))
      (if (or (eql key table-key)
              (eq table-key free-hash-marker))
        vector-index
        (let* ((secondary-hash (%svref secondary-keys-*-2
                                       (logand 7 hash-code)))
               (initial-index vector-index)
               (first-deleted-index (and reuse
                                         (eq table-key deleted-hash-key-marker)
                                         vector-index))
               (count (+ entries entries))
               (length (+ count $nhash.vector_overhead)))
          (declare (fixnum secondary-hash initial-index count length))
          (loop
            (incf vector-index secondary-hash)
            (when (>= vector-index length)
              (decf vector-index count))
            (setq table-key (%svref vector vector-index))
            (when (= vector-index initial-index)
              (return (or first-deleted-index
                          (error "Bug: no room in table"))))
            (if (eql table-key key)
              (return vector-index)
              (if (eq table-key free-hash-marker)
                (return (or first-deleted-index vector-index))
                (if (and reuse
                         (null first-deleted-index)
                         (eq table-key deleted-hash-key-marker))
                  (setq first-deleted-index vector-index))))))))
    (eq-hash-find-for-put hash key)))

(defun %make-rehash-bits (hash &optional (size (nhash.vector-size (nhash.vector hash))))
  (declare (fixnum size))
  (let ((rehash-bits (nhash.rehash-bits hash)))
    (unless (and rehash-bits
                 (>= (uvsize rehash-bits) size))
      (return-from %make-rehash-bits
        (setf (nhash.rehash-bits hash) (make-array size :element-type 'bit :initial-element 0))))
    (fill (the simple-bit-vector rehash-bits) 0)))

;;; Rehash.  Caller should have exclusive access to the hash table
;;; and have disabled interrupts.
(defun %rehash (hash)
  (when (hash-lock-free-p hash)
    (error "How did we get here?"))
  (let* ((vector (nhash.vector hash))
         (flags (nhash.vector.flags vector))
         (vector-index (- $nhash.vector_overhead 2))
         (size (nhash.vector-size vector))
         (rehash-bits (%make-rehash-bits hash size))
         (index -1))
    (declare (fixnum size index vector-index))
    (setf (nhash.vector.flags vector)
          (logand flags $nhash-clear-key-bits-mask))
    (setf (nhash.vector.cache-key vector) free-hash-marker
          (nhash.vector.cache-value vector) nil)
    (%set-does-not-need-rehashing hash)
    (loop
      (when (>= (incf index) size) (return))
      (setq vector-index (+ vector-index 2))
      (unless (%already-rehashed-p index rehash-bits)
        (let* ((key (%svref vector vector-index))
               (deleted (eq key deleted-hash-key-marker)))
          (unless
            (when (or deleted (eq key free-hash-marker))
              (if deleted  ; one less deleted entry
                (let ((count (1- (nhash.vector.deleted-count vector))))
                  (declare (fixnum count))
                  (setf (nhash.vector.deleted-count vector) count)
                  (if (< count 0)
                    (let ((wdc (nhash.vector.weak-deletions-count vector)))
                      (setf (nhash.vector.weak-deletions-count vector) 0)
                      (incf (nhash.vector.deleted-count vector) wdc)
                      (decf (nhash.vector.count vector) wdc)))
                  (incf (nhash.grow-threshold hash))
                  ;; Change deleted to free
                  (setf (%svref vector vector-index) free-hash-marker)))
              t)
            (let* ((last-index index)
                   (value (%svref vector (the fixnum (1+ vector-index))))
                   (first t))
                (loop
                  (let ((vector (nhash.vector hash))
                        (found-index (%rehash-probe rehash-bits hash key)))
                    (%set-already-rehashed-p found-index rehash-bits)
                    (if (eq last-index found-index)
                      (return)
                      (let* ((found-vector-index (index->vector-index found-index))
                             (newkey (%svref vector found-vector-index))
                             (newvalue (%svref vector (the fixnum (1+ found-vector-index)))))
			(declare (fixnum found-vector-index))
                        (when first ; or (eq last-index index) ?
                          (setq first nil)
                          (setf (%svref vector vector-index) free-hash-marker)
                          (setf (%svref vector (the fixnum (1+ vector-index))) free-hash-marker))
                        (%set-hash-table-vector-key vector found-vector-index key)
                        (setf (%svref vector (the fixnum (1+ found-vector-index))) value)                       
                        (when (or (eq newkey free-hash-marker)
                                  (setq deleted (eq newkey deleted-hash-key-marker)))
                          (when deleted
                            (let ((count (1- (nhash.vector.deleted-count vector))))
                              (declare (fixnum count))
                              (setf (nhash.vector.deleted-count vector) count)
                              (if (< count 0)
                                (let ((wdc (nhash.vector.weak-deletions-count vector)))
                                  (setf (nhash.vector.weak-deletions-count vector) 0)
                                  (incf (nhash.vector.deleted-count vector) wdc)
                                  (decf (nhash.vector.count vector) wdc)))
                              (incf (nhash.grow-threshold hash))))
                          (return))
                        (when (eq key newkey)
                          (cerror "Delete one of the entries." "Duplicate key: ~s in ~s ~s ~s ~s ~s"
                                  key hash value newvalue index found-index)                       
                          (decf (nhash.vector.count vector))
                          (incf (nhash.grow-threshold hash))
                          (return))
                        (setq key newkey
                              value newvalue
                              last-index found-index)))))))))))
    t )

;;; Hash to an index that is not set in rehash-bits
  
(defun %rehash-probe (rehash-bits hash key)
  (declare (optimize (speed 3)(safety 0)))  
  (multiple-value-bind (hash-code index entries)(compute-hash-code hash key t)
    (declare (fixnum hash-code index entries))
    (when (null hash-code)(cerror "nuts" "Nuts"))
    (let* ((vector (nhash.vector hash))
           (vector-index (index->vector-index  index)))
      (if (or (not (%already-rehashed-p index rehash-bits))
              (eq key (%svref vector vector-index)))
        (return-from %rehash-probe index)
        (let ((second (%svref secondary-keys (%ilogand 7 hash-code))))
          (declare (fixnum second))
          (loop
            (setq index (+ index second))
            (when (>= index entries)
              (setq index (- index entries)))
            (when (or (not (%already-rehashed-p index rehash-bits))
                      (eq key (%svref vector (index->vector-index index))))
              (return-from %rehash-probe index))))))))

;;; Returns one value: the index of the entry in the vector
;;; Since we're growing, we don't need to compare and can't find a key that's
;;; already there.
(defun %growhash-probe (vector hash key)
  (declare (optimize (speed 3)(safety 0)))
  (multiple-value-bind (hash-code index entries)(compute-hash-code hash key t vector)
    (declare (fixnum hash-code index entries))
    (let* ((vector-index (index->vector-index  index))
           (vector-key nil))
      (declare (fixnum vector-index))
      (if (or (eq free-hash-marker
                  (setq vector-key (%svref vector vector-index)))
              (eq deleted-hash-key-marker vector-key))
        (return-from %growhash-probe index)
        (let ((second (%svref secondary-keys (%ilogand 7 hash-code))))
          (declare (fixnum second))
          (loop
            (setq index (+ index second))
            (when (>= index entries)
              (setq index (- index entries)))
            (when (or (eq free-hash-marker
                          (setq vector-key (%svref vector (index->vector-index index))))
                      (eq deleted-hash-key-marker vector-key))
              (return-from %growhash-probe index))))))))

;;;;;;;;;;;;;
;;
;; Mapping functions are in "ccl:lib;hash"
;;



;;;;;;;;;;;;;
;;
;; Hashing functions
;; EQ & the EQ part of EQL are done in-line.
;;









;;; so whats so special about bit vectors as opposed to any other vectors of bytes
;;; For starters, it's guaranteed that they exist in the implementation; that may
;;; not be true of other immediate vector types.
(defun bit-vector-hash (bv)
  (declare (optimize (speed 3)(safety 0)))
  (let ((length (length bv)))
    (declare (fixnum length)) ;will this always be true? it's true of all vectors.
    (multiple-value-bind (data offset) (array-data-and-offset bv)
      (declare (type simple-bit-vector data) (fixnum offset))
      (let* ((hash 0)
             (limit (+ length offset))
             (nbytes (ash (the fixnum (+ length 7)) -3)))
        (declare (fixnum hash limit nbytes))
        (dotimes (i nbytes (mixup-hash-code hash))
          (let* ((w 0))
            (declare (fixnum w))
            (dotimes (j 8 (setq hash (+ (the fixnum (ash hash -3))  w)))
              (setq w (the fixnum
                        (logxor
                         (the fixnum
                           (ash (if (< offset limit) 
                                  (the fixnum (sbit data offset))
                                  0)
                                (the fixnum j)))
                         w)))
              (incf offset))))))))

#|
(defun bit-vector-hash (bv)
  (declare (optimize (speed 3)(safety 0)))
  (let ((length (length bv)))
    (declare (fixnum length))
    (let* ((all (+ length 15))
           (nwds (ash all -4))
           (rem (logand all 15))
           (hash 0)
           (mask (ash (the fixnum (1- (the fixnum (expt 2 rem))))(the fixnum(- 16 rem)))))
      (declare (fixnum all nwds rem hash mask))
      (multiple-value-bind (data offset)
                           (array-data-and-offset bv)
        (declare (fixnum offset))
        (locally (declare (type (simple-array (unsigned-byte 16) (*)) data))
          (dotimes (i nwds)
            (setq hash (%i+ hash (aref data (the fixnum (+ i offset))))))
          (when (neq 0 mask)            
            (setq hash (%i+ hash (%ilogand mask (aref data (the fixnum (+ offset nwds)))))))
          (mixup-hash-code hash))))))
|#


;;; Same as %%equalhash, but different:
;;;  1) Real numbers are hashed as if they were double-floats.  The real components of complex numbers
;;;     are hashed as double-floats and XORed together.
;;;  2) Characters and strings are hashed in a case-insensitive manner.
;;;  3) Hash tables are hashed based on their size and type.
;;;  4) Structures and CL array types are hashed based on their content.


;;; check fixnum befor immediate-p. call %%eqlhash

(defun %%equalphash (key)
  (cond ((or (fixnump key)(short-float-p key))
         (%dfloat-hash (float key 1.0d0))) 
        ((immediate-p-macro key)
         (mixup-hash-code (strip-tag-to-fixnum (if (characterp key)(char-upcase key) key))))
        ((bignump key)
         (if (<= most-negative-double-float key most-positive-double-float)
           (%dfloat-hash (float key 1.0d0))  ; with-stack-double-floats
           (%%eqlhash-internal key)))
        ((double-float-p key)
         (%dfloat-hash key))
        ((ratiop key)
         (%ilogxor (%%equalphash (numerator key)) (%%equalphash (denominator key))))
        ((complexp key)
         (%ilogxor (%%equalphash (realpart key)) (%%equalphash (imagpart key))))
        ((hash-table-p key)
         (equalphash-hash-table key))
        ((or (istructp key)
             (structurep key))  ; was (gvectorp key)
         (%%equalphash-structure 11 key))
        ((or (arrayp key)) ;(uvectorp key)) ;??
         (%%equalphash-array 11 key))
        ((consp key)
         (%%equalphash-aux 11 key))
        (t (%%eqlhash key))))


(defun equalphash-hash-table (hash-table)
  (let ((hash (%%equalhash "HASH-TABLE"))
        addressp)
    (declare (fixnum hash))
    (incf hash (the fixnum (%%eqhash (hash-table-count hash-table))))
    (multiple-value-bind (h ap) (%%eqhash (nhash.comparef hash-table))
      (declare (fixnum h))
      (incf hash h)
      (if ap (setq addressp t)))
    (multiple-value-bind (h ap) (%%eqhash (nhash.keytransF hash-table))
      (declare (fixnum h))
      (incf hash h)
      (if ap (setq addressp t)))
    (values hash addressp)))

(defun %%equalphash-structure (limit key)
  (let* ((size (uvsize key))
         (hash (mixup-hash-code size))
         addressp)
    (declare (fixnum limit size hash))
    (dotimes (i size)
      (multiple-value-bind (h ap) (%%equalphash-aux limit (%svref key i))
        (declare (fixnum h))
        (setq hash (the fixnum (+ (the fixnum (rotate-hash-code hash)) h)))
        (if ap (setq addressp t)))
      (when (<= (decf limit) 0)
        (setq hash (the fixnum (+ (the fixnum (rotate-hash-code hash))
                                  (the fixnum (mixup-hash-code 11)))))
        (return)))
    (values hash addressp)))

(defun %%equalphash-array (limit key)
  (multiple-value-bind (array offset) (array-data-and-offset key)
    (let* ((rank (array-rank key))
           (vectorp (eql rank 1))
           (size (if vectorp (length key) (array-total-size key)))
           (hash (mixup-hash-code rank))
           addressp)
      (declare (fixnum size hash limit rank))
      (if vectorp
        (setq hash
              (the fixnum
                   (+ (the fixnum (rotate-hash-code hash))
                      (the fixnum (mixup-hash-code size)))))
        (dotimes (i rank)
          (declare (fixnum i))
          (setq hash
                (the fixnum 
                     (+ (the fixnum (rotate-hash-code hash))
                        (the fixnum
                             (mixup-hash-code (array-dimension key i))))))))      
      (dotimes (i size)
        (declare (fixnum i))
        (multiple-value-bind (h ap) (%%equalphash-aux limit (uvref array offset))
          (declare (fixnum h))
          (setq hash (the fixnum (+ (the fixnum (rotate-hash-code hash)) h)))
          (if ap (setq addressp t)))
        (when (<= (decf limit) 0)
          (setq hash (the fixnum (+ (the fixnum (rotate-hash-code hash))
                                    (the fixnum (mixup-hash-code 11)))))
          (return))
        (incf offset))
      (values hash addressp))))

(defun %%equalphash-aux (limit key)
  (if (<= limit 0) 
    (mixup-hash-code 11)
    (if (null key) (mixup-hash-code 17)
        (cond ((consp key)
               (let ((hash 0)
                     address-p)
                 (do ((l limit (1- l)))
                     ((eq l 0)(values hash address-p))
                   (multiple-value-bind (ahash ap)
                                        (%%equalphash-aux l (if (consp key)(car key) key))
                     (setq hash (mixup-hash-code (logxor ahash hash)))
                     (if ap (setq address-p t)))
                   (when (not (consp key))
                     (return (values hash address-p)))
                   (setq key (cdr key)))))
              ((typep key 'hash-table)
               (equalphash-hash-table key))
              ; what are the dudes called that contain bits? they are uvectors but not gvectors?
              ; ivectors.
              ((or (istructp key)
                   (structurep key))    ;was (gvectorp key)
               (%%equalphash-structure limit key))
              ((or (arrayp key))  ; (uvectorp key))
               (%%equalphash-array limit key))
              (t (%%equalphash key))))))

(defun alist-hash-table (alist &rest hash-table-args)
  (declare (dynamic-extent hash-table-args))
  (if (typep alist 'hash-table)
    alist
    (let ((hash-table (apply #'make-hash-table hash-table-args)))
      (dolist (cons alist) (puthash (car cons) hash-table (cdr cons)))
      hash-table)))

(defun %hash-table-equalp (x y)
  ;; X and Y are both hash tables
  (and (eq (hash-table-test x)
           (hash-table-test y))
       (eql (hash-table-count x)
            (hash-table-count y))
       (block nil
         (let* ((default (cons nil nil))
                (foo #'(lambda (k v)
                         (let ((y-value (gethash k y default)))
                           (unless (and (neq default y-value)
                                        (equalp v y-value))
                             (return nil))))))
           (declare (dynamic-extent foo default))
           (maphash foo x))
         t)))

(defun sxhash (s-expr)
  "Computes a hash code for S-EXPR and returns it as an integer."
  (logand (sxhash-aux s-expr 7 17) target::target-most-positive-fixnum))

(defun sxhash-aux (expr counter key)
  (declare (fixnum counter))
  (if (> counter 0)
    (typecase expr
      ((or string bit-vector number character)  (+ key (%%equalhash expr)))
      (logical-pathname
       (dotimes (i (uvsize expr) key)
         (declare (fixnum i))
         (setq key (+ key (sxhash-aux (%svref expr i) (1- counter) key)))))
      (pathname
       ;; Don't consider %PHYSICAL-PATHNAME-VERSION to be significant
       (dotimes (i (uvsize expr) key)
         (declare (fixnum i))
         (unless (= i %physical-pathname-version)
           (setq key (+ key (sxhash-aux (%svref expr i) (1- counter) key))))))
      (symbol (+ key (%%equalhash (symbol-name expr))))
      (cons (sxhash-aux
             (cdr expr)
             (the fixnum (1- counter))             
             (+ key (sxhash-aux (car expr) (the fixnum (1- counter)) key))))
      (t (+  key (%%equalhash (symbol-name (%type-of expr))))))
    key))



#+(or ppc32-target x8632-target)
(defun immediate-p (thing)
  (let* ((tag (lisptag thing)))
    (declare (fixnum tag))
    (or (= tag target::tag-fixnum)
        (= tag target::tag-imm))))

#+ppc64-target
(defun immediate-p (thing)
  (let* ((tag (lisptag thing)))
    (declare (fixnum tag))
    (or (= tag ppc64::tag-fixnum)
        (= (logand tag ppc64::lowtagmask) ppc64::lowtag-imm))))

#+x8664-target
(defun immediate-p (thing)
  (let* ((tag (lisptag thing)))
    (declare (type (unsigned-byte 3) tag))
    (logbitp tag
             (logior (ash 1 x8664::tag-fixnum)
                     (ash 1 x8664::tag-imm-0)
                     (ash 1 x8664::tag-imm-1)))))



(defun %cons-nhash-vector (size &optional (flags 0))
  (declare (fixnum size))
  (let* ((vector (%alloc-misc (+ (+ size size) $nhash.vector_overhead) target::subtag-hash-vector free-hash-marker)))
    (%init-nhash-vector vector flags)
    vector))

(defun %init-nhash-vector (vector flags)
  (let ((size (vector-index->index (uvsize vector))))
    (declare (fixnum size))
    (setf (nhash.vector.link vector) 0
          (nhash.vector.flags vector) flags
          (nhash.vector.gc-count vector) (%get-gc-count)
          (nhash.vector.free-alist vector) nil
          (nhash.vector.finalization-alist vector) nil
          (nhash.vector.weak-deletions-count vector) 0
          (nhash.vector.hash vector) nil
          (nhash.vector.deleted-count vector) 0
          (nhash.vector.count vector) 0
          (nhash.vector.cache-key vector) free-hash-marker
          (nhash.vector.cache-value vector) nil
          (nhash.vector.cache-idx vector) nil
          (nhash.vector.size vector) size
          (nhash.vector.size-reciprocal vector) (floor (ash 1 (- target::nbits-in-word target::fixnumshift)) size))))

(defun assert-hash-table-readonly (hash)
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (or (nhash.read-only hash)
      (when (nhash.owner hash)
        (error "Hash~table ~s is thread-private and can't be made read-only for that reason" hash))
      (with-lock-context
        (without-interrupts
         (write-lock-hash-table hash)
         (let* ((flags (nhash.vector.flags (nhash.vector hash))))
           (declare (fixnum flags))
           (when (or (logbitp $nhash_track_keys_bit flags)
                     (logbitp $nhash_component_address_bit flags))
             (format t "~&Hash-table ~s uses address-based hashing and can't yet be made read-only for that reason." hash)
             (unlock-hash-table hash nil)
             (return-from assert-hash-table-readonly nil))
           (setf (nhash.read-only hash) t)
           (unlock-hash-table hash nil)
           t)))))

;; This is dangerous, if multiple threads are accessing a read-only
;; hash table. Use it responsibly.
(defun assert-hash-table-writeable (hash)
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (when (nhash.read-only hash)
    (setf (nhash.read-only hash) nil)
    t))

(defun readonly-hash-table-p (hash)
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (nhash.read-only hash))

(defun hash-table-owner (hash)
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (nhash.owner hash))

(defun claim-hash-table (hash &optional steal)
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (let* ((owner (nhash.owner hash)))
    (if owner
      (or (eq owner *current-process*)
          (when steal
            (setf (nhash.owner hash) *current-process*)))
      (progn
        (unless (hash-lock-free-p hash)
          (write-lock-hash-table hash)
          (setf (nhash.exclusion-lock hash) nil))
        (setf (nhash.owner hash) *current-process*)
        t))))

  
;; ** TODO: for lock-free hash tables, we don't need to copy,
;; we could map over the actual hash table vector, because it's
;; always valid.
(defun lock-free-enumerate-hash-keys-and-values (hash keys values)
  (do* ((in (nhash.vector hash))
        (in-idx $nhash.vector_overhead (+ in-idx 2))
        (insize (uvsize in))
        (outsize (length (or keys values)))
        (out-idx 0))
       ((or (= in-idx insize)
            (= out-idx outsize))
        out-idx)
    (declare (fixnum in-idx insize out-idx outsize))
    (let* ((key (%svref in in-idx)))
      (unless (eq key free-hash-marker)
        (let ((val (%svref in (%i+ in-idx 1))))
          (when (eq val rehashing-value-marker)
            ;; This table is being rehashed.  Wait to finish and try again
            (lock-free-rehash hash)
            (return-from lock-free-enumerate-hash-keys-and-values
                         (lock-free-enumerate-hash-keys-and-values hash keys values)))
          (unless (eq val free-hash-marker)
            (when (eql key deleted-hash-key-marker)
              (error "Bug: deleted key but not value?"))
            (when keys (setf (%svref keys out-idx) key))
            (when values (setf (%svref values out-idx) val))
            (incf out-idx)))))))

(defun enumerate-hash-keys-and-values (hash keys values)
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (when (hash-lock-free-p hash)
    (return-from enumerate-hash-keys-and-values
                 (lock-free-enumerate-hash-keys-and-values hash keys values)))
  (with-lock-context
    (without-interrupts
     (let* ((readonly (eq (read-lock-hash-table hash) :readonly)))
       (do* ((in (nhash.vector hash))
             (in-idx $nhash.vector_overhead (+ in-idx 2))
             (insize (uvsize in))
             (outsize (length (or keys values)))
             (out-idx 0))
           ((or (= in-idx insize)
                (= out-idx outsize))
              (unlock-hash-table hash readonly)
              out-idx)
         (declare (fixnum in-idx insize out-idx outsize))
         (let* ((key (%svref in in-idx)))
           (unless (or (eq key free-hash-marker)
                       (eq key deleted-hash-key-marker))
             (when keys
               (setf (%svref keys out-idx) key))
             (when values
               (setf (%svref values out-idx) (%svref in (%i+ in-idx 1))))
             (incf out-idx))))))))

(defun enumerate-hash-keys (hash out)
  (enumerate-hash-keys-and-values hash out nil))
