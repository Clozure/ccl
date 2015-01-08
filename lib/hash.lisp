;;;-*- Mode: Lisp; Package: CCL -*-
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

;;  This is just the stuff (make-load-form, print-object) that can't be fasloaded earlier.


;;;;;;;;;;;;;
;;
;; hash.lisp
;; New hash table implementation

;;;;;;;;;;;;;
;;
;; Things I didn't do
;;
;; Save the 32-bit hash code along with the key so that growing the table can
;; avoid calling the hashing function (at least until a GC happens during growing).
;;
;; Maybe use Knuth's better method for hashing:
;; find two primes N-2, N.  N is the table size.
;; First probe is at primary = (mod (funcall (nhash.keytransF h) key) N)
;; Secondary probes are spaced by (mod (funcall (nhash.keytransF h) key) N-2)
;; This does a bit better scrambling of the secondary probes, but costs another divide.
;;
;; Rethink how finalization is reported to the user.  Maybe have a finalization function which
;; is called with the hash table and the deleted key & value.


;;;;;;;;;;;;;
;;
;; Documentation
;;
;; MAKE-HASH-TABLE is extended to accept a :HASH-FUNCTION keyword arg which
;; defaults for the 4 Common Lisp defined :TEST's.  Also, any fbound symbol can
;; be used for the :TEST argument.  The HASH-FUNCTION is a function of one
;; argument, the key, which returns one or two values:
;;
;; 1) HASH-CODE
;; 2) ADDRESSP
;;
;; The HASH-CODE can be any object.  If it is a relocateable object (not a
;; fixnum, short float, or immediate) then ADDRESSP will default to :KEY
;; and it is an error if NIL is returned for ADDRESSP.
;;
;; If ADDRESSP is NIL, the hashing code assumes that no addresses were used
;; in computing the HASH-CODE.  If ADDRESSP is :KEY (which is the default
;; if the hash function returns only one value and it is relocateable) then
;; the hashing code assumes that only the KEY's address was used to compute
;; the HASH-CODE.  Otherwise, it is assumed that the address of a
;; component of the key was used to compute the HASH-CODE.
;;
;;
;;
;; Some (proposed) functions for using in user hashing functions:
;;
;; (HASH-CODE object)
;;
;; returns two values:
;;
;; 1) HASH-CODE
;; 2) ADDRESSP
;;
;; HASH-CODE is the object transformed into a fixnum by changing its tag
;; bits to a fixnum's tag.  ADDRESSP is true if the object was
;; relocateable. ;;
;;
;; (FIXNUM-ADD o1 o2)
;; Combines two objects additively and returns a fixnum.
;; If the two objects are fixnums, will be the same as (+ o1 o2) except
;; that the result can not be a bignum.
;;
;; (FIXNUM-MULTIPLY o1 o2)
;; Combines two objects multiplicatively and returns a fixnum.
;;
;; (FIXNUM-FLOOR dividend &optional divisor)
;; Same as Common Lisp's FLOOR function, but converts the objects into
;; fixnums before doing the divide and returns two fixnums: quotient &
;; remainder.
;;
;;;;;;;;;;;;;
;;
;; Implementation details.
;;
;; Hash table vectors have a header that the garbage collector knows
;; about followed by alternating keys and values.  Empty slots have a
;; key of (%UNBOUND-MARKER), deleted slots are denoted by a key of
;; (%SLOT-UNBOUND-MARKER), except in the case of "lock-free" hash
;; tables, which see below.
;;
;; Four bits in the nhash.vector.flags fixnum interact with the garbage
;; collector.  This description uses the symbols that represent bit numbers
;; in a fixnum.  $nhash_xxx_bit has a corresponding $nhash_lap_xxx_bit which
;; gives the byte offset of the bit for LAP code.  The two bytes in
;; question are at offsets $nhash.vector-weak-byte and
;; $nhash.vector-track-keys-byte offsets from the tagged vector.
;; The raw 32 bits of the fixnum at nhash.vector.flags look like:
;;
;;     TKEC0000 00000000 WVFZ0000 00000000
;;
;;
;; $nhash_track_keys_bit         "T" in the diagram above
;;                               Sign bit of the longword at $nhash.vector.flags
;;                               or the byte at $nhash.vector-track-keys-byte.
;;                               If set, GC tracks relocation of keys in the
;;                               vector.
;; $nhash_key_moved_bit          "K" in the diagram above
;;                               Set by GC to indicate that a key moved.
;;                               If $nhash_track_keys_bit is clear, this bit is set to
;;                               indicate that any GC will require a rehash.
;;                               GC never clears this bit, but may set it if
;;                               $nhash_track_keys_bit is set.
;; $nhash_component_address_bit  "C" in the diagram above.
;;                               Ignored by GC.  Set to indicate that the
;;                               address of a component of a key was used. 
;;                               Means that $nhash_track_keys_bit will
;;                               never be set until all such keys are
;;                               removed.
;; $nhash_weak_bit               "W" in the diagram above
;;                               Sign bit of the byte at $nhash.vector-weak-byte
;;                               Set to indicate a weak hash table
;; $nhash_weak_value_bit         "V" in the diagram above
;;                               If clear, the table is weak on key
;;                               If set, the table is weak on value
;; $nhash_finalizeable_bit       "F" in the diagram above
;;                               If set the table is finalizeable:
;;                               If any key/value pairs are removed, they will be added to
;;                               the nhash.vector.finalization-alist using cons cells
;;                               from nhash.vector.free-alist
;; $nhash_keys_frozen_bit       "Z" in diagram above.
;;                               If set, GC will remove weak entries by setting the
;;                               value to (%slot-unbound-marker), leaving key unchanged.

(in-package "CCL")


(eval-when (:compile-toplevel :execute)
  (require "HASHENV" "ccl:xdump;hashenv"))

(defvar *hash-table-class*
  (progn
;    #+sparc-target (dbg)
    (find-class 'hash-table)))

(setf (type-predicate 'hash-table) 'hash-table-p)


(defmethod print-object ((table hash-table) stream)
  (print-unreadable-object (table stream :type t :identity t)
    (format stream "~S ~S size ~D/~D"
            ':test (hash-table-test table)
            (hash-table-count table)
            (hash-table-size table))
    (when (readonly-hash-table-p table)
      (format stream " (Readonly)"))))


#+vaporware
;;; Of course, the lisp version of this would be too slow ...
(defun hash-table-finalization-list (hash-table)
  (unless (hash-table-p hash-table)
    (report-bad-arg hash-table 'hash-table))
  (let* ((vector (nhash.vector hash-table))
         (flags (nhash.vector.flags vector)))
    (declare (fixnum flags))
    (if (logbitp $nhash_finalizeable_bit flags)
      (nhash.vector.finalization-alist vector)
      (error "~S is not a finalizeable hash table" hash-table))))

#+vaporware
(defun (setf hash-table-finalization-list) (value hash-table)
  (unless (hash-table-p hash-table)
    (report-bad-arg hash-table 'hash-table))
  (let* ((vector (nhash.vector hash-table))
         (flags (nhash.vector.flags vector)))
    (declare (fixnum flags))
    (if (logbitp $nhash_finalizeable_bit flags)
      (setf (nhash.vector.finalization-alist vector) value)
      (error "~S is not a finalizeable hash table" hash-table))))

(defsetf gethash puthash)

; Returns nil, :key or :value
(defun hash-table-weak-p (hash)
  (unless (hash-table-p hash)
    (setq hash (require-type hash 'hash-table)))
  (let* ((vector (nhash.vector hash))
         (flags (nhash.vector.flags vector)))
    (when (logbitp $nhash_weak_bit flags)
      (if (logbitp $nhash_weak_value_bit flags)
        :value
        :key))))

;;; It would be pretty complicated to offer a way of doing (SETF
;;; HASH-TABLE-WEAK-P) after the hash-table's been created, and
;;; it's not clear that that'd be incredibly useful.



;;;;;;;;;;;;;
;;
;; Mapping functions
;;



(defun next-hash-table-iteration-1 (state)
  (do* ((index (nhti.index state) (1+ index))
        (keys (nhti.keys state))
        (values (nhti.values state))
        (nkeys (nhti.nkeys state)))
       ((>= index nkeys)
        (setf (nhti.index state) nkeys)
        nil)
    (declare (fixnum index nkeys)
             (simple-vector keys))
    (let* ((key (svref keys index))
           (value (svref values index)))
        (setf (nhti.index state) (1+ index))
        (return (values t key value)))))



(defun maphash (function hash-table)
  "For each entry in HASH-TABLE, call the designated two-argument function
   on the key and value of the entry. Return NIL."
  (with-hash-table-iterator (m hash-table)
    (loop
      (multiple-value-bind (found key value) (m)
        (unless found (return))
        (funcall function key value)))))



(defmethod make-load-form ((hash hash-table) &optional env)
  (declare (ignore env))
  (let ((keytransF (nhash.keytransF hash))
        (compareF (nhash.compareF hash))
        (vector (nhash.vector hash))
        (private (if (nhash.owner hash) '*current-process*))
        (lock-free-p (hash-lock-free-p hash)))

    (flet ((convert (f)
             (if (or (fixnump f) (symbolp f))
               `',f
               `(symbol-function ',(function-name f)))))
      (values
       `(%cons-hash-table
         nil nil nil ,(nhash.grow-threshold hash) ,(nhash.rehash-ratio hash) ,(nhash.rehash-size hash)
        nil nil ,private ,lock-free-p ,(nhash.min-size hash))
       `(%initialize-hash-table ,hash ,(convert keytransF) ,(convert compareF) ',vector)))))

(defun needs-rehashing (hash)
  (%set-needs-rehashing hash))

(defun %initialize-hash-table (hash keytransF compareF vector)
  (setf (nhash.keytransF hash) keytransF
        (nhash.compareF hash) compareF)
  (setf (nhash.find hash)
        (case comparef
          (0 #'eq-hash-find)
          (-1 #'eql-hash-find)
          (t #'general-hash-find))
        (nhash.find-new hash)
        (case comparef
          (0 #'eq-hash-find-for-put)
          (-1 #'eql-hash-find-for-put)
          (t #'general-hash-find-for-put)))
  (setf (nhash.vector hash) vector)
  (%set-needs-rehashing hash))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support for locking hash tables while fasdumping
;;


(defun fasl-lock-hash-table (hash-table)
  (setq hash-table (require-type hash-table 'hash-table))
  (without-interrupts
   (let* ((lock (nhash.exclusion-lock hash-table)))
     (if lock
       (progn
         (if (hash-lock-free-p hash-table)
           ;; For lock-free hash tables, this only makes sure nobody is
           ;; rehashing the table.  It doesn't necessarily stop readers
           ;; or writers (unless they need to rehash).
           (grab-lock lock)
           (write-lock-rwlock lock))
         (push hash-table *fcomp-locked-hash-tables*))
       (unless (eq (nhash.owner hash-table) *current-process*)
         (error "Current process doesn't own hash-table ~s" hash-table))))))

(defun fasl-unlock-hash-tables ()
  (dolist (h *fcomp-locked-hash-tables*)
    (let* ((lock (nhash.exclusion-lock h)))
      (if (hash-lock-free-p h)
        (release-lock lock)
        (unlock-rwlock lock)))))

; end
