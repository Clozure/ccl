;;;-*- Mode: Lisp; Package: CCL -*-
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

(in-package "CCL")

(eval-when (:compile-toplevel :execute)

;;; It's wired in to the code that the length of this vector is 8 and
;;; that its largest element is < 30
(defconstant secondary-keys #(3 5 7 11 13 17 19 23))
(defconstant secondary-keys-*-2 #(6 10 14 22 26 34 38 46))


(defconstant $nhash.lock-free #x80000)

; The hash.vector cell contains a vector with some longwords of overhead
; followed by alternating keys and values.
;; If you change anything here, also update the kernel def in XXX-constantsNN.h
(def-accessors () %svref
  nhash.vector.link                     ; GC link for weak vectors
  nhash.vector.flags                    ; a fixnum of flags
  nhash.vector.gc-count                 ; gc-count kernel global
  nhash.vector.free-alist               ; empty alist entries for finalization
  nhash.vector.finalization-alist       ; deleted out key/value pairs put here
  nhash.vector.weak-deletions-count     ; incremented when the GC deletes an element
  nhash.vector.hash                     ; back-pointer
  nhash.vector.deleted-count            ; if lock-free, hint to GC to delete marked keys.
                                        ; else number of deleted entries
  nhash.vector.count                    ; number of valid entries [not maintained if lock-free]
  nhash.vector.cache-idx                ; index of last cached key/value pair
  nhash.vector.cache-key                ; cached key
  nhash.vector.cache-value              ; cached value
  nhash.vector.size                     ; number of entries in table
  nhash.vector.size-reciprocal          ; shifted reciprocal of nhash.vector.size
  )


; number of longwords of overhead in nhash.vector.
; Must be a multiple of 2 or INDEX parameters in LAP code will not be tagged as fixnums.
(defconstant $nhash.vector_overhead 14)

(defconstant $nhash_weak_bit 12)        ; weak hash table
(defconstant $nhash_weak_value_bit 11)  ; weak on value vice key if this bit set
(defconstant $nhash_finalizeable_bit 10)
(defconstant $nhash_keys_frozen_bit 9)  ; GC must not change key slots when deleting
(defconstant $nhash_weak_flags_mask
  (bitset $nhash_keys_frozen_bit (bitset $nhash_weak_bit (bitset $nhash_weak_value_bit (bitset $nhash_finalizeable_bit 0)))))


(defconstant $nhash_track_keys_bit 28)  ; request GC to track relocation of keys.
(defconstant $nhash_key_moved_bit 27)   ; set by GC if a key moved.
(defconstant $nhash_ephemeral_bit 26)   ; set if a hash code was computed using an address
                                        ; in ephemeral space
(defconstant $nhash_component_address_bit 25) ; a hash code was computed from a key's component



(defconstant $nhash-growing-bit 16)
(defconstant $nhash-rehashing-bit 17)

)

(declare-arch-specific-macro immediate-p-macro)

(declare-arch-specific-macro hashed-by-identity)
          
	 
;; state is #(hash-table index key-vector count)  
(def-accessors %svref
  nhti.hash-table
  nhti.index
  nhti.keys
  nhti.values
  nhti.nkeys)

#+x8632-target
(defconstant +nil-hash+ 201404780)

#-x8632-target
(defconstant +nil-hash+ (mixup-hash-code (%pname-hash "NIL" 3)))







