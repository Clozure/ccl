;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2005 Clozure Associates
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

;;; Low-level support for hash-consing.

(in-package "CCL")

(defpackage "OPENMCL-HONS"
  (:use "CL")
  (:nicknames "HONS")
  (:export "HONS-INDEX-USED-P" "HONS-SPACE-DELETED-MARKER"
           "HONS-SPACE-FREE-MARKER"
           "HONS-SPACE-SIZE" "HONSP" "HONS-FROM-INDEX"
            "HONS-SPACE-REF-CAR" "HONS-SPACE-REF-CDR"
           "HONS-SPACE-CONS" "DELETED-HONS-COUNT" "INVALID-HONS-INDEX"
           "INVALID-HONS-INDEX-INDEX"))


;;; At this level. the API is basically:
;;;
;;;
;;; (OPENMCL-HONS:HONS-SPACE-DELETED-MARKER) [MACRO]
;;; Returns another constant value used to indicate a
;;; "deleted" cell in a HONS hash table; the CAR and CDR of
;;; a pair are set to this value by the GC if the HONS which
;;; addresses that pair becomes garbage.  This value is used
;;; in Clozure CL to denote unbound slots in STANDARD-INSTANCEs,
;;; so setting a slot in a standard-instance to this value
;;; is roughly equivalent to calling SLOT-MAKUNBOUND.  This
;;; value prints as #<Slot-Unbound>.
;;;
;;; (OPENMCL-HONS:HONS-SPACE-FREE-MARKER) [MACRO]
;;; Returns another constant value used to indicate a
;;; "free" cell in a HONS hash table; the CAR and CDR of
;;; a pair are initially set to this value by the GC if the HONS which
;;; addresses that pair becomes garbage.  This value is used
;;; in Clozure CL to denote unbound special variabls
;;; setting a special variable to this value
;;; is roughly equivalent to calling MAKUNBOUND.  This
;;; value prints as #<Unbound>.

;;; (OPENCL-HONS:HONS-SPACE-SIZE)
;;; Returns a non-negative integer denoting the number of
;;; statically allocated pairs reserved for hash consing.
;;;
;;; OPENMCL-HONS:HONS-SPACE-SIZE can be used with SETF, to specify a
;;; new size in pairs.  The new size should be a non-negative
;;; fixnum.  If the new size is less than the current size,
;;; any references to HONSes whose index is between the
;;; current and new size will be set to NIL.           
;;; Otherwise, any newly allocated pairs will have their CAR and CDR both
;;; set to the value returned by (OPENMCL-HONS:HONS-SPACE-FREE).
;;;
;;; (OPENMCL-HONS:HONSP <thing>)
;;; If <thing> is a CONS and is allocated within hons-space,
;;; returns the index of the pair addressed by <thing> (e.g.,
;;; the return value will be a non-negative integer less than
;;; (OPENMCL-HONS:HONS-SPACE-SIZE).  If <thing> is not a CONS or is not
;;; allocated within hons-space, returns NIL.
;;;
;;; (OPENCL-HONS:HONS-FROM-INDEX <index>) If <index> is a non-negative
;;; integer less than (OPENMCL-HONS:HONS-SPACE-SIZE), returns a
;;; CONS-typed pointer to the <index>th pair in hons-space.  (If
;;; <thing> is a HONS, then (EQ (OPENMCL-HONS:HONS-FROM-INDEX
;;; (OPENMCL-HONS:HONSP <thing>)) <thing>) is true).  Signals an error
;;; of type OPENMCL-HONS:INVALID-HONS-INDEX if <index> is a fixnum but
;;; not a valid index.  Signals a TYPE-ERROR if <index> is not a fixum.
;;;
;;; (OPENMCL-HONS:HONS-SPACE-REF-CAR <index>)
;;; (OPENMCL-HONS:HONS-SPACE-REF-CDR <index>)
;;; Semantically equivalent to (CAR (OPENMCL-HONS:HONS-FROM-INDEX <index>)) and
;;; (CDR (OPENMCL-HONS:HONS-FROM-INDEX <index>)), respectively.  (May not be
;;; implemented in a way that actually calls OPENMCL-HONS:HONS-FROM-INDEX.)
;;;
;;; (OPENMCL-HONS:HONS-SPACE-CONS <index> <new-car> <new-cdr>)
;;; Equivalent to:
;;; (let* ((x (OPENMCL-HONS:HONS-FROM-INDEX <index>)))
;;;   (setf (car x) <new-car>
;;;         (cdr x) <new-cdr>)
;;;   x)
;;;
;;; (OPENMCL-HONS:HONS-INDEX-USED-P <index>)
;;; If <index> is a valid index, returns a Lisp boolean indicating
;;; whether or not
;;; (a) OPENMCL-HONS:HONS-FROM-INDEX has been called on it
;;; and (b) the GC has not marked the index as being deleted
;;; are both true.

;;; (OPENMCL-HONS:DELETED-HONS-COUNT)
;;; Returns the total number of pairs in hons space that the GC has deleted
;;; (because they were unreachable); a "deleted" pair has its CAR and CDR
;;; set to the value of (OPENMCL-HONS:HONS-DELETED-MARKER), but (since these
;;; things are statically allocated) the space that the pair occupied remains
;;; part of hons space.
;;; Information about the number of deleted pairs may help to guide hashing
;;; algorithms, but it's not yet clear whether this global count is that
;;; useful; it may be replaced or extended in the future.


(define-condition openmcl-hons:invalid-hons-index ()
  ((index :initarg :index :reader openmcl-hons:invalid-hons-index-index))
  (:report (lambda (c s)
             (format s "Invalid HONS index ~s ."
                     (openmcl-hons:invalid-hons-index-index c)))))


(defmacro openmcl-hons:hons-space-deleted-marker ()
  "Returns the value used to indicate deleted HONS cells."
  (%slot-unbound-marker))

(defmacro openmcl-hons:hons-space-free-marker ()
  "Returns the value used to indicate free HONS cells."
  (%unbound-marker))

(defun (setf openmcl-hons:hons-space-size) (npairs)
  "Argument NPAIRS should be a non-negative fixnum.  Tries to grow or
   shrink the static hons area so that it contains NPAIRS pairs.
   NPAIRS may be rounded to the next multiple of the machine word size.
   Returns the number of pairs in the HONS space after it's made the
   (possibly unsuccessful) attempt.  (Attempts to increase HONS space
   size may fail if insufficient address space is available.)
   If NPAIRS is less than the current hons space size, any \"dangling\"
   references to HONS cells in the deleted region will be set to NIL."
  (check-type npairs (integer 0 #.(- (1+ target::most-positive-fixnum)
                                     target::nbits-in-word)))
  (set-hons-space-size npairs))

#+ppc-target
(defppclapfunction set-hons-space-size ((npairs arg_z))
  (check-nargs 1)
  (mflr loc-pc)
  #+ppc32-target
  (bla .SPgetu32)
  #+ppc64-target
  (bla .SPgetu64)
  (mtlr loc-pc)
  (mr imm1 imm0)
  (li imm0 arch::gc-trap-function-set-hons-area-size)
  (trlgei allocptr 0)
  #+ppc32-target
  (ba .SPmakeu32)
  #+ppc64-target
  (ba .SPmakeu64))


#+x8664-target
(defx86lapfunction set-hons-space-size ((npairs arg_z))
  (check-nargs 1)
  (save-simple-frame)
  (call-subprim .SPgetu64)
  (movq (% imm0) (% imm1))
  (movq ($ arch::gc-trap-function-set-hons-area-size) (% imm0))
  (uuo-gc-trap)
  (restore-simple-frame)
  (jmp-subprim .SPmakeu64))

(defun openmcl-hons:hons-space-size ()
  "Returns the current size of the static hons area."
  (%fixnum-ref-natural (%get-kernel-global 'tenured-area)
                       target::area.static-dnodes))

#+ppc-target
(defppclapfunction openmcl-hons:honsp ((thing arg_z))
  "If THING is a CONS cell allocated in the hons area, return an integer
   which denotes that cell's index in hons space - an integer between
   0 (inclusive) and the hons-space size (exclusive).  Otherwise, return
   NIL."
  (check-nargs 1)
  (extract-fulltag imm2 thing)
  (ref-global imm0 tenured-area)
  (cmpri cr2 imm2 target::fulltag-cons)
  (ldr imm1 target::area.static-dnodes imm0)
  (ldr imm0 target::area.low imm0)
  (slri imm1 imm1 (1+ target::word-shift))
  (bne cr2 @no)
  (add imm1 imm0 imm1)
  (cmpr cr0 thing imm0)
  (cmpr cr1 thing imm1)
  (blt cr0 @no)
  (bgt cr1 @no)
  (subi arg_z arg_z target::fulltag-cons)
  (sub arg_z arg_z imm0)
  (srri arg_z arg_z 1)
  (blr)
  @no
  (li arg_z nil)
  (blr))

#+x8664-target
(defx86lapfunction openmcl-hons:honsp ((thing arg_z))
  "If THING is a CONS cell allocated in the hons area, return an integer
   which denotes that cell's index in hons space - an integer between
   0 (inclusive) and the hons-space size (exclusive).  Otherwise, return
   NIL."
  (check-nargs 1)
  (extract-fulltag thing imm1)
  (ref-global tenured-area imm0)
  (cmpb ($ target::fulltag-cons) (% imm1.b))
  (movq (@ target::area.static-dnodes (% imm0)) (% imm1))
  (movq (@ target::area.low (% imm0)) (% imm0))
  (jne @no)
  (shr ($ (1+ target::word-shift)) (% imm1))
  (add (% imm0) (% imm1))
  (rcmpq (% thing) (% imm0))
  (jb @no)
  (rcmpq (% thing) (% imm1))
  (jae @no)
  (subq ($ target::fulltag-cons) (% arg_z))
  (subq (% imm0) (% arg_z))
  (shr ($ 1) (% arg_z))
  (single-value-return)
  @no
  (movq ($ nil) (% arg_z))
  (single-value-return))

#+ppc-target
(defppclapfunction openmcl-hons:hons-from-index ((index arg_z))
  "If INDEX is a fixnum between 0 (inclusive) and the current hons space size
   (exclusive), return a statically allocated CONS cell.  Otherwise, signal
   an error."
  (check-nargs 1)
  (extract-lisptag imm0 index)
  (cmpri cr0 index 0)
  (cmpri cr1 imm0 target::tag-fixnum)
  (ref-global imm0 tenured-area)
  (unbox-fixnum imm1 arg_z)
  (ldr imm2 target::area.static-dnodes imm0)
  (bne cr1 @bad)
  (cmpr cr2 imm1 imm2)
  (blt cr0 @bad)
  (ldr imm2 target::area.static-used imm0)
  (ldr imm0 target::area.low imm0)
  (bge cr2 @bad)
  (add arg_z index index)
  (add arg_z imm0 arg_z)
  (la arg_z target::fulltag-cons arg_z)
  (sub imm0 arg_z imm0)
  (set-bit-at-index imm2 imm0)
  (blr)
  @bad
  (save-lisp-context)
  (load-constant arg_x openmcl-hons:invalid-hons-index)
  (load-constant arg_y :index)
  (set-nargs 3)
  (load-constant fname error)
  (bla .SPjmpsym)
  (ba .SPpopj))

#+x8664-target
(defx86lapfunction openmcl-hons:hons-from-index ((index arg_z))
  "If INDEX is a fixnum between 0 (inclusive) and the current hons space size
   (exclusive), return a statically allocated CONS cell.  Otherwise, signal
   an error."
  (check-nargs 1)
  (testb ($ x8664::fixnummask) (%b index))
  (ref-global tenured-area temp0)
  (jne @bad)
  (unbox-fixnum index imm1)
  (rcmpq (% imm1) (@ target::area.static-dnodes (% temp0)))
  (jae @bad)
  (shl ($ 1) (% index))
  (movq (% index) (% imm0))
  (addq (@ target::area.low (% temp0)) (% index))
  (addq ($ target::fulltag-cons) (% arg_z))
  (movq (@ target::area.static-used (% temp0)) (% temp0))
  (movq (% imm1) (% imm0))
  (andl ($ 63) (% imm0))
  (xorb ($ 63) (%b imm0))
  (shrq ($ 6) (% imm1))
  (lock)
  (btsq (% imm0) (@ (% temp0) (% imm1) 8))
  (single-value-return)
  @bad
  (save-simple-frame)
  (load-constant openmcl-hons:invalid-hons-index arg_x)
  (load-constant :index arg_y)
  (call-symbol error 3)
  (restore-simple-frame)
  (single-value-return))



#+ppc-target
(defppclapfunction openmcl-hons:hons-index-used-p ((index arg_z))
  "If INDEX is a fixnum between 0 (inclusive) and the current hons space size
   (exclusive), return a boolean indicating whether the pair is used.
   Otherwise, signal an error."
  (check-nargs 1)
  (extract-lisptag imm0 index)
  (cmpri cr0 index 0)
  (cmpri cr1 imm0 target::tag-fixnum)
  (ref-global imm0 tenured-area)
  (unbox-fixnum imm1 arg_z)
  (ldr imm2 target::area.static-dnodes imm0)
  (bne cr1 @bad)
  (cmpr cr2 imm1 imm2)
  (blt cr0 @bad)
  (ldr imm2 target::area.static-used imm0)
  (ldr imm0 target::area.low imm0)
  (bge cr2 @bad)
  (add imm0 index index)
  (test-bit-at-index imm2 imm0)
  (li arg_z nil)
  (beqlr)
  (li arg_z t)
  (blr)
  @bad
  (save-lisp-context)
  (load-constant arg_x openmcl-hons:invalid-hons-index)
  (load-constant arg_y :index)
  (set-nargs 3)
  (load-constant fname error)
  (bla .SPjmpsym)
  (ba .SPpopj))

#+x8664-target
(defx86lapfunction openmcl-hons:hons-index-used-p ((index arg_z))
  "If INDEX is a fixnum between 0 (inclusive) and the current hons space size
   (exclusive), return a boolean indicating whether the pair is used.
   Otherwise, signal an error."
  (check-nargs 1)
  (testb ($ x8664::fixnummask) (%b index))
  (ref-global tenured-area temp0)
  (jne @bad)
  (unbox-fixnum index imm1)
  (rcmpq (% imm1) (@ target::area.static-dnodes (% temp0)))
  (jae @bad)
  (movq (@ target::area.static-used (% temp0)) (% temp0))
  (movq (% imm1) (% imm0))
  (andl ($ 63) (% imm0))
  (xorb ($ 63) (%b imm0))
  (shrq ($ 6) (% imm1))
  (btq (% imm0) (@ (% temp0) (% imm1) 8))
  (movl ($ x8664::t-value) (%l imm0))
  (leaq (@ (- x8664::t-offset) (% imm0)) (% arg_z))
  (cmovbl (%l imm0) (%l arg_z))
  (single-value-return)
  @bad
  (save-simple-frame)
  (load-constant openmcl-hons:invalid-hons-index arg_x)
  (load-constant :index arg_y)
  (call-symbol error 3)
  (restore-simple-frame)
  (single-value-return))


#+ppc-target
(defppclapfunction openmcl-hons:hons-space-ref-car ((index arg_z))
  "If INDEX is in bounds (non-negative and less than the current hons-space size),
   return the CAR of the pair at that index.  The return value could be any
   lisp object, or (HONS-SPACE-DELETED-MARKER).
   If INDEX is not in bounds, an error is signaled."
  (check-nargs 1)
  (extract-lisptag imm0 index)
  (cmpri cr0 index 0)
  (cmpri cr1 imm0 target::tag-fixnum)
  (ref-global imm0 tenured-area)
  (unbox-fixnum imm1 arg_z)
  (ldr imm2 target::area.static-dnodes imm0)
  (bne cr1 @bad)
  (cmpr cr2 imm1 imm2)
  (blt cr0 @bad)
  (ldr imm0 target::area.low imm0)
  (bge cr2 @bad)
  (add arg_z index index)
  (add imm0 imm0 arg_z)
  (ldr arg_z (+ target::cons.car target::fulltag-cons) imm0)
  (blr)
  @bad
  (save-lisp-context)
  (load-constant arg_x openmcl-hons:invalid-hons-index)
  (load-constant arg_y :index)
  (set-nargs 3)
  (load-constant fname error)
  (bla .SPjmpsym)
  (ba .SPpopj))

#+x8664-target
(defx86lapfunction openmcl-hons:hons-space-ref-car ((index arg_z))
  "If INDEX is in bounds (non-negative and less than the current hons-space size),
   return the CAR of the pair at that index.  The return value could be any
   lisp object, or (HONS-SPACE-DELETED-MARKER).
   If INDEX is not in bounds, an error is signaled."
  (check-nargs 1)
  (testb ($ x8664::fixnummask) (%b index))
  (ref-global tenured-area temp0)
  (jne @bad)
  (unbox-fixnum index imm1)
  (rcmpq (% imm1) (@ target::area.static-dnodes (% temp0)))
  (jae @bad)
  (shlq ($ 1) (% index))
  (addq (@ target::area.low (% temp0)) (% arg_z))
  (movq (@ (+ target::cons.car target::fulltag-cons) (% arg_z)) (% arg_z))
  (single-value-return)
  @bad
  (save-simple-frame)
  (load-constant openmcl-hons:invalid-hons-index arg_x)
  (load-constant :index arg_y)
  (call-symbol error 3)
  (restore-simple-frame)
  (single-value-return))

#+ppc-target
(defppclapfunction openmcl-hons:hons-space-ref-cdr ((index arg_z))
  "If INDEX is in bounds (non-negative and less than the current hons-space size),
   return the CAR of the pair at that index.  The return value could be any
   lisp object, or either (HONS-SPACE-FREE-MARKER) or (HONS-SPACE-DELETED-MARKER).
   If INDEX is not in bounds, an error is signaled."
  (check-nargs 1)
  (extract-lisptag imm0 index)
  (cmpri cr0 index 0)
  (cmpri cr1 imm0 target::tag-fixnum)
  (ref-global imm0 tenured-area)
  (unbox-fixnum imm1 arg_z)
  (ldr imm2 target::area.static-dnodes imm0)
  (bne cr1 @bad)
  (cmpr cr2 imm1 imm2)
  (blt cr0 @bad)
  (ldr imm0 target::area.low imm0)
  (bge cr2 @bad)
  (add arg_z index index)
  (add imm0 imm0 arg_z)
  (ldr arg_z (+ target::cons.cdr target::fulltag-cons) imm0)
  (blr)
  @bad
  (save-lisp-context)
  (load-constant arg_x openmcl-hons:invalid-hons-index)
  (load-constant arg_y :index)
  (set-nargs 3)
  (load-constant fname error)
  (bla .SPjmpsym)
  (ba .SPpopj))

#+x8664-target
(defx86lapfunction openmcl-hons:hons-space-ref-cdr ((index arg_z))
  "If INDEX is in bounds (non-negative and less than the current hons-space size),
   return the CDR of the pair at that index.  The return value could be any
   lisp object, or (HONS-SPACE-DELETED-MARKER).
   If INDEX is not in bounds, an error is signaled."
  (check-nargs 1)
  (testb ($ x8664::fixnummask) (%b index))
  (ref-global tenured-area temp0)
  (jne @bad)
  (unbox-fixnum index imm1)
  (rcmpq (% imm1) (@ target::area.static-dnodes (% temp0)))
  (jae @bad)
  (shlq ($ 1) (% index))
  (addq (@ target::area.low (% temp0)) (% arg_z))
  (movq (@ (+ target::cons.cdr target::fulltag-cons) (% arg_z)) (% arg_z))
  (single-value-return)
  @bad
  (save-simple-frame)
  (load-constant openmcl-hons:invalid-hons-index arg_x)
  (load-constant :index arg_y)
  (call-symbol error 3)
  (restore-simple-frame)
  (single-value-return))




(defun openmcl-hons:hons-space-cons (index new-car new-cdr)
  "Return a CONS cell with the specified NEW-CAR and NEW-CDR,
   allocated at the INDEXth pair in hons space."
  (let* ((hons (openmcl-hons:hons-from-index index)))
    (setf (car hons) new-car
          (cdr hons) new-cdr)
    hons))

;;; We might have multiple (logical) tables in hons space, and
;;; would probably like to know how many pairs had been deleted
;;; from each table.  (How to express that to the GC in some
;;; way that would allow it to efficiently track this is an
;;; open question.)  For now, the GC just maintains a global
;;; count of static pairs that it's deleted.
(defun openmcl-hons:deleted-hons-count ()
  "Returns the total number of pairs in hons space that have
   been deleted by the GC."
  (%get-kernel-global 'deleted-static-pairs))

(defun (setf openmcl-hons:deleted-hons-count) (new)
  (check-type new (and fixnum unsigned-byte))
  (%set-kernel-global 'deleted-static-pairs new))

(provide "HASH-CONS")
