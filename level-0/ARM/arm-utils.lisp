;;; -*- Mode: Lisp; Package: CCL; -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates
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

(defarmlapfunction %address-of ((arg arg_z))
  ;; %address-of a fixnum is a fixnum, just for spite.
  ;; %address-of anything else is the address of that thing as an integer.
  (test-fixnum arg)
  (mov imm0 arg_z)
  (bxeq lr)
  (tst imm0 (:$ #xc0000000))            ; see if result fits in a fixnum, sorta
  (box-fixnum arg_z imm0)               ; assume it did
  (bxeq lr)                             ; else arg_z tagged ok, but missing bits
  (spjump .SPmakeu32)         ; put all bits in bignum.
)



;;; "areas" are fixnum-tagged and, for the most part, so are their
;;; contents.

;;; The nilreg-relative global all-areas is a doubly-linked-list header
;;; that describes nothing.  Its successor describes the current/active
;;; dynamic heap.  Return a fixnum which "points to" that area, after
;;; ensuring that the "active" pointers associated with the current thread's
;;; stacks are correct.



(defarmlapfunction %normalize-areas ()
  (let ((address imm0)
        (temp imm2))

    ;; Update active pointer for vsp area.
    (ldr address (:@ arm::rcontext (:$ arm::tcr.vs-area)))
    (str vsp (:@ address (:$ arm::area.active)))
    
    ; Update active pointer for SP area
    (ldr arg_z (:@ arm::rcontext (:$ arm::tcr.cs-area)))
    (str sp (:@ arg_z (:$ arm::area.active)))


    (ref-global arg_z all-areas)
    (ldr arg_z (:@ arg_z (:$ arm::area.succ)))

    (bx lr)))

(defarmlapfunction %active-dynamic-area ()
  (ref-global arg_z all-areas)
  (ldr arg_z (:@ arg_z (:$ arm::area.succ)))
  (bx lr))

  
(defarmlapfunction %object-in-stack-area-p ((object arg_y) (area arg_z))
  (ldr imm0 (:@ area (:$ arm::area.active)))
  (ldr imm1 (:@ area (:$ arm::area.high)))
  (mov arg_z (:$ arm::nil-value))
  (cmp object imm0)
  (bxlo lr)
  (cmp object imm1)
  (bxhs lr)
  (add arg_z arg_z (:$ arm::t-offset))
  (bx lr))

(defarmlapfunction %object-in-heap-area-p ((object arg_y) (area arg_z))
  (ldr imm0 (:@ area (:$ arm::area.low)))
  (ldr imm1 (:@ area (:$ arm::area.active)))
  (mov arg_z (:$ arm::nil-value))
  (cmp object imm0)
  (bxlo lr)
  (cmp object imm1)
  (bxhs lr)
  (add arg_z arg_z (:$ arm::t-offset))
  (bx lr))


(defarmlapfunction walk-static-area ((a arg_y) (f arg_z))
  (let ((fun temp0)
        (obj temp1)
        (limit temp2)
        (header imm0)
        (tag imm1)
        (subtag imm2))
    (build-lisp-frame)
    (mov fun f)
    (ldr limit (:@ a (:$ arm::area.active)))
    (ldr obj (:@ a (:$ arm::area.low)))
    (b @test)
    @loop
    (ldr header (:@ obj (:$ 0)))
    (extract-fulltag tag header)
    (cmp tag (:$ arm::fulltag-immheader))    
    (cmpne tag (:$ arm::fulltag-nodeheader))
    (beq @misc)
    (add arg_z obj (:$ arm::fulltag-cons))
    (set-nargs 1)
    (stmdb (:! vsp) (fun obj limit))
    (mov nfn fun)
    (sploadlr .SPfuncall)
    (blx lr)
    (ldmia (:! vsp) (fun obj limit))
    (add obj obj (:$ arm::cons.size))
    (b @test)
    @misc
    (add arg_z obj (:$ arm::fulltag-misc))
    (stmdb (:! vsp) (fun obj limit))
    (set-nargs 1)
    (mov nfn fun)
    (sploadlr .SPfuncall)
    (blx lr)
    (ldmia (:! vsp) (fun obj limit))
    (ldr header (:@ obj (:$ 0)))
    (extract-fulltag tag header)
    (cmp tag (:$ arm::fulltag-nodeheader))
    (extract-lowbyte subtag header)
    (bic header header (:$ arm::subtag-mask))
    (mov header (:lsr header (:$ (- arm::num-subtag-bits arm::word-shift))))
    (beq @bump)
    (cmp subtag (:$ arm::max-32-bit-ivector-subtag))
    (bls @bump)
    (cmp subtag (:$ arm::max-8-bit-ivector-subtag))
    (movls header (:lsr header (:$ 2)))
    (bls @bump)
    (cmp subtag (:$ arm::max-16-bit-ivector-subtag))
    (movls header (:lsr header (:$ 1)))
    (bls @bump)
    (cmp subtag (:$ arm::subtag-double-float-vector))
    (movls header (:lsl header (:$ 1)))
    (bls @bump)
    (mov header (:lsr header (:$ 2)))
    (add header header (:$ 7))
    (mov header (:lsr header (:$ 3)))
    @bump
    (add header header (:$ (+ 4 7)))
    (bic header header (:$ arm::fulltagmask))
    (add obj obj header)
    @test
    (cmp obj limit)
    (blo @loop)
    (return-lisp-frame)))



;;; This walks the active "dynamic" area.  Objects might be moving around
;;; while we're doing this, so we have to be a lot more careful than we 
;;; are when walking a static area.
;;; There's the vague notion that we can't take an interrupt when
;;; "initptr" doesn't equal "freeptr", though what kind of hooks into a
;;; preemptive scheduler we'd need to enforce this is unclear.  We use
;;; initptr as an untagged pointer here (and set it to freeptr when we've
;;; got a tagged pointer to the current object.)
;;; There are a couple of approaches to termination:
;;;  a) Allocate a "sentinel" cons, and terminate when we run into it.
;;;  b) Check the area limit (which is changing if we're consing) and
;;;     terminate when we hit it.
;;; (b) loses if the function conses.  (a) conses.  I can't think of anything
;;; better than (a).
;;; This, of course, assumes that any GC we're doing does in-place compaction
;;; (or at least preserves the relative order of objects in the heap.)

(defarmlapfunction %walk-dynamic-area ((a arg_y) (f arg_z))
  (let ((fun temp1)
        (obj temp0)
        (sentinel temp2)
        (header imm0)
        (tag imm1)
        (subtag imm2))
    (ref-global imm1 tenured-area)   
    (build-lisp-frame)
    (mov allocptr (:$ -8))
    (str allocptr (:@ rcontext (:$ arm::tcr.save-allocbase)))
    (cmp imm1 (:$ 0))
    (mov fun f)
    (movne a imm1)
    (sub allocptr allocptr (:$ (- arm::cons.size arm::fulltag-cons)))
    (ldr imm1 (:@ rcontext (:$ arm::tcr.save-allocbase)))
    (cmp allocptr imm1)
    (bhi @no-trap)
    (uuo-alloc-trap)
    @no-trap
    (mov sentinel allocptr)
    (bic allocptr allocptr (:$ arm::fulltagmask))
    (ldr obj (:@ a (:$ arm::area.low)))
    (b @test)
    @loop
    (test-fixnum obj)
    (beq @no-debug-trap)
    (uuo-debug-trap)
    @no-debug-trap
    (ldr header (:@ obj (:$ 0)))
    (extract-fulltag tag header)
    (cmp tag (:$ arm::fulltag-immheader))    
    (cmpne tag (:$ arm::fulltag-nodeheader))
    (beq @misc)
    (add arg_z obj (:$ arm::fulltag-cons))
    (cmp arg_z sentinel)
    (bhs @done)
    (set-nargs 1)
    (stmdb (:! vsp) (arg_z fun sentinel))
    (mov nfn fun)
    (sploadlr .SPfuncall)
    (blx lr)
    (ldmia (:! vsp) (obj fun sentinel))
    (add obj obj (:$ (- arm::cons.size arm::fulltag-cons)))
    (b @test)
    @misc
    (add arg_z obj (:$ arm::fulltag-misc))
    (stmdb (:! vsp) (arg_z fun sentinel))
    (set-nargs 1)
    (mov nfn fun)
    (sploadlr .SPfuncall)
    (blx lr)
    (ldmia (:! vsp) (obj fun sentinel))
    (sub obj obj (:$ arm::fulltag-misc))
    (ldr header (:@ obj (:$ 0)))
    (extract-fulltag tag header)
    (cmp tag (:$ arm::fulltag-nodeheader))
    (extract-lowbyte subtag header)
    (bic header header (:$ arm::subtag-mask))
    (mov header (:lsr header (:$ (- arm::num-subtag-bits arm::word-shift))))
    (beq @bump)
    (cmp subtag (:$ arm::max-32-bit-ivector-subtag))
    (bls @bump)
    (cmp subtag (:$ arm::max-8-bit-ivector-subtag))
    (movls header (:lsr header (:$ 2)))
    (bls @bump)
    (cmp subtag (:$ arm::max-16-bit-ivector-subtag))
    (movls header (:lsr header (:$ 1)))
    (bls @bump)
    (cmp subtag (:$ arm::subtag-double-float-vector))
    (movls header (:lsl header (:$ 1)))
    (bls @bump)
    (mov header (:lsr header (:$ 2)))
    (add header header (:$ 7))
    (mov header (:lsr header (:$ 3)))
    @bump
    (mov imm2 obj)
    (add header header (:$ (+ 4 7)))
    (bic header header (:$ arm::fulltagmask))
    (add obj obj header)
    @test
    (cmp obj sentinel)
    (blo @loop)
    (uuo-debug-trap)
    @done
    (return-lisp-frame)))



(defun walk-dynamic-area (area func)
  (with-other-threads-suspended
      (%walk-dynamic-area area func)))



(defarmlapfunction %class-of-instance ((i arg_z))
  (svref arg_z instance.class-wrapper i)
  (svref arg_z %wrapper-class arg_z)
  (bx lr))

(defarmlapfunction class-of ((x arg_z))
  (check-nargs 1)
  (extract-fulltag imm0 x)
  (cmp imm0 (:$ arm::fulltag-misc))
  (beq @misc)
  (extract-lowbyte imm0 x)
  (b @done)
  @misc
  (extract-subtag imm0 x)
  @done
  (mov imm0 (:lsl imm0 (:$ arm::word-shift)))
  (ldr temp1 (:@ nfn  '*class-table*))
  (add imm0 imm0 (:$ arm::misc-data-offset))
  (ldr temp1 (:@ temp1 (:$ arm::symbol.vcell)))
  (ldr temp0 (:@ temp1 imm0)) ; get entry from table
  (cmp temp0 'nil)
  (beq @bad)
  ;; functionp?
  (extract-typecode imm1 temp0)
  (cmp imm1 (:$ arm::subtag-function))
  (bne @ret)  ; not function - return entry
  ;; else jump to the fn
  (set-nargs 1)
  (mov nfn temp0)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint)))
  @bad
  (set-nargs 1)
  (ldr fname (:@ nfn 'no-class-error))
  (ldr nfn (:@ fname (:$ arm::symbol.fcell)))
  (ldr pc (:@ nfn (:$ arm::function.entrypoint)))
  @ret
  (mov arg_z temp0)  ; return frob from table
  (bx lr))

(defarmlapfunction full-gccount ()
  (ref-global arg_z tenured-area)
  (cmp arg_z (:$ 0))
  (bne @from-area)
  (ref-global arg_z gc-count)
  (bx lr)
  @from-area
  (ldr arg_z (:@ arg_z (:$ arm::area.gc-count)))
  (bx lr))


(defarmlapfunction gc ()
  (check-nargs 0)
  (mov imm0 (:$ arch::gc-trap-function-gc))
  (uuo-gc-trap)
  (mov arg_z 'nil)
  (bx lr))


;;; Make a list.  This can be faster than doing so by doing CONS
;;; repeatedly, since the latter strategy might triger the GC several
;;; times if N is large.
(defarmlapfunction %allocate-list ((initial-element arg_y) (nconses arg_z))
  (check-nargs 2)
  (build-lisp-frame)
  (mov fn nfn)
  (uuo-kernel-service (:$ arch::error-allocate-list))
  (vpush1 arg_z)
  (vpush1 arg_y)
  (set-nargs 2)
  (spjump .SPnvalret))



(defarmlapfunction egc ((arg arg_z))
  "Enable the EGC if arg is non-nil, disables the EGC otherwise. Return
the previous enabled status. Although this function is thread-safe (in
the sense that calls to it are serialized), it doesn't make a whole lot
of sense to be turning the EGC on and off from multiple threads ..."
  (check-nargs 1)
  (sub imm1 arg 'nil)
  (mov imm0 (:$ arch::gc-trap-function-egc-control))
  (uuo-gc-trap)
  (bx lr))



(defarmlapfunction %configure-egc ((e0size arg_x)
				   (e1size arg_y)
				   (e2size arg_z))
  (check-nargs 3)
  (mov imm0 (:$ arch::gc-trap-function-configure-egc))
  (uuo-gc-trap)
  (bx lr))

(defarmlapfunction purify ()
  (mov imm0 (:$ arch::gc-trap-function-purify))
  (uuo-gc-trap)
  (mov arg_z 'nil)
  (bx lr))


(defarmlapfunction impurify ()
  (mov imm0 (:$ arch::gc-trap-function-impurify))
  (uuo-gc-trap)
  (mov arg_z 'nil)
  (bx lr))

(defarmlapfunction lisp-heap-gc-threshold ()
  "Return the value of the kernel variable that specifies the amount
of free space to leave in the heap after full GC."
  (check-nargs 0)
  (mov imm0 (:$ arch::gc-trap-function-get-lisp-heap-threshold))
  (uuo-gc-trap)
  (spjump .SPmakeu32))

(defarmlapfunction set-lisp-heap-gc-threshold ((new arg_z))
  "Set the value of the kernel variable that specifies the amount of free
space to leave in the heap after full GC to new-value, which should be a
non-negative fixnum. Returns the value of that kernel variable (which may
be somewhat larger than what was specified)."  
  (check-nargs 1)
  (build-lisp-frame)
  (sploadlr .SPgetu32)
  (blx lr)
  (mov imm1 imm0)
  (mov imm0 (:$ arch::gc-trap-function-set-lisp-heap-threshold))
  (uuo-gc-trap)
  (restore-lisp-frame imm1)
  (spjump .SPmakeu32))


(defarmlapfunction use-lisp-heap-gc-threshold ()
  "Try to grow or shrink lisp's heap space, so that the free space is(approximately) equal to the current heap threshold. Return NIL"
  (check-nargs 0) 
  (mov imm0 (:$ arch::gc-trap-function-use-lisp-heap-threshold))
  (uuo-gc-trap)
  (mov arg_z 'nil)
  (bx lr))


(defarmlapfunction freeze ()
  "Do a full GC, then consider all heap-allocated objects which survive to be non-relocatable."
  (check-nargs 0)
  (mov imm0 (:$ arch::gc-trap-function-freeze))
  (uuo-gc-trap)
  (spjump .SPmakeu32))

(defarmlapfunction flash-freeze ()
  "Like FREEZE, but don't GC first."
  (check-nargs 0)
  (mov imm0 (:$ arch::gc-trap-function-flash-freeze))
  (uuo-gc-trap)
  (spjump .SPmakeu32))

(defarmlapfunction allow-heap-allocation ((arg arg_z))
  "If ARG is true, signal an ALLOCATION-DISABLED condition on attempts
at heap allocation."
  (:arglist (arg))
  (check-nargs 1)
  (cmp arg_z (:$ arm::nil-value))
  (mov imm0 (:$ arch::gc-trap-function-allocation-control))
  (mov imm1 (:$ 0))                     ;disallow
  (movne imm1 (:$ 1))                   ;allow if arg non-null
  (uuo-gc-trap)
  (bx lr))



(defarmlapfunction heap-allocation-allowed-p ()
  "Return T if heap allocation is allowed, NIL otherwise."
  (check-nargs 0)
  (mov imm0 (:$ arch::gc-trap-function-allocation-control))
  (mov imm1 (:$ 2))                     ;query
  (uuo-gc-trap)
  (bx lr))

(defun %watch (uvector)
  (declare (ignore uvector))
  (error "watching objects not supported on ARM yet"))

(defun %unwatch (watched new)
  (declare (ignore watched new))
  (error "watching objects not supported on ARM yet"))


  
(defarmlapfunction %ensure-static-conses ()
  (check-nargs 0)
  (mov imm0 (:$ arch::gc-trap-function-ensure-static-conses))
  (uuo-gc-trap)
  (mov arg_z 'nil)
  (bx lr))


;;; offset is a fixnum, one of the arm::kernel-import-xxx constants.
;;; Returns that kernel import, a fixnum.
(defarmlapfunction %kernel-import ((offset arg_z))
  (ref-global imm0 kernel-imports)
  (ldr imm0 (:@ imm0 (:asr arg_z (:$ arm::fixnumshift))))
  (box-fixnum arg_z imm0)
  (bx lr))

(defarmlapfunction %get-unboxed-ptr ((macptr arg_z))
  (macptr-ptr imm0 arg_z)
  (ldr arg_z (:@ imm0 (:$ 0)))
  (bx lr))


(defarmlapfunction %revive-macptr ((p arg_z))
  (mov imm0 (:$ arm::subtag-macptr))
  (strb imm0 (:@ p (:$ arm::misc-subtag-offset)))
  (bx lr))

(defarmlapfunction %macptr-type ((p arg_z))
  (check-nargs 1)
  (trap-unless-xtype= p arm::subtag-macptr)
  (svref imm0 arm::macptr.type-cell p)
  (box-fixnum arg_z imm0)
  (bx lr))
  
(defarmlapfunction %macptr-domain ((p arg_z))
  (check-nargs 1)
  (trap-unless-xtype= p arm::subtag-macptr)
  (svref imm0 arm::macptr.domain-cell p)
  (box-fixnum arg_z imm0)
  (bx lr))

(defarmlapfunction %set-macptr-type ((p arg_y) (new arg_z))
  (check-nargs 2)
  (unbox-fixnum imm1 new)
  (trap-unless-xtype= p arm::subtag-macptr)
  (svset imm1 arm::macptr.type-cell p)
  (bx lr))

(defarmlapfunction %set-macptr-domain ((p arg_y) (new arg_z))
  (check-nargs 2)
  (unbox-fixnum imm1 new)
  (trap-unless-xtype= p arm::subtag-macptr)
  (svset imm1 arm::macptr.domain-cell p)
  (bx lr))

(defarmlapfunction true ()
  (:arglist (&rest ignore))
  (cmp nargs '3)
  (mov arg_z (:$ arm::nil-value))
  (add arg_z arg_z (:$ arm::t-offset))
  (bxls lr)
  (sub imm0 nargs '3)
  (add vsp vsp imm0)
  (bx lr))

(defarmlapfunction false ()
  (:arglist (&rest ignore))
  (cmp nargs '3)
  (mov arg_z (:$ arm::nil-value))
  (bxls lr)
  (sub imm0 nargs '3)
  (add vsp vsp imm0)
  (bx lr))

(defarmlapfunction constant-ref ()
  (:arglist (&rest ignore))
  (cmp nargs '3)
  (ldr arg_z (:@ nfn 'constant))
  (bxls lr)
  (sub imm0 nargs '3)
  (add vsp vsp imm0)
  (bx lr))

;;; end
