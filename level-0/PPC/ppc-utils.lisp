;;; -*- Mode: Lisp; Package: CCL; -*-
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

#+ppc32-target
(defppclapfunction %address-of ((arg arg_z))
  ;; %address-of a fixnum is a fixnum, just for spite.
  ;; %address-of anything else is the address of that thing as an integer.
  (clrlwi. imm0 arg (- 32 ppc32::nlisptagbits))
  (beqlr cr0)
  (mr imm0 arg_z)
  ;; set cr0_eq if result fits in a fixnum
  (clrrwi. imm1 imm0 (- ppc32::least-significant-bit ppc32::nfixnumtagbits))
  (box-fixnum arg_z imm0)               ; assume it did
  (beqlr+ cr0)                          ; else arg_z tagged ok, but missing bits
  (ba .SPmakeu32)         ; put all bits in bignum.
)

#+ppc64-target
(defppclapfunction %address-of ((arg arg_z))
  ;; %address-of a fixnum is a fixnum, just for spite.
  ;; %address-of anything else is the address of that thing as an integer.
  (clrldi. imm0 arg (- 64 ppc64::nlisptagbits))
  (beqlr cr0)
  (mr imm0 arg_z)
  ;; set cr0_eq if result fits in a fixnum
  (clrrdi. imm1 imm0 (- ppc64::least-significant-bit ppc64::nfixnumtagbits))
  (box-fixnum arg_z imm0)               ; assume it did
  (beqlr+ cr0)                          ; else arg_z tagged ok, but missing bits
  (ba .SPmakeu64)         ; put all bits in bignum.
)

;;; "areas" are fixnum-tagged and, for the most part, so are their
;;; contents.

;;; The nilreg-relative global all-areas is a doubly-linked-list header
;;; that describes nothing.  Its successor describes the current/active
;;; dynamic heap.  Return a fixnum which "points to" that area, after
;;; ensuring that the "active" pointers associated with the current thread's
;;; stacks are correct.



(defppclapfunction %normalize-areas ()
  (let ((address imm0)
        (temp imm2))

    ; update active pointer for tsp area.
    (ldr address target::tcr.ts-area target::rcontext)
    (str tsp target::area.active address)
    
    ;; Update active pointer for vsp area.
    (ldr address target::tcr.vs-area target::rcontext)
    (str vsp target::area.active address)
    
    ; Update active pointer for SP area
    (ldr arg_z target::tcr.cs-area target::rcontext)
    (str sp target::area.active arg_z)


    (ref-global arg_z all-areas)
    (ldr arg_z target::area.succ arg_z)

    (blr)))

(defppclapfunction %active-dynamic-area ()
  (ref-global arg_z all-areas)
  (ldr arg_z target::area.succ arg_z)
  (blr))

  
(defppclapfunction %object-in-stack-area-p ((object arg_y) (area arg_z))
  (ldr imm0 target::area.active area)
  (cmplr cr0 object imm0)
  (ldr imm1 target::area.high area)
  (cmplr cr1 object imm1)
  (li arg_z nil)
  (bltlr cr0)
  (bgelr cr1)
  (la arg_z target::t-offset arg_z)
  (blr))

(defppclapfunction %object-in-heap-area-p ((object arg_y) (area arg_z))
  (ldr imm0 target::area.low area)
  (cmplr cr0 object imm0)
  (ldr imm1 target::area.active area)
  (cmplr cr1 object imm1)
  (li arg_z nil)
  (bltlr cr0)
  (bgelr cr1)
  (la arg_z target::t-offset arg_z)
  (blr))


#+ppc32-target
(defppclapfunction walk-static-area ((a arg_y) (f arg_z))
  (let ((fun save0)
        (obj save1)
        (limit save2)
        (header imm0)
        (tag imm1)
        (subtag imm2)
        (bytes imm3)
        (elements imm0))
    (save-lisp-context)
    (:regsave limit 0)
    (vpush fun)
    (vpush obj)
    (vpush limit)
    (mr fun f)
    (lwz limit ppc32::area.active a)
    (lwz obj ppc32::area.low a)
    (b @test)
    @loop
    (lwz header 0 obj)
    (extract-fulltag tag header)
    (cmpwi cr0 tag ppc32::fulltag-immheader)
    (cmpwi cr1 tag ppc32::fulltag-nodeheader)
    (beq cr0 @misc)
    (beq cr1 @misc)
    (la arg_z ppc32::fulltag-cons obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (la obj ppc32::cons.size obj)
    (b @test)
    @misc
    (la arg_z ppc32::fulltag-misc obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (lwz header 0 obj)
    (extract-fulltag tag header)
    (cmpwi cr1 tag ppc32::fulltag-nodeheader)
    (clrlwi subtag header (- 32 ppc32::num-subtag-bits))
    (cmpwi cr2 subtag ppc32::max-32-bit-ivector-subtag)
    (cmpwi cr3 subtag ppc32::max-8-bit-ivector-subtag)
    (cmpwi cr4 subtag ppc32::max-16-bit-ivector-subtag)
    (cmpwi cr5 subtag ppc32::subtag-double-float-vector)
    (header-size elements header)
    (slwi bytes elements 2)
    (beq cr1 @bump)
    (ble cr2 @bump)
    (mr bytes elements)
    (ble cr3 @bump)
    (slwi bytes elements 1)
    (ble cr4 @bump)
    (slwi bytes elements 3)
    (beq cr5 @bump)
    (la elements 7 elements)
    (srwi bytes elements 3)
    @bump
    (la bytes (+ 4 7) bytes)
    (clrrwi bytes bytes 3)
    (add obj obj bytes)
    @test
    (cmplw :cr0 obj limit)
    (blt cr0 @loop)
    (vpop limit)
    (vpop obj)
    (vpop fun)
    (restore-full-lisp-context)
    (blr)))

#+ppc64-target
(defppclapfunction walk-static-area ((a arg_y) (f arg_z))
  (let ((fun save0)
        (obj save1)
        (limit save2)
        (header imm0)
        (tag imm1)
        (subtag imm2)
        (bytes imm3)
        (elements imm0))
    (save-lisp-context)
    (:regsave limit 0)
    (vpush fun)
    (vpush obj)
    (vpush limit)
    (mr fun f)
    (ld limit ppc64::area.active a)
    (ld obj ppc64::area.low a)
    (b @test)
    @loop
    (ld header 0 obj)
    (extract-lowtag tag header)
    (cmpri cr0 tag ppc64::lowtag-immheader)
    (cmpri cr1 tag ppc64::lowtag-nodeheader)
    (beq cr0 @misc)
    (beq cr1 @misc)
    (la arg_z ppc64::fulltag-cons obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (la obj ppc64::cons.size obj)
    (b @test)
    @misc
    (la arg_z ppc64::fulltag-misc obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (ldr header 0 obj)
    (extract-lowtag tag header)
    (extract-fulltag subtag header)
    (cmpri cr1 tag ppc64::lowtag-nodeheader)
    (extract-lowbyte tag header)
    (cmpri cr2 subtag ppc64::ivector-class-64-bit)
    (cmpri cr3 subtag ppc64::ivector-class-8-bit)
    (cmpri cr4 subtag ppc64::ivector-class-32-bit)
    (cmpri cr5 tag ppc64::subtag-bit-vector)
    (header-size elements header)
    (sldi bytes elements 3)
    (beq cr1 @bump)
    (beq cr2 @bump)
    (mr bytes elements)
    (beq cr3 @bump)
    (sldi bytes elements 2)
    (beq cr4 @bump)
    (sldi bytes elements 1)
    (bne cr5 @bump)
    (la elements 7 elements)
    (srdi bytes elements 3)
    @bump
    (la bytes (+ 8 15) bytes)
    (clrrdi bytes bytes 4)
    (add obj obj bytes)
    @test
    (cmpld :cr0 obj limit)
    (blt cr0 @loop)
    (vpop limit)
    (vpop obj)
    (vpop fun)
    (restore-full-lisp-context)
    (blr)))

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

#+ppc32-target
(defppclapfunction %walk-dynamic-area ((a arg_y) (f arg_z))
  (let ((fun save0)
        (obj save1)
        (sentinel save2)
        (header imm0)
        (tag imm1)
        (subtag imm2)
        (bytes imm3)
        (elements imm4))
    (save-lisp-context)
    (:regsave sentinel 0)
    (vpush fun)
    (vpush obj)
    (vpush sentinel)
    (ref-global imm0 tenured-area)
    (cmpwi cr0 imm0 0)
    (li allocbase #xfff8)
    (la allocptr (- ppc32::fulltag-cons ppc32::cons.size) allocptr)
    (twllt allocptr allocbase)
    (mr sentinel allocptr)
    (clrrwi allocptr allocptr ppc32::ntagbits)
    (mr fun f)
    (if :ne
      (mr a imm0))    
    (lwz imm5 ppc32::area.low a)
    @loop
    (lwz header 0 imm5)
    (extract-fulltag tag header)
    (cmpwi cr0 tag ppc32::fulltag-immheader)
    (cmpwi cr1 tag ppc32::fulltag-nodeheader)
    (beq cr0 @misc)
    (beq cr1 @misc)
    (la obj ppc32::fulltag-cons imm5)
    (cmpw cr0 obj sentinel)
    (mr arg_z obj)
    (set-nargs 1)
    (mr temp0 fun)
    (beq cr0 @done)
    (bla .SPfuncall)
    (la imm5 (- ppc32::cons.size ppc32::fulltag-cons) obj)
    (b @loop)
    @misc
    (la obj ppc32::fulltag-misc imm5)
    (mr arg_z obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (getvheader header obj)
    (extract-fulltag tag header)
    (cmpwi cr1 tag ppc32::fulltag-nodeheader)
    (cmpwi cr7 tag ppc32::fulltag-immheader)
    (clrlwi subtag header (- 32 ppc32::num-subtag-bits))
    (cmpwi cr2 subtag ppc32::max-32-bit-ivector-subtag)
    (cmpwi cr3 subtag ppc32::max-8-bit-ivector-subtag)
    (cmpwi cr4 subtag ppc32::max-16-bit-ivector-subtag)
    (cmpwi cr5 subtag ppc32::subtag-double-float-vector)
    (header-size elements header)
    (slwi bytes elements 2)
    (beq cr1 @bump)
    (if (:cr7 :ne)
      (twle 0 0))
    (ble cr2 @bump)
    (mr bytes elements)
    (ble cr3 @bump)
    (slwi bytes elements 1)
    (ble cr4 @bump)
    (slwi bytes elements 3)
    (beq cr5 @bump)
    (la elements 7 elements)
    (srwi bytes elements 3)
    @bump
    (la bytes (+ 4 7) bytes)
    (clrrwi bytes bytes 3)
    (subi imm5 obj ppc32::fulltag-misc)
    (add imm5 imm5 bytes)
    (cmpw cr0 imm5  sentinel)
    (blt cr0 @loop)
    (uuo_interr 0 0)
    (b @loop)
    @done
    (li arg_z nil)
    (vpop sentinel)
    (vpop obj)
    (vpop fun)
    (restore-full-lisp-context)
    (blr)))

#+ppc64-target
(defppclapfunction %walk-dynamic-area ((a arg_y) (f arg_z))
  (let ((fun save0)
        (obj save1)
        (sentinel save2)
        (header imm0)
        (tag imm1)
        (subtag imm2)
        (bytes imm3)
        (elements imm4))
    (save-lisp-context)
    (:regsave sentinel 0)
    (vpush fun)
    (vpush obj)
    (vpush sentinel)
    (ref-global imm0 tenured-area)
    (cmpdi cr0 imm0 0)
    (lwi allocbase #x8000)
    (sldi allocbase allocbase 32)
    (subi allocbase allocbase 16)
    (la allocptr (- ppc64::fulltag-cons ppc64::cons.size) allocptr)
    (tdlt allocptr allocbase)
    (mr sentinel allocptr)
    (clrrdi allocptr allocptr ppc64::ntagbits)
    (mr fun f)
    (if :ne
      (mr a imm0))    
    (ld imm5 ppc64::area.low a)
    @loop
    (ld header 0 imm5)
    (extract-lowtag tag header)
    (cmpdi cr0 tag ppc64::lowtag-immheader)
    (cmpdi cr1 tag ppc64::lowtag-nodeheader)
    (beq cr0 @misc)
    (beq cr1 @misc)
    (la obj ppc64::fulltag-cons imm5)
    (cmpd cr0 obj sentinel)
    (mr arg_z obj)
    (set-nargs 1)
    (mr temp0 fun)
    (beq cr0 @done)
    (bla .SPfuncall)
    (la imm5 (- ppc64::cons.size ppc64::fulltag-cons) obj)
    (b @loop)
    @misc
    (la obj ppc64::fulltag-misc imm5)
    (mr arg_z obj)
    (set-nargs 1)
    (mr temp0 fun)
    (bla .SPFuncall)
    (getvheader header obj)
    (extract-lowtag tag header)    
    (extract-fulltag subtag header)
    (cmpdi cr1 tag ppc64::lowtag-nodeheader)
    (extract-lowbyte tag header)
    (cmpri cr2 subtag ppc64::ivector-class-64-bit)
    (cmpri cr3 subtag ppc64::ivector-class-8-bit)
    (cmpri cr4 subtag ppc64::ivector-class-32-bit)
    (cmpri cr5 tag ppc64::subtag-bit-vector)
    (header-size elements header)
    (sldi bytes elements 3)
    (beq cr1 @bump)
    (beq cr2 @bump)
    (mr bytes elements)
    (beq cr3 @bump)
    (sldi bytes elements 2)
    (beq cr4 @bump)
    (sldi bytes elements 1)
    (bne cr5 @bump)
    (la elements 7 elements)
    (srdi bytes elements 3)
    @bump
    (la bytes (+ 8 15) bytes)
    (clrrdi bytes bytes 4)
    (subi imm5 obj ppc64::fulltag-misc)
    (add imm5 imm5 bytes)
    (b @loop)
    @done
    (li arg_z nil)
    (vpop sentinel)
    (vpop obj)
    (vpop fun)
    (restore-full-lisp-context)
    (blr)))

(defun walk-dynamic-area (area func)
  (with-other-threads-suspended
      (%walk-dynamic-area area func)))



(defppclapfunction %class-of-instance ((i arg_z))
  (svref arg_z instance.class-wrapper i)
  (svref arg_z %wrapper-class arg_z)
  (blr))

(defppclapfunction class-of ((x arg_z))
  (check-nargs 1)
  (extract-fulltag imm0 x)
  (cmpri imm0 target::fulltag-misc)
  (beq @misc)
  (extract-lowbyte imm0 x)
  (b @done)
  @misc
  (extract-subtag imm0 x)
  @done
  (slri imm0 imm0 target::word-shift)
  (ldr temp1 '*class-table* nfn)
  (addi imm0 imm0 target::misc-data-offset)
  (ldr temp1 target::symbol.vcell temp1)
  (ldrx temp0 temp1 imm0) ; get entry from table
  (cmpri cr0 temp0 nil)
  (beq @bad)
  ;; functionp?
  (extract-typecode imm1 temp0)
  (cmpri imm1 target::subtag-function)
  (bne @ret)  ; not function - return entry
  ;; else jump to the fn
  (mr nfn temp0)
  (ldr temp0 target::misc-data-offset temp0)
  (SET-NARGS 1)
  (mtctr temp0)
  (bctr)
  @bad
  (ldr fname 'no-class-error nfn)
  (ba .spjmpsym)
  @ret
  (mr arg_z temp0)  ; return frob from table
  (blr))

(defppclapfunction full-gccount ()
  (ref-global arg_z tenured-area)
  (cmpri cr0 arg_z 0)
  (if :eq
    (ref-global arg_z gc-count)
    (ldr arg_z target::area.gc-count arg_z))
  (blr))


(defppclapfunction gc ()
  (check-nargs 0)
  (li imm0 arch::gc-trap-function-gc)
  (trlgei allocptr 0)
  (li arg_z target::nil-value)
  (blr))


(defppclapfunction egc ((arg arg_z))
  "Enable the EGC if arg is non-nil, disables the EGC otherwise. Return
the previous enabled status. Although this function is thread-safe (in
the sense that calls to it are serialized), it doesn't make a whole lot
of sense to be turning the EGC on and off from multiple threads ..."
  (check-nargs 1)
  (subi imm1 arg nil)
  (li imm0 arch::gc-trap-function-egc-control)
  (trlgei allocptr 0)
  (blr))



(defppclapfunction %configure-egc ((e0size arg_x)
				   (e1size arg_y)
				   (e2size arg_z))
  (check-nargs 3)
  (li imm0 arch::gc-trap-function-configure-egc)
  (trlgei allocptr 0)
  (blr))

(defppclapfunction purify ()
  (li imm0 arch::gc-trap-function-purify)
  (trlgei allocptr 0)
  (li arg_z nil)
  (blr))


(defppclapfunction impurify ()
  (li imm0 arch::gc-trap-function-impurify)
  (trlgei allocptr 0)
  (li arg_z nil)
  (blr))

(defppclapfunction lisp-heap-gc-threshold ()
  "Return the value of the kernel variable that specifies the amount
of free space to leave in the heap after full GC."
  (check-nargs 0)
  (li imm0 arch::gc-trap-function-get-lisp-heap-threshold)
  (trlgei allocptr 0)
  #+ppc32-target
  (ba .SPmakeu32)
  #+ppc64-target
  (ba .SPmakeu64))

(defppclapfunction set-lisp-heap-gc-threshold ((new arg_z))
  "Set the value of the kernel variable that specifies the amount of free
space to leave in the heap after full GC to new-value, which should be a
non-negative fixnum. Returns the value of that kernel variable (which may
be somewhat larger than what was specified)."
  (check-nargs 1)
  (mflr loc-pc)
  #+ppc32-target
  (bla .SPgetu32)
  #+ppc64-target
  (bla .SPgetu64)
  (mtlr loc-pc)
  (mr imm1 imm0)
  (li imm0 arch::gc-trap-function-set-lisp-heap-threshold)
  (trlgei allocptr 0)
  #+ppc32-target
  (ba .SPmakeu32)
  #+ppc64-target
  (ba .SPmakeu64))


(defppclapfunction use-lisp-heap-gc-threshold ()
  "Try to grow or shrink lisp's heap space, so that the free space is(approximately) equal to the current heap threshold. Return NIL"
  (check-nargs 0) 
  (li imm0 arch::gc-trap-function-use-lisp-heap-threshold)
  (trlgei allocptr 0)
  (li arg_z nil)
  (blr))


(defppclapfunction freeze ()
  "Do a full GC, then consider all heap-allocated objects which survive to be non-relocatable."
  (check-nargs 0)
  (li imm0 arch::gc-trap-function-freeze)
  (trlgei allocptr 0)
  #+64-bit-target
  (ba .SPmakeu64)
  #+32-bit-target
  (ba .SPmakeu32))
  


;;; offset is a fixnum, one of the target::kernel-import-xxx constants.
;;; Returns that kernel import, a fixnum.
(defppclapfunction %kernel-import ((offset arg_z))
  (ref-global imm0 kernel-imports)
  (unbox-fixnum imm1 arg_z)
  (ldrx arg_z imm0 imm1)
  (blr))

(defppclapfunction %get-unboxed-ptr ((macptr arg_z))
  (macptr-ptr imm0 arg_z)
  (ldr arg_z 0 imm0)
  (blr))


(defppclapfunction %revive-macptr ((p arg_z))
  (li imm0 target::subtag-macptr)
  (stb imm0 target::misc-subtag-offset p)
  (blr))

(defppclapfunction %macptr-type ((p arg_z))
  (check-nargs 1)
  (trap-unless-typecode= p target::subtag-macptr)
  (svref imm0 target::macptr.type-cell p)
  (box-fixnum arg_z imm0)
  (blr))
  
(defppclapfunction %macptr-domain ((p arg_z))
  (check-nargs 1)
  (trap-unless-typecode= p target::subtag-macptr)
  (svref imm0 target::macptr.domain-cell p)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %set-macptr-type ((p arg_y) (new arg_z))
  (check-nargs 2)
  (unbox-fixnum imm1 new)
  (trap-unless-typecode= p target::subtag-macptr)
  (svset imm1 target::macptr.type-cell p)
  (blr))

(defppclapfunction %set-macptr-domain ((p arg_y) (new arg_z))
  (check-nargs 2)
  (unbox-fixnum imm1 new)
  (trap-unless-typecode= p target::subtag-macptr)
  (svset imm1 target::macptr.domain-cell p)
  (blr))

(defppclapfunction true ()
  (cmplri nargs '3)
  (li arg_z t)
  (blelr)
  (subi imm0 nargs '3)
  (add vsp vsp imm0)
  (blr))

(defppclapfunction false ()
  (cmplri nargs '3)
  (li arg_z nil)
  (blelr)
  (subi imm0 nargs '3)
  (add vsp vsp imm0)
  (blr))

(lfun-bits #'true #.(encode-lambda-list '(&lap &rest ignore)))
(lfun-bits #'false #.(encode-lambda-list '(&lap &rest ignore)))

;;; end
