;;; -*- Mode: Lisp; Package: CCL -*-
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

;;; level-0;x86;x86-misc.lisp


(in-package "CCL")

;;; Copy N bytes from pointer src, starting at byte offset src-offset,
;;; to ivector dest, starting at offset dest-offset.
;;; It's fine to leave this in lap.
;;; Depending on alignment, it might make sense to move more than
;;; a byte at a time.
;;; Does no arg checking of any kind.  Really.

(defppclapfunction %copy-ptr-to-ivector ((src (* 1 target::node-size) )
                                         (src-byte-offset 0) 
                                         (dest arg_x)
                                         (dest-byte-offset arg_y)
                                         (nbytes arg_z))
  (let ((src-reg imm0)
        (src-byteptr imm1)
        (src-node-reg temp0)
        (dest-byteptr imm2)
        (val imm3)
        (node-temp temp1))
    (cmpri cr0 nbytes 0)
    (ldr src-node-reg src vsp)
    (macptr-ptr src-reg src-node-reg)
    (ldr src-byteptr src-byte-offset vsp)
    (unbox-fixnum src-byteptr src-byteptr)
    (unbox-fixnum dest-byteptr dest-byte-offset)
    (la dest-byteptr target::misc-data-offset dest-byteptr)
    (b @test)
    @loop
    (subi nbytes nbytes '1)
    (cmpri cr0 nbytes '0)
    (lbzx val src-reg src-byteptr)
    (la src-byteptr 1 src-byteptr)
    (stbx val dest dest-byteptr)
    (la dest-byteptr 1 dest-byteptr)
    @test
    (bne cr0 @loop)
    (mr arg_z dest)
    (la vsp '2 vsp)
    (blr)))

(defppclapfunction %copy-ivector-to-ptr ((src (* 1 target::node-size))
                                         (src-byte-offset 0) 
                                         (dest arg_x)
                                         (dest-byte-offset arg_y)
                                         (nbytes arg_z))
  (ldr temp0 src vsp)
  (cmpri cr0 nbytes 0)
  (ldr imm0 src-byte-offset vsp)
  (unbox-fixnum imm0 imm0)
  (la imm0 target::misc-data-offset imm0)
  (unbox-fixnum imm2 dest-byte-offset)
  (ldr imm1 target::macptr.address dest)
  (b @test)
  @loop
  (subi nbytes nbytes '1)
  (cmpri cr0 nbytes 0)
  (lbzx imm3 temp0 imm0)
  (addi imm0 imm0 1)
  (stbx imm3 imm1 imm2)
  (addi imm2 imm2 1)
  @test
  (bne cr0 @loop)
  (mr arg_z dest)
  (la vsp '2 vsp)
  (blr))

#+ppc32-target
(defppclapfunction %copy-ivector-to-ivector ((src 4) 
                                             (src-byte-offset 0) 
                                             (dest arg_x)
                                             (dest-byte-offset arg_y)
                                             (nbytes arg_z))
  (lwz temp0 src vsp)
  (cmpwi cr0 nbytes 0)
  (cmpw cr2 temp0 dest)   ; source and dest same?
  (rlwinm imm3 nbytes 0 (- 30 target::fixnum-shift) 31)  
  (lwz imm0 src-byte-offset vsp)
  (rlwinm imm1 imm0 0 (- 30 target::fixnum-shift) 31)
  (or imm3 imm3 imm1)
  (unbox-fixnum imm0 imm0)
  (la imm0 target::misc-data-offset imm0)
  (unbox-fixnum imm2 dest-byte-offset)
  (rlwimi imm1 imm2 0 30 31)
  (or imm3 imm3 imm1)
  (cmpwi cr1 imm3 0)  ; is everybody multiple of 4?
  (la imm2 target::misc-data-offset imm2)
  (beq cr2 @SisD)   ; source and dest same
  @fwd
  (beq :cr1 @wtest)
  (b @test)

  @loop
  (subi nbytes nbytes '1)
  (cmpwi cr0 nbytes 0)
  (lbzx imm3 temp0 imm0)
  (addi imm0 imm0 1)
  (stbx imm3 dest imm2)
  (addi imm2 imm2 1)
  @test
  (bne cr0 @loop)
  (mr arg_z dest)
  (la vsp 8 vsp)
  (blr)

  @words      ; source and dest different - words 
  (subi nbytes nbytes '4)  
  (cmpwi cr0 nbytes 0)
  (lwzx imm3 temp0 imm0)
  (addi imm0 imm0 4)
  (stwx imm3 dest imm2)
  (addi imm2 imm2 4)
  @wtest
  (bgt cr0 @words)
  @done
  (mr arg_z dest)
  (la vsp 8 vsp)
  (blr)

  @SisD
  (cmpw cr2 imm0 imm2) ; cmp src and dest
  (bgt cr2 @fwd)
  ;(B @bwd) 
  

  ; Copy backwards when src & dest are the same and we're sliding down
  @bwd ; ok
  (unbox-fixnum imm3 nbytes)
  (add imm0 imm0 imm3)
  (add imm2 imm2 imm3)
  (b @test2)
  @loop2
  (subi nbytes nbytes '1)
  (cmpwi cr0 nbytes 0)
  (subi imm0 imm0 1)
  (lbzx imm3 temp0 imm0)
  (subi imm2 imm2 1)
  (stbx imm3 dest imm2)
  @test2
  (bne cr0 @loop2)
  (b @done))

#+ppc64-target
(defppclapfunction %copy-ivector-to-ivector ((src-offset 8) 
                                             (src-byte-offset-offset 0) 
                                             (dest arg_x)
                                             (dest-byte-offset arg_y)
                                             (nbytes arg_z))
  (let ((src temp0)
        (src-byte-offset imm0))
    (subi nbytes nbytes '1)
    (ld src-byte-offset src-byte-offset-offset vsp)
    (cmpdi nbytes 0 )
    (ld src src-offset vsp)
    (la vsp '2 vsp)
    (cmpd cr1 src dest)
    (cmpdi cr2 src-byte-offset dest-byte-offset)
    (unbox-fixnum src-byte-offset src-byte-offset)
    (unbox-fixnum imm1 dest-byte-offset)
    (la imm0 target::misc-data-offset src-byte-offset)
    (la imm1 target::misc-data-offset imm1)
    (bne cr1 @test)
    ;; Maybe overlap, or maybe nothing to do.
    (beq cr2 @done)                       ; same vectors, same offsets
    (blt cr2 @back)                       ; copy backwards, avoid overlap
    (b @test)
    @loop
    (subi nbytes nbytes '1)
    (lbzx imm3 src imm0)
    (cmpdi nbytes 0)
    (addi imm0 imm0 1)
    (stbx imm3 dest imm1)
    (addi imm1 imm1 1)
    @test
    (bge @loop)
    @done
    (mr arg_z dest)
    (blr)
    @back
    ;; nbytes was predecremented above
    (unbox-fixnum imm2 nbytes)
    (add imm0 imm2 imm0)
    (add imm1 imm2 imm1)
    (b @back-test)
    @back-loop
    (subi nbytes nbytes '1)
    (lbzx imm3 src imm0)
    (cmpdi nbytes 0)
    (subi imm0 imm0 1)
    (stbx imm3 dest imm1)
    (subi imm1 imm1 1)
    @back-test
    (bge @back-loop)
    (mr arg_z dest)
    (blr)))
  

(defppclapfunction %copy-gvector-to-gvector ((src (* 1 target::node-size))
					     (src-element 0)
					     (dest arg_x)
					     (dest-element arg_y)
					     (nelements arg_z))
  (subi nelements nelements '1)
  (cmpri nelements 0)
  (ldr imm0 src-element vsp)
  (ldr temp0 src vsp)
  (la vsp '2 vsp)
  (cmpr cr1 temp0 dest)
  (cmpri cr2 src-element dest-element)
  (la imm0 target::misc-data-offset imm0)
  (la imm1 target::misc-data-offset dest-element)
  (bne cr1 @test)
  ;; Maybe overlap, or maybe nothing to do.
  (beq cr2 @done)                       ; same vectors, same offsets
  (blt cr2 @back)                       ; copy backwards, avoid overlap
  (b @test)
  @loop
  (subi nelements nelements '1)
  (cmpri nelements 0)
  (ldrx temp1 temp0 imm0)
  (addi imm0 imm0 '1)
  (strx temp1 dest imm1)
  (addi imm1 imm1 '1)
  @test
  (bge @loop)
  @done
  (mr arg_z dest)
  (blr)
  @back
  ;; We decremented NELEMENTS by 1 above.
  (add imm1 nelements imm1)
  (add imm0 nelements imm0)
  (b @back-test)
  @back-loop
  (subi nelements nelements '1)
  (cmpri nelements 0)
  (ldrx temp1 temp0 imm0)
  (subi imm0 imm0 '1)
  (strx temp1 dest imm1)
  (subi imm1 imm1 '1)
  @back-test
  (bge @back-loop)
  (mr arg_z dest)
  (blr))
  
  



#+ppc32-target
(defppclapfunction %heap-bytes-allocated ()
  (lwz imm2 target::tcr.last-allocptr ppc32::rcontext)
  (cmpwi cr1 imm2 0)
  (cmpwi allocptr -8)			;void_allocptr
  (lwz imm0 target::tcr.total-bytes-allocated-high ppc32::rcontext)
  (lwz imm1 target::tcr.total-bytes-allocated-low ppc32::rcontext)
  (sub imm2 imm2 allocptr)
  (beq cr1 @go)
  (beq @go)
  (addc imm1 imm1 imm2)
  (addze imm0 imm0)
  @go
  (ba .SPmakeu64))

#+ppc64-target
(defppclapfunction %heap-bytes-allocated ()
  (ld imm2 target::tcr.last-allocptr ppc64::rcontext)
  (cmpri cr1 imm2 0)
  (cmpri allocptr -16)			;void_allocptr
  (ld imm0 target::tcr.total-bytes-allocated-high ppc64::rcontext)
  (sub imm2 imm2 allocptr)
  (beq cr1 @go)
  (beq @go)
  (add imm0 imm0 imm2)
  @go
  (ba .SPmakeu64))


(defppclapfunction values ()
  (:arglist (&rest values))
  (vpush-argregs)
  (add temp0 nargs vsp)
  (ba .SPvalues))

;; It would be nice if (%setf-macptr macptr (ash (the fixnum value) ash::fixnumshift))
;; would do this inline.
#+ppc-target
(defppclapfunction %setf-macptr-to-object ((macptr arg_y) (object arg_z))
  (check-nargs 2)
  (trap-unless-typecode= arg_y target::subtag-macptr)
  (str arg_z target::macptr.address arg_y)
  (blr))

(defppclapfunction %fixnum-from-macptr ((macptr arg_z))
  (check-nargs 1)
  (trap-unless-typecode= arg_z target::subtag-macptr)
  (ldr imm0 target::macptr.address arg_z)
  (trap-unless-lisptag= imm0 target::tag-fixnum imm1)
  (mr arg_z imm0)
  (blr))

#+ppc32-target
(defppclapfunction %%get-unsigned-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr ppc32::subtag-macptr)
  (macptr-ptr imm1 ptr)
  (unbox-fixnum imm2 offset)
  (add imm2 imm2 imm1)
  (lwz imm0 0 imm2)
  (lwz imm1 4 imm2)
  (ba .SPmakeu64))

#+ppc64-target
(defppclapfunction %%get-unsigned-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr ppc64::subtag-macptr)
  (macptr-ptr imm1 ptr)
  (unbox-fixnum imm2 offset)
  (ldx imm0 imm2 imm1)
  (ba .SPmakeu64))

#+ppc32-target
(defppclapfunction %%get-signed-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr ppc32::subtag-macptr)
  (macptr-ptr imm1 ptr)
  (unbox-fixnum imm2 offset)
  (add imm2 imm2 imm1)
  (lwz imm0 0 imm2)
  (lwz imm1 4 imm2)
  (ba .SPmakes64))

#+ppc64-target
(defppclapfunction %%get-signed-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr ppc64::subtag-macptr)
  (macptr-ptr imm1 ptr)
  (unbox-fixnum imm2 offset)
  (ldx imm0 imm2 imm1)
  (ba .SPmakes64))

#+ppc32-target
(defppclapfunction %%set-unsigned-longlong ((ptr arg_x)
					      (offset arg_y)
					      (val arg_z))
  (save-lisp-context)
  (trap-unless-typecode= ptr ppc32::subtag-macptr)
  (bla .SPgetu64)
  (macptr-ptr imm2 ptr)
  (unbox-fixnum imm3 offset)
  (add imm2 imm3 imm2)
  (stw imm0 0 imm2)
  (stw imm1 4 imm2)
  (ba .SPpopj))

#+ppc64-target
(defppclapfunction %%set-unsigned-longlong ((ptr arg_x)
                                            (offset arg_y)
                                            (val arg_z))
  (save-lisp-context)
  (trap-unless-typecode= ptr ppc64::subtag-macptr)
  (bla .SPgetu64)
  (macptr-ptr imm2 ptr)
  (unbox-fixnum imm3 offset)
  (stdx imm0 imm3 imm2)
  (ba .SPpopj))

#+ppc32-target
(defppclapfunction %%set-signed-longlong ((ptr arg_x)
					    (offset arg_y)
					    (val arg_z))
  (save-lisp-context)
  (trap-unless-typecode= ptr ppc32::subtag-macptr)
  (bla .SPgets64)
  (macptr-ptr imm2 ptr)
  (unbox-fixnum imm3 offset)
  (add imm2 imm3 imm2)
  (stw imm0 0 imm2)
  (stw imm1 4 imm2)
  (ba .SPpopj))

#+ppc64-target
(defppclapfunction %%set-signed-longlong ((ptr arg_x)
                                          (offset arg_y)
                                          (val arg_z))
  (save-lisp-context)
  (trap-unless-typecode= ptr target::subtag-macptr)
  (bla .SPgets64)
  (macptr-ptr imm2 ptr)
  (unbox-fixnum imm3 offset)
  (stdx imm0 imm3 imm2)
  (ba .SPpopj))

(defppclapfunction interrupt-level ()
  (ldr arg_z target::tcr.tlb-pointer target::rcontext)
  (ldr arg_z target::interrupt-level-binding-index arg_z)
  (blr))


(defppclapfunction disable-lisp-interrupts ()
  (li imm0 '-1)
  (ldr imm1 target::tcr.tlb-pointer target::rcontext)
  (ldr arg_z target::interrupt-level-binding-index imm1)
  (str imm0 target::interrupt-level-binding-index imm1)
  (blr))

(defppclapfunction set-interrupt-level ((new arg_z))
  (ldr imm1 target::tcr.tlb-pointer target::rcontext)
  (trap-unless-lisptag= new target::tag-fixnum imm0)
  (str new target::interrupt-level-binding-index imm1)
  (blr))

;;; If we're restoring the interrupt level to 0 and an interrupt
;;; was pending, restore the level to 1 and zero the pending status.
(defppclapfunction restore-interrupt-level ((old arg_z))
  (cmpri :cr1 old 0)
  (ldr imm0 target::tcr.interrupt-pending target::rcontext)
  (ldr imm1 target::tcr.tlb-pointer target::rcontext)
  (cmpri :cr0 imm0 0)
  (bne :cr1 @store)
  (beq :cr0 @store)
  (str rzero target::tcr.interrupt-pending target::rcontext)
  (li old '1)
  @store
  (str old target::interrupt-level-binding-index imm1)
  (blr))



(defppclapfunction %current-tcr ()
  (mr arg_z target::rcontext)
  (blr))

(defppclapfunction %tcr-toplevel-function ((tcr arg_z))
  (check-nargs 1)
  (cmpr tcr target::rcontext)
  (mr imm0 vsp)
  (ldr temp0 target::tcr.vs-area tcr)
  (ldr imm1 target::area.high temp0)
  (beq @room)
  (ldr imm0 target::area.active temp0)
  @room
  (cmpr imm1 imm0)
  (li arg_z nil)
  (beqlr)
  (ldr arg_z (- target::node-size) imm1)
  (blr))

(defppclapfunction %set-tcr-toplevel-function ((tcr arg_y) (fun arg_z))
  (check-nargs 2)
  (cmpr tcr target::rcontext)
  (mr imm0 vsp)
  (ldr temp0 target::tcr.vs-area tcr)
  (ldr imm1 target::area.high temp0)
  (beq @check-room)
  (ldr imm0 target::area.active temp0)
  @check-room
  (cmpr imm1 imm0)
  (push rzero imm1)
  (bne @have-room)
  (str imm1 target::area.active temp0)
  (str imm1 target::tcr.save-vsp tcr)
  @have-room
  (str fun 0 imm1)
  (blr))

;;; This needs to be done out-of-line, to handle EGC memoization.
(defppclapfunction %store-node-conditional ((offset 0) (object arg_x) (old arg_y) (new arg_z))
  (ba .SPstore-node-conditional))

(defppclapfunction %store-immediate-conditional ((offset 0) (object arg_x) (old arg_y) (new arg_z))
  (vpop temp0)
  (unbox-fixnum imm0 temp0)
  (let ((current temp1))
    @again
    (lrarx current object imm0)
    (cmpr current old)
    (bne @lose)
    (strcx. new object imm0)
    (bne @again)
    (isync)
    (li arg_z (+ target::t-offset (target-nil-value)))
    (blr)
    @lose
    (li imm0 target::reservation-discharge)
    (strcx. rzero rzero imm0)
    (li arg_z nil)
    (blr)))

(defppclapfunction set-%gcable-macptrs% ((ptr target::arg_z))
  (li imm0 (+ (target-nil-value) (target::kernel-global gcable-pointers)))
  @again
  (lrarx arg_y rzero imm0)
  (str arg_y target::xmacptr.link ptr)
  (strcx. ptr rzero imm0)
  (bne @again)
  (isync)
  (blr))

;;; Atomically increment or decrement the gc-inhibit-count kernel-global
;;; (It's decremented if it's currently negative, incremented otherwise.)
(defppclapfunction %lock-gc-lock ()
  (li imm0 (+ (target-nil-value) (target::kernel-global gc-inhibit-count)))
  @again
  (lrarx arg_y rzero imm0)
  (cmpri cr1 arg_y 0)
  (addi arg_z arg_y '1)
  (bge cr1 @store)
  (subi arg_z arg_y '1)
  @store
  (strcx. arg_z rzero imm0)
  (bne @again)
;;  (isync)
  (blr))

;;; Atomically decrement or increment the gc-inhibit-count kernel-global
;;; (It's incremented if it's currently negative, incremented otherwise.)
;;; If it's incremented from -1 to 0, try to GC (maybe just a little.)
(defppclapfunction %unlock-gc-lock ()
;;  (sync)
  (li imm0 (+ (target-nil-value) (target::kernel-global gc-inhibit-count)))
  @again
  (lrarx arg_y rzero imm0)
  (cmpri cr1 arg_y -1)
  (subi arg_z arg_y '1)
  (bgt cr1 @store)
  (addi arg_z arg_y '1)
  @store
  (strcx. arg_z rzero imm0)
  (bne @again)
  (bnelr cr1)
  ;; The GC tried to run while it was inhibited.  Unless something else
  ;; has just inhibited it, it should be possible to GC now.
  (li imm0 arch::gc-trap-function-immediate-gc)
  (trlgei allocptr 0)
  (blr))



(defppclapfunction %atomic-incf-node ((by arg_x) (node arg_y) (disp arg_z))
  (check-nargs 3)
  (unbox-fixnum imm1 disp)
  @again
  (lrarx arg_z node imm1)
  (add arg_z arg_z by)
  (strcx. arg_z node imm1)
  (bne- @again)
  (isync)
  (blr))

(defppclapfunction %atomic-incf-ptr ((ptr arg_z))
  (macptr-ptr imm1 ptr)
  @again
  (lrarx imm0 0 imm1)
  (addi imm0 imm0 1)
  (strcx. imm0 0 imm1)
  (bne @again)
  (isync)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %atomic-incf-ptr-by ((ptr arg_y) (by arg_z))
  (macptr-ptr imm1 ptr)
  (unbox-fixnum imm2 by)
  @again
  (lrarx imm0 0 imm1)
  (add imm0 imm0 imm2)
  (strcx. imm0 0 imm1)
  (bne @again)
  (isync)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %atomic-decf-ptr ((ptr arg_z))
  (macptr-ptr imm1 ptr)
  @again
  (lrarx imm0 0 imm1)
  (subi imm0 imm0 1)
  (strcx. imm0 0 imm1)
  (bne @again)
  (isync)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %atomic-decf-ptr-if-positive ((ptr arg_z))
  (macptr-ptr imm1 ptr)
  @again
  (lrarx imm0 0 imm1)
  (cmpri cr1 imm0 0)
  (subi imm0 imm0 1)
  (beq @done)
  (strcx. imm0 0 imm1)
  (bne @again)
  (isync)
  (box-fixnum arg_z imm0)
  (blr)
  @done
  (li imm1 target::reservation-discharge)
  (box-fixnum arg_z imm0)
  (strcx. rzero rzero imm1)
  (blr))

(defppclapfunction %atomic-swap-ptr ((ptr arg_y) (newval arg_z))
  (sync)
  (macptr-ptr imm1 ptr)
  (unbox-fixnum imm2 arg_z)
  @again
  (lrarx imm0 0 imm1)
  (strcx. imm2 0 imm1)
  (bne @again)
  (isync)
  (box-fixnum arg_z imm0)
  (blr))

;;; Try to store the fixnum NEWVAL at PTR, if and only if the old value
;;; was equal to OLDVAL.  Return the old value
(defppclapfunction %ptr-store-conditional ((ptr arg_x) (expected-oldval arg_y) (newval arg_z))
  (macptr-ptr imm0 ptr)
  (unbox-fixnum imm1 expected-oldval)
  (unbox-fixnum imm2 newval)
  @again
  (lrarx imm3 0 imm0)
  (cmpr imm3 imm1)
  (bne- @done)
  (strcx. imm2 0 imm0)
  (bne- @again)
  (isync)
  (box-fixnum arg_z imm3)
  (blr)
  @done
  (li imm0 target::reservation-discharge)
  (box-fixnum arg_z imm3)
  (strcx. rzero 0 imm0)
  (blr))

(defppclapfunction %ptr-store-fixnum-conditional ((ptr arg_x) (expected-oldval arg_y) (newval arg_z))
  (let ((address imm0)
        (actual-oldval imm1))
    (macptr-ptr address ptr)
    @again
    (lrarx actual-oldval 0 address)
    (cmpr actual-oldval expected-oldval)
    (bne- @done)
    (strcx. newval 0 address)
    (bne- @again)
    (isync)
    (mr arg_z actual-oldval)
    (blr)
    @done
    (li address target::reservation-discharge)
    (mr arg_z actual-oldval)
    (strcx. rzero 0 address)
    (blr)))




(defppclapfunction %macptr->dead-macptr ((macptr arg_z))
  (check-nargs 1)
  (li imm0 target::subtag-dead-macptr)
  (stb imm0 target::misc-subtag-offset macptr)
  (blr))

(defppclapfunction %%apply-in-frame ((catch-count imm0) (srv temp0) (tsp-count imm0) (db-link imm0)
                                     (parent arg_x) (function arg_y) (arglist arg_z))
  (check-nargs 7)

  ; Throw through catch-count catch frames
  (lwz imm0 12 vsp)                      ; catch-count
  (vpush parent)
  (vpush function)
  (vpush arglist)
  (bla .SPnthrowvalues)

  ; Pop tsp-count TSP frames
  (lwz tsp-count 16 vsp)
  (cmpi cr0 tsp-count 0)
  (b @test)
@loop
  (subi tsp-count tsp-count '1)
  (cmpi cr0 tsp-count 0)
  (lwz tsp 0 tsp)
@test
  (bne cr0 @loop)

  ; Pop dynamic bindings until we get to db-link
  (lwz imm0 12 vsp)                     ; db-link
  (lwz imm1 target::tcr.db-link target::rcontext)
  (cmp cr0 imm0 imm1)
  (beq cr0 @restore-regs)               ; .SPunbind-to expects there to be something to do
  (bla .SPunbind-to)

@restore-regs
  ; restore the saved registers from srv
  (lwz srv 20 vsp)
@get0
  (svref imm0 1 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get1)
  (lwz save0 0 imm0)
@get1
  (svref imm0 2 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get2)
  (lwz save1 0 imm0)
@get2
  (svref imm0 3 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get3)
  (lwz save2 0 imm0)
@get3
  (svref imm0 4 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get4)
  (lwz save3 0 imm0)
@get4
  (svref imm0 5 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get5)
  (lwz save4 0 imm0)
@get5
  (svref imm0 6 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get6)
  (lwz save5 0 imm0)
@get6
  (svref imm0 7 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @get7)
  (lwz save6 0 imm0)
@get7
  (svref imm0 8 srv)
  (cmpwi cr0 imm0 (target-nil-value))
  (beq @got)
  (lwz save7 0 imm0)
@got

  (vpop arg_z)                          ; arglist
  (vpop temp0)                          ; function
  (vpop parent)                         ; parent
  (extract-lisptag imm0 parent)
  (cmpi cr0 imm0 target::tag-fixnum)
  (if (:cr0 :ne)
    ; Parent is a fake-stack-frame. Make it real
    (progn
      (svref sp %fake-stack-frame.sp parent)
      (stwu sp (- target::lisp-frame.size) sp)
      (svref fn %fake-stack-frame.fn parent)
      (stw fn target::lisp-frame.savefn sp)
      (svref temp1 %fake-stack-frame.vsp parent)
      (stw temp1 target::lisp-frame.savevsp sp)
      (svref temp1 %fake-stack-frame.lr parent)
      (extract-lisptag imm0 temp1)
      (cmpi cr0 imm0 target::tag-fixnum)
      (if (:cr0 :ne)
        ;; must be a macptr encoding the actual link register
        (macptr-ptr loc-pc temp1)
        ;; Fixnum is offset from start of function vector
        (progn
          (svref temp2 0 fn)        ; function vector
          (unbox-fixnum temp1 temp1)
          (add loc-pc temp2 temp1)))
      (stw loc-pc target::lisp-frame.savelr sp))
    ;; Parent is a real stack frame
    (mr sp parent))
  (set-nargs 0)
  (bla .SPspreadargz)
  (ba .SPtfuncallgen))

#+ppc32-target
;;; Easiest to do this in lap, to avoid consing bignums and/or 
;;; multiple-value hair.
;;; Bang through code-vector until the end or a 0 (traceback table
;;; header) is found.  Return high-half, low-half of last instruction
;;; and index where found.
(defppclapfunction %code-vector-last-instruction ((cv arg_z))
  (let ((previ imm0)
        (nexti imm1)
        (idx imm2)
        (offset imm3)
        (len imm4))
    (vector-length len cv len)
    (li idx 0)
    (cmpw cr0 idx len)
    (li offset target::misc-data-offset)
    (li nexti 0)
    (b @test)
    @loop
    (mr previ nexti)
    (lwzx nexti cv offset)
    (cmpwi cr1 nexti 0)
    (addi idx idx '1)
    (cmpw cr0 idx len)
    (addi offset offset '1)
    (beq cr1 @done)
    @test
    (bne cr0 @loop)
    (mr previ nexti)
    @done
    (digit-h temp0 previ)
    (digit-l temp1 previ)
    (subi idx idx '1)
    (vpush temp0)
    (vpush temp1)
    (vpush idx)
    (set-nargs 3)
    (la temp0 '3 vsp)
    (ba .SPvalues)))

#+ppc64-target
(defun %code-vector-last-instruction (cv)
  (do* ((i 1 (1+ i))
        (instr nil)
        (n (uvsize cv)))
       ((= i n) instr)
    (declare (fixnum i n))
    (let* ((next (uvref cv i)))
      (declare (type (unsigned-byte 32) next))
      (if (zerop next)
        (return instr)
        (setq instr next)))))

        

  
(defppclapfunction %%save-application ((flags arg_y) (fd arg_z))
  (unbox-fixnum imm0 flags)
  (ori imm0 imm0 arch::gc-trap-function-save-application)
  (unbox-fixnum imm1 fd)
  (trlgei allocptr 0)
  (blr))

(defppclapfunction %metering-info ((ptr arg_z))
  (ref-global imm0 metering-info)
  (stw imm0 target::macptr.address ptr)
  (blr))

(defppclapfunction %misc-address-fixnum ((misc-object arg_z))
  (check-nargs 1)
  (la arg_z target::misc-data-offset misc-object)
  (blr))


#+ppc32-target
(defppclapfunction fudge-heap-pointer ((ptr arg_x) (subtype arg_y) (len arg_z))
  (check-nargs 3)
  (macptr-ptr imm1 ptr) ; address in macptr
  (addi imm0 imm1 9)     ; 2 for delta + 7 for alignment
  (clrrwi imm0 imm0 3)   ; Clear low three bits to align
  (subf imm1 imm1 imm0)  ; imm1 = delta
  (sth imm1 -2 imm0)     ; save delta halfword
  (unbox-fixnum imm1 subtype)  ; subtype at low end of imm1
  (rlwimi imm1 len (- target::num-subtag-bits target::fixnum-shift) 0 (- 31 target::num-subtag-bits))
  (stw imm1 0 imm0)       ; store subtype & length
  (addi arg_z imm0 target::fulltag-misc) ; tag it, return it
  (blr))

#+ppc64-target
(defppclapfunction fudge-heap-pointer ((ptr arg_x) (subtype arg_y) (len arg_z))
  (check-nargs 3)
  (macptr-ptr imm1 ptr) ; address in macptr
  (addi imm0 imm1 17)     ; 2 for delta + 15 for alignment
  (clrrdi imm0 imm0 4)   ; Clear low four bits to align
  (subf imm1 imm1 imm0)  ; imm1 = delta
  (sth imm1 -2 imm0)     ; save delta halfword
  (unbox-fixnum imm1 subtype)  ; subtype at low end of imm1
  (sldi imm2 len (- target::num-subtag-bits target::fixnum-shift))
  (or imm1 imm2 imm1)
  (std imm1 0 imm0)       ; store subtype & length
  (addi arg_z imm0 target::fulltag-misc) ; tag it, return it
  (blr))

(defppclapfunction %%make-disposable ((ptr arg_y) (vector arg_z))
  (check-nargs 2)
  (subi imm0 vector target::fulltag-misc) ; imm0 is addr = vect less tag
  (lhz imm1 -2 imm0)   ; get delta
  (sub imm0 imm0 imm1)  ; vector addr (less tag)  - delta is orig addr
  (str imm0 target::macptr.address ptr) 
  (blr))

#+ppc32-target
(defppclapfunction %vect-data-to-macptr ((vect arg_y) (ptr arg_z))
  ;; put address of vect data in macptr.  For all vector types
  ;; other than DOUBLE-FLOAT (or vectors thereof), the first byte
  ;; of data is at PPC32::MISC-DATA-OFFSET; for the double-float
  ;; types, it's at PPC32::MISC-DFLOAT-OFFSET.
  (extract-subtag imm0 vect)
  (cmpwi cr0 imm0 ppc32::subtag-double-float-vector)
  (cmpwi cr1 imm0 ppc32::subtag-double-float)
  (addi temp0 vect ppc32::misc-data-offset)
  (beq cr0 @dfloat)
  (beq cr1 @dfloat)
  (stw temp0 ppc32::macptr.address arg_z)
  (blr)
  @dfloat
  (addi temp0 vect ppc32::misc-dfloat-offset)
  (stw temp0 ppc32::macptr.address arg_z)
  (blr))

#+ppc64-target
(defppclapfunction %vect-data-to-macptr ((vect arg_y) (ptr arg_z))
  (la imm0 ppc64::misc-data-offset vect)
  (std imm0 ppc64::macptr.address ptr)
  (blr))

(defppclapfunction get-saved-register-values ()
  (vpush save0)
  (vpush save1)
  (vpush save2)
  (vpush save3)
  (vpush save4)
  (vpush save5)
  (vpush save6)
  (vpush save7)
  (la temp0 (* 8 target::node-size) vsp)
  (set-nargs 8)
  (ba .SPvalues))


(defppclapfunction %current-db-link ()
  (ldr arg_z target::tcr.db-link target::rcontext)
  (blr))

(defppclapfunction %no-thread-local-binding-marker ()
  (li arg_z target::subtag-no-thread-local-binding)
  (blr))


(defppclapfunction break-event-pending-p ()
  (ref-global arg_z target::intflag)
  (set-global rzero target::intflag)
  (cmpri arg_z 0)
  (li arg_z nil)
  (beqlr)
  (la arg_z target::t-offset arg_z)
  (blr))


;;; Should be called with interrupts disabled.
(defppclapfunction %safe-get-ptr ((src arg_y) (dest arg_z))
  (check-nargs 2)
  (macptr-ptr imm0 src)
  (str imm0 target::tcr.safe-ref-address target::rcontext)
  (ldr imm0 0 imm0)                     ; may fault
  (str imm0 target::macptr.address dest)
  (blr))



;;; r13 contains thread context on Linux/Darwin PPC64.
;;; That's maintained in r2 on LinuxPPC32, and not maintained
;;; in a GPR on DarwinPPC32
(defppclapfunction %get-os-context ()
  #+ppc64-target (mr arg_z 13)
  #+linuxppc32-target (mr arg_z 2)
  #+darinppc32-target (mr arg_z 0)
  (blr))

(defppclapfunction %check-deferred-gc ()
  (ldr imm0 target::tcr.flags target::rcontext)
  (slri. imm0 imm0 (- (1- target::nbits-in-word) (+ arch::tcr-flag-bit-pending-suspend target::fixnumshift)))
  (li arg_z nil)
  (bgelr)
  (uuo_interr arch::error-propagate-suspend rzero)
  (li arg_z t)
  (blr))

(defppclapfunction %%tcr-interrupt ((target arg_z))
  (check-nargs 1)
  (uuo_interr arch::error-interrupt rzero)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %suspend-tcr ((target arg_z))
  (check-nargs 1)
  (uuo_interr arch::error-suspend rzero)
  (ne0->boolean arg_z imm0 imm1)
  (blr))

(defppclapfunction %suspend-other-threads ()
  (check-nargs 0)
  (uuo_interr arch::error-suspend-all rzero)
  (li arg_z nil)
  (blr))

(defppclapfunction %resume-tcr ((target arg_z))
  (check-nargs 1)
  (uuo_interr arch::error-resume rzero)
  (ne0->boolean arg_z imm0 imm1)
  (blr))

(defppclapfunction %resume-other-threads ()
  (check-nargs 0)
  (uuo_interr arch::error-resume-all rzero)
  (li arg_z nil)
  (blr))

(defppclapfunction %atomic-pop-static-cons ()
  (li imm0 (+ (target-nil-value) (target::kernel-global static-conses)))
  @again
  (lrarx arg_z rzero imm0)
  (cmpri arg_z (target-nil-value))
  (beq @lose)
  (%cdr arg_y arg_z)
  (strcx. arg_y rzero imm0)
  (isync)
  (bne @again)
  (blr)
  @lose
  (li imm0 target::reservation-discharge)
  (strcx. rzero rzero imm0)
  (blr))

(defppclapfunction %staticp ((x arg_z))
  (check-nargs 1)
  (ref-global temp0 tenured-area)
  (ldr imm1 target::area.low temp0)
  (sub imm0 x imm1)
  (ldr imm1 target::area.static-dnodes temp0)
  (srri imm0 imm0 target::dnode-shift)
  (li arg_z nil)
  (cmplr imm0 imm1)
  (bgelr)
  (box-fixnum arg_z imm0)
  (blr))

(defppclapfunction %static-inverse-cons ((n arg_z))
  (check-nargs 1)
  (ref-global temp0 tenured-area)
  (ldr imm1 target::area.low temp0)
  (add imm1 n imm1)
  (add imm1 n imm1)
  (la arg_z target::fulltag-cons imm1)
  (blr))
  

; end of ppc-misc.lisp
