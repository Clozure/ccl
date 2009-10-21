;;; -*- Mode: Lisp; Package: CCL -*-
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

;;; level-0;x86;x86-misc.lisp


(in-package "CCL")
#+x8664-target
(progn

;;; Copy N bytes from pointer src, starting at byte offset src-offset,
;;; to ivector dest, starting at offset dest-offset.
;;; It's fine to leave this in lap.
;;; Depending on alignment, it might make sense to move more than
;;; a byte at a time.
;;; Does no arg checking of any kind.  Really.

(defx86lapfunction %copy-ptr-to-ivector ((src (* 2 x8664::node-size) )
                                         (src-byte-offset (* 1 x8664::node-size))
                                         #|(ra 0)|#
                                         (dest arg_x)
                                         (dest-byte-offset arg_y)
                                         (nbytes arg_z))
  (let ((rsrc temp0)
        (rsrc-byte-offset temp1))
    (testq (% nbytes) (% nbytes))
    (movq (@ src-byte-offset (% rsp)) (% rsrc-byte-offset))         ; boxed src-byte-offset
    (movq (@ src (% rsp)) (% rsrc))     ; src macptr
    (jmp @test)
    @loop
    (unbox-fixnum rsrc-byte-offset imm0)
    (addq ($ '1) (% rsrc-byte-offset))
    (addq (@ x8664::macptr.address (% rsrc)) (% imm0))
    (movb (@ (% imm0)) (%b imm0))
    (unbox-fixnum dest-byte-offset imm1)
    (addq ($ '1) (% dest-byte-offset))
    (movb (%b imm0) (@ x8664::misc-data-offset (% dest) (% imm1)))
    (subq ($ '1) (% nbytes))
    @test
    (jne @loop)
    (movq (% dest) (% arg_z))
    (single-value-return 4)))

(defx86lapfunction %copy-ivector-to-ptr ((src (* 2 x8664::node-size))
                                         (src-byte-offset (* 1 x8664::node-size))
                                         #|(ra 0)|#
                                         (dest arg_x)
                                         (dest-byte-offset arg_y)
                                         (nbytes arg_z))
  (let ((rsrc temp0)
        (rsrc-byte-offset temp1))
    (testq (% nbytes) (% nbytes))
    (movq (@ src-byte-offset (% rsp)) (% rsrc-byte-offset))
    (movq (@ src (% rsp)) (% rsrc))
    (jmp @test)
    @loop
    (unbox-fixnum rsrc-byte-offset imm0)
    (addq ($ '1) (% rsrc-byte-offset))
    (movb (@ x8664::misc-data-offset (% rsrc) (% imm0)) (%b imm0))
    (unbox-fixnum dest-byte-offset imm1)
    (addq ($ '1) (% dest-byte-offset))
    (addq (@ x8664::macptr.address (%q dest)) (% imm1))
    (movb (%b imm0) (@ (% imm1)))
    (subq ($ '1) (% nbytes))
    @test
    (jne @loop)
    (movq (% dest) (% arg_z))
    (single-value-return 4)))


(defun %copy-ivector-to-ivector (src src-byte-offset dest dest-byte-offset nbytes)
  (declare (fixnum src-byte-offset dest-byte-offset nbytes))
  (if (or (eq src dest)
          (not (eql 0 src-byte-offset))
          (not (eql 0 dest-byte-offset))
          (< nbytes 8))
    (%copy-ivector-to-ivector-bytes src src-byte-offset dest dest-byte-offset nbytes)
    (%copy-ivector-to-ivector-words src dest (ash nbytes -3) (logand nbytes 7))))

(defx86lapfunction %copy-ivector-to-ivector-words ((src 8)
                                                   #|(ra 0)|#
                                                   (dest arg_x)
                                                   (nwords arg_y)
                                                   (nbytes arg_z))
  (let ((rsrc temp0)
         (ridx imm1)
         (rval imm0))
    (xorl (%l ridx) (%l ridx))
    (movq (@ src (% rsp)) (% rsrc))
    (jmp @word-test)
    @word-loop
    (movq (@ x8664::misc-data-offset (% rsrc) (% ridx)) (% rval))
    (movq (% rval) (@ x8664::misc-data-offset (% dest) (% ridx)))
    (addq ($ 8) (% ridx))
    @word-test
    (cmpq (% ridx) (% nwords))
    (jne @word-loop)
    (jmp @byte-test)
    @byte-loop
    (movb (@ x8664::misc-data-offset (% rsrc) (% ridx)) (%b rval))
    (movb (%b rval) (@ x8664::misc-data-offset (% dest) (% ridx)))
    (addq ($ 1) (% ridx))
    @byte-test
    (subq ($ '1) (% nbytes))
    (jns @byte-loop)
    (movq (% dest) (% arg_z))
    (single-value-return 3)))
          
    
    

(defx86lapfunction %copy-ivector-to-ivector-bytes ((src-offset 16) 
                                                   (src-byte-offset 8)
                                                   #|(ra 0)|#
                                                   (dest arg_x)
                                                   (dest-byte-offset arg_y)
                                                   (nbytes arg_z))
  (let ((rsrc temp0)
        (rsrc-byte-offset temp1))
    (movq (@ src-byte-offset (% rsp)) (% rsrc-byte-offset))
    (movq (@ src-offset (% rsp)) (% rsrc))
    (cmpq (% dest) (% rsrc))
    (jne @front)
    (cmpq (% src-byte-offset) (% dest-byte-offset))
    (jg @back)
    @front
    (testq (% nbytes) (% nbytes))
    (jmp @front-test)
    @front-loop
    (unbox-fixnum rsrc-byte-offset imm0)
    (addq ($ '1) (% rsrc-byte-offset))
    (movb (@ x8664::misc-data-offset (% rsrc) (% imm0)) (%b imm0))
    (unbox-fixnum dest-byte-offset imm1)
    (addq ($ '1) (% dest-byte-offset))
    (movb (%b imm0) (@ x8664::misc-data-offset (% dest) (% imm1)))
    (subq ($ '1) (% nbytes))
    @front-test
    (jne @front-loop)
    (movq (% dest) (% arg_z))
    (single-value-return 4)
    @back
    (addq (% nbytes) (% rsrc-byte-offset))
    (addq (% nbytes) (% dest-byte-offset))
    (testq (% nbytes) (% nbytes))
    (jmp @back-test)
    @back-loop
    (subq ($ '1) (% rsrc-byte-offset))
    (unbox-fixnum rsrc-byte-offset imm0)
    (movb (@ x8664::misc-data-offset (% rsrc) (% imm0)) (%b imm0))
    (subq ($ '1) (% dest-byte-offset))
    (unbox-fixnum dest-byte-offset imm1)
    (subq ($ '1) (% nbytes))
    (movb (%b imm0) (@ x8664::misc-data-offset (% dest) (% imm1)))
    @back-test
    (jne @back-loop)
    (movq (% dest) (% arg_z))
    (single-value-return 4)))
  

(defx86lapfunction %copy-gvector-to-gvector ((src (* 2 x8664::node-size))
					     (src-element (* 1 x8664::node-size))
                                             #|(ra 0)|#
					     (dest arg_x)
					     (dest-element arg_y)
					     (nelements arg_z))
  (let ((rsrc temp0)
        (rsrc-element imm1)
        (val temp1))
    (movq (@ src-element (% rsp)) (% rsrc-element))
    (movq (@ src (% rsp)) (% rsrc))
    (cmpq (% rsrc) (% dest))
    (jne @front)
    (rcmp (% rsrc-element) (% dest-element))
    (jl @back)
    @front
    (testq (% nelements) (% nelements))
    (jmp @front-test)
    @front-loop
    (movq (@ x8664::misc-data-offset (% rsrc) (% rsrc-element)) (% val))
    (addq ($ '1) (% rsrc-element))
    (movq (% val) (@ x8664::misc-data-offset (% dest) (% dest-element)))
    (addq ($ '1) (% dest-element))
    (subq ($ '1) (% nelements))
    @front-test
    (jne @front-loop)
    (movq (% dest) (% arg_z))
    (single-value-return 4)
    @back
    (addq (% nelements) (% rsrc-element))
    (addq (% nelements) (% dest-element))
    (testq (% nelements) (% nelements))
    (jmp @back-test)
    @back-loop
    (subq ($ '1) (% rsrc-element))
    (movq (@ x8664::misc-data-offset (% rsrc) (% rsrc-element)) (% val))
    (subq ($ '1) (% dest-element))
    (movq (% val) (@ x8664::misc-data-offset (% dest) (% dest-element)))
    (subq ($ '1) (% nelements))
    @back-test
    (jne @back-loop)
    (movq (% dest) (% arg_z))
    (single-value-return 4)))

(defx86lapfunction %heap-bytes-allocated ()
  (movq (:rcontext x8664::tcr.save-allocptr) (% temp1))
  (movq (:rcontext x8664::tcr.last-allocptr) (% temp0))
  (cmpq ($ -16) (% temp1))
  (movq (:rcontext x8664::tcr.total-bytes-allocated) (% imm0))
  (jz @go)
  (movq (% temp0) (% temp2))
  (subq (% temp1) (% temp0))
  (testq (% temp2) (% temp2))
  (jz @go)
  (add (% temp0) (% imm0))
  @go
  (jmp-subprim .SPmakeu64))


(defx86lapfunction values ()
  (:arglist (&rest values))
  (save-frame-variable-arg-count)
  (push-argregs)
  (jmp-subprim .SPnvalret))

(defx86lapfunction rdtsc ()
  (:byte #x0f)                          ;two-byte rdtsc opcode
  (:byte #x31)                          ;is #x0f #x31
  (shlq ($ 32) (% rdx))
  (orq (% rdx) (% rax))
  (imul ($ (* 2 target::node-size)) (% rax) (% arg_z))
  (shrq ($ 1) (% arg_z))
  (single-value-return))

;;; Return all 64 bits of the time-stamp counter as an unsigned integer.
(defx86lapfunction rdtsc64 ()
  (:byte #x0f)                          ;two-byte rdtsc opcode
  (:byte #x31)                          ;is #x0f #x31
  (shlq ($ 32) (% rdx))
  (orq (% rdx) (% rax))
  (jmp-subprim .SPmakeu64))

;;; It would be nice if (%setf-macptr macptr (ash (the fixnum value)
;;; ash::fixnumshift)) would do this inline.

(defx86lapfunction %setf-macptr-to-object ((macptr arg_y) (object arg_z))
  (check-nargs 2)
  (trap-unless-typecode= macptr x8664::subtag-macptr)
  (movq (% object) (@ x8664::macptr.address (% macptr)))
  (single-value-return))

(defx86lapfunction %fixnum-from-macptr ((macptr arg_z))
  (check-nargs 1)
  (trap-unless-typecode= arg_z x8664::subtag-macptr)
  (movq (@ x8664::macptr.address (% arg_z)) (% imm0))
  (trap-unless-lisptag= imm0 x8664::tag-fixnum imm1)
  (movq (% imm0) (% arg_z))
  (single-value-return))


(defx86lapfunction %%get-unsigned-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr x8664::subtag-macptr)
  (macptr-ptr ptr imm1)
  (unbox-fixnum offset imm0)
  (movq (@ (% imm1) (% imm0)) (% imm0))
  (jmp-subprim .SPmakeu64))


(defx86lapfunction %%get-signed-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr x8664::subtag-macptr)
  (macptr-ptr ptr imm1)
  (unbox-fixnum offset imm0)
  (movq (@ (% imm1) (% imm0)) (% imm0))
  (jmp-subprim .SPmakes64))




(defx86lapfunction %%set-unsigned-longlong ((ptr arg_x)
                                            (offset arg_y)
                                            (val arg_z))
  (save-simple-frame)
  (trap-unless-typecode= ptr x8664::subtag-macptr)
  (call-subprim .SPgetu64)
  (macptr-ptr ptr imm2)
  (unbox-fixnum offset imm1)
  (movq (% imm0) (@ (% imm2) (% imm1)))
  (restore-simple-frame)
  (single-value-return))


(defx86lapfunction %%set-signed-longlong ((ptr arg_x)
                                          (offset arg_y)
                                          (val arg_z))
  (save-simple-frame)
  (trap-unless-typecode= ptr x8664::subtag-macptr)
  (call-subprim .SPgets64)
  (macptr-ptr ptr imm2)
  (unbox-fixnum offset imm1)
  (movq (% imm0) (@ (% imm2) (% imm1)))
  (restore-simple-frame)
  (single-value-return))

(defx86lapfunction interrupt-level ()
  (movq (:rcontext x8664::tcr.tlb-pointer) (% imm1))
  (movq (@ x8664::interrupt-level-binding-index (% imm1)) (% arg_z))
  (single-value-return))

(defx86lapfunction set-interrupt-level ((new arg_z))
  (movq (:rcontext x8664::tcr.tlb-pointer) (% imm1))
  (trap-unless-fixnum new)
  (movq (% new) (@ x8664::interrupt-level-binding-index (% imm1)))
  (single-value-return))

(defx86lapfunction %current-tcr ()
  (movq (:rcontext x8664::tcr.linear) (% arg_z))
  (single-value-return))

(defx86lapfunction %tcr-toplevel-function ((tcr arg_z))
  (check-nargs 1)
  (cmpq (% tcr) (:rcontext x8664::tcr.linear))
  (movq (% rsp) (% imm0))
  (movq (@ x8664::tcr.vs-area (% tcr)) (% temp0))
  (movq (@ x8664::area.high (% temp0)) (% imm1))
  (jz @room)
  (movq (@ x8664::area.active (% temp0)) (% imm0))
  @room
  (cmpq (% imm1) (% imm0))
  (movl ($ (target-nil-value)) (%l arg_z))
  (cmovneq (@ (- x8664::node-size) (% imm1)) (% arg_z))
  (single-value-return))

(defx86lapfunction %set-tcr-toplevel-function ((tcr arg_y) (fun arg_z))
  (check-nargs 2)
  (cmpq (% tcr) (:rcontext x8664::tcr.linear))
  (movq (% rsp) (% imm0))
  (movq (@ x8664::tcr.vs-area (% tcr)) (% temp0))
  (movq (@ x8664::area.high (% temp0)) (% imm1))
  (jz @room)
  (movq (@ x8664::area.active (% temp0)) (% imm0))
  @room
  (cmpq (% imm1) (% imm0))
  (leaq (@ (- x8664::node-size) (% imm1)) (% imm1))
  (movq ($ 0) (@ (% imm1)))
  (jne @have-room)
  (movq (% imm1) (@ x8664::area.active (% temp0)))
  (movq (% imm1) (@ x8664::tcr.save-vsp (% tcr)))
  @have-room
  (movq (% fun) (@ (% imm1)))
  (single-value-return))

;;; This needs to be done out-of-line, to handle EGC memoization.
(defx86lapfunction %store-node-conditional ((offset 8) #|(ra 0)|# (object arg_x) (old arg_y) (new arg_z))
  (movq (@ offset (% rsp)) (% temp0))
  (save-simple-frame)
  (call-subprim .SPstore-node-conditional)
  (restore-simple-frame)
  (single-value-return 3))

(defx86lapfunction %store-immediate-conditional ((offset 8) #|(ra 0)|# (object arg_x) (old arg_y) (new arg_z))
  (movq (@ offset (% rsp)) (% temp0))
  (unbox-fixnum temp0 imm1)
  @again
  (movq (@ (% object) (% imm1)) (% rax))
  (cmpq (% rax) (% old))
  (jne @lose)
  (lock)
  (cmpxchgq (% new) (@ (% object) (% imm1)))
  (jne @again)
  (movl ($ (target-t-value)) (%l arg_z))
  (single-value-return 3)
  @lose
  (movl ($ (target-nil-value)) (%l arg_z))
  (single-value-return 3))

(defx86lapfunction set-%gcable-macptrs% ((ptr x8664::arg_z))
  @again
  (movq (@ (+ (target-nil-value) (x8664::kernel-global gcable-pointers)))
        (% rax))
  (movq (% rax) (@ x8664::xmacptr.link (% ptr)))
  (lock)
  (cmpxchgq (% ptr) (@ (+ (target-nil-value) (x8664::kernel-global gcable-pointers))))
  (jne @again)
  (single-value-return))

;;; Atomically increment or decrement the gc-inhibit-count kernel-global
;;; (It's decremented if it's currently negative, incremented otherwise.)
(defx86lapfunction %lock-gc-lock ()
  @again
  (movq (@ (+ (target-nil-value) (x8664::kernel-global gc-inhibit-count))) (% rax))
  (lea (@ '-1 (% rax)) (% temp0))
  (lea (@ '1 (% rax)) (% arg_z))
  (testq (% rax) (% rax))
  (cmovsq (% temp0) (% arg_z))
  (lock)
  (cmpxchgq (% arg_z) (@ (+ (target-nil-value) (x8664::kernel-global gc-inhibit-count))))
  (jnz @again)
  (single-value-return))

;;; Atomically decrement or increment the gc-inhibit-count kernel-global
;;; (It's incremented if it's currently negative, incremented otherwise.)
;;; If it's incremented from -1 to 0, try to GC (maybe just a little.)
(defx86lapfunction %unlock-gc-lock ()
  @again
  (movq (@ (+ (target-nil-value) (x8664::kernel-global gc-inhibit-count)))
        (% rax))
  (lea (@ '1 (% rax)) (% arg_x))
  (cmpq ($ -1) (% rax))
  (lea (@ '-1 (% rax)) (% arg_z))
  (cmovleq (% arg_x) (% arg_z))
  (lock)
  (cmpxchgq (% arg_z) (@ (+ (target-nil-value) (x8664::kernel-global gc-inhibit-count))))
  (jne @again)
  (cmpq ($ '-1) (% rax))
  (jne @done)
  ;; The GC tried to run while it was inhibited.  Unless something else
  ;; has just inhibited it, it should be possible to GC now.
  (mov ($ arch::gc-trap-function-immediate-gc) (% imm0))
  (uuo-gc-trap)
  @done
  (single-value-return))

;;; Return true iff we were able to increment a non-negative
;;; lock._value




(defx86lapfunction %atomic-incf-node ((by arg_x) (node arg_y) (disp arg_z))
  (check-nargs 3)
  (unbox-fixnum disp imm1)
  @again
  (movq (@ (% node) (% imm1)) (% rax))
  (lea (@ (% rax) (% by)) (% arg_z))
  (lock)
  (cmpxchgq (% arg_z) (@ (% node) (% imm1)))
  (jne @again)
  (single-value-return))

(defx86lapfunction %atomic-incf-ptr ((ptr arg_z))
  (macptr-ptr ptr imm2)
  @again
  (movq (@ (% imm2)) (% rax))
  (lea (@ 1 (% rax)) (% imm1))
  (lock)
  (cmpxchgq (% imm1) (@ (% imm2)))
  (jne @again)
  (box-fixnum imm1 arg_z)
  (single-value-return))

(defx86lapfunction %atomic-incf-ptr-by ((ptr arg_y) (by arg_z))
  (macptr-ptr ptr imm2)
  @again
  (movq (@ (% imm2)) (% rax))
  (unbox-fixnum by imm1)
  (add (% rax) (% imm1))
  (lock)
  (cmpxchgq (% imm1) (@ (% imm2)))
  (jnz @again)
  (box-fixnum imm1 arg_z)
  (single-value-return))


(defx86lapfunction %atomic-decf-ptr ((ptr arg_z))
  (macptr-ptr ptr imm2)
  @again
  (movq (@ (% imm2)) (% rax))
  (lea (@ -1 (% rax)) (% imm1))
  (lock)
  (cmpxchgq (% imm1) (@ (% imm2)))
  (jnz @again)
  (box-fixnum imm1 arg_z)
  (single-value-return))

(defx86lapfunction %atomic-decf-ptr-if-positive ((ptr arg_z))
  (macptr-ptr ptr imm2)
  @again
  (movq (@ (% imm2)) (% rax))
  (testq (% rax) (% rax))
  (lea (@ -1 (% rax)) (% imm1))
  (jz @done)
  (lock)
  (cmpxchgq (% imm1) (@ (% imm2)))
  (jnz @again)
  @done
  (box-fixnum imm1 arg_z)
  (single-value-return))


(defx86lapfunction %atomic-swap-ptr ((ptr arg_y) (newval arg_z))
  (macptr-ptr arg_y imm1)
  (unbox-fixnum newval imm0)
  (lock)
  (xchgq (% imm0) (@ (% imm1)))
  (box-fixnum imm0 arg_z)
  (single-value-return))

;;; Try to store the fixnum NEWVAL at PTR, if and only if the old value
;;; was equal to OLDVAL.  Return the old value
(defx86lapfunction %ptr-store-conditional ((ptr arg_x) (expected-oldval arg_y) (newval arg_z))
  (macptr-ptr ptr imm2)
  @again
  (movq (@ (% imm2)) (% imm0))
  (box-fixnum imm0 temp0)
  (cmpq (% temp0) (% expected-oldval))
  (jne @done)
  (unbox-fixnum newval imm1)
  (lock)
  (cmpxchgq (% imm1) (@ (% imm2)))
  (jne @again)
  @done
  (movq (% temp0) (% arg_z))
  (single-value-return))

(defx86lapfunction %ptr-store-fixnum-conditional ((ptr arg_x) (expected-oldval arg_y) (newval arg_z))
  (let ((address imm1))
    (macptr-ptr ptr address)
    @again
    (movq (@ (% address)) (% imm0))
    (cmpq (% imm0) (% expected-oldval))
    (jne @done)
    (lock)
    (cmpxchgq (% newval) (@ (% address)))
    (jne @again)
    @done
    (movq (% imm0) (% arg_z))
    (single-value-return)))

(defx86lapfunction xchgl ((newval arg_y) (ptr arg_z))
  (unbox-fixnum newval imm0)
  (macptr-ptr ptr imm1)
  (lock)                                ; implicit ?
  (xchgl (% imm0.l) (@ (% imm1)))
  (box-fixnum imm0 arg_z)
  (single-value-return))
  
                          


(defx86lapfunction %macptr->dead-macptr ((macptr arg_z))
  (check-nargs 1)
  (movb ($ x8664::subtag-dead-macptr) (@ x8664::misc-subtag-offset (% macptr)))
  (single-value-return))




  
(defx86lapfunction %%save-application ((flags arg_y) (fd arg_z))
  (unbox-fixnum flags imm0)
  (orq ($ arch::gc-trap-function-save-application) (% imm0))
  (unbox-fixnum fd imm1)
  (uuo-gc-trap)
  (single-value-return))



(defx86lapfunction %misc-address-fixnum ((misc-object arg_z))
  (check-nargs 1)
  (lea (@ x8664::misc-data-offset (% misc-object)) (% arg_z))
  (single-value-return))


(defx86lapfunction fudge-heap-pointer ((ptr arg_x) (subtype arg_y) (len arg_z))
  (check-nargs 3)
  (macptr-ptr ptr imm1) ; address in macptr
  (lea (@ 17 (% imm1)) (% imm0))     ; 2 for delta + 15 for alignment
  (andb ($ -16) (%b  imm0))   ; Clear low four bits to align
  (subq (% imm0) (% imm1))  ; imm1 = -delta
  (negw (%w imm1))
  (movw (%w imm1) (@  -2 (% imm0)))     ; save delta halfword
  (unbox-fixnum subtype imm1)  ; subtype at low end of imm1
  (shlq ($ (- x8664::num-subtag-bits x8664::fixnum-shift)) (% len ))
  (orq (% len) (% imm1))
  (movq (% imm1) (@ (% imm0)))       ; store subtype & length
  (lea (@ x8664::fulltag-misc (% imm0)) (% arg_z)) ; tag it, return it
  (single-value-return))

(defx86lapfunction %%make-disposable ((ptr arg_y) (vector arg_z))
  (check-nargs 2)
  (lea (@ (- x8664::fulltag-misc) (% vector)) (% imm0)) ; imm0 is addr = vect less tag
  (movzwq (@ -2 (% imm0)) (% imm1))     ; get delta
  (subq (% imm1) (% imm0))              ; vector addr (less tag)  - delta is orig addr
  (movq (% imm0) (@ x8664::macptr.address (% ptr)))
  (single-value-return))


(defx86lapfunction %vect-data-to-macptr ((vect arg_y) (ptr arg_z))
  (lea (@ x8664::misc-data-offset (% vect)) (% imm0))
  (movq (% imm0) (@ x8664::macptr.address (% ptr)))
  (single-value-return))

(defx86lapfunction get-saved-register-values ()
  (movq (% rsp) (% temp0))
  (push (% save0))
  (push (% save1))
  (push (% save2))
  (push (% save3))                      ; this'd be the TCR on Win64.
  (set-nargs 4)
  (jmp-subprim .SPvalues))


(defx86lapfunction %current-db-link ()
  (movq (:rcontext x8664::tcr.db-link) (% arg_z))
  (single-value-return))

(defx86lapfunction %no-thread-local-binding-marker ()
  (movq ($ x8664::subtag-no-thread-local-binding) (% arg_z))
  (single-value-return))


(defx86lapfunction pending-user-interrupt ()
  (xorq (% imm0) (% imm0))
  (ref-global x8664::intflag arg_z)
  ;; If another signal happens now, it will get ignored, same as if it happened
  ;; before whatever signal is in arg_z.  But then these are async signals, so
  ;; who can be sure it didn't actually happen just before...
  (set-global imm0 x8664::intflag)
  (single-value-return))


(defx86lapfunction debug-trap-with-string ((arg arg_z))
  (check-nargs 1)
  (uuo-error-debug-trap-with-string)
  (single-value-return))

(defx86lapfunction %safe-get-ptr ((src arg_y) (dest arg_z))
  (check-nargs 2)
  (save-simple-frame)
  (macptr-ptr src imm0)
  (leaq (@ (:^ done) (% fn)) (% ra0))
  (movq (% imm0) (:rcontext x8664::tcr.safe-ref-address))
  (movq (@ (% imm0)) (% imm0))
  (jmp done)
  (:tra done)
  (recover-fn-from-rip)
  (movq ($ 0) (:rcontext x8664::tcr.safe-ref-address))
  (movq (% imm0) (@ x8664::macptr.address (% dest)))
  (restore-simple-frame)
  (single-value-return))

;;; This was intentded to work around a bug in #_nanosleep in early
;;; Leopard test releases.  It's probably not necessary any more; is
;;; it still called ?

(defx86lapfunction %check-deferred-gc ()
  (btq ($ (+ arch::tcr-flag-bit-pending-suspend target::fixnumshift)) (:rcontext x8664::tcr.flags))
  (movl ($ (target-nil-value)) (% arg_z.l))
  (jae @done)
  (ud2a)
  (:byte 3)
  (movl ($ (target-t-value)) (% arg_z.l))
  @done
  (single-value-return))

(defx86lapfunction %%tcr-interrupt ((target arg_z))
  (check-nargs 1)
  (ud2a)
  (:byte 4)
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx86lapfunction %suspend-tcr ((target arg_z))
  (check-nargs 1)
  (ud2a)
  (:byte 5)
  (movzbl (%b imm0) (%l imm0))
  (testl (%l imm0) (%l imm0))
  (movl ($ (target-nil-value)) (%l arg_z))
  (cmovnel (@ (+ target::t-offset target::symbol.vcell) (% arg_z)) (%l arg_z))
  (single-value-return))

(defx86lapfunction %suspend-other-threads ()
  (check-nargs 0)
  (ud2a)
  (:byte 6)
  (movl ($ (target-nil-value)) (%l arg_z))
  (single-value-return))

(defx86lapfunction %resume-tcr ((target arg_z))
  (check-nargs 1)
  (ud2a)
  (:byte 7)
  (movzbl (%b imm0) (%l imm0))
  (testl (%l imm0) (%l imm0))
  (movl ($ (target-nil-value)) (%l arg_z))
  (cmovnel (@ (+ target::t-offset target::symbol.vcell) (% arg_z)) (%l arg_z))
  (single-value-return))

(defx86lapfunction %resume-other-threads ()
  (check-nargs 0)
  (ud2a)
  (:byte 8)
  (movl ($ (target-nil-value)) (%l arg_z))
  (single-value-return))

(defx86lapfunction %kill-tcr ((target arg_z))
  (check-nargs 1)
  (ud2a)
  (:byte 9)
  (testb (%b imm0) (%b imm0))
  (movl ($ (target-nil-value)) (%l arg_z))
  (cmovnel (@ (+ target::t-offset target::symbol.vcell) (% arg_z)) (%l arg_z))
  (single-value-return))
  

(defx86lapfunction %get-spin-lock ((p arg_z))
  (check-nargs 1)
  (save-simple-frame)
  @again
  (macptr-ptr arg_z imm1)
  (movq (@ '*spin-lock-tries* (% fn)) (% temp0))
  (movq (@ '*spin-lock-timeouts* (% fn)) (% temp1))
  (movq (@ target::symbol.vcell (% temp0)) (% temp0))
  (movq (:rcontext x8664::tcr.linear) (% arg_y))
  @try-swap
  (xorq (% rax) (% rax))
  (lock)
  (cmpxchgq (% arg_y) (@ (% imm1)))
  (je @done)
  @spin
  (pause)
  (cmpq ($ 0) (@ (% imm1)))
  (je @try-swap)
  (subq ($ '1) (% temp0))
  (jne @spin)
  @wait
  (addq ($ x8664::fixnumone) (@ x8664::symbol.vcell (% temp1)))
  (pushq (% arg_z))
  (call-symbol yield 0)
  (popq (% arg_z))
  (jmp @again)
  @done
  (restore-simple-frame)
  (single-value-return))

;;; This is a prototype; it can't easily keep its arguments on the stack,
;;; or in registers, because its job involves unwinding the stack and
;;; restoring registers.  Its parameters are thus kept in constants,
;;; and this protoype is cloned (with the right parameters).

;;; For win64 (which doesn't really have a "save3" register), the code
;;; which instantiates this should always set save3-offset to 0.
(defx86lapfunction %%apply-in-frame-proto ()
  (:fixed-constants (target-frame target-catch target-db-link target-xcf target-tsp target-foreign-sp save0-offset save1-offset save2-offset save3-offset function args))
  (check-nargs 0)
  ;;(uuo-error-debug-trap)
  (movq (@ 'target-catch (% fn)) (% temp0))
  (xorl (%l imm0) (%l imm0))
  (cmpb ($ x8664::fulltag-nil) (%b temp0))
  (movq (:rcontext target::tcr.catch-top) (% arg_z))
  (jz @did-catch)
  @find-catch
  (testq (% arg_z) (% arg_z))
  (jz @did-catch)                       ; never found target catch
  (addq ($ '1)  (% imm0))
  (cmpq (% temp0) (% arg_z))
  (je @found-catch)
  (movq (@ target::catch-frame.link (% arg_z)) (% arg_z))
  (jmp @find-catch)
  @found-catch
  (set-nargs 0)                         ; redundant, but ...
  (lea (@ (:^ @back-from-nthrow) (% fn)) (% ra0))
  (:talign 4)
  (jmp-subprim .SPnthrowvalues)
  @back-from-nthrow
  (recover-fn-from-rip)
  @did-catch
  ;; Restore special bindings
  (movq (@ 'target-db-link (% fn)) (% imm0))
  (cmpb ($ x8664::fulltag-nil) (%b imm0))
  (jz @no-unbind)
  (call-subprim .SPunbind-to)
  @no-unbind
  ;; If there's at least one exception frame between the target
  ;; frame and the last catch (or the point of departure), restore
  ;; the NVRs and foreign sp from the oldest such frame
  (movq (@ 'target-xcf (% fn)) (% arg_z))
  (cmpb ($ x8664::fulltag-nil) (%b arg_z))
  (jz @no-xcf)
  (movq (@ target::xcf.xp (% arg_z)) (% arg_y))
  ;; arg_y points to a "portable" ucontext.  Find the platform-specifc
  ;; "gpr vector" in the uc_mcontext, load the NVRs and stack/frame
  ;; pointer from there.
  #+linuxx8664-target
  (progn
    (addq ($ gp-regs-offset) (% arg_y))
    (movq (@ (* #$REG_R15 8) (% arg_y)) (% r15))
    (movq (@ (* #$REG_R14 8) (% arg_y)) (% r14))
    (movq (@ (* #$REG_R12 8) (% arg_y)) (% r12))
    (movq (@ (* #$REG_R11 8) (% arg_y)) (% r11))
    (movq (@ (* #$REG_RBP 8) (% arg_y)) (% rbp))
    (movq (@ (* #$REG_RSP 8) (% arg_y)) (% rsp)))
  #+freebsdx8664-target
  (progn
    ;; If you think that this is ugly, just wait until you see the Darwin
    ;; version.
    (addq ($ gp-regs-offset) (% arg_y))
    (movq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__mcontext)) :mc_r15)) -3) (% arg_y)) (% r15))
    (movq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__mcontext)) :mc_r14)) -3) (% arg_y)) (% r14))
    (movq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__mcontext)) :mc_r12)) -3) (% arg_y)) (% r12))
    (movq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__mcontext)) :mc_r11)) -3) (% arg_y)) (% r11))
    (movq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__mcontext)) :mc_rbp)) -3) (% arg_y)) (% rbp))
    (movq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__mcontext)) :mc_rsp)) -3) (% arg_y)) (% rsp)))
  #+darwinx8664-target
  (progn
    ;; Yes, this is ugly.
    (movq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__darwin_ucontext)) :uc_mcontext)) -3) (% arg_y)) (% arg_y))
    (addq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__darwin_mcontext64)) :__ss)) -3)) (% arg_y))
    (movq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__darwin_x86_thread_state64)) :__r15)) -3) (% arg_y)) (% r15))
    (movq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__darwin_x86_thread_state64)) :__r14)) -3) (% arg_y)) (% r14))
    (movq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__darwin_x86_thread_state64)) :__r12)) -3) (% arg_y)) (% r12))
    (movq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__darwin_x86_thread_state64)) :__r11)) -3) (% arg_y)) (% r11))
    (movq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__darwin_x86_thread_state64)) :__rbp)) -3) (% arg_y)) (% rbp))
    (movq (@ (ash (foreign-record-field-offset (%find-foreign-record-type-field (parse-foreign-type '(:struct :__darwin_x86_thread_state64)) :__rsp)) -3) (% arg_y)) (% rsp)))
  ;; This is our best (possibly only) chance to get
  ;; the foreign sp right.
  (movq (@ target::xcf.prev-xframe (% arg_z)) (% temp0))
  (movq (@ target::xcf.foreign-sp (% arg_z)) (% imm0))
  (movq (% temp0) (:rcontext target::tcr.xframe))
  (movq (% imm0) (:rcontext target::tcr.foreign-sp))
  ;; All done processing the xcf.  NVRs may have been
  ;; saved between the last catch/last xcf and the
  ;; target frame.  The save-n-offset parameter/constants
  ;; are either 0 or negative offsets from the target frame
  ;; of the stack location where the corresponding GPR
  ;; was saved.
  @no-xcf
  (movq (@ 'target-tsp (% fn)) (% imm0))
  (cmpb ($ x8664::fulltag-nil) (%b imm0))
  (movq (@ 'target-foreign-sp (% fn)) (% temp0))
  (je @no-tsp)
  (movq (% imm0) (:rcontext target::tcr.save-tsp))
  (movq (% imm0) (:rcontext target::tcr.next-tsp))
  @no-tsp
  (cmpb ($ x8664::fulltag-nil) (%b temp0))
  (je @no-sp)
  (movq (% temp0) (:rcontext target::tcr.foreign-sp))
  @no-sp
  (movq (@ 'target-frame (% fn)) (% rbp))
  (movq (@ 'save0-offset (% fn)) (% arg_x))
  (movq (@ 'save1-offset (% fn)) (% arg_y))
  (movq (@ 'save2-offset (% fn)) (% arg_z))
  (movq (@ 'save3-offset (% fn)) (% temp0))
  (testq (% arg_x) (% arg_x))
  (cmovneq (@ (% rbp) (% arg_x)) (% save0))
  (testq (% arg_y) (% arg_y))
  (cmovneq (@ (% rbp) (% arg_x)) (% save1))
  (testq (% arg_z) (% arg_z))
  (cmovneq (@ (% rbp) (% arg_x)) (% save2))
  (testq (% temp0) (% temp0))
  (cmovneq (@ (% rbp) (% arg_x)) (% save3))
  (leave)
  (pop (% temp0))                       ; return address, not used by subprim
  (set-nargs 0)
  (movq (@ 'args (% fn)) (% arg_z))
  (lea (@ (:^ @back-from-spread) (% fn)) (% ra0))
  (:talign 4)
  (jmp-subprim .SPspreadargz)
  @back-from-spread
  (recover-fn-from-rip)                 ; .SPspreadargz preserves %fn, but ...
  (push (% temp0))                      ; return address
  (jmp (@ 'function (% fn))))
  

(defx86lapfunction %atomic-pop-static-cons ()
  @again
  (movq (@ (+ (target-nil-value) (x8664::kernel-global static-conses))) (% rax))
  (cmpq ($ (target-nil-value)) (% rax))
  (jz @lose)
  (%cdr rax temp0)
  (lock)
  (cmpxchgq (% temp0) (@ (+ (target-nil-value) (x8664::kernel-global static-conses))))
  (jnz @again)
  @lose
  (movq (% rax) (% arg_z))
  (single-value-return))

(defx86lapfunction %augment-static-conses ((head arg_y) (tail arg_z))
  @again
  (movq (@ (+ (target-nil-value) (x8664::kernel-global static-conses))) (% rax))
  (movq (% rax) (@ target::cons.cdr (% tail)))
  (lock)
  (cmpxchgq (% head) (@ (+ (target-nil-value) (x8664::kernel-global static-conses))))
  (jnz @again)
  @lose
  (movl ($ (target-nil-value)) (% arg_z.l))
  (single-value-return))
  
(defx86lapfunction %staticp ((x arg_z))
  (check-nargs 1)
  (ref-global tenured-area temp0)
  (movq (% x) (% imm0))
  (subq (@ target::area.low (% temp0)) (% imm0))
  (shrq ($ target::dnode-shift) (% imm0))
  (cmpq (@ target::area.static-dnodes (% temp0)) (% imm0))
  (leaq (@ (% imm0) target::fixnumone) (% arg_z))
  (movl ($ (target-nil-value)) (%l imm0))
  (cmovaeq (% imm0) (% arg_z))
  (single-value-return))

(defx86lapfunction %static-inverse-cons ((n arg_z))
  (check-nargs 1)
  (ref-global tenured-area temp0)
  (movq (@ target::area.low (% temp0)) (% imm0))
  (leaq (@ target::fulltag-cons (% imm0) (% n) 2) (% arg_z))
  (single-value-return))


  

;;; end of x86-misc.lisp
) ; #+x8664-target
