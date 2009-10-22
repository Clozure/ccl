; -*- Mode: Lisp; Package: CCL; -*-
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


(in-package "CCL")
#+x8664-target
(progn

(defx86lapfunction %address-of ((arg arg_z))
  ;; %address-of a fixnum is a fixnum, just for spite.
  ;; %address-of anything else is the address of that thing as an integer.
  (testb ($ x8664::fixnummask) (%b arg))
  (je @done)
  (movq (% arg) (% imm0))
  (jmp-subprim .SPmakeu64)
  @done
  (single-value-return))

;;; "areas" are fixnum-tagged and, for the most part, so are their
;;; contents.

;;; The nilreg-relative global all-areas is a doubly-linked-list header
;;; that describes nothing.  Its successor describes the current/active
;;; dynamic heap.  Return a fixnum which "points to" that area, after
;;; ensuring that the "active" pointers associated with the current thread's
;;; stacks are correct.



(defx86lapfunction %normalize-areas ()
  (let ((address temp0)
        (temp temp1))

    ; update active pointer for tsp area.
    (movq (:rcontext x8664::tcr.ts-area) (% address))
    (movq (:rcontext x8664::tcr.save-tsp) (% temp))
    (movq (% temp) (@ x8664::area.active (% address)))
    
    ;; Update active pointer for vsp area.
    (movq (:rcontext x8664::tcr.vs-area) (% address))
    (movq (% rsp) (@ x8664::area.active (% address)))

    (ref-global all-areas arg_z)
    (movq (@ x8664::area.succ (% arg_z)) (% arg_z))

    (single-value-return)))

(defx86lapfunction %active-dynamic-area ()
  (ref-global all-areas arg_z)
  (movq (@ x8664::area.succ (% arg_z)) (% arg_z))
  (single-value-return))

  
(defx86lapfunction %object-in-stack-area-p ((object arg_y) (area arg_z))
  (movq (@ x8664::area.active (% area)) (% imm0))
  (movq (@ x8664::area.high (% area)) (% imm1))
  (rcmp (% object) (% imm0))
  (movq ($ nil) (% arg_z))
  (movq ($ t) (% imm0))
  (jb @done)
  (rcmp (% object) (% imm1))
  (cmovbq (% imm0) (% arg_z))
  @done
  (single-value-return))

(defx86lapfunction %object-in-heap-area-p ((object arg_y) (area arg_z))
  (rcmp (% object) (@ x8664::area.low (% area)))
  (setae (%b imm0))
  (rcmp (% object) (@ x8664::area.low (% area)))
  (setb (%b imm1))
  (andb (% imm1.b) (% imm0.b))
  (andl ($ x8664::t-offset) (%l imm0))
  (lea (@ (target-nil-value) (% imm0)) (% arg_z))
  (single-value-return))




(defx86lapfunction walk-static-area ((a arg_y) (f arg_z))
  (let ((fun save0)
        (obj save1)
        (limit save2))
    (save-simple-frame)
    (push (% fun))
    (push (% obj))
    (push (% limit))
    (movq (% f) (% fun))
    (movq (@ x8664::area.active (% a)) (% limit))
    (movq (@ x8664::area.low (% a)) (% obj))
    (jmp @test)
    @loop
    (movb (@ (% obj)) (% imm0.b))
    (andb ($ x8664::fulltagmask) (% imm0.b))
    (cmpb ($ x8664::fulltag-nodeheader-0) (% imm0.b))
    (je @misc)
    (cmpb ($ x8664::fulltag-nodeheader-1) (% imm0.b))
    (je @misc)
    (cmpb ($ x8664::fulltag-immheader-0) (% imm0.b))
    (je @misc)
    (cmpb ($ x8664::fulltag-immheader-2) (% imm0.b))
    (je @misc)
    (cmpb ($ x8664::fulltag-immheader-1) (% imm0.b))
    (jne @cons)
    @misc
    (lea (@ x8664::fulltag-misc (% obj)) (% obj))
    (movq (% obj) (% arg_z))
    (set-nargs 1)
    (:talign 4)
    (call (% fun))
    (recover-fn-from-rip)
    (getvheader obj imm1)
    (movb (% imm1.b) (% imm0.b))
    (andb ($ x8664::fulltagmask) (% imm0.b))
    (cmpb ($ x8664::fulltag-nodeheader-0) (% imm0.b))
    (je @64)
    (cmpb ($ x8664::fulltag-nodeheader-1) (% imm0.b))
    (je @64)
    (cmpb ($ x8664::ivector-class-64-bit) (% imm0.b))
    (jne @not64)
    @64
    (shrq ($ x8664::num-subtag-bits) (% imm1))
    (shlq ($ x8664::word-shift) (% imm1))
    (jmp @uvector-next)
    @not64
    (cmpb ($ x8664::ivector-class-32-bit) (% imm0.b))
    (jne @not32)
    (shrq ($ x8664::num-subtag-bits) (% imm1))
    (shlq ($ 2) (% imm1))
    (jmp @uvector-next)
    @not32
    (cmpb ($ (- x8664::subtag-bit-vector 256)) (% imm1.b))
    (jne @not-bit)
    (shrq ($ x8664::num-subtag-bits) (% imm1))
    (addq ($ 7) (% imm1))
    (shrq ($ 3) (% imm1))
    (jmp @uvector-next)
    @not-bit
    (rcmpb (% imm1.b) ($ (- x8664::min-8-bit-ivector-subtag 256)))
    (jb @16)
    (shrq ($ x8664::num-subtag-bits) (% imm1))
    (jmp @uvector-next)
    @16
    (shrq ($ x8664::num-subtag-bits) (% imm1))
    (shlq ($ 1) (% imm1))
    (jmp @uvector-next)
    @cons
    (addq ($ x8664::fulltag-cons) (% obj))
    (movq (% obj) (% arg_z))
    (set-nargs 1)
    (:talign 4)
    (call (% fun))
    (recover-fn-from-rip)
    (addq ($ (- x8664::cons.size x8664::fulltag-cons)) (% obj))
    (jmp @test)
    ;; size of OBJ in bytes (without header or alignment padding)
    ;; in imm1.
    @uvector-next
    (addq ($ (+ x8664::node-size (1- x8664::dnode-size))) (% imm1))
    (andb ($ (lognot (1- x8664::dnode-size))) (% imm1.b))
    (lea (@ (- x8664::fulltag-misc) (% obj) (% imm1)) (% obj))
    @test
    (cmpq (% limit) (% obj))
    (jb @loop)
    (pop (% limit))
    (pop (% obj))
    (pop (% fun))
    (movl ($ (target-nil-value)) (% arg_z.l))
    (restore-simple-frame)
    (single-value-return)))



;;; This walks the active "dynamic" area.  Objects might be moving around
;;; while we're doing this, so we have to be a lot more careful than we 
;;; are when walking a static area.
;;; There are a couple of approaches to termination:
;;;  a) Allocate a "sentinel" cons, and terminate when we run into it.
;;;  b) Check the area limit (which is changing if we're consing) and
;;;     terminate when we hit it.
;;; (b) loses if the function conses.  (a) conses.  I can't think of anything
;;; better than (a).
;;; This, of course, assumes that any GC we're doing does in-place compaction
;;; (or at least preserves the relative order of objects in the heap.)

(defx86lapfunction %walk-dynamic-area ((a arg_y) (f arg_z))
  (let ((fun save0)
        (obj save1)
        (limit save2))
    (save-simple-frame)
    (push (% fun))
    (push (% obj))
    (push (% limit))
    (movq (% f) (% fun))
    (ref-global tenured-area a)
    (movq (@ x8664::area.low (% a)) (% obj))
    (subq ($ (- x8664::cons.size x8664::fulltag-cons))
          (:rcontext x8664::tcr.save-allocptr))
    (movq (:rcontext x8664::tcr.save-allocptr) (% allocptr))
    (cmpq (:rcontext x8664::tcr.save-allocbase) (% allocptr))
    (ja @ok)
    (uuo-alloc)
    @ok
    (andb ($ (lognot x8664::fulltagmask))
          (:rcontext x8664::tcr.save-allocptr))
    (movq (% allocptr) (% limit))
    (jmp @test)
    @loop
    (movb (@ (% obj)) (% imm0.b))
    (andb ($ x8664::fulltagmask) (% imm0.b))
    (cmpb ($ x8664::fulltag-nodeheader-0) (% imm0.b))
    (je @misc)
    (cmpb ($ x8664::fulltag-nodeheader-1) (% imm0.b))
    (je @misc)
    (cmpb ($ x8664::fulltag-immheader-0) (% imm0.b))
    (je @misc)
    (cmpb ($ x8664::fulltag-immheader-2) (% imm0.b))
    (je @misc)
    (cmpb ($ x8664::fulltag-immheader-1) (% imm0.b))
    (jne @cons)
    @misc
    (lea (@ x8664::fulltag-misc (% obj)) (% obj))
    (movq (% obj) (% arg_z))
    (set-nargs 1)
    (:talign 4)
    (call (% fun))
    (recover-fn-from-rip)
    (getvheader obj imm1)
    (movb (% imm1.b) (% imm0.b))
    (andb ($ x8664::fulltagmask) (% imm0.b))
    (cmpb ($ x8664::fulltag-nodeheader-0) (% imm0.b))
    (je @64)
    (cmpb ($ x8664::fulltag-nodeheader-1) (% imm0.b))
    (je @64)
    (cmpb ($ x8664::ivector-class-64-bit) (% imm0.b))
    (jne @not64)
    @64
    (shrq ($ x8664::num-subtag-bits) (% imm1))
    (shlq ($ x8664::word-shift) (% imm1))
    (jmp @uvector-next)
    @not64
    (cmpb ($ x8664::ivector-class-32-bit) (% imm0.b))
    (jne @not32)
    (shrq ($ x8664::num-subtag-bits) (% imm1))
    (shlq ($ 2) (% imm1))
    (jmp @uvector-next)
    @not32
    (cmpb ($ (- x8664::subtag-bit-vector 256)) (% imm1.b))
    (jne @not-bit)
    (shrq ($ x8664::num-subtag-bits) (% imm1))
    (addq ($ 7) (% imm1))
    (shrq ($ 3) (% imm1))
    (jmp @uvector-next)
    @not-bit
    (rcmpb (% imm1.b) ($ (- x8664::min-8-bit-ivector-subtag 256)))
    (jb @16)
    (shrq ($ x8664::num-subtag-bits) (% imm1))
    (jmp @uvector-next)
    @16
    (shrq ($ x8664::num-subtag-bits) (% imm1))
    (shlq ($ 1) (% imm1))
    (jmp @uvector-next)
    @cons
    (addq ($ x8664::fulltag-cons) (% obj))
    (cmpq (% obj) (% limit))
    (movq (% obj) (% arg_z))
    (je @done)
    (set-nargs 1)
    (:talign 4)
    (call (% fun))
    (recover-fn-from-rip)
    (addq ($ (- x8664::cons.size x8664::fulltag-cons)) (% obj))
    (jmp @test)
    ;; size of OBJ in bytes (without header or alignment padding)
    ;; in imm1.
    @uvector-next
    (addq ($ (+ x8664::node-size (1- x8664::dnode-size))) (% imm1))
    (andb ($ (lognot (1- x8664::dnode-size))) (% imm1.b))
    (lea (@ (- x8664::fulltag-misc) (% obj) (% imm1)) (% obj))
    @test
    (cmpq (% limit) (% obj))
    (jb @loop)
    @done
    (pop (% limit))
    (pop (% obj))
    (pop (% fun))
    (movl ($ (target-nil-value)) (% arg_z.l))
    (restore-simple-frame)
    (single-value-return)))

(defun walk-dynamic-area (area func)
  (with-other-threads-suspended
      (%walk-dynamic-area area func)))



(defx86lapfunction %class-of-instance ((i arg_z))
  (svref i instance.class-wrapper arg_z)
  (svref arg_z %wrapper-class arg_z)
  (single-value-return))

(defx86lapfunction class-of ((x arg_z))
  (check-nargs 1)
  (movw ($ (logior (ash 1 x8664::tag-list)
                   (ash 1 x8664::tag-imm-1)))
        (%w imm1))
  (extract-lisptag x imm0)
  (btw (% imm0.w) (% imm1.w))
  (cmovbl (% arg_z.l) (% imm0.l))
  (movq (@ '*class-table* (% fn)) (% temp1))
  (cmpb ($ x8664::tag-misc) (% imm0.b))
  (jne @have-tag)
  (extract-subtag x imm0)
  @have-tag
  (movq (@ x8664::symbol.vcell (% temp1)) (% temp1))
  (movzbl (% imm0.b) (% imm0.l))
  (movq (@ x8664::misc-data-offset (% temp1) (% imm0) 8) (% temp0))
  (cmpb ($ x8664::fulltag-nil) (%b temp0))
  (je @bad)
  (extract-fulltag temp0 imm0)
  (cmpb ($ x8664::fulltag-function) (%b imm0))
  (jne @ret)
  (set-nargs 1)
  (jmp (% temp0))
  @bad
  (load-constant no-class-error fname)
  (set-nargs 1)
  (jmp  (@ x8664::symbol.fcell (% fname)))
  @ret
  (movq (% temp0) (% arg_z))  ; return frob from table
  (single-value-return))

(defx86lapfunction full-gccount ()
  (ref-global tenured-area arg_z)
  (testq (% arg_z) (% arg_z))
  (cmoveq (@ (+ (target-nil-value) (x8664::%kernel-global 'gc-count))) (% arg_z))
  (cmovneq (@ x8664::area.gc-count (% arg_z)) (% arg_z))
  (single-value-return))


(defx86lapfunction gc ()
  (check-nargs 0)
  (movq ($ arch::gc-trap-function-gc) (% imm0))
  (uuo-gc-trap)
  (movq ($ nil) (% arg_z))
  (single-value-return))


(defx86lapfunction egc ((arg arg_z))
  "Enable the EGC if arg is non-nil, disables the EGC otherwise. Return
the previous enabled status. Although this function is thread-safe (in
the sense that calls to it are serialized), it doesn't make a whole lot
of sense to be turning the EGC on and off from multiple threads ..."
  (check-nargs 1)
  (clrq imm1)
  (cmp-reg-to-nil arg)
  (setne (% imm1.b))
  (movq ($ arch::gc-trap-function-egc-control) (% imm0))
  (uuo-gc-trap)
  (single-value-return))




(defx86lapfunction %configure-egc ((e0size arg_x)
				   (e1size arg_y)
				   (e2size arg_z))
  (check-nargs 3)
  (movq ($ arch::gc-trap-function-configure-egc) (% imm0))
  (uuo-gc-trap)
  (single-value-return))

(defx86lapfunction purify ()
  (check-nargs 0)
  (movq ($ arch::gc-trap-function-purify) (% imm0))
  (uuo-gc-trap)
  (movq ($ nil) (% arg_z))
  (single-value-return))


(defx86lapfunction impurify ()
  (check-nargs 0)
  (movq ($ arch::gc-trap-function-impurify) (% imm0))
  (uuo-gc-trap)
  (movq ($ nil) (% arg_z))
  (single-value-return))


(defx86lapfunction lisp-heap-gc-threshold ()
  "Return the value of the kernel variable that specifies the amount
of free space to leave in the heap after full GC."
  (check-nargs 0)
  (movq ($ arch::gc-trap-function-get-lisp-heap-threshold) (% imm0))
  (uuo-gc-trap)
  #+x8632-target
  (jmp-subprim .SPmakeu32)
  #+x8664-target
  (jmp-subprim .SPmakeu64))

(defx86lapfunction set-lisp-heap-gc-threshold ((new arg_z))
  "Set the value of the kernel variable that specifies the amount of free
space to leave in the heap after full GC to new-value, which should be a
non-negative fixnum. Returns the value of that kernel variable (which may
be somewhat larger than what was specified)."
  (check-nargs 1)
  (save-simple-frame)
  (call-subprim .SPgetu64)
  (movq (% imm0) (% imm1))
  (movq ($ arch::gc-trap-function-set-lisp-heap-threshold) (% imm0))
  (uuo-gc-trap)
  (restore-simple-frame)
  (jmp-subprim .SPmakeu64))


(defx86lapfunction use-lisp-heap-gc-threshold ()
  "Try to grow or shrink lisp's heap space, so that the free space is (approximately) equal to the current heap threshold. Return NIL"
  (check-nargs 0) 
  (movq ($ arch::gc-trap-function-use-lisp-heap-threshold) (% imm0))
  (uuo-gc-trap)
  (movl ($ (target-nil-value)) (%l arg_z))
  (single-value-return))

(defx86lapfunction freeze ()
  "Do a full GC, then consider all heap-allocated objects which survive to be non-relocatable."
  (movl ($ arch::gc-trap-function-freeze) (% imm0.l))
  (uuo-gc-trap)
  (jmp-subprim .SPmakeu64))

(defx86lapfunction flash-freeze ()
  "Like FREEZE, without the GC."
  (movl ($ arch::gc-trap-function-flash-freeze) (% imm0.l))
  (uuo-gc-trap)
  (jmp-subprim .SPmakeu64))

(defx86lapfunction %watch ((thing arg_z))
  (check-nargs 1)
  (movl ($ arch::watch-trap-function-watch) (%l imm0))
  (uuo-watch-trap)
  (single-value-return))

(defx86lapfunction %unwatch ((watched arg_y) (new arg_z))
  (check-nargs 2)
  (movl ($ arch::watch-trap-function-unwatch) (%l imm0))
  (uuo-watch-trap)
  (single-value-return))

(defx86lapfunction %allocate-list ((initial-element arg_y) (nconses arg_z))
  (check-nargs 2)
  (save-simple-frame)
  (ud2a)
  (:byte 10)
  (push (% arg_z))
  (push (% allocptr))
  (set-nargs 2)
  (jmp-subprim .SPnvalret))

  


;;; offset is a fixnum, one of the x8664::kernel-import-xxx constants.
;;; Returns that kernel import, a fixnum.
(defx86lapfunction %kernel-import ((offset arg_z))
  (ref-global kernel-imports imm0)
  (unbox-fixnum arg_z imm1)
  (movq (@ (% imm0) (% imm1)) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx86lapfunction %get-unboxed-ptr ((macptr arg_z))
  (macptr-ptr arg_z imm0)
  (movq (@ (% imm0)) (% arg_z))
  (single-value-return))


(defx86lapfunction %revive-macptr ((p arg_z))
  (movb ($ x8664::subtag-macptr) (@ x8664::misc-subtag-offset (% p)))
  (single-value-return))

(defx86lapfunction %macptr-type ((p arg_z))
  (check-nargs 1)
  (trap-unless-typecode= p x8664::subtag-macptr)
  (svref p x8664::macptr.type-cell imm0)
  (box-fixnum imm0 arg_z)
  (single-value-return))
  
(defx86lapfunction %macptr-domain ((p arg_z))
  (check-nargs 1)
  (trap-unless-typecode= p x8664::subtag-macptr)
  (svref p x8664::macptr.domain-cell imm0)
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx86lapfunction %set-macptr-type ((p arg_y) (new arg_z))
  (check-nargs 2)
  (unbox-fixnum new imm1)
  (trap-unless-typecode= p x8664::subtag-macptr)
  (svset p x8664::macptr.type-cell imm1)
  (single-value-return))

(defx86lapfunction %set-macptr-domain ((p arg_y) (new arg_z))
  (check-nargs 2)
  (unbox-fixnum new imm1)
  (trap-unless-typecode= p x8664::subtag-macptr)
  (svset p x8664::macptr.domain-cell imm1)
  (single-value-return))

(defx86lapfunction true ()
  (pop (% ra0))
  (subq ($ '3) (% nargs.q))
  (leaq (@ '2 (% rsp) (% nargs.q)) (% imm0))
  (cmovaq (% imm0) (% rsp))
  (movl ($ (target-t-value)) (%l arg_z))
  (push (% ra0))
  (single-value-return))

(defx86lapfunction false ()
  (pop (% ra0))
  (subq ($ '3) (% nargs.q))
  (leaq (@ '2 (% rsp) (% nargs.q)) (% imm0))
  (cmovaq (% imm0) (% rsp))
  (movl ($ (target-nil-value)) (%l arg_z))
  (push (% ra0))
  (single-value-return))



;;; end
) ; #+x8664-target
