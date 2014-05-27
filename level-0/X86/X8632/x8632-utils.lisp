;;; Copyright 2009 Clozure Associates
;;; This file is part of Clozure CL.  
;;;
;;; Clozure CL is licensed under the terms of the Lisp Lesser GNU
;;; Public License , known as the LLGPL and distributed with Clozure
;;; CL as the file "LICENSE".  The LLGPL consists of a preamble and
;;; the LGPL, which is distributed with Clozure CL as the file "LGPL".
;;; Where these conflict, the preamble takes precedence.
;;;
;;; Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;; The LLGPL is also available online at
;;; http://opensource.franz.com/preamble.html

(in-package "CCL")

(defx8632lapfunction %address-of ((arg arg_z))
  ;; %address-of a fixnum is a fixnum, just for spite.
  ;; %address-of anything else is the address of that thing as an integer.
  (testb ($ x8632::fixnummask) (%b arg))
  (je @done)
  (movl (% arg) (% imm0))
  (jmp-subprim .SPmakeu32)
  @done
  (single-value-return))

;;; "areas" are fixnum-tagged and, for the most part, so are their
;;; contents.

;;; The nilreg-relative global all-areas is a doubly-linked-list header
;;; that describes nothing.  Its successor describes the current/active
;;; dynamic heap.  Return a fixnum which "points to" that area, after
;;; ensuring that the "active" pointers associated with the current thread's
;;; stacks are correct.

(defx8632lapfunction %normalize-areas ()
  (let ((address temp0)
        (temp temp1))

    ; update active pointer for tsp area.
    (movl (:rcontext x8632::tcr.ts-area) (% address))
    (movl (:rcontext x8632::tcr.save-tsp) (% temp))
    (movl (% temp) (@ x8632::area.active (% address)))
    
    ;; Update active pointer for vsp area.
    (movl (:rcontext x8632::tcr.vs-area) (% address))
    (movl (% esp) (@ x8632::area.active (% address)))

    (ref-global all-areas arg_z)
    (movl (@ x8632::area.succ (% arg_z)) (% arg_z))

    (single-value-return)))

(defx8632lapfunction %active-dynamic-area ()
  (ref-global all-areas arg_z)
  (movl (@ x8632::area.succ (% arg_z)) (% arg_z))
  (single-value-return))

(defx8632lapfunction %object-in-stack-area-p ((object arg_y) (area arg_z))
  (rcmp (% object) (@ x8632::area.active (% area)))
  (movl ($ nil) (% temp0))
  (movl ($ t) (% imm0))
  (jb @done)
  (rcmp (% object) (@ x8632::area.high (% area)))
  (cmovbl (% imm0) (% temp0))
  @done
  (movl (% temp0) (% arg_z))
  (single-value-return))

(defx8632lapfunction %object-in-heap-area-p ((object arg_y) (area arg_z))
  (rcmp (% object) (@ x8632::area.low (% area)))
  (movl ($ nil) (% temp0))
  (movl ($ t) (% imm0))
  (jb @done)
  (rcmp (% object) (@ x8632::area.active (% area)))
  (cmovbl (% imm0) (% temp0))
  @done
  (movl (% temp0) (% arg_z))
  (single-value-return))

;;; In these heap-walking functions, all other threads should be
;;; suspended; the only consing that should happen is any consing
;;; that the function (the "f" argument) does when we call it.
;;;
;;; We can therefore basically walk dnode-aligned addresses (but we
;;; have to be careful, especially in the %WALK-DYNAMIC-AREA case,
;;; to hold onto only tagged pointers when we call the funtion, since
;;; consing by the called function could cause a gc).

(defx8632lapfunction walk-static-area ((a arg_y) (f arg_z))
  (let ((obj temp0)
	(fun -4)
	(limit -8))
    (save-simple-frame)
    (push (% f))
    (pushl (@ x8632::area.active (% a)))
    (movl (@ x8632::area.low (% a)) (% obj))
    (jmp @test)
    @loop
    (movb (@ (% obj)) (% imm0.b))
    (andb ($ x8632::fulltagmask) (% imm0.b))
    (cmpb ($ x8632::fulltag-immheader) (% imm0.b))
    (je @misc)
    (cmpb ($ x8632::fulltag-nodeheader) (% imm0.b))
    (je @misc)
    ;; not a header, so must be a cons
    (add ($ x8632::fulltag-cons) (% obj))
    (mov (% obj) (% arg_z))
    (set-nargs 1)
    (push (% obj))
    (:talign 5)
    (call (@ fun (% ebp)))
    (recover-fn)
    (pop (% obj))
    (add ($ (- x8632::cons.size x8632::fulltag-cons)) (% obj))
    (jmp @test)
    @misc
    (lea (@ x8632::fulltag-misc (% obj)) (% arg_z))
    (set-nargs 1)
    (push (% obj))
    (:talign 5)
    (call (@ fun (% ebp)))
    (recover-fn)
    (pop (% obj))
    (mov (@ (% obj)) (% imm0))
    (andb ($ x8632::fulltagmask) (% imm0.b))
    (cmpb ($ x8632::fulltag-nodeheader) (% imm0.b))
    (mov (@ (% obj)) (% imm0))
    (je @32)
    (cmpb ($ x8632::max-32-bit-ivector-subtag) (% imm0.b))
    (jbe @32)
    (cmpb ($ x8632::max-8-bit-ivector-subtag) (% imm0.b))
    (jbe @8)
    (cmpb ($ x8632::max-16-bit-ivector-subtag) (% imm0.b))
    (jbe @16)
    (cmpb ($ x8632::subtag-complex-double-float-vector) (% imm0.b))
    (je @complex-double-float-vector)
    (cmpb ($ x8632::subtag-bit-vector) (% imm0.b))
    (jne @double-float)
    ;; if we get here, it's a bit vector
    (shrl ($ x8632::num-subtag-bits) (% imm0))
    (add ($ 7) (% imm0))
    (shrl ($ 3) (% imm0))
    (jmp @uvector-next)
    @double-float
    (shrl ($ x8632::num-subtag-bits) (% imm0))
    (shll ($ 3) (% imm0))
    (jmp @uvector-next)
    @complex-double-float-vector
    (shrl ($ x8632::num-subtag-bits) (% imm0))
    (shll ($ 4) (% imm0))
    (jmp @uvector-next)
    @8
    (shrl ($ x8632::num-subtag-bits) (% imm0))
    (jmp @uvector-next)
    @16
    (shrl ($ x8632::num-subtag-bits) (% imm0))
    (shll ($ 1) (% imm0))
    (jmp @uvector-next)
    @32
    (shrl ($ x8632::num-subtag-bits) (% imm0))
    (shll ($ 2) (% imm0))
    ;; size of obj in bytes (without header or alignment padding)
    ;; is in imm0
    @uvector-next
    (add ($ (+ x8632::node-size (1- x8632::dnode-size))) (% imm0))
    (andb ($ (lognot (1- x8632::dnode-size))) (% imm0.b))
    (add (% imm0) (% obj))
    @test
    (cmpl (@ limit (% ebp)) (% obj))
    (jb @loop)
    (movl ($ (target-nil-value)) (% arg_z))
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

(defx8632lapfunction %walk-dynamic-area ((a arg_y) (f arg_z))
  (let ((obj temp0)
	(fun -4)
	(sentinel -8))
    (save-simple-frame)
    (push (% f))
    (subl ($ (- x8632::cons.size x8632::fulltag-cons))
	  (:rcontext x8632::tcr.save-allocptr))
    (movl (:rcontext x8632::tcr.save-allocptr) (% allocptr)) ;aka temp0
    (cmpl (:rcontext x8632::tcr.save-allocbase) (% allocptr))
    (ja @ok)
    (uuo-alloc)
    @ok
    (andb ($ (lognot x8632::fulltagmask))
	  (:rcontext x8632::tcr.save-allocptr))
    (push (% allocptr))			;sentinel
    (ref-global tenured-area a)
    (movl (@ x8632::area.low (% a)) (% obj))
    (jmp @test)
    @loop
    (movb (@ (% obj)) (% imm0.b))
    (andb ($ x8632::fulltagmask) (% imm0.b))
    (cmpb ($ x8632::fulltag-immheader) (% imm0.b))
    (je @misc)
    (cmpb ($ x8632::fulltag-nodeheader) (% imm0.b))
    (je @misc)
    ;; not a header, so must be a cons
    (add ($ x8632::fulltag-cons) (% obj))
    (mov (% obj) (% arg_z))
    (set-nargs 1)
    (push (% obj))
    (:talign 5)
    (call (@ fun (% ebp)))
    (recover-fn)
    (pop (% obj))
    (add ($ (- x8632::cons.size x8632::fulltag-cons)) (% obj))
    (jmp @test)
    @misc
    (add ($ x8632::fulltag-misc) (% obj))
    (mov (% obj) (% arg_z))
    (set-nargs 1)
    (push (% obj))
    (:talign 5)
    (call (@ fun (% ebp)))
    (recover-fn)
    (pop (% obj))
    (sub ($ x8632::fulltag-misc) (% obj))
    (mov (@ (% obj)) (% imm0))
    (andb ($ x8632::fulltagmask) (% imm0.b))
    (cmpb ($ x8632::fulltag-nodeheader) (% imm0.b))
    (mov (@ (% obj)) (% imm0))
    (je @32)
    (cmpb ($ x8632::max-32-bit-ivector-subtag) (% imm0.b))
    (jbe @32)
    (cmpb ($ x8632::max-8-bit-ivector-subtag) (% imm0.b))
    (jbe @8)
    (cmpb ($ x8632::max-16-bit-ivector-subtag) (% imm0.b))
    (jbe @16)
    (cmpb ($ x8632::subtag-complex-double-float-vector) (% imm0.b))
    (je @complex-double-float-vector)
    (cmpb ($ x8632::subtag-bit-vector) (% imm0.b))
    (jne @double-float)
    ;; if we get here, it's a bit vector
    (shrl ($ x8632::num-subtag-bits) (% imm0))
    (add ($ 7) (% imm0))
    (shrl ($ 3) (% imm0))
    (jmp @uvector-next)
    @double-float
    (shrl ($ x8632::num-subtag-bits) (% imm0))
    (shll ($ 3) (% imm0))
    (jmp @uvector-next)
    @complex-double-float-vector
    (shrl ($ x8632::num-subtag-bits) (% imm0))
    (shll ($ 4) (% imm0))
    (jmp @uvector-next)
    @8
    (shrl ($ x8632::num-subtag-bits) (% imm0))
    (jmp @uvector-next)
    @16
    (shrl ($ x8632::num-subtag-bits) (% imm0))
    (shll ($ 1) (% imm0))
    (jmp @uvector-next)
    @32
    (shrl ($ x8632::num-subtag-bits) (% imm0))
    (shll ($ 2) (% imm0))
    ;; size of obj in bytes (without header or alignment padding)
    ;; is in imm0
    @uvector-next
    (add ($ (+ x8632::node-size (1- x8632::dnode-size))) (% imm0))
    (andb ($ (lognot (1- x8632::dnode-size))) (% imm0.b))
    (add (% imm0) (% obj))
    @test
    (cmpl (@ sentinel (% ebp)) (% obj))
    (jb @loop)
    @done
    (movl ($ (target-nil-value)) (% arg_z))
    (restore-simple-frame)
    (single-value-return)))

;;; xxx duplicated in level-0/x86-utils.lisp
(defun walk-dynamic-area (area func)
  (with-other-threads-suspended
      (%walk-dynamic-area area func)))

(defx8632lapfunction %class-of-instance ((i arg_z))
  (svref i instance.class-wrapper arg_z)
  (svref arg_z %wrapper-class arg_z)
  (single-value-return))

(defx8632lapfunction class-of ((x arg_z))
  (check-nargs 1)
  (extract-fulltag x imm0)
  (cmpb ($ x8632::fulltag-misc) (% imm0.b))
  (movl (% arg_z) (% imm0))
  (jne @have-tag)
  (extract-subtag x imm0)
  @have-tag
  (movl (@ '*class-table* (% fn)) (% temp1))
  (movl (@ x8632::symbol.vcell (% temp1)) (% temp1))
  (movzbl (% imm0.b) (% imm0))
  (movl (@ x8632::misc-data-offset (% temp1) (% imm0) 4) (% temp0))
  (cmpl ($ (target-nil-value)) (% temp0))
  (je @bad)
  ;; functionp?
  (extract-typecode temp0 imm0)
  (cmpb ($ x8632::subtag-function) (% imm0.b))
  (jne @ret)
  ;; jump to the function
  (set-nargs 1)
  (jmp (% temp0))
  @bad
  (load-constant no-class-error fname)
  (set-nargs 1)
  (jmp (@ x8632::symbol.fcell (% fname)))
  @ret
  (movl (% temp0) (% arg_z))		;return frob from table
  (single-value-return))

(defx8632lapfunction gc ()
  (check-nargs 0)
  (movl ($ arch::gc-trap-function-gc) (% imm0))
  (uuo-gc-trap)
  (movl ($ nil) (% arg_z))
  (single-value-return))

(defx8632lapfunction full-gccount ()
  (ref-global tenured-area arg_z)
  (test (% arg_z) (% arg_z))
  (cmovel (@ (+ (target-nil-value) (x8632::%kernel-global 'gc-count))) (% arg_z))
  (cmovnel (@ x8632::area.gc-count (% arg_z)) (% arg_z))
  (single-value-return))

(defx8632lapfunction egc ((arg arg_z))
  "Enable the EGC if arg is non-nil, disables the EGC otherwise. Return
the previous enabled status. Although this function is thread-safe (in
the sense that calls to it are serialized), it doesn't make a whole lot
of sense to be turning the EGC on and off from multiple threads ..."
  (check-nargs 1)
  (clrl imm0)
  (cmp-reg-to-nil arg)
  (setne (% imm0.b))
  (movd (% imm0) (% mm0))
  (movl ($ arch::gc-trap-function-egc-control) (% imm0))
  (uuo-gc-trap)
  (single-value-return))

(defx8632lapfunction %configure-egc ((e0size 4)
				     #|(ra 0)|#
				     (e1size arg_y)
				     (e2size arg_z))
  (check-nargs 3)
  (movl (@ e0size (% esp)) (% temp0))
  (movl ($ arch::gc-trap-function-configure-egc) (% imm0))
  (uuo-gc-trap)
  (single-value-return 3))

(defx8632lapfunction purify ()
  (check-nargs 0)
  (movl ($ arch::gc-trap-function-purify) (% imm0))
  (uuo-gc-trap)
  (movl ($ nil) (% arg_z))
  (single-value-return))

(defx8632lapfunction impurify ()
  (check-nargs 0)
  (movl ($ arch::gc-trap-function-impurify) (% imm0))
  (uuo-gc-trap)
  (movl ($ nil) (% arg_z))
  (single-value-return))

(defx8632lapfunction lisp-heap-gc-threshold ()
  "Return the value of the kernel variable that specifies the amount
of free space to leave in the heap after full GC."
  (check-nargs 0)
  (movl ($ arch::gc-trap-function-get-lisp-heap-threshold) (% imm0))
  (uuo-gc-trap)
  (jmp-subprim .SPmakeu32))

(defx8632lapfunction set-lisp-heap-gc-threshold ((new arg_z))
  "Set the value of the kernel variable that specifies the amount of free
space to leave in the heap after full GC to new-value, which should be a
non-negative fixnum. Returns the value of that kernel variable (which may
be somewhat larger than what was specified)."
  (check-nargs 1)
  (save-simple-frame)
  (call-subprim .SPgetu32)
  (movd (% imm0) (% mm0))
  (movl ($ arch::gc-trap-function-set-lisp-heap-threshold) (% imm0))
  (uuo-gc-trap)
  (restore-simple-frame)
  (jmp-subprim .SPmakeu32))

(defx8632lapfunction use-lisp-heap-gc-threshold ()
  "Try to grow or shrink lisp's heap space, so that the free space is (approximately) equal to the current heap threshold. Return NIL"
  (check-nargs 0) 
  (movl ($ arch::gc-trap-function-use-lisp-heap-threshold) (% imm0))
  (uuo-gc-trap)
  (movl ($ (target-nil-value)) (%l arg_z))
  (single-value-return))


(defx8632lapfunction %watch ((uvector arg_z))
  (check-nargs 1)
  (movl ($ arch::watch-trap-function-watch) (%l imm0))
  (uuo-watch-trap)
  (single-value-return))

(defx8632lapfunction %unwatch ((watched arg_y) (new arg_z))
  (check-nargs 2)
  (movl ($ arch::watch-trap-function-unwatch) (%l imm0))
  (uuo-watch-trap)
  (single-value-return))

(defx8632lapfunction %allocate-list ((initial-element arg_y) (nconses arg_z))
  (check-nargs 2)
  (save-simple-frame)
  (ud2a)
  (:byte 10)
  (push (% arg_z))
  (push (% allocptr))
  (set-nargs 2)
  (jmp-subprim .SPnvalret))

(defx8632lapfunction %ensure-static-conses ()
  (check-nargs 0)
  (movl ($ arch::gc-trap-function-ensure-static-conses) (% imm0))
  (uuo-gc-trap)
  (movl ($ (target-nil-value)) (% arg_z))
  (single-value-return))

(defx8632lapfunction set-gc-notification-threshold ((threshold arg_z))
  "Set the value of the kernel variable that can be used to trigger
GC notifications."
  (check-nargs 1)
  (save-simple-frame)
  (call-subprim .SPgetu32)
  (movd (% imm0) (% mm0))
  (movl ($ arch::gc-trap-function-set-gc-notification-threshold) (% imm0))
  (uuo-gc-trap)
  (restore-simple-frame)
  (jmp-subprim .SPmakeu32))
  
(defx8632lapfunction get-gc-notification-threshold ()
  "Get the value of the kernel variable that can be used to trigger
GC notifications."
  (check-nargs 0)
  (movl ($ arch::gc-trap-function-get-gc-notification-threshold) (% imm0))
  (uuo-gc-trap)
  (jmp-subprim .SPmakeu32))

(defx8632lapfunction allow-heap-allocation ((flag arg_z))
  (check-nargs 1)
  (cmpl ($ (target-nil-value)) (% arg_z))
  (setne (%b imm0))
  (andl ($ 1) (%l imm0))
  (movd (% imm0) (% xmm0))
  (movl ($ arch::gc-trap-function-allocation-control) (%l imm0))
  (uuo-gc-trap)
  (single-value-return))

(defx8632lapfunction heap-allocation-allowed-p ()
  (check-nargs 0)
  (movl ($ 2) (% imm0))
  (movd (% imm0) (% xmm0))
  (movl ($ arch::gc-trap-function-allocation-control) (%l imm0))
  (uuo-gc-trap)
  (single-value-return))
  

;;; offset is a fixnum, one of the x8632::kernel-import-xxx constants.
;;; Returns that kernel import, a fixnum.
(defx8632lapfunction %kernel-import ((offset arg_z))
  (unbox-fixnum arg_z imm0)
  (addl (@ (+ (target-nil-value) (x8632::%kernel-global 'kernel-imports))) (% imm0))
  (movl (@ (% imm0)) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx8632lapfunction %get-unboxed-ptr ((macptr arg_z))
  (macptr-ptr arg_z imm0)
  (movl (@ (% imm0)) (% arg_z))
  (single-value-return))

(defx8632lapfunction %revive-macptr ((p arg_z))
  (movb ($ x8632::subtag-macptr) (@ x8632::misc-subtag-offset (% p)))
  (single-value-return))

(defx86lapfunction %macptr-type ((p arg_z))
  (check-nargs 1)
  (trap-unless-typecode= p x8632::subtag-macptr)
  (svref p x8632::macptr.type-cell imm0)
  (box-fixnum imm0 arg_z)
  (single-value-return))
  
(defx86lapfunction %macptr-domain ((p arg_z))
  (check-nargs 1)
  (trap-unless-typecode= p x8632::subtag-macptr)
  (svref p x8632::macptr.domain-cell imm0)
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx8632lapfunction %set-macptr-type ((p arg_y) (new arg_z))
  (check-nargs 2)
  (trap-unless-typecode= p x8632::subtag-macptr)
  (unbox-fixnum new imm0)
  (svset p x8632::macptr.type-cell imm0)
  (single-value-return))

(defx8632lapfunction %set-macptr-domain ((p arg_y) (new arg_z))
  (check-nargs 2)
  (trap-unless-typecode= p x8632::subtag-macptr)
  (unbox-fixnum new imm0)
  (svset p x8632::macptr.domain-cell imm0)
  (single-value-return))

(defx8632lapfunction true ()
  (pop (% temp0))
  (subl ($ '2) (% nargs))
  (leal (@ '2 (% esp) (% nargs)) (% imm0))
  (cmoval (% imm0) (% esp))
  (movl ($ (target-t-value)) (% arg_z))
  (push (% temp0))
  (single-value-return))

(defx8632lapfunction false ()
  (pop (% temp0))
  (subl ($ '2) (% nargs))
  (leal (@ '2 (% esp) (% nargs)) (% imm0))
  (cmoval (% imm0) (% esp))
  (movl ($ (target-nil-value)) (% arg_z))
  (push (% temp0))
  (single-value-return))

(defx8632lapfunction constant-ref ()
  (pop (% temp0))
  (subl ($ '2) (% nargs))
  (leal (@ '2 (% esp) (% nargs)) (% imm0))
  (cmoval (% imm0) (% esp))
  (movl (@ 'constant (% fn)) (% arg_z))
  (push (% temp0))
  (single-value-return))

(defx8632lapfunction int3 ()
  (int ($ 3))
  (single-value-return))
