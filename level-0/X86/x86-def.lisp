;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2006-2009 Clozure Associates and contributors
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

(defx86lapfunction %function-vector-to-function ((arg arg_z))
  (trap-unless-typecode= arg x8664::subtag-function)
  (addb ($ (- x8664::fulltag-function x8664::fulltag-misc)) (% arg_z.b))
  (single-value-return))

(defx86lapfunction %function-to-function-vector  ((arg arg_z))
  (trap-unless-fulltag= arg x8664::fulltag-function)
  (subb ($ (- x8664::fulltag-function x8664::fulltag-misc)) (% arg_z.b))
  (single-value-return))

(defx86lapfunction %function-code-words ((fun arg_z))
  (trap-unless-fulltag= fun x8664::fulltag-function)
  (movl (@ (- x8664::node-size x8664::fulltag-function) (% fun)) (% imm0.l))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx86lapfunction %nth-immediate ((fun arg_y) (n arg_z))
  (trap-unless-fulltag= fun x8664::fulltag-function)
  (movl (@ (- x8664::node-size x8664::fulltag-function) (% fun)) (% imm0.l))
  (lea (@ (% n) (% imm0) 8) (% imm0))
  (movq (@ (- x8664::node-size x8664::fulltag-function) (% fun) (% imm0))
        (% arg_z))
  (single-value-return))

(defx86lapfunction %set-nth-immediate ((fun arg_x) (n arg_y) (new arg_z))
  (trap-unless-fulltag= fun x8664::fulltag-function)
  (movl (@ (- x8664::node-size x8664::fulltag-function) (% fun)) (% imm0.l))
  (lea (@ (% n) (% imm0) 8) (% arg_y))
  (subb ($ (- x8664::fulltag-function x8664::fulltag-misc)) (%b arg_x))
  (jmp-subprim .SPgvset))

(defx86lapfunction %function-code-byte ((fun arg_y) (pc arg_z))
  (unbox-fixnum pc imm0)
  (movzbl (@ (% fun) (% imm0)) (% imm0.l))
  (box-fixnum imm0 arg_z)
  (single-value-return))


;;; Returns 3 values: mask of registers used in the function, stack location
;;; from which they'd be restored, relative PC at which they're saved. If
;;; the mask is 0, the values NIL NIL NIL are returned. If either the stack
;;; location or relative PC is #xff, both of those values will be returned
;;; as NIL.
(defx86lapfunction %function-register-usage ((f arg_z))
  (check-nargs 1)
  (trap-unless-fulltag= f x8664::fulltag-function)
  (movzbl (@ -1 (% f)) (% imm0.l))
  (shll ($ 8) (% imm0.l))
  (box-fixnum imm0 arg_x)
  (movq (% rsp) (% temp0))
  (set-nargs 3)
  (je @no-regs)
  (movzbl (@ -2 (% f)) (% imm0.l))
  (movzbl (@ -3 (% f)) (% imm1.l))
  (cmpb ($ #xff) (% imm0.b))
  (je @unencodable)
  (cmpb ($ #xff) (% imm1.b))
  (je @unencodable)
  (box-fixnum imm0 arg_y)
  (box-fixnum imm1 arg_z)
  (push (% arg_x))
  (push (% arg_y))
  (push (% arg_z))
  (jmp-subprim .SPvalues)
  @unencodable
  (push (% arg_x))
  (pushq ($ nil))
  (pushq ($ nil))
  (jmp-subprim .SPvalues)
  @no-regs
  (pushq ($ nil))
  (pushq ($ nil))
  (pushq ($ nil))
  (jmp-subprim .SPvalues))
  
        

(defx86lapfunction %make-code-executable ((codev arg_z))
  (single-value-return))

;;; Make a new function, with PROTO's code and the specified immediates.
;;; IMMEDIATES should contain lfun-bits as the last element.
(defun %clone-x86-function (proto &rest immediates)
  (declare (dynamic-extent immediates))
  (let* ((protov (%function-to-function-vector proto))
         (code-words (%function-code-words proto))
         (numimms (length immediates))
         (newv (allocate-typed-vector :function (the fixnum (+ code-words numimms)))))
    (declare (fixnum code-words numimms))
    (%copy-ivector-to-ivector protov 0 newv 0 (the fixnum (ash code-words target::word-shift)))
    (do* ((k code-words (1+ k))
          (imms immediates (cdr imms)))
         ((null imms) (%function-vector-to-function newv))
      (declare (fixnum k) (list imms))
      (setf (%svref newv k) (car imms)))))

(defun %copy-function (proto &optional target)
  (let* ((protov (%function-to-function-vector proto))
         (code-words (%function-code-words proto))
         (total-words (uvsize protov))
         (newv (if target
                 (%function-to-function-vector target)
                 (allocate-typed-vector :function total-words))))
    (declare (fixnum code-words total-words))
    (when target
      (unless (and (eql code-words (%function-code-words target))
                   (eql total-words (uvsize newv)))
        (error "Wrong size target ~s" target)))
    (%copy-ivector-to-ivector protov 0 newv 0 (the fixnum (ash code-words target::word-shift)))
    (loop for k fixnum from code-words below total-words
      do (setf (%svref newv k) (%svref protov k)))
    (%function-vector-to-function newv)))

(defun replace-function-code (target proto)
  (let* ((target-words (%function-code-words target))
         (proto-words (%function-code-words proto)))
    (declare (fixnum target-words proto-words))
    (if (= target-words proto-words)
      (progn
        (%copy-ivector-to-ivector (%function-to-function-vector proto)
                                  0
                                  (%function-to-function-vector target)
                                  0
                                  (the fixnum (ash target-words
                                                   target::word-shift)))
        target)
      (error "Code size mismatch: target = ~s, proto = ~s"
             target-words proto-words))))
         

(defx86lapfunction %get-kernel-global-from-offset ((offset arg_z))
  (check-nargs 1)
  (unbox-fixnum offset imm0)
  (movq (@ (target-nil-value) (% imm0)) (% arg_z))
  (single-value-return))

(defx86lapfunction %set-kernel-global-from-offset ((offset arg_y) (new-value arg_z))
  (check-nargs 2)
  (unbox-fixnum offset imm0)
  (movq (% arg_z) (@ (target-nil-value) (% imm0)))
  (single-value-return))


(defx86lapfunction %get-kernel-global-ptr-from-offset ((offset arg_y)
						       (ptr arg_z))
  (check-nargs 2)
  (unbox-fixnum offset imm0)
  (movq (@ (target-nil-value) (% imm0)) (% imm0))
  (movq (% imm0) (@ x8664::macptr.address (% ptr)))
  (single-value-return))




(defx86lapfunction %fixnum-ref ((fixnum arg_y) #| &optional |# (offset arg_z))
  (:arglist (fixnum &optional offset))
  (check-nargs 1 2)
  (cmpl ($ x8664::fixnumone) (% nargs))
  (jne @2-args)
  (movq (% offset) (% fixnum))
  (xorl (%l offset) (%l offset))
  @2-args
  (unbox-fixnum offset imm0)
  (movq (@ (% fixnum) (% imm0)) (% arg_z))
  (single-value-return))

(defx86lapfunction %fixnum-ref-natural ((fixnum arg_y) #| &optional |# (offset arg_z))
  (:arglist (fixnum &optional offset))
  (check-nargs 1 2)
  (cmpl ($ x8664::fixnumone) (% nargs))
  (jne @2-args)
  (movq (% offset) (% fixnum))
  (xorl (%l offset) (%l offset))
  @2-args
  (unbox-fixnum offset imm0)
  (movq (@ (% fixnum) (% imm0)) (% imm0))
  (jmp-subprim .SPmakeu64))

(defx86lapfunction %fixnum-set ((fixnum arg_x) (offset arg_y) #| &optional |# (new-value arg_z))
  (:arglist (fixnum offset &optional newval))
  (check-nargs 2 3)
  (cmpl ($ '2) (% nargs))
  (jne @3-args)
  (movq (% offset) (% fixnum))
  (xorl (%l offset) (%l offset))
  @3-args
  (unbox-fixnum offset imm0)
  (movq (% new-value) (@ (% fixnum) (% imm0)))
  (movq (% new-value) (% arg_z))
  (single-value-return))


(defx86lapfunction %fixnum-set-natural ((fixnum arg_x) (offset arg_y) #| &optional |# (new-value arg_z))
  (:arglist (fixnum offset &optional newval))
  (check-nargs 2 3)
  (save-simple-frame)
  (cmpl ($ '2) (% nargs))
  (jne @3-args)
  (movq (% offset) (% fixnum))
  (xorl (%l offset) (%l offset))
  @3-args
  (call-subprim .SPgetu64)
  (unbox-fixnum offset imm1)
  (movq (% imm0) (@ (% fixnum) (% imm1)))
  (restore-simple-frame)
  (single-value-return))


(defx86lapfunction %current-frame-ptr ()
  (check-nargs 0)
  (movq (% rbp) (% arg_z))
  (single-value-return))


(defx86lapfunction %current-tsp ()
  (check-nargs 0)
  (movq (:rcontext x8664::tcr.save-tsp) (% arg_z))
  (single-value-return))


(defx86lapfunction %%frame-backlink ((p arg_z))
  (check-nargs 1)
  (movq (@ (% arg_z)) (% arg_z))
  (single-value-return))

;;; Look for "lea -nnnn(%rip),%fn" AT the tra; if that's present, use
;;; the dispacement -nnnn to find the function.  The end of the
;;; encoded displacement is
;;; x8664::recover-fn-from-rip-disp-offset (= 7) bytes from the tra.
(defx86lapfunction %return-address-function ((r arg_z))
  (extract-lisptag r imm0)
  (cmpb ($ x8664::tag-tra) (% imm0.b))
  (jne @fail)
  (cmpw ($ x8664::recover-fn-from-rip-word0) (@ (% r)))
  (jne @fail)
  (cmpb ($ x8664::recover-fn-from-rip-byte2) (@ 2 (% r)))
  (movslq (@ x8664::recover-fn-from-rip-disp-offset (% r)) (% imm0))
  (jne @fail)
  (lea (@ x8664::recover-fn-from-rip-length (% imm0) (% r)) (% arg_z))
  (single-value-return)
  @fail
  (movl ($ (target-nil-value)) (% arg_z.l))
  (single-value-return))

(defx86lapfunction %return-address-offset ((r arg_z))
  (extract-lisptag r imm0)
  (cmpb ($ x8664::tag-tra) (% imm0.b))
  (jne @fail)
  (cmpw ($ x8664::recover-fn-from-rip-word0) (@ (% r)))
  (jne @fail)
  (cmpb ($ x8664::recover-fn-from-rip-byte2) (@ 2 (% r)))
  (movslq (@ x8664::recover-fn-from-rip-disp-offset (% r)) (% imm0))
  (jne @fail)
  (negq (% imm0))
  (leaq (@ (- (ash x8664::recover-fn-from-rip-length x8664::fixnumshift)) (% imm0) 8) (% arg_z))
  (single-value-return)
  @fail
  (movl ($ (target-nil-value)) (% arg_z.l))
  (single-value-return))

;;; It's always been the case that the function associated with a
;;; frame pointer is the caller of the function that "uses" that frame.
(defun %cfp-lfun (p)
  (let* ((ra (%fixnum-ref p x8664::lisp-frame.return-address)))
    (if (eq ra (%get-kernel-global ret1valaddr))
      (setq ra (%fixnum-ref p x8664::lisp-frame.xtra)))
    (values (%return-address-function ra) (%return-address-offset ra))))



(defx86lapfunction %uvector-data-fixnum ((uv arg_z))
  (check-nargs 1)
  (trap-unless-fulltag= arg_z x8664::fulltag-misc)
  (addq ($ x8664::misc-data-offset) (% arg_z))
  (single-value-return))

(defx86lapfunction %catch-top ((tcr arg_z))
  (check-nargs 1)
  (movl ($ (target-nil-value)) (%l arg_y))
  (movq (:rcontext x8664::tcr.catch-top) (% arg_z))
  (testb (%b arg_z) (%b arg_z))
  (cmoveq (% arg_y) (% arg_z))
  (single-value-return))

(defx86lapfunction %catch-tsp ((catch arg_z))
  (check-nargs 1)
  (lea (@  (- (+ target::fulltag-misc
                                 (ash 1 (1+ target::word-shift)))) (% arg_z))
       (% arg_z))
  (single-value-return))



;;; Same as %address-of, but doesn't cons any bignums
;;; It also left shift fixnums just like everything else.
(defx86lapfunction %fixnum-address-of ((x arg_z))
  (check-nargs 1)
  (box-fixnum x arg_z)
  (single-value-return))

(defx86lapfunction %save-standard-binding-list ((bindings arg_z))
  (movq (:rcontext x8664::tcr.vs-area) (% imm0))
  (movq (@ x8664::area.high (% imm0)) (% imm1))
  (subq ($ x8664::node-size) (% imm1))
  (movq (% bindings) (@ (% imm1)))
  (single-value-return))

(defx86lapfunction %saved-bindings-address ()
  (movq (:rcontext x8664::tcr.vs-area) (% imm0))
  (movq (@ x8664::area.high (% imm0)) (% imm1))
  (lea (@ (- x8664::node-size) (% imm1)) (% arg_z))
  (single-value-return))

(defx86lapfunction %get-object ((macptr arg_y) (offset arg_z))
  (check-nargs 2)
  (trap-unless-typecode= macptr x8664::subtag-macptr)
  (macptr-ptr macptr imm0)
  (trap-unless-lisptag= offset target::tag-fixnum imm1)
  (unbox-fixnum offset imm1)
  (movq (@ (% imm0) (% imm1)) (% arg_z))
  (single-value-return))


(defx86lapfunction %set-object ((macptr arg_x) (offset arg_y) (value arg_z))
  (check-nargs 3)
  (trap-unless-typecode= macptr target::subtag-macptr)
  (macptr-ptr macptr imm0)
  (trap-unless-lisptag= offset target::tag-fixnum imm1)
  (unbox-fixnum offset imm1)
  (movq (% arg_z) (@ (% imm0) (% imm1)))
  (single-value-return))

(defx86lapfunction %apply-lexpr-with-method-context ((magic arg_x)
                                                     (function arg_y)
                                                     (args arg_z))
  ;; Somebody's called (or tail-called) us.
  ;; Put magic arg in x8664::next-method-context (= x8664::temp0).
  ;; Put function in x8664::xfn until we're ready to jump to it.
  ;; Set nargs to 0, then spread "args" on stack (clobbers arg_x, arg_y, arg_z,
  ;;   but preserves x866::xfn/x8664::next-method-context.
  ;; Jump to the function in x8664::xfn.
  (popq (% ra0))
  (movq (% magic) (% next-method-context))
  (movq (% function) (% xfn))
  (set-nargs 0)
  (movq (@ (% args)) (% imm0))          ;lexpr-count
  (movl (%l imm0) (% nargs))
  (leaq (@ x8664::node-size (% arg_z) (% imm0)) (% imm1))
  (subl ($ '3) (% imm0))
  (jbe @reg-only)
  ;; Some args will be pushed; reserve a frame
  (pushq ($ x8664::reserved-frame-marker))
  (pushq ($ x8664::reserved-frame-marker))
  @pushloop
  (pushq (@ (- x8664::node-size) (% imm1)))
  (subq ($ x8664::node-size) (% imm1))
  (subq ($ x8664::node-size) (% imm0))
  (jne @pushloop)
  @three
  (movq (@ (* x8664::node-size 3) (% arg_z)) (% arg_x))
  @two
  (movq (@ (* x8664::node-size 2) (% arg_z)) (% arg_y))
  @one
  (movq (@ (* x8664::node-size 1) (% arg_z)) (% arg_z))
  (jmp @go)
  @reg-only
  (testl (% nargs) (% nargs))
  (je @go)
  (rcmpl (% nargs) ($ '2))
  (je @two)
  (jb @one)
  (jmp @three)
  @go
  (push (% ra0))
  (jmp (% xfn)))

(defx86lapfunction %apply-with-method-context ((magic arg_x)
                                               (function arg_y)
                                               (args arg_z))
  ;; Somebody's called (or tail-called) us.
  ;; Put magic arg in x8664::next-method-context (= x8664::temp0).
  ;; Put function in x8664::xfn (= x8664::temp1).
  ;; Set nargs to 0, then spread "args" on stack (clobbers arg_x, arg_y, arg_z,
  ;;   but preserves x8664::xfn/x8664::next-method-context.
  ;; Jump to the function in x8664::xfn.
  (pop (% ra0))  
  (movq (% magic) (% x8664::next-method-context))
  (movq (% function) (% x8664::xfn))
  (movq (% args) (% arg_y))             ; in case of error
  (set-nargs 0)
  (xorl (% imm0.l) (% imm0.l))
  (push (% imm0))                       ; reserve frame (might discard
  (push (% imm0))                       ; it if nothing is passed on stack.)
  (cmp-reg-to-nil arg_z)
  (je @done)
  @loop
  (extract-fulltag arg_z imm1)
  (cmpb ($ x8664::fulltag-cons) (%b imm1))
  (jne @bad)
  (%car arg_z arg_x)
  (%cdr arg_z arg_z)
  (lea (@ x8664::node-size (% imm0)) (% imm0))
  (cmp-reg-to-nil arg_z)
  (push (% arg_x))
  (jne @loop)
  @done
  (addl (%l imm0) (% nargs))
  (jne @pop)
  @discard-and-go
  (discard-reserved-frame)
  (jmp @go)
  @pop
  (cmpl($ '1) (% nargs))
  (pop (% arg_z))
  (je @discard-and-go)
  (cmpl ($ '2) (% nargs))
  (pop (% arg_y))
  (je @discard-and-go)
  (cmpl ($ '3) (% nargs))
  (pop (% arg_x))
  (je @discard-and-go)
  @go
  (push (% ra0))
  (jmp (% xfn))
  @bad
  (addq (% imm0) (% rsp))
  (movq (% arg_y) (% arg_z))
  (movq ($ (ash $XNOSPREAD x8664::fixnumshift)) (% arg_y))
  (set-nargs 2)
  (jmp-subprim .SPksignalerr))


;;; The idea here is to call METHOD in the same stack frame in
;;; which the lexpr was originally called.  The lexpr can't
;;; have had any required arguments, %APPLY-LEXPR-TAIL-WISE
;;; must have been tail-called, and the frame built on lexpr
;;; entry must be in %rbp.
(defx86lapfunction %apply-lexpr-tail-wise ((method arg_y) (args arg_z))
  (addq ($ x8664::node-size) (% rsp))   ; discard extra return address
  (movq (% method) (% xfn))
  (movq (% args) (% rsp))
  (pop (%q nargs))
  (movq (@ x8664::lisp-frame.return-address (% rbp)) (% ra0))
  (movq (@ 0 (% rbp)) (% rbp))
  (rcmpl (% nargs) ($ '3))
  (jbe @pop-regs)
  ;; More than 3 args; some must have been pushed by caller,
  ;; so retain the reserved frame.
  (pop (% arg_z))
  (pop (% arg_y))
  (pop (% arg_x))
  (jmp @popped)
  @pop-regs
  (je @pop3)
  (rcmpl (% nargs) ($ '1))
  (jb @discard)
  (ja @pop2)
  (pop (% arg_z))
  (jmp @discard)
  @pop3
  (pop (% arg_z))
  (pop (% arg_y))
  (pop (% arg_x))
  (jmp @discard)
  @pop2
  (pop (% arg_z))
  (pop (% arg_y))
  @discard
  (discard-reserved-frame)
  @popped
  (push (% ra0))
  (jmp (% xfn)))



(defun closure-function (fun)
  (while (and (functionp fun)  (not (compiled-function-p fun)))
    (setq fun (%nth-immediate fun 0))
    (when (vectorp fun)
      (setq fun (svref fun 0))))
  fun)

;;; For use by (setf (apply ...) ...)
;;; (apply+ f butlast last) = (apply f (append butlast (list last)))

(defun apply+ (&lap function arg1 arg2 &rest other-args)
  (x86-lap-function apply+ ()
   (:arglist (function arg1 arg2 &rest other-args))
   (check-nargs 3 nil)
   (cmpl ($ '3) (% nargs))
   (pop (% ra0))
   (ja @no-frame)
   (pushq ($ x8664::reserved-frame-marker))
   (pushq ($ x8664::reserved-frame-marker))
@no-frame         
   (push (% arg_x))
   (movq (% arg_z) (% temp0))           ; last
   (movq (% arg_y) (% arg_z))           ; butlast
   (subl ($ '2) (% nargs))              ; remove count for butlast & last
   ;; Do .SPspreadargz inline here
   (xorl (%l imm0) (%l imm0))
   (movq (% arg_z) (% arg_y))           ; save in case of error
   (cmp-reg-to-nil arg_z)
   (je @done)
   @loop
   (extract-fulltag arg_z imm1)
   (cmpb ($ x8664::fulltag-cons) (%b imm1))
   (jne @bad)
   (%car arg_z arg_x)
   (%cdr arg_z arg_z)
   (addl ($ '1) (%l imm0))
   (cmp-reg-to-nil arg_z)   
   (push (% arg_x))
   (jne @loop)
   @done
   ;; nargs was at least 1 when we started spreading, and can't have gotten
   ;; any smaller. 
   (addl (%l imm0) (% nargs))
   (movq (% temp0) (% arg_z))
   (pop (% arg_y))
   (pop (% arg_x))
   (addl ($ '1) (% nargs))
   (cmpl ($ '3) (% nargs))
   (jne @no-discard)
   (discard-reserved-frame)
   @no-discard
   (load-constant funcall temp0)
   (push (% ra0))
   (jmp-subprim .SPfuncall)
   @bad                                 ; error spreading list.
   (add (% imm0) (% rsp))               ; discard whatever's been pushed
   (movq (% arg_y) (% arg_z))
   (movl ($ '#.$XNOSPREAD) (%l arg_y))
   (set-nargs 2)
   (jmp-subprim .SPksignalerr) ))



;;; This needs to:
;;; (a) load FP arg regs from the FP-REGS argument
;;; (b) call the .SPffcall subprimitive, which will discard the foreign stack frame
;;;     allocated by WITH-VARIABLE-C-FRAME in %FF-CALL
;;; (c) re-establish the same foreign stack frame and store the result regs
;;;     (%rax/%xmm0) there
#-win64-target
(defx86lapfunction %do-ff-call ((nfp 0) (frame arg_x) (fp-regs arg_y) (entry arg_z))
  (popq (% ra0))
  (popq (% rax))
  (movq (% rbp) (@  (% rsp)))
  (movq (% rsp) (% rbp))
  (movq (% ra0) (@ 8 (% rbp)))
  (macptr-ptr fp-regs temp0)
  (sarq ($ x8664::fixnumshift) (% rax))
  (movq (@ (% temp0)) (% fp0))
  (movq (@ 8 (% temp0)) (% fp1))
  (movq (@ 16 (% temp0)) (% fp2))
  (movq (@ 24 (% temp0)) (% fp3))
  (movq (@ 32 (% temp0)) (% fp4))
  (movq (@ 40 (% temp0)) (% fp5))
  (movq (@ 48 (% temp0)) (% fp6))
  (movq (@ 56 (% temp0)) (% fp7))
  (call-subprim .SPffcall)
  (movq (:rcontext x8664::tcr.foreign-sp) (% mm5))
  (movq (% mm5) (@ (% frame)))
  (movq (% frame) (:rcontext x8664::tcr.foreign-sp))
  (movq (% rax) (@ 8 (% frame)))
  (movq (% fp0) (@ 16 (% frame)))
  (movl ($ nil) (%l arg_z))
  (restore-simple-frame)
  (single-value-return))

#+win64-target
(defx86lapfunction %do-ff-call ((frame arg_y) (entry arg_z))
  (save-simple-frame)
  (movq (@ 16 (% frame)) (% fp0))
  (movq (@ 24 (% frame)) (% fp1))
  (movq (@ 32 (% frame)) (% fp2))
  (movq (@ 40 (% frame)) (% fp3))
  (call-subprim .SPffcall)
  (movq (:rcontext x8664::tcr.foreign-sp) (% mm5))
  (movq (% mm5) (@ (% frame)))
  (movq (% frame) (:rcontext x8664::tcr.foreign-sp))
  (movq (% rax) (@ 8 (% frame)))
  (movq (% fp0) (@ 16 (% frame)))
  (movl ($ nil) (%l arg_z))
  (restore-simple-frame)
  (single-value-return))

(defx86lapfunction %do-ff-call-return-registers ((fp-regs 8)(nfp 0) (frame arg_x) (regbuf arg_y) (entry arg_z))
  (popq (% ra0))
  (popq (% rax))
  (popq (% temp0))
  (movq (% rbp) (@  (% rsp)))
  (movq (% rsp) (% rbp))
  (movq (% ra0) (@ 8 (% rbp)))
  (macptr-ptr temp0 temp0)
  (sarq ($ x8664::fixnumshift) (% rax))
  (movq (@ (% temp0)) (% fp0))
  (movq (@ 8 (% temp0)) (% fp1))
  (movq (@ 16 (% temp0)) (% fp2))
  (movq (@ 24 (% temp0)) (% fp3))
  (movq (@ 32 (% temp0)) (% fp4))
  (movq (@ 40 (% temp0)) (% fp5))
  (movq (@ 48 (% temp0)) (% fp6))
  (movq (@ 56 (% temp0)) (% fp7))
  (call-subprim .SPffcall-return-registers)
  (movq (:rcontext x8664::tcr.foreign-sp) (% mm5))
  (movq (% mm5) (@ (% frame)))
  (movq (% frame) (:rcontext x8664::tcr.foreign-sp))
  (movl ($ nil) (%l arg_z))
  (restore-simple-frame)
  (single-value-return))
  
#-win64-target
(defun %ff-call (entry &rest specs-and-vals)
  (declare (dynamic-extent specs-and-vals))
  (let* ((len (length specs-and-vals))
         (total-words 0)
         (regbuf nil))
    (declare (fixnum len total-words))
    (let* ((result-spec (or (car (last specs-and-vals)) :void))
           (nargs (ash (the fixnum (1- len)) -1))
           (n-fp-args 0))
      (declare (fixnum nargs n-fp-args))
      (ecase result-spec
        ((:address :unsigned-doubleword :signed-doubleword
                   :single-float :double-float
                   :signed-fullword :unsigned-fullword
                   :signed-halfword :unsigned-halfword
                   :signed-byte :unsigned-byte
                   :void)
         (do* ((i 0 (1+ i))
               (specs specs-and-vals (cddr specs))
               (spec (car specs) (car specs)))
              ((= i nargs))
           (declare (fixnum i))
           (case spec
             ((:address :unsigned-doubleword :signed-doubleword
                        :single-float :double-float
                        :signed-fullword :unsigned-fullword
                        :signed-halfword :unsigned-halfword
                        :signed-byte :unsigned-byte)
              (incf total-words))
             (:registers )
             (t (if (typep spec 'unsigned-byte)
                  (incf total-words spec)
                  (error "unknown arg spec ~s" spec)))))
         ;; It's necessary to ensure that the C frame is the youngest thing on
         ;; the foreign stack here.
         (%stack-block ((fp-args (* 8 8)))
           (with-macptrs ((argptr))
             (with-variable-c-frame
                 total-words frame
                 (%setf-macptr-to-object argptr frame)
                 (let* ((gpr-offset 16)
                        (other-offset (+ gpr-offset (* 6 8))))
                   (declare (fixnum gpr-offset other-offset))
                   (do* ((i 0 (1+ i))
                         (ngpr-args 0)
                         (specs specs-and-vals (cddr specs))
                         (spec (car specs) (car specs))
                         (val (cadr specs) (cadr specs)))
                        ((= i nargs))
                     (declare (fixnum i))
                     (case spec
                       (:address
                        (incf ngpr-args)
                        (cond ((<= ngpr-args 6)
                               (setf (%get-ptr argptr gpr-offset) val)
                               (incf gpr-offset 8))
                              (t
                               (setf (%get-ptr argptr other-offset) val)
                               (incf other-offset 8))))
                       ((:signed-doubleword :signed-fullword :signed-halfword
                                            :signed-byte)
                        (incf ngpr-args)
                        (cond ((<= ngpr-args 6)
                               (setf (%%get-signed-longlong argptr gpr-offset) val)
                               (incf gpr-offset 8))
                              (t
                               (setf (%%get-signed-longlong argptr other-offset) val)
                               (incf other-offset 8))))
                       ((:unsigned-doubleword :unsigned-fullword :unsigned-halfword
                                              :unsigned-byte)
                        (incf ngpr-args)
                        (cond ((<= ngpr-args 6)
                               (setf (%%get-unsigned-longlong argptr gpr-offset) val)
                               (incf gpr-offset 8))
                              (t
                               (setf (%%get-unsigned-longlong argptr other-offset) val)
                               (incf other-offset 8))))
                       (:double-float
                        (cond ((< n-fp-args 8)
                               (setf (%get-double-float fp-args (* n-fp-args 8)) val)
                               (incf n-fp-args))
                              (t
                               (setf (%get-double-float argptr other-offset) val)
                               (incf other-offset 8))))
                       (:single-float
                        (cond ((< n-fp-args 8)
                               (setf (%get-single-float fp-args (* n-fp-args 8))
                                     val)
                               (incf n-fp-args))
                              (t 
                               (setf (%get-single-float argptr other-offset) val)
                               (incf other-offset 8))))
                       (:registers (setq regbuf val))
                       (t
                        (let* ((p 0))
                          (declare (fixnum p))
                          (dotimes (i (the fixnum spec))
                            (setf (%get-ptr argptr other-offset) (%get-ptr val p))
                            (incf p 8)
                            (incf other-offset 8)))))))
                 (if regbuf
                   (%do-ff-call-return-registers fp-args (min n-fp-args 8) frame regbuf entry)
                   (%do-ff-call (min n-fp-args 8) frame fp-args entry))
                 (ecase result-spec
                   (:void nil)
                   (:address (%get-ptr argptr 8))
                   (:unsigned-byte (%get-unsigned-byte argptr 8))
                   (:signed-byte (%get-signed-byte argptr 8))
                   (:unsigned-halfword (%get-unsigned-word argptr 8))
                   (:signed-halfword (%get-signed-word argptr 8))
                   (:unsigned-fullword (%get-unsigned-long argptr 8))
                   (:signed-fullword (%get-signed-long argptr 8))
                   (:unsigned-doubleword (%get-natural argptr 8))
                   (:signed-doubleword (%get-signed-natural argptr 8))
                   (:single-float (%get-single-float argptr 16))
                   (:double-float (%get-double-float argptr 16)))))))))))

#+win64-target
(defun %ff-call (entry &rest specs-and-vals)
  (declare (dynamic-extent specs-and-vals))
  (let* ((len (length specs-and-vals))
         (total-words 0))
    (declare (fixnum len total-words))
    (let* ((result-spec (or (car (last specs-and-vals)) :void))
           (nargs (ash (the fixnum (1- len)) -1)))
      (declare (fixnum nargs))
      (ecase result-spec
        ((:address :unsigned-doubleword :signed-doubleword
                   :single-float :double-float
                   :signed-fullword :unsigned-fullword
                   :signed-halfword :unsigned-halfword
                   :signed-byte :unsigned-byte
                   :void)
         (do* ((i 0 (1+ i))
               (specs specs-and-vals (cddr specs))
               (spec (car specs) (car specs)))
              ((= i nargs))
           (declare (fixnum i))
           (case spec
             ((:address :unsigned-doubleword :signed-doubleword
                        :single-float :double-float
                        :signed-fullword :unsigned-fullword
                        :signed-halfword :unsigned-halfword
                        :signed-byte :unsigned-byte)
              (incf total-words))
             (t (if (typep spec 'unsigned-byte)
                  (incf total-words spec)
                  (error "unknown arg spec ~s" spec)))))
         ;; It's necessary to ensure that the C frame is the youngest thing on
         ;; the foreign stack here.
         (with-macptrs ((argptr))
             (with-variable-c-frame
                 total-words frame
                 (%setf-macptr-to-object argptr frame)
                 (let* ((arg-offset 16))
                   (declare (fixnum arg-offset))
                   (do* ((i 0 (1+ i))
                         (specs specs-and-vals (cddr specs))
                         (spec (car specs) (car specs))
                         (val (cadr specs) (cadr specs)))
                        ((= i nargs))
                     (declare (fixnum i))
                     (case spec
                       (:address
                        (setf (%get-ptr argptr arg-offset) val)
                        (incf arg-offset 8))
                       ((:signed-doubleword :signed-fullword :signed-halfword
                                            :signed-byte)
                        (setf (%%get-signed-longlong argptr arg-offset) val)
                        (incf arg-offset 8))
                       ((:unsigned-doubleword :unsigned-fullword :unsigned-halfword
                                              :unsigned-byte)
                        (setf (%%get-unsigned-longlong argptr arg-offset) val)
                        (incf arg-offset 8))
                       (:double-float
                        (setf (%get-double-float argptr arg-offset) val)
                        (incf arg-offset 8))
                       (:single-float
                        (setf (%get-single-float argptr arg-offset) val)
                        (incf arg-offset 8))
                       (t
                        (let* ((p 0))
                          (declare (fixnum p))
                          (dotimes (i (the fixnum spec))
                            (setf (%get-ptr argptr arg-offset) (%get-ptr val p))
                            (incf p 8)
                            (incf arg-offset 8)))))))
                 (%do-ff-call frame entry)
                 (ecase result-spec
                   (:void nil)
                   (:address (%get-ptr argptr 8))
                   (:unsigned-byte (%get-unsigned-byte argptr 8))
                   (:signed-byte (%get-signed-byte argptr 8))
                   (:unsigned-halfword (%get-unsigned-word argptr 8))
                   (:signed-halfword (%get-signed-word argptr 8))
                   (:unsigned-fullword (%get-unsigned-long argptr 8))
                   (:signed-fullword (%get-signed-long argptr 8))
                   (:unsigned-doubleword (%get-natural argptr 8))
                   (:signed-doubleword (%get-signed-natural argptr 8))
                   (:single-float (%get-single-float argptr 16))
                   (:double-float (%get-double-float argptr 16))))))))))

(defx86lapfunction %throw ()
  (:arglist (&rest args))
  (push-argregs)
  (subl ($ x8664::fixnumone) (% nargs))
  (lea (:@ (:^ @back) (% fn)) (% ra0))
  (:talign 4)
  (jmp-subprim .SPthrow)
  @back
  (recover-fn-from-rip)
  (uuo-error-reg-not-tag (% temp0) ($ x8664::subtag-catch-frame)))


                                 

;;; end of x86-def.lisp
) ; #+x8664-target
