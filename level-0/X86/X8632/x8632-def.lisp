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

;;; Use the offsets in a function's self-reference table to replace
;;; the :self in (movl ($ :self) (% fn)) wih the function's actual
;;; address.
(defx8632lapfunction %update-self-references ((fun arg_z))
  (movzwl (@ x8632::misc-data-offset (% fun)) (% imm0)) ;imm word count
  (btr ($ 15) (% imm0))
  (jnc @proceed)
  (imm-word-count fun imm0 temp0)
  (subl ($ '2) (% temp0))
  (jmp @load-offset)
  @proceed
  (subl ($ 2) (% imm0))
  (box-fixnum imm0 temp0)		;byte offset of first self-ref offset
  (jmp @load-offset)
  @loop
  (movl (% fun) (@ x8632::misc-header-offset (% fun) (% imm0)))
  (subl ($ '1) (% temp0))
  @load-offset
  (movl (@ x8632::misc-data-offset (% fun) (% temp0)) (% imm0))
  (test (% imm0) (% imm0))
  (jne @loop)
  (single-value-return))

(defx8632lapfunction %function-code-words ((fun arg_z))
  (trap-unless-typecode= fun x8632::subtag-function)
  (movzwl (@ x8632::misc-data-offset (% fun)) (% imm0))
  (btr ($ 15) (% imm0))
  (jnc @proceed)
  (imm-word-count fun imm0 temp0)
  (movl (% temp0) (% arg_z))
  (single-value-return)
  @proceed
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx8632lapfunction %nth-immediate ((fun arg_y) (n arg_z))
  (trap-unless-typecode= fun x8632::subtag-function)
  (movzwl (@ x8632::misc-data-offset (% fun)) (% imm0))
  (btr ($ 15) (% imm0))
  (jnc @proceed)
  (imm-word-count fun imm0 temp0)
  (unbox-fixnum temp0 imm0)
  @proceed
  (lea (@ (% n) (% imm0) 4) (% imm0))
  (movl (@ x8632::misc-data-offset (% fun) (% imm0)) (% arg_z))
  (single-value-return))

(defx8632lapfunction %set-nth-immediate ((fun 4) #|(ra 0)|# (n arg_y) (new arg_z))
  (popl (@ 8 (% esp)))
  (popl (% temp0))
  (addl ($ 4) (% esp))
  (trap-unless-typecode= temp0 x8632::subtag-function)
  (movzwl (@ x8632::misc-data-offset (% temp0)) (% imm0))
  (lea (@ (% n) (% imm0) 4) (% arg_y))
  ;; expects gvector in temp0
  (jmp-subprim .SPgvset))

(defx8632lapfunction %function-code-byte ((fun arg_y) (pc arg_z))
  (unbox-fixnum pc imm0)
  (movzbl (@ (% fun) (% imm0)) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defx8632lapfunction %function-register-usage ((f arg_z))
  (check-nargs 1)
  (trap-unless-typecode= f x8632::subtag-function)
  (movl (% esp) (% temp0))
  (pushl ($ nil))
  (pushl ($ nil))
  (jmp-subprim .SPvalues))

;;; XXX probably should unify these next two with the x8664 versions.

;;; Make a new function, with PROTO's code and the specified immediates.
;;; IMMEDIATES should contain lfun-bits as the last element.
(defun %clone-x86-function (proto &rest immediates)
  (declare (dynamic-extent immediates))
  (let* ((protov (function-to-function-vector proto))
         (code-words (%function-code-words proto))
         (numimms (length immediates))
         (newv (allocate-typed-vector :function (the fixnum (+ code-words numimms)))))
    (declare (fixnum code-words numimms))
    (%copy-ivector-to-ivector protov 0 newv 0 (the fixnum (ash code-words target::word-shift)))
    #||
    ;; XXX bootstrapping
    (setf (ldb (byte 16 0) (uvref newv 0)) (logior #x8000 numimms))
    ||#
    (%update-self-references newv)
    (do* ((k code-words (1+ k))
          (imms immediates (cdr imms)))
         ((null imms) (function-vector-to-function newv))
      (declare (fixnum k) (list imms))
      (setf (%svref newv k) (car imms)))))

(defun %copy-function (proto &optional target)
  (let* ((protov (function-to-function-vector proto))
         (code-words (%function-code-words proto))
         (total-words (uvsize protov))
         (newv (if target
                 (function-to-function-vector target)
                 (allocate-typed-vector :function total-words))))
    (declare (fixnum code-words total-words))
    (when target
      (unless (and (eql code-words (%function-code-words target))
                   (eql total-words (uvsize newv)))
        (error "Wrong size target ~s" target)))
    (%copy-ivector-to-ivector protov 0 newv 0 (the fixnum (ash code-words target::word-shift)))
    (loop for k fixnum from code-words below total-words
      do (setf (%svref newv k) (%svref protov k)))
    #||
    (setf (ldb (byte 16 0) (uvref newv 0))
	  (logior #x8000 (- total-words code-words)))
    ||#
    (%update-self-references (function-vector-to-function newv))))

(defun replace-function-code (target proto)
  (let* ((target-words (%function-code-words target))
         (proto-words (%function-code-words proto)))
    (declare (fixnum target-words proto-words))
    (if (= target-words proto-words)
      (progn
        (%copy-ivector-to-ivector (function-to-function-vector proto)
                                  0
                                  (function-to-function-vector target)
                                  0
                                  (the fixnum (ash target-words
                                                   target::word-shift)))
	(%update-self-references target)
        target)
      (error "Code size mismatch: target = ~s, proto = ~s"
             target-words proto-words))))

(defx8632lapfunction %get-kernel-global-from-offset ((offset arg_z))
  (check-nargs 1)
  (unbox-fixnum offset imm0)
  (movl (@ (target-nil-value) (% imm0)) (% arg_z))
  (single-value-return))

(defx8632lapfunction %set-kernel-global-from-offset ((offset arg_y)
						     (new-value arg_z))
  (check-nargs 2)
  (unbox-fixnum offset imm0)
  (movl (% arg_z) (@ (target-nil-value) (% imm0)))
  (single-value-return))

(defx8632lapfunction %get-kernel-global-ptr-from-offset ((offset arg_y)
							 (ptr arg_z))
  (check-nargs 2)
  (unbox-fixnum offset imm0)
  (movl (@ (target-nil-value) (% imm0)) (% imm0))
  (movl (% imm0) (@ x8632::macptr.address (% ptr)))
  (single-value-return))

(defx8632lapfunction %fixnum-ref ((fixnum arg_y) #| &optional |# (offset arg_z))
  (:arglist (fixnum &optional offset))
  (check-nargs 1 2)
  (cmpl ($ x8632::fixnumone) (% nargs))
  (jne @2-args)
  (movl (% offset) (% fixnum))
  (xorl (%l offset) (%l offset))
  @2-args
  (unbox-fixnum offset imm0)
  (movl (@ (% fixnum) (% imm0)) (% arg_z))
  (single-value-return))

(defx8632lapfunction %fixnum-ref-natural ((fixnum arg_y) #| &optional |# (offset arg_z))
  (:arglist (fixnum &optional offset))
  (check-nargs 1 2)
  (cmpl ($ x8632::fixnumone) (% nargs))
  (jne @2-args)
  (movl (% offset) (% fixnum))
  (xorl (%l offset) (%l offset))
  @2-args
  (unbox-fixnum offset imm0)
  (movl (@ (% fixnum) (% imm0)) (% imm0))
  (jmp-subprim .SPmakeu32))

(defx8632lapfunction %fixnum-set ((fixnum 4) #|(ra 0)|# (offset arg_y) #| &optional |# (new-value arg_z))
  (:arglist (fixnum offset &optional newval))
  (check-nargs 2 3)
  (cmpl ($ '2) (% nargs))
  (jne @3-args)
  (movl (% new-value) (% offset))
  (single-value-return)
  @3-args
  (movl (@ fixnum (% esp)) (% temp0))
  (unbox-fixnum offset imm0)
  (movl (% new-value) (@ (% temp0) (% imm0)))
  (single-value-return 3))


(defx8632lapfunction %fixnum-set-natural ((fixnum 4) #|(ra 0)|# (offset arg_y) #| &optional |# (new-value arg_z))
  (:arglist (fixnum offsnet &optional newval))
  (check-nargs 2 3)
  (cmpl ($ '2) (% nargs))
  (jne @3-args)
  (save-simple-frame)
  (movl (% offset) (% temp0))
  (xorl (% offset) (% offset))
  (jmp @common)
  @3-args
  (movl (% ebp) (@ 8 (% esp)))
  (lea (@ 8 (% esp)) (% ebp))
  (popl (@ 4 (% ebp)))
  (popl (% temp0))
  @common
  (call-subprim .SPgetu32)		;puts u32 in imm0
  (mark-as-imm temp1)
  (unbox-fixnum offset temp1)
  (movl (% imm0) (@ (% temp0) (% temp1)))
  (mark-as-node temp1)
  (restore-simple-frame)
  (single-value-return))


(defx8632lapfunction %current-frame-ptr ()
  (check-nargs 0)
  (movl (% ebp) (% arg_z))
  (single-value-return))


(defx8632lapfunction %current-tsp ()
  (check-nargs 0)
  (movl (:rcontext x8632::tcr.save-tsp) (% arg_z))
  (single-value-return))


(defx8632lapfunction %%frame-backlink ((p arg_z))
  (check-nargs 1)
  (movl (@ (% arg_z)) (% arg_z))
  (single-value-return))

;;; Look for "movl $imm32,%fn at the tra;  if present, then $imm32 is
;;; the address of the function.
;;;
;;; That is: #b10111111 <imm32>
;;;                ^^^^
;;;   operand size || register number (%fn/%edi)

(defx8632lapfunction %return-address-function ((r arg_z))
  (extract-fulltag r imm0)
  (cmpb ($ x8632::fulltag-tra) (% imm0.b))
  (jne @fail)
  (cmpb ($ x8632::recover-fn-opcode-byte) (@ (% r)))
  (jne @fail)
  (movl (@ x8632::recover-fn-address-offset (% r)) (% arg_z))
  (single-value-return)
  @fail
  (movl ($ (target-nil-value)) (% arg_z))
  (single-value-return))

(defx8632lapfunction %return-address-offset ((r arg_z))
  (extract-fulltag r imm0)
  (cmpb ($ x8632::fulltag-tra) (% imm0.b))
  (jne @fail)
  (cmpb ($ x8632::recover-fn-opcode-byte) (@ (% r)))
  (jne @fail)
  (movl (@ x8632::recover-fn-address-offset (% r)) (% imm0))
  (subl (% arg_z) (% imm0))
  (negl (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return)
  @fail
  (movl ($ (target-nil-value)) (% arg_z))
  (single-value-return))

;;; It's always been the case that the function associated with a
;;; frame pointer is the caller of the function that "uses" that frame.
(defun %cfp-lfun (p)
  (let* ((ra (%fixnum-ref p x8632::lisp-frame.return-address)))
    (if (eq ra (%get-kernel-global ret1valaddr))
      (setq ra (%fixnum-ref p x8632::lisp-frame.xtra)))
    (values (%return-address-function ra) (%return-address-offset ra))))

(defx8632lapfunction %uvector-data-fixnum ((uv arg_z))
  (check-nargs 1)
  (trap-unless-fulltag= arg_z x8632::fulltag-misc)
  (addl ($ x8632::misc-data-offset) (% arg_z))
  (single-value-return))

(defx8632lapfunction %catch-top ((tcr arg_z))
  (check-nargs 1)
  (movl ($ (target-nil-value)) (% arg_y))
  (movl (:rcontext x8632::tcr.catch-top) (% arg_z))
  (testb (%b arg_z) (%b arg_z))
  (cmovel (% arg_y) (% arg_z))
  (single-value-return))

(defx8632lapfunction %catch-tsp ((catch arg_z))
  (check-nargs 1)
  (lea (@  (- (+ x8632::fulltag-misc
		 (ash 1 (1+ x8632::word-shift)))) (% arg_z))
       (% arg_z))
  (single-value-return))

;;; Same as %address-of, but doesn't cons any bignums
;;; It also left shift fixnums just like everything else.
(defx8632lapfunction %fixnum-address-of ((x arg_z))
  (check-nargs 1)
  (box-fixnum x arg_z)
  (single-value-return))

(defx8632lapfunction %save-standard-binding-list ((bindings arg_z))
  (mark-as-imm temp0)
  (movl (:rcontext x8632::tcr.vs-area) (% imm0))
  (movl (@ x8632::area.high (% imm0)) (% temp0))
  (subl ($ x8632::node-size) (% temp0))
  (movl (% bindings) (@ (% temp0)))
  (mark-as-node temp0)
  (single-value-return))

(defx8632lapfunction %saved-bindings-address ()
  (mark-as-imm temp0)
  (movl (:rcontext x8632::tcr.vs-area) (% imm0))
  (movl (@ x8632::area.high (% imm0)) (% temp0))
  (leal (@ (- x8632::node-size) (% temp0)) (% arg_z))
  (mark-as-node temp0)
  (single-value-return))

(defx8632lapfunction %get-object ((macptr arg_y) (offset arg_z))
  (check-nargs 2)
  (trap-unless-typecode= macptr x8632::subtag-macptr)
  (trap-unless-lisptag= offset x8632::tag-fixnum)
  (macptr-ptr macptr imm0)
  (mark-as-imm temp0)
  (unbox-fixnum offset temp0)
  (movl (@ (% imm0) (% temp0)) (% arg_z))
  (mark-as-node temp0)
  (single-value-return))

(defx8632lapfunction %set-object ((macptr 4) #|(ra 0)|# (offset arg_y) (value arg_z))
  (check-nargs 3)
  (movl (@ macptr (% esp)) (% temp1))
  (trap-unless-typecode= temp1 x8632::subtag-macptr)
  (trap-unless-lisptag= offset x8632::tag-fixnum)
  (macptr-ptr temp1 imm0)
  (mark-as-imm temp0)
  (unbox-fixnum offset temp0)
  (movl (% arg_z) (@ (% imm0) (% temp0)))
  (mark-as-node temp0)
  (single-value-return 3))

(defx8632lapfunction %apply-lexpr-with-method-context ((magic 4)
						       #|(ra 0)|#
						       (function arg_y)
						       (args arg_z))
  ;; Somebody's called (or tail-called) us.
  ;; * Put magic arg in %rcontext:tcr.next-method-context
  ;; * Put function somewhere safe until we're ready to jump to it
  ;; * Set nargs to 0, then spread "args" on stack (clobbers regs)
  ;; * Jump to function (saved previously)
  (popl (:rcontext x8632::tcr.save0))	;return address
  (popl (:rcontext x8632::tcr.next-method-context)) ;magic arg
  (discard-reserved-frame)
  (movl (% function) (:rcontext x8632::tcr.save1))
  (set-nargs 0)
  (movl (@ (% args)) (% temp0))		;lexpr-count
  (movl (% temp0) (% nargs))
  (leal (@ x8632::node-size (% arg_z) (% temp0)) (% imm0))
  (subl ($ '2) (% temp0))
  (jbe @reg-only)
  ;; Some args will be pushed; reserve a frame.
  (pushl ($ x8632::reserved-frame-marker))
  (pushl ($ x8632::reserved-frame-marker))
  @pushloop
  (pushl (@ (- x8632::node-size) (% imm0)))
  (subl ($ x8632::node-size) (% imm0))
  (subl ($ x8632::node-size) (% temp0))
  (jne @pushloop)
  @two
  (movl (@ (* x8632::node-size 2) (% arg_z)) (% arg_y))
  @one
  (movl (@ (* x8632::node-size 1) (% arg_z)) (% arg_z))
  (jmp @go)
  @reg-only
  (rcmp (% nargs) ($ '1))
  (je @one)
  (jb @go)
  (jmp @two)
  @go
  (pushl (:rcontext x8632::tcr.save0))	 ;return address
  (movl (:rcontext x8632::tcr.save1) (% temp0)) ;function
  (movapd (% fpzero) (:rcontext x8632::tcr.save0)) ;clear spill area
  (jmp (% temp0)))

(defx8632lapfunction %apply-with-method-context ((magic 4)
						 #|(ra 0)|#
						 (function arg_y)
						 (args arg_z))
  ;; Similar to above.
  (popl (:rcontext x8632::tcr.save0))	;save return address
  (popl (:rcontext x8632::tcr.next-method-context))	;
  (discard-reserved-frame)
  (movl (% args) (:rcontext x8632::tcr.save2))	;in case of error
  (set-nargs 0)
  (pushl ($ target::reserved-frame-marker))		;reserve frame (might discard it
  (pushl ($ target::reserved-frame-marker))		;if nothing is passed on stack)
  (cmp-reg-to-nil arg_z)
  (je @done)
  @loop
  (extract-fulltag arg_z imm0)
  (cmpb ($ x8632::fulltag-cons) (% imm0.b)) ;nil is a cons on x8632, but we
  (jne @bad)				     ; checked for it already.
  (add ($ '1) (% nargs))			;shorter than lea (imm0 is eax)
  (pushl (@ target::cons.car (% arg_z)))
  (%cdr arg_z arg_z)
  (cmp-reg-to-nil arg_z)
  (jne @loop)
  @done
  ;; arg_y about to get clobbered; put function into temp0
  (movl (% function) (% temp0))
  ;; temp1 (aka nargs) contains number of args just pushed
  (test (% nargs) (% nargs))
  (jne @pop)
  @discard-and-go
  (discard-reserved-frame)
  (jmp @go)
  @pop
  (cmpl ($ '1) (% nargs))
  (pop (% arg_z))
  (je @discard-and-go)
  (cmpl ($ '2) (% nargs))
  (pop (% arg_y))
  (je @discard-and-go)
  @go
  (pushl (:rcontext x8632::tcr.save0))	 ;return address
  (movapd (% fpzero) (:rcontext x8632::tcr.save0)) ;clear out spill area
  (jmp (% temp0))
  @bad
  (addl (% nargs) (% esp))
  (movl (:rcontext x8632::tcr.save1) (% arg_z)) ;saved args
  (movapd (% fpzero) (:rcontext x8632::tcr.save0)) ;clear out spill area
  (movl ($ '#.$XNOSPREAD) (% arg_y))
  (set-nargs 2)
  (jmp-subprim .SPksignalerr))

;;; The idea here is to call METHOD in the same stack frame in
;;; which the lexpr was originally called.  The lexpr can't
;;; have had any required arguments, %APPLY-LEXPR-TAIL-WISE
;;; must have been tail-called, and the frame built on lexpr
;;; entry must be in %rbp.
(defx8632lapfunction %apply-lexpr-tail-wise ((method arg_y) (args arg_z))
  (addl ($ x8632::node-size) (% esp))   ; discard extra return address
  (movl (% method) (% xfn))		;temp1
  (movl (% args) (% esp))
  (popl (% imm0))			;nargs
  (movl (@ x8632::lisp-frame.return-address (% ebp)) (% temp0))
  (movl (@ 0 (% ebp)) (% ebp))
  (rcmpl (% imm0) ($ '2))
  (jbe @pop-regs)
  ;; More than 2 args; some must have been pushed by caller,
  ;; so retain the reserved frame.
  (pop (% arg_z))
  (pop (% arg_y))
  (jmp @popped)
  @pop-regs
  (rcmpl (% imm0) ($ '1))
  (jb @discard)
  (ja @pop2)
  (pop (% arg_z))
  (jmp @discard)
  @pop2
  (pop (% arg_z))
  (pop (% arg_y))
  @discard
  (discard-reserved-frame)
  @popped
  (push (% temp0))			;return address
  (movl (% xfn) (% temp0))		;temp1 is also nargs
  (movl (% imm0) (% nargs))
  (jmp (% temp0)))

(defun closure-function (fun)
  (while (and (functionp fun) (not (compiled-function-p fun)))
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
   (popl (:rcontext x8632::tcr.save0))	;save return address
   ;; only two arg regs on x8632, so the caller will always push a frame
   (movl (% arg_z) (% temp0))           ; last
   (movl (% arg_y) (% arg_z))           ; butlast
   (subl ($ '2) (% nargs))              ; remove count for butlast & last
   (movd (% temp1) (% mm0))		;save nargs (aka temp1) for later
   ;; Do .SPspreadargz inline here
   (xorl (%l temp1) (%l temp1))
   (movl (% arg_z) (:rcontext x8632::tcr.save1)) ; save in case of error
   (cmp-reg-to-nil arg_z)
   (je @done)
   ;;(mark-as-imm temp1)
   @loop
   (extract-fulltag arg_z imm0)
   (cmpb ($ x8632::fulltag-cons) (%b imm0))
   (jne @bad)
   (%car arg_z arg_y)
   (%cdr arg_z arg_z)
   (addl ($ '1) (%l temp1))
   (cmp-reg-to-nil arg_z)   
   (push (% arg_y))
   (jne @loop)
   @done
   ;; nargs was at least 1 when we started spreading, and can't have gotten
   ;; any smaller. 
   (movd (% mm0) (% arg_y))		;nargs from before loop
   (addl (% arg_y) (% temp1))		;did I mention nargs is temp1?
   (movl (% temp0) (% arg_z))
   (pop (% arg_y))
   (addl ($ '1) (% nargs))
   (load-constant funcall temp0)
   (pushl (:rcontext x8632::tcr.save0))	;return address
   (movapd (% fpzero) (:rcontext x8632::tcr.save0)) ;clear out spill area
   (jmp-subprim .SPfuncall)
   @bad				      ;error spreading list.
   (add (% temp1) (% esp))	      ;discard whatever's been pushed
   (movl (:rcontext x8632::tcr.save1) (% arg_z))
   (movapd (% fpzero) (:rcontext x8632::tcr.save0)) ;clear out spill area
   (movl ($ '#.$XNOSPREAD) (% arg_y))
   (set-nargs 2)
   (jmp-subprim .SPksignalerr) ))



;;; This needs to:
;;; (a) call the .SPffcall subprimitive, which will discard the foreign
;;;     stack frame allocated by WITH-VARIABLE-C-FRAME in %FF-CALL
;;; (b) re-establish the same foreign stack frame and store the results
;;;     there.
;;;
;;; The flags argument tells us what/where the result is:
;;;
;;;; flags   meaning
;;;    0     32-bit value in EAX
;;;    1     single-float value on x87 stack
;;;    2     double-float value on x87 stack
;;;    3     64-bit value with low half in EAX, high half in tcr.unboxed1

(defx8632lapfunction %do-ff-call ((flags 4) #|(ra 0)|# (frame arg_y) (entry arg_z))
  (save-stackargs-frame 1)
  (push (% arg_y))
  (push (% arg_z))
  (call-subprim .SPffcall)
  ;; there might be an fp result on x87 stack, so don't use
  ;; any mmx instructions until the result has been read.
  (movd (:rcontext x8632::tcr.foreign-sp) (% xmm0))
  (movd (% xmm0) (@ (% frame)))
  (movl (% frame) (:rcontext x8632::tcr.foreign-sp))
  (cmpl ($ 0) (@ -4 (% ebp)))
  (jne @fp-or-doubleword)
  (movl (% eax) (@ 4 (% frame)))
  @done
  (movl ($ nil) (% arg_z))
  (restore-simple-frame)
  (single-value-return)
  @fp-or-doubleword
  (cmpl ($ '2) (@ -4 (% ebp)))
  (jl @single)
  (je @double)
  ;; high 32 bits in tcr.unboxed1 (see .SPffcall)
  (movl (% eax) (@ 4 (% frame)))
  (movl (:rcontext x8632::tcr.unboxed1) (% eax))
  (movl (% eax) (@ 8 (% frame)))
  (jmp @done)
  @single
  (fstps (@ 4 (% frame)))
  (jmp @done)
  @double
  (fstpl (@ 4 (% frame)))
  (jmp @done))

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
	     (:registers
	      (error "don't know what to do with argspec ~s" spec))
	     ((:double-float :unsigned-doubleword :signed-doubleword)
	      (incf total-words 2))
	     ((:address :single-float
			:signed-fullword :unsigned-fullword
			:signed-halfword :unsigned-halfword
			:signed-byte :unsigned-byte)
              (incf total-words))
	     (t (if (typep spec 'unsigned-byte)
		  (incf total-words spec)
		  (error "Invalid argument spec ~s" spec)))))
	 ;; It's necessary to ensure that the C frame is the youngest thing on
	 ;; the foreign stack here.
	 (with-macptrs ((argptr))
	   (with-variable-c-frame
	       total-words frame
	       (%setf-macptr-to-object argptr frame)
	       (let* ((offset 8))
		 (do* ((i 0 (1+ i))
		       (specs specs-and-vals (cddr specs))
		       (spec (car specs) (car specs))
		       (val (cadr specs) (cadr specs)))
		      ((= i nargs))
		   (declare (fixnum i))
		   (case spec
		     (:double-float
		      (setf (%get-double-float argptr offset) val)
		      (incf offset 8))
		     (:single-float
		      (setf (%get-single-float argptr offset) val)
		      (incf offset 4))
		     (:signed-doubleword
		      (setf (%%get-signed-longlong argptr offset) val)
		      (incf offset 8))
		     (:unsigned-doubleword
		      (setf (%%get-unsigned-longlong argptr offset) val)
		      (incf offset 8))
		     (:address
		      (setf (%get-ptr argptr offset) val)
		      (incf offset 4))
		     ((:signed-fullword :signed-halfword :signed-byte)
		      (setf (%get-signed-natural argptr offset) val)
		      (incf offset 4))
		     ((:unsigned-fullword :unsigned-halfword :unsigned-byte)
		      (setf (%get-natural argptr offset) val)
		      (incf offset 4))
		     (t
		      (let* ((p 0))
			(declare (fixnum p))
			(dotimes (i (the fixnum spec))
			  (setf (%get-ptr argptr offset) (%get-ptr val p))
			  (incf p 4)
			  (incf offset 4))))))
		 (let ((flags (case result-spec
				(:single-float 1)
				(:double-float 2)
				((:signed-doubleword :unsigned-doubleword) 3)
				(t 0))))
		   (%do-ff-call flags frame entry))
		 (ecase result-spec
		   (:void nil)
		   (:address (%get-ptr argptr 4))
		   (:unsigned-byte (%get-unsigned-byte argptr 4))
		   (:signed-byte (%get-signed-byte argptr 4))
		   (:unsigned-halfword (%get-unsigned-word argptr 4))
		   (:signed-halfword (%get-signed-word argptr 4))
		   (:unsigned-fullword (%get-natural argptr 4))
		   (:signed-fullword (%get-signed-natural argptr 4))
		   (:unsigned-doubleword (%%get-unsigned-longlong argptr 4))
		   (:signed-doubleword (%%get-signed-longlong argptr 4))
		   (:single-float (%get-single-float argptr 4))
		   (:double-float (%get-double-float argptr 4)))))))))))

;;; end of x86-def.lisp
