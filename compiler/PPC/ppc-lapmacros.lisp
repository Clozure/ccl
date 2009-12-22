;;;-*- Mode: Lisp; Package: CCL -*-
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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "PPC-LAP"))
  

(defppclapmacro clrrri (&rest args)
  (target-arch-case
   (:ppc32 `(clrrwi ,@args))
   (:ppc64 `(clrrdi ,@args))))

(defppclapmacro clrlri (&rest args)
  (target-arch-case
   (:ppc32 `(clrlwi ,@args))
   (:ppc64 `(clrldi ,@args))))

(defppclapmacro clrlri. (&rest args)
  (target-arch-case
   (:ppc32 `(clrlwi. ,@args))
   (:ppc64 `(clrldi. ,@args))))

(defppclapmacro ldr (&rest args)
  (target-arch-case
   (:ppc32 `(lwz ,@args))
   (:ppc64 `(ld ,@args))))

(defppclapmacro ldrx (&rest args)
  (target-arch-case
   (:ppc32 `(lwzx ,@args))
   (:ppc64 `(ldx ,@args))))

(defppclapmacro ldru (&rest args)
  (target-arch-case
   (:ppc32 `(lwzu ,@args))
   (:ppc64 `(ldu ,@args))))

(defppclapmacro str (&rest args)
  (target-arch-case
   (:ppc32 `(stw ,@args))
   (:ppc64 `(std ,@args))))

(defppclapmacro strx (&rest args)
  (target-arch-case
   (:ppc32 `(stwx ,@args))
   (:ppc64 `(stdx ,@args))))

(defppclapmacro stru (&rest args)
  (target-arch-case
   (:ppc32 `(stwu ,@args))
   (:ppc64 `(stdu ,@args))))

(defppclapmacro strux (&rest args)
  (target-arch-case
   (:ppc32 `(stwux ,@args))
   (:ppc64 `(stdux ,@args))))

(defppclapmacro lrarx (&rest args)
  (target-arch-case
   (:ppc32 `(lwarx ,@args))
   (:ppc64 `(ldarx ,@args))))

(defppclapmacro strcx. (&rest args)
  (target-arch-case
   (:ppc32 `(stwcx. ,@args))
   (:ppc64 `(stdcx. ,@args))))
  
(defppclapmacro cmpr (&rest args)
  (target-arch-case
   (:ppc32 `(cmpw ,@args))
   (:ppc64 `(cmpd ,@args))))

(defppclapmacro cmpri (&rest args)
  (target-arch-case
   (:ppc32 `(cmpwi ,@args))
   (:ppc64 `(cmpdi ,@args))))

(defppclapmacro cmplr (&rest args)
  (target-arch-case
   (:ppc32 `(cmplw ,@args))
   (:ppc64 `(cmpld ,@args))))

(defppclapmacro cmplri (&rest args)
  (target-arch-case
   (:ppc32 `(cmplwi ,@args))
   (:ppc64 `(cmpldi ,@args))))

(defppclapmacro trlge (&rest args)
  (target-arch-case
   (:ppc32 `(twlge ,@args))
   (:ppc64 `(tdlge ,@args))))

(defppclapmacro trlgei (&rest args)
  (target-arch-case
   (:ppc32 `(twlgei ,@args))
   (:ppc64 `(tdlgei ,@args))))

(defppclapmacro trllt (&rest args)
  (target-arch-case
   (:ppc32 `(twllt ,@args))
   (:ppc64 `(tdllt ,@args))))

(defppclapmacro trllti (&rest args)
  (target-arch-case
   (:ppc32 `(twllti ,@args))
   (:ppc64 `(tdllti ,@args))))

(defppclapmacro trlgti (&rest args)
  (target-arch-case
   (:ppc32 `(twlgti ,@args))
   (:ppc64 `(tdlgti ,@args))))

(defppclapmacro trlti (&rest args)
  (target-arch-case
   (:ppc32 `(twlti ,@args))
   (:ppc64 `(tdlti ,@args))))

(defppclapmacro trlle (&rest args)
  (target-arch-case
   (:ppc32 `(twlle ,@args))
   (:ppc64 `(tdlle ,@args))))

(defppclapmacro treqi (&rest args)
  (target-arch-case
   (:ppc32 `(tweqi ,@args))
   (:ppc64 `(tdeqi ,@args))))

(defppclapmacro trnei (&rest args)
  (target-arch-case
   (:ppc32 `(twnei ,@args))
   (:ppc64 `(tdnei ,@args))))

(defppclapmacro trgti (&rest args)
  (target-arch-case
   (:ppc32 `(twgti ,@args))
   (:ppc64 `(tdgti ,@args))))


(defppclapmacro srari (&rest args)
  (target-arch-case
   (:ppc32 `(srawi ,@args))
   (:ppc64 `(sradi ,@args))))

(defppclapmacro srar (&rest args)
  (target-arch-case
   (:ppc32 `(sraw ,@args))
   (:ppc64 `(srad ,@args))))

(defppclapmacro slr (&rest args)
  (target-arch-case
   (:ppc32 `(slw ,@args))
   (:ppc64 `(sld ,@args))))

(defppclapmacro srri (&rest args)
  (target-arch-case
   (:ppc32 `(srwi ,@args))
   (:ppc64 `(srdi ,@args))))

(defppclapmacro slri (&rest args)
  (target-arch-case
   (:ppc32 `(slwi ,@args))
   (:ppc64 `(sldi ,@args))))

(defppclapmacro slri. (&rest args)
  (target-arch-case
   (:ppc32 `(slwi. ,@args))
   (:ppc64 `(sldi. ,@args))))

(defppclapmacro srr (&rest args)
  (target-arch-case
   (:ppc32 `(srw ,@args))
   (:ppc64 `(srd ,@args))))

(defppclapmacro bkpt ()
  `(tweq rzero rzero))

(defppclapmacro dbg (&optional save-lr?)
  (if save-lr?
    `(progn
       (mflr loc-pc)
       (str imm0 -40 sp) ; better than clobbering imm0
       (bla .SPbreakpoint)
       (ldr imm0 -40 sp)
       (mtlr loc-pc))
    `(bla .SPbreakpoint)))

(defppclapmacro lwi (dest n)
  (setq n (logand n #xffffffff))
  (let* ((mask #xffff8000)
         (masked (logand n mask))
         (high (ash n -16))
         (low (logand #xffff n)))
    (if (or (= 0 masked) (= mask masked))
      `(li ,dest ,low)
      (if (= low 0)
        `(lis ,dest ,high)
        `(progn
           (lis ,dest ,high)
           (ori ,dest ,dest ,low))))))

(defppclapmacro set-nargs (n)
  (check-type n (unsigned-byte 13))
  `(li nargs ',n))

(defppclapmacro check-nargs (min &optional (max min))
  (if (eq max min)
    `(trnei nargs ',min)
    (if (null max)
      (unless (= min 0)
        `(trllti nargs ',min))
      (if (= min 0)
        `(trlgti nargs ',max)
        `(progn
           (trllti nargs ',min)
           (trlgti nargs ',max))))))

;; Event-polling involves checking to see if the value of the current
;; thread's interrupt-level is > 0.  For now, use nargs; this may
;; change to "any register BUT nargs".  (Note that most number-of-args
;; traps use unsigned comparisons.)
(defppclapmacro event-poll ()
  (target-arch-case
   (:ppc32
    '(progn
      (lwz nargs ppc32::tcr.tlb-pointer ppc32::rcontext)
      (lwz nargs ppc32::interrupt-level-binding-index nargs)
      (twgti nargs 0)))
   (:ppc64
    '(progn     
      (ld nargs ppc64::tcr.tlb-pointer ppc64::rcontext)
      (ld nargs ppc64::interrupt-level-binding-index nargs)
      (tdgti nargs 0)))))
    

;;; There's no "else"; learn to say "(progn ...)".
;;; Note also that the condition is a CR bit specification (or a "negated" one).
;;; Whatever affected that bit (hopefully) happened earlier in the pipeline.
(defppclapmacro if (test then &optional (else nil else-p))
  (multiple-value-bind (bitform negated) (ppc-lap-parse-test test)
    (let* ((false-label (gensym)))
      (if (not else-p)
      `(progn
         (,(if negated 'bt 'bf) ,bitform ,false-label)
         ,then
         ,false-label)
      (let* ((cont-label (gensym)))
        `(progn
          (,(if negated 'bt 'bf) ,bitform ,false-label)
          ,then
          (b ,cont-label)
          ,false-label
          ,else
          ,cont-label))))))

(defppclapmacro save-pc ()
  `(mflr loc-pc))

;;; This needs to be done if we aren't a leaf function (e.g., if we
;;; clobber our return address or need to reference any constants.  Note
;;; that it's not atomic wrt a preemptive scheduler, but we need to
;;; pretend that it will be.)  The VSP to be saved is the value of the
;;; VSP before any of this function's arguments were vpushed by its
;;; caller; that's not the same as the VSP register if any non-register
;;; arguments were received, but is usually easy to compute.

(defppclapmacro save-lisp-context (&optional (vsp 'vsp) (save-pc t))
  (target-arch-case
   (:ppc32
    `(progn
      ,@(if save-pc 
            '((save-pc)))
      (stwu sp (- ppc32::lisp-frame.size) sp)
      (stw fn ppc32::lisp-frame.savefn sp)
      (stw loc-pc ppc32::lisp-frame.savelr sp)
      (stw ,vsp ppc32::lisp-frame.savevsp sp)
      (mr fn nfn)))
   (:ppc64
    `(progn
      ,@(if save-pc 
            '((save-pc)))
      (stdu sp (- ppc64::lisp-frame.size) sp)
      (std fn ppc64::lisp-frame.savefn sp)
      (std loc-pc ppc64::lisp-frame.savelr sp)
      (std ,vsp ppc64::lisp-frame.savevsp sp)
      (mr fn nfn)))))

;;; There are a few cases to deal with when restoring: whether or not
;;; to restore the vsp, whether we need to saved LR back in the LR or
;;; whether it only needs to get as far as loc-pc, etc.  This fully
;;; restores everything (letting the caller specify some register
;;; other than the VSP, if that's useful.)  Note that, since FN gets
;;; restored, it's no longer possible to use it to address the current
;;; function's constants.
(defppclapmacro restore-full-lisp-context (&optional (vsp 'vsp))
  (target-arch-case
   (:ppc32
    `(progn
      (lwz loc-pc ppc32::lisp-frame.savelr sp)
      (lwz ,vsp ppc32::lisp-frame.savevsp sp)
      (mtlr loc-pc)
      (lwz fn ppc32::lisp-frame.savefn sp)
      (la sp ppc32::lisp-frame.size sp)))
   (:ppc64
    `(progn
      (ld loc-pc ppc64::lisp-frame.savelr sp)
      (ld ,vsp ppc64::lisp-frame.savevsp sp)
      (mtlr loc-pc)
      (ld fn ppc64::lisp-frame.savefn sp)
      (la sp ppc64::lisp-frame.size sp)))))

(defppclapmacro restore-pc ()
  `(mtlr loc-pc))

(defppclapmacro push (src stack)
  `(stru ,src ,(- (arch::target-lisp-node-size (backend-target-arch *target-backend*))) ,stack))

(defppclapmacro vpush (src)
  `(push ,src vsp))

;;; You typically don't want to do this to pop a single register (it's better to
;;; do a sequence of loads, and then adjust the stack pointer.)

(defppclapmacro pop (dest stack)
  `(progn
     (ldr ,dest 0 ,stack)
     (la ,stack ,(arch::target-lisp-node-size (backend-target-arch *target-backend*)) ,stack)))

(defppclapmacro vpop (dest)
  `(pop ,dest vsp))

(defppclapmacro %cdr (dest node)
  (target-arch-case
   (:ppc32 `(lwz ,dest ppc32::cons.cdr ,node))
   (:ppc64 `(ld ,dest ppc64::cons.cdr ,node))))

(defppclapmacro %car (dest node)
  (target-arch-case
   (:ppc32 `(lwz ,dest ppc32::cons.car ,node))
   (:ppc64 `(ld ,dest ppc64::cons.car ,node))))

(defppclapmacro extract-lisptag (dest node)
  (let* ((tb *target-backend*))
    `(clrlri ,dest ,node (- ,(arch::target-nbits-in-word (backend-target-arch tb))
                          ,(arch::target-nlisptagbits (backend-target-arch tb))))))

(defppclapmacro extract-fulltag (dest node)
  (let* ((tb *target-backend*))
  `(clrlri ,dest ,node (- ,(arch::target-nbits-in-word (backend-target-arch tb))
                        ,(arch::target-ntagbits (backend-target-arch tb))))))

(defppclapmacro extract-lowtag (dest node)
  (target-arch-case
   (:ppc32
    (error "EXTRACT-LOWTAG lapmacro makes no sense on PPC32."))
   (:ppc64
    `(clrldi ,dest ,node (- 64 ppc64::nlowtagbits)))))


(defppclapmacro extract-subtag (dest node)
  (target-arch-case
   (:ppc32
    `(lbz ,dest ppc32::misc-subtag-offset ,node))
   (:ppc64
    `(lbz ,dest ppc64::misc-subtag-offset ,node))))

(defppclapmacro extract-typecode (dest node &optional (crf :cr0))
  (target-arch-case
   (:ppc32
    `(progn
      (extract-lisptag ,dest ,node)
      (cmpwi ,crf ,dest ppc32::tag-misc)
      (if (,crf :eq)
        (extract-subtag ,dest ,node))))
   (:ppc64
    `(progn
      (extract-fulltag ,dest ,node)
      (cmpdi ,crf ,dest ppc64::fulltag-misc)
      (extract-lisptag ,dest ,dest)
      (if (,crf :eq)
        (extract-subtag ,dest ,node))))))

(defppclapmacro trap-unless-lisptag= (node tag &optional (immreg ppc::imm0))
  `(progn
     (extract-lisptag ,immreg ,node)
     (trnei ,immreg ,tag)))

(defppclapmacro trap-unless-fulltag= (node tag &optional (immreg ppc::imm0))
  `(progn
     (extract-fulltag ,immreg ,node)
     (trnei ,immreg ,tag)))


(defppclapmacro trap-unless-typecode= (node tag &optional (immreg ppc::imm0) (crf :cr0))
  `(progn
     (extract-typecode ,immreg ,node ,crf)
     (trnei ,immreg ,tag)))


(defppclapmacro load-constant (dest constant)
  `(ldr ,dest ',constant fn))

;;; This is about as hard on the pipeline as anything I can think of.
(defppclapmacro call-symbol (function-name)
  (target-arch-case
   (:ppc32
    `(progn
      (load-constant fname ,function-name)
      (lwz nfn ppc32::symbol.fcell fname)
      (lwz loc-pc ppc32::misc-data-offset nfn)
      (mtctr loc-pc)
      (bctrl)))
   (:ppc64
    `(progn
      (load-constant fname ,function-name)
      (ld nfn ppc64::symbol.fcell fname)
      (ld loc-pc ppc64::misc-data-offset nfn)
      (mtctr loc-pc)
      (bctrl)))))

(defppclapmacro sp-call-symbol (function-name)
  `(progn
     (load-constant fname ,function-name)
     (bla .SPjmpsym)))

(defppclapmacro getvheader (dest src)
  (target-arch-case
   (:ppc32
    `(lwz ,dest ppc32::misc-header-offset ,src))
   (:ppc64
    `(ld ,dest ppc64::misc-header-offset ,src))))

;;; "Size" is unboxed element-count.
(defppclapmacro header-size (dest vheader)
  (target-arch-case
   (:ppc32
    `(srwi ,dest ,vheader ppc32::num-subtag-bits))
   (:ppc64
    `(srdi ,dest ,vheader ppc64::num-subtag-bits))))


;;; "Length" is fixnum element-count.
(defppclapmacro header-length (dest vheader)
  (target-arch-case
   (:ppc32
    `(rlwinm ,dest 
      ,vheader 
      (- ppc32::nbits-in-word (- ppc32::num-subtag-bits ppc32::nfixnumtagbits))
      (- ppc32::num-subtag-bits ppc32::nfixnumtagbits)
      (- ppc32::least-significant-bit ppc32::nfixnumtagbits)))
   (:ppc64
    `(progn
      (rldicr ,dest
       ,vheader
       (- 64 (- ppc64::num-subtag-bits ppc64::fixnumshift))
       (- 63 ppc64::fixnumshift))
      (clrldi ,dest ,dest (- ppc64::num-subtag-bits ppc64::fixnumshift))))))
  

(defppclapmacro header-subtag[fixnum] (dest vheader)
  (target-arch-case
   (:ppc32
    `(rlwinm ,dest
           ,vheader
           ppc32::fixnumshift
           (- ppc32::nbits-in-word (+ ppc32::num-subtag-bits ppc32::nfixnumtagbits))
           (- ppc32::least-significant-bit ppc32::nfixnumtagbits)))
   (:ppc64
    `(clrlsldi ,dest
      ,vheader (- ppc64::nbits-in-word ppc64::num-subtag-bits)
      ppc64::fixnumshift))))


(defppclapmacro vector-size (dest v vheader)
  `(progn
     (getvheader ,vheader ,v)
     (header-size ,dest ,vheader)))

(defppclapmacro vector-length (dest v vheader)
  `(progn
     (getvheader ,vheader ,v)
     (header-length ,dest ,vheader)))


;;; Reference a 32-bit miscobj entry at a variable index.
;;; Make the caller explicitly designate a scratch register
;;; to use for the scaled index.

(defppclapmacro vref32 (dest miscobj index scaled-idx)
  `(progn
     (la ,scaled-idx ppc32::misc-data-offset ,index)
     (lwzx ,dest ,miscobj ,scaled-idx)))

;; The simple (no-memoization) case.
(defppclapmacro vset32 (src miscobj index scaled-idx)
  `(progn
     (la ,scaled-idx ppc32::misc-data-offset ,index)
     (stwx ,src ,miscobj ,scaled-idx)))

(defppclapmacro extract-lowbyte (dest src)
  (target-arch-case
   (:ppc32 `(clrlwi ,dest ,src (- 32 8)))
   (:ppc64 `(clrldi ,dest ,src (- 64 8)))))

(defppclapmacro unbox-fixnum (dest src)
  (target-arch-case
   (:ppc32
    `(srawi ,dest ,src ppc32::fixnumshift))
   (:ppc64
    `(sradi ,dest ,src ppc64::fixnumshift))))

(defppclapmacro box-fixnum (dest src)
  (target-arch-case
   (:ppc32
    `(slwi ,dest ,src ppc32::fixnumshift))
   (:ppc64
    `(sldi ,dest ,src ppc64::fixnumshift))))



;;; If crf is specified, type checks src
(defppclapmacro unbox-base-char (dest src &optional crf)
  (if (null crf)
    (target-arch-case
     (:ppc32 `(srwi ,dest ,src ppc32::charcode-shift))
     (:ppc64 `(srdi ,dest ,src ppc64::charcode-shift)))
    (let ((label (gensym)))
      (target-arch-case
       (:ppc32 `(progn
                 (clrlwi ,dest ,src (- ppc32::nbits-in-word ppc32::charcode-shift))
                 (cmpwi ,crf ,dest ppc32::subtag-character)
                 (srwi ,dest ,src ppc32::charcode-shift)
                 (beq+ ,crf ,label)
                 (uuo_interr arch::error-object-not-base-char ,src)
                 ,label))
       (:ppc64
        `(progn
          (clrldi ,dest ,src (- ppc64::nbits-in-word ppc64::num-subtag-bits))
          (cmpdi ,crf ,dest ppc64::subtag-character)
          (srdi ,dest ,src ppc64::charcode-shift)
          (beq+ ,crf ,label)
          (uuo_interr arch::error-object-not-base-char ,src)
          ,label))))))




(defppclapmacro ref-global (reg sym)
  (target-arch-case
   (:ppc32
    (let* ((offset (ppc32::%kernel-global sym)))
      `(lwz ,reg (+ ,offset (target-nil-value)) 0)))
   (:ppc64
    (let* ((offset (ppc64::%kernel-global sym)))
      `(ld ,reg (+ ,offset (target-nil-value)) 0)))))

(defppclapmacro set-global (reg sym)
  (target-arch-case
   (:ppc32
    (let* ((offset (ppc32::%kernel-global sym)))
      `(stw ,reg (+ ,offset (target-nil-value)) 0)))
   (:ppc64
    (let* ((offset (ppc64::%kernel-global sym)))
      `(std ,reg (+ ,offset (target-nil-value)) 0)))))

;;; Set "dest" to those bits in "src" that are other than those that
;;; would be set if "src" is a fixnum and of type (unsigned-byte
;;; "width").  If no bits are set in "dest", then "src" is indeed of
;;; type (unsigned-byte "width").  Set (:CR0 :EQ) according to the
;;; result.
(defppclapmacro extract-unsigned-byte-bits. (dest src width)
  (target-arch-case
   (:ppc32
    `(rlwinm. ,dest ,src 0 (- 32 ppc32::fixnumshift) (- 31 (+ ,width ppc32::fixnumshift))))
   (:ppc64
    `(rldicr. ,dest ,src (- 64 ppc64::fixnumshift) (- 63 ,width)))))



;;; You generally don't want to have to say "mfcr": it crosses functional
;;; units and forces synchronization (all preceding insns must complete,
;;; no subsequent insns may start.)
;;; There are often algebraic ways of computing ppc32::t-offset:

;;; Src has all but the least significant bit clear.  Map low bit to T/NIL.
(defppclapmacro bit0->boolean (dest src temp)
  (target-arch-case
   (:ppc32
    `(progn
      (rlwimi ,temp ,src 4 27 27)
      (addi ,dest ,temp (target-nil-value))))
   (:ppc64
    `(progn
      (mulli ,temp ,src ppc64::t-offset) ; temp = ppc64::t-offset, or 0
      (addi ,dest ,temp (target-nil-value)))))) ; dest = (src == 1), lisp-wise

(defppclapmacro eq0->boolean (dest src temp)
  (target-arch-case
   (:ppc32
    `(progn
      (cntlzw ,temp ,src)                ; 32 leading zeros if (src == 0)
      (srwi ,temp ,temp 5)               ; temp = (src == 0), C-wise
      (bit0->boolean ,dest ,temp ,temp)))
   (:ppc64
    `(progn
      (cntlzd ,temp ,src)               ; 64 leading zeros if (src == 0)
      (srdi ,temp ,temp 6)              ; temp = (src == 0), C-wise
      (bit0->boolean ,dest ,temp ,temp)))))

(defppclapmacro ne0->boolean (dest src temp)
  (target-arch-case
   (:ppc32
    `(progn
      (cntlzw ,temp ,src)                
      (slw ,temp ,src ,temp)
      (srwi ,temp ,temp 31)
      (bit0->boolean ,dest ,temp ,temp)))
   (:ppc64
    `(progn
      (cntlzd ,temp ,src)
      (sld ,temp ,src ,temp)
      (srdi ,temp ,temp 63) 
      (bit0->boolean ,dest ,temp ,temp)))))

(defppclapmacro eq->boolean (dest rx ry temp)
  `(progn
     (sub ,temp ,rx ,ry)
     (eq0->boolean ,dest ,temp ,temp)))


(defppclapmacro repeat (n inst)
  (let* ((insts ()))
    (dotimes (i n `(progn ,@(nreverse insts)))
      (push inst insts))))

(defppclapmacro get-single-float (dest node)
  (target-arch-case
   (:ppc32
    `(lfs ,dest ppc32::single-float.value ,node))
   (:ppc64
    `(progn
      (std ,node ppc64::tcr.single-float-convert ppc64::rcontext)
      (lfs ,dest ppc64::tcr.single-float-convert ppc64::rcontext)))))

(defppclapmacro get-double-float (dest node)
  (target-arch-case
   (:ppc32
    `(lfd ,dest ppc32::double-float.value ,node))
   (:ppc64
    `(lfd ,dest ppc64::double-float.value ,node))))
  

(defppclapmacro put-single-float (src node)
  (target-arch-case
   (:ppc32
    `(stfs ,src ppc32::single-float.value ,node))
   (:ppc64
    `(progn
      (stfs ,src ppc64::tcr.single-float-convert ppc64::rcontext)
      (ld ,node ppc64::tcr.single-float-convert ppc64::rcontext)))))

(defppclapmacro put-double-float (src node)
  (target-arch-case
   (:ppc32
    `(stfd ,src ppc32::double-float.value ,node))
   (:ppc64
    `(stfd ,src ppc64::double-float.value ,node))))

(defppclapmacro clear-fpu-exceptions ()
  `(mtfsf #xfc #.ppc::fp-zero))



;;; from ppc-bignum.lisp
(defppclapmacro digit-h (dest src)
  (target-arch-case
   (:ppc32
    `(rlwinm ,dest ,src (+ 16 ppc32::fixnumshift) (- 16 ppc32::fixnumshift) (- 31 ppc32::fixnumshift)))
   (:ppc64
    (error "DIGIT-H on PPC64 ?"))))

;;; from ppc-bignum.lisp
(defppclapmacro digit-l (dest src)
  (target-arch-case
   (:ppc32
    `(clrlslwi ,dest ,src 16 ppc32::fixnumshift))
   (:ppc64
    (error "DIGIT-L on PPC64 ?"))))
  
;;; from ppc-bignum.lisp
(defppclapmacro compose-digit (dest high low)
  (target-arch-case
   (:ppc32
    `(progn
      (rlwinm ,dest ,low (- ppc32::nbits-in-word ppc32::fixnumshift) 16 31)
      (rlwimi ,dest ,high (- 16 ppc32::fixnumshift) 0 15)))
   (:ppc64
    (error "COMPOSE-DIGIT on PPC64 ?"))))

(defppclapmacro macptr-ptr (dest macptr)
  (target-arch-case
   (:ppc32
    `(lwz ,dest ppc32::macptr.address ,macptr))
   (:ppc64
    `(ld ,dest ppc64::macptr.address ,macptr))))

(defppclapmacro svref (dest index vector)
  (target-arch-case
   (:ppc32
    `(lwz ,dest (+ (* 4 ,index) ppc32::misc-data-offset) ,vector))
   (:ppc64
    `(ld ,dest (+ (* 8 ,index) ppc64::misc-data-offset) ,vector))))

;;; This evals its args in the wrong order.
;;; Can't imagine any code will care.
(defppclapmacro svset (new-value index vector)
  (target-arch-case
   (:ppc32
    `(stw ,new-value (+ (* 4 ,index) ppc32::misc-data-offset) ,vector))
   (:ppc64
    `(std ,new-value (+ (* 8 ,index) ppc64::misc-data-offset) ,vector))))

(defppclapmacro vpush-argregs ()
  (let* ((none (gensym))
         (two (gensym))
         (one (gensym)))
  `(progn
     (cmpri cr1 nargs '2)
     (cmpri cr0 nargs 0)
     (beq cr1 ,two)
     (beq cr0 ,none)
     (blt cr1 ,one)
     (vpush arg_x)
     ,two
     (vpush arg_y)
     ,one
     (vpush arg_z)
     ,none)))



;;; Saving and restoring AltiVec registers.

;;; Note that under the EABI (to which PPCLinux conforms), the OS
;;; doesn't attach any special significance to the value of the VRSAVE
;;; register (spr 256).  Under some other ABIs, VRSAVE is a bitmask
;;; which indicates which vector registers are live at context switch
;;; time.  These macros contain code to maintain VRSAVE when the
;;; variable *ALTIVEC-LAPMACROS-MAINTAIN-VRSAVE-P* is true at
;;; macroexpand time; that variable is initialized to true if and only
;;; if :EABI-TARGET is not on *FEATURES*.  Making this behavior
;;; optional is supposed to help make code which uses these macros
;;; easier to port to other platforms.

;;; From what I can tell, a function that takes incoming arguments in
;;; vector registers (vr2 ... vr13) (and doesn't use any other vector
;;; registers) doesn't need to assert that it uses any vector
;;; registers (even on platforms that maintain VRSAVE.)  A function
;;; that uses vector registers that were not incoming arguments has to
;;; assert that it uses those registers on platforms that maintain
;;; VRSAVE.  On all platforms, a function that uses any non-volatile
;;; vector registers (vr20 ... vr31) has to assert that it uses these
;;; registers and save and restore the caller's value of these registers
;;; around that usage.

(defparameter *altivec-lapmacros-maintain-vrsave-p*
  #-eabi-target t
  #+eabi-target nil
  "Control the expansion of certain lap macros. Initialized to NIL on
LinuxPPC; initialized to T on platforms (such as MacOS X/Darwin) that
require that the VRSAVE SPR contain a bitmask of active vector registers
at all times.")

(defun %vr-register-mask (reglist)
  (let* ((mask 0))
    (dolist (reg reglist mask)
      (let* ((regval (ppc-vector-register-name-or-expression reg)))
        (unless (typep regval '(mod 32))
          (error "Bad AltiVec register - ~s" reg))
        (setq mask (logior mask (ash #x80000000 (- regval))))))))



;;; Build a frame on the temp stack large enough to hold N 128-bit vector
;;; registers and the saved value of the VRSAVE spr.  That frame will look
;;; like:
;;; #x??????I0   backpointer to previous tstack frame
;;; #x??????I4   non-zero marker: frame doesn't contain tagged lisp data
;;; #x??????I8   saved VRSAVE
;;; #x??????IC   pad word for alignment
;;; #x??????J0   first saved vector register
;;; #x??????K0   second saved vector register
;;;   ...
;;; #x??????X0   last saved vector register
;;; #x??????Y0   (possibly) 8 bytes wasted for alignment.
;;; #x????????   UNKNOWN; not necessarily the previous tstack frame
;;;
;;;  Use the specified immediate register to build the frame.
;;;  Save the caller's VRSAVE in the frame.

(defppclapmacro %build-vrsave-frame (n tempreg)
  (if (or (> n 0) *altivec-lapmacros-maintain-vrsave-p*)
    (if (zerop n)
      ;; Just make room for vrsave; no need to align to 16-byte boundary.
      `(progn
	(stwu tsp -16 tsp)
	(stw tsp 4 tsp))
      `(progn
	(la ,tempreg ,(- (ash (1+ n) 4)) ppc::tsp)
	(clrrwi ,tempreg ,tempreg 4)	; align to 16-byte boundary
	(sub ,tempreg ,tempreg ppc32::tsp) ; calculate (aligned) frame size.
	(stwux ppc::tsp ppc::tsp ,tempreg)
	(stw ppc::tsp 4 ppc::tsp)))	; non-zero: non-lisp
    `(progn)))

;;; Save the current value of the VRSAVE spr in the newly-created
;;; tstack frame.

(defppclapmacro %save-vrsave (tempreg)
  (if *altivec-lapmacros-maintain-vrsave-p*
    `(progn
      (mfspr ,tempreg 256)		; SPR 256 = vrsave
      (stw ,tempreg 8 tsp))
    `(progn)))



;;; When this is expanded, "tempreg" should contain the caller's vrsave.
(defppclapmacro %update-vrsave (tempreg mask)
  (let* ((mask-high (ldb (byte 16 16) mask))
         (mask-low (ldb (byte 16 0) mask)))
    `(progn
       ,@(unless (zerop mask-high) `((oris ,tempreg ,tempreg ,mask-high)))
       ,@(unless (zerop mask-low) `((ori ,tempreg ,tempreg ,mask-low)))
       (mtspr 256 ,tempreg))))

;;; Save each of the vector regs in "nvrs" into the current tstack 
;;; frame, starting at offset 16
(defppclapmacro %save-vector-regs (nvrs tempreg)
  (let* ((insts ()))
    (do* ((offset 16 (+ 16 offset))
          (regs nvrs (cdr regs)))
         ((null regs) `(progn ,@(nreverse insts)))
      (declare (fixnum offset))
      (push `(la ,tempreg ,offset ppc::tsp) insts)
      (push `(stvx ,(car regs) ppc::rzero ,tempreg) insts))))


;;; Pretty much the same idea, only we restore VRSAVE first and
;;; discard the tstack frame after we've reloaded the vector regs.
(defppclapmacro %restore-vector-regs (nvrs tempreg)
  (let* ((loads ()))
    (do* ((offset 16 (+ 16 offset))
          (regs nvrs (cdr regs)))
         ((null regs) `(progn
			,@ (when *altivec-lapmacros-maintain-vrsave-p*
			     `((progn
				 (lwz ,tempreg 8 ppc::tsp)
				 (mtspr 256 ,tempreg))))
			,@(nreverse loads)
			(lwz ppc::tsp 0 ppc::tsp)))
      (declare (fixnum offset))
      (push `(la ,tempreg ,offset ppc::tsp) loads)
      (push `(lvx ,(car regs) ppc::rzero ,tempreg) loads))))


(defun %extract-non-volatile-vector-registers (vector-reg-list)
  (let* ((nvrs ()))
    (dolist (reg vector-reg-list (nreverse nvrs))
      (let* ((regval (ppc-vector-register-name-or-expression reg)))
        (unless (typep regval '(mod 32))
          (error "Bad AltiVec register - ~s" reg))
        (when (>= regval 20)
          (pushnew regval nvrs))))))


;;; One could imagine something more elaborate:
;;; 1) Binding a global bitmask that represents the assembly-time notion
;;;    of VRSAVE's contents; #'ppc-vector-register-name-or-expression
;;;    could then warn if a vector register wasn't marked as active.
;;;    Maybe a good idea, but PPC-LAP would have to bind that special
;;;    variable to 0 to make things reentrant.
;;; 2) Binding a user-specified variable to the list of NVRs that need
;;;    to be restored, so that it'd be more convenient to insert one's
;;;    own calls to %RESTORE-VECTOR-REGS at appropriate points.
;;; Ad infinitum.  As is, this allows one to execute a "flat" body of code
;;;   that's bracketed by the stuff needed to keep VRSAVE in sync and
;;;   to save and restore any non-volatile vector registers specified.
;;;   That body of code is "flat" in the sense that it doesn't return,
;;;   tail-call, establish a catch or unwind-protect frame, etc.
;;;   It -can- contain lisp or foreign function calls.

(defppclapmacro %with-altivec-registers ((&key (immreg 'ppc::imm0)) reglist &body body)
  (let* ((mask (%vr-register-mask reglist))
         (nvrs (%extract-non-volatile-vector-registers reglist))
         (num-nvrs (length nvrs)))
    (if (or *altivec-lapmacros-maintain-vrsave-p* nvrs)
      `(progn
	(%build-vrsave-frame ,num-nvrs ,immreg)
	(%save-vrsave ,immreg)
	,@ (if *altivec-lapmacros-maintain-vrsave-p*
	     `((%update-vrsave ,immreg ,mask)))
	(%save-vector-regs ,nvrs ,immreg)
	(progn ,@body)
	(%restore-vector-regs ,nvrs ,immreg))
      `(progn ,@body))))


(defppclapmacro with-altivec-registers (reglist &body body)
  "Specify the set of AltiVec registers used in body. If
*altivec-lapmacros-maintain-vrsave-p* is true when the macro is expanded,
generates code to save the VRSAVE SPR and updates VRSAVE to incude a
bitmask generated from the specified register list. Generates code which
saves any non-volatile vector registers which appear in the register list,
executes body, and restores the saved non-volatile vector registers (and,
if *altivec-lapmacros-maintain-vrsave-p* is true, restores VRSAVE as well.
Uses the IMM0 register (r3) as a temporary."
  `(%with-altivec-registers () ,reglist ,@body))


;;; Create an aligned buffer on the temp stack, large enough for N vector
;;; registers.  Make base be a pointer to this buffer (base can be
;;; any available GPR, since the buffer will be fixnum-tagged.) N should
;;; be a constant.
;;; The intent here is that the register 'base' can be used in subsequent
;;; stvx/lvx instructions.  Any vector registers involved in such instructions
;;; must have their corresponding bits saved in VRSAVE on platforms where
;;; that matters.

(defppclapmacro allocate-vector-buffer (base n)
  `(progn
    (stwux tsp (- (ash (1+ ,n) 4)))	; allocate a frame on temp stack
    (stw tsp 4 tsp)			; temp frame contains immediate data
    (la ,base (+ 8 8) tsp)		; skip header, round up
    (clrrwi ,base ,base 4)))		; align (round down)

;;; Execute the specified body of code; on entry to that body, BASE
;;; will point to the lowest address of a vector-aligned buffer with
;;; room for N vector registers.  On exit, the buffer will be
;;; deallocated.  The body should preserve the value of BASE as long
;;; as it needs to reference the buffer.

(defppclapmacro with-vector-buffer (base n &body body)
  "Generate code which allocates a 16-byte aligned buffer large enough
to contain N vector registers; the GPR base points to the lowest address
of this buffer. After processing body, the buffer will be deallocated.
The body should preserve the value of base as long as it needs to
reference the buffer. It's intended that base be used as a base register
in stvx and lvx instructions within the body."
  `(progn
    (allocate-vector-buffer ,base ,n)
    (progn
      (progn ,@body)
      (unlink tsp))))

#|

;;; This is just intended to test the macros; I can't test whether or not the code works.

(defppclapfunction load-array ((n arg_z))
  (check-nargs 1)
  (with-altivec-registers (vr1 vr2 vr3 vr27) ; Clobbers imm0
    (li imm0 ppc32::misc-data-offset)
    (lvx vr1 arg_z imm0)		; load MSQ
    (lvsl vr27 arg_z imm0)		; set the permute vector
    (addi imm0 imm0 16)			; address of LSQ
    (lvx vr2 arg_z imm0)		; load LSQ
    (vperm vr3 vr1 vr2 vr27)		; aligned result appears in VR3
    (dbg t))				; Look at result in some debugger
  (blr))
|#

;;; see "Optimizing PowerPC Code" p. 156
;;; Note that the constant #x4330000080000000 is now in fp-s32conv

(defppclapmacro int-to-freg (int freg imm)
  (target-arch-case
   (:ppc32
    `(let ((temp 8)
           (temp.h 8)
           (temp.l 12))
      (stwu tsp -16 tsp)
      (stw tsp 4 tsp)
      (stfd ppc::fp-s32conv temp tsp)
      (unbox-fixnum ,imm ,int)
      (xoris ,imm ,imm #x8000)       ; invert sign of unboxed fixnum
      (stw ,imm temp.l tsp)
      (lfd ,freg temp tsp)
      (lwz tsp 0 tsp)
      (fsub ,freg ,freg ppc::fp-s32conv)))
   (:ppc64
    `(progn
      (unbox-fixnum ,imm ,int)
      (std ,imm -8 sp)
      (lfd ,freg -8 sp)
      (fcfid ,freg ,freg)))))

;;; Set the most significant bit in DEST, clear all other bits.
(defppclapmacro load-highbit (dest)
  (target-arch-case
   (:ppc32
    `(lis ,dest #x8000))
   (:ppc64
    `(progn
      (lis ,dest #x8000)
      (sldi ,dest ,dest 32)))))

(defppclapmacro extract-bit-shift-count (dest src)
  (target-arch-case
   (:ppc32 `(clrlwi ,dest ,src (- 32 ppc32::bitmap-shift)))
   (:ppc64 `(clrldi ,dest ,src (- 64 ppc64::bitmap-shift)))))

;;; "index" is the result of subtracting a base address from some
;;; possibly tagged pointer.  "bitwords" is the address of the first
;;; word of an (untagged) bitvector.
(defppclapmacro set-bit-at-index (bitwords index &optional (mask ppc::imm3) (count ppc::imm4) (was ppc::imm1))
  (let* ((done (gensym))
         (again (gensym)))
    `(progn
      (load-highbit ,mask)
      (srri ,index ,index ,(target-arch-case
                            (:ppc32 ppc32::dnode-shift)
                            (:ppc64 ppc64::dnode-shift)))
      (extract-bit-shift-count ,count ,index)
      (srr ,mask ,mask ,count)
      (srri ,index ,index ,(target-arch-case
                            (:ppc32 ppc32::bitmap-shift)
                            (:ppc64 ppc64::bitmap-shift)))
      (slri ,index ,index  ,(target-arch-case
                            (:ppc32 ppc32::word-shift)
                            (:ppc64 ppc64::word-shift)))
      (ldrx ,was ,bitwords ,index)
      (and. ,was ,was ,mask)
      (bne ,done)
      ,again
      (lrarx ,was ,bitwords ,index)
      (or ,was ,was ,mask)
      (strcx. ,was ,bitwords ,index)
      (bne ,again)
      ,done)))

;;; Like SET-BIT-AT-INDEX, but sets CR0[EQ] iff the index'th bit
;;; is set.
(defppclapmacro test-bit-at-index (bitwords index &optional (mask ppc::imm3) (count ppc::imm4) (was ppc::imm1))
  `(progn
    (load-highbit ,mask)
    (srri ,index ,index ,(target-arch-case
                          (:ppc32 ppc32::dnode-shift)
                          (:ppc64 ppc64::dnode-shift)))
    (extract-bit-shift-count ,count ,index)
    (srr ,mask ,mask ,count)
    (srri ,index ,index ,(target-arch-case
                          (:ppc32 ppc32::bitmap-shift)
                          (:ppc64 ppc64::bitmap-shift)))
    (slri ,index ,index  ,(target-arch-case
                           (:ppc32 ppc32::word-shift)
                           (:ppc64 ppc64::word-shift)))
    (ldrx ,was ,bitwords ,index)
    (and. ,mask ,was ,mask)))
                                           
(defppclapmacro u32-ref (dest index vector)
  `(lwz ,dest (+ (* 4 ,index) target::misc-data-offset) ,vector))

(defppclapmacro u32-set (new-value index vector)
  `(stw ,new-value (+ (* 4 ,index) target::misc-data-offset) ,vector))

(provide "PPC-LAPMACROS")

;;; end of ppc-lapmacros.lisp
