;;;-*- Mode: Lisp; Package: CCL -*-
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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "ARM-LAP"))




(defarmlapmacro set-nargs (n)
  (check-type n (unsigned-byte 8))
  `(mov nargs (:$ (ash ,n arm::fixnumshift))))

(defarmlapmacro check-nargs (min &optional (max min))
  (let* ((ok1 (gensym))
         (ok2 (gensym)))
    (if (eq max min)
      `(progn
        (cmp nargs (:$ (ash ,min arm::fixnumshift)))
        (beq ,ok1)
        (uuo-error-wrong-nargs (:? ne))
        ,ok1)
      (if (null max)
        (unless (= min 0)
          `(progn
            (cmp nargs (:$ (ash ,min arm::fixnumshift)))
            (bhs ,ok1)
            (uuo-error-wrong-nargs (:? lo))
            ,ok1))
        (if (= min 0)
          `(progn
            (cmp nargs (:$ (ash ,max arm::fixnumshift)))
            (bls ,ok1)
            (uuo-error-wrong-nargs (:? hi))
            ,ok1)
          `(progn
            (cmp nargs (:$ (ash ,min arm::fixnumshift)))
            (bhs ,ok1)
            (uuo-error-wrong-nargs (:? lo))
            ,ok1
            (cmp nargs (:$ (ash ,max arm::fixnumshift)))
            (bls ,ok2)
            (uuo-error-wrong-nargs (:? hi))
            ,ok2))))))





;;; This needs to be done if we aren't a leaf function (e.g., if we
;;; clobber our return address or need to reference any constants.  Note
;;; that it's not atomic wrt a preemptive scheduler, but we need to
;;; pretend that it will be.)  The VSP to be saved is the value of the
;;; VSP before any of this function's arguments were vpushed by its
;;; caller; that's not the same as the VSP register if any non-register
;;; arguments were received, but is usually easy to compute.

(defarmlapmacro build-lisp-frame (&optional (marker-reg 'imm0) (vsp 'vsp))
  `(progn
    (mov ,marker-reg (:$ arm::lisp-frame-marker))
    (stmdb (:! sp) (,marker-reg ,vsp fn lr))))

(defarmlapmacro restore-lisp-frame (&optional (marker-reg 'imm0) (vsp 'vsp))
  `(ldmia (:! sp) (,marker-reg ,vsp fn lr)))

(defarmlapmacro return-lisp-frame (&optional (marker-reg 'imm0))
  `(ldmia (:! sp) (,marker-reg vsp fn pc)))

(defarmlapmacro discard-lisp-frame ()
  `(add sp sp (:$ arm::lisp-frame.size)))


(defarmlapmacro push1 (src stack)
  `(str ,src (:+@! ,stack (:$ (- arm::node-size)))))

(defarmlapmacro vpush1 (src)
  `(push1 ,src vsp))


(defarmlapmacro pop1 (dest stack)
  `(ldr ,dest (:@+ ,stack (:$ arm::node-size))))

(defarmlapmacro vpop1 (dest)
  `(pop1 ,dest vsp))

(defarmlapmacro %cdr (dest node)
  `(ldr ,dest (:@ ,node (:$ arm::cons.cdr))))

(defarmlapmacro %car (dest node)
  `(ldr ,dest (:@ ,node (:$ arm::cons.car))))



(defarmlapmacro extract-lisptag (dest node)
  `(and ,dest ,node (:$ arm::tagmask)))

(defarmlapmacro extract-fulltag (dest node)
  `(and ,dest ,node (:$ arm::fulltagmask)))


(defarmlapmacro extract-subtag (dest node)
  `(ldrb ,dest (:@ ,node (:$ arm::misc-subtag-offset))))

(defarmlapmacro extract-typecode (dest node)
  `(progn
    (and ,dest ,node (:$ arm::tagmask))
    (cmp ,dest (:$ arm::tag-misc))
    (ldrbeq ,dest (:@ ,node (:$ arm::misc-subtag-offset)))))

;;; Set the EQ bit if NODE is a fixnum
(defarmlapmacro test-fixnum (node)
  `(tst ,node (:$ arm::tagmask)))

(defarmlapmacro trap-unless-fixnum (node)
  (let* ((ok (gensym)))
  `(progn
    (test-fixnum ,node)
    (beq ,ok)
    (uuo-error-reg-not-lisptag ,node (:$ arm::tag-fixnum))
    ,ok)))


(defarmlapmacro trap-unless-lisptag= (node tag &optional (immreg 'imm0))
  (let* ((ok (gensym)))
  `(progn
    (extract-lisptag ,immreg ,node)
    (cmp ,immreg (:$ ,tag))
    (beq ,ok)
    (uuo-error-reg-not-lisptag ,node (:$ ,tag))
    ,ok)))

(defarmlapmacro trap-unless-fulltag= (node tag &optional (immreg 'imm0))
  (let* ((ok (gensym)))
  `(progn
    (extract-fulltag ,immreg ,node)
    (cmp ,immreg (:$ ,tag))
    (beq ,ok)
    (uuo-error-reg-not-fulltag ,node (:$ ,tag))
    ,ok)))


(defarmlapmacro trap-unless-xtype= (node tag &optional (immreg 'imm0))
  (let* ((ok (gensym)))
  `(progn
    (extract-typecode ,immreg ,node)
    (cmp ,immreg (:$ ,tag))
    (beq ,ok)
    (uuo-error-reg-not-xtype ,node (:$ ,tag))
    ,ok)))


(defarmlapmacro load-constant (dest constant)
  `(ldr ,dest (:@ fn ',constant)))

;;; This is about as hard on the pipeline as anything I can think of.
(defarmlapmacro call-symbol (function-name)
  `(progn
    (load-constant fname ,function-name)
    (ldr nfn (:@ fname (:$ arm::symbol.fcell)))
    (ldr lr (:@ nfn (:$ arm::function.entrypoint)))
    (blx lr)))

(defarmlapmacro sp-call-symbol (function-name)
  `(progn
     (load-constant fname ,function-name)
     (bla .SPjmpsym)))

(defarmlapmacro getvheader (dest src)
  `(ldr ,dest (:@ ,src (:$ arm::misc-header-offset))))

;;; "Size" is unboxed element-count.
(defarmlapmacro header-size (dest vheader)
  `(mov ,dest (:lsr ,vheader (:$ arm::num-subtag-bits))))


;;; "Length" is fixnum element-count.
(defarmlapmacro header-length (dest vheader)
  `(progn
    (bic ,dest ,vheader (:$ arm::subtag-mask))
    (mov ,dest (:lsr ,dest (:$ (- arm::num-subtag-bits arm::fixnumshift))))))


(defarmlapmacro header-subtag[fixnum] (dest vheader)
  `(progn
    (mov ,dest (:$ (ash arm::subtag-mask arm::fixnumshift)))
    (and ,dest ,dest (:lsl ,vheader (:$ arm::fixnumshift)))))


(defarmlapmacro vector-size (dest v vheader)
  `(progn
     (getvheader ,vheader ,v)
     (header-size ,dest ,vheader)))

(defarmlapmacro vector-length (dest v vheader)
  `(progn
     (getvheader ,vheader ,v)
     (header-length ,dest ,vheader)))


;;; Reference a 32-bit miscobj entry at a variable index.
;;; Make the caller explicitly designate a scratch register
;;; to use for the scaled index.

(defarmlapmacro vref32 (dest miscobj index scaled-idx)
  `(progn
    (add ,scaled-idx ,index (:$ arm::misc-data-offset))
    (ldr ,dest (:@ ,miscobj ,scaled-idx))))

;; The simple (no-memoization) case.
(defarmlapmacro vset32 (src miscobj index scaled-idx)
  `(progn
    (add ,scaled-idx ,index (:$ arm::misc-data-offset))
    (str ,src (:@ ,miscobj ,scaled-idx))))

(defarmlapmacro extract-lowbyte (dest src)
  `(and ,dest ,src (:$ arm::subtag-mask)))

(defarmlapmacro unbox-fixnum (dest src)
  `(mov ,dest (:asr ,src (:$ arm::fixnumshift))))

(defarmlapmacro box-fixnum (dest src)
  `(mov ,dest (:lsl ,src (:$ arm::fixnumshift))))



;;; If check is non-nil, type checks src
(defarmlapmacro unbox-base-char (dest src &optional check)
  `(progn
    ,@(if check
          `((trap-unless-xtype= ,src arm::subtag-character ,dest)))
    (mov ,dest ,src (:lsr (:$ arm::charcode-shift)))))




(defarmlapmacro ref-global (reg sym)
  (let* ((offset (arm::%kernel-global sym)))
    `(progn
      (mov ,reg (:$ (- arm::nil-value arm::fulltag-nil)))
      (ldr ,reg (:@ ,reg (:$ ,offset))))))

(defarmlapmacro set-global (reg sym temp)
  (let* ((offset (arm::%kernel-global sym)))
    `(progn
      (mov ,temp (:$ (- arm::nil-value arm::fulltag-nil)))
      (str ,reg (:@ ,temp (:$ ,offset))))))







(defarmlapmacro cond->boolean (cc dest rx ry)
  `(progn
    (cmp ,rx ,ry)
    (mov ,dest 'nil)
    (add (:? ,cc) ,dest ,dest (:$ arm::t-offset))))


(defarmlapmacro repeat (n inst)
  (let* ((insts ()))
    (dotimes (i n `(progn ,@(nreverse insts)))
      (push inst insts))))

(defarmlapmacro get-single-float (dest node temp)
  `(progn
    (ldr ,temp (:@ ,node (:$ arm::single-float.value)))
    (fmsr ,dest ,temp)))

(defarmlapmacro get-double-float (dest node)
  `(progn
    (ldrd imm0 (:@ ,node (:$ arm::double-float.value)))
    (fmdrr ,dest imm0 imm1)))
  

(defarmlapmacro put-single-float (src node temp)
  `(progn
    (fmrs ,temp ,src)
    (str ,temp (:@ ,node (:$ arm::single-float.value)))))

(defarmlapmacro put-double-float (src node)
  `(progn
    (fmrrd imm0 imm1 ,src)
    (strd imm0 (:@ ,node (:$ arm::double-float.value)))))


(defarmlapmacro clear-fpu-exceptions ()
  (error "Later."))



(defarmlapmacro digit-h (dest src)
  `(progn
    (mov ,dest (:$ (ash #xff arm::fixnumshift)))
    (orr ,dest ,dest (:lsl ,dest (:$ 8)))
    (and ,dest ,dest (:lsr ,src  (:$ (- 16 arm::fixnumshift))))))

(defarmlapmacro digit-l (dest src)
  `(progn
    (mov ,dest (:$ (ash #xff arm::fixnumshift)))
    (orr ,dest ,dest (:lsl ,dest (:$ 8)))
    (and ,dest ,dest (:lsl ,src  (:$ arm::fixnumshift)))))
  

(defarmlapmacro compose-digit (dest high low)
  ;; Can we assume that HIGH and LOW are boxed 16-bit fixnums ?
  ;; This code does ...
  `(progn
    (movw ,dest (:$ #xffff))
    (and ,dest ,dest (:lsr ,low (:$ arm::fixnumshift)))
    (orr ,dest ,dest (:lsl ,high (:$ (- 16 arm::fixnumshift))))))

(defarmlapmacro macptr-ptr (dest macptr)
  `(ldr ,dest (:@ ,macptr (:$ arm::macptr.address))))

(defarmlapmacro svref (dest index vector)
 `(ldr ,dest (:@ ,vector (:$ (+ (* 4 ,index) arm::misc-data-offset)))))

;;; Immediate indices (for e.g. gfs) don't account for the entrypoint.
(defarmlapmacro nth-immediate (dest index vector)
  `(svref ,dest (1+ ,index) ,vector))

;;; This evals its args in the wrong order.
;;; Can't imagine any code will care.
(defarmlapmacro svset (new-value index vector)
  `(str ,new-value (:@ ,vector (:$ (+ (* 4 ,index) arm::misc-data-offset)))))

(defarmlapmacro vpush-argregs ()
  (let* ((none (gensym)))
  `(progn
    (cmp nargs (:$ 0))
    (beq ,none)
    (cmp nargs '2)
    (strgt arg_x (:@! vsp (:$ (- arm::node-size))))
    (strge arg_y (:@! vsp (:$ (- arm::node-size))))
    (str arg_z (:@! vsp (:$ (- arm::node-size))))
     ,none)))





;;; Set the most significant bit in DEST, clear all other bits.
(defarmlapmacro load-highbit (dest)
  `(mov ,dest (:$ #x80000000)))

                                           
(defarmlapmacro u32-ref (dest index vector)
  `(ldr ,dest (:@ ,vector (:$ (+ (* 4 ,index) arm::misc-data-offset)))))

(defarmlapmacro u32-set (new-value index vector)
  `(str ,new-value (:@ ,vector (:$ (+ (* 4 ,index) arm::misc-data-offset)))))

;;; Load the low 32 bits of the integer constant VAL into REG, using movw/movt.
(defarmlapmacro lri (reg val)
  (let* ((high (ldb (byte 16 16) val))
         (low (ldb (byte 16 0) val)))
  `(progn
    (movw ,reg (:$ ,low))
    ,@(unless (zerop high)
       `((movt ,reg (:$ ,high)))))))

(defarmlapmacro push-fprs (n)
  "Save N fprs starting at d8 on the control stack.
   (This actually loads a vector header into d7 and
   stores n+1 FPRs, starting at d7.  Clobbers imm0 and
   imm1."
  `(progn
    (movw imm0 (:$ (logior (ash (1+ ,n) arm::num-subtag-bits)
                            arm::subtag-double-float-vector)))
    (mov imm1 (:$ 0))
    (fmdrr d7 imm0 imm1)
    (fstmdbd d7 (:! sp) ,(1+ n))))

(defarmlapmacro pop-fprs (n)
  "Restore N fprs starting at d8 from the top of the control
   stack.  (This actually restores N+1 fprs starting at d7;
   on exit, d7 will contain the vector header that had been
   on top of the stack.)"
  `(fldmiad d7 (:! sp) ,(1+ n)))



(provide "ARM-LAPMACROS")

;;; end of arm-lapmacros.lisp
