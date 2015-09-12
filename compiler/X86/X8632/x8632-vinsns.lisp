;;;-*- Mode: Lisp; Package: (CCL :use CL) -*-

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "VINSN")
  (require "X8632-BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "X8632ENV"))

(defun unsigned-to-signed (u nbits)
  (if (logbitp (1- nbits) u)
    (- u (ash 1 nbits))
    u))

(defmacro define-x8632-vinsn (vinsn-name (results args &optional temps) &body body)
  (%define-vinsn *x8632-backend* vinsn-name results args temps body))

(define-x8632-vinsn scale-32bit-misc-index (((dest :u32))
					    ((idx :imm)	; A fixnum
					     )
					    ())
  (movl (:%l idx) (:%l dest)))

(define-x8632-vinsn scale-16bit-misc-index (((dest :u32))
					    ((idx :imm))) ; A fixnum
  (movl (:%l idx) (:%l dest))
  (shrl (:$ub 1) (:%l dest)))

(define-x8632-vinsn scale-8bit-misc-index (((dest :u32))
					    ((idx :imm))) ; A fixnum
  (movl (:%l idx) (:%l dest))
  (shrl (:$ub 2) (:%l dest)))

;;; same as above, but looks better in bit vector contexts
(define-x8632-vinsn scale-1bit-misc-index (((dest :u32))
					    ((idx :imm))) ; A fixnum
  (movl (:%l idx) (:%l dest))
  (shrl (:$ub 2) (:%l dest)))

(define-x8632-vinsn misc-ref-u32 (((dest :u32))
				  ((v :lisp)
				   (scaled-idx :u32)))
  (movl (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx)) (:%l dest)))

(define-x8632-vinsn misc-ref-double-float  (((dest :double-float))
                                            ((v :lisp)
                                             (scaled-idx :imm)))
  (movsd (:@ x8632::misc-dfloat-offset (:%l v) (:%l scaled-idx) 2) (:%xmm dest)))

(define-x8632-vinsn misc-ref-c-double-float  (((dest :double-float))
                                              ((v :lisp)
					       (idx :s32const)))
  (movsd (:@ (:apply + x8632::misc-dfloat-offset (:apply ash idx 3)) (:%l v)) (:%xmm dest)))

(define-x8632-vinsn misc-ref-node  (((dest :lisp))
                                    ((v :lisp)
                                     (scaled-idx :imm)))
  (movl (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx)) (:%l dest)))

(define-x8632-vinsn (push-misc-ref-node :push :node :vsp) (()
							   ((v :lisp)
							    (scaled-idx :imm)))
  (pushl (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx))))

(define-x8632-vinsn misc-set-node (()
				   ((val :lisp)
				    (v :lisp)
				    (unscaled-idx :imm))
				   ())
  (movl (:%l val) (:@ x8632::misc-data-offset (:%l v) (:%l unscaled-idx))))

(define-x8632-vinsn misc-set-immediate-node (()
                                             ((val :s32const)
                                              (v :lisp)
                                              (unscaled-idx :imm))
                                             ())
  (movl (:$l val) (:@ x8632::misc-data-offset (:%l v) (:%l unscaled-idx))))

(define-x8632-vinsn misc-set-single-float (()
					   ((val :single-float)
					    (v :lisp)
					    (scaled-idx :u32))
					   ())
  (movss (:%xmm val) (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx))))

(define-x8632-vinsn misc-set-double-float (()
				   ((val :double-float)
				    (v :lisp)
				    (unscaled-idx :imm))
				   ())
  (movsd (:%xmm val) (:@ x8632::misc-dfloat-offset (:%l v) (:%l unscaled-idx) 2)))

(define-x8632-vinsn misc-set-complex-single-float (()
				   ((val :complex-single-float)
				    (v :lisp)
				    (unscaled-idx :imm))
				   ())
  (movq (:%xmm val) (:@ x8632::complex-single-float.realpart (:%l  v) (:%l unscaled-idx) 2)))

(define-x8632-vinsn misc-set-complex-double-float (()
                                                   ((val :complex-double-float)
                                                    (v :lisp)
                                                    (unscaled-idx :imm))
 				   ())
  (movdqu (:%xmm val) (:@ x8632::complex-double-float.realpart (:%l  v) (:%l unscaled-idx) 4)))

(define-x8632-vinsn misc-ref-u8 (((dest :u8))
                                 ((v :lisp)
                                  (scaled-idx :s32)))
  (movzbl (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx)) (:%l dest)))

(define-x8632-vinsn misc-ref-s8 (((dest :s8))
                                 ((v :lisp)
                                  (scaled-idx :s32)))
  (movsbl (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx)) (:%l dest)))

(define-x8632-vinsn misc-ref-c-u16 (((dest :u16))
				    ((v :lisp)
				     (idx :u32const)))
  (movzwl (:@ (:apply + x8632::misc-data-offset (:apply ash idx 1)) (:%l v)) (:%l dest)))

(define-x8632-vinsn misc-ref-c-s16 (((dest :s16))
				    ((v :lisp)
				     (idx :u32const)))
  (movswl (:@ (:apply + x8632::misc-data-offset (:apply ash idx 1)) (:%l v)) (:%l dest)))

(define-x8632-vinsn misc-ref-u16 (((dest :u16))
                                  ((v :lisp)
                                   (scaled-idx :s32)))
  (movzwl (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx)) (:%l dest)))

(define-x8632-vinsn misc-ref-u32 (((dest :u32))
                                  ((v :lisp)
                                   (scaled-idx :s32)))
  (movl (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx)) (:%l dest)))

(define-x8632-vinsn misc-ref-single-float (((dest :single-float))
                                           ((v :lisp)
                                            (scaled-idx :s32)))
  (movss (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx)) (:%xmm dest)))

(define-x8632-vinsn misc-ref-complex-single-float  (((dest :complex-single-float))
                                                    ((v :lisp)
                                                     (scaled-idx :imm)))
  (movq (:@ x8632::complex-single-float.realpart (:%l v) (:%l scaled-idx) 2) (:%xmm dest)))

(define-x8632-vinsn misc-ref-c-complex-single-float  (((dest :complex-single-float))
                                                       ((v :lisp)
                                                        (idx :s32const)))
  (movq (:@ (:apply + x8632::complex-single-float.realpart (:apply ash idx x8632::word-shift)) (:%l v)) (:%xmm dest)))

(define-x8632-vinsn misc-ref-complex-double-float  (((dest :complex-double-float))
                                                    ((v :lisp)
                                                     (scaled-idx :imm)))
  (movdqu (:@ x8632::complex-double-float.realpart (:%l v) (:%l scaled-idx) 4) (:%xmm dest)))

(define-x8632-vinsn misc-ref-s32 (((dest :s32))
                                  ((v :lisp)
                                   (scaled-idx :s32)))
  (movl (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx)) (:%l dest)))

(define-x8632-vinsn misc-ref-s16 (((dest :s16))
                                  ((v :lisp)
                                   (scaled-idx :s32)))
  (movswl (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx)) (:%l dest)))

(define-x8632-vinsn misc-ref-c-node  (((dest :lisp))
				     ((v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movl (:@ (:apply + x8632::misc-data-offset (:apply ash idx x8632::word-shift)) (:%l v)) (:%l dest)))

(define-x8632-vinsn (push-misc-ref-c-node :push :node :vsp)
    (()
     ((v :lisp)
      (idx :u32const)) ; sic
     ())
  (pushl (:@ (:apply + x8632::misc-data-offset (:apply ash idx x8632::word-shift)) (:%l v))))

(define-x8632-vinsn misc-ref-c-u32  (((dest :u32))
				     ((v :lisp)
				      (idx :u32const)) ; sic
				     ())
  ;; xxx - should the 2 be x8632::word-shift?
  (movl (:@ (:apply + x8632::misc-data-offset (:apply ash idx 2)) (:%l v)) (:%l dest)))

(define-x8632-vinsn misc-ref-c-s32  (((dest :s32))
				     ((v :lisp)
				      (idx :s32const)) ; sic
				     ())
  (movl (:@ (:apply + x8632::misc-data-offset (:apply ash idx x8632::word-shift)) (:%l v)) (:%l dest)))

(define-x8632-vinsn misc-ref-c-single-float  (((dest :single-float))
                                              ((v :lisp)
                                               (idx :s32const)) ; sic
                                              ())
  (movss (:@ (:apply + x8632::misc-data-offset (:apply ash idx x8632::word-shift)) (:%l v)) (:%xmm dest)))

(define-x8632-vinsn misc-ref-c-u8  (((dest :u32))
				     ((v :lisp)
				      (idx :s32const)) ; sic
				     ())
  (movzbl (:@ (:apply + x8632::misc-data-offset idx) (:%l v)) (:%l dest)))

(define-x8632-vinsn misc-ref-c-s8  (((dest :s32))
				     ((v :lisp)
				      (idx :s32const)) ; sic
				     ())
  (movsbl (:@ (:apply + x8632::misc-data-offset idx) (:%l v)) (:%l dest)))

(define-x8632-vinsn misc-set-c-s8  (((val :s8))
				    ((v :lisp)
				     (idx :u32const))
				    ())
  (movb (:%b val) (:@ (:apply + x8632::misc-data-offset idx) (:%l v))))

(define-x8632-vinsn misc-set-s8  (((val :s8))
				  ((v :lisp)
				   (scaled-idx :s32))
				  ())
  (movb (:%b val) (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx))))

(define-x8632-vinsn mem-ref-s8 (((dest :s8))
				((src :address)
				 (index :s32)))
  (movsbl (:@ (:%l src) (:%l index)) (:%l dest)))

(define-x8632-vinsn misc-set-c-node (()
				     ((val :lisp)
				      (v :lisp)
				     (idx :s32const)))
  (movl (:%l val) (:@ (:apply + x8632::misc-data-offset (:apply ash idx 2)) (:%l v))))

(define-x8632-vinsn misc-set-immediate-c-node (()
                                               ((val :s32const)
                                                (v :lisp)
                                                (idx :s32const)))
  (movl (:$l val) (:@ (:apply + x8632::misc-data-offset (:apply ash idx 2)) (:%l v))))

;;; xxx don't know if this is right
(define-x8632-vinsn set-closure-iorward-reference (()
                                                   ((val :lisp)
                                                    (closure :lisp)
                                                    (idx :s32const)))
  (movl (:%l val) (:@ (:apply + x8632::misc-data-offset (:apply ash idx x8632::word-shift)) (:%l closure))))

(define-x8632-vinsn misc-set-c-double-float (()
				    ((val :double-float)
				     (v :lisp)
				     (idx :s32const)))
  (movsd (:%xmm val) (:@ (:apply + x8632::misc-dfloat-offset (:apply ash idx 3)) (:%l v))))

(define-x8632-vinsn (call-known-symbol :call) (((result (:lisp x8632::arg_z)))
                                               ()
					       ((entry (:label 1))))
  (:talign x8632::fulltag-tra)
  (call (:@ x8632::symbol.fcell (:% x8632::fname)))
  (movl (:$self 0) (:%l x8632::fn)))

(define-x8632-vinsn (jump-known-symbol :jumplr) (()
                                                 ())

  (jmp (:@ x8632::symbol.fcell (:% x8632::fname))))

(define-x8632-vinsn set-nargs (()
			       ((n :u16const))
                               ((casualty (:lisp #.x8632::nargs))))
  (:if (:pred = n 0)
    (xorl (:%l x8632::nargs) (:%l x8632::nargs))
    (movl (:$l (:apply ash n x8632::fixnumshift)) (:%l x8632::nargs))))


(define-x8632-vinsn check-exact-nargs (()
                                       ((n :u16const)))
  :resume
  (:if (:pred = n 0)
    (testl (:%l x8632::nargs) (:%l x8632::nargs))
    (:if (:pred < n 32)
      (cmpl (:$b (:apply ash n x8632::fixnumshift)) (:%l x8632::nargs))
      (cmpl (:$l (:apply ash n x8632::fixnumshift)) (:%l x8632::nargs))))
  (jne :bad)
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-wrong-number-of-args)))

(define-x8632-vinsn check-min-nargs (()
				     ((min :u16const)))
  :resume
  (:if (:pred = min 1)
    (:progn
      (testl (:%l x8632::nargs) (:%l x8632::nargs))
      (je :toofew))
    (:progn
      (:if (:pred < min 32)
        (rcmpl (:%l x8632::nargs) (:$b (:apply ash min x8632::fixnumshift)))
        (rcmpl (:%l x8632::nargs) (:$l (:apply ash min x8632::fixnumshift))))
      (jb :toofew)))
  (:anchored-uuo-section :resume)
  :toofew
  (:anchored-uuo (uuo-error-too-few-args)))

(define-x8632-vinsn check-max-nargs (()
				     ((n :u16const)))
  :resume
  (:if (:pred < n 32)
   (rcmpl (:%l x8632::nargs) (:$b (:apply ash n x8632::fixnumshift)))
   (rcmpl (:%l x8632::nargs) (:$l (:apply ash n x8632::fixnumshift))))
  (ja :bad)
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-too-many-args)))

(define-x8632-vinsn check-min-max-nargs (()
                                         ((min :u16const)
                                          (max :u16const)))
  :resume
  (:if (:pred = min 1)
    (:progn
      (testl (:%l x8632::nargs) (:%l x8632::nargs))
      (je :toofew))
    (:progn
      (:if (:pred < min 32)
        (rcmpl (:%l x8632::nargs) (:$b (:apply ash min x8632::word-shift)))
        (rcmpl (:%l x8632::nargs) (:$l (:apply ash min x8632::word-shift))))
      (jb :toofew)))
  (:if (:pred < max 32)
   (rcmpl (:%l x8632::nargs) (:$b (:apply ash max x8632::word-shift)))
   (rcmpl (:%l x8632::nargs) (:$l (:apply ash max x8632::word-shift))))
  (ja :toomany)
  
  (:anchored-uuo-section :resume)
  :toofew
  (:anchored-uuo (uuo-error-too-few-args))
  (:anchored-uuo-section :resume)
  :toomany
  (:anchored-uuo (uuo-error-too-many-args)))

(define-x8632-vinsn default-1-arg (()
                                   ((min :u16const)))
  (:if (:pred < min 32)
    (rcmpl (:%l x8632::nargs) (:$b (:apply ash min x8632::fixnumshift)))
    (rcmpl (:%l x8632::nargs) (:$l (:apply ash min x8632::fixnumshift))))
  (jne :done)
  ((:pred >= min 2)
   (pushl (:%l x8632::arg_y)))
  ((:pred >= min 1)
   (movl (:%l x8632::arg_z) (:%l x8632::arg_y)))
  (movl (:$l (:apply target-nil-value)) (:%l x8632::arg_z))
  :done)

(define-x8632-vinsn default-2-args (()
				    ((min :u16const)))
  (:if (:pred < (:apply 1+ min) 32)
    (rcmpl (:%l x8632::nargs) (:$b (:apply ash (:apply 1+ min) x8632::fixnumshift)))
    (rcmpl (:%l x8632::nargs) (:$l (:apply ash (:apply 1+ min) x8632::fixnumshift))))
  (ja :done)
  (je :one)
  ;; We got "min" args; arg_y & arg_z default to nil
  ((:pred >= min 2)
   (pushl (:%l x8632::arg_y)))
  ((:pred >= min 1)
   (pushl (:%l x8632::arg_z)))
  (movl (:$l (:apply target-nil-value)) (:%l x8632::arg_y))
  (jmp :last)
  :one
  ;; We got min+1 args: arg_y was supplied, arg_z defaults to nil.
  ((:pred >= min 1)
   (pushl (:%l x8632::arg_y)))
  (movl (:%l x8632::arg_z) (:%l x8632::arg_y))
  :last
  (movl (:$l (:apply target-nil-value)) (:%l x8632::arg_z))
  :done)

(define-x8632-vinsn default-optionals (()
                                       ((n :u16const))
                                       ((temp :u32)
					(nargs (:lisp #.x8632::nargs))))
  (movl (:%l x8632::nargs) (:%l temp))
  (:if (:pred < n 32)
    (rcmpl (:%l x8632::nargs) (:$b (:apply ash n x8632::fixnumshift)))
    (rcmpl (:%l x8632::nargs) (:$l (:apply ash n x8632::fixnumshift))))
  (jae :done)
  :loop
  (addl (:$b x8632::fixnumone) (:%l temp))
  (pushl (:$l (:apply target-nil-value)))
  (:if (:pred < n 32)
    (cmpl (:$b (:apply ash n x8632::fixnumshift)) (:%l temp))
    (cmpl (:$l (:apply ash n x8632::fixnumshift)) (:%l temp)))
  (jne :loop)
  :done)

(define-x8632-vinsn save-lisp-context-no-stack-args (()
                                                     ())
  (pushl (:%l x8632::ebp))
  (movl (:%l x8632::esp) (:%l x8632::ebp)))

(define-x8632-vinsn save-lisp-context-offset (()
					      ((nbytes-pushed :s32const)))
  (movl (:%l x8632::ebp) (:@ (:apply + nbytes-pushed x8632::node-size) (:%l x8632::esp)))
  (leal (:@ (:apply + nbytes-pushed x8632::node-size) (:%l x8632::esp)) (:%l x8632::ebp))
  (popl  (:@ x8632::node-size (:%l x8632::ebp))))

(define-x8632-vinsn save-lisp-context-variable-arg-count (()
                                                          ()
                                                          ((temp :u32)
							   (nargs (:lisp #.x8632::nargs))))
  (movl (:%l x8632::nargs) (:%l temp))
  (subl (:$b (* $numx8632argregs x8632::node-size)) (:%l temp))
  (jle :push)
  (movl (:%l x8632::ebp) (:@ x8632::node-size (:%l x8632::esp) (:%l temp)))
  (leal (:@ x8632::node-size (:%l x8632::esp) (:%l temp)) (:%l x8632::ebp))
  (popl (:@ x8632::node-size (:%l x8632::ebp)))
  (jmp :done)
  :push
  (pushl (:%l x8632::ebp))
  (movl (:%l x8632::esp) (:%l x8632::ebp))
  :done)

;;; We know that some args were pushed, but don't know how many were
;;; passed.
(define-x8632-vinsn save-lisp-context-in-frame (()
                                                ()
                                                ((temp :u32)
						 (nargs (:lisp #.x8632::nargs))))
  (movl (:%l x8632::nargs) (:%l temp))
  (subl (:$b (* $numx8632argregs x8632::node-size)) (:%l temp))
  (movl (:%l x8632::ebp) (:@ x8632::node-size (:%l x8632::esp) (:%l temp)))
  (leal (:@ x8632::node-size (:%l x8632::esp) (:%l temp)) (:%l x8632::ebp))
  (popl  (:@ x8632::node-size (:%l x8632::ebp))))

(define-x8632-vinsn (vpush-register :push :node :vsp)
    (()
     ((reg :lisp)))
  (pushl (:% reg)))

(define-x8632-vinsn (vpush-fixnum :push :node :vsp)
    (()
     ((const :s32const)))
  (:if (:and  (:pred < const 128) (:pred >= const -128))
    (pushl (:$b const))
    (pushl (:$l const))))

(define-x8632-vinsn vframe-load (((dest :lisp))
				 ((frame-offset :u16const)
				  (cur-vsp :u16const)))
  (movl (:@ (:apply - (:apply + frame-offset x8632::word-size-in-bytes)) (:%l x8632::ebp)) (:%l dest)))

(define-x8632-vinsn compare-vframe-offset-to-nil (()
                                                  ((frame-offset :u16const)
                                                   (cur-vsp :u16const)))
  (cmpl (:$l (:apply target-nil-value)) (:@ (:apply - (:apply + frame-offset x8632::word-size-in-bytes)) (:%l x8632::ebp))))


(define-x8632-vinsn compare-vframe-offset-to-fixnum (()
                                                     ((frame-offset :stack-offset)
      (cur-vsp :stack-offset)
      (fixval :s32const)))
  (:if (:and (:pred < fixval 128) (:pred >= fixval -128))
    (cmpl (:$b fixval) (:@ (:apply - (:apply + frame-offset x8632::word-size-in-bytes)) (:%l x8632::ebp)))
    (cmpl (:$l fixval) (:@ (:apply - (:apply + frame-offset x8632::word-size-in-bytes)) (:%l x8632::ebp)))))

(define-x8632-vinsn add-constant-to-vframe-offset (()
                                                   ((frame-offset :u16const)
                                                    (constant :s32const)))
  (:if (:and (:pred < constant 128) (:pred >= constant -128))
    (addl (:$b constant) (:@ (:apply - (:apply + frame-offset x8632::word-size-in-bytes)) (:%l x8632::ebp)))
    (addl (:$l constant) (:@ (:apply - (:apply + frame-offset x8632::word-size-in-bytes)) (:%l x8632::ebp)))))

(define-x8632-vinsn compare-value-cell-to-nil (()
                                               ((vcell :lisp)))
  (cmpl (:$l (:apply target-nil-value)) (:@ x8632::value-cell.value (:%l vcell))))



(define-x8632-vinsn (vframe-push :push :node :vsp)
    (()
     ((frame-offset :u16const)
      (cur-vsp :u16const)))
  (pushl (:@ (:apply - (:apply + frame-offset x8632::word-size-in-bytes)) (:%l x8632::ebp))))

(define-x8632-vinsn vframe-store (()
				  ((src :lisp)
				   (frame-offset :u16const)
				   (cur-vsp :u16const)))
  (movl (:%l src) (:@ (:apply - (:apply + frame-offset x8632::word-size-in-bytes)) (:%l x8632::ebp))))


        
(define-x8632-vinsn (popj :lispcontext :pop :vsp :lrRestore :jumpLR)
    (()
     ())
  (leave)
  (ret))

(define-x8632-vinsn (popj-via-jump :lispcontext :pop :vsp :lrRestore :jumpLR)
    (()
     ((lab :label)))
  (jmp lab))

(define-x8632-vinsn (restore-full-lisp-context :lispcontext :pop :vsp )
    (()
     ())
  (leave))

(define-x8632-vinsn compare-to-nil (()
                                    ((arg0 t)))
  (cmpl (:$l (:apply target-nil-value)) (:%l arg0)))

(define-x8632-vinsn compare-to-t (()
				  ((arg0 t)))
  (cmpl (:$l (:apply target-t-value)) (:%l arg0)))

(define-x8632-vinsn ref-constant (((dest :lisp))
                                  ((lab :label)))
  (movl (:@ (:^ lab) (:%l x8632::fn)) (:%l dest)))

(define-x8632-vinsn compare-constant-to-register (()
                                                  ((lab :label)
                                                   (reg :lisp)))
  (cmpl (:@ (:^ lab) (:%l x8632::fn)) (:%l reg)))

(define-x8632-vinsn (vpush-constant :push :node :vsp) (()
                                                       ((lab :label)))
  (pushl (:@ (:^ lab) (:%l x8632::fn))))

(define-x8632-vinsn (jump :jump)
    (()
     ((label :label)))
  (jmp label))

(define-x8632-vinsn (cbranch-true :branch) (()
					    ((label :label)
					     (crbit :u8const)))
  (jcc (:$ub crbit) label))

(define-x8632-vinsn (cbranch-false :branch) (()
					     ((label :label)
					      (crbit :u8const)))
  (jcc (:$ub (:apply logxor 1 crbit)) label))

(define-x8632-vinsn (lri :constant-ref) (((dest :imm))
                                         ((intval :s32const))
                                         ())
  (:if (:pred = intval 0)
   (xorl (:%l dest) (:%l dest))
   (movl (:$l intval) (:%l dest))))

(define-x8632-vinsn (lriu :constant-ref) (((dest :imm))
                                          ((intval :u32const))
                                          ())
  (:if (:pred = intval 0)
    (xorl (:%l dest) (:%l dest))
    (movl (:$l intval) (:%l dest))))

;;; In the following trap/branch-unless vinsns, it might be worth
;;; trying to use byte instructions when the args are known to be
;;; accessible as byte regs.  It also might be possible to
;;; special-case eax/ax/al.

(define-x8632-vinsn trap-unless-bit (()
                                     ((value :lisp)))
  :resume
  (testl (:$l (lognot x8632::fixnumone)) (:%l value))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l value) (:$ub arch::error-object-not-bit))))

;;; note that NIL is just a distinguished CONS.
;;; the tag formerly known as fulltag-nil is now
;;; for tagged return addresses.
(define-x8632-vinsn trap-unless-list (()
				      ((object :lisp))
				      ((tag :u8)))
  :resume
  (movl (:% object) (:% tag))
  (andl (:$b x8632::fulltagmask) (:% tag))
  (cmpl (:$b x8632::fulltag-cons) (:% tag))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad  
  (:anchored-uuo (uuo-error-reg-not-list (:%l object))))

(define-x8632-vinsn trap-unless-cons (()
				      ((object :lisp))
				      ((tag :u8)))
  ;; special check for NIL (which is a distinguished CONS on x8632)
  :resume
  (cmpl (:$l (:apply target-nil-value)) (:%l object))
  (je :bad)
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::fulltagmask) (:%l tag))
  (cmpl (:$b x8632::fulltag-cons) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%l object) (:$ub x8632::fulltag-cons))))

(define-x8632-vinsn set-z-flag-if-consp (()
					 ((object :lisp))
					 ((tag (:u32 #.x8632::imm0))))
  (movl (:%l object) (:%accl tag))
  (andl (:$b x8632::fulltagmask) (:%accl tag))
  (cmpb (:$b x8632::fulltag-cons) (:%accb tag))
  (setne (:%b x8632::ah))
  (cmpl (:$l (:apply target-nil-value)) (:% object))
  (sete (:%b x8632::al))
  (orb (:%b x8632::ah) (:%b x8632::al)))

(define-x8632-vinsn set-z-if-uvector-type (((crf :crf))
                                           ((thing :lisp)
                                            (type :u8const))
                                           ((tag :u8)))
  (movl (:%l thing) (:%l  tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :done)
  (cmpb (:$b type) (:@ x8632::misc-subtag-offset (:%l thing)))
  :done)

(define-x8632-vinsn trap-unless-uvector (()
                                         ((object :lisp))
                                         ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%l object) (:$ub x8632::tag-misc))))

(define-x8632-vinsn trap-unless-character (()
					   ((object :lisp))
					   ((tag :u8)))
  ;; xxx can't be sure that object will be in a byte-accessible register
  :resume
  (movl (:%l object) (:%l tag))
  (cmpb (:$b x8632::subtag-character) (:%b tag))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo(uuo-error-reg-not-tag (:%l object) (:$ub x8632::subtag-character))))

(define-x8632-vinsn trap-unless-fixnum (()
                                        ((object :lisp))
                                        ())
  :resume
  (testl (:$l x8632::tagmask) (:%l object))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-fixnum (:%l object))))

(define-x8632-vinsn set-flags-from-lisptag (()
                                            ((reg :lisp)))
  (testl (:$l x8632::tagmask) (:%l reg)))

(define-x8632-vinsn trap-unless-typecode= (()
					   ((object :lisp)
					    (tagval :u8const))
					   ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  ((:pred = (:apply %hard-regspec-value tag) x8632::eax)
   ;; accumulator
   (andl (:$b x8632::tagmask) (:%accl tag))
   (cmpl (:$b x8632::tag-misc) (:%accl tag)))
  ((:pred > (:apply %hard-regspec-value tag) x8632::eax)
   (andl (:$b x8632::tagmask) (:%l tag))
   (cmpl (:$b x8632::tag-misc) (:%l tag)))
  (jne :have-tag)
  ;; This needs to be a sign-extending mov, since the cmpl below
  ;; will sign-extend the 8-bit constant operand.
  (movsbl (:@ x8632::misc-subtag-offset (:%l object)) (:%l tag))
  :have-tag
  (cmpl (:$b tagval) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%l object) (:$ub tagval))))

(define-x8632-vinsn trap-unless-single-float (()
                                              ((object :lisp))
                                              ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :bad)
  (movsbl (:@ x8632::misc-subtag-offset (:%l object)) (:%l tag))
  (cmpl (:$b x8632::subtag-single-float) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%l object) (:$ub x8632::subtag-single-float))))

(define-x8632-vinsn trap-unless-double-float (()
                                              ((object :lisp))
                                              ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :bad)
  (movsbl (:@ x8632::misc-subtag-offset (:%l object)) (:%l tag))
  (cmpl (:$b x8632::subtag-double-float) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%l object) (:$ub x8632::subtag-double-float))))

(define-x8632-vinsn trap-unless-complex-double-float (()
                                                      ((object :lisp))
                                                      ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :bad)
  (movsbl (:@ x8632::misc-subtag-offset (:%l object)) (:%l tag))
  (cmpl (:$b x8632::subtag-complex-double-float) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%l object) (:$ub x8632::subtag-complex-double-float))))

(define-x8632-vinsn trap-unless-complex-single-float (()
                                                      ((object :lisp))
                                                      ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :bad)
  (movsbl (:@ x8632::misc-subtag-offset (:%l object)) (:%l tag))
  (cmpl (:$b x8632::subtag-complex-single-float) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%l object) (:$ub x8632::subtag-complex-single-float))))

(define-x8632-vinsn trap-unless-macptr (()
                                        ((object :lisp))
                                        ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :have-tag)
  (movsbl (:@ x8632::misc-subtag-offset (:%l object)) (:%l tag))
  :have-tag
  (cmpl (:$b x8632::subtag-macptr) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%l object) (:$ub x8632::subtag-macptr))))

(define-x8632-vinsn check-misc-bound (()
				      ((idx :imm)
				       (v :lisp))
				      ((temp :u32)))
  :resume
  (movl (:@ x8632::misc-header-offset (:%l v)) (:%l temp))
  ((:and (:pred >= (:apply %hard-regspec-value temp) x8632::eax)
	 (:pred <= (:apply %hard-regspec-value temp) x8632::ebx))
   (xorb (:%b temp) (:%b temp))
   (shrl (:$ub (- x8632::num-subtag-bits x8632::fixnumshift)) (:%l temp)))
  ((:pred > (:apply %hard-regspec-value temp) x8632::ebx)
   (shrl (:$ub x8632::num-subtag-bits) (:%l temp))
   (shll (:$ub x8632::fixnumshift) (:%l temp)))
  (rcmpl (:%l idx) (:%l temp))
  (jae :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-vector-bounds (:%l idx) (:%l v))))

(define-x8632-vinsn set-z-if-header-type (((crf :crf))
                                          ((src :lisp)
                                           (type :u8const))
                                          ((flags :u32)))
  (movl (:@ x8632::vectorH.flags (:%q  src)) (:%l flags))
  (shrl (:$ub (+ x8632::fixnumshift 8)) (:%l flags))
  (cmpb (:$b type) (:%b flags)))

(define-x8632-vinsn deref-vector-header (((vector :lisp)
                                          (index :lisp))
                                         ((vector :lisp)
                                          (index :lisp)))
  :again
  (addl (:@ x8632::vectorH.displacement (:%l vector)) (:%l index))
  (btw (:$ub (+ x8632::fixnumshift $arh_disp_bit)) (:@ x8632::vectorH.flags (:%l vector)))
  (movl (:@ x8632::vectorH.data-vector (:%l vector)) (:%l vector))
  (jb :again))

(define-x8632-vinsn check-vector-header-bound (()
                                               ((v :lisp)
                                                (idx :imm)))
  :resume
  (cmpl (:@ x8632::vectorH.physsize (:%l v)) (:%l idx))
  (jae :bad)
    (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-vector-bounds (:%l idx) (:%l v))))


(define-x8632-vinsn %cdr (((dest :lisp))
			  ((src :lisp)))
  (movl (:@ x8632::cons.cdr (:%l src)) (:%l dest)))

(define-x8632-vinsn (%vpush-cdr :push :node :vsp)
    (()
     ((src :lisp)))
  (pushl (:@ x8632::cons.cdr (:%l src))))

(define-x8632-vinsn %car (((dest :lisp))
			  ((src :lisp)))
  (movl (:@ x8632::cons.car (:%l src)) (:%l dest)))

(define-x8632-vinsn (%vpush-car :push :node :vsp)
    (()
     ((src :lisp)))
  (pushl (:@ x8632::cons.car (:%l src))))

(define-x8632-vinsn u32->char (((dest :lisp)
                               (src :u8))
			      ((src :u8))
			      ())
  (shll (:$ub x8632::charcode-shift) (:%l src))
  (leal (:@ x8632::subtag-character (:%l src)) (:%l dest)))

(define-x8632-vinsn (load-nil :constant-ref) (((dest t))
					      ())
  (movl (:$l (:apply target-nil-value)) (:%l dest)))


(define-x8632-vinsn (load-t :constant-ref) (((dest t))
					    ())
  (movl (:$l (:apply target-t-value)) (:%l dest)))

(define-x8632-vinsn extract-tag (((tag :u8))
                                 ((object :lisp)))
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag)))

(define-x8632-vinsn extract-tag-fixnum (((tag :imm))
					((object :lisp)))
  (leal (:@ (:%l object) 4) (:%l tag))
  (andl (:$b (ash x8632::tagmask x8632::fixnumshift)) (:%l tag)))

(define-x8632-vinsn extract-fulltag (((tag :u8))
                                 ((object :lisp)))
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::fulltagmask) (:%l tag)))

(define-x8632-vinsn extract-fulltag-fixnum (((tag :imm))
                                            ((object :lisp)))
  (:if (:pred =
              (:apply %hard-regspec-value tag)
              (:apply %hard-regspec-value object))
    (shll (:$ub x8632::fixnumshift) (:%l object))
    (imull (:$b x8632::fixnumone) (:%l object) (:%l tag)))
  (andl (:$b (ash x8632::fulltagmask x8632::fixnumshift)) (:%l tag)))

(define-x8632-vinsn extract-typecode (((tag :u32))
                                      ((object :lisp)))
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :have-tag)
  (movzbl (:@ x8632::misc-subtag-offset (:%l object)) (:%l tag))
  :have-tag)

(define-x8632-vinsn extract-typecode-fixnum (((tag :imm))
                                             ((object :lisp))
                                             ((temp :u32)))
  (movl (:%l object) (:%l temp))
  (andl (:$b x8632::tagmask) (:%l temp))
  (cmpl (:$b x8632::tag-misc) (:%l temp))
  (jne :have-tag)
  (movzbl (:@ x8632::misc-subtag-offset (:%l object)) (:%l temp))
  :have-tag
  (leal (:@ (:%l temp) 4) (:%l tag)))

(define-x8632-vinsn compare-reg-to-zero (()
                                         ((reg :imm)))
  (testl (:%l reg) (:%l reg)))

;;; life will be sad if reg isn't byte accessible
(define-x8632-vinsn compare-u8-reg-to-zero (()
                                            ((reg :u8)))
  (testb (:%b reg) (:%b reg)))

(define-x8632-vinsn cr-bit->boolean (((dest :lisp))
                                     ((crbit :u8const))
                                     ((temp :u32)))
  (movl (:$l (:apply target-t-value)) (:%l temp))
  (leal (:@ (- x8632::t-offset) (:%l temp)) (:%l dest))
  (cmovccl (:$ub crbit) (:%l temp) (:%l dest)))

(define-x8632-vinsn compare-s32-constant (()
                                          ((val :imm)
                                           (const :s32const)))
  (:if (:or  (:pred < const -128) (:pred > const 127))
    (rcmpl (:%l val) (:$l const))
    (rcmpl (:%l val) (:$b const))))

(define-x8632-vinsn compare-u31-constant (()
                                          ((val :u32)
                                           (const :u32const)))
  ((:pred > const 127)
   (rcmpl (:%l val) (:$l const)))
  ((:not (:pred > const 127))
   (rcmpl (:%l val) (:$b const))))

(define-x8632-vinsn compare-u8-constant (()
                                         ((val :u8)
                                          (const :u8const)))
  (:if (:pred = (:apply %hard-regspec-value val) x8632::eax)
    (rcmpb (:%accb val) (:$b const))
    (:if (:pred <= (:apply %hard-regspec-value val) x8632::ebx)
      (rcmpb (:%b val) (:$b const))
      (rcmpl (:%l val) (:$l const)))))

(define-x8632-vinsn cons (((dest :lisp))
                          ((car :lisp)
                           (cdr :lisp))
			  ((allocptr (:lisp #.x8632::allocptr))))
  (subl (:$b (- x8632::cons.size x8632::fulltag-cons)) (:@ (:%seg :rcontext) x8632::tcr.save-allocptr))
  (movl (:@ (:%seg :rcontext) x8632::tcr.save-allocptr) (:%l x8632::allocptr))
  (rcmpl (:%l x8632::allocptr) (:@ (:%seg :rcontext) x8632::tcr.save-allocbase))
  (ja :no-trap)
  (uuo-alloc)
  :no-trap
  (andb (:$b (lognot x8632::fulltagmask)) (:@ (:%seg :rcontext) x8632::tcr.save-allocptr))
  (movl (:%l car) (:@ x8632::cons.car (:%l x8632::allocptr)))
  (movl (:%l cdr) (:@ x8632::cons.cdr (:%l x8632::allocptr)))
  (movl (:%l x8632::allocptr) (:%l dest)))

(define-x8632-vinsn unbox-u8 (((dest :u8))
			      ((src :lisp)))
  :resume
  (movl (:$l (lognot (ash #xff x8632::fixnumshift))) (:%l dest))
  (andl (:% src) (:% dest))
  (jne :bad)
  (movl (:%l src) (:%l dest))
  (shrl (:$ub x8632::fixnumshift) (:%l dest))

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l src) (:$ub arch::error-object-not-unsigned-byte-8))))

(define-x8632-vinsn %unbox-u8 (((dest :u8))
			      ((src :lisp)))
  (movl (:%l src) (:%l dest))
  (shrl (:$ub x8632::fixnumshift) (:%l dest))
  (andl (:$l #xff) (:%l dest)))

(define-x8632-vinsn unbox-s8 (((dest :s8))
			      ((src :lisp)))
  :resume
  (movl (:%l src) (:%l dest))
  (shll (:$ub (- x8632::nbits-in-word (+ 8 x8632::fixnumshift))) (:%l dest))
  (sarl (:$ub (- x8632::nbits-in-word (+ 8 x8632::fixnumshift))) (:%l dest))
  (cmpl (:%l src) (:%l dest))
  (jne :bad)
  (testl (:$l x8632::fixnummask) (:%l dest))
  (jne :bad)
  (sarl (:$ub x8632::fixnumshift) (:%l dest))

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l src) (:$ub arch::error-object-not-signed-byte-8))))

(define-x8632-vinsn %unbox-s8 (((dest :s8))
                               ((src :lisp)))
  (movl (:%l src) (:%l dest))
  (sarl (:$ub x8632::fixnumshift) (:%l dest)))

(define-x8632-vinsn unbox-u16 (((dest :u16))
			      ((src :lisp)))
  :resume
  (testl (:$l (lognot (ash #xffff x8632::fixnumshift))) (:% src))
  (movl (:%l src) (:%l dest))
  (jne :bad)
  (shrl (:$ub x8632::fixnumshift) (:%l dest))
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l src) (:$ub arch::error-object-not-unsigned-byte-16))))

(define-x8632-vinsn %unbox-u16 (((dest :u16))
			      ((src :lisp)))
  (movl (:%l src) (:%l dest))
  (shrl (:$ub x8632::fixnumshift) (:%l dest)))

(define-x8632-vinsn unbox-s16 (((dest :s16))
			      ((src :lisp)))
  :resume
  (movl (:%l src) (:%l dest))
  (shll (:$ub (- x8632::nbits-in-word (+ 16 x8632::fixnumshift))) (:%l dest))
  (sarl (:$ub (- x8632::nbits-in-word (+ 16 x8632::fixnumshift))) (:%l dest))
  (cmpl (:%l src) (:%l dest))
  (jne :bad)
  (testl (:$l x8632::fixnummask) (:%l dest))
  (jne :bad)
  (sarl (:$ub x8632::fixnumshift) (:%l dest))

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l src) (:$ub arch::error-object-not-signed-byte-16))))

(define-x8632-vinsn %unbox-s16 (((dest :s16))
                                ((src :lisp)))
  (movl (:%l src) (:%l dest))
  (sarl (:$ub x8632::fixnumshift) (:%l dest)))

;;; An object is of type (UNSIGNED-BYTE 32) iff
;;;  a) it's of type (UNSIGNED-BYTE 30) (e.g., an unsigned fixnum)
;;;  b) it's a bignum of length 1 and the 0'th digit is positive
;;;  c) it's a bignum of length 2 and the sign-digit is 0.
(define-x8632-vinsn unbox-u32 (((dest :u32))
                               ((src :lisp)))
  :resume
  (movl (:$l (lognot (ash x8632::target-most-positive-fixnum x8632::fixnumshift))) (:%l dest))
  (testl (:%l dest) (:%l src))
  (movl (:%l src) (:%l dest))
  (jnz :maybe-bignum)
  (sarl (:$ub x8632::fixnumshift) (:%l dest))
  (jmp :done)
  :maybe-bignum
  (andl (:$b x8632::tagmask) (:%l dest))
  (cmpl (:$b x8632::tag-misc) (:%l dest))
  (jne :bad)
  (movl (:@ x8632::misc-header-offset (:%l src)) (:%l dest))
  (cmpl (:$l x8632::two-digit-bignum-header) (:%l dest))
  (je :two)
  (cmpl (:$l x8632::one-digit-bignum-header) (:%l dest))
  (jne :bad)
  (movl (:@ x8632::misc-data-offset (:%l src)) (:%l dest))
  (testl (:%l dest) (:%l dest))
  (js :bad)
  (jmp :done)
  :two
  (movl (:@ (+ 4 x8632::misc-data-offset) (:%l src)) (:%l dest))
  (testl (:%l dest) (:%l dest))
  (jne :bad)
  (movl (:@ x8632::misc-data-offset (:%l src)) (:%l dest))
  :done
  
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l src) (:$ub arch::error-object-not-unsigned-byte-32))))

;;; an object is of type (SIGNED-BYTE 32) iff
;;; a) it's a fixnum
;;; b) it's a bignum with exactly one digit.
(define-x8632-vinsn unbox-s32 (((dest :s32))
                               ((src :lisp)))
  :resume
  (movl (:%l src) (:%l dest))
  (sarl (:$ub x8632::fixnumshift) (:%l dest))
  ;; Was it a fixnum ?
  (testl (:$l x8632::fixnummask) (:%l src))
  (je :done)
  ;; May be a 1-digit bignum
  (movl (:%l src) (:%l dest))
  (andl (:$b x8632::tagmask) (:%l dest))
  (cmpl (:$b x8632::tag-misc) (:%l dest))
  (jne :bad)
  (cmpl (:$l x8632::one-digit-bignum-header) (:@ x8632::misc-header-offset (:%l src)))
  (movl (:@ x8632::misc-data-offset (:%l src)) (:%l dest))
  (jne :bad)
  :done

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l src) (:$ub arch::error-object-not-signed-byte-32))))

(define-x8632-vinsn sign-extend-s8 (((dest :s32))
                                    ((src :s8)))
  (movsbl (:%b src) (:%l dest)))

(define-x8632-vinsn sign-extend-s16 (((dest :s32))
                                     ((src :s16)))
  (movswl (:%w src) (:%l dest)))

(define-x8632-vinsn zero-extend-u8 (((dest :s32))
                                    ((src :u8)))
  (:if (:pred < (:apply %hard-regspec-value src) 4)
   (movzbl (:%b src) (:%l dest))
  (:progn
    (movl (:%l src) (:%l dest))
    (movzbl (:%b dest) (:%l dest)))))
  

(define-x8632-vinsn zero-extend-u16 (((dest :s32))
                                     ((src :u16)))
  (movzwl (:%w src) (:%l dest)))

(define-x8632-vinsn (jump-subprim :jumpLR) (()
					    ((spno :s32const)))
  (jmp (:@ spno)))

;;; Call a subprimitive using a tail-aligned CALL instruction.
(define-x8632-vinsn (call-subprim :call)  (()
                                           ((spno :s32const))
                                           ((entry (:label 1))))
  (:talign x8632::fulltag-tra)
  (call (:@ spno))
  (movl (:$self 0) (:% x8632::fn)))

(define-x8632-vinsn (call-subprim-no-return)  (()
                                           ((spno :s32const))
                                           ((entry (:label 1))))
  (:talign x8632::fulltag-tra)
  (call (:@ spno))
  (movl (:$self 0) (:% x8632::fn)))

(define-x8632-vinsn %ilognot (((dest :imm)
                               (src :imm))
                              ((src :imm)))
  (xorl (:$b (- x8632::fixnumone)) (:%l dest)))

(define-x8632-vinsn %logand-c (((dest t)
                                (val t))
                               ((val t)
                                (const :s32const)))
  (:if (:and (:pred >= const -128) (:pred <= const 127))
    (andl (:$b const) (:%l val))
    (andl (:$l const) (:%l val))))

(define-x8632-vinsn %logior-c (((dest t)
                                (val t))
                               ((val t)
                                (const :s32const)))
  (:if (:and (:pred >= const -128) (:pred <= const 127))
    (orl (:$b const) (:%l val))
    (orl (:$l const) (:%l val))))

(define-x8632-vinsn %logxor-c (((dest t)
                                (val t))
                               ((val t)
                                (const :s32const)))
  (:if (:and (:pred >= const -128) (:pred <= const 127))
    (xorl (:$b const) (:%l val))
    (xorl (:$l const) (:%l val))))

(define-x8632-vinsn character->fixnum (((dest :lisp))
				       ((src :lisp))
				       ())
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movl (:%l src) (:%l dest)))

  (:if (:pred <= (:apply %hard-regspec-value dest) x8632::ebx)
    (xorb (:%b dest) (:%b dest))
    (andl (:$l -256) (:%l dest)))
  (shrl (:$ub (- x8632::charcode-shift x8632::fixnumshift)) (:%l dest)))

(define-x8632-vinsn compare (()
                             ((x t)
                              (y t)))
  (rcmpl (:%l x) (:%l y)))

(define-x8632-vinsn negate-fixnum (((val :lisp))
                                   ((val :imm)))
  (negl (:% val)))



(define-x8632-vinsn handle-fixnum-overflow-inline
    (()
     ((val :lisp))
     ((imm (:u32 #.x8632::imm0))
      (freeptr (:lisp #.x8632::allocptr))))
  (movl (:%l val) (:%l imm))
  (sarl (:$ub x8632::fixnumshift) (:%l imm))
  (xorl (:$l #xc0000000) (:%l imm))
  ;; stash bignum digit
  (movd (:%l imm) (:%mmx x8632::mm1))
  ;; set header
  (movl (:$l x8632::one-digit-bignum-header) (:%l imm))
  (movd (:%l imm) (:%mmx x8632::mm0))
  ;; need 8 bytes of aligned memory for 1 digit bignum
  (movl (:$l (- 8 x8632::fulltag-misc)) (:%l imm))
  (subl (:%l imm) (:@ (:%seg :rcontext) x8632::tcr.save-allocptr))
  (movl (:@ (:%seg :rcontext) x8632::tcr.save-allocptr) (:%l freeptr))
  (rcmpl (:%l freeptr) (:@ (:%seg :rcontext) x8632::tcr.save-allocbase))
  (ja :no-trap)
  (uuo-alloc)
  :no-trap
  (movd (:%mmx x8632::mm0) (:@ x8632::misc-header-offset (:%l freeptr)))
  (andb (:$b (lognot x8632::fulltagmask)) (:@ (:%seg :rcontext) x8632::tcr.save-allocptr))
  ((:not (:pred = freeptr
		(:apply %hard-regspec-value val)))
   (movl (:%l freeptr) (:%l val)))
  (movd (:%mmx x8632::mm1) (:@ x8632::misc-data-offset (:%l val))))

  
(define-x8632-vinsn set-bigits-after-fixnum-overflow (()
                                                      ((bignum :lisp)))
  (movd (:%mmx x8632::mm1) (:@ x8632::misc-data-offset (:%l bignum))))  


(define-x8632-vinsn %set-z-flag-if-s32-fits-in-fixnum (((dest :imm))
						       ((src :s32))
						       ((temp :s32)))
  (movl (:%l src) (:%l temp))
  (shll (:$ub x8632::fixnumshift) (:%l temp))
  (movl (:%l temp) (:%l dest))          ; tagged as a fixnum
  (sarl (:$ub x8632::fixnumshift) (:%l temp))
  (cmpl (:%l src) (:%l temp)))

(define-x8632-vinsn %set-z-flag-if-u32-fits-in-fixnum (((dest :imm))
                                                       ((src :u32))
                                                       ((temp :u32)))
  (movl (:%l src) (:%l temp))
  (shll (:$ub (1+ x8632::fixnumshift)) (:%l temp))
  (movl (:%l temp) (:%l dest))          ; tagged as an even fixnum
  (shrl (:$ub (1+ x8632::fixnumshift)) (:%l temp))
  (shrl (:%l dest))
  (cmpl (:%l src) (:%l temp))
  :done)

;;; setup-bignum-alloc-for-s32-overflow
;;; setup-bignum-alloc-for-u32-overflow

(define-x8632-vinsn setup-uvector-allocation (()
					      ((header :imm)))
  (movd (:%l header) (:%mmx x8632::mm0)))

;;; The code that runs in response to the uuo-alloc
;;; expects a header in mm0, and a size in imm0.
;;; mm0 is an implicit arg (it contains the uvector header)
;;; size is actually an arg, not a temporary,
;;; but it appears that there's isn't a way to enforce
;;; register usage on vinsn args.
(define-x8632-vinsn %allocate-uvector (((dest :lisp))
				       ()
				       ((size (:u32 #.x8632::imm0))
					(freeptr (:lisp #.x8632::allocptr))))
  (subl (:%l size) (:@ (:%seg :rcontext) x8632::tcr.save-allocptr))
  (movl (:@ (:%seg :rcontext) x8632::tcr.save-allocptr) (:%l freeptr))
  (rcmpl (:%l freeptr) (:@ (:%seg :rcontext) x8632::tcr.save-allocbase))
  (ja :no-trap)
  (uuo-alloc)
  :no-trap
  (movd (:%mmx x8632::mm0) (:@ x8632::misc-header-offset (:%l freeptr)))
  (andb (:$b (lognot x8632::fulltagmask)) (:@ (:%seg :rcontext) x8632::tcr.save-allocptr))
  ((:not (:pred = freeptr
		(:apply %hard-regspec-value dest)))
   (movl (:%l freeptr) (:%l dest))))

(define-x8632-vinsn box-fixnum (((dest :imm))
                                ((src :s32)))
  ;;(imull (:$b x8632::fixnumone) (:%l src) (:%l dest))
  (leal (:@ (:%l src) x8632::fixnumone) (:%l dest)))



(define-x8632-vinsn (return-or-fix-overflow :jumpLR) (()
                                                      ())
  (jo :fix)
  (:byte #xf3) (ret)
  :fix
  (jmp (:@ .SPfix-overflow)))

(define-x8632-vinsn add-constant (((dest :imm))
                                  ((dest :imm)
                                   (const :s32const)))
  (:if (:and (:pred >= const -128) (:pred <= const 127))
    (addl (:$b const) (:%l dest))
    (addl (:$l const) (:%l dest))))

(define-x8632-vinsn add-constant3 (((dest :imm))
                                   ((src :imm)
                                    (const :s32const)))
  (:if (:pred = (:apply %hard-regspec-value dest)
          (:apply %hard-regspec-value src))
    (:if (:and (:pred >= const -128) (:pred <= const 127))
      (addl (:$b const) (:%l dest))
      (addl (:$l const) (:%l dest)))
    (leal (:@ const (:%l src)) (:%l dest))))

(define-x8632-vinsn fixnum-add2  (((dest :imm))
                                  ((dest :imm)
                                   (other :imm)))
  (addl (:%l other) (:%l dest)))

(define-x8632-vinsn fixnum-sub2  (((dest :imm))
                                  ((x :imm)
                                   (y :imm))
                                  ((temp :imm)))
  ((:pred = (:apply %hard-regspec-value x) (:apply %hard-regspec-value dest))
   (subl (:%l y) (:%l dest)))
  ((:not (:pred = (:apply %hard-regspec-value x) (:apply %hard-regspec-value dest)))
   ((:pred = (:apply %hard-regspec-value y) (:apply %hard-regspec-value dest)) 
    (movl (:%l x) (:%l temp))
    (subl (:%l y) (:%l temp))
    (movl (:%l temp) (:%l dest)))
   ((:not (:pred = (:apply %hard-regspec-value y) (:apply %hard-regspec-value dest)))
    (movl (:%l x) (:%l dest))
    (subl (:%l y) (:%l dest)))))

(define-x8632-vinsn fixnum-add3 (((dest :imm))
                                 ((x :imm)
                                  (y :imm)))
  
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (addl (:%l y) (:%l dest)))
  ((:not (:pred =
                (:apply %hard-regspec-value x)
                (:apply %hard-regspec-value dest)))
   ((:pred =
           (:apply %hard-regspec-value y)
           (:apply %hard-regspec-value dest))
    (addl (:%l x) (:%l dest)))
   ((:not (:pred =
                 (:apply %hard-regspec-value y)
                 (:apply %hard-regspec-value dest)))
    (leal (:@ (:%l x) (:%l y)) (:%l dest)))))

(define-x8632-vinsn copy-gpr (((dest t))
			      ((src t)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movl (:%l src) (:%l dest))))

(define-x8632-vinsn (vpop-register :pop :node :vsp)
    (((dest :lisp))
     ())
  (popl (:%l dest)))

(define-x8632-vinsn (vpop-gvector-element :pop :node :vsp)
    (()
     ((vector :lisp)
      (idx :u32const)))
  (popl (:@ (:apply + x8632::misc-data-offset (:apply ash idx x8632::word-shift))
            (:%l vector))))

(define-x8632-vinsn (push-argregs :push :node :vsp) (()
						     ())
  (rcmpl (:%l x8632::nargs) (:$b (* 1 x8632::node-size)))
  (jb :done)
  (je :one)
  (pushl (:%l x8632::arg_y))
  :one
  (pushl (:%l x8632::arg_z))
  :done)

(define-x8632-vinsn (push-max-argregs :push :node :vsp) (()
                                                         ((max :u32const)))
  ((:pred >= max 2)
   (rcmpl (:%l x8632::nargs) (:$b (* 1 x8632::node-size)))
   (jb :done)
   (je :one)
   (pushl (:%l x8632::arg_y))
   :one
   (pushl (:%l x8632::arg_z))
   :done)
  ((:pred = max 1)
   (testl (:%l x8632::nargs) (:%l x8632::nargs))
   (je :done)
   (pushl (:%l x8632::arg_z))
   :done))

(define-x8632-vinsn (call-label :call) (()
					((label :label)))
  (:talign 5)
  (call label)
  (movl (:$self 0) (:%l x8632::fn)))

(define-x8632-vinsn double-float-compare (()
					  ((arg0 :double-float)
					   (arg1 :double-float)))
  (comisd (:%xmm arg1) (:%xmm arg0)))

(define-x8632-vinsn single-float-compare (()
					  ((arg0 :single-float)
					   (arg1 :single-float)))
  (comiss (:%xmm arg1) (:%xmm arg0)))

(define-x8632-vinsn double-float+-2 (((result :double-float))
				     ((x :double-float)
				      (y :double-float)))
  ((:pred =
          (:apply %hard-regspec-value result)
          (:apply %hard-regspec-value x))
   (addsd (:%xmm y) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:pred =
                (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value y)))
   (addsd (:%xmm x) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value y))))
   (movsd (:%xmm x) (:%xmm result))
   (addsd (:%xmm y) (:%xmm result))))

;;; Caller guarantees (not (eq y result))
(define-x8632-vinsn double-float--2 (((result :double-float))
				     ((x :double-float)
				      (y :double-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movsd (:%xmm x) (:%xmm result)))
  (subsd (:%xmm y) (:%xmm result)))

(define-x8632-vinsn double-float*-2 (((result :double-float))
				     ((x :double-float)
                                      (y :double-float)))
  ((:pred =
          (:apply %hard-regspec-value result)
          (:apply %hard-regspec-value x))
   (mulsd (:%xmm y) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:pred =
                (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value y)))
   (mulsd (:%xmm x) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value y))))
   (movsd (:%xmm x) (:%xmm result))
   (mulsd (:%xmm y) (:%xmm result))))

;;; Caller guarantees (not (eq y result))
(define-x8632-vinsn double-float/-2 (((result :double-float))
				     ((x :double-float)
				      (y :double-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movsd (:%xmm x) (:%xmm result)))
  (divsd (:%xmm y) (:%xmm result)))


(define-x8632-vinsn complex-double-float+-2 (((result :complex-double-float))
                                             ((x :complex-double-float)
                                              (y :complex-double-float)))
  ((:pred =
          (:apply %hard-regspec-value result)
          (:apply %hard-regspec-value x))
   (addpd (:%xmm y) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:pred =
                (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value y)))
   (addpd (:%xmm x) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value y))))
   (movapd (:%xmm x) (:%xmm result))
   (addpd (:%xmm y) (:%xmm result))))

;;; Caller guarantees (not (eq y result))
(define-x8632-vinsn complex-double-float--2 (((result :complex-double-float))
				     ((x :complex-double-float)
				      (y :complex-double-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movapd (:%xmm x) (:%xmm result)))
  (subpd (:%xmm y) (:%xmm result)))

(define-x8632-vinsn complex-double-float*-2 (((result :complex-double-float))
                                             ((x :complex-double-float)
                                              (y :complex-double-float))
                                             ((b :double-float)
                                              (ix :double-float)
                                              (iy :double-float)))
  (movapd (:%xmm x) (:%xmm ix))
  (shufpd (:$ub 1) (:%xmm x8632::fpzero) (:%xmm ix))
  (movapd (:%xmm y) (:%xmm iy))
  (shufpd (:$ub 1) (:%xmm x8632::fpzero) (:%xmm iy))
  (movsd (:%xmm y) (:%xmm result))
  (mulsd (:%xmm x) (:%xmm result))
  (movsd (:%xmm iy) (:%xmm b))
  (mulsd (:%xmm ix) (:%xmm b))
  (subsd (:%xmm b) (:%xmm result))
  (mulsd (:%xmm x) (:%xmm iy))
  (mulsd (:%xmm y) (:%xmm ix))
  (addsd (:%xmm ix) (:%xmm iy))
  (shufpd (:$ub 0) (:%xmm iy) (:%xmm result)))

(define-x8632-vinsn single-float+-2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float)))
  ((:pred =
          (:apply %hard-regspec-value result)
          (:apply %hard-regspec-value x))
   (addss (:%xmm y) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:pred =
                (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value y)))
   (addss (:%xmm x) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value y))))
   (movss (:%xmm x) (:%xmm result))
   (addss (:%xmm y) (:%xmm result))))

;;; Caller guarantees (not (eq y result))
(define-x8632-vinsn single-float--2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movss (:%xmm x) (:%xmm result)))
  (subss (:%xmm y) (:%xmm result)))

(define-x8632-vinsn single-float*-2 (((result :single-float))
				     ((x :single-float)
                                      (y :single-float)))
    ((:pred =
          (:apply %hard-regspec-value result)
          (:apply %hard-regspec-value x))
   (mulss (:%xmm y) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:pred =
                (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value y)))
   (mulss (:%xmm x) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value y))))
   (movss (:%xmm x) (:%xmm result))
   (mulss (:%xmm y) (:%xmm result))))

;;; Caller guarantees (not (eq y result))
(define-x8632-vinsn single-float/-2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movss (:%xmm x) (:%xmm result)))
  (divss (:%xmm y) (:%xmm result)))

(define-x8632-vinsn complex-single-float+-2 (((result :complex-single-float))
                                             ((x :complex-single-float)
                                              (y :complex-single-float)))
  ((:pred =
          (:apply %hard-regspec-value result)
          (:apply %hard-regspec-value x))
   (addps (:%xmm y) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:pred =
                (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value y)))
   (addps (:%xmm x) (:%xmm result)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value x)))
         (:not (:pred =
                      (:apply %hard-regspec-value result)
                      (:apply %hard-regspec-value y))))
   (movq (:%xmm x) (:%xmm result))
   (addps (:%xmm y) (:%xmm result))))

;;; Caller guarantees (not (eq y result))
(define-x8632-vinsn complex-single-float--2 (((result :complex-single-float))
				     ((x :complex-single-float)
				      (y :complex-single-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movq (:%xmm x) (:%xmm result)))
  (subps (:%xmm y) (:%xmm result)))

(define-x8632-vinsn complex-single-float*-2 (((result :complex-single-float))
                                             ((x :complex-single-float)
                                              (y :complex-single-float))
                                             ((b :single-float)
                                              (ix :single-float)
                                              (iy :single-float)))
  (movq (:%xmm x) (:%xmm ix))
  (psrlq (:$ub 32) (:%xmm ix))
  (movq (:%xmm y) (:%xmm iy))
  (psrlq (:$ub 32)  (:%xmm iy))
  (movss(:%xmm y) (:%xmm result))
  (mulss (:%xmm x) (:%xmm result))
  (movss (:%xmm iy) (:%xmm b))
  (mulss (:%xmm ix) (:%xmm b))
  (subss (:%xmm b) (:%xmm result))
  (mulss (:%xmm x) (:%xmm iy))
  (mulss (:%xmm y) (:%xmm ix))
  (addss (:%xmm ix) (:%xmm iy))
  (unpcklps (:%xmm iy) (:%xmm result)))

(define-x8632-vinsn get-single (((result :single-float))
                                ((source :lisp)))
  (movss (:@ x8632::single-float.value (:%l source)) (:%xmm result)))

(define-x8632-vinsn get-double (((result :double-float))
                                ((source :lisp)))
  (movsd (:@ x8632::double-float.value (:%l source)) (:%xmm result)))

(define-x8632-vinsn get-complex-double-float (((result :complex-double-float))
                                              ((source :lisp)))
  (movdqu (:@  x8632::complex-double-float.realpart (:%l source)) (:%xmm result)))

(define-x8632-vinsn get-complex-single-float (((result :complex-single-float))
                                              ((source :lisp)))
  (movq (:@  x8632::complex-single-float.realpart (:%l source)) (:%xmm result)))

;;; Extract a double-float value, typechecking in the process.
;;; IWBNI we could simply call the "trap-unless-typecode=" vinsn here,
;;; instead of replicating it ..
(define-x8632-vinsn get-double? (((target :double-float))
				 ((source :lisp))
				 ((tag :u8)))
  :resume
  (movl (:%l source) (:%l tag))
  ((:pred = (:apply %hard-regspec-value tag) x8632::eax)
   (andl (:$b x8632::tagmask) (:%accl tag))
   (cmpl (:$b x8632::tag-misc) (:%accl tag)))
  ((:pred > (:apply %hard-regspec-value tag) x8632::eax)
   (andl (:$b x8632::tagmask) (:%l tag))
   (cmpl (:$b x8632::tag-misc) (:%l tag)))
  (jne :have-tag)
  (movsbl (:@ x8632::misc-subtag-offset (:%l source)) (:%l tag))
  :have-tag
  (cmpl (:$b x8632::subtag-double-float) (:%l tag))
  (jne :bad)
  (movsd (:@  x8632::double-float.value (:%l source)) (:%xmm target))

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%l source) (:$ub x8632::subtag-double-float))))

(define-x8632-vinsn copy-double-float (((dest :double-float))
                                       ((src :double-float)))
  (movsd (:%xmm src) (:%xmm dest)))

(define-x8632-vinsn copy-single-float (((dest :single-float))
                                       ((src :single-float)))
  (movss (:%xmm src) (:%xmm dest)))

(define-x8632-vinsn copy-complex-single-float (((dest :complex-single-float))
                                               ((src :complex-single-float)))
  (movq (:%xmm src) (:%xmm dest)))
 
(define-x8632-vinsn copy-complex-double-float (((dest :complex-double-float))
                                               ((src :complex-double-float)))
  (movapd (:%xmm src) (:%xmm dest)))

(define-x8632-vinsn copy-single-to-double (((dest :double-float))
                                           ((src :single-float)))
  (cvtss2sd (:%xmm src) (:%xmm dest)))

(define-x8632-vinsn copy-double-to-single (((dest :single-float))
                                           ((src :double-float)))
  (cvtsd2ss (:%xmm src) (:%xmm dest)))

;;; these two clobber unboxed0, unboxed1 in tcr
;;; (There's no way to move a value from the x87 stack to an xmm register,
;;; so we have to go through memory.)
(define-x8632-vinsn fp-stack-to-single (((dest :single-float))
					())
  (fstps (:@ (:%seg :rcontext) x8632::tcr.unboxed0))
  (movss (:@ (:%seg :rcontext) x8632::tcr.unboxed0) (:%xmm dest)))

(define-x8632-vinsn fp-stack-to-double (((dest :double-float))
					())
  (fstpl (:@ (:%seg :rcontext) x8632::tcr.unboxed0))
  (movsd (:@ (:%seg :rcontext) x8632::tcr.unboxed0) (:%xmm dest)))

(define-x8632-vinsn fitvals (()
                             ((n :u16const))
                             ((imm :u32)))
  ((:pred = n 0)
   (xorl (:%l imm) (:%l imm)))
  ((:not (:pred = n 0))
   (movl (:$l (:apply ash n x8632::fixnumshift)) (:%l imm)))
  (subl (:%l x8632::nargs) (:%l imm))
  (jae :push-more)
  (subl (:%l imm) (:%l x8632::esp))
  (jmp :done)
  :push-loop
  (pushl (:$l (:apply target-nil-value)))
  (addl (:$b x8632::node-size) (:%l x8632::nargs))
  (subl (:$b x8632::node-size) (:%l imm))
  :push-more
  (jne :push-loop)
  :done)

(define-x8632-vinsn (nvalret :jumpLR) (()
                                       ())
  (jmp (:@ .SPnvalret)))

(define-x8632-vinsn lisp-word-ref (((dest t))
				   ((base t)
				    (offset t)))
  (movl (:@ (:%l base) (:%l offset)) (:%l  dest)))

(define-x8632-vinsn lisp-word-ref-c (((dest t))
				     ((base t)
				      (offset :s32const)))
  ((:pred = offset 0)
   (movl (:@ (:%l base)) (:%l dest)))
  ((:not (:pred = offset 0))
   (movl (:@ offset (:%l base)) (:%l dest))))

;; start-mv-call

(define-x8632-vinsn (vpush-label :push :node :vsp) (()
						    ((label :label))
						    ((temp :lisp)))
  (leal (:@ (:^ label) (:%l x8632::fn)) (:%l temp))
  (pushl (:%l temp)))

(define-x8632-vinsn (emit-aligned-label :align) (()
                                                ((label :label)))
  ;; We don't care about label.
  ;; We just want the label following this stuff to be tra-tagged.
  (:align 3)
  (nop) (nop) (nop) (nop) (nop))

;; pass-multiple-values-symbol
;;; %ra0 is pointing into %fn, so no need to copy %fn here.
(define-x8632-vinsn pass-multiple-values-symbol (()
                                                 ())
  (pushl (:@ (:apply + (:apply target-nil-value) (x8632::%kernel-global 'x86::ret1valaddr)))) 
  (jmp (:@ x8632::symbol.fcell (:% x8632::fname))))

(define-x8632-vinsn (xpass-multiple-values-symbol :call  :extended-call :jumplr) (()
                                                                                  ((lab :label)))
  (pushl (:@ (:apply + (:apply target-nil-value) (x8632::%kernel-global 'x86::ret1valaddr)))) 
  (jmp (:@ x8632::symbol.fcell (:% x8632::fname))))

;;; It'd be good to have a variant that deals with a known function
;;; as well as this. 
(define-x8632-vinsn pass-multiple-values (()
                                          ()
                                          ((tag :u8)))
  :resume
  (movl (:%l x8632::temp0) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :bad)
  (movsbl (:@ x8632::misc-subtag-offset (:%l x8632::temp0)) (:%l tag))
  (cmpl (:$b x8632::subtag-function) (:%l tag))
  (cmovel (:%l x8632::temp0) (:%l x8632::fn))
  (je :go)
  (cmpl (:$b x8632::subtag-symbol) (:%l tag))
  (cmovel (:@ x8632::symbol.fcell (:%l x8632::fname)) (:%l x8632::fn))
  (jne :bad)
  :go
  (pushl (:@ (:apply + (:apply target-nil-value) (x8632::%kernel-global 'x86::ret1valaddr))))
  (jmp (:%l x8632::fn))
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-not-callable))
)

(define-x8632-vinsn (xpass-multiple-values  :call  :extended-call :jumplr) (()
                                          ((lab :label))
                                          ((tag :u8)))
  :resume
  (movl (:%l x8632::temp0) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :bad)
  (movsbl (:@ x8632::misc-subtag-offset (:%l x8632::temp0)) (:%l tag))
  (cmpl (:$b x8632::subtag-function) (:%l tag))
  (cmovel (:%l x8632::temp0) (:%l x8632::fn))
  (je :go)
  (cmpl (:$b x8632::subtag-symbol) (:%l tag))
  (cmovel (:@ x8632::symbol.fcell (:%l x8632::fname)) (:%l x8632::fn))
  (jne :bad)
  :go
  (pushl (:@ (:apply + (:apply target-nil-value) (x8632::%kernel-global 'x86::ret1valaddr))))
  (jmp (:%l x8632::fn))
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-not-callable))
)
(define-x8632-vinsn (pass-multiple-values-known-function :jumplr) (((fnreg :lisp))
                                                                    ())
  (pushl (:@ (:apply + (:apply target-nil-value) (x8632::%kernel-global 'x86::ret1valaddr)))) 
  (jmp (:%l fnreg)))

(define-x8632-vinsn (xpass-multiple-values-known-function ) (() ((lab :label) (fnreg :lisp)))
  (pushl (:@ (:apply + (:apply target-nil-value) (x8632::%kernel-global 'x86::ret1valaddr)))) 
  (jmp (:%l fnreg)))

(define-x8632-vinsn reserve-outgoing-frame (()
                                            ())
  (pushl (:$b x8632::reserved-frame-marker))
  (pushl (:$b x8632::reserved-frame-marker)))

;; implicit temp0 arg
(define-x8632-vinsn (call-known-function :call) (()
						 ()
                                                 ())
  (:talign 5)
  (call (:%l x8632::temp0))
  (movl (:$self 0) (:%l x8632::fn)))

(define-x8632-vinsn (jump-known-function :jumplr) (()
                                                   ())
  (jmp (:%l x8632::temp0)))

(define-x8632-vinsn (list :call) (()
                                  ()
				  ((temp (:lisp #.x8632::temp0))))
  (leal (:@ (:^ :back) (:%l x8632::fn)) (:%l x8632::temp0))
  (:talign 5)
  (jmp (:@ .SPconslist))
  :back
  (movl (:$self 0) (:%l x8632::fn)))

(define-x8632-vinsn make-fixed-stack-gvector (((dest :lisp))
                                              ((aligned-size :u32const)
                                               (header :s32const))
                                              ((tempa :imm)
                                               (tempb :imm)))
  (:if (:and (:pred >= (:apply + aligned-size x8632::dnode-size) -128)
             (:pred <= (:apply + aligned-size x8632::dnode-size) 127))
   (subl (:$b (:apply + aligned-size x8632::dnode-size))
         (:@ (:%seg :rcontext) x8632::tcr.next-tsp))
   (subl (:$l (:apply + aligned-size x8632::dnode-size))
         (:@ (:%seg :rcontext) x8632::tcr.next-tsp)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.save-tsp) (:%l tempb))
  (movl (:@ (:%seg :rcontext) x8632::tcr.next-tsp) (:%l tempa))
  (movd (:%l tempb) (:%mmx x8632::stack-temp))
  :loop
  (movsd (:%xmm x8632::fpzero) (:@ -8 (:%l tempb)))
  (subl (:$b x8632::dnode-size) (:%l tempb))
  (cmpl (:%l tempa) (:%l tempb))
  (jnz :loop)
  (movd (:%mmx x8632::stack-temp) (:@ (:%l tempa)))
  (movl (:%l tempa) (:@ (:%seg :rcontext) x8632::tcr.save-tsp))
  (movl (:$l header) (:@ x8632::dnode-size (:%l tempa)))
  (leal (:@ (+ x8632::dnode-size x8632::fulltag-misc) (:%l tempa)) (:%l dest)))




(define-x8632-vinsn make-tsp-vcell (((dest :lisp))
				    ((closed :lisp))
				    ((temp :imm)))
  (subl (:$b (+ x8632::value-cell.size x8632::dnode-size)) (:@ (:%seg :rcontext) x8632::tcr.next-tsp))
  (movd (:@ (:%seg :rcontext) x8632::tcr.save-tsp) (:%mmx x8632::stack-temp))
  (movl (:@ (:%seg :rcontext) x8632::tcr.next-tsp) (:%l temp))
  (movsd (:%xmm x8632::fpzero) (:@ (:%l temp)))
  (movsd (:%xmm x8632::fpzero) (:@ x8632::dnode-size (:%l temp)))
  (movd (:%mmx x8632::stack-temp) (:@ (:%l temp))) 
  (movl (:%l temp) (:@ (:%seg :rcontext) x8632::tcr.save-tsp))  
  (movl (:$l x8632::value-cell-header) (:@ x8632::dnode-size (:%l temp)))
  (movl (:%l closed) (:@ (+ x8632::dnode-size x8632::node-size) (:%l temp)))
  (leal (:@ (+ x8632::dnode-size x8632::fulltag-misc) (:%l temp)) (:%l dest)))

(define-x8632-vinsn make-tsp-cons (((dest :lisp))
				   ((car :lisp) (cdr :lisp))
				   ((temp :imm)))
  (subl (:$b (+ x8632::cons.size x8632::dnode-size)) (:@ (:%seg :rcontext) x8632::tcr.next-tsp))
  (movl (:@ (:%seg :rcontext) x8632::tcr.next-tsp) (:%l temp))
  (movq (:%xmm x8632::fpzero) (:@ (:%l temp)))
  (movq (:%xmm x8632::fpzero) (:@ 8 (:%l temp)))
  (movd (:@ (:%seg :rcontext) x8632::tcr.save-tsp) (:%mmx x8632::stack-temp))
  (movd (:%mmx x8632::stack-temp) (:@ (:%l temp)))
  (movl (:%l temp) (:@ (:%seg :rcontext) x8632::tcr.save-tsp))
  (leal (:@ (+ x8632::dnode-size x8632::fulltag-cons) (:%l temp)) (:%l temp))
  (movl (:%l car) (:@ x8632::cons.car (:%l temp)))
  (movl (:%l cdr) (:@ x8632::cons.cdr (:%l temp)))
  (movl (:%l temp) (:%l dest)))


;; make-fixed-stack-gvector

(define-x8632-vinsn (discard-temp-frame :tsp :pop :discard) (()
                                                             ()
                                                             ((temp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.save-tsp) (:%l temp))
  (movl (:@ (:%l temp)) (:%l temp))
  (movl (:%l temp) (:@ (:%seg :rcontext) x8632::tcr.save-tsp))
  (movl (:%l temp) (:@ (:%seg :rcontext) x8632::tcr.next-tsp))
  )

(define-x8632-vinsn (discard-c-frame  :pop :discard) (()
                                                          ()
                                                          ((temp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movl (:@ (:%l temp)) (:%l temp))
  (movl (:%l temp) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp)))

  
(define-x8632-vinsn (vstack-discard :vsp :pop :discard) (()
				    ((nwords :u32const)))
  ((:not (:pred = nwords 0))
   ((:pred < nwords 16)
    (addl (:$b (:apply ash nwords x8632::word-shift)) (:%l x8632::esp)))
   ((:not (:pred < nwords 16))
    (addl (:$l (:apply ash nwords x8632::word-shift)) (:%l x8632::esp)))))

(defmacro define-x8632-subprim-lea-jmp-vinsn ((name &rest other-attrs) spno)
  `(define-x8632-vinsn (,name :call :subprim ,@other-attrs) (()
								  ()
								  ((ra (:lisp #.x8632::ra0))))
    (leal (:@ (:^ :back) (:%l x8632::fn)) (:%l ra))
    (:talign 5)
    (jmp (:@ ,spno))
    :back
    (movl (:$self 0) (:%l x8632::fn))))

(defmacro define-x8632-subprim-call-vinsn ((name &rest other-attrs) spno)
  `(define-x8632-vinsn (,name :call :subprim ,@other-attrs) (() () ())
    (:talign 5)
    (call (:@ ,spno))
    :back
    (movl (:$self 0) (:%l x8632::fn))))

(defmacro define-x8632-subprim-jump-vinsn ((name &rest other-attrs) spno)
  `(define-x8632-vinsn (,name :jumpLR ,@other-attrs) (() ())
    (jmp (:@ ,spno))))

;;; xxx don't know if this is right
(define-x8632-vinsn set-closure-forward-reference (()
                                                   ((val :lisp)
                                                    (closure :lisp)
                                                    (idx :s32const)))
  (movl (:%l val) (:@ (:apply + x8632::misc-data-offset (:apply ash idx x8632::word-shift)) (:%l closure))))

(define-x8632-vinsn (nthrowvalues :call :subprim) (()
                                                        ())
  (jmp (:@ .SPnthrowvalues)))

(define-x8632-vinsn (nthrow1value :call :subprim) (()
                                                        ()
							)

  (jmp (:@ .SPnthrow1value)))


(define-x8632-vinsn set-single-c-arg (()
                                      ((arg :single-float)
                                       (offset :u32const))
				      ((temp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movss (:%xmm arg) (:@ (:apply + 8 (:apply ash offset 2)) (:%l temp))))

(define-x8632-vinsn reload-single-c-arg (((arg :single-float))
                                         ((offset :u32const))
					 ((temp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movss (:@ (:apply + 8 (:apply ash offset 2)) (:%l temp)) (:%xmm arg)))

(define-x8632-vinsn set-double-c-arg (()
                                      ((arg :double-float)
                                       (offset :u32const))
				      ((temp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movsd (:%xmm arg) (:@ (:apply + 8 (:apply ash offset 2)) (:%l temp))))

(define-x8632-vinsn reload-double-c-arg (((arg :double-float))
                                         ((offset :u32const))
					 ((temp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movsd (:@ (:apply + 8 (:apply ash offset 2)) (:%l temp)) (:%xmm arg)))

;;; .SPffcall has stored %edx in tcr.unboxed1.  Load %mm0 with a 
;;; 64-bit value composed from %edx:%eax.
(define-x8632-vinsn get-64-bit-ffcall-result (()
                                              ())
  (movl (:%l x8632::eax) (:@ (:%seg :rcontext) x8632::tcr.unboxed0))
  (movq (:@ (:%seg :rcontext) x8632::tcr.unboxed0) (:%mmx x8632::mm0)))

(define-x8632-subprim-call-vinsn (ff-call)  .SPffcall)

(define-x8632-subprim-call-vinsn (syscall)  .SPsyscall)

(define-x8632-subprim-call-vinsn (syscall2)  .SPsyscall2)

(define-x8632-subprim-call-vinsn (setqsym) .SPsetqsym)

(define-x8632-subprim-call-vinsn (gets32) .SPgets32)

(define-x8632-subprim-call-vinsn (getu32) .SPgetu32)

(define-x8632-subprim-call-vinsn (gets64) .SPgets64)

(define-x8632-subprim-call-vinsn (getu64) .SPgetu64)

(define-x8632-subprim-call-vinsn (makes64) .SPmakes64)

(define-x8632-subprim-call-vinsn (makeu64) .SPmakeu64)

(define-x8632-subprim-lea-jmp-vinsn (stack-cons-list*)  .SPstkconslist-star)

(define-x8632-subprim-lea-jmp-vinsn (list*) .SPconslist-star)

(define-x8632-subprim-lea-jmp-vinsn (bind-interrupt-level-0) .SPbind-interrupt-level-0)

(define-x8632-vinsn bind-interrupt-level-0-inline (()
                                                   ()
                                                   ((temp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.tlb-pointer) (:%l temp))
  (cmpl (:$b 0) (:@ x8632::interrupt-level-binding-index (:%l temp)))
  (pushl (:@ x8632::interrupt-level-binding-index (:%l temp)))
  (pushl (:$b x8632::interrupt-level-binding-index))
  (pushl (:@ (:%seg :rcontext) x8632::tcr.db-link))
  (movl (:$l 0) (:@ x8632::interrupt-level-binding-index (:%l temp)))
  (movl (:%l x8632::esp) (:@ (:%seg :rcontext) x8632::tcr.db-link))
  (jns :done)
  (btrl (:$ub 31) (:@ (:%seg :rcontext) x8632::tcr.interrupt-pending))
  (jae :done)
  (ud2a)
  (:byte 2)
  :done)

(define-x8632-vinsn bind-interrupt-level-m1-inline (()
						    ()
						    ((temp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.tlb-pointer) (:%l temp))
  (pushl (:@ x8632::interrupt-level-binding-index (:%l temp)))
  (pushl (:$b x8632::interrupt-level-binding-index))
  (pushl (:@ (:%seg :rcontext) x8632::tcr.db-link))
  (movl (:$l (ash -1 x8632::fixnumshift)) (:@ x8632::interrupt-level-binding-index (:%l temp)))
  (movl (:%l x8632::esp) (:@ (:%seg :rcontext) x8632::tcr.db-link)))

(define-x8632-subprim-lea-jmp-vinsn (bind-interrupt-level-m1) .SPbind-interrupt-level-m1)

(define-x8632-subprim-lea-jmp-vinsn (bind-interrupt-level) .SPbind-interrupt-level)

(define-x8632-subprim-call-vinsn (unbind-interrupt-level) .SPunbind-interrupt-level)

#||
(define-x8632-vinsn unbind-interrupt-level-inline (()
                                                   ()
                                                   ((link :imm)
                                                    (curval :imm)
                                                    (oldval :imm)
                                                    (tlb :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.tlb-pointer) (:%l tlb))
  (movl (:@ (:%seg :rcontext) x8632::tcr.db-link) (:%l link))
  (movl (:@ x8632::interrupt-level-binding-index (:%l tlb)) (:%l curval))
  (testl (:%l curval) (:%l curval))
  (movl (:@ 8 #|binding.val|# (:%l link)) (:%l oldval))
  (movl (:@ #|binding.link|# (:%l link)) (:%l link))
  (movl (:%l oldval) (:@ x8632::interrupt-level-binding-index (:%l tlb)))
  (movl (:%l link) (:@ (:%seg :rcontext) x8632::tcr.db-link))
  (jns :done)
  (testl (:%l oldval) (:%l oldval))
  (js :done)
  (btrl (:$ub 31) (:@ (:%seg :rcontext) x8632::tcr.interrupt-pending))
  (jae :done)
  (ud2a)
  (:byte 2)
  :done)
||#

(define-x8632-vinsn (jump-return-pc :jumpLR) (()
					      ())
  (ret))

(define-x8632-vinsn label-address (((dest :lisp))
                                   ((lab :label)))
  
  (leal (:@ (:^ lab)  (:%l x8632::fn)) (:%l dest)))
;;; xxx
(define-x8632-vinsn (nmkcatchmv :call :subprim) (()
						      ((lab :label))
						      ((entry (:label 1))
						       (xfn (:lisp #.x8632::xfn))))
  (leal (:@ (:^ lab)  (:%l x8632::fn)) (:%l xfn))
  (:talign 5)
  (call (:@ .SPmkcatchmv))
  :back
  (movl (:$self 0) (:%l x8632::fn)))

(define-x8632-vinsn (nmkcatch1v :call :subprim) (()
                                                     ((lab :label))
                                                     ((xfn (:lisp #.x8632::xfn))))
  (leal (:@ (:^ lab)  (:%l x8632::fn)) (:%l x8632::xfn))
  (:talign 5)
  (call (:@ .SPmkcatch1v))
  :back
  (movl (:$self 0) (:%l x8632::fn)))


(define-x8632-vinsn (make-simple-unwind :call :subprim) (()
                                                     ())
  (jmp (:@ .SPmkunwind)))

(define-x8632-vinsn (nmkunwind  :call :subprim :needs-frame-pointer) (()
                                                     ())
  (jmp (:@ .SPnmkunwind)))

(define-x8632-vinsn u16->u32 (((dest :u32))
			      ((src :u16)))
  (movzwl (:%w src) (:%l dest)))

(define-x8632-vinsn u8->u32 (((dest :u32))
			     ((src :u8)))
  ((:pred < (:apply %hard-regspec-value src) 4)
   (movzbl (:%b src) (:%l dest)))
  ((:pred >= (:apply %hard-regspec-value src) 4)
   (movl (:%l src) (:%l dest))
   (movzbl (:%b dest) (:%l dest))))
   

(define-x8632-vinsn s16->s32 (((dest :s32))
			      ((src :s16)))
  (movswl (:%w src) (:%l dest)))

(define-x8632-vinsn s8->s32 (((dest :s32))
			     ((src :s8)))
  (movsbl (:%b src) (:%l dest)))

(define-x8632-subprim-jump-vinsn (tail-call-fn-gen) .SPtcallnfngen)

(define-x8632-subprim-jump-vinsn (tail-call-fn-vsp) .SPtcallnfnvsp)

(define-x8632-vinsn set-eq-bit (()
                                ())
  (testb (:%b x8632::arg_z) (:%b x8632::arg_z)))

;;; %schar8
;;; %schar32
;;; %set-schar8
;;; %set-schar32

(define-x8632-vinsn misc-set-c-single-float (((val :single-float))
					     ((v :lisp)
					      (idx :u32const)))
  (movss (:%xmm val) (:@ (:apply + x8632::misc-data-offset (:apply ash idx 2)) (:%l v))))

(define-x8632-vinsn array-data-vector-ref (((dest :lisp))
					   ((header :lisp)))
  (movl (:@ x8632::arrayH.data-vector (:%l header)) (:%l dest)))

(define-x8632-vinsn set-z-flag-if-istruct-typep (()
                                                 ((val :lisp)
                                                  (type :lisp))
                                                 ((tag :u8)
                                                  (valtype :lisp)))
  (xorl (:%l valtype) (:%l valtype))
  (movl (:%l val) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :have-tag)
  (movsbl (:@ x8632::misc-subtag-offset (:%l val)) (:%l tag))
  :have-tag
  (cmpl (:$b x8632::subtag-istruct) (:%l tag))
  (jne :do-compare)
  (movl (:@ x8632::misc-data-offset (:%l val)) (:%l valtype))
  :do-compare
  (cmpl (:%l valtype) (:%l type)))

(define-x8632-subprim-call-vinsn (subtag-misc-ref) .SPsubtag-misc-ref)

(define-x8632-subprim-call-vinsn (subtag-misc-set) .SPsubtag-misc-set)

(define-x8632-vinsn mem-set-c-constant-fullword (()
                                                 ((val :s32const)
                                                  (dest :address)
                                                  (offset :s32const)))
  ((:pred = offset 0)
   (movl (:$l val) (:@ (:%l dest))))
  ((:not (:pred = offset 0))
   (movl (:$l val) (:@ offset (:%l dest)))))

(define-x8632-vinsn mem-set-c-halfword (()
					((val :u16)
					 (dest :address)
					 (offset :s32const)))
  ((:pred = offset 0)
   (movw (:%w val) (:@ (:%l dest))))
  ((:not (:pred = offset 0))
   (movw (:%w val) (:@ offset (:%l dest)))))

(define-x8632-vinsn mem-set-c-constant-halfword (()
                                                 ((val :s16const)
                                                  (dest :address)
                                                  (offset :s32const)))
  ((:pred = offset 0)
   (movw (:$w val) (:@ (:%l dest))))
  ((:not (:pred = offset 0))
   (movw (:$w val) (:@ offset (:%l dest)))))

(define-x8632-vinsn mem-set-c-constant-byte (()
                                                 ((val :s8const)
                                                  (dest :address)
                                                  (offset :s32const)))
  ((:pred = offset 0)
   (movb (:$b val) (:@ (:%l dest))))
  ((:not (:pred = offset 0))
   (movb (:$b val) (:@ offset (:%l dest)))))

(define-x8632-vinsn mem-set-c-byte (()
				    ((val :u8)
				     (dest :address)
				     (offset :s32const)))
  ((:pred = offset 0)
   (movb (:%b val) (:@ (:%l dest))))
  ((:not (:pred = offset 0))
   (movb (:%b val) (:@ offset (:%l dest)))))

(define-x8632-vinsn mem-ref-c-absolute-u8 (((dest :u8))
                                           ((addr :s32const)))
  (movzbl (:@ addr) (:%l dest)))

(define-x8632-vinsn mem-ref-c-absolute-s8 (((dest :s8))
                                           ((addr :s32const)))
  (movsbl (:@ addr) (:%l dest)))

(define-x8632-vinsn mem-ref-c-absolute-u16 (((dest :u16))
                                           ((addr :s32const)))
  (movzwl (:@ addr) (:%l dest)))

(define-x8632-vinsn mem-ref-c-absolute-s16 (((dest :s16))
                                           ((addr :s32const)))
  (movswl (:@ addr) (:%l dest)))

(define-x8632-vinsn mem-ref-c-absolute-fullword (((dest :u32))
                                                 ((addr :s32const)))
  (movl (:@ addr) (:%l dest)))

(define-x8632-vinsn mem-ref-c-absolute-signed-fullword (((dest :s32))
                                                        ((addr :s32const)))
  (movl (:@ addr) (:%l dest)))

(define-x8632-vinsn mem-ref-c-absolute-natural (((dest :u32))
                                                   ((addr :s32const)))
  (movl (:@ addr) (:%l dest)))

(define-x8632-vinsn mem-ref-u8 (((dest :u8))
				((src :address)
				 (index :s32)))
  (movzbl (:@ (:%l src) (:%l index)) (:%l dest)))

(define-x8632-vinsn mem-ref-c-u16 (((dest :u16))
				   ((src :address)
				    (index :s32const)))
  ((:pred = index 0)  
   (movzwl (:@ (:%l src)) (:%l dest)))
  ((:not (:pred = index 0))
   (movzwl (:@ index (:%l src)) (:%l dest))))

(define-x8632-vinsn mem-ref-u16 (((dest :u16))
				 ((src :address)
				  (index :s32)))
  (movzwl (:@ (:%l src) (:%l index)) (:%l dest)))

(define-x8632-vinsn mem-ref-c-s16 (((dest :s16))
				   ((src :address)
				    (index :s32const)))
  ((:pred = index 0)
   (movswl (:@ (:%l src)) (:%l dest)))
  ((:not (:pred = index 0))
   (movswl (:@ index (:%l src)) (:%l dest))))

(define-x8632-vinsn mem-ref-s16 (((dest :s16))
				 ((src :address)
				  (index :s32)))
  (movswl (:@ (:%l src) (:%l index)) (:%l dest)))

(define-x8632-vinsn mem-ref-c-u8 (((dest :u8))
				  ((src :address)
				   (index :s16const)))
  ((:pred = index 0)
   (movzbl (:@  (:%l src)) (:%l dest)))
  ((:not (:pred = index 0))
   (movzbl (:@ index (:%l src)) (:%l dest))))

(define-x8632-vinsn mem-ref-u8 (((dest :u8))
				((src :address)
				 (index :s32)))
  (movzbl (:@ (:%l src) (:%l index)) (:%l dest)))

(define-x8632-vinsn mem-ref-c-s8 (((dest :s8))
				  ((src :address)
				   (index :s16const)))
  ((:pred = index 0)
   (movsbl (:@ (:%l src)) (:%l dest)))
  ((:not (:pred = index 0))
   (movsbl (:@ index (:%l src)) (:%l dest))))

(define-x8632-vinsn misc-set-c-s8  (((val :s8))
				    ((v :lisp)
				     (idx :u32const))
				    ())
  (movb (:%b val) (:@ (:apply + x8632::misc-data-offset idx) (:%l v))))

(define-x8632-vinsn misc-set-s8  (((val :s8))
				  ((v :lisp)
				   (scaled-idx :s32))
				  ())
  (movb (:%b val) (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx))))

(define-x8632-vinsn mem-ref-s8 (((dest :s8))
				((src :address)
				 (index :s32)))
  (movsbl (:@ (:%l src) (:%l index)) (:%l dest)))

(define-x8632-vinsn mem-set-constant-fullword (()
                                               ((val :s32const)
                                                (ptr :address)
                                                (offset :s32)))
  (movl (:$l val) (:@ (:%l ptr) (:%l offset))))


(define-x8632-vinsn mem-set-constant-halfword (()
                                               ((val :s16const)
                                                (ptr :address)
                                                (offset :s32)))
  (movw (:$w val) (:@ (:%l ptr) (:%l offset))))

(define-x8632-vinsn mem-set-constant-byte (()
                                           ((val :s8const)
                                            (ptr :address)
                                            (offset :s32)))
  (movb (:$b val) (:@ (:%l ptr) (:%l offset))))

(define-x8632-vinsn misc-set-c-u8  (((val :u8))
				    ((v :lisp)
				     (idx :u32const))
				    ())
  (movb (:%b val) (:@ (:apply + x8632::misc-data-offset idx) (:%l v))))

(define-x8632-vinsn misc-set-u8  (((val :u8))
				  ((v :lisp)
				   (scaled-idx :s32))
				  ())
  (movb (:%b val) (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx))))

(define-x8632-vinsn misc-set-c-u16  (()
                                    ((val :u16)
                                     (v :lisp)
                                     (idx :s32const))
                                    ())
  (movw (:%w val) (:@ (:apply + x8632::misc-data-offset (:apply * 2 idx)) (:%l v))))

(define-x8632-vinsn misc-set-u16  (()
                                   ((val :u16)
                                    (v :lisp)
                                    (scaled-idx :s32))
                                   ())
  (movw (:%w val) (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx))))

(define-x8632-vinsn misc-set-c-s16  (()
                                    ((val :s16)
                                     (v :lisp)
                                     (idx :s32const))
                                    ())
  (movw (:%w val) (:@ (:apply + x8632::misc-data-offset (:apply * 2 idx)) (:%l v))))

(define-x8632-vinsn misc-set-s16  (()
                                   ((val :s16)
                                    (v :lisp)
                                    (scaled-idx :s32))
                                   ())
  (movw (:%w val) (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx))))

(define-x8632-vinsn misc-set-c-u32  (()
				     ((val :u32)
                                      (v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movl (:%l val) (:@ (:apply + x8632::misc-data-offset (:apply ash idx 2)) (:%l v))))

(define-x8632-vinsn misc-set-u32  (()
                                   ((val :u32)
                                    (v :lisp)
                                    (scaled-idx :imm))
                                   ())
  (movl (:%l val) (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx))))

(define-x8632-vinsn misc-set-c-s32  (()
				     ((val :s32)
                                      (v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movl (:%l val) (:@ (:apply + x8632::misc-data-offset (:apply ash idx 2)) (:%l v))))

(define-x8632-vinsn misc-set-s32  (()
                                   ((val :s32)
                                    (v :lisp)
                                    (scaled-idx :imm))
                                   ())
  (movl (:%l val) (:@ x8632::misc-data-offset (:%l v) (:%l scaled-idx))))

(define-x8632-vinsn %iasr (((dest :imm))
			   ((count :imm)
			    (src :imm))
			   ((temp :s32)
                            (shiftcount (:s32 #.x8632::ecx))))
  (movl (:%l count) (:%l temp))
  (sarl (:$ub x8632::fixnumshift) (:%l temp))
  (movl (:$l 31) (:%l shiftcount))
  (rcmpl (:%l temp) (:%l shiftcount))
  (cmovbel (:%l temp) (:%l shiftcount))
  (movl (:%l src) (:%l temp))
  (sarl (:%shift x8632::cl) (:%l temp))
  (andl (:$l (lognot x8632::fixnummask)) (:%l temp))
  (movl (:%l temp) (:%l dest)))

(define-x8632-vinsn %ilsr (((dest :imm))
			   ((count :imm)
			    (src :imm))
			   ((temp :s32)
                            (shiftcount (:s32 #.x8632::ecx))))
  (movl (:%l count) (:%l temp))
  (sarl (:$ub x8632::fixnumshift) (:%l temp))
  (movl (:$l 31) (:%l shiftcount))
  (rcmpl (:%l temp) (:%l shiftcount))
  (cmovbel (:%l temp) (:%l shiftcount))
  (movl (:%l src) (:%l temp))
  (shrl (:%shift x8632::cl) (:%l temp))
  (andl (:$b (lognot x8632::fixnummask)) (:%l temp))
  (movl (:%l temp) (:%l dest)))

(define-x8632-vinsn %iasr-c (((dest :imm))
			     ((count :u8const)
			      (src :imm))
			     ((temp :s32)))
  (movl (:%l src) (:%l temp))
  (sarl (:$ub count) (:%l temp))
  (andb (:$b (lognot x8632::fixnummask)) (:%b temp))
  (movl (:%l temp) (:%l dest)))

(define-x8632-vinsn %ilsr-c (((dest :imm))
			     ((count :u8const)
			      (src :imm))
			     ((temp :s32)))
  (movl (:%l src) (:%l temp))
  (shrl (:$ub count) (:%l temp))
  ;; xxx --- use :%acc
  (andb (:$b (lognot x8632::fixnummask)) (:%b temp))
  (movl (:%l temp) (:%l dest)))

(define-x8632-vinsn %ilsl (((dest :imm))
			   ((count :imm)
			    (src :imm))
			   ((shiftcount (:s32 #.x8632::ecx))))
  
  (movl (:$l (ash 31 x8632::fixnumshift)) (:%l shiftcount))
  (rcmpl (:%l count) (:%l shiftcount))
  (cmovbl (:%l count) (:%l shiftcount))
  (sarl (:$ub x8632::fixnumshift) (:%l shiftcount))
  ((:not (:pred =
                (:apply %hard-regspec-value src)
                (:apply %hard-regspec-value dest)))
   (movl (:%l src) (:%l dest)))
  (shll (:%shift x8632::cl) (:%l dest)))

(define-x8632-vinsn %ilsl-c (((dest :imm))
			     ((count :u8const)
			      (src :imm)))
  ((:not (:pred =
                (:apply %hard-regspec-value src)
                (:apply %hard-regspec-value dest)))
   (movl (:%l src) (:%l dest)))
  (shll (:$ub count) (:%l dest)))

(define-x8632-vinsn fixnum-ash-left (((dest :lisp))
                                     ((num :lisp)
                                      (amt :lisp))
                                     ((shiftcount (:s32 #.x8632::ecx))))
  (movl (:%l amt) (:%l shiftcount))
  (sarl (:$ub x8632::fixnumshift) (:%l shiftcount))
  ((:not (:pred =
                (:apply %hard-regspec-value num)
                (:apply %hard-regspec-value dest)))
   (movl (:%l num) (:%l dest)))
  (shll (:%shift x8632::cl) (:%l dest)))

(define-x8632-vinsn fixnum-ash (((dest :lisp))
                                ((num :lisp)
                                 (amt :lisp))
                                ((shiftcount (:s32 #.x8632::ecx))
                                 (temp (:s32))))
  (movl (:%l amt) (:%l shiftcount))
  (sarl (:$ub x8632::fixnumshift) (:%l shiftcount))
  (jns :left)
  (negl (:%l shiftcount))
  (movl (:%l num) (:%l temp))
  (sarl (:$ub x8632::fixnumshift) (:%l temp))
  (sarl (:%shift x8632::cl) (:%l temp))
  (imull  (:$b x8632::fixnumone) (:%l temp)(:%l dest))
  (jmp :done)
  :left
  ((:not (:pred =
                (:apply %hard-regspec-value num)
                (:apply %hard-regspec-value dest)))
   (movl (:%l num) (:%l dest)))
  (shll (:%shift x8632::cl) (:%l dest))
  :done)

;;; In safe code, something else has ensured that the value is of type
;;; BIT.
(define-x8632-vinsn set-variable-bit-to-variable-value (()
                                                        ((vec :lisp)
                                                         (word-index :s32)
                                                         (bitnum :u8)
                                                         (value :lisp)))
  (testl (:%l value) (:%l value))
  (je :clr)
  (btsl (:%l bitnum) (:@ x8632::misc-data-offset (:%l vec) (:%l word-index) 4))
  (jmp :done)
  :clr
  (btrl (:%l bitnum) (:@ x8632::misc-data-offset (:%l vec) (:%l word-index) 4))
  :done)

;;; In safe code, something else has ensured that the value is of type
;;; BIT.
(define-x8632-vinsn nset-variable-bit-to-variable-value (()
							 ((vec :lisp)
							  (index :s32)
							  (value :lisp)))
  (testl (:%l value) (:%l value))
  (je :clr)
  (btsl (:%l index) (:@ x8632::misc-data-offset (:%l vec)))
  (jmp :done)
  :clr
  (btrl (:%l index) (:@ x8632::misc-data-offset (:%l vec)))
  :done)

(define-x8632-vinsn nset-variable-bit-to-zero (()
                                              ((vec :lisp)
                                               (index :s32)))
  (btrl (:%l index) (:@ x8632::misc-data-offset (:%l vec))))

(define-x8632-vinsn nset-variable-bit-to-one (()
					     ((vec :lisp)
					      (index :s32)))
  (btsl (:%l index) (:@ x8632::misc-data-offset (:%l vec))))

(define-x8632-vinsn set-variable-bit-to-zero (()
                                              ((vec :lisp)
                                               (word-index :s32)
                                               (bitnum :u8)))
  (btrl (:%l bitnum) (:@ x8632::misc-data-offset (:%l vec) (:%l word-index) 4)))

(define-x8632-vinsn set-variable-bit-to-one (()
					     ((vec :lisp)
					      (word-index :s32)
					      (bitnum :u8)))
  (btsl (:%l bitnum) (:@ x8632::misc-data-offset (:%l vec) (:%l word-index) 4)))

(define-x8632-vinsn set-constant-bit-to-zero (()
                                              ((src :lisp)
                                               (idx :u32const)))
  (btrl (:$ub (:apply logand 31 idx))
        (:@ (:apply + x8632::misc-data-offset (:apply ash (:apply ash idx -5) x8632::word-shift)) (:%l src))))

(define-x8632-vinsn set-constant-bit-to-one (()
                                             ((src :lisp)
                                              (idx :u32const)))
  (btsl (:$ub (:apply logand 31 idx))
        (:@ (:apply + x8632::misc-data-offset (:apply ash (:apply ash idx -5) x8632::word-shift)) (:%l src))))

(define-x8632-vinsn set-constant-bit-to-variable-value (()
                                                        ((src :lisp)
                                                         (idx :u32const)
                                                         (value :lisp)))
  (testl (:%l value) (:%l value))
  (je :clr)
  (btsl (:$ub (:apply logand 31 idx))
        (:@ (:apply + x8632::misc-data-offset (:apply ash (:apply ash idx -5) x8632::word-shift)) (:%l src)))
  (jmp :done)
  :clr
  (btrl (:$ub (:apply logand 31 idx))
        (:@ (:apply + x8632::misc-data-offset (:apply ash (:apply ash idx -5) x8632::word-shift)) (:%l src)))
  :done)

(define-x8632-vinsn require-fixnum (()
                                    ((object :lisp)))
  :again
  ((:pred <= (:apply %hard-regspec-value object) x8632::ebx)
   (testb (:$b x8632::fixnummask) (:%b object)))
  ((:pred > (:apply %hard-regspec-value object) x8632::ebx)
   (testl (:$l x8632::fixnummask) (:%l object)))
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-fixnum))))

(define-x8632-vinsn require-integer (()
                                     ((object :lisp))
                                     ((tag :u8)))
  :again
  (movl (:%l object) (:%l tag))
  ((:pred = (:apply %hard-regspec-value tag) x8632::eax)
   (andb (:$b x8632::fixnummask) (:%accb tag)))
  ((:and (:pred > (:apply %hard-regspec-value tag) x8632::eax)
	 (:pred <= (:apply %hard-regspec-value tag) x8632::ebx))
   (andb (:$b x8632::fixnummask) (:%b tag)))
  ((:pred > (:apply %hard-regspec-value object) x8632::ebx)
   (andl (:$l x8632::fixnummask) (:%l tag)))
  (je :got-it)
  ((:pred = (:apply %hard-regspec-value tag) x8632::eax)
   (cmpb (:$b x8632::tag-misc) (:%accb tag)))
  ((:and (:pred > (:apply %hard-regspec-value tag) x8632::eax)
	 (:pred <= (:apply %hard-regspec-value tag) x8632::ebx))
   (cmpb (:$b x8632::tag-misc) (:%b tag)))
  ((:pred > (:apply %hard-regspec-value object) x8632::ebx)
   (cmpl (:$l x8632::tag-misc) (:%l tag)))
  (jne :bad)
  (cmpb (:$b x8632::subtag-bignum) (:@ x8632::misc-subtag-offset (:%l object)))
  (jne :bad)
  :got-it

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-integer))))

(define-x8632-vinsn require-simple-vector (()
                                           ((object :lisp))
                                           ((tag :u8)))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::fixnummask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :bad)
  (cmpb (:$b x8632::subtag-simple-vector) (:@ x8632::misc-subtag-offset (:%l object)))
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-simple-vector))))

(define-x8632-vinsn require-simple-string (()
                                           ((object :lisp))
                                           ((tag :u8)))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::fixnummask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :bad)
  (cmpb (:$b x8632::subtag-simple-base-string) (:@ x8632::misc-subtag-offset (:%l object)))
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-simple-string))))


;;; naive
(define-x8632-vinsn require-real (()
                                    ((object :lisp))
                                    ((tag :u8)
                                     (mask :lisp)))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :have-tag)
  (movzbl (:@ x8632::misc-subtag-offset (:%l object)) (:%l tag))
  :have-tag
  (cmpl (:$b (1- (- x8632::nbits-in-word x8632::fixnumshift))) (:%l tag))
  (movl (:$l (ash (logior (ash 1 x8632::tag-fixnum)
                          (ash 1 x8632::subtag-single-float)
                          (ash 1 x8632::subtag-double-float)
                          (ash 1 x8632::subtag-bignum)
                          (ash 1 x8632::subtag-ratio))
                  x8632::fixnumshift)) (:%l mask))
  (ja :bad)
  (addl (:$b x8632::fixnumshift) (:%l tag))
  (btl (:%l tag) (:%l mask))
  (jnc :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-real))))

;;; naive
(define-x8632-vinsn require-number (()
                                    ((object :lisp))
                                    ((tag :u8)
                                     (mask :lisp)))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :have-tag)
  (movzbl (:@ x8632::misc-subtag-offset (:%l object)) (:%l tag))
  :have-tag
  (cmpl (:$b x8632::subtag-complex-single-float) (:%l tag))
  (jz :good)
  (cmpl (:$b x8632::subtag-complex-double-float) (:%l tag))
  (jz :good)
  (cmpl (:$b (1- (- x8632::nbits-in-word x8632::fixnumshift))) (:%l tag))
  (movl (:$l (ash (logior (ash 1 x8632::tag-fixnum)
                          (ash 1 x8632::subtag-single-float)
                          (ash 1 x8632::subtag-double-float)
                          (ash 1 x8632::subtag-bignum)
                          (ash 1 x8632::subtag-ratio)
                          (ash 1 x8632::subtag-complex))
                  x8632::fixnumshift)) (:%l mask))
  (ja :bad)
  (addl (:$b x8632::fixnumshift) (:%l tag))
  (btl (:%l tag) (:%l mask))
  (jnc :bad)
  :good
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-number))))

(define-x8632-vinsn require-list (()
                                  ((object :lisp))
                                  ((tag :u8)))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::fulltagmask) (:%l tag))
  (cmpl (:$b x8632::fulltag-cons) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-list))))

(define-x8632-vinsn require-symbol (()
                                    ((object :lisp))
                                    ((tag :u8)))
  :again
  (cmpl (:$l (:apply target-nil-value)) (:%l object))
  (je :got-it)
  (movl (:%l object) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :bad)
  (cmpb (:$b x8632::subtag-symbol) (:@ x8632::misc-subtag-offset (:%l object)))
  (jne :bad)
  :got-it
  
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-symbol)))
)

(define-x8632-vinsn require-character (()
				       ((object :lisp))
                                       ((tag (:u8 #.x8632::imm0))))
  :again
  ((:pred < (:apply %hard-regspec-value object) 4)
   (cmpb (:$b x8632::subtag-character) (:%b object)))
  ((:not (:pred < (:apply %hard-regspec-value object) 4))
   (movl (:%l object) (:%l tag))
   (cmpb (:$b x8632::subtag-character) (:%b tag)))
  
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-character))))

(define-x8632-vinsn require-s8 (()
				((object :lisp))
				((tag :u32)))
  :again
  (movl (:%l object) (:%l tag))
  (shll (:$ub (- x8632::nbits-in-word (+ 8 x8632::fixnumshift))) (:%l tag))
  (sarl (:$ub (- x8632::nbits-in-word 8)) (:%l tag))
  (shll (:$ub x8632::fixnumshift) (:%l tag))
  (cmpl (:%l object) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-signed-byte-8))))

(define-x8632-vinsn require-u8 (()
				((object :lisp))
				((tag :u32)))
  :again
  (movl (:$l (lognot (ash #xff x8632::fixnumshift))) (:%l tag))
  (andl (:%l object) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-unsigned-byte-8))))

(define-x8632-vinsn require-s16 (()
				((object :lisp))
				((tag :s32)))
  :again
  (movl (:%l object) (:%l tag))
  (shll (:$ub (- x8632::nbits-in-word (+ 16 x8632::fixnumshift))) (:%l tag))
  (sarl (:$ub (- x8632::nbits-in-word 16)) (:%l tag))
  (shll (:$ub x8632::fixnumshift) (:%l tag))
  (cmpl (:%l object) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-signed-byte-16))))

(define-x8632-vinsn require-u16 (()
				((object :lisp))
				((tag :u32)))
  :again
  (movl (:$l (lognot (ash #xffff x8632::fixnumshift))) (:%l tag))
  (andl (:%l object) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-unsigned-byte-16))))

(define-x8632-vinsn require-s32 (()
				 ((object :lisp))
				 ((tag :s32)))
  :again
  (testl (:$l x8632::fixnummask) (:%l object))
  (movl (:%l object) (:%l tag))
  (je :ok)
  (andl (:$l x8632::fulltagmask) (:%l tag))
  (cmpl (:$l x8632::fulltag-misc) (:%l tag))
  (jne :bad)
  (cmpl (:$l x8632::one-digit-bignum-header) (:@ x8632::misc-header-offset (:%l object)))
  (jne :bad)
  :ok
  
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-signed-byte-32))))

(define-x8632-vinsn require-u32 (()
				 ((object :lisp))
				 ((tag :s32)))
  :again
  (testl (:$l x8632::fixnummask) (:%l object))
  (movl (:%l object) (:%l tag))
  (je :ok-if-non-negative)
  (andl (:$l x8632::fulltagmask) (:%l tag))
  (cmpl (:$l x8632::fulltag-misc) (:%l tag))
  (jne :bad)
  (cmpl (:$l x8632::one-digit-bignum-header) (:@ x8632::misc-header-offset (:%l object)))
  (je :one)
  (cmpl (:$l x8632::two-digit-bignum-header) (:@ x8632::misc-header-offset (:%l object)))
  (jne :bad)
  (cmpl (:$b 0) (:@ (+ x8632::misc-data-offset 4) (:%l object)))
  (je :ok)
  :bad
  (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-unsigned-byte-32))
  (jmp :again)
  :one
  (movl (:@ x8632::misc-data-offset (:%l object)) (:%l tag))
  :ok-if-non-negative
  (testl (:%l tag) (:%l tag))
  (js :bad)
  :ok)

(define-x8632-vinsn require-s64 (()
				 ((object :lisp))
				 ((tag :s32)))
  :again
  (testl (:$l x8632::fixnummask) (:%l object))
  (movl (:%l object) (:%l tag))
  (je :ok)
  (andl (:$l x8632::fulltagmask) (:%l tag))
  (cmpl (:$l x8632::fulltag-misc) (:%l tag))
  (jne :bad)
  (cmpl (:$l x8632::two-digit-bignum-header) (:@ x8632::misc-header-offset (:%l object)))
  (jne :bad)
  :ok

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-signed-byte-64))))

;;; Something is an (UNSIGNED-BYTE 64) iff:
;;; a) it's a non-negative FIXNUM
;;; b) it's a 1- or 2-digit BIGNUM and the sign bit is clear
;;; c) it's a 3-digit BIGNUM and the high word is 0
(define-x8632-vinsn require-u64 (()
				 ((object :lisp))
				 ((tag :s32)))
  :again
  (testl (:$l (logior (ash 1 31) x8632::fixnummask)) (:%l object))
  (movl (:%l object) (:%l tag))
  (je :ok)
  (andl (:$l x8632::fulltagmask) (:%l tag))
  (cmpl (:$l x8632::fulltag-misc) (:%l tag))
  (jne :bad)
  (cmpl (:$l x8632::three-digit-bignum-header) (:@ x8632::misc-header-offset (:%l object)))
  (jne :notthree)
  (cmpl (:$b 0) (:@ (+ x8632::misc-data-offset 8) (:%l object)))
  (je :ok)
  :bad
  (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-unsigned-byte-64))
  (jmp :again)
  :notthree
  (xorl (:%l tag) (:%l tag))
  (cmpl (:$l x8632::one-digit-bignum-header) (:@ x8632::misc-header-offset (:%l object)))
  (je :testsign)
  (addl (:$b x8632::fixnumone) (:%l tag))
  (cmpl (:$l x8632::two-digit-bignum-header) (:@ x8632::misc-header-offset (:%l object)))
  (jne :bad)
  :testsign
  (cmpl (:$b 0) (:@ x8632::misc-data-offset (:%l object) (:%l tag)))
  (js :bad)
  :ok)

(define-x8632-vinsn require-char-code (()
                                       ((object :lisp))
                                       ((tag :u32)))
  :again
  ((:pred <= (:apply %hard-regspec-value object) x8632::ebx)
   (testb (:$b x8632::fixnummask) (:%b object)))
  ((:pred > (:apply %hard-regspec-value object) x8632::ebx)
   (testl (:$l x8632::fixnummask) (:%l object)))  
  (jne :bad)
  (cmpl (:$l (ash #x110000 x8632::fixnumshift)) (:%l object))
  (jae :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%l object) (:$ub arch::error-object-not-mod-char-code-limit))))

(define-x8632-vinsn mask-base-char (((dest :u8))
                                    ((src :lisp)))
  ((:pred < (:apply %hard-regspec-value src) 4)
   (movzbl (:%b src) (:%l dest)))
  ((:pred >= (:apply %hard-regspec-value src) 4)
   (movl (:%l src) (:%l dest))
   (movzbl (:%b dest) (:%l dest))))
   
   

(define-x8632-vinsn event-poll (()
                                ())
  (btrl (:$ub 31) (:@ (:%seg :rcontext) x8632::tcr.interrupt-pending))
  (jae :no-interrupt)
  (ud2a)
  (:byte 2)
  :no-interrupt)

;;; check-2d-bound
;;; check-3d-bound

(define-x8632-vinsn 2d-dim1 (((dest :u32))
			     ((header :lisp)))
  (movl (:@ (+ x8632::misc-data-offset (* 4 (1+ x8632::arrayH.dim0-cell)))
	    (:%l header)) (:%l dest))
  (sarl (:$ub x8632::fixnumshift) (:%l dest)))

;;; 3d-dims

;;; xxx
(define-x8632-vinsn 2d-unscaled-index (((dest :imm)
                                        (dim1 :u32))
				       ((dim1 :u32)
                                        (i :imm)
					(j :imm)))

  (imull (:%l i) (:%l dim1))
  (leal (:@ (:%l j) (:%l dim1)) (:%l dest)))

;;; 3d-unscaled-index

(define-x8632-vinsn set-z-flag-if-arg-fixnum (()
                                              ((arg :lisp)))
  ((:pred <= (:apply %hard-regspec-value arg) x8632::ebx)
   (testb (:$b x8632::fixnummask) (:%b arg)))
  ((:pred > (:apply %hard-regspec-value arg) x8632::ebx)
   (testl (:$l x8632::fixnummask) (:%l arg))))

(define-x8632-vinsn set-z-flag-if-both-args-fixnums (()
                                                     ((a :lisp)
                                                      (b :lisp))
                                                     ((tag :u32)))
  (movl (:%l a) (:%l tag))
  (orl (:%l b) (:%l tag))
  ((:pred = (:apply %hard-regspec-value tag) x8632::eax)
   (testb (:$b x8632::fixnummask) (:%accb tag)))
  ((:and (:pred > (:apply %hard-regspec-value tag) x8632::eax)
	 (:pred <= (:apply %hard-regspec-value tag) x8632::ebx))
   (testb (:$b x8632::fixnummask) (:%b tag)))
  ((:pred > (:apply %hard-regspec-value tag) x8632::ebx)
   (testl (:$l x8632::fixnummask) (:%l tag))))
                                              


(define-x8632-vinsn fixnum->single-float (((f :single-float))
                                          ((arg :lisp))
                                          ((unboxed :s32)))
  (movl (:%l arg) (:%l unboxed))
  (sarl (:$ub x8632::fixnumshift) (:%l unboxed))
  (cvtsi2ssl (:%l unboxed) (:%xmm f)))

(define-x8632-vinsn fixnum->double-float (((f :double-float))
                                          ((arg :lisp))
                                          ((unboxed :s32)))
  (movl (:%l arg) (:%l unboxed))
  (sarl (:$ub x8632::fixnumshift) (:%l unboxed))
  (cvtsi2sdl (:%l unboxed) (:%xmm f)))

(define-x8632-vinsn xchg-registers (()
                                    ((a t)
                                     (b t)))
  (xchgl (:%l a) (:%l b)))

(define-x8632-vinsn establish-fn (()
                                  ())
  (movl (:$self 0) (:%l x8632::fn)))

(define-x8632-vinsn %scharcode32 (((code :imm))
				  ((str :lisp)
				   (idx :imm))
				  ((imm :u32)))
  (movl (:@ x8632::misc-data-offset (:%l str) (:%l idx)) (:%l imm))
  (imull (:$b x8632::fixnumone) (:%l imm) (:%l code)))

(define-x8632-vinsn %set-scharcode32 (()
				      ((str :lisp)
				       (idx :imm)
				       (code :imm))
				      ((imm :u32)))
  (movl (:%l code) (:%l imm))
  (shrl (:$ub x8632::fixnumshift) (:%l imm))
  (movl (:%l imm) (:@ x8632::misc-data-offset (:%l str) (:%l idx))))


(define-x8632-subprim-jump-vinsn (tail-call-sym-slide) .SPtcallsymslide)

(define-x8632-subprim-jump-vinsn (tail-call-sym-vsp) .SPtcallsymvsp)


(define-x8632-vinsn character->code (((dest :u32))
				     ((src :lisp)))
  (movl (:%l src) (:%l dest))
  (sarl (:$ub x8632::charcode-shift) (:%l dest)))

(define-x8632-vinsn (adjust-vsp :vsp :pop :discard)
    (()
     ((amount :s32const)))
  ((:and (:pred >= amount -128) (:pred <= amount 127))
   (addl (:$b amount) (:%l x8632::esp)))
  ((:not (:and (:pred >= amount -128) (:pred <= amount 127)))
   (addl (:$l amount) (:%l x8632::esp))))


(define-x8632-vinsn (call-subprim-1 :call :subprim) (((dest t))
							  ((spno :s32const)
							   (x t)))
  (:talign 5)
  (call (:@ spno))
  (movl (:$self 0) (:%l x8632::fn)))

(define-x8632-vinsn (call-subprim-2 :call :subprim) (((dest t))
							  ((spno :s32const)
							   (y t)
							   (z t)))
  (:talign 5)
  (call (:@ spno))
  (movl (:$self 0) (:%l x8632::fn)))

(define-x8632-vinsn %symbol->symptr (((dest :lisp))
                                     ((src :lisp))
                                     ((tag :u8)))
  :resume
  (cmpl (:$l (:apply target-nil-value)) (:%l src))
  (je :nilsym)
  (movl (:%l src) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :bad)
  (movsbl (:@ x8632::misc-subtag-offset (:%l src)) (:%l tag))
  (cmpl (:$b x8632::subtag-symbol) (:%l tag))
  (jne :bad)
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movl (:% src) (:% dest)))
  (jmp :ok)
  :nilsym
  (movl (:$l (:apply + (:apply target-nil-value) x8632::nilsym-offset)) (:%l dest))
  :ok
  
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%l src) (:$ub x8632::subtag-symbol))))

(define-x8632-vinsn single-float-bits (((dest :u32))
				       ((src :lisp)))
  (movl (:@ x8632::misc-data-offset (:%l src)) (:%l dest)))

(define-x8632-vinsn zero-double-float-register (((dest :double-float))
                                                ())
  (movsd (:%xmm x8632::fpzero) (:%xmm dest)))

(define-x8632-vinsn zero-single-float-register (((dest :single-float))
                                                ())
  (movss (:%xmm x8632::fpzero) (:%xmm dest)))

(define-x8632-subprim-lea-jmp-vinsn (heap-rest-arg) .SPheap-rest-arg)
(define-x8632-subprim-lea-jmp-vinsn (stack-rest-arg) .SPstack-rest-arg)
(define-x8632-subprim-lea-jmp-vinsn (req-stack-rest-arg) .SPreq-stack-rest-arg)


(define-x8632-subprim-call-vinsn (stack-misc-alloc) .SPstack-misc-alloc)

(define-x8632-vinsn misc-element-count-fixnum (((dest :imm))
                                               ((src :lisp))
                                               ((temp :u32)))
  (movl (:@ x8632::misc-header-offset (:%l src)) (:%l temp))
  (shrl (:$ub x8632::num-subtag-bits) (:%l temp))
  (leal (:@ (:%l temp) 4) (:%l dest)))

(define-x8632-vinsn %logior2 (((dest :imm))
                              ((x :imm)
                               (y :imm)))
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (orl (:%l y) (:%l dest)))
  ((:not (:pred =
                (:apply %hard-regspec-value x)
                (:apply %hard-regspec-value dest)))
   ((:pred =
           (:apply %hard-regspec-value y)
           (:apply %hard-regspec-value dest))
    (orl (:%l x) (:%l dest)))
   ((:not (:pred =
                 (:apply %hard-regspec-value y)
                 (:apply %hard-regspec-value dest)))
    (movl (:%l x) (:%l dest))
    (orl (:%l y) (:%l dest)))))

(define-x8632-vinsn %logand2 (((dest :imm))
                              ((x :imm)
                               (y :imm)))
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (andl (:%l y) (:%l dest)))
  ((:not (:pred =
                (:apply %hard-regspec-value x)
                (:apply %hard-regspec-value dest)))
   ((:pred =
           (:apply %hard-regspec-value y)
           (:apply %hard-regspec-value dest))
    (andl (:%l x) (:%l dest)))
   ((:not (:pred =
                 (:apply %hard-regspec-value y)
                 (:apply %hard-regspec-value dest)))
    (movl (:%l x) (:%l dest))
    (andl (:%l y) (:%l dest)))))

(define-x8632-vinsn %logxor2 (((dest :imm))
                              ((x :imm)
                               (y :imm)))
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (xorl (:%l y) (:%l dest)))
  ((:not (:pred =
                (:apply %hard-regspec-value x)
                (:apply %hard-regspec-value dest)))
   ((:pred =
           (:apply %hard-regspec-value y)
           (:apply %hard-regspec-value dest))
    (xorl (:%l x) (:%l dest)))
   ((:not (:pred =
                 (:apply %hard-regspec-value y)
                 (:apply %hard-regspec-value dest)))
    (movl (:%l x) (:%l dest))
    (xorl (:%l y) (:%l dest)))))



(define-x8632-subprim-call-vinsn (misc-ref) .SPmisc-ref)

(define-x8632-subprim-call-vinsn (ksignalerr) .SPksignalerr)

(define-x8632-subprim-call-vinsn (misc-alloc-init) .SPmisc-alloc-init)

(define-x8632-subprim-call-vinsn (misc-alloc) .SPmisc-alloc)

(define-x8632-subprim-lea-jmp-vinsn (make-stack-gvector)  .SPstkgvector)

(define-x8632-vinsn load-character-constant (((dest :lisp))
                                             ((code :u32const))
                                             ())
  (movl (:$l (:apply logior (:apply ash code 8) x8632::subtag-character))
        (:%l dest)))


(define-x8632-vinsn setup-single-float-allocation (()
						   ())
  (movl (:$l (arch::make-vheader x8632::single-float.element-count x8632::subtag-single-float)) (:%l x8632::imm0))
  (movd (:%l x8632::imm0) (:%mmx x8632::mm0))
  (movl (:$l (- x8632::single-float.size x8632::fulltag-misc)) (:%l x8632::imm0)))
  
(define-x8632-vinsn setup-double-float-allocation (()
                                                   ())
  (movl (:$l (arch::make-vheader x8632::double-float.element-count x8632::subtag-double-float)) (:%l x8632::imm0))
  (movd (:%l x8632::imm0) (:%mmx x8632::mm0))
  (movl (:$l (- x8632::double-float.size x8632::fulltag-misc)) (:%l x8632::imm0)))

(define-x8632-vinsn setup-complex-single-float-allocation (()
                                                            ())
  (movl (:$l (arch::make-vheader x8632::complex-single-float.element-count x8632::subtag-complex-single-float)) (:%l x8632::imm0))
  (movd (:%l x8632::imm0) (:%mmx x8632::mm0))
  (movl (:$l (- x8632::complex-single-float.size x8632::fulltag-misc)) (:%l x8632::imm0)))

(define-x8632-vinsn setup-complex-double-float-allocation (()
                                                           ())
  (movl (:$l (arch::make-vheader x8632::complex-double-float.element-count x8632::subtag-complex-double-float)) (:%l x8632::imm0))
  (movd (:%l x8632::imm0) (:%mmx x8632::mm0))
  (movl (:$l (- x8632::complex-double-float.size x8632::fulltag-misc)) (:%l x8632::imm0)))

(define-x8632-vinsn set-single-float-value (()
                                            ((node :lisp)
                                             (val :single-float)))
  (movss (:%xmm val) (:@ x8632::single-float.value (:%l node))))

(define-x8632-vinsn set-double-float-value (()
                                            ((node :lisp)
                                             (val :double-float)))
  (movsd (:%xmm val) (:@ x8632::double-float.value (:%l node))))

(define-x8632-vinsn set-complex-double-float-value (()
                                                    ((node :lisp)
                                                     (val :complex-double-float)))
  (movdqu (:%xmm val) (:@ x8632::complex-double-float.realpart (:%l node))))

(define-x8632-vinsn set-complex-single-float-value (()
                                                    ((node :lisp)
                                                     (val :complex-single-float)))
  (movq (:%xmm val) (:@ x8632::complex-single-float.realpart (:%l node))))


(define-x8632-vinsn word-index-and-bitnum-from-index (((word-index :u32)
                                                       (bitnum :u8))
                                                      ((index :imm)))
  (movl (:%l index) (:%l word-index))
  (shrl (:$ub x8632::fixnumshift) (:%l word-index))
  (movl (:$l 31) (:%l bitnum))
  (andl (:%l word-index) (:%l bitnum))
  (shrl (:$ub 5) (:%l word-index)))

(define-x8632-vinsn ref-bit-vector-fixnum (((dest :imm)
                                            (bitnum :u8))
                                           ((bitnum :u8)
                                            (bitvector :lisp)
                                            (word-index :u32)))
  (btl (:%l bitnum) (:@ x8632::misc-data-offset (:%l bitvector) (:%l word-index) 4))
  (setb (:%b bitnum))
  (negb (:%b bitnum))
  (andl (:$l x8632::fixnumone) (:%l bitnum))
  (movl (:%l bitnum) (:%l dest)))

(define-x8632-vinsn nref-bit-vector-fixnum (((dest :imm)
					     (bitnum :s32))
					    ((bitnum :s32)
					     (bitvector :lisp))
					    ())
  (btl (:%l bitnum) (:@ x8632::misc-data-offset (:%l bitvector)))
  (setc (:%b bitnum))
  (movzbl (:%b bitnum) (:%l bitnum))
  (imull (:$b x8632::fixnumone) (:%l bitnum) (:%l dest)))

(define-x8632-vinsn nref-bit-vector-flags (()
					   ((bitnum :s32)
					    (bitvector :lisp))
					   ())
  (btl (:%l bitnum) (:@ x8632::misc-data-offset (:%l bitvector))))

(define-x8632-vinsn misc-ref-c-bit-fixnum (((dest :imm))
                                           ((src :lisp)
                                            (idx :u32const))
                                           ((temp :u8)))
  (btl (:$ub (:apply logand 31 idx))
       (:@ (:apply + x8632::misc-data-offset (:apply ash (:apply ash idx -5) x8632::word-shift)) (:%l src)))
  (setc (:%b temp))
  (movzbl (:%b temp) (:%l temp))
  (imull (:$b x8632::fixnumone) (:%l temp) (:%l dest)))

(define-x8632-vinsn misc-ref-c-bit-flags (()
					  ((src :lisp)
					   (idx :u64const)))
  (btl (:$ub (:apply logand 31 idx))
       (:@ (:apply + x8632::misc-data-offset (:apply ash (:apply ash idx -5) x8632::word-shift)) (:%l src))))

(define-x8632-vinsn set-macptr-address (()
					((addr :address)
					 (src :lisp))
					())
  (movl (:%l addr) (:@ x8632::macptr.address (:%l src))))

(define-x8632-vinsn deref-macptr (((addr :address))
				  ((src :lisp))
				  ())
  (movl (:@ x8632::macptr.address (:%l src)) (:%l addr)))

(define-x8632-vinsn setup-macptr-allocation (()
                                             ((src :address)))
  (movd (:%l src) (:%mmx x8632::mm1))	;see %set-new-macptr-value, below
  (movl (:$l x8632::macptr-header) (:%l x8632::imm0))
  (movd (:%l x8632::imm0) (:%mmx x8632::mm0))
  (movl (:$l (- x8632::macptr.size x8632::fulltag-misc)) (:%l x8632::imm0)))

(define-x8632-vinsn %set-new-macptr-value (()
                                           ((ptr :lisp)))
  (movd (:%mmx x8632::mm1) (:@ x8632::macptr.address (:%l ptr))))

(define-x8632-vinsn mem-ref-natural (((dest :u32))
				     ((src :address)
				      (index :s32)))
  (movl (:@ (:%l src) (:%l index)) (:%l dest)))

(define-x8632-vinsn mem-ref-c-fullword (((dest :u32))
					((src :address)
					 (index :s32const)))
  ((:pred = index 0)
   (movl (:@ (:%l src)) (:%l dest)))
  ((:not (:pred = index 0))
   (movl (:@ index (:%l src)) (:%l dest))))

(define-x8632-vinsn mem-ref-c-signed-fullword (((dest :s32))
                                               ((src :address)
                                                (index :s32const)))
  ((:pred = index 0)
   (movl (:@ (:%l src)) (:%l dest)))
  ((:not (:pred = index 0))
   (movl (:@ index (:%l src)) (:%l dest))))

(define-x8632-vinsn mem-ref-c-single-float (((dest :single-float))
					    ((src :address)
					     (index :s32const)))
  ((:pred = index 0)
   (movss (:@ (:%l src)) (:%xmm dest)))
  ((:not (:pred = index 0))
   (movss (:@ index (:%l src)) (:%xmm dest))))

(define-x8632-vinsn mem-set-c-single-float (()
					    ((val :single-float)
					     (src :address)
					     (index :s16const)))
  ((:pred = index 0)
   (movss (:%xmm val) (:@ (:%l src))))
  ((:not (:pred = index 0))
   (movss (:%xmm val) (:@ index (:%l src)))))

(define-x8632-vinsn mem-ref-c-natural (((dest :u32))
                                       ((src :address)
                                        (index :s32const)))
  ((:pred = index 0)
   (movl (:@ (:%l src)) (:%l dest)))
  ((:not (:pred = index 0))
   (movl (:@ index (:%l src)) (:%l dest))))

(define-x8632-vinsn mem-ref-c-double-float (((dest :double-float))
                                            ((src :address)
                                             (index :s32const)))
  ((:pred = index 0)
   (movsd (:@ (:%l src)) (:%xmm dest)))
  ((:not (:pred = index 0))
   (movsd (:@ index (:%l src)) (:%xmm dest))))

(define-x8632-vinsn mem-set-c-double-float (()
					    ((val :double-float)
					     (src :address)
					     (index :s32const)))
  ((:pred = index 0)
   (movsd (:%xmm val) (:@ (:%l src))))
  ((:not (:pred = index 0))
   (movsd (:%xmm val) (:@ index (:%l src)))))

(define-x8632-vinsn mem-ref-fullword (((dest :u32))
				      ((src :address)
				       (index :s32)))
  (movl (:@ (:%l src) (:%l index)) (:%l dest)))

(define-x8632-vinsn mem-ref-signed-fullword (((dest :s32))
                                             ((src :address)
                                              (index :s32)))
  (movl (:@ (:%l src) (:%l index)) (:%l dest)))

(define-x8632-vinsn macptr->stack (((dest :lisp))
                                   ((ptr :address))
				   ((temp :imm)))
  (movd (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%mmx x8632::stack-temp))
  (subl (:$b (+ 8 x8632::macptr.size)) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movd (:%mmx x8632::stack-temp) (:@ (:%l temp)))
  (movl (:%l x8632::ebp) (:@ 4 (:%l temp)))
  (leal (:@ (+ 8 x8632::fulltag-misc) (:%l  temp)) (:%l dest))
  (movl (:$l x8632::macptr-header) (:@ x8632::macptr.header (:%l dest)))
  (movl (:%l ptr) (:@ x8632::macptr.address (:%l dest)))
  (movsd (:%xmm x8632::fpzero)  (:@ x8632::macptr.domain (:%l dest))))

(define-x8632-vinsn fixnum->signed-natural (((dest :s32))
                                            ((src :imm)))
  (movl (:%l src) (:%l dest))
  (sarl (:$ub x8632::fixnumshift) (:%l dest)))

(define-x8632-vinsn fixnum->unsigned-natural (((dest :u32))
                                              ((src :imm)))
  (movl (:%l src) (:%l dest))
  (shrl (:$ub x8632::fixnumshift) (:%l dest)))

(define-x8632-vinsn mem-set-double-float (()
					  ((val :double-float)
					   (src :address)
					   (index :s32)))
  (movsd (:%xmm val) (:@ (:%l src) (:%l index))))

(define-x8632-vinsn mem-set-single-float (()
					  ((val :single-float)
					   (src :address)
					   (index :s32)))
  (movss (:%xmm val) (:@ (:%l src) (:%l index))))

(define-x8632-vinsn mem-set-c-fullword (()
                                          ((val :u32)
                                           (dest :address)
                                           (offset :s32const)))
  ((:pred = offset 0)
   (movl (:%l val) (:@ (:%l dest))))
  ((:not (:pred = offset 0))
   (movl (:%l val) (:@ offset (:%l dest)))))

(define-x8632-vinsn mem-set-bit-variable-value (((src :address))
                                                ((src :address)
                                                 (offset :lisp)
                                                 (value :lisp))
                                                ((temp :lisp)))
  ;; (mark-as-imm temp)
  (btrl (:$ub (:apply %hard-regspec-value temp))
	(:@ (:%seg :rcontext) x8632::tcr.node-regs-mask))
  (movl (:%l offset) (:%l temp))
  (shrl (:$ub (+ 5 x8632::fixnumshift)) (:%l temp))
  (leal (:@ (:%l src) (:%l temp) 4) (:%l src))
  (movl (:%l offset) (:%l temp))
  (shrl (:$ub x8632::fixnumshift) (:%l temp))
  (andl (:$l 31) (:%l temp))
  (testl (:%l value) (:%l value))
  (jne :set)
  (btrl (:%l temp) (:@ (:%l src)))
  (jmp :done)
  :set
  (btsl (:%l temp) (:@ (:%l src)))
  :done
  ;; (mark-as-node temp)
  (xorl (:%l temp) (:%l temp))
  (btsl (:$ub (:apply %hard-regspec-value temp))
	(:@ (:%seg :rcontext) x8632::tcr.node-regs-mask)))


(define-x8632-vinsn mem-set-c-bit-variable-value (()
                                                  ((src :address)
                                                   (offset :s32const)
                                                   (value :lisp)))
  (testl (:%l value) (:%l value))
  (jne :set)
  ((:pred = 0 (:apply ash offset -5))
   (btrl (:$ub (:apply logand 31 offset))
        (:@  (:%l src))))
  ((:not (:pred = 0 (:apply ash offset -5)))
   (btrl (:$ub (:apply logand 31 offset))
         (:@ (:apply ash (:apply ash offset -5) 4) (:%l src))))
  (jmp :done)
  :set
  ((:pred = 0 (:apply ash offset -5))
   (btsl (:$ub (:apply logand 31 offset))
         (:@  (:%l src))))
  ((:not (:pred = 0 (:apply ash offset -5)))
   (btsl (:$ub (:apply logand 31 offset))
         (:@ (:apply ash (:apply ash offset -5) 2) (:%l src))))
  :done)

(define-x8632-vinsn %natural+  (((result :u32))
                               ((result :u32)
                                (other :u32)))
  (addl (:%l other) (:%l result)))

(define-x8632-vinsn %natural+-c (((result :u32))
                                ((result :u32)
                                 (constant :u32const)))
  (addl (:$l (:apply unsigned-to-signed constant 32)) (:%l result)))

(define-x8632-vinsn %natural-  (((result :u32))
				((result :u32)
				 (other :u32)))
  (subl (:%l other) (:%l result)))

(define-x8632-vinsn %natural--c (((result :u32))
                                ((result :u32)
                                 (constant :u32const)))
  (subl (:$l (:apply unsigned-to-signed constant 32)) (:%l result)))

(define-x8632-vinsn %natural-logior (((result :u32))
                                    ((result :u32)
                                     (other :u32)))
  (orl (:%l other) (:%l result)))

(define-x8632-vinsn %natural-logior-c (((result :u32))
                                      ((result :u32)
                                       (constant :u32const)))
  (orl (:$l (:apply unsigned-to-signed constant 32)) (:%l result)))

(define-x8632-vinsn %natural-logand (((result :u32))
                                    ((result :u32)
                                     (other :u32)))
  (andl (:%l other) (:%l result)))

(define-x8632-vinsn %natural-logand-c (((result :u32))
                                      ((result :u32)
                                       (constant :u32const)))
  (andl (:$l (:apply unsigned-to-signed constant 32)) (:%l result)))

(define-x8632-vinsn %natural-logxor (((result :u32))
                                    ((result :u32)
                                     (other :u32)))
  (xorl (:%l other) (:%l result)))

(define-x8632-vinsn %natural-logxor-c (((result :u32))
                                       ((result :u32)
                                        (constant :u32const)))
  (xorl (:$l (:apply unsigned-to-signed constant 32)) (:%l result)))

(define-x8632-vinsn natural-shift-left (((dest :u32))
                                        ((dest :u32)
                                         (amt :u8const)))
  (shll (:$ub amt) (:%l dest)))

(define-x8632-vinsn natural-shift-right (((dest :u32))
                                         ((dest :u32)
                                          (amt :u8const)))
  (shrl (:$ub amt) (:%l dest)))

(define-x8632-vinsn recover-fn (()
				())
  (movl (:$self 0) (:%l x8632::fn)))

;;; xxx probably wrong
(define-x8632-vinsn (call-subprim-3 :call :subprim) (((dest t))
							  ((spno :s32const)
							   (x t)
							   (y t)
							   (z t)))
  (:talign 5)
  (call (:@ spno))
  (movl (:$self 0) (:%l x8632::fn)))

(define-x8632-vinsn vcell-ref (((dest :lisp))
			       ((vcell :lisp)))
  (movl (:@ x8632::misc-data-offset (:%l vcell)) (:%l dest)))

(define-x8632-vinsn setup-vcell-allocation (()
                                            ())
  (movl (:$l x8632::value-cell-header) (:%l x8632::imm0))
  (movd (:%l x8632::imm0) (:%mmx x8632::mm0))
  (movl (:$l (- x8632::value-cell.size x8632::fulltag-misc)) (:%l x8632::imm0)))

(define-x8632-vinsn %init-vcell (()
                                 ((vcell :lisp)
                                  (closed :lisp)))
  (movl (:%l closed) (:@ x8632::value-cell.value (:%l vcell))))

;;; "old" mkunwind.  Used by PROGV, since the binding of *interrupt-level*
;;; on entry to the new mkunwind confuses the issue.

(define-x8632-vinsn (mkunwind :call :subprim) (()
                                                     ())
  (jmp (:@ .SPmkunwind)))

;;; Funcall the function or symbol in temp0 and obtain the single
;;; value that it returns.
(define-x8632-subprim-call-vinsn (funcall) .SPfuncall)

(define-x8632-vinsn tail-funcall (()
                                  ()
                                  ((tag :u8)))
  :resume
  (movl (:%l x8632::temp0) (:%l tag))
  ((:pred = (:apply %hard-regspec-value tag) x8632::eax)
   (andl (:$b x8632::tagmask) (:%accl tag))
   (cmpl (:$b x8632::tag-misc) (:%accl tag)))
  ((:pred > (:apply %hard-regspec-value tag) x8632::eax)
   (andl (:$b x8632::tagmask) (:%l tag))
   (cmpl (:$b x8632::tag-misc) (:%l tag)))
  (jne :bad)
  (movsbl (:@ x8632::misc-subtag-offset (:%l x8632::temp0)) (:%l tag))
  (cmpl (:$b x8632::subtag-function) (:%l tag))
  (je :go)
  (cmpl (:$b x8632::subtag-symbol) (:%l tag))
  (cmovel (:@ x8632::symbol.fcell (:%l x8632::temp0)) (:%l x8632::temp0))
  (jne :bad)
  :go
  (jmp (:%l x8632::temp0))

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-not-callable)))

;;; Magic numbers in here include the address of .SPcall-closure.

;;; movl $self, %fn
;;; jmp *20660 (.SPcall-closure)
(define-x8632-vinsn init-nclosure (()
                                   ((closure :lisp)))
  (movb (:$b 6) (:@ x8632::misc-data-offset (:%l closure))) ;imm word count
  (movb (:$b #xbf) (:@ (+ x8632::misc-data-offset 2) (:%l closure))) ;movl $self, %fn
  (movb (:$b #xff) (:@ (+ x8632::misc-data-offset 7) (:%l closure))) ;jmp
  (movl (:$l #x0150b425) (:@ (+ x8632::misc-data-offset 8) (:%l closure))) ;.SPcall-closure
  ;; already aligned
  ;; (movl ($ 0) (:@ (+ x8632::misc-data-offset 12))) ;"end" of self-references
  (movb (:$b 7) (:@ (+ x8632::misc-data-offset 16) (:%l closure))) ;self-reference offset
  (movb (:$b x8632::function-boundary-marker) (:@ (+ x8632::misc-data-offset 20) (:%l closure)))
  ;; If the GC moved the closure before we finished creating its
  ;; self-reference table, it wouldn't have updated this self-reference
  (movl (:%l closure) (:@ (+ x8632::misc-data-offset 3) (:%l closure))))

(define-x8632-vinsn finalize-closure (((closure :lisp))
                                      ((closure :lisp)))
  (nop))


(define-x8632-vinsn (ref-symbol-value :call :subprim)
    (((val :lisp))
     ((sym (:lisp (:ne val)))))
  (:talign 5)
  (call (:@ .SPspecrefcheck))
  (movl (:$self 0) (:%l x8632::fn)))

(define-x8632-vinsn ref-symbol-value-inline (((dest :lisp))
					     ((src (:lisp (:ne dest))))
					     ((table :imm)
					      (idx :imm)))
  :resume
  (movl (:@ x8632::symbol.binding-index (:%l src)) (:%l idx))
  (xorl (:%l table) (:%l table))
  (rcmpl (:%l idx) (:@ (:%seg :rcontext) x8632::tcr.tlb-limit))
  (cmovael (:%l table) (:%l idx))
  (movl (:@ (:%seg :rcontext) x8632::tcr.tlb-pointer) (:%l table))
  (movl (:@ (:%l table) (:%l idx)) (:%l dest))
  (cmpl (:$l x8632::subtag-no-thread-local-binding) (:%l dest))
  (cmovel (:@ x8632::symbol.vcell (:%l src)) (:%l dest))
  (cmpl (:$l x8632::unbound-marker) (:%l dest))
  (je :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-unbound (:%l src))))

(define-x8632-vinsn (%ref-symbol-value :call :subprim)
    (((val :lisp))
     ((sym (:lisp (:ne val)))))
  (:talign 5)
  (call (:@ .SPspecref))
  (movl (:$self 0) (:%l x8632::fn)))

(define-x8632-vinsn %ref-symbol-value-inline (((dest :lisp))
                                              ((src (:lisp (:ne dest))))
                                              ((idx :imm)))
  ;; binding index 0 always contains a no-thread-local-binding
  ;; marker, so treat out-of-range indices as 0 to avoid branches.
  (movl (:@ x8632::symbol.binding-index (:%l src)) (:%l idx))
  (xorl (:% dest) (:% dest))
  (rcmpl (:%l idx) (:@ (:%seg :rcontext) x8632::tcr.tlb-limit))
  (cmovael (:% dest) (:% idx))
  (addl (:@ (:%seg :rcontext) x8632::tcr.tlb-pointer) (:%l idx))
  (movl (:@ (:%l idx)) (:%l dest))
  (cmpl (:$l x8632::subtag-no-thread-local-binding) (:%l dest))
  (cmovel (:@ x8632::symbol.vcell (:%l src)) (:%l dest)))

(define-x8632-vinsn ref-interrupt-level (((dest :imm))
                                         ()
                                         ((temp :u32)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.tlb-pointer) (:%l temp))
  (movl (:@ x8632::interrupt-level-binding-index (:%l temp)) (:%l dest)))

(define-x8632-subprim-lea-jmp-vinsn (bind-nil)  .SPbind-nil)

(define-x8632-subprim-lea-jmp-vinsn (bind-self)  .SPbind-self)

(define-x8632-subprim-lea-jmp-vinsn (bind-self-boundp-check)  .SPbind-self-boundp-check)

(define-x8632-subprim-lea-jmp-vinsn (bind)  .SPbind)

(define-x8632-vinsn (dpayback :call :subprim) (()
                                                    ((n :s16const))
                                                    ((temp (:u32 #.x8632::imm0))
                                                     (entry (:label 1))))
  ((:pred > n 0)
   (:if (:pred > n 1)
     (:progn
       (movl (:$l n) (:%l temp))
       (:talign 5)
       (call (:@ .SPunbind-n)))
     (:progn
       (:talign 5)
       (call (:@ .SPunbind))))
   (movl (:$self 0) (:%l x8632::fn))))

(define-x8632-subprim-jump-vinsn (tail-call-sym-gen) .SPtcallsymgen)

(define-x8632-subprim-call-vinsn (make-stack-list)  .Spmakestacklist)

(define-x8632-vinsn node-slot-ref  (((dest :lisp))
				    ((node :lisp)
				     (cellno :u32const)))
  (movl (:@ (:apply + x8632::misc-data-offset (:apply ash cellno 2))
            (:%l node)) (:%l dest)))

(define-x8632-subprim-lea-jmp-vinsn (stack-cons-list)  .SPstkconslist)

(define-x8632-vinsn save-lexpr-argregs (()
                                        ((min-fixed :u16const)))
  ((:pred >= min-fixed $numx8632argregs)
   (pushl (:%l x8632::arg_y))
   (pushl (:%l x8632::arg_z)))
  ((:pred = min-fixed 1)                ; at least one arg
   (rcmpl (:%l x8632::nargs) (:$b (ash 1 x8632::word-shift)))
   (je :z1)				;skip arg_y if exactly 1
   (pushl (:%l x8632::arg_y))
   :z1
   (pushl (:%l x8632::arg_z)))
  ((:pred = min-fixed 0)
   (rcmpl (:%l x8632::nargs) (:$b (ash 1 x8632::word-shift)))
   (je :z0)				;exactly one
   (jl :none)				;none
                                        ;two or more...
   (pushl (:%l x8632::arg_y))
   :z0
   (pushl (:%l x8632::arg_z))
   :none
   )
  ((:not (:pred = min-fixed 0))
   (leal (:@ (:apply - (:apply ash min-fixed x8632::word-shift)) (:%l x8632::nargs))
         (:%l x8632::nargs)))
  (pushl (:%l x8632::nargs))
  (movl (:%l x8632::esp) (:%l x8632::arg_z)))

;;; The frame that was built (by SAVE-LISP-CONTEXT-VARIABLE-ARG-COUNT
;;; and SAVE-LEXPR-ARGREGS) contains an unknown number of arguments
;;; followed by the count of non-required arguments; the count is on
;;; top of the stack and its address is in %arg_z.  We need to build a
;;; frame so that the function can address its arguments (copies of
;;; the required arguments and the lexpr) and locals; when the
;;; function returns, it should one or more values (depending on how
;;; it was called) and discard the hidden lexpr frame.  At this point,
;;; %ra0 still contains the "real" return address. If it's not the
;;; magic multiple-value address, we can make the function return to
;;; something that does a single-value return (.SPpopj); otherwise, we
;;; need to make it return multiple values to the real caller. (Unlike
;;; the PPC, this case only involves creating one frame here, but that
;;; frame has two return addresses.)
(define-x8632-vinsn build-lexpr-frame (()
                                       ()
                                       ((temp :imm)
					(ra0 (:lisp #.x8632::ra0))))
  (movl (:@ (:apply + (:apply target-nil-value) (x8632::%kernel-global 'x86::ret1valaddr)))
        (:%l temp))
  (cmpl (:%l temp) (:%l ra0))
  (je :multiple)
  (pushl (:@ (:apply + (:apply target-nil-value) (x8632::%kernel-global 'x86::lexpr-return1v))))
  (jmp :finish)
  :multiple
  (pushl (:@ (:apply + (:apply target-nil-value) (x8632::%kernel-global 'x86::lexpr-return))))
  (pushl (:%l temp))
  :finish
  (pushl (:%l x8632::ebp))
  (movl (:%l x8632::esp) (:%l x8632::ebp)))

(define-x8632-vinsn copy-lexpr-argument (()
					 ((n :u16const))
					 ((temp :imm)))
  (movl (:@ (:%l x8632::arg_z)) (:%l temp))
  (pushl (:@ (:apply ash n x8632::word-shift) (:%l x8632::arg_z) (:%l temp))))

(define-x8632-vinsn %current-tcr (((dest :lisp))
                                 ())
  (movl (:@ (:%seg :rcontext) x8632::tcr.linear) (:%l dest)))

(define-x8632-vinsn (setq-special :call :subprim)
    (()
     ((sym :lisp)
      (val :lisp)))
  (:talign 5)
  (call (:@ .SPspecset))
  (movl (:$self 0) (:%l x8632::fn)))

(define-x8632-vinsn pop-argument-registers (()
                                            ())
  (testl (:%l x8632::nargs) (:%l x8632::nargs))
  (je :done)
  (rcmpl (:%l x8632::nargs) (:$l (ash 1 x8632::word-shift)))
  (popl (:%l x8632::arg_z))
  (je :done)
  (popl (:%l x8632::arg_y))
  :done)

(define-x8632-vinsn %symptr->symvector (((target :lisp))
                                        ((target :lisp)))
  (nop))

(define-x8632-vinsn %symvector->symptr (((target :lisp))
                                        ((target :lisp)))
  (nop))

(define-x8632-subprim-lea-jmp-vinsn (spread-lexpr)  .SPspread-lexpr-z)

(define-x8632-vinsn mem-ref-double-float (((dest :double-float))
					  ((src :address)
					   (index :s32)))
  (movsd (:@ (:%l src) (:%l index)) (:%xmm dest)))

(define-x8632-vinsn mem-ref-single-float (((dest :single-float))
					  ((src :address)
					   (index :s32)))
  (movss (:@ (:%l src) (:%l index)) (:%xmm dest)))

;;; This would normally be put in %nargs, but we need an
;;; extra node register for passing stuff into
;;; SPdestructuring_bind and friends.
(define-x8632-vinsn load-adl (()
			      ((n :u32const)))
  (movl (:$l n) (:%l x8632::imm0)))

(define-x8632-subprim-lea-jmp-vinsn (macro-bind) .SPmacro-bind)

(define-x8632-subprim-lea-jmp-vinsn (destructuring-bind-inner) .SPdestructuring-bind-inner)

(define-x8632-subprim-lea-jmp-vinsn  (destructuring-bind) .SPdestructuring-bind)


(define-x8632-vinsn symbol-function (((val :lisp))
                                     ((sym (:lisp (:ne val))))
                                     ((tag :u8)))
  :resume
  (movl (:@ x8632::symbol.fcell (:%l sym)) (:%l val))
  (movl (:%l val) (:%l tag))
  (andl (:$b x8632::tagmask) (:%l tag))
  (cmpl (:$b x8632::tag-misc) (:%l tag))
  (jne :bad)
  (movsbl (:@ x8632::misc-subtag-offset (:%l val)) (:%l tag))
  (cmpl (:$b x8632::subtag-function) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-udf (:%l sym))))

(define-x8632-subprim-jump-vinsn (tail-call-fn-slide) .SPtcallnfnslide)

(define-x8632-vinsn load-double-float-constant (((dest :double-float))
                                                ((lab :label)))
  (movsd (:@ (:^ lab) (:%l x8632::fn)) (:%xmm dest)))

(define-x8632-vinsn load-single-float-constant (((dest :single-float))
                                                ((lab :label)))
  (movss (:@ (:^ lab) (:%l x8632::fn)) (:%xmm dest)))

(define-x8632-subprim-call-vinsn (misc-set) .SPmisc-set)

(define-x8632-subprim-lea-jmp-vinsn (slide-values) .SPmvslide)

(define-x8632-subprim-lea-jmp-vinsn (spread-list)  .SPspreadargz)

;;; Even though it's implemented by calling a subprim, THROW is really
;;; a JUMP (to a possibly unknown destination).  If the destination's
;;; really known, it should probably be inlined (stack-cleanup, value
;;; transfer & jump ...)
(define-x8632-vinsn (throw :jump-unknown) (()
						 ()
                                                 ((entry (:label 1))
						  (ra (:lisp #.x8632::ra0))))
  (leal (:@ (:^ :back) (:%l x8632::fn)) (:%l ra))
  (:talign 5)
  (jmp (:@ .SPthrow))
  :back
  (movl (:$self 0) (:%l x8632::fn))
  (uuo-error-reg-not-tag (:%l x8632::temp0) (:$ub x8632::subtag-catch-frame)))

(define-x8632-vinsn unbox-base-char (((dest :u32))
				     ((src :lisp)))
  (movl (:%l src) (:%l dest))
  ((:pred = (:apply %hard-regspec-value dest) x8632::eax)
   (cmpb (:$b x8632::subtag-character) (:%accb dest)))
  ((:and (:pred > (:apply %hard-regspec-value dest) x8632::eax)
	 (:pred <= (:apply %hard-regspec-value dest) x8632::ebx))
   (cmpb (:$b x8632::subtag-character) (:%b dest)))
  ((:pred > (:apply %hard-regspec-value dest) x8632::ebx)
   ;; very rare case, if even possible...
   (andl (:$l #xff) (:%l dest))
   (cmpl (:$b x8632::subtag-character) (:%l dest))
   (cmovel (:%l src) (:%l dest)))
  (je ::got-it)
  (uuo-error-reg-not-tag (:%l src) (:$ub x8632::subtag-character))
  :got-it
  (shrl (:$ub x8632::charcode-shift) (:%l dest)))

(define-x8632-subprim-lea-jmp-vinsn (save-values) .SPsave-values)

(define-x8632-subprim-lea-jmp-vinsn (recover-values)  .SPrecover-values)

(define-x8632-subprim-lea-jmp-vinsn (recover-values-for-mvcall) .SPrecover-values-for-mvcall)

(define-x8632-subprim-lea-jmp-vinsn (add-values) .SPadd-values)

(define-x8632-subprim-call-vinsn (make-stack-block)  .SPmakestackblock)

(define-x8632-subprim-call-vinsn (make-stack-block0)  .Spmakestackblock0)

;;; "dest" is preallocated, presumably on a stack somewhere.
(define-x8632-vinsn store-single (()
				  ((dest :lisp)
				   (source :single-float))
				  ())
  (movss (:%xmm source) (:@  x8632::single-float.value (:%l dest))))

;;; "dest" is preallocated, presumably on a stack somewhere.
(define-x8632-vinsn store-double (()
				  ((dest :lisp)
				   (source :double-float))
				  ())
  (movsd (:%xmm source) (:@  x8632::double-float.value (:%l dest))))

(define-x8632-vinsn fixnum->char (((dest :lisp))
				  ((src :imm))
				  ((temp :u32)))
  (movl (:%l src) (:%l temp))
  (sarl (:$ub (+ x8632::fixnumshift 11)) (:%l temp))
  (cmpl (:$b (ash #xd800 -11))(:%l temp))
  (movl (:$l (:apply target-nil-value)) (:%l temp))
  (cmovel (:%l temp) (:%l dest))
  (je :done)
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movl (:%l src) (:%l dest)))
  (shll (:$ub (- x8632::charcode-shift x8632::fixnumshift)) (:%l dest))
  (addl (:$b x8632::subtag-character) (:%l dest))
  :done)

;;; src is known to be a code for which CODE-CHAR returns non-nil.
(define-x8632-vinsn code-char->char (((dest :lisp))
				     ((src :imm))
				     ())
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movl (:%l src) (:%l dest)))
  (shll (:$ub (- x8632::charcode-shift x8632::fixnumshift)) (:%l dest))
  (addl (:$b x8632::subtag-character) (:%l dest))
  :done)

(define-x8632-vinsn sign-extend-halfword (((dest :imm))
					  ((src :imm)))
  (movl (:%l src ) (:%l dest))
  (shll (:$ub (- 16 x8632::fixnumshift)) (:%l dest))
  (sarl (:$ub (- 16 x8632::fixnumshift)) (:%l dest)))

(define-x8632-subprim-jump-vinsn (tail-funcall-gen) .SPtfuncallgen)

(define-x8632-vinsn %init-gvector (()
                                   ((v :lisp)
                                    (nbytes :u32const))
                                   ((count :imm)))
  (movl (:$l nbytes) (:%l count))
  (jmp :test)
  :loop
  (popl (:@ x8632::misc-data-offset (:%l v) (:%l count)))
  :test
  (subl (:$b x8632::node-size) (:%l count))
  (jge :loop))

(define-x8632-subprim-jump-vinsn (tail-funcall-slide) .SPtfuncallslide)

(define-x8632-vinsn nth-value (((result :lisp))
                               ()
                               ((temp :u32)
				(nargs (:lisp #.x8632::nargs))))
  (leal (:@ (:%l x8632::esp) (:%l x8632::nargs)) (:%l temp))
  (subl (:@ (:%l temp)) (:%l x8632::nargs))
  (movl (:$l (:apply target-nil-value)) (:%l result))
  (jle :done)
  ;; I -think- that a CMOV would be safe here, assuming that N wasn't
  ;; extremely large.  Don't know if we can assume that.
  (movl (:@ (- x8632::node-size) (:%l x8632::esp) (:%l x8632::nargs)) (:%l result))
  :done
  (leal (:@ x8632::node-size (:%l temp)) (:%l x8632::esp)))


(define-x8632-subprim-lea-jmp-vinsn (req-heap-rest-arg) .SPreq-heap-rest-arg)

(define-x8632-subprim-call-vinsn (stack-misc-alloc-init)  .SPstack-misc-alloc-init)

(define-x8632-vinsn %debug-trap (()
                                 ())
  (uuo-error-debug-trap))

(define-x8632-vinsn double-to-single (((result :single-float))
                                      ((arg :double-float)))
  (cvtsd2ss (:%xmm arg) (:%xmm result)))

(define-x8632-vinsn single-to-double (((result :double-float))
                                      ((arg :single-float)))
  (cvtss2sd (:%xmm arg) (:%xmm result)))

(define-x8632-vinsn alloc-c-frame (()
                                   ((nwords :u32const))
				   ((temp :imm)))
  (movd (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%mmx x8632::stack-temp))
  ;; Work around Apple bug number 6386516 (open stub may clobber stack)
  ;; by leaving an extra word of space in the parameter area.
  (subl (:$l (:apply ash (:apply 1+ nwords) x8632::word-shift))
	(:@ (:%seg :rcontext) x8632::tcr.foreign-sp))
  ;; align stack to 16-byte boundary
  (andb (:$b -16) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp))
  (subl (:$b (* 2 x8632::node-size)) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movd (:%mmx x8632::stack-temp) (:@ (:%l temp)))
  (movl (:% x8632::ebp) (:@ 4 (:%l temp))))

(define-x8632-vinsn alloc-variable-c-frame (()
                                            ((nwords :imm))
                                            ((temp :imm)))
  (movd (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%mmx x8632::stack-temp))
  ;; Work around Apple bug number 6386516 (open stub may clobber stack)
  ;; by leaving an extra word of space in the parameter area.
  ;; Note that nwords is a fixnum.
  (leal (:@ 4 (:%l nwords)) (:%l temp))
  (subl (:%l temp) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp))
  ;; align stack to 16-byte boundary
  (andb (:$b -16) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp))
  (subl (:$b (* 2 x8632::node-size)) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movd (:%mmx x8632::stack-temp) (:@ (:%l temp)))
  (movl (:% x8632::ebp) (:@ 4 (:%l temp))))

(define-x8632-vinsn set-c-arg (()
                               ((arg :u32)
                                (offset :u32const))
			       ((temp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movl (:%l arg) (:@ (:apply + 8 (:apply ash offset 2)) (:%l temp))))

;;; This is a pretty big crock.
(define-x8632-vinsn set-c-arg-from-mm0 (()
					((offset :u32const))
					((temp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movq (:%mmx x8632::mm0) (:@ (:apply + 8 (:apply ash offset 2)) (:%l temp))))

(define-x8632-vinsn eep.address (((dest t))
				 ((src (:lisp (:ne dest )))))
  :resume
  (movl (:@ (+ (ash 1 x8632::word-shift) x8632::misc-data-offset) (:%l src))
        (:%l dest))
  (cmpl (:$l (:apply target-nil-value)) (:%l dest))
  (je :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-eep-unresolved (:%l src) (:%l dest))))

(define-x8632-subprim-lea-jmp-vinsn (heap-cons-rest-arg) .SPheap-cons-rest-arg)

(define-x8632-subprim-lea-jmp-vinsn (stack-cons-rest-arg) .SPstack-cons-rest-arg)

(define-x8632-subprim-lea-jmp-vinsn (make-stack-vector)  .SPmkstackv)

(define-x8632-vinsn %current-frame-ptr (((dest :imm))
					())
  (movl (:%l x8632::ebp) (:%l dest)))

(define-x8632-vinsn %foreign-stack-pointer (((dest :imm))
                                            ())
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l dest)))


(define-x8632-vinsn  %slot-ref (((dest :lisp))
				((instance (:lisp (:ne dest)))
				 (index :lisp)))
  (movl (:@ x8632::misc-data-offset (:%l instance) (:%l index)) (:%l dest))
  (cmpl (:$l x8632::slot-unbound-marker) (:%l dest))
  (je :bad)
  :resume
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-slot-unbound (:%l dest) (:%l instance) (:%l index))))



(define-x8632-vinsn symbol-ref (((dest :lisp))
                                ((src :lisp)
                                 (cellno :u32const)))
  (movl (:@ (:apply + (- x8632::node-size x8632::fulltag-misc)
                    (:apply ash cellno 2))
              (:%l src)) (:%l dest)))

(define-x8632-vinsn mem-ref-c-bit-fixnum (((dest :lisp))
                                          ((src :address)
                                           (offset :s32const))
                                          ((temp :imm)))
  ((:pred = 0 (:apply ash offset -5))
   (btl (:$ub (:apply logand 31 offset))
        (:@  (:%l src))))
  ((:not (:pred = 0 (:apply ash offset -5)))
   (btl (:$ub (:apply logand 31 offset))
        (:@ (:apply ash (:apply ash offset -5) 2) (:%l src))))
  (movl (:$l x8632::fixnumone) (:%l temp))
  (movl (:$l 0) (:%l dest))
  (cmovbl (:%l temp) (:%l dest)))

(define-x8632-vinsn mem-ref-bit-fixnum (((dest :lisp)
                                         (src :address))
                                        ((src :address)
                                         (offset :lisp))
                                        ((temp :lisp)))
  ;; (mark-as-imm temp)
  (btrl (:$ub (:apply %hard-regspec-value temp))
	(:@ (:%seg :rcontext) x8632::tcr.node-regs-mask))
  (movl (:%l offset) (:%l temp))
  (shrl (:$ub (+ 5 x8632::fixnumshift)) (:%l temp))
  (leal (:@ (:%l src) (:%l temp) 4) (:%l src))
  (movl (:%l offset) (:%l temp))
  (shrl (:$ub x8632::fixnumshift) (:%l temp))
  (andl (:$l 31) (:%l temp))
  (btl (:%l temp) (:@ (:%l src)))
  (movl (:$l x8632::fixnumone) (:%l temp))
  (leal (:@ (- x8632::fixnumone) (:%l temp)) (:%l dest))
  (cmovbl (:%l temp) (:%l dest))
  ;; (mark-as-node temp)
  (xorl (:%l temp) (:%l temp))
  (btsl (:$ub (:apply %hard-regspec-value temp))
	(:@ (:%seg :rcontext) x8632::tcr.node-regs-mask)))

(define-x8632-subprim-call-vinsn (progvsave) .SPprogvsave)

(define-x8632-subprim-jump-vinsn (progvrestore) .SPprogvrestore)

(define-x8632-subprim-lea-jmp-vinsn (simple-keywords) .SPsimple-keywords)

(define-x8632-subprim-lea-jmp-vinsn (keyword-args) .SPkeyword-args)

(define-x8632-subprim-lea-jmp-vinsn (keyword-bind) .SPkeyword-bind)

(define-x8632-vinsn set-high-halfword (()
				       ((dest :imm)
					(n :s16const)))
  (orl (:$l (:apply ash n 16)) (:%l dest)))

(define-x8632-vinsn scale-nargs (()
				 ((nfixed :s16const)))
  ((:pred > nfixed 0)
   ((:pred < nfixed 32)
    (subl (:$b (:apply ash nfixed x8632::word-shift)) (:%l x8632::nargs)))
   ((:pred >= nfixed 32)
    (subl (:$l (:apply ash nfixed x8632::word-shift)) (:%l x8632::nargs)))))

(define-x8632-vinsn opt-supplied-p (()
                                    ((num-opt :u16const))
                                    ((nargs (:u32 #.x8632::nargs))
                                     (imm :imm)))
  (xorl (:%l imm) (:%l imm))
  (movl (:$l (:apply target-nil-value)) (:%l x8632::arg_y))
  :loop
  (rcmpl (:%l imm) (:%l nargs))
  (movl (:%l x8632::arg_y) (:%l x8632::arg_z))
  (cmovll (:@ (+ x8632::t-offset x8632::symbol.vcell) (:%l x8632::arg_y)) (:%l  x8632::arg_z))
  (addl (:$b x8632::node-size) (:%l imm))
  (rcmpl (:%l imm) (:$l (:apply ash num-opt x8632::fixnumshift)))
  (pushl (:%l x8632::arg_z))
  (jne :loop))

(define-x8632-vinsn one-opt-supplied-p (()
                                        ()
					((temp :u32)))
  (testl (:%l x8632::nargs) (:%l x8632::nargs))
  (setne (:%b temp))
  (negb (:%b temp))
  (andl (:$b x8632::t-offset) (:%l temp))
  (addl (:$l (:apply target-nil-value)) (:%l temp))
  (pushl (:%l temp)))

;; needs some love
(define-x8632-vinsn two-opt-supplied-p (()
                                        ())
  (rcmpl (:%l x8632::nargs) (:$b (:apply ash 2 x8632::word-shift)))
  (jge :two)
  (rcmpl (:%l x8632::nargs) (:$b (:apply ash 1 x8632::word-shift)))
  (je :one)
  ;; none
  (pushl (:$l (:apply target-nil-value)))
  (pushl (:$l (:apply target-nil-value)))
  (jmp :done)
  :one
  (pushl (:$l (:apply target-t-value)))
  (pushl (:$l (:apply target-nil-value)))
  (jmp :done)
  :two
  (pushl (:$l (:apply target-t-value)))
  (pushl (:$l (:apply target-t-value)))
  :done)

(define-x8632-vinsn set-c-flag-if-constant-logbitp (()
                                                    ((bit :u8const)
                                                     (int :imm)))
  (btl (:$ub bit) (:%l int)))

(define-x8632-vinsn set-c-flag-if-variable-logbitp (()
                                                    ((bit :imm)
                                                     (int :imm))
						    ((temp :u32)))
  (movl (:%l bit) (:%l temp))
  (sarl (:$ub x8632::fixnumshift) (:%l temp))
  (addl (:$b x8632::fixnumshift) (:%l temp))
  ;; Would be nice to use a cmov here, but the branch is probably
  ;; cheaper than trying to scare up an additional unboxed temporary.
  (cmpb (:$ub 31) (:%b temp))
  (jbe :test)
  (movl (:$l 31) (:%l temp))
  :test
  (btl (:%l temp) (:%l int)))

(define-x8632-vinsn multiply-immediate (((dest :imm))
                                        ((src :imm)
                                         (const :s32const)))
  ((:and (:pred >= const -128) (:pred <= const 127))
   (imull (:$b const) (:%l src) (:%l dest)))
  ((:not (:and (:pred >= const -128) (:pred <= const 127)))
   (imull (:$l const) (:%l src) (:%l dest))))

(define-x8632-vinsn multiply-fixnums (((dest :imm))
                                      ((x :imm)
                                       (y :imm))
                                      ((unboxed :s32)))
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (movl (:%l y) (:%l unboxed))
   (sarl (:$ub x8632::fixnumshift) (:%l unboxed))
   (imull (:%l unboxed) (:%l dest)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value x)
                      (:apply %hard-regspec-value dest)))
         (:pred =
                (:apply %hard-regspec-value y)
                (:apply %hard-regspec-value dest)))
   (movl (:%l x) (:%l unboxed))
   (sarl (:$ub x8632::fixnumshift) (:%l unboxed))
   (imull (:%l unboxed) (:%l dest)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value x)
                      (:apply %hard-regspec-value dest)))
         (:not (:pred =
                      (:apply %hard-regspec-value y)
                      (:apply %hard-regspec-value dest))))
   (movl (:%l y) (:%l dest))
   (movl (:%l x) (:%l unboxed))
   (sarl (:$ub x8632::fixnumshift) (:%l unboxed))
   (imull (:%l unboxed) (:%l dest))))


(define-x8632-vinsn mark-as-imm (()
				 ((reg :imm)))
  (btrl (:$ub (:apply %hard-regspec-value reg)) (:@ (:%seg :rcontext) x8632::tcr.node-regs-mask)))

(define-x8632-vinsn mark-as-node (((reg :imm))
				  ((reg :imm)))
  (xorl (:%l reg) (:%l reg))
  (btsl (:$ub (:apply %hard-regspec-value reg)) (:@ (:%seg :rcontext) x8632::tcr.node-regs-mask)))

(define-x8632-vinsn mark-temp1-as-node-preserving-flags (()
                                                        ()
                                                        ((reg (:u32 #.x8632::temp1))))
  (movl (:$l 0) (:%l reg))              ;not xorl!
  (cld))                                ;well, preserving most flags.

  

(define-x8632-vinsn save-nfp (()
                              ()
                              ((temp :imm)))
  ((:pred > (:apply x862-max-nfp-depth) 0)
   (movd (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%mmx x8632::stack-temp))
   (:if (:pred < (:apply + 16 (:apply x862-max-nfp-depth)) 128)
     (subl (:$b (:apply + 16 (:apply x862-max-nfp-depth))) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp))
     (subl (:$l (:apply + 16 (:apply x862-max-nfp-depth))) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp)))
   (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
   (movd (:%mmx x8632::stack-temp) (:@ (:%l temp)))
   (movd (:@ (:%seg :rcontext) x8632::tcr.nfp) (:%mmx x8632::stack-temp))
   (movd (:%mmx x8632::stack-temp) (:@ 4 (:%l temp)))
   (movl (:% temp) (:@ (:%seg :rcontext) x8632::tcr.nfp))))

(define-x8632-vinsn load-nfp (((dest :imm))
                              ())
  (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:%l dest)))

(define-x8632-vinsn restore-nfp (()
                                 ()
                                 ((temp :imm)))
  ((:pred > (:apply x862-max-nfp-depth) 0)
   (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:%l temp))
   (movd (:@ 4 (:%l temp)) (:%mmx x8632::stack-temp))
   (movl (:@ (:%l temp)) (:%l temp))
   (movl (:%l temp) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp))
   (movd (:%mmx x8632::stack-temp)(:@ (:%seg :rcontext) x8632::tcr.nfp))))

(define-x8632-vinsn (nfp-store-unboxed-word :nfp :set) (()
                                                        ((val :u32)
                                                         (offset :u16const)))
  (movd (:%l val) (:%mmx x8632::stack-temp))
  (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:%l val))
  (movd (:%mmx x8632::stack-temp) (:@ (:apply + 16 offset) (:% val)))
  (movd (:%mmx x8632::stack-temp) (:% val)))


(define-x8632-vinsn (nfp-load-unboxed-word :nfp :ref) (((val :u32))
                                                       ((offset :u16const)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:%l val))
  (movl (:@ (:apply + 16 offset) (:% val)) (:%l val)))

(define-x8632-vinsn (nfp-store-single-float :nfp :set)
 (()
                                                        ((val :single-float)
                                                         (offset :u16const)
                                                         (nfp :imm)))
  (movss (:%xmm val) (:@ (:apply + 16 offset) (:% nfp))))

(define-x8632-vinsn (nfp-store-double-float :nfp :set) (()
                                                        ((val :double-float)
                                                         (offset :u16const)
                                                         (nfp :imm)))
  (movsd (:%xmm val) (:@ (:apply + 16 offset) (:% nfp))))

(define-x8632-vinsn (spill-double-float :nfp :set) (()
                                                    ((val :double-float)
                                                     (offset :u16const))
                                                    ((nfp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:% nfp))
  (movsd (:%xmm val) (:@ (:apply + 16 offset) (:% nfp))))

(define-x8632-vinsn (reload-double-float :nfp :ref) (()
                                                    ((val :double-float)
                                                     (offset :u16const))
                                                    ((nfp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:% nfp))
  (movsd (:@ (:apply + 16 offset) (:% nfp)) (:%xmm val)))

(define-x8632-vinsn (spill-single-float :nfp :set) (()
                                                    ((val :single-float)
                                                     (offset :u16const))
                                                    ((nfp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:% nfp))
  (movss (:%xmm val) (:@ (:apply + 16 offset) (:% nfp))))

(define-x8632-vinsn (reload-single-float :nfp :ref) (()
                                                    ((val :single-float)
                                                     (offset :u16const))
                                                    ((nfp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:% nfp))
  (movss (:@ (:apply + 16 offset) (:% nfp)) (:%xmm val)))

(define-x8632-vinsn (nfp-load-double-float :nfp :ref) (((val :double-float))
                                                       ((offset :u16const)
                                                       (nfp :imm)))
  (movsd (:@ (:apply + 16 offset) (:% nfp)) (:%xmm val)))


(define-x8632-vinsn (nfp-load-single-float :nfp :ref) (((val :single-float))
                                                       ((offset :u16const)
                                                        (nfp :imm)))
  (movss (:@ (:apply + 16 offset) (:% nfp)) (:%xmm val)))

(define-x8632-vinsn (nfp-store-complex-single-float :nfp :set) (()
                                                                ((val :complex-single-float)
                                                                 (offset :u16const)
                                                                (nfp :imm)))
  (movq (:%xmm val) (:@ (:apply + 16 offset) (:%l nfp))))

(define-x8632-vinsn nfp-load-complex-single-float (((val :complex-single-float))
                                                   ((offset :u16const)
                                                  (nfp :imm)))
  (movq (:@ (:apply + 16 offset) (:%l nfp)) (:%xmm val)))


(define-x8632-vinsn (nfp-store-complex-double-float :nfp :set) (()
                                                                ((val :complex-double-float)
                                                                 (offset :u16const)
                                                                (nfp :imm)))
  (movdqu (:%xmm val) (:@ (:apply + 16 offset) (:%l nfp))))

(define-x8632-vinsn (nfp-load-complex-double-float :nfp :ref) (((val :complex-double-float))
                                                               ((offset :u16const)
                                                               (nfp :imm)))
  (movdqu (:@ (:apply + 16 offset) (:%l nfp)) (:%xmm val)))

(define-x8632-vinsn nfp-compare-natural-register (()
                                                  ((offset :u16const)
                                                   (reg :u32))
                                                  ((nfp :lisp))) ; sic
  (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:%l nfp))
  (cmpl (:%l reg) (:@ (:apply + offset 16) (:%l nfp))))

(define-x8632-vinsn nfp-logior-natural-register (()
                                                  ((offset :u16const)
                                                   (reg :u32))
                                                  ((nfp :lisp))) ; sic
  (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:%l nfp))
  (orl (:@ (:apply + offset 16) (:%l nfp)) (:%l reg)))

(define-x8632-vinsn nfp-logand-natural-register (()
                                                  ((offset :u16const)
                                                   (reg :u32))
                                                  ((nfp :lisp))) ; sic
  (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:%l nfp))
  (andl (:@ (:apply + offset 16) (:%l nfp)) (:%l reg)))

(define-x8632-vinsn nfp-logxor-natural-register (()
                                                  ((offset :u16const)
                                                   (reg :u32))
                                                  ((nfp :lisp))) ; sic
  (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:%l nfp))
  (xorl (:@ (:apply + offset 16) (:%l nfp)) (:%l reg)))

(define-x8632-vinsn nfp-add-natural-register (()
                                                  ((offset :u16const)
                                                   (reg :u32))
                                                  ((nfp :lisp))) ; sic
  (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:%l nfp))
  (addl (:@ (:apply + offset 16) (:%l nfp)) (:%l reg)))

(define-x8632-vinsn nfp-subtract-natural-register (()
                                                  ((offset :u16const)
                                                   (reg :u32))
                                                  ((nfp :lisp))) ; sic
  (movl (:@ (:%seg :rcontext) x8632::tcr.nfp) (:%l nfp))
  (subl (:%l reg) (:@ (:apply + offset 16) (:%l nfp)))
  (movl (:@ (:apply + offset 16) (:%l nfp)) (:%l reg)))




(define-x8632-vinsn (temp-push-unboxed-word :push :word )
    (()
     ((w :u32))
     ((temp :imm)))
  (movd (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%mmx x8632::stack-temp))
  (subl (:$b 16) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movd (:%mmx x8632::stack-temp) (:@ (:%l temp)))
  (movl (:%l x8632::ebp) (:@ 4 (:%l temp)))
  (movl (:%l w) (:@ 8 (:%l temp))))

(define-x8632-vinsn (temp-pop-unboxed-word :pop :word )
    (((w :u32))
     ())
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l w))
  (movl (:@ 8 (:%l w)) (:%l w))
  (addl (:$b 16) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp)))



(define-x8632-vinsn (temp-pop-temp1-as-unboxed-word :pop :word )
    (()
     ()
     ((w (:u32 #.x8632::temp1))))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l w))
  (std)
  (movl (:@ 8 (:%l w)) (:%l w))
  (addl (:$b 16) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp)))

(define-x8632-vinsn (temp-push-node :push :word :tsp)
    (()
     ((w :lisp))
     ((temp :imm)))
  (subl (:$b (* 2 x8632::dnode-size)) (:@ (:%seg :rcontext) x8632::tcr.next-tsp))
  (movd (:@ (:%seg :rcontext) x8632::tcr.save-tsp) (:%mmx x8632::stack-temp))
  (movl (:@ (:%seg :rcontext) x8632::tcr.next-tsp) (:%l temp))
  (movsd (:%xmm x8632::fpzero) (:@ (:%l temp)))
  (movsd (:%xmm x8632::fpzero) (:@ x8632::dnode-size (:%l temp)))
  (movd (:%mmx x8632::stack-temp) (:@ (:%l temp)))
  (movl (:%l temp) (:@ (:%seg :rcontext) x8632::tcr.save-tsp))
  (movl (:%l w) (:@ x8632::dnode-size (:%l temp))))

(define-x8632-vinsn (temp-pop-node :pop :word :tsp)
    (((w :lisp))
     ()
     ((temp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.save-tsp) (:%l temp))
  (movl (:@ x8632::dnode-size (:%l temp)) (:%l w))
  (movl (:@ (:%l temp)) (:%l temp))
  (movl (:%l temp) (:@ (:%seg :rcontext) x8632::tcr.save-tsp))  
  (movl (:%l temp) (:@ (:%seg :rcontext) x8632::tcr.next-tsp)))

(define-x8632-vinsn (temp-push-single-float :push :word )
    (()
     ((f :single-float))
     ((temp :imm)))
  (movd (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%mmx x8632::stack-temp))
  (subl (:$b 16) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movd (:%mmx x8632::stack-temp) (:@ (:%l temp)))
  (movl (:%l x8632::ebp) (:@ 4 (:%l temp)))
  (movss (:%xmm f) (:@ 8 (:%l temp))))

(define-x8632-vinsn (temp-pop-single-float :pop :word )
    (((f :single-float))
     ()
     ((temp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movss (:@ 8 (:%l temp)) (:%xmm f))
  (addl (:$b 16) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp)))

(define-x8632-vinsn (temp-push-double-float :push :word )
    (()
     ((f :double-float))
     ((temp :imm)))
  (movd (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%mmx x8632::stack-temp))
  (subl (:$b 16) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movd (:%mmx x8632::stack-temp) (:@ (:%l temp)))
  (movl (:%l x8632::ebp) (:@ 4 (:%l temp)))
  (movsd (:%xmm f) (:@ 8 (:%l temp))))

(define-x8632-vinsn (temp-pop-double-float :pop :word )
    (((f :double-float))
     ()
     ((temp :imm)))
  (movl (:@ (:%seg :rcontext) x8632::tcr.foreign-sp) (:%l temp))
  (movsd (:@ 8 (:%l temp)) (:%xmm f))
  (addl (:$b 16) (:@ (:%seg :rcontext) x8632::tcr.foreign-sp)))

(define-x8632-vinsn load-next-method-context (((dest :lisp))
					      ())
  (movl (:@ (:%seg :rcontext) x8632::tcr.next-method-context) (:%l dest))
  (movl (:$l 0) (:@ (:%seg :rcontext) x8632::tcr.next-method-context)))

(define-x8632-vinsn save-node-register-to-spill-area (()
					 ((src :lisp)))
  ;; maybe add constant to index slot 0--3
  (movl (:%l src) (:@ (:%seg :rcontext) x8632::tcr.save3)))

(define-x8632-vinsn load-node-register-from-spill-area (((dest :lisp))
							())
  (movl (:@ (:%seg :rcontext) x8632::tcr.save3) (:%l dest))
  (movss (:%xmm x8632::fpzero) (:@ (:%seg :rcontext) x8632::tcr.save3)))

(define-x8632-vinsn align-loop-head (()
				     ())
)

(define-x8632-vinsn double-float-negate (((reg :double-float))
                                         ((reg :double-float)
                                          (tmp :double-float)))
  (movsd (:@ (:^ :const) (:% x8632::fn)) (:%xmm tmp))
  (pxor (:%xmm tmp) (:%xmm reg))

  (:uuo-section)
  :const
  (:long 0)
  (:long #x-80000000))

(define-x8632-vinsn single-float-negate (((reg :single-float))
                                         ((reg :single-float)
                                          (tmp :single-float)))
  (movss (:@ (:^ :const) (:% x8632::fn)) (:%xmm tmp))
  (pxor (:%xmm tmp) (:%xmm reg))
  (:uuo-section)
  :const
  (:long #x80000000))

(define-x8632-vinsn fixnum-ref-c-double-float (((dest :double-float))
                                               ((base :imm)
                                                (idx :u32const)))
  (movsd (:@ (:apply ash idx 3) (:%l base)) (:%xmm dest)))

(define-x8632-vinsn fixnum-ref-double-float  (((dest :double-float))
                                               ((base :imm)
                                                (idx :imm)))
  (movsd (:@ (:%l base) (:%l idx) 2) (:%xmm dest)))

(define-x8632-vinsn fixnum-set-c-double-float (()
                                               ((base :imm)
                                                (idx :u32const)
                                                (val :double-float)))
  (movsd (:%xmm val) (:@ (:apply ash idx 3) (:%l base))))

(define-x8632-vinsn fixnum-set-double-float  (()
                                               ((base :imm)
                                                (idx :imm)
                                                (val :double-float)))
  (movsd (:%xmm val) (:@ (:%l base) (:%l idx) 2)))

(define-x8632-vinsn pop-outgoing-arg (((n :u16const))
                                      ())
  (popl (:@ (:apply * n (- x8632::node-size)) (:%l x8632::ebp))))

(define-x8632-vinsn slide-nth-arg (()
                                   ((n :u16const)
                                    (nstackargs :u16const)
                                    (temp :lisp)))
  (movl (:@ (:apply * (:apply - nstackargs (:apply + 1 n)) x8632::node-size) (:%l x8632::esp)) (:%l temp))
  (movl (:%l temp) (:@ (:apply * (:apply + n 1) (- x8632::node-size)) (:%l x8632::ebp))))

(define-x8632-vinsn set-tail-vsp (((n :u16const))
                                  ())
  ((:pred = 0 n)
   (movl (:%l x8632::ebp) (:%l x8632::esp)))
  ((:not (:pred = 0 n))
   (leal (:@ (:apply * n (- x8632::node-size)) (:%l x8632::ebp)) (:%l x8632::esp))))

;;; If we've have outgoing arguments in a tail call and are calling
;;; some function (rather than jumping to an internal entry point), we
;;; need to push the caller's return address and unlink its frame
;;; pointer.
(define-x8632-vinsn prepare-tail-call (()
                                       ())
  (pushl (:@ x8632::node-size (:%l x8632::ebp)))
  (movl (:@ (:% x8632::ebp)) (:% x8632::ebp)))

(define-x8632-vinsn set-carry-if-fixnum-in-range
    (((idx :u32))
     ((reg :imm)
      (minval :s32const)
      (maxval :u32const)))
  (movl (:%l reg) (:%l idx))
  (sarl (:$ub x8632::fixnumshift) (:%l idx))
  ((:not (:pred zerop minval))
   ((:and (:pred < minval 128) (:pred >= minval -128))
    (subl (:$b minval) (:%l idx)))
   ((:not (:and (:pred < minval 128) (:pred >= minval -128)))
    (subl (:$l minval) (:%l idx))))
  ((:pred < maxval 128)
   (cmpl (:$b maxval) (:%l idx)))
  ((:pred >= maxval 128)
   (cmpl (:$l maxval) (:%l idx))))

(define-x8632-vinsn (ijmp :branch) (((reg :u32))
                                    ((reg :u32)
                                     (count :s32const))
                                    ((rjmp :lisp)))
  (movl (:@ (:^ :jtab) (:%l x8632::fn) (:%l reg) 4) (:%l reg))
  (leal (:@ (:%l x8632::fn) (:%l reg)) (:%l rjmp))
  (jmp (:%l rjmp))
  (:uuo-section)
  (:align 2)
  (:long count)
  :jtab)

(define-x8632-vinsn jtabentry (()
                               ((label :label)))
  (:uuo-section)
  (:long (:^ label)))

(define-x8632-vinsn ivector-typecode-p (((dest :lisp))
                                        ((src :lisp))
                                        ((temp :u32)))
  (movl (:% src) (:% temp))
  (andl (:$l (logior (ash x8632::fulltagmask x8632::fixnumshift) x8632::tagmask)) (:% temp))
  (cmpl (:$l (ash x8632::fulltag-immheader x8632::fixnumshift)) (:% temp))
  (movl (:$l 0) (:% temp))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
     (cmovel (:% src) (:% dest)))
  (cmovnel (:% temp) (:% dest)))
                                        
(define-x8632-vinsn gvector-typecode-p (((dest :lisp))
                                        ((src :lisp))
                                        ((temp :u32)))
  (movl (:% src) (:% temp))
  (andl (:$l (logior (ash x8632::fulltagmask x8632::fixnumshift) x8632::tagmask)) (:% temp))
  (cmpl (:$l (ash x8632::fulltag-nodeheader x8632::fixnumshift)) (:% temp))
  (movl (:$l 0) (:% temp))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
     (cmovel (:% src) (:% dest)))
  (cmovnel (:% temp) (:% dest)))


(define-x8632-vinsn  %complex-single-float-realpart
    (((dest :single-float))
     ((src :complex-single-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movss (:%xmm src) (:%xmm dest))))

(define-x8632-vinsn  %complex-single-float-imagpart
    (((dest :single-float))
     ((src :complex-single-float)))
  ((:not (:pred = (:apply %hard-regspec-value src ) (:apply %hard-regspec-value dest)))
   (movapd (:%xmm src) (:%xmm dest)))
  (psrlq (:$ub 32) (:%xmm dest)))

(define-x8632-vinsn  %complex-double-float-realpart
    (((dest :double-float))
     ((src :complex-double-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movsd (:%xmm src) (:%xmm dest))))

(define-x8632-vinsn  %complex-double-float-imagpart
    (((dest :double-float))
     ((src :complex-double-float)))
  ((:not (:pred = (:apply %hard-regspec-value src ) (:apply %hard-regspec-value dest)))
   (movapd (:%xmm src) (:%xmm dest)))
  (shufpd (:$ub 1) (:%xmm x8632::fpzero) (:%xmm dest)))


(define-x8632-vinsn %make-complex-single-float
    (((dest :complex-single-float))
     ((r :single-float)
      (i :single-float)))
  ((:not (:pred = (:apply %hard-regspec-value r) (:apply %hard-regspec-value dest)))
   (movss (:%xmm r) (:%xmm dest)))
  (unpcklps (:%xmm i) (:%xmm dest)))

(define-x8632-vinsn %make-complex-double-float
    (((dest :complex-double-float))
     ((r :double-float)
     (i :double-float)))
  ((:not (:pred = (:apply %hard-regspec-value r) (:apply %hard-regspec-value dest)))
   (:movupd (:%xmm r) (:%xmm dest)))
  (shufpd (:$ub 0) (:%xmm i) (:%xmm dest)))
  




(queue-fixup
 (fixup-x86-vinsn-templates
  *x8632-vinsn-templates*
  x86::*x86-opcode-template-lists* *x8632-backend*))

(provide "X8632-VINSNS")
