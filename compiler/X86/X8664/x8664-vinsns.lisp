;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 2005-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "VINSN")
  (require "X8664-BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "X8664ENV"))

(defmacro define-x8664-vinsn (vinsn-name (results args &optional temps) &body body)
  (%define-vinsn *x8664-backend* vinsn-name results args temps body))


    
(define-x8664-vinsn scale-32bit-misc-index (((dest :u64))
					    ((idx :imm)	; A fixnum
					     )
					    ())
  (movq (:%q idx) (:%q dest))
  (shrq (:$1 1) (:%q dest)))

(define-x8664-vinsn scale-16bit-misc-index (((dest :u64))
					    ((idx :imm)))

  (movq (:%q idx) (:%q dest))
  (shrq (:$ub 2) (:%q dest)))

(define-x8664-vinsn scale-8bit-misc-index (((dest :u64))
					    ((idx :imm)	; A fixnum
					     )
					    ())
  (movq (:%q idx) (:%q dest))
  (shrq (:$ub 3) (:%q dest)))

;;; same as above, but looks better in bit vector contexts
(define-x8664-vinsn scale-1bit-misc-index (((dest :u64))
					    ((idx :imm)	; A fixnum
					     )
					    ())
  (movq (:%q idx) (:%q dest))
  (shrq (:$ub 3) (:%q dest)))

(define-x8664-vinsn misc-ref-u64  (((dest :u64))
                                  ((v :lisp)
                                   (scaled-idx :imm)))
  (movq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))

(define-x8664-vinsn misc-ref-double-float  (((dest :double-float))
                                            ((v :lisp)
                                             (scaled-idx :imm)))
  (movsd (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%xmm dest)))

(define-x8664-vinsn misc-ref-complex-single-float  (((dest :complex-single-float))
                                                    ((v :lisp)
                                                     (scaled-idx :imm)))
  (movq (:@ x8664::complex-single-float.realpart (:%q v) (:%q scaled-idx)) (:%xmm dest)))

(define-x8664-vinsn misc-ref-complex-double-float  (((dest :complex-double-float))
                                                    ((v :lisp)
                                                     (scaled-idx :imm)))
  (movdqu (:@ x8664::complex-double-float.realpart (:%q v) (:%q scaled-idx) 2) (:%xmm dest)))

(define-x8664-vinsn misc-ref-c-double-float  (((dest :double-float))
                                              ((v :lisp)
                                             (idx :s32const)))
  (movsd (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%xmm dest)))


(define-x8664-vinsn misc-ref-c-complex-single-float  (((dest :complex-single-float))
                                                       ((v :lisp)
                                                        (idx :s32const)))
  (movq (:@ (:apply + x8664::complex-single-float.realpart (:apply ash idx x8664::word-shift)) (:%q v)) (:%xmm dest)))

(define-x8664-vinsn misc-ref-node  (((dest :lisp))
                                    ((v :lisp)
                                     (scaled-idx :imm)))
  (movq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))

(define-x8664-vinsn (push-misc-ref-node :push :node :vsp)  (()
                                                            ((v :lisp)
                                                             (scaled-idx :imm)))
  (pushq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn misc-set-node (()
				   ((val :lisp)
				    (v :lisp)
				    (unscaled-idx :imm))
				   ())
  (movq (:%q val) (:@ x8664::misc-data-offset (:%q  v) (:%q unscaled-idx))))

(define-x8664-vinsn misc-set-immediate-node (()
                                             ((val :s32const)
                                              (v :lisp)
                                              (unscaled-idx :imm))
                                            ())
  (movq (:$l val) (:@ x8664::misc-data-offset (:%q  v) (:%q unscaled-idx))))


(define-x8664-vinsn misc-set-double-float (()
				   ((val :double-float)
				    (v :lisp)
				    (unscaled-idx :imm))
				   ())
  (movsd (:%xmm val) (:@ x8664::misc-data-offset (:%q  v) (:%q unscaled-idx))))

(define-x8664-vinsn misc-set-complex-single-float (()
				   ((val :complex-single-float)
				    (v :lisp)
				    (unscaled-idx :imm))
				   ())
  (movq (:%xmm val) (:@ x8664::complex-single-float.realpart (:%q  v) (:%q unscaled-idx))))

(define-x8664-vinsn misc-set-complex-double-float (()
                                                   ((val :complex-double-float)
                                                    (v :lisp)
                                                    (unscaled-idx :imm))
				   ())
  (movdqu (:%xmm val) (:@ x8664::complex-double-float.realpart (:%q  v) (:%q unscaled-idx) 2)))

(define-x8664-vinsn misc-ref-u8 (((dest :u8))
                                 ((v :lisp)
                                  (scaled-idx :s64)))
  (movzbl (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%l dest)))

(define-x8664-vinsn misc-ref-s8 (((dest :s8))
                                 ((v :lisp)
                                  (scaled-idx :s64)))
  (movsbq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))

(define-x8664-vinsn misc-ref-u16 (((dest :u16))
                                  ((v :lisp)
                                   (scaled-idx :s64)))
  (movzwl (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%l dest)))

(define-x8664-vinsn misc-ref-u32 (((dest :u32))
                                  ((v :lisp)
                                   (scaled-idx :s64)))
  (movl (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%l dest)))


(define-x8664-vinsn misc-ref-single-float (((dest :single-float))
                                           ((v :lisp)
                                            (scaled-idx :s64)))
  (movss(:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%xmm dest)))

(define-x8664-vinsn misc-ref-s32 (((dest :s32))
                                  ((v :lisp)
                                   (scaled-idx :s64)))
  (movslq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))

(define-x8664-vinsn misc-ref-s16 (((dest :s16))
                                  ((v :lisp)
                                   (scaled-idx :s64)))
  (movswq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))

(define-x8664-vinsn misc-ref-s64  (((dest :s64))
                                  ((v :lisp)
                                   (scaled-idx :imm)))
  (movq (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx)) (:%q dest)))


(define-x8664-vinsn misc-ref-c-node  (((dest :lisp))
				     ((v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%q dest)))


(define-x8664-vinsn (push-misc-ref-c-node :push :node :vsp)
    (()
     ((v :lisp)
      (idx :u32const)) ; sic
     ())
  (pushq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v))))

(define-x8664-vinsn misc-ref-c-u64  (((dest :u64))
				     ((v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%q dest)))


(define-x8664-vinsn misc-ref-c-s64  (((dest :s64))
				     ((v :lisp)
				      (idx :s32const)) ; sic
				     ())
  (movq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%q dest)))


(define-x8664-vinsn misc-ref-c-u32  (((dest :u32))
				     ((v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movl (:@ (:apply + x8664::misc-data-offset (:apply ash idx 2)) (:%q v)) (:%l dest)))

(define-x8664-vinsn misc-ref-c-s32  (((dest :s32))
				     ((v :lisp)
				      (idx :s32const)) ; sic
				     ())
  (movslq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q v)) (:%q dest)))

(define-x8664-vinsn misc-ref-c-single-float  (((dest :single-float))
                                              ((v :lisp)
                                               (idx :s32const)) ; sic
                                              ())
  (movss (:@ (:apply + x8664::misc-data-offset (:apply ash idx 2)) (:%q v)) (:%xmm dest)))

(define-x8664-vinsn misc-ref-c-u8  (((dest :u64))
				     ((v :lisp)
				      (idx :s32const)) ; sic
				     ())
  (movzbl (:@ (:apply + x8664::misc-data-offset idx) (:%q v)) (:%l dest)))

(define-x8664-vinsn misc-ref-c-s8  (((dest :s64))
				     ((v :lisp)
				      (idx :s32const)) ; sic
				     ())
  (movsbq (:@ (:apply + x8664::misc-data-offset idx) (:%q v)) (:%q dest)))

(define-x8664-vinsn misc-set-u64 (()
                                  ((val :u64)
                                   (v :lisp)
                                   (idx :u64)))
  (movq (:%q val) (:@ x8664::misc-data-offset (:%q v) (:%q idx))))

(define-x8664-vinsn misc-set-immediate-u64 (()
                                            ((val :u32const)
                                             (v :lisp)
                                             (idx :u64)))
  (movq (:$l val) (:@ x8664::misc-data-offset (:%q v) (:%q idx))))

(define-x8664-vinsn misc-set-c-u64 (()
				    ((val :u64)
				     (v :lisp)
				     (idx :u32const)))
  (movq (:%q val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 3)) (:%q v))))

(define-x8664-vinsn misc-set-immediate-c-u64 (()
                                              ((val :u32const)
                                               (v :lisp)
                                               (idx :u32const)))
  (movq (:$l val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 3)) (:%q v))))

(define-x8664-vinsn misc-set-s64 (()
                                  ((val :s64)
                                   (v :lisp)
                                   (scaled-idx :imm)))
  (movq (:%q val) (:@ x8664::misc-data-offset  (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn misc-set-immediate-s64 (()
                                            ((val :s32const)
                                             (v :lisp)
                                             (scaled-idx :imm)))
  (movq (:$l val) (:@ x8664::misc-data-offset  (:%q v) (:%q scaled-idx))))


(define-x8664-vinsn misc-set-c-s64 (()
				    ((val :s64)
				     (v :lisp)
				     (idx :s32const)))
  (movq (:%q val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 3)) (:%q v))))

(define-x8664-vinsn misc-set-immediate-c-s64 (()
                                              ((val :s32const)
                                               (v :lisp)
                                               (idx :s32const)))
  (movq (:$l val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 3)) (:%q v))))


(define-x8664-vinsn misc-set-c-node (()
				    ((val :lisp)
				     (v :lisp)
				     (idx :s32const)))
  (movq (:%q val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 3)) (:%q v))))




(define-x8664-vinsn misc-set-immediate-c-node (()
                                               ((val :s32const)
                                                (v :lisp)
                                                (idx :s32const)))
  (movq (:$l val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 3)) (:%q v))))

(define-x8664-vinsn set-closure-forward-reference (()
                                                   ((val :lisp)
                                                    (closure :lisp)
                                                    (idx :s32const)))
  (movq (:%q val) (:@ (:apply + x8664::misc-function-offset (:apply ash idx x8664::word-shift)) (:%q closure))))


(define-x8664-vinsn misc-set-c-double-float (()
				    ((val :double-float)
				     (v :lisp)
				     (idx :s32const)))
  (movsd (:%xmm val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 3)) (:%q v))))



(define-x8664-vinsn (call-known-symbol :call) (((result (:lisp x8664::arg_z)))
                                               ()
					       ((entry (:label 1))))
  (:talign 4)
  (call (:@ x8664::symbol.fcell (:% x8664::fname)))
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn)))


(define-x8664-vinsn (jump-known-symbol :jumplr) (()
                                                 ())

  (jmp (:@ x8664::symbol.fcell (:% x8664::fname))))

(define-x8664-vinsn set-nargs (()
			       ((n :s16const)))
  ((:pred = n 0)
   (xorl (:%l x8664::nargs ) (:%l x8664::nargs )))
  ((:not (:pred = n 0))
   (movl (:$l (:apply ash n x8664::word-shift)) (:%l x8664::nargs ))))

(define-x8664-vinsn check-exact-nargs (()
                                       ((n :u16const)))
  :resume
  ((:pred = n 0)
   (testl (:%l x8664::nargs) (:%l x8664::nargs)))
  ((:not (:pred = n 0))
   ((:pred < n 16)
   (cmpl (:$b (:apply ash n x8664::word-shift)) (:%l x8664::nargs)))
   ((:pred >= n 16)
    (cmpl (:$l (:apply ash n x8664::word-shift)) (:%l x8664::nargs))))
  (jne :bad)
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-wrong-number-of-args)))

(define-x8664-vinsn check-min-nargs (()
                                       ((min :u16const)))
  :resume
  ((:pred = min 1)
   (testl (:%l x8664::nargs) (:%l x8664::nargs))
   (je :toofew))
  ((:not (:pred = min 1))
   ((:pred < min 16)
    (rcmpl (:%l x8664::nargs) (:$b (:apply ash min x8664::word-shift))))
   ((:pred >= min 16)
    (rcmpl (:%l x8664::nargs) (:$l (:apply ash min x8664::word-shift))))
   (jb :toofew))  
  
  (:anchored-uuo-section :resume)
  :toofew
  (:anchored-uuo (uuo-error-too-few-args)))

(define-x8664-vinsn check-max-nargs (()
                                       ((n :u16const)))
  :resume
  ((:pred < n 16)
   (rcmpl (:%l x8664::nargs) (:$b (:apply ash n x8664::word-shift))))
  ((:pred >= n 16)
   (rcmpl (:%l x8664::nargs) (:$l (:apply ash n x8664::word-shift))))
  (ja :bad)
  
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-too-many-args)))


(define-x8664-vinsn check-min-max-nargs (()
                                         ((min :u16const)
                                          (max :u16const)))
  :resume
  ((:pred = min 1)
   (testl (:%l x8664::nargs) (:%l x8664::nargs))
   (je :toofew))
  ((:not (:pred = min 1))
   ((:pred < min 16)
    (rcmpl (:%l x8664::nargs) (:$b (:apply ash min x8664::word-shift))))
   ((:pred >= min 16)
    (rcmpl (:%l x8664::nargs) (:$l (:apply ash min x8664::word-shift))))
   (jb :toofew))
  ((:pred < max 16)
   (rcmpl (:%l x8664::nargs) (:$b (:apply ash max x8664::word-shift))))
  ((:pred >= max 16)
   (rcmpl (:%l x8664::nargs) (:$l (:apply ash max x8664::word-shift))))
  (ja :toomany)
  
  (:anchored-uuo-section :resume)
  :toofew
  (:anchored-uuo (uuo-error-too-few-args))
  (:anchored-uuo-section :resume)
  :toomany
  (:anchored-uuo (uuo-error-too-many-args)))


(define-x8664-vinsn default-1-arg (()
                                   ((min :u16const)))
  ((:pred < min 16)
   (rcmpl (:%l x8664::nargs) (:$b (:apply ash min x8664::word-shift))))
  ((:pred >= min 16)
   (rcmpl (:%l x8664::nargs) (:$l (:apply ash min x8664::word-shift))))
  (jne :done)
  ((:pred >= min 3)
   (pushq (:%q x8664::arg_x)))
  ((:pred >= min 2)
   (movq (:%q x8664::arg_y) (:%q x8664::arg_x)))
  ((:pred >= min 1)
   (movq (:%q x8664::arg_z) (:%q x8664::arg_y)))
  (movq (:$l (:apply target-nil-value)) (:%q x8664::arg_z))
  :done)


(define-x8664-vinsn default-2-args (()
				    ((min :u16const)))
  ((:pred < (:apply 1+ min) 16)
   (rcmpl (:%l x8664::nargs ) (:$b (:apply ash (:apply 1+ min) x8664::word-shift))))
  ((:pred >= (:apply 1+ min) 16)
   (rcmpl (:%l x8664::nargs ) (:$l (:apply ash (:apply 1+ min) x8664::word-shift))))
  (ja :done)
  (je :one)
  ;; We got "min" args; arg_y & arg_z default to nil
  ((:pred >= min 3)
   (pushq (:%q x8664::arg_x)))   
  ((:pred >= min 2)
   (pushq (:%q x8664::arg_y)))
  ((:pred >= min 1)
   (movq (:%q x8664::arg_z) (:%q x8664::arg_x)))
  (movl (:$l (:apply target-nil-value)) (:%l x8664::arg_y))
  (jmp :last)
  :one
  ;; We got min+1 args: arg_y was supplied, arg_z defaults to nil.
  ((:pred >= min 2)
   (pushq (:%q x8664::arg_x)))
  ((:pred >= min 1)
   (movq (:%q x8664::arg_y) (:%q x8664::arg_x)))
  (movq (:%q x8664::arg_z) (:%q x8664::arg_y))
  :last
  (movq (:$l (:apply target-nil-value)) (:%q x8664::arg_z))
  :done)

(define-x8664-vinsn default-3-args (()
				    ((min :u16const)))
  ((:pred < (:apply + 2 min) 16)
   (rcmpl (:%l x8664::nargs ) (:$b (:apply ash (:apply + 2 min) x8664::word-shift))))
  ((:pred >= (:apply + 2 min) 16)
   (rcmpl (:%l x8664::nargs ) (:$l (:apply ash (:apply + 2 min) x8664::word-shift))))
  (ja :done)
  (je :two)
  ((:pred < min 16)
   (rcmpl (:%l x8664::nargs ) (:$b (:apply ash min x8664::word-shift))))
  ((:pred >= min 16)
   (rcmpl (:%l x8664::nargs ) (:$l (:apply ash min x8664::word-shift))))
  (je :none)
  ;; The first (of three) &optional args was supplied.
  ((:pred >= min 2)
   (pushq (:%q x8664::arg_x)))
  ((:pred >= min 1)
   (pushq (:%q x8664::arg_y)))
  (movq (:%q x8664::arg_z) (:%q x8664::arg_x))
  (jmp :last-2)
  :two
  ;; The first two (of three) &optional args were supplied.
  ((:pred >= min 1)
   (pushq (:%q x8664::arg_x)))
  (movq (:%q x8664::arg_y) (:%q x8664::arg_x))
  (movq (:%q x8664::arg_z) (:%q x8664::arg_y))
  (jmp :last-1)
  ;; None of the three &optional args was provided.
  :none
  ((:pred >= min 3)
   (pushq (:%q x8664::arg_x)))
  ((:pred >= min 2)
   (pushq (:%q x8664::arg_y)))
  ((:pred >= min 1)
   (pushq (:%q x8664::arg_z)))
  (movl (:$l (:apply target-nil-value)) (:%l x8664::arg_x))
  :last-2
  (movl (:$l (:apply target-nil-value)) (:%l x8664::arg_y))
  :last-1
  (movl (:$l (:apply target-nil-value)) (:%l x8664::arg_z))
  :done)


(define-x8664-vinsn default-optionals (()
                                       ((n :u16const))
                                       ((temp :u64)))
  ((:pred < n 16)
   (rcmpl (:%l x8664::nargs) (:$b (:apply ash n x8664::word-shift))))
  ((:pred >= n 16)
   (rcmpl (:%l x8664::nargs) (:$l (:apply ash n x8664::word-shift))))
  (movl (:%l x8664::nargs) (:%l temp))
  (jae :done)
  :loop
  (addl (:$b x8664::fixnumone) (:%l temp))
  ((:pred < n 16)
   (cmpl (:$b (:apply ash n x8664::word-shift)) (:%l temp)))
  ((:pred >= n 16)
   (cmpl (:$l (:apply ash n x8664::word-shift)) (:%l temp)))  
  (pushq (:$l (:apply target-nil-value)))
  (jne :loop)
  :done)
  

(define-x8664-vinsn (save-lisp-context-no-stack-args :uses-frame-pointer) (()
                                                     ())
  (pushq (:%q x8664::rbp))
  (movq (:%q x8664::rsp) (:%q x8664::rbp)))


(define-x8664-vinsn (save-lisp-context-offset :needs-frame-pointer) (()
					      ((nbytes-pushed :s32const)))
  (movq (:%q x8664::rbp) (:@ (:apply + nbytes-pushed x8664::node-size) (:%q x8664::rsp)))
  (leaq (:@ (:apply + nbytes-pushed x8664::node-size) (:%q x8664::rsp)) (:%q x8664::rbp))
  (popq  (:@ x8664::node-size (:%q x8664::rbp))))

(define-x8664-vinsn (save-lisp-context-variable-arg-count :needs-frame-pointer) (()
                                                          ()
                                                          ((temp :u64)))
  (movl (:%l x8664::nargs) (:%l temp))
  (subq (:$b (* $numx8664argregs x8664::node-size)) (:%q temp))
  (jle :push)
  (movq (:%q x8664::rbp) (:@ x8664::node-size (:%q x8664::rsp) (:%q temp)))
  (leaq (:@ x8664::node-size (:%q x8664::rsp) (:%q temp)) (:%q x8664::rbp))
  (popq  (:@ 8 (:%q x8664::rbp)))
  (jmp :done)
  :push
  (pushq (:%q x8664::rbp))
  (movq (:%q x8664::rsp) (:%q x8664::rbp))
  :done)

;;; We know that some args were pushed, but don't know how many were
;;; passed.
(define-x8664-vinsn (save-lisp-context-in-frame :needs-frame-pointer) (()
                                                ()
                                                ((temp :u64)))
  (movl (:%l x8664::nargs) (:%l temp))
  (subq (:$b (* $numx8664argregs x8664::node-size)) (:%q temp))
  (movq (:%q x8664::rbp) (:@ x8664::node-size (:%q x8664::rsp) (:%q temp)))
  (leaq (:@ x8664::node-size (:%q x8664::rsp) (:%q temp)) (:%q x8664::rbp))
  (popq  (:@ x8664::node-size (:%q x8664::rbp))))


(define-x8664-vinsn (vpush-register :push :node :vsp)
    (()
     ((reg :lisp)))
  (pushq (:% reg)))

(define-x8664-vinsn (vpush-fixnum :push :node :vsp)
    (()
     ((const :s32const)))
  ((:and  (:pred < const 128) (:pred >= const -128))
   (pushq (:$b const)))
  ((:not (:and  (:pred < const 128) (:pred >= const -128)))
   (pushq (:$l const))))



(define-x8664-vinsn (vframe-load :vsp :ref :needs-frame-pointer)
    (((dest :lisp))
     ((frame-offset :stack-offset)
      (cur-vsp :stack-offset)))
  (movq (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp)) (:%q dest)))

(define-x8664-vinsn (compare-vframe-offset-to-nil :vsp :ref :needs-frame-pointer)
    (()
     ((frame-offset :u16const)
      (cur-vsp :u16const)))
  (cmpb (:$b x8664::fulltag-nil) (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp))))

(define-x8664-vinsn (compare-vframe-offset-to-fixnum :vsp :ref :needs-frame-pointer)
    (()
     ((frame-offset :stack-offset)
      (cur-vsp :stack-offset)
      (fixval :s32const)))
  ((:and (:pred < fixval 128) (:pred >= fixval -128))
   (cmpq (:$b fixval) (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp))))
  ((:not (:and (:pred < fixval 128) (:pred >= fixval -128)))
   (cmpq (:$l fixval) (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp)))))


(define-x8664-vinsn (add-constant-to-vframe-offset :vsp :ref :set :needs-frame-pointer)
    (()
     ((frame-offset :stack-offset)
      (cur-vsp :stack-offset)
      (constant :s32const)))
  ((:and (:pred < constant 128) (:pred >= constant -128))
   (addq (:$b constant) (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp))))
  ((:not (:and (:pred < constant 128) (:pred >= constant -128)))
   (addq (:$l constant) (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp)))))
  

(define-x8664-vinsn compare-value-cell-to-nil (()
                                               ((vcell :lisp)))
  (cmpb (:$b x8664::fulltag-nil) (:@ x8664::value-cell.value (:%q vcell))))



(define-x8664-vinsn (vframe-push :push :node :vsp :ref :needs-frame-pointer)
    (()
     ((frame-offset :stack-offset)
      (cur-vsp :stack-offset)))
  (pushq (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp))))

(define-x8664-vinsn (vframe-store :vsp :set :needs-frame-pointer)
    (()
     ((src :lisp)
      (frame-offset :stack-offset)
      (cur-vsp :stack-offset)))
  (movq (:%q src) (:@ (:apply - (:apply + frame-offset x8664::word-size-in-bytes)) (:%q x8664::rbp))))


        




(define-x8664-vinsn (restore-full-lisp-context :lispcontext :pop :vsp :uses-frame-pointer)
    (()
     ())
  (movq (:%q x8664::rbp) (:%q x8664::rsp))
  (popq (:%q x8664::rbp))
  )

(define-x8664-vinsn compare-to-nil (()
                                    ((arg0 t)))
  (cmpq (:$l (target-nil-value)) (:%q arg0)))

(define-x8664-vinsn compare-to-t (()
                                    ((arg0 t)))
  (cmpq (:$l (:apply target-t-value)) (:%q arg0)))

(define-x8664-vinsn (ref-constant :constant-ref) (((dest :lisp))
                                  ((lab :label)))
  (movq (:@ (:^ lab) (:%q x8664::fn)) (:%q dest)))

(define-x8664-vinsn compare-constant-to-register (()
                                                  ((lab :label)
                                                   (reg :lisp)))
  (cmpq (:@ (:^ lab) (:%q x8664::fn)) (:%q reg)))


(define-x8664-vinsn (vpush-constant :push :node :vsp) (()
                                                       ((lab :label)))
  (pushq (:@ (:^ lab) (:%q x8664::fn))))

  
(define-x8664-vinsn (jump :jump)
    (()
     ((label :label)))
  (jmp label))

(define-x8664-vinsn (cbranch-true :branch) (()
					    ((label :label)
					     (crbit :u8const)))
  (jcc (:$ub crbit) label))

(define-x8664-vinsn (cbranch-false :branch) (()
					     ((label :label)
					      (crbit :u8const)))
  (jcc (:$ub (:apply logxor 1 crbit)) label))


(define-x8664-vinsn (lri :constant-ref) (((dest :imm))
                                         ((intval :s64const))
                                         ())
  ((:pred = intval 0)
   (xorl (:%l dest) (:%l dest)))
  ((:and (:pred /= intval 0)
         (:pred >= intval  -2147483648)
         (:pred <= intval #xffffffff))
   ((:pred > intval 0)
    (movl (:$l intval) (:%l dest)))
   ((:pred < intval 0)
    (movq (:$l intval) (:%q dest))))
  ((:or (:pred < intval  -2147483648)
        (:pred > intval #xffffffff))
   (movq (:$q (:apply logand #xffffffffffffffff intval)) (:%q dest))))

(define-x8664-vinsn (lriu :constant-ref) (((dest :imm))
                                         ((intval :u64const))
                                         ())
  ((:pred = intval 0)
   (xorl (:%l dest) (:%l dest)))
  ((:and (:pred > intval 0)
         (:pred <= intval #xffffffff))
   (movl (:$l intval) (:%l dest)))
  ((:pred > intval #xffffffff)
   (movq (:$q (:apply logand #xffffffffffffffff intval)) (:%q dest))))

(define-x8664-vinsn trap-unless-bit (()
                                     ((value :lisp)))
  :resume
  (testq (:$l (lognot x8664::fixnumone)) (:%q value))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q value) (:$ub arch::error-object-not-bit)))
  )

(define-x8664-vinsn trap-unless-list (()
				      ((object :lisp))
				      ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-list) (:%l tag))
  (jne :bad)
  
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-list (:%q object))))



(define-x8664-vinsn trap-unless-cons (()
                                         ((object :lisp))
                                         ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::fulltagmask) (:%l tag))
  (cmpl (:$b x8664::fulltag-cons) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%q object) (:$ub x8664::fulltag-cons))))

(define-x8664-vinsn set-z-flag-if-consp (()
                                         ((object :lisp))
                                         ((tag :u8)))
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::fulltagmask) (:%l tag))
  (cmpl (:$b x8664::fulltag-cons) (:%l tag)))

(define-x8664-vinsn trap-unless-uvector (()
                                         ((object :lisp))
                                         ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%q object) (:$ub x8664::tag-misc))))
  
(define-x8664-vinsn trap-unless-single-float (()
                                              ((object :lisp)))
  :resume
  (cmpb (:$b x8664::tag-single-float) (:%b object))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%q object) (:$ub x8664::tag-single-float))))

(define-x8664-vinsn trap-unless-character (()
                                              ((object :lisp)))
  :resume
  (cmpb (:$b x8664::subtag-character) (:%b object))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%q object) (:$ub x8664::subtag-character))))

(define-x8664-vinsn trap-unless-fixnum (()
                                        ((object :lisp))
                                        ())
  :resume
  (testb (:$b x8664::tagmask) (:%b object))
  (jne :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-fixnum (:%q object))))

(define-x8664-vinsn set-flags-from-lisptag (()
                                            ((reg :lisp)))
  (testb (:$b x8664::tagmask) (:%b reg)))
                                            

(define-x8664-vinsn trap-unless-typecode= (()
					   ((object :lisp)
					    (tagval :u16const))
					   ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :have-tag)
  ;; This needs to be a sign-extending mov, since the cmpl below
  ;; will sign-extend the 8-bit constant operand.
  (movsbl (:@ x8664::misc-subtag-offset (:%q object)) (:%l tag))
  :have-tag
  (cmpl (:$b tagval) (:%l tag))
  (jne :bad)
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%q object) (:$ub tagval))))

(define-x8664-vinsn trap-unless-double-float (()
                                              ((object :lisp))
                                              ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :have-tag)
  (movsbl (:@ x8664::misc-subtag-offset (:%q object)) (:%l tag))
  :have-tag
  (cmpl (:$b x8664::subtag-double-float) (:%l tag))
  (jne :bad)
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%q object) (:$ub x8664::subtag-double-float))))

(define-x8664-vinsn trap-unless-complex-double-float (()
                                                      ((object :lisp))
                                                      ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :have-tag)
  (movsbl (:@ x8664::misc-subtag-offset (:%q object)) (:%l tag))
  :have-tag
  (cmpl (:$b x8664::subtag-complex-double-float) (:%l tag))
  (jne :bad)
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%q object) (:$ub x8664::subtag-complex-double-float))))

(define-x8664-vinsn trap-unless-complex-single-float (()
                                                      ((object :lisp))
                                                      ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :have-tag)
  (movsbl (:@ x8664::misc-subtag-offset (:%q object)) (:%l tag))
  :have-tag
  (cmpl (:$b x8664::subtag-complex-single-float) (:%l tag))
  (jne :bad)
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%q object) (:$ub x8664::subtag-complex-single-float))))




(define-x8664-vinsn trap-unless-macptr (()
                                        ((object :lisp))
                                        ((tag :u8)))
  :resume
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :have-tag)
  (movzbl (:@ x8664::misc-subtag-offset (:%q object)) (:%l tag))
  :have-tag
  (cmpb (:$b x8664::subtag-macptr) (:%b tag))
  (jne :bad)
  
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%q object) (:$ub x8664::subtag-macptr))))


(define-x8664-vinsn check-misc-bound (()
				      ((idx :imm)
				       (v :lisp))
				      ((temp :u64)))
  :resume
  (movq (:@ x8664::misc-header-offset (:%q v)) (:%q temp))
  (shrq (:$ub x8664::num-subtag-bits) (:%q temp))
  (shlq (:$ub x8664::fixnumshift) (:%q temp))
  (rcmpq (:%q idx) (:%q temp))
  (jae :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-vector-bounds (:%q idx) (:%q v))))

(define-x8664-vinsn check-vector-header-bound (()
                                               ((v :lisp)
                                                (idx :imm)))
  :resume
  (cmpq (:@ x8664::vectorH.physsize (:%q v)) (:%q idx))
  (jae :bad)
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-vector-bounds (:%q idx) (:%q v))))

(define-x8664-vinsn deref-vector-header (((vector :lisp)
                                          (index :lisp))
                                         ((vector :lisp)
                                          (index :lisp)))
  :again
  (addq (:@ x8664::vectorH.displacement (:%q vector)) (:%q index))
  (btw (:$ub (+ x8664::fixnumshift $arh_disp_bit)) (:@ x8664::vectorH.flags (:%q vector)))
  (movq (:@ x8664::vectorH.data-vector (:%q vector)) (:%q vector))
  (jb :again))


(define-x8664-vinsn %cdr (((dest :lisp))
			  ((src :lisp)))
  (movq (:@ x8664::cons.cdr (:%q src)) (:%q dest)))

(define-x8664-vinsn (%vpush-cdr :push :node :vsp)
    (()
     ((src :lisp)))
  (pushq (:@ x8664::cons.cdr (:%q src))))

(define-x8664-vinsn %car (((dest :lisp))
			  ((src :lisp)))
  (movq (:@ x8664::cons.car (:%q src)) (:%q dest)))

(define-x8664-vinsn (%vpush-car :push :node :vsp)
    (()
     ((src :lisp)))
  (pushq (:@ x8664::cons.car (:%q src))))


(define-x8664-vinsn u32->char (((dest :lisp)
                               (src :u8))
			      ((src :u8))
			      ())
  (shll (:$ub x8664::charcode-shift) (:%l src))
  (leaq  (:@ x8664::subtag-character (:%q src)) (:%q dest)))


(define-x8664-vinsn (load-nil :constant-ref) (((dest t))
					      ())
  (movl (:$l (:apply target-nil-value)) (:%l dest)))


(define-x8664-vinsn (load-t :constant-ref) (((dest t))
					    ())
  (movl(:$l (:apply target-t-value)) (:%l dest)))


(define-x8664-vinsn extract-tag (((tag :u8))
                                 ((object :lisp)))
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag)))

(define-x8664-vinsn extract-tag-fixnum (((tag :imm))
					((object :lisp)))
  ((:pred =
          (:apply %hard-regspec-value tag)
          (:apply %hard-regspec-value object))
   (shll (:$ub x8664::fixnumshift) (:%l object)))
  ((:not (:pred =
          (:apply %hard-regspec-value tag)
          (:apply %hard-regspec-value object)))
   (imull (:$b x8664::fixnumone) (:%l object) (:%l tag)))
  (andl (:$b (ash x8664::tagmask x8664::fixnumshift)) (:%l tag)))

(define-x8664-vinsn extract-fulltag (((tag :u8))
                                 ((object :lisp)))
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::fulltagmask) (:%l tag)))

(define-x8664-vinsn extract-fulltag-fixnum (((tag :imm))
                                            ((object :lisp)))
  ((:pred =
          (:apply %hard-regspec-value tag)
          (:apply %hard-regspec-value object))
   (shll (:$ub x8664::fixnumshift) (:%l object)))
  ((:not (:pred =
          (:apply %hard-regspec-value tag)
          (:apply %hard-regspec-value object)))
   (imull (:$b x8664::fixnumone) (:%l object) (:%l tag)))
  (andl (:$b (ash x8664::fulltagmask x8664::fixnumshift)) (:%l tag)))

(define-x8664-vinsn extract-typecode (((tag :u32))
                                      ((object :lisp)))
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :have-tag)
  (movzbl (:@ x8664::misc-subtag-offset (:%q object)) (:%l tag))
  :have-tag)

(define-x8664-vinsn extract-typecode-fixnum (((tag :imm))
                                             ((object :lisp))
                                             ((temp :u32)))
  (movl (:%l object) (:%l temp))
  (andl (:$b x8664::tagmask) (:%l temp))
  (cmpl (:$b x8664::tag-misc) (:%l temp))
  (jne :have-tag)
  (movzbl (:@ x8664::misc-subtag-offset (:%q object)) (:%l temp))
  :have-tag
  (imulq (:$b x8664::fixnumone) (:%q temp) (:%q tag)))


(define-x8664-vinsn compare-reg-to-zero (()
                                         ((reg :imm)))
  (testq (:%q reg) (:%q reg)))

(define-x8664-vinsn compare-u8-reg-to-zero (()
                                            ((reg :u8)))
  (testb (:%b reg) (:%b reg)))

(define-x8664-vinsn cr-bit->boolean (((dest :lisp))
                                     ((crbit :u8const))
                                     ((true :u64)))
  (movl (:$l (:apply target-t-value)) (:%l true))
  (movl (:$l (:apply target-nil-value)) (:%l dest))
  (cmovccl (:$ub crbit) (:%l true) (:%l dest)))





(define-x8664-vinsn compare-s32-constant (()
                                            ((val :imm)
                                             (const :s32const)))
  ((:or  (:pred < const -128) (:pred > const 127))
   (rcmpq (:%q val) (:$l const)))
  ((:not (:or  (:pred < const -128) (:pred > const 127)))
   (rcmpq (:%q val) (:$b const))))

(define-x8664-vinsn compare-u31-constant (()
                                          ((val :u64)
                                           (const :u32const)))
  ((:pred > const 127)
   (rcmpq (:%q val) (:$l const)))
  ((:not (:pred > const 127))
   (rcmpq (:%q val) (:$b const))))

(define-x8664-vinsn compare-u8-constant (()
                                         ((val :u8)
                                          (const :u8const)))
  #|
  ((:pred logbitp 7 const)
   (movzbl (:%b val) (:%l val))
   (rcmpw (:%w val) (:$w const)))
  ((:not (:pred logbitp 7 const))
   (rcmpb (:%b val) (:$b const)))
  ||#
  (rcmpb (:%b val) (:$b const))
  )

(define-x8664-vinsn allocate-cons-cell (((allocptr :lisp)) ; must be x8664::allocptr
                                        ())
  (subq (:$b (- x8664::cons.size x8664::fulltag-cons)) (:rcontext x8664::tcr.save-allocptr))
  (movq (:rcontext x8664::tcr.save-allocptr) (:%q allocptr))
  (rcmpq (:%q allocptr) (:rcontext x8664::tcr.save-allocbase))
  (:byte #x77) (:byte #x02)             ;(ja :no-trap)

  (uuo-alloc)
  :no-trap

  (andb (:$b (lognot x8664::fulltagmask)) (:rcontext x8664::tcr.save-allocptr))
)

(define-x8664-vinsn init-cons (((dest :lisp))
                               ((car :lisp)
                                (cdr :lisp)
                                (cell :lisp)))
  (movq (:%q car) (:@ x8664::cons.car (:%q cell)))
  (movq (:%q cdr) (:@ x8664::cons.cdr (:%q cell)))
  (movq (:%q cell) (:%q dest)))

(define-x8664-vinsn cons (((dest :lisp))
                          ((car :lisp)
                           (cdr :lisp))
			  ((allocptr (:lisp #.x8664::allocptr))))
  (subq (:$b (- x8664::cons.size x8664::fulltag-cons)) (:rcontext x8664::tcr.save-allocptr))
  (movq (:rcontext x8664::tcr.save-allocptr) (:%q allocptr))
  (rcmpq (:%q allocptr) (:rcontext x8664::tcr.save-allocbase))
  (:byte #x77) (:byte #x02)             ;(ja :no-trap)
  (uuo-alloc)
  :no-trap

  (andb (:$b (lognot x8664::fulltagmask)) (:rcontext x8664::tcr.save-allocptr))
  (movq (:%q car) (:@ x8664::cons.car (:%q allocptr)))
  (movq (:%q cdr) (:@ x8664::cons.cdr (:%q allocptr)))
  ((:pred /= (:apply %hard-regspec-value dest) (:apply %hard-regspec-value x8664::allocptr)) 
   (movq (:%q allocptr) (:%q dest))))

(define-x8664-vinsn make-vcell  (((dest :lisp))
                                 ((val :lisp))
                                 ((allocptr (:lisp #.x8664::allocptr))
                                  (disp (:s64 #.x8664::imm1))
                                  (header (:u64 #.x8664::imm0))))
  (movl (:$l x8664::value-cell-header) (:%l header))
  (movl (:$l (- x8664::value-cell.size x8664::fulltag-misc)) (:%l disp))
  (subq (:%q disp) (:rcontext x8664::tcr.save-allocptr))
  (movq (:rcontext x8664::tcr.save-allocptr) (:%q allocptr))
  (rcmpq (:%q allocptr) (:rcontext x8664::tcr.save-allocbase))
  (:byte #x77) (:byte #x02)             ;(ja :no-trap)
  (uuo-alloc)
  :no-trap
  (movq (:%q header) (:@ x8664::misc-header-offset (:%q allocptr)))
  (andb (:$b (lognot x8664::fulltagmask)) (:rcontext x8664::tcr.save-allocptr))

  (movq (:%q allocptr) (:%q dest))
  (movq (:%q ) (:@ x8664::value-cell.value (:%q dest))))
  
(define-x8664-vinsn ref (()
                         ((arg t)))
  )



(define-x8664-vinsn (spill :spill :needs-frame-pointer) (()
                           ((src :lisp)
                            (slot :stack-offset)))
  (movq (:%q src) (:@ (:apply - (:apply ash (:apply 1+ slot) 3)) (:%q x8664::rbp))))

(define-x8664-vinsn (spill-natural  :spill :nfp :set) (()
                                                        ((val :u64)
                                                         (offset :u16const))
                                                       ((nfp :lisp)))
  (movq (:rcontext x8664::tcr.nfp) (:%q nfp))
  (movq (:%q val) (:@ (:apply + 16 offset) (:%q nfp))))



(define-x8664-vinsn (spill-single-float  :spill :nfp :set) (()
                                                            ((val :single-float)
                                                             (offset :u16const))
                                                            ((nfp :lisp)))
  (movq (:rcontext x8664::tcr.nfp) (:%q nfp))
  (movss (:%xmm val) (:@ (:apply + 16 offset) (:%q nfp))))




(define-x8664-vinsn (spill-double-float  :spill :nfp :set) (()
                                                            ((val :double-float)
                                                             (offset :u16const))
                                                            ((nfp :lisp)))
  (movq (:rcontext x8664::tcr.nfp) (:%q nfp))
  (movsd (:%xmm val) (:@ (:apply + 16 offset) (:%q nfp))))




(define-x8664-vinsn (spill-complex-single-float :spill :nfp :set) (()
                                                                   ((val :complex-single-float)
                                                                    (offset :u16const))
                                                                   ((nfp :lisp)))
                                                                   
                                                    
  (movq (:rcontext x8664::tcr.nfp) (:%q nfp))
  (movq (:%xmm val) (:@ (:apply + 16 offset) (:%q nfp))))

(define-x8664-vinsn (spill-complex-double-float :spill :nfp :set)
    (()
     ((val :complex-double-float)
      (offset :u16const))
     ((temp :imm)))
  (movq (:rcontext x8664::tcr.nfp) (:%q temp))
  (movdqu (:%xmm val) (:@ (:apply + 16 offset) (:%q temp))))

(define-x8664-vinsn (reload :reload :needs-frame-pointer) (((dest :lisp))
                           ((slot :stack-offset)))
  
  (movq (:@ (:apply - (:apply ash (:apply 1+ slot) 3)) (:%q x8664::rbp)) (:%q dest)))

(define-x8664-vinsn unbox-u8 (((dest :u8))
			      ((src :lisp)))
  :resume
  (movq (:$l (lognot (ash #xff x8664::fixnumshift))) (:%q dest))
  (andq (:% src) (:% dest))
  (jne :bad)
  (movq (:%q src) (:%q dest))
  (shrq (:$ub x8664::fixnumshift) (:%q dest))

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-unsigned-byte-8))))

(define-x8664-vinsn %unbox-u8 (((dest :u8))
			      ((src :lisp)))
  (movl (:%l src) (:%l dest))
  (shrl (:$ub x8664::fixnumshift) (:%l dest))
  (movzbl (:%b dest) (:%l dest)))

(define-x8664-vinsn unbox-s8 (((dest :s8))
			      ((src :lisp)))
  :resume
  (movq (:%q src) (:%q dest))
  (shlq (:$ub (- x8664::nbits-in-word (+ 8 x8664::fixnumshift))) (:%q dest))
  (sarq (:$ub (- x8664::nbits-in-word (+ 8 x8664::fixnumshift))) (:%q dest))
  (cmpq (:%q src) (:%q dest))
  (jne :bad)
  (testb (:$b x8664::fixnummask) (:%b dest))
  (jne :bad)
  (sarq (:$ub x8664::fixnumshift) (:%q dest))

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-signed-byte-8))))

(define-x8664-vinsn %unbox-s8 (((dest :s8))
                               ((src :lisp)))
  (movq (:%q src) (:%q dest))
  (sarq (:$ub x8664::fixnumshift) (:%q dest)))

(define-x8664-vinsn unbox-u16 (((dest :u16))
			      ((src :lisp)))
  :resume
  (testq (:$l (lognot (ash #xffff x8664::fixnumshift))) (:% src))
  (movq (:%q src) (:%q dest))
  (jne :bad)
  (shrq (:$ub x8664::fixnumshift) (:%q dest))
  
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-unsigned-byte-16))))

(define-x8664-vinsn %unbox-u16 (((dest :u16))
			      ((src :lisp)))
  (movq (:%q src) (:%q dest))
  (shrq (:$ub x8664::fixnumshift) (:%q dest)))

(define-x8664-vinsn unbox-s16 (((dest :s16))
			      ((src :lisp)))
  :resume
  (movq (:%q src) (:%q dest))
  (shlq (:$ub (- x8664::nbits-in-word (+ 16 x8664::fixnumshift))) (:%q dest))
  (sarq (:$ub (- x8664::nbits-in-word (+ 16 x8664::fixnumshift))) (:%q dest))
  (cmpq (:%q src) (:%q dest))
  (jne :bad)
  (testb (:$b x8664::fixnummask) (:%b dest))
  (jne :bad)
  (sarq (:$ub x8664::fixnumshift) (:%q dest))
  
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-signed-byte-16))))

(define-x8664-vinsn %unbox-s16 (((dest :s16))
                                ((src :lisp)))
  (movq (:%q src) (:%q dest))
  (sarq (:$ub x8664::fixnumshift) (:%q dest)))

(define-x8664-vinsn unbox-u32 (((dest :u32))
			      ((src :lisp)))
  :resume
  (movq (:$q (lognot (ash #xffffffff x8664::fixnumshift))) (:%q dest))
  (testq (:% src) (:% dest))
  (jne :bad)
  (movq (:%q src) (:%q dest))
  (shrq (:$ub x8664::fixnumshift) (:%q dest))

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-unsigned-byte-32))))

(define-x8664-vinsn %unbox-u32 (((dest :u32))
			      ((src :lisp)))

  (movq (:%q src) (:%q dest))
  (shrq (:$ub x8664::fixnumshift) (:%q dest)))

(define-x8664-vinsn unbox-s32 (((dest :s32))
                               ((src :lisp)))
  :resume
  (movq (:%q src) (:%q dest))
  (shlq (:$ub (- x8664::nbits-in-word (+ 32 x8664::fixnumshift))) (:%q dest))
  (sarq (:$ub (- x8664::nbits-in-word (+ 32 x8664::fixnumshift))) (:%q dest))
  (cmpq (:%q src) (:%q dest))
  (jne :bad)
  (testb (:$b x8664::fixnummask) (:%b dest))
  (jne :bad)
  (sarq (:$ub x8664::fixnumshift) (:%q dest))
  
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-signed-byte-32))))

(define-x8664-vinsn %unbox-s32 (((dest :s32))
                               ((src :lisp)))
  (movq (:%q src) (:%q dest))
  (sarq (:$ub x8664::fixnumshift) (:%q dest)))


(define-x8664-vinsn unbox-u64 (((dest :u64))
                               ((src :lisp)))
  :resume
  (movq (:$q (lognot (ash x8664::target-most-positive-fixnum x8664::fixnumshift))) (:%q dest))
  (testq (:%q dest) (:%q src))
  (movq (:%q src) (:%q dest))
  (jnz :maybe-bignum)
  (shrq (:$ub x8664::fixnumshift) (:%q dest))
  (jmp :done)
  :maybe-bignum
  (andl (:$b x8664::tagmask) (:%l dest))
  (cmpl (:$b x8664::tag-misc) (:%l dest))
  (jne :have-tag)
  (movzbl (:@ x8664::misc-subtag-offset (:%q src)) (:%l dest))
  :have-tag
  (cmpl (:$b x8664::subtag-bignum) (:%l dest))
  (jne :bad)
  (movq (:@ x8664::misc-header-offset (:%q src)) (:%q dest))
  (cmpq (:$l x8664::three-digit-bignum-header) (:%q dest))
  (je :three)
  (cmpq (:$l x8664::two-digit-bignum-header) (:%q dest))
  (jne :bad)
  (movq (:@ x8664::misc-data-offset (:%q src)) (:%q dest))
  (testq (:%q dest) (:%q dest))
  (js :bad)
  (jmp :done)

  :three
  (movl (:@ (+ 8 x8664::misc-data-offset) (:%q src)) (:%l dest))
  (testl (:%l dest) (:%l dest))
  (movq (:@ x8664::misc-data-offset (:%q src)) (:%q dest))
  (jne :bad)
  :done
  
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-unsigned-byte-64))))

(define-x8664-vinsn unbox-s64 (((dest :s64))
                               ((src :lisp)))
  :resume
  (movq (:%q src) (:%q dest))
  (sarq (:$ub x8664::fixnumshift) (:%q dest))
  ;; Was it a fixnum ?
  (testb (:$b x8664::fixnummask) (:%b src))
  (je :done)
  ;; May be a 2-digit bignum
  (movl (:%l src) (:%l dest))
  (andl (:$b x8664::tagmask) (:%l dest))
  (cmpl (:$b x8664::tag-misc) (:%l dest))
  (jne :bad)
  (cmpq (:$l x8664::two-digit-bignum-header) (:@ x8664::misc-header-offset (:%q src)))
  (movq (:@ x8664::misc-data-offset (:%q src)) (:%q dest))
  (jne :bad)
  :done

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q src) (:$ub arch::error-object-not-signed-byte-64))))

(define-x8664-vinsn sign-extend-s8 (((dest :s64))
                                    ((src :s8)))
  (movsbq (:%b src) (:%q dest)))

(define-x8664-vinsn sign-extend-s16 (((dest :s64))
                                     ((src :s16)))
  (movswq (:%w src) (:%q dest)))

(define-x8664-vinsn sign-extend-s32 (((dest :s64))
                                     ((src :s32)))
  (movslq (:%l src) (:%q dest)))


(define-x8664-vinsn zero-extend-u8 (((dest :s64))
                                    ((src :u8)))
  (movzbl (:%b src) (:%l dest)))

(define-x8664-vinsn zero-extend-u16 (((dest :s64))
                                     ((src :u16)))
  (movzwl (:%w src) (:%l dest)))

(define-x8664-vinsn zero-extend-u32 (((dest :s64))
                                     ((src :u32)))
  (movl (:%l src) (:%l dest)))

(define-x8664-vinsn (jump-subprim :jumpLR) (()
					    ((spno :s32const)))
  (jmp (:@ spno)))

;;; Call a subprimitive using a tail-aligned CALL instruction.
(define-x8664-vinsn (call-subprim :call :subprim) (()
                                                   ((spno :s32const))
                                                   ((entry (:label 1))))
  (:talign 4)
  (call (:@ spno))
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn)))

(define-x8664-vinsn (call-subprim-no-return) (()
                                                   ((spno :s32const))
                                                   ((entry (:label 1))))
  (:talign 4)
  (call (:@ spno))
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn)))



(define-x8664-vinsn %logand-c (((dest t))
                               ((val t)
                                (const :s32const)))
  ((:not (:pred = (:apply %hard-regspec-value val)
                (:apply %hard-regspec-value dest)))
   (movq (:%q val) (:%q dest)))
  ((:and (:pred >= const -128) (:pred <= const 127))
   (andq (:$b const) (:%q dest)))
  ((:not (:and (:pred >= const -128) (:pred <= const 127)))
   (andq (:$l const) (:%q dest))))

(define-x8664-vinsn %logior-c (((dest t)
                                (val t))
                               ((val t)
                                (const :s32const)))
  ((:and (:pred >= const -128) (:pred <= const 127))
   (orq (:$b const) (:%q val)))
  ((:not (:and (:pred >= const -128) (:pred <= const 127)))
   (orq (:$l const) (:%q val))))

(define-x8664-vinsn %logxor-c (((dest t)
                                (val t))
                               ((val t)
                                (const :s32const)))
  ((:and (:pred >= const -128) (:pred <= const 127))
   (xorq (:$b const) (:%q val)))
  ((:not (:and (:pred >= const -128) (:pred <= const 127)))
   (xorq (:$l const) (:%q val))))

(define-x8664-vinsn character->fixnum (((dest :lisp))
				       ((src :lisp))
				       ())
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movq (:%q src) (:%q dest)))
  (shrq (:$ub (- x8664::charcode-shift x8664::fixnumshift)) (:%q dest)))

(define-x8664-vinsn compare (()
                             ((x t)
                              (y t)))
  (rcmpq (:%q x) (:%q y)))

(define-x8664-vinsn negate-fixnum (((val :lisp))
                                   ((val :imm)))
  (negq (:% val)))

(define-x8664-vinsn handle-fixnum-overflow-inline
    (()
     ((val :lisp))
     ((header (:u64 #.x8664::imm0))
      (scaled-size (:u64 #.x8664::imm1))
      (bigits (:u64 #.x8664::imm2))
      (freeptr (:lisp #.x8664::allocptr))))
  (movq (:%q val) (:%q bigits))
  (btcq (:$ub 63) (:%q bigits))
  (sarq (:$ub x8664::fixnumshift) (:%q bigits))
  (btcq (:$ub 60) (:%q bigits))
  (movl (:$l x8664::two-digit-bignum-header) (:%l header))
  (xorl (:%l scaled-size) (:%l scaled-size))
  (movb (:$b (- 16 x8664::fulltag-misc)) (:%b scaled-size))
  (subq (:%q scaled-size) (:rcontext x8664::tcr.save-allocptr))
  (movq (:rcontext x8664::tcr.save-allocptr) (:%q freeptr))
  (rcmpq (:%q freeptr) (:rcontext x8664::tcr.save-allocbase))
  (:byte #x77) (:byte #x02)             ;(ja :no-trap)
  (uuo-alloc)
  :no-trap
  (movq (:%q header) (:@ x8664::misc-header-offset (:%q freeptr)))
  (andb (:$b (lognot x8664::fulltagmask)) (:rcontext x8664::tcr.save-allocptr))
  ((:not (:pred = freeptr
                (:apply %hard-regspec-value val)))
   (movq (:%q freeptr) (:%q val)))
  (movq (:%q bigits) (:@ x8664::misc-data-offset (:%q val))))

(define-x8664-vinsn %set-z-flag-if-s64-fits-in-fixnum (((dest :imm))
                                                       ((src :s64))
                                                       ((temp :s64)))
  (movq (:%q src) (:%q temp))
  (shlq (:$ub x8664::fixnumshift) (:%q temp))
  (movq (:%q temp) (:%q dest))          ; tagged as a fixnum
  (sarq (:$ub x8664::fixnumshift) (:%q temp))
  (cmpq (:%q src) (:%q temp)))

(define-x8664-vinsn %set-z-flag-if-u64-fits-in-fixnum (((dest :imm))
                                                       ((src :u64)))
  (movq (:$q (ash #xf 60)) (:%q dest))
  (testq (:%q dest) (:%q src))
  (leaq (:@ (:%q src) x8664::fixnumone) (:% dest)))


(define-x8664-vinsn setup-bignum-alloc-for-s64-overflow (()
                                                         ((src :s64)))
  (movq (:%q src) (:%q x8664::imm2))
  (movl (:$l x8664::two-digit-bignum-header) (:%l x8664::imm0.l))
  (movl (:$l (- 16 x8664::fulltag-misc)) (:%l x8664::imm1.l)))


;;; If the sign bit is set in SRC, need to make a 3-digit bignum
;;; that requires 32 bytes of aligned memory
(define-x8664-vinsn setup-bignum-alloc-for-u64-overflow (()
                                                         ((src :s64)))
  (testq (:%q src) (:%q src))
  (movq (:%q src) (:%q x8664::imm2))
  (movl (:$l x8664::two-digit-bignum-header) (:%l x8664::imm0.l))
  (movl (:$l (- 16 x8664::fulltag-misc)) (:%l x8664::imm1.l))
  (jns :done)
  (movl (:$l x8664::three-digit-bignum-header) (:%l x8664::imm0.l))
  (movl (:$l (- 32 x8664::fulltag-misc)) (:%l x8664::imm1.l))
  :done)
  
  

(define-x8664-vinsn %allocate-uvector (((dest :lisp))
                                       ()
                                       ((header (:u64 #.x8664::imm0))
                                        (freeptr (:lisp #.x8664::allocptr))))
  (subq (:%q x8664::imm1) (:rcontext x8664::tcr.save-allocptr))
  (movq (:rcontext x8664::tcr.save-allocptr) (:%q freeptr))
  (rcmpq (:%q freeptr) (:rcontext x8664::tcr.save-allocbase))
  (:byte #x77) (:byte #x02) ;(ja :no-trap)
  (uuo-alloc)
  :no-trap
  (movq (:%q header) (:@ x8664::misc-header-offset (:%q freeptr)))
  (andb (:$b (lognot x8664::fulltagmask)) (:rcontext x8664::tcr.save-allocptr))
  ((:not (:pred eql freeptr
                (:apply %hard-regspec-value dest)))
   (movq (:%q freeptr) (:%q dest))))

(define-x8664-vinsn set-bigits-after-fixnum-overflow (()
                                                      ((bignum :lisp)))
  (movq (:%q x8664::imm2) (:@ x8664::misc-data-offset (:%q bignum))))
  

                                                       
(define-x8664-vinsn box-fixnum (((dest :imm))
                                ((src :s8)))
  (imulq (:$b x8664::fixnumone) (:%q src)(:%q dest)))

;;; don't modify src
(define-x8664-vinsn box-fixnum-carefully (((dest :imm))
                                          ((src :s8))
                                          ((temp :s8)))
  (movq (:%q src) (:%q temp))
  (shlq (:$ub x8664::fixnumshift) (:%q temp))
  (movq (:%q temp) (:%q dest)))

(define-x8664-vinsn box-fixnum* (((dest :imm)
                                  (src :s8))
                                  
                                 ((src :s8))
                                         )
  (shlq (:$ub x8664::fixnumshift) (:%q src))
  (movq (:%q src) (:%q dest)))

(define-x8664-vinsn (return-or-fix-overflow :jumpLR)(()
                                            ())
  (jo :fix)
  (:byte #xf3) (ret)
  :fix
  (jmp (:@ .SPfix-overflow)))

(define-x8664-vinsn add-constant (((dest :imm))
                                  ((dest :imm)
                                   (const :s32const)))
  ((:and (:pred >= const -128) (:pred <= const 127))
   (addq (:$b const) (:%q dest)))
  ((:not (:and (:pred >= const -128) (:pred <= const 127)))
   (addq (:$l const) (:%q dest))))

(define-x8664-vinsn add-constant3 (((dest :imm))
                                   ((src :imm)
                                    (const :s32const)))
  ((:pred = (:apply %hard-regspec-value dest)
          (:apply %hard-regspec-value src))
   ((:and (:pred >= const -128) (:pred <= const 127))
    (addq (:$b const) (:%q dest)))
   ((:not (:and (:pred >= const -128) (:pred <= const 127)))
    (addq (:$l const) (:%q dest))))
  ((:not (:pred = (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (leaq (:@ const (:%q src)) (:%q dest))))

  

(define-x8664-vinsn fixnum-add2  (((dest :imm))
                                  ((dest :imm)
                                   (other :imm)))
  (addq (:%q other) (:%q dest)))

(define-x8664-vinsn fixnum-sub2  (((dest :imm))
                                  ((x :imm)
                                   (y :imm))
                                  ((temp :imm)))
  ((:pred = (:apply %hard-regspec-value x) (:apply %hard-regspec-value dest))
   (subq (:%q y) (:%q dest)))
  ((:not (:pred = (:apply %hard-regspec-value x) (:apply %hard-regspec-value dest)))
   ((:pred = (:apply %hard-regspec-value y) (:apply %hard-regspec-value dest)) 
    (movq (:%q x) (:%q temp))
    (subq (:%q y) (:%q temp))
    (movq (:%q temp) (:%q dest)))
   ((:not (:pred = (:apply %hard-regspec-value y) (:apply %hard-regspec-value dest)))
    (movq (:%q x) (:%q dest))
    (subq (:%q y) (:%q dest)))))



(define-x8664-vinsn fixnum-add3 (((dest :imm))
                                 ((x :imm)
                                  (y :imm)))
  
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (addq (:%q y) (:%q dest)))
  ((:not (:pred =
                (:apply %hard-regspec-value x)
                (:apply %hard-regspec-value dest)))
   ((:pred =
           (:apply %hard-regspec-value y)
           (:apply %hard-regspec-value dest))
    (addq (:%q x) (:%q dest)))
   ((:not (:pred =
                 (:apply %hard-regspec-value y)
                 (:apply %hard-regspec-value dest)))
    (leaq (:@ (:%q x) (:%q y)) (:%q dest)))))

(define-x8664-vinsn (ref-gpr) (()
                               ((src t)))
  ;; no code
  )
  

(define-x8664-vinsn (copy-gpr :trivial-copy) (((dest t))
                                              ((src t)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movq (:%q src) (:%q dest))))

(define-x8664-vinsn copy-gpr! (((dest t))
                               ((src t)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movq (:%q src) (:%q dest))))

(define-x8664-vinsn copy-incoming-register-arg  (((dest t))
                                                 ((src t)))
  ((:not (:pred eq src dest))
   (movq (:%q src) (:%q dest))))

(define-x8664-vinsn (vpop-register :pop :node :vsp)
    (((dest :lisp))
     ())
  (popq (:%q dest)))

(define-x8664-vinsn (vpop-gvector-element :pop :node :vsp)
    (()
     ((vector :lisp)
      (idx :u32const))
     ())
  (popq (:@ (:apply + x8664::misc-data-offset (:apply ash idx x8664::word-shift)) (:%q vector))))
                                           
(define-x8664-vinsn (push-argregs :push :node :vsp) (()
                                                      ())
  (testl (:%l x8664::nargs) (:%l x8664::nargs))
  (jz :done)
  (rcmpl (:%l x8664::nargs) (:$b (* 2 x8664::node-size)))
  (jb :one)
  (je :two)
  (pushq (:%q x8664::arg_x))
  :two
  (pushq (:%q x8664::arg_y))
  :one
  (pushq (:%q x8664::arg_z))
  :done)

(define-x8664-vinsn (push-max-argregs :push :node :vsp) (()
                                                         ((max :u32const)))
  ((:pred >= max 3)
   (testl (:%l x8664::nargs) (:%l x8664::nargs))
   (jz :done)
   (rcmpl (:%l x8664::nargs) (:$b (* 2 x8664::node-size)))
   (jb :one)
   (je :two)
   (pushq (:%q x8664::arg_x))
   :two
   (pushq (:%q x8664::arg_y))
   :one
   (pushq (:%q x8664::arg_z))
   :done)
  ((:pred = max 2)
   (rcmpl (:%l x8664::nargs) (:$b (* 1 x8664::node-size)))
   (jb :done)
   (je :one)
   (pushq (:%q x8664::arg_y))
   :one
   (pushq (:%q x8664::arg_z))
   :done)
  ((:pred = max 1)
   (testl (:%l x8664::nargs) (:%l x8664::nargs))
   (je :done)
   (pushq (:%q x8664::arg_z))
   :done))

(define-x8664-vinsn (call-label :call) (()
					((label :label))
                                        ((entry (:label 1))))
  (:talign 4)
  (call label)
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn)))

(define-x8664-vinsn double-float-compare (()
					  ((arg0 :double-float)
					   (arg1 :double-float)))
  (comisd (:%xmm arg1) (:%xmm arg0)))

(define-x8664-vinsn single-float-compare (()
					  ((arg0 :single-float)
					   (arg1 :single-float)))
  (comiss (:%xmm arg1) (:%xmm arg0)))
              

(define-x8664-vinsn double-float+-2 (((result :double-float))
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
(define-x8664-vinsn double-float--2 (((result :double-float))
				     ((x :double-float)
				      (y :double-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movsd (:%xmm x) (:%xmm result)))
  (subsd (:%xmm y) (:%xmm result)))

(define-x8664-vinsn double-float*-2 (((result :double-float))
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
(define-x8664-vinsn double-float/-2 (((result :double-float))
				     ((x :double-float)
				      (y :double-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movsd (:%xmm x) (:%xmm result)))
  (divsd (:%xmm y) (:%xmm result)))


(define-x8664-vinsn complex-double-float+-2 (((result :complex-double-float))
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
(define-x8664-vinsn complex-double-float--2 (((result :complex-double-float))
				     ((x :complex-double-float)
				      (y :complex-double-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movapd (:%xmm x) (:%xmm result)))
  (subpd (:%xmm y) (:%xmm result)))

(define-x8664-vinsn complex-double-float*-2 (((result :complex-double-float))
                                             ((x :complex-double-float)
                                              (y :complex-double-float))
                                             ((b :double-float)
                                              (ix :double-float)
                                              (iy :double-float)))
  (movapd (:%xmm x) (:%xmm ix))
  (shufpd (:$ub 1) (:%xmm x8664::fpzero) (:%xmm ix))
  (movapd (:%xmm y) (:%xmm iy))
  (shufpd (:$ub 1) (:%xmm x8664::fpzero) (:%xmm iy))
  (movsd (:%xmm y) (:%xmm result))
  (mulsd (:%xmm x) (:%xmm result))
  (movsd (:%xmm iy) (:%xmm b))
  (mulsd (:%xmm ix) (:%xmm b))
  (subsd (:%xmm b) (:%xmm result))
  (mulsd (:%xmm x) (:%xmm iy))
  (mulsd (:%xmm y) (:%xmm ix))
  (addsd (:%xmm ix) (:%xmm iy))
  (shufpd (:$ub 0) (:%xmm iy) (:%xmm result)))
         
  
                                             
  


(define-x8664-vinsn single-float+-2 (((result :single-float))
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
(define-x8664-vinsn single-float--2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movss (:%xmm x) (:%xmm result)))
  (subss (:%xmm y) (:%xmm result)))

(define-x8664-vinsn single-float*-2 (((result :single-float))
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
(define-x8664-vinsn single-float/-2 (((result :single-float))
				     ((x :single-float)
				      (y :single-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movss (:%xmm x) (:%xmm result)))
  (divss (:%xmm y) (:%xmm result)))

(define-x8664-vinsn complex-single-float+-2 (((result :complex-single-float))
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
(define-x8664-vinsn complex-single-float--2 (((result :complex-single-float))
				     ((x :complex-single-float)
				      (y :complex-single-float)))
  ((:not (:pred = (:apply %hard-regspec-value result)
                (:apply %hard-regspec-value x)))
   (movq (:%xmm x) (:%xmm result)))
  (subps (:%xmm y) (:%xmm result)))

(define-x8664-vinsn complex-single-float*-2 (((result :complex-single-float))
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

(define-x8664-vinsn get-single (((result :single-float))
                                ((source :lisp)))
  (movd (:%q source) (:%xmm result))
  (psrlq (:$ub 32) (:%xmm result)))

(define-x8664-vinsn get-double (((result :double-float))
                                ((source :lisp)))
  (movsd (:@  x8664::double-float.value (:%q source)) (:%xmm result)))

(define-x8664-vinsn get-complex-double-float (((result :complex-double-float))
                                              ((source :lisp)))
  (movdqu (:@  x8664::complex-double-float.realpart (:%q source)) (:%xmm result)))

(define-x8664-vinsn get-complex-single-float (((result :complex-single-float))
                                              ((source :lisp)))
  (movq (:@  x8664::complex-single-float.realpart (:%q source)) (:%xmm result)))


;;; Extract a double-float value, typechecking in the process.
;;; IWBNI we could simply call the "trap-unless-typecode=" vinsn here,
;;; instead of replicating it ..

(define-x8664-vinsn get-double? (((target :double-float))
				 ((source :lisp))
				 ((tag :u8)))
  :resume
  (movl (:%l source) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :have-tag)
  (movsbl (:@ x8664::misc-subtag-offset (:%q source)) (:%l tag))
  :have-tag
  (cmpl (:$b x8664::subtag-double-float) (:%l tag))
  (jne :bad)
  (movsd (:@  x8664::double-float.value (:%q source)) (:%xmm target))

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%q source) (:$ub x8664::subtag-double-float))))

(define-x8664-vinsn single->node (((result :lisp)
                                   (source :single-float))
                                  ((source :single-float)))
  (psllq (:$ub 32) (:%xmm source))
  (movd (:%xmm source) (:%q result))
  (movb (:$b x8664::tag-single-float) (:%b result)))

(define-x8664-vinsn (copy-double-float :trivial-copy) (((dest :double-float))
                                                       ((src :double-float)))
  (movsd (:%xmm src) (:%xmm dest)))

(define-x8664-vinsn (copy-single-float :trivial-copy) (((dest :single-float))
                                                       ((src :single-float)))
  (movss (:%xmm src) (:%xmm dest)))

(define-x8664-vinsn (copy-complex-single-float :trivial-copy) (((dest :complex-single-float))
                                                               ((src :complex-single-float)))
  (movq (:%xmm src) (:%xmm dest)))

(define-x8664-vinsn (copy-complex-double-float :trivial-copy)  (((dest :complex-double-float))


                                                                ((src :complex-double-float)))
  (movapd (:%xmm src) (:%xmm dest)))


(define-x8664-vinsn copy-single-to-double (((dest :double-float))
                                           ((src :single-float)))
  (cvtss2sd (:%xmm src) (:%xmm dest)))

(define-x8664-vinsn copy-double-to-single (((dest :single-float))
                                           ((src :double-float)))
  (cvtsd2ss (:%xmm src) (:%xmm dest)))

(define-x8664-vinsn u8->fixnum (((result :imm)) 
				((val :u8)) 
				())
  (leaq (:@ (:%q val) 8) (:%q result)))

(define-x8664-vinsn fitvals (()
                             ((n :u16const))
                             ((imm :u16)))
  ((:pred = n 0)
   (xorl (:%l imm) (:%l imm)))
  ((:not (:pred = n 0))
   (movl (:$l (:apply ash n x8664::fixnumshift)) (:%l imm)))
  (subl (:%l x8664::nargs) (:%l imm))
  (jae :push-more)
  (movslq (:%l imm) (:%q imm))
  (subq (:%q imm) (:%q x8664::rsp))    ; sic.  "imm" ia negative here
  (jmp :done)
  :push-loop

  (pushq (:$l (:apply target-nil-value)))
  (addl (:$b x8664::node-size) (:%l x8664::nargs))
  (subl (:$b x8664::node-size) (:%l imm))
  :push-more
  (jne :push-loop)
  :done)
  
(define-x8664-vinsn (nvalret :jumpLR :needs-frame-pointer) (()
                                       ())
  
  (jmp (:@ .SPnvalret)))


(define-x8664-vinsn lisp-word-ref (((dest t))
				   ((base t)
				    (offset t)))
  (movq (:@ (:%q base) (:%q offset)) (:%q  dest)))


(define-x8664-vinsn lisp-word-ref-c (((dest t))
				     ((base t)
				      (offset :s32const)))
  ((:pred = offset 0)
   (movq (:@ (:%q base)) (:%q dest)))
  ((:not (:pred = offset 0))
   (movq (:@ offset (:%q base)) (:%q dest))))


(define-x8664-vinsn (vpush-label :push :node :vsp) (()
                                                    ((label :label))
                                                    ((temp :lisp)))
  (leaq (:@ (:^ label) (:%q x8664::fn)) (:%q temp))
  (pushq (:%q temp)))

;; ????
(define-x8664-vinsn (emit-aligned-label :align) (()
                                                 ((label :label)))
  (:align 3)
  (:long (:^ label)))

(define-x8664-vinsn (align-referenced-label :align) (()
                                                 ((label :label)))
  #+later
  ((:pred plusp (:apply length (:apply vinsn-label-refs label)))
   (:align 4)))
  


;;; %ra0 is pointing into %fn, so no need to copy %fn here.
(define-x8664-vinsn (xpass-multiple-values-symbol :call  :extended-call :jumplr)
    (()
     ((lab :label))
     ())                                                                
  (pushq (:@ (:apply + (:apply target-nil-value) (x8664::%kernel-global 'x86::ret1valaddr))))
  (:talign 4)
  (jmp (:@ x8664::symbol.fcell (:% x8664::fname)))

  )

(define-x8664-vinsn (pass-multiple-values-symbol :call  :extended-call :jumplr)
    (()
     ()
     ())                                                                
  (pushq (:@ (:apply + (:apply target-nil-value) (x8664::%kernel-global 'x86::ret1valaddr))))
  (jmp (:@ x8664::symbol.fcell (:% x8664::fname)))

  )

;;; It'd be good to have a variant that deals with a known function
;;; as well as this. 
(define-x8664-vinsn (xpass-multiple-values :call :extended-call :jumplr) (()
                                                    ((lab :label))
                                                    ((tag :u8)))
  :resume
  (movl (:%l x8664::temp0) (:%l tag))
  (andl (:$b x8664::fulltagmask) (:%l tag))
  (cmpl (:$b x8664::fulltag-symbol) (:%l tag))
  (cmovgq (:%q x8664::temp0) (:%q x8664::fn))
  (jl :bad)
  (cmoveq (:@ x8664::symbol.fcell (:%q x8664::fname)) (:%q x8664::fn))
  (pushq (:@ (:apply + (:apply target-nil-value) (x8664::%kernel-global 'x86::ret1valaddr))))
  (jmp (:%q x8664::fn))

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-not-callable)))

(define-x8664-vinsn (pass-multiple-values :call  :jumplr) (()
                                                    ()
                                                    ((tag :u8)))
  :resume
  (movl (:%l x8664::temp0) (:%l tag))
  (andl (:$b x8664::fulltagmask) (:%l tag))
  (cmpl (:$b x8664::fulltag-symbol) (:%l tag))
  (cmovgq (:%q x8664::temp0) (:%q x8664::fn))
  (jl :bad)
  (cmoveq (:@ x8664::symbol.fcell (:%q x8664::fname)) (:%q x8664::fn))
  (pushq (:@ (:apply + (:apply target-nil-value) (x8664::%kernel-global 'x86::ret1valaddr))))
  (jmp (:%q x8664::fn))

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-not-callable)))

(define-x8664-vinsn (xpass-multiple-values-known-function :call :extended-call :jumplr)
    (() ((lab :label) (fnreg :lisp)))
  (pushq (:@ (:apply + (:apply target-nil-value) (x8664::%kernel-global 'x86::ret1valaddr)))) 
  (jmp (:%q fnreg)))

(define-x8664-vinsn (pass-multiple-values-known-function :call :extended-call :jumplr)
    (() ((fnreg :lisp)))
  (pushq (:@ (:apply + (:apply target-nil-value) (x8664::%kernel-global 'x86::ret1valaddr)))) 
  (jmp (:%q fnreg)))

(define-x8664-vinsn reserve-outgoing-frame (()
                                            ())
  (pushq (:$b x8664::reserved-frame-marker))
  (pushq (:$b x8664::reserved-frame-marker)))

(define-x8664-vinsn (pop-return-address-into-reserved-frame) (()
                                                              ())
  (popq (:@ 16 (:%q x8664::rsp))))
  


(define-x8664-vinsn (call-known-function :call) (()
						 ()
                                                 ((entry (:label 1))))
  (:talign 4)
  (call (:%q x8664::temp0))
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn)))

(define-x8664-vinsn (jump-known-function :jumplr) (()
						   ()
                                                   ((xfn (:lisp #.x8664::xfn))))
  (movq (:%q x8664::fn) (:%q x8664::xfn))
  (movq (:%q x8664::temp0)  (:%q x8664::fn))
  (jmp (:%q x8664::fn)))

(define-x8664-vinsn (list :call) (()
                                  ()
                                  ((entry (:label 1))))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (:talign 4)
  (jmp (:@ .SPconslist))
  :back
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn)))


(define-x8664-vinsn (make-tsp-cons ) (((dest :lisp))
				   ((car :lisp) (cdr :lisp))
				   ((temp :imm)
				    (stack-temp :imm)))
  (subq (:$b (+ x8664::cons.size x8664::dnode-size)) (:rcontext x8664::tcr.next-tsp))
  (movq (:rcontext x8664::tcr.next-tsp) (:%q temp))
  (movapd (:%xmm x8664::fpzero) (:@ (:%q temp)))
  (movapd (:%xmm x8664::fpzero) (:@ 16 (:%q temp)))
  (movq (:rcontext x8664::tcr.save-tsp) (:%q stack-temp))
  (movq (:%q stack-temp) (:@ (:%q temp)))
  (movq (:% x8664::rbp) (:@ x8664::tsp-frame.rbp (:%q temp)))
  (movq (:%q temp) (:rcontext x8664::tcr.save-tsp))
  (leaq (:@ (+ x8664::dnode-size x8664::fulltag-cons) (:%q temp)) (:%q temp))
  (movq (:%q car) (:@ x8664::cons.car (:%q temp)))
  (movq (:%q cdr) (:@ x8664::cons.cdr (:%q temp)))
  (movq (:%q temp) (:%q dest)))

(define-x8664-vinsn make-fixed-stack-gvector (((dest :lisp))
                                              ((aligned-size :u32const)
                                               (header :s32const))
                                              ((tempa :imm)
                                               (tempb :imm)
					       (stack-temp :imm)))
  ((:and (:pred >= (:apply + aligned-size x8664::dnode-size) -128)
         (:pred <= (:apply + aligned-size x8664::dnode-size) 127))
   (subq (:$b (:apply + aligned-size x8664::dnode-size))
         (:rcontext x8664::tcr.next-tsp)))
  ((:not (:and (:pred >= (:apply + aligned-size x8664::dnode-size) -128)
               (:pred <= (:apply + aligned-size x8664::dnode-size) 127)))
   (subq (:$l (:apply + aligned-size x8664::dnode-size))
         (:rcontext x8664::tcr.next-tsp)))
  (movq (:rcontext x8664::tcr.save-tsp) (:%q tempb))
  (movq (:rcontext x8664::tcr.next-tsp) (:%q tempa))
  (movq (:%q tempb) (:%q stack-temp))
  :loop
  (movapd (:%xmm x8664::fpzero) (:@ -16 (:%q tempb)))
  (subq (:$b x8664::dnode-size) (:%q tempb))
  (cmpq (:%q tempa) (:%q tempb))
  (jnz :loop)
  (movq (:%q stack-temp) (:@ (:%q tempa)))
  (movq (:% x8664::rbp) (:@ x8664::tsp-frame.rbp (:%q tempa)))
  (movq (:%q tempa) (:rcontext x8664::tcr.save-tsp))
  (movl (:$l header) (:@ x8664::dnode-size (:%q tempa)))
  (leaq (:@ (+ x8664::dnode-size x8664::fulltag-misc) (:%q tempa)) (:%q dest)))


(define-x8664-vinsn (discard-temp-frame :tsp :pop :discard) (()
					()
                                        ((temp :imm)))
  (movq (:rcontext x8664::tcr.save-tsp) (:%q temp))
  (movq (:@ (:%q temp)) (:%q temp))
  (movq (:%q temp) (:rcontext x8664::tcr.save-tsp))
  (movq (:%q temp) (:rcontext x8664::tcr.next-tsp))
  )

(define-x8664-vinsn (discard-c-frame  :pop :discard) (()
                                     ()
                                     ((temp :imm)))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q temp))
  (movq (:@ (:%q temp)) (:%q temp))
  (movq (:%q temp) (:rcontext x8664::tcr.foreign-sp)))

  
(define-x8664-vinsn (vstack-discard :vsp :pop :discard) (()
				    ((nwords :u32const)))
  ((:not (:pred = nwords 0))
   ((:pred < nwords 16)
    (addq (:$b (:apply ash nwords x8664::word-shift)) (:%q x8664::rsp)))
   ((:not (:pred < nwords 16))
    (addq (:$l (:apply ash nwords x8664::word-shift)) (:%q x8664::rsp)))))


(defmacro define-x8664-subprim-lea-jmp-vinsn ((name &rest other-attrs) spno)
  `(define-x8664-vinsn (,name :call :subprim ,@other-attrs) (() () ((entry (:label 1))))
    (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
    (:talign 4)
    (jmp (:@ ,spno))
    :back
    (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn))))

(defmacro define-x8664-subprim-call-vinsn ((name &rest other-attrs) spno)
  `(define-x8664-vinsn (,name :call :subprim ,@other-attrs) (() () ((entry (:label 1))))
    (:talign 4)
    (call (:@ ,spno))
    :back
    (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn))))

(defmacro define-x8664-subprim-jump-vinsn ((name &rest other-attrs) spno)
  `(define-x8664-vinsn (,name :jumpLR ,@other-attrs) (() ())
    (jmp (:@ ,spno))))

(define-x8664-vinsn (nthrowvalues :call :subprim :needs-frame-pointer) (() ())
  (jmp (:@ .SPnthrowvalues)))

(define-x8664-vinsn (nthrow1value :call :subprim :needs-frame-pointer) (() ())
  (jmp (:@ .SPnthrow1value)))



(define-x8664-subprim-lea-jmp-vinsn (bind-interrupt-level-0)  .SPbind-interrupt-level-0)


(define-x8664-vinsn (bind-interrupt-level-0-inline) (()
                                                   ()
                                                   ((temp :imm)))
  (movq (:rcontext x8664::tcr.tlb-pointer) (:%q temp))
  (cmpq (:$b 0) (:@ x8664::interrupt-level-binding-index (:%q temp)))
  (pushq (:@ x8664::interrupt-level-binding-index (:%q temp)))
  (pushq (:$b x8664::interrupt-level-binding-index))
  (pushq (:rcontext x8664::tcr.db-link))
  (movq (:$l 0) (:@ x8664::interrupt-level-binding-index (:%q temp)))
  (movq (:%q x8664::rsp) (:rcontext x8664::tcr.db-link))
  (jns :done)
  (btrq (:$ub 63) (:rcontext x8664::tcr.interrupt-pending))
  (jae :done)
  (ud2a)
  (:byte 2)
  :done)
  
  

(define-x8664-subprim-lea-jmp-vinsn (bind-interrupt-level-m1) .SPbind-interrupt-level-m1)

(define-x8664-vinsn bind-interrupt-level-m1-inline (()
                                                   ()
                                                   ((temp :imm)))
  (movq (:rcontext x8664::tcr.tlb-pointer) (:%q temp))
  (pushq (:@ x8664::interrupt-level-binding-index (:%q temp)))
  (pushq (:$b x8664::interrupt-level-binding-index))
  (pushq (:rcontext x8664::tcr.db-link))
  (movq (:$l (ash -1 x8664::fixnumshift)) (:@ x8664::interrupt-level-binding-index (:%q temp)))
  (movq (:%q x8664::rsp) (:rcontext x8664::tcr.db-link)))

(define-x8664-subprim-lea-jmp-vinsn (bind-interrupt-level) .SPbind-interrupt-level)

(define-x8664-subprim-call-vinsn (unbind-interrupt-level) .SPunbind-interrupt-level)

(define-x8664-vinsn unbind-interrupt-level-inline (()
                                                   ()
                                                   ((link :imm)
                                                    (curval :imm)
                                                    (oldval :imm)
                                                    (tlb :imm)))
  (movq (:rcontext x8664::tcr.tlb-pointer) (:%q tlb))
  (movq (:rcontext x8664::tcr.db-link) (:%q link))
  (movq (:@ x8664::interrupt-level-binding-index (:%q tlb)) (:%q curval))
  (testq (:%q curval) (:%q curval))
  (movq (:@ 16 #|binding.val|# (:%q link)) (:%q oldval))
  (movq (:@ #|binding.link|# (:%q link)) (:%q link))
  (movq (:%q oldval) (:@ x8664::interrupt-level-binding-index (:%q tlb)))
  (movq (:%q link) (:rcontext x8664::tcr.db-link))
  (jns :done)
  (testq (:%q oldval) (:%q oldval))
  (js :done)
  (btrq (:$ub 63) (:rcontext x8664::tcr.interrupt-pending))
  (jae :done)
  (ud2a)
  (:byte 2)
  :done)  

(define-x8664-vinsn (jump-return-pc :jumpLR)
    (()
     ())
  (ret))

(define-x8664-vinsn label-address (((dest :lisp))
                                   ((lab :label)))
  
  (leaq (:@ (:^ lab)  (:%q x8664::fn)) (:%q dest)))                               


(define-x8664-vinsn (mkunwind :call :subprim :needs-frame-pointer) (() ())
                                        ; sic
  (jmp (:@ .SPmkunwind)))

(define-x8664-vinsn (nmkunwind :call :subprim :needs-frame-pointer) (() ())

  (jmp (:@ .SPnmkunwind)))

;;; "old" mkunwind.  Used by PROGV, since the binding of *interrupt-level*
;;; on entry to the new mkunwind confuses the issue.


(define-x8664-subprim-lea-jmp-vinsn (gvector) .SPgvector)

(define-x8664-subprim-call-vinsn (getu64) .SPgetu64)

;;; Call something callable and obtain the single value that it
;;; returns.
(define-x8664-vinsn (funcall :call) (()
				     ()
				     ((tag :u8)
				      (entry (:label 1))))
  :resume
  (movl (:%l x8664::temp0) (:%l tag))
  (andl (:$b x8664::fulltagmask) (:%l tag))
  (cmpl (:$b x8664::fulltag-symbol) (:%l tag))
  (cmovgq (:%q x8664::temp0) (:%q x8664::xfn))
  (jl :bad)
  (cmoveq (:@ x8664::symbol.fcell (:%q x8664::fname)) (:%q x8664::xfn))
  (:talign 4)
  (call (:%q x8664::xfn))
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn))
  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-not-callable)))

(define-x8664-vinsn (tail-funcall :jumplr) (()
					    ()
					    ((tag (:u8 #.x8664::imm0))))
  :resume
  (movl (:%l x8664::temp0) (:%l tag))
  (andl (:$b x8664::fulltagmask) (:%l tag))
  (cmpl (:$b x8664::fulltag-symbol) (:%l tag))
  (cmovgq (:%q x8664::temp0) (:%q x8664::xfn))
  (jl :bad)
  (cmoveq (:@ x8664::symbol.fcell (:%q x8664::fname)) (:%q x8664::xfn))
  (jmp (:%q x8664::xfn))

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-not-callable)))
                             


;;; Magic numbers in here include the address of .SPcall-closure.
(define-x8664-vinsn init-nclosure (()
                                   ((closure :lisp))
                                   ((imm0 :u64)))
  (movq (:$q #x24fffffffff92d8d) (:%q imm0))
  (movb (:$b 4) (:@ x8664::misc-data-offset (:%q closure))) ; code word count
  (movb (:$b #x4c) (:@ (+ x8664::misc-data-offset 7) (:%q closure))) ; 1st byte of lea
  (movq (:%q imm0) (:@ (+ x8664::misc-data-offset 8) (:%q closure))) ; rest of lea, start of jmp
  (movl (:$l #x01516825) (:@ (+ x8664::misc-data-offset 16) (:%q closure)))
  (movb (:$b x8664::function-boundary-marker) (:@ (+ x8664::misc-data-offset 24)  (:%q closure))))


(define-x8664-vinsn finalize-closure (((closure :lisp))
                                      ((closure :lisp)))
  (addq (:$b (- x8664::fulltag-function x8664::fulltag-misc)) (:%q closure)))


(define-x8664-vinsn (ref-symbol-value :call :subprim)
    (((val :lisp))
     ((sym (:lisp (:ne val))))
     ((entry (:label 1))))
  (:talign 4)
  (call (:@ .SPspecrefcheck))
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn)) )

(define-x8664-vinsn ref-symbol-value-inline (((dest :lisp))
                                              ((src (:lisp (:ne dest))))
                                              ((table :imm)
                                               (idx :imm)))
  :resume
  (movq (:@ x8664::symbol.binding-index (:%q src)) (:%q idx))
  ;; The entry for binding-index 0 in the table always
  ;; contains a no-thread-local-binding marker; treat
  ;; out-of-bounds indices as 0 to avoid branches
  (xorl (:%l table) (:%l table))
  (rcmpq (:%q idx) (:rcontext x8664::tcr.tlb-limit))
  (cmovael (:%l table) (:%l idx))
  (movq (:rcontext x8664::tcr.tlb-pointer) (:%q table))
  (movq (:@ (:%q table) (:%q idx)) (:%q dest))
  (cmpl (:$b x8664::subtag-no-thread-local-binding) (:%l dest))
  (cmoveq (:@ x8664::symbol.vcell (:%q src)) (:%q dest))
  :test
  (cmpl (:$b x8664::unbound-marker) (:%l dest))
  (je :bad)

  (:anchored-uuo-section :resume)
  :bad
  (:anchored-uuo (uuo-error-unbound (:%q src))))


(define-x8664-vinsn (%ref-symbol-value :call :subprim)
    (((val :lisp))
     ((sym (:lisp (:ne val))))
     ((entry (:label 1))))
  (:talign 4)
  (call (:@ .SPspecref))
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn)))

(define-x8664-vinsn %ref-symbol-value-inline (((dest :lisp))
                                              ((src (:lisp (:ne dest))))
                                              ((table :imm)
                                               (idx :imm)))
  ;; Treat out-of-bounds indices as index 0
  (movq (:@ x8664::symbol.binding-index (:%q src)) (:%q idx))
  (xorl (:%l table) (:%l table))
  (rcmpq (:%q idx) (:rcontext x8664::tcr.tlb-limit))
  (cmovael (:%l table) (:%l idx))
  (movq (:rcontext x8664::tcr.tlb-pointer) (:%q table))
  (movq (:@ (:%q table) (:%q idx)) (:%q dest))
  (cmpb (:$b x8664::subtag-no-thread-local-binding) (:%b dest))
  (cmoveq (:@ x8664::symbol.vcell (:%q src)) (:%q dest)))

(define-x8664-vinsn ref-interrupt-level (((dest :imm))
                                         ()
                                         ((temp :u64)))
  (movq (:rcontext x8664::tcr.tlb-pointer) (:%q temp))
  (movq (:@ x8664::INTERRUPT-LEVEL-BINDING-INDEX (:%q temp)) (:%q dest)))




(define-x8664-vinsn setup-double-float-allocation (()
                                                   ())
  (movl (:$l (arch::make-vheader x8664::double-float.element-count x8664::subtag-double-float)) (:%l x8664::imm0.l))
  (movl (:$l (- x8664::double-float.size x8664::fulltag-misc)) (:%l x8664::imm1.l)))

(define-x8664-vinsn setup-complex-double-float-allocation (()
                                                   ())
  (movl (:$l (arch::make-vheader 6 x8664::subtag-complex-double-float)) (:%l x8664::imm0.l))
  (movl (:$l (- x8664::complex-double-float.size x8664::fulltag-misc)) (:%l x8664::imm1.l)))

(define-x8664-vinsn setup-complex-single-float-allocation (()
                                                   ())
  (movl (:$l (arch::make-vheader 2 x8664::subtag-complex-single-float)) (:%l x8664::imm0.l))
  (movl (:$l (- x8664::complex-single-float.size x8664::fulltag-misc)) (:%l x8664::imm1.l)))

(define-x8664-vinsn set-double-float-value (()
                                            ((node :lisp)
                                             (val :double-float)))
  (movsd (:%xmm val) (:@ x8664::double-float.value (:%q node))))

(define-x8664-vinsn set-complex-double-float-value (()
                                                    ((node :lisp)
                                                     (val :complex-double-float)))
  (movdqu (:%xmm val) (:@ x8664::complex-double-float.realpart (:%q node))))

(define-x8664-vinsn set-complex-single-float-value (()
                                                    ((node :lisp)
                                                     (val :complex-single-float)))
  (movq (:%xmm val) (:@ x8664::complex-single-float.realpart (:%q node))))


(define-x8664-vinsn word-index-and-bitnum-from-index (((word-index :u64)
                                                       (bitnum :u8))
                                                      ((index :imm)))
  (movq (:%q index) (:%q word-index))
  (shrq (:$ub x8664::fixnumshift) (:%q word-index))
  (movl (:$l 63) (:%l bitnum))
  (andl (:%l word-index) (:%l bitnum))
  (shrq (:$ub 6) (:%q word-index)))

(define-x8664-vinsn ref-bit-vector-fixnum (((dest :imm)
                                            (bitnum :u8))
                                           ((bitnum :u8)
                                            (bitvector :lisp)
                                            (word-index :u64)))
  (btq (:%q bitnum) (:@ x8664::misc-data-offset (:%q bitvector) (:%q word-index) 8))
  (setb (:%b bitnum))
  (negb (:%b bitnum))
  (andl (:$l x8664::fixnumone) (:%l bitnum))
  (movl (:%l bitnum) (:%l dest)))

(define-x8664-vinsn nref-bit-vector-fixnum (((dest :imm)
					     (bitnum :s64))
					    ((bitnum :s64)
					     (bitvector :lisp))
					    ())
  (btq (:%q bitnum) (:@ x8664::misc-data-offset (:%q bitvector)))
  (setc (:%b bitnum))
  (movzbl (:%b bitnum) (:%l bitnum))
  (imull (:$b x8664::fixnumone) (:%l bitnum) (:%l dest)))


(define-x8664-vinsn nref-bit-vector-flags (()
					    ((bitnum :s64)
					     (bitvector :lisp))
					    ())
  (btq (:%q bitnum) (:@ x8664::misc-data-offset (:%q bitvector))))

(define-x8664-vinsn misc-ref-c-bit-fixnum (((dest :imm))
                                           ((src :lisp)
                                            (idx :u64const))
                                           ((temp :u8)))
  (btq (:$ub (:apply logand 63 idx))
       (:@ (:apply + x8664::misc-data-offset (:apply ash (:apply ash idx -6) x8664::word-shift)) (:%q src)))
  (setc (:%b temp))
  (movzbl (:%b temp) (:%l temp))
  (imull (:$b x8664::fixnumone) (:%l temp) (:%l dest)))


(define-x8664-vinsn misc-ref-c-bit-flags (()
                                           ((src :lisp)
                                            (idx :u64const))
                                          )
  (btq (:$ub (:apply logand 63 idx))
       (:@ (:apply + x8664::misc-data-offset (:apply ash (:apply ash idx -6) x8664::word-shift)) (:%q src))))

(define-x8664-vinsn deref-macptr (((addr :address))
				  ((src :lisp))
				  ())
  (movq (:@ x8664::macptr.address (:%q src)) (:%q addr)))

(define-x8664-vinsn reserve-spill-area (() () ((count :u32)))
  ((:pred > (:apply  x8664-spill-area-needed) 0)
   ((:pred = (:apply  x8664-spill-area-needed) 1)
    (pushq (:$b 0)))
   ((:pred = (:apply  x8664-spill-area-needed) 2)
    (pushq (:$b 0))
    (pushq (:$b 0)))
  ((:pred = (:apply  x8664-spill-area-needed) 3)
   (pushq (:$b 0))
   (pushq (:$b 0))
   (pushq (:$b 0)))
   ((:pred = (:apply  x8664-spill-area-needed) 4)
    (pushq (:$b 0))
    (pushq (:$b 0))
    (pushq (:$b 0))
    (pushq (:$b 0)))
   ((:pred = (:apply  x8664-spill-area-needed) 5)
    (pushq (:$b 0))
    (pushq (:$b 0))
    (pushq (:$b 0))
    (pushq (:$b 0))
    (pushq (:$b 0)))
   ((:pred > (:apply  x8664-spill-area-needed) 5)
    (movl (:$l (:apply  x8664-spill-area-needed)) (:%l count))
    :loop
    (pushq (:$b 0))
    (subl (:$b 1) (:%l count))
    (jge :loop))))
  
(define-x8664-vinsn save-nfp (()
                              ()
                              ((nfp :imm)))
  ((:pred > (:apply x862-max-nfp-depth) 0)
   (movq (:rcontext x8664::tcr.foreign-sp) (:%mmx x8664::stack-temp))
   (:if (:pred < (:apply + 16 (:apply x862-max-nfp-depth)) 128)
     (subq (:$b (:apply + 16 (:apply x862-max-nfp-depth))) (:rcontext x8664::tcr.foreign-sp))
     (subq (:$l (:apply + 16 (:apply x862-max-nfp-depth))) (:rcontext x8664::tcr.foreign-sp)))
   (movq (:rcontext x8664::tcr.foreign-sp) (:%q nfp))
   (movq (:%mmx x8664::stack-temp) (:@ (:%q nfp)))
   (movq (:rcontext x8664::tcr.nfp) (:%mmx x8664::stack-temp))
   (movq (:%mmx x8664::stack-temp) (:@ 8 (:%q nfp)))
   (movq (:%q nfp) (:rcontext x8664::tcr.nfp))))

(define-x8664-vinsn restore-nfp (()
                                 ()
                                 ((temp :imm)))
  ((:pred > (:apply x862-max-nfp-depth) 0)
   (movq (:rcontext x8664::tcr.nfp) (:%q temp))
   (movq (:@ 8 (:%q temp)) (:%mmx x8664::stack-temp))
   (movq (:@ (:%q temp)) (:%q temp))
   (movq (:%q temp) (:rcontext x8664::tcr.foreign-sp))
   (movq (:%mmx x8664::stack-temp) (:rcontext x8664::tcr.nfp))))

(define-x8664-vinsn (reload-natural  :reload :nfp :ref) (((val :u64))
							 ((offset :u16const)))
  (movq (:rcontext x8664::tcr.nfp) (:%q val))
  (movq (:@ (:apply + 16 offset) (:%q val)) (:%q val)))

(define-x8664-vinsn (reload-double-float :reload :nfp :ref) (((val :double-float))
							     ((offset :u16const))
							     ((temp :imm)))
  (movq (:rcontext x8664::tcr.nfp) (:%q temp))
  (movsd (:@ (:apply + 16 offset) (:% temp)) (:%xmm val)))

(define-x8664-vinsn (reload-single-float :reload :nfp :ref) (((val :single-float))
							     ((offset :u16const))
							     ((temp :imm)))
  (movq (:rcontext x8664::tcr.nfp) (:%q temp))
  (movss (:@ (:apply + 16 offset) (:% temp)) (:%xmm val)))

(define-x8664-vinsn (reload-complex-single-float :reload :nfp :ref)
    (((val :complex-single-float))
     ((offset :u16const))
     ((temp :imm)))
  (movq (:rcontext x8664::tcr.nfp) (:%q temp))
  (movq (:@ (:apply + 16 offset) (:%q temp)) (:%xmm val)))

(define-x8664-vinsn (reload-complex-double-float :reload :nfp :ref)
    (((val :complex-double-float))
     ((offset :u16const))
     ((temp :imm)))
  (movq (:rcontext x8664::tcr.nfp) (:%q temp))
  (movdqu (:@ (:apply + 16 offset) (:% temp)) (:%xmm val)))

(define-x8664-vinsn (temp-push-unboxed-word :push :word) (()
							  ((w :u64))
							  ((temp :imm)
							   (stack-temp :imm)))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q stack-temp))
  (subq (:$b (* 2 x8664::dnode-size)) (:rcontext x8664::tcr.foreign-sp))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q temp))
  (movq (:%q stack-temp) (:@ (:%q temp)))
  (movq (:% x8664::rbp) (:@ x8664::csp-frame.rbp (:%q temp)))
  (movq (:%q w) (:@ x8664::dnode-size (:%q temp))))

(define-x8664-vinsn (temp-push-node :push :word :tsp) (()
						       ((w :lisp))
						       ((temp :imm)
							(stack-temp :imm)))
  (subq (:$b (* 2 x8664::dnode-size)) (:rcontext x8664::tcr.next-tsp))
  (movq (:rcontext x8664::tcr.save-tsp) (:%q stack-temp))
  (movq (:rcontext x8664::tcr.next-tsp) (:%q temp))
  (movapd (:%xmm x8664::fpzero) (:@ (:%q temp)))
  (movapd (:%xmm x8664::fpzero) (:@ 16 (:%q temp)))
  (movq (:%q stack-temp) (:@ (:%q temp)))
  (movq (:% x8664::rbp) (:@ x8664::tsp-frame.rbp (:%q temp)))  
  (movq (:%q temp) (:rcontext x8664::tcr.save-tsp))
  (movq (:%q w) (:@ x8664::dnode-size (:%q temp))))

(define-x8664-vinsn (temp-push-double-float :push :word ) (()
							   ((f :double-float))
							   ((temp :imm)
							    (stack-temp :imm)))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q stack-temp))  
  (subq (:$b (* 2 x8664::dnode-size)) (:rcontext x8664::tcr.foreign-sp))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q temp))  
  (movq (:%q stack-temp) (:@ (:%q temp)))
  (movq (:% x8664::rbp) (:@ x8664::csp-frame.rbp (:%q temp)))
  (movapd (:%xmm f) (:@ x8664::dnode-size (:%q temp))))

(define-x8664-vinsn (vpush-single-float :push :word :vsp)
    (()
     ((f :single-float)))
  (pushq (:$b x8664::tag-single-float))
  (movss (:%xmm f) (:@ 4 (:%q x8664::rsp))))

(define-x8664-vinsn (vpop-single-float :pop :word :vsp)
    (()
     ((f :single-float)))
  (movss (:@ 4 (:%q x8664::rsp)) (:%xmm f))
  (addq (:$b x8664::node-size) (:%q x8664::rsp)))

(define-x8664-vinsn (temp-pop-unboxed-word :pop :word )
    (((w :u64))
     ()
     ((temp (:u64 #.x8664::ra0))))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movq (:@ x8664::dnode-size (:%q x8664::ra0)) (:%q w))
  (addq (:$b (* 2 x8664::dnode-size)) (:rcontext x8664::tcr.foreign-sp)))


(define-x8664-vinsn (temp-pop-node :pop :word :tsp)
        (((w :lisp))
         ()
         ((temp :imm)))
  (movq (:rcontext x8664::tcr.save-tsp) (:%q temp))
  (movq (:@ x8664::dnode-size (:%q temp)) (:%q w))
  (movq (:@ (:%q temp)) (:%q temp))
  (movq (:%q temp) (:rcontext x8664::tcr.save-tsp))  
  (movq (:%q temp) (:rcontext x8664::tcr.next-tsp)))

(define-x8664-vinsn (temp-pop-double-float :pop :word)
    (((f :double-float))
     ()
     ((temp (:u64 #.x8664::ra0))))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movapd (:@ x8664::dnode-size (:%q x8664::ra0)) (:%xmm f))
  (addq (:$b (* 2 x8664::dnode-size)) (:rcontext x8664::tcr.foreign-sp)))



(define-x8664-vinsn macptr->stack (((dest :lisp))
                                   ((ptr :address))
				   ((temp :imm)
				    (stack-temp :imm)))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q stack-temp))
  (subq (:$b (+ x8664::dnode-size x8664::macptr.size)) (:rcontext x8664::tcr.foreign-sp))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q temp))
  (movq (:%q stack-temp) (:@ (:%q temp)))
  (movq (:% x8664::rbp) (:@ x8664::csp-frame.rbp (:%q temp)))
  (leaq (:@ (+ x8664::dnode-size x8664::fulltag-misc) (:%q temp)) (:%q dest))
  (movq (:$l x8664::macptr-header) (:@ x8664::macptr.header (:%q dest)))
  (movq (:%q ptr) (:@ x8664::macptr.address (:%q dest)))
  (movapd (:%xmm x8664::fpzero)  (:@ x8664::macptr.domain (:%q dest))))

(define-x8664-vinsn fixnum->signed-natural (((dest :s64))
                                            ((src :imm)))
  (movq (:%q src) (:%q dest))
  (sarq (:$ub x8664::fixnumshift) (:%q dest)))

(define-x8664-vinsn mem-set-double-float (()
					  ((val :double-float)
					   (src :address)
					   (index :s64)))
  (movsd (:%xmm val) (:@ (:%q src) (:%q  index))))

(define-x8664-vinsn mem-set-single-float (()
					  ((val :single-float)
					   (src :address)
					   (index :s64)))
  (movss (:%xmm val) (:@ (:%q src) (:%q  index))))



(define-x8664-vinsn mem-set-c-doubleword (()
                                          ((val :u64)
                                           (dest :address)
                                           (offset :s32const)))
  ((:pred = offset 0)
   (movq (:%q val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movq (:%q val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-fullword (()
                                          ((val :u32)
                                           (dest :address)
                                           (offset :s32const)))
  ((:pred = offset 0)
   (movl (:%l val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movl (:%l val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-halfword (()
                                          ((val :u16)
                                           (dest :address)
                                           (offset :s32const)))
  ((:pred = offset 0)
   (movw (:%w val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movw (:%w val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-byte (()
                                          ((val :u8)
                                           (dest :address)
                                           (offset :s32const)))
  ((:pred = offset 0)
   (movb (:%b val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movb (:%b val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-constant-doubleword (()
                                                   ((val :s32const)
                                                    (dest :address)
                                                    (offset :s32const)))
  ((:pred = offset 0)
   (movq (:$l val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movq (:$l val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-constant-fullword (()
                                                 ((val :s32const)
                                                  (dest :address)
                                                  (offset :s32const)))
  ((:pred = offset 0)
   (movl (:$l val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movl (:$l val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-constant-halfword (()
                                                 ((val :s16const)
                                                  (dest :address)
                                                  (offset :s32const)))
  ((:pred = offset 0)
   (movw (:$w val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movw (:$w val) (:@ offset (:%q dest)))))

(define-x8664-vinsn mem-set-c-constant-byte (()
                                                 ((val :s8const)
                                                  (dest :address)
                                                  (offset :s32const)))
  ((:pred = offset 0)
   (movb (:$b val) (:@ (:%q dest))))
  ((:not (:pred = offset 0))
   (movb (:$b val) (:@ offset (:%q dest)))))






(define-x8664-vinsn mem-ref-natural (((dest :u64))
                                        ((src :address)
                                         (index :s64)))
  (movq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn setup-macptr-allocation (()
                                             ((src :address)))
  (movq (:%q src) (:%q x8664::imm2))
  (movl (:$l x8664::macptr-header) (:%l x8664::imm0.l))
  (movl (:$l (- x8664::macptr.size x8664::fulltag-misc)) (:%l x8664::imm1.l)))

(define-x8664-vinsn %set-new-macptr-value (()
                                           ((ptr :lisp)))
  (movq (:%q x8664::imm2) (:@ x8664::macptr.address (:%q ptr))))

(define-x8664-vinsn mem-ref-c-fullword (((dest :u32))
					((src :address)
					 (index :s32const)))
  ((:pred = index 0)
   (movl (:@ (:%q src)) (:%l dest)))
  ((:not (:pred = index 0))
   (movl (:@ index (:%q src)) (:%l dest))))

(define-x8664-vinsn mem-ref-c-signed-fullword (((dest :s32))
                                               ((src :address)
                                                (index :s32const)))
  ((:pred = index 0)
   (movslq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movslq (:@ index (:%q src)) (:%q dest))))


(define-x8664-vinsn mem-ref-c-single-float (((dest :single-float))
                                           ((src :address)
                                            (index :s32const)))
  ((:pred = index 0)
   (movss (:@ (:%q src)) (:%xmm dest)))
  ((:not (:pred = index 0))
   (movss (:@ index (:%q src)) (:%xmm dest))))

(define-x8664-vinsn mem-set-c-single-float (()
					    ((val :single-float)
					     (src :address)
					     (index :s16const)))
  ((:pred = index 0)
   (movss (:%xmm val) (:@ (:%q src))))
  ((:not (:pred = index 0))
   (movss (:%xmm val) (:@ index (:%q src)))))

(define-x8664-vinsn mem-ref-c-doubleword (((dest :u64))
                                          ((src :address)
                                           (index :s32const)))
  ((:pred = index 0)
   (movq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn mem-ref-c-signed-doubleword (((dest :s64))
                                                 ((src :address)
                                                  (index :s32const)))
  ((:pred = index 0)
   (movq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn mem-ref-c-natural (((dest :u64))
                                       ((src :address)
                                        (index :s32const)))
  ((:pred = index 0)
   (movq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn mem-ref-c-double-float (((dest :double-float))
                                            ((src :address)
                                             (index :s32const)))
  ((:pred = index 0)
   (movsd (:@ (:%q src)) (:%xmm dest)))
  ((:not (:pred = index 0))
   (movsd (:@ index (:%q src)) (:%xmm dest))))

(define-x8664-vinsn mem-set-c-double-float (()
					    ((val :double-float)
					     (src :address)
					     (index :s16const)))
  ((:pred = index 0)
   (movsd (:%xmm val) (:@ (:%q src))))
  ((:not (:pred = index 0))
   (movsd (:%xmm val) (:@ index (:%q src)))))

(define-x8664-vinsn mem-ref-fullword (((dest :u32))
				      ((src :address)
				       (index :s64)))
  (movl (:@ (:%q src) (:%q index)) (:%l dest)))

(define-x8664-vinsn mem-ref-signed-fullword (((dest :s32))
                                             ((src :address)
                                              (index :s64)))
  (movslq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-ref-doubleword (((dest :u64))
                                        ((src :address)
                                         (index :s64)))
  (movq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-ref-natural (((dest :u64))
                                        ((src :address)
                                         (index :s64)))
  (movq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-ref-signed-doubleword (((dest :s64))
                                               ((src :address)
                                                (index :s64)))
  (movq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-ref-c-u16 (((dest :u16))
				   ((src :address)
				    (index :s32const)))
  ((:pred = index 0)  
   (movzwq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movzwq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn mem-ref-u16 (((dest :u16))
				 ((src :address)
				  (index :s64)))
  (movzwq (:@ (:%q src) (:%q index)) (:%q dest)))


(define-x8664-vinsn mem-ref-c-s16 (((dest :s16))
				   ((src :address)
				    (index :s32const)))
  ((:pred = index 0)
   (movswq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movswq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn mem-ref-s16 (((dest :s16))
				 ((src :address)
				  (index :s32)))
  (movswq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-ref-c-u8 (((dest :u8))
				  ((src :address)
				   (index :s16const)))
  ((:pred = index 0)
   (movzbq (:@  (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movzbq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn mem-ref-u8 (((dest :u8))
				((src :address)
				 (index :s32)))
  (movzbq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-ref-c-s8 (((dest :s8))
				  ((src :address)
				   (index :s16const)))
  ((:pred = index 0)
   (movsbq (:@ (:%q src)) (:%q dest)))
  ((:not (:pred = index 0))
   (movsbq (:@ index (:%q src)) (:%q dest))))

(define-x8664-vinsn misc-set-c-s8  (((val :s8))
				    ((v :lisp)
				     (idx :u32const))
				    ())
  (movb (:%b val) (:@ (:apply + x8664::misc-data-offset idx) (:%q v))))

(define-x8664-vinsn misc-set-s8  (((val :s8))
				  ((v :lisp)
				   (scaled-idx :s64))
				  ())
  (movb (:%b val) (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn mem-ref-s8 (((dest :s8))
				((src :address)
				 (index :s32)))
  (movsbq (:@ (:%q src) (:%q index)) (:%q dest)))

(define-x8664-vinsn mem-set-constant-doubleword (()
                                                 ((val :s32const)
                                                  (ptr :address)
                                                  (offset :s64)))
  (movq (:$l val) (:@ (:%q ptr) (:%q offset))))

(define-x8664-vinsn mem-set-constant-fullword (()
                                               ((val :s32const)
                                                (ptr :address)
                                                (offset :s64)))
  (movl (:$l val) (:@ (:%q ptr) (:%q offset))))


(define-x8664-vinsn mem-set-constant-halfword (()
                                               ((val :s16const)
                                                (ptr :address)
                                                (offset :s64)))
  (movw (:$w val) (:@ (:%q ptr) (:%q offset))))

(define-x8664-vinsn mem-set-constant-byte (()
                                           ((val :s8const)
                                            (ptr :address)
                                            (offset :s64)))
  (movb (:$b val) (:@ (:%q ptr) (:%q offset))))

(define-x8664-vinsn misc-set-c-u8  (((val :u8))
				    ((v :lisp)
				     (idx :u32const))
				    ())
  (movb (:%b val) (:@ (:apply + x8664::misc-data-offset idx) (:%q v))))

(define-x8664-vinsn misc-set-u8  (((val :u8))
				  ((v :lisp)
				   (scaled-idx :s64))
				  ())
  (movb (:%b val) (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn misc-set-c-u8  (((val :u8))
                                    ((v :lisp)
                                     (idx :s32const))
                                    ())
  (movb (:%b val) (:@ (:apply + x8664::misc-data-offset idx) (:%q v))))

(define-x8664-vinsn misc-set-u8  (()
				  ((val :u8)
                                   (v :lisp)
				   (scaled-idx :s64))
				  ())
  (movb (:%b val) (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn misc-set-c-u16  (()
                                    ((val :u16)
                                     (v :lisp)
                                     (idx :s32const))
                                    ())
  (movw (:%w val) (:@ (:apply + x8664::misc-data-offset (:apply * 2 idx)) (:%q v))))


(define-x8664-vinsn misc-set-u16  (()
                                   ((val :u16)
                                    (v :lisp)
                                    (scaled-idx :s64))
                                   ())
  (movw (:%w val) (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn misc-set-c-s16  (()
                                    ((val :s16)
                                     (v :lisp)
                                     (idx :s32const))
                                    ())
  (movw (:%w val) (:@ (:apply + x8664::misc-data-offset (:apply * 2 idx)) (:%q v))))


(define-x8664-vinsn misc-set-s16  (()
                                   ((val :s16)
                                    (v :lisp)
                                    (scaled-idx :s64))
                                   ())
  (movw (:%w val) (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn misc-set-c-u32  (()
				     ((val :u32)
                                      (v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movl (:%l val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 2)) (:%q v))))

(define-x8664-vinsn misc-set-u32  (()
                                   ((val :u32)
                                    (v :lisp)
                                    (scaled-idx :s64))
                                   ())
  (movl (:%l val) (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn misc-set-c-s32  (()
				     ((val :s32)
                                      (v :lisp)
				      (idx :u32const)) ; sic
				     ())
  (movl (:%l val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 2)) (:%q v))))

(define-x8664-vinsn misc-set-s32  (()
                                   ((val :s32)
                                    (v :lisp)
                                    (scaled-idx :s64))
                                   ())
  (movl (:%l val) (:@ x8664::misc-data-offset (:%q v) (:%q scaled-idx))))

(define-x8664-vinsn %iasr (((dest :lisp))
			   ((count :lisp)
			    (src :lisp))
			   ((temp (:s64 #.x8664::rax))
                            (shiftcount (:s64 #.x8664::rcx))))
  (movq (:%q count) (:%q temp))
  (sarq (:$ub x8664::fixnumshift) (:%q temp))
  (movl (:$l 63) (:%l shiftcount))
  (rcmpq (:%q temp) (:%q shiftcount))
  (cmovbel (:%l temp) (:%l shiftcount))
  (movq (:%q src) (:%q temp))
  (sarq (:%shift x8664::cl) (:%q temp))
  (andq (:$b (lognot x8664::fixnummask)) (:%q temp))
  (movq (:%q temp) (:%q dest)))

(define-x8664-vinsn %ilsr (((dest :imm))
			   ((count :imm)
			    (src :imm))
			   ((temp (:s64 #.x8664::rax))
                            (shiftcount (:s64 #.x8664::rcx))))
  (movq (:%q count) (:%q temp))
  (sarq (:$ub x8664::fixnumshift) (:%q temp))
  (movl (:$l 63) (:%l shiftcount))
  (rcmpq (:%q temp) (:%q shiftcount))
  (cmovbel (:%l temp) (:%l shiftcount))
  (movq (:%q src) (:%q temp))
  (shrq (:%shift x8664::cl) (:%q temp))
  (andq (:$b (lognot x8664::fixnummask)) (:%q temp))
  (movq (:%q temp) (:%q dest)))


(define-x8664-vinsn %iasr-c (((dest :imm))
			     ((count :u8const)
			      (src :imm))
			     ((temp :s64)))
  (movq (:%q src) (:%q temp))
  (sarq (:$ub count) (:%q temp))
  (andq (:$b (lognot x8664::fixnummask)) (:%q temp))
  (movq (:%q temp) (:%q dest)))

(define-x8664-vinsn %ilsr-c (((dest :imm))
			     ((count :u8const)
			      (src :imm))
			     ((temp :s64)))
  (movq (:%q src) (:%q temp))
  (shrq (:$ub count) (:%q temp))
  (andb (:$b (lognot x8664::fixnummask)) (:%b temp))
  (movq (:%q temp) (:%q dest)))

(define-x8664-vinsn %ilsl (((dest :imm))
			   ((count :imm)
			    (src :imm))
			   ((shiftcount (:s64 #.x8664::rcx))))
  (movl (:$l (ash 63 x8664::fixnumshift)) (:%l shiftcount))
  (rcmpq (:%q count) (:%q shiftcount))
  (cmovbl (:%l count) (:%l shiftcount))
  (sarl (:$ub x8664::fixnumshift) (:%l shiftcount))
  ((:not (:pred =
                (:apply %hard-regspec-value src)
                (:apply %hard-regspec-value dest)))
   (movq (:%q src) (:%q dest)))
  (shlq (:%shift x8664::cl) (:%q dest)))

(define-x8664-vinsn %ilsl-c (((dest :imm))
			     ((count :u8const)
			      (src :imm)))
  ((:not (:pred =
                (:apply %hard-regspec-value src)
                (:apply %hard-regspec-value dest)))
   (movq (:%q src) (:%q dest)))
  (shlq (:$ub count) (:%q dest)))

(define-x8664-vinsn fixnum-ash-left (((dest :lisp))
                                     ((num :lisp)
                                      (amt :lisp))
                                     ((shiftcount (:s64 #.x8664::rcx))))
  (movq (:%q amt) (:%q shiftcount))
  (sarq (:$ub x8664::fixnumshift) (:%q shiftcount))
  ((:not (:pred =
                (:apply %hard-regspec-value num)
                (:apply %hard-regspec-value dest)))
   (movq (:%q num) (:%q dest)))
  (shlq (:%shift x8664::cl) (:%q dest)))

(define-x8664-vinsn fixnum-ash (((dest :lisp))
                                ((num :lisp)
                                 (amt :lisp))
                                ((shiftcount (:s64 #.x8664::rcx))
                                 (temp (:s64))))
  (movq (:%q amt) (:%q shiftcount))
  (sarq (:$ub x8664::fixnumshift) (:%q shiftcount))
  (jns :left)
  (negq (:%q shiftcount))
  (movq (:%q num) (:%q temp))
  (sarq (:$ub x8664::fixnumshift) (:%q temp))
  (sarq (:%shift x8664::cl) (:%q temp))
  (imulq  (:$b x8664::fixnumone) (:%q temp)(:%q dest))
  (jmp :done)
  :left
  ((:not (:pred =
                (:apply %hard-regspec-value num)
                (:apply %hard-regspec-value dest)))
   (movq (:%q num) (:%q dest)))
  (shlq (:%shift x8664::cl) (:%q dest))
  :done)
                                    

;;; In safe code, something else has ensured that the value is of type
;;; BIT.
(define-x8664-vinsn set-variable-bit-to-variable-value (()
                                                        ((vec :lisp)
                                                         (word-index :s64)
                                                         (bitnum :u8)
                                                         (value :lisp)))
  (testb (:%b value) (:%b value))
  (je :clr)
  (btsq (:%q bitnum) (:@ x8664::misc-data-offset (:%q vec) (:%q word-index) 8))
  (jmp :done)
  :clr
  (btrq (:%q bitnum) (:@ x8664::misc-data-offset (:%q vec) (:%q word-index) 8))
  :done)

(define-x8664-vinsn set-variable-bit-to-zero (()
                                              ((vec :lisp)
                                               (word-index :s64)
                                               (bitnum :u8)))
  (btrq (:%q bitnum) (:@ x8664::misc-data-offset (:%q vec) (:%q word-index) 8)))

(define-x8664-vinsn set-variable-bit-to-one (()
                                              ((vec :lisp)
                                               (word-index :s64)
                                               (bitnum :u8)))
  (btsq (:%q bitnum) (:@ x8664::misc-data-offset (:%q vec) (:%q word-index) 8)))

;;; In safe code, something else has ensured that the value is of type
;;; BIT.
(define-x8664-vinsn nset-variable-bit-to-variable-value (()
                                                        ((vec :lisp)
                                                         (index :s64)
                                                         (value :lisp)))
  (testb (:%b value) (:%b value))
  (je :clr)
  (btsq (:%q index) (:@ x8664::misc-data-offset (:%q vec)))
  (jmp :done)
  :clr
  (btrq (:%q index) (:@ x8664::misc-data-offset (:%q vec)))
  :done)

(define-x8664-vinsn nset-variable-bit-to-zero (()
                                              ((vec :lisp)
                                               (index :s64)))
  (btrq (:%q index) (:@ x8664::misc-data-offset (:%q vec))))

(define-x8664-vinsn nset-variable-bit-to-one (()
                                              ((vec :lisp)
                                               (index :s64)))
  (btsq (:%q index) (:@ x8664::misc-data-offset (:%q vec))))

(define-x8664-vinsn set-constant-bit-to-zero (()
                                              ((src :lisp)
                                               (idx :u64const)))
  (btrq (:$ub (:apply logand 63 idx))
        (:@ (:apply + x8664::misc-data-offset (:apply ash (:apply ash idx -6) x8664::word-shift)) (:%q src))))

(define-x8664-vinsn set-constant-bit-to-one (()
                                             ((src :lisp)
                                              (idx :u64const)))
  (btsq (:$ub (:apply logand 63 idx))
        (:@ (:apply + x8664::misc-data-offset (:apply ash (:apply ash idx -6) x8664::word-shift)) (:%q src))))

(define-x8664-vinsn set-constant-bit-to-variable-value (()
                                                        ((src :lisp)
                                                         (idx :u64const)
                                                         (value :lisp)))
  (testb (:%b value) (:%b value))
  (je :clr)
  (btsq (:$ub (:apply logand 63 idx))
        (:@ (:apply + x8664::misc-data-offset (:apply ash (:apply ash idx -6) x8664::word-shift)) (:%q src)))
  (jmp :done)
  :clr
  (btrq (:$ub (:apply logand 63 idx))
        (:@ (:apply + x8664::misc-data-offset (:apply ash (:apply ash idx -6) x8664::word-shift)) (:%q src)))
  :done)


(define-x8664-vinsn require-fixnum (()
                                    ((object :lisp)))
  :again
  (testl (:$l x8664::fixnummask) (:%l object))
  (jne :bad)
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-fixnum))))

(define-x8664-vinsn require-integer (()
                                     ((object :lisp))
                                     ((tag :u8)))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::fixnummask) (:%l tag))
  (je :got-it)
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :bad)
  (cmpb (:$b x8664::subtag-bignum) (:@ x8664::misc-subtag-offset (:%q object)))
  (jne :bad)
  :got-it

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-integer))))

(define-x8664-vinsn require-simple-vector (()
                                           ((object :lisp))
                                           ((tag :u8)))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::fixnummask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :bad)
  (cmpb (:$b x8664::subtag-simple-vector) (:@ x8664::misc-subtag-offset (:%q object)))
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-simple-vector))))

(define-x8664-vinsn require-simple-string (()
                                           ((object :lisp))
                                           ((tag :u8)))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::fixnummask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :bad)
  (cmpb (:$b x8664::subtag-simple-base-string) (:@ x8664::misc-subtag-offset (:%q object)))
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-simple-string))))
                                    
(define-x8664-vinsn require-real (()
                                    ((object :lisp))
                                    ((tag :u8)
                                     (mask :u64)))
  (movq (:$q (logior (ash 1 x8664::tag-fixnum)
                     (ash 1 x8664::tag-single-float)
                     (ash 1 x8664::subtag-double-float)
                     (ash 1 x8664::subtag-bignum)
                     (ash 1 x8664::subtag-ratio)))
        (:%q mask))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :have-tag)
  (movzbl (:@ x8664::misc-subtag-offset (:%q object)) (:%l tag))
  :have-tag
  (rcmpl (:%l tag) (:$b 64))
  (jae :bad)
  (btq (:%q tag) (:%q mask))
  (jae :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-real))))

(define-x8664-vinsn require-number (()
                                    ((object :lisp))
                                    ((tag :u8)
                                     (mask :u64)))
  (movq (:$q (logior (ash 1 x8664::tag-fixnum)
                     (ash 1 x8664::tag-single-float)
                     (ash 1 x8664::subtag-double-float)
                     (ash 1 x8664::subtag-bignum)
                     (ash 1 x8664::subtag-ratio)
                     (ash 1 x8664::subtag-complex)))
        (:%q mask))
  :again
  (movl (:%l object) (:%l tag))  
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :have-tag)
  (movzbl (:@ x8664::misc-subtag-offset (:%q object)) (:%l tag))
  :have-tag
  (cmpl (:$b x8664::subtag-complex-single-float) (:%l tag))
  (jz :good)
  (cmpl (:$b x8664::subtag-complex-double-float) (:%l tag))
  (jz :good)
  (rcmpl (:%l tag) (:$b 64))
  (jae :bad)
  (btq (:%q tag) (:%q mask))
  (jae :bad)
  :good
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-number))))

(define-x8664-vinsn require-list (()
                                  ((object :lisp))
                                  ((tag :u8)))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-list) (:%l tag))
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-list))))

(define-x8664-vinsn require-symbol (()
                                    ((object :lisp))
                                    ((tag :u8)))
  :again
  (movzbl (:%b object) (:%l tag))
  (cmpl (:$b x8664::fulltag-nil) (:%l tag))
  (je :good)
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-symbol) (:%l tag))
  (jne :bad)
  :good

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-symbol))))

(define-x8664-vinsn require-character (()
				((object :lisp)))
  :again
  (cmpb (:$b x8664::subtag-character) (:%b object))
  (jne :bad)
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-character))))

(define-x8664-vinsn require-s8 (()
				((object :lisp))
				((tag :u32)))
  :again
  (movq (:%q object) (:%q tag))
  (shlq (:$ub (- x8664::nbits-in-word (+ 8 x8664::fixnumshift))) (:%q tag))
  (sarq (:$ub (- x8664::nbits-in-word 8)) (:%q tag))
  (shlq (:$ub x8664::fixnumshift) (:%q tag))
  (cmpq (:%q object) (:%q tag))
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-signed-byte-8))))

(define-x8664-vinsn require-u8 (()
				((object :lisp))
				((tag :u32)))
  :again
  (movq (:$l (lognot (ash #xff x8664::fixnumshift))) (:%q tag))
  (andq (:% object) (:% tag))
  (jne :bad)
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-unsigned-byte-8))))

(define-x8664-vinsn require-s16 (()
				((object :lisp))
				((tag :s64)))
  :again
  (movq (:%q object) (:%q tag))
  (shlq (:$ub (- x8664::nbits-in-word (+ 16 x8664::fixnumshift))) (:%q tag))
  (sarq (:$ub (- x8664::nbits-in-word 16)) (:%q tag))
  (shlq (:$ub x8664::fixnumshift) (:%q tag))
  (cmpq (:%q object) (:%q tag))
  (jne :bad)
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-signed-byte-16))))

(define-x8664-vinsn require-u16 (()
				((object :lisp))
				((tag :u32)))
  :again
  (movq (:$l (lognot (ash #xffff x8664::fixnumshift))) (:%q tag))
  (andq (:% object) (:% tag))
  (jne :bad)
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-unsigned-byte-16))))

(define-x8664-vinsn require-s32 (()
				((object :lisp))
				((tag :s64)))
  :again
  (movq (:%q object) (:%q tag))
  (shlq (:$ub (- x8664::nbits-in-word (+ 32 x8664::fixnumshift))) (:%q tag))
  (sarq (:$ub (- x8664::nbits-in-word 32)) (:%q tag))
  (shlq (:$ub x8664::fixnumshift) (:%q tag))
  (cmpq (:%q object) (:%q tag))
  (jne :bad)
  (testl (:$l x8664::fixnummask) (:%l object))
  (jne :bad)
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-signed-byte-32))))

(define-x8664-vinsn require-u32 (()
                                 ((object :lisp))
                                 ((tag :u32)))
  :again
  (movq (:$q (lognot (ash #xffffffff x8664::fixnumshift))) (:%q tag))
  (andq (:% object) (:% tag))
  (jne :bad)
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-unsigned-byte-32))))

(define-x8664-vinsn require-s64 (()
				((object :lisp))
				((tag :s64)))
  :again
  (testl (:$l x8664::fixnummask) (:%l object))
  (movl (:%l object) (:%l tag))
  (je :ok)
  (andl (:$b x8664::fulltagmask) (:%l tag))
  (cmpl (:$b x8664::fulltag-misc) (:%l tag))
  (jne :bad)
  (cmpq (:$l x8664::two-digit-bignum-header) (:@ x8664::misc-header-offset (:%q object)))
  (jne :bad)
  :ok
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-signed-byte-64))))

(define-x8664-vinsn require-u64 (()
				((object :lisp))
				((tag :s64)))
  :again
  (testl (:$l x8664::fixnummask) (:%l object))
  (movq (:%q object) (:%q tag))
  (je :ok-if-non-negative)
  (andl (:$b x8664::fulltagmask) (:%l tag))
  (cmpl (:$b x8664::fulltag-misc) (:%l tag))
  (jne :bad)
  (cmpq (:$l x8664::two-digit-bignum-header) (:@ x8664::misc-header-offset (:%q object)))
  (je :two)
  (cmpq (:$l x8664::three-digit-bignum-header) (:@ x8664::misc-header-offset (:%q object)))
  (jne :bad)
  (cmpl (:$b 0) (:@ (+ x8664::misc-data-offset 8) (:%q object)))
  (je :ok)
  (jmp :bad)
  :two
  (movq (:@ x8664::misc-data-offset (:%q object)) (:%q tag))
  :ok-if-non-negative
  (testq (:%q tag) (:%q tag))
  (js :bad)
  :ok
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-unsigned-byte-64))))

(define-x8664-vinsn require-char-code (()
                                       ((object :lisp))
                                       ((tag :u32)))
  :again
  (testl (:$l x8664::fixnummask) (:%l object))
  (jne :bad)
  (cmpq (:$l (ash #x110000 x8664::fixnumshift)) (:%q object))
  (jae :bad)
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub arch::error-object-not-mod-char-code-limit))))


;;; set DEST to 
(define-x8664-vinsn mask-base-char (((dest :u8))
                                    ((src :lisp)))
  (movzbl (:%b src) (:%l dest))) 

(define-x8664-vinsn single-float-bits (((dest :u32))
                                       ((src :lisp)))
  (movq (:%q src) (:%q dest))
  (shrq (:$ub 32) (:%q dest)))

(define-x8664-vinsn zero-double-float-register (((dest :double-float))
                                                ())
  (movsd (:%xmm x8664::fpzero) (:%xmm dest)))

(define-x8664-vinsn zero-single-float-register (((dest :single-float))
                                                ())
  (movss (:%xmm x8664::fpzero) (:%xmm dest)))

(define-x8664-subprim-lea-jmp-vinsn (heap-rest-arg) .SPheap-rest-arg)
(define-x8664-subprim-lea-jmp-vinsn (stack-rest-arg) .SPstack-rest-arg)
(define-x8664-subprim-lea-jmp-vinsn (req-stack-rest-arg) .SPreq-stack-rest-arg)

(define-x8664-subprim-call-vinsn (stack-misc-alloc) .SPstack-misc-alloc)

(define-x8664-vinsn misc-element-count-fixnum (((dest :imm))
                                               ((src :lisp))
                                               ((temp :u64)))
  (movq (:@ x8664::misc-header-offset (:%q src)) (:%q temp))
  (shrq (:$ub x8664::num-subtag-bits) (:%q temp))
  (imulq (:$b x8664::fixnumone) (:%q temp)(:%q dest)))

(define-x8664-vinsn %logior2 (((dest :imm))
                              ((x :imm)
                               (y :imm)))
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (orq (:%q y) (:%q dest)))
  ((:not (:pred =
                (:apply %hard-regspec-value x)
                (:apply %hard-regspec-value dest)))
   ((:pred =
           (:apply %hard-regspec-value y)
           (:apply %hard-regspec-value dest))
    (orq (:%q x) (:%q dest)))
   ((:not (:pred =
                 (:apply %hard-regspec-value y)
                 (:apply %hard-regspec-value dest)))
    (movq (:%q x) (:%q dest))
    (orq (:%q y) (:%q dest)))))

(define-x8664-vinsn %logand2 (((dest :imm))
                              ((x :imm)
                               (y :imm)))
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (andq (:%q y) (:%q dest)))
  ((:not (:pred =
                (:apply %hard-regspec-value x)
                (:apply %hard-regspec-value dest)))
   ((:pred =
           (:apply %hard-regspec-value y)
           (:apply %hard-regspec-value dest))
    (andq (:%q x) (:%q dest)))
   ((:not (:pred =
                 (:apply %hard-regspec-value y)
                 (:apply %hard-regspec-value dest)))
    (movq (:%q x) (:%q dest))
    (andq (:%q y) (:%q dest)))))

(define-x8664-vinsn %logxor2 (((dest :imm))
                              ((x :imm)
                               (y :imm)))
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (xorq (:%q y) (:%q dest)))
  ((:not (:pred =
                (:apply %hard-regspec-value x)
                (:apply %hard-regspec-value dest)))
   ((:pred =
           (:apply %hard-regspec-value y)
           (:apply %hard-regspec-value dest))
    (xorq (:%q x) (:%q dest)))
   ((:not (:pred =
                 (:apply %hard-regspec-value y)
                 (:apply %hard-regspec-value dest)))
    (movq (:%q x) (:%q dest))
    (xorq (:%q y) (:%q dest)))))


(define-x8664-vinsn vcell-ref (((dest :lisp))
			       ((vcell :lisp)))
  (movq (:@ x8664::misc-data-offset (:%q vcell)) (:%q dest)))

(define-x8664-vinsn (call-subprim-3 :call :subprim) (((dest t))
							  ((spno :s32const)
							   (x t)
							   (y t)
							   (z t))
                                                          ((entry (:label 1))))
  (:talign 4)
  (call (:@ spno))
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn)))

(define-x8664-vinsn setup-vcell-allocation (()
                                            ())
  (movl (:$l x8664::value-cell-header) (:%l x8664::imm0))
  (movl (:$l (- x8664::value-cell.size x8664::fulltag-misc)) (:%l x8664::imm1)))

(define-x8664-vinsn %init-vcell (()
                                 ((vcell :lisp)
                                  (closed :lisp)))
  (movq (:%q closed) (:@ x8664::value-cell.value (:%q vcell))))

(define-x8664-subprim-call-vinsn (progvsave :needs-frame-pointer) .SPprogvsave)

(define-x8664-subprim-jump-vinsn (progvrestore :call :needs-frame-pointer) .SPprogvrestore)

(define-x8664-subprim-lea-jmp-vinsn (simple-keywords) .SPsimple-keywords)

(define-x8664-subprim-lea-jmp-vinsn (keyword-args) .SPkeyword-args)

(define-x8664-subprim-lea-jmp-vinsn (keyword-bind) .SPkeyword-bind)

(define-x8664-vinsn scale-nargs (()
				 ((nfixed :s16const)))
  ((:pred > nfixed 0)
   ((:pred < nfixed 16)
    (subl (:$b (:apply ash nfixed x8664::word-shift)) (:%l x8664::nargs)))
   ((:pred >= nfixed 16)
    (subl (:$l (:apply ash nfixed x8664::word-shift)) (:%l x8664::nargs)))))

(define-x8664-vinsn opt-supplied-p (()
                                    ((num-opt :u16const))
                                    ((nargs (:u64 #.x8664::nargs))
                                     (imm :imm)))
  (xorl (:%l imm) (:%l imm))
  (movl (:$l (:apply target-nil-value)) (:%l x8664::arg_y))
  :loop
  (rcmpl (:%l imm) (:%l nargs))
  (movl (:%l x8664::arg_y) (:%l x8664::arg_z))
  (cmovll (:@ (+ x8664::t-offset x8664::symbol.vcell) (:%l x8664::arg_y)) (:%l  x8664::arg_z))
  (addl (:$b x8664::node-size) (:%l imm))
  (rcmpl (:%l imm) (:$l (:apply ash num-opt x8664::fixnumshift)))
  (pushq (:%q x8664::arg_z))
  (jne :loop))

(define-x8664-vinsn one-opt-supplied-p (()
                                        ()
                                        ((temp :u64)))
  (testl (:%l x8664::nargs) (:%l x8664::nargs))
  (movl (:$l (:apply target-nil-value)) (:%l temp))
  (cmovnel (:@ (+ x8664::t-offset x8664::symbol.vcell) (:%l temp)) (:%l temp))
  (pushq (:%q temp)))

(define-x8664-vinsn two-opt-supplied-p (()
                                        ()
                                        ((temp0 :u64)
                                         (temp1 :u64)))
  (rcmpl (:%l x8664::nargs) (:$b x8664::node-size))
  (movl (:$l (:apply target-nil-value)) (:%l temp0))
  (movl (:%l temp0) (:%l temp1))
  (cmovael (:@ (+ x8664::t-offset x8664::symbol.vcell) (:%l temp0)) (:%l temp0))
  (cmoval (:@ (+ x8664::t-offset x8664::symbol.vcell) (:%l temp1)) (:%l temp1))
  (pushq (:%q temp0))
  (pushq (:%q temp1)))


(define-x8664-vinsn set-c-flag-if-constant-logbitp (()
                                                    ((bit :u8const)
                                                     (int :imm)))
  (btq (:$ub bit) (:%q int)))

(define-x8664-vinsn set-c-flag-if-variable-logbitp (()
                                                    ((bit :imm)
                                                     (int :imm))
                                                    ((temp0 :u8)
                                                     (temp1 :u8)))
  (movl (:$l 63) (:%l temp1))
  (movq (:%q bit) (:%q temp0))
  (sarq (:$ub x8664::fixnumshift) (:%q temp0))
  (addq (:$b x8664::fixnumshift) (:%q temp0))
  (rcmpq (:%q temp0) (:%q temp1))
  (cmoval (:%l temp1) (:%l temp0))
  (btq (:%q temp0) (:%q int)))

(define-x8664-vinsn multiply-immediate (((dest :imm))
                                        ((src :imm)
                                         (const :s32const)))
  ((:and (:pred >= const -128) (:pred <= const 127))
   (imulq (:$b const) (:%q src) (:%q dest)))
  ((:not (:and (:pred >= const -128) (:pred <= const 127)))
   (imulq (:$l const) (:%q src) (:%q dest))))

(define-x8664-vinsn multiply-fixnums (((dest :imm))
                                      ((x :imm)
                                       (y :imm))
                                      ((unboxed :s64)))
  ((:pred =
          (:apply %hard-regspec-value x)
          (:apply %hard-regspec-value dest))
   (movq (:%q y) (:%q unboxed))
   (sarq (:$ub x8664::fixnumshift) (:%q unboxed))
   (imulq (:%q unboxed) (:%q dest)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value x)
                      (:apply %hard-regspec-value dest)))
         (:pred =
                (:apply %hard-regspec-value y)
                (:apply %hard-regspec-value dest)))
   (movq (:%q x) (:%q unboxed))
   (sarq (:$ub x8664::fixnumshift) (:%q unboxed))
   (imulq (:%q unboxed) (:%q dest)))
  ((:and (:not (:pred =
                      (:apply %hard-regspec-value x)
                      (:apply %hard-regspec-value dest)))
         (:not (:pred =
                      (:apply %hard-regspec-value y)
                      (:apply %hard-regspec-value dest))))
   (movq (:%q y) (:%q dest))
   (movq (:%q x) (:%q unboxed))
   (sarq (:$ub x8664::fixnumshift) (:%q unboxed))
   (imulq (:%q unboxed) (:%q dest))))

   
(define-x8664-vinsn save-lexpr-argregs (()
                                        ((min-fixed :u16const)))
  ((:pred >= min-fixed $numx8664argregs)
   (pushq (:%q x8664::arg_x))
   (pushq (:%q x8664::arg_y))
   (pushq (:%q x8664::arg_z)))
  ((:pred = min-fixed 2)                ; at least 2 args
   (cmpl (:$b (ash 2 x8664::word-shift)) (:%l x8664::nargs))
   (je :yz2)                      ; skip arg_x if exactly 2
   (pushq (:%q x8664::arg_x))
   :yz2
   (pushq (:%q x8664::arg_y))
   (pushq (:%q x8664::arg_z)))
  ((:pred = min-fixed 1)                ; at least one arg
   (rcmpl (:%l x8664::nargs) (:$b  (ash 2 x8664::word-shift)))
   (jl :z1)                       ; branch if exactly one
   (je :yz1)                      ; branch if exactly two
   (pushq (:%q x8664::arg_x))
   :yz1
   (pushq (:%q x8664::arg_y))
   :z1
   (pushq (:%q x8664::arg_z)))
  ((:pred = min-fixed 0)
   (testl (:%l x8664::nargs) (:%l x8664::nargs))
   (je  :none)                     ; exactly zero
   (rcmpl (:%l x8664::nargs) (:$b (ash 2 x8664::word-shift)))
   (je :yz0)                      ; exactly two
   (jl :z0)                       ; one
                                        ; Three or more ...
   (pushq (:%q x8664::arg_x))
   :yz0
   (pushq (:%q x8664::arg_y))
   :z0
   (pushq (:%q x8664::arg_z))
   :none
   )
  ((:not (:pred = min-fixed 0))
   (leaq (:@ (:apply - (:apply ash min-fixed x8664::word-shift)) (:%q x8664::nargs))
         (:%q x8664::nargs)))
  (pushq (:%q x8664::nargs))
  (movq (:%q x8664::rsp) (:%q x8664::arg_z)))




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
(define-x8664-vinsn (build-lexpr-frame :needs-frame-pointer) (()
                                       ()
                                       ((temp :imm)))
  (movq (:@ (:apply + (:apply target-nil-value) (x8664::%kernel-global 'x86::ret1valaddr)))
        (:%q temp))
  (cmpq (:%q temp)
        (:%q x8664::ra0))
  (je :multiple)
  (pushq (:@ (:apply + (:apply target-nil-value) (x8664::%kernel-global 'x86::lexpr-return1v))))
  (jmp :finish)
  :multiple
  (pushq (:@ (:apply + (:apply target-nil-value) (x8664::%kernel-global 'x86::lexpr-return))))
  (pushq (:%q temp))
  :finish
  (pushq (:%q x8664::rbp))
  (movq (:%q x8664::rsp) (:%q x8664::rbp)))


(define-x8664-vinsn copy-lexpr-argument (()
					 ((n :u16const))
					 ((temp :imm)))
  (movq (:@ (:%q x8664::arg_z)) (:%q temp))
  (pushq (:@ (:apply ash n x8664::word-shift) (:%q x8664::arg_z) (:%q temp))))


(define-x8664-vinsn %current-tcr (((dest :lisp))
                                 ())
  (movq (:rcontext x8664::tcr.linear) (:%q dest)))

(define-x8664-vinsn (setq-special :call :subprim)
    (()
     ((sym :lisp)
      (val :lisp))
     ((entry (:label 1))))
  (:talign 4)
  (call (:@ .SPspecset))
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn)))

(define-x8664-vinsn set-z-flag-if-istruct-typep (()
                                                 ((val :lisp)
                                                  (type :lisp))
                                                 ((tag :u8)
                                                  (valtype :lisp)))
  (xorl (:%l valtype) (:%l valtype))
  (movl (:%l val) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :have-tag)
  (movzbl (:@ x8664::misc-subtag-offset (:%q val)) (:%l tag))
  :have-tag
  (cmpl (:$b x8664::subtag-istruct) (:%l tag))
  (jne :do-compare)
  (movq (:@ x8664::misc-data-offset (:%q val)) (:%q valtype))
  :do-compare
  (cmpq (:%q valtype) (:%q type)))

(define-x8664-subprim-call-vinsn (misc-ref) .SPmisc-ref)

(define-x8664-subprim-call-vinsn (ksignalerr) .SPksignalerr)

(define-x8664-subprim-call-vinsn (misc-alloc-init) .SPmisc-alloc-init)

(define-x8664-subprim-call-vinsn (misc-alloc) .SPmisc-alloc) 

(define-x8664-subprim-lea-jmp-vinsn (make-stack-gvector)  .SPstkgvector)

(define-x8664-vinsn load-character-constant (((dest :lisp))
                                             ((code :u32const))
                                             ())
  (movl (:$l (:apply logior (:apply ash code 8) x8664::subtag-character))
        (:%l dest)))

(define-x8664-vinsn %scharcode8 (((code :imm))
				((str :lisp)
				 (idx :imm))
				((imm :u64)))
  (movq (:%q idx) (:%q imm))
  (sarq (:$ub x8664::fixnumshift) (:%q imm))
  (movzbl (:@ x8664::misc-data-offset (:%q str) (:%q imm)) (:%l imm))
  (imulq (:$b x8664::fixnumone) (:%q imm)(:%q code)))

(define-x8664-vinsn %scharcode32 (((code :imm))
				((str :lisp)
				 (idx :imm))
				((imm :u64)))
  (movq (:%q idx) (:%q imm))
  (sarq (:$ub 1) (:%q imm))
  (movl (:@ x8664::misc-data-offset (:%q str) (:%q imm)) (:%l imm))
  (imulq (:$b x8664::fixnumone) (:%q imm)(:%q code)))

(define-x8664-subprim-jump-vinsn (tail-call-sym-slide :needs-frame-pointer) .SPtcallsymslide)

(define-x8664-subprim-jump-vinsn (tail-call-sym-vsp :needs-frame-pointer) .SPtcallsymvsp)


(define-x8664-vinsn character->code (((dest :u32))
				     ((src :lisp)))
  (movq (:%q src) (:%q dest))
  (sarq (:$ub x8664::charcode-shift) (:%q  dest)))

(define-x8664-vinsn (adjust-vsp :vsp :pop :discard)
    (()
     ((amount :s32const)))
  ((:and (:pred >= amount -128) (:pred <= amount 127))
   (addq (:$b amount) (:%q x8664::rsp)))
  ((:not (:and (:pred >= amount -128) (:pred <= amount 127)))
   (addq (:$l amount) (:%q x8664::rsp))))

(define-x8664-vinsn (call-subprim-2 :call :subprim) (((dest t))
							  ((spno :s32const)
							   (y t)
							   (z t))
                                                          ((entry (:label 1))))
  (:talign 4)
  (call (:@ spno))
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn)))

(define-x8664-vinsn (call-subprim-1 :call :subprim) (((dest t))
							  ((spno :s32const)
							   (x t))
                                                          ((entry (:label 1))))
  (:talign 4)
  (call (:@ spno))
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn)))



(define-x8664-vinsn set-macptr-address (()
					((addr :address)
					 (src :lisp))
					())
  (movq (:%q addr) (:@ x8664::macptr.address (:%q src))))

(define-x8664-vinsn %symbol->symptr (((dest :lisp))
                                     ((src :lisp))
                                     ((tag :u8)))
  :begin
  (movl (:$l (:apply + (:apply target-nil-value) x8664::nilsym-offset)) (:%l tag))
  (cmpb (:$b x8664::fulltag-nil) (:%b src))
  (cmoveq (:%q tag) (:%q dest))
  (movl (:%l src) (:%l tag))
  (je :ok)
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-symbol) (:%l tag))
  (jne :bad)

  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movq (:% src) (:% dest)))
  :ok
  (:anchored-uuo-section :begin)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%q src) (:$ub x8664::fulltag-symbol))))

(define-x8664-vinsn symbol-function (((val :lisp))
                                     ((sym (:lisp (:ne val))))
                                     ((tag :u8)))
  :anchor
  (movq (:@ x8664::symbol.fcell (:%q sym)) (:%q val))
  (movl (:%l val) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-function) (:%l tag))
  (jne :bad)
  
  (:anchored-uuo-section :anchor)
  :bad
  (:anchored-uuo (uuo-error-udf (:%q sym))))

(define-x8664-subprim-jump-vinsn (tail-call-fn-slide :needs-frame-pointer) .SPtcallnfnslide)

(define-x8664-vinsn load-double-float-constant (((dest :double-float))
                                                ((lab :label)
))
  (movsd (:@ (:^ lab) (:%q x8664::fn)) (:%xmm dest)))

(define-x8664-vinsn load-single-float-constant (((dest :single-float))
                                                ((lab :label)
))
  (movss (:@ (:^ lab) (:%q x8664::fn)) (:%xmm dest)))

(define-x8664-subprim-call-vinsn (misc-set) .SPmisc-set)

(define-x8664-subprim-lea-jmp-vinsn (slide-values :needs-frame-pointer) .SPmvslide)

(define-x8664-subprim-lea-jmp-vinsn (spread-list)  .SPspreadargz)

;;; Even though it's implemented by calling a subprim, THROW is really
;;; a JUMP (to a possibly unknown destination).  If the destination's
;;; really known, it should probably be inlined (stack-cleanup, value
;;; transfer & jump ...)
(define-x8664-vinsn (throw :jump-unknown :needs-frame-pointer) (()
                                           ()
                                           ((entry (:label 1))))
  (leaq (:@ (:^ :back) (:%q x8664::fn)) (:%q x8664::ra0))
  (:talign 4)
  (jmp (:@ .SPthrow))
  :back
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn))
  (uuo-error-reg-not-tag (:%q x8664::temp0) (:$ub x8664::subtag-catch-frame)))



(define-x8664-vinsn unbox-base-char (((dest :u64))
				     ((src :lisp)))
  :anchor
  (movq (:%q src) (:%q dest))
  (shrq (:$ub x8664::charcode-shift) (:%q dest))
  (cmpb (:$b x8664::subtag-character) (:%b src))
  (jne :bad)
  (:anchored-uuo-section :anchor)
  :bad
  (:anchored-uuo (uuo-error-reg-not-tag (:%q src) (:$ub x8664::subtag-character))))

(define-x8664-subprim-lea-jmp-vinsn (save-values) .SPsave-values)

(define-x8664-subprim-lea-jmp-vinsn (recover-values)  .SPrecover-values)

(define-x8664-subprim-lea-jmp-vinsn (recover-values-for-mvcall) .SPrecover-values-for-mvcall)

(define-x8664-subprim-lea-jmp-vinsn (add-values) .SPadd-values)

(define-x8664-subprim-call-vinsn (make-stack-block)  .SPmakestackblock)

(define-x8664-subprim-call-vinsn (make-stack-block0)  .Spmakestackblock0)

;;; "dest" is preallocated, presumably on a stack somewhere.
(define-x8664-vinsn store-double (()
				  ((dest :lisp)
				   (source :double-float))
				  ())
  (movsd (:%xmm source) (:@  x8664::double-float.value (:%q dest))))

(define-x8664-vinsn fixnum->char (((dest :lisp))
				  ((src :imm))
				  ((temp :u32)))
  (movl (:%l src) (:%l temp))
  (sarl (:$ub (+ x8664::fixnumshift 11)) (:%l temp))
  (cmpl (:$b (ash #xd800 -11))(:%l temp))
  (movl (:$l (:apply target-nil-value)) (:%l temp))
  (cmovel (:%l temp) (:%l dest))
  (je :done)
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movl (:%l src) (:%l dest)))
  (shll (:$ub (- x8664::charcode-shift x8664::fixnumshift)) (:%l dest))
  (addl (:$b x8664::subtag-character) (:%l dest))
  :done)

;;; src is known to be a code for which CODE-CHAR returns non-nil.
(define-x8664-vinsn code-char->char (((dest :lisp))
				  ((src :imm))
				  ())
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movl (:%l src) (:%l dest)))
  (shll (:$ub (- x8664::charcode-shift x8664::fixnumshift)) (:%l dest))
  (addl (:$b x8664::subtag-character) (:%l dest))
  :done)


(define-x8664-vinsn sign-extend-halfword (((dest :imm))
					  ((src :imm)))
  (movq (:%q src ) (:%q dest))
  (shlq (:$ub (- 48 x8664::fixnumshift)) (:%q dest))
  (sarq (:$ub (- 48 x8664::fixnumshift)) (:%q dest)))

(define-x8664-subprim-jump-vinsn (tail-funcall-gen :needs-frame-pointer) .SPtfuncallgen)

(define-x8664-subprim-call-vinsn (gets64) .SPgets64)

(define-x8664-subprim-call-vinsn (getu64) .SPgetu64)

(define-x8664-vinsn %init-gvector (()
                                   ((v :lisp)
                                    (nbytes :u32const))
                                   ((count :imm)))
  (movl (:$l nbytes) (:%l count))
  (jmp :test)
  :loop
  (popq (:@ x8664::misc-data-offset (:%q v) (:%q count)))
  :test
  (subq (:$b x8664::node-size) (:%q count))
  (jge :loop))

(define-x8664-subprim-jump-vinsn (tail-funcall-slide :needs-frame-pointer) .SPtfuncallslide)

(define-x8664-vinsn nth-value (((result :lisp))
                               ()
                               ((imm0 :u64)))
  (leaq (:@ (:%q x8664::rsp) (:%q x8664::nargs)) (:%q imm0))
  (subq (:@ (:%q imm0)) (:%q x8664::nargs))
  (movl (:$l (:apply target-nil-value)) (:%l result))
  (jle :done)
  ;; I -think- that a CMOV would be safe here, assuming that N wasn't
  ;; extremely large.  Don't know if we can assume that.
  (movq (:@ (- x8664::node-size) (:%q x8664::rsp) (:%q x8664::nargs)) (:%q result))
  :done
  (leaq (:@ x8664::node-size (:%q imm0)) (:%q x8664::rsp)))


(define-x8664-subprim-lea-jmp-vinsn (req-heap-rest-arg) .SPreq-heap-rest-arg)

(define-x8664-subprim-call-vinsn (stack-misc-alloc-init)  .SPstack-misc-alloc-init)

(define-x8664-vinsn fixnum->unsigned-natural (((dest :u64))
                                              ((src :imm)))
  (movq (:%q src) (:%q dest))
  (shrq (:$ub x8664::fixnumshift) (:%q dest)))

(define-x8664-vinsn %debug-trap (()
                                 ())
  (uuo-error-debug-trap))

(define-x8664-vinsn double-to-single (((result :single-float))
                                      ((arg :double-float)))
  (cvtsd2ss (:%xmm arg) (:%xmm result)))

(define-x8664-vinsn single-to-double (((result :double-float))
                                      ((arg :single-float)))
  (cvtss2sd (:%xmm arg) (:%xmm result)))


(define-x8664-vinsn alloc-c-frame (()
                                   ((nwords :u32const))
				   ((temp :imm)
				    (stack-temp :imm)))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q stack-temp))
  ((:pred < (:apply ash (:apply logandc2 (:apply + nwords 9) 1) x8664::word-shift) 128)
   (subq (:$b (:apply ash (:apply logandc2 (:apply + nwords 9) 1) x8664::word-shift)) (:rcontext x8664::tcr.foreign-sp)))
  ((:not (:pred < (:apply ash (:apply logandc2 (:apply + nwords 9) 1) x8664::word-shift) 128))
   (subq (:$l (:apply ash (:apply logandc2 (:apply + nwords 9) 1) x8664::word-shift)) (:rcontext x8664::tcr.foreign-sp)))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q temp))
  (movq (:%q stack-temp) (:@ (:%q temp)))
  (movq (:% x8664::rbp) (:@ x8664::csp-frame.rbp (:%q temp))))

(define-x8664-vinsn alloc-variable-c-frame (()
                                            ((nwords :imm))
                                            ((size :s64)
					     (temp :imm)
					     (stack-temp :imm)))
  (leaq (:@ (* 9 x8664::node-size) (:%q nwords)) (:%q size))
  (andb (:$b (lognot x8664::fulltagmask)) (:%b size))

  (movq (:rcontext x8664::tcr.foreign-sp) (:%q stack-temp))
  (subq (:%q size) (:rcontext x8664::tcr.foreign-sp))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q temp))
  (movq (:%q stack-temp) (:@ (:%q temp)))
  (movq (:% x8664::rbp) (:@ x8664::csp-frame.rbp (:%q temp))))

(define-x8664-vinsn set-c-arg (()
                               ((arg :u64)
                                (offset :u32const))
                               ((temp (:u64 #.x8664::ra0))))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movq (:%q arg) (:@ (:apply + 16 (:apply ash offset 3)) (:%q x8664::ra0))))

(define-x8664-vinsn set-single-c-arg (()
                                      ((arg :single-float)
                                       (offset :u32const))
                                      ((temp (:u64 #.x8664::ra0))))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movss (:%xmm arg) (:@ (:apply + 16 (:apply ash offset 3)) (:%q x8664::ra0))))

(define-x8664-vinsn reload-single-c-arg (((arg :single-float))
                                         ((offset :u32const))
                                         ((u64 (:u64 #.x8664::ra0))))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movss (:@ (:apply + 16 (:apply ash offset 3)) (:%q x8664::ra0)) (:%xmm arg)))

(define-x8664-vinsn set-double-c-arg (()
                                      ((arg :double-float)
                                       (offset :u32const))
                                      ((temp (:u64 #.x8664::ra0))))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movsd (:%xmm arg) (:@ (:apply + 16 (:apply ash offset 3)) (:%q x8664::ra0))))

(define-x8664-vinsn reload-double-c-arg (((arg :double-float))
                                         ((offset :u32const))
                                         ((temp (:u64 #.x8664::ra0))))
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q x8664::ra0))
  (movsd (:@ (:apply + 16 (:apply ash offset 3)) (:%q x8664::ra0)) (:%xmm arg)))

(define-x8664-subprim-call-vinsn (ff-call)  .SPffcall)

(define-x8664-subprim-call-vinsn (ff-call-return-registers)  .SPffcall-return-registers)

(define-x8664-subprim-call-vinsn (syscall)  .SPsyscall)

(define-x8664-subprim-call-vinsn (setqsym) .SPsetqsym)

(define-x8664-vinsn recover-fn-from-rip (()
                                         ())
  (leaq (:@ (:apply - (:^ :disp)) (:%q x8664::rip)) (:%q x8664::fn))
  :disp)



(define-x8664-subprim-call-vinsn (makeu64) .SPmakeu64)

(define-x8664-subprim-call-vinsn (makes64) .SPmakes64)

(define-x8664-subprim-lea-jmp-vinsn (stack-cons-list*)  .SPstkconslist-star)

(define-x8664-subprim-lea-jmp-vinsn (list*) .SPconslist-star)

(define-x8664-vinsn make-tsp-vcell (((dest :lisp))
				    ((closed :lisp))
				    ((temp :imm)
				     (stack-temp :imm)))
  (subq (:$b (+ x8664::value-cell.size x8664::dnode-size)) (:rcontext x8664::tcr.next-tsp))
  (movq (:rcontext x8664::tcr.save-tsp) (:%q stack-temp))
  (movq (:rcontext x8664::tcr.next-tsp) (:%q temp))
  (movapd (:%xmm x8664::fpzero) (:@ (:%q temp)))
  (movapd (:%xmm x8664::fpzero) (:@ x8664::dnode-size (:%q temp)))
  (movq (:%q stack-temp) (:@ (:%q temp)))
  (movq (:% x8664::rbp) (:@ x8664::tsp-frame.rbp (:%q temp)))  
  (movq (:%q temp) (:rcontext x8664::tcr.save-tsp))  
  (movq (:$l x8664::value-cell-header) (:@ x8664::dnode-size (:%q temp)))
  (movq (:%q closed) (:@ (+ x8664::dnode-size x8664::node-size) (:%q temp)))
  (leaq (:@ (+ x8664::dnode-size x8664::fulltag-misc) (:%q temp)) (:%q dest)))

(define-x8664-subprim-lea-jmp-vinsn (bind-nil)  .SPbind-nil)

(define-x8664-subprim-lea-jmp-vinsn (bind-self)  .SPbind-self)

(define-x8664-subprim-lea-jmp-vinsn (bind-self-boundp-check)  .SPbind-self-boundp-check)

(define-x8664-subprim-lea-jmp-vinsn (bind)  .SPbind)


(define-x8664-vinsn (bind-inline ) (()
                                 ((sym :lisp)
                                  (val :lisp))
                                 ((idx :lisp)
                                  (tlb-pointer :lisp)))
  (movq (:@ x8664::symbol.binding-index (:%q sym)) (:%q idx))
  (cmpq (:rcontext x8664::tcr.tlb-limit) (:%q idx))
  (jb :tlb-ok)
  (pushq (:%q idx))
  (ud2a)
  (:byte 1)
  :tlb-ok
  (movq (:rcontext target::tcr.tlb-pointer) (:%q tlb-pointer))
  (pushq (:@ (:%q tlb-pointer) (:%q idx)))
  (pushq (:%q idx))
  (pushq (:rcontext target::tcr.db-link))
  (movq (:%q target::rsp)(:rcontext target::tcr.db-link))
  (movq (:%q val) (:@ (:%q tlb-pointer) (:%q idx))))


(define-x8664-vinsn unbind-inline (()
                                   ()
                                   ((link :lisp)
                                    (tlb-pointer :lisp)
                                    (idx :lisp)
                                    (val :lisp)))
  
  (movq (:rcontext target::tcr.db-link) (:%q link))
  (movq (:rcontext target::tcr.tlb-pointer) (:%q tlb-pointer))
  (movq (:@  8 (:%q link)) (:%q idx))
  (movq (:@ 16 (:%q link)) (:%q val))
  (movq (:@ (:%q link)) (:%q link))
  (movq (:% val) (:@ (:%q tlb-pointer) (:% idx)))
  (movq (:%q link) (:rcontext target::tcr.db-link)))
        
  

(define-x8664-subprim-jump-vinsn (tail-call-sym-gen :needs-frame-pointer) .SPtcallsymgen)

(define-x8664-subprim-call-vinsn (make-stack-list)  .Spmakestacklist)

(define-x8664-vinsn node-slot-ref  (((dest :lisp))
				    ((node :lisp)
				     (cellno :u32const)))
  (movq (:@ (:apply + x8664::misc-data-offset (:apply ash cellno 3))
            (:%q node)) (:%q dest)))

(define-x8664-subprim-lea-jmp-vinsn (stack-cons-list)  .SPstkconslist)


(define-x8664-vinsn  %slot-ref (((dest :lisp))
				((instance (:lisp (:ne dest)))
				 (index :lisp)))
  (movq (:@ x8664::misc-data-offset (:%q instance) (:%q index)) (:%q dest))
  (cmpl (:$b x8664::slot-unbound-marker) (:%l dest))
  (je :bad)
  :ok
  (:anchored-uuo-section :ok)
  :bad
  (:anchored-uuo (uuo-error-slot-unbound (:%q dest) (:%q instance) (:%q index))))

(define-x8664-vinsn eep.address (((dest t))
				 ((src (:lisp (:ne dest )))))
  (movq (:@ (+ (ash 1 x8664::word-shift) x8664::misc-data-offset) (:%q src))
        (:%q dest))
  (cmpb (:$b x8664::fulltag-nil) (:%b dest))
  (je :bad)
  :ok
  (:anchored-uuo-section :ok)
  :bad
  (:anchored-uuo (uuo-error-eep-unresolved (:%q src) (:%q dest))))


(define-x8664-subprim-lea-jmp-vinsn (heap-cons-rest-arg) .SPheap-cons-rest-arg)

(define-x8664-subprim-lea-jmp-vinsn (stack-cons-rest-arg) .SPstack-cons-rest-arg)

(define-x8664-subprim-lea-jmp-vinsn (make-stack-vector)  .SPmkstackv)

(define-x8664-vinsn (%current-frame-ptr :needs-frame-pointer) (((dest :imm))
					())
  (movq (:%q x8664::rbp) (:%q dest)))

(define-x8664-vinsn %foreign-stack-pointer (((dest :imm))
                                            ())
  (movq (:rcontext x8664::tcr.foreign-sp) (:%q dest)))


(define-x8664-vinsn %set-scharcode8 (()
				    ((str :lisp)
				     (idx :imm)
				     (code :imm))
				    ((imm :u64)
				     (imm1 :u64)))
  (movq (:%q code) (:%q imm1))
  (movq (:%q idx) (:%q imm))
  (shrq (:$ub x8664::fixnumshift) (:%q imm1))
  (shrq (:$ub x8664::word-shift) (:%q imm))
  (movb (:%b imm1) (:@ x8664::misc-data-offset (:%q str) (:%q imm))))


(define-x8664-vinsn %set-scharcode32 (()
				    ((str :lisp)
				     (idx :imm)
				     (code :imm))
				    ((imm :u64)
				     (imm1 :u64)))
  (movq (:%q code) (:%q imm1))
  (movq (:%q idx) (:%q imm))
  (shrq (:$ub x8664::fixnumshift) (:%q imm1))
  (shrq (:$ub 1) (:%q imm))
  (movl (:%l imm1) (:@ x8664::misc-data-offset (:%q str) (:%q imm))))




(define-x8664-vinsn pop-argument-registers (()
                                            ())
  (testl (:%l x8664::nargs) (:%l x8664::nargs))
  (je :done)
  (rcmpl (:%l x8664::nargs) (:$b (ash 2 x8664::word-shift)))
  (popq (:%q x8664::arg_z))
  (jb :done)
  (popq (:%q x8664::arg_y))
  (je :done)
  (popq (:%q x8664::arg_x))
  :done)

(define-x8664-vinsn %symptr->symvector (((target :lisp))
                                        ((target :lisp)))
  (subb (:$b (- x8664::fulltag-symbol x8664::fulltag-misc)) (:%b target)))

(define-x8664-vinsn %symvector->symptr (((target :lisp))
                                        ((target :lisp)))
  (addb (:$b (- x8664::fulltag-symbol x8664::fulltag-misc)) (:%b target)))


(define-x8664-subprim-lea-jmp-vinsn (spread-lexpr)  .SPspread-lexpr-z)

(define-x8664-vinsn mem-ref-double-float (((dest :double-float))
                                           ((src :address)
                                            (index :s64)))
  (movsd (:@ (:%q src) (:%q index)) (:%xmm dest)))

(define-x8664-vinsn mem-ref-single-float (((dest :single-float))
                                           ((src :address)
                                            (index :s64)))
  (movss (:@ (:%q src) (:%q index)) (:%xmm dest)))

(define-x8664-vinsn zero-extend-nargs (()
                                       ())
  (movzwl (:%w x8664::nargs) (:%l x8664::nargs)))

(define-x8664-vinsn load-adl (()
			      ((n :u32const)))
  (movl (:$l n) (:%l x8664::nargs)))

(define-x8664-subprim-lea-jmp-vinsn (macro-bind) .SPmacro-bind)

(define-x8664-subprim-lea-jmp-vinsn (destructuring-bind-inner) .SPdestructuring-bind-inner)

(define-x8664-subprim-lea-jmp-vinsn  (destructuring-bind) .SPdestructuring-bind)

(define-x8664-vinsn symbol-ref (((dest :lisp))
                                ((src :lisp)
                                 (cellno :u32const)))
  (movq (:@ (:apply + (- x8664::node-size x8664::fulltag-symbol)
                    (:apply ash cellno 3))
              (:%q src)) (:%q dest)))

(define-x8664-vinsn mem-ref-c-bit-fixnum (((dest :lisp))
                                          ((src :address)
                                           (offset :s32const))
                                          ((temp :u32)))
  ((:pred = 0 (:apply ash offset -6))
   (btq (:$ub (:apply logand 63 offset))
        (:@  (:%q src))))
  ((:not (:pred = 0 (:apply ash offset -6)))
   (btq (:$ub (:apply logand 63 offset))
        (:@ (:apply ash (:apply ash offset -6) 3) (:%q src))))
  (movl (:$l x8664::fixnumone) (:%l temp))
  (leaq (:@ (- x8664::fixnumone) (:%q temp)) (:%q dest))
  (cmovbl (:%l temp) (:%l dest)))

(define-x8664-vinsn mem-ref-c-bit (((dest :lisp))
                                   ((src :address)
                                    (offset :s32const))
                                   ((temp :u32)))
  ((:pred = 0 (:apply ash offset -6))
   (btq (:$ub (:apply logand 63 offset))
        (:@  (:%q src))))
  ((:not (:pred = 0 (:apply ash offset -6)))
   (btq (:$ub (:apply logand 63 offset))
        (:@ (:apply ash (:apply ash offset -6) 3) (:%q src))))
  (setb (:%b temp))
  (movzbl (:%b temp) (:%l dest)))

(define-x8664-vinsn mem-ref-bit-fixnum (((dest :lisp)
                                         (src :address))
                                        ((src :address)
                                         (offset :lisp))
                                        ((temp :u32)))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub (+ 6 x8664::fixnumshift)) (:%q temp))
  (leaq (:@ (:%q src) (:%q temp) 8) (:%q src))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub x8664::fixnumshift) (:%q temp))
  (andl (:$l 63) (:%l temp))
  (btq (:%q temp) (:@ (:%q src)))
  (movl (:$l x8664::fixnumone) (:%l temp))
  (leaq (:@ (- x8664::fixnumone) (:%q temp)) (:%q dest))
  (cmovbl (:%l temp) (:%l dest)))

(define-x8664-vinsn mem-ref-bit (((dest :lisp)
                                  (src :address))
                                 ((src :address)
                                  (offset :lisp))
                                 ((temp :u32)))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub (+ 6 x8664::fixnumshift)) (:%q temp))
  (leaq (:@ (:%q src) (:%q temp) 8) (:%q src))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub x8664::fixnumshift) (:%q temp))
  (andl (:$l 63) (:%l temp))
  (btq (:%q temp) (:@ (:%q src)))
  (setb (:%b temp))
  (movzbl (:%b temp) (:%l dest)))

  
(define-x8664-vinsn mem-set-c-bit-0 (()
				     ((src :address)
                                      (offset :s32const)))
  
  ((:pred = 0 (:apply ash offset -6))
   (btrq (:$ub (:apply logand 63 offset))
        (:@  (:%q src))))
  ((:not (:pred = 0 (:apply ash offset -6)))
   (btrq (:$ub (:apply logand 63 offset))
         (:@ (:apply ash (:apply ash offset -6) 3) (:%q src)))))

(define-x8664-vinsn mem-set-c-bit-1 (()
				     ((src :address)
                                      (offset :s32const)))
  
  ((:pred = 0 (:apply ash offset -6))
   (btsq (:$ub (:apply logand 63 offset))
         (:@  (:%q src))))
  ((:not (:pred = 0 (:apply ash offset -6)))
   (btsq (:$ub (:apply logand 63 offset))
         (:@ (:apply ash (:apply ash offset -6) 3) (:%q src)))))

(define-x8664-vinsn mem-set-c-bit-variable-value (()
                                                  ((src :address)
                                                   (offset :s32const)
                                                   (value :lisp)))
  (testq (:%q value) (:%q value))
  (jne :set)
  ((:pred = 0 (:apply ash offset -6))
   (btrq (:$ub (:apply logand 63 offset))
        (:@  (:%q src))))
  ((:not (:pred = 0 (:apply ash offset -6)))
   (btrq (:$ub (:apply logand 63 offset))
         (:@ (:apply ash (:apply ash offset -6) 3) (:%q src))))
  (jmp :done)
  :set
  ((:pred = 0 (:apply ash offset -6))
   (btsq (:$ub (:apply logand 63 offset))
         (:@  (:%q src))))
  ((:not (:pred = 0 (:apply ash offset -6)))
   (btsq (:$ub (:apply logand 63 offset))
         (:@ (:apply ash (:apply ash offset -6) 3) (:%q src))))
  :done)


(define-x8664-vinsn mem-set-bit-0 (((src :address))
                                   ((src :address)
                                    (offset :lisp))
                                   ((temp :u32)))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub (+ 6 x8664::fixnumshift)) (:%q temp))
  (leaq (:@ (:%q src) (:%q temp) 8) (:%q src))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub x8664::fixnumshift) (:%q temp))
  (andl (:$l 63) (:%l temp))
  (btrq (:%q temp) (:@ (:%q src))))

(define-x8664-vinsn mem-set-bit-1 (((src :address))
                                   ((src :address)
                                    (offset :lisp))
                                   ((temp :u32)))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub (+ 6 x8664::fixnumshift)) (:%q temp))
  (leaq (:@ (:%q src) (:%q temp) 8) (:%q src))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub x8664::fixnumshift) (:%q temp))
  (andl (:$l 63) (:%l temp))
  (btsq (:%q temp) (:@ (:%q src))))


(define-x8664-vinsn mem-set-bit-variable-value (((src :address))
                                                ((src :address)
                                                 (offset :lisp)
                                                 (value :lisp))
                                                ((temp :u32)))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub (+ 6 x8664::fixnumshift)) (:%q temp))
  (leaq (:@ (:%q src) (:%q temp) 8) (:%q src))
  (movq (:%q offset) (:%q temp))
  (shrq (:$ub x8664::fixnumshift) (:%q temp))
  (andl (:$l 63) (:%l temp))
  (testq (:%q value) (:%q value))
  (jne :set)
  (btrq (:%q temp) (:@ (:%q src)))
  (jmp :done)
  :set
  (btsq (:%q temp) (:@ (:%q src)))
  :done)

(define-x8664-vinsn %natural+  (((result :u64))
                               ((result :u64)
                                (other :u64)))
  (addq (:%q other) (:%q result)))

(define-x8664-vinsn %natural+-c (((result :u64))
                                ((result :u64)
                                 (constant :s32const)))
  (addq (:$l constant) (:%q result)))

(define-x8664-vinsn %natural-  (((result :u64))
                                ((result :u64)
                                 (other :u64)))
  (subq (:%q other) (:%q result)))

(define-x8664-vinsn %natural--c (((result :u64))
                                ((result :u64)
                                 (constant :s32const)))
  (subq (:$l constant) (:%q result)))

(define-x8664-vinsn %natural-logior (((result :u64))
                                    ((result :u64)
                                     (other :u64)))
  (orq (:%q other) (:%q result)))

(define-x8664-vinsn %natural-logior-c (((result :u64))
                                      ((result :u64)
                                       (constant :s32const)))
  (orq (:$l constant) (:%q result)))

(define-x8664-vinsn %natural-logand (((result :u64))
                                    ((result :u64)
                                     (other :u64)))
  (andq (:%q other) (:%q result)))

(define-x8664-vinsn %natural-logand-c (((result :u64))
                                      ((result :u64)
                                       (constant :s32const)))
  (andq (:$l constant) (:%q result)))

(define-x8664-vinsn %natural-logxor (((result :u64))
                                    ((result :u64)
                                     (other :u64)))
  (xorq (:%q other) (:%q result)))

(define-x8664-vinsn %natural-logxor-c (((result :u64))
                                       ((result :u64)
                                        (constant :s32const)))
  (xorq (:$l constant) (:%q result)))

(define-x8664-vinsn natural-shift-left (((dest :u64))
                                        ((dest :u64)
                                         (amt :u8const)))
  (shlq (:$ub amt) (:%q dest)))

(define-x8664-vinsn natural-shift-right (((dest :u64))
                                         ((dest :u64)
                                          (amt :u8const)))
  (shrq (:$ub amt) (:%q dest)))

(define-x8664-vinsn set-z-if-uvector-type (((crf :crf))
                                           ((src :lisp)
                                            (type :u8const))
                                           ((tag :u8)))
  (movl (:%l src) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :done)
  (cmpb (:$b type) (:@ x8664::misc-subtag-offset (:%q src)))
  :done)

(define-x8664-vinsn set-z-if-header-type (((crf :crf))
                                          ((src :lisp)
                                           (type :u8const))
                                          ((flags :u32)))
  (movl (:@ x8664::vectorH.flags (:%q  src)) (:%l flags))
  (shrl (:$ub (+ x8664::fixnumshift 8)) (:%l flags))
  (cmpb (:$b type) (:%b flags))
)

(define-x8664-vinsn trap-unless-simple-array-2 (()
                                                ((object :lisp)
                                                 (expected-flags :u32const)
                                                 (type-error :u8const))
                                                ((tag :u8)))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :bad)
  (cmpb (:$b x8664::subtag-arrayH) (:@ x8664::misc-subtag-offset (:%q object)))
  (jne :bad)
  (cmpq (:$b (ash 2 x8664::fixnumshift)) (:@ x8664::arrayH.rank (:%q object)))
  (jne :bad)
  (cmpq (:$l (:apply ash expected-flags x8664::fixnumshift)) (:@ x8664::arrayH.flags (:%q object)))
  (jne :bad)

  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub type-error))))

(define-x8664-vinsn set-z-if-typed-array (((crf :crf))
                                            ((object :lisp)
                                             (expected-type :u8const)
                                             (rank :u8const))
                                            ((tag :u8)))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :notz)
  (cmpb (:$b x8664::subtag-arrayH) (:@ x8664::misc-subtag-offset (:%q object)))
  (jne :notz)
  (cmpq (:$b (:apply ash rank x8664::fixnumshift)) (:@ x8664::arrayH.rank (:%q object)))
  (jne :notz)
  (movl  (:@ x8664::arrayH.flags (:%q object)) (:%l tag))
  (shrl (:$ub (+ x8664::fixnumshift 8)) (:%l tag))
  (cmpb (:$b expected-type) (:%b tag))
  :notz
)



(define-x8664-vinsn trap-unless-simple-array-3 (()
                                                ((object :lisp)
                                                 (expected-flags :u32const)
                                                 (type-error :u8const))
                                                ((tag :u8)))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :bad)
  (cmpb (:$b x8664::subtag-arrayH) (:@ x8664::misc-subtag-offset (:%q object)))
  (jne :bad)
  (cmpq (:$b (ash 3 x8664::fixnumshift)) (:@ x8664::arrayH.rank (:%q object)))
  (jne :bad)
  (cmpq (:$l (:apply ash expected-flags x8664::fixnumshift)) (:@ x8664::arrayH.flags (:%q object)))
  (jne :bad)
  (:anchored-uuo-section :again)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q object) (:$ub type-error))))
  
(define-x8664-vinsn trap-unless-array-header (()
                                              ((object :lisp))
                                              ((tag :u8)))
  :again
  (movl (:%l object) (:%l tag))
  (andl (:$b x8664::tagmask) (:%l tag))
  (cmpl (:$b x8664::tag-misc) (:%l tag))
  (jne :trap)
  (cmpb (:$b x8664::subtag-arrayH) (:@ x8664::misc-subtag-offset (:%q object)))
  (jne :trap)

  (:anchored-uuo-section :again)
  :trap
  (:anchored-uuo (uuo-error-reg-not-tag (:%q object) (:$ub x8664::subtag-arrayH))))

(define-x8664-vinsn check-arrayH-rank (()
                                       ((header :lisp)
                                        (expected :u32const))
                                       ((rank :imm)))
  :anchor
  (movl (:$l (:apply ash expected x8664::fixnumshift)) (:%l rank))
  (cmpq (:@ x8664::arrayH.rank (:%q header)) (:%q rank))
  (jne :bad)
  (:anchored-uuo-section :anchor)
  :bad
  (:anchored-uuo (uuo-error-array-rank (:%q header) (:%q rank))))

(define-x8664-vinsn check-arrayH-flags (()
                                       ((header :lisp)
                                        (expected :u32const)
                                        (type-error :u8const)))
  :anchor
  (cmpq (:$l (:apply ash expected x8664::fixnumshift))
        (:@ x8664::arrayH.flags (:%q header)))
  (jne :bad)
  (:anchored-uuo-section :anchor)
  :bad
  (:anchored-uuo (uuo-error-reg-not-type (:%q header) (:$ub type-error))))

(define-x8664-vinsn misc-ref-c-u16  (((dest :u16))
				     ((v :lisp)
				      (idx :u32const))
				     ())
  (movzwl (:@ (:apply + x8664::misc-data-offset (:apply ash idx 1)) (:%q v)) (:%l dest)))

(define-x8664-vinsn misc-ref-c-s16  (((dest :s16))
				     ((v :lisp)
				      (idx :u32const))
				     ())
  (movswq (:@ (:apply + x8664::misc-data-offset (:apply ash idx 1)) (:%q v)) (:%q dest)))

(define-x8664-vinsn misc-set-single-float (()
					   ((val :single-float)
					    (v :lisp)
					    (scaled-idx :u32)))
  (movss (:%xmm val) (:@ x8664::misc-data-offset (:% v) (:% scaled-idx))))

(define-x8664-vinsn u16->u32 (((dest :u32))
			      ((src :u16)))
  (movzwl (:%w src) (:%l dest)))

(define-x8664-vinsn u8->u32 (((dest :u32))
			     ((src :u8)))
  (movzbl (:%b src) (:%l dest)))


(define-x8664-vinsn s16->s32 (((dest :s32))
			      ((src :s16)))
  (movswl (:%w src) (:%l dest)))

(define-x8664-vinsn s8->s32 (((dest :s32))
			     ((src :s8)))
  (movsbl (:%b src) (:%l dest)))

(define-x8664-subprim-jump-vinsn (tail-call-fn-gen) .SPtcallnfngen)

(define-x8664-subprim-jump-vinsn (tail-call-fn-vsp) .SPtcallnfnvsp)

(define-x8664-vinsn set-eq-bit (()
                                ())
  (testb (:%b x8664::arg_z) (:%b x8664::arg_z)))

(define-x8664-vinsn clr-eq-bit (()
                                ()
                                ((temp :u32)))
  (xorl (:%l temp) (:%l temp))
  (incl (:%l temp)))

(define-x8664-vinsn %schar8 (((char :imm))
			    ((str :lisp)
			     (idx :imm))
			    ((imm :u32)))
  (movq (:%q idx) (:%q imm))
  (shrq (:$ub x8664::fixnumshift) (:%q imm))
  (movzbl (:@ x8664::misc-data-offset (:%q str) (:%q imm)) (:%l imm))
  (shll (:$ub x8664::charcode-shift) (:%l imm))
  (leaq (:@ x8664::subtag-character (:%q imm)) (:%q char)))

(define-x8664-vinsn %schar32 (((char :imm))
                              ((str :lisp)
                               (idx :imm))
                              ((imm :u32)))
  (movq (:%q idx) (:%q imm))
  (shrq (:$ub 1) (:%q imm))
  (movl (:@ x8664::misc-data-offset (:%q str) (:%q imm)) (:%l imm))
  (shll (:$ub x8664::charcode-shift) (:%l imm))
  (leaq (:@ x8664::subtag-character (:%q imm)) (:%q char)))


(define-x8664-vinsn %set-schar8 (()
                                 ((str :lisp)
                                  (idx :imm)
                                  (char :imm))
                                 ((imm0 :u64)
                                  (imm1 :u64)))
  (movq (:%q idx) (:%q imm0))
  (movl (:%l char) (:%l imm1))
  (shrq (:$ub x8664::fixnumshift) (:%q imm0))
  (shrl (:$ub x8664::charcode-shift) (:%l imm1))
  (movb (:%b imm1) (:@ x8664::misc-data-offset (:%q str) (:%q imm0))))

(define-x8664-vinsn %set-schar32 (()
                                 ((str :lisp)
                                  (idx :imm)
                                  (char :imm))
                                 ((imm0 :u64)
                                  (imm1 :u64)))
  (movq (:%q idx) (:%q imm0))
  (movl (:%l char) (:%l imm1))
  (shrq (:$ub 1) (:%q imm0))
  (shrl (:$ub x8664::charcode-shift) (:%l imm1))
  (movl (:%l imm1) (:@ x8664::misc-data-offset (:%q str) (:%q imm0))))

(define-x8664-vinsn misc-set-c-single-float (((val :single-float))
					     ((v :lisp)
					      (idx :u32const)))
  (movss (:%xmm val) (:@ (:apply + x8664::misc-data-offset (:apply ash idx 2)) (:%q v))))

(define-x8664-vinsn array-data-vector-ref (((dest :lisp))
					   ((header :lisp)))
  (movq (:@ x8664::arrayH.data-vector (:%q header)) (:%q dest)))

(define-x8664-subprim-call-vinsn (subtag-misc-ref) .SPsubtag-misc-ref)

(define-x8664-subprim-call-vinsn (subtag-misc-set) .SPsubtag-misc-set)

(define-x8664-vinsn mem-ref-c-absolute-u8 (((dest :u8))
                                           ((addr :s32const)))
  (movzbl (:@ addr) (:%l dest)))

(define-x8664-vinsn mem-ref-c-absolute-s8 (((dest :s8))
                                           ((addr :s32const)))
  (movsbq (:@ addr) (:%q dest)))

(define-x8664-vinsn mem-ref-c-absolute-u16 (((dest :u16))
                                           ((addr :s32const)))
  (movzwl (:@ addr) (:%l dest)))

(define-x8664-vinsn mem-ref-c-absolute-s16 (((dest :s16))
                                           ((addr :s32const)))
  (movswq (:@ addr) (:%q dest)))

(define-x8664-vinsn mem-ref-c-absolute-fullword (((dest :u32))
                                                 ((addr :s32const)))
  (movl (:@ addr) (:%l dest)))

(define-x8664-vinsn mem-ref-c-absolute-signed-fullword (((dest :s32))
                                                        ((addr :s32const)))
  (movslq (:@ addr) (:%q dest)))

(define-x8664-vinsn mem-ref-c-absolute-doubleword (((dest :s64))
                                                   ((addr :s32const)))
  (movq (:@ addr) (:%q dest)))

(define-x8664-vinsn mem-ref-c-absolute-signed-doubleword (((dest :s64))
                                                          ((addr :s32const)))
  (movq (:@ addr) (:%q dest)))

(define-x8664-vinsn mem-ref-c-absolute-natural (((dest :u64))
                                                   ((addr :s32const)))
  (movq (:@ addr) (:%q dest)))

(define-x8664-vinsn event-poll (()
                                ())
  (btrq (:$ub 63) (:rcontext x8664::tcr.interrupt-pending))
  (jae :no-interrupt)
  (ud2a)
  (:byte 2)
  :no-interrupt)

;;; Return dim1 (unboxed)
(define-x8664-vinsn check-2d-bound (((dim :u64))
				    ((i :imm)
				     (j :imm)
				     (header :lisp)))
  :anchor
  (cmpq (:@ (+ x8664::misc-data-offset (* 8 x8664::arrayH.dim0-cell)) (:%q header)) (:%q i))
  (jae :bad-i)
  (movq (:@ (+ x8664::misc-data-offset (* 8 (1+ x8664::arrayH.dim0-cell))) (:%q header))
        (:%q dim))
  (cmpq (:%q dim) (:%q j))
  (jae :bad-j)
  (sarq (:$ub x8664::fixnumshift) (:%q dim))
  (:anchored-uuo-section :anchor)
  :bad-i
  (:anchored-uuo (uuo-error-array-bounds (:%q i) (:%q header)))
  (:anchored-uuo-section :anchor)
  :bad-j
  (:anchored-uuo (uuo-error-array-bounds (:%q j) (:%q header))))

;;; Return dim1, dim2 (unboxed)
(define-x8664-vinsn check-3d-bound (((dim1 :u64)
                                     (dim2 :u64))
				    ((i :imm)
				     (j :imm)
                                     (k :imm)
				     (header :lisp)))
  :anchor
  (cmpq (:@ (+ x8664::misc-data-offset (* 8 x8664::arrayH.dim0-cell)) (:%q header)) (:%q i))
  (jae :bad-i)
  (movq (:@ (+ x8664::misc-data-offset (* 8 (1+ x8664::arrayH.dim0-cell))) (:%q header)) (:%q dim1))
  (cmpq (:%q dim1) (:%q j))
  (jae :bad-j)
  (sarq (:$ub x8664::fixnumshift) (:%q dim1))
  (movq (:@ (+ x8664::misc-data-offset (* 8 (+ 2 x8664::arrayH.dim0-cell))) (:%q header)) (:%q dim2))
  (cmpq (:%q dim2) (:%q k))
  (jae :bad-k)
  (sarq (:$ub x8664::fixnumshift) (:%q dim2))
  (:anchored-uuo-section :anchor)
  :bad-i
  (:anchored-uuo (uuo-error-array-bounds (:%q i) (:%q header)))
  (:anchored-uuo-section :anchor)
  :bad-j
  (:anchored-uuo (uuo-error-array-bounds (:%q j) (:%q header)))
  (:anchored-uuo-section :anchor)
  :bad-k
  (:anchored-uuo (uuo-error-array-bounds (:%q k) (:%q header)))
  )


(define-x8664-vinsn 2d-dim1 (((dest :u64))
			     ((header :lisp)))
  (movq (:@ (+ x8664::misc-data-offset (* 8 (1+ x8664::arrayH.dim0-cell))) (:%q header))
        (:%q dest))
  (sarq (:$ub x8664::fixnumshift) (:%q dest)))


(define-x8664-vinsn 3d-dims (((dim1 :u64)
                              (dim2 :u64))
			     ((header :lisp)))
  (movq (:@ (+ x8664::misc-data-offset (* 8 (1+ x8664::arrayH.dim0-cell))) (:%q header)) (:%q dim1))
  (movq (:@ (+ x8664::misc-data-offset (* 8 (+ 2 x8664::arrayH.dim0-cell))) (:%q header)) (:%q dim2))
  (sarq (:$ub x8664::fixnumshift) (:%q dim1))
  (sarq (:$ub x8664::fixnumshift) (:%q dim2)))

(define-x8664-vinsn 2d-unscaled-index (((dest :imm)
                                        (dim1 :u64))
				       ((dim1 :u64)
                                        (i :imm)
					(j :imm)))

  (imulq (:%q i) (:%q dim1))
  (leaq (:@ (:%q j) (:%q dim1)) (:%q dest)))


;; dest <- (+ (* i dim1 dim2) (* j dim2) k)
(define-x8664-vinsn 3d-unscaled-index (((dest :imm)
                                        (dim1 :u64)
                                        (dim2 :u64))
				       ((dim1 :u64)
                                        (dim2 :u64)
                                        (i :imm)
					(j :imm)
                                        (k :imm)))
  (imulq (:%q dim2) (:%q dim1))
  (imulq (:%q j) (:%q dim2))
  (imulq (:%q i) (:%q dim1))
  (addq (:%q dim1) (:%q dim2))
  (leaq (:@ (:%q k) (:%q dim2)) (:%q dest)))

(define-x8664-vinsn set-z-flag-if-arg-fixnum (()
                                              ((arg :imm)))
  (testb (:$b x8664::fixnummask) (:%b arg)))

(define-x8664-vinsn set-z-flag-if-both-args-fixnums (()
                                                     ((a :imm)
                                                      (b :imm))
                                                     ((tag :u32)))
  (movl (:%l a) (:%l tag))
  (orl (:%l b) (:%l tag))
  (testb (:$b x8664::fixnummask) (:%b tag)))





(define-x8664-vinsn fixnum->single-float (((f :single-float))
                                          ((arg :lisp))
                                          ((unboxed :s64)))
  (movq (:%q arg) (:%q unboxed))
  (sarq (:$ub x8664::fixnumshift) (:%q unboxed))
  (cvtsi2ssq (:%q unboxed) (:%xmm f)))

(define-x8664-vinsn fixnum->double-float (((f :double-float))
                                          ((arg :lisp))
                                          ((unboxed :s64)))
  (movq (:%q arg) (:%q unboxed))
  (sarq (:$ub x8664::fixnumshift) (:%q unboxed))
  (cvtsi2sdq (:%q unboxed) (:%xmm f)))


(define-x8664-vinsn xchg-registers (()
                                    ((a t)
                                     (b t)))
  (xchgq (:%q a) (:%q b)))

(define-x8664-vinsn establish-fn (()
                                  ()
                                  ((entry (:label 1))))
  (leaq (:@ (:^ entry) (:% x8664::rip)) (:%q x8664::fn)))

(define-x8664-vinsn %ilognot (((dest :imm)
                               (src :imm))
                              ((src :imm)))
  (xorq (:$b (- x8664::fixnumone)) (:%q dest)))
                              

(define-x8664-vinsn align-loop-head (()
                                     ()
                                     ())
  (:align 4))

(define-x8664-vinsn double-float-negate (((reg :double-float))
                                         ((reg :double-float))
					 ((tmp :double-float)))
  (movsd (:@ (:^ :const) (:% x8664::fn)) (:%xmm tmp))
  (pxor (:%xmm tmp) (:%xmm reg))
  (:uuo-section)
  :const
  (:long 0)
  (:long #x-80000000))

(define-x8664-vinsn single-float-negate (((reg :single-float))
                                         ((reg :single-float))
					 ((tmp :single-float)))
  (movss (:@ (:^ :const) (:% x8664::fn)) (:%xmm tmp)) 
  (pxor (:%xmm tmp) (:%xmm reg))
  (:uuo-section)
  :const
  (:long #x80000000))

(define-x8664-vinsn fixnum-ref-c-double-float (((dest :double-float))
                                               ((base :imm)
                                                (idx :u32const)))
  (movsd (:@ (:apply ash idx 3) (:%q base)) (:%xmm dest)))

(define-x8664-vinsn fixnum-ref-double-float  (((dest :double-float))
                                               ((base :imm)
                                                (idx :imm)))
  (movsd (:@ (:%q base) (:%q idx)) (:%xmm dest)))

(define-x8664-vinsn fixnum-set-c-double-float (()
                                               ((base :imm)
                                                (idx :u32const)
                                                (val :double-float)))
  (movsd (:%xmm val) (:@ (:apply ash idx 3) (:%q base))))

(define-x8664-vinsn fixnum-set-double-float  (()
                                               ((base :imm)
                                                (idx :imm)
                                                (val :double-float)))
  (movsd (:%xmm val) (:@ (:%q base) (:%q idx))))


(define-x8664-vinsn (pop-outgoing-arg :needs-frame-pointer) (((n :u16const))
                                      ())
  (popq (:@ (:apply * n (- x8664::node-size)) (:%q x8664::rbp))))

(define-x8664-vinsn (slide-nth-arg :needs-frame-pointer) (()
                                   ((n :u16const)
                                    (nstackargs :u16const)
                                    (temp :lisp)))
  (movq (:@ (:apply * (:apply - nstackargs (:apply + 1 n)) x8664::node-size) (:%q x8664::rsp)) (:%q temp))
  (movq (:%q temp) (:@ (:apply * (:apply + n 1) (- x8664::node-size)) (:%q x8664::rbp))))
                                   

(define-x8664-vinsn (set-tail-vsp :needs-frame-pointer) (((nargs :u16const))
                                  ())
  ((:pred = 0 nargs)
   (movq (:%q x8664::rbp) (:%q x8664::rsp)))
  ((:not (:pred = 0 nargs))
   (leaq (:@ (:apply * nargs (- x8664::node-size)) (:%q x8664::rbp)) (:%q x8664::rsp))))


;;; If we've used one of the fixed-stack-args !slideN vinsns above
;;; and are calling some function (rather than jumping to an internal
;;; entry point), we need to push the caller's return address and unlink
;;; its frame pointer.
(define-x8664-vinsn (prepare-tail-call :needs-frame-pointer) (()
                                       ())
  (pushq (:@ x8664::node-size (:%q x8664::rbp)))
  (movq (:@ (:%q x8664::rbp)) (:%q x8664::rbp)))

(define-x8664-vinsn (reuse-frame :needs-frame-pointer) (()())
  (movq (:%q x8664::rbp) (:%q x8664::rsp)))

(define-x8664-vinsn set-carry-if-fixnum-in-range
    (((idx :u32))
     ((reg :imm)
      (minval :s32const)
      (maxval :u32const)))
  (movq (:%q reg) (:%q idx))
  (sarq (:$ub x8664::fixnumshift) (:%q idx))
  ((:not (:pred zerop minval))
   ((:and (:pred < minval 128) (:pred >= minval -128))
    (subq (:$b minval) (:%q idx)))
   ((:not (:and (:pred < minval 128) (:pred >= minval -128)))
    (subq (:$l minval) (:%q idx))))
  ((:pred < maxval 128)
   (cmpq (:$b maxval) (:%q idx)))
  ((:pred >= maxval 128)
   (cmpq (:$l maxval) (:%q idx))))

(define-x8664-vinsn (ijmp ) (((idx :u32))
                                    ((idx :u32)
                                     (count :u32const))
                                    ((rjmp :lisp)))
  (movl (:@ (:^ :jtab) (:%q x8664::fn) (:%q idx) 4) (:%l idx))
  (leaq (:@ (:%q x8664::fn) (:%q idx)) (:%q rjmp))
  (jmp (:%q rjmp))
  (:uuo-section)
  (:align 2)
  (:long count)
  :jtab)
  

(define-x8664-vinsn jtabentry (()
                               ((label :label)))
  (:uuo-section)
  (:long (:^ label)))


(define-x8664-vinsn ivector-typecode-p  (((dest :lisp))
                                         ((src :lisp))
                                         ((temp :u32)
                                          (mask :u32)))
  (movl (:%l src) (:%l temp))
  (shrl (:$ub x8664::fixnumshift) (:%l temp))
  (andl (:$b x8664::fulltagmask) (:%l temp))
  (movl (:$l (logior (ash 1 x8664::fulltag-immheader-0)
                     (ash 1 x8664::fulltag-immheader-1)
                     (ash 1 x8664::fulltag-immheader-2))) (:%l mask))
  (btl (:%l temp) (:%l mask))
  (movl (:$l 0)  (:%l mask))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movl (:%l src) (:%l dest)))
  (cmovael (:%l mask) (:%l dest)))

(define-x8664-vinsn gvector-typecode-p  (((dest :lisp))
                                         ((src :lisp))
                                         ((temp :u32)
                                          (mask :u32)))
  (movl (:%l src) (:%l temp))
  (shrl (:$ub x8664::fixnumshift) (:%l temp))
  (andl (:$b x8664::fulltagmask) (:%l temp))
  (movl (:$l (logior (ash 1 x8664::fulltag-nodeheader-0)
                    (ash 1 x8664::fulltag-nodeheader-1))) (:%l mask))
  (btl (:%l temp) (:%l mask))
  (movl (:$l 0)  (:%l mask))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movl (:%l src) (:%l dest)))
  (cmovael (:%l mask) (:%l dest)))

(define-x8664-vinsn  %complex-single-float-realpart
    (((dest :single-float))
     ((src :complex-single-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movss (:%xmm src) (:%xmm dest))))

(define-x8664-vinsn  %complex-single-float-imagpart
    (((dest :single-float))
     ((src :complex-single-float)))
  ((:not (:pred = (:apply %hard-regspec-value src ) (:apply %hard-regspec-value dest)))
   (movapd (:%xmm src) (:%xmm dest)))
  (psrlq (:$ub 32) (:%xmm dest)))

(define-x8664-vinsn  %complex-double-float-realpart
    (((dest :double-float))
     ((src :complex-double-float)))
  ((:not (:pred =
                (:apply %hard-regspec-value dest)
                (:apply %hard-regspec-value src)))
   (movsd (:%xmm src) (:%xmm dest))))

(define-x8664-vinsn  %complex-double-float-imagpart
    (((dest :double-float))
     ((src :complex-double-float)))
  ((:not (:pred = (:apply %hard-regspec-value src ) (:apply %hard-regspec-value dest)))
   (movapd (:%xmm src) (:%xmm dest)))
  (shufpd (:$ub 1) (:%xmm target::fpzero) (:%xmm dest)))


(define-x8664-vinsn %make-complex-single-float
    (((dest :complex-single-float))
     ((r :single-float)
      (i :single-float)))
  ((:not (:pred = (:apply %hard-regspec-value r) (:apply %hard-regspec-value dest)))
   (movss (:%xmm r) (:%xmm dest)))
  (unpcklps (:%xmm i) (:%xmm dest)))

(define-x8664-vinsn %make-complex-double-float
    (((dest :complex-double-float))
     ((r :double-float)
     (i :double-float)))
  ((:not (:pred = (:apply %hard-regspec-value r) (:apply %hard-regspec-value dest)))
   (movapd (:%xmm r) (:%xmm dest)))
  (shufpd (:$ub 0) (:%xmm i) (:%xmm dest)))
  



(queue-fixup
 (fixup-x86-vinsn-templates
  *x8664-vinsn-templates*
  x86::*x86-opcode-template-lists* *x8664-backend*))


(provide "X8664-VINSNS")

