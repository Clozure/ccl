(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "VINSN")
  (require "ARM64-BACKEND"))

(defmacro define-arm64-vinsn (vinsn-name (results args &optional temps)
                              &body body)
  (%define-vinsn *arm64-backend* vinsn-name results args temps body))

(define-arm64-vinsn misc-ref-c-node (((dest :lisp))
                                     ((v :lisp)
                                      (idx :u32))
                                     ())
  ;; this range is limited
  (ldur dest (:@ v (:$ (:apply + arm64::misc-data-offset idx)))))

(define-arm64-vinsn check-exact-nargs (()
                                       ((n :u16const)))
  (cmp nargs (:$ (:apply ash n 3)))
  (b.eq :ok)
  (uuo-error-wrong-nargs)
  :ok)
                                                  
(define-arm64-vinsn save-lisp-context-no-stack-args (()
                                                     ()
                                                     ((marker-reg :imm)))
  (mov marker-reg (:$ arm64::lisp-frame-marker))
  (stp marker-reg vsp (:@! sp (:$ 32)))
  (stp fn lr (:@ sp (:$ 16))))
  
(define-arm64-vinsn (vpush-register :push :node :vsp) (()
                                                       ((reg :lisp)))
  (str reg (:@! vsp (:$ (- arm64::node-size)))))

#|
;;; Save NFP: allocate an NFP frame on the C stack for unboxed temporaries.
;;; Frame layout: [saved-old-tcr.nfp at SP+0] [data at SP+8 ... SP+8+max_depth]
;;; Sets tcr.nfp = new SP so nested NFP vinsns can find the frame.
(define-arm64-vinsn save-nfp (()
                              ()       
                              ((temp :imm)))
  ((:pred > (:apply arm642-max-nfp-depth) 0)
   (ldr temp (:@ rcontext (:$ arm64::tcr.nfp)))
   (sub sp sp (:$ (:apply arm642-nfp-frame-size)))
   (str temp (:@ sp (:$ 0)))
   (add temp sp (:$ 0))
   (str temp (:@ rcontext (:$ arm64::tcr.nfp)))))

;;; Restore NFP: pop the NFP frame, restoring old tcr.nfp.
(define-arm64-vinsn restore-nfp (()
                                 ()
                                 ((temp :imm)))
  ((:pred > (:apply arm642-max-nfp-depth) 0)
   (ldr temp (:@ sp (:$ 0)))
   (str temp (:@ rcontext (:$ arm64::tcr.nfp)))
   (add sp sp (:$ (:apply arm642-nfp-frame-size)))))
|#

;;; Return from function: restore context and return.
(define-arm64-vinsn (popj :lispcontext :pop :lrRestore :jumpLR)
    (()
     ())
  (ldp fn lr (:@ sp (:$ 16)))
  (ldr vsp (:@ sp (:$ 8)))              ;ignore marker
  (add sp sp (:$ 32))
  (ret))

(define-arm64-vinsn unbox-s64 (((dest :s64))
                               ((src :lisp)))
  (asr dest src (:$ arm64::fixnumshift))
  ;; is it a fixnum?
  (ands xzr src (:$ arm64::fixnummask))
  (b.eq :good)
  ;; maybe a 2-digit bignum?
  (and dest src (:$ arm64::fulltagmask))
  (cmp dest (:$ arm64::fulltag-misc))
  (b.eq :miscobj)
  :bad
  (uuo-error-reg-not-type src (:$ub arch::error-object-not-signed-byte-64))
  :miscobj
  (ldr dest (:@ src (:$ arm64::misc-header-offset)))
  (cmp dest (:$ arm64::two-digit-bignum-header))
  (b.ne :bad)
  (ldr dest (:@ src (:$ arm64::misc-data-offset)))
  :good)

;;; Materialize a Lisp boolean branchlessly: DEST = CC ? t : nil, where CC
;;; is a 4-bit condition code and the NZCV flags were set by a preceding
;;; compare.  t lives at rnil + t-offset; put it in TRUE, then conditionally
;;; select it or nil (in rnil) into DEST.
(define-arm64-vinsn cond->boolean (((dest :lisp))
                                   ((cc :u8const))
                                   ((true :imm)))
  (add true rnil (:$ arm64::t-offset))
  (csel dest true rnil (:? cc)))

(define-arm64-vinsn load-nil (((dest :lisp))
                              ())
  (mov dest rnil))

(define-arm64-vinsn vframe-load (((dest :lisp))
                                  ((frame-offset :u16const)
                                   (cur-vsp :u16const)))
  (ldr dest (:@ arm64::vsp (:$ (:apply - (:apply - cur-vsp
                                                 arm64::word-size-in-bytes)
                                       frame-offset)))))

;;; Reconcile the template ordinals baked into the vinsns just defined
;;; with the assembler's current template table, in case this file was
;;; compiled against a differently-ordered table.
(fixup-arm64-vinsn-templates)
