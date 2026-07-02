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
  (ldur dest (:@ v (:$ (:apply arm64::misc-data-offset idx)))))

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

;;; Return from function: restore context and return.
(define-arm64-vinsn (popj :lispcontext :pop :lrRestore :jumpLR)
    (()
     ())
  (ldp fn lr (:@ sp (:$ 16)))
  (ldr vsp (:@ sp (:$ 8)))              ;ignore marker
  (add sp sp (:$ 32))
  (ret))
