(in-package "CCL")

(defarm64lapmacro load-constant (dest constant)
  (let ((offset (arm64::constant-offset constant)))
    (if (typep offset '(signed-byte 9))
      `(ldur ,dest (:@ fn (:$ ,offset)))
      (error "constant ~s is too far away: use load-indexed-constant"
             constant))))

(defarm64lapmacro load-indexed-constant (dest constant idxreg)
  `(progn
     (movz ,idxreg (:$ ,(arm64::constant-offset constant)))
     (ldr ,dest (:@ fn ,idxreg))))

;;; This needs pc_luser_xp support so that building the frame looks
;;; atomic to the gc
(defarm64lapmacro build-lisp-frame (&optional (marker-reg 'imm0))
  `(progn
     (mov ,marker-reg (:$ arm64::lisp-frame-marker))
     (stp ,marker-reg vsp (:@! sp (:$ -32)))
     (stp fn lr (:@ sp (:$ 16)))))

(defarm64lapmacro restore-lisp-frame ()
  `(progn
     (ldp fn lr (:@ sp (:$ 16)))
     (ldr vsp (:@ sp (:$ 8)))            ;ignore marker
     (add sp sp (:$ 32))))

(defarm64lapmacro unbox-fixnum (dest src)
  `(asr ,dest ,src (:$ arm64::fixnumshift)))

(defarm64lapmacro box-fixnum (dest src)
  `(lsl ,dest ,src (:$ arm64::fixnumshift)))

(defarm64lapmacro get-double-float (dest node)
  `(ldur ,dest (:@ ,node (:$ arm64::double-float.value))))

;; a single-float is stored in top 32 bits
(defarm64lapmacro get-single-float-bits (dest node)
  `(lsr ,dest ,node (:$ 32)))

(defarm64lapmacro call-subprim (spname)
  (let ((offset (arm64::subprimitive-offset spname)))
    (if offset
      `(progn
         (add imm0 rnil (:$ ,offset))
         (blr imm0))
      (error "unknown subprimitive name ~s" spname))))

(defarm64lapmacro set-nargs (n)
  (check-type n (unsigned-byte 13))
  `(movz nargs (:$ ',n)))

(defarm64lapmacro check-nargs (min &optional (max min))
  (let ((ok1 (gensym "@"))
        (ok2 (gensym "@")))
    (if (eq max min)
      `(progn
         (cmp nargs (:$ (ash ,min arm64::fixnumshift)))
         (b.eq ,ok1)
         (uuo-error-wrong-nargs)
         ,ok1)
      (if (null max)
        (unless (= min 0)
          `(progn
             (cmp nargs (:$ (ash ,min arm64::fixnumshift)))
             (b.hs ,ok1)
             (uuo-error-wrong-nargs)
             ,ok1))
        (if (= min 0)
          `(progn
             (cmp nargs (:$ (ash ,max arm64::fixnumshift)))
             (b.ls ,ok1)
             (uuo-error-wrong-nargs)
             ,ok1)
          `(progn
             (cmp nargs (:$ (ash ,min arm64::fixnumshift)))
             (b.hs ,ok1)
             (uuo-error-wrong-nargs)
             ,ok1
             (cmp nargs (:$ (ash ,max arm64::fixnumshift)))
             (b.ls ,ok2)
             (uuo-error-wrong-nargs)
             ,ok2))))))

(provide "ARM64-LAPMACROS")
