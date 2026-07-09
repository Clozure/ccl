(in-package "CCL")

;;; Check for and report invalid immediate operand syntax
(defun arm64-bitfield-imm (operand)
  (if (and (consp operand) (eq (car operand) :$) (= (length operand) 2))
    (cadr operand)
    (error "bitfield lsb/width must be written (:$ n), not ~s" operand)))

;;; Implement these bitfield extract aliases as lapmacros because in
;;; these cases the underlying imms operand is a function of *both*
;;; immediate operands of the alias.  The assembler expects to encode
;;; a field in the instruction from a single operand.

(defarm64lapmacro sbfx (rd rn lsb width)
  (let ((l (arm64-bitfield-imm lsb)) (w (arm64-bitfield-imm width)))
    `(sbfm ,rd ,rn (:$ ,l) (:$ (+ ,l ,w -1)))))

(defarm64lapmacro ubfx (rd rn lsb width)
  (let ((l (arm64-bitfield-imm lsb)) (w (arm64-bitfield-imm width)))
    `(ubfm ,rd ,rn (:$ ,l) (:$ (+ ,l ,w -1)))))

(defarm64lapmacro bfxil (rd rn lsb width)
  (let ((l (arm64-bitfield-imm lsb)) (w (arm64-bitfield-imm width)))
    `(bfm ,rd ,rn (:$ ,l) (:$ (+ ,l ,w -1)))))

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

(defarm64lapmacro box-fixnum (dest src)
  `(lsl ,dest ,src (:$ arm64::fixnumshift)))

(defarm64lapmacro unbox-fixnum (dest src)
  `(asr ,dest ,src (:$ arm64::fixnumshift)))

(defarm64lapmacro call-subprim (spname)
  (let ((offset (arm64::subprimitive-offset spname)))
    (if offset
      `(progn
         (add imm0 rnil (:$ ,offset))
         (blr imm0))
      (error "unknown subprimitive name ~s" spname))))

(defarm64lapmacro check-nargs (min &optional (max min))
  (let ((ok1 (gensym "@"))
        (ok2 (gensym "@")))
    (if (= max min)
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
