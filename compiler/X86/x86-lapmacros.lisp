;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2005-2009 Clozure Associates and contributors.
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

;;; Comparisons make more sense if arg order is "dest, src", instead
;;; of the gas/ATT arg order.

(defx86lapmacro rcmp (src dest)
  `(cmp ,dest ,src))

(defx86lapmacro clrl (reg)
  `(xorl (% ,reg) (% ,reg)))

(defx86lapmacro clrq (reg)
  `(xorq (% ,reg) (% ,reg)))

(defx86lapmacro set-nargs (n)
  (cond ((= n 0) `(xorl (% nargs) (% nargs)))
        (t `(movl ($ ',n) (% nargs)))))

(defx86lapmacro anchored-uuo (form)
  `(progn
    ,form
    (:byte 0)))

(defx86lapmacro check-nargs (min &optional (max min))
  (let* ((anchor (gensym))
         (bad (gensym)))
    (if (and max (= max min))
      `(progn
        ,anchor
        ,(if (eql min 0)
             `(testl (% nargs) (% nargs))
             `(rcmp (% nargs) ($ ',min)))
        (jne ,bad)
        (:anchored-uuo-section ,anchor)
        ,bad
        (anchored-uuo (uuo-error-wrong-number-of-args))
        (:main-section nil))
      (if (null max)
        (unless (zerop min)
          `(progn
            ,anchor
            (rcmp (% nargs) ($ ',min))
            (jb ,bad)
            (:anchored-uuo-section ,anchor)
            ,bad
            (anchored-uuo (uuo-error-too-few-args))
            (:main-section nil)))
        (if (zerop min)
          `(progn
            ,anchor
            (rcmp (% nargs) ($ ',max))
            (ja ,bad)
            (:anchored-uuo-section ,anchor)
            ,bad
            (anchored-uuo (uuo-error-too-many-args))
            (:main-section nil))
          (let* ((toofew (gensym))
                 (toomany (gensym)))
            `(progn
              ,anchor
              (rcmp (% nargs) ($ ',min))
              (jb ,toofew)
              (rcmp (% nargs) ($ ',max))
              (ja ,toomany)
              (:anchored-uuo-section ,anchor)
              ,toofew
              (anchored-uuo (uuo-error-too-few-args))
              (:anchored-uuo-section ,anchor)
              ,toomany
              (anchored-uuo (uuo-error-too-many-args)))))))))


(defx86lapmacro extract-lisptag (node dest)
  (target-arch-case
   (:x8632
    `(progn
       (movl ($ x8632::tagmask) (% ,dest))
       (andl (%l ,node) (%l ,dest))))
   (:x8664
    `(progn
       (movb ($ x8664::tagmask) (%b ,dest))
       (andb (%b ,node) (%b ,dest))))))

(defx86lapmacro extract-fulltag (node dest)
  (target-arch-case
   (:x8632
    `(progn
       (movl ($ x8632::fulltagmask) (%l ,dest))
       (andl (%l ,node) (%l ,dest))))
   (:x8664
    `(progn
       (movb ($ x8664::fulltagmask) (%b ,dest))
       (andb (%b ,node) (%b ,dest))))))

(defx86lapmacro extract-subtag (node dest)
  (target-arch-case
   (:x8632
    `(movb (@ x8632::misc-subtag-offset (% ,node)) (%b ,dest)))
   (:x8664
    `(movb (@ x8664::misc-subtag-offset (% ,node)) (%b ,dest)))))

(defx86lapmacro extract-typecode (node dest)
  ;;; In general, these things are only defined to affect the low
  ;;; byte of the destination register.  This can also affect
  ;;; the #xff00 byte.
  (let* ((done (gensym)))
    (target-arch-case
     (:x8632
      `(progn
	 (extract-lisptag ,node ,dest)
	 (rcmp (%b ,dest) ($ x8632::tag-misc))
	 (jne ,done)
	 (movb (@  x8632::misc-subtag-offset (% ,node)) (%b ,dest))
	 ,done))
     (:x8664
      `(progn
	 (extract-lisptag ,node ,dest)
	 (rcmp (%b ,dest) ($ x8664::tag-misc))
	 (jne ,done)
	 (movb (@  x8664::misc-subtag-offset (% ,node)) (%b ,dest))
	 ,done)))))

(defx86lapmacro trap-unless-typecode= (node tag &optional (immreg 'imm0))
  (let* ((bad (gensym))
         (anchor (gensym)))
    `(progn
      ,anchor
      (extract-typecode ,node ,immreg)
      (cmpb ($ ,tag) (%b ,immreg))
      (jne ,bad)
      (:anchored-uuo-section ,anchor)
      ,bad
      (:anchored-uuo (uuo-error-reg-not-tag (% ,node) ($ ,tag)))
      (:main-section nil))))

(defx86lapmacro trap-unless-fulltag= (node tag &optional (immreg 'imm0))
  (let* ((ok (gensym)))
    `(progn
      (extract-fulltag ,node ,immreg)
      (cmpb ($ ,tag) (%b ,immreg))
      (je.pt ,ok)
      (uuo-error-reg-not-tag (% ,node) ($ ,tag))
      ,ok)))

(defx86lapmacro trap-unless-lisptag= (node tag &optional (immreg 'imm0))
  (let* ((ok (gensym)))
    `(progn
      (extract-lisptag ,node ,immreg)
      (cmpb ($ ,tag) (%b ,immreg))
      (je.pt ,ok)
      (uuo-error-reg-not-tag (% ,node) ($ ,tag))
      ,ok)))

(defx86lapmacro trap-unless-fixnum (node)
  (let* ((ok (gensym)))
    (target-arch-case
     (:x8632
      `(progn
	 (test ($ x8632::tagmask) (% ,node))
	 (je.pt ,ok)
	 (uuo-error-reg-not-fixnum (% ,node))
	 ,ok))
     (:x8664
      `(progn
	 (testb ($ x8664::tagmask) (%b ,node))
	 (je.pt ,ok)
	 (uuo-error-reg-not-fixnum (% ,node))
	 ,ok)))))

;;; On x8664, NIL has its own tag, so no other lisp object can
;;; have the same low byte as NIL.  On x8632, NIL is a just
;;; a distiguished CONS.
(defx86lapmacro cmp-reg-to-nil (reg)
  (target-arch-case
   (:x8632
    `(cmpl ($ (target-nil-value)) (%l ,reg)))
   (:x8664
    `(cmpb ($ (logand #xff (target-nil-value))) (%b ,reg)))))

(defx86lapmacro unbox-fixnum (src dest)
  (target-arch-case
   (:x8632
    `(progn
       (mov (% ,src) (% ,dest))
       (sar ($ x8632::fixnumshift) (% ,dest))))
   (:x8664
    `(progn
       (mov (% ,src) (% ,dest))
       (sar ($ x8664::fixnumshift) (% ,dest))))))

(defx86lapmacro box-fixnum (src dest)
  (target-arch-case
   (:x8632
    `(imull ($ x8632::fixnumone) (% ,src) (% ,dest)))
   (:x8664
    `(imulq ($ x8664::fixnumone) (% ,src) (% ,dest)))))

(defx86lapmacro get-single-float (node dest)
  (target-arch-case
   (:x8632
    `(movss (@ x8632::single-float.value (% ,node)) (% ,dest)))
   (:x8664
    `(progn
       (movd (% ,node) (% ,dest))
       (psrlq ($ 32) (% ,dest))))))

;;; Note that this modifies the src argument in the x8664 case.
(defx86lapmacro put-single-float (src node)
  (target-arch-case
   (:x8632
    `(movss (% ,src) (@ x8632::single-float.value (% ,node))))
   (:x8664
    `(progn
       (psllq ($ 32) (% ,src))
       (movd (% ,src) (% ,node))
       (movb ($ x8664::tag-single-float) (%b ,node))))))

(defx86lapmacro get-double-float (src fpreg)
  (target-arch-case
   (:x8632
    `(movsd (@ x8632::double-float.value (% ,src)) (% ,fpreg)))
   (:x8664
    `(movsd (@ x8664::double-float.value (% ,src)) (% ,fpreg)))))

(defx86lapmacro put-double-float (fpreg dest)
  (target-arch-case
   (:x8632
    `(movsd (% ,fpreg) (@ x8632::double-float.value (% ,dest))))
   (:x8664
    `(movsd (% ,fpreg) (@ x8664::double-float.value (% ,dest))))))
 
(defx86lapmacro getvheader (src dest)
  (target-arch-case
   (:x8632
    `(movl (@ x8632::misc-header-offset (% ,src)) (% ,dest)))
   (:x8664
    `(movq (@ x8664::misc-header-offset (% ,src)) (% ,dest)))))

;;; "Size" is unboxed element-count.  vheader and dest should
;;; both be immediate registers
(defx86lapmacro header-size (vheader dest)
  (target-arch-case
   (:x8632
    `(progn
       (mov (% ,vheader) (% ,dest))
       (shr ($ x8632::num-subtag-bits) (% ,dest))))
   (:x8664
    `(progn
       (mov (% ,vheader) (% ,dest))
       (shr ($ x8664::num-subtag-bits) (% ,dest))))))

;;; "Length" is fixnum element-count.
(defx86lapmacro header-length (vheader dest)
  (target-arch-case
   (:x8632
    `(progn
       (movl ($ (lognot 255)) (% ,dest))
       (andl (% ,vheader) (% ,dest))
       (shr ($ (- x8632::num-subtag-bits x8632::fixnumshift)) (% ,dest))))
   (:x8664
    `(progn
       (movq ($ (lognot 255)) (% ,dest))
       (andq (% ,vheader) (% ,dest))
       (shr ($ (- x8664::num-subtag-bits x8664::fixnumshift)) (% ,dest))))))

(defx86lapmacro header-subtag[fixnum] (vheader dest)
  `(progn
    (lea (@ (% ,vheader) 8) (% ,dest))
    (andl ($ '255) (%l ,dest))))

(defx86lapmacro vector-size (vector vheader dest)
  `(progn
    (getvheader ,vector ,vheader)
    (header-size ,vheader ,dest)))

(defx86lapmacro vector-length (vector dest)
  (target-arch-case
   (:x8632
    `(progn
       (movl ($ (lognot 255)) (% ,dest))
       (andl (@ x8632::misc-header-offset (% ,vector)) (% ,dest))
       (shr ($ (- x8632::num-subtag-bits x8632::fixnumshift)) (% ,dest))))
   (:x8664
    `(progn
       (movq ($ (lognot 255)) (% ,dest))
       (andq (@ x8664::misc-header-offset (% ,vector)) (% ,dest))
       (shr ($ (- x8664::num-subtag-bits x8664::fixnumshift)) (% ,dest))))))

(defx86lapmacro int-to-double (int temp double)
  (target-arch-case
   (:x8632
    `(progn
       (unbox-fixnum  ,int ,temp)
       (cvtsi2sdl (% ,temp) (% ,double))))
   (:x8664
    `(progn
       (unbox-fixnum  ,int ,temp)
       (cvtsi2sdq (% ,temp) (% ,double))))))

(defx86lapmacro int-to-single (int temp single)
  (target-arch-case
   (:x8632
    `(progn
       (unbox-fixnum ,int ,temp)
       (cvtsi2ssl (% ,temp) (% ,single))))
   (:x8664
    `(progn
       (unbox-fixnum ,int ,temp)
       (cvtsi2ssq (% ,temp) (% ,single))))))

(defx86lapmacro ref-global (global reg)
  (target-arch-case
   (:x8632
    `(movl (@ (+ (target-nil-value) ,(x8632::%kernel-global global))) (% ,reg)))
   (:x8664
    `(movq (@ (+ (target-nil-value) ,(x8664::%kernel-global global))) (% ,reg)))))

(defx86lapmacro ref-global.l (global reg)
  (target-arch-case
   (:x8632
    `(movl (@ (+ (target-nil-value) ,(x8632::%kernel-global global))) (%l ,reg)))
   (:x8664
    `(movl (@ (+ (target-nil-value) ,(x8664::%kernel-global global))) (%l ,reg)))))

(defx86lapmacro set-global (reg global)
  (target-arch-case
   (:x8632
    `(movl (% ,reg) (@ (+ (target-nil-value) ,(x8632::%kernel-global global)))))
   (:x8664
    `(movq (% ,reg) (@ (+ (target-nil-value) ,(x8664::%kernel-global global)))))))

(defx86lapmacro macptr-ptr (src dest)
  (target-arch-case
   (:x8632
    `(movl (@ x8632::macptr.address (% ,src)) (% ,dest)))
   (:x8664
    `(movq (@ x8664::macptr.address (% ,src)) (% ,dest)))))

;;; CODE is unboxed char-code (in low 8 bits); CHAR needs to be boxed.
(defx86lapmacro box-character (code char)
  (target-arch-case
   (:x8632
    `(progn
       (box-fixnum ,code ,char)
       (shl ($ (- x8632::charcode-shift x8632::fixnumshift)) (% ,char))
       (movb ($ x8632::subtag-character) (%b ,char))))
   (:x8664
    `(progn
       (box-fixnum ,code ,char)
       (shl ($ (- x8664::charcode-shift x8664::fixnumshift)) (% ,char))
       (movb ($ x8664::subtag-character) (%b ,char))))))
  
;;; index is a constant
(defx86lapmacro svref (vector index dest)
  (target-arch-case
   (:x8632
    `(movl (@ (+ x8632::misc-data-offset (* ,index 4)) (% ,vector)) (% ,dest)))
   (:x8664
    `(movq (@ (+ x8664::misc-data-offset (* ,index 8)) (% ,vector)) (% ,dest)))))

;;; Index is still a constant
(defx86lapmacro svset (vector index new)
  (target-arch-case
   (:x8632
    `(movl (% ,new) (@ (+ x8632::misc-data-offset (* ,index 4)) (% ,vector))))
   (:x8664
    `(movq (% ,new) (@ (+ x8664::misc-data-offset (* ,index 8)) (% ,vector))))))


;;; Frames, function entry and exit.


;;; Simple frame, since the caller didn't reserve space for it.
(defx86lapmacro save-simple-frame ()
  (target-arch-case
   (:x8632
    `(progn
       (pushl (% ebp))
       (movl (% esp) (% ebp))))
   (:x8664
    `(progn
       (pushq (% rbp))
       (movq (% rsp) (% rbp))))))

(defx86lapmacro save-stackargs-frame (nstackargs)
  (target-arch-case
   (:x8632
    `(progn
      (movl (% ebp) (@ ,(* (1+ nstackargs) x8632::node-size) (% esp)))
      (leal (@ ,(* (1+ nstackargs) x8632::node-size) (% esp)) (% ebp))
      (popl (@ x8632::node-size (% ebp)))))
   (:x8664
    `(progn
      (movq (% rbp) (@ ,(* (1+ nstackargs) x8664::node-size) (% rsp)))
      (leaq (@ ,(* (1+ nstackargs) x8664::node-size) (% rsp)) (% rbp))
      (popq (@ x8632::node-size (% rbp)))))))

(defx86lapmacro save-frame-variable-arg-count ()
  (let* ((push (gensym))
         (done (gensym)))
    (target-arch-case
     (:x8632
      `(progn
	 (movl (% nargs) (% imm0))
	 (subl ($ (* $numx8632argregs x8632::node-size)) (% imm0))
	 (jle ,push)
	 (movl (% ebp) (@ 4 (% esp) (% imm0)))
	 (leal (@ 4 (% esp) (% imm0)) (% ebp))
	 (popl (@ 4 (% ebp)))
	 (jmp ,done)
	 ,push
	 (save-simple-frame)
	 ,done))
     (:x8664
      `(progn
	 (movl (% nargs) (%l imm0))
	 (subq ($ (* $numx8664argregs x8664::node-size)) (% imm0))
	 (jle ,push)
	 (movq (% rbp) (@ 8 (% rsp) (% imm0)))
	 (leaq (@ 8 (% rsp) (% imm0)) (% rbp))
	 (popq (@ 8 (% rbp)))
	 (jmp ,done)
	 ,push
	 (save-simple-frame)
	 ,done)))))


(defx86lapmacro restore-simple-frame ()
  `(progn
    (leave)))

(defx86lapmacro discard-reserved-frame ()
  (target-arch-case
   (:x8632
    `(add ($ '2) (% esp)))
   (:x8664
    `(add ($ '2) (% rsp)))))

;;; Return to caller.
(defx86lapmacro single-value-return (&optional (words-to-discard 0))
  (target-arch-case
   (:x8632
    (if (zerop words-to-discard)
	`(ret)
	`(ret ($ ,(* x8632::node-size words-to-discard)))))
   (:x8664
    (if (zerop words-to-discard)
	`(ret)
	`(ret ($ ,(* x8664::node-size words-to-discard)))))))

(defun x86-subprim-offset (name)
  (let* ((info (find name (arch::target-subprims-table (backend-target-arch *target-backend*)) :test #'string-equal :key #'subprimitive-info-name))
         (offset (when info 
                   (subprimitive-info-offset info))))
    (or offset      
	(error "Unknown subprim: ~s" name))))

(defx86lapmacro jmp-subprim (name)
  `(jmp (@ ,(x86-subprim-offset name))))

(defx86lapmacro recover-fn ()
  `(movl ($ :self) (% fn)))

(defx86lapmacro call-subprim (name)
  (target-arch-case
   (:x8632
    `(progn
       (:talign x8632::fulltag-tra)
       (call (@ ,(x86-subprim-offset name)))
       (recover-fn)))
   (:x8664
    `(progn
       (:talign 4)
       (call (@ ,(x86-subprim-offset name)))
       (recover-fn-from-rip)))))

 (defx86lapmacro %car (src dest)
  (target-arch-case
   (:x8632
    `(movl (@ x8632::cons.car (% ,src)) (% ,dest)))
   (:x8664
    `(movq (@ x8664::cons.car (% ,src)) (% ,dest)))))

(defx86lapmacro %cdr (src dest)
  (target-arch-case
   (:x8632
    `(movl (@ x8632::cons.cdr (% ,src)) (% ,dest)))
   (:x8664
    `(movq (@ x8664::cons.cdr (% ,src)) (% ,dest)))))

(defx86lapmacro stack-probe ()
  (target-arch-case
   (:x8632
    (let* ((ok (gensym)))
      `(progn
	 (rcmp (% esp) (@ (% rcontext) x8632::tcr.cs-limit))
	 (jae.pt ,ok)
	 (uuo-stack-overflow)
	 ,ok)))
   (:x8664
    (let* ((ok (gensym)))
      `(progn
	 (rcmp (% rsp) (@ (% rcontext) x8664::tcr.cs-limit))
	 (jae.pt ,ok)
	 (uuo-stack-overflow)
	 ,ok)))))

(defx86lapmacro load-constant (constant dest &optional (fn 'fn))
  (target-arch-case
   (:x8632
    `(movl (@ ',constant (% ,fn)) (% ,dest)))
   (:x8664
    `(movq (@ ',constant (% ,fn)) (% ,dest)))))

(defx86lapmacro recover-fn-from-rip ()
  (let* ((next (gensym)))
    `(progn
      (lea (@ (- (:^ ,next)) (% rip)) (% fn))
      ,next)))

;;; call symbol named NAME, setting nargs to NARGS.  Do the TRA
;;; hair.   Args should already be in arg regs, and we expect
;;; to return a single value.
(defx86lapmacro call-symbol (name nargs)
  (target-arch-case
   (:x8632
    `(progn
       (load-constant ,name fname)
       (set-nargs ,nargs)
       (:talign 5)
       (call (@ x8632::symbol.fcell (% fname)))
       (recover-fn)))
   (:x8664
    `(progn
       (load-constant ,name fname)
       (set-nargs ,nargs)
       (:talign 4)
       (call (@ x8664::symbol.fcell (% fname)))
       (recover-fn-from-rip)))))


;;;  tail call the function named by NAME with nargs NARGS.  %FN is
;;;  the caller, which will be in %FN on entry to the callee.  For the
;;;  couple of instructions where neither %RA0 or %FN point to the
;;;  current function, ensure that %XFN does; this is necessary to
;;;  prevent the current function from being GCed halfway through
;;;  those couple of instructions.

(defx86lapmacro jump-symbol (name nargs)
  (target-arch-case
   (:x8632
    `(progn
       (load-constant ,name fname)
       (set-nargs ,nargs)
       (jmp (@ x8632::symbol.fcell (% fname)))))
   (:x8664
    `(progn
       (load-constant ,name fname)
       (set-nargs ,nargs)
       (jmp (@ x8664::symbol.fcell (% fname)))))))

(defx86lapmacro push-argregs ()
  (let* ((done (gensym))
         (yz (gensym))
         (z (gensym)))
    (target-arch-case
     (:x8632
      `(progn
	 (testl (% nargs) (% nargs))
	 (je ,done)
	 (cmpl ($ '1) (% nargs))
	 (je ,z)
	 (push (% arg_y))
	 ,z
	 (push (% arg_z))
	 ,done))
     (:x8664
      `(progn
	 (testl (% nargs) (% nargs))
	 (je ,done)
	 (cmpl ($ '2) (% nargs))
	 (je ,yz)
	 (jb ,z)
	 (push (% arg_x))
	 ,yz
	 (push (% arg_y))
	 ,z
	 (push (% arg_z))
	 ,done)))))

;;; clears reg
(defx86lapmacro mark-as-node (reg)
  (let* ((regnum (logand #x7 (x86::gpr-ordinal (string reg))))
	 (bit (ash 1 regnum)))
    `(progn
       (xorl (% ,reg) (% ,reg))
       (orb ($ ,bit) (@ (% :rcontext) x8632::tcr.node-regs-mask)))))

(defx86lapmacro mark-as-imm (reg)
  (let* ((regnum (logand #x7 (x86::gpr-ordinal (string reg))))
	 (bit (ash 1 regnum)))
    `(progn
       (andb ($ (lognot ,bit)) (@ (% :rcontext) x8632::tcr.node-regs-mask)))))

(defx86lapmacro compose-digit (high low dest)
  (target-arch-case
   (:x8632
    `(progn
       (unbox-fixnum ,low ,dest)
       (andl ($ #xffff) (% ,dest))
       (shll ($ (- 16 x8632::fixnumshift)) (% ,high))
       (orl (% ,high) (% ,dest))))
   (:x8664
    (error "compose-digit on x8664?"))))

(defx86lapmacro imm-word-count (fn imm dest)
  `(progn
     (movzwl (@ x8632::misc-data-offset (% ,fn)) (% ,imm))
     (btr ($ 15) (% ,imm))
     (vector-length ,fn ,dest)
     (box-fixnum ,imm ,imm)
     (subl (% ,imm) (% ,dest))))

(defx86lapmacro double-constant (name value)
  (multiple-value-bind (high low)
      (double-float-bits (float value 1.0d0))
    `(progn
       (:uuo-section)
       (:align 3)
       ,name
       (:long ,low)
       (:long ,high)
       (:main-section))))
