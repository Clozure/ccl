;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2005, Clozure Associates and contributors.
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

;;; Comparisons make more sense if arg order is "dest, src", instead
;;; of the gas/ATT arg order.

(defx86lapmacro rcmp (src dest)
  `(cmp ,dest ,src))

(defx86lapmacro clrq (reg)
  `(xorq (% ,reg) (% ,reg)))

(defx86lapmacro set-nargs (n)
  (if (eql n 0)
    `(xorw (% nargs) (% nargs))
    `(movw ($ ',n) (% nargs))))

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
             `(testw (% nargs) (% nargs))
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
  `(progn
    (movb ($ x8664::tagmask) (%b ,dest))
    (andb (%b ,node) (%b ,dest))))

(defx86lapmacro extract-fulltag (node dest)
  `(progn
    (movb ($ x8664::fulltagmask) (%b ,dest))
    (andb (%b ,node) (%b ,dest))))

(defx86lapmacro extract-subtag (node dest)
  `(movb (@ x8664::misc-subtag-offset (% ,node)) (%b ,dest)))

(defx86lapmacro extract-typecode (node dest)
  ;;; In general, these things are only defined to affect the low
  ;;; byte of the destination register.  This can also affect
  ;;; the #xff00 byte.
  (let* ((done (gensym)))
    `(progn
      (extract-lisptag ,node ,dest)
      (rcmp (%b ,dest) ($ x8664::tag-misc))
      (jne ,done)
      (movb (@  x8664::misc-subtag-offset (% ,node)) (%b ,dest))
      ,done)))

(defx86lapmacro trap-unless-typecode= (node tag &optional (immreg 'imm0))
  (let* ((ok (gensym)))
    `(progn
      (extract-typecode ,node ,immreg)
      (cmpb ($ ,tag) (%b ,immreg))
      (je.pt ,ok)
      (uuo-error-reg-not-tag (% ,node) ($ ,tag))
      ,ok)))

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
    `(progn
      (testb ($ x8664::tagmask) (%b ,node))
      (je.pt ,ok)
      (uuo-error-reg-not-fixnum (% ,node))
      ,ok)))

;;; On x8664, NIL has its own tag, so no other lisp object can
;;; have the same low byte as NIL.  (That probably won't be
;;; true on x8632.)
(defx86lapmacro cmp-reg-to-nil (reg)
  `(cmpb ($ (logand #xff x8664::nil-value)) (%b ,reg)))


(defx86lapmacro unbox-fixnum (src dest)
  `(progn
    (mov (% ,src) (% ,dest))
    (sar ($ x8664::fixnumshift) (% ,dest))))

(defx86lapmacro box-fixnum (src dest)
  `(imulq ($ x8664::fixnumone) (% ,src) (% ,dest)))


(defx86lapmacro get-single-float (node dest)
  `(progn
    (movd (% ,node) (% ,dest))
    (psrlq ($ 32) (% ,dest))))


;;; Note that this modifies the src argument.
(defx86lapmacro put-single-float (src node)
  `(progn
    (psllq ($ 32) (% ,src))
    (movd (% ,src) (% ,node))
    (movb ($ x8664::tag-single-float) (%b ,node))))

(defx86lapmacro get-double-float (src fpreg)
  `(movsd (@ x8664::double-float.value (% ,src)) (% ,fpreg)))

(defx86lapmacro put-double-float (fpreg dest)
  `(movsd (% ,fpreg) (@ x8664::double-float.value (% ,dest))))
  

  
(defx86lapmacro getvheader (src dest)
  `(movq (@ x8664::misc-header-offset (% ,src)) (% ,dest)))

;;; "Size" is unboxed element-count.  vheader and dest should
;;; both be immediate registers
(defx86lapmacro header-size (vheader dest)
  `(progn
    (mov (% ,vheader) (% ,dest))
    (shr ($ x8664::num-subtag-bits) (% ,dest))))


;;; "Length" is fixnum element-count.
(defx86lapmacro header-length (vheader dest)
  `(progn
    (movq ($ (lognot 255)) (% ,dest))
    (andq (% ,vheader) (% ,dest))
    (shr ($ (- x8664::num-subtag-bits x8664::fixnumshift)) (% ,dest))))

(defx86lapmacro header-subtag[fixnum] (vheader dest)
  `(progn
    (lea (@ (% ,vheader) 8) (% ,dest))
    (andl ($ '255) (%l ,dest))))

(defx86lapmacro vector-size (vector vheader dest)
  `(progn
    (getvheader ,vector ,vheader)
    (header-size ,vheader ,dest)))

(defx86lapmacro vector-length (vector dest)
  `(progn
    (movq ($ (lognot 255)) (% ,dest))
    (andq (@ x8664::misc-header-offset (% ,vector)) (% ,dest))
    (shr ($ (- x8664::num-subtag-bits x8664::fixnumshift)) (% ,dest))))  


(defx86lapmacro int-to-double (int temp double)
  `(progn
    (unbox-fixnum  ,int ,temp)
    (cvtsi2sdq (% ,temp) (% ,double))))

(defx86lapmacro int-to-single (int temp single)
  `(progn
    (unbox-fixnum ,int ,temp)
    (cvtsi2ssq (% ,temp) (% ,single))))

(defx86lapmacro ref-global (global reg)
  `(movq (@ (+ x8664::nil-value ,(x8664::%kernel-global global))) (% ,reg)))

(defx86lapmacro ref-global.l (global reg)
  `(movl (@ (+ x8664::nil-value ,(x8664::%kernel-global global))) (%l ,reg)))

(defx86lapmacro set-global (reg global)
  `(movq (% ,reg) (@ (+ x8664::nil-value ,(x8664::%kernel-global global)))))

(defx86lapmacro macptr-ptr (src dest)
  `(movq (@ x8664::macptr.address (% ,src)) (% ,dest)))

;;; CODE is unboxed char-code (in low 8 bits); CHAR needs to be boxed.
(defx86lapmacro box-character (code char)
  `(progn
    (box-fixnum ,code ,char)
    (shl ($ (- x8664::charcode-shift x8664::fixnumshift)) (% ,char))
    (movb ($ x8664::subtag-character) (%b ,char))))

  
;;; index is a constant
(defx86lapmacro svref (vector index dest)
  `(movq (@ (+ x8664::misc-data-offset (* ,index 8)) (% ,vector)) (% ,dest)))

;;; Index is still a constant
(defx86lapmacro svset (vector index new)
  `(movq (% ,new) (@ (+ x8664::misc-data-offset (* ,index 8)) (% ,vector))))



;;; Frames, function entry and exit.


;;; Simple frame, since the caller didn't reserve space for it.
(defx86lapmacro save-simple-frame ()
  `(progn
    (pushq (% rbp))
    (movq (% rsp) (% rbp))))

(defx86lapmacro save-frame-variable-arg-count ()
  (let* ((push (gensym))
         (done (gensym)))
  `(progn
    (movzwl (% nargs) (%l imm0))
    (subq ($ (* $numx8664argregs x8664::node-size)) (% imm0))
    (jle ,push)
    (movq (% rbp) (@ 8 (% rsp) (% imm0)))
    (leaq (@ 8 (% rsp) (% imm0)) (% rbp))
    (popq (@ 8 (% rbp)))
    (jmp ,done)
    ,push
    (save-simple-frame)
    ,done)))


(defx86lapmacro restore-simple-frame ()
  `(progn
    (leave)))



(defx86lapmacro discard-reserved-frame ()
  `(add ($ '2) (% rsp)))

;;; Return to caller.
(defx86lapmacro single-value-return (&optional (words-to-discard 0))
  (if (zerop words-to-discard)
    `(ret)
    `(ret ($ ,(* x8664::node-size words-to-discard)))))

;;; Using *x8664-backend* here is wrong but expedient.
(defun x86-subprim-offset (name)
  (let* ((info (find name (arch::target-subprims-table (backend-target-arch *x8664-backend*)) :test #'string-equal :key #'subprimitive-info-name))
         (offset (when info 
                   (subprimitive-info-offset info))))
    (or offset      
      (error "Unknown subprim: ~s" name))))

(defx86lapmacro jmp-subprim (name)
  `(jmp (@ ,(x86-subprim-offset name))))

(defx86lapmacro call-subprim (name)
  `(progn
    (:talign 4)
    (call (@ ,(x86-subprim-offset name)))
    (recover-fn-from-rip)))

     
(defx86lapmacro %car (src dest)
  `(movq (@ x8664::cons.car (% ,src)) (% ,dest)))

(defx86lapmacro %cdr (src dest)
  `(movq (@ x8664::cons.cdr (% ,src)) (% ,dest)))

(defx86lapmacro stack-probe ()
  (let* ((ok (gensym)))
    `(progn
      (rcmp (% rsp) (@ (% rcontext) x8664::tcr.cs-limit))
      (jae.pt ,ok)
      (uuo-stack-overflow)
      ,ok)))

(defx86lapmacro load-constant (constant dest &optional (fn 'fn))
  `(movq (@ ',constant (% ,fn)) (% ,dest)))

(defx86lapmacro recover-fn-from-rip ()
  (let* ((next (gensym)))
    `(progn
      (lea (@ (- (:^ ,next)) (% rip)) (% fn))
      ,next)))

;;; call symbol named NAME, setting nargs to NARGS.  Do the TRA
;;; hair.   Args should already be in arg regs, and we expect
;;; to return a single value.
(defx86lapmacro call-symbol (name nargs)
  `(progn
    (load-constant ,name fname)
    (set-nargs ,nargs)
    (:talign 4)
    (call (@ x8664::symbol.fcell (% fname)))
    (recover-fn-from-rip)))


;;;  tail call the function named by NAME with nargs NARGS.  %FN is
;;;  the caller, which will be in %FN on entry to the callee.  For the
;;;  couple of instructions where neither %RA0 or %FN point to the
;;;  current function, ensure that %XFN does; this is necessary to
;;;  prevent the current function from being GCed halfway through
;;;  those couple of instructions.
(defx86lapmacro jump-symbol (name nargs)
  `(progn
    (load-constant ,name fname)
    (set-nargs ,nargs)
    (jmp (@ x8664::symbol.fcell (% fname)))))

(defx86lapmacro push-argregs ()
  (let* ((done (gensym))
         (yz (gensym))
         (z (gensym)))
  `(progn
    (testw (% nargs) (% nargs))
    (je ,done)
    (cmpw ($ '2) (% nargs))
    (je ,yz)
    (jb ,z)
    (push (% arg_x))
    ,yz
    (push (% arg_y))
    ,z
    (push (% arg_z))
    ,done)))
    
