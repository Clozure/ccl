;;; -*- Mode: Lisp; Package: CCL -*-
;;; ARM64-SPECIFIC — a callback trampoline is raw machine code, so the
;;; body is inherently per-ISA; no PPC64 or Clozure-WIP analog encoding
;;; exists (PPC64 `ba' / ARM32 `ldr pc,[pc,#-4]` — see below).
;;;
;;; arm64-callback-support.lisp — callback trampoline generator for Matt
;;; Emerson's upstream ARM64 (low-tag) design.
;;;
;;; ISA-specific by nature (the body is raw machine code): the LOGIC
;;; mirrors arm-callback-support.lisp:19 and x86-callback-support.lisp:21
;;; (allocate a callback pointer, stamp the callback index into the
;;; register the kernel's _SPcallback reads, jump there, make the stub
;;; executable, return the pointer).  PPC64 reaches the subprim with a
;;; `ba' absolute branch and ARM32 with an `ldr pc,[pc,#-4]' literal
;;; jump; neither encoding exists on AArch64, hence the x16 literal jump.
;;;
;;; Trampoline layout (32 bytes, entered by FOREIGN code under AAPCS64;
;;; x10 and x16 (IP0) are caller-saved scratch, safe to clobber.  x8 is
;;; NOT: AAPCS64 6.9 delivers the indirect result-area pointer there
;;; when the callback returns a >16-byte non-HFA record, and _SPcallback
;;; captures it into the callback frame -- the index lived in x8 until
;;; 16m71 and silently destroyed that pointer):
;;;
;;;    0: movz x10, #lo16(index)        ; unboxed callback index
;;;    4: movk x10, #hi16(index), lsl 16
;;;    8: ldr  x16, .+16                ; load _SPcallback address
;;;   12: br   x16                      ;   from the literal at +24
;;;   16: nop                           ; pad literal to 8-byte alignment
;;;   20: nop
;;;   24: .quad <_SPcallback kernel address>
;;;
;;; MATCHED PAIR: _SPcallback (upstream-port/lisp-kernel/spentry-E-ffi.s)
;;; reads the index from arg_y = x10 (`mov save0, arg_y`).  Change this
;;; generator and that entry together.

(in-package "CCL")

(defun make-callback-trampoline (index &optional info)
  (declare (ignorable info))
  (let* ((p (%allocate-callback-pointer 32))
         (addr (%lookup-subprim-address
                #.(arm64::subprimitive-offset ".SPcallback"))))
    (setf (%get-unsigned-long p 0)          ; movz x10,#lo16(index)
          (logior #xd280000a (ash (ldb (byte 16 0) index) 5))
          (%get-unsigned-long p 4)          ; movk x10,#hi16(index),lsl #16
          (logior #xf2a0000a (ash (ldb (byte 16 16) index) 5))
          (%get-unsigned-long p 8)  #x58000090   ; ldr x16,.+16
          (%get-unsigned-long p 12) #xd61f0200   ; br x16
          (%get-unsigned-long p 16) #xd503201f   ; nop
          (%get-unsigned-long p 20) #xd503201f   ; nop
          (%%get-unsigned-longlong p 24) addr)
    ;; I/D-cache sync — REQUIRED on arm64 before the stub is executed
    ;; (same idiom as %make-code-executable, arm64-def.lisp).
    (ff-call (%kernel-import #.arm64::kernel-import-makedataexecutable)
             :address p
             :unsigned-fullword 32
             :void)
    p))
