;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates and contributors.
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

;;; Some primitives for accessing Intel I/O ports from CCL.
;;; Note that port access requires special privileges which
;;; the OS may or may not provide (and generally only provides
;;; to root/privileged users if it does provide them.)
;;; Port addresses must be unsigned 16-bit integers.
;;; Values written via '%outb', '%outw', and '%outl' must be
;;; unsigned            8-bit    16-bit  or   32-bit integers
;;;
;;; (%inb port) - read an unsigned 8-bit byte from the specified I/O port
;;; (%inw port) -                  16-bit
;;; (%inl port) -                  32-bit
;;; (%outb val port) - write an unsigned 8-bit value to the specified I/O port
;;; (%outw val port) - write an unsigned 16-bit value to the specified I/O port
;;; (%outl val port) - write an unsigned 32-bit value to the specified I/O port


#+x8632-target
(progn
(defx8632lapfunction %inb ((port arg_z))
  (mark-as-imm temp1)
  (unbox-fixnum port edx)
  (:byte #xec)                          ;inb (%dx),%al
  (mark-as-node temp1)
  (movzbl (% al) (% eax))
  (box-fixnum eax arg_z)
  (single-value-return))

  
(defx8632lapfunction %inw ((port arg_z))
  (mark-as-imm temp1)
  (unbox-fixnum port edx)
  (:byte #x66) (:byte #xed)             ;inw (%dx),%ax
  (mark-as-node temp1)
  (movzwl (% ax) (% eax))
  (box-fixnum eax arg_z)
  (single-value-return))

(defx8632lapfunction %inl ((port arg_z))
  (mark-as-imm temp1)
  (unbox-fixnum port edx)
  (:byte #xed)                          ;inl (%dx),%eax
  (mark-as-node temp1)
  (jmp-subprim .SPmakeu32))


(defx8632lapfunction %outb ((val arg_y) (port arg_z))
  (unbox-fixnum val eax)
  (mark-as-imm temp1)
  (unbox-fixnum port edx)
  (:byte #xee)                          ;outb %al,(%dx)
  (mark-as-node temp1)
  (mov (% val) (% arg_z))
  (single-value-return))


(defx8632lapfunction %outw ((val arg_y) (port arg_z))
  (unbox-fixnum val eax)
  (mark-as-imm temp1)
  (unbox-fixnum port edx)
  (:byte #x66) (:byte #xef)                          ;outw %ax,(%dx)
  (mark-as-node temp1)
  (mov (% val) (% arg_z))
  (single-value-return))


(defx8632lapfunction %outl ((val arg_y) (port arg_z))
  (save-simple-frame)
  (pushl (% port))
  (movl (% val) (% arg_z))
  (call-subprim .SPgetu32)
  (popl (% temp0))
  (mark-as-imm temp1)
  (unbox-fixnum temp0 edx)
  (:byte #xef)                          ;outl %eax,(%dx)
  (mark-as-node temp1)
  (restore-simple-frame)
  (single-value-return))
)

#+x8664-target
(progn
(defx86lapfunction %inb ((port arg_z))
  (unbox-fixnum port rdx)
  (:byte #xec)                          ;inb (%dx),%al
  (movzbl (% al) (% eax))
  (box-fixnum rax arg_z)
  (single-value-return))

  
(defx86lapfunction %inw ((port arg_z))
  (unbox-fixnum port rdx)
  (:byte #x66) (:byte #xed)             ;inw (%dx),%ax
  (movzwl (% ax) (% eax))
  (box-fixnum rax arg_z)
  (single-value-return))

(defx86lapfunction %inl ((port arg_z))
  (unbox-fixnum port rdx)
  (:byte #xed)                          ;inl (%dx),%eax
  (box-fixnum rax arg_z)
  (single-value-return))


(defx86lapfunction %outb ((val arg_y) (port arg_z))
  (unbox-fixnum val rax)
  (unbox-fixnum port rdx)
  (:byte #xee)                          ;outb %al,(%dx)
  (movq (% val) (% arg_z))
  (single-value-return))


(defx86lapfunction %outw ((val arg_y) (port arg_z))
  (unbox-fixnum val rax)
  (unbox-fixnum port rdx)
  (:byte #x66) (:byte #xef)                          ;outw %ax,(%dx)
  (mov (% val) (% arg_z))
  (single-value-return))


(defx86lapfunction %outl ((val arg_y) (port arg_z))
  (unbox-fixnum val imm0)
  (unbox-fixnum port rdx)
  (:byte #xef)                          ;outl %eax,(%dx)
  (mov (% val) (% arg_z))
  (single-value-return))
)


;;; Linux provides two primitives which allow a process running as
;;; a privileged user to execute I/O instructions.

;;; #_ioperm can be used to gain/renounce access to a range if I/O
;;; ports; all ports in that range must be below #x4000.
;;; #_iopl can be used to set the calling process's privilege level
;;; to a value between 0 and 3; 0 being the level at which user code
;;; usually runs and 3 being the most privileged level.

#+(and linux-target x86-target)
(progn
(defun ioperm (enable-p first-port last-port)
  (check-type first-port (integer 0 (#x400)))
  (check-type last-port (integer 0 (#x400)))
  (unless (<= first-port last-port)
    (error "First port ~d must be <= last port ~d." first-port last-port))
  (or (eql 0
           (external-call "ioperm"
                          :unsigned-long first-port
                          :unsigned-long (1+ (- last-port first-port))
                          :int (if enable-p 1 0)
                          :int))
      (error "Error ~aing port access: ~a."
             (if enable-p "enabl" "disabl")
             (%strerror (%get-errno)))))

(defun iopl (level)
  (check-type level (integer 0 3))
  (or (eql 0 (external-call "iopl" :int level :int))
      (error "Can't set I/O privilege level to ~d: ~a."
             level
             (%strerror (%get-errno)))))
)

;;; Other OSes may provide similar functionality.

