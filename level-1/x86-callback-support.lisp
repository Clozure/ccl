;;;
;;;   Copyright (C) 2005-2006 Clozure Associates and contributors
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


#+x8664-target  
(defun make-callback-trampoline (index &optional info)
  (declare (ignore info))
  (let* ((p (%allocate-callback-pointer 16))
         (addr #.(subprim-name->offset '.SPcallback)))
    (setf (%get-unsigned-byte p 0) #x41 ; movl $n,%r11d
          (%get-unsigned-byte p 1) #xc7
          (%get-unsigned-byte p 2) #xc3
          (%get-unsigned-byte p 3) (ldb (byte 8 0) index)
          (%get-unsigned-byte p 4) (ldb (byte 8 8) index)
          (%get-unsigned-byte p 5) (ldb (byte 8 16) index)
          (%get-unsigned-byte p 6) (ldb (byte 8 24) index)
          (%get-unsigned-byte p 7) #xff  ; jmp *
          (%get-unsigned-byte p 8) #x24
          (%get-unsigned-byte p 9) #x25
          (%get-unsigned-byte p 10) (ldb (byte 8 0) addr)
          (%get-unsigned-byte p 11) (ldb (byte 8 8) addr)
          (%get-unsigned-byte p 12) (ldb (byte 8 16) addr)
          (%get-unsigned-byte p 13) (ldb (byte 8 24) addr))
    p))
          
#+x8632-target          
(defun make-callback-trampoline (index &optional info)
  (let* ((p (%allocate-callback-pointer 12))
         (addr #.(subprim-name->offset '.SPcallback)))
    ;; If the optional info parameter is supplied, it will contain
    ;; some stuff in bits 23 through 31.
    ;;
    ;; If bit 23 is set, that indicates that the caller will pass a
    ;; "hidden" argument which is a pointer to appropriate storage for
    ;; holding a returned structure.  .SPcallback will have to discard
    ;; this extra argument upon return.
    ;;
    ;; The high 8 bits denote the number of words that .SPcallback
    ;; will have to discard upon return (used for _stdcall on
    ;; Windows).  Bit 23 won't be set in this case: we will have
    ;; already added in the extra word to discard if that's necessary.
    ;; 
    ;; These bits are be packed into the value that .SPcallback
    ;; receives in %eax.  Bits 0 through 22 are the callback index.
    (if info
      (setf (ldb (byte 23 0) info) index)
      (setq info index))
    (setf (%get-unsigned-byte p 0) #xb8 ; movl $n,%eax
          (%get-unsigned-byte p 1) (ldb (byte 8 0) info)
          (%get-unsigned-byte p 2) (ldb (byte 8 8) info)
          (%get-unsigned-byte p 3) (ldb (byte 8 16) info)
          (%get-unsigned-byte p 4) (ldb (byte 8 24) info)
          (%get-unsigned-byte p 5) #xff  ; jmp *
          (%get-unsigned-byte p 6) #x24
          (%get-unsigned-byte p 7) #x25
          (%get-unsigned-byte p 8) (ldb (byte 8 0) addr)
          (%get-unsigned-byte p 9) (ldb (byte 8 8) addr)
          (%get-unsigned-byte p 10) (ldb (byte 8 16) addr)
          (%get-unsigned-byte p 11) (ldb (byte 8 24) addr))
    p))
  
