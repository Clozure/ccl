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
(defun make-callback-trampoline (index &optional discard-stack-bytes)
  (declare (ignore discard-stack-bytes))
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
(defun make-callback-trampoline (index &optional (discard-stack-bytes 0))
  (let* ((p (%allocate-callback-pointer 12))
         (addr #.(subprim-name->offset '.SPcallback)))
    (setf (%get-unsigned-byte p 0) #xb8 ; movl $n,%eax
          (%get-unsigned-byte p 1) (ldb (byte 8 0) index)
          (%get-unsigned-byte p 2) (ldb (byte 8 8) index)
          (%get-unsigned-byte p 3) (ldb (byte 8 16) index)
          (%get-unsigned-byte p 4) (ldb (byte 8 24) (ash (or discard-stack-bytes 0) (- x8632::word-shift)))
          (%get-unsigned-byte p 5) #xff  ; jmp *
          (%get-unsigned-byte p 6) #x24
          (%get-unsigned-byte p 7) #x25
          (%get-unsigned-byte p 8) (ldb (byte 8 0) addr)
          (%get-unsigned-byte p 9) (ldb (byte 8 8) addr)
          (%get-unsigned-byte p 10) (ldb (byte 8 16) addr)
          (%get-unsigned-byte p 11) (ldb (byte 8 24) addr))
    p))
  
