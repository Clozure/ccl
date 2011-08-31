; -*- Mode:Lisp; Package:CCL; -*-
;;;
;;;   Copyright (C) 2005-2009 Clozure Associates
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

(defconstant $numx8632saveregs 0)
(defconstant $numx8632argregs 2)


(defconstant x8632-nonvolatile-registers-mask 0)

(defconstant x8632-arg-registers-mask
  (logior (ash 1 x8632::arg_z)
          (ash 1 x8632::arg_y)))
  
(defconstant x8632-temp-registers-mask
  (logior (ash 1 x8632::temp0)
	  (ash 1 x8632::temp1)))
  
(defconstant x8632-tagged-registers-mask
  (logior x8632-temp-registers-mask
          x8632-arg-registers-mask
          x8632-nonvolatile-registers-mask))



(defconstant x8632-temp-node-regs 
  (make-mask x8632::temp0
	     x8632::temp1
             x8632::arg_y
             x8632::arg_z))

(defconstant x8632-nonvolatile-node-regs 0)

(defconstant x8632-node-regs (logior x8632-temp-node-regs x8632-nonvolatile-node-regs))

(defconstant x8632-imm-regs (make-mask
                             x8632::imm0))

;;; Fine if we assume SSE support;  not so hot when using x87
(defconstant x8632-temp-fp-regs (make-mask (logand x8632::fp0 7)
                                           (logand x8632::fp1 7)
                                           (logand x8632::fp2 7)
                                           (logand x8632::fp3 7)
                                           (logand x8632::fp4 7)
                                           (logand x8632::fp5 7)
                                           (logand x8632::fp6 7)
                                           (logand x8632::fp7 7)))
                               


(defconstant x8632-cr-fields (make-mask 0))

;;; hmm.
(defconstant $undo-x86-c-frame 16)


(ccl::provide "X8632ENV")
