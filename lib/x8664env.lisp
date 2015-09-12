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

(defconstant $numx8664saveregs 0)
(defconstant $numx8664argregs 3)


(defconstant x8664-nonvolatile-registers-mask
  0)

(defconstant x8664-arg-registers-mask
  (logior (ash 1 x8664::arg_z)
          (ash 1 x8664::arg_y)
          (ash 1 x8664::arg_x)))

(defconstant x8664-temp-registers-mask
  (logior (ash 1 x8664::temp0)
          (ash 1 x8664::temp1)
          (ash 1 x8664::temp2)
          (ash 1 x8664::temp3)
          (ash 1 x8664::temp4)
          (ash 1 x8664::temp5)))



(defconstant x8664-tagged-registers-mask
  (logior x8664-temp-registers-mask
          x8664-arg-registers-mask
          x8664-nonvolatile-registers-mask))


(defconstant x8664-temp-node-regs 
  (make-mask x8664::temp0
             x8664::temp1
             x8664::temp2
             x8664::temp3
             x8664::temp4
             x8664::temp5
             x8664::arg_x
             x8664::arg_y
             x8664::arg_z))

(defconstant x8664-nonvolatile-node-regs
  0)


(defconstant x8664-node-regs (logior x8664-temp-node-regs x8664-nonvolatile-node-regs))

(defconstant x8664-imm-regs (make-mask
                             x8664::imm0
                             x8664::imm1
                             x8664::imm2))

(defconstant x8664-temp-fp-regs (make-mask (logand x8664::fp0 15)
                                           (logand x8664::fp1 15)
                                           (logand x8664::fp2 15)
                                           (logand x8664::fp3 15)
                                           (logand x8664::fp4 15)
                                           (logand x8664::fp5 15)
                                           (logand x8664::fp6 15)
                                           (logand x8664::fp7 15)
                                           (logand x8664::fp8 15)
                                           (logand x8664::fp9 15)
                                           (logand x8664::fp10 15)                               
                                           (logand x8664::fp11 15)                               
                                           (logand x8664::fp12 15)                               
                                           (logand x8664::fp13 15)                               
                                           (logand x8664::fp14 15)                               
))
                               


(defconstant x8664-cr-fields (make-mask 0))

(defconstant $undo-x86-c-frame 16)


(ccl::provide "X8664ENV")
