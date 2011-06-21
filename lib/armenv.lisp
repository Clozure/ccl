;;; -*- Mode:Lisp; Package:CCL; -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates
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

(defconstant $numarmsaveregs 0)
(defconstant $numarmargregs 3)


(defconstant arm-nonvolatile-registers-mask
  0)

(defconstant arm-arg-registers-mask
  (logior (ash 1 arm::arg_z)
          (ash 1 arm::arg_y)
          (ash 1 arm::arg_x)))

(defconstant arm-temp-registers-mask
  (logior (ash 1 arm::temp0)
          (ash 1 arm::temp1)
          (ash 1 arm::temp2)))


(defconstant arm-tagged-registers-mask
  (logior arm-temp-registers-mask
          arm-arg-registers-mask
          arm-nonvolatile-registers-mask))



(defconstant arm-temp-node-regs 
  (make-mask arm::temp0
             arm::temp1
             arm::temp2
             arm::arg_x
             arm::arg_y
             arm::arg_z))

(defconstant arm-nonvolatile-node-regs
  0)


(defconstant arm-node-regs (logior arm-temp-node-regs arm-nonvolatile-node-regs))

(defconstant arm-imm-regs (make-mask
                            arm::imm0
                            arm::imm1
                            arm::imm2))

(defconstant arm-temp-fp-regs (1- (ash 1 16)))

(defconstant arm-cr-fields (make-mask 0))

                               




(defconstant $undo-arm-c-frame 16)


(ccl::provide "ARMENV")
