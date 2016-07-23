; -*- Mode:Lisp; Package:CCL; -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

(defconstant $numppcsaveregs 8)
(defconstant $numppcargregs 3)


(defconstant ppc-nonvolatile-registers-mask
  (logior (ash 1 ppc::save0)
          (ash 1 ppc::save1)
          (ash 1 ppc::save2)
          (ash 1 ppc::save3)
          (ash 1 ppc::save4)
          (ash 1 ppc::save5)
          (ash 1 ppc::save6)
          (ash 1 ppc::save7)))

(defconstant ppc-arg-registers-mask
  (logior (ash 1 ppc::arg_z)
          (ash 1 ppc::arg_y)
          (ash 1 ppc::arg_x)))

(defconstant ppc-temp-registers-mask
  (logior (ash 1 ppc::temp0)
          (ash 1 ppc::temp1)
          (ash 1 ppc::temp2)
          (ash 1 ppc::temp3)))


(defconstant ppc-tagged-registers-mask
  (logior ppc-temp-registers-mask
          ppc-arg-registers-mask
          ppc-nonvolatile-registers-mask))



(defconstant ppc-temp-node-regs 
  (make-mask ppc::temp0
             ppc::temp1
             ppc::temp2
             ppc::temp3
             ppc::arg_x
             ppc::arg_y
             ppc::arg_z))

(defconstant ppc-nonvolatile-node-regs
  (make-mask ppc::save0
             ppc::save1
             ppc::save2
             ppc::save3
             ppc::save4
             ppc::save5
             ppc::save6
             ppc::save7))


(defconstant ppc-node-regs (logior ppc-temp-node-regs ppc-nonvolatile-node-regs))

(defconstant ppc-imm-regs (make-mask
                            ppc::imm0
                            ppc::imm1
                            ppc::imm2
                            ppc::imm3
                            ppc::imm4
                            ppc::imm5))

(defconstant ppc-temp-fp-regs (1- (ash 1 ppc::fp14)))
                               
(defconstant ppc-cr-fields
  (make-mask 0 (ash 4 -2) (ash 8 -2) (ash 12 -2) (ash 16 -2) (ash 20 -2) (ash 24 -2) (ash 28 -2)))



(defconstant $undo-ppc-c-frame 16)


(ccl::provide "PPCENV")
