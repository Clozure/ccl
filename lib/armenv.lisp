;;; -*- Mode:Lisp; Package:CCL; -*-
;;;
;;; Copyright 2010 Clozure Associates
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

(defconstant arm-temp-fp-regs (1- (ash 1 28)))

(defconstant arm-cr-fields (make-mask 0))

                               




(defconstant $undo-arm-c-frame 16)


(ccl::provide "ARMENV")
