; -*- Mode:Lisp; Package:CCL; -*-
;;;
;;; Copyright 2005-2009 Clozure Associates
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
