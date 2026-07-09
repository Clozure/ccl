;;;;-*- Mode: Lisp; Package: CCL -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(in-package "CCL")

(defarm64lapfunction %fixnum-signum ((number arg_z))
  (cmp number (:$ 0))
  (cset imm0 (:? gt))                  ;1 if number > 0, else 0
  (csinv imm0 imm0 xzr (:? ge))        ;same (0 or 1) if number >= 0, else -1
  (lsl arg_z imm0 (:$ arm64::fixnumshift))
  (ret))

(defarm64lapfunction %ilogcount ((number arg_z))
  (fmov d0 number)
  (cnt (:8b d0) (:8b d0))
  (addv b0 (:8b d0))
  (fmov (:w imm0) (:s d0))
  (lsl arg_z imm0 (:$ arm64::fixnumshift))
  (ret))

;; positive count: shift left, negative count: shift right
(defarm64lapfunction %iash ((number arg_y) (count arg_z))
  (unbox-fixnum imm1 count)
  (negs imm2 imm1)
  (b.lt @left)
  (unbox-fixnum imm0 number)
  (asr imm0 imm0 imm2)
  (box-fixnum arg_z imm0)
  (ret)
  @left
  (lsl arg_z number imm1)
  (ret))
