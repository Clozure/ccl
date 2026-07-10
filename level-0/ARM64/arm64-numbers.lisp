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

;; (integer-length n) = (- 63 (cls n))
(defarm64lapfunction %fixnum-intlen ((number arg_z))
  (unbox-fixnum imm0 number)
  (cls imm0 imm0)                       ;result in [0, 63]
  (eor imm0 imm0 (:$ 63))               ;trick: computes (- 63 cls)
  (box-fixnum arg_z imm0)
  (ret))

(defarm64lapfunction %truncate-double-float->fixnum ((arg arg_z))
  (get-double-float d0 arg)
  (fcvtzs imm0 d0)
  (box-fixnum arg_z imm0)
  (ret))

(defarm64lapfunction %truncate-single-float->fixnum ((arg arg_z))
  (get-single-float-bits imm0 arg)
  (fmov s0 (:w imm0))
  (fcvtzs imm0 s0)
  (box-fixnum arg_z imm0)
  (ret))

(defarm64lapfunction %round-nearest-double-float->fixnum ((arg arg_z))
  (get-double-float d0 arg)
  (fcvtns imm0 d0)
  (box-fixnum arg_z imm0)
  (ret))

(defarm64lapfunction %round-nearest-single-float->fixnum ((arg arg_z))
  (get-single-float-bits imm0 arg)
  (fmov s0 (:w imm0))
  (fcvtns imm0 s0)
  (box-fixnum arg_z imm0)
  (ret))
