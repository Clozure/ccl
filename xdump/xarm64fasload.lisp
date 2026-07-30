;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 2016 Clozure Associates
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

(eval-when (:compile-toplevel :execute)
  (require "FASLENV" "ccl:xdump;faslenv")
  (require "ARM64-LAP"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "XFASLOAD" "ccl:xdump;xfasload"))


#+nil
(defun xload-arm-set-entrypoint (xload-fn)
  (setf (xload-%svref xload-fn 0)
        (logandc2 (xload-%svref xload-fn 1) arm::fixnummask)))

#+nil
(defun xload-arm-lap-word (instruction-form)
  (if (listp instruction-form)
    (uvref (uvref (compile nil
                           `(lambda (&lap 0)
                             (arm-lap-function () ((?? 0))
                              ,instruction-form)))
                  1)
           0)
    instruction-form))

;;; udf: uuo_error_apply_macro fname -- "funcalled a macro or special
;;; operator name"; the kernel routes it to error_apply_macro_or_special.
;;; A64 UDF is a 16-bit immediate in an all-zero opcode, so the word is
;;; arithmetic on the kernel's own uuo encoding (lisp-kernel/arm64-uuo.s):
;;; (7<<7) | (Rfname=15<<2) | unary=1.
(defparameter *arm64-macro-apply-code*
  (make-array 1 :element-type '(unsigned-byte 32)
              :initial-contents '(#x3BD)))

#|
(defparameter *arm-macro-apply-code*
  (let* ((code-vector (uvref (compile nil
                                      '(lambda (&lap 0)
                                        (arm-lap-function () ()
                                         (build-lisp-frame imm0)
                                         (sploadlr .SPheap-rest-arg)
                                         (blx lr)
                                         (vpop1 arg_z)
                                         (mov arg_y fname)
                                         (mov arg_x '#.$xnotfun)
                                         (set-nargs 3)
                                         (spjump .SPksignalerr))))
                             1))
         (n (uvsize code-vector))
         (u32-vector (make-array n :element-type '(unsigned-byte 32))))
    (declare (fixnum n))
    (dotimes (i n u32-vector)
      (setf (uvref u32-vector i)
            (uvref code-vector i)))))
|#

(defun arm64-fixup-macro-apply-code ()
  *arm64-macro-apply-code*)


#|
(defparameter *arm-closure-trampoline-code*
  (let* ((code0 (xload-arm-lap-word `(ldr pc (:@ rcontext (:$ ,(arm::arm-subprimitive-offset '.SPcall-closure)))))))
    (make-array 1
                :element-type '(unsigned-byte 32)
                :initial-contents
                (list code0))))
|#

;;; The real .SPcall-closure dispatch, in the canonical subprim-call shape
;;; (movz imm1,#offset / ldr imm1,[rcontext,imm1] / br imm1).  Hand-encoded
;;; because xload writes raw code words:
;;;   movz x1,#imm16   = #xD2800001 | imm16<<5
;;;   ldr  x1,[x28,x1] = #xF8616B81  (LDR reg, option LSL 0, Rn=28, Rt=1)
;;;   br   x1          = #xD61F0020
;;; The offset is computed at xload time from the subprim table.
(defparameter *arm64-closure-trampoline-code*
  (let ((offset (arm64::subprimitive-offset ".SPcall-closure")))
    (make-array 3 :element-type '(unsigned-byte 32)
                :initial-contents
                (list (logior #xD2800001 (ash offset 5))
                      #xF8616B81
                      #xD61F0020))))

#|
;;; For now, do this with a UUO so that the kernel can catch it.
(defparameter *arm-udf-code*
  (let* ((code '((uuo-error-udf-call (:? al) fname)
                 (ldr nfn (:@ fname (:$ arm::symbol.fcell)))
                 (ldr pc (:@ nfn (:$ arm::function.entrypoint))))))
    (make-array (length code)
                :element-type '(unsigned-byte 32)
                :initial-contents
                (mapcar #'xload-arm-lap-word code))))
|#

;;; For now, do this with a UUO so that the kernel can catch it:
;;; uuo_error_udf_call fname = (4<<7) | (Rfname=15<<2) | unary=1.
(defparameter *arm64-udf-code*
  (make-array 1 :element-type '(unsigned-byte 32)
              :initial-contents '(#x23D)))

(defun arm64-initialize-static-space ()
  ;; x8664-initialize-static-space's shape (the commented-out sketch below
  ;; was already these two xload-make-cons lines): one page-filling
  ;; ivector, then the misaligned-cons NIL pun, which lands NIL at
  ;; canonical-nil-value.
  (xload-make-ivector *xload-static-space*
                      (xload-target-subtype :unsigned-64-bit-vector)
                      (1- (/ 4096 8)))
  (xload-make-cons *xload-target-nil* 0 *xload-static-space*)
  (xload-make-cons 0 *xload-target-nil* *xload-static-space*))



#+nil
(defparameter *linuxarm-xload-backend*
  (make-backend-xload-info
   :name :linuxarm
   :macro-apply-code-function 'arm-fixup-macro-apply-code
   :closure-trampoline-code *arm-closure-trampoline-code*
   :udf-code *arm-udf-code*
   :default-image-name "ccl:ccl;arm-boot"
   :default-startup-file-name "level-1.lafsl"
   :subdirs '("ccl:level-0;ARM;")
   :compiler-target-name :linuxarm
   :image-base-address #x10000000
   :nil-relative-symbols arm::*arm-nil-relative-symbols*
   :static-space-init-function 'arm-initialize-static-space
   :purespace-reserve (ash 64 20)
   :static-space-address (- (- arm::nil-value arm::fulltag-nil) (ash 1 12))
))

#+nil
(add-xload-backend *linuxarm-xload-backend*)

(defparameter *linuxarm64-xload-backend*
  (make-backend-xload-info
   :name :linuxarm64
   :macro-apply-code-function 'arm64-fixup-macro-apply-code
   :closure-trampoline-code *arm64-closure-trampoline-code*
   :udf-code *arm64-udf-code*
   :default-image-name "ccl:ccl;arm64-boot.image"
   :default-startup-file-name "level-1.la64fsl"
   :subdirs '("ccl:level-0;ARM64;")
   :compiler-target-name :linuxarm64
   ;; x8664's values.  The arch already pins the x8664-style static space
   ;; (canonical-nil-value = #x13000 + fulltag-nil, and the
   ;; nilreg-relative-symbol scheme packs symbols above NIL exactly as on
   ;; x8664), so xdump/xx8664-fasload.lisp is the donor: static space at
   ;; #x12000 = (+ (ash 1 16) (ash 2 12)), one page-filling ivector, then
   ;; the two-cons NIL pun at #x13000.
   :image-base-address #x300000000000
   :nil-relative-symbols arm64::*nilreg-relative-symbols*
   :static-space-init-function 'arm64-initialize-static-space
   :purespace-reserve (ash 128 30)
   :static-space-address (+ (ash 1 16) (ash 2 12))
))

(add-xload-backend *linuxarm64-xload-backend*)

(defparameter *darwinarm64-xload-backend*
  (make-backend-xload-info
   :name :darwinarm64
   :macro-apply-code-function 'arm64-fixup-macro-apply-code
   :closure-trampoline-code *arm64-closure-trampoline-code*
   :udf-code *arm64-udf-code*
   :default-image-name "ccl:ccl;arm64-boot.image"
   :default-startup-file-name "level-1.da64fsl"
   :subdirs '("ccl:level-0;ARM64;")
   :compiler-target-name :darwinarm64
   :image-base-address 0 ;xxx
   :nil-relative-symbols arm64::*nilreg-relative-symbols*
   :static-space-init-function 'arm64-initialize-static-space
   :purespace-reserve (ash 128 30)
   :static-space-address 0 ;xxx
))

(add-xload-backend *darwinarm64-xload-backend*)

#+linuxarm-target
(progn
(setq *xload-default-backend* *linuxarm-xload-backend*)
)

#+linuxarm64-target
(progn
(setq *xload-default-backend* *linuxarm64-xload-backend*)
)






