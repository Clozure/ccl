;;;; -*- Mode: Lisp; Package: CCL -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(in-package "CCL")

;;; Darwin arm64 FTD entry points.  The AAPCS64 record classification,
;;; ff-call expansion, and callback glue are OS-independent and live in
;;; lib/ffi-arm64.lisp; delegate to them.
;;;
;;; Darwin deviations from standard AAPCS64
;;; (https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms)
;;; are handled downstream, keyed on the target OS:
;;;   * variadic args are stack-only: %external-call-expander emits a
;;;     :variadic sentinel at the CDB :void boundary and
;;;     arm642-aapcs64-ff-call forces following args onto 8-byte slots
;;;   * non-variadic stack overflow uses natural-size packing

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "FFI-ARM64"))

(defun arm64-darwin::record-type-returns-structure-as-first-arg (rtype)
  (arm64::record-type-returns-structure-as-first-arg rtype))

(defun arm64-darwin::expand-ff-call (callform args
                                     &key
                                       (arg-coerce
                                        #'null-coerce-foreign-arg)
                                       (result-coerce
                                        #'null-coerce-foreign-result))
  (arm64::expand-ff-call callform args
                         :arg-coerce arg-coerce
                         :result-coerce result-coerce))

(defun arm64-darwin::generate-callback-bindings (stack-ptr fp-args-ptr
                                                 argvars argspecs result-spec
                                                 struct-result-name)
  (arm64::generate-callback-bindings
   stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name))

(defun arm64-darwin::generate-callback-return-value (stack-ptr fp-args-ptr
                                                     result return-type
                                                     struct-return-arg)
  (arm64::generate-callback-return-value
   stack-ptr fp-args-ptr result return-type struct-return-arg))
