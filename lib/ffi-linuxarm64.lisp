;;;; -*- Mode: Lisp; Package: CCL -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

(in-package "CCL")

;;; Linux arm64 FTD entry points.  The AAPCS64 record classification,
;;; ff-call expansion, and callback glue are OS-independent and live in
;;; lib/ffi-arm64.lisp; delegate to them.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "FFI-ARM64"))

(defun arm64-linux::record-type-returns-structure-as-first-arg (rtype)
  (arm64::record-type-returns-structure-as-first-arg rtype))

(defun arm64-linux::expand-ff-call (callform args
                                    &key
                                      (arg-coerce
                                       #'null-coerce-foreign-arg)
                                      (result-coerce
                                       #'null-coerce-foreign-result))
  (arm64::expand-ff-call callform args
                         :arg-coerce arg-coerce
                         :result-coerce result-coerce))

(defun arm64-linux::generate-callback-bindings (stack-ptr fp-args-ptr
                                                argvars argspecs result-spec
                                                struct-result-name)
  (arm64::generate-callback-bindings
   stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name))

(defun arm64-linux::generate-callback-return-value (stack-ptr fp-args-ptr
                                                    result return-type
                                                    struct-return-arg)
  (arm64::generate-callback-return-value
   stack-ptr fp-args-ptr result return-type struct-return-arg))
