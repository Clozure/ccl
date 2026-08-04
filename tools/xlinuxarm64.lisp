;;;; -*- Mode: Lisp; Package: CCL -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

;;; Load the linuxarm64 backend into a host lisp, so that
;;;
;;;     (cross-compile-ccl :linuxarm64 t)
;;;     (cross-xload-level-0 :linuxarm64 :force)
;;;
;;; work.  This is the linuxarm64 twin of tools/xarm64.lisp, which does the same
;;; job for darwinarm64.
;;;
;;; Without it, both forms fail before doing any work: SETUP-ARM64-FTD is called
;;; only from tools/xarm64.lisp, which passes *DARWINARM64-BACKEND*, so
;;; *LINUXARM64-BACKEND* has no foreign type data and
;;; %WITH-CROSS-COMPILATION-TARGET signals
;;;
;;;     No foreign type data loaded for target named :LINUXARM64
;;;
;;; Every module named below is already registered: ffi-linuxarm64 in
;;; lib/systems.lisp, and (:linuxarm64 'ffi-linuxarm64) in lib/compile-ccl.lisp.
;;; The ARM64 FTD in compiler/ARM64/arm64-backend.lisp gives its interface
;;; package as "ARM64-LINUX".

(in-package "CCL")

(defpackage "ARM64-LINUX" (:use))

(defun load-linuxarm64-backend ()
  (in-development-mode
    (load "ccl:lib;systems.lisp")
    (load "ccl:lib;compile-ccl"))
  (update-modules '(arm64-arch arm64-asm arm64-lap arm64-backend
                    arm64-vinsns arm642)
                  t)
  (setup-arm64-ftd *linuxarm64-backend*)
  (update-modules '(arm64-lapmacros arm64-disassemble ffi-linuxarm64) t)
  (update-modules *arm64-xload-modules* t))

(load-linuxarm64-backend)
