;;;; -*- Mode: Lisp; Package: CCL -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

;;; Load the linuxarm64 backend into a host lisp, so that
;;;
;;;     (cross-compile-ccl :linuxarm64 t)
;;;     (cross-xload-level-0 :linuxarm64 :force)
;;;
;;; work.  This is the linuxarm64 twin of tools/xarm64.lisp, which
;;; does the same job for darwinarm64.
;;;
;;; You also need the interface database in ccl:arm64-headers; (sold
;;; separately)

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
