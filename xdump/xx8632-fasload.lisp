;;;-*- Mode: Lisp; Package: CCL -*-

;;; Copyright 2009 Clozure Associates
;;; This file is part of Clozure CL.  
;;;
;;; Clozure CL is licensed under the terms of the Lisp Lesser GNU
;;; Public License , known as the LLGPL and distributed with Clozure
;;; CL as the file "LICENSE".  The LLGPL consists of a preamble and
;;; the LGPL, which is distributed with Clozure CL as the file "LGPL".
;;; Where these conflict, the preamble takes precedence.
;;;
;;; Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;; The LLGPL is also available online at
;;; http://opensource.franz.com/preamble.html

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "FASLENV" "ccl:xdump;faslenv")
  (require "X86-LAP"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "XFASLOAD" "ccl:xdump;xfasload"))

(defparameter *x8632-macro-apply-code*
  #xc9cd0000)	    ;uuo-error-call-macro-or-special-operator

(defun x8632-fixup-macro-apply-code ()
  *x8632-macro-apply-code*)

;;; For now, do this with a UUO so that the kernel can catch it.
(defparameter *x8632-udf-code*
  #xc7cd0000)			;uuo-error-udf-call

(defun x8632-initialize-static-space ()
  (xload-make-ivector *xload-static-space*
                      (xload-target-subtype :unsigned-32-bit-vector)
                      (1- (/ 4096 4)))
  (xload-make-cons *xload-target-nil* *xload-target-nil* *xload-static-space*))

(defparameter *x8632-darwin-xload-backend*
  (make-backend-xload-info
   :name  :darwinx8632
   :macro-apply-code-function 'x8632-fixup-macro-apply-code
   :closure-trampoline-code nil
   :udf-code *x8632-udf-code*
   :default-image-name "ccl:ccl;x86-boot32.image"
   :default-startup-file-name "level-1.dx32fsl"
   :subdirs '("ccl:level-0;X86;X8632;" "ccl:level-0;X86;")
   :compiler-target-name :darwinx8632
   :image-base-address #x04000000
   :nil-relative-symbols x86::*x86-nil-relative-symbols*
   :static-space-init-function 'x8632-initialize-static-space
   :purespace-reserve (ash 128 20)
   :static-space-address (+ (ash 1 16) (ash 2 12))
))

(add-xload-backend *x8632-darwin-xload-backend*)

(defparameter *x8632-linux-xload-backend*
  (make-backend-xload-info
   :name  :linuxx8632
   :macro-apply-code-function 'x8632-fixup-macro-apply-code
   :closure-trampoline-code nil
   :udf-code *x8632-udf-code*
   :default-image-name "ccl:ccl;x86-boot32"
   :default-startup-file-name "level-1.lx32fsl"
   :subdirs '("ccl:level-0;X86;X8632;" "ccl:level-0;X86;")
   :compiler-target-name :linuxx8632
   :image-base-address #x10000000
   :nil-relative-symbols x86::*x86-nil-relative-symbols*
   :static-space-init-function 'x8632-initialize-static-space
   :purespace-reserve (ash 128 20)
   :static-space-address (+ (ash 1 16) (ash 2 12))
))

(add-xload-backend *x8632-linux-xload-backend*)

(defparameter *x8632-windows-xload-backend*
  (make-backend-xload-info
   :name  :win32
   :macro-apply-code-function 'x8632-fixup-macro-apply-code
   :closure-trampoline-code nil
   :udf-code *x8632-udf-code*
   :default-image-name "ccl:ccl;wx86-boot32.image"
   :default-startup-file-name "level-1.wx32fsl"
   :subdirs '("ccl:level-0;X86;X8632;" "ccl:level-0;X86;")
   :compiler-target-name :win32
   :image-base-address #x04000000
   :nil-relative-symbols x86::*x86-nil-relative-symbols*
   :static-space-init-function 'x8632-initialize-static-space
   :purespace-reserve (ash 128 20)
   :static-space-address (+ (ash 1 16) (ash 2 12))
))

(add-xload-backend *x8632-windows-xload-backend*)

(defparameter *x8632-solaris-xload-backend*
  (make-backend-xload-info
   :name  :solarisx8632
   :macro-apply-code-function 'x8632-fixup-macro-apply-code
   :closure-trampoline-code nil
   :udf-code *x8632-udf-code*
   :default-image-name "ccl:ccl;sx86-boot32"
   :default-startup-file-name "level-1.sx32fsl"
   :subdirs '("ccl:level-0;X86;X8632;" "ccl:level-0;X86;")
   :compiler-target-name :solarisx8632
   :image-base-address #x10000000
   :nil-relative-symbols x86::*x86-nil-relative-symbols*
   :static-space-init-function 'x8632-initialize-static-space
   :purespace-reserve (ash 128 20)
   :static-space-address (+ (ash 1 16) (ash 2 12))
))

(add-xload-backend *x8632-solaris-xload-backend*)

(defparameter *x8632-freebsd-xload-backend*
  (make-backend-xload-info
   :name  :freebsdx8632
   :macro-apply-code-function 'x8632-fixup-macro-apply-code
   :closure-trampoline-code nil
   :udf-code *x8632-udf-code*
   :default-image-name "ccl:ccl;fx86-boot32"
   :default-startup-file-name "level-1.fx32fsl"
   :subdirs '("ccl:level-0;X86;X8632;" "ccl:level-0;X86;")
   :compiler-target-name :freebsdx8632
   :image-base-address #x30000000
   :nil-relative-symbols x86::*x86-nil-relative-symbols*
   :static-space-init-function 'x8632-initialize-static-space
   :purespace-reserve (ash 128 20)
   :static-space-address (+ (ash 1 16) (ash 2 12))
))

(add-xload-backend *x8632-freebsd-xload-backend*)

#+x8632-target
(progn
  #+darwin-target
  (setq *xload-default-backend* *x8632-darwin-xload-backend*)
  #+linux-target
  (setq *xload-default-backend* *x8632-linux-xload-backend*)
  #+windows-target
  (setq *xload-default-backend* *x8632-windows-xload-backend*)
  #+solaris-target
  (setq *xload-default-backend* *x8632-solaris-xload-backend*)
  #+freebsd-target
  (setq *xload-default-backend* *x8632-freebsd-xload-backend*)
  )
