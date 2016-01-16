;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2001 Clozure Associates
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
  (require "X86-LAP"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "XFASLOAD" "ccl:xdump;xfasload"))

(defun xload-x8664-lap-code (instructions)
  (let* ((f (%define-x86-lap-function nil instructions)))
    (if (= (typecode f) target::subtag-xfunction)
      (uvref f 0)
      f)))

(defparameter *x8664-macro-apply-code*
  #xc9cd0000000000)


(defun x8664-fixup-macro-apply-code ()
  *x8664-macro-apply-code*)


(defparameter *x8664-closure-trampoline-code*
  (xload-x8664-lap-code '((jmp-subprim  .SPcall-closure))))



;;; For now, do this with a UUO so that the kernel can catch it.
(defparameter *x8664-udf-code*
  #xc7cd0000000000)


(defun x8664-initialize-static-space ()
  (xload-make-ivector *xload-static-space*
                      (xload-target-subtype :unsigned-64-bit-vector) 
                      (1- (/ 4096 8)))
  (xload-make-cons *xload-target-nil* 0 *xload-static-space*)
  (xload-make-cons 0 *xload-target-nil* *xload-static-space*))
                      

(defparameter *x8664-linux-xload-backend*
  (make-backend-xload-info
   :name  :linuxx8664
   :macro-apply-code-function 'x8664-fixup-macro-apply-code
   :closure-trampoline-code *x8664-closure-trampoline-code*
   :udf-code *x8664-udf-code*
   :default-image-name "ccl:ccl;x86-boot64"
   :default-startup-file-name "level-1.lx64fsl"
   :subdirs '("ccl:level-0;X86;X8664;" "ccl:level-0;X86;")
   :compiler-target-name :linuxx8664
   :image-base-address #x300000000000
   :nil-relative-symbols x86::*x86-nil-relative-symbols*
   :static-space-init-function 'x8664-initialize-static-space
   :purespace-reserve (ash 128 30)
   :static-space-address (+ (ash 1 16) (ash 2 12))
))


(add-xload-backend *x8664-linux-xload-backend*)


(defparameter *x8664-freebsd-xload-backend*
  (make-backend-xload-info
   :name  :freebsdx8664
   :macro-apply-code-function 'x8664-fixup-macro-apply-code
   :closure-trampoline-code *x8664-closure-trampoline-code*
   :udf-code *x8664-udf-code*
   :default-image-name "ccl:ccl;fx86-boot64"
   :default-startup-file-name "level-1.fx64fsl"
   :subdirs '("ccl:level-0;X86;X8664;" "ccl:level-0;X86;")
   :compiler-target-name :freebsdx8664
   :image-base-address #x300000000000
   :nil-relative-symbols x86::*x86-nil-relative-symbols*
   :static-space-init-function 'x8664-initialize-static-space
   :purespace-reserve (ash 128 30)
   :static-space-address (+ (ash 1 16) (ash 2 12))
))

(add-xload-backend *x8664-freebsd-xload-backend*)

(defparameter *x8664-darwin-xload-backend*
  (make-backend-xload-info
   :name  :darwinx8664
   :macro-apply-code-function 'x8664-fixup-macro-apply-code
   :closure-trampoline-code *x8664-closure-trampoline-code*
   :udf-code *x8664-udf-code*
   :default-image-name "ccl:ccl;x86-boot64.image"
   :default-startup-file-name "level-1.dx64fsl"
   :subdirs '("ccl:level-0;X86;X8664;" "ccl:level-0;X86;")
   :compiler-target-name :darwinx8664
   :image-base-address #x300000000000
   :nil-relative-symbols x86::*x86-nil-relative-symbols*
   :static-space-init-function 'x8664-initialize-static-space
   :purespace-reserve (ash 128 30)
   :static-space-address (+ (ash 1 16) (ash 2 12))
))

(add-xload-backend *x8664-darwin-xload-backend*)

(defparameter *x8664-solaris-xload-backend*
  (make-backend-xload-info
   :name  :solarisx8664
   :macro-apply-code-function 'x8664-fixup-macro-apply-code
   :closure-trampoline-code *x8664-closure-trampoline-code*
   :udf-code *x8664-udf-code*
   :default-image-name "ccl:ccl;sx86-boot64"
   :default-startup-file-name "level-1.sx64fsl"
   :subdirs '("ccl:level-0;X86;X8664;" "ccl:level-0;X86;")
   :compiler-target-name :solarisx8664
   :image-base-address #x300000000000
   :nil-relative-symbols x86::*x86-nil-relative-symbols*
   :static-space-init-function 'x8664-initialize-static-space
   :purespace-reserve (ash 128 30)
   :static-space-address (+ (ash 1 16) (ash 2 12))
))

(add-xload-backend *x8664-solaris-xload-backend*)

(defparameter *x8664-windows-xload-backend*
  (make-backend-xload-info
   :name  :win64
   :macro-apply-code-function 'x8664-fixup-macro-apply-code
   :closure-trampoline-code *x8664-closure-trampoline-code*
   :udf-code *x8664-udf-code*
   :default-image-name "ccl:ccl;wx86-boot64.image"
   :default-startup-file-name "level-1.wx64fsl"
   :subdirs '("ccl:level-0;X86;X8664;" "ccl:level-0;X86;")
   :compiler-target-name :win64
   :image-base-address #x100000000
   :nil-relative-symbols x86::*x86-nil-relative-symbols*
   :static-space-init-function 'x8664-initialize-static-space
   :purespace-reserve (ash 128 30)
   :static-space-address (+ (ash 1 16) (ash 2 12))
))

(add-xload-backend *x8664-windows-xload-backend*)

#+x8664-target
(progn
  #+linux-target
  (setq *xload-default-backend* *x8664-linux-xload-backend*)
  #+freebsd-target
  (setq *xload-default-backend* *x8664-freebsd-xload-backend*)
  #+darwin-target
  (setq *xload-default-backend* *x8664-darwin-xload-backend*)
  #+solaris-target
  (setq *xload-default-backend* *x8664-solaris-xload-backend*)
  #+windows-target
  (setq *xload-default-backend* *x8664-windows-xload-backend*))




