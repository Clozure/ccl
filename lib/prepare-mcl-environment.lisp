;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
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

;; prepare-mcl-environment.lisp
;; Load this into a PPCCL to make it into an MCL-PPC for shipping
;; Sort of.

(in-package "CCL")

(defun %reset-outermost-binding (symbol value)
  (let* ((symvector (symptr->symvector symbol))
         (idx (%svref symvector target::symbol.binding-index-cell))
         (marker (%no-thread-local-binding-marker)))
    (if (> idx 0)
      (do-db-links (db var)
        (when (eq var idx)
          (let* ((oldval (%fixnum-ref db (* 2 target::node-size))))
            (unless (eq oldval marker)
              (setf (%fixnum-ref db (* 2 target::node-size)) value))))))
    (setf (uvref symvector target::symbol.vcell-cell) value)))

(defun freeze-current-definitions ()
  ;; Set the frozen bits so that redefine-kernel-function
  ;; will error if a builtin function is redefined.
  (do-all-symbols (s)
    (when (fboundp s)
      (%symbol-bits s (bitset $sym_fbit_frozen (%symbol-bits s)))))
  ;; Force an error if a kernel method is redefined.
  (make-all-methods-kernel))

(defun thaw-current-definitions ()
  ;; Clear the frozen bits on all fboundp symbols
  (do-all-symbols (s)
    (when (fboundp s)
      (%symbol-bits s (bitclr $sym_fbit_frozen (%symbol-bits s)))))
  ;; Allow redefinition of kernel methods.
  (make-all-methods-non-kernel))

(defun set-user-environment (&optional (freeze-definitions nil))
  "Arrange that the outermost special bindings of *PACKAGE* and
*WARN-IF-REDEFINE-KERNEL* restore values of the CL-USER package and T
respectively, and set *CCL-SAVE-SOURCE-LOCATIONS* to :NO-TEXT.
If the optional argument is true, marks all globally defined
functions and methods as being predefined (this is a fairly
expensive operation.)"
  (when freeze-definitions
    (freeze-current-definitions))
  ;; enable redefine-kernel-function's error checking
  (%reset-outermost-binding '*warn-if-redefine-kernel* t)
  ;; Set the top-level *package* to the CL-USER package
  (%reset-outermost-binding '*package* (find-package "CL-USER"))
  (setq *ccl-save-source-locations* :NO-TEXT))

(defun set-development-environment (&optional (thaw-definitions nil))
  "Arrange that the outermost special bindings of *PACKAGE* and
*WARN-IF-REDEFINE-KERNEL* restore values of the CCL package and NIL
respectively, and set *ccl-save-source-locations* to T. If the
optional argument is true, mark all globally defined functions and
methods as being not predefined (this is a fairly expensive operation.)"
  (when thaw-definitions
    (thaw-current-definitions))
  ;; enable redefine-kernel-function's error checking
  (%reset-outermost-binding '*warn-if-redefine-kernel* nil)
  ;; Set the top-level *package* to the CCL package
  (%reset-outermost-binding '*package* (find-package "CCL"))
  (setq *ccl-save-source-locations* T))
  


(defmacro in-development-mode (&body body)
  `(let* ((*package* (find-package "CCL"))
	  (*warn-if-redefine-kernel* nil))
    ,@body))




