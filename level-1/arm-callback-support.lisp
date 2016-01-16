;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 2010 Clozure Associates
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

(defun make-callback-trampoline (index &optional info)
  (declare (ignore info))
  (let* ((p (%allocate-callback-pointer 16)))
    (macrolet ((arm-lap-word (instruction-form)
                 (uvref (uvref (compile nil `(lambda (&lap 0) (arm-lap-function () ((?? 0)) ,instruction-form))) 1) 0)))
      (setf (%get-unsigned-long p 0)
            (dpb (ldb (byte 8 0) index)
                 (byte 8 0)
                 (arm-lap-word (mov r12 (:$ ??))))
            (%get-unsigned-long p 4)
            (dpb (ldb (byte 8 8) index)
                 (byte 8 0)
                 (dpb 12 (byte 4 8)
                      (arm-lap-word (orr r12 r12  (:$ ??)))))
            (%get-unsigned-long p 8)
            (arm-lap-word (ldr pc (:@ pc (:$ -4))))
            (%get-unsigned-long p 12)
            (%lookup-subprim-address #.(subprim-name->offset '.SPeabi-callback)))
      (ff-call (%kernel-import #.arm::kernel-import-makedataexecutable) 
               :address p 
               :unsigned-fullword 16
               :void)
      p)))
                    
