;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 2010 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

;;; NetBSD/ARM uses the ARM EABI calling convention.
(defun arm-netbsd::record-type-returns-structure-as-first-arg (rtype)
  (arm::eabi-record-type-returns-structure-as-first-arg rtype))

(defun arm-netbsd::expand-ff-call
    (callform args &key (arg-coerce #'null-coerce-foreign-arg)
                        (result-coerce #'null-coerce-foreign-result))
  (arm::eabi-expand-ff-call callform args
                            :arg-coerce arg-coerce
                            :result-coerce result-coerce))

(defun arm-netbsd::generate-callback-bindings
    (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (arm::eabi-generate-callback-bindings stack-ptr fp-args-ptr argvars
                                         argspecs result-spec
                                         struct-result-name))

(defun arm-netbsd::generate-callback-return-value
    (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (arm::eabi-generate-callback-return-value stack-ptr fp-args-ptr result
                                             return-type struct-return-arg))
