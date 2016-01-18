;;;-*- Mode: Lisp; Package: CCL -*-
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

;;; AndroidARM:
;;; Structures whose size is 64 bits are passed by value; the caller
;;; instead passes a pointer to the structure or a copy of it.
;;; Structures whose size is <= 32 bits are returned as scalars.
(defun arm-android::record-type-returns-structure-as-first-arg (rtype)
  (arm::eabi-record-type-returns-structure-as-first-arg rtype))


(defun arm-android::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (arm::eabi-expand-ff-call callform args :arg-coerce arg-coerce :result-coerce result-coerce))

;;; Return 7 values:
;;; A list of RLET bindings
;;; A list of LET* bindings
;;; A list of DYNAMIC-EXTENT declarations for the LET* bindings
;;; A list of initializaton forms for (some) structure args
;;; A FOREIGN-TYPE representing the "actual" return type.
;;; A form which can be used to initialize FP-ARGS-PTR, relative
;;;  to STACK-PTR.  (This is unused on androidarm.)
;;; The byte offset of the foreign return address, relative to STACK-PTR
(defun arm-android::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (arm::eabi-generate-callback-bindings stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name))


(defun arm-android::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (arm::eabi-generate-callback-return-value stack-ptr fp-args-ptr result return-type struct-return-arg))
      
                 
