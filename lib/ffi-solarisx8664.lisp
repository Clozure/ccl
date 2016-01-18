;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 2008-2009 Clozure Associates
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

;;; It looks like x86-64 Linux, FreeBSD, Darwin, and Solaris all share
;;; the same ABI.

(defun x86-solaris64::record-type-returns-structure-as-first-arg (rtype)
  (x8664::record-type-returns-structure-as-first-arg rtype))



(defun x86-solaris64::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (x8664::expand-ff-call callform args :arg-coerce arg-coerce :result-coerce result-coerce))
                           

(defun x86-solaris64::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-return-name)
  (x8664::generate-callback-bindings stack-ptr fp-args-ptr argvars argspecs result-spec struct-return-name))

(defun x86-solaris64::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (x8664::generate-callback-return-value stack-ptr fp-args-ptr result return-type struct-return-arg))
