;;;
;;; Copyright 2009 Clozure Associates
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

;;; Cygwin compiler returns small structures in registers
;;; (just like on Darwin, apparently).
(defun win32::record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
	     (not (typep rtype 'unsigned-byte))
	     (not (member rtype *foreign-representation-type-keywords*
			  :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
		    rtype
		    (parse-foreign-type rtype)))
	   (nbits (ensure-foreign-type-bits ftype)))
      (not (member nbits '(8 16 32 64))))))

(defun win32::expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (x8632::expand-ff-call callform args :arg-coerce arg-coerce :result-coerce result-coerce))

(defun win32::generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (x8632::generate-callback-bindings stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name))

(defun win32::generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (x8632::generate-callback-return-value stack-ptr fp-args-ptr result return-type struct-return-arg))

