;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates and contributors
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

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
      
                 
