;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2008-2009 Clozure Associates and contributors
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
