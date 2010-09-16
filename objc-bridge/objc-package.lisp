;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2007-2009 Clozure Associates and contributors.
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
;;;

(in-package "CCL")

;;; All class names and instance variable names are interned in the NS package
;;; Force all symbols interned in the NS package to be external

(defpackage "NS"
  (:use)
  (:export "+CGFLOAT-ZERO+" "CGFLOAT" "CG-FLOAT"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (package-force-export "NS"))

;;; ObjC function names (as produced by #/) are interned in NSF.
(defpackage "NEXTSTEP-FUNCTIONS"
  (:use)
  (:nicknames "NSFUN"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (package-force-export "NSFUN"))

(defpackage "OBJC"
  (:use)
  (:export "OBJC-OBJECT" "OBJC-CLASS-OBJECT" "OBJC-CLASS" "OBJC-METACLASS"
           "@CLASS" "@SELECTOR" "MAKE-OBJC-INSTANCE" "RETURNING-FOREIGN-STRUCT"
           "DEFMETHOD" "SLET" "SEND" "SEND/STRET" "SEND-SUPER" "SEND-SUPER/STRET"
           "DEFINE-OBJC-METHOD" "DEFINE-OBJC-CLASS-METHOD"
           "OBJC-MESSAGE-SEND" "OBJC-MESSAGE-SEND-STRET"
           "OBJC-MESSAGE-SEND-SUPER" "OBJC-MESSAGE-SEND-SUPER-STRET"
           "LOAD-FRAMEWORK" "*OBJC-DESCRIPTION-MAX-LENGTH*"
           "REMOVE-LISP-SLOTS" "WITH-AUTORELEASE-POOL"
           "MAKE-NSSTRING" "LISP-STRING-FROM-NSSTRING"
           "WITH-AUTORELEASED-NSSTRINGS"
           ))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(objc:@class objc:@selector objc:make-objc-instance
            objc:send objc:send/stret objc:send-super objc:send-super/stret
            ns:+cgfloat-zero+ ns:cgfloat ns:cg-float
            objc:define-objc-method objc:define-objc-class-method
            objc:objc-message-send objc:objc-message-send-stret
            objc:objc-message-send-super objc:objc-message-send-super-stret
            objc:*objc-description-max-length* objc:with-autorelease-pool
            objc:lisp-string-from-nsstring objc:with-autoreleased-nsstrings)
          "CCL"))

(provide "OBJC-PACKAGE")
