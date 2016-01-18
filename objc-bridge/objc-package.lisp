;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 2007-2009 Clozure Associates
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
