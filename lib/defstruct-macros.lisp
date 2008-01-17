;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

; This file is needed to compile DEFSTRUCT and anything accessing defstruct
; data structures.

(in-package "CCL")

(eval-when (:execute :compile-toplevel)
  (require "LISPEQU"))

(defconstant $struct-r/o 24)             ; Read-only bit in refinfo fixnum
(defconstant $struct-inherited 25)		; Struct slot is  inherited.


(defconstant $defstruct-nth 0)   ; Anything that won't conflict with array types...
(defconstant $defstruct-struct 8)
(defconstant $defstruct-simple-vector 16)


(defmacro ssd-name (ssd) `(car ,ssd))
;(defmacro ssd-type (ssd) (declare (ignore ssd)) t)
(defmacro ssd-initform (ssd) `(cadr ,ssd))
;(defmacro ssd-refinfo (ssd) `(cddr ,ssd))

(defmacro ssd-update-refinfo ((ssd refinfo-var) new-refinfo-form)
  (check-type refinfo-var symbol)
  (let ((refinfo-cons (gensym)))
    `(let* ((,refinfo-cons (cdr ,ssd))
            (,refinfo-var (cdr ,refinfo-cons)))
       (when (consp ,refinfo-var)
         (setq ,refinfo-cons ,refinfo-var)
         (setq ,refinfo-var (%cdr ,refinfo-cons)))
       (%rplacd ,refinfo-cons ,new-refinfo-form))))

(defmacro refinfo-offset (refinfo) `(%ilogand2 #xFFFF ,refinfo))
(defmacro refinfo-r/o (refinfo) `(%ilogbitp $struct-r/o ,refinfo))
(defmacro refinfo-reftype (refinfo) `(%ilogand2 #xFF (%ilsr 16 ,refinfo)))

(defmacro ssd-offset (ssd) `(refinfo-offset (ssd-refinfo ,ssd)))
(defmacro ssd-r/o (ssd) `(refinfo-r/o (ssd-refinfo ,ssd)))
(defmacro ssd-reftype (ssd) `(refinfo-reftype (ssd-refinfo ,ssd)))

(defmacro ssd-set-initform (ssd value) `(rplaca (cdr ,ssd) ,value))

#| these are fns now
(defmacro ssd-set-reftype (ssd reftype)      ;-> ssd multiply evaluated
  `(rplacd (cdr ,ssd) (%ilogior2 (%ilogand2 #x100FFFF (cdr (%cdr ,ssd)))
                                 (%ilsl 16 ,reftype))))

(defmacro ssd-set-r/o (ssd)                  ;-> ssd multiply evaluated
  `(rplacd (cdr ,ssd) (%ilogior2 #x1000000 (cdr (%cdr ,ssd)))))

(defmacro copy-ssd (ssd)                     ;-> ssd multiply evaluated
  `(list* (car ,ssd) (car (%cdr ,ssd)) (%cddr ,ssd)))
|#

(defmacro named-ssd (name slot-list) `(assq ,name ,slot-list))

(defmacro sd-name (sd) `(car (svref ,sd 2)))
(defmacro sd-type (sd) `(svref ,sd 0))
(defmacro sd-slots (sd) `(svref ,sd 1))
(defmacro sd-superclasses (sd) `(svref ,sd 2))
(defmacro sd-size (sd) `(svref ,sd 3))
(defmacro sd-constructor (sd) `(svref ,sd 4))
(defmacro sd-print-function (sd) `(svref ,sd 5))
(defmacro sd-set-print-function (sd value) `(svset ,sd 5 ,value))
(defmacro sd-refnames (sd) `(svref ,sd 6))

(defmacro struct-name (struct) `(car (uvref ,struct 0)))
(defmacro struct-def (struct) `(gethash (car (uvref ,struct 0)) %defstructs%))

;Can use this to let the printer print with print-function, reader read with
;constructor and slot-names, inspector inspect with slot-names.
;Everything else you have to arrange yourself.
#+ignore
(defmacro pretend-i-am-a-structure (name constructor print-function &rest slot-names)
  (let ((slots slot-names) (offset 1) (supers (list name)))
    (while slots
      (%rplaca slots (make-ssd (%car slots) () offset t))
      (ssd-set-reftype (%car slots) $v_struct)
      (setq slots (%cdr slots) offset (1+ offset)))
    (push (make-ssd 0 `',supers 0 t) slot-names)
    (ssd-set-reftype (%car slot-names) $v_struct)
    `(puthash ',name %defstructs%
          '#(internal-structure  ;Make structure-class-p false.
             ,slot-names
             ,supers
             ,offset
             ,constructor
             ,print-function
             nil))))

(provide 'defstruct-macros)

; End of defstruct-macros.lisp
