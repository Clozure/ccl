;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2006-2009 Clozure Associates and contributors
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

#+x8664-target
(progn

;;; It's easier to keep this is LAP; we want to play around with its
;;; constants.


;;; This just maps a SLOT-ID to a SLOT-DEFINITION or NIL.
;;; The map is a vector of (UNSIGNED-BYTE 8); this should
;;; be used when there are fewer than 255 slots in the class.
(defx86lapfunction %small-map-slot-id-lookup ((slot-id arg_z))
  (movq (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index arg_x)
  (vector-length temp1 imm0)
  (xorl (%l imm1) (%l imm1))
  (rcmpq (% arg_x) (% imm0))
  (movq (@ 'table (% fn)) (% temp0))
  (ja @have-table-index)
  (movq (% arg_x) (% imm1))
  (shrq ($ x8664::word-shift) (% imm1))
  (movzbl (@ x8664::misc-data-offset (% temp1) (% imm1)) (%l imm1))
  @have-table-index
  (movq (@ x8664::misc-data-offset (% temp0) (% imm1) 8) (% arg_z))
  (single-value-return))

;;; The same idea, only the map is a vector of (UNSIGNED-BYTE 32).
(defx86lapfunction %large-map-slot-id-lookup ((slot-id arg_z))
  (movq (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index arg_x)
  (vector-length temp1 imm0)
  (xorl (%l imm1) (%l imm1))
  (rcmpq (% arg_x) (% imm0))
  (movq (@ 'table (% fn)) (% temp0))
  (ja @have-table-index)
  (movq (% arg_x) (% imm1))
  (shrq ($ 1) (% imm1))
  (movl (@ x8664::misc-data-offset (% temp1) (% imm1)) (%l imm1))
  @have-table-index
  (movq (@ x8664::misc-data-offset (% temp0) (% imm1) 8) (% arg_z))
  (single-value-return))


(defx86lapfunction %small-slot-id-value ((instance arg_y) (slot-id arg_z))
  (movq (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index arg_x)
  (vector-length temp1 imm0)
  (xorl (%l imm1) (%l imm1))
  (rcmpq (% arg_x) (% imm0))
  (movq (@ 'table (% fn)) (% temp0))
  (ja @missing)
  (movq (% arg_x) (% imm1))
  (shrq ($ x8664::word-shift) (% imm1))
  (movzbl (@ x8664::misc-data-offset (% temp1) (% imm1)) (%l imm1))
  (testl (%l imm1) (%l imm1))
  (je @missing)
  (movq (@ x8664::misc-data-offset (% temp0) (% imm1) 8) (% arg_z))
  (movq (@ 'class (% fn)) (% arg_x))
  (set-nargs 3)
  (jmp (@ '%maybe-std-slot-value-using-class (% fn)))
  @missing                              ; (%slot-id-ref-missing instance id)
  (set-nargs 2)
  (jmp (@'%slot-id-ref-missing (% fn))))

(defx86lapfunction %large-slot-id-value ((instance arg_y) (slot-id arg_z))  
  (movq (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index arg_x)
  (vector-length temp1 imm0)
  (xorl (%l imm1) (%l imm1))
  (rcmpq (% arg_x) (% imm0))
  (movq (@ 'table (% fn)) (% temp0))
  (ja @missing)
  (movq (% arg_x) (% imm1))
  (shrq ($ 1) (% imm1))
  (movl (@ x8664::misc-data-offset (% temp1) (% imm1)) (%l imm1))
  (testl (%l imm1) (%l imm1))
  (je @missing)
  (movq (@ x8664::misc-data-offset (% temp0) (% imm1) 8) (% arg_z))
  (movq (@ 'class (% fn)) (% arg_x))
  (set-nargs 3)
  (jmp (@ '%maybe-std-slot-value-using-class (% fn)))
  @missing                              ; (%slot-id-ref-missing instance id)
  (set-nargs 2)
  (jmp (@'%slot-id-ref-missing (% fn))))

  
(defx86lapfunction %small-set-slot-id-value ((instance arg_x)
                                             (slot-id arg_y)
                                             (new-value arg_z))
  (movq (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index imm1)
  (vector-length temp1 imm0)
  (rcmpq (% imm1) (% imm0))
  (movq (@ 'table (% fn)) (% temp0))
  (ja @missing)
  (shrq ($ x8664::word-shift) (% rdx))
  (movzbl (@ x8664::misc-data-offset (% temp1) (% imm1)) (%l imm1))
  (testl (%l imm1) (%l imm1))
  (je @missing)
  (popq (% ra0))
  (pushq ($ 0))                         ; reserve frame
  (pushq ($ 0))
  (pushq (@ 'class (% fn)))
  (movq (@ x8664::misc-data-offset (% temp0) (% imm1) 8) (% arg_y))
  (set-nargs 4)
  (pushq (% ra0))
  (jmp (@ '%maybe-std-setf-slot-value-using-class (% fn)))
  @missing                              ; (%slot-id-set-missing instance id new-value)
  (set-nargs 3)
  (jmp (@ '%slot-id-set-missing (% fn))))


(defx86lapfunction %large-set-slot-id-value ((instance arg_x)
                                             (slot-id arg_y)
                                             (new-value arg_z))
  (movq (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index imm1)
  (vector-length temp1 imm0)
  (rcmpq (% imm1) (% imm0))
  (movq (@ 'table (% fn)) (% temp0))
  (ja @missing)
  (shrq ($ 1) (% rdx))
  (movl (@ x8664::misc-data-offset (% temp1) (% imm1)) (%l imm1))
  (testl (%l imm1) (%l imm1))
  (je @missing)
  (popq (% ra0))
  (pushq ($ 0))                         ; reserve frame
  (pushq ($ 0))
  (pushq (@ 'class (% fn)))
  (pushq (% ra0))
  (movq (@ x8664::misc-data-offset (% temp0) (% imm1) 8) (% arg_y))
  (set-nargs 4)
  (jmp (@ '%maybe-std-setf-slot-value-using-class (% fn)))
  @missing                              ; (%slot-id-set-missing instance id new-value)
  (set-nargs 3)
  (jmp (@'%slot-id-ref-missing (% fn))))


;;; All of the generic function trampoline functions have to be
;;; exactly the same size (x8664::gf-code-size) in words.  The
;;; largest of these - the general-case *GF-PROTO* - is currently
;;; "really" a little under 15 words, so X8664::GF-CODE-SIZE is
;;; just a little bigger than that.
(defparameter *gf-proto*
  (nfunction
   gag
   (lambda (&lap &lexpr args)
     (x86-lap-function 
      gag 
      ()
      (:fixed-constants (class-wrapper slots dispatch-table dcode hash))
      (:code-size x8664::gf-code-size)
      #+count-gf-calls
      (progn
        (lock)
        (addq ($ x8664::fixnumone) (@ 'hash (% fn))))
      (movq (@ (% rsp)) (% ra0))
      (save-frame-variable-arg-count)
      (push-argregs)
      (pushq (%q nargs))
      (movq (% rsp) (% arg_z))
      (ref-global.l ret1valaddr imm0)
      (cmpq (% ra0) (% imm0))
      (je @multiple)
      (ref-global.l lexpr-return1v ra0)
      (jmp @call)
      @multiple
      (pushq (@ (+ (target-nil-value) (x8664::%kernel-global 'lexpr-return))))
      (movq (% imm0) (% ra0))
      @call
      (push (% ra0))
      (movq (@ 'dispatch-table (% fn)) (% arg_y))
      (set-nargs 2)
      (jmp (@ 'dcode (% fn)))  ; dcode function
      ))))

;;; is a winner - saves ~15%
(defx86lapfunction gag-one-arg ((arg arg_z))
  (:fixed-constants (class-wrapper slots dispatch-table dcode hash))
  (:code-size x8664::gf-code-size)
  #+count-gf-calls
  (progn
    (lock)
    (addq ($ x8664::fixnumone) (@ 'hash (% fn))))
  (check-nargs 1)
  (movq (@ 'dispatch-table (% fn)) (% arg_y))
  (set-nargs 2)
  (jmp (@ 'dcode (% fn))))

(defx86lapfunction gag-two-arg ((arg0 arg_y) (arg1 arg_z))
  (:fixed-constants (class-wrapper slots dispatch-table dcode hash))
  (:code-size x8664::gf-code-size)
  #+count-gf-calls
  (progn
    (lock)
    (addq ($ x8664::fixnumone) (@ 'hash (% fn))))
  (check-nargs 2)
  (movq (@ 'dispatch-table (% fn)) (% arg_x))
  (set-nargs 3)
  (jmp (@ 'dcode (% fn))))


(defx86lapfunction funcallable-trampoline ()
  (:fixed-constants (class-wrapper slots dispatch-table dcode hash))
  (:code-size x8664::gf-code-size)
  (jmp (@ 'dcode (% fn))))


;;; This is in LAP so that it can reference itself in the error message.
;;; (It needs to be cloned, so %fn will be unique to each copy.)
;;; It can't work for this to reference any of its own constants.
(defx86lapfunction unset-fin-trampoline ()
  (:code-size x8664::gf-code-size)
  (save-frame-variable-arg-count)
  (call-subprim .SPheap-rest-arg)
  (pop (% arg_z))
  (movq ($ '#.$XNOFINFUNCTION) (% arg_x))
  (movq (% fn) (% arg_y))
  (set-nargs 3)
  (call-subprim .SPksignalerr)
  ;(movq ($ (target-nil-value)) (% arg_z))
  (leave)
  (single-value-return))



(defparameter *cm-proto*
  (nfunction
   gag
   (lambda (&lap &lexpr args)
     (x86-lap-function 
      gag 
      ()
      (:fixed-constants (thing dcode gf bits))
      (movq (@ (% rsp)) (% ra0))
      (save-frame-variable-arg-count)
      (push-argregs)
      (pushq (%q nargs))
      (movq (% rsp) (% arg_z))
      (ref-global ret1valaddr imm0)
      (cmpq (% ra0) (% imm0))
      (je @multiple)
      (ref-global lexpr-return1v ra0)
      (jmp @call)
      @multiple
      (pushq (@ (+ (target-nil-value) (x8664::%kernel-global 'lexpr-return))))
      (movq (% imm0) (% ra0))
      @call
      (push (% ra0))
      (movq (@ 'thing (% fn)) (% arg_y))
      (set-nargs 2)
      (jmp (@ 'dcode (% fn)))))))




) ; #+x8664-target
