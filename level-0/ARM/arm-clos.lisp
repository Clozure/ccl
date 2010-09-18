;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
;;;   Copyright (C) 1994-2001 Digitool, Inc
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

;;; It's easier to keep this is LAP; we want to play around with its
;;; constants.

;;; This just maps a SLOT-ID to a SLOT-DEFINITION or NIL.
;;; The map is a vector of (UNSIGNED-BYTE 8); this should
;;; be used when there are less than 255 slots in the class.
(defarmlapfunction %small-map-slot-id-lookup ((slot-id arg_z))
  (ldr temp1 (:@ nfn 'map))
  (svref arg_x slot-id.index slot-id)
  (getvheader imm0 temp1)
  (header-length imm1 imm0)
  (ldr temp0 (:@ nfn 'table))
  (cmp arg_x imm1)
  (mov imm0 (:lsr arg_x (:$ arm::word-shift)))
  (add imm0 imm0 (:$ arm::misc-data-offset))
  (mov imm1 (:$ arm::misc-data-offset))
  (ldrblo imm1 (:@ temp1 imm0))
  (movlo imm1 (:lsl imm1 (:$ arm::word-shift)))
  (addlo imm1 imm1 (:$ arm::misc-data-offset))
  (ldr arg_z (:@ temp0 imm1))
  (bx lr))

;;; The same idea, only the map is a vector of (UNSIGNED-BYTE 32).
(defarmlapfunction %large-map-slot-id-lookup ((slot-id arg_z))
  (ldr temp1 (:@ nfn 'map))
  (svref arg_x slot-id.index slot-id)
  (getvheader imm0 temp1)
  (header-length imm1 imm0)
  (ldr temp0 (:@ nfn 'table))
  (cmp arg_x imm1)
  (add imm0 arg_x (:$ arm::misc-data-offset))
  (mov imm1 (:$ arm::misc-data-offset))
  (ldrlo imm1 (:@ temp1 imm0))
  (addlo imm1 imm1 (:$ arm::misc-data-offset))
  (ldr arg_z (:@ temp0 imm1))
  (bx lr))

(defarmlapfunction %small-slot-id-value ((instance arg_y) (slot-id arg_z))
  (ldr temp1 (:@ nfn 'map))
  (svref arg_x slot-id.index slot-id)
  (getvheader imm0 temp1)
  (ldr temp0 (:@ nfn 'table))
  (header-length imm1 imm0)
  (cmp arg_x imm1)
  (mov imm0 (:lsr arg_x (:$ arm::word-shift)))
  (add imm0 imm0 (:$ arm::misc-data-offset))
  (bhs @missing)
  (ldrb imm1 (:@ temp1 imm0))
  (cmp imm1 (:$ 0))
  (mov imm1 (:lsl imm1 (:$ arm::word-shift)))
  (add imm1 imm1 (:$ arm::misc-data-offset))
  (beq @missing)
  (ldr arg_z (:@ temp0 imm1))
  (ldr arg_x (:@ nfn 'class))
  (ldr nfn (:@ nfn '%maybe-std-slot-value))
  (set-nargs 3)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint)))
  @missing                              ; (%slot-id-ref-missing instance id)
  (ldr nfn (:@ nfn '%slot-id-ref-missing))
  (set-nargs 2)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint))))

(defarmlapfunction %large-slot-id-value ((instance arg_y) (slot-id arg_z))
  (ldr temp1 (:@ nfn 'map))
  (svref arg_x slot-id.index slot-id)
  (getvheader imm0 temp1)
  (ldr temp0 (:@ nfn 'table))
  (header-length imm1 imm0)
  (cmp arg_x imm1)
  (movhs arg_x (:$ 0))
  (add imm0 arg_x (:$ arm::misc-data-offset))
  (ldr imm1 (:@ temp1 imm0))
  (movs imm1 (:lsl imm1 (:$ arm::fixnumshift)))
  (add imm1 imm1 (:$ arm::misc-data-offset))
  @have-scaled-table-index
  (ldrne arg_x (:@ nfn 'class))
  (ldrne nfn (:@ nfn '%maybe-std-slot-value-using-class))
  (ldrne arg_z (:@ temp0 imm1))
  (set-nargs 3)
  (ldrne pc (:@ nfn (:$ arm::function.entrypoint)))
  @missing                              ; (%slot-id-ref-missing instance id)
  (ldr nfn (:@ nfn '%slot-id-ref-missing))
  (set-nargs 2)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint))))


(defarmlapfunction %small-set-slot-id-value ((instance arg_x)
                                             (slot-id arg_y)
                                             (new-value arg_z))
  (ldr temp1 (:@ nfn 'map))
  (svref temp0 slot-id.index slot-id)
  (getvheader imm0 temp1)
  (header-length imm1 imm0)
  (cmp temp0 imm1)
  (mov imm0 (:lsr temp0 (:$ arm::word-shift)))
  (ldr temp0 (:@ nfn 'table))
  (add imm0 imm0 (:$ arm::misc-data-offset))
  (bhs @missing)
  (ldrb imm1 (:@ temp1 imm0))
  (cmp imm1 (:$ 0))
  (mov imm1 (:lsl imm1 (:$ arm::word-shift)))
  (add imm1 imm1 (:$ arm::misc-data-offset))
  (beq @missing)
  @have-scaled-table-index
  (ldr temp1 (:@ nfn 'class))
  (ldr arg_y (:@ temp0 imm1))
  (ldr nfn (:@ nfn '%maybe-std-setf-slot-value-using-class))
  (set-nargs 4)
  (vpush1 temp1)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint)))
  @missing                              ; (%slot-id-set-missing instance id new-value)
  (ldr nfn (:@ nfn '%slot-id-set-missing))
  (set-nargs 3)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint))))

(defarmlapfunction %large-set-slot-id-value ((instance arg_x)
                                             (slot-id arg_y)
                                             (new-value arg_z))
  (ldr temp1 (:@ nfn 'map))
  (svref temp0 slot-id.index slot-id)
  (getvheader imm0 temp1)
  (header-length imm1 imm0)
  (cmp temp0 imm1)
  (add imm0 temp0 (:$ arm::misc-data-offset))
  (ldr temp0 (:@ nfn 'table))
  (bhs @missing)
  (ldr imm1 (:@ temp1 imm0))
  (movs imm1 (:lsl imm1 (:$ arm::fixnumshift)))
  (add imm1 imm1 (:$ arm::misc-data-offset))
  (beq @missing)
  @have-scaled-table-index
  (ldr temp1 (:@ nfn 'class))
  (ldr arg_y (:@ temp0 imm1))
  (ldr nfn (:@ nfn '%maybe-std-setf-slot-value-using-class))
  (set-nargs 4)
  (vpush1 temp1)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint)))
  @missing                              ; (%slot-id-set-missing instance id new-value)
  (ldr nfn (:@ nfn '%slot-id-ref-missing))
  (set-nargs 3)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint)))
)

(defparameter *gf-proto*
  (nfunction
   gag
   (lambda (&lap &lexpr args)
     (arm-lap-function 
      gag 
      ()
      (vpush-argregs)
      (vpush1 nargs)
      (ref-global arg_x ret1valaddr)
      (add imm1 vsp nargs)
      (add imm1 imm1 (:$ arm::node-size))                  ; caller's vsp
      (cmp lr arg_x)
      (build-lisp-frame imm0 imm1)
      (mov fn (:$ 0))
      (moveq lr (:$ (- arm::nil-value arm::fulltag-nil)))
      (ldreq lr (:@ lr (:$ (arm::%kernel-global 'arm::lexpr-return))))
      (stmdbeq (:! sp) (imm0 imm1 fn lr))
      (moveq lr arg_x)
      (movne lr (:$ (- arm::nil-value arm::fulltag-nil)))
      (ldrne lr (:@ lr (:$ (arm::%kernel-global 'arm::lexpr-return1v))))
      (mov arg_z vsp)
      (nth-immediate arg_y gf.dispatch-table nfn) ; dispatch-table
      (set-nargs 2)
      (nth-immediate nfn gf.dcode nfn) ; dcode function
      (ldr pc (:@ nfn (:$ arm::function.entrypoint)))))))


      
      

(defarmlapfunction funcallable-trampoline ()
  (nth-immediate nfn gf.dcode nfn)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint))))

;;; This can't reference any of the function's constants.
(defarmlapfunction unset-fin-trampoline ()
  (build-lisp-frame)
  (bla .SPheap-rest-arg)                 ; cons up an &rest arg, vpush it
  (vpop1 arg_z)                          ; whoops, didn't really want to
  (mov arg_x '#.$XNOFINFUNCTION)
  (mov arg_y nfn)
  (set-nargs 3)
  (bla .SPksignalerr)
  (mov arg_z 'nil)
  (return-lisp-frame))

;;; is a winner - saves ~15%
(defarmlapfunction gag-one-arg ((arg arg_z))
  (check-nargs 1)  
  (nth-immediate arg_y gf.dispatch-table nfn) ; mention dt first
  (set-nargs 2)
  (nth-immediate nfn gf.dcode nfn)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint))))


(defarmlapfunction gag-two-arg ((arg0 arg_y) (arg1 arg_z))
  (check-nargs 2)  
  (nth-immediate arg_x gf.dispatch-table nfn) ; mention dt first
  (set-nargs 3)
  (nth-immediate nfn gf.dcode nfn)
  (ldr pc (:@ nfn (:$ arm::function.entrypoint))))

(defparameter *cm-proto*
  (nfunction
   gag
   (lambda (&lap &lexpr args)
     (arm-lap-function 
      gag 
      ()
      (vpush-argregs)
      (vpush1 nargs)
      (ref-global arg_x ret1valaddr)
      (add imm1 vsp nargs)
      (add imm1 imm1 (:$ arm::node-size))                  ; caller's vsp
      (cmp lr arg_x)
      (build-lisp-frame imm0 imm1)
      (mov fn (:$ 0))
      (moveq lr (:$ (- arm::nil-value arm::fulltag-nil)))
      (ldreq lr (:@ lr (:$ (arm::%kernel-global 'arm::lexpr-return))))
      (stmdbeq (:! sp) (imm0 imm1 fn lr))
      (moveq lr arg_x)
      (movne lr (:$ (- arm::nil-value arm::fulltag-nil)))
      (ldrne lr (:@ lr (:$ (arm::%kernel-global 'arm::lexpr-return1v))))
      (mov arg_z vsp)
      (nth-immediate arg_y combined-method.thing nfn) ; thing
      (set-nargs 2)
      (nth-immediate nfn combined-method.dcode nfn) ; dcode function
      (ldr pc (:@ nfn (:$ arm::function.entrypoint)))))))
