;;; Copyright 2009 Clozure Associates
;;; This file is part of Clozure CL.  
;;;
;;; Clozure CL is licensed under the terms of the Lisp Lesser GNU
;;; Public License , known as the LLGPL and distributed with Clozure
;;; CL as the file "LICENSE".  The LLGPL consists of a preamble and
;;; the LGPL, which is distributed with Clozure CL as the file "LGPL".
;;; Where these conflict, the preamble takes precedence.
;;;
;;; Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;; The LLGPL is also available online at
;;; http://opensource.franz.com/preamble.html

(in-package "CCL")

;;; This just maps a SLOT-ID to a SLOT-DEFINITION or NIL.
;;; The map is a vector of (UNSIGNED-BYTE 8); this should
;;; be used when there are fewer than 255 slots in the class.
(defx8632lapfunction %small-map-slot-id-lookup ((slot-id arg_z))
  (movl (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index arg_y)
  (vector-length temp1 temp0)
  (xorl (%l imm0) (%l imm0))
  (rcmpl (% arg_y) (% temp0))
  (ja @have-table-index)
  (movl (% arg_y) (% imm0))
  (shrl ($ x8632::word-shift) (% imm0))
  (movzbl (@ x8632::misc-data-offset (% temp1) (% imm0)) (%l imm0))
  ;(shll ($ x8632::word-shift) (% imm0))
  @have-table-index
  (movl (@ 'table (% fn)) (% temp0))
  (movl (@ x8632::misc-data-offset (% temp0) (% imm0) 4) (% arg_z))
  (single-value-return))

;;; The same idea, only the map is a vector of (UNSIGNED-BYTE 32).
(defx8632lapfunction %large-map-slot-id-lookup ((slot-id arg_z))
  (movl (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index arg_y)
  (vector-length temp1 temp0)
  (xorl (%l imm0) (%l imm0))
  (rcmpl (% arg_y) (% temp0))
  (ja @have-table-index)
  (movl (% arg_y) (% imm0))
  (movl (@ x8632::misc-data-offset (% temp1) (% imm0)) (%l imm0))
  @have-table-index
  (movl (@ 'table (% fn)) (% temp0))
  (movl (@ x8632::misc-data-offset (% temp0) (% imm0) 4) (% arg_z))
  (single-value-return))

(defx8632lapfunction %small-slot-id-value ((instance arg_y) (slot-id arg_z))
  (movl (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index temp0)
  (vector-length temp1 imm0)
  (rcmpl (% temp0) (% imm0))
  (movl ($ 0) (% imm0))			;don't disturb flags
  (ja @missing)
  (movl (% temp0) (% imm0))
  (shrl ($ x8632::word-shift) (% imm0))
  (movzbl (@ x8632::misc-data-offset (% temp1) (% imm0)) (% imm0))
  (testl (% imm0) (% imm0))
  (je @missing)
  (movl (@ 'table (% fn)) (% temp0))
  (movl (@ x8632::misc-data-offset (% temp0) (% imm0) 4) (% arg_z))
  (popl (% ra0))
  (pushl ($ x8632::reserved-frame-marker))
  (pushl ($ x8632::reserved-frame-marker))
  (pushl (@ 'class (% fn)))
  (set-nargs 3)
  (pushl (% ra0))
  (jmp (@ '%maybe-std-slot-value-using-class (% fn)))
  @missing                              ; (%slot-id-ref-missing instance id)
  (set-nargs 2)
  (jmp (@'%slot-id-ref-missing (% fn))))

(defx8632lapfunction %large-slot-id-value ((instance arg_y) (slot-id arg_z))  
  (movl (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index temp0)
  (vector-length temp1 imm0)
  (rcmp (% temp0) (% imm0))
  (movl ($ 0) (% imm0))
  (ja @missing)
  (movl (% temp0) (% imm0))
  (movl (@ x8632::misc-data-offset (% temp1) (% imm0)) (% imm0))
  (test (% imm0) (% imm0))
  (je @missing)
  (movl (@ 'table (% fn)) (% temp0))
  (movl (@ x8632::misc-data-offset (% temp0) (% imm0) 4) (% arg_z))
  (popl (% ra0))
  (pushl ($ x8632::reserved-frame-marker))
  (pushl ($ x8632::reserved-frame-marker))
  (pushl (@ 'class (% fn)))
  (set-nargs 3)
  (pushl (% ra0))
  (jmp (@ '%maybe-std-slot-value-using-class (% fn)))
  @missing                              ; (%slot-id-ref-missing instance id)
  (set-nargs 2)
  (jmp (@'%slot-id-ref-missing (% fn))))

(defx86lapfunction %small-set-slot-id-value ((instance 4)
					     #|(ra 0)|#
                                             (slot-id arg_y)
                                             (new-value arg_z))
  (movl (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index imm0)
  (vector-length temp1 temp0)
  (rcmpl (% imm0) (% temp0))
  (ja @missing)
  (shrl ($ x8632::word-shift) (% imm0))
  (movzbl (@ x8632::misc-data-offset (% temp1) (% imm0)) (% imm0))
  (testl (% imm0) (% imm0))
  (je @missing)
  (movl (@ 'table (% fn)) (% temp0))
  (movl (@ x8632::misc-data-offset (% temp0) (% imm0) 4) (% arg_y))
  (popl (% temp0))			;return address
  (popl (% temp1))			;instance
  ;; use existing frame
  (pushl (@ 'class (% fn)))
  (pushl (% temp1))
  (pushl (% temp0))
  (set-nargs 4)
  ;; (%maybe-std-setf-slot-value-using-class class instance slotd new)
  (jmp (@ '%maybe-std-setf-slot-value-using-class (% fn)))
  @missing
  (set-nargs 3)
  ;; (%slot-id-set-missing instance id new)
  (jmp (@ '%slot-id-set-missing (% fn))))

(defx8632lapfunction %large-set-slot-id-value ((instance 4)
					       #|(ra 0)|#
					       (slot-id arg_y)
					       (new-value arg_z))
  (movl (@ 'map (% fn)) (% temp1))
  (svref slot-id slot-id.index imm0)
  (vector-length temp1 temp0)
  (rcmpl (% imm0) (% temp0))
  (ja @missing)
  (movl (@ x8632::misc-data-offset (% temp1) (% imm0)) (%l imm0))
  (testl (%l imm0) (%l imm0))
  (je @missing)
  (movl (@ 'table (% fn)) (% temp0))
  (movl (@ x8632::misc-data-offset (% temp0) (% imm0) 4) (% arg_y))
  (popl (% temp0))			;return addr
  (popl (% temp1))			;instance
  (pushl (@ 'class (% fn)))
  (pushl (% temp1))
  (pushl (% temp0))
  (set-nargs 4)
  (jmp (@ '%maybe-std-setf-slot-value-using-class (% fn)))
  @missing                              ; (%slot-id-set-missing instance id new-value)
  (set-nargs 3)
  (jmp (@'%slot-id-ref-missing (% fn))))

;;; All of the generic function trampoline functions have to be
;;; exactly the same size (x8632::gf-code-size) in words.  The largest
;;; of these - the general-case *GF-PROTO* - is currently about 27
;;; words, so X8632::GF-CODE-SIZE is just a little bigger than that.
;;; (Note that x8632::gf-code-size has to include space for the
;;; self-reference table, which takes up another couple of words in
;;; addition to the machine instructions.)
(defparameter *gf-proto*
  (nfunction
   gag
   (lambda (&lap &lexpr args)
     (x86-lap-function 
      gag 
      ()
      (:fixed-constants (class-wrapper slots dispatch-table dcode hash))
      (:code-size x8632::gf-code-size)
      (movl (@ (% esp)) (% ra0))
      (save-frame-variable-arg-count)
      (push-argregs)
      (pushl (%l nargs))
      (movl (% esp) (% arg_z))
      (ref-global.l ret1valaddr imm0)
      (cmpl (% ra0) (% imm0))
      (je @multiple)
      (ref-global.l lexpr-return1v ra0)
      (jmp @call)
      @multiple
      (pushl (@ (+ (target-nil-value) (x8632::%kernel-global 'lexpr-return))))
      (movl (% imm0) (% ra0))
      @call
      (push (% ra0))
      (movl (@ 'dispatch-table (% fn)) (% arg_y))
      (set-nargs 2)
      (jmp (@ 'dcode (% fn)))  ; dcode function
      ))))

(defx8632lapfunction gag-one-arg ((arg arg_z))
  (:fixed-constants (class-wrapper slots dispatch-table dcode hash))
  (:code-size x8632::gf-code-size)
  (check-nargs 1)
  (movl (@ 'dispatch-table (% fn)) (% arg_y))
  (set-nargs 2)
  (jmp (@ 'dcode (% fn))))

(defx8632lapfunction gag-two-arg ((arg0 arg_y) (arg1 arg_z))
  (:fixed-constants (class-wrapper slots dispatch-table dcode hash))
  (:code-size x8632::gf-code-size)
  (check-nargs 2)
  (pop (% ra0))
  (pushl ($ x8632::reserved-frame-marker))
  (pushl ($ x8632::reserved-frame-marker))
  (pushl (@ 'dispatch-table (% fn)))
  (push (% ra0))
  (set-nargs 3)
  (jmp (@ 'dcode (% fn))))

(defx8632lapfunction funcallable-trampoline ()
  (:fixed-constants (class-wrapper slots dispatch-table dcode hash))
  (:code-size x8632::gf-code-size)
  (jmp (@ 'dcode (% fn))))

;;; This is in LAP so that it can reference itself in the error message.
;;; (It needs to be cloned, so %fn will be unique to each copy.)
;;; It can't work for this to reference any of its own constants.
(defx8632lapfunction unset-fin-trampoline ()
  (:code-size x8632::gf-code-size)
  (save-frame-variable-arg-count)
  (call-subprim .SPheap-rest-arg)
  (pop (% arg_z))
  (pushl ($ x8632::reserved-frame-marker))
  (pushl ($ x8632::reserved-frame-marker))
  (pushl ($ '#.$XNOFINFUNCTION))
  (movl (% fn) (% arg_y))
  (set-nargs 3)
  (call-subprim .SPksignalerr)
  ;(movl ($ (target-nil-value)) (% arg_z))
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
      (movl (@ (% esp)) (% ra0))
      (save-frame-variable-arg-count)
      (push-argregs)
      (pushl (% nargs))
      (movl (% esp) (% arg_z))
      (ref-global ret1valaddr imm0)
      (cmpl (% ra0) (% imm0))
      (je @multiple)
      (ref-global lexpr-return1v ra0)
      (jmp @call)
      @multiple
      (pushl (@ (+ (target-nil-value) (x8632::%kernel-global 'lexpr-return))))
      (movl (% imm0) (% ra0))
      @call
      (push (% ra0))
      (movl (@ 'thing (% fn)) (% arg_y))
      (set-nargs 2)
      (jmp (@ 'dcode (% fn)))))))
