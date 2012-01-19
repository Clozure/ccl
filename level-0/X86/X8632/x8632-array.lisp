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

(eval-when (:compile-toplevel :execute)
  (require "X8632-ARCH")
  (require "X86-LAPMACROS"))

;; rewrite in LAP someday (soon).
(defun %init-misc (val uvector)
  (dotimes (i (uvsize uvector) uvector)
    (setf (uvref uvector i) val)))

;;; Make a new vector of size newsize whose subtag matches that of oldv-arg.
;;; Blast the contents of the old vector into the new one as quickly as
;;; possible; leave remaining elements of new vector undefined (0).
;;; Return new-vector.

    
;;; argument is a vector header or an array header.  Or else.
(defx8632lapfunction %array-header-data-and-offset ((a arg_z))
  (let ((offset arg_y)
        (temp temp1))
    (movl (% esp) (% temp0))
    (movl ($ '0) (%l offset))
    (movl (% a) (% temp))
    @loop
    (movl (@ target::arrayH.data-vector (% temp)) (% a))
    (extract-subtag a imm0)
    (addl (@ target::arrayH.displacement (% temp)) (% offset))
    (rcmp (% imm0) ($ target::subtag-vectorH))
    (movl (% a) (% temp))
    (jle @loop)
    (push (% a))
    (push (% offset))
    (set-nargs 2)
    (jmp-subprim  .SPvalues)))

(defx8632lapfunction %boole-clr ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl ($ 0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-set ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl ($ -1) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-1 ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ b0 (% esp)) (% temp1))
  (movl (@ x8632::misc-data-offset (% temp1) (% temp0)) (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-2 ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ x8632::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-c1 ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ b0 (% esp)) (% temp1))
  (movl (@ x8632::misc-data-offset (% temp1) (% temp0)) (% imm0))
  (notl (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-c2 ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ x8632::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notl (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-and ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ b0 (% esp)) (% temp1))
  (movl (@ x8632::misc-data-offset (% temp1) (% temp0)) (% imm0))
  (andl (@ x8632::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-ior ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ b0 (% esp)) (% temp1))
  (movl (@ x8632::misc-data-offset (% temp1) (% temp0)) (% imm0))
  (orl (@ x8632::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-xor ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ b0 (% esp)) (% temp1))
  (movl (@ x8632::misc-data-offset (% temp1) (% temp0)) (% imm0))
  (xorl (@ x8632::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-eqv ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ b0 (% esp)) (% temp1))
  (movl (@ x8632::misc-data-offset (% temp1) (% temp0)) (% imm0))
  (xorl (@ x8632::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notl (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-nand ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ b0 (% esp)) (% temp1))
  (movl (@ x8632::misc-data-offset (% temp1) (% temp0)) (% imm0))
  (andl (@ x8632::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notl (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-nor ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ b0 (% esp)) (% temp1))
  (movl (@ x8632::misc-data-offset (% temp1) (% temp0)) (% imm0))
  (orl (@ x8632::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notl (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-andc1 ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ b0 (% esp)) (% temp1))
  (movl (@ x8632::misc-data-offset (% temp1) (% temp0)) (% imm0))
  (notl (% imm0))
  (andl (@ x8632::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-andc2 ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ b0 (% esp)) (% temp1))
  (movl (@ x8632::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notl (% imm0))
  (andl (@ x8632::misc-data-offset (% temp1) (% temp0)) (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-orc1 ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ b0 (% esp)) (% temp1))
  (movl (@ x8632::misc-data-offset (% temp1) (% temp0)) (% imm0))
  (notl (% imm0))
  (orl (@ x8632::misc-data-offset (% b1) (% temp0)) (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defx8632lapfunction %boole-orc2 ((len 8) (b0 4) #|(ra 0)|# (b1 arg_y) (dest arg_z))
  (movl (@ len (% esp)) (% temp0))
  (jmp @test)
  @loop
  (movl (@ b0 (% esp)) (% temp1))
  (movl (@ x8632::misc-data-offset (% b1) (% temp0)) (% imm0))
  (notl (% imm0))
  (orl (@ x8632::misc-data-offset (% temp1) (% temp0)) (% imm0))
  (movl (% imm0) (@ x8632::misc-data-offset (% dest) (% temp0)))
  @test
  (subl ($ '1) (% temp0))
  (jge @loop)
  (single-value-return 4))

(defparameter *simple-bit-boole-functions* ())

(setq *simple-bit-boole-functions*
      (vector
       #'%boole-clr
       #'%boole-set
       #'%boole-1
       #'%boole-2
       #'%boole-c1
       #'%boole-c2
       #'%boole-and
       #'%boole-ior
       #'%boole-xor
       #'%boole-eqv
       #'%boole-nand
       #'%boole-nor
       #'%boole-andc1
       #'%boole-andc2
       #'%boole-orc1
       #'%boole-orc2))

(defun %simple-bit-boole (op b1 b2 result)
  (funcall (svref *simple-bit-boole-functions* op)
           (ash (the fixnum (+ (length result) 31)) -5)
           b1
           b2
           result))

(defx8632lapfunction %aref2 ((array 4) #|(ra 0)|# (i arg_y) (j arg_z))
  (check-nargs 3)
  (popl (@ 8 (% esp)))			;ra to first word of reserved frame
  (pop (% temp0))
  (addl ($ '1) (% esp))			;discard other word of reserved frame
  (jmp-subprim .SParef2))

(defx8632lapfunction %aref3 ((array 8) (i 4) #|(ra 0)|# (j arg_y) (k arg_z))
  (check-nargs 4)
  (popl (@ 12 (% esp)))
  (pop (% temp0))
  (pop (% temp1))
  (addl ($ '1) (% esp))
  (jmp-subprim .SParef3))

(defx8632lapfunction %aset2 ((array 8) (i 4) #|(ra 0)|# (j arg_y) (newval arg_z))
  (check-nargs 4)
  (popl (@ 12 (% esp)))
  (pop (% temp0))
  (pop (% temp1))
  (addl ($ '1) (% esp))
  (jmp-subprim .SPaset2))

;;; We're out of registers.  Put i on the stack.
(defx8632lapfunction %aset3 ((array 12) (i 8) (j 4) #|(ra 0)|# (k arg_y) (newval arg_z))
  (check-nargs 5)
  (popl (@ 16 (% esp)))
  (pop (% temp0))
  (popl (@ 4 (% esp)))
  (pop (% temp1))
  (jmp-subprim .SPaset3))

