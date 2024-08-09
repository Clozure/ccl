;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 2010 Clozure Associates
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
(defparameter *arm-xtype-specifiers* (make-array 256 :initial-element nil))

(macrolet ((init-arm-xtype-table (&rest pairs)
             (let* ((table (gensym)))
               (collect ((body))
                 (dolist (pair pairs)
                   (destructuring-bind (code . spec) pair
                     (body `(setf (svref ,table ,code) ',spec))))
                 `(let* ((,table *arm-xtype-specifiers*))
                   ,@(body))))))
  (init-arm-xtype-table
   (arm::tag-fixnum . fixnum)
   (arm::tag-list . list)
   (arm::xtype-integer . integer)
   (arm::xtype-s64 . (signed-byte 64))
   (arm::xtype-u64 . (unsigned-byte 64))
   (arm::xtype-s32 . (signed-byte 32))
   (arm::xtype-u32 . (unsigned-byte 32))
   (arm::xtype-s16 . (signed-byte 16))
   (arm::xtype-u16 . (unsigned-byte 16))
   (arm::xtype-s8  . (signed-byte 8))
   (arm::xtype-u8  . (unsigned-byte 8))
   (arm::xtype-bit . bit)
   (arm::xtype-rational . rational)
   (arm::xtype-real . real)
   (arm::xtype-number . number)
   (arm::xtype-char-code . (mod #x110000))
   (arm::xtype-unsigned-byte-24 . (unsigned-byte 24))
   (arm::xtype-array2d . (array * (* *)))
   (arm::xtype-array3d . (array * (* * *)))
   (arm::subtag-bignum . bignum)
   (arm::subtag-ratio . ratio)
   (arm::subtag-single-float . single-float)
   (arm::subtag-double-float . double-float)
   (arm::subtag-complex . complex)
   (arm::subtag-macptr . macptr)
   (arm::subtag-code-vector . code-vector)
   (arm::subtag-xcode-vector . xcode-vector)
   (arm::subtag-catch-frame . catch-frame)
   (arm::subtag-function . function)
   (arm::subtag-basic-stream . basic-stream)
   (arm::subtag-symbol . symbol)
   (arm::subtag-lock . lock)
   (arm::subtag-hash-vector . hash-vector)
   (arm::subtag-pool . pool)
   (arm::subtag-weak . population)
   (arm::subtag-package . package)
   (arm::subtag-slot-vector . slot-vector)
   (arm::subtag-instance . standard-object)
   (arm::subtag-struct . structure-object)
   (arm::subtag-istruct . istruct)      ;??
   (arm::subtag-value-cell . value-cell)
   (arm::subtag-xfunction . xfunction)
   (arm::subtag-arrayH . array-header)
   (arm::subtag-vectorH . vector-header)
   (arm::subtag-simple-vector . simple-vector)
   (arm::subtag-single-float-vector . (simple-array single-float (*)))
   (arm::subtag-u32-vector . (simple-array (unsigned-byte 32) (*)))
   (arm::subtag-s32-vector . (simple-array (signed-byte 32) (*)))
   (arm::subtag-fixnum-vector . (simple-array fixnum (*)))
   (arm::subtag-simple-base-string . simple-base-string)
   (arm::subtag-u8-vector . (simple-array (unsigned-byte 8) (*)))
   (arm::subtag-s8-vector . (simple-array (signed-byte 8) (*)))   
   (arm::subtag-u16-vector . (simple-array (unsigned-byte 16) (*)))
   (arm::subtag-double-float-vector . (simple-array double-float (*)))
   (arm::subtag-bit-vector . simple-bit-vector)
   (arm::subtag-complex-single-float-vector . (simple-array (complex single-float) (*)))
   (arm::subtag-complex-double-float-vector . (simple-array (complex double-float) (*)))))
  

;;; Return a pointer to the saved VFP info a ucontext's mcontext,
;;; and the FPSCR values in that info as an unsigned 32-bit integer.
;;; Return a null pointer an 0 if this info can't be found.
(defun xp-vfp-info (xp)
  (let* ((p (pref xp :ucontext_t.uc_regspace)))
    (loop
      (let* ((magic (%get-unsigned-long p)))
        (case magic
          (#x56465001                    ;VFP magic
           (%incf-ptr p 8)
           (return (values p (%get-unsigned-long p (* 32 8)))))
          ((#x12ef842a #x5065cf03)      ;IWMMXT or CRUNCH magic
           (%incf-ptr p (%get-unsigned-long p 4)))
          (otherwise
           (return (values #-cross-compiling +null-ptr+ #+cross-compiling (%null-ptr)  0))))))))

(defun xp-argument-list (xp)
  (let ((nargs (xp-gpr-lisp xp arm::nargs))     ; tagged as a fixnum (how convenient)
        (arg-x (xp-gpr-lisp xp arm::arg_x))
        (arg-y (xp-gpr-lisp xp arm::arg_y))
        (arg-z (xp-gpr-lisp xp arm::arg_z)))
    (cond ((eql nargs 0) nil)
          ((eql nargs 1) (list arg-z))
          ((eql nargs 2) (list arg-y arg-z))
          (t (let ((args (list arg-x arg-y arg-z)))
               (if (eql nargs 3)
                 args
                 (let ((vsp (xp-gpr-macptr xp arm::vsp)))
                   (dotimes (i (- nargs 3))
                     (push (%get-object vsp (* i target::node-size)) args))
                   args)))))))

(defun handle-udf-call (xp frame-ptr)
  (let* ((args (xp-argument-list xp))
         (values (multiple-value-list
                  (%kernel-restart-internal
                   $xudfcall
                   (list (maybe-setf-name (xp-gpr-lisp xp arm::fname)) args)
                   frame-ptr)))
         (stack-argcnt (max 0 (- (length args) 3)))
         (vsp (%i+ (xp-gpr-lisp xp arm::vsp) stack-argcnt))
         (f #'(lambda (values) (apply #'values values))))
    (setf (xp-gpr-lisp xp arm::vsp) vsp
          (xp-gpr-lisp xp arm::nargs) 1
          (xp-gpr-lisp xp arm::arg_z) values
          (xp-gpr-lisp xp arm::nfn) f)
    ;; handle_uuo() (in the lisp kernel) will not bump the PC here.
    (setf (xp-gpr-lisp xp arm::pc) (uvref f 0))))

(defun preceding-fpu-instruction (xp)
  (let* ((f (xp-gpr-lisp xp arm::fn)))
    (when (typep f 'function)
      (let* ((idx
              (loop (let* ((pc (xp-gpr-lisp xp arm::pc))
                           (entry (%svref f 0))
                           (d (- pc entry)))
                      (when (and (eql pc (xp-gpr-lisp xp arm::pc))
                                 (eql entry (%svref f 0)))
                        (return d)))))
             (cv (%svref f 1)))
        (declare (fixnum idx))
        (do* ((i (1- idx) (1- i)))
             ((< i 0))
          (declare (fixnum i))
          (let* ((inst (uvref cv i))
                 (masked (logand inst #x0f000f00)))
            (when (and (or (eql masked #x0e000b00)
                           (eql masked #x0e000a00))
                       ;; Ignore fmxr, fmrx ...
                       (not (eql #x10 (logand inst #xff))))
              (return inst))))))))
             
  
(defcallback %xerr-disp (:address xp
                                  :signed-fullword error-number
                                  :unsigned-fullword arg
                                  :unsigned-fullword fnreg
                                  :unsigned-fullword relative-pc
                                  :int)
  (let* ((fn (unless (eql 0 fnreg) (xp-gpr-lisp xp fnreg)))
         (delta 0))
    (with-xp-stack-frames (xp fn frame-ptr)
      (with-error-reentry-detection
          (cond
            ((eql 0 error-number)       ; Hopefully a UUO.
             (setq delta 4)
             (if (/= (logand arg #x0ff000f0) #x07f000f0)
               (%error "Unknown non-UUO: #x~x" (list arg) frame-ptr)
               (let* ((condition (ldb (byte 4 28) arg))
                      (uuo (ldb (byte 28 0) arg))
                      (format (ldb (byte 4 0) uuo)))
                 (case format
                   ((2 10)              ; uuo-format-[c]error-lisptag
                    (%error (make-condition
                             'type-error
                             :datum (xp-gpr-lisp xp (ldb (byte 4 8) uuo))
                             :expected-type
                             (svref #(fixnum list uvector immediate)
                                    (ldb (byte 2 12) uuo)))
                            nil
                            frame-ptr))
                   ((3 11)
                    (%error (make-condition
                             'type-error
                             :datum (xp-gpr-lisp xp (ldb (byte 4 8) uuo))
                             :expected-type
                             (svref #(fixnum null bogus immediate fixnum cons uvector bogus)
                                    (ldb (byte 3 12) uuo)))
                            nil
                            frame-ptr))
                   ((4 12)
                    (%error (make-condition
                             'type-error
                             :datum (xp-gpr-lisp xp (ldb (byte 4 8) uuo))
                             :expected-type
                             (svref *arm-xtype-specifiers* (ldb (byte 8 12) uuo)))
                            nil
                            frame-ptr))
                   (8                   ;nullary error.  Only one, atm.
                    (case (ldb (byte 12 8) uuo)
                      (1                ;why 1?
                       (let* ((condition-name
                               (cond ((eq condition arm::arm-cond-lo)
                                      'too-few-arguments)
                                     ((eq condition arm::arm-cond-hs)
                                      'too-many-arguments)
                                     (t
                                      ;;(assert condition arm::arm-cond-ne)
                                      (let* ((cpsr (xp-gpr-signed-long xp
                                                                       xp-cpsr-regno)))
                                        (if (logbitp 29 cpsr)
                                          'too-many-arguments
                                          'too-few-arguments))))))
                         (%error condition-name
                                 (list :nargs (xp-gpr-lisp xp arm::nargs)
                                       :fn fn)
                                 frame-ptr)))
                      (t
                       (%error "Unknown nullary UUO code ~d"
                               (list (ldb (byte 12 8) uuo))
                               frame-ptr))))
                   (9                   ;unary error
                    (let* ((code (ldb (byte 8 12) uuo))
                           (regno (ldb (byte 4 8) uuo))
                           (arg (xp-gpr-lisp xp regno)))
                      (case code
                        ((0 1)
                         (setf (xp-gpr-lisp xp regno)
                               (%kernel-restart-internal $xvunbnd
                                                         (list arg)
                                                         frame-ptr)))
                        (2
                         (%error (make-condition 'type-error
                                                 :datum arg
                                                 :expected-type '(or symbol function)
                                                 :format-control
                                                 "~S is not of type ~S, and can't be FUNCALLed or APPLYed")
                                 nil frame-ptr))
                        (4
                         (%error (make-condition 'cant-throw-error
                                                 :tag arg)
                                 nil frame-ptr))
                        (5
                         (setq delta 0)
                         (handle-udf-call xp frame-ptr))
                        (6
                         (%err-disp-internal $xfunbnd (list arg) frame-ptr))
                        (t
                         (error "Unknown unary UUO with code ~d." code)))))
                   (14
                    (let* ((reg-a (ldb (byte 4 8) uuo))
                           (arg-b (xp-gpr-lisp xp (ldb (byte 4 12) uuo)))
                           (arg-c (xp-gpr-lisp xp (ldb (byte 4 16) uuo))))
                      (setq *error-reentry-count* 0)
                      (setf (xp-gpr-lisp xp reg-a)
                            (%slot-unbound-trap arg-b arg-c frame-ptr))))
                   (15
                    (let* ((reg-a (ldb (byte 4 8) uuo))
                           (arga (xp-gpr-lisp xp reg-a))
                           (argb (xp-gpr-lisp xp (ldb (byte 4 12) uuo)))
                           (code (ldb (byte 4 16) uuo)))
                      (case code
                        ((0 1)          ;do we report these the same way?
                         (%error (%rsc-string $xarroob)
                                 (list arga argb)
                                 frame-ptr))
                        (4
                         (let* ((eep-or-fv (xp-gpr-lisp xp (ldb (byte 4 12) uuo)))
                                (dest-reg (ldb (byte 4 8) uuo)))
                           (etypecase eep-or-fv
                             (external-entry-point
                              (resolve-eep eep-or-fv)
                              (setf (xp-gpr-lisp xp dest-reg)
                                    (eep.address eep-or-fv)))
                             (foreign-variable
                              (resolve-foreign-variable eep-or-fv)
                              (setf (xp-gpr-lisp xp dest-reg)
                                    (fv.addr eep-or-fv))))))
                        (5              ;fpu
                         (let* ((vfp-regs (xp-vfp-info xp))
                                (status (xp-gpr-unsigned-long xp (ldb (byte 4 8) uuo)))
                                (instruction (preceding-fpu-instruction xp))
                                (condition-name (fp-condition-name-from-fpscr-status status)))
                           (if (and condition-name instruction)
                             (let* ((template (find-arm-instruction-template instruction))
                                    (operation (if template (arithmetic-error-operation-from-instruction template) 'unknown))
                                    (operands (if template (arithmetic-error-operands-from-instruction template instruction vfp-regs xp))))
                               (%error condition-name `(:operation ,operation :operands ,operands) frame-ptr))
                             (%error "FPU exception, fpscr = ~d" (list status) frame-ptr))
                         ))
                        (6              ;array rank
                         (%err-disp-internal $XNDIMS
                                             (list
                                              argb
                                              arga)
                                             frame-ptr))
                        (7              ;array flags
                         ;; This is currently only used to signal that
                         ;; a (purported) array header doesn't have the
                         ;; flags which denote a simple-array with
                         ;; a particular subtype.  Decode things, then
                         ;; signal a TYPE-ERROR.
                         (let* ((array (xp-gpr-lisp xp (ldb (byte 4 12) uuo)))
                                (flags (xp-gpr-lisp xp (ldb (byte 4 8) uuo)))
                                (subtag (ldb target::arrayH.flags-cell-subtag-byte flags))
                                (element-type
                                 (type-specifier
                                  (array-ctype-element-type
                                   (specifier-type (svref *arm-xtype-specifiers* subtag))))))

                           (%error (make-condition
                                    'type-error
                                    :datum array
                                    :expected-type `(,(if (logbitp $arh_simple_bit flags) 'simple-array 'array) ,element-type))
                                   nil
                                   frame-ptr)))                        
                        (t
                         (error "Unknown code in binary UUO: ~d" code)))))
                   (5
                    (%error "Index value ~s is out of bounds for axis ~s of ~s."
                            (list
                             (xp-gpr-lisp xp (ldb (byte 4 16) uuo))
                             (xp-gpr-lisp xp (ldb (byte 4 12) uuo))
                             (xp-gpr-lisp xp (ldb (byte 4 8) uuo)))
                            frame-ptr))
                   (t
                    (error "Unknown UUO, format ~d" format))))))
            ((eql error-number arch::error-stack-overflow)
             (%error
              (make-condition
               'stack-overflow-condition 
               :format-control "Stack overflow on ~a stack."
               :format-arguments (list (if (eql arg arm::vsp) "value" "control")))
              nil frame-ptr))
            ((eql error-number arch::error-allocation-disabled)
             (restart-case (%error 'allocation-disabled nil frame-ptr)
               (continue ()
                         :report (lambda (stream)
                                   (format stream "retry the heap allocation.")))))
            (t
             (error "%errdisp callback: error-number = ~d, arg = #x~x, fnreg = ~d, rpc = ~d"
                    error-number arg fnreg relative-pc)))))
    delta))
