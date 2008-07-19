;;; x86-trap-support
;;;
;;;   Copyright (C) 2005-2006 Clozure Associates and contributors
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


(in-package "CCL")

#+x8664-target
(defun xp-argument-count (xp)
  (ldb (byte (- 16 x8664::fixnumshift) 0)
                    (encoded-gpr-lisp xp x8664::nargs.q)))

#+x8632-target
(defun xp-argument-count (xp)
  (encoded-gpr-lisp xp target::nargs))

#+x8664-target
(defun xp-argument-list (xp)
  (let ((nargs (xp-argument-count xp))
        (arg-x (encoded-gpr-lisp xp x8664::arg_x))
        (arg-y (encoded-gpr-lisp xp x8664::arg_y))
        (arg-z (encoded-gpr-lisp xp x8664::arg_z)))
    (cond ((eql nargs 0) nil)
          ((eql nargs 1) (list arg-z))
          ((eql nargs 2) (list arg-y arg-z))
          (t
           (let ((args (list arg-x arg-y arg-z)))
             (if (eql nargs 3)
               args
               (let ((sp (%inc-ptr (encoded-gpr-macptr xp x8664::rsp)
                                   (+ x8664::node-size x8664::xcf.size))))
                 (dotimes (i (- nargs 3))
                   (push (%get-object sp (* i x8664::node-size)) args))
                 args)))))))

#+x8632-target
(defun xp-argument-list (xp)
  (let ((nargs (xp-argument-count xp))
        (arg-y (encoded-gpr-lisp xp x8632::arg_y))
        (arg-z (encoded-gpr-lisp xp x8632::arg_z)))
    (cond ((eql nargs 0) nil)
          ((eql nargs 1) (list arg-z))
	  (t
	   (let ((args (list arg-y arg-z)))
	     (if (eql nargs 2)
	       args
	       (let ((sp (%inc-ptr (encoded-gpr-macptr xp x8632::esp)
				   (+ x8632::node-size x8632::xcf.size))))
		 (dotimes (i (- nargs 2))
		   (push (%get-object sp (* i x8632::node-size)) args))
		 args)))))))

;;; Making this be continuable is hard, because of the xcf on the
;;; stack and the way that the kernel saves/restores rsp and rbp
;;; before calling out.  If we get around those problems, then
;;; we have to also deal with the fact that the return address
;;; is on the stack.  Easiest to make the kernel deal with that,
;;; and just set %fn to the function that returns the values
;;; returned by the (newly defined) function and %arg_z to
;;; that list of values.
(defun handle-udf-call (xp frame-ptr)
  (let* ((args (xp-argument-list xp))
         (values (multiple-value-list
                  (%kernel-restart-internal
                   $xudfcall
                   (list (maybe-setf-name (encoded-gpr-lisp xp target::fname)) args)
                   frame-ptr)))
         (f #'(lambda (values) (apply #'values values))))
    (setf (encoded-gpr-lisp xp target::arg_z) values
          (encoded-gpr-lisp xp target::fn) f)))

#+x8664-target
(defcallback %xerr-disp (:address xp :address xcf :int)
  (with-error-reentry-detection
      (let* ((frame-ptr (macptr->fixnum xcf))
             (fn (%get-object xcf x8664::xcf.nominal-function))
             (op0 (%get-xcf-byte xcf 0))
             (op1 (%get-xcf-byte xcf 1))
             (op2 (%get-xcf-byte xcf 2)))
        (declare (type (unsigned-byte 8) op0 op1 op2))
        (let* ((skip 2))
          (if (and (= op0 #xcd)
                   (>= op1 #x70))
            (cond ((< op1 #x90)
                   (setq skip (%check-anchored-uuo xcf 3))
                   (setq *error-reentry-count* 0)
                   (setf (encoded-gpr-lisp xp (ldb (byte 4 0) op1))
                         (%slot-unbound-trap
                          (encoded-gpr-lisp xp (ldb (byte 4 4) op2))
                          (encoded-gpr-lisp xp (ldb (byte 4 0) op2))
                          frame-ptr)))
                  ((< op1 #xa0)
                   (setq skip (%check-anchored-uuo xcf 2))
                   ;; #x9x - register X is a symbol.  It's unbound.
                   (%kernel-restart-internal $xvunbnd
                                             (list
                                              (encoded-gpr-lisp
                                               xp
                                               (ldb (byte 4 0) op1)))
                                             frame-ptr))
                  ((< op1 #xb0)
                   (setq skip (%check-anchored-uuo xcf 2))
                   (%err-disp-internal $xfunbnd
                                       (list (encoded-gpr-lisp
                                              xp
                                              (ldb (byte 4 0) op1)))
                                       frame-ptr))
                  ((< op1 #xc0)
                   (setq skip (%check-anchored-uuo xcf 3))
                   (%err-disp-internal 
                    #.(car (rassoc 'type-error *kernel-simple-error-classes*))
                    (list (encoded-gpr-lisp
                           xp
                           (ldb (byte 4 0) op1))
                          (logandc2 op2 arch::error-type-error))
                    frame-ptr))
                  ((= op1 #xc0)
                   (setq skip (%check-anchored-uuo xcf 2))
                   (%error 'too-few-arguments
                           (list :nargs (xp-argument-count xp)
                                 :fn fn)
                           frame-ptr))
                  ((= op1 #xc1)
                   (setq skip (%check-anchored-uuo xcf 2))
                   (%error 'too-many-arguments
                           (list :nargs (xp-argument-count xp)
                                 :fn fn)
                           frame-ptr))
                  ((= op1 #xc2)
                   (setq skip (%check-anchored-uuo xcf 2))
                   (let* ((flags (xp-flags-register xp))
                          (nargs (xp-argument-count xp))
                          (carry-bit (logbitp x86::x86-carry-flag-bit flags)))
                     (if carry-bit
                       (%error 'too-few-arguments
                               (list :nargs nargs
                                     :fn fn)
                               frame-ptr)
                       (%error 'too-many-arguments
                               (list :nargs nargs
                                     :fn fn)
                               frame-ptr))))
                  ((= op1 #xc3)         ;array rank
                   (setq skip (%check-anchored-uuo xcf 3))                   
                   (%err-disp-internal $XNDIMS
                                       (list (encoded-gpr-lisp xp (ldb (byte 4 4) op2))
                                             (encoded-gpr-lisp xp (ldb (byte 4 0) op2)))
                                       frame-ptr))
                  ((= op1 #xc6)
                   (setq skip (%check-anchored-uuo xcf 2))
                   (%error (make-condition 'type-error
                                           :datum (encoded-gpr-lisp xp x8664::temp0)
                                           :expected-type '(or symbol function)
                                           :format-control
                                           "~S is not of type ~S, and can't be FUNCALLed or APPLYed")
                           nil frame-ptr))
                  ((= op1 #xc7)
                   (handle-udf-call xp frame-ptr)
                   (setq skip 0))
                  ((or (= op1 #xc8) (= op1 #xcb))
                   (setq skip (%check-anchored-uuo xcf 3))
                   (%error (%rsc-string $xarroob)
                           (list (encoded-gpr-lisp xp (ldb (byte 4 4) op2))
                                 (encoded-gpr-lisp xp (ldb (byte 4 0) op2)))
                           frame-ptr))
                  ((= op1 #xc9)
                   (setq skip (%check-anchored-uuo xcf 2))
                   (%err-disp-internal $xnotfun
                                       (list (encoded-gpr-lisp xp x8664::temp0))
                                       frame-ptr))
                  ;; #xca = uuo-error-debug-trap
                  ((= op1 #xcc)
                   ;; external entry point or foreign variable
                   (setq skip (%check-anchored-uuo xcf 3))
                   (let* ((eep-or-fv (encoded-gpr-lisp xp (ldb (byte 4 4) op2))))
                     (etypecase eep-or-fv
                       (external-entry-point
                        (resolve-eep eep-or-fv)
                        (setf (encoded-gpr-lisp xp (ldb (byte 4 0) op2))
                              (eep.address eep-or-fv)))
                       (foreign-variable
                        (resolve-foreign-variable eep-or-fv)
                        (setf (encoded-gpr-lisp xp (ldb (byte 4 0) op2))
                              (fv.addr eep-or-fv))))))
                  ((< op1 #xe0)
                   (setq skip (%check-anchored-uuo xcf 3))
                   (if (= op2 x8664::subtag-catch-frame)
                     (%error (make-condition 'cant-throw-error
                                             :tag (encoded-gpr-lisp
                                                   xp
                                                   (ldb (byte 4 0) op1)))
                             nil frame-ptr)
                     (let* ((typename
                             (cond ((= op2 x8664::tag-fixnum) 'fixnum)
                                   ((= op2 x8664::tag-single-float) 'single-float)
                                   ((= op2 x8664::subtag-character) 'character)
                                   ((= op2 x8664::fulltag-cons) 'cons)
                                   ((= op2 x8664::tag-misc) 'uvector)
                                   ((= op2 x8664::fulltag-symbol) 'symbol)
                                   ((= op2 x8664::fulltag-function) 'function)
                                   (t (let* ((class (logand op2 x8664::fulltagmask))
                                             (high4 (ash op2 (- x8664::ntagbits))))
                                        (cond ((= class x8664::fulltag-nodeheader-0)
                                               (svref *nodeheader-0-types* high4))
                                              ((= class x8664::fulltag-nodeheader-1)
                                               (svref *nodeheader-1-types* high4))
                                              ((= class x8664::fulltag-immheader-0)
                                               (svref *immheader-0-types* high4))
                                              ((= class x8664::fulltag-immheader-1)
                                               (svref *immheader-1-types* high4))
                                              ((= class x8664::fulltag-immheader-2)
                                               (svref *immheader-2-types* high4))
                                              (t (list 'bogus op2))))))))
                       (%error (make-condition 'type-error
                                               :datum (encoded-gpr-lisp
                                                       xp
                                                       (ldb (byte 4 0) op1))
                                               :expected-type typename)
                               nil
                               frame-ptr))))
                  ((< op1 #xf0)
                   (setq skip (%check-anchored-uuo xcf 2))
                   (%error (make-condition 'type-error
                                           :datum (encoded-gpr-lisp
                                                   xp
                                                   (ldb (byte 4 0) op1))
                                           :expected-type 'list)
                           nil
                           frame-ptr))
                  (t
                   (setq skip (%check-anchored-uuo xcf 2))
                   (%error (make-condition 'type-error
                                           :datum (encoded-gpr-lisp
                                                   xp
                                                   (ldb (byte 4 0) op1))
                                           :expected-type 'fixnum)
                           nil
                           frame-ptr)))
            (%error "Unknown trap: #x~x~%xp=~s"
                    (list (list op0 op1 op2) xp)
                    frame-ptr))
          skip))))

;;; lots of duplicated code here
#+x8632-target
(defcallback %xerr-disp (:address xp :address xcf :int)
  (with-error-reentry-detection
      (let* ((frame-ptr (macptr->fixnum xcf))
             (fn (%get-object xcf x8632::xcf.nominal-function))
             (op0 (%get-xcf-byte xcf 0))
             (op1 (%get-xcf-byte xcf 1))
             (op2 (%get-xcf-byte xcf 2)))
        (declare (type (unsigned-byte 8) op0 op1 op2))
        (let* ((skip 2))
          (if (and (= op0 #xcd)
                   (>= op1 #x70))
            (cond ((< op1 #x90)
                   (setq skip 3)
                   (setq *error-reentry-count* 0)
                   (setf (encoded-gpr-lisp xp (ldb (byte 4 0) op1))
                         (%slot-unbound-trap
                          (encoded-gpr-lisp xp (ldb (byte 4 4) op2))
                          (encoded-gpr-lisp xp (ldb (byte 4 0) op2))
                          frame-ptr)))
                  ((< op1 #xa0)
                   ;; #x9x - register X is a symbol.  It's unbound.
                   (%kernel-restart-internal $xvunbnd
                                             (list
                                              (encoded-gpr-lisp
                                               xp
                                               (ldb (byte 4 0) op1)))
                                             frame-ptr))
                  ((< op1 #xb0)
                   (%err-disp-internal $xfunbnd
                                       (list (encoded-gpr-lisp
                                              xp
                                              (ldb (byte 4 0) op1)))
                                       frame-ptr))
                  ((< op1 #xc0)
                   (setq skip 3)
                   (%err-disp-internal 
                    #.(car (rassoc 'type-error *kernel-simple-error-classes*))
                    (list (encoded-gpr-lisp
                           xp
                           (ldb (byte 4 0) op1))
                          (logandc2 op2 arch::error-type-error))
                    frame-ptr))
                  ((= op1 #xc0)
                   (%error 'too-few-arguments
                           (list :nargs (xp-argument-count xp)
                                 :fn fn)
                           frame-ptr))
                  ((= op1 #xc1)
                   (%error 'too-many-arguments
                           (list :nargs (xp-argument-count xp)
                                 :fn fn)
                           frame-ptr))
                  ((= op1 #xc2)
                   (let* ((flags (xp-flags-register xp))
                          (nargs (xp-argument-count xp))
                          (carry-bit (logbitp x86::x86-carry-flag-bit flags)))
                     (if carry-bit
                       (%error 'too-few-arguments
                               (list :nargs nargs
                                     :fn fn)
                               frame-ptr)
                       (%error 'too-many-arguments
                               (list :nargs nargs
                                     :fn fn)
                               frame-ptr))))
                  ((= op1 #xc3)         ;array rank
                   (%err-disp-internal $XNDIMS
                                       (list (encoded-gpr-lisp xp (ldb (byte 4 4) op2))
                                             (encoded-gpr-lisp xp (ldb (byte 4 0) op2)))
                                       frame-ptr))
                  ((= op1 #xc6)
                   (%error (make-condition 'type-error
                                           :datum (encoded-gpr-lisp xp x8632::temp0)
                                           :expected-type '(or symbol function)
                                           :format-control
                                           "~S is not of type ~S, and can't be FUNCALLed or APPLYed")
                           nil frame-ptr))
                  ((= op1 #xc7)
                   (handle-udf-call xp frame-ptr)
                   (setq skip 0))
                  ((or (= op1 #xc8) (= op1 #xcb))
                   (setq skip 3)
                   (%error (%rsc-string $xarroob)
                           (list (encoded-gpr-lisp xp (ldb (byte 4 4) op2))
                                 (encoded-gpr-lisp xp (ldb (byte 4 0) op2)))
                           frame-ptr))
                  ((= op1 #xc9)
                   (%err-disp-internal $xnotfun
                                       (list (encoded-gpr-lisp xp x8632::temp0))
                                       frame-ptr))
                  ;; #xca = uuo-error-debug-trap
                  ((= op1 #xcc)
                   ;; external entry point or foreign variable
                   (setq skip 3)
                   (let* ((eep-or-fv (encoded-gpr-lisp xp (ldb (byte 4 4) op2))))
                     (etypecase eep-or-fv
                       (external-entry-point
                        (resolve-eep eep-or-fv)
                        (setf (encoded-gpr-lisp xp (ldb (byte 4 0) op2))
                              (eep.address eep-or-fv)))
                       (foreign-variable
                        (resolve-foreign-variable eep-or-fv)
                        (setf (encoded-gpr-lisp xp (ldb (byte 4 0) op2))
                              (fv.addr eep-or-fv))))))
                  ((< op1 #xe0)
                   (setq skip 3)
                   (if (= op2 x8632::subtag-catch-frame)
                     (%error (make-condition 'cant-throw-error
                                             :tag (encoded-gpr-lisp
                                                   xp
                                                   (ldb (byte 4 0) op1)))
                             nil frame-ptr)
                     (let* ((typename
                             (cond ((= op2 x8632::tag-fixnum) 'fixnum)
                                   ((= op2 x8632::subtag-character) 'character)
                                   ((= op2 x8632::fulltag-cons) 'cons)
                                   ((= op2 x8632::tag-misc) 'uvector)
				   (t (let* ((class (logand op2 x8632::fulltagmask))
                                             (high5 (ash op2 (- x8632::ntagbits))))
                                        (cond ((= class x8632::fulltag-nodeheader)
                                               (svref *nodeheader-types* high5))
                                              ((= class x8632::fulltag-immheader)
                                               (svref *immheader-types* high5))
                                              (t (list 'bogus op2))))))))
                       (%error (make-condition 'type-error
                                               :datum (encoded-gpr-lisp
                                                       xp
                                                       (ldb (byte 4 0) op1))
                                               :expected-type typename)
                               nil
                               frame-ptr))))
                  ((< op1 #xf0)
                   (%error (make-condition 'type-error
                                           :datum (encoded-gpr-lisp
                                                   xp
                                                   (ldb (byte 4 0) op1))
                                           :expected-type 'list)
                           nil
                           frame-ptr))
                  (t
                   (%error (make-condition 'type-error
                                           :datum (encoded-gpr-lisp
                                                   xp
                                                   (ldb (byte 4 0) op1))
                                           :expected-type 'fixnum)
                           nil
                           frame-ptr)))
            (%error "Unknown trap: #x~x~%xp=~s"
                    (list (list op0 op1 op2) xp)
                    frame-ptr))
          skip))))
                 
