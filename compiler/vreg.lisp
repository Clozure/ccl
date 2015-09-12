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


(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl::require "ARCH"))

(in-package "CCL")

(defvar *logical-register-counter* -1)


  
(defstruct (lreg
            (:print-function print-lreg)
            (:constructor %make-lreg))
  (value nil :type t)                   ; physical reg or frame address or ...
  (id (incf (the fixnum *logical-register-counter*)) :type fixnum)                   ; for printing
  (class 0 :type fixnum)                ; target storage class: GPR, FPR, CRF ...
  (mode 0 :type fixnum)                 ; mode (:u8, :address, etc)
  (type 0 :type fixnum)                 ; type
  (defs () :type list)                  ; list of vinsns which assign to this reg
  (refs () :type list)                  ; list of vinsns which reference this vreg
  (local-p nil)
  (wired t :type boolean)               ; when true, targeted value must be preserved.
  (flags 0 :type fixnum)				;
  (spill-offset nil :type (or null fixnum))
  (interval nil)
  
)

(defconstant lreg-spill-bit 0)
(defconstant lreg-flag-spill (ash 1 lreg-spill-bit))
(defconstant lreg-pre-spill-bit 1) ; incoming arg
(defconstant lreg-flag-pre-spill (ash 1 lreg-pre-spill-bit))

(defun spilled-lreg-p (x) (if (typep x 'lreg) (logbitp lreg-spill-bit (lreg-flags x))))

(defun free-lreg (l)
  (declare (ignore l)))



(defun make-lreg (value class mode type wired)
  (%make-lreg :value value :class class :type type :mode mode :wired wired))

(defun make-wired-lreg (value &key
                              local-p
			      (class (hard-regspec-class value))
			      (mode (get-regspec-mode value))
			      (type (get-node-regspec-type-modes value)))
  (let* ((vinsns *vinsn-list*)
         (lreg  (%make-lreg :value  (hard-regspec-value value)
                            :id (length (vinsn-list-lregs vinsns))
                            :class class
                            :mode mode
                            :type type
                            :wired t
                            :local-p local-p)))

    (if vinsns
      (vector-push-extend lreg (vinsn-list-lregs vinsns))
      (error "no vinsns"))
    lreg))

(defun print-lreg (l s d)
  (declare (ignore d))
 (print-unreadable-object (l s :type t)
    (format s "~d" (lreg-id l))
    (let* ((spilled (logbitp lreg-spill-bit (lreg-flags l)))
           (value (if spilled (lreg-spill-offset L)(lreg-value l)))
           (class (lreg-class l))
	   (mode-name (if (eq class hard-reg-class-gpr)
			(car (rassoc (lreg-mode l) *mode-name-value-alist*))
                        (if (eq class hard-reg-class-fpr)
                          (string (fpr-mode-value-name (lreg-mode l)))))))
      (format s " ~a "
              (case class
                (#.hard-reg-class-fpr "FPR")
                (#.hard-reg-class-gpr "GPR")
                (#.hard-reg-class-crf "CRF")
                (t  (format nil "class ~d" class))))
      (if value
        (progn
          (if spilled
            (format s "@ ~s" value)
            (format s (if (lreg-wired l) "[~s]" "{~s}") value))
          (when mode-name
            (format s "/~a" mode-name)))
	(progn
	  (if mode-name
	    (format s "{?/~a}" mode-name)
	    (format s "{?}")))))))



                    

(defparameter *spec-class-storage-class-alist*
  `((:lisp . ,arch::storage-class-lisp)
    (:lisp-lreg  . ,arch::storage-class-lisp)
    (:imm . ,arch::storage-class-imm)
    (:wordptr . ,arch::storage-class-wordptr)
    (:u8 . ,arch::storage-class-u8)
    (:s8 . ,arch::storage-class-s8)
    (:u16 . ,arch::storage-class-u16)
    (:s16 . ,arch::storage-class-s16)
    (:u32 . ,arch::storage-class-u32)
    (:s32 . ,arch::storage-class-s32)
    (:u64 . ,arch::storage-class-u64)
    (:s64 . ,arch::storage-class-s64)
    (:address . ,arch::storage-class-address)
    (:single-float . ,arch::storage-class-single-float)
    (:double-float . ,arch::storage-class-double-float)
    (:pc . ,arch::storage-class-pc)
    (:locative . ,arch::storage-class-locative)
    (:crf . ,arch::storage-class-crf)
    (:crbit . ,arch::storage-class-crbit)
    (:crfbit . ,arch::storage-class-crfbit)
    (:complex-double-float . ,arch::storage-class-complex-double-float)
    (:complex-single-float . ,arch::storage-class-complex-double-float)
    (t . nil)))
    
(defun spec-class->storage-class (class-name)
  (or (cdr (assoc class-name *spec-class-storage-class-alist* :test #'eq))
      (error "Unknown storage-class specifier: ~s" class-name)))
   
(defun vreg-ok-for-storage-class (vreg sclass)
  (declare (ignore vreg sclass))
  t)



(defparameter *vreg-specifier-constant-constraints*
  `((:u8const . ,(specifier-type '(unsigned-byte 8)))
    (:u16const . ,(specifier-type '(unsigned-byte 16)))
    (:u32const . ,(specifier-type '(unsigned-byte 32)))
    (:u64const . ,(specifier-type '(unsigned-byte 64)))
    (:s8const . ,(specifier-type '(signed-byte 8)))
    (:s16const . ,(specifier-type '(signed-byte 16)))
    (:s32const . ,(specifier-type '(signed-byte 32)))
    (:s64const . ,(specifier-type '(signed-byte 64)))
    (:stack-offset . ,(specifier-type '(signed-byte 32)))))

(defun match-vreg-value (vreg value)  (declare (ignorable vreg value))      ; at least until this -does- something.
  ;(format t "~&vreg = ~s, value = ~s" vreg value)
  t)

(defun match-vreg-constraint (constraint vreg template valvect n)
  (let* ((res&args (vinsn-template-results&args template))
         (target (cadr constraint))
         (matchspec (assq target res&args))
         (matchpos (if matchspec (position matchspec res&args))))
    (unless matchpos
      (warn "Unknown template vreg name ~s in constraint ~s." target constraint))
    (unless (< matchpos n)
      (warn "Forward-referenced vreg name ~s in constraint ~s." target constraint))
    (let* ((target-val (svref valvect matchpos)))
      (unless (ecase (car constraint) (:eq (eq vreg target-val)) (:ne (neq vreg target-val)))
        (warn "~& use of vreg ~s conflicts with value already assigned ~
               to ~s wrt constraint ~s ." vreg (car matchspec) constraint)))))

(defun note-vinsn-sets-gpr (vinsn gpr)
  (setf (vinsn-gprs-set vinsn) (logior (vinsn-gprs-set vinsn) (ash 1 gpr))))

(defun note-vinsn-sets-fpr-lreg (vinsn fpr)
  (setf (vinsn-fprs-set vinsn) (logior (vinsn-fprs-set vinsn)
                                       (target-fpr-mask (hard-regspec-value fpr)
                                                        (get-regspec-mode fpr)))))

(defun note-vinsn-refs-gpr (vinsn gpr)
  (when (and (fboundp 'vinsn-gprs-read)
             (> (uvsize vinsn) 8))
    (setf (vinsn-gprs-read vinsn) (logior (vinsn-gprs-read vinsn) (ash 1 gpr)))))

(defun note-vinsn-refs-fpr-lreg (vinsn fpr)
  (when (and (fboundp 'vinsn-gprs-read)
             (> (uvsize vinsn) 8))
    (setf (vinsn-fprs-read vinsn) (logior (vinsn-fprs-read vinsn)
                                       (target-fpr-mask (hard-regspec-value fpr)
                                                        (get-regspec-mode fpr))))))

(defparameter *check-vregs* nil)

(defun match-vreg (vreg spec vinsn vp n)
  (declare (fixnum n))
  (let* ((class (if (atom spec) spec (car spec)))
         (value (if (atom spec) nil (cadr spec)))
         (template (vinsn-template vinsn))
         (result-p (< n (the fixnum (length (vinsn-template-result-vreg-specs template))))))
    (let* ((spec-class (assoc class *spec-class-storage-class-alist* :test #'eq)))
      (if spec-class
        (let* ((vreg-value (hard-regspec-value vreg)))
          (if (typep vreg 'fixnum) 
            (progn
              (when *check-vregs* (format t "~& vinsn ~s got fixnum for ~s" (vinsn-template-name template) class))
              (setq vreg vreg-value))

            (if (typep vreg 'lreg)
              (unless (or (vinsn-attribute-p vinsn :spill) (vinsn-attribute-p vinsn :reload))
                (if result-p
                  (pushnew vinsn (lreg-defs vreg))
                  (pushnew vinsn (lreg-refs vreg))))
              (error "Bad vreg: ~s" vreg)))
	  (when vreg-value
	    (case class
	      (:crf (use-crf-temp vreg-value))
	      ((:u8 :s8 :u16 :s16 :u32 :s32 :u64 :s64 :address)
	       (if result-p
                 (note-vinsn-sets-gpr vinsn vreg-value)
                 (note-vinsn-refs-gpr vinsn vreg-value))
	       (use-imm-temp vreg-value))
	      ((:single-float :double-float :complex-single-float :complex-double-float)
	       (use-fp-reg vreg)
	       (if result-p
                 (note-vinsn-sets-fpr-lreg vinsn vreg)
                 (note-vinsn-refs-fpr-lreg vinsn vreg)))
	      ((:imm t)
	       (if result-p
                 (note-vinsn-sets-gpr vinsn vreg-value)
                 (note-vinsn-refs-gpr vinsn vreg-value))
	       (if (logbitp vreg-value *backend-imm-temps*)
		 (use-imm-temp vreg-value)
		 (use-node-temp vreg-value)))
	      (:lisp
	       (use-node-temp vreg-value)
	       (if result-p
                 (note-vinsn-sets-gpr vinsn vreg-value)
                 (note-vinsn-refs-gpr vinsn vreg-value)))
              (:extended)))
          (unless (or (eq class 't) (vreg-ok-for-storage-class vreg class))
            (warn "~s was expected to have storage class matching specifier ~s" vreg class))
          (when value
            (if (atom value)
              (match-vreg-value vreg-value value)
              (match-vreg-constraint value vreg-value template vp n))))
        (if (eq class :label)
          (progn
            (unless (typep vreg 'vinsn-label)
              (error "Label expected, found ~s." vreg))
            (push vinsn (vinsn-label-refs vreg)))
          (let* ((ctype (cdr (assoc class *vreg-specifier-constant-constraints* :test #'eq))))
            (unless ctype (error "Unknown vreg constraint : ~s ." class))
            (unless (ctypep vreg ctype)
              (error "~S : value doesn't match constraint ~s in template for ~s ." vreg class (vinsn-template-name template)))))))
    vreg))



(ccl::provide "VREG")
