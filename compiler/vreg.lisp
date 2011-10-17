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

(def-standard-initial-binding *lreg-freelist* (%cons-pool))

  
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
  (conflicts () :type list)             ; other lregs which can't map to the same physical reg
  (wired t :type boolean)               ; when true, targeted value must be preserved.
  (info nil)				; Whatever; used in printing.
)

(defun free-lreg (l)
  (without-interrupts                   ; explicitly
   (let* ((p *lreg-freelist*))
     (setf (lreg-value l) (pool.data p)
           (pool.data p) l)
     nil)))

(defun alloc-lreg ()
  (let* ((p *lreg-freelist*))
    (without-interrupts 
     (let* ((l (pool.data p)))
       (when l 
         (setf (pool.data p) (lreg-value l))
         (setf (lreg-defs l) nil
               (lreg-refs l) nil
               (lreg-conflicts l) nil
               (lreg-id l) (incf *logical-register-counter*)
               (lreg-wired l) t)
         l)))))

(defun make-lreg (value class mode type wired)
  (let* ((l (alloc-lreg)))
    (cond (l
           (setf (lreg-value l) value
                 (lreg-class l) class
                 (lreg-type l) type
                 (lreg-mode l) mode
                 (lreg-wired l) wired)           
           l)
          (t (%make-lreg :value value :class class :type type :mode mode :wired wired)))))
 

(defun print-lreg (l s d)
  (declare (ignore d))
  (print-unreadable-object (l s :type t)
    (format s "~d" (lreg-id l))
    (let* ((value (lreg-value l))
           (class (lreg-class l))
	   (mode-name (if (eq class hard-reg-class-gpr)
			(car (rassoc (lreg-mode l) *mode-name-value-alist*))
                        (if (eq class hard-reg-class-fpr)
                          (case (lreg-mode l)
                            (#.hard-reg-class-fpr-mode-double "DOUBLE")
                            (#.hard-reg-class-fpr-mode-single "SINGLE"))))))
      (format s " ~a "
              (case class
                (#.hard-reg-class-fpr "FPR")
                (#.hard-reg-class-gpr "GPR")
                (#.hard-reg-class-crf "CRF")
                (t  (format nil "class ~d" class))))
      (if value
        (progn
          (format s (if (lreg-wired l) "[~s]" "{~s}") value)
          (when mode-name
            (format s "/~a" mode-name)))
	(progn
	  (if mode-name
	    (format s "{?/~a}" mode-name)
	    (format s "{?}")))))))

(def-standard-initial-binding *lcell-freelist* (%cons-pool))
(defvar *next-lcell-id* -1)

(defstruct (lcell 
            (:print-function print-lcell)
            (:constructor %make-lcell (kind parent width attributes info)))
  (kind :node)         ; for printing
  (id (incf (the fixnum *next-lcell-id*)) :type fixnum)                          ; 
  (parent nil)                          ; backpointer to unique parent
  (children nil)                        ; list of children
  (width 4)                             ; size in bytes or NIL if deleted
  (offset nil)                          ; sum of ancestor's widths or 0, NIL if deleted
  (refs nil)                            ; vinsns which load/store into this cell
  (attributes 0 :type fixnum)           ; bitmask
  (info nil))                           ; whatever

(defun print-lcell (c s d)
  (declare (ignore d))
  (print-unreadable-object (c s :type t)
    (format s "~d" (lcell-id c))
    (let* ((offset (lcell-offset c)))
      (when offset
        (format s "@#x~x" offset)))))

(defun free-lcell (c)
  (without-interrupts                   ; explicitly
   (let* ((p *lcell-freelist*))
     (setf (lcell-kind c) (pool.data p)
           (pool.data p) c)
     nil)))

(defun alloc-lcell (kind parent width attributes info)
  (let* ((p *lcell-freelist*))
    (without-interrupts 
     (let* ((c (pool.data p)))
       (when c 
         (setf (pool.data p) (lcell-kind c))
         (setf (lcell-kind c) kind
               (lcell-parent c) parent
               (lcell-width c) width
               (lcell-attributes c) (the fixnum attributes)
               (lcell-info c) info
               (lcell-offset c) nil
               (lcell-refs c) nil
               (lcell-children c) nil
               (lcell-id c) (incf *next-lcell-id*))
         c)))))

(defun make-lcell (kind parent width attributes info)
  (let* ((c (or (alloc-lcell kind parent width attributes info)
                (%make-lcell kind parent width attributes info))))
    (when parent (push c (lcell-children parent)))
    c))
 
; Recursively calculate, but don't cache (or pay attention to previously calculated offsets) 
(defun calc-lcell-offset (c)
  (if c
    (let* ((p (lcell-parent c)))
      (if (null p)
        0
        (+ (calc-lcell-offset p) (or (lcell-width p) 0))))
    0))

; A cell's "depth" is its offset + its width
(defun calc-lcell-depth (c)
  (if c 
    (+ (calc-lcell-offset c) (or (lcell-width c) 0))
    0))

; I don't know why "compute" means "memoize", but it does.
(defun compute-lcell-offset (c)
  (or (lcell-offset c)
      (setf (lcell-offset c)
            (let* ((p (lcell-parent c)))
              (if (null p)
                0
                (+ (compute-lcell-offset p) (or (lcell-width p) 0)))))))

(defun compute-lcell-depth (c)
  (if c
    (+ (compute-lcell-offset c) (or (lcell-width c) 0))
    0))



                    

(defparameter *spec-class-storage-class-alist*
  `((:lisp . ,arch::storage-class-lisp)
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
    (:lcell . ,(specifier-type 'lcell))))

(defun match-vreg-value (vreg value)
  (declare (ignorable vreg value))      ; at least until this -does- something.
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
                                                        (if (eql (get-regspec-mode fpr)
                                                                 hard-reg-class-fpr-mode-single)
                                                          :single-float
                                                          :double-float)))))

(defun note-vinsn-refs-gpr (vinsn gpr)
  (when (and (fboundp 'vinsn-gprs-read)
             (> (uvsize vinsn) 8))
    (setf (vinsn-gprs-read vinsn) (logior (vinsn-gprs-read vinsn) (ash 1 gpr)))))

(defun note-vinsn-refs-fpr-lreg (vinsn fpr)
  (when (and (fboundp 'vinsn-gprs-read)
             (> (uvsize vinsn) 8))
    (setf (vinsn-fprs-read vinsn) (logior (vinsn-fprs-read vinsn)
                                       (target-fpr-mask (hard-regspec-value fpr)
                                                        (if (eql (get-regspec-mode fpr)
                                                                 hard-reg-class-fpr-mode-single)
                                                          :single-float
                                                          :double-float))))))


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
            (setq vreg vreg-value)
            (if (typep vreg 'lreg)
              (if result-p
                (pushnew vinsn (lreg-defs vreg))
                (pushnew vinsn (lreg-refs vreg)))
              (error "Bad vreg: ~s" vreg)))
	  (when vreg-value
	    (case class
	      (:crf (use-crf-temp vreg-value))
	      ((:u8 :s8 :u16 :s16 :u32 :s32 :u64 :s64 :address)
	       (if result-p
                 (note-vinsn-sets-gpr vinsn vreg-value)
                 (note-vinsn-refs-gpr vinsn vreg-value))
	       (use-imm-temp vreg-value))
	      ((:single-float :double-float)
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
    (when (typep vreg 'lcell)
      (pushnew vinsn (lcell-refs vreg)))
    vreg))

(defun note-lreg-conflict (lreg conflicts-with)
  (and (typep lreg 'lreg)
       (typep conflicts-with 'lreg)
       (pushnew conflicts-with (lreg-conflicts lreg))
       (pushnew lreg (lreg-conflicts conflicts-with))
       t))

(ccl::provide "VREG")
