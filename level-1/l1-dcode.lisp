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


(in-package "CCL")






(defun %make-gf-instance (class &key
                                name
                                (method-combination *standard-method-combination* mcomb-p)
                                (method-class *standard-method-class* mclass-p)
                                declarations
                                (lambda-list nil ll-p)
                                (argument-precedence-order nil apo-p)
                                &allow-other-keys)
  (when mcomb-p
    (unless (typep method-combination 'method-combination)
      (report-bad-arg method-combination 'method-combination)))
  (when mclass-p
    (if (symbolp method-class)
      (setq method-class (find-class method-class)))
    (unless (subtypep method-class *method-class*)
      (error "~s is not a subtype of ~s." method-class *method-class*)))
  (when declarations
    (unless (list-length declarations)
      (error "~s is not a proper list" declarations)))
  ;; Fix APO, lambda-list
  (if apo-p
    (if (not ll-p)
      (error "Cannot specify ~s without specifying ~s" :argument-precedence-order
	     :lambda-list)))
  (let* ((gf (%allocate-gf-instance class)))
    (setf (sgf.name gf) name
          (sgf.method-combination gf) method-combination
          (sgf.methods gf) nil
          (sgf.method-class gf) method-class
          (sgf.decls gf) declarations
          (sgf.%lambda-list gf) :unspecified
	  (sgf.dependents gf) nil)
    (when ll-p
      (if apo-p
        (set-gf-arg-info gf :lambda-list lambda-list
                         :argument-precedence-order argument-precedence-order)
        (set-gf-arg-info gf :lambda-list lambda-list)))
    gf))

(defun gf-arg-info-valid-p (gf)
  (let* ((bits (lfun-bits gf)))
    (declare (fixnum bits))
    (not (and (logbitp $lfbits-aok-bit bits)
	      (not (logbitp $lfbits-keys-bit bits))))))

;;; Derive a GF lambda list from the method's lambda list.
(defun flatten-method-lambda-list (lambda-list)
  (collect ((ll))
    (dolist (x lambda-list (ll))
      (if (atom x)
        (if (eq x '&aux)
          (return (ll))
          (ll x))
        (ll (car x))))))
          
(defun %maybe-compute-gf-lambda-list (gf method)
  (let* ((gf-ll (sgf.%lambda-list gf)))
    (if (eq gf-ll :unspecified)
      (and method
           (let* ((method-lambda-list (%method-lambda-list method))
                  (method-has-&key (member '&key method-lambda-list))
                  (method-has-&allow-other-keys
                   (member '&allow-other-keys method-lambda-list)))
             (if method-has-&key
               (nconc (ldiff method-lambda-list (cdr method-has-&key))
                      (if method-has-&allow-other-keys
                        '(&allow-other-keys)))
               (flatten-method-lambda-list method-lambda-list))))
      gf-ll)))
             
             
;;; Borrowed from PCL, sort of.  We can encode required/optional/restp/keyp
;;; information in the gf's lfun-bits
(defun set-gf-arg-info (gf &key new-method (lambda-list nil lambda-list-p)
                           (argument-precedence-order nil apo-p))
  (let* ((methods (%gf-methods gf))
         (dt (%gf-dispatch-table gf))
         (gf-lfun-bits (lfun-bits gf))
         (first-method-p (and new-method (null methods))))
    (declare (fixnum gf-lfun-bits))
    (unless lambda-list-p
      (setq lambda-list
            (%maybe-compute-gf-lambda-list gf (or (car (last methods))
                                                  new-method))))
    (when (or lambda-list-p
              (and first-method-p
                   (eq (%gf-%lambda-list gf) :unspecified)))
      (multiple-value-bind (newbits keyvect)
          (encode-lambda-list lambda-list t)
        (declare (fixnum newbits))
        (when (and methods (not first-method-p))
          (unless (and (= (ldb $lfbits-numreq gf-lfun-bits)
                          (ldb $lfbits-numreq newbits))
                       (= (ldb $lfbits-numopt gf-lfun-bits)
                          (ldb $lfbits-numopt newbits))
                       (eq (or (logbitp $lfbits-keys-bit gf-lfun-bits)
                               (logbitp $lfbits-rest-bit gf-lfun-bits)
                               (logbitp $lfbits-restv-bit gf-lfun-bits))
                           (or (logbitp $lfbits-keys-bit newbits)
                               (logbitp $lfbits-rest-bit newbits)
                               (logbitp $lfbits-restv-bit newbits))))
            (cerror (format nil
                            "Remove ~d method~:p from the generic-function and ~
                             change its lambda list."
                            (length (%gf-methods gf)))
                    "New lambda list of generic function ~s is not congruent ~
                     with lambda lists of existing methods.~%~
                     Generic-function's   : ~s~%~
                     Method's lambda-list : ~s~%"
                    gf lambda-list (%method-lambda-list (car methods)))
            (loop
               (let ((methods (%gf-methods gf)))
                 (if methods
                     (remove-method gf (car methods))
                     (return))))
            (%set-defgeneric-keys gf nil)))
        (when lambda-list-p
          (setf (%gf-%lambda-list gf) lambda-list
                (%gf-dispatch-table-keyvect dt) keyvect))
        (when (and apo-p lambda-list-p)
          (let* ((old-precedence-list (%gf-dispatch-table-precedence-list dt)))
            (setf (%gf-dispatch-table-precedence-list dt)
                  (canonicalize-argument-precedence-order
                   argument-precedence-order
                   (required-lambda-list-args lambda-list)))
            (unless (equal old-precedence-list
                           (%gf-dispatch-table-precedence-list dt))
              (clear-gf-dispatch-table dt))))
        (lfun-bits gf (logior (ash 1 $lfbits-gfn-bit)
                              (logand $lfbits-args-mask newbits)))))
    (when new-method
      (check-defmethod-congruency gf new-method))))
        
(defun %gf-name (gf &optional (new-name nil new-name-p))
  (let* ((old-name (%standard-generic-function-instance-location-access
                    gf sgf.name)))
    (if new-name-p
      (setf (sgf.name gf) new-name))
    (unless (eq old-name (%slot-unbound-marker))
      old-name)))



	     
(defun make-n+1th-arg-combined-method (methods gf argnum)
  (let ((table (make-gf-dispatch-table)))
    (setf (%gf-dispatch-table-methods table) methods
          (%gf-dispatch-table-argnum table) (%i+ 1 argnum))
    (let ((self (%cons-combined-method gf table #'%%nth-arg-dcode))) ; <<
      (setf (%gf-dispatch-table-gf table) self)
      self)))

;;; Bring the generic function to the smallest possible size by removing
;;; any cached recomputable info.  Currently this means clearing out the
;;; combined methods from the dispatch table.

(defun clear-gf-cache (gf)
  #-bccl (unless t (typep gf 'standard-generic-function) 
           (report-bad-arg gf 'standard-generic-function))
  (let ((dt (%gf-dispatch-table gf)))
    (if (eq (%gf-dispatch-table-size dt) *min-gf-dispatch-table-size*)
      (clear-gf-dispatch-table dt)
      (let ((new (make-gf-dispatch-table)))
        (setf (%gf-dispatch-table-methods new) (%gf-dispatch-table-methods dt))
        (setf (%gf-dispatch-table-precedence-list new)
              (%gf-dispatch-table-precedence-list dt))
        (setf (%gf-dispatch-table-gf new) gf)
        (setf (%gf-dispatch-table-keyvect new)
              (%gf-dispatch-table-keyvect dt))
        (setf (%gf-dispatch-table-argnum new) (%gf-dispatch-table-argnum dt))
        (setf (%gf-dispatch-table gf) new)))))

(defun %gf-dispatch-table-store-conditional (dt index new)
  "Returns T if the new value can be stored in DT at INDEX, replacing a NIL.
   Returns NIL - without storing anything - if the value already in DT
   at INDEX is non-NIL at the time of the store."
  (let ((offset (+ (ash (%i+ index %gf-dispatch-table-first-data)
                        target::word-shift)
                   target::misc-data-offset)))
    (or (%store-node-conditional offset dt nil new)
        (%store-node-conditional offset dt *gf-dispatch-bug* new))))

(defun grow-gf-dispatch-table (gf-or-cm wrapper table-entry &optional obsolete-wrappers-p)
  ;; Grow the table associated with gf and insert table-entry as the value for
  ;; wrapper.  Wrapper is a class-wrapper.  Assumes that it is not obsolete.
  (let* ((dt (if (generic-function-p gf-or-cm)
               (%gf-dispatch-table gf-or-cm)
               (%combined-method-methods gf-or-cm)))
         (size (%gf-dispatch-table-size dt))
         (new-size (if obsolete-wrappers-p
                     size
                     (%i+ size size)))
         new-dt)
    (if (> new-size *max-gf-dispatch-table-size*)
      (progn 
        (setq new-dt (clear-gf-dispatch-table dt)
                   *gf-dt-ovf-cnt* (%i+ *gf-dt-ovf-cnt* 1)))
      (progn
        (setq new-dt (make-gf-dispatch-table new-size))
        (setf (%gf-dispatch-table-methods new-dt) (%gf-dispatch-table-methods dt)
              (%gf-dispatch-table-precedence-list new-dt) (%gf-dispatch-table-precedence-list dt)
              (%gf-dispatch-table-keyvect new-dt) (%gf-dispatch-table-keyvect dt)
              (%gf-dispatch-table-gf new-dt) gf-or-cm
              (%gf-dispatch-table-argnum new-dt) (%gf-dispatch-table-argnum dt))
        (let ((i 0) index w cm)
          (dotimes (j (%ilsr 1 (%gf-dispatch-table-size dt)))
	    (declare (fixnum j))
            (unless (or (null (setq w (%gf-dispatch-table-ref dt i)))
                        (eql 0 (%wrapper-hash-index w))
                        (no-applicable-method-cm-p
                         (setq cm (%gf-dispatch-table-ref dt (%i+ i 1)))))
              (setq index (find-gf-dispatch-table-index new-dt w t))
              (setf (%gf-dispatch-table-ref new-dt index) w)
              (setf (%gf-dispatch-table-ref new-dt (%i+ index 1)) cm))
            (setq i (%i+ i 2))))))
    (let ((index (find-gf-dispatch-table-index new-dt wrapper t)))
      (setf (%gf-dispatch-table-ref new-dt index) wrapper)
      (setf (%gf-dispatch-table-ref new-dt (%i+ index 1)) table-entry))
    (if (generic-function-p gf-or-cm)
      (setf (%gf-dispatch-table gf-or-cm) new-dt)
      (setf (%combined-method-methods gf-or-cm) new-dt))))


(defun inner-lfun-bits (function &optional value)
  (lfun-bits (closure-function function) value))



;;; probably want to use alists vs. hash-tables initially


;;; only used if error - well not really
(defun collect-lexpr-args (args first &optional last) 
  (if (listp args)
    (subseq args first (or last (length args)))
    (let ((res nil))
      (when (not last)(setq last (%lexpr-count args)))
      (dotimes (i (- last first))
        (setq res (push (%lexpr-ref args last (+ first i)) res)))
      (nreverse res))))




(defmacro with-list-from-lexpr ((list lexpr) &body body)
  (let ((len (gensym)))
    `(let* ((,len (%lexpr-count ,lexpr))
            (,list  (make-list ,len)))
       (declare (dynamic-extent ,list) (fixnum ,len))       
       (do* ((i 0 (1+ i))
             (ls ,list (cdr ls)))
            ((= i ,len) ,list)
         (declare (fixnum i) (list ls))
         (declare (optimize (speed 3)(safety 0)))
         (%rplaca ls (%lexpr-ref ,lexpr ,len i)))
       ,@body)))



(defmacro %standard-instance-p (i)
  `(eq (typecode ,i) ,(type-keyword-code :instance)))



(declaim (inline %find-1st-arg-combined-method))
(declaim (inline %find-nth-arg-combined-method))




(defun %find-1st-arg-combined-method (dt arg)
  (let ((wrapper (instance-class-wrapper arg)))
    (when (eql 0 (%wrapper-hash-index wrapper))
      (update-obsolete-instance arg)
      (setq wrapper (instance-class-wrapper arg)))
    (let* ((mask (%gf-dispatch-table-mask dt))
           (index (%ilsl 1 (%ilogand mask (%wrapper-hash-index wrapper))))
           table-wrapper flag)
      (declare (fixnum index mask))
      (loop 
        (if (eq (setq table-wrapper (%gf-dispatch-table-ref dt index)) wrapper)
          (return (%gf-dispatch-table-ref dt  (the fixnum (1+ index))))
          (progn
            (when (null (%gf-dispatch-table-ref dt (the fixnum (1+ index))))
              (if (or (neq table-wrapper (%unbound-marker))
                      (eql 0 flag))
                (without-interrupts     ; why?
                 (return (1st-arg-combined-method-trap (%gf-dispatch-table-gf dt) wrapper arg))) ; the only difference?
                (setq flag 0 index -2)))
            (setq index (+ 2 index))))))))

;;; for calls from outside - e.g. stream-reader
(defun find-1st-arg-combined-method (gf arg)
  (declare (optimize (speed 3)(safety 0)))
  (%find-1st-arg-combined-method (%gf-dispatch-table gf) arg))


;;; more PC - it it possible one needs to go round more than once? -
;;; seems unlikely
(defun %find-nth-arg-combined-method (dt arg args)  
  (declare (optimize (speed 3)(safety 0)))
  (let ((wrapper (instance-class-wrapper arg)))
    (when (eql 0 (%wrapper-hash-index wrapper))
      (update-obsolete-instance arg)
      (setq wrapper (instance-class-wrapper arg)))
    (let* ((mask (%gf-dispatch-table-mask dt))
           (index (%ilsl 1 (%ilogand mask (%wrapper-hash-index wrapper))))
           table-wrapper flag)
      (declare (fixnum index mask))
      (loop 
        (if (eq (setq table-wrapper (%gf-dispatch-table-ref dt index)) wrapper)
          (return (%gf-dispatch-table-ref dt (the fixnum (1+ index))))
          (progn
            (when (null (%gf-dispatch-table-ref dt (the fixnum (1+ index))))
              (if (or (neq table-wrapper (%unbound-marker))
                      (eql 0 flag))
                (without-interrupts     ; why?
                 (let ((gf (%gf-dispatch-table-gf dt)))
                   (if (listp args)
                     (return (nth-arg-combined-method-trap-0 gf dt wrapper args))
                     (with-list-from-lexpr (args-list args)
                       (return (nth-arg-combined-method-trap-0 gf dt wrapper args-list))))))
                (setq flag 0 index -2)))
            (setq index (+ 2 index))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;; Generic functions and methods ;;;;;;;;;;;;;;;;;;;;
(defun %class-cpl (class)
  (if (%standard-instance-p class)
    (%class.cpl class)
    (or
     (and (typep class 'macptr)
	  (let* ((slots (foreign-slots-vector class)))
	    (and slots (%slot-ref slots %class.cpl))))
     (error "Can't determine CPL of class ~s" class))))


(defun standard-method-p (thing)
  (when (%standard-instance-p thing)
    (let* ((cpl (%class-cpl (%wrapper-class (instance.class-wrapper thing))))
           (smc *standard-method-class*))
      (dolist (c cpl)
        (if (eq c smc)(return t))))))



(defun %method-function-p (thing)
  (when (functionp thing)
    (let ((bits (lfun-bits thing)))
      (declare (fixnum bits))
      (logbitp $lfbits-method-bit bits))))




(setf (type-predicate 'standard-generic-function) 'standard-generic-function-p)
(setf (type-predicate 'combined-method) 'combined-method-p)

(setf (type-predicate 'standard-method) 'standard-method-p)

;; Maybe we shouldn't make this a real type...
(setf (type-predicate 'method-function) '%method-function-p)


(defvar %all-gfs% (%cons-population nil))


(eval-when (:compile-toplevel :execute)
(defconstant $lfbits-numinh-mask (logior (dpb -1 $lfbits-numinh 0)
                                         (%ilsl $lfbits-nonnullenv-bit 1)))
)


#+ppc-target
(defvar *fi-trampoline-code* (uvref #'funcallable-trampoline 0))


#+ppc-target
(defvar *unset-fin-code* (uvref #'unset-fin-trampoline 0))



#+ppc-target
(defvar *gf-proto-code* (uvref *gf-proto* 0))

;;; The "early" version of %ALLOCATE-GF-INSTANCE.
(setf (fdefinition '%allocate-gf-instance)
      #'(lambda (class)
	  (declare (ignorable class))
	  (setq class *standard-generic-function-class*)
	  (let* ((wrapper (%class.own-wrapper class))
		 (len (length #.(%wrapper-instance-slots (class-own-wrapper
							  *standard-generic-function-class*))))
		 (dt (make-gf-dispatch-table))
		 (slots (allocate-typed-vector :slot-vector (1+ len) (%slot-unbound-marker)))
		 (fn #+ppc-target
                   (gvector :function
			      *gf-proto-code*
			      wrapper
			      slots
			      dt
			      #'%%0-arg-dcode
			      0
			      (%ilogior (%ilsl $lfbits-gfn-bit 1)
					(%ilogand $lfbits-args-mask 0)))
                   #+x86-target
                   (%clone-x86-function *gf-proto*
                                        wrapper
                                        slots
                                        dt
                                        #'%%0-arg-dcode
                                        0
                                        (%ilogior (%ilsl $lfbits-gfn-bit 1)
                                                  (%ilogand $lfbits-args-mask 0)))))
	    (setf ;(gf.hash fn) (strip-tag-to-fixnum fn)
		  (slot-vector.instance slots) fn
		  (%gf-dispatch-table-gf dt) fn)
	    (push fn (population.data %all-gfs%))
	    fn)))






  


(defparameter *gf-proto-one-arg*  #'gag-one-arg)
(defparameter *gf-proto-two-arg*  #'gag-two-arg)




#+ppc-target
(defvar *cm-proto-code* (uvref *cm-proto* 0))

(defun %cons-combined-method (gf thing dcode)
  ;; set bits and name = gf
  #+ppc-target
  (gvector :function
           *cm-proto-code*
           thing
           dcode
           gf
           (%ilogior (%ilsl $lfbits-cm-bit 1)
                            (%ilogand $lfbits-args-mask (lfun-bits gf))))
  #+x86-target
  (%clone-x86-function *cm-proto*
                       thing
                       dcode
                       gf
                       (%ilogior (%ilsl $lfbits-cm-bit 1)
                                 (%ilogand $lfbits-args-mask (lfun-bits gf)))))

(defun %gf-dispatch-table (gf)
  ;(require-type gf 'standard-generic-function)
  (gf.dispatch-table gf))

(defun %gf-dcode (gf)
  ;(require-type gf 'standard-generic-function)
  (gf.dcode gf))

(defun %set-gf-dcode (gf dcode)
  (let ((gf (require-type gf 'funcallable-standard-object))
        (dcode (require-type dcode 'function)))
    (replace-function-code gf (or (cdr (assq dcode dcode-proto-alist))
                                  #'funcallable-trampoline))
    (setf (gf.dcode gf) dcode)))

(defun %set-gf-dispatch-table (gf val)
  (setf (gf.dispatch-table gf) val))

(defun %combined-method-methods  (cm)
  ;(require-type cm 'combined-method)
  (combined-method.thing cm))

(defun %combined-method-dcode (cm)
  ;(require-type cm 'combined-method)
  (combined-method.dcode cm))

(defun %set-combined-method-methods (cm val)
  (setf (combined-method.thing cm) val))

(defun %set-combined-method-dcode (cm val)
  (setf (combined-method.dcode cm) val))

(declaim (inline funcallable-instance-p))
(defun funcallable-instance-p (thing)
  (when (typep thing 'function)
    (let ((bits (lfun-bits-known-function thing)))
      (declare (fixnum bits))
      (eq (ash 1 $lfbits-gfn-bit)
	  (logand bits (logior (ash 1 $lfbits-gfn-bit)
			       (ash 1 $lfbits-method-bit)))))))

(setf (type-predicate 'funcallable-standard-object) 'funcallable-instance-p)

(defstatic *generic-function-class-wrapper* nil)
(defstatic *standard-generic-function-class-wrapper* nil)

(defun generic-function-p (thing)
  (and (typep thing 'funcallable-standard-object)
       (let* ((wrapper (gf.instance.class-wrapper thing)))
         ;; In practice, many generic-functions are standard-generic-functions.
         (or (eq *standard-generic-function-class-wrapper* wrapper)
             (eq *generic-function-class-wrapper* wrapper)
             (let* ((bits (or (%wrapper-cpl-bits wrapper)
                              (make-cpl-bits (%inited-class-cpl (%wrapper-class wrapper)))))
                    (ordinal (%wrapper-class-ordinal *generic-function-class-wrapper*)))
               (and bits ordinal
                    (locally (declare (simple-bit-vector bits)
                                      (fixnum ordinal)
                                      (optimize (speed 3) (safety 0)))
                      (and (< ordinal (length bits))
                           (eql 1 (sbit bits ordinal))))))))))


(defun standard-generic-function-p (thing)
  (and (typep thing 'function)
       (let ((bits (lfun-bits-known-function thing)))
	 (declare (fixnum bits))
	 (eq (ash 1 $lfbits-gfn-bit)
	     (logand bits (logior (ash 1 $lfbits-gfn-bit)
				  (ash 1 $lfbits-method-bit)))))
       (or (eq (%class.own-wrapper *standard-generic-function-class*)
	       (gf.instance.class-wrapper thing))
	   (memq  *standard-generic-function-class*
		  (%inited-class-cpl (class-of thing))))))


(defun combined-method-p (thing)
  (when (functionp thing)
    (let ((bits (lfun-bits-known-function thing)))
      (declare (fixnum bits))
      (eq (ash 1 $lfbits-cm-bit)
	  (logand bits
		  (logior (ash 1 $lfbits-cm-bit)
			  (ash 1 $lfbits-method-bit)))))))

(setf (type-predicate 'generic-function) 'generic-function-p)

(setf (type-predicate 'standard-generic-function) 'standard-generic-function-p)
(setf (type-predicate 'funcallable-standard-object) 'funcallable-instance-p)
(setf (type-predicate 'combined-method) 'combined-method-p)



;;; A generic-function looks like:
;;; 
;;; header | trampoline |  dispatch-table | dcode | name | bits
;;; %svref :    0              1              2       3      4
;;;
;;; The trampoline is *gf-proto*'s code vector.
;;; The dispatch-table and dcode are sort of settable closed-over variables.

(defsetf %gf-dispatch-table %set-gf-dispatch-table)

(defun %gf-methods (gf)
  (sgf.methods gf))

(defun %gf-precedence-list (gf)
  (%gf-dispatch-table-precedence-list (%gf-dispatch-table gf)))

(defun %gf-%lambda-list (gf)
  (sgf.%lambda-list gf))

(defun (setf %gf-%lambda-list) (new gf)
  (setf (sgf.%lambda-list gf) new))

;;; Returns INSTANCE if it is either a standard instance of a
;;; standard gf, else nil.
(defun %maybe-gf-instance (instance)
  (if (or (standard-generic-function-p instance)
	  (%standard-instance-p instance))
    instance))

(defsetf %gf-dcode %set-gf-dcode)

(defun %gf-method-class (gf)
  (sgf.method-class gf))


(defun %gf-method-combination (gf)
  (sgf.method-combination gf))

(defun %combined-method-methods  (cm)
  (combined-method.thing cm))

(defun %combined-method-dcode (cm)
  ;(require-type cm 'combined-method)
  (combined-method.dcode cm))


; need setters too

(defsetf %combined-method-methods %set-combined-method-methods)

(defparameter *min-gf-dispatch-table-size* 2
  "The minimum size of a generic-function dispatch table")

(defun make-gf-dispatch-table (&optional (size *min-gf-dispatch-table-size*))
  (when (<= size 0) (report-bad-arg size '(integer 1)))
  (setq size (%imax (%ilsl (%i- (integer-length (%i+ size size -1))
                                1)
                           1)           ; next power of 2
                    *min-gf-dispatch-table-size*))
  (let ((res (%cons-gf-dispatch-table size)))
    (setf (%gf-dispatch-table-mask res) (%i- (%ilsr 1 size) 1)
          (%gf-dispatch-table-argnum res) 0
          (%gf-dispatch-table-ref res size) (%unbound-marker))
    res))

;;; I wanted this to be faster - I didn't
(defun clear-gf-dispatch-table (dt)
  (let ((i %gf-dispatch-table-first-data))
    (dotimes (j (%gf-dispatch-table-size dt))
      (declare (fixnum j))
      (setf (%svref dt i) nil 
            i (%i+ i 1)))
    (setf (%svref dt i) (%unbound-marker)) ; paranoia...
    (setf (svref dt (%i+ 1 i)) nil))
  dt)


; Remove all combined-methods from the world
(defun clear-all-gf-caches ()
  (dolist (f (population-data %all-gfs%))
    (clear-gf-cache f))
  (clrhash *combined-methods*)
  nil)


;;; Searches for an empty slot in dt at the hash-index for wrapper.
;;; Returns nil if the table was full.
(defun find-gf-dispatch-table-index (dt wrapper &optional skip-full-check?)
  (let ((contains-obsolete-wrappers-p nil)
        (mask (%gf-dispatch-table-mask dt)))
    (declare (fixnum mask))
    (unless skip-full-check?
      (let* ((size (1+ mask))
             (max-count (- size (the fixnum (ash (the fixnum (+ size 3)) -2))))
             (index 0)
             (count 0))
        (declare (fixnum size max-count index count))
        (dotimes (i size)
          (declare (fixnum i))
          (let ((wrapper (%gf-dispatch-table-ref dt index)))
            (if wrapper
              (if (eql 0 (%wrapper-hash-index wrapper))
                (setf contains-obsolete-wrappers-p t
                      (%gf-dispatch-table-ref dt index) *obsolete-wrapper*
                      (%gf-dispatch-table-ref dt (1+ index)) *gf-dispatch-bug*)
                (setq count (%i+ count 1)))))
          (setq index (%i+ index 2)))
        (when (> count max-count)
          (return-from find-gf-dispatch-table-index (values nil contains-obsolete-wrappers-p)))))
    (let* ((index (ash (logand mask (%wrapper-hash-index wrapper)) 1))
           (flag nil)
           table-wrapper)      
      (values
       (loop
         (while (and (neq wrapper
                          (setq table-wrapper (%gf-dispatch-table-ref dt index)))
                     (%gf-dispatch-table-ref dt (1+ index))
                     (neq 0 (%wrapper-hash-index table-wrapper)))
           (setq index (%i+ index 2)))
         (if (eq (%unbound-marker) table-wrapper)
           (if flag
             (return nil)         ; table full
             (setq flag 1
                   index 0))
           (return index)))
       contains-obsolete-wrappers-p))))


(defvar *obsolete-wrapper* #(obsolete-wrapper 0))
(defvar *gf-dispatch-bug*
  #'(lambda (&rest rest)
      (declare (ignore rest))
      (error "Generic-function dispatch bug!")))

  
;;; This maximum is necessary because of the 32 bit arithmetic in
;;; find-gf-dispatch-table-index.
(defparameter *max-gf-dispatch-table-size* (expt 2 16))
(defvar *gf-dt-ovf-cnt* 0)              ; overflow count

(defvar *no-applicable-method-hash* nil)


(let* ((eql-specializers-lock (make-lock))
       (eql-specializers-hash (make-hash-table :test #'eql)))
  (defun intern-eql-specializer (object)
    (with-lock-grabbed (eql-specializers-lock)
      (or (gethash object eql-specializers-hash)
	  (setf (gethash object eql-specializers-hash)
		(make-instance 'eql-specializer :object object))))))


(setq *no-applicable-method-hash* (make-hash-table :test 'eq :size 0 :weak :key))


(defun make-no-applicable-method-function (gf)
  (if *no-applicable-method-hash*
    (progn
      (or (gethash gf *no-applicable-method-hash*))
      (setf (gethash gf *no-applicable-method-hash*)
            (%cons-no-applicable-method gf)))
    (%cons-no-applicable-method gf)))

(defun %cons-no-applicable-method (gf)
  (%cons-combined-method gf gf #'%%no-applicable-method))

; Returns true if F is a combined-method that calls no-applicable-method
(defun no-applicable-method-cm-p (f)
  (and (typep f 'combined-method)
       (eq '%%no-applicable-method
           (function-name (%combined-method-dcode f)))))


(defun %%no-applicable-method (gf args)
  (if (listp args)
    (apply #'no-applicable-method gf args)
    (%apply-lexpr #'no-applicable-method gf args )))

;;; if obsolete-wrappers-p is true, will rehash instead of grow.
;;; It would be better to do the rehash in place, but I'm lazy today.


(defun arg-wrapper (arg)
  (or (standard-object-p arg)
      (%class.own-wrapper (class-of arg))
      (error "~a has no wrapper" arg)))

;;;;;;;;;;;;;;;;;;;;;;;;; generic-function dcode ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dcode functions using other than *gf-proto*
(defparameter dcode-proto-alist ())

(defun register-dcode-proto (dcode proto)
  (let ((a (assoc dcode dcode-proto-alist)))
    (if a
      (setf (cdr a) proto)
      (push (cons dcode proto) dcode-proto-alist))))


;;; Simple case for generic-functions with no specializers
;;; Why anyone would want to do this I can't imagine.

(defun %%0-arg-dcode (dispatch-table args) ; need to get gf from table
  (let ((method (or (%gf-dispatch-table-ref dispatch-table 1)
                    (0-arg-combined-method-trap
                     (%gf-dispatch-table-gf dispatch-table)))))
    (if (not (listp args))
      (progn
        (%apply-lexpr-tail-wise method args))
      (apply method args))))

(register-dcode-proto #'%%0-arg-dcode *gf-proto*)

(defun dcode-too-few-args (arg-count cm-or-gf)
  (error (make-condition 'too-few-arguments
                         :nargs arg-count
                         :fn (combined-method-gf cm-or-gf))))



(defun %%1st-arg-dcode (dt  args)
  ;(declare (dynamic-extent args))
  (if (not (listp args))
    (let* ((args-len (%lexpr-count args)))
      (if (neq 0 args-len) 
        (let ((method (%find-1st-arg-combined-method dt (%lexpr-ref args args-len 0))))
	  (%apply-lexpr-tail-wise method args))
        (dcode-too-few-args 0 (%gf-dispatch-table-gf dt))))
    (let* ()  ; happens if traced
      (when (null args) (dcode-too-few-args 0 (%gf-dispatch-table-gf dt)))
      (let ((method (%find-1st-arg-combined-method dt (%car args))))
        (apply method args)))))
(register-dcode-proto #'%%1st-arg-dcode *gf-proto*)

(defun %%one-arg-dcode (dt  arg)
  (let ((method (%find-1st-arg-combined-method dt arg)))
    (funcall method arg)))
(register-dcode-proto #'%%one-arg-dcode *gf-proto-one-arg*)

;;; two args - specialized on first
(defun %%1st-two-arg-dcode (dt arg1 arg2)
  (let ((method (%find-1st-arg-combined-method dt arg1)))
    (funcall method arg1 arg2)))
(register-dcode-proto #'%%1st-two-arg-dcode *gf-proto-two-arg*)


;;;  arg is dispatch-table and argnum is in the dispatch table
(defun %%nth-arg-dcode (dt args)
  (if (listp args)
    (let* ((args-len (list-length args))
           (argnum (%gf-dispatch-table-argnum dt)))
      (declare (fixnum args-len argnum))
      (when (>= argnum args-len) (dcode-too-few-args args-len (%gf-dispatch-table-gf dt)))
      (let ((method (%find-nth-arg-combined-method dt (nth argnum args) args)))
        (apply method args)))
    (let* ((args-len (%lexpr-count args))
           (argnum (%gf-dispatch-table-argnum dt)))
      (declare (fixnum args-len argnum))
      (when (>= argnum args-len) (dcode-too-few-args args-len (%gf-dispatch-table-gf dt)))
      (let ((method (%find-nth-arg-combined-method dt (%lexpr-ref args args-len argnum) args)))
	(%apply-lexpr-tail-wise method args)))))
(register-dcode-proto #'%%nth-arg-dcode *gf-proto*)

(defun 0-arg-combined-method-trap (gf)
  (let* ((methods (%gf-methods gf))
         (mc (%gf-method-combination gf))
         (cm (if (eq mc *standard-method-combination*)
               (make-standard-combined-method methods nil gf)
               (compute-effective-method-function 
                gf 
                mc
                (sort-methods (copy-list methods) nil)))))
    (setf (%gf-dispatch-table-ref (%gf-dispatch-table gf) 1) cm)
    cm))

(defun compute-effective-method-function (gf mc methods)  
  (if methods
    (compute-effective-method gf mc methods)
    (make-no-applicable-method-function gf)))

(defun 1st-arg-combined-method-trap (gf wrapper arg)
  ;; Here when we can't find the method in the dispatch table.
  ;; Compute it and add it to the table.  This code will remain in Lisp.
  (let ((table (%gf-dispatch-table gf))
        (combined-method (compute-1st-arg-combined-method gf arg wrapper)))
    (multiple-value-bind (index obsolete-wrappers-p)
        (find-gf-dispatch-table-index table wrapper)
      (if index
          (if (%gf-dispatch-table-store-conditional table (%i+ index 1) combined-method)
            (setf (%gf-dispatch-table-ref table index) wrapper))
          (grow-gf-dispatch-table gf wrapper combined-method obsolete-wrappers-p)))
    combined-method))

(defvar *cpl-classes* nil)

(defun %inited-class-cpl (class &optional initialize-can-fail)
  (or (%class-cpl class)
      (if (memq class *cpl-classes*)
        (compute-cpl class)
        (let ((*cpl-classes* (cons class *cpl-classes*)))
          (declare (dynamic-extent *cpl-classes*))
          (update-class class initialize-can-fail)
          (%class-cpl class)))))


(defun compute-1st-arg-combined-method (gf arg &optional 
                                           (wrapper (arg-wrapper arg)))
  (let* ((methods (%gf-dispatch-table-methods (%gf-dispatch-table gf)))
         (cpl (%inited-class-cpl (%wrapper-class wrapper)))
         (method-combination (%gf-method-combination gf))
         applicable-methods eql-methods specializer)
    (dolist (method methods)
      (setq specializer (%car (%method.specializers method)))
      (if (typep specializer 'eql-specializer)
        (when (cpl-memq (%wrapper-class (arg-wrapper (eql-specializer-object specializer))) cpl)
          (push method eql-methods))
        (when (cpl-memq specializer cpl)
          (push method applicable-methods))))
    (if (null eql-methods)
      (if (eq method-combination *standard-method-combination*)
        (make-standard-combined-method applicable-methods (list cpl) gf)
        (compute-effective-method-function 
         gf 
         method-combination
         (sort-methods applicable-methods
                       (list cpl)
                       (%gf-precedence-list gf))))
      (make-eql-combined-method  
       eql-methods applicable-methods (list cpl) gf 0 nil method-combination))))
      


(defvar *combined-methods* (make-hash-table  :test 'equal :weak :value))                          

(defun gethash-combined-method (key)
  (gethash key *combined-methods*))

(defun puthash-combined-method (key value)
  (setf (gethash key *combined-methods*) value))

;;; Some statistics on the hash table above
(defvar *returned-combined-methods* 0)
(defvar *consed-combined-methods* 0)

;;; Assumes methods are already sorted if cpls is nil
(defun make-standard-combined-method (methods cpls gf &optional
                                              (ok-if-no-primaries (null methods)))
  (unless (null cpls)
    (setq methods (sort-methods 
                   methods cpls (%gf-precedence-list (combined-method-gf gf)))))
  (let* ((keywords (compute-allowable-keywords-vector gf methods))
         (combined-method (make-standard-combined-method-internal
                           methods gf keywords ok-if-no-primaries)))
    (if (and keywords methods)
      (make-keyword-checking-combined-method gf combined-method keywords)
      combined-method)))


;;; Initialized below after the functions exist.
(defvar *clos-initialization-functions* nil)

;;; Returns NIL if all keywords allowed, or a vector of the allowable ones.
(defun compute-allowable-keywords-vector (gf methods)
  (setq gf (combined-method-gf gf))
  (unless (memq gf *clos-initialization-functions*)
    (let* ((gbits (inner-lfun-bits gf))
           (&key-mentioned-p (logbitp $lfbits-keys-bit gbits)))
      (unless (or (logbitp $lfbits-aok-bit gbits)
                  (dolist (method methods)
                    (let ((mbits (lfun-bits (%method.function method))))
                      (when (logbitp $lfbits-keys-bit mbits)
                        (setq &key-mentioned-p t)
                        (if (logbitp $lfbits-aok-bit mbits)
                          (return t)))))
                  (not &key-mentioned-p))
        (let (keys)
          (flet ((adjoin-keys (keyvect keys)
                              (when keyvect
                                (dovector (key keyvect) (pushnew key keys)))
                              keys))
            (when (logbitp $lfbits-keys-bit gbits)
              (setq keys (adjoin-keys (%defgeneric-keys gf) keys)))
            (dolist (method methods)
              (let ((f (%inner-method-function method)))
                (when (logbitp $lfbits-keys-bit (lfun-bits f))
                  (setq keys (adjoin-keys (lfun-keyvect f) keys))))))
          (apply #'vector keys))))))


(defun make-keyword-checking-combined-method (gf combined-method keyvect)
  (let* ((bits (inner-lfun-bits gf))
         (numreq (ldb $lfbits-numreq bits))
         (key-index (+ numreq (ldb $lfbits-numopt bits))))
    (%cons-combined-method 
     gf       
     (vector key-index keyvect combined-method)
     #'%%check-keywords)))



(defun odd-keys-error (varg l) 
  (let ((gf (combined-method-gf (%svref varg 2))))
    (signal-program-error "Odd number of keyword args to ~s~%keyargs: ~s" gf l)))


(defun bad-key-error (key varg l)
  (let* ((keys (%svref varg 1))
         (gf (combined-method-gf (%svref varg 2)))
         (*print-array* t)
         (*print-readably* t)
         (readable-keys (format nil "~s" keys)))
    (signal-program-error "Bad keyword ~s to ~s.~%keyargs: ~s~%allowable keys are ~a." key gf l readable-keys)))

; vector arg is (vector key-index keyvect combined-method) ; the next combined method

(defun %%check-keywords (vector-arg args)
  (flet ((do-it (vector-arg args)
           (let* ((args-len (length args))
                  (keyvect (%svref vector-arg 1))
                  (keyvect-len (length keyvect))
                  (key-index (%svref vector-arg 0)))
					; vector arg is (vector key-index keyvect combined-method) ; the next combined method
             (declare (fixnum args-len key-index keyvect-len))
             (when (>= args-len key-index)
               (let* ((keys-in (- args-len key-index)))	; actually * 2
                 (declare (fixnum  key-index keys-in keyvect-len))
                 (when (logbitp 0 keys-in) (odd-keys-error vector-arg (collect-lexpr-args args key-index args-len)))
		 (unless (%cadr (%pl-search (nthcdr key-index args) :allow-other-keys))
		   (do ((i key-index (+ i 2))
			(kargs (nthcdr key-index args) (cddr kargs)))
		       ((eq i args-len))
		     (declare (fixnum i))
		     (let ((key (car kargs)))
		       (when (not (or (eq key :allow-other-keys)
				      (dotimes (i keyvect-len nil)
					(if (eq key (%svref keyvect i))
					  (return t)))))
			 (bad-key-error key vector-arg (collect-lexpr-args args key-index args-len))
			 ))))))
             (let ((method (%svref vector-arg 2)))
					; magic here ?? not needed
               (apply method args)))))
    (if (listp args)
      (do-it vector-arg args)
      (with-list-from-lexpr (args-list args)
        (do-it vector-arg args-list)))))



  


;;; called from %%call-next-method-with-args - its the key-or-init-fn 
;;; called from call-next-method-with-args - just check the blooming keys
;;; dont invoke any methods - maybe use x%%check-keywords with last vector elt nil
; means dont call any methods - but need the gf or method for error message
(defun x-%%check-keywords (vector-arg ARGS)
  ;(declare (dynamic-extent args))
    ; vector arg is (vector key-index keyvect unused)
  (let* ((ARGS-LEN (length args))
         (keyvect (%svref vector-arg 1))
         (keyvect-len (length keyvect))
         (key-index (%svref vector-arg 0))
         (keys-in (- args-len key-index))
         aok)  ; actually * 2
    (declare (fixnum args-len key-index keys-in keyvect-len))
    
    (when (logbitp 0 keys-in) (odd-keys-error vector-arg (collect-lexpr-args args key-index args-len)))
    (do ((i key-index (+ i 2))
         (kargs (nthcdr key-index args) (cddr kargs)))
        ((eq i args-len))
      (declare (fixnum i))
      (when aok (return))
      (let ((key (car kargs)))
        (when (and (eq key :allow-other-keys)
                   (cadr kargs))
          (return))
        (when (not (dotimes (i keyvect-len nil)
                     (if (eq key (%svref keyvect i))
                       (return t))))
          ; not found - is :allow-other-keys t in rest of user args
          (when (not (do ((remargs kargs (cddr remargs)))
                         ((null remargs) nil)
                       (when (and (eq (car remargs) :allow-other-keys)
                                  (cadr remargs))
                         (setq aok t)
                         (return t))))              
            (bad-key-error key vector-arg 
                           (collect-lexpr-args args key-index args-len))))))))
#| ; testing
(setq keyvect  #(:a :b ))
(setq foo (make-array 3))
(setf (aref foo 0) keyvect (aref foo 1) 2)
(setf (aref foo 2)(method window-close (window)))
( %%check-keywords 1 2 :a 3 :c 4 foo)
( %%check-keywords 1 2 :a 3 :b 4 :d foo)
|#
 
    



;;; Map an effective-method to it's generic-function.
;;; This is only used for effective-method's which are not combined-method's
;;; (e.g. those created by non-STANDARD method-combination)
(defvar *effective-method-gfs* (make-hash-table :test 'eq :weak :key))


(defun get-combined-method (method-list gf)
  (let ((cm (gethash-combined-method method-list)))
    (when cm
      (setq gf (combined-method-gf gf))
      (if (combined-method-p cm)
        (and (eq (combined-method-gf cm) gf) cm)
        (and (eq (gethash cm *effective-method-gfs*) gf) cm)))))

(defun put-combined-method (method-list cm gf)
  (unless (%method-function-p cm)       ; don't bother with non-combined methods
    (puthash-combined-method method-list cm)
    (unless (combined-method-p cm)
      (setf (gethash cm *effective-method-gfs*) (combined-method-gf gf))))
  cm)

(defun make-standard-combined-method-internal (methods gf &optional 
                                                       keywords
                                                       (ok-if-no-primaries
                                                        (null methods)))
  (let ((method-list (and methods (compute-method-list methods nil))))
    (if method-list                 ; no applicable primary methods
      (if (atom method-list)
        (%method.function method-list)    ; can jump right to the method-function
        (progn
          (incf *returned-combined-methods*)  ; dont need this
          (if (contains-call-next-method-with-args-p method-list)
            (make-cnm-combined-method gf methods method-list keywords)
            (or (get-combined-method method-list gf)
                (progn
                  (incf *consed-combined-methods*)  ; dont need this
                  (puthash-combined-method
                   method-list
                   (%cons-combined-method
                    gf method-list #'%%standard-combined-method-dcode)))))))
      (if ok-if-no-primaries
        (make-no-applicable-method-function (combined-method-gf gf))
        (no-applicable-primary-method gf methods)))))

; Initialized after the initialization (generic) functions exist.
(defvar *initialization-functions-alist* nil)

;;; This could be in-line above, but I was getting confused.

;;; ok
(defun make-cnm-combined-method (gf methods method-list keywords)
  (setq gf (combined-method-gf gf))
  (let ((key (cons methods method-list)))
    (or (get-combined-method key gf)
        (let* (key-or-init-arg
               key-or-init-fn)
          (if keywords
            (let* ((bits (inner-lfun-bits gf))
                   (numreq (ldb $lfbits-numreq bits))
                   (key-index (+ numreq (ldb $lfbits-numopt bits))))
              (setq key-or-init-arg (vector key-index keywords gf))
              (setq key-or-init-fn #'x-%%check-keywords))
            (let ((init-cell (assq gf *initialization-functions-alist*)))
              (when init-cell                
                (setq key-or-init-arg init-cell)
                (setq key-or-init-fn #'%%cnm-with-args-check-initargs))))
          (incf *consed-combined-methods*)
          (let* ((vect (vector gf methods key-or-init-arg key-or-init-fn method-list))
                 (self (%cons-combined-method
                        gf vect #'%%cnm-with-args-combined-method-dcode)))
            ;(setf (svref vect 4) self)
            (puthash-combined-method ; if  testing 1 2 3 dont put in our real table
             key
             self))))))


(defparameter *check-call-next-method-with-args* t)

(defun contains-call-next-method-with-args-p (method-list)
  (when *check-call-next-method-with-args*
    (let ((methods method-list)
          method)
      (loop
        (setq method (pop methods))
        (unless methods (return nil))
        (unless (listp method)
          (if (logbitp $lfbits-nextmeth-with-args-bit
                       (lfun-bits (%method.function method)))
            (return t)))))))

;;; The METHODS arg is a sorted list of applicable methods.  Returns
;;; the method-list expected by
;;; %%before-and-after-combined-method-dcode or a single method, or
;;; NIL if there are no applicable primaries
(defun compute-method-list (methods &optional (sub-dispatch? t))
  (let (arounds befores primaries afters qs)
    (dolist (m methods)
      (setq qs (%method.qualifiers m))
      (if qs
        (if (cdr qs)
          (%invalid-method-error
           m "Multiple method qualifiers not allowed in ~s method combination"
           'standard)
          (case (car qs)
            (:before (push m befores))
            (:after (push m afters))
            (:around (push m arounds))
            (t (%invalid-method-error m "~s is not one of ~s, ~s, and ~s."
                                      (car qs) :before :after :around))))
        (push m primaries)))
    (setq primaries (nreverse primaries)
          arounds (nreverse arounds)
          befores (nreverse befores))
    (unless sub-dispatch?
      (setq primaries (nremove-uncallable-next-methods primaries)
            arounds (nremove-uncallable-next-methods arounds)))
    (flet ((next-method-bit-p (method)
                              (logbitp $lfbits-nextmeth-bit 
                                       (lfun-bits (%method.function method)))))
      (unless (null primaries)            ; return NIL if no applicable primary methods
        (when (and arounds
                   (not sub-dispatch?)
                   (not (next-method-bit-p (car (last arounds)))))
          ;; Arounds don't call-next-method, can't get to befores,
          ;; afters, or primaries
          (setq primaries arounds
                arounds nil
                befores nil
                afters nil))
        (if (and (null befores) (null afters)
                 (progn
                   (when arounds
                     (setq primaries (nconc arounds primaries)
                           arounds nil)
                     (unless sub-dispatch?
                       (setq primaries (nremove-uncallable-next-methods primaries))))
                   t)
                 (null (cdr primaries))
                 (not (next-method-bit-p (car primaries))))
          (car primaries)                 ; single method, no call-next-method
          (let ((method-list primaries))
            (if (or befores afters)
              (setq method-list (cons befores (cons afters method-list))))
            (nconc arounds method-list)))))))



(defun %invalid-method-error (method format-string &rest format-args)
  (error "~s is an invalid method.~%~?" method format-string format-args))

(defun %method-combination-error (format-string &rest args)
  (apply #'error format-string args))



(defun combined-method-gf (gf-or-cm)
  (let ((gf gf-or-cm))
    (while (combined-method-p gf)
      (setq gf (lfun-name gf)))
    gf))


(defun nth-arg-combined-method-trap-0 (gf-or-cm table wrapper args)
  (let* ((argnum (%gf-dispatch-table-argnum table))
         (arg (nth argnum args)))
    (nth-arg-combined-method-trap gf-or-cm table argnum args arg wrapper)))


(defun nth-arg-combined-method-trap (gf-or-cm table argnum args &optional
                                              (arg (nth-or-gf-error 
                                                    argnum args gf-or-cm))
                                              (wrapper (arg-wrapper arg)))
  ;; Here when we can't find the method in the dispatch table.
  ;; Compute it and add it to the table.  This code will remain in Lisp.
  (multiple-value-bind (combined-method sub-dispatch?)
      (compute-nth-arg-combined-method
       gf-or-cm (%gf-dispatch-table-methods table) argnum args
       wrapper)
    (multiple-value-bind (index obsolete-wrappers-p)
        (find-gf-dispatch-table-index table wrapper)
      (if index
        (if (%gf-dispatch-table-store-conditional table (%i+ index 1) combined-method)
          (setf (%gf-dispatch-table-ref table index) wrapper))
        (grow-gf-dispatch-table gf-or-cm wrapper combined-method obsolete-wrappers-p)))
    (if sub-dispatch?
      (let ((table (%combined-method-methods combined-method)))
        (nth-arg-combined-method-trap
         combined-method
         table
         (%gf-dispatch-table-argnum table)
         args))
      combined-method)))

;;; Returns (values combined-method sub-dispatch?)
;;; If sub-dispatch? is true, need to compute a combined-method on the
;;; next arg.
(defun compute-nth-arg-combined-method (gf methods argnum args &optional 
                                           (wrapper (arg-wrapper
                                                     (nth-or-gf-error
                                                      argnum args gf))))
  (let* ((cpl (%inited-class-cpl (%wrapper-class wrapper)))
         (real-gf (combined-method-gf gf))
         (mc (%gf-method-combination real-gf))
         (standard-mc? (eq mc *standard-method-combination*))
         applicable-methods eql-methods specializers specializer sub-dispatch?)
    (dolist (method methods)
      ;;(require-type method 'standard-method)   ; for debugging.
      (setq specializers (nthcdr argnum (%method.specializers method))
            specializer (%car specializers))
      (when (if (typep specializer 'eql-specializer)
              (when (cpl-memq (%wrapper-class
                                (arg-wrapper (eql-specializer-object specializer))) cpl)
                (push method eql-methods))
              (when (cpl-memq specializer cpl)
                (push method applicable-methods)))
        (if (contains-non-t-specializer? (%cdr specializers))
          (setq sub-dispatch? t))))
    (if (or eql-methods applicable-methods)
      (if (or (not standard-mc?)
            (contains-primary-method? applicable-methods)
            (contains-primary-method? eql-methods))
        (let ((cpls (args-cpls args)))
          (if eql-methods
            (make-eql-combined-method
             eql-methods applicable-methods cpls gf argnum sub-dispatch? mc)
            (if sub-dispatch?
              (values (make-n+1th-arg-combined-method applicable-methods gf argnum)
                      t)
              (if standard-mc?
                (make-standard-combined-method applicable-methods cpls gf)
                (compute-effective-method-function
                 real-gf mc (sort-methods applicable-methods
                                          (args-cpls args)
                                          (%gf-precedence-list real-gf)))))))
        (no-applicable-primary-method
         real-gf
         (sort-methods (append eql-methods applicable-methods)
                       (args-cpls args)
                       (%gf-precedence-list real-gf))))
       (make-no-applicable-method-function real-gf))))

(defun nth-or-gf-error (n l gf)
  (declare (fixnum l))
  (do* ((i 0 (1+ i))
        (l l (cdr l)))
       ((null l) (dcode-too-few-args i gf))
    (declare (fixnum i))
    (if (= i n)
      (return (car l)))))

(defun contains-non-t-specializer? (specializer-list)
  (dolist (s specializer-list nil)
    (unless (eq *t-class* s)
      (return t))))

(defun contains-primary-method? (method-list)
  (dolist (m method-list nil)
    (if (null (%method.qualifiers m))
      (return t))))

(defun args-cpls (args &aux res)
  (dolist (arg args)
    (push (%inited-class-cpl (%wrapper-class (arg-wrapper arg))) res))
  (nreverse res))


(defun compute-eql-combined-method-hash-table-threshold (&optional (iters 1000000) (max 200))
  (flet ((time-assq (cnt iters)
           (let ((alist (loop for i from 1 to cnt collect (cons i i)))
                 (start-time (get-internal-run-time))
                 (j 0)
                 res)
             (declare (fixnum j))
             (dotimes (i iters)
               (declare (fixnum i))
               (setq res (cdr (assq j alist)))
               (when (>= (incf j) cnt) (setq j 0)))
             (values (- (get-internal-run-time) start-time) res)))
         (time-hash (cnt iters)
           (let ((hash (make-hash-table :test 'eq))
                 start-time
                 (j 0)
                 res)
             (declare (fixnum j))
             (dotimes (i cnt)
               (setf (gethash i hash) i))
             (assert-hash-table-readonly hash)
             (setq start-time (get-internal-run-time))
             (dotimes (i iters)
               (declare (fixnum i))
               (setq res (gethash i hash))
               (when (>= (incf j) cnt) (setq j 0)))
             (values (- (get-internal-run-time) start-time) res))))
    (dotimes (i max)
      (let ((time-assq (time-assq i iters))
            (time-hash (time-hash i iters)))
        (format t "i: ~d, assq: ~d, hash: ~d~%" i time-assq time-hash)
        (when (> time-assq time-hash) (return i))))))

;; Value computed on a dual-core 2.4 GHz AMD Opteron running FC3
;; This isn't the result of compute-eql-combined-method-hash-table-threshold,
;; it's the value at which assq takes 3/4 the time of hash, which weights
;; towards the worst case of the eql method, not the average for uniform inputs.
(defparameter *eql-combined-method-hash-table-threshold* 45)

;;; A vector might be a little faster than an alist, but the hash table case
;;; will speed up large numbers of methods.
(defun make-eql-combined-method (eql-methods methods cpls gf argnum sub-dispatch? &optional
                                             (method-combination *standard-method-combination*))
  (let ((eql-ms (copy-list eql-methods))
        (precedence-list (%gf-precedence-list (combined-method-gf gf)))
        (standard-mc? (eq method-combination *standard-method-combination*))
        (real-gf (combined-method-gf gf))
        eql-method-alist
        (can-use-eq? t))
    (unless sub-dispatch?
      (setq methods (sort-methods methods cpls precedence-list)))
    (while eql-ms
      (let ((eql-element (eql-specializer-object (nth argnum (%method.specializers (car eql-ms)))))
            (this-element-methods eql-ms)
            cell last-cell)
        (if (or (and (numberp eql-element) (not (fixnump eql-element)))
                (macptrp eql-element))
          (setq can-use-eq? nil))
        (setf eql-ms (%cdr eql-ms)
              (%cdr this-element-methods) nil
              cell eql-ms)
        (while cell
          (if (eql eql-element
                     (eql-specializer-object (nth argnum (%method.specializers (car cell)))))
            (let ((cell-save cell))
              (if last-cell
                (setf (%cdr last-cell) (cdr cell))
                (setq eql-ms (cdr eql-ms)))
              (setf cell (cdr cell)
                    (%cdr cell-save) this-element-methods
                    this-element-methods cell-save))
            (setq last-cell cell
                  cell (cdr cell))))
        (let* ((sorted-methods
                (sort-methods (nreconc (copy-list this-element-methods)
                                       (copy-list methods))
                              cpls
                              precedence-list))
               (method-list (and standard-mc? (compute-method-list sorted-methods sub-dispatch?))))
          (when (or (not standard-mc?)
                    (memq method-list this-element-methods)
                    (and (consp method-list)
                         (labels ((member-anywhere (tem mlist)
                                    (member tem mlist
                                            :test #'(lambda (tem el)
                                                      (if (listp el)
                                                        (member-anywhere tem el)
                                                        (member el tem))))))
                           (member-anywhere this-element-methods method-list))))
            ; Do EQL comparison only if the EQL methods can run
            ; (e.g. does not come after a primary method that does not call-next-method)
            (push (cons eql-element
                        (if sub-dispatch?
                          (make-n+1th-arg-combined-method
                           sorted-methods gf argnum)
                          (if standard-mc?
                            (make-standard-combined-method sorted-methods nil gf)
                            (compute-effective-method-function
                             real-gf method-combination sorted-methods))))
                  eql-method-alist)))))
    ;;eql-method-alist has (element . combined-method) pairs.
    ;;for now, we're going to use assq or assoc
    (let ((default-method (if sub-dispatch?
                            (make-n+1th-arg-combined-method
                             methods gf argnum)
                            (if standard-mc?
                              (make-standard-combined-method methods nil gf t)
                              (compute-effective-method-function
                               real-gf method-combination methods)))))
      (if eql-method-alist
        (if (> (length eql-method-alist) *eql-combined-method-hash-table-threshold*)
          (let ((hash (make-hash-table :test (if can-use-eq? 'eq 'eql))))
            (dolist (pair eql-method-alist)
              (setf (gethash (car pair) hash) (cdr pair)))
            (assert-hash-table-readonly hash)
            (%cons-combined-method 
             gf (cons argnum (cons hash default-method))
             #'%%hash-table-combined-method-dcode))
          (%cons-combined-method
           gf (cons argnum (cons eql-method-alist default-method))
           (if can-use-eq? 
               #'%%assq-combined-method-dcode
               #'%%assoc-combined-method-dcode)))
        default-method))))


(defun %%assq-combined-method-dcode (stuff args)
  ;; stuff is (argnum eql-method-list . default-method)
  ;(declare (dynamic-extent args))
  (if (listp args)
    (let* ((args-len (list-length args))
           (argnum (car stuff)))
      (when (>= argnum args-len)(signal-program-error  "Too few args to ~s." (%method-gf (cddr stuff))))
      (let* ((arg (nth argnum args))
             (thing (assq arg (cadr stuff)))) ; are these things methods or method-functions? - fns    
        (if thing 
          (apply (cdr thing) args)
          (apply (cddr stuff) args))))
    (let* ((args-len (%lexpr-count args))
           (argnum (car stuff)))
      (when (>= argnum args-len)(signal-program-error "Too few args to ~s." (%method-gf (cddr stuff))))
      (let* ((arg (%lexpr-ref args args-len argnum))
             (thing (assq arg (cadr stuff))))
        (if thing 
          (%apply-lexpr (cdr thing) args)
          (%apply-lexpr (cddr stuff) args))))))
  

(DEFun %%assoc-combined-method-dcode (stuff args)
  ;; stuff is (argnum eql-method-list . default-method)
  ;(declare (dynamic-extent args))
  (if (listp args)
    (let* ((args-len (list-length args))
           (argnum (car stuff)))
      (when (>= argnum args-len)(signal-program-error "Too few args to ~s." (%method-gf (cddr stuff))))
      (let* ((arg (nth argnum args))
             (thing (assoc arg (cadr stuff)))) ; are these things methods or method-functions?    
        (if thing 
          (apply (cdr thing) args)
          (apply (cddr stuff) args))))
    (let* ((args-len (%lexpr-count args))
           (argnum (car stuff)))
      (when (>= argnum args-len)(signal-program-error "Too few args to ~s." (%method-gf (cddr stuff))))
      (let* ((arg (%lexpr-ref args args-len argnum))
             (thing (assoc arg (cadr stuff)))) ; are these things methods or method-functions?    
        (if thing 
          (%apply-lexpr (cdr thing) args)
          (%apply-lexpr (cddr stuff) args))))))



(defun %%hash-table-combined-method-dcode (stuff args)
  ;; stuff is (argnum eql-hash-table . default-method)
  ;(declare (dynamic-extent args))
  (if (listp args)
    (let* ((args-len (list-length args))
           (argnum (car stuff)))
      (when (>= argnum args-len)(signal-program-error "Too few args to ~s." (%method-gf (cddr stuff))))
      (let* ((arg (nth argnum args)))
        (apply (gethash arg (cadr stuff) (cddr stuff)) args)))
    (let* ((args-len (%lexpr-count args))
           (argnum (car stuff)))
      (when (>= argnum args-len)(signal-program-error "Too few args to ~s." (%method-gf (cddr stuff))))
      (let* ((arg (%lexpr-ref args args-len argnum)))
        (%apply-lexpr (gethash arg (cadr stuff) (cddr stuff)) args)))))


;;; Assumes the two methods have the same number of specializers and
;;; that each specializer of each method is in the corresponding
;;; element of cpls (e.g. cpls is a list of the cpl's for the classes
;;; of args for which both method1 & method2 are applicable.
(defun %method< (method1 method2 cpls)
  (let ((s1s (%method.specializers method1))
        (s2s (%method.specializers method2))
        s1 s2 cpl)
    (loop
      (if (null s1s)
        (return (method-qualifiers< method1 method2)))
      (setq s1 (%pop s1s)
            s2 (%pop s2s)
            cpl (%pop cpls))
      (cond ((typep s1 'eql-specializer) 
             (unless (eq s1 s2)
               (return t)))
            ((typep s2 'eql-specializer) (return nil))
            ((eq s1 s2))
            (t (return (%i< (cpl-index s1 cpl) (cpl-index s2 cpl))))))))

(defun %simple-method< (method1 method2 cpl)
  (let ((s1 (%car (%method.specializers method1)))
        (s2 (%car (%method.specializers method2))))
    (cond ((typep s1 'eql-specializer) 
           (if (eq s1 s2)
             (method-qualifiers< method1 method2)
             t))
          ((typep s2 'eql-specializer) nil)
          ((eq s1 s2) (method-qualifiers< method1 method2))
          (t (%i< (cpl-index s1 cpl) (cpl-index s2 cpl))))))

; Sort methods with argument-precedence-order
(defun %hairy-method< (method1 method2 cpls apo)
  (let ((s1s (%method.specializers method1))
        (s2s (%method.specializers method2))
        s1 s2 cpl index)
    (loop
      (if (null apo)
        (return (method-qualifiers< method1 method2)))
      (setq index (pop apo))
      (setq s1 (nth index s1s)
            s2 (nth index s2s)
            cpl (nth index cpls))
      (cond ((typep s1 'eql-specializer) 
             (unless (eq s1 s2)
               (return t)))
            ((typep s2 'eql-specializer) (return nil))
            ((eq s1 s2))
            (t (return (%i< (cpl-index s1 cpl) (cpl-index s2 cpl))))))))

; This can matter if the user removes & reinstalls methods between
; invoking a generic-function and doing call-next-method with args.
; Hence, we need a truly canonical sort order for the methods
; (or a smarter comparison than EQUAL in %%cnm-with-args-check-methods).
(defun method-qualifiers< (method1 method2)
  (labels ((qualifier-list< (ql1 ql2 &aux q1 q2)
              (cond ((null ql1) (not (null ql2)))
                    ((null ql2) nil)
                    ((eq (setq q1 (car ql1)) (setq q2 (car ql2)))
                     (qualifier-list< (cdr ql1) (cdr ql2)))
                    ((string-lessp q1 q2) t)
                    ; This isn't entirely correct.
                    ; two qualifiers with the same pname in different packages
                    ; are not comparable here.
                    ; Unfortunately, users can change package names, hence,
                    ; comparing the package names doesn't work either.
                    (t nil))))
    (qualifier-list< (%method.qualifiers method1) (%method.qualifiers method2))))
       
(defun sort-methods (methods cpls &optional apo)
  (cond ((null cpls) methods)
        ((null (%cdr cpls))
         (setq cpls (%car cpls))
         (flet ((simple-sort-fn (m1 m2)
                  (%simple-method< m1 m2 cpls)))
           (declare (dynamic-extent #'simple-sort-fn))
           (%sort-list-no-key methods #'simple-sort-fn)))
        ((null apo)                     ; no unusual argument-precedence-order
         (flet ((sort-fn (m1 m2) 
                  (%method< m1 m2 cpls)))
           (declare (dynamic-extent #'sort-fn))
           (%sort-list-no-key methods #'sort-fn)))
        (t                              ; I guess some people are just plain rude
         (flet ((hairy-sort-fn (m1 m2)
                  (%hairy-method< m1 m2 cpls apo)))
           (declare (dynamic-extent #'hairy-sort-fn))
           (%sort-list-no-key methods #'hairy-sort-fn)))))

(defun nremove-uncallable-next-methods (methods)
  (do ((m methods (%cdr m))
       mbits)
      ((null m))
    (setq mbits (lfun-bits (%method.function (%car m))))
    (unless (logbitp $lfbits-nextmeth-bit mbits)
      (setf (%cdr m) nil)
      (return)))
  methods)


(defun cpl-index (superclass cpl)
  ;; This will be table lookup later.  Also we'll prelookup the tables
  ;; in compute-1st-arg-combined-methods above.
  (locally (declare (optimize (speed 3)(safety 0)))
    (do ((i 0 (%i+ i 1))
         (cpl cpl (%cdr cpl)))
        ((null cpl) nil)
      (if (eq superclass (%car cpl))
        (return i)))))

(defun cpl-memq (superclass cpl)
  (locally (declare (optimize (speed 3)(safety 0)))
    (do ((cpl cpl (%cdr cpl)))
        ((null cpl) nil)
      (if (eq superclass (%car cpl))
        (return cpl)))))

;;; Combined method interpretation


;;; magic is a list of (cnm-cm (methods) . args) cnm-cm is the
;;; argument checker for call-next-method-with-args or nil could make
;;; it be a cons as a flag that magic has been heap consed - done
;;; could also switch car and cadr if we do &lexpr business then if
;;; cddr is lexpr-p (aka (not listp)) thats the clue also would need
;;; to do lexpr-apply or apply depending on the state.


(defun %%standard-combined-method-dcode (methods args)
  ;; combined-methods as made by make-combined-method are in methods
  ;; args are as put there by the caller of the gf.
  (let* ((car-meths (car methods))
         (cell-2 (cons methods args))
         (magic (cons nil cell-2)))
    ;; i.e. magic is nil methods . args
    (declare (dynamic-extent magic)
             (dynamic-extent cell-2))    
    (if (listp car-meths)
      (%%before-and-after-combined-method-dcode magic)
      (progn       
        (if (not (cdr methods))
          (%rplaca (cdr magic) car-meths)
          (%rplaca (cdr magic) (cdr methods)))
        ; so maybe its a combined-method ?? - no
        (apply-with-method-context magic (%method.function car-meths) args)))))

;;; args is list, old-args may be lexpr
(defun cmp-args-old-args (args old-args numreq)
  (declare (optimize (speed 3)(safety 0)))
  (if (listp old-args)
    (do ((newl args (cdr newl))
         (oldl old-args (cdr oldl))
         (i 0 (1+ i)))
        ((eql i numreq) t)
      (when (neq (car newl)(car oldl))(return nil)))
    (let ((len (%lexpr-count old-args)))
      (do ((newl args (cdr newl))
           (i 0 (1+ i)))
          ((eql i numreq) t)
        (when (neq (car newl)(%lexpr-ref old-args len i))(return nil))))))        


; called from call-next-method-with-args with magic supplied and 1st time around with not
(defun %%cnm-with-args-combined-method-dcode (thing args &optional magic) ; was &rest args
  ;(declare (dynamic-extent args))
  ; now thing is vector of gf orig methods, arg for key or initarg check, key or initarg fnction
  ; and our job is to do all the arg checking
  (let ()
    (when magic
      (flet ((do-it (thing args)
               (let* ((args-len (length args))
                      (gf (svref thing 0))  ; could get this from a method
                      (numreq (ldb $lfbits-numreq (inner-lfun-bits gf)))
                      (next-methods (cadr magic)))
                 ;(when (null self)(error "Next method with args context error"))
                 (when (neq 0 numreq)
                   ; oh screw it - old-args may be lexpr too
                   (let ((old-args (cddr magic)))
                     (when (< args-len numreq) (signal-program-error "Too few args to ~S" gf))
                     (when (null (cmp-args-old-args args old-args numreq))
                       ; required args not eq - usually true, we expect
                       (let ((new-methods (%compute-applicable-methods* gf args))
                             (old-methods (svref thing 1)))
                         (when (not (equal new-methods old-methods))
                           (error '"Applicable-methods changed in call-next-method.~%~
                                    Should be: ~s~%Was: ~s~%Next-methods: ~s"
                                  old-methods new-methods next-methods))))))
                 (let ((key-or-init-fn (svref thing 3)))
                   (when key-or-init-fn 
                     ; was apply
                     (funcall key-or-init-fn (svref thing 2) args))))))
        (if (listp args)
          (do-it thing args)
          (with-list-from-lexpr (args-list args)
            (do-it thing args-list)))))
    ; ok done checking - lets do it 
    (let* ((methods (if magic (cadr magic)(svref thing 4)))  ;<< was 5 this is nil unless cnm with args
           ; was if magic
           (car-meths (car methods))
           (cell-2 (cons methods args))
           (magic (cons thing cell-2)))
      (declare (dynamic-extent magic cell-2))
      ; i.e. magic is thing methods . args
      ;(%rplaca magic thing)
      ;(setf (cadr magic) methods)
      ;(%rplaca (cdr magic) methods)
      ;(setf (cddr magic) args)
      ;(%rplacd (cdr magic) args)
      (if (listp car-meths)
        (progn
          (%%before-and-after-combined-method-dcode magic))
        (progn       
          (if (not (cdr methods))
            (%rplaca (cdr magic) car-meths)
            (%rplaca (cdr magic) (cdr methods)))
          ; so maybe its a combined-method ?? - no
          (apply-with-method-context magic (%method.function car-meths) args))))))



;;; here if car of methods is listp. methods = (befores afters . primaries)
(defun %%before-and-after-combined-method-dcode (magic) 
  (declare (list magic))
  (let* ((methods (cadr magic))         
         (befores (car methods))         
         (cdr-meths (cdr methods))
         (primaries (cdr cdr-meths))
         (afters (car cdr-meths))
         (args (cddr magic)))
    (declare (list befores afters primaries))
    (when befores 
      (dolist (method befores)
        (rplaca (cdr magic) method)
        (apply-with-method-context magic (%method.function method) args)))
    (let* ((cdr (cdr primaries))
           (method-function (%method.function (car primaries))))   ; guaranteed non nil?
      (rplaca (cdr magic) (if (null cdr)(car primaries) cdr))      
      (if (null afters)
        (apply-with-method-context magic method-function args)  ; tail call if possible
        (multiple-value-prog1
          (apply-with-method-context magic method-function args)        
          (dolist (method afters)
            (rplaca (cdr magic) method)
            (apply-with-method-context magic (%method.function method) args)))))))


; This is called by the compiler expansion of next-method-p
; I think there's a bug going around... LAP fever! I'm immune
(defun %next-method-p (magic)
  (let ((methods (%cadr magic)))
    (consp methods)))


(defun %call-next-method (magic &rest args) ; if args supplied they are new ones
  (declare (dynamic-extent args)) 
  (if args
    (apply #'%call-next-method-with-args magic args)
    (let* ((next-methods (%cadr magic))) ; don't get this closed magic stuff      
      (if (not (consp next-methods))
        ( %no-next-method  magic)            
        (let ((args (%cddr magic)))  ; get original args
          ;The unwind-protect is needed in case some hacker in his/her wisdom decides to:
          ; (defmethod foo (x) (catch 'foo (call-next-method)) (call-next-method))
          ; where the next-method throws to 'foo.
          ; The alternative is to make a new magic var with args
          ; actually not that fancy (call-next-method)(call-next-method) is same problem
          (let ()
            (unwind-protect
              (if (listp (car next-methods))
                ( %%before-and-after-combined-method-dcode magic)
                (let ((cdr (cdr next-methods)))
                  (rplaca (cdr magic)(if (not cdr)(car next-methods) cdr))
                  (let ((method-function (%method.function (car next-methods))))
                    (apply-with-method-context magic method-function args))))
              (rplaca (cdr magic) next-methods))))))))

;; Note: we need to change the compiler to call this when it can prove that
;; call-next-method cannot be called a second time. I believe thats done.


(defun %tail-call-next-method (magic)
  (let* ((next-methods (%cadr magic))  ; or make it car
         (args (%cddr magic))) ; get original args        
    (if (not (consp next-methods)) ; or consp?
      ( %no-next-method magic)
      (if (listp (car next-methods))
        ( %%before-and-after-combined-method-dcode magic)
        (let ((cdr (cdr next-methods)))
          (rplaca (cdr magic) (if (not cdr)(car next-methods) cdr))
          (apply-with-method-context magic (%method.function (car next-methods)) args))))))

;;; may be simpler to blow another cell so magic looks like
;;; (cnm-cm/nil next-methods . args) - done
;;; and also use first cell to mean heap-consed if itsa cons

(defun %call-next-method-with-args (magic &rest args)
  (declare (dynamic-extent args))
  (if (null args)
    (%call-next-method magic)
    (let* ((methods (%cadr magic)))
      (if (not (consp methods))
        (%no-next-method  magic)
        (let* ((cnm-cm (car magic)))
          ; a combined method
          (when (consp cnm-cm)(setq cnm-cm (car cnm-cm)))
          ; could just put the vector in car magic & no self needed in vector?
          (let ((the-vect cnm-cm)) ;  <<
            (funcall #'%%cnm-with-args-combined-method-dcode ;(%combined-method-dcode cnm-cm)
                     the-vect
                     args
                     magic)))))))



; called from x%%call-next-method-with-args - its the key-or-init-fn 
(defun %%cnm-with-args-check-initargs (init-cell args)
  ; here we forget the lexpr idea because it wants to cdr
  ;(declare (dynamic-extent args))
  (let* ((rest (cdr args))
         (first-arg (car args)))
    (declare (list rest))
    (let* ((initargs rest)
           (init-function (car init-cell))
           (instance (cond ((eq init-function #'update-instance-for-different-class)
                            (setq initargs (cdr rest))
                            (car rest))
                           ((eq init-function #'shared-initialize)
                            (setq initargs (cdr rest))
                            first-arg)
                           ((eq init-function #'update-instance-for-redefined-class)
                            (setq initargs (%cdddr rest))
                            first-arg)
                           (t first-arg)))
           (class (class-of instance))
           bad-initarg)
      (dolist (functions (cdr init-cell)
                         (error "Bad initarg: ~s to call-next-method for ~s~%on ~s"
                                bad-initarg instance (car init-cell)))
        (multiple-value-bind 
          (errorp bad-key)
          (if (eq (car functions) #'initialize-instance)
            (apply #'check-initargs instance class initargs nil
                   #'initialize-instance #'allocate-instance #'shared-initialize
                   nil)
            (apply #'check-initargs instance class initargs nil functions))
          (if errorp
            (unless bad-initarg (setq bad-initarg bad-key))
            (return t)))))))



(defun %no-next-method (magic)
  (let* ((method (%cadr magic)))
    (if (consp method) (setq method (car method)))
    (unless (typep method 'standard-method)
      (error "call-next-method called outside of generic-function dispatch context.~@
              Usually indicates an error in a define-method-combination form."))
    (let ((args (cddr magic))
          (gf (%method.gf method)))
      (if (listp args)
        (apply #'no-next-method gf method args)
        (%apply-lexpr #'no-next-method gf method args)))))




;;; This makes a consed version of the magic first arg to a method.
;;; Called when someone closes over the magic arg. (i.e. does (george
;;; #'call-next-method))

(defun %cons-magic-next-method-arg (magic)
  ; car is a cons as a flag that its already heap-consed! - else cnm-cm or nil
  (if (consp (car magic))
    magic
    (list* (list (car magic))
           (if (consp (%cadr magic))
             (copy-list (%cadr magic)) ; is this copy needed - probably not
             (cadr magic))
           (let ((args (%cddr magic)))
             (if (listp args)
               (copy-list args)
               (let* ((len (%lexpr-count args))
                      (l (make-list len)))
                 (do ((i 0 (1+ i))
                      (list l (cdr list)))
                     ((null list))
                   (%rplaca list (%lexpr-ref args len i)))
                 l))))))


; Support CALL-METHOD in DEFINE-METHOD-COMBINATION
(defun %%call-method* (method next-methods args)
  (let* ((method-function (%method.function method))
         (bits (lfun-bits method-function)))
    (declare (fixnum bits))
    (if (not (and (logbitp $lfbits-nextmeth-bit  bits)
                  (logbitp  $lfbits-method-bit bits)))
      (if (listp args)
        (apply method-function args)
        (%apply-lexpr method-function args))
      (let* ((cell-2 (cons next-methods args))
             (magic (cons nil cell-2)))
        (declare (dynamic-extent magic)
                 (dynamic-extent cell-2))  
        (if (null next-methods)
          (%rplaca (cdr magic) method))
        (apply-with-method-context magic method-function args)))))

; Error checking version for user's to call
(defun %call-method* (method next-methods args)
  (let* ((method-function (%method.function method))
         (bits (lfun-bits method-function)))
    (declare (fixnum bits))
    (if (not (and (logbitp $lfbits-nextmeth-bit  bits)
                  (logbitp  $lfbits-method-bit bits)))
      (progn
        (require-type method 'standard-method)
        (if (listp args)
          (apply method-function args)
          (%apply-lexpr method-function args)))
      (progn
        (do* ((list next-methods (cdr list)))
             ((null list))
          (when (not (listp list))
            (%err-disp $XIMPROPERLIST next-methods))
          (when (not (standard-method-p (car list)))
            (report-bad-arg (car list) 'standard-method))) 
        (let* ((cell-2 (cons next-methods args))
               (magic (cons nil cell-2)))
          (declare (dynamic-extent magic)
                   (dynamic-extent cell-2))  
          (if (null next-methods)
            (%rplaca (cdr magic) method))
          (apply-with-method-context magic method-function args))))))



