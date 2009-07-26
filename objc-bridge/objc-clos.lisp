;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2003-2004 Clozure Associates and contributors.
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
;;;
;;; TO DO
;;;  - Both method creation and invocation should be faster and cons less
;;;  - Resolve messages with repeated keywords
;;;    (rename them to :range1:range2 or don't use &key in GFs and methods)
;;;  - How to integrate SEND-SUPER with CALL-NEXT-METHOD?
;;;  - Variable arity ObjC methods
;;;  - Pass-by-ref structures need to keep track of IN, OUT, IN/OUT info
;;;  - Need to canonicalize and retain every returned :ID
;;;  - Support :BEFORE, :AFTER and :AROUND for ObjC methods
;;;  - User-defined ObjC methods via DEFMETHOD (or DEFINE-OBJ-METHOD)
;;;  - Need to fully handle init keywords and ObjC init messages

;;; Package and module stuff

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  #+(or apple-objc cocotron-objc)
  (use-interface-dir :cocoa)
  #+gnu-objc
  (use-interface-dir :gnustep))

(require "SEQUENCE-UTILS")
;;; We need OBJC-FOREIGN-ARG-TYPE from the bridge to process ivar types
(require "BRIDGE")


(defparameter *objc-import-private-ivars* t "When true, the CLASS-DIRECT-SLOTS of imported ObjC classes will contain slot definitions for instance variables whose name starts with an underscore.  Note that this may exacerbate compatibility problems.")





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                 Testing                                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Enable some debugging output.
(defparameter *objc-clos-debug* nil)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     OBJC Foreign Object Domain                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant objc-type-flags (byte 3 20))
(defconstant objc-type-index (byte 20 0))
(defconstant objc-flag-instance 0)
(defconstant objc-flag-class 1)
(defconstant objc-flag-metaclass 2)

(defvar *objc-class-class*)
(defvar *objc-metaclass-class*)

(defvar *objc-object-slot-vectors* (make-hash-table :test #'eql))
(defvar *objc-canonical-instances* (make-hash-table :test #'eql :weak :value))

(defun raw-macptr-for-instance (instance)
  (let* ((p (%null-ptr)))
    (%set-macptr-domain p 1)		; not an ObjC object, but EQL to one
    (%setf-macptr p instance)
    p))

(defun register-canonical-objc-instance (instance raw-ptr)
  ;(terminate-when-unreachable instance)
  ;(retain-objc-instance instance)
  (setf (gethash raw-ptr *objc-canonical-instances*) instance))

(defun canonicalize-objc-instance (instance)
  (or (gethash instance *objc-canonical-instances*)
      (register-canonical-objc-instance
       (setq instance (%inc-ptr instance 0))
       (raw-macptr-for-instance instance))))


(defun recognize-objc-object (p)
  (labels ((recognize (p mapped)
             (let* ((idx (objc-class-id p)))
               (if idx
                 (%set-macptr-type p (dpb objc-flag-class objc-type-flags idx))
                 (if (setq idx (objc-metaclass-id p))
                   (%set-macptr-type p (dpb objc-flag-metaclass objc-type-flags idx))
                   (if (setq idx (%objc-instance-class-index p))
                     (%set-macptr-type p idx)
                     (unless mapped
                       (if (maybe-map-objc-classes)
                         (recognize p t)))))))))
    (recognize p nil)))

(defun release-canonical-nsobject (object)
  object)

  

(defun %objc-domain-class-of (p)
  (let* ((type (%macptr-type p))
	 (flags (ldb objc-type-flags type))
	 (index (ldb objc-type-index type)))
    (declare (fixnum type flags index))
    (ecase flags
      (#.objc-flag-instance (id->objc-class index))
      (#.objc-flag-class (objc-class-id->objc-metaclass index))
      (#.objc-flag-metaclass *objc-metaclass-class*))))
  
(defun %objc-domain-classp (p)
  (let* ((type (%macptr-type p))
	 (flags (ldb objc-type-flags type)))
    (declare (fixnum type flags))
    (not (= flags objc-flag-instance))))

(defun %objc-domain-instance-class-wrapper (p)
  (let* ((type (%macptr-type p))
	 (flags (ldb objc-type-flags type))
	 (index (ldb objc-type-index type)))
    (declare (fixnum type flags index))
    (ecase flags
      (#.objc-flag-instance (id->objc-class-wrapper index))
      (#.objc-flag-class (id->objc-metaclass-wrapper (objc-class-id->objc-metaclass-id index)))
      (#.objc-flag-metaclass (%class.own-wrapper *objc-metaclass-class*)))))

(defun %objc-domain-class-own-wrapper (p)
  (let* ((type (%macptr-type p))
	 (flags (ldb objc-type-flags type))
	 (index (ldb objc-type-index type)))
    (declare (fixnum type flags index))
    (ecase flags
      (#.objc-flag-instance nil)
      (#.objc-flag-class (id->objc-class-wrapper index))
      (#.objc-flag-metaclass (id->objc-metaclass-wrapper index)))))

(defun has-lisp-slot-vector (p)
  (gethash p *objc-object-slot-vectors*))

(defun %remove-lisp-slot-vector (p)
  (remhash p *objc-object-slot-vectors*))

(defun %objc-domain-slots-vector (p)
       (let* ((type (%macptr-type p))
             (flags (ldb objc-type-flags type))
             (index (ldb objc-type-index type)))
        (declare (fixnum type flags index))
        (ecase flags
          (#.objc-flag-instance (or (gethash p *objc-object-slot-vectors*)
                                    ; try to allocate the slot vector on demand
                                    (let* ((raw-ptr (raw-macptr-for-instance p))
                                           (slot-vector (create-foreign-instance-slot-vector (class-of p))))
                                      (when slot-vector
                                        (setf (slot-vector.instance slot-vector) raw-ptr)
                                        (setf (gethash raw-ptr *objc-object-slot-vectors*) slot-vector)
					(register-canonical-objc-instance p raw-ptr)
					(initialize-instance p))
                                      slot-vector)
                                    (error "~s has no slots." p)))
          (#.objc-flag-class (id->objc-class-slots-vector index))
          (#.objc-flag-metaclass (id->objc-metaclass-slots-vector index)))))

(defun %objc-domain-class-ordinal (p)
  (let* ((type (%macptr-type p))
	 (flags (ldb objc-type-flags type))
	 (index (ldb objc-type-index type)))
    (declare (fixnum type flags index))
    (ecase flags
      (#.objc-flag-instance nil)
      (#.objc-flag-class (objc-class-id->ordinal index))
      (#.objc-flag-metaclass (objc-metaclass-id->ordinal index)))))

(defun %set-objc-domain-class-ordinal (p new)
  (let* ((type (%macptr-type p))
	 (flags (ldb objc-type-flags type))
	 (index (ldb objc-type-index type)))
    (declare (fixnum type flags index))
    (ecase flags
      (#.objc-flag-instance nil)
      (#.objc-flag-class (setf (objc-class-id->ordinal index) new))
      (#.objc-flag-metaclass (setf (objc-metaclass-id->ordinal index) new)))))

(defloadvar *objc-object-domain*
    (register-foreign-object-domain :objc
				:recognize #'recognize-objc-object
				:class-of #'%objc-domain-class-of
				:classp #'%objc-domain-classp
				:instance-class-wrapper
				#'%objc-domain-instance-class-wrapper
				:class-own-wrapper
				#'%objc-domain-class-own-wrapper
				:slots-vector #'%objc-domain-slots-vector
				:class-ordinal #'%objc-domain-class-ordinal
				:set-class-ordinal
				#'%set-objc-domain-class-ordinal))

;;; P is known to be a (possibly null!) instance of some ObjC class.
(defun %set-objc-instance-type (p)
  (unless (%null-ptr-p p)
    (let* ((parent (pref p :objc_object.isa))
           (id (objc-class-id parent)))
      (when id
        (%set-macptr-domain p *objc-object-domain*)
        (%set-macptr-type p id)))))

;;; P is known to be of type :ID.  It may be null.
(defun %set-objc-id-type (p)
  (let* ((idx (objc-class-id p)))
    (if idx
      (progn
        (%set-macptr-domain p *objc-object-domain*)
        (%set-macptr-type p (dpb objc-flag-class objc-type-flags idx)))
      (if (setq idx (objc-metaclass-id p))
        (progn
          (%set-macptr-domain p *objc-object-domain*)  
          (%set-macptr-type p (dpb objc-flag-metaclass objc-type-flags idx)))
        (%set-objc-instance-type p)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ObjC Objects, Classes and Metaclasses                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass objc:objc-object (foreign-standard-object)
    ())

;;; "Real" OBJC-CLASSes and OBJC-METACLASSEs are subtypes of this
;;; abstract class.  We need to keep track of those classes that're
;;; implemented in lisp separately (so that they can be restored after
;;; SAVE-APPLICATION).

(defclass objc:objc-class-object (foreign-class objc:objc-object)
    ((foreign :initform nil :initarg :foreign)
     (peer :initform nil :initarg :peer)))

(defclass objc:objc-metaclass (objc:objc-class-object)
    ())

(setq *objc-metaclass-class* (find-class 'objc:objc-metaclass))

(defclass objc:objc-class (objc:objc-class-object)
    ())

(setq *objc-class-class* (find-class 'objc:objc-class))

(deftype @metaclass (&optional string)
  (declare (ignore string))
  'objc:objc-class)

(defmethod objc-metaclass-p ((c class))
  nil)

(defmethod objc-metaclass-p ((c objc:objc-class-object))
  (%objc-metaclass-p c))


(defmethod print-object ((c objc:objc-class) stream)
  (print-unreadable-object (c stream)
    (format stream "~s ~:[~;[MetaClass] ~]~s (#x~x)" 
	    'objc:objc-class 
	    (objc-metaclass-p c) 
	    (if (slot-boundp c 'name)
              (class-name c)
              "<unnamed>")
	    (%ptr-to-int c))))

(defmethod print-object ((c objc:objc-metaclass) stream)
  (print-unreadable-object (c stream)
    (format stream "~s ~s (#x~x)" 
	    'objc:objc-metaclass 
	    (if (slot-boundp c 'name)
              (class-name c)
              "<unnamed>") 
	    (%ptr-to-int c))))

(defmethod print-object ((o objc:objc-object) stream)
  (if (objc-object-p o)
    (print-unreadable-object (o stream :type t)
      (format stream
              (if (and (typep o 'ns::ns-string)
                       (initialized-nsobject-p o))
                "~s (#x~x)"
                "~a (#x~x)")
              (nsobject-description o) (%ptr-to-int o)))
    (format stream "#<Bogus ObjC Object #x~X>" (%ptr-to-int o))))



  


(defun make-objc-class-object-slots-vector (class meta)
  (let* ((n (1+ (length (extract-instance-effective-slotds meta))))
	 (slots (allocate-typed-vector :slot-vector n (%slot-unbound-marker))))
    (setf (slot-vector.instance slots) class)
    slots))

(defun make-objc-metaclass-slots-vector (metaclass)
  (make-objc-class-object-slots-vector metaclass *objc-metaclass-class*))

(defun make-objc-class-slots-vector (class)
  (make-objc-class-object-slots-vector class *objc-class-class*))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              Slot Protocol                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Accessing Lisp slots

(defmethod slot-boundp-using-class ((class objc:objc-class-object)
				    instance
				    (slotd standard-effective-slot-definition))
  (%std-slot-vector-boundp (%objc-domain-slots-vector instance) slotd))

(defmethod slot-value-using-class ((class objc:objc-class-object)
				   instance
				   (slotd standard-effective-slot-definition))
  (%std-slot-vector-value (%objc-domain-slots-vector instance) slotd))

(defmethod (setf slot-value-using-class)
    (new
     (class objc:objc-class-object)
     instance
     (slotd standard-effective-slot-definition))
  (%set-std-slot-vector-value (%objc-domain-slots-vector instance) slotd new))


;;; Metaclasses for foreign slots

(defclass foreign-direct-slot-definition (direct-slot-definition)
  ((foreign-type  :initform :id :accessor foreign-slot-definition-foreign-type)
   (bit-offset :initarg :bit-offset
               :initform nil
               :accessor foreign-direct-slot-definition-bit-offset
               :documentation "A bit-offset, relative to the start of the
               instance's slots.  The corresponding effective slot definition's
                offset is strictly determined by this value")))

(defmethod shared-initialize :after ((slotd foreign-direct-slot-definition)
                                     slot-names
                                     &key (foreign-type :id))
  (declare (ignore slot-names))
  (unless (typep foreign-type 'foreign-type)
    (setq foreign-type (parse-foreign-type foreign-type)))
  (setf (foreign-slot-definition-foreign-type slotd) foreign-type))


(defclass foreign-effective-slot-definition (effective-slot-definition)
  ((foreign-type :initarg :foreign-type :initform :id :accessor foreign-slot-definition-foreign-type)
   (getter :type function :accessor foreign-slot-definition-getter)
   (setter :type function :accessor foreign-slot-definition-setter)))


;;; Use the foreign slot metaclasses if the slot has a :FOREIGN-TYPE attribute
;;  

(defmethod direct-slot-definition-class ((class objc:objc-class-object)
					 &rest initargs)
  (if (getf initargs :foreign-type)
    (find-class 'foreign-direct-slot-definition)
    (find-class 'standard-direct-slot-definition)))

(defmethod effective-slot-definition-class ((class objc:objc-class-object)
					    &rest initargs)
  (if (getf initargs :foreign-type)
    (find-class 'foreign-effective-slot-definition)
    (find-class 'standard-effective-slot-definition)))


(defun set-objc-foreign-direct-slot-offsets (dslotds bit-offset)
  (dolist (d dslotds)
    (let* ((ftype (foreign-slot-definition-foreign-type d))
           (type-alignment (progn (ensure-foreign-type-bits ftype)
                                  (foreign-type-alignment ftype))))
      (setf (foreign-direct-slot-definition-bit-offset d)
            (setq bit-offset
                  (align-offset bit-offset type-alignment)))
      (setq bit-offset (+ bit-offset (foreign-type-bits ftype))))))

(defmethod (setf class-direct-slots) :before (dslotds (class objc::objc-class))
  #-(or apple-objc-2.0 cocotron-objc)
  (let* ((foreign-dslotds
	  (loop for d in dslotds
		when (typep d 'foreign-direct-slot-definition)
		collect d))
         (bit-offset (dolist (c (class-direct-superclasses class) 0)
                       (when (typep c 'objc::objc-class)
                         (return
                           (ash (%objc-class-instance-size c)
                                3))))))
    (unless
        (dolist (d foreign-dslotds t)
          (if (not (foreign-direct-slot-definition-bit-offset d))
            (return nil)))
      (set-objc-foreign-direct-slot-offsets foreign-dslotds bit-offset)))
  #+(or apple-objc-2.0 cocotron-objc)
  ;; Add ivars for each foreign direct slot, then ask the runtime for
  ;; the ivar's byte offset.  (Note that the ObjC 2.0 ivar initialization
  ;; protocol doesn't seem to offer support for bitfield-valued ivars.)
  (dolist (dslotd dslotds)
    (when (typep dslotd 'foreign-direct-slot-definition)
      (let* ((string (lisp-defined-slot-name-to-objc-slot-name (slot-definition-name dslotd)))
             (type (foreign-slot-definition-foreign-type dslotd))
             (encoding (progn
                         (ensure-foreign-type-bits type)
                         (encode-objc-type type)))
             (size (ceiling (foreign-type-bits type) 8))
             (align (round (log (ceiling (foreign-type-alignment type) 8) 2))))
        (with-cstrs ((name string)
                     (encoding encoding))
          (when (eql #$NO (#_class_addIvar class name size align encoding))
            (error "class_addIvar failed"))
          (with-macptrs ((ivar (#_class_getInstanceVariable class name)))
            (unless (%null-ptr-p ivar)
              (let* ((offset (#_ivar_getOffset ivar)))
                (setf (foreign-direct-slot-definition-bit-offset dslotd)
                      (ash offset 3))))))))))


#+(or apple-objc-2.0 cocotron-objc)
(defun %revive-foreign-slots (class)
  (dolist (dslotd (class-direct-slots class))
    (when (typep dslotd 'foreign-direct-slot-definition)
      (let* ((string (lisp-defined-slot-name-to-objc-slot-name (slot-definition-name dslotd)))
             (type (foreign-slot-definition-foreign-type dslotd))
             (encoding (progn
                         (ensure-foreign-type-bits type)
                         (encode-objc-type type)))
             (size (ceiling (foreign-type-bits type) 8))
             (align (round (log (ceiling (foreign-type-alignment type) 8) 2))))
        (with-cstrs ((name string)
                     (encoding encoding))
          (#_class_addIvar class name size align encoding)
          (with-macptrs ((ivar (#_class_getInstanceVariable class name)))
              (unless (%null-ptr-p ivar)
                (let* ((offset (#_ivar_getOffset ivar)))
                  (unless (eql (foreign-direct-slot-definition-bit-offset dslotd)
                               (ash offset 3))
                    (dbg))))))))))

(defun lisp-defined-slot-name-to-objc-slot-name (lisp-name)
  (lisp-to-objc-message (list lisp-name)))

;;; This is only going to be called on a class created by the user;
;;; each foreign direct slotd's offset field should already have been
;;; set to the slot's bit offset.
#-(or apple-objc-2.0 cocotron-objc)
(defun %make-objc-ivars (class)
  (let* ((start-offset (superclass-instance-size class))
	 (foreign-dslotds (loop for s in (class-direct-slots class)
				when (typep s 'foreign-direct-slot-definition)
				collect s)))
    (if (null foreign-dslotds)
      (values (%null-ptr) start-offset)
      (let* ((n (length foreign-dslotds))
	     (offset start-offset)
	     (ivars (malloc (+ 4 (* n (%foreign-type-or-record-size
				       :objc_ivar :bytes))))))
      (setf (pref ivars :objc_ivar_list.ivar_count) n)
      (do* ((l foreign-dslotds (cdr l))
	    (dslotd (car l) (car l))
	    (ivar (pref ivars :objc_ivar_list.ivar_list)
		  (%inc-ptr ivar (%foreign-type-or-record-size
				 :objc_ivar :bytes))))
	   ((null l) (values ivars (ash (align-offset offset 32) 3)))
	(let* ((string (lisp-defined-slot-name-to-objc-slot-name (slot-definition-name dslotd)))
	       (type (foreign-slot-definition-foreign-type dslotd))
	       (encoding (progn
                           (ensure-foreign-type-bits type)
                           (encode-objc-type type))))
	  (setq offset (foreign-direct-slot-definition-bit-offset dslotd))
	  (setf (pref ivar :objc_ivar.ivar_name) (make-cstring string)
		(pref ivar :objc_ivar.ivar_type) (make-cstring encoding)
		(pref ivar :objc_ivar.ivar_offset) (ash offset -3))
          (incf offset (foreign-type-bits type))))))))
  
  

(defun %objc-ivar-offset-in-class (name c)
  ;; If C is a non-null ObjC class that contains an instance variable
  ;; named NAME, return that instance variable's offset,  else return
  ;; NIL.
  #+(or apple-objc-2.0 cocotron-objc)
  (with-cstrs ((name name))
    (with-macptrs ((ivar (#_class_getInstanceVariable c name)))
      (unless (%null-ptr-p ivar)
        (#_ivar_getOffset ivar))))
  #-(or apple-objc-2.0 cocotron-objc)
  (when (objc-class-p c)
    (with-macptrs ((ivars (pref c :objc_class.ivars)))
      (unless (%null-ptr-p ivars)
	(loop with n = (pref ivars :objc_ivar_list.ivar_count)
	      for i from 1 to n
	      for ivar = (pref ivars :objc_ivar_list.ivar_list) 
	          then (%inc-ptr ivar (record-length :objc_ivar))
	      when (string= name (%get-cstring (pref ivar :objc_ivar.ivar_name)))
	        do (return-from %objc-ivar-offset-in-class (pref ivar :objc_ivar.ivar_offset)))))))

(defun %objc-ivar-offset (name c)
  (labels ((locate-objc-slot (name class)
	     (unless (%null-ptr-p class)
		 (or (%objc-ivar-offset-in-class name class)
		     (with-macptrs ((super #+(or apple-objc-2.0 cocotron-objc)
                                           (#_class_getSuperclass class)
                                           #-(or apple-objc-2.0 cocotron-objc)
                                           (pref class :objc_class.super_class)))
		       (unless (or (%null-ptr-p super) (eql super class))
			 (locate-objc-slot name super)))))))
    (when (objc-class-p c)
      (or (locate-objc-slot name c)
	  (error "No ObjC instance variable named ~S in ~S" name c)))))

;;; Maintain the class wrapper of an ObjC class or metaclass.

(defmethod (setf class-own-wrapper) :after (wrapper (class objc::objc-metaclass))
  (setf (id->objc-metaclass-wrapper (objc-metaclass-id class)) wrapper))

(defmethod (setf class-own-wrapper) :after (wrapper (class objc::objc-class))
  (setf (id->objc-class-wrapper (objc-class-id class)) wrapper))

;;; Return the getter and setter functions for a foreign slot
;;; NOTE: Should be changed to use FOREIGN-TYPE-TO-REPRESENTATION-TYPE

(defun compute-foreign-slot-accessors (eslotd)
  (let* ((ftype (foreign-slot-definition-foreign-type eslotd))
         (ordinal (foreign-type-ordinal ftype)))
    (etypecase ftype
      (foreign-integer-type
       (let* ((bits (foreign-integer-type-bits ftype))
	      (align (foreign-integer-type-alignment ftype))
	      (signed (foreign-integer-type-signed ftype)))
         (if (= bits align)
	   (case bits
	     (1 (values #'%get-bit #'%set-bit))
	     (8 (values (if signed #'%get-signed-byte #'%get-unsigned-byte)
			#'%set-byte))
	     (16 (values (if signed #'%get-signed-word #'%get-unsigned-word)
			 #'%set-word))
	     (32 (values (if signed #'%get-signed-long #'%get-unsigned-long)
			 #'%set-long))
	     (64 (if signed
		   (values #'%%get-signed-longlong #'%%set-signed-longlong)
		   (values #'%%get-unsigned-longlong #'%%set-unsigned-longlong)))
             (t (values #'(lambda (ptr offset)
                       (%get-bitfield ptr offset bits))
                   #'(lambda (ptr offset new)
                       (setf (%get-bitfield ptr offset bits) new)))))
           (values #'(lambda (ptr offset)
                       (%get-bitfield ptr offset bits))
                   #'(lambda (ptr offset new)
                       (setf (%get-bitfield ptr offset bits) new))))))
      (foreign-double-float-type
       (values #'%get-double-float #'%set-double-float))
      (foreign-single-float-type
       (values #'%get-single-float #'%set-single-float))
      (foreign-pointer-type
       (if (objc-id-type-p ftype)
         (values #'%get-ptr #'%set-ptr)
         (let* ((to (foreign-pointer-type-to ftype))
                (to-ordinal (if to (foreign-type-ordinal to) 0)))
           (values #'(lambda (ptr offset)
                       (let* ((p (%null-ptr)))
                         (%setf-macptr p (%get-ptr ptr offset))
                         (unless (%null-ptr-p p)
                           (%set-macptr-domain p 1)
                           (%set-macptr-type p to-ordinal))
                         p))
                   #'%set-ptr))))
      (foreign-mem-block-type
       (let* ((nbytes (%foreign-type-or-record-size ftype :bytes)))
	 (values #'(lambda (ptr offset)
                     (let* ((p (%inc-ptr ptr offset)))
                       (%set-macptr-type p ordinal)
                       p))
                 #'(lambda (pointer offset new)
				(setf (%composite-pointer-ref
				       nbytes
				       pointer
				       offset)
				      new))))))))
    


;;; Shadow SLOT-CLASS's COMPUTE-EFFECTIVE-SLOT-DEFINITION with a
;;; method for OBJC-CLASSes that sets up foreign slot info.

(defmethod compute-effective-slot-definition :around ((class objc:objc-class-object)
						      name
						      direct-slots)
  (let* ((first (first direct-slots)))
    (if (not (typep first 'foreign-direct-slot-definition))
      (call-next-method)
      (let* ((initer (dolist (s direct-slots)
		       (when (%slot-definition-initfunction s)
			 (return s))))
	     (documentor (dolist (s direct-slots)
			   (when (%slot-definition-documentation s)
			     (return s))))
	     (initargs (let* ((initargs nil))
			 (dolist (dslot direct-slots initargs)
			   (dolist (dslot-arg (%slot-definition-initargs  dslot))
			     (pushnew dslot-arg initargs :test #'eq)))))
	     (eslotd
	       (make-effective-slot-definition
		class
		:name name
		:allocation :instance
		:type (or (%slot-definition-type first) t)
		:documentation (when documentor (nth-value
				      1
				      (%slot-definition-documentation
				       documentor)))
		:class (%slot-definition-class first)
		:initargs initargs
		:initfunction (if initer
				(%slot-definition-initfunction initer))
		:initform (if initer (%slot-definition-initform initer))
		:foreign-type (foreign-slot-definition-foreign-type first))))
      (multiple-value-bind (getter setter) (compute-foreign-slot-accessors eslotd)
	(setf (foreign-slot-definition-getter eslotd) getter)
	(setf (foreign-slot-definition-setter eslotd) setter))
      eslotd))))

(defun bit-offset-to-location (bit-offset foreign-type)
  (ensure-foreign-type-bits foreign-type)
  (let* ((bits (foreign-type-bits foreign-type)))
    (if (or (= bits 1)
            (and (not (typep foreign-type 'foreign-mem-block-type))
                 (not (= bits (foreign-type-alignment foreign-type)))))
      bit-offset
      (ash bit-offset -3))))

;;; Determine the location of each slot
;;; An effective slot's location is
;;; a) a function of the class's origin (superclass-instance-size)
;;;    and the corresponding direct class's offset, if it's defined in the
;;;    class (has a corresponding direct-slot-definition in the class)
;;; b) Exactly the same as the superclass's version's location, because
;;;    of single inheritance.

(defun determine-foreign-slot-location (class slot-name)
  (or
   (dolist (d (class-direct-slots class))
     (when (and (eq slot-name (slot-definition-name d))
                (typep d 'foreign-direct-slot-definition))
       (return (bit-offset-to-location
                (foreign-direct-slot-definition-bit-offset d)
                (foreign-slot-definition-foreign-type d )))))
   (dolist (super (class-direct-superclasses class))
     (when (typep super 'objc:objc-class) ; can be at most 1
       (let* ((e (find slot-name (class-slots super) :key #'slot-definition-name)))
	 (when e (return (slot-definition-location e))))))
   (error "Can't find slot definition for ~s in ~s" slot-name class)))
	  

(defmethod compute-slots :around ((class objc:objc-class-object))
  (flet ((foreign-slot-p (s) (typep s 'foreign-effective-slot-definition)))
    (let* ((cpl (%class-precedence-list class))
	   (slots (call-next-method))
	   (instance-slots 
	    (remove-if #'foreign-slot-p 
		       (remove :class slots :key #'%slot-definition-allocation)))
	   (class-slots (remove :instance slots :key #'%slot-definition-allocation))
	   (foreign-slots (remove-if-not #'foreign-slot-p slots)))
      (setq instance-slots
	    (sort-effective-instance-slotds instance-slots class cpl))
      (when *objc-clos-debug*
	(format t "Instance slots: ~S~%Class Slots: ~S~%Foreign Slots: ~S~%"
		instance-slots class-slots foreign-slots))
      (loop for islot in instance-slots
	    for loc = 1 then (1+ loc)
	    do (setf (%slot-definition-location islot) loc))
      (dolist (cslot class-slots)
	(setf (%slot-definition-location cslot)
	      (assoc (%slot-definition-name cslot)
		     (%class-get (%slot-definition-class cslot) :class-slots)
		     :test #'eq)))
      (dolist (fslot foreign-slots)
	(setf (%slot-definition-location fslot)
	      (determine-foreign-slot-location
	       class
	       (%slot-definition-name fslot))))
      (append instance-slots class-slots foreign-slots))))


;;; Accessing foreign slots

(defmethod slot-boundp-using-class ((class objc:objc-class-object)
				    instance
				    (slotd foreign-effective-slot-definition))
  (declare (ignore class instance slotd))
  ;; foreign slots are always bound
  t)

(defmethod slot-makunbound-using-class ((class objc:objc-class-object)
					instance
					(slotd foreign-effective-slot-definition))
  (declare (ignore instance))
  (error "Foreign slots cannot be unbound: ~S" (slot-definition-name slotd)))

(defmethod slot-value-using-class ((class objc:objc-class-object)
				   instance
				   (slotd foreign-effective-slot-definition))
  (funcall (foreign-slot-definition-getter slotd)
	   instance
	   (slot-definition-location slotd)))

(defmethod (setf slot-value-using-class) (value
					  (class objc:objc-class-object)
					  instance
					  (slotd foreign-effective-slot-definition))
  (funcall (foreign-slot-definition-setter slotd)
	   instance
	   (slot-definition-location slotd)
	   value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;            Instance Allocation and Initialization Protocols            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-instance ((class objc:objc-class-object) &rest initargs)
  (let ((instance (apply #'allocate-instance class initargs)))
    (if (%null-ptr-p instance)
      instance
      (apply #'initialize-instance instance initargs))))


(defun remove-slot-initargs (class initargs)
  (let* ((slot-initargs (class-slot-initargs class))) ; cache this, maybe
    (collect ((new-initargs))
    (loop for l = initargs then (cddr l)
	  when (null l) do (return-from remove-slot-initargs (new-initargs))
	  unless (member (first l)  slot-initargs :test #'eq)
          do
          (new-initargs (car l))
          (new-initargs (cadr l))))))

(defun create-foreign-instance-slot-vector (class)
  (let* ((max 0))
    (dolist (slotd (class-slots class)
	     (unless (zerop max)
	       (allocate-typed-vector :slot-vector (1+ max) (%slot-unbound-marker))))
      (when (typep slotd 'standard-effective-slot-definition)
	(let* ((loc (slot-definition-location slotd)))
	  (if (> loc max)
	    (setq max loc)))))))

	       
					 
(defmethod allocate-instance ((class objc:objc-class) &rest initargs &key &allow-other-keys)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let* ((instance
	  (multiple-value-bind (ks vs) (keys-and-vals (remove-slot-initargs
						       class
						       initargs))
	    (send-objc-init-message (allocate-objc-object class) ks vs))))
    (unless (%null-ptr-p instance)
      (or (gethash instance *objc-object-slot-vectors*)
          (let* ((slot-vector (create-foreign-instance-slot-vector class)))
            (when slot-vector
              (let* ((raw-ptr (raw-macptr-for-instance instance)))
                (setf (slot-vector.instance slot-vector) raw-ptr)
                (setf (gethash raw-ptr *objc-object-slot-vectors*) slot-vector)
                (register-canonical-objc-instance instance raw-ptr))))))
    instance))




(defmethod initialize-instance ((instance objc:objc-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defmethod reinitialize-instance ((instance objc:objc-object) &rest initargs)
  (apply #'shared-initialize instance nil initargs))

(defmethod initialize-instance :after ((class objc:objc-class) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-value class 'foreign)
    #-(or apple-objc-2.0 cocotron-objc)
    (multiple-value-bind (ivars instance-size)
	(%make-objc-ivars class)
      (%add-objc-class class ivars instance-size))
    #+(or apple-objc-2.0 cocotron-objc)
    (%add-objc-class class)))

(defmethod shared-initialize ((instance objc:objc-object) slot-names 
			      &rest initargs)
  (let ((class (class-of instance)))
    ;; Initialize CLOS slots
    (dolist (slotd (class-slots class))
      (when (not (typep slotd 'foreign-effective-slot-definition)) ; For now
	(let ((sname (slot-definition-name slotd))
	      (slot-type (slot-definition-type slotd))
	      (typepred (slot-value slotd 'type-predicate))
	      (initfunction (slot-definition-initfunction slotd)))
	  (multiple-value-bind (ignore newval foundp)
			       (get-properties initargs
					       (slot-definition-initargs slotd))
	    (declare (ignore ignore))
	    (if foundp
		(if (or (null typepred)
                        (funcall typepred newval))
		    (setf (slot-value instance sname) newval)
		  (report-bad-arg newval slot-type))
	      (let* ((loc (slot-definition-location slotd))
		     (curval (%standard-instance-instance-location-access
			     instance loc)))
		(when (and (or (eq slot-names t) 
			       (member sname slot-names :test #'eq))
			   (eq curval (%slot-unbound-marker))
			   initfunction)
		  (let ((newval (funcall initfunction)))
		    (unless (or (null typepred)
                                (funcall typepred newval))
		      (report-bad-arg newval slot-type))
		    (setf (%standard-instance-instance-location-access
			   instance loc)
			  newval)))))))))
    instance))

(defmethod shared-initialize :after ((spec foreign-effective-slot-definition)
				     slot-names
				     &key &allow-other-keys)
  (declare (ignore slot-names))
  (setf (slot-value spec 'type-predicate) #'true))

;;; The CLASS-OF an existing OBJC:OBJC-CLASS is an OBJC:OBJC-METACLASS,
;;; but not necessarily the one specified as a :metaclass option to
;;; DEFCLASS or ENSURE-CLASS.  Allow an existing class to be reinitialized,
;;; as long as the specified :metaclass and the class's own class have
;;; the same metaclass and specified metaclass is a root class.

(defmethod ensure-class-using-class ((class objc:objc-class)
				     name
				     &rest keys &key)
  (multiple-value-bind (metaclass initargs)
      (ensure-class-metaclass-and-initargs class keys)
    (let* ((existing-metaclass (class-of class)))
      (if (and (eq (class-of metaclass)
		   (class-of existing-metaclass))
	       ;; A root metaclass has the corresponding class as
	       ;; its superclass, and that class has no superclass.
	       (with-macptrs ((super #+(or apple-objc-2.0 cocotron-objc)
                                     (#_class_getSuperclass metaclass)
                                     #-(or apple-objc-2.0 cocotron-objc)
                                     (pref metaclass :objc_class.super_class)))
		 (and (not (%null-ptr-p super))
		      (not (%objc-metaclass-p super))
		      (%null-ptr-p
                       #+(or apple-objc-2.0 cocotron-objc)
                       (#_class_getSuperclass super)
                       #-(or apple-objc-2.0 cocotron-objc)
                       (pref super :objc_class.super_class)))))
	;; Whew! it's ok to reinitialize the class.
	(progn
	  (apply #'reinitialize-instance class initargs)
	  (setf (find-class name) class))
	(error "Can't change metaclass of ~s to ~s." class metaclass)))))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              Class Definition and Finalization Protocols               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Create the ObjC class/metaclass pair and dress it up in its minimal CLOS garb
;;; This currently requires that exactly one of DIRECT-SUPERCLASSES be a
;;; already existing subclass of OBJC:OBJC-CLASS

(defun compute-objc-variable-name (sym)
  (let* ((pname (string sym))
	 (first-alpha (position-if #'alpha-char-p pname)))
    (string-downcase
     (apply #'string-cat 
	    (mapcar #'string-capitalize (split-if-char #\- pname :elide)))
     :end (if first-alpha (1+ first-alpha) 1))))

(defmethod allocate-instance ((metaclass objc:objc-metaclass) 
			      &key name direct-superclasses
			      &allow-other-keys)
  (let ((superclass
	 (loop for s in direct-superclasses
	       when (typep s 'objc:objc-class)
	         collect s into objc-supers
	       finally 
	       (if (= (length objc-supers) 1)
		   (return (first objc-supers))
		 (error "Exactly one OBJC:OBJC-CLASS must appear in ~S, found ~S" 
			direct-superclasses
			(length objc-supers))))))
    (%allocate-objc-class name superclass)))

(defmethod shared-initialize ((class objc:objc-class-object) slot-names &rest initargs)
  (%shared-initialize class slot-names initargs))

(defmethod validate-superclass ((c1 objc:objc-class) (c2 objc:objc-class))
  t)

(defmethod make-instances-obsolete ((class objc:objc-class))
  class)

;;; Reader/writer methods for instances of OBJC:OBJC-CLASS
(defmethod reader-method-class ((class objc:objc-class)
				(dslotd direct-slot-definition)
				&rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-reader-method))

(defmethod writer-method-class ((class objc:objc-class)
				(dslotd direct-slot-definition)
				&rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-writer-method))


;;; By the time we see this, the slot name has been transformed to the form
;;; "(load-time-value (ensure-slot-id <slot-name>))".
;;; This only works if the setter is SETF inverse of the getter.
(define-compiler-macro slot-id-value (&whole call instance slot-name &environment env)
  (or
   (let* ((type nil))
     (if (and (symbolp instance)
              (subtypep (setq type (cdr (assq 'type (nth-value 2 (variable-information instance env)))))
                        'objc:objc-object)
              (consp slot-name)
              (eq (car slot-name) 'load-time-value)
              (consp (cdr slot-name))
              (null (cddr slot-name))
              (eq (caadr slot-name) 'ensure-slot-id)
              (consp (cdadr slot-name))
              (null (cddadr slot-name))
              (setq slot-name (cadadr slot-name))
              (quoted-form-p slot-name)
              (setq slot-name (cadr slot-name)))
       (let* ((class (find-class type nil))
              (eslotd (when class (find slot-name (class-slots class)
                                        :key #'slot-definition-name))))
         (when (typep eslotd 'foreign-effective-slot-definition)
           (let* ((getter (foreign-slot-definition-getter eslotd))
                  (name (if (typep getter 'compiled-function)
                          (function-name getter))))
             (when name
               `(,name ,instance ,(slot-definition-location eslotd))))))))
   call))


