;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2002-2003 Clozure Associates
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


;;; Utilities for interacting with the Apple/GNU Objective-C runtime
;;; systems.

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+darwin-target (pushnew :apple-objc *features*)
  #+(and darwin-target 64-bit-target) (pushnew :apple-objc-2.0 *features*)
  #-darwin-target (pushnew :gnu-objc *features*))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character
   #\#
   #\@
   (nfunction
    |objc-#@-reader|
    (lambda (stream subchar numarg)
      (declare (ignore subchar numarg))
      (let* ((string (read stream)))
	(unless *read-suppress*
          (check-type string string)
          `(@ ,string)))))))

(eval-when (:compile-toplevel :execute)
  #+apple-objc
  (progn
    (use-interface-dir :cocoa)
    #+nomore
    (use-interface-dir :carbon))        ; need :carbon for things in this file
  #+gnu-objc
  (use-interface-dir :gnustep))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "OBJC-PACKAGE")
  (require "SPLAY-TREE")
  (require "NAME-TRANSLATION")
  (require "OBJC-CLOS"))

;;; NSInteger and NSUInteger probably belong here.
;;; CGFloat not so much.

#-apple-objc-2.0
(progn
  (def-foreign-type :<CGF>loat :float)
  (def-foreign-type :<NSUI>nteger :unsigned)
  (def-foreign-type :<NSI>nteger :signed)
  )

(defconstant +cgfloat-zero+
  #+(and apple-objc-2.0 64-bit-target) 0.0d0
  #-(and apple-objc-2.0 64-bit-target) 0.0f0)

(deftype cgfloat ()
  #+(and apple-objc-2.0 64-bit-target) 'double-float
  #-(and apple-objc-2.0 64-bit-target) 'single-float)

(deftype cg-float () 'cgfloat)

(deftype nsuinteger ()
  #+(and apple-objc-2.0 64-bit-target) '(unsigned-byte 64)
  #-(and apple-objc-2.0 64-bit-target) '(unsigned-byte 32))

(deftype nsinteger ()
  #+(and apple-objc-2.0 64-bit-target) '(signed-byte 64)
  #-(and apple-objc-2.0 64-bit-target) '(signed-byte 32))


(defloadvar *NSApp* nil )

;;; Apple ObjC 2.0 provides (#_objc_getProtocol name).  In other
;;; runtimes, there doesn't seem to be any way to find a Protocol
;;; object given its name.  We need to be able to ask at runtime
;;; whether a given object conforms to a protocol in order to
;;; know when a protocol method is ambiguous, at least when the
;;; message contains ambiguous methods and some methods are protocol
;;; methods
(defvar *objc-protocols* (make-hash-table :test #'equal))


(defstruct objc-protocol
  name
  address)


(defun clear-objc-protocols ()
  (maphash #'(lambda (name proto)
	       (declare (ignore name))
	       (setf (objc-protocol-address proto) nil))
	   *objc-protocols*))

(defun lookup-objc-protocol (name)
  (values (gethash name *objc-protocols*)))

(defun ensure-objc-classptr-resolved (classptr)
  #+apple-objc (declare (ignore classptr))
  #+gnu-objc
  (unless (logtest #$_CLS_RESOLV (pref classptr :objc_class.info))
    (external-call "__objc_resolve_class_links" :void)))



(defstruct private-objc-class-info
  name
  declared-ancestor)

(defun compute-objc-direct-slots-from-info (info class)
  (let* ((ns-package (find-package "NS")))
    (mapcar #'(lambda (field)
                (let* ((name (compute-lisp-name (unescape-foreign-name
                                                 (foreign-record-field-name
                                                  field))
                                                ns-package))

                       (type (foreign-record-field-type field))
                       (offset (progn
                                    (ensure-foreign-type-bits type)
                                    (foreign-record-field-offset field))))
                  (make-instance 'foreign-direct-slot-definition
                                 :initfunction #'false
                                 :initform nil
                                 :name name
                                 :foreign-type type
                                 :class class
                                 :bit-offset offset
                                 :allocation :instance)))
            (db-objc-class-info-ivars info))))


(defun %ptr< (x y)
  (< (the (unsigned-byte #+64-bit-target 64 #+32-bit-target 32)
       (%ptr-to-int x))
     (the (unsigned-byte #+64-bit-target 64 #+32-bit-target 32)
       (%ptr-to-int Y))))

(let* ((objc-class-map (make-splay-tree #'%ptr-eql #'%ptr<))
       (objc-metaclass-map (make-splay-tree #'%ptr-eql #'%ptr<))
       ;;; These are NOT lisp classes; we mostly want to keep track
       ;;; of them so that we can pretend that instances of them
       ;;; are instances of some known (declared) superclass.
       (private-objc-classes (make-splay-tree #'%ptr-eql #'%ptr<))
       (objc-class-lock (make-lock))
       (next-objc-class-id 0)
       (next-objc-metaclass-id 0)
       (class-table-size 1024)
       (c (make-array class-table-size))
       (m (make-array class-table-size))
       (cw (make-array 1024 :initial-element nil))
       (mw (make-array 1024 :initial-element nil))
       (csv (make-array 1024))
       (msv (make-array 1024))
       (class-id->metaclass-id (make-array 1024 :initial-element nil))
       (class-foreign-names (make-array 1024))
       (metaclass-foreign-names (make-array 1024))
       (class-id->ordinal (make-array 1024 :initial-element nil))
       (metaclass-id->ordinal (make-array 1024 :initial-element nil))
       )

  (flet ((grow-vectors ()
	   (let* ((old-size class-table-size)
		  (new-size (* 2 old-size)))
	     (declare (fixnum old-size new-size))
	     (macrolet ((extend (v)
                              `(setq ,v (%extend-vector old-size ,v new-size))))
                   (extend c)
                   (extend m)
                   (extend cw)
                   (extend mw)
		   (fill cw nil :start old-size :end new-size)
		   (fill mw nil :start old-size :end new-size)
                   (extend csv)
                   (extend msv)
		   (extend class-id->metaclass-id)
		   (fill class-id->metaclass-id nil :start old-size :end new-size)
		   (extend class-foreign-names)
		   (extend metaclass-foreign-names)
		   (extend class-id->ordinal)
		   (extend metaclass-id->ordinal)
		   (fill class-id->ordinal nil :start old-size :end new-size)
		   (fill metaclass-id->ordinal nil
			 :start old-size :end new-size))
	     (setq class-table-size new-size))))
    (flet ((assign-next-class-id ()
	     (let* ((id next-objc-class-id))
	       (if (= (incf next-objc-class-id) class-table-size)
		 (grow-vectors))
	       id))
	   (assign-next-metaclass-id ()
	     (let* ((id next-objc-metaclass-id))
	       (if (= (incf next-objc-metaclass-id) class-table-size)
		 (grow-vectors))
	       id)))
      (defun id->objc-class (i)
	(svref c i))
      (defun (setf id->objc-class) (new i)
	(setf (svref c i) new))
      (defun id->objc-metaclass (i)
	(svref m i))
      (defun (setf id->objc-metaclass) (new i)
	(setf (svref m i) new))
      (defun id->objc-class-wrapper (i)
	(svref cw i))
      (defun (setf id->objc-class-wrapper) (new i)
	(setf (svref cw i) new))
      (defun id->objc-metaclass-wrapper (i)
	(svref mw i))
      (defun (setf id->objc-metaclass-wrapper) (new i)
	(setf (svref mw i) new))
      (defun id->objc-class-slots-vector (i)
	(svref csv i))
      (defun (setf id->objc-class-slots-vector) (new i)
	(setf (svref csv i) new))
      (defun id->objc-metaclass-slots-vector (i)
	(svref msv i))
      (defun (setf id->objc-metaclass-slots-vector) (new i)
	(setf (svref msv i) new))
      (defun objc-class-id-foreign-name (i)
	(svref class-foreign-names i))
      (defun (setf objc-class-id-foreign-name) (new i)
	(setf (svref class-foreign-names i) new))
      (defun objc-metaclass-id-foreign-name (i)
	(svref metaclass-foreign-names i))
      (defun (setf objc-metaclass-id-foreign-name) (new i)
	(setf (svref metaclass-foreign-names i) new))
      (defun %clear-objc-class-maps ()
	(with-lock-grabbed (objc-class-lock)
	  (setf (splay-tree-root objc-class-map) nil
		(splay-tree-root objc-metaclass-map) nil
                (splay-tree-root private-objc-classes) nil
		(splay-tree-count objc-class-map) 0
		(splay-tree-count objc-metaclass-map) 0
                (splay-tree-count private-objc-classes) 0)))
      (flet ((install-objc-metaclass (meta)
	       (or (splay-tree-get objc-metaclass-map meta)
		   (let* ((id (assign-next-metaclass-id))
			  (meta (%inc-ptr meta 0)))
		     (splay-tree-put objc-metaclass-map meta id)
		     (setf (svref m id) meta
			   (svref msv id)
			   (make-objc-metaclass-slots-vector meta)
			   (svref metaclass-id->ordinal id)
			   (%next-class-ordinal))
		     id))))
	(defun register-objc-class (class)
	  "ensure that the class is mapped to a small integer and associate a slots-vector with it."
	  (with-lock-grabbed (objc-class-lock)
	    (ensure-objc-classptr-resolved class)
	    (or (splay-tree-get objc-class-map class)
		(let* ((id (assign-next-class-id))
		       (class (%inc-ptr class 0))
		       (meta (pref class #+apple-objc :objc_class.isa #+gnu-objc :objc_class.class_pointer)))
		  (splay-tree-put objc-class-map class id)
		  (setf (svref c id) class
			(svref csv id)
			(make-objc-class-slots-vector class)
			(svref class-id->metaclass-id id)
			(install-objc-metaclass meta)
			(svref class-id->ordinal id) (%next-class-ordinal))
		  id)))))
      (defun objc-class-id (class)
	(with-lock-grabbed (objc-class-lock)
	  (splay-tree-get objc-class-map class)))
      (defun objc-metaclass-id (meta)
	(with-lock-grabbed (objc-class-lock)
	  (splay-tree-get objc-metaclass-map meta)))
      (defun objc-class-id->objc-metaclass-id (class-id)
	(svref class-id->metaclass-id class-id))
      (defun objc-class-id->objc-metaclass (class-id)
	(svref m (svref class-id->metaclass-id class-id)))
      (defun objc-class-id->ordinal (i)
	(svref class-id->ordinal i))
      (defun (setf objc-class-id->ordinal) (new i)
	(setf (svref class-id->ordinal i) new))
      (defun objc-metaclass-id->ordinal (m)
	(svref metaclass-id->ordinal m))
      (defun (setf objc-metaclass-id->ordinal) (new m)
	(setf (svref class-id->ordinal m) new))
      (defun objc-class-map () objc-class-map)
      (defun %objc-class-count () next-objc-class-id)
      (defun objc-metaclass-map () objc-metaclass-map)
      (defun %objc-metaclass-count () next-objc-metaclass-id)
      (defun %register-private-objc-class (c name)
        (splay-tree-put private-objc-classes c (make-private-objc-class-info :name name)))
      (defun %get-private-objc-class (c)
        (splay-tree-get private-objc-classes c))
      (defun (setf %get-private-objc-class) (public c)
        (let* ((node (binary-tree-get private-objc-classes c)))
          (if node
            (setf (tree-node-value node) public)
            (error "Private class ~s not found" c))))
      (defun private-objc-classes ()
        private-objc-classes))))

(pushnew #'%clear-objc-class-maps *save-exit-functions* :test #'eq
         :key #'function-name)

(defun do-all-objc-classes (f)
  (map-splay-tree (objc-class-map) #'(lambda (id)
				       (funcall f (id->objc-class id)))))

(defun canonicalize-registered-class (c)
  (let* ((id (objc-class-id c)))
    (if id
      (id->objc-class id)
      (error "Class ~S isn't recognized." c))))

(defun canonicalize-registered-metaclass (m)
  (let* ((id (objc-metaclass-id m)))
    (if id
      (id->objc-metaclass id)
      (error "Class ~S isn't recognized." m))))

(defun canonicalize-registered-class-or-metaclass (x)
  (if (%objc-metaclass-p x)
    (canonicalize-registered-metaclass x)
    (canonicalize-registered-class x)))


;;; Open shared libs.
#+darwin-target
(progn
(defloadvar *cocoa-event-process* *initial-process*)


(defun current-ns-thread ()
  (with-cstrs ((class-name "NSThread")
               (message-selector-name "currentThread"))
    (let* ((nsthread-class (#_objc_lookUpClass class-name))
           (message-selector (#_sel_getUid message-selector-name)))
      (#_objc_msgSend nsthread-class message-selector)
      nil)))
  
(defun create-void-nsthread ()
  ;; Create an NSThread which does nothing but exit.
  ;; This'll help to convince the AppKit that we're
  ;; multitheaded.  (A lot of other things, including
  ;; the ObjC runtime, seem to have already noticed.)
  (with-cstrs ((thread-class-name "NSThread")
               (pool-class-name "NSAutoreleasePool")
               (thread-message-selector-name "detachNewThreadSelector:toTarget:withObject:")
               (exit-selector-name "exit")
               (alloc-selector-name "alloc")
               (init-selector-name "init")
               (release-selector-name "release"))
    (let* ((nsthread-class (#_objc_lookUpClass thread-class-name))
           (pool-class (#_objc_lookUpClass pool-class-name))
           (thread-message-selector (#_sel_getUid thread-message-selector-name))
           (exit-selector (#_sel_getUid exit-selector-name))
           (alloc-selector (#_sel_getUid alloc-selector-name))
           (init-selector (#_sel_getUid init-selector-name))
           (release-selector (#_sel_getUid release-selector-name))
           (pool (#_objc_msgSend
                  (#_objc_msgSend pool-class
                                  alloc-selector)
                  init-selector)))
      (unwind-protect
           (#_objc_msgSend nsthread-class thread-message-selector
                           :address exit-selector
                           :address nsthread-class
                           :address (%null-ptr))
        (#_objc_msgSend pool release-selector))
      nil)))

(defun run-in-cocoa-process-and-wait  (f)
  (let* ((process *cocoa-event-process*)
	 (success (cons nil nil))
	 (done (make-semaphore)))
    (process-interrupt process #'(lambda ()
				   (unwind-protect
					(progn
					  (setf (car success) (funcall f)))
				     (signal-semaphore done))))
    (wait-on-semaphore done)
    (car success)))


(def-ccl-pointers cocoa-framework ()
  (run-in-cocoa-process-and-wait
   #'(lambda ()
       ;; We need to load and "initialize" the CoreFoundation library
       ;; in the thread that's going to process events.  Looking up a
       ;; symbol in the library should cause it to be initialized
       (open-shared-library "/System/Library/Frameworks/Cocoa.framework/Cocoa")
       ;(#_GetCurrentEventQueue)
       (current-ns-thread)
       (create-void-nsthread))))


(defun find-cfstring-sections ()
  (warn "~s is obsolete" 'find-cfstring-sections))

)

#+gnu-objc
(progn
(defparameter *gnustep-system-root* "/usr/GNUstep/" "The root of all evil.")
(defparameter *gnustep-libraries-pathname*
  (merge-pathnames "System/Library/Libraries/" *gnustep-system-root*))

(defloadvar *pending-loaded-classes* ())

(defcallback register-class-callback (:address class :address category :void)
  (let* ((id (map-objc-class class)))
    (unless (%null-ptr-p category)
      (let* ((cell (or (assoc id *pending-loaded-classes*)
                       (let* ((c (list id)))
                         (push c *pending-loaded-classes*)
                         c))))
        (push (%inc-ptr category 0) (cdr cell))))))

;;; Shouldn't really be GNU-objc-specific.

(defun get-c-format-string (c-format-ptr c-arg-ptr)
  (do* ((n 128))
       ()
    (declare (fixnum n))
    (%stack-block ((buf n))
      (let* ((m (#_vsnprintf buf n c-format-ptr c-arg-ptr)))
	(declare (fixnum m))
	(cond ((< m 0) (return nil))
	      ((< m n) (return (%get-cstring buf)))
	      (t (setq n m)))))))



(defun init-gnustep-framework ()
  (or (getenv "GNUSTEP_SYSTEM_ROOT")
      (setenv "GNUSTEP_SYSTEM_ROOT" *gnustep-system-root*))
  (open-shared-library "libobjc.so.1")
  (setf (%get-ptr (foreign-symbol-address "_objc_load_callback"))
        register-class-callback)
  (open-shared-library (namestring (merge-pathnames "libgnustep-base.so"
                                                    *gnustep-libraries-pathname*)))
  (open-shared-library (namestring (merge-pathnames "libgnustep-gui.so"
                                                    *gnustep-libraries-pathname*))))

(def-ccl-pointers gnustep-framework ()
  (init-gnustep-framework))
)

(defun get-appkit-version ()
  #+apple-objc
  #&NSAppKitVersionNumber
  #+gnu-objc
  (get-foundation-version))

(defun get-foundation-version ()
  #&NSFoundationVersionNumber
  #+gnu-objc (%get-cstring (foreign-symbol-address "gnustep_base_version")))

(defparameter *appkit-library-version-number* (get-appkit-version))
(defparameter *foundation-library-version-number* (get-foundation-version))

(defparameter *extension-framework-paths* ())

;;; An instance of NSConstantString (which is a subclass of NSString)
;;; consists of a pointer to the NSConstantString class (which the
;;; global "_NSConstantStringClassReference" conveniently refers to), a
;;; pointer to an array of 8-bit characters (doesn't have to be #\Nul
;;; terminated, but doesn't hurt) and the length of that string (not
;;; counting any #\Nul.)
;;; The global reference to the "NSConstantString" class allows us to
;;; make instances of NSConstantString, ala the @"foo" construct in
;;; ObjC.  Sure it's ugly, but it seems to be exactly what the ObjC
;;; compiler does.


(defloadvar *NSConstantString-class*
  (with-cstrs ((name "NSConstantString"))
    #+apple-objc (#_objc_lookUpClass name)
    #+gnu-objc (#_objc_lookup_class name)))




#+apple-objc
(progn
;;; NSException-handling stuff.
;;; First, we have to jump through some hoops so that #_longjmp can
;;; jump through some hoops (a jmp_buf) and wind up throwing to a
;;; lisp catch tag.

;;; These constants (offsets in the jmp_buf structure) come from
;;; the _setjmp.h header file in the Darwin LibC source.

#+ppc32-target
(progn
(defconstant JMP-lr #x54 "link register (return address) offset in jmp_buf")
#|(defconstant JMP-ctr #x5c "count register jmp_buf offset")|#
(defconstant JMP-sp 0 "stack pointer offset in jmp_buf")
(defconstant JMP-r14 12 "offset of r14 (which we clobber) in jmp_buf")
(defconstant JMP-r15 16 "offset of r14 (which we also clobber) in jmp_buf"))

#+ppc64-target
(progn
(defconstant JMP-lr #xa8 "link register (return address) offset in jmp_buf")
#|(defconstant JMP-ctr #x5c "count register jmp_buf offset")|#
(defconstant JMP-sp 0 "stack pointer offset in jmp_buf")
(defconstant JMP-r13 #x10 "offset of r13 (which we preserve) in jmp_buf")
(defconstant JMP-r14 #x18 "offset of r14 (which we clobber) in jmp_buf")
(defconstant JMP-r15 #x20 "offset of r15 (which we also clobber) in jmp_buf"))

;;; These constants also come from Libc sources.  Hey, who needs
;;; header files ?
#+x8664-target
(progn
(defconstant JB-RBX 0)
(defconstant JB-RBP 8)
(defconstant JB-RSP 16)
(defconstant JB-R12 24)
(defconstant JB-R13 32)
(defconstant JB-R14 40)
(defconstant JB-R15 48)
(defconstant JB-RIP 56)
(defconstant JB-RFLAGS 64)
(defconstant JB-MXCSR 72)
(defconstant JB-FPCONTROL 76)
(defconstant JB-MASK 80)
)

;;; I think that we know where these constants come from.
#+x8632-target
(progn
  (defconstant JB-FPCW 0)
  (defconstant JB-MASK 4)
  (defconstant JB-MXCSR 8)
  (defconstant JB-EBX 12)
  (defconstant JB-ECX 16)
  (defconstant JB-EDX 20)
  (defconstant JB-EDI 24)
  (defconstant JB-ESI 28)
  (defconstant JB-EBP 32)
  (defconstant JB-ESP 36)
  (defconstant JB-SS 40)
  (defconstant JB-EFLAGS 44)
  (defconstant JB-EIP 48)
  (defconstant JB-CS 52)
  (defconstant JB-DS 56)
  (defconstant JB-ES 60)
  (defconstant JB-FS 64)
  (defconstant JB-GS 68)


  )

 

;;; A malloc'ed pointer to three words of machine code.  The first
;;; instruction copies the address of the trampoline callback from r14
;;; to the count register.  The second instruction (rather obviously)
;;; copies r15 to r4.  A C function passes its second argument in r4,
;;; but since r4 isn't saved in a jmp_buf, we have to do this copy.
;;; The second instruction just jumps to the address in the count
;;; register, which is where we really wanted to go in the first
;;; place.

#+ppc-target
(macrolet ((ppc-lap-word (instruction-form)
             (uvref (uvref (compile nil
                                    `(lambda (&lap 0)
                                      (ppc-lap-function () ((?? 0))
                                       ,instruction-form)))
                           0) #+ppc64-target 1 #+ppc32-target 0)))
  (defloadvar *setjmp-catch-lr-code*
      (let* ((p (malloc 12)))
        (setf (%get-unsigned-long p 0) (ppc-lap-word (mtctr 14))
              (%get-unsigned-long p 4) (ppc-lap-word (mr 4 15))
              (%get-unsigned-long p 8) (ppc-lap-word (bctr)))
        ;;; Force this code out of the data cache and into memory, so
        ;;; that it'll get loaded into the icache.
        (ff-call (%kernel-import #.target::kernel-import-makedataexecutable) 
                 :address p 
                 :unsigned-fullword 12
                 :void)
        p)))

;;; This isn't used; it isn't right, either.
#+x8664-target
(defloadvar *setjmp-catch-rip-code*
    (let* ((code-bytes '(#x4c #x89 #xe6     ; movq %r12, %rsi
                         #xff #xd3))        ; call *%rbx
           (nbytes (length code-bytes))
           (p (malloc nbytes)))
      (dotimes (i nbytes p)
        (setf (%get-unsigned-byte p i) (pop code-bytes)))))

#+x8632-target
(defloadvar *setjmp-catch-rip-code*
    (let* ((code-bytes '(#x83 #xec #x10 ; subl $16,%esp
                         #x89 #x04 #x24 ; movl %eax,(%esp)
                         #x89 #x7c #x24 #x04   ; movl %edi,4(%esp)
                         #xff #xd3))    ; call *%ebx
           (nbytes (length code-bytes))
           (p (malloc nbytes)))
      (dotimes (i nbytes p)
        (setf (%get-unsigned-byte p i) (pop code-bytes)))))

;;; Catch frames are allocated on a stack, so it's OK to pass their
;;; addresses around to foreign code.
(defcallback throw-to-catch-frame (:signed-fullword value
                                   :address frame
                                   :void)
  (throw (%get-object frame target::catch-frame.catch-tag) value))

;;; Initialize a jmp_buf so that when it's #_longjmp-ed to, it'll
;;; wind up calling THROW-TO-CATCH-FRAME with the specified catch
;;; frame as its second argument.  The C frame used here is just
;;; an empty C stack frame from which the callback will be called.

#+ppc-target
(defun %associate-jmp-buf-with-catch-frame (jmp-buf catch-frame c-frame)
  (%set-object jmp-buf JMP-sp c-frame)
  (%set-object jmp-buf JMP-r15 catch-frame)
  #+ppc64-target
  (%set-object jmp-buf JMP-r13 (%get-os-context))
  (setf (%get-ptr jmp-buf JMP-lr) *setjmp-catch-lr-code*
        (%get-ptr jmp-buf JMP-r14) throw-to-catch-frame)
  t)

#+x8664-target
(defun %associate-jmp-buf-with-catch-frame (jmp-buf catch-frame c-frame)
  (setf (%get-ptr jmp-buf JB-rbx) throw-to-catch-frame
        (%get-ptr jmp-buf JB-rip) *setjmp-catch-rip-code*)
  (setf (%get-unsigned-long jmp-buf JB-mxcsr) #x1f80
        (%get-unsigned-long jmp-buf JB-fpcontrol) #x37f)
  (%set-object jmp-buf JB-RSP c-frame)
  (%set-object jmp-buf JB-RBP c-frame)
  (%set-object jmp-buf JB-r12 catch-frame)
  t)

#+x8632-target
;;; Ugh.  Apple stores segment register values in jmp_bufs.  You know,
;;; since they're so volatile and everything.
(defun %associate-jmp-buf-with-catch-frame (jmp-buf catch-frame c-frame)
  (setf (%get-unsigned-word jmp-buf JB-FS) (%get-fs-register)
        (%get-unsigned-word jmp-buf JB-GS) (%get-gs-register)
        (%get-unsigned-word jmp-buf JB-CS) #x17
        (%get-unsigned-word jmp-buf JB-DS) #x1f
        (%get-unsigned-word jmp-buf JB-ES) #x1f
        (%get-unsigned-word jmp-buf JB-SS) #x1f)
  (%set-object jmp-buf JB-ESP c-frame)
  (%set-object jmp-buf JB-EBP c-frame)
  (setf (%get-unsigned-long jmp-buf JB-MXCSR) #x1f80
        (%get-unsigned-long jmp-buf JB-FPCW) #x37f
        (%get-unsigned-long jmp-buf JB-MASK) 0)
  (setf (%get-ptr jmp-buf JB-EBX) throw-to-catch-frame
        (%get-ptr jmp-buf JB-EIP) *setjmp-catch-rip-code*)
  (%set-object jmp-buf JB-EDI catch-frame)
  t)
  

        

)

;;; When starting up an image that's had ObjC classes in it, all of
;;; those canonical classes (and metaclasses) will have had their type
;;; changed (by SAVE-APPLICATION) to, CCL::DEAD-MACPTR and the addresses
;;; of those classes may be bogus.  The splay trees (objc-class/metaclass-map)
;;; should be empty.
;;; For each class that -had- had an assigned ID, determine its ObjC
;;; class name, and ask ObjC where (if anywhere) the class is now.
;;; If we get a non-null answer, revive the class pointer and set its
;;; address appropriately, then add an entry to the splay tree; this
;;; means that classes that existed on both sides of SAVE-APPLICATION
;;; will retain the same ID.

(defun revive-objc-classes ()
  ;; We need to do some things so that we can use (@class ...)
  ;; and (@selector ...) early.
  (invalidate-objc-class-descriptors)
  (clear-objc-selectors)
  (clear-objc-protocols)
  (reset-objc-class-count)
  ;; Ensure that any addon frameworks are loaded.
  (dolist (path *extension-framework-paths*)
    (%reload-objc-framework path))
  ;; Make a first pass over the class and metaclass tables;
  ;; resolving those foreign classes that existed in the old
  ;; image and still exist in the new.
  (let* ((class-map (objc-class-map))
	 (metaclass-map (objc-metaclass-map))
	 (nclasses (%objc-class-count)))
    (dotimes (i nclasses)
      (let* ((c (id->objc-class i))
	     (meta-id (objc-class-id->objc-metaclass-id i))
	     (m (id->objc-metaclass meta-id)))
        (unless (typep c 'macptr)
          (%revive-macptr c)
          (%setf-macptr c (%null-ptr)))
        (unless (typep m 'macptr)
          (%revive-macptr m)
          (%setf-macptr m (%null-ptr)))
	(unless (splay-tree-get class-map c)
	  (%set-pointer-to-objc-class-address (objc-class-id-foreign-name i) c)
	  ;; If the class is valid and the metaclass is still
	  ;; unmapped, set the metaclass pointer's address and map it.
	  (unless (%null-ptr-p c)
	    (splay-tree-put class-map c i)
	    (unless (splay-tree-get metaclass-map m)
              (%setf-macptr m (pref c #+apple-objc :objc_class.isa
				      #+gnu-objc :objc_class.class_pointer))
	      (splay-tree-put metaclass-map m meta-id))
            (note-class-protocols c)))))
    ;; Second pass: install class objects for user-defined classes,
    ;; assuming the superclasses are already "revived".  If the
    ;; superclass is itself user-defined, it'll appear first in the
    ;; class table; that's an artifact of the current implementation.
    (dotimes (i nclasses)
      (let* ((c (id->objc-class i)))
	(when (and (%null-ptr-p c)
		   (not (slot-value c 'foreign)))
	  (let* ((super (dolist (s (class-direct-superclasses c)
				 (error "No ObjC superclass of ~s" c))
			  (when (objc-class-p s) (return s))))
		 (meta-id (objc-class-id->objc-metaclass-id i))
		 (m (id->objc-metaclass meta-id)))
            (let* ((class (make-objc-class-pair super (make-cstring (objc-class-id-foreign-name i))))
                   (meta (pref class #+apple-objc :objc_class.isa
                               #+gnu-objc :objc-class.class_pointer)))
	    (unless (splay-tree-get metaclass-map m)
	      (%revive-macptr m)
	      (%setf-macptr m meta)
	      (splay-tree-put metaclass-map m meta-id))
	    (%setf-macptr c class))
            #+apple-objc-2.0
            (%revive-foreign-slots c)
            #+apple-objc-2.0
            (%add-objc-class c)
            #-apple-objc-2.0
	    (multiple-value-bind (ivars instance-size)
		(%make-objc-ivars c)
	      (%add-objc-class c ivars instance-size))
	    (splay-tree-put class-map c i)))))
    ;; Finally, iterate over all classes in the runtime world.
    ;; Register any class that's not found in the class map
    ;; as a "private" ObjC class.
    ;; Iterate over all classes in the runtime.  Those that
    ;; aren't already registered will get identified as
    ;; "private" (undeclared) ObjC classes.
    ;; Note that this means that if an application bundle
    ;; was saved on (for instance) Panther and Tiger interfaces
    ;; were used, and then the application is run on Tiger, any
    ;; Tiger-specific classes will not be magically integrated
    ;; into CLOS in the running application.
    ;; A development envronment might want to provide such a
    ;; mechanism; it would need access to Panther class
    ;; declarations, and - in the general case - a standalone
    ;; application doesn't necessarily have access to the
    ;; interface database.
    (map-objc-classes nil)
    ))

(pushnew #'revive-objc-classes *lisp-system-pointer-functions*
	 :test #'eq
	 :key #'function-name)
    

(defun %objc-class-instance-size (c)
  #+apple-objc-2.0
  (#_class_getInstanceSize c)
  #-apple-objc-2.0
  (pref c :objc_class.instance_size))

(defun find-named-objc-superclass (class string)
  (unless (or (null string) (%null-ptr-p class))
    (with-macptrs ((name #+apple-objc-2.0 (#_class_getName class)
                         #-apple-objc-2.0 (pref class :objc_class.name)))
      (or
       (dotimes (i (length string) class)
         (let* ((b (%get-unsigned-byte name i)))
           (unless (eq b (char-code (schar string i)))
             (return))))
       (find-named-objc-superclass #+apple-objc-2.0 (#_class_getSuperclass class)
                                   #-apple-objc-2.0 (pref class :objc_class.super_class)
                                   string)))))

(defun install-foreign-objc-class (class &optional (use-db t))
  (let* ((id (objc-class-id class)))
    (unless id
      (let* ((name (%get-cstring #+apple-objc-2.0 (#_class_getName class)
                                 #-apple-objc-2.0 (pref class :objc_class.name)))
             (decl (get-objc-class-decl name use-db)))
        (if (null decl)
          (or (%get-private-objc-class class)
              (%register-private-objc-class class name))
          (progn
            (setq id (register-objc-class class)
                  class (id->objc-class id))
            ;; If not mapped, map the superclass (if there is one.)
            (let* ((super (find-named-objc-superclass
                           #+apple-objc-2.0
                           (#_class_getSuperclass class)
                           #-apple-objc-2.0
                           (pref class :objc_class.super_class)
                           (db-objc-class-info-superclass-name decl))))
              (unless (null super)
                (install-foreign-objc-class super))
              (let* ((class-name 
                      (objc-to-lisp-classname
                       name
                       "NS"))
                     (meta-id
                      (objc-class-id->objc-metaclass-id id)) 
                     (meta (id->objc-metaclass meta-id)))
                ;; Metaclass may already be initialized.  It'll have a
                ;; class wrapper if so.
                (unless (id->objc-metaclass-wrapper meta-id)
                  (let* ((meta-foreign-name
                          (%get-cstring
                           #+apple-objc-2.0
                           (#_class_getName meta)
                           #-apple-objc-2.0
                           (pref meta :objc_class.name)))
                         (meta-name
                          (intern
                           (concatenate 'string
                                        "+"
                                        (string
                                         (objc-to-lisp-classname
                                          meta-foreign-name
                                          "NS")))
                           "NS"))
                         (meta-super
                          (if super (pref super #+apple-objc :objc_class.isa
                                          #+gnu-objc :objc_class.class_pointer))))
                    ;; It's important (here and when initializing the
                    ;; class below) to use the "canonical"
                    ;; (registered) version of the class, since some
                    ;; things in CLOS assume EQness.  We probably
                    ;; don't want to violate that assumption; it'll be
                    ;; easier to revive a saved image if we don't have
                    ;; a lot of EQL-but-not-EQ class pointers to deal
                    ;; with.
                    (initialize-instance
                     meta
                     :name meta-name
                     :direct-superclasses
                     (list
                      (if (or (null meta-super)
                              (not (%objc-metaclass-p meta-super)))
                        (find-class 'objc:objc-class)
                        (canonicalize-registered-metaclass meta-super)))
                     :peer class
                     :foreign t)
                    (setf (objc-metaclass-id-foreign-name meta-id)
                          meta-foreign-name)
                    (setf (find-class meta-name) meta)
                    (%defglobal meta-name meta)))
                (setf (slot-value class 'direct-slots)
                      (compute-objc-direct-slots-from-info decl class))
                (initialize-instance
                 class
                 :name class-name
                 :direct-superclasses
                 (list
                  (if (null super)
                    (find-class 'objc:objc-object)
                    (canonicalize-registered-class super)))
                 :peer meta
                 :foreign t)
                (setf (objc-class-id-foreign-name id)
                      name)
                (setf (find-class class-name) class)
                (%defglobal class-name class)
                class))))))))
				


;;; Execute the body with the variable NSSTR bound to a
;;; stack-allocated NSConstantString instance (made from
;;; *NSConstantString-class*, CSTRING and LEN).
(defmacro with-nsstr ((nsstr cstring len) &body body)
  #+apple-objc
  `(rlet ((,nsstr :<NSC>onstant<S>tring
	   :isa *NSConstantString-class*
	   :bytes ,cstring
	   :num<B>ytes ,len))
      ,@body)
  #+gnu-objc
  `(rlet ((,nsstr :<NXC>onstant<S>tring
	   :isa *NSConstantString-class*
	   :c_string ,cstring
	   :len ,len))
    ,@body))

;;; Make a persistent (heap-allocated) NSConstantString.

(defun %make-constant-nsstring (string)
  "Make a persistent (heap-allocated) NSConstantString from the
argument lisp string."
  #+apple-objc
  (make-record :<NSC>onstant<S>tring
	       :isa *NSConstantString-Class*
	       :bytes (make-cstring string)
	       :num<B>ytes (length string))
  #+gnu-objc
  (make-record :<NXC>onstant<S>tring
	       :isa *NSConstantString-Class*
	       :c_string (make-cstring string)
	       :len (length string))
  )

;;; Class declarations
(defparameter *objc-class-declarations* (make-hash-table :test #'equal))

(defun register-objc-class-decls ()
  (do-interface-dirs (d)
    (dolist (class-name (cdb-enumerate-keys (db-objc-classes d)))
      (get-objc-class-decl class-name t))))


(defun get-objc-class-decl (class-name &optional (use-db nil))
  (or (gethash class-name *objc-class-declarations*)
      (and use-db
           (let* ((decl (%find-objc-class-info class-name)))
             (when decl
               (setf (gethash class-name *objc-class-declarations*) decl))))))

(defun %ensure-class-declaration (name super-name)
  (unless (get-objc-class-decl name)
    (setf (gethash name *objc-class-declarations*)
          (make-db-objc-class-info :class-name (string name)
                                   :superclass-name (string super-name))))
  name)

;;; It's hard (and questionable) to allow ivars here.
(defmacro declare-objc-class (name super-name)
  `(%ensure-class-declaration ',name ',super-name))

;;; Intern NSConstantString instances.
(defvar *objc-constant-strings* (make-hash-table :test #'equal))

(defstruct objc-constant-string
  string
  nsstringptr)

(defun ns-constant-string (string)
  (or (gethash string *objc-constant-strings*)
      (setf (gethash string *objc-constant-strings*)
	    (make-objc-constant-string :string string
				       :nsstringptr (%make-constant-nsstring string)))))

(def-ccl-pointers objc-strings ()
  (maphash #'(lambda (string cached)
	       (setf (objc-constant-string-nsstringptr cached)
		     (%make-constant-nsstring string)))
	   *objc-constant-strings*))

(defmethod make-load-form ((s objc-constant-string) &optional env)
  (declare (ignore env))
  `(ns-constant-string ,(objc-constant-string-string s)))

(defmacro @ (string)
  `(objc-constant-string-nsstringptr ,(ns-constant-string string)))

#+gnu-objc
(progn
  (defcallback lisp-objc-error-handler (:id receiver :int errcode (:* :char) format :address argptr :<BOOL>)
    (let* ((message (get-c-format-string format argptr)))
      (error "ObjC runtime error ~d, receiver ~s :~& ~a"
	     errcode receiver message))
    #$YES)

  (def-ccl-pointers install-lisp-objc-error-handler ()
    (#_objc_set_error_handler lisp-objc-error-handler)))





;;; Registering named objc classes.


(defun objc-class-name-string (name)
  (etypecase name
    (symbol (lisp-to-objc-classname name))
    (string name)))

;;; We'd presumably cache this result somewhere, so we'd only do the
;;; lookup once per session (in general.)
(defun lookup-objc-class (name &optional error-p)
  (with-cstrs ((cstr (objc-class-name-string name)))
    (let* ((p (#+apple-objc #_objc_lookUpClass
               #+gnu-objc #_objc_lookup_class
	       cstr)))
      (if (%null-ptr-p p)
	(if error-p
	  (error "ObjC class ~a not found" name))
	p))))

(defun %set-pointer-to-objc-class-address (class-name-string ptr)
  (with-cstrs ((cstr class-name-string))
    (%setf-macptr ptr
		  (#+apple-objc #_objc_lookUpClass
		   #+gnu-objc #_objc_lookup_class
		   cstr)))
  nil)
   
		  

(defvar *objc-class-descriptors* (make-hash-table :test #'equal))


(defstruct objc-class-descriptor
  name
  classptr)

(defun invalidate-objc-class-descriptors ()
  (maphash #'(lambda (name descriptor)
	       (declare (ignore name))
	       (setf (objc-class-descriptor-classptr descriptor) nil))
	   *objc-class-descriptors*))

(defun %objc-class-classptr (class-descriptor &optional (error-p t))
  (or (objc-class-descriptor-classptr class-descriptor)
      (setf (objc-class-descriptor-classptr class-descriptor)
	    (lookup-objc-class (objc-class-descriptor-name class-descriptor)
			       error-p))))

(defun load-objc-class-descriptor (name)
  (let* ((descriptor (or (gethash name *objc-class-descriptors*)
			 (setf (gethash name *objc-class-descriptors*)
			       (make-objc-class-descriptor  :name name)))))
    (%objc-class-classptr descriptor nil)
    descriptor))

(defmacro objc-class-descriptor (name)
  `(load-objc-class-descriptor ,name))

(defmethod make-load-form ((o objc-class-descriptor) &optional env)
  (declare (ignore env))
  `(load-objc-class-descriptor ,(objc-class-descriptor-name o)))

(defmacro @class (name)
  (let* ((name (objc-class-name-string name)))
    `(the (@metaclass ,name) (%objc-class-classptr ,(objc-class-descriptor name)))))

;;; This isn't quite the inverse operation of LOOKUP-OBJC-CLASS: it
;;; returns a simple C string.  and can be applied to a class or any
;;; instance (returning the class name.)
(defun objc-class-name (object)
  #+apple-objc
  (with-macptrs (p)
    (%setf-macptr p (#_object_getClassName object))
    (unless (%null-ptr-p p)
      (%get-cstring p)))
  #+gnu-objc
  (unless (%null-ptr-p object)
    (with-macptrs ((parent (pref object :objc_object.class_pointer)))
      (unless (%null-ptr-p parent)
        (if (logtest (pref parent :objc_class.info) #$_CLS_CLASS)
          (%get-cstring (pref parent :objc_class.name))
          (%get-cstring (pref object :objc_class.name)))))))


;;; Likewise, we want to cache the selectors ("SEL"s) which identify
;;; method names.  They can vary from session to session, but within
;;; a session, all methods with a given name (e.g, "init") will be
;;; represented by the same SEL.
(defun get-selector-for (method-name &optional error)
  (with-cstrs ((cmethod-name method-name))
    (let* ((p (#+apple-objc #_sel_getUid
	       #+gnu-objc #_sel_get_uid
	       cmethod-name)))
      (if (%null-ptr-p p)
	(if error
	  (error "Can't find ObjC selector for ~a" method-name))
	p))))

(defvar *objc-selectors* (make-hash-table :test #'equal))

(defstruct objc-selector
  name
  %sel)

(defun %get-SELECTOR (selector &optional (error-p t))
  (or (objc-selector-%sel selector)
      (setf (objc-selector-%sel selector)
	    (get-selector-for (objc-selector-name selector) error-p))))

(defun clear-objc-selectors ()
  (maphash #'(lambda (name sel)
	       (declare (ignore name))
	       (setf (objc-selector-%sel sel) nil))
	   *objc-selectors*))

;;; Find or create a SELECTOR; don't bother resolving it.
(defun ensure-objc-selector (name)
  (setq name (string name))
  (or (gethash name *objc-selectors*)
      (setf (gethash name *objc-selectors*)
            (make-objc-selector :name name))))

(defun load-objc-selector (name)
  (let* ((selector (ensure-objc-selector name)))
    (%get-SELECTOR selector nil)
    selector))

(defmacro @SELECTOR (name)
  `(%get-selector ,(load-objc-selector name)))

(defmethod make-load-form ((s objc-selector) &optional env)
  (declare (ignore env))
  `(load-objc-selector ,(objc-selector-name s)))


;;; Convert a Lisp object X to a desired foreign type FTYPE 
;;; The following conversions are currently done:
;;;   - T/NIL => #$YES/#$NO
;;;   - NIL => (%null-ptr)
;;;   - Lisp numbers  => SINGLE-FLOAT when possible

(defun coerce-to-bool (x)
  (let ((x-temp (gensym)))
    `(let ((,x-temp ,x))
       (if (or (eq ,x-temp 0) (null ,x-temp))
         #.#$NO
         #.#$YES))))

(declaim (inline %coerce-to-bool))
(defun %coerce-to-bool (x)
  (if (and x (not (eql x 0)))
    #$YES
    #$NO))

(defun coerce-to-address (x)
  (let ((x-temp (gensym)))
    `(let ((,x-temp ,x))
       (cond ((null ,x-temp) +null-ptr+)
	     (t ,x-temp)))))

;;; This is generally a bad idea; it forces us to
;;; box intermediate pointer arguments in order
;;; to typecase on them, and it's not clear to
;;; me that it offers much in the way of additional
;;; expressiveness.
(declaim (inline %coerce-to-address))
(defun %coerce-to-address (x)
  (etypecase x
    (macptr x)
    (null (%null-ptr))))

(defun coerce-to-foreign-type (x ftype)
   (cond ((and (constantp x) (constantp ftype))
          (case ftype
            (:id (if (null x) `(%null-ptr) (coerce-to-address x)))
            (:<BOOL> (coerce-to-bool (eval x)))
            (t x)))
         ((constantp ftype)
          (case ftype
            (:id `(%coerce-to-address ,x))
            (:<BOOL> `(%coerce-to-bool ,x))
            (t x)))
         (t `(case ,(if (atom ftype) ftype)
               (:id (%coerce-to-address ,x))
               (:<BOOL> (%coerce-to-bool ,x))
               (t ,x)))))

(defun objc-arg-coerce (typespec arg)
  (case typespec
    (:<BOOL> `(%coerce-to-bool ,arg))
    (:id `(%coerce-to-address ,arg))
    (t arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       Boolean Return Hackery                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Convert a foreign object X to T or NIL 

(defun coerce-from-bool (x)
  (cond
   ((eq x #$NO) nil)
   ((eq x #$YES) t)
   (t (error "Cannot coerce ~S to T or NIL" x))))

(defun objc-result-coerce (type result)
  (cond ((eq type :<BOOL>)
         `(coerce-from-bool ,result))
        (t result)))

;;; Add a faster way to get the message from a SEL by taking advantage of the
;;; fact that a selector is really just a canonicalized, interned C string
;;; containing the message.  (This is an admitted modularity violation;
;;; there's a more portable but slower way to do this if we ever need to.)


(defun lisp-string-from-sel (sel)
  (%get-cstring
   #+apple-objc sel
   #+gnu-objc (#_sel_get_name sel)))

;;; #_objc_msgSend takes two required arguments (the receiving object
;;; and the method selector) and 0 or more additional arguments;
;;; there'd have to be some macrology to handle common cases, since we
;;; want the compiler to see all of the args in a foreign call.

;;; I don't remmber what the second half of the above comment might
;;; have been talking about.

(defmacro objc-message-send (receiver selector-name &rest argspecs)
  (when (evenp (length argspecs))
    (setq argspecs (append argspecs '(:id))))
  #+apple-objc
  (funcall (ftd-ff-call-expand-function *target-ftd*)
           `(%ff-call (%reference-external-entry-point (load-time-value (external "_objc_msgSend"))))
           `(:address ,receiver :<SEL> (@selector ,selector-name) ,@argspecs)
           :arg-coerce 'objc-arg-coerce
           :result-coerce 'objc-result-coerce)  
  #+gnu-objc
    (let* ((r (gensym))
	 (s (gensym))
	 (imp (gensym)))
    `(with-macptrs ((,r ,receiver)
		    (,s (@selector ,selector-name))
		    (,imp (external-call "objc_msg_lookup"
					:id ,r
					:<SEL> ,s
					:<IMP>)))
      (funcall (ftd-ff-call-expand-function *target-ftd*)
       `(%ff-call ,imp)
       `(:address ,receiver :<SEL> (@selector ,selector-name) ,@argspecs)
       :arg-coerce 'objc-arg-coerce
       :result-coerce 'objc-result-coerce))))

(defmacro objc-message-send-with-selector (receiver selector &rest argspecs)
  (when (evenp (length argspecs))
    (setq argspecs (append argspecs '(:id))))
  #+apple-objc
  (funcall (ftd-ff-call-expand-function *target-ftd*)
           `(%ff-call (%reference-external-entry-point (load-time-value (external "_objc_msgSend"))))
           `(:address ,receiver :<SEL> (%get-selector ,selector) ,@argspecs)
           :arg-coerce 'objc-arg-coerce
           :result-coerce 'objc-result-coerce)  
  #+gnu-objc
    (let* ((r (gensym))
	 (s (gensym))
	 (imp (gensym)))
    `(with-macptrs ((,r ,receiver)
		    (,s (%get-selector ,selector))
		    (,imp (external-call "objc_msg_lookup"
					:id ,r
					:<SEL> ,s
					:<IMP>)))
      (funcall (ftd-ff-call-expand-function *target-ftd*)
       `(%ff-call ,imp)
       `(:address ,receiver :<SEL> ,s ,@argspecs)
       :arg-coerce 'objc-arg-coerce
       :result-coerce 'objc-result-coerce))))

;;; A method that returns a structure does so by platform-dependent
;;; means.  One of those means (which is fairly common) is to pass a
;;; pointer to an instance of a structure type as a first argument to
;;; the method implementation function (thereby making SELF the second
;;; argument, etc.), but whether or not it's actually done that way
;;; depends on the platform and on the structure type.  The special
;;; variable CCL::*TARGET-FTD* holds a structure (of type
;;; CCL::FOREIGN-TYPE-DATA) which describes some static attributes of
;;; the foreign type system on the target platform and contains some
;;; functions which can determine dynamic ABI attributes.  One such
;;; function can be used to determine whether or not the "invisible
;;; first arg" convention is used to return structures of a given
;;; foreign type; another function in *TARGET-FTD* can be used to
;;; construct a foreign function call form that handles
;;; structure-return and structure-types-as-arguments details.  In the
;;; Apple ObjC runtime, #_objc_msgSend_stret must be used if the
;;; invisible-first-argument convention is used to return a structure
;;; and must NOT be used otherwise. (The Darwin ppc64 and all
;;; supported x86-64 ABIs often use more complicated structure return
;;; conventions than ppc32 Darwin or ppc Linux.)  We should use
;;; OBJC-MESSAGE-SEND-STRET to send any message that returns a
;;; structure or union, regardless of how that structure return is
;;; actually implemented.

(defmacro objc-message-send-stret (structptr receiver selector-name &rest argspecs)
    #+apple-objc
    (let* ((return-typespec (car (last argspecs)))
           (entry-name (if (funcall (ftd-ff-call-struct-return-by-implicit-arg-function *target-ftd*) return-typespec)
                         "_objc_msgSend_stret"
                         "_objc_msgSend")))
      (funcall (ftd-ff-call-expand-function *target-ftd*)
               `(%ff-call (%reference-external-entry-point (load-time-value (external ,entry-name))))
        `(,structptr :address ,receiver :<SEL> (@selector ,selector-name) ,@argspecs)
               :arg-coerce 'objc-arg-coerce
               :result-coerce 'objc-result-coerce))
    #+gnu-objc
    (let* ((r (gensym))
	 (s (gensym))
	 (imp (gensym)))
    `(with-macptrs ((,r ,receiver)
		    (,s (@selector ,selector-name))
		    (,imp (external-call "objc_msg_lookup"
					 :id ,r
					 :<SEL> ,s
					 :<IMP>)))
      ,      (funcall (ftd-ff-call-expand-function *target-ftd*)
               `(%ff-call ,imp)
              `(,structptr :address ,receiver :<SEL> ,s ,@argspecs)
               :arg-coerce 'objc-arg-coerce
               :result-coerce 'objc-result-coerce))))

(defmacro objc-message-send-stret-with-selector (structptr receiver selector &rest argspecs)
    #+apple-objc
    (let* ((return-typespec (car (last argspecs)))
           (entry-name (if (funcall (ftd-ff-call-struct-return-by-implicit-arg-function *target-ftd*) return-typespec)
                         "_objc_msgSend_stret"
                         "_objc_msgSend")))
      (funcall (ftd-ff-call-expand-function *target-ftd*)
               `(%ff-call (%reference-external-entry-point (load-time-value (external ,entry-name))))
        `(,structptr :address ,receiver :<SEL> (%get-selector ,selector) ,@argspecs)
               :arg-coerce 'objc-arg-coerce
               :result-coerce 'objc-result-coerce))
    #+gnu-objc
    (let* ((r (gensym))
	 (s (gensym))
	 (imp (gensym)))
    `(with-macptrs ((,r ,receiver)
		    (,s (%get-selector ,selector))
		    (,imp (external-call "objc_msg_lookup"
					 :id ,r
					 :<SEL> ,s
					 :<IMP>)))
      ,      (funcall (ftd-ff-call-expand-function *target-ftd*)
               `(%ff-call ,imp)
              `(,structptr :address ,receiver :<SEL> ,s ,@argspecs)
               :arg-coerce 'objc-arg-coerce
               :result-coerce 'objc-result-coerce))))

;;; #_objc_msgSendSuper is similar to #_objc_msgSend; its first argument
;;; is a pointer to a structure of type objc_super {self,  the defining
;;; class's superclass}.  It only makes sense to use this inside an
;;; objc method.
(defmacro objc-message-send-super (super selector-name &rest argspecs)
  (when (evenp (length argspecs))
    (setq argspecs (append argspecs '(:id))))
  #+apple-objc
  (funcall (ftd-ff-call-expand-function *target-ftd*)
           `(%ff-call (%reference-external-entry-point (load-time-value (external "_objc_msgSendSuper"))))
           `(:address ,super :<SEL> (@selector ,selector-name) ,@argspecs)
           :arg-coerce 'objc-arg-coerce
           :result-coerce 'objc-result-coerce)
  #+gnu-objc
  (let* ((sup (gensym))
	 (sel (gensym))
	 (imp (gensym)))
    `(with-macptrs ((,sup ,super)
		    (,sel (@selector ,selector-name))
		    (,imp (external-call "objc_msg_lookup_super"
					 :<S>uper_t ,sup
					 :<SEL> ,sel
					 :<IMP>)))
  (funcall (ftd-ff-call-expand-function *target-ftd*)
   `(%ff-call ,imp)
   `(:id (pref ,sup :<S>uper.self)
     :<SEL> ,sel
     ,@argspecs)))))

(defmacro objc-message-send-super-with-selector (super selector &rest argspecs)
  (when (evenp (length argspecs))
    (setq argspecs (append argspecs '(:id))))
  #+apple-objc
  (funcall (ftd-ff-call-expand-function *target-ftd*)
           `(%ff-call (%reference-external-entry-point (load-time-value (external "_objc_msgSendSuper"))))
           `(:address ,super :<SEL> ,selector ,@argspecs)
           :arg-coerce 'objc-arg-coerce
           :result-coerce 'objc-result-coerce)
  #+gnu-objc
  (let* ((sup (gensym))
	 (sel (gensym))
	 (imp (gensym)))
    `(with-macptrs ((,sup ,super)
		    (,sel ,selector)
		    (,imp (external-call "objc_msg_lookup_super"
					 :<S>uper_t ,sup
					 :<SEL> ,sel
					 :<IMP>)))
  (funcall (ftd-ff-call-expand-function *target-ftd*)
   `(%ff-call ,imp)
   `(:id (pref ,sup :<S>uper.self)
     :<SEL> ,sel
     ,@argspecs)))))

;;; Send to superclass method, returning a structure. See above.
(defmacro objc-message-send-super-stret
    (structptr super selector-name &rest argspecs)
  #+apple-objc
    (let* ((return-typespec (car (last argspecs)))
           (entry-name (if (funcall (ftd-ff-call-struct-return-by-implicit-arg-function *target-ftd*) return-typespec)
                         "_objc_msgSendSuper_stret"
                         "_objc_msgSendSuper")))
      (funcall (ftd-ff-call-expand-function *target-ftd*)
               `(%ff-call (%reference-external-entry-point (load-time-value (external ,entry-name))))
               `(,structptr :address ,super :<SEL> (@selector ,selector-name) ,@argspecs)
               :arg-coerce 'objc-arg-coerce
               :result-coerce 'objc-result-coerce))
  #+gnu-objc
  (let* ((sup (gensym))
	 (sel (gensym))
	 (imp (gensym)))
    `(with-macptrs ((,sup ,super)
		    (,sel (@selector ,selector-name))
		    (,imp (external-call "objc_msg_lookup_super"
					 :<S>uper_t ,sup
					 :<SEL> ,sel
					 :<IMP>)))
      (funcall (ftd-ff-call-expand-function *target-ftd*)
       `(%ff-call ,imp)
       ,structptr
       :id (pref ,sup :<S>uper.self)
       :<SEL> ,sel
       ,@argspecs))))

(defmacro objc-message-send-super-stret-with-selector
    (structptr super selector &rest argspecs)
  #+apple-objc
    (let* ((return-typespec (car (last argspecs)))
           (entry-name (if (funcall (ftd-ff-call-struct-return-by-implicit-arg-function *target-ftd*) return-typespec)
                         "_objc_msgSendSuper_stret"
                         "_objc_msgSendSuper")))
      (funcall (ftd-ff-call-expand-function *target-ftd*)
               `(%ff-call (%reference-external-entry-point (load-time-value (external ,entry-name))))
               `(,structptr :address ,super :<SEL> ,selector ,@argspecs)
               :arg-coerce 'objc-arg-coerce
               :result-coerce 'objc-result-coerce))
  #+gnu-objc
  (let* ((sup (gensym))
	 (sel (gensym))
	 (imp (gensym)))
    `(with-macptrs ((,sup ,super)
		    (,sel ,selector)
		    (,imp (external-call "objc_msg_lookup_super"
					 :<S>uper_t ,sup
					 :<SEL> ,sel
					 :<IMP>)))
      (funcall (ftd-ff-call-expand-function *target-ftd*)
       `(%ff-call ,imp)
       ,structptr
       :id (pref ,sup :<S>uper.self)
       :<SEL> ,sel
       ,@argspecs))))

(defun message-send-form-for-call (receiver selector args super-p struct-return-var)
  (if struct-return-var
    (if super-p
      `(objc-message-send-super-stret-with-selector ,struct-return-var ,receiver ,selector ,@args)
      `(objc-message-send-stret-with-selector ,struct-return-var ,receiver ,selector ,@args))
    (if super-p
      `(objc-message-send-super-with-selector ,receiver ,selector ,@args)
      `(objc-message-send-with-selector ,receiver ,selector ,@args))))


#+(and apple-objc x8664-target)
(defun %process-varargs-list (gpr-pointer fpr-pointer stack-pointer ngprs nfprs nstackargs arglist)
  (dolist (arg-temp arglist)
    (typecase arg-temp
      ((signed-byte 64)
       (if (< ngprs 6)
         (progn
           (setf (paref gpr-pointer (:* (:signed 64)) ngprs) arg-temp)
           (incf ngprs))
         (progn
           (setf (paref stack-pointer (:* (:signed 64)) nstackargs) arg-temp)
           (incf nstackargs))))
      ((unsigned-byte 64)
       (if (< ngprs 6)
         (progn
           (setf (paref gpr-pointer (:* (:unsigned 64)) ngprs) arg-temp)
           (incf ngprs))
         (progn
           (setf (paref stack-pointer (:* (:unsigned 64)) nstackargs) arg-temp)
           (incf nstackargs))))
      (macptr
       (if (< ngprs 6)
         (progn
           (setf (paref gpr-pointer (:* :address) ngprs) arg-temp)
           (incf ngprs))
         (progn
           (setf (paref stack-pointer (:* :address) nstackargs) arg-temp)
           (incf nstackargs))))
      (single-float
       (if (< nfprs 8)
         (progn
           (setf (%get-single-float fpr-pointer (* nfprs 16))
                 arg-temp)
           (incf nfprs))
         (progn
           (setf (paref stack-pointer (:* :float) (* 2 nstackargs)) arg-temp)
           (incf nstackargs))))
      (double-float
       (if (< nfprs 8)
         (progn
           (setf (%get-double-float fpr-pointer (* nfprs 16))
                 arg-temp)
           (incf nfprs))
         (progn
           (setf (paref stack-pointer (:* :double) nstackargs)
                 arg-temp)
           (incf nstackargs)))))))

#+(and apple-objc x8632-target)
(defun %process-varargs-list (ptr index arglist)
  (dolist (arg-temp arglist)
    (typecase arg-temp
      ((signed-byte 32)
       (setf (paref ptr (:* (:signed 32)) index) arg-temp)
       (incf index))
      ((unsigned-byte 32)
       (setf (paref ptr (:* (:unsigned 32)) index) arg-temp)
       (incf index))
      (macptr
       (setf (paref ptr (:* :address) index) arg-temp)
       (incf index))
      (single-float
       (setf (paref ptr (:* :single-float) index) arg-temp)
       (incf index))
      (double-float
       (setf (paref ptr (:* :double-float) index) arg-temp)
       (incf index 2))
      ((or (signed-byte 64)
	   (unsigned-byte 64))
       (setf (paref ptr (:* :unsigned) index) (ldb (byte 32 32) arg-temp))
       (incf index)
       (setf (paref ptr (:* :unsigned) index) (ldb (byte 32 0) arg-temp))
       (incf index)))))

#+(and apple-objc ppc32-target)
(defun %process-varargs-list (gpr-pointer fpr-pointer ngprs nfprs arglist)
  (dolist (arg-temp arglist)
    (typecase arg-temp
      ((signed-byte 32)
       (setf (paref gpr-pointer (:* (:signed 32)) ngprs) arg-temp)
       (incf ngprs))
      ((unsigned-byte 32)
       (setf (paref gpr-pointer (:* (:unsigned 32)) ngprs) arg-temp)
       (incf ngprs))
      (macptr
       (setf (paref gpr-pointer (:* :address) ngprs) arg-temp)
       (incf ngprs))
      (single-float
       (when (< nfprs 13)
         (setf (paref fpr-pointer (:* :double-float) nfprs) (float arg-temp 0.0d0))
         (incf nfprs))
       (setf (paref gpr-pointer (:* :single-float) ngprs) arg-temp)
       (incf ngprs))
      (double-float
       (when (< nfprs 13)
         (setf (paref fpr-pointer (:* :double-float) nfprs) arg-temp)
         (incf nfprs))
       (multiple-value-bind (high low) (double-float-bits arg-temp)
         (setf (paref gpr-pointer (:* :unsigned) ngprs) high)
         (incf ngprs)
         (setf (paref gpr-pointer (:* :unsigned) ngprs) low)
         (incf nfprs)))
      ((or (signed-byte 64)
           (unsigned-byte 64))
       (setf (paref gpr-pointer (:* :unsigned) ngprs) (ldb (byte 32 32) arg-temp))
       (incf ngprs)
       (setf (paref gpr-pointer (:* :unsigned) ngprs) (ldb (byte 32 0) arg-temp))
       (incf ngprs)))))

#+(and apple-objc ppc64-target)
(defun %process-varargs-list (gpr-pointer fpr-pointer ngprs nfprs arglist)
  (dolist (arg-temp arglist)
    (typecase arg-temp
      ((signed-byte 64)
       (setf (paref gpr-pointer (:* (:signed 64)) ngprs) arg-temp)
       (incf ngprs))
      ((unsigned-byte 64)
       (setf (paref gpr-pointer (:* (:unsigned 64)) ngprs) arg-temp)
       (incf ngprs))
      (macptr
       (setf (paref gpr-pointer (:* :address) ngprs) arg-temp)
       (incf ngprs))
      (single-float
       (when (< nfprs 13)
         (setf (paref fpr-pointer (:* :double-float) nfprs) (float arg-temp 0.0d0))
         (incf nfprs))
       (setf (paref gpr-pointer (:* (:unsigned 64)) ngprs) (single-float-bits arg-temp))
       (incf ngprs))
      (double-float
       (when (< nfprs 13)
         (setf (paref fpr-pointer (:* :double-float) nfprs) arg-temp)
         (incf nfprs))
       (setf (paref gpr-pointer (:* :double-float) ngprs) arg-temp)
       (incf ngprs)))))

                          
#+apple-objc
(eval-when (:compile-toplevel :execute)
  #+(and ppc-target (not apple-objc-2.0))
  (def-foreign-type :<MARG>
      (:struct nil
               (:fp<P>arams (:array :double 13))
               (:linkage (:array :uintptr_t 6))
               (:reg<P>arams (:array :uintptr_t 8))
               (:stack<P>arams (:array :uintptr_t) 0)))
  )

  
#+(and apple-objc-2.0 x8664-target)
(defun %compile-varargs-send-function-for-signature (sig)
  (let* ((return-type-spec (foreign-type-to-representation-type (car sig)))
         (op (case return-type-spec
               (:address '%get-ptr)
               (:unsigned-byte '%get-unsigned-byte)
               (:signed-byte '%get-signed-byte)
               (:unsigned-halfword '%get-unsigned-word)
               (:signed-halfword '%get-signed-word)
               (:unsigned-fullword '%get-unsigned-long)
               (:signed-fullword '%get-signed-long)
               (:unsigned-doubleword '%get-natural)
               (:signed-doubleword '%get-signed-natural)
               (:single-float '%get-single-float)
               (:double-float '%get-double-float)))
         (result-offset
          (case op
            ((:single-float :double-float) 0)
            (t -8)))
         (arg-type-specs (butlast (cdr sig)))
         (args (objc-gen-message-arglist (length arg-type-specs)))
         (receiver (gensym))
         (selector (gensym))
         (rest-arg (gensym))
         (arg-temp (gensym))
         (regparams (gensym))
         (stackparams (gensym))
         (fpparams (gensym))
         (cframe (gensym))
         (selptr (gensym))
         (gpr-total (gensym))
         (fpr-total (gensym))
         (stack-total (gensym))
         (n-static-gprs 2)              ;receiver, selptr
         (n-static-fprs 0)
         (n-static-stack-args 0))
    (collect ((static-arg-forms))
      (static-arg-forms `(setf (paref ,regparams (:* address) 0) ,receiver))
      (static-arg-forms `(setf (paref ,regparams (:* address) 1) ,selptr))
      (do* ((args args (cdr args))
            (arg-type-specs arg-type-specs (cdr arg-type-specs)))
           ((null args))
        (let* ((arg (car args))
               (spec (car arg-type-specs))
               (static-arg-type (parse-foreign-type spec))
               (gpr-base (if (< n-static-gprs 6) regparams stackparams))
               (fpr-base (if (< n-static-fprs 8) fpparams stackparams))
               (gpr-offset (if (< n-static-gprs 6) n-static-gprs n-static-stack-args))
               (fpr-offset (if (< n-static-fprs 8)
                             (* 8 n-static-fprs)
                             (* 8 n-static-stack-args))))
          (etypecase static-arg-type
            (foreign-integer-type
             (if (eq spec :<BOOL>)
               (setq arg `(%coerce-to-bool ,arg)))
             (static-arg-forms
              `(setf (paref ,gpr-base (:* (
                                           ,(if (foreign-integer-type-signed static-arg-type)
                                                :signed
                                                :unsigned)
                                           ,(foreign-integer-type-bits static-arg-type))) ,gpr-offset)
                ,arg))
             (if (< n-static-gprs 6)
               (incf n-static-gprs)
               (incf n-static-stack-args)))
            (foreign-single-float-type
             (static-arg-forms
              `(setf (%get-single-float ,fpr-base ,fpr-offset) ,arg))
             (if (< n-static-fprs 8)
               (incf n-static-fprs)
               (incf n-static-stack-args)))
            (foreign-double-float-type
             (static-arg-forms
              `(setf (%get-double-float ,fpr-base ,fpr-offset) ,arg))
             (if (< n-static-fprs 8)
               (incf n-static-fprs)
               (incf n-static-stack-args)))
            (foreign-pointer-type
             (static-arg-forms
              `(setf (paref ,gpr-base (:* address) ,gpr-offset) ,arg))
             (if (< n-static-gprs 6)
               (incf n-static-gprs)
               (incf n-static-stack-args))))))
      (compile
       nil
       `(lambda (,receiver ,selector ,@args &rest ,rest-arg)
         (declare (dynamic-extent ,rest-arg))
         (let* ((,selptr (%get-selector ,selector))
                (,gpr-total ,n-static-gprs)
                (,fpr-total ,n-static-fprs)
                (,stack-total ,n-static-stack-args))
           (dolist (,arg-temp ,rest-arg)
             (if (or (typep ,arg-temp 'double-float)
                     (typep ,arg-temp 'single-float))
               (if (< ,fpr-total 8)
                 (incf ,fpr-total)
                 (incf ,stack-total))
               (if (< ,gpr-total 6)
                 (incf ,gpr-total)
                 (incf ,stack-total))))
           (%stack-block ((,fpparams (* 8 8)))
             (with-macptrs (,regparams ,stackparams)
               (with-variable-c-frame
                   (+ 8 ,stack-total) ,cframe
                   (%setf-macptr-to-object ,regparams (+ ,cframe 2))
                   (%setf-macptr-to-object ,stackparams (+ ,cframe 8))
                   (progn ,@(static-arg-forms))
                   (%process-varargs-list ,regparams ,fpparams ,stackparams ,n-static-gprs ,n-static-fprs ,n-static-stack-args ,rest-arg)
                   (%do-ff-call ,fpr-total ,cframe ,fpparams (%reference-external-entry-point (load-time-value (external "_objc_msgSend"))))
                   ,@(if op
                         `((,op ,regparams ,result-offset))
                         `(())))))))))))


#+(and apple-objc ppc32-target)
(defun %compile-varargs-send-function-for-signature (sig)
  (let* ((return-type-spec (car sig))
         (arg-type-specs (butlast (cdr sig)))
         (args (objc-gen-message-arglist (length arg-type-specs)))
         (receiver (gensym))
         (selector (gensym))
         (rest-arg (gensym))
         (arg-temp (gensym))
         (marg-ptr (gensym))
         (regparams (gensym))
         (selptr (gensym))
         (gpr-total (gensym))
         (n-static-gprs 2)              ;receiver, selptr
         (n-static-fprs 0))
    (collect ((static-arg-forms))
      (static-arg-forms `(setf (paref ,regparams (:* address) 0) ,receiver))
      (static-arg-forms `(setf (paref ,regparams (:* address) 1) ,selptr))
      (do* ((args args (cdr args))
            (arg-type-specs arg-type-specs (cdr arg-type-specs)))
           ((null args))
        (let* ((arg (car args))
               (spec (car arg-type-specs))
               (static-arg-type (parse-foreign-type spec))
               (gpr-base regparams)
               (fpr-base marg-ptr)
               (gpr-offset (* n-static-gprs 4)))
          (etypecase static-arg-type
            (foreign-integer-type
             (let* ((bits (foreign-type-bits static-arg-type))
                    (signed (foreign-integer-type-signed static-arg-type)))
               (if (> bits 32)
                 (progn
                   (static-arg-forms
                    `(setf (,(if signed '%%get-signed-longlong '%%get-unsigned-long-long)
                            ,gpr-base ,gpr-offset)
                      ,arg))
                   (incf n-static-gprs 2))
                 (progn
                   (if (eq spec :<BOOL>)
                     (setq arg `(%coerce-to-bool ,arg)))
                   (static-arg-forms
                    `(setf (paref ,gpr-base (:* (
                                                 ,(if (foreign-integer-type-signed static-arg-type)
                                                      :signed
                                                      :unsigned)
                                           32)) ,gpr-offset)
                ,arg))
                   (incf n-static-gprs)))))
            (foreign-single-float-type
             (static-arg-forms
              `(setf (paref ,gpr-base (:* :single-float) ,n-static-gprs) ,arg))
             (when (< n-static-fprs 13)
               (static-arg-forms
                `(setf (paref ,fpr-base (:* :double-float) ,n-static-fprs)
                  (float (paref ,gpr-base (:* :single-float) ,n-static-gprs) 0.0d0)))
               (incf n-static-fprs))
             (incf n-static-gprs))
            (foreign-double-float-type
             (static-arg-forms
              `(setf (%get-double-float ,gpr-base ,gpr-offset) ,arg))
             (when (< n-static-fprs 13)
               (static-arg-forms
                `(setf (paref ,fpr-base (:* :double-float) ,n-static-fprs)
                  (%get-double-float ,gpr-base ,gpr-offset)))
               (incf n-static-fprs))
             (incf n-static-gprs 2))
            (foreign-pointer-type
             (static-arg-forms
              `(setf (paref ,gpr-base (:* address) ,n-static-gprs) ,arg))
               (incf n-static-gprs)))))
      (compile
       nil
       `(lambda (,receiver ,selector ,@args &rest ,rest-arg)
         (declare (dynamic-extent ,rest-arg))
         (let* ((,selptr (%get-selector ,selector))
                (,gpr-total ,n-static-gprs))
           (dolist (,arg-temp ,rest-arg)
             (if (or (typep ,arg-temp 'double-float)
                     (and (typep ,arg-temp 'integer)
                          (if (< ,arg-temp 0)
                            (>= (integer-length ,arg-temp) 32)
                            (> (integer-length ,arg-temp) 32))))
               (incf ,gpr-total 2)
               (incf ,gpr-total 1)))
           (if (> ,gpr-total 8)
             (setq ,gpr-total (- ,gpr-total 8))
             (setq ,gpr-total 0))           
           (%stack-block ((,marg-ptr (+ ,(%foreign-type-or-record-size
                                          :<MARG> :bytes)
                                        (* 4 ,gpr-total))))
             
             (with-macptrs ((,regparams (pref ,marg-ptr :<MARG>.reg<P>arams)))
               (progn ,@(static-arg-forms))
               (%process-varargs-list ,regparams ,marg-ptr ,n-static-gprs ,n-static-fprs  ,rest-arg)
               (external-call "_objc_msgSendv"
                              :address ,receiver
                              :address ,selptr
                              :size_t (+ 32 (* 4 ,gpr-total))
                              :address ,marg-ptr
                              ,return-type-spec)))))))))

#+(and apple-objc x8632-target)
(defun %compile-varargs-send-function-for-signature (sig)
  (let* ((return-type-spec (car sig))
         (arg-type-specs (butlast (cdr sig)))
         (args (objc-gen-message-arglist (length arg-type-specs)))
         (receiver (gensym))
         (selector (gensym))
         (rest-arg (gensym))
         (arg-temp (gensym))
         (marg-ptr (gensym))
	 (static-arg-words 2)		;receiver, selptr
	 (marg-words (gensym))
         (selptr (gensym)))
    (collect ((static-arg-forms))
      (static-arg-forms `(setf (paref ,marg-ptr (:* address) 0) ,receiver))
      (static-arg-forms `(setf (paref ,marg-ptr (:* address) 1) ,selptr))
      (do* ((args args (cdr args))
            (arg-type-specs arg-type-specs (cdr arg-type-specs)))
           ((null args))
        (let* ((arg (car args))
               (spec (car arg-type-specs))
               (static-arg-type (parse-foreign-type spec)))
          (etypecase static-arg-type
            (foreign-integer-type
             (let* ((bits (foreign-type-bits static-arg-type))
                    (signed (foreign-integer-type-signed static-arg-type)))
               (if (> bits 32)
                 (progn
                   (static-arg-forms
                    `(setf (,(if signed '%%get-signed-longlong '%%get-unsigned-long-long)
			     ,marg-ptr (* 4 ,static-arg-words))
			   ,arg))
                   (incf static-arg-words 2))
                 (progn
                   (if (eq spec :<BOOL>)
                     (setq arg `(%coerce-to-bool ,arg)))
                   (static-arg-forms
                    `(setf (paref ,marg-ptr (:* 
					     (,(if (foreign-integer-type-signed 
						    static-arg-type)
						   :signed
						   :unsigned)
					       32)) ,static-arg-words)
			   ,arg))
                   (incf static-arg-words)))))
            (foreign-single-float-type
             (static-arg-forms
              `(setf (paref ,marg-ptr (:* :single-float) ,static-arg-words) ,arg))
             (incf static-arg-words))
            (foreign-double-float-type
             (static-arg-forms
              `(setf (%get-double-float ,marg-ptr (* 4 ,static-arg-words)) ,arg))
             (incf static-arg-words 2))
            (foreign-pointer-type
             (static-arg-forms
              `(setf (paref ,marg-ptr (:* address) ,static-arg-words) ,arg))
	     (incf static-arg-words)))))
      (compile
       nil
       `(lambda (,receiver ,selector ,@args &rest ,rest-arg)
	  (declare (dynamic-extent ,rest-arg))
	  (let* ((,selptr (%get-selector ,selector))
		 (,marg-words ,static-arg-words))
	    (dolist (,arg-temp ,rest-arg)
	      (if (or (typep ,arg-temp 'double-float)
		      (and (typep ,arg-temp 'integer)
			   (if (< ,arg-temp 0)
			     (>= (integer-length ,arg-temp) 32)
			     (> (integer-length ,arg-temp) 32))))
		(incf ,marg-words 2)
		(incf ,marg-words 1)))
	    (%stack-block ((,marg-ptr ,marg-words))
	      (progn ,@(static-arg-forms))
	      (%process-varargs-list ,marg-ptr ,static-arg-words ,rest-arg)
	      (external-call "_objc_msgSendv"
			     :address ,receiver
			     :address ,selptr
			     :size_t (* 4 ,marg-words)
			     :address ,marg-ptr
			     ,return-type-spec))))))))

#+(and apple-objc ppc64-target)
(defun %compile-varargs-send-function-for-signature (sig)
  (let* ((return-type-spec (car sig))
         (arg-type-specs (butlast (cdr sig)))
         (args (objc-gen-message-arglist (length arg-type-specs)))
         (receiver (gensym))
         (selector (gensym))
         (rest-arg (gensym))
         (arg-temp (gensym))
         (marg-ptr (gensym))
         (regparams (gensym))
         (selptr (gensym))
         (gpr-total (gensym))
         (n-static-gprs 2)              ;receiver, selptr
         (n-static-fprs 0))
    (collect ((static-arg-forms))
      (static-arg-forms `(setf (paref ,regparams (:* address) 0) ,receiver))
      (static-arg-forms `(setf (paref ,regparams (:* address) 1) ,selptr))
      (do* ((args args (cdr args))
            (arg-type-specs arg-type-specs (cdr arg-type-specs)))
           ((null args))
        (let* ((arg (car args))
               (spec (car arg-type-specs))
               (static-arg-type (parse-foreign-type spec))
               (gpr-base regparams)
               (fpr-base marg-ptr)
               (gpr-offset (* n-static-gprs 8)))
          (etypecase static-arg-type
            (foreign-integer-type
             (if (eq spec :<BOOL>)
               (setq arg `(%coerce-to-bool ,arg)))
             (static-arg-forms
              `(setf (paref ,gpr-base (:* (
                                           ,(if (foreign-integer-type-signed static-arg-type)
                                                :signed
                                                :unsigned)
                                           64)) ,gpr-offset)
                ,arg))
             (incf n-static-gprs))
            (foreign-single-float-type
             (static-arg-forms
              `(setf (%get-single-float ,gpr-base ,(+ 4 (* 8 n-static-gprs))) ,arg))
             (when (< n-static-fprs 13)
               (static-arg-forms
                `(setf (paref ,fpr-base (:* :double-float) ,n-static-fprs)
                  (float (%get-single-float ,gpr-base ,(+ 4 (* 8 n-static-gprs))) 0.0d0)))
               (incf n-static-fprs))
             (incf n-static-gprs))
            (foreign-double-float-type
             (static-arg-forms
              `(setf (%get-double-float ,gpr-base ,gpr-offset) ,arg))
             (when (< n-static-fprs 13)
               (static-arg-forms
                `(setf (paref ,fpr-base (:* :double-float) ,n-static-fprs)
                  (%get-double-float ,gpr-base ,gpr-offset)))
               (incf n-static-fprs))
             (incf n-static-gprs 1))
            (foreign-pointer-type
             (static-arg-forms
              `(setf (paref ,gpr-base (:* address) ,n-static-gprs) ,arg))
             (incf n-static-gprs)))))
      
      (progn
        nil
        `(lambda (,receiver ,selector ,@args &rest ,rest-arg)
          (declare (dynamic-extent ,rest-arg))
          (let* ((,selptr (%get-selector ,selector))
                 (,gpr-total ,n-static-gprs))
            (dolist (,arg-temp ,rest-arg)
              (declare (ignore ,arg-temp))
              (incf ,gpr-total 1))
            (if (> ,gpr-total 8)
              (setq ,gpr-total (- ,gpr-total 8))
              (setq ,gpr-total 0))           
            (%stack-block ((,marg-ptr (+ ,(%foreign-type-or-record-size
                                           :<MARG> :bytes)
                                         (* 8 ,gpr-total))))
             
              (with-macptrs ((,regparams (pref ,marg-ptr :<MARG>.reg<P>arams)))
                (progn ,@(static-arg-forms))
                (%process-varargs-list ,regparams ,marg-ptr ,n-static-gprs ,n-static-fprs  ,rest-arg)
                (external-call "_objc_msgSendv"
                               :address ,receiver
                               :address ,selptr
                               :size_t (+ 64 (* 8 ,gpr-total))
                               :address ,marg-ptr
                               ,return-type-spec)))))))))




(defun %compile-send-function-for-signature (sig &optional super-p)
  (let* ((return-type-spec (car sig))
         (arg-type-specs (cdr sig)))
    (if (eq (car (last arg-type-specs)) :void)
      (%compile-varargs-send-function-for-signature sig)
      (let* ((args (objc-gen-message-arglist (length arg-type-specs)))
             (struct-return-var nil)
             (receiver (gensym))
             (selector (gensym)))
        (collect ((call)
                  (lets))
          (let* ((result-type (parse-foreign-type return-type-spec)))
            (when (typep result-type 'foreign-record-type)
              (setq struct-return-var (gensym))
              (lets `(,struct-return-var (make-gcable-record ,return-type-spec))))

            (do ((args args (cdr args))
                 (spec (pop arg-type-specs) (pop arg-type-specs)))
                ((null args) (call return-type-spec))
              (let* ((arg (car args)))
                 (call spec)
                 (case spec
                   (:<BOOL> (call `(%coerce-to-bool ,arg)))
                   (:id (call `(%coerce-to-address ,arg)))
		   (:<CGF>loat (call `(float ,arg +cgfloat-zero+)))
                   (t
                    (call arg)))))
            (let* ((call (call))
                   (lets (lets))
                   (body (message-send-form-for-call receiver selector call super-p struct-return-var)))
              (if struct-return-var
                (setq body `(progn ,body ,struct-return-var)))
              (if lets
                (setq body `(let* ,lets
                             ,body)))
              (compile nil
                       `(lambda (,receiver ,selector ,@args)
                         ,body)))))))))

(defun compile-send-function-for-signature (sig)
  (%compile-send-function-for-signature sig nil))
                           
                    


;;; The first 8 words of non-fp arguments get passed in R3-R10
#+ppc-target
(defvar *objc-gpr-offsets*
  #+32-bit-target
  #(4 8 12 16 20 24 28 32)
  #+64-bit-target
  #(8 16 24 32 40 48 56 64)
  )



;;; The first 13 fp arguments get passed in F1-F13 (and also "consume"
;;; a GPR or two.)  It's certainly possible for an FP arg and a non-
;;; FP arg to share the same "offset", and parameter offsets aren't
;;; strictly increasing.
#+ppc-target
(defvar *objc-fpr-offsets*
  #+32-bit-target
  #(36 44 52 60  68  76  84  92 100 108 116 124 132)
  #+64-bit-target
  #(68 76 84 92 100 108 116 124 132 140 148 156 164))

;;; Just to make things even more confusing: once we've filled in the
;;; first 8 words of the parameter area, args that aren't passed in
;;; FP-regs get assigned offsets starting at 32.  That almost makes
;;; sense (even though it conflicts with the last offset in
;;; *objc-gpr-offsets* (assigned to R10), but we then have to add
;;; this constant to the memory offset.
(defconstant objc-forwarding-stack-offset 8)

(defvar *objc-id-type* (parse-foreign-type :id))
(defvar *objc-sel-type* (parse-foreign-type :<SEL>))
(defvar *objc-char-type* (parse-foreign-type :char))


(defun encode-objc-type (type &optional for-ivar recursive)
  (if (or (eq type *objc-id-type*)
	  (foreign-type-= type *objc-id-type*))
    "@"
    (if (or (eq type *objc-sel-type*)
	    (foreign-type-= type *objc-sel-type*))
      ":"
      (if (eq (foreign-type-class type) 'root)
	"v"
	(typecase type
	  (foreign-pointer-type
	   (let* ((target (foreign-pointer-type-to type)))
	     (if (or (eq target *objc-char-type*)
		     (foreign-type-= target *objc-char-type*))
	       "*"
	       (format nil "^~a" (encode-objc-type target nil t)))))
	  (foreign-double-float-type "d")
	  (foreign-single-float-type "f")
	  (foreign-integer-type
	   (let* ((signed (foreign-integer-type-signed type))
		  (bits (foreign-integer-type-bits type)))
	     (if (eq (foreign-integer-type-alignment type) 1)
	       (format nil "b~d" bits)
	       (cond ((= bits 8)
		      (if signed "c" "C"))
		     ((= bits 16)
		      (if signed "s" "S"))
		     ((= bits 32)
		      ;; Should be some way of noting "longness".
		      (if signed "i" "I"))
		     ((= bits 64)
		      (if signed "q" "Q"))))))
	  (foreign-record-type
	   (ensure-foreign-type-bits type)
	   (let* ((name (unescape-foreign-name
			 (or (foreign-record-type-name type) "?")))
		  (kind (foreign-record-type-kind type))
		  (fields (foreign-record-type-fields type)))
	     (with-output-to-string (s)
				    (format s "~c~a=" (if (eq kind :struct) #\{ #\() name)
				    (dolist (f fields (format s "~a" (if (eq kind :struct) #\} #\))))
				      (when for-ivar
					(format s "\"~a\""
						(unescape-foreign-name
						 (or (foreign-record-field-name f) ""))))
                                      (unless recursive
                                        (format s "~a" (encode-objc-type
                                                        (foreign-record-field-type f) nil nil)))))))
        (foreign-array-type
	   (ensure-foreign-type-bits type)
	   (let* ((dims (foreign-array-type-dimensions type))
		  (element-type (foreign-array-type-element-type type)))
	     (if dims (format nil "[~d~a]"
			      (car dims)
			      (encode-objc-type element-type nil t))
	       (if (or (eq element-type *objc-char-type*)
		       (foreign-type-= element-type *objc-char-type*))
		 "*"
		 (format nil "^~a" (encode-objc-type element-type nil t))))))
	  (t (break "type = ~s" type)))))))

#+ppc-target
(defun encode-objc-method-arglist (arglist result-spec)
  (let* ((gprs-used 0)
	 (fprs-used 0)
	 (arg-info
	  (flet ((current-memory-arg-offset ()
		   (+ 32 (* 4 (- gprs-used 8))
		      objc-forwarding-stack-offset)))
	    (flet ((current-gpr-arg-offset ()
		     (if (< gprs-used 8)
		       (svref *objc-gpr-offsets* gprs-used)
		       (current-memory-arg-offset)))
		   (current-fpr-arg-offset ()
		     (if (< fprs-used 13)
		       (svref *objc-fpr-offsets* fprs-used)
		       (current-memory-arg-offset))))
	      (let* ((result nil))
		(dolist (argspec arglist (nreverse result))
		  (let* ((arg (parse-foreign-type argspec))
			 (offset 0)
			 (size 0))
		    (typecase arg
		      (foreign-double-float-type
		       (setq size 8 offset (current-fpr-arg-offset))
		       (incf fprs-used)
		       (incf gprs-used 2))
		      (foreign-single-float-type
		       (setq size target::node-size offset (current-fpr-arg-offset))
		       (incf fprs-used)
		       (incf gprs-used 1))
		      (foreign-pointer-type
		       (setq size target::node-size offset (current-gpr-arg-offset))
		       (incf gprs-used))
		      (foreign-integer-type
		       (let* ((bits (foreign-type-bits arg)))
			 (setq size (ceiling bits 8)
			       offset (current-gpr-arg-offset))
			 (incf gprs-used (ceiling bits target::nbits-in-word))))
		      ((or foreign-record-type foreign-array-type)
		       (let* ((bits (ensure-foreign-type-bits arg)))
			 (setq size (ceiling bits 8)
			       offset (current-gpr-arg-offset))
			 (incf gprs-used (ceiling bits target::nbits-in-word))))
		      (t (break "argspec = ~s, arg = ~s" argspec arg)))
		    (push (list (encode-objc-type arg) offset size) result))))))))
    (declare (fixnum gprs-used fprs-used))
    (let* ((max-parm-end
	    (- (apply #'max (mapcar #'(lambda (i) (+ (cadr i) (caddr i)))
				    arg-info))
	       objc-forwarding-stack-offset)))
      (format nil "~a~d~:{~a~d~}"
	      (encode-objc-type
	       (parse-foreign-type result-spec))
	      max-parm-end
	      arg-info))))

#+x86-target
(defun encode-objc-method-arglist (arglist result-spec)
  (let* ((offset 0)
	 (arg-info
          (let* ((result nil))
		(dolist (argspec arglist (nreverse result))
		  (let* ((arg (parse-foreign-type argspec))
                         (delta target::node-size))
		    (typecase arg
		      (foreign-double-float-type)
		      (foreign-single-float-type)
		      ((or foreign-pointer-type foreign-array-type))
		      (foreign-integer-type)
		      (foreign-record-type
		       (let* ((bits (ensure-foreign-type-bits arg)))
			 (setq delta (ceiling bits target::node-size))))
		      (t (break "argspec = ~s, arg = ~s" argspec arg)))
		    (push (list (encode-objc-type arg) offset) result)
                    (setq offset (* target::node-size (ceiling (+ offset delta) target::node-size))))))))
    (let* ((max-parm-end offset))
      (format nil "~a~d~:{~a~d~}"
	      (encode-objc-type
	       (parse-foreign-type result-spec))
	      max-parm-end
	      arg-info))))

;;; In Apple Objc, a class's methods are stored in a (-1)-terminated
;;; vector of method lists.  In GNU ObjC, method lists are linked
;;; together.
(defun %make-method-vector ()
  #+apple-objc
  (let* ((method-vector (malloc 16)))
    (setf (%get-signed-long method-vector 0) 0
	  (%get-signed-long method-vector 4) 0
	  (%get-signed-long method-vector 8) 0
	  (%get-signed-long method-vector 12) -1)
    method-vector))


;;; Make a meta-class object (with no instance variables or class
;;; methods.)
#-apple-objc-2.0
(defun %make-basic-meta-class (nameptr superptr rootptr)
  #+apple-objc
  (let* ((method-vector (%make-method-vector)))
    (make-record :objc_class
		 :isa (pref rootptr :objc_class.isa)
		 :super_class (pref superptr :objc_class.isa)
		 :name nameptr
		 :version 0
		 :info #$CLS_META
		 :instance_size 0
		 :ivars (%null-ptr)
		 :method<L>ists method-vector
		 :cache (%null-ptr)
		 :protocols (%null-ptr)))
  #+gnu-objc
  (make-record :objc_class
               :class_pointer (pref rootptr :objc_class.class_pointer)
               :super_class (pref superptr :objc_class.class_pointer)
               :name nameptr
               :version 0
               :info #$_CLS_META
               :instance_size 0
               :ivars (%null-ptr)
               :methods (%null-ptr)
               :dtable (%null-ptr)
               :subclass_list (%null-ptr)
               :sibling_class (%null-ptr)
               :protocols (%null-ptr)
               :gc_object_type (%null-ptr)))

#-apple-objc-2.0
(defun %make-class-object (metaptr superptr nameptr ivars instance-size)
  #+apple-objc
  (let* ((method-vector (%make-method-vector)))
    (make-record :objc_class
		 :isa metaptr
		 :super_class superptr
		 :name nameptr
		 :version 0
		 :info #$CLS_CLASS
		 :instance_size instance-size
		 :ivars ivars
		 :method<L>ists method-vector
		 :cache (%null-ptr)
		 :protocols (%null-ptr)))
  #+gnu-objc
  (make-record :objc_class
		 :class_pointer metaptr
		 :super_class superptr
		 :name nameptr
		 :version 0
		 :info #$_CLS_CLASS
		 :instance_size instance-size
		 :ivars ivars
		 :methods (%null-ptr)
		 :dtable (%null-ptr)
		 :protocols (%null-ptr)))

(defun make-objc-class-pair (superptr nameptr)
  #+apple-objc-2.0
  (#_objc_allocateClassPair superptr nameptr 0)
  #-apple-objc-2.0
  (%make-class-object
   (%make-basic-meta-class nameptr superptr (@class "NSObject"))
   superptr
   nameptr
   (%null-ptr)
   0))

(defun superclass-instance-size (class)
  (with-macptrs ((super #+apple-objc-2.0 (#_class_getSuperclass class)
                        #-apple-objc-2.0 (pref class :objc_class.super_class)))
    (if (%null-ptr-p super)
      0
      (%objc-class-instance-size super))))

	


#+gnu-objc
(progn
(defloadvar *gnu-objc-runtime-mutex*
    (%get-ptr (foreign-symbol-address "__objc_runtime_mutex")))
(defmacro with-gnu-objc-mutex-locked ((mutex) &body body)
  (let* ((mname (gensym)))
    `(let ((,mname ,mutex))
      (unwind-protect
	   (progn
	     (external-call "objc_mutex_lock" :address ,mname :void)
	     ,@body)
	(external-call "objc_mutex_lock" :address ,mname :void)))))
)

(defun %objc-metaclass-p (class)
  #+apple-objc-2.0 (not (eql #$NO (#_class_isMetaClass class)))
  #-apple-objc-2.0
  (logtest (pref class :objc_class.info)
	   #+apple-objc #$CLS_META
	   #+gnu-objc #$_CLS_META))

;; No way to tell in Objc-2.0.  Does anything care ?
#-apple-objc-2.0
(defun %objc-class-posing-p (class)
  (logtest (pref class :objc_class.info)
	   #+apple-objc #$CLS_POSING
	   #+gnu-objc #$_CLS_POSING))




;;; Create (malloc) class and metaclass objects with the specified
;;; name (string) and superclass name.  Initialize the metaclass
;;; instance, but don't install the class in the ObjC runtime system
;;; (yet): we don't know anything about its ivars and don't know
;;; how big instances will be yet.
;;; If an ObjC class with this name already exists, we're very
;;; confused; check for that case and error out if it occurs.
(defun %allocate-objc-class (name superptr)
  (let* ((class-name (compute-objc-classname name)))
    (if (lookup-objc-class class-name nil)
      (error "An Objective C class with name ~s already exists." class-name))
    (let* ((nameptr (make-cstring class-name))
	   (id (register-objc-class
                (make-objc-class-pair superptr nameptr)
))
	   (meta-id (objc-class-id->objc-metaclass-id id))
	   (meta (id->objc-metaclass meta-id))
	   (class (id->objc-class id))
	   (meta-name (intern (format nil "+~a" name)
			      (symbol-package name)))
	   (meta-super (canonicalize-registered-metaclass
                        #+apple-objc-2.0
                        (#_class_getSuperclass meta)
                        #-apple-objc-2.0
			(pref meta :objc_class.super_class))))
      (initialize-instance meta
			 :name meta-name
			 :direct-superclasses (list meta-super))
      (setf (objc-class-id-foreign-name id) class-name
	    (objc-metaclass-id-foreign-name meta-id) class-name
	    (find-class meta-name) meta)
      (%defglobal name class)
      (%defglobal meta-name meta)
    class)))

;;; Set up the class's ivar_list and instance_size fields, then
;;; add the class to the ObjC runtime.
#-apple-objc-2.0
(defun %add-objc-class (class ivars instance-size)
  (setf
   (pref class :objc_class.ivars) ivars
   (pref class :objc_class.instance_size) instance-size)
  #+apple-objc
  (#_objc_addClass class)
  #+gnu-objc
  ;; Why would anyone want to create a class without creating a Module ?
  ;; Rather than ask that vexing question, let's create a Module with
  ;; one class in it and use #___objc_exec_class to add the Module.
  ;; (I mean "... to add the class", of course.
  ;; It appears that we have to heap allocate the module, symtab, and
  ;; module name: the GNU ObjC runtime wants to add the module to a list
  ;; that it subsequently ignores.
  (let* ((name (make-cstring "Phony Module"))
	 (symtab (malloc (+ (record-length :objc_symtab) (record-length (:* :void)))))
	 (m (make-record :objc_module
			 :version 8 #|OBJC_VERSION|#
			 :size (record-length :<M>odule)
			 :name name
			 :symtab symtab)))
    (setf (%get-ptr symtab (record-length :objc_symtab)) (%null-ptr))
    (setf (pref symtab :objc_symtab.sel_ref_cnt) 0
	  (pref symtab :objc_symtab.refs) (%null-ptr)
	  (pref symtab :objc_symtab.cls_def_cnt) 1
	  (pref symtab :objc_symtab.cat_def_cnt) 0
	  (%get-ptr (pref symtab :objc_symtab.defs)) class
	  (pref class :objc_class.info) (logior #$_CLS_RESOLV (pref class :objc_class.info)))
    (#___objc_exec_class m)))

#+apple-objc-2.0
(defun %add-objc-class (class)
  (#_objc_registerClassPair class))







(let* ((objc-gen-message-args (make-array 10 :fill-pointer 0 :adjustable t)))
  (defun %objc-gen-message-arg (n)
    (let* ((len (length objc-gen-message-args)))
      (do* ((i len (1+ i)))
           ((> i n) (aref objc-gen-message-args n))
        (vector-push-extend (intern (format nil "ARG~d" i)) objc-gen-message-args)))))

(defun objc-gen-message-arglist (n)
  (collect ((args))
    (dotimes (i n (args)) (args (%objc-gen-message-arg i)))))



;;; Call get-objc-message-info for all known init messages.  (A
;;; message is an "init message" if it starts with the string "init",
;;; and has at least one declared method that returns :ID and is not a
;;; protocol method.
(defun register-objc-init-messages ()
  (do-interface-dirs (d)
    (dolist (init (cdb-enumerate-keys (db-objc-methods d)
                                      #'(lambda (string)
                                          (string= string "init" :end1 (min (length string) 4)))))
      (get-objc-message-info init))))

    
(defvar *objc-init-messages-for-init-keywords* (make-hash-table :test #'equal)
  "Maps from lists of init keywords to dispatch-functions for init messages")



(defun send-objc-init-message (instance init-keywords args)
  (let* ((info (gethash init-keywords *objc-init-messages-for-init-keywords*)))
    (unless info
      (let* ((name (lisp-to-objc-init init-keywords))
             (name-info (get-objc-message-info name nil)))
        (unless name-info
          (error "Unknown ObjC init message: ~s" name))
        (setf (gethash init-keywords *objc-init-messages-for-init-keywords*)
              (setq info name-info))))
    (apply (objc-message-info-lisp-name info) instance args)))
                   

  

                  

;;; Return the "canonical" version of P iff it's a known ObjC class
(defun objc-class-p (p)
  (if (typep p 'macptr)
    (let* ((id (objc-class-id p)))
      (if id (id->objc-class id)))))

;;; Return the canonical version of P iff it's a known ObjC metaclass
(defun objc-metaclass-p (p)
  (if (typep p 'macptr)
    (let* ((id (objc-metaclass-id p)))
      (if id (id->objc-metaclass id)))))

;;; If P is an ObjC instance, return a pointer to its class.
;;; This assumes that all instances are allocated via something that's
;;; ultimately malloc-based.
(defun objc-instance-p (p)
  (when (typep p 'macptr)
    (let* ((idx (%objc-instance-class-index p)))
      (if idx (id->objc-class  idx)))))




(defun objc-private-class-id (classptr)
  (let* ((info (%get-private-objc-class classptr)))
    (when info
      (or (private-objc-class-info-declared-ancestor info)
          (with-macptrs ((super #+apple-objc-2.0 (#_class_getSuperclass classptr)
                                #-apple-objc-2.0 (pref classptr :objc_class.super_class)))
            (loop
              (when (%null-ptr-p super)
                (return))
              (let* ((id (objc-class-id super)))
                (if id
                  (return (setf (private-objc-class-info-declared-ancestor info)
                                id))
                  (%setf-macptr super #+apple-objc-2.0 (#_class_getSuperclass super)
                                #-apple-objc-2.0 (pref super :objc_class.super_class))))))))))

(defun objc-class-or-private-class-id (classptr)
  (or (objc-class-id classptr)
      (objc-private-class-id classptr)))


(defun %objc-instance-class-index (p)
  (unless (%null-ptr-p p)
    (if (with-macptrs (q)
          (safe-get-ptr p q)
          (not (%null-ptr-p q)))
      (with-macptrs ((parent #+apple-objc (pref p :objc_object.isa)
                             #+gnu-objc (pref p :objc_object.class_pointer)))
        (or
         (objc-class-id parent)
         (objc-private-class-id parent))))))


;;; If an instance, return (values :INSTANCE <class>)
;;; If a class, return (values :CLASS <class>).
;;; If a metaclass, return (values :METACLASS <metaclass>).
;;; Else return (values NIL NIL).
(defun objc-object-p (p)
  (let* ((instance-p (objc-instance-p p)))
    (if instance-p
      (values :instance instance-p)
      (let* ((class-p (objc-class-p p)))
	(if class-p
	  (values :class class-p)
	  (let* ((metaclass-p (objc-metaclass-p p)))
	    (if metaclass-p
	      (values :metaclass metaclass-p)
	      (values nil nil))))))))

       



;;; If the class contains an mlist that contains a method that
;;; matches (is EQL to) the selector, remove the mlist and
;;; set its IMP; return the containing mlist.
;;; If the class doesn't contain any matching mlist, create
;;; an mlist with one method slot, initialize the method, and
;;; return the new mlist.  Doing it this way ensures
;;; that the objc runtime will invalidate any cached references
;;; to the old IMP, at least as far as objc method dispatch is
;;; concerned.
#-apple-objc-2.0
(defun %mlist-containing (classptr selector typestring imp)
  #-apple-objc (declare (ignore classptr selector typestring imp))
  #+apple-objc
  (%stack-block ((iter 4))
    (setf (%get-ptr iter) (%null-ptr))
    (loop
	(let* ((mlist (#_class_nextMethodList classptr iter)))
	  (when (%null-ptr-p mlist)
	    (let* ((mlist (make-record :objc_method_list
				       :method_count 1))
		   (method (pref mlist :objc_method_list.method_list)))
	      (setf (pref method :objc_method.method_name) selector
		    (pref method :objc_method.method_types)
		    (make-cstring typestring)
		    (pref method :objc_method.method_imp) imp)
	      (return mlist)))
	  (do* ((n (pref mlist :objc_method_list.method_count))
		(i 0 (1+ i))
		(method (pref mlist :objc_method_list.method_list)
			(%incf-ptr method (record-length :objc_method))))
	       ((= i n))
	    (declare (fixnum i n))
	    (when (eql selector (pref method :objc_method.method_name))
	      (#_class_removeMethods classptr mlist)
	      (setf (pref method :objc_method.method_imp) imp)
	      (return-from %mlist-containing mlist)))))))
	      

(defun %add-objc-method (classptr selector typestring imp)
  #+apple-objc-2.0
  (with-cstrs ((typestring typestring))
    (or (not (eql #$NO (#_class_addMethod classptr selector imp typestring)))
        (let* ((m (if (objc-metaclass-p classptr)
                    (#_class_getClassMethod classptr selector)
                    (#_class_getInstanceMethod classptr selector))))
          (if (not (%null-ptr-p m))
            (#_method_setImplementation m imp)
            (error "Can't add ~s method to class ~s" selector typestring)))))
  #-apple-objc-2.0
  (progn
    #+apple-objc
    (#_class_addMethods classptr
                        (%mlist-containing classptr selector typestring imp))
    #+gnu-objc
  ;;; We have to do this ourselves, and have to do it with the runtime
  ;;; mutex held.
    (with-gnu-objc-mutex-locked (*gnu-objc-runtime-mutex*)
      (let* ((ctypestring (make-cstring typestring))
             (new-mlist nil))
        (with-macptrs ((method (external-call "search_for_method_in_list"
                                              :address (pref classptr :objc_class.methods)
                                              :address selector
                                              :address)))
          (when (%null-ptr-p method)
            (setq new-mlist (make-record :objc_method_list :method_count 1))
            (%setf-macptr method (pref new-mlist :objc_method_list.method_list)))
          (setf (pref method :objc_method.method_name) selector
                (pref method :objc_method.method_types) ctypestring
                (pref method :objc_method.method_imp) imp)
          (if new-mlist
            (external-call "GSObjCAddMethods"
                           :address classptr
                           :address new-mlist
                           :void)
            (external-call "__objc_update_dispatch_table_for_class"
                           :address classptr
                           :void)))))))

(defvar *lisp-objc-methods* (make-hash-table :test #'eq))

(defstruct lisp-objc-method
  class-descriptor
  sel
  typestring
  class-p				;t for class methods
  imp					; callback ptr
  )

(defun %add-lisp-objc-method (m)
  (let* ((class (%objc-class-classptr (lisp-objc-method-class-descriptor m)))
	 (sel (%get-selector (lisp-objc-method-sel m)))
	 (typestring (lisp-objc-method-typestring m))
	 (imp (lisp-objc-method-imp m)))
    (%add-objc-method
     (if (lisp-objc-method-class-p m)
       (pref class #+apple-objc :objc_class.isa #+gnu-objc :objc_class.class_pointer)
       class)
     sel
     typestring
     imp)))

(def-ccl-pointers add-objc-methods ()
  (maphash #'(lambda (impname m)
	       (declare (ignore impname))
	       (%add-lisp-objc-method m))
	   *lisp-objc-methods*))

(defun %define-lisp-objc-method (impname classname selname typestring imp
					 &optional class-p)
  (%add-lisp-objc-method
   (setf (gethash impname *lisp-objc-methods*)
	 (make-lisp-objc-method
	  :class-descriptor (load-objc-class-descriptor classname)
	  :sel (load-objc-selector selname)
	  :typestring typestring
	  :imp imp
	  :class-p class-p)))
  impname)
    




;;; If any of the argspecs denote a value of type :<BOOL>, push an
;;; appropriate SETQ on the front of the body.  (Order doesn't matter.)
(defun coerce-foreign-boolean-args (argspecs body)
  (do* ((argspecs argspecs (cddr argspecs))
	(type (car argspecs) (car argspecs))
	(var (cadr argspecs) (cadr argspecs)))
       ((null argspecs) body)
    (when (eq type :<BOOL>)
      (push `(setq ,var (not (eql ,var 0))) body))))
      
(defun lisp-boolean->foreign-boolean (form)
  (let* ((val (gensym)))
    `((let* ((,val (progn ,@form)))
	(if (and ,val (not (eql 0 ,val))) 1 0)))))

;;; Return, as multiple values:
;;;  the selector name, as a string
;;;  the ObjC class name, as a string
;;;  the foreign result type
;;;  the foreign argument type/argument list
;;;  the body
;;;  a string which encodes the foreign result and argument types
(defun parse-objc-method (selector-arg class-arg body)
  (let* ((class-name (objc-class-name-string class-arg))
	 (selector-form selector-arg)
	 (selector nil)
	 (argspecs nil)
	 (resulttype nil)
         (struct-return nil))
    (flet ((bad-selector (why) (error "Can't parse method selector ~s : ~a"
				   selector-arg why)))
      (typecase selector-form
	(string
	 (let* ((specs (pop body)))
	     (setq selector selector-form)
	     (if (evenp (length specs))
	       (setq argspecs specs resulttype :id)
	       (setq resulttype (car (last specs))
		     argspecs (butlast specs)))))
	(cons				;sic
	 (setq resulttype (pop selector-form))
	 (unless (consp selector-form)
	   (bad-selector "selector-form not a cons"))
	 (ccl::collect ((components)
			 (specs))
	   ;; At this point, selector-form should be either a list of
	   ;; a single symbol (a lispified version of the selector name
	   ;; of a selector that takes no arguments) or a list of keyword/
	   ;; variable pairs.  Each keyword is a lispified component of
	   ;; the selector name; each "variable" is either a symbol
	   ;; or a list of the form (<foreign-type> <symbol>), where
	   ;; an atomic variable is shorthand for (:id <symbol>).
	   (if (and (null (cdr selector-form))
		    (car selector-form)
		    (typep (car selector-form) 'symbol)
		    (not (typep (car selector-form) 'keyword)))
	     (components (car selector-form))
	     (progn
	       (unless (evenp (length selector-form))
		 (bad-selector "Odd length"))
	       (do* ((s selector-form (cddr s))
		     (comp (car s) (car s))
		     (var (cadr s) (cadr s)))
		    ((null s))
		 (unless (typep comp 'keyword) (bad-selector "not a keyword"))
		 (components comp)
		 (cond ((atom var)
			(unless (and var (symbolp var))
			  (bad-selector "not a non-null symbol"))
			(specs :id)
			(specs var))
		       ((and (consp (cdr var))
			     (null (cddr var))
			     (cadr var)
			     (symbolp (cadr var)))
			(specs (car var))
			(specs (cadr var)))
		       (t (bad-selector "bad variable/type clause"))))))
	   (setq argspecs (specs)
		 selector (lisp-to-objc-message (components)))))
	(t (bad-selector "general failure")))
      ;; If the result type is of the form (:STRUCT <typespec> <name>),
      ;; make <name> be the first argument.
      (when (and (consp resulttype)
		 (eq (car resulttype) :struct))
	(destructuring-bind (typespec name) (cdr resulttype)
          (let* ((rtype (%foreign-type-or-record typespec)))
            (if (and (typep name 'symbol)
                     (typep rtype 'foreign-record-type))
              (setq struct-return name
                    resulttype (unparse-foreign-type rtype))
              (bad-selector "Bad struct return type")))))
      (values selector
	      class-name
	      resulttype
	      argspecs
	      body
	      (do* ((argtypes ())
		    (argspecs argspecs (cddr argspecs)))
		   ((null argspecs) (encode-objc-method-arglist
				     `(:id :<sel> ,@(nreverse argtypes))
				     resulttype))
		(push (car argspecs) argtypes))
              struct-return))))

(defun objc-method-definition-form (class-p selector-arg class-arg body env)
  (multiple-value-bind (selector-name
			class-name
			resulttype
			argspecs
			body
			typestring
                        struct-return)
      (parse-objc-method selector-arg class-arg body)
    (%declare-objc-method selector-name
                          class-name
                          class-p
                          (concise-foreign-type resulttype)
                          (collect ((argtypes))
                            (do* ((argspecs argspecs (cddr argspecs)))
                                 ((null argspecs) (mapcar #'concise-foreign-type (argtypes)))
                              (argtypes (car argspecs)))))
    (let* ((self (intern "SELF")))
      (multiple-value-bind (body decls) (parse-body body env)
        (unless class-p
          (push `(%set-objc-instance-type ,self) body))
	(setq body (coerce-foreign-boolean-args argspecs body))
	(if (eq resulttype :<BOOL>)
	  (setq body (lisp-boolean->foreign-boolean body)))
	(let* ((impname (intern (format nil "~c[~a ~a]"
					(if class-p #\+ #\-)
					class-name
					selector-name)))
	       (_cmd (intern "_CMD"))
	       (super (gensym "SUPER"))
	       (params `(:id ,self :<sel> ,_cmd)))
          (when struct-return
            (push struct-return params))
          (setq params (nconc params argspecs))
	  `(progn
	    (defcallback ,impname
                (:without-interrupts nil
                 #+(and openmcl-native-threads apple-objc) :error-return
                 #+(and openmcl-native-threads apple-objc)  (condition objc-callback-error-return) ,@params ,resulttype)
              (declare (ignorable ,_cmd))
              ,@decls
              (rlet ((,super :objc_super
                       #+apple-objc :receiver #+gnu-objc :self ,self
                       #+apple-objc-2.0 :super_class #-apple-objc-2.0 :class
                       ,@(if class-p
                             #+apple-objc-2.0
                             `((external-call "_class_getSuperclass"
                                :address (pref (@class ,class-name) :objc_class.isa) :address))
                             #-apple-objc-2.0
                             `((pref
                                (pref (@class ,class-name)
                                 #+apple-objc :objc_class.isa
                                 #+gnu-objc :objc_class.class_pointer)
                                :objc_class.super_class))
                             #+apple-objc-2.0
                             `((external-call "_class_getSuperclass"
                                :address (@class ,class-name) :address))
                             #-apple-objc-2.0
                             `((pref (@class ,class-name) :objc_class.super_class)))))
                (macrolet ((send-super (msg &rest args &environment env) 
                             (make-optimized-send nil msg args env nil ',super ,class-name))
                           (send-super/stret (s msg &rest args &environment env) 
                             (make-optimized-send nil msg args env s ',super ,class-name)))
                  ,@body)))
	    (%define-lisp-objc-method
	     ',impname
	     ,class-name
	     ,selector-name
	     ,typestring
	     ,impname
	     ,class-p)))))))

(defmacro define-objc-method ((selector-arg class-arg)
			      &body body &environment env)
  (objc-method-definition-form nil selector-arg class-arg body env))

(defmacro define-objc-class-method ((selector-arg class-arg)
				     &body body &environment env)
  (objc-method-definition-form t selector-arg class-arg body env))


(declaim (inline %objc-struct-return))

(defun %objc-struct-return (return-temp size value)
  (unless (eq return-temp value)
    (#_bcopy value return-temp size)))

(defmacro objc:defmethod (name (self-arg &rest other-args) &body body &environment env)
  (collect ((arglist)
            (arg-names)
            (arg-types)
            (bool-args)
            (type-assertions))
    (let* ((result-type nil)
           (struct-return-var nil)
           (struct-return-size nil)
           (selector nil)
           (cmd (intern "_CMD"))
           (class-p nil)
           (objc-class-name nil))
      (if (atom name)
        (setq selector (string name) result-type :id)
        (setq selector (string (car name)) result-type (concise-foreign-type (or (cadr name) :id))))
      (destructuring-bind (self-name lisp-class-name) self-arg
        (arg-names self-name)
        (arg-types :id)
        ;; Hack-o-rama
        (let* ((lisp-class-name (string lisp-class-name)))
          (if (eq (schar lisp-class-name 0) #\+)
            (setq class-p t lisp-class-name (subseq lisp-class-name 1)))
          (setq objc-class-name (lisp-to-objc-classname lisp-class-name)))
        (let* ((rtype (parse-foreign-type result-type)))
          (when (typep rtype 'foreign-record-type)
            (setq struct-return-var (gensym))
            (setq struct-return-size (ceiling (foreign-type-bits rtype) 8))
            (arglist struct-return-var)))
        (arg-types :<SEL>)
        (arg-names cmd)
        (dolist (arg other-args)
          (if (atom arg)
            (progn
              (arg-types :id)
              (arg-names arg))
            (destructuring-bind (arg-name arg-type) arg
              (let* ((concise-type (concise-foreign-type arg-type)))
                (unless (eq concise-type :id)
                  (let* ((ftype (parse-foreign-type concise-type)))
                    (if (typep ftype 'foreign-pointer-type)
                      (setq ftype (foreign-pointer-type-to ftype)))
                    (if (and (typep ftype 'foreign-record-type)
                             (foreign-record-type-name ftype))
                      (type-assertions `(%set-macptr-type ,arg-name
                                         (foreign-type-ordinal (load-time-value (%foreign-type-or-record ,(foreign-record-type-name ftype)))))))))
                (arg-types concise-type)
                (arg-names arg-name)))))
        (let* ((arg-names (arg-names))
               (arg-types (arg-types)))
          (do* ((names arg-names)
                (types arg-types))
               ((null types) (arglist result-type))
            (let* ((name (pop names))
                   (type (pop types)))
              (arglist type)
              (arglist name)
              (if (eq type :<BOOL>)
                (bool-args `(setq ,name (not (eql ,name 0)))))))
          (let* ((impname (intern (format nil "~c[~a ~a]"
                                          (if class-p #\+ #\-)
                                          objc-class-name
                                          selector)))
                 (typestring (encode-objc-method-arglist arg-types result-type))
                 (signature (cons result-type (cddr arg-types))))
            (multiple-value-bind (body decls) (parse-body body env)
              
              (setq body `((progn ,@(bool-args) ,@(type-assertions) ,@body)))
              (if (eq result-type :<BOOL>)
                (setq body `((%coerce-to-bool ,@body))))
              (when struct-return-var
                (setq body `((%objc-struct-return ,struct-return-var ,struct-return-size ,@body)))
                (setq body `((flet ((struct-return-var-function ()
                                      ,struct-return-var))
                               (declaim (inline struct-return-var-function))
                               ,@body)))
                (setq body `((macrolet ((objc:returning-foreign-struct ((var) &body body)
                                          `(let* ((,var (struct-return-var-function)))
                                            ,@body)))
                               ,@body))))
              (setq body `((flet ((call-next-method (&rest args)
                                  (declare (dynamic-extent args))
                                  (apply (function ,(if class-p
                                                        '%call-next-objc-class-method
                                                        '%call-next-objc-method))
                                         ,self-name
                                         (@class ,objc-class-name)
                                         (@selector ,selector)
                                         ',signature
                                         args)))
                                 (declare (inline call-next-method))
                                 ,@body)))
              `(progn
                (%declare-objc-method
                 ',selector
                 ',objc-class-name
                 ,class-p
                 ',result-type
                 ',(cddr arg-types))
                (defcallback ,impname ( :error-return (condition objc-callback-error-return) ,@(arglist))
                  (declare (ignorable ,self-name ,cmd)
                           (unsettable ,self-name)
                           ,@(unless class-p `((type ,lisp-class-name ,self-name))))
                  ,@decls
                  ,@body)
                (%define-lisp-objc-method
                 ',impname
                 ,objc-class-name
                 ,selector
                 ,typestring
                 ,impname
                 ,class-p)))))))))

      
           
  

(defun class-get-instance-method (class sel)
  #+apple-objc (#_class_getInstanceMethod class sel)
  #+gnu-objc (#_class_get_instance_method class sel))

(defun class-get-class-method (class sel)
  #+apple-objc (#_class_getClassMethod class sel)
  #+gnu-objc   (#_class_get_class_method class sel))

(defun method-get-number-of-arguments (m)
  #+apple-objc (#_method_getNumberOfArguments m)
  #+gnu-objc (#_method_get_number_of_arguments m))

#+(and apple-objc (not apple-objc-2.0) ppc-target)
(progn
(defloadvar *original-deallocate-hook*
        #&_dealloc)

(defcallback deallocate-nsobject (:address obj :int)
  (unless (%null-ptr-p obj)
    (remhash obj *objc-object-slot-vectors*))
  (ff-call *original-deallocate-hook* :address obj :int))

(defun install-lisp-deallocate-hook ()
  (setf #&_dealloc deallocate-nsobject))

#+later
(def-ccl-pointers install-deallocate-hook ()
  (install-lisp-deallocate-hook))

(defun uninstall-lisp-deallocate-hook ()
  (clrhash *objc-object-slot-vectors*)
  (setf #&_dealloc *original-deallocate-hook*))

(pushnew #'uninstall-lisp-deallocate-hook *save-exit-functions* :test #'eq
         :key #'function-name)
)

  



(defloadvar *nsstring-newline* #@"
")


;;; Execute BODY with an autorelease pool

(defmacro with-autorelease-pool (&body body)
  (let ((pool-temp (gensym)))
    `(let ((,pool-temp (create-autorelease-pool)))
      (unwind-protect
	   (progn ,@body)
	(release-autorelease-pool ,pool-temp)))))


(defun %make-nsstring (string)
  (with-encoded-cstrs :utf-8 ((s string))
    (%make-nsstring-from-utf8-c-string s)))



#+apple-objc-2.0
;;; New!!! Improved!!! At best, half-right!!!
(defmacro with-ns-exceptions-as-errors (&body body)
  `(progn ,@body))
                 
             
    
#-apple-objc-2.0
(defmacro with-ns-exceptions-as-errors (&body body)
  #+apple-objc
  (let* ((nshandler (gensym))
         (cframe (gensym)))
    `(rletZ ((,nshandler :<NSH>andler2))
      (unwind-protect
           (progn
             (external-call "__NSAddHandler2" :address ,nshandler :void)
             (catch ,nshandler
               (with-c-frame ,cframe
                 (%associate-jmp-buf-with-catch-frame
                  ,nshandler
                  (%fixnum-ref (%current-tcr) target::tcr.catch-top)
                  ,cframe)
                 (progn
                   ,@body))))
        (check-ns-exception ,nshandler))))
  #+gnu-objc
  `(progn ,@body)
  )





#+(and apple-objc (not apple-objc-2.0))
(defun check-ns-exception (nshandler)
  (with-macptrs ((exception (external-call "__NSExceptionObjectFromHandler2"
                                           :address nshandler
                                           :address)))
    (if (%null-ptr-p exception)
      (external-call "__NSRemoveHandler2" :address nshandler :void)
      (error (ns-exception->lisp-condition (%inc-ptr exception 0))))))




