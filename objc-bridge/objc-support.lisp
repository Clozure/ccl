;;;-*-Mode: LISP; Package: CCL -*-

(in-package "CCL")

#+darwin-target
(unless (>= (parse-integer (software-version) :junk-allowed t) 10)
  (error "the Objective-C bridge needs at least Mac OS X 10.6"))

(defloadvar *warned-deprecated-constructs* ())

(defun warn-about-deprecated-objc-bridge-construct (whole alternative)
  (let* ((construct (car whole)))
    (unless (member construct *warned-deprecated-constructs*)
      (push construct *warned-deprecated-constructs*)
      (warn "~s, as used in ~s, is deprecated.  Use ~a instead."
            construct whole alternative))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "BRIDGE"))

(defun allocate-objc-object (class)
  (#/alloc class))



(defun conforms-to-protocol (thing protocol)
  (#/conformsToProtocol: thing (objc-protocol-address protocol)))




#+(or apple-objc cocotron-objc)
(defun iterate-over-objc-classes (fn)
  (let* ((n (#_objc_getClassList (%null-ptr) 0)))
    (declare (fixnum n))
    (%stack-block ((buffer (the fixnum (ash n target::word-shift))))
      (#_objc_getClassList buffer n)
      (do* ((i 0 (1+ i)))
           ((= i n) (values))
        (declare (fixnum i))
        (funcall fn (paref buffer (:* :id) i))))))

#+(or apple-objc cocotron-objc)
(defun count-objc-classes ()
  (#_objc_getClassList (%null-ptr) 0))  

#+gnu-objc
(defun iterate-over-objc-classes (fn)
  (rletZ ((enum-state :address))
    (loop
      (let* ((class (#_objc_next_class enum-state)))
        (if (%null-ptr-p class)
          (return)
          (funcall fn class))))))

#+gnu-objc
(defun count-objc-classes ()
  (let* ((n 0))
    (declare (fixnum n))
    (rletZ ((enum-state :address))
      (if (%null-ptr-p (#_objc_next_class enum-state))
        (return n)
        (incf n)))))


(defun %note-protocol (p)
  ;; In Cocotron (which is ultimately based on the GNU ObjC runtime),
  ;; it may be the case that some Protocol objects aren't fully initialized
  ;; when this code runs, hence the sleazy use of PREF here.
  (with-macptrs ((cname #+cocotron-objc (pref p #>Protocol.nameCString)
                        #-cocotron-objc (objc-message-send p "name" :address)))
    (let* ((namelen (%cstrlen cname))
           (name (make-string namelen)))
      (declare (dynamic-extent name))
      (%str-from-ptr cname namelen name)
      (let* ((proto (or (gethash name *objc-protocols*)
                        (progn
                          (setq name (subseq name 0))
                          (setf (gethash name *objc-protocols*)
                                (make-objc-protocol :name name))))))
        (unless (objc-protocol-address proto)
          (setf (objc-protocol-address proto) (%inc-ptr p 0)))
        proto))))

(defun note-class-protocols (class)
  #-(or apple-objc-2.0)
  (do* ((protocols (pref class :objc_class.protocols)
                   (pref protocols :objc_protocol_list.next)))
       ((%null-ptr-p protocols))
    (let* ((count (pref protocols :objc_protocol_list.count)))
      (with-macptrs ((list (pref protocols :objc_protocol_list.list)))
        (dotimes (i count)
          (with-macptrs ((p (paref list (:* (:* (:struct :<P>rotocol))) i)))
            (%note-protocol p))))))
  #+(or apple-objc-2.0)
  (rlet ((p-out-count :int 0))
    (with-macptrs ((protocols (#_class_copyProtocolList class p-out-count)))
      (let* ((n (pref p-out-count :int)))
        (dotimes (i n)
          (with-macptrs ((p (paref protocols (:* (:* (:struct :<P>rotocol))) i)))
            (%note-protocol p))))
      (unless (%null-ptr-p protocols) (#_free protocols)))))
            

(defun map-objc-classes (&optional (lookup-in-database-p t))
  (iterate-over-objc-classes
   #'(lambda (class)
       (note-class-protocols class)
       (install-foreign-objc-class class lookup-in-database-p))))

(let* ((nclasses 0)
       (lock (make-lock)))
  (declare (fixnum nclasses))
  (defun maybe-map-objc-classes (&optional use-db)
    (with-lock-grabbed (lock)
      (let* ((new (count-objc-classes)))
        (declare (fixnum new))
        (unless (= nclasses new)
          (setq nclasses new)
          (map-objc-classes use-db))
        t)))
  (defun reset-objc-class-count ()
    (with-lock-grabbed (lock)
      (setq nclasses 0))))

(register-objc-class-decls)
(maybe-map-objc-classes t)


(defvar *class-init-keywords* (make-hash-table :test #'eq))

(defun process-init-message (message-info)
  (let* ((keys (objc-to-lisp-init (objc-message-info-message-name message-info))))
    (when keys
      (let* ((keyinfo (cons keys (objc-message-info-lisp-name message-info))))
        (dolist (method (objc-message-info-methods message-info))
          (when (and (eq :id (objc-method-info-result-type method))
                     (let* ((flags (objc-method-info-flags method)))
                       (not (or (memq :class flags)
                                (memq :protocol flags)))))
	    (with-cstrs ((s (objc-method-info-class-name method)))
	      (unless (%null-ptr-p (#_objc_lookUpClass s))
		(let* ((class (canonicalize-registered-class
			       (find-objc-class (objc-method-info-class-name method)))))
		  (pushnew keyinfo (gethash class *class-init-keywords*)
			   :test #'equal))))))))))

(register-objc-init-messages)
(register-objc-set-messages)





(defun all-init-keywords-for-class (c)
  (let* ((keyinfo ()))
    (dolist (class (class-precedence-list c))
      (when (eq class ns:ns-object)
        (return keyinfo))
      (dolist (class-keys (gethash class *class-init-keywords*))
        (pushnew class-keys keyinfo :test #'equal)))))

(defun send-init-message-for-class (class initargs)
  (let* ((all-keywords-for-class (all-init-keywords-for-class class)))
    (multiple-value-bind (initfunction args)
        (if all-keywords-for-class
          (let* ((candidate-functions ())
                 (candidate-arglists ())
                 (missing-keyword (cons nil nil)))
            (declare (dynamic-extent missing-keyword))
            (dolist (keys-and-function all-keywords-for-class)
              (collect ((arglist))
                (destructuring-bind (keys . function) keys-and-function
                  (dolist (key keys (progn (push function candidate-functions)
                                           (push (arglist) candidate-arglists)))
                    (let* ((val (getf initargs key missing-keyword)))
                      (if (eq missing-keyword val)
                        (return)
                        (arglist val)))))))
            (if candidate-functions
              (if (null (cdr candidate-functions))
                (values (car candidate-functions) (car candidate-arglists))
                ;; Pick the longest match, if that's unique.  If there's
                ;; no unique longest match, complain.
                (let* ((maxlen 0)
                       (maxfun ())
                       (maxargs ())
                       (duplicate-match nil))
                  (declare (fixnum maxlen))
                  (do* ((functions candidate-functions (cdr functions))
                        (arglists candidate-arglists (cdr arglists)))
                       ((null functions)
                        (if duplicate-match
                          (values nil nil)
                          (values maxfun maxargs)))
                    (let* ((arglist (car arglists))
                           (n (length arglist)))
                      (declare (fixnum n))
                      (if (> n maxlen)
                        (setq maxlen n
                              duplicate-match nil
                              maxargs arglist
                              maxfun (car functions))
                        (if (= n maxlen)
                          (setq duplicate-match t)))))))
              (values '#/init nil)))
          (values '#/init nil))
      (if initfunction
        (let* ((instance (apply initfunction (#/alloc class) args)))
          (ensure-lisp-slots instance class)
          instance)
        (error "Can't determine ObjC init function for class ~s and initargs ~s." class initargs)))))

#+gnu-objc
(defun iterate-over-class-methods (class method-function)
  (do* ((mlist (pref class :objc_class.methods)
	       (pref mlist :objc_method_list.method_next)))
       ((%null-ptr-p mlist))
    (do* ((n (pref mlist :objc_method_list.method_count))
	  (i 0 (1+ i))
	  (method (pref mlist :objc_method_list.method_list)
		  (%incf-ptr method (record-length :objc_method))))
	 ((= i n))
      (declare (fixnum i n))
      (funcall method-function method class))))

#+gnu-objc
(progn
  ;; Er, um ... this needs lots-o-work.
  (let* ((objc-class-count 0))
    (defun reset-objc-class-count () (setq objc-class-count 0))
    (defun note-all-library-methods (method-function)
      (do* ((i objc-class-count (1+ i))
	    (class (id->objc-class i) (id->objc-class i)))
	   ((eq class 0))
	(iterate-over-class-methods class method-function)
	(iterate-over-class-methods (id->objc-metaclass i) method-function))))
  (def-ccl-pointers revive-objc-classes ()
    (reset-objc-class-count)))



#+apple-objc-2.0
(progn
(defun setup-objc-exception-globals ()
  (flet ((set-global (offset name)
           (setf (%get-ptr (%int-to-ptr (+ (target-nil-value) (%kernel-global-offset offset))))
                 (foreign-symbol-address name))))
    (set-global 'objc-2-personality "___objc_personality_v0")
    (set-global 'objc-2-begin-catch "objc_begin_catch")
    (set-global 'objc-2-end-catch "objc_end_catch")
    (set-global 'unwind-resume "__Unwind_Resume")))


(def-ccl-pointers setup-objc-exception-handling ()
  (setup-objc-exception-globals))

(setup-objc-exception-globals)
)

#+(or apple-objc cocotron-objc)         ; not really
(progn


#+ppc-target
(defun objc-callback-error-return (condition return-value-pointer return-address-pointer)
  ;; On PPC, the "address" of an external entry point is always
  ;; aligned on a 32-bit word boundary.  On PPC32, it can always
  ;; be represented as a fixnum; on PPC64, it might be a pointer
  ;; instead.
  ;; Note that this clobbers the actual (foreign) return address,
  ;; replacing it with the address of #__NSRaiseError.  Note also
  ;; that storing the NSException object as the return value has
  ;; the desired effect of causing #__NSRaiseError to be called
  ;; with that NSException as its argument (because r3 is used both
  ;; as the canonical return value register and used to pass the
  ;; first argument on PPC.)
  (process-debug-condition *current-process* condition (%get-frame-ptr))
  (let* ((addr (%reference-external-entry-point (load-time-value (external "__NSRaiseError")))))
    (if (typep addr 'fixnum)
      (%set-object return-address-pointer 0 addr)
      (setf (%get-ptr return-address-pointer 0) addr)))
  (setf (%get-ptr return-value-pointer 0) (ns-exception condition))
  nil)

#+x8664-target
(progn
(defloadvar *x8664-objc-callback-error-return-trampoline*
    (let* ((code-bytes '(#x48 #x89 #xc7      ; movq %rax %rdi
                         #x66 #x48 #x0f #x7e #xc0 ; movd %xmm0,%rax
                         #x52                ; pushq %rdx
                         #xff #xe0))         ; jmp *rax
           (nbytes (length code-bytes))
           (ptr (%allocate-callback-pointer 16)))
      (dotimes (i nbytes ptr)
        (setf (%get-unsigned-byte ptr i) (pop code-bytes)))))

(defun objc-callback-error-return (condition return-value-pointer return-address-pointer) 
  ;; The callback glue reserves space for %rax at return-value-pointer-8,
  ;; for %rdx at -16, for %xmm0 at -24.  Store NS-EXCEPTION in the
  ;; %rax slot, the address of #_objc_exception_throw in the %rdx slot, the
  ;; original return address in the %xmm0 slot, and force a return to
  ;; the trampoline code above.
  (process-debug-condition *current-process* condition (%get-frame-ptr))
  (setf (%get-ptr return-value-pointer -8) (ns-exception condition)
        (%get-ptr return-value-pointer -16) (%get-ptr return-address-pointer 0)
        (%get-ptr return-address-pointer 0) *x8664-objc-callback-error-return-trampoline*)
  ;; A foreign entry point is always an integer on x8664.
  (let* ((addr (%reference-external-entry-point (load-time-value (external "_objc_exception_throw")))))
    (if (< addr 0)                      ;unlikely
      (setf (%%get-signed-longlong return-value-pointer -24) addr)
      (setf (%%get-unsigned-longlong return-value-pointer -24) addr)))
  nil)

(defun objc-propagate-throw (throw-info return-value-pointer return-address-pointer) 
  ;; The callback glue reserves space for %rax at
  ;; return-value-pointer-8, for %rdx at -16, for %xmm0 at -24.  Store
  ;; encapsulated info about the throw in the %rax slot, the address
  ;; of #_objc_exception_throw in the %rdx slot, the original return
  ;; address in the %xmm0 slot, and force a return to the trampoline
  ;; code above.
  (setf (%get-ptr return-value-pointer -8) (encapsulate-throw-info throw-info)
        (%get-ptr return-value-pointer -16) (%get-ptr return-address-pointer 0)
        (%get-ptr return-address-pointer 0) *x8664-objc-callback-error-return-trampoline*)
  ;; A foreign entry point is always an integer on x8664.
  (let* ((addr (%reference-external-entry-point (load-time-value (external "_objc_exception_throw")))))
    (if (< addr 0)                      ;unlikely
      (setf (%%get-signed-longlong return-value-pointer -24) addr)
      (setf (%%get-unsigned-longlong return-value-pointer -24) addr)))
  nil)


)

#+x8632-target
(progn

(defloadvar *x8632-objc-callback-error-return-trampoline*
    (let* ((code-bytes '(#x83 #xec #x10      ; subl $16,%esp
                         #x89 #x04 #x24      ; movl %eax,(%esp)
                         #x52                ; pushl %edx
                         #xff #xe1))         ; jmp *ecx
           (nbytes (length code-bytes))
           (ptr (%allocate-callback-pointer 16)))
      (dotimes (i nbytes ptr)
        (setf (%get-unsigned-byte ptr i) (pop code-bytes)))))

(defun objc-callback-error-return (condition return-value-pointer return-address-pointer)
  (process-debug-condition *current-process* condition (%get-frame-ptr))
  (let* ((addr (%reference-external-entry-point (load-time-value (external #+cocotron-objc "_NSRaiseException" #-cocotron-objc "__NSRaiseError")))))
    (setf (%get-unsigned-long return-value-pointer -12 ) addr))
  (setf (%get-ptr return-value-pointer -8) (ns-exception condition)
        (%get-ptr return-value-pointer -4) (%get-ptr return-address-pointer)
        (%get-ptr return-address-pointer) *x8632-objc-callback-error-return-trampoline*)
  nil)

(defun objc-propagate-throw (throw-info return-value-pointer return-address-pointer)
  (let* ((addr (%reference-external-entry-point (load-time-value (external #+cocotron-objc "_NSRaiseException" #-cocotron-objc "__NSRaiseError")))))
    (setf (%get-unsigned-long return-value-pointer -12 ) addr))
  (setf (%get-ptr return-value-pointer -8) (encapsulate-throw-info throw-info)
        (%get-ptr return-value-pointer -4) (%get-ptr return-address-pointer)
        (%get-ptr return-address-pointer) *x8632-objc-callback-error-return-trampoline*)
  nil)
)

)




(defvar *condition-id-map* (make-id-map) "Map lisp conditions to small integers")

;;; Encapsulate an NSException in a lisp condition.
(define-condition ns-exception (error)
  ((ns-exception :initarg :ns-exception :accessor ns-exception))
  (:report (lambda (c s)
             (format s "Objective-C runtime exception: ~&~a"
                     (nsobject-description (ns-exception c))))))

(defun ensure-dealloc-method-for-class (class)
  (let* ((direct-slots (class-direct-slots class))
         (effective-slots (class-slots class)))
    (when (and (dolist (d direct-slots)
                 (when (and (typep d 'standard-direct-slot-definition)
                            (eq :instance (slot-definition-allocation d)))
                   (return t)))
               (dolist (e effective-slots t)
                 (when (and (typep e 'standard-effective-slot-definition)
                            (eq :instance (slot-definition-allocation e))
                            (not (find (slot-definition-name e)
                                       direct-slots
                                         :key #'slot-definition-name
                                         :test #'eq)))
                   (return))))
      (eval `(objc:defmethod (#/dealloc :void) ((self ,(class-name class)))
              (objc:remove-lisp-slots self)
              (call-next-method))))))

(eval-when (:compile-toplevel :execute)
  (declaim (ftype (function (&rest t) t) objc-callback-error-return)))

(defclass ns-lisp-exception (ns::ns-exception)
    ((condition :initarg :condition :initform nil :reader ns-lisp-exception-condition))
  (:metaclass ns::+ns-object))

(objc:defmethod #/init ((self ns-lisp-exception))
  (#/initWithName:reason:userInfo: self #@"lisp exception" #@"lisp exception" +null-ptr+))

(defclass encapsulated-lisp-throw (ns::ns-exception)
    ((id :foreign-type #>NSUInteger))
  (:metaclass ns::+ns-object))

(objc:defmethod #/init ((self encapsulated-lisp-throw))
  (#/initWithName:reason:userInfo: self #@"encapsulated throw" #@"encapsulated-throw" +null-ptr+))

(defun encapsulate-throw-info (info)
  (let* ((els (make-instance 'encapsulated-lisp-throw)))
    (setf (slot-value els 'id)
          (assign-id-map-id *condition-id-map* info))
    els))

(defun recognize-objc-exception (x)
  (if (typep x 'encapsulated-lisp-throw)
    (apply #'%throw (with-slots (id) x (id-map-object *condition-id-map* id)))
    (if (typep x 'ns:ns-exception)
      (ns-exception->lisp-condition x))))

(pushnew 'recognize-objc-exception *foreign-error-condition-recognizers*)

(defun objc:make-nsstring (string)
  (with-encoded-cstrs :utf-8 ((s string))
    (#/initWithUTF8String: (#/alloc ns:ns-string) s)))

(defun %make-nsstring (string)
  (objc:make-nsstring string))

(defmacro with-autoreleased-nsstring ((nsstring lisp-string) &body body)
  `(let* ((,nsstring (%make-nsstring ,lisp-string)))
     (#/autorelease ,nsstring)
     ,@body))

(defmacro objc:with-autoreleased-nsstrings (speclist &body body)
  (with-specs-aux 'with-autoreleased-nsstring speclist body))

(defun retain-objc-instance (instance)
  (#/retain instance))

;;; May have to create/release autorelease pools before the bridge
;;; is fully reinitialized, so use low-level OBJC-MESSAGE-SEND
;;; and @class.
(defun create-autorelease-pool ()
  (objc-message-send
   (objc-message-send (@class "NSAutoreleasePool") "alloc") "init"))

(defun release-autorelease-pool (p)
  (objc-message-send p "release" :void))


(defun lisp-string-from-nsstring (nsstring)
  (with-autorelease-pool
      ;; It's not clear that it's even possible to lose information
      ;; when converting to UTF-8, but allow lossage to occur, just in
      ;; case.
      (let* ((data (#/dataUsingEncoding:allowLossyConversion:
                    nsstring #$NSUTF8StringEncoding t))
             (len (#/length data)))
        (if (= len 0)
          ""
          (let* ((bytes (#/bytes data))
                 (nchars (utf-8-length-of-memory-encoding bytes len 0))
                 (string (make-string nchars)))
            (utf-8-memory-decode bytes len 0 string)
            string)))))



     




(objc:defmethod #/reason ((self ns-lisp-exception))
  (with-slots (condition) self
    (if condition
      (#/autorelease (%make-nsstring (format nil "~A" condition)))
      (call-next-method))))

(objc:defmethod #/description ((self ns-lisp-exception))
  (#/stringWithFormat: ns:ns-string #@"Lisp exception: %@" (#/reason self)))


                     
(defun ns-exception->lisp-condition (nsexception)
  (if (typep nsexception 'ns-lisp-exception)
    (ns-lisp-exception-condition nsexception)
    (make-condition 'ns-exception :ns-exception nsexception)))


(defmethod ns-exception ((c condition))
  "Map a lisp condition object to an NSException.  Note that instances
of the NS-EXCEPTION condition class implement this by accessing an
instance variable."
  ;;; Create an NSLispException with a lispid that encapsulates
  ;;; this condition.

  ;; (dbg (format nil "~a" c))
  ;;(#_NSLog #@"Lisp exception: %@" :id (%make-nsstring (format nil "~a" c)))
  (make-instance 'ns-lisp-exception :condition c))




(defun open-main-bundle ()
  (#/mainBundle ns:ns-bundle))

;;; Create a new immutable dictionary just like src, replacing the
;;; value of each key in key-value-pairs with the corresponding value.
(defun copy-dictionary (src &rest key-value-pairs)
  (declare (dynamic-extent key-value-pairs))
  ;(#_NSLog #@"src = %@" :id src)
  (let* ((count (#/count src))
	 (enum (#/keyEnumerator src))
         (keys (#/arrayWithCapacity: ns:ns-mutable-array count))
         (values (#/arrayWithCapacity: ns:ns-mutable-array count)))
    (loop
	(let* ((nextkey (#/nextObject enum)))
	  (when (%null-ptr-p nextkey)
	    (return))
	  (do* ((kvps key-value-pairs (cddr kvps))
		(newkey (car kvps) (car kvps))
		(newval (cadr kvps) (cadr kvps)))
	       ((null kvps)
		;; Copy the key, value pair from the src dict
                (#/addObject: keys nextkey)
                (#/addObject: values (#/objectForKey: src nextkey)))
	    (when (#/isEqualToString: nextkey newkey)
              (#/addObject: keys nextkey)
              (#/addObject: values newval)
	      (return)))))
    (make-instance 'ns:ns-dictionary
                   :with-objects values
                   :for-keys keys)))




(defparameter *objc-description-max-length* 1024 "Limit on the length of NSObject description strings if non-NIL.")

(defun %cf-instance-p (instance)
  #-apple-objc (declare (ignore instance))
  #+apple-objc
  (> (objc-message-send instance "_cfTypeID" #>CFTypeID) 1))
  

(defun initialized-nsobject-p (nsobject)
  (or (objc-class-p nsobject)
      (objc-metaclass-p nsobject)
      (has-lisp-slot-vector nsobject)
      (let* ((cf-p (%cf-instance-p nsobject)) 
             (isize (if cf-p (external-call "malloc_size" :address nsobject :size_t) (%objc-class-instance-size (#/class nsobject))))
             (skip (if cf-p (+ (record-length :id) 4 #+64-bit-target 4) (record-length :id))))
        (declare (fixnum isize skip))
        (or (> skip isize)
            (do* ((i skip (1+ i)))
                 ((>= i isize))
              (declare (fixnum i))
              (unless (zerop (the (unsigned-byte 8) (%get-unsigned-byte nsobject i)))
                (return t)))))))
  
(defun nsobject-description (nsobject)
  "Returns a lisp string that describes nsobject.  Note that some
NSObjects describe themselves in more detail than others."
  (if (initialized-nsobject-p nsobject)
    (with-autorelease-pool
        (let* ((desc (#/description nsobject)))
          (if (or (null *objc-description-max-length*)
                  (< (#/length desc) *objc-description-max-length*))
            (lisp-string-from-nsstring desc)
            (ns:with-ns-range (r 0 *objc-description-max-length*)
              (format nil "~a[...]"(lisp-string-from-nsstring (#/substringWithRange: desc r)))))))
    "[uninitialized]"))





(defun lisp-string-from-nsstring-substring (nsstring start length)
  (let* ((substring (#/substringWithRange: nsstring (ns:make-ns-range start length))))
    (lisp-string-from-nsstring substring)))

(def-standard-initial-binding *listener-autorelease-pool* nil)

(setq *listener-autorelease-pool* (create-autorelease-pool))

(define-toplevel-command :global rap () "Release and reestablish *LISTENER-AUTORELEASE-POOL*"
  (when (eql *break-level* 0)
    (without-interrupts
     (when (boundp '*listener-autorelease-pool*)
       (let* ((old *listener-autorelease-pool*))
	 (if old (release-autorelease-pool old))
	 (setq *listener-autorelease-pool* (create-autorelease-pool)))))))

#+apple-objc
(defun show-autorelease-pools ()
  (objc-message-send (@class ns-autorelease-pool) "showPools" :void))

#+gnu-objc
(defun show-autorelease-pools ()
  (do* ((current (objc-message-send (@class ns-autorelease-pool) "currentPool")
		 (objc-message-send current "_parentAutoreleasePool"))
	(i 0 (1+ i)))
       ((%null-ptr-p current) (values))
    (format t "~& ~d : ~a [~d]"
	    i
	    (nsobject-description current)
	    (pref current :<NSA>utorelease<P>ool._released_count))))

#+cocotron-objc
(defun show-autorelease-pools ()
  (%string-to-stderr  "No info about current thread's autorelease pools is available"))

(define-toplevel-command :global sap () "Log information about current thread's autorelease-pool(s) to C's standard error stream"
  (show-autorelease-pools))

(define-toplevel-command :global kap () "Release (but don't reestablish) *LISTENER-AUTORELEASE-POOL*"
  (when (eql *break-level* 0)
    (without-interrupts
     (when (boundp '*listener-autorelease-pool*)
       (let* ((p *listener-autorelease-pool*))
	 (setq *listener-autorelease-pool* nil)
	 (release-autorelease-pool p))))))

;;; Use the interfaces for an add-on ObjC framework.  We need to
;;; tell the bridge to reconsider what it knows about the type
;;; signatures of ObjC messages, since the new headers may define
;;; a method whose type signature differs from the message's existing
;;; methods.  (This probably doesn't happen too often, but it's
;;; possible that some SENDs that have already been compiled would
;;; need to be recompiled with that augmented method type info, e.g.,
;;; because ambiguity was introduced.)

(defun augment-objc-interfaces (dirname)
  (use-interface-dir dirname)
  (register-objc-class-decls)
  (update-objc-method-info))

;;; A list of "standard" locations which are known to contain
;;; framework bundles.  We should look in ~/Library/Frameworks/" first,
;;; if it exists.
(defparameter *standard-framework-directories*
  (list #p"/Library/Frameworks/"
        #p"/System/Library/Frameworks/"))



;;; This has to run during application (re-)initializtion, so it
;;; uses lower-level bridge features.
(defun %reload-objc-framework (path)
  (when (probe-file path)
    (let* ((namestring (native-translated-namestring path)))
      (with-cstrs ((cnamestring namestring))
        (with-nsstr (nsnamestring cnamestring (length namestring))
          (with-autorelease-pool
              (let* ((bundle (objc-message-send (@class "NSBundle")
                                                "bundleWithPath:"
                                                :id nsnamestring :id)))
                (unless (%null-ptr-p bundle)
                  (objc-message-send bundle "load" :<BOOL>)))))))))


(defun load-objc-extension-framework (name)
  (let* ((dirs *standard-framework-directories*)
         (home-frameworks (make-pathname :defaults nil
                                         :directory
                                         (append (pathname-directory
                                                  (user-homedir-pathname))
                                                 '("Library" "Frameworks"))))
         (fname (list (format nil "~a.framework" name))))
    (when (probe-file home-frameworks)
      (pushnew home-frameworks dirs :test #'equalp))
    (dolist (d dirs)
      (let* ((path (probe-file (make-pathname :defaults nil
                                              :directory (append (pathname-directory d)
                                                                 fname)))))
        (when path
          (let* ((namestring (native-translated-namestring path)))
            (with-cstrs ((cnamestring namestring))
              (with-nsstr (nsnamestring cnamestring (length namestring))
                (with-autorelease-pool
                    (let* ((bundle (#/bundleWithPath: ns:ns-bundle nsnamestring))
                           (winning (unless (%null-ptr-p bundle)
                                      t)))
                      (when winning
                        (let* ((libpath (#/executablePath bundle)))
                          (unless (%null-ptr-p libpath)
                            (open-shared-library (lisp-string-from-nsstring
                                                  libpath))))
                        (#/load bundle)
                        (pushnew path *extension-framework-paths*
                                 :test #'equalp)
                        (map-objc-classes)
                        ;; Update info about init messages.
                        (register-objc-init-messages)
                        (register-objc-set-messages))
                      (return winning)))))))))))

(defun objc:load-framework (framework-name interfaces-name)
  (use-interface-dir interfaces-name)
  (or (load-objc-extension-framework framework-name)
      (error "Can't load ObjC framework ~s" framework-name))
  (augment-objc-interfaces interfaces-name))

                      
(defmethod print-object ((p ns:protocol) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~a (#x~x)"
            (%get-cstring (#/name p))
            (%ptr-to-int p))))

                                         
(defmethod terminate ((instance objc:objc-object))
  (objc-message-send instance "release"))

(defloadvar *tagged-instance-class-indices* ())

(defun %safe-get-objc-class (instance)
  (let* ((tcr (%current-tcr)))
    (without-interrupts
     (unwind-protect
          (progn
            (setf (%fixnum-ref tcr target::tcr.safe-ref-address) 1)
            (objc-message-send instance "class"))
       (setf (%fixnum-ref tcr target::tcr.safe-ref-address) 0)))))

(defun lookup-tagged-instance-class (instance)
  (let* ((tag (tagged-objc-instance-p instance)))
    (if tag
      (let* ((class (%safe-get-objc-class instance)))
        (unless (%null-ptr-p class)
          (install-foreign-objc-class class nil)
          (let* ((idx (objc-class-or-private-class-id class)))
            (atomic-push-uvector-cell (symptr->symvector '*tagged-instance-class-indices*)
                                      target::symbol.vcell-cell
                                      (cons tag idx))
            idx))))))

      


(defun objc-tagged-instance-class-index (instance tag)
  (or (cdr (assoc tag *tagged-instance-class-indices* :test #'eq))
      (lookup-tagged-instance-class instance)))
  



(provide "OBJC-SUPPORT")
