;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2001 Clozure Associates
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

;;; This is a slightly-watered-down version of CMUCL's ALIEN-TYPE system.

(in-package "CCL")

(defstruct (interface-dir
	     (:include dll-node)
	    )
  (name)
  (subdir)
  (constants-interface-db-file)
  (functions-interface-db-file)
  (records-interface-db-file)
  (types-interface-db-file)
  (vars-interface-db-file)
  (objc-classes-interface-db-file)
  (objc-methods-interface-db-file))

(defmethod print-object ((d interface-dir) stream)
  (print-unreadable-object (d stream :type t :identity t)
    (format stream "~s ~s"
            (interface-dir-name d)
            (interface-dir-subdir d))))

;;; We can't reference foreign types early in the cold load,
;;; but we want things like RLET to be able to set a pointer's
;;; type based on the foreign-type's "ordinal".  We therefore
;;; seem to have to arrange that certain types have fixed,
;;; "canonical" ordinals.  I doubt if we need more than a handful
;;; of these, but let's burn 100

(defconstant max-canonical-foreign-type-ordinal 100)

;;; Some foreign types are "common" (POSIXy things that're available
;;; on most platforms; some are very platform-specific.  It's getting
;;; to be a mess to keep those separate by reader conditionalization,
;;; so use the first 50 ordinals for "common" foreign types and the
;;; next 50 for platform-specific stuff.

(defconstant max-common-foreign-type-ordinal 50)

;;; This is intended to try to encapsulate foreign type stuff, to
;;; ease cross-compilation (among other things.)

(defstruct (foreign-type-data (:conc-name ftd-)
			      (:constructor make-ftd))
  (translators (make-hash-table :test #'eq))
  (kind-info (make-hash-table :test #'eq))
  (definitions (make-hash-table :test #'eq))
  (struct-definitions (make-hash-table :test #'eq))
  (union-definitions (make-hash-table :test #'eq))
  ;; Do we even use this ?
  (enum-definitions (make-hash-table :test #'eq))
  (interface-db-directory ())
  (interface-package-name ())
  (external-function-definitions (make-hash-table :test #'eq))
  (dirlist (make-dll-header))
  (attributes ())
  (ff-call-expand-function ())
  (ff-call-struct-return-by-implicit-arg-function ())
  (callback-bindings-function ())
  (callback-return-value-function ())
  (ordinal max-canonical-foreign-type-ordinal)
  (ordinal-lock (make-lock))
  (ordinal-types (make-hash-table :test #'eq :weak :value))
  (pointer-types (make-hash-table :test #'eq))
  (array-types (make-hash-table :test #'equal))
  (platform-ordinal-types ()))




(defvar *host-ftd* (make-ftd
                    :interface-db-directory
                    #.(ecase (backend-name *target-backend*)
                        (:linuxppc32 "ccl:headers;")
                        (:darwinppc32 "ccl:darwin-headers;")
                        (:darwinppc64 "ccl:darwin-headers64;")
                        (:linuxppc64 "ccl:headers64;")
			(:darwinx8632 "ccl:darwin-x86-headers;")
                        (:linuxx8664 "ccl:x86-headers64;")
                        (:darwinx8664 "ccl:darwin-x86-headers64;")
                        (:freebsdx8664 "ccl:freebsd-headers64;")
                        (:solarisx8664 "ccl:solarisx64-headers;")
                        (:win64 "ccl:win64-headers;")
                        (:linuxx8632 "ccl:x86-headers;")
                        (:win32 "ccl:win32-headers;")
                        (:solarisx8632 "ccl:solarisx86-headers;")
                        (:freebsdx8632 "ccl:freebsd-headers;"))
                    :interface-package-name
                    #.(ftd-interface-package-name *target-ftd*)
                    :attributes
                    '(:bits-per-word #+64-bit-target 64 #+32-bit-target 32
                      #+win64-target :bits-per-long #+win64-target 32
                      :signed-char #+darwinppc-target t #-darwinppc-target nil
                      :struct-by-value #+darwinppc-target t #-darwinppc-target nil
                      :struct-return-in-registers #+(or (and darwinppc-target 64-bit-target)) t #-(or (and darwinppc-target 64-bit-target)) nil
                      :struct-return-explicit  #+(or (and darwinppc-target 64-bit-target)) t #-(or (and darwinppc-target 64-bit-target)) nil
                      :struct-by-value-by-field  #+(or (and darwinppc-target 64-bit-target)) t #-(or (and darwinppc-target 64-bit-target)) nil
                    
                      :prepend-underscores #+darwinppc-target t #-darwinppc-target nil)
                    :ff-call-expand-function
                    'os::expand-ff-call
                    :ff-call-struct-return-by-implicit-arg-function
                    'os::record-type-returns-structure-as-first-arg
                    :callback-bindings-function
                    'os::generate-callback-bindings
                    :callback-return-value-function
                    'os::generate-callback-return-value
                    :platform-ordinal-types
                    (case (backend-name *target-backend*)
                        (:win64 '((:struct :_stat64)))
                        (:win32 '((:struct :__stat64)))
                        (t
                         (case (target-os-name *target-backend*)
                           (:darwin '((:struct :host_basic_info)))
                           (:solaris '((:struct :lifnum)
                                       (:struct :lifconf)))
                           (t ()))))))
                    
(defvar *target-ftd* *host-ftd*)
(setf (backend-target-foreign-type-data *host-backend*)
      *host-ftd*)

(defun next-foreign-type-ordinal (&optional (ftd *target-ftd*))
  (with-lock-grabbed ((ftd-ordinal-lock ftd))
    (incf (ftd-ordinal ftd))))


(defmacro do-interface-dirs ((dir &optional (ftd '*target-ftd*)) &body body)
  `(do-dll-nodes  (,dir (ftd-dirlist ,ftd))
    ,@body))

(defun find-interface-dir (name &optional (ftd *target-ftd*))
  (do-interface-dirs (d ftd)
    (when (eq name (interface-dir-name d))
      (return d))))

(defun require-interface-dir (name &optional (ftd *target-ftd*))
  (or (find-interface-dir name ftd)
      (error "Interface directory ~s not found" name)))

(defun ensure-interface-dir (name &optional (ftd *target-ftd*))
  (or (find-interface-dir name ftd)
      (let* ((d (make-interface-dir
		 :name name
		 :subdir (make-pathname
			  :directory
			  `(:relative ,(string-downcase name))))))
	(append-dll-node d (ftd-dirlist ftd)))))

(defun use-interface-dir (name &optional (ftd *target-ftd*))
  "Tell OpenMCL to add the interface directory denoted by dir-id to the
list of interface directories which it consults for foreign type and
function information. Arrange that that directory is searched before any
others.

Note that use-interface-dir merely adds an entry to a search list. If the
named directory doesn't exist in the file system or doesn't contain a set
of database files, a runtime error may occur when OpenMCL tries to open some
database file in that directory, and it will try to open such a database
file whenever it needs to find any foreign type or function information.
unuse-interface-dir may come in handy in that case."
  (let* ((d (ensure-interface-dir name ftd)))
    (move-dll-nodes d (ftd-dirlist ftd))
    d))

(defun unuse-interface-dir (name &optional (ftd *target-ftd*))
  "Tell OpenMCL to remove the interface directory denoted by dir-id from
the list of interface directories which are consulted for foreign type
and function information. Returns T if the directory was on the search
list, NIL otherwise."
  (let* ((d (find-interface-dir name ftd)))
    (when d
      (remove-dll-node d)
      t)))


(use-interface-dir :libc)


;;;; Utility functions.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun align-offset (offset alignment)
    (let ((extra (rem offset alignment)))
      (if (zerop extra) offset (+ offset (- alignment extra)))))

  (defun guess-alignment (bits)
    (cond ((null bits) nil)
          ((> bits 32) 64)
          ((> bits 16) 32)
          ((> bits 8) 16)
          ((= bits 8) 8)
          (t 1)))

  (defstruct foreign-type-class
    (name nil #|:type symbol|#)
    (include nil :type (or null foreign-type-class))
    (unparse nil :type (or null function))
    (type= nil :type (or null function))
    (lisp-rep nil :type (or null function))
    (foreign-rep nil :type (or null function))
    (extract-gen nil :type (or null function))
    (deposit-gen nil :type (or null function))
    (naturalize-gen nil :type (or null function))
    (deport-gen nil :type (or null function))
    ;; Cast?
    (arg-tn nil :type (or null function))
    (result-tn nil :type (or null function))
    (subtypep nil :type (or null function)))

  (defvar *foreign-type-classes* (make-hash-table :test #'eq))

  (defun info-foreign-type-translator (x &optional (ftd *target-ftd*))
    (gethash (make-keyword x) (ftd-translators ftd)))
  (defun (setf info-foreign-type-translator) (val x &optional (ftd *target-ftd*))
    (setf (gethash (make-keyword x) (ftd-translators ftd)) val))

  (defun note-foreign-type-ordinal (type ftd)
    (let* ((ordinal (and type (foreign-type-ordinal type))))
      (when (and ordinal (not (eql 0 ordinal)))
        (with-lock-grabbed ((ftd-ordinal-lock ftd))
          (setf (gethash ordinal (ftd-ordinal-types ftd)) type)))))
  
  (defun info-foreign-type-kind (x &optional (ftd *target-ftd*))
    (if (info-foreign-type-translator x ftd)
      :primitive
      (or (gethash (make-keyword x) (ftd-kind-info ftd)) :unknown)))
  (defun (setf info-foreign-type-kind) (val x &optional (ftd *target-ftd*))
    (setf (gethash (make-keyword x) (ftd-kind-info ftd)) val))
		   
  (defun info-foreign-type-definition (x &optional (ftd *target-ftd*))
    (gethash (make-keyword x) (ftd-definitions ftd)))
  (defun (setf info-foreign-type-definition) (val x &optional (ftd *target-ftd*))
    (note-foreign-type-ordinal val ftd)
    (setf (gethash (make-keyword x) (ftd-definitions ftd)) val))
  (defun clear-info-foreign-type-definition (x &optional (ftd *target-ftd*))
    (remhash (make-keyword x) (ftd-definitions ftd)))

  (defun info-foreign-type-struct (x &optional (ftd *target-ftd*))
    (gethash (make-keyword x) (ftd-struct-definitions ftd)))
  (defun (setf info-foreign-type-struct) (val x &optional (ftd *target-ftd*))
    (let* ((name (make-keyword x)))
      (when (gethash name (ftd-union-definitions ftd))
        (cerror "Define ~s as a struct type"
                "~s is already defined as a union type"
                name)
        (remhash name (ftd-union-definitions ftd)))
      (note-foreign-type-ordinal val ftd)
      (setf (gethash name (ftd-struct-definitions ftd)) val)))

  (defun info-foreign-type-union (x &optional (ftd *target-ftd*))
    (gethash (make-keyword x) (ftd-union-definitions ftd)))
  (defun (setf info-foreign-type-union) (val x  &optional (ftd *target-ftd*))
    (let* ((name (make-keyword x)))
      (when (gethash name (ftd-struct-definitions ftd))
        (cerror "Define ~s as a union type"
                "~s is already defined as a struct type"
                name)
        (remhash name (ftd-struct-definitions ftd)))
    (note-foreign-type-ordinal val ftd)
    (setf (gethash name (ftd-union-definitions ftd)) val)))

  (defun info-foreign-type-enum (x  &optional (ftd *target-ftd*))
    (gethash (make-keyword x) (ftd-enum-definitions ftd)))
  (defun (setf info-foreign-type-enum) (val x &optional (ftd *target-ftd*))
    (note-foreign-type-ordinal val ftd)
    (setf (gethash (make-keyword x) (ftd-enum-definitions ftd)) val))

  (defun require-foreign-type-class (name)
    (or (gethash name  *foreign-type-classes*)
        (error "Unknown foreign type class ~s" name)))

  (defun find-or-create-foreign-type-class (name include)
    (let* ((old (gethash name *foreign-type-classes*))
           (include-class (if include (require-foreign-type-class include))))
      (if old
        (setf (foreign-type-class-name old) include-class)
        (setf (gethash name *foreign-type-classes*)
              (make-foreign-type-class :name name :include include-class)))))


  (defconstant method-slot-alist
    '((:unparse . foreign-type-class-unparse)
      (:type= . foreign-type-class-type=)
      (:subtypep . foreign-type-class-subtypep)
      (:lisp-rep . foreign-type-class-lisp-rep)
      (:foreign-rep . foreign-type-class-foreign-rep)
      (:extract-gen . foreign-type-class-extract-gen)
      (:deposit-gen . foreign-type-class-deposit-gen)
      (:naturalize-gen . foreign-type-class-naturalize-gen)
      (:deport-gen . foreign-type-class-deport-gen)
      ;; Cast?
      (:arg-tn . foreign-type-class-arg-tn)
      (:result-tn . foreign-type-class-result-tn)))

  (defun method-slot (method)
    (cdr (or (assoc method method-slot-alist)
             (error "No method ~S" method))))
  )

(defmethod print-object ((f foreign-type-class) out)
  (print-unreadable-object (f out :type t :identity t)
    (prin1 (foreign-type-class-name f) out)))


;;; We define a keyword "BOA" constructor so that we can reference the slots
;;; names in init forms.
;;;
(defmacro def-foreign-type-class ((name &key include include-args) &rest slots)
  (let ((defstruct-name
	 (intern (concatenate 'string "FOREIGN-" (symbol-name name) "-TYPE"))))
    (multiple-value-bind
	(include include-defstruct overrides)
	(etypecase include
	  (null
	   (values nil 'foreign-type nil))
	  (symbol
	   (values
	    include
	    (intern (concatenate 'string
				 "FOREIGN-" (symbol-name include) "-TYPE"))
	    nil))
	  (list
	   (values
	    (car include)
	    (intern (concatenate 'string
				 "FOREIGN-" (symbol-name (car include)) "-TYPE"))
	    (cdr include))))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (find-or-create-foreign-type-class ',name ',(or include 'root)))
	 (defstruct (,defstruct-name
			(:include ,include-defstruct
				  (class ',name)
				  ,@overrides)
			(:constructor
			 ,(intern (concatenate 'string "MAKE-"
					       (string defstruct-name)))
			 (&key class bits alignment
			       ,@(mapcar #'(lambda (x)
					     (if (atom x) x (car x)))
					 slots)
			       ,@include-args)))
	   ,@slots)))))

(defmacro def-foreign-type-method ((class method) lambda-list &rest body)
  (let ((defun-name (intern (concatenate 'string
					 (symbol-name class)
					 "-"
					 (symbol-name method)
					 "-METHOD"))))
    `(progn
       (defun ,defun-name ,lambda-list
	 ,@body)
       (setf (,(method-slot method) (require-foreign-type-class ',class))
	     #',defun-name))))

(defmacro invoke-foreign-type-method (method type &rest args)
  (let ((slot (method-slot method)))
    (once-only ((type type))
      `(funcall (do ((class (require-foreign-type-class (foreign-type-class ,type))
			    (foreign-type-class-include class)))
		    ((null class)
		     (error "Method ~S not defined for ~S"
			    ',method (foreign-type-class ,type)))
		  (let ((fn (,slot class)))
		    (when fn
		      (return fn))))
		,type ,@args))))


;;;; Foreign-type defstruct.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (find-or-create-foreign-type-class 'root nil))

(defstruct (foreign-type
	    (:constructor make-foreign-type (&key class bits alignment ordinal))
	    (:print-object
	     (lambda (s out)
	       (print-unreadable-object (s out :type t :identity t)
		 (prin1 (unparse-foreign-type s) out)))))
  (class 'root :type symbol)
  (bits nil :type (or null unsigned-byte))
  (alignment (guess-alignment bits) :type (or null unsigned-byte))
  (ordinal (next-foreign-type-ordinal)))



(defmethod make-load-form ((s foreign-type) &optional env)
  (if (eq s *void-foreign-type*)
    '*void-foreign-type*
    (make-load-form-saving-slots s :environment env)))




;;;; Type parsing and unparsing.

(defvar *auxiliary-type-definitions* nil)
(defvar *new-auxiliary-types*)

;;; WITH-AUXILIARY-FOREIGN-TYPES -- internal.
;;;
;;; Process stuff in a new scope.
;;;
(defmacro with-auxiliary-foreign-types (&body body)
  `(let ((*auxiliary-type-definitions*
	  (if (boundp '*new-auxiliary-types*)
	      (append *new-auxiliary-types* *auxiliary-type-definitions*)
	      *auxiliary-type-definitions*))
	 (*new-auxiliary-types* nil))
     ,@body))

;;; PARSE-FOREIGN-TYPE -- public
;;;
(defun parse-foreign-type (type &optional (ftd *target-ftd*))
  "Parse the list structure TYPE as a foreign type specifier and return
   the resultant foreign-type structure."
  (if (boundp '*new-auxiliary-types*)
    (%parse-foreign-type type ftd)
    (let ((*new-auxiliary-types* nil))
      (%parse-foreign-type type ftd))))

(defun %parse-foreign-type (type &optional (ftd *target-ftd*))
  (if (consp type)
    (let ((translator (info-foreign-type-translator (car type) ftd)))
      (unless translator
        (error "Unknown foreign type: ~S" type))
      (funcall translator type nil))
    (case (info-foreign-type-kind type)
      (:primitive
       (let ((translator (info-foreign-type-translator type ftd)))
         (unless translator
           (error "No translator for primitive foreign type ~S?" type))
      (funcall translator (list type) nil)))
      (:defined
          (or (info-foreign-type-definition type ftd)
              (error "Definition missing for foreign type ~S?" type)))
      (:unknown
       (let* ((loaded (load-foreign-type type ftd)))
	 (if loaded
	   (setq type loaded)))
       (or (info-foreign-type-definition type ftd)
           (error "Unknown foreign type: ~S" type))))))

(defun auxiliary-foreign-type (kind name &optional (ftd *target-ftd*))
  (declare (ignore ftd))
  (flet ((aux-defn-matches (x)
           (and (eq (first x) kind) (eq (second x) name))))
    (let ((in-auxiliaries
           (or (find-if #'aux-defn-matches *new-auxiliary-types*)
               (find-if #'aux-defn-matches *auxiliary-type-definitions*))))
      (if in-auxiliaries
        (values (third in-auxiliaries) t)))))

(defun %set-auxiliary-foreign-type (kind name defn &optional (ftd *target-ftd*))
  (declare (ignore ftd))
  (flet ((aux-defn-matches (x)
	   (and (eq (first x) kind) (eq (second x) name))))
    (when (find-if #'aux-defn-matches *new-auxiliary-types*)
      (error "Attempt to multiple define ~A ~S." kind name))
    (when (find-if #'aux-defn-matches *auxiliary-type-definitions*)
      (error "Attempt to shadow definition of ~A ~S." kind name)))
  (push (list kind name defn) *new-auxiliary-types*)
  defn)

(defsetf auxiliary-foreign-type %set-auxiliary-foreign-type)


(defun ensure-foreign-type (x)
  (if (typep x 'foreign-type)
    x
    (parse-foreign-type x)))

;;; *record-type-already-unparsed* -- internal
;;;
;;; Holds the list of record types that have already been unparsed.  This is
;;; used to keep from outputing the slots again if the same structure shows
;;; up twice.
;;; 
(defvar *record-types-already-unparsed*)

;;; UNPARSE-FOREIGN-TYPE -- public.
;;; 
(defun unparse-foreign-type (type)
  "Convert the foreign-type structure TYPE back into a list specification of
   the type."
  (declare (type foreign-type type))
  (let ((*record-types-already-unparsed* nil))
    (%unparse-foreign-type type)))

;;; %UNPARSE-FOREIGN-TYPE -- internal.
;;;
;;; Does all the work of UNPARSE-FOREIGN-TYPE.  It's seperate because we need
;;; to recurse inside the binding of *record-types-already-unparsed*.
;;; 
(defun %unparse-foreign-type (type)
  (invoke-foreign-type-method :unparse type))




;;;; Foreign type defining stuff.

(defmacro def-foreign-type-translator (name lambda-list &body body &environment env)
  (expand-type-macro '%def-foreign-type-translator name lambda-list body env))


(defun %def-foreign-type-translator (name translator docs)
  (declare (ignore docs))
  (setf (info-foreign-type-translator name) translator)
  (clear-info-foreign-type-definition name)
  #+nil
  (setf (documentation name 'foreign-type) docs)
  name)


(defmacro def-foreign-type (name type)
  "If name is non-NIL, define name to be an alias for the foreign type
specified by foreign-type-spec. If foreign-type-spec is a named structure
or union type, additionally defines that structure or union type.

If name is NIL, foreign-type-spec must be a named foreign struct or union
definition, in which case the foreign structure or union definition is put
in effect.

Note that there are two separate namespaces for foreign type names, one for
the names of ordinary types and one for the names of structs and unions.
Which one name refers to depends on foreign-type-spec in the obvious manner."
  (with-auxiliary-foreign-types
    (let ((foreign-type (parse-foreign-type type)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 ,@(when *new-auxiliary-types*
	     `((%def-auxiliary-foreign-types ',*new-auxiliary-types*)))
	 ,@(when name
	     `((%def-foreign-type ',name ',foreign-type)))))))

(defun %def-auxiliary-foreign-types (types)
  (dolist (info types)
    (destructuring-bind (kind name defn) info
      (macrolet ((frob (accessor)
		   `(let ((old (,accessor name)))
		      (unless (or (null old) (foreign-type-= old defn))
			(warn "Redefining ~A ~S to be:~%  ~S,~%was:~%  ~S"
			      kind name defn old))
		      (setf (,accessor name) defn))))
	(ecase kind
	  (:struct (frob info-foreign-type-struct))
	  (:union (frob info-foreign-type-union))
	  (:enum (frob info-foreign-type-enum)))))))

(defun %def-foreign-type (name new &optional (ftd *target-ftd*))
  (ecase (info-foreign-type-kind name ftd)
    (:primitive
     (error "~S is a built-in foreign type." name))
    (:defined
     (let ((old (info-foreign-type-definition name ftd)))
       (unless (or (null old) (foreign-type-= new old))
	 (warn "Redefining ~S to be:~%  ~S,~%was~%  ~S" name
	       (unparse-foreign-type new) (unparse-foreign-type old)))))
    (:unknown))
  (setf (info-foreign-type-definition name ftd) new)
  (setf (info-foreign-type-kind name ftd) :defined)
  name)



;;;; Interfaces to the different methods

(defun foreign-type-= (type1 type2)
  "Return T iff TYPE1 and TYPE2 describe equivalent foreign types."
  (or (eq type1 type2)
      (and (eq (foreign-type-class type1)
	       (foreign-type-class type2))
	   (invoke-foreign-type-method :type= type1 type2))))

(defun foreign-subtype-p (type1 type2)
  "Return T iff the foreign type TYPE1 is a subtype of TYPE2.  Currently, the
   only supported subtype relationships are is that any pointer type is a
   subtype of (* t), and any array type first dimension will match 
   (array <eltype> nil ...).  Otherwise, the two types have to be
   FOREIGN-TYPE-=."
  (or (eq type1 type2)
      (invoke-foreign-type-method :subtypep type1 type2)))

(defun foreign-typep (object type)
  "Return T iff OBJECT is a foreign of type TYPE."
  (let ((lisp-rep-type (compute-lisp-rep-type type)))
    (if lisp-rep-type
	(typep object lisp-rep-type))))


(defun compute-naturalize-lambda (type)
  `(lambda (foreign ignore)
     (declare (ignore ignore))
     ,(invoke-foreign-type-method :naturalize-gen type 'foreign)))

(defun compute-deport-lambda (type)
  (declare (type foreign-type type))
  (multiple-value-bind
      (form value-type)
      (invoke-foreign-type-method :deport-gen type 'value)
    `(lambda (value ignore)
       (declare (type ,(or value-type
			   (compute-lisp-rep-type type)
			   `(foreign ,type))
		      value)
		(ignore ignore))
       ,form)))

(defun compute-extract-lambda (type)
  `(lambda (sap offset ignore)
     (declare (type system-area-pointer sap)
	      (type unsigned-byte offset)
	      (ignore ignore))
     (naturalize ,(invoke-foreign-type-method :extract-gen type 'sap 'offset)
		 ',type)))

(defun compute-deposit-lambda (type)
  (declare (type foreign-type type))
  `(lambda (sap offset ignore value)
     (declare (type system-area-pointer sap)
	      (type unsigned-byte offset)
	      (ignore ignore))
     (let ((value (deport value ',type)))
       ,(invoke-foreign-type-method :deposit-gen type 'sap 'offset 'value)
       ;; Note: the reason we don't just return the pre-deported value
       ;; is because that would inhibit any (deport (naturalize ...))
       ;; optimizations that might have otherwise happen.  Re-naturalizing
       ;; the value might cause extra consing, but is flushable, so probably
       ;; results in better code.
       (naturalize value ',type))))

(defun compute-lisp-rep-type (type)
  (invoke-foreign-type-method :lisp-rep type))

(defun compute-foreign-rep-type (type)
  (invoke-foreign-type-method :foreign-rep type))





;;;; Default methods.

(defvar *void-foreign-type* (make-foreign-type :class 'root :bits 0 :alignment 0 :ordinal 0))

(def-foreign-type-method (root :unparse) (type)
  (if (eq type *void-foreign-type*)
    :void
    `(!!unknown-foreign-type!! ,(type-of type))))

(def-foreign-type-method (root :type=) (type1 type2)
  (declare (ignore type1 type2))
  t)

(def-foreign-type-method (root :subtypep) (type1 type2)
  (foreign-type-= type1 type2))

(def-foreign-type-method (root :lisp-rep) (type)
  (declare (ignore type))
  nil)

(def-foreign-type-method (root :foreign-rep) (type)
  (declare (ignore type))
  '*)

(def-foreign-type-method (root :naturalize-gen) (type foreign)
  (declare (ignore foreign))
  (error "Cannot represent ~S typed foreigns." type))

(def-foreign-type-method (root :deport-gen) (type object)
  (declare (ignore object))
  (error "Cannot represent ~S typed foreigns." type))

(def-foreign-type-method (root :extract-gen) (type sap offset)
  (declare (ignore sap offset))
  (error "Cannot represent ~S typed foreigns." type))

(def-foreign-type-method (root :deposit-gen) (type sap offset value)
  `(setf ,(invoke-foreign-type-method :extract-gen type sap offset) ,value))

(def-foreign-type-method (root :arg-tn) (type state)
  (declare (ignore state))
  (error "Cannot pass foreigns of type ~S as arguments to call-out"
	 (unparse-foreign-type type)))

(def-foreign-type-method (root :result-tn) (type state)
  (declare (ignore state))
  (error "Cannot return foreigns of type ~S from call-out"
	 (unparse-foreign-type type)))



;;;; The INTEGER type.

(def-foreign-type-class (integer)
  (signed t :type (member t nil)))

(defvar *unsigned-integer-types*
  (let* ((a (make-array 65)))
    (dotimes (i 65 a)
      (setf (svref a i) (make-foreign-integer-type :signed nil
						   :bits i
						   :alignment
						   (if (= 1 (logcount i))
                                                     i
                                                     1))))))

(defvar *signed-integer-types*
  (let* ((a (make-array 65)))
    (dotimes (i 65 a)
      (setf (svref a i) (make-foreign-integer-type :signed t
						   :bits i
						   :alignment
                                                   (if (= 1 (logcount i))
                                                     i
                                                     1))))))
         

(defvar *bool-type* (make-foreign-integer-type :bits 8 :signed #+darwin-target t #-darwin-target nil))

						  

(def-foreign-type-method (integer :unparse) (type)
  (if (eq type *bool-type*)
    :<BOOL>
    (let* ((bits (foreign-integer-type-bits type))
           (signed (foreign-integer-type-signed type))
           (alignment (foreign-integer-type-alignment type)))
      (if (eql alignment 1)
        (if (eql bits 1)
          :bit
          `(:bitfield ,bits))
        (list (if signed :signed :unsigned) bits)))))
  
(def-foreign-type-method (integer :type=) (type1 type2)
  (and (eq (foreign-integer-type-signed type1)
	   (foreign-integer-type-signed type2))
       (= (foreign-integer-type-bits type1)
	  (foreign-integer-type-bits type2))))

(def-foreign-type-method (integer :lisp-rep) (type)
  (list (if (foreign-integer-type-signed type) 'signed-byte 'unsigned-byte)
	(foreign-integer-type-bits type)))

(def-foreign-type-method (integer :foreign-rep) (type)
  (list (if (foreign-integer-type-signed type) 'signed-byte 'unsigned-byte)
	(foreign-integer-type-bits type)))

(def-foreign-type-method (integer :naturalize-gen) (type foreign)
  (declare (ignore type))
  foreign)

(def-foreign-type-method (integer :deport-gen) (type value)
  (declare (ignore type))
  value)

(def-foreign-type-method (integer :extract-gen) (type sap offset)
  (declare (type foreign-integer-type type))
  (let ((ref-form
	 (if (foreign-integer-type-signed type)
	  (case (foreign-integer-type-bits type)
	    (8 `(%get-signed-byte ,sap (/ ,offset 8)))
	    (16 `(%get-signed-word ,sap (/ ,offset 8)))
	    (32 `(%get-signed-long ,sap (/ ,offset 8)))
	    (64 `(%%get-signed-longlong ,sap (/ ,offset 8))))
	  (case (foreign-integer-type-bits type)
            (1 `(%get-bit ,sap ,offset))
	    (8 `(%get-unsigned-byte ,sap (/ ,offset 8)))
	    (16 `(%get-unsigned-word ,sap (/ ,offset 8)))
	    (32 `(%get-unsigned-long ,sap (/ ,offset 8)))
	    (64 `(%%get-unsigned-longlong ,sap (/ ,offset 8)))
	    (t  `(%get-bitfield ,sap ,offset ,(foreign-integer-type-bits type)))))))
    (or ref-form
	(error "Cannot extract ~D bit integers."
	       (foreign-integer-type-bits type)))))



;;;; The BOOLEAN type.

(def-foreign-type-class (boolean :include integer :include-args (signed)))



(def-foreign-type-method (boolean :lisp-rep) (type)
  (declare (ignore type))
  `(member t nil))

(def-foreign-type-method (boolean :naturalize-gen) (type foreign)
  (declare (ignore type))
  `(not (zerop ,foreign)))

(def-foreign-type-method (boolean :deport-gen) (type value)
  (declare (ignore type))
  `(if ,value 1 0))


(def-foreign-type-method (boolean :unparse) (type)
  `(boolean ,(foreign-boolean-type-bits type)))


;;;; the FLOAT types.

(def-foreign-type-class (float)
  (type () :type symbol))

(def-foreign-type-method (float :unparse) (type)
  (foreign-float-type-type type))

(def-foreign-type-method (float :lisp-rep) (type)
  (foreign-float-type-type type))

(def-foreign-type-method (float :foreign-rep) (type)
  (foreign-float-type-type type))

(def-foreign-type-method (float :naturalize-gen) (type foreign)
  (declare (ignore type))
  foreign)

(def-foreign-type-method (float :deport-gen) (type value)
  (declare (ignore type))
  value)


(def-foreign-type-class (single-float :include (float (bits 32))
				    :include-args (type)))


(def-foreign-type-method (single-float :extract-gen) (type sap offset)
  (declare (ignore type))
  `(%get-single-float ,sap (/ ,offset 8)))


(def-foreign-type-class (double-float :include (float (bits 64))
				    :include-args (type)))


(def-foreign-type-method (double-float :extract-gen) (type sap offset)
  (declare (ignore type))
  `(%get-double-float ,sap (/ ,offset 8)))



;;;; The MACPTR type

(def-foreign-type-class (macptr))


(def-foreign-type-method (macptr :unparse) (type)
  (declare (ignore type))
  'macptr)

(def-foreign-type-method (macptr :lisp-rep) (type)
  (declare (ignore type))
  'macptr)

(def-foreign-type-method (macptr :foreign-rep) (type)
  (declare (ignore type))
  'macptr)

(def-foreign-type-method (macptr :naturalize-gen) (type foreign)
  (declare (ignore type))
  foreign)

(def-foreign-type-method (macptr :deport-gen) (type object)
  (declare (ignore type))
  object)

(def-foreign-type-method (macptr :extract-gen) (type sap offset)
  (declare (ignore type))
  `(%get-ptr ,sap (/ ,offset 8)))


;;;; the FOREIGN-VALUE type.

(def-foreign-type-class (foreign-value :include macptr))

(def-foreign-type-method (foreign-value :lisp-rep) (type)
  (declare (ignore type))
  nil)

(def-foreign-type-method (foreign-value :naturalize-gen) (type foreign)
  `(%macptr-foreign ,foreign ',type))

(def-foreign-type-method (foreign-value :deport-gen) (type value)
  (declare (ignore type))
  `(foreign-macptr ,value))



;;;; The POINTER type.

(def-foreign-type-class (pointer :include (foreign-value))
  (to *void-foreign-type* :type (or symbol foreign-type)))



(def-foreign-type-method (pointer :unparse) (type)
  (let ((to (foreign-pointer-type-to type)))
    `(:* ,(if to
	     (%unparse-foreign-type to)
	     :void))))

(def-foreign-type-method (pointer :type=) (type1 type2)
  (let ((to1 (foreign-pointer-type-to type1))
	(to2 (foreign-pointer-type-to type2)))
    (if to1
	(if to2
	    (foreign-type-= to1 to2)
	    nil)
	(null to2))))

(def-foreign-type-method (pointer :subtypep) (type1 type2)
  (and (foreign-pointer-type-p type2)
       (let ((to1 (foreign-pointer-type-to type1))
	     (to2 (foreign-pointer-type-to type2)))
	 (if to1
	     (if to2
		 (foreign-subtype-p to1 to2)
		 t)
	     (null to2)))))

(def-foreign-type-method (pointer :deport-gen) (type value)
  (values
   `(etypecase ,value
      (null
       (%int-to-ptr 0))
      (macptr
       ,value)
      ((foreign ,type)
       (foreign-sap ,value)))
   `(or null macptr (foreign ,type))))


;;;; The MEM-BLOCK type.


(def-foreign-type-class (mem-block :include foreign-value))

(def-foreign-type-method (mem-block :extract-gen) (type sap offset)
  (let* ((nbytes (%foreign-type-or-record-size type :bytes)))
    `(%composite-pointer-ref ,nbytes ,sap (/ ,offset 8))))

(def-foreign-type-method (mem-block :deposit-gen) (type sap offset value)
  (let ((bits (foreign-mem-block-type-bits type)))
    (unless bits
      (error "Cannot deposit foreigns of type ~S (unknown size)." type))
    `(%copy-macptr-to-macptr ,value 0 ,sap ,offset ',bits)))



;;;; The ARRAY type.

(def-foreign-type-class (array :include mem-block)
  (element-type () :type foreign-type)
  (dimensions () :type list))



(def-foreign-type-method (array :unparse) (type)
  `(array ,(%unparse-foreign-type (foreign-array-type-element-type type))
	  ,@(foreign-array-type-dimensions type)))

(def-foreign-type-method (array :type=) (type1 type2)
  (and (equal (foreign-array-type-dimensions type1)
	      (foreign-array-type-dimensions type2))
       (foreign-type-= (foreign-array-type-element-type type1)
                       (foreign-array-type-element-type type2))))

(def-foreign-type-method (array :subtypep) (type1 type2)
  (and (foreign-array-type-p type2)
       (let ((dim1 (foreign-array-type-dimensions type1))
	     (dim2 (foreign-array-type-dimensions type2)))
	 (and (= (length dim1) (length dim2))
	      (or (and dim2
		       (null (car dim2))
		       (equal (cdr dim1) (cdr dim2)))
		  (equal dim1 dim2))
	      (foreign-subtype-p (foreign-array-type-element-type type1)
			       (foreign-array-type-element-type type2))))))


;;;; The RECORD type.

(defstruct (foreign-record-field
	     (:print-object
	      (lambda (field stream)
		(print-unreadable-object (field stream :type t)
		  (funcall (formatter "~S ~S~@[ ~D@~D~]")
			   stream
			   (foreign-record-field-type field)
			   (foreign-record-field-name field)
			   (foreign-record-field-bits field)
                           (foreign-record-field-offset field))))))
  (name () :type symbol)
  (type () :type foreign-type)
  (bits nil :type (or unsigned-byte null))
  (offset 0 :type unsigned-byte))



(defmethod make-load-form ((f foreign-record-field) &optional env)
  (make-load-form-saving-slots f :environment env))

(def-foreign-type-class (record :include mem-block)
  (kind :struct :type (member :struct :union :transparent-union))
  (name nil :type (or symbol null))
  (fields nil :type list)
  ;; For, e.g., records defined with #pragma options align=mac68k
  ;; in effect.  When non-nil, this specifies the maximum alignment
  ;; of record fields and the overall alignment of the record.
  (alt-align nil :type (or unsigned-byte null)))

(defmethod make-load-form ((r foreign-record-type) &optional environment)
  (declare (ignore environment))
  `(parse-foreign-type ',(unparse-foreign-type r)))


(defun parse-foreign-record-type (kind name fields &optional (ftd *target-ftd*))
  (let* ((result (if name
                   (or
                    (ecase kind
                      (:struct (info-foreign-type-struct name ftd))
                      ((:union :transparent-union) (info-foreign-type-union name ftd)))
                    (case kind
                      (:struct (setf (info-foreign-type-struct name ftd)
                                     (make-foreign-record-type :name name :kind :struct)))
                      ((:union :transparent-union)
                       (setf (info-foreign-type-union name ftd)
                                     (make-foreign-record-type :name name :kind kind)))))
                   (make-foreign-record-type :kind kind))))
    (when fields
      (multiple-value-bind (parsed-fields alignment bits)
          (parse-field-list fields kind (foreign-record-type-alt-align result))
        (let* ((old-fields (foreign-record-type-fields result)))
          (setf (foreign-record-type-fields result) parsed-fields
                (foreign-record-type-alignment result) alignment
                (foreign-record-type-bits result) bits)
          (when old-fields
            (unless (record-fields-match old-fields parsed-fields 5)
              (warn "Redefining ~a ~s fields to be:~%~s~%were~%~s"
                    kind name parsed-fields old-fields))))))
    (if name
      (unless (eq (auxiliary-foreign-type kind name) result)
        (setf (auxiliary-foreign-type kind name) result)))
    result))

;;; PARSE-FOREIGN-RECORD-FIELDS -- internal
;;;
;;; Used by parse-foreign-type to parse the fields of struct and union
;;; types.  RESULT holds the record type we are paring the fields of,
;;; and FIELDS is the list of field specifications.
;;;
(defun parse-field-list (fields kind &optional alt-alignment)
  (collect ((parsed-fields))
    (let* ((total-bits 0)
           (overall-alignment 1)
           (first-field-p t)
           (attributes (ftd-attributes *target-ftd*))
           (poweropen-alignment (getf attributes :poweropen-alignment)))
          
      (dolist (field fields)
        (destructuring-bind (var type &optional bits) field
          (declare (ignore bits))
          (let* ((field-type (parse-foreign-type type))
                 (bits (ensure-foreign-type-bits field-type))
                 (natural-alignment (foreign-type-alignment field-type))
                 (alignment (if alt-alignment
                              (min natural-alignment alt-alignment)
                              (if poweropen-alignment
                                (if first-field-p
                                  (progn
                                    (setq first-field-p nil)
                                    natural-alignment)
                                  (min 32 natural-alignment))
                                natural-alignment)))
                 (parsed-field
                  (make-foreign-record-field :type field-type
                                             :name var)))
            (parsed-fields parsed-field)
            (when (null bits)
              (error "Unknown size: ~S"
                     (unparse-foreign-type field-type)))
            (when (null alignment)
              (error "Unknown alignment: ~S"
                     (unparse-foreign-type field-type)))
            (setf overall-alignment (max overall-alignment (if (< alignment 8) 32 alignment)))
            (ecase kind
              (:struct
               (let ((offset (align-offset total-bits alignment)))
                 (setf (foreign-record-field-offset parsed-field) offset)
                 (setf (foreign-record-field-bits parsed-field) bits)
                 (setf total-bits (+ offset bits))))
              ((:union :transparent-union)
               (setf total-bits (max total-bits bits)))))))
      (values (parsed-fields)
              (or alt-alignment overall-alignment)
              (align-offset total-bits (or alt-alignment overall-alignment))))))
            


(defun parse-foreign-record-fields (result fields)
  (declare (type foreign-record-type result)
	   (type list fields))
  (multiple-value-bind (parsed-fields alignment bits)
      (parse-field-list fields (foreign-record-type-kind result) (foreign-record-type-alt-align result))
    (setf (foreign-record-type-fields result) parsed-fields
          (foreign-record-type-alignment result) alignment
          (foreign-record-type-bits result) bits)))


(def-foreign-type-method (record :unparse) (type)
  `(,(case (foreign-record-type-kind type)
       (:struct :struct)
       (:union :union)
       (:transparent-union :transparent-union)
       (t '???))
    ,(foreign-record-type-name type)
    ,@(unless (member type *record-types-already-unparsed* :test #'eq)
	(push type *record-types-already-unparsed*)
	(mapcar #'(lambda (field)
		    `(,(foreign-record-field-name field)
		      ,(%unparse-foreign-type (foreign-record-field-type field))
		      ,@(if (foreign-record-field-bits field)
			    (list (foreign-record-field-bits field)))))
		(foreign-record-type-fields type)))))

;;; Test the record fields. The depth is limiting in case of cyclic
;;; pointers.
(defun record-fields-match (fields1 fields2 depth)
  (declare (type list fields1 fields2)
	   (type (mod 64) depth))
  (labels ((record-type-= (type1 type2 depth)
	     (and (eq (foreign-record-type-name type1)
		      (foreign-record-type-name type2))
		  (eq (foreign-record-type-kind type1)
		      (foreign-record-type-kind type2))
		  (= (length (foreign-record-type-fields type1))
		     (length (foreign-record-type-fields type2)))
		  (record-fields-match (foreign-record-type-fields type1)
				       (foreign-record-type-fields type2)
				       (1+ depth))))
	   (pointer-type-= (type1 type2 depth)
	     (let ((to1 (foreign-pointer-type-to type1))
		   (to2 (foreign-pointer-type-to type2)))
	       (if to1
		   (if to2
		    (or (> depth 10)
		       (type-= to1 to2 (1+ depth)))
		       nil)
		   (null to2))))
	   (type-= (type1 type2 depth)
	     (cond ((and (foreign-pointer-type-p type1)
			 (foreign-pointer-type-p type2))
		    (or (> depth 10)
			(pointer-type-= type1 type2 depth)))
		   ((and (foreign-record-type-p type1)
			 (foreign-record-type-p type2))
		    (record-type-= type1 type2 depth))
		   (t
		    (foreign-type-= type1 type2)))))
    (do ((fields1-rem fields1 (rest fields1-rem))
	 (fields2-rem fields2 (rest fields2-rem)))
	((or (eq fields1-rem fields2-rem)
	     (endp fields1-rem)
             (endp fields2-rem))
	 (eq fields1-rem fields2-rem))
      (let ((field1 (first fields1-rem))
	    (field2 (first fields2-rem)))
	(declare (type foreign-record-field field1 field2))
	(unless (and (eq (foreign-record-field-name field1)
			 (foreign-record-field-name field2))
		     (eql (foreign-record-field-bits field1)
			  (foreign-record-field-bits field2))
		     (eql (foreign-record-field-offset field1)
			  (foreign-record-field-offset field2))
		     (let ((field1 (foreign-record-field-type field1))
			   (field2 (foreign-record-field-type field2)))
		       (type-= field1 field2 (1+ depth))))
	  (return nil))))))

(def-foreign-type-method (record :type=) (type1 type2)
  (and (eq (foreign-record-type-name type1)
	   (foreign-record-type-name type2))
       (eq (foreign-record-type-kind type1)
	   (foreign-record-type-kind type2))
       (= (length (foreign-record-type-fields type1))
	  (length (foreign-record-type-fields type2)))
       (record-fields-match (foreign-record-type-fields type1)
			    (foreign-record-type-fields type2) 0)))


;;;; The FUNCTION and VALUES types.

(defvar *values-type-okay* nil)

(def-foreign-type-class (function :include mem-block)
  (result-type () :type foreign-type)
  (arg-types () :type list)
  (stub nil :type (or null function)))



(def-foreign-type-method (function :unparse) (type)
  `(function ,(%unparse-foreign-type (foreign-function-type-result-type type))
	     ,@(mapcar #'%unparse-foreign-type
		       (foreign-function-type-arg-types type))))

(def-foreign-type-method (function :type=) (type1 type2)
  (and (foreign-type-= (foreign-function-type-result-type type1)
		     (foreign-function-type-result-type type2))
       (= (length (foreign-function-type-arg-types type1))
	  (length (foreign-function-type-arg-types type2)))
       (every #'foreign-type-=
	      (foreign-function-type-arg-types type1)
	      (foreign-function-type-arg-types type2))))


(def-foreign-type-class (values)
  (values () :type list))



(def-foreign-type-method (values :unparse) (type)
  `(values ,@(mapcar #'%unparse-foreign-type
		     (foreign-values-type-values type))))

(def-foreign-type-method (values :type=) (type1 type2)
  (and (= (length (foreign-values-type-values type1))
	  (length (foreign-values-type-values type2)))
       (every #'foreign-type-=
	      (foreign-values-type-values type1)
	      (foreign-values-type-values type2))))




;;;; The FOREIGN-SIZE macro.

(defmacro foreign-size (type &optional (units :bits))
  "Return the size of the foreign type TYPE.  UNITS specifies the units to
   use and can be either :BITS, :BYTES, or :WORDS."
  (let* ((foreign-type (parse-foreign-type type))
         (bits (ensure-foreign-type-bits foreign-type)))
    (if bits
      (values (ceiling bits
                       (ecase units
                         (:bits 1)
                         (:bytes 8)
                         (:words 32))))
      (error "Unknown size for foreign type ~S."
             (unparse-foreign-type foreign-type)))))

(defun ensure-foreign-type-bits (type)
  (or (foreign-type-bits type)
      (and (typep type 'foreign-record-type)
           (let* ((name (foreign-record-type-name type)))
             (and name
                  (load-record name)
                  (foreign-type-bits type))))
      (and (typep type 'foreign-array-type)
	   (let* ((element-type (foreign-array-type-element-type type))
		  (dims (foreign-array-type-dimensions type)))
	     (if (and (ensure-foreign-type-bits element-type)
		      (every #'integerp dims))
	       (setf (foreign-array-type-alignment type)
		     (foreign-type-alignment element-type)
		     (foreign-array-type-bits type)
		     (* (align-offset (foreign-type-bits element-type)
				      (foreign-type-alignment element-type))
			(reduce #'* dims))))))))

(defun require-foreign-type-bits (type)
  (or (ensure-foreign-type-bits type)
      (error "Can't determine attributes of foreign type ~s" type)))

(defun %find-foreign-record (name)
  (or (info-foreign-type-struct name)
      (info-foreign-type-union name)
      (load-record name)))


(defun %foreign-type-or-record (type)
  (if (typep type 'foreign-type)
    type
    (if (consp type)
      (parse-foreign-type type)
      (or (%find-foreign-record type)
	  (parse-foreign-type type)))))

(defun %foreign-type-or-record-size (type &optional (units :bits))
  (let* ((info (%foreign-type-or-record type))
         (bits (ensure-foreign-type-bits info)))
    (if bits
      (values (ceiling bits
                       (ecase units
                         (:bits 1)
                         (:bytes 8)
                         (:words 32))))
      (error "Unknown size for foreign type ~S."
             (unparse-foreign-type info)))))

(defun %find-foreign-record-type-field (type field-name)
  (ensure-foreign-type-bits type)       ;load the record type if necessary.
  (let* ((fields (foreign-record-type-fields type)))
    (or (find field-name  fields :key #'foreign-record-field-name :test #'string-equal)
                         (error "Record type ~a has no field named ~s.~&Valid field names are: ~&~a"
                                (foreign-record-type-name type)
                                field-name
                                (mapcar #'foreign-record-field-name fields)))))

(defun %foreign-access-form (base-form type bit-offset accessors)
  (if (null accessors)
    (invoke-foreign-type-method :extract-gen type base-form bit-offset)
    (etypecase type
      (foreign-record-type
       (let* ((field (%find-foreign-record-type-field type (car accessors))))
         (%foreign-access-form base-form
                               (foreign-record-field-type field)
                               (+ bit-offset (foreign-record-field-offset field))
                               (cdr accessors))))
      (foreign-pointer-type
       (%foreign-access-form
        (invoke-foreign-type-method :extract-gen type base-form bit-offset)
        (foreign-pointer-type-to type)
        0
        accessors)))))

(defun %foreign-array-access-form (base-form type index-form)
  (etypecase type
    ((or foreign-pointer-type foreign-array-type)
     (let* ((to (foreign-pointer-type-to type))
            (size (foreign-type-bits to))
            (bit-offset `(the fixnum (* ,size (the fixnum ,index-form)))))
       (invoke-foreign-type-method :extract-gen to base-form bit-offset)))))




;;;; Naturalize, deport, extract-foreign-value, deposit-foreign-value

(defun naturalize (foreign type)
  (declare (type foreign-type type))
  (funcall (coerce (compute-naturalize-lambda type) 'function)
           foreign type))

(defun deport (value type)
  (declare (type foreign-type type))
  (funcall (coerce (compute-deport-lambda type) 'function)
           value type))

(defun extract-foreign-value (sap offset type)
  (declare (type macptr sap)
           (type unsigned-byte offset)
           (type foreign-type type))
  (funcall (coerce (compute-extract-lambda type) 'function)
           sap offset type))

(defun deposit-foreign-value (sap offset type value)
  (declare (type macptr sap)
           (type unsigned-byte offset)
           (type foreign-type type))
  (funcall (coerce (compute-deposit-lambda type) 'function)
           sap offset type value))



(defmacro external (name)
  "If there is already an EXTERNAL-ENTRY-POINT for the symbol named by name,
find it and return it. If not, create one and return it.

Try to resolve the entry point to a memory address, and identify the
containing library.

Be aware that under Darwin, external functions which are callable from C
have underscores prepended to their names, as in '_fopen'."
  `(load-eep ,name))

(defmacro external-call (name &rest args)
  "Call the foreign function at the address obtained by resolving the
external-entry-point associated with name, passing the values of each arg
as a foreign argument of type indicated by the corresponding
arg-type-specifier. Returns the foreign function result (coerced to a
Lisp object of type indicated by result-type-specifier), or NIL if
result-type-specifer is :VOID or NIL"
  `(ff-call (%reference-external-entry-point
	     (load-time-value (external ,name))) ,@args))

(defmacro ff-call (entry &rest args)
  "Call the foreign function at address entrypoint passing the values of
each arg as a foreign argument of type indicated by the corresponding
arg-type-specifier. Returns the foreign function result (coerced to a
Lisp object of type indicated by result-type-specifier), or NIL if
result-type-specifer is :VOID or NIL"
  (funcall (ftd-ff-call-expand-function *target-ftd*)
           `(%ff-call ,entry) args))
	
	  

(defmethod make-load-form ((eep external-entry-point) &optional env)
  (declare (ignore env))
  `(load-eep ,(eep.name eep)))


(defmethod print-object ((eep external-entry-point) out)
  (print-unreadable-object (eep out :type t :identity t)
    (format out "~s" (eep.name eep))
    (let* ((addr (eep.address eep))
	   (container (eep.container eep)))
      (if addr
        #+ppc-target
        (progn
          #+32-bit-target
          (format out " (#x~8,'0x) " (logand #xffffffff (ash addr 2)))
          #+64-bit-target
          (format out " (#x~16,'0x) " (if (typep addr 'integer)
                                        (logand #xffffffffffffffff (ash addr 2))
                                        (%ptr-to-int addr))))
	#+x8632-target
	(format out " (#x~8,'0x) " addr)
        #+x8664-target
        (format out " (#x~16,'0x) " addr)
	(format out " {unresolved} "))
      (when (and container (or (not (typep container 'macptr))
				    (not (%null-ptr-p container))))
	(format out "~a" (shlib.soname container))))))



(defun %cons-foreign-variable (name type &optional container)
  (%istruct 'foreign-variable nil name type container))

(defmethod make-load-form ((fv foreign-variable) &optional env)
  (declare (ignore env))
  `(load-fv ,(fv.name fv) ',(fv.type fv)))

(defmethod print-object ((fv foreign-variable) out)
  (print-unreadable-object (fv out :type t :identity t)
    (format out "~s" (fv.name fv))
    (let* ((addr (fv.addr fv))
	   (container (fv.container fv)))
      (if addr
        #+32-bit-target
	(format out " (#x~8,'0x) " (logand #xffffffff (%ptr-to-int addr)))
        #+64-bit-target
        	(format out " (#x~16,'0x) " (logand #xfffffffffffffffff (%ptr-to-int addr)))
	(format out " {unresolved} "))
      (when (and container (or (not (typep container 'macptr))
				    (not (%null-ptr-p container))))
	(format out "~a" (shlib.soname container))))))


(defmethod print-object ((s shlib) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (format stream "~a" (or (shlib.soname s) (shlib.pathname s)))))

#-(or darwin-target windows-target)
(defun dlerror ()
  (with-macptrs ((p))
    (%setf-macptr p (#_dlerror))
    (unless (%null-ptr-p p) (%get-cstring p))))

(defstruct (external-function-definition (:conc-name "EFD-")
                                         (:constructor
                                          make-external-function-definition
                                          (&key entry-name arg-specs
                                                result-spec
                                                (min-args (length arg-specs))))
                                         )
  (entry-name "" :type string)
  (arg-specs () :type list)
  (result-spec nil :type (or symbol list))
  (min-args 0 :type fixnum))


(defun %external-call-expander (whole env)
  (declare (ignore env))
  (destructuring-bind (name &rest args) whole
    (collect ((call))
      (let* ((info (or (gethash name (ftd-external-function-definitions
                                      *target-ftd*))
                       (error "Unknown external-function: ~s" name)))
             (external-name (efd-entry-name info))
             (arg-specs (efd-arg-specs info))
             (result (efd-result-spec info))
             (monitor (eq (car args) :monitor-exception-ports)))
        (when monitor
          (setq args (cdr args))
          (call :monitor-exception-ports))
        (let* ((rtype (parse-foreign-type result)))
          (if (typep rtype 'foreign-record-type)
            (call (pop args))))
        (do* ((specs arg-specs (cdr specs))
              (args args (cdr args)))
             ((null specs)
              (call result)
              (if args
                (error "Extra arguments in ~s"  whole)
                `(external-call ,external-name ,@(call))))
          (let* ((spec (car specs)))
            (cond ((eq spec :void)
                   ;; must be last arg-spec; remaining args should be
                   ;; keyword/value pairs
                   (unless (evenp (length args))
                     (error "Remaining arguments should be keyword/value pairs: ~s"
                            args))
                   (do* ()
                        ((null args))
                     (call (pop args))
                     (call (pop args))))
                  (t
                   (call spec)
                   (if args
                     (call (car args))
                     (error "Missing arguments in ~s" whole))))))))))

(defun translate-foreign-arg-type (foreign-type-spec)
  (let* ((foreign-type (parse-foreign-type foreign-type-spec)))
    (etypecase foreign-type
      (foreign-pointer-type :address)
      (foreign-integer-type
       (let* ((bits (foreign-integer-type-bits foreign-type))
              (signed (foreign-integer-type-signed foreign-type)))
         (declare (fixnum bits))
         (cond ((<= bits 8) (if signed :signed-byte :unsigned-byte))
               ((<= bits 16) (if signed :signed-halfword :unsigned-halfword))
               ((<= bits 32) (if signed :signed-fullword :unsigned-fullword))
               ((<= bits 64) (if signed :signed-doubleword :unsigned-doubleword))
               (t `(:record ,bits)))))
      (foreign-float-type
       (ecase (foreign-float-type-bits foreign-type)
         (32 :single-float)
         (64 :double-float)))
      (foreign-record-type
       `(:record ,(foreign-record-type-bits foreign-type))))))
      

(defmacro define-external-function (name (&rest arg-specs) result-spec
					 &key (min-args (length arg-specs)))
  (let* ((entry-name nil)
         (package (find-package (ftd-interface-package-name *target-ftd*)))
         (arg-keywords (mapcar #'translate-foreign-arg-type arg-specs))
         (result-keyword (unless (and (symbolp result-spec)
                                    (eq (make-keyword result-spec) :void))
                               (translate-foreign-arg-type result-spec))))
    (when (and (consp result-keyword) (eq (car result-keyword) :record))
      (push :address arg-keywords)
      (setq result-keyword nil))
    (if (consp name)
      (setq entry-name (cadr name) name (intern (unescape-foreign-name
                                                 (car name))
                                                package))
      (progn
        (setq entry-name (unescape-foreign-name name)
              name (intern entry-name package))
        (if (getf (ftd-attributes *target-ftd*)
                  :prepend-underscore)
          (setq entry-name (concatenate 'string "_" entry-name)))))
    `(progn
      (setf (gethash ',name (ftd-external-function-definitions *target-ftd*))
       (make-external-function-definition
	:entry-name ',entry-name
	:arg-specs ',arg-keywords
	:result-spec ',result-keyword
	:min-args ,min-args))
      (setf (macro-function ',name) #'%external-call-expander)
      ',name)))


#+darwinppc-target
(defun open-dylib (name)
  (with-cstrs ((name name))
    (#_NSAddImage name (logior #$NSADDIMAGE_OPTION_RETURN_ON_ERROR 
			       #$NSADDIMAGE_OPTION_WITH_SEARCHING))))

(defparameter *foreign-representation-type-keywords*
  `(:signed-doubleword :signed-fullword :signed-halfword :signed-byte
    :unsigned-doubleword :unsigned-fullword :unsigned-halfword :unsigned-byte
    :address
    :single-float :double-float
    :void))

(defun null-coerce-foreign-arg (arg-type-keyword argform)
  (declare (ignore arg-type-keyword))
  argform)

(defun null-coerce-foreign-result (result-type-keyword resultform)
  (declare (ignore result-type-keyword))
  resultform)

(defun foreign-type-to-representation-type (f)
  (if (or (member f *foreign-representation-type-keywords*)
	  (typep f 'unsigned-byte))
    f
    (let* ((ftype (if (typep f 'foreign-type)
                    f
                    (parse-foreign-type f))))
      (or
       (and (eq (foreign-type-class ftype) 'root) :void)	 
       (typecase ftype
	 ((or foreign-pointer-type foreign-array-type) :address)
	 (foreign-double-float-type :double-float)
	 (foreign-single-float-type :single-float)
	 (foreign-integer-type
	  (let* ((signed (foreign-integer-type-signed ftype))
		 (bits (foreign-integer-type-bits ftype)))
	    (if signed
	      (if (<= bits 8)
		:signed-byte
		(if (<= bits 16)
		  :signed-halfword
		  (if (<= bits 32)
		    :signed-fullword
		    (if (<= bits 64)
		      :signed-doubleword))))
	      (if (<= bits 8)
		:unsigned-byte
		(if (<= bits 16)
		  :unsigned-halfword
		  (if (<= bits 32)
		    :unsigned-fullword
		    (if (<= bits 64)
		      :unsigned-doubleword)))))))
	 (foreign-record-type
          (if (getf (ftd-attributes *target-ftd*)
                  :struct-by-value)
            (let* ((bits (ensure-foreign-type-bits ftype)))
              (ceiling bits (target-word-size-case
                             (32 32)
                             (64 64))))
          :address)))
       (error "can't determine representation keyword for ~s" f)))))

(defun foreign-record-accessor-names (record-type &optional prefix)
  (collect ((accessors))
    (dolist (field (foreign-record-type-fields record-type) (accessors))
      (let* ((field-name (append prefix (list (foreign-record-field-name field))))
	     (field-type (foreign-record-field-type field)))
	(if (typep field-type 'foreign-record-type)
	  (dolist (s (foreign-record-accessor-names field-type field-name))
	    (accessors s))
	  (accessors field-name))))))

;;; Are all (scalar) fields in the field-list FIELDS floats ?'
(defun all-floats-in-field-list (fields)
  (dolist (field fields t)
    (let* ((field-type (foreign-record-field-type field)))
      (cond ((typep field-type 'foreign-record-type)
             (unless (all-floats-in-field-list (foreign-record-type-fields field-type))
                                     (return nil)))
            ((typep field-type 'foreign-array-type)
             (unless (typep (foreign-array-type-element-type field-type) 'foreign-float-type)
               (return nil)))
            (t (unless (typep field-type 'foreign-float-type)
                 (return nil)))))))

;;; Are any (scalar) fields in the field-list FIELDS floats ?
(defun some-floats-in-field-list (fields)
  (dolist (field fields)
    (let* ((field-type (foreign-record-field-type field)))
      (cond ((typep field-type 'foreign-float-type)
             (return t))
            ((typep field-type 'foreign-record-type)
             (if (some-floats-in-field-list (foreign-record-type-fields field-type))
               (return t)))
            ((typep field-type 'foreign-array-type)
             (if (typep (foreign-array-type-element-type field-type)
                        'foreign-float-type)
               (return t)))))))

;;; We don't use foreign type ordinals when cross-compiling,
;;; so the read-time conditionalization is OK here.

#-windows-target
(defparameter *canonical-os-foreign-types*
  '((:struct :timespec)
    (:struct :stat)
    (:struct :passwd)
    #>Dl_info
    (:array (:struct :pollfd) 1)) )

#+windows-target
(defparameter *canonical-os-foreign-types*
  `(#>FILETIME
    #>SYSTEM_INFO
    #>HANDLE
    #>PROCESS_INFORMATION
    #>STARTUPINFO
    (:array #>HANDLE 2)
    #>DWORD
    (:array #>wchar_t #.#$MAX_PATH)
    #>fd_set
    #>DWORD_PTR))
    
    
(defun canonicalize-foreign-type-ordinals (ftd)
  (let* ((canonical-ordinal 0))          ; used for :VOID
    (flet ((canonicalize-foreign-type-ordinal (spec)
             (let* ((new-ordinal (incf canonical-ordinal)))
               (when spec
                 (let* ((type (parse-foreign-type spec))
                        (old-ordinal (foreign-type-ordinal type)))
                   (unless (eql new-ordinal old-ordinal)
                     (remhash old-ordinal (ftd-ordinal-types ftd))
                     (setf (foreign-type-ordinal type) new-ordinal)
                     (note-foreign-type-ordinal type ftd))))
               new-ordinal)))
      (canonicalize-foreign-type-ordinal :signed)
      (canonicalize-foreign-type-ordinal :unsigned)
      (canonicalize-foreign-type-ordinal #+64-bit-target :long #-64-bit-target nil)
      (canonicalize-foreign-type-ordinal :address)
      (canonicalize-foreign-type-ordinal '(:struct :sockaddr_in))
      (canonicalize-foreign-type-ordinal '(:struct :sockaddr_un))
      (canonicalize-foreign-type-ordinal '(:struct :linger))
      (canonicalize-foreign-type-ordinal '(:struct :hostent))
      (canonicalize-foreign-type-ordinal '(:array :unsigned-long 3))
      (canonicalize-foreign-type-ordinal '(:* :char))
      (canonicalize-foreign-type-ordinal '(:struct :in_addr))
      (canonicalize-foreign-type-ordinal '(:struct :cdb-datum))
      (canonicalize-foreign-type-ordinal '(:struct :dbm-constant))
      (canonicalize-foreign-type-ordinal '(:* (:struct :hostent)))
      (canonicalize-foreign-type-ordinal '(:array :int 2))
      (canonicalize-foreign-type-ordinal '(:array (:struct :pollfd) 1))
      (canonicalize-foreign-type-ordinal '(:struct :dirent))
      (canonicalize-foreign-type-ordinal '(:struct :timeval))
      (canonicalize-foreign-type-ordinal '(:struct :addrinfo))

      (setq canonical-ordinal (1- max-common-foreign-type-ordinal))

      (dolist (spec *canonical-os-foreign-types*)
        (canonicalize-foreign-type-ordinal spec))
      (dolist (spec (ftd-platform-ordinal-types ftd))
        (canonicalize-foreign-type-ordinal spec)))))

(defun install-standard-foreign-types (ftd)
  (let* ((*target-ftd* ftd)
         (natural-word-size (getf (ftd-attributes ftd) :bits-per-word))
         (long-word-size (or (getf (ftd-attributes ftd) :bits-per-long)
                             natural-word-size)))

    (def-foreign-type-translator signed (&optional (bits 32))
      (if (<= bits 64)
        (svref *signed-integer-types* bits)
        (make-foreign-integer-type :bits bits)))


    (def-foreign-type-translator integer (&optional (bits 32))
      (if (<= bits 64)
        (svref *signed-integer-types* bits)
        (make-foreign-integer-type :bits bits)))

    (def-foreign-type-translator unsigned (&optional (bits 32))
      (if (<= bits 64)
        (svref *unsigned-integer-types* bits)
        (make-foreign-integer-type :bits bits :signed nil)))

    (def-foreign-type-translator bitfield (&optional (bits 1))
      (make-foreign-integer-type :bits bits :signed nil :alignment 1))

    (def-foreign-type-translator root ()
      (make-foreign-type :class 'root :bits 0 :alignment 0))

    (def-foreign-type-translator :<BOOL> () *bool-type*)

    (def-foreign-type-translator single-float ()
      (make-foreign-single-float-type :type 'single-float))

    (def-foreign-type-translator double-float ()
      (make-foreign-double-float-type :type 'double-float))

    (def-foreign-type-translator macptr ()
      (make-foreign-macptr-type :bits natural-word-size))

    (def-foreign-type-translator values (&rest values)
      (unless *values-type-okay*
        (error "Cannot use values types here."))
      (let ((*values-type-okay* nil))
        (make-foreign-values-type
         :values (mapcar #'parse-foreign-type values))))

    (def-foreign-type-translator function (result-type &rest arg-types)
      (make-foreign-function-type
       :result-type (let ((*values-type-okay* t))
                      (parse-foreign-type result-type))
       :arg-types (mapcar #'parse-foreign-type arg-types)))

    (def-foreign-type-translator struct (name &rest fields)
      (parse-foreign-record-type :struct name fields))
    
    (def-foreign-type-translator union (name &rest fields)
      (parse-foreign-record-type :union name fields))

    (def-foreign-type-translator transparent-union (name &rest fields)
      (parse-foreign-record-type :transparent-union name fields))

    (def-foreign-type-translator array (ele-type &rest dims)
      (when dims
	;; cross-compiling kludge. replaces '(or index null)
        (unless (typep (first dims) `(or
				      ,(target-word-size-case
					(32 '(integer 0 #.(expt 2 24)))
					(64 '(integer 0 #.(expt 2 56))))
				      null))
          (error "First dimension is not a non-negative fixnum or NIL: ~S"
                 (first dims)))
        (let ((loser (find-if-not #'(lambda (x) (typep x 'index))
                                  (rest dims))))
          (when loser
            (error "Dimension is not a non-negative fixnum: ~S" loser))))
	
      (let* ((type (parse-foreign-type ele-type))
             (pair (cons type dims)))
        (declare (dynamic-extent pair))
        (ensure-foreign-type-bits type)
        (or (gethash pair (ftd-array-types *target-ftd*))
            (setf (gethash (cons type dims) (ftd-array-types *target-ftd*))
                  
                  (make-foreign-array-type
                   :element-type type
                   :dimensions dims
                   :alignment (foreign-type-alignment type)
                   :bits (if (and (ensure-foreign-type-bits type)
                                  (every #'integerp dims))
                           (* (align-offset (foreign-type-bits type)
                                            (foreign-type-alignment type))
                              (reduce #'* dims))))))))

    (def-foreign-type-translator * (to)
      (let* ((ftd *target-ftd*)
             (to (if (eq to t) *void-foreign-type* (parse-foreign-type to ftd))))
        (or (gethash to (ftd-pointer-types ftd))
            (setf (gethash to (ftd-pointer-types *target-ftd*))
                  (make-foreign-pointer-type
                   :to to
                   :bits natural-word-size)))))
    
    (def-foreign-type-translator boolean (&optional (bits 32))
      (make-foreign-boolean-type :bits bits :signed nil))

    (def-foreign-type signed-char (signed 8))
    (def-foreign-type signed-byte (signed 8))
    (def-foreign-type short (signed 16))
    (def-foreign-type signed-halfword short)
    (def-foreign-type int (signed 32))
    (def-foreign-type signed-fullword int)
    (def-foreign-type signed-short (signed 16))
    (def-foreign-type signed-int (signed 32))
    (def-foreign-type signed-doubleword (signed 64))
    (def-foreign-type char #-darwin-target (unsigned 8)
                      #+darwin-target (signed 8))
    (def-foreign-type unsigned-char (unsigned 8))
    (def-foreign-type unsigned-byte (unsigned 8))
    (def-foreign-type unsigned-short (unsigned 16))
    (def-foreign-type unsigned-halfword unsigned-short)
    (def-foreign-type unsigned-int (unsigned 32))
    (def-foreign-type unsigned-fullword unsigned-int)
    (def-foreign-type unsigned-doubleword (unsigned 64))
    (def-foreign-type bit (bitfield 1))

    (def-foreign-type float single-float)
    (def-foreign-type double double-float)

    (%def-foreign-type :void *void-foreign-type*)
    (def-foreign-type address (* :void))
    (let* ((signed-long-type (parse-foreign-type
                              `(:signed ,long-word-size)))
           (unsigned-long-type (parse-foreign-type
                                `(:unsigned ,long-word-size))))
      (%def-foreign-type :long signed-long-type ftd)
      (%def-foreign-type :signed-long signed-long-type ftd)
      (%def-foreign-type :unsigned-long unsigned-long-type ftd))
    ;;
    ;; Defining the handful of foreign structures that are used
    ;; to build OpenMCL here ensures that all backends see appropriate
    ;; definitions of them.
    ;;
    ;; Don't use DEF-FOREIGN-TYPE here; this often runs too
    ;; early in the cold load for that to work.
    ;;
    (parse-foreign-type
     '(:struct :cdb-datum
       (:data (* t))
       (:size (:unsigned 32)))
     ftd)
    (parse-foreign-type
     '(:struct :dbm-constant
       (:class (:unsigned 32))
       (:pad (:unsigned 32))
       (:value
        (:union nil
         (:s32 (:signed 32))
         (:u32 (:unsigned 32))
         (:single-float :float)
         (:double-float :double))))
     ftd)
    ;; This matches the xframe-list struct definition in
    ;; "ccl:lisp-kernel;constants.h"
    (parse-foreign-type
     '(:struct :xframe-list
       (:this (:* t #|(struct :ucontext)|#))
       (:prev (:* (:struct  :xframe-list))))
    ftd)
  ))

(defmethod make-load-form ((p macptr) &optional env)
  (declare (ignore env))
  (let* ((value (%ptr-to-int p)))
    (unless (or (< value 65536)
                (>= value (- (ash 1 target::nbits-in-word) 65536)))
      (error "~&~s can't be referenced as a constant because its address contains more than 16 significant bits." p))
    (if (zerop value)
      '+null-ptr+
      `(%int-to-ptr ,value))))




