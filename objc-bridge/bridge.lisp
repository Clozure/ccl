;;;; -*- Mode: Lisp; Package: CCL -*-
;;;; bridge.lisp
;;;;
;;;; A Lisp bridge for Cocoa
;;;;
;;;; This provides:
;;;;   (1) Convenient Lisp syntax for instantiating ObjC classes
;;;;   (2) Convenient Lisp syntax for invoking ObjC methods
;;;;
;;;; Copyright (c) 2003 Randall D. Beer
;;;; 
;;;; This software is licensed under the terms of the Lisp Lesser GNU Public
;;;; License, known as the LLGPL.  The LLGPL consists of a preamble and 
;;;; the LGPL. Where these conflict, the preamble takes precedence.  The 
;;;; LLGPL is available online at http://opensource.franz.com/preamble.html.
;;;;
;;;; Please send comments and bug reports to <beer@eecs.cwru.edu>

;;; Temporary package and module stuff 

(in-package "CCL")

(require "OBJC-RUNTIME")
(require "NAME-TRANSLATION")

;;; Used in PRINT-OBJECT methods.

(defun describe-macptr-allocation-and-address (p stream)
  (format stream " ~@[~a ~](#x~x)"
          (%macptr-allocation-string p)
          (%ptr-to-int p)))

(defstruct typed-foreign-struct-info
  foreign-type
  lisp-class-name
  initializer
  constructor
  with-form-name
  predicate-name)

(defparameter *typed-foreign-struct-info* ())

(defun note-typed-foreign-struct-info (foreign-type lisp-class-name initializer constructor with-form-name predicate-name)
  (let* ((info (find foreign-type *typed-foreign-struct-info* :test #'equal :key #'typed-foreign-struct-info-foreign-type)))
    (unless info
      (setq info (make-typed-foreign-struct-info :foreign-type foreign-type))
      (push info *typed-foreign-struct-info*))
    (setf (typed-foreign-struct-info-lisp-class-name info) lisp-class-name
          (typed-foreign-struct-info-initializer info) initializer
          (typed-foreign-struct-info-constructor info) constructor
          (typed-foreign-struct-info-with-form-name info) with-form-name
          (typed-foreign-struct-info-predicate-name info) predicate-name)
    info))
  
;;; This gets installed as the COMPILER-MACRO-FUNCTION on any dispatch
;;; function associated with a method that passes structures by value.
(defun hoist-struct-constructors (whole env)
  (declare (ignorable env))
  (destructuring-bind (operator receiver &rest args) whole
    ;;See if any arguments are "obviously" known structure-creation forms.
    (if (null (dolist (arg args)
                (if (and (consp arg)
                         (find (car arg) *typed-foreign-struct-info* :key #'typed-foreign-struct-info-constructor))
                  (return t))))
      whole
      ;;; Simplest to hoist one call, then let compiler-macroexpand
      ;;; call us again.
      (let* ((with-name nil)
             (info nil)
             (temp (gensym)))
        (collect ((new-args))
          (new-args operator)
          (new-args receiver)
          (dolist (arg args)
            (if (or info
                    (atom arg)
                    (not (setq info (find (car arg) *typed-foreign-struct-info* :key #'typed-foreign-struct-info-constructor))))
              (new-args arg)
              (progn
                (setq with-name (typed-foreign-struct-info-with-form-name info))
                (if (cdr arg)
                  (new-args `(progn (,(typed-foreign-struct-info-initializer info)
                                     ,temp
                                     ,@(cdr arg))
                              ,temp))
                  (new-args temp)))))
          `(,with-name (,temp)
            (values ,(new-args))))))))
          
        
      
(defun define-typed-foreign-struct-accessor (type-name lisp-accessor-name foreign-accessor &optional (transform-output #'identity) (transform-input #'identity))
  (let* ((arg (gensym))
         (val (gensym)))
    `(progn
      (declaim (inline ,lisp-accessor-name))
      (defun ,lisp-accessor-name (,arg)
        (if (typep ,arg ',type-name)
          ,(funcall transform-input `(pref ,arg ,foreign-accessor))
          (report-bad-arg ,arg ',type-name)))
      (declaim (inline (setf ,lisp-accessor-name)))
      (defun (setf ,lisp-accessor-name) (,val ,arg)
        (if (typep ,arg ',type-name)
          (setf (pref ,arg ,foreign-accessor) ,(funcall transform-output val))
          (report-bad-arg ,arg ',type-name))))))

(defun define-typed-foreign-struct-accessors (type-name tuples)
  (collect ((body))
    (dolist (tuple tuples `(progn ,@(body)))
      (body (apply #'define-typed-foreign-struct-accessor type-name (cdr tuple))))))

(defun define-typed-foreign-struct-initializer (init-function-name  tuples)
  (when init-function-name
    (let* ((struct (gensym)))
      (collect ((initforms)
                (args))
        (args struct)
        (dolist (tuple tuples)
          (destructuring-bind (arg-name lisp-accessor foreign-accessor &optional (transform #'identity)) tuple
            (declare (ignore lisp-accessor))
            (args arg-name)
            (initforms `(setf (pref ,struct ,foreign-accessor) ,(funcall transform arg-name)))))
        `(progn
          (declaim (inline ,init-function-name))
          (defun ,init-function-name ,(args)
            (declare (ignorable ,struct))
            ,@(initforms)
            ,struct))))))

(defun define-typed-foreign-struct-creation-function (creation-function-name init-function-name foreign-type accessors)
  (when creation-function-name
    (let* ((struct (gensym))
           (arg-names (mapcar #'car accessors)))
      `(defun ,creation-function-name ,arg-names
        (let* ((,struct (make-gcable-record ,foreign-type)))
          (,init-function-name ,struct ,@arg-names)
          ,struct)))))

(defun define-typed-foreign-struct-class-with-form (with-form-name foreign-type init-function-name)
  (declare (ignorable init-function-name))
  (when with-form-name
  `(defmacro ,with-form-name ((instance &rest inits) &body body)
    (multiple-value-bind (body decls) (parse-body body nil)
      `(rlet ((,instance ,,foreign-type))
        ,@decls
        ,@(when inits
                `((,',init-function-name ,instance ,@inits)))
        ,@body)))))
         

(defmacro define-typed-foreign-struct-class (class-name (foreign-type predicate-name init-function-name creation-function-name with-form-name) &rest accessors)
  (let* ((arg (gensym)))
    `(progn
      (%register-type-ordinal-class (parse-foreign-type ',foreign-type) ',class-name)
      (def-foreign-type ,class-name  ,foreign-type)
      (declaim (inline ,predicate-name))
      (note-typed-foreign-struct-info ',foreign-type ',class-name ',init-function-name ',creation-function-name ',with-form-name ',predicate-name)
      (defun ,predicate-name (,arg)
        (and (typep ,arg 'macptr)
             (<= (the fixnum (%macptr-domain ,arg)) 1)
             (= (the fixnum (%macptr-type ,arg))
                (foreign-type-ordinal (load-time-value (parse-foreign-type ',foreign-type))))))
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (setf (type-predicate ',class-name) ',predicate-name))
      ,(define-typed-foreign-struct-initializer init-function-name accessors)
      ,(define-typed-foreign-struct-creation-function creation-function-name init-function-name foreign-type accessors)
      ,(define-typed-foreign-struct-class-with-form with-form-name foreign-type init-function-name)
      ,(define-typed-foreign-struct-accessors class-name accessors)
      ',class-name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun wrap-cg-float (x)
    `(float ,x +cgfloat-zero+)))



;;; AEDesc (Apple Event Descriptor)

(define-typed-foreign-struct-class ns::aedesc (:<AED>esc ns::aedesc-p ns::init-aedesc ns::make-aedesc ns::with-aedesc)
  (descriptor-type ns::aedesc-descriptor-type :<AED>esc.descriptor<T>ype)
  (data-handle ns::aedesc-data-handle :<AED>esc.data<H>andle))


(defmethod print-object ((a ns::aedesc) stream)
  (print-unreadable-object (a stream :type t :identity (%gcable-ptr-p a))
    (unless (%null-ptr-p a)
      (format stream "~s ~s"
              (ns::aedesc-descriptor-type a)
              (ns::aedesc-data-handle a)))
    (describe-macptr-allocation-and-address a stream)))

;;; It's not clear how useful this would be; I think that it's
;;; part of the ObjC 2.0 extensible iteration stuff ("foreach").
#+apple-objc-2.0
(define-typed-foreign-struct-class ns::ns-fast-enumeration-state (:<NSF>ast<E>numeration<S>tate ns::ns-fast-enumeration-state-p ns::init-ns-fast-enumeration-state ns::make-ns-fast-enumeration-state ns::with-ns-fast-enumeration-state))

;;; NSAffineTransformStruct CGAffineTransform
(define-typed-foreign-struct-class ns::ns-affine-transform-struct (:<NSA>ffine<T>ransform<S>truct ns::ns-affine-transform-struct-p ns::init-ns-affine-transform-struct ns::make-ns-affine-transform-struct ns::wint-ns-affine-transform-struct)
    (m11 ns::ns-affine-transform-struct-m11 :<NSA>ffine<T>ransform<S>truct.m11 wrap-cg-float)
    (m12 ns::ns-affine-transform-struct-m12 :<NSA>ffine<T>ransform<S>truct.m12 wrap-cg-float)
    (m21 ns::ns-affine-transform-struct-m21 :<NSA>ffine<T>ransform<S>truct.m21 wrap-cg-float)
    (m22 ns::ns-affine-transform-struct-m22 :<NSA>ffine<T>ransform<S>truct.m22 wrap-cg-float)
    (tx ns::ns-affine-transform-struct-tx :<NSA>ffine<T>ransform<S>truct.t<X> wrap-cg-float)
    (ty ns::ns-affine-transform-struct-ty :<NSA>ffine<T>ransform<S>truct.t<Y> wrap-cg-float))


(defmethod print-object ((transform ns::ns-affine-transform-struct) stream)
  (print-unreadable-object (transform stream :type t :identity t)
    (format stream "~s ~s ~s ~s ~s ~s"
            (ns::ns-affine-transform-struct-m11 transform)
            (ns::ns-affine-transform-struct-m12 transform)
            (ns::ns-affine-transform-struct-m21 transform)
            (ns::ns-affine-transform-struct-m22 transform)
            (ns::ns-affine-transform-struct-tx transform)
            (ns::ns-affine-transform-struct-ty transform))
    (describe-macptr-allocation-and-address transform stream)))





;;; An <NSA>ffine<T>ransform<S>truct is identical to a
;;; (:struct :<GGA>ffine<T>ransform), except for the names of its fields.

(setf (foreign-type-ordinal (parse-foreign-type '(:struct :<GGA>ffine<T>ransform)))
      (foreign-type-ordinal (parse-foreign-type :<NSA>ffine<T>ransform<S>truct)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun unwrap-boolean (form)
    `(not (eql 0 ,form)))
  (defun wrap-boolean (form)
    `(if ,form 1 0)))


;;; NSDecimal
(define-typed-foreign-struct-class ns::ns-decimal (:<NSD>ecimal ns::ns-decimal-p nil nil nil)
  (nil ns::ns-decimal-exponent :<NSD>ecimal._exponent)
  (nil ns::ns-decimal-length :<NSD>ecimal._length)
  (nil ns::ns-decimal-is-negative :<NSD>ecimal._is<N>egative wrap-boolean unwrap-boolean)
  (nil ns::ns-decimal-is-compact :<NSD>ecimal._is<C>ompact wrap-boolean unwrap-boolean))
  

(defun ns::init-ns-decimal (data exponent length is-negative is-compact mantissa)
  (setf (pref data :<NSD>ecimal._exponent) exponent
        (pref data :<NSD>ecimal._length) length
        (pref data :<NSD>ecimal._is<N>egative) (if is-negative 1 0)
        (pref data :<NSD>ecimal._is<C>ompact) (if is-compact 1 0))
    (let* ((v (coerce mantissa '(vector (unsigned-byte 16) 8))))
      (declare (type (simple-array (unsigned-byte 16) (8)) v))
      (with-macptrs ((m (pref data :<NSD>ecimal._mantissa)))
        (dotimes (i 8)
          (setf (paref m (:* (:unsigned 16)) i) (aref v i))))))

(defun ns::make-ns-decimal (exponent length is-negative is-compact mantissa)  
  (let* ((data (make-gcable-record :<NSD>ecimal)))
    (ns::init-ns-decimal data exponent length is-negative is-compact mantissa)
    data))




(defun ns::ns-decimal-mantissa (decimal)
  (if (typep decimal 'ns::ns-decimal)
    (let* ((dest (make-array 8 :element-type '(unsigned-byte 16))))
      (with-macptrs ((m (pref decimal :<NSD>ecimal._mantissa)))
        (dotimes (i 8 dest)
        (setf (aref dest i) (paref m (:* (:unsigned 16)) i)))))
    (report-bad-arg decimal 'ns::ns-decimal)))

(defun (setf ns::ns-decimal-mantissa) (new decimal)
  (if (typep decimal 'ns::ns-decimal)
    (let* ((src (coerce new '(simple-array (unsigned-byte 16) (8)))))
      (declare (type (simple-array (unsigned-byte 16) 8) src))
      (with-macptrs ((m (pref decimal :<NSD>ecimal._mantissa)))
        (dotimes (i 8 new)
          (setf (paref m (:* (:unsigned 16)) i) (aref src i)))))
    (report-bad-arg decimal 'ns::ns-decimal)))

(defmethod print-object ((d ns::ns-decimal) stream)
  (print-unreadable-object (d stream :type t :identity t)
    (unless (%null-ptr-p d)
      (format stream "exponent = ~d, length = ~s, is-negative = ~s, is-compact = ~s, mantissa = ~s" (ns::ns-decimal-exponent d) (ns::ns-decimal-length d) (ns::ns-decimal-is-negative d) (ns::ns-decimal-is-compact d) (ns::ns-decimal-mantissa d)))
    (describe-macptr-allocation-and-address d stream)))



    
;;; NSRect

(define-typed-foreign-struct-class ns::ns-rect (:<NSR>ect ns::ns-rect-p ns::init-ns-rect ns::make-ns-rect ns::with-ns-rect)
  (x ns::ns-rect-x :<NSR>ect.origin.x wrap-cg-float)
  (y ns::ns-rect-y :<NSR>ect.origin.y wrap-cg-float)
  (width ns::ns-rect-width :<NSR>ect.size.width wrap-cg-float)
  (height ns::ns-rect-height :<NSR>ect.size.height wrap-cg-float))


(defmethod print-object ((r ns::ns-rect) stream)
  (print-unreadable-object (r stream :type t :identity t)
    (unless (%null-ptr-p r)
      (flet ((maybe-round (x)
               (multiple-value-bind (q r) (round x)
                 (if (zerop r) q x))))
        (format stream "~s X ~s @ ~s,~s"
                (maybe-round (ns::ns-rect-width r))
                (maybe-round (ns::ns-rect-height r))
                (maybe-round (ns::ns-rect-x r))
                (maybe-round (ns::ns-rect-y r)))
        (describe-macptr-allocation-and-address r stream)))))



;;; NSSize
(define-typed-foreign-struct-class ns::ns-size (:<NSS>ize ns::ns-size-p ns::init-ns-size ns::make-ns-size ns::with-ns-size)
  (width ns::ns-size-width :<NSS>ize.width wrap-cg-float)
  (height ns::ns-size-height :<NSS>ize.height wrap-cg-float))


(defmethod print-object ((s ns::ns-size) stream)
  (flet ((maybe-round (x)
           (multiple-value-bind (q r) (round x)
             (if (zerop r) q x))))
    (unless (%null-ptr-p s)
      (print-unreadable-object (s stream :type t :identity t)
        (format stream "~s X ~s"
                (maybe-round (ns::ns-size-width s))
                (maybe-round (ns::ns-size-height s)))))
    (describe-macptr-allocation-and-address s stream)))


;;; NSPoint
(define-typed-foreign-struct-class ns::ns-point (:<NSP>oint ns::ns-point-p ns::init-ns-point ns::make-ns-point ns::with-ns-point)
  (x ns::ns-point-x :<NSP>oint.x wrap-cg-float)
  (y ns::ns-point-y :<NSP>oint.y wrap-cg-float))

(defmethod print-object ((p ns::ns-point) stream)
  (flet ((maybe-round (x)
           (multiple-value-bind (q r) (round x)
             (if (zerop r) q x))))
    (print-unreadable-object (p stream :type t :identity t)
      (unless (%null-ptr-p p)
        (format stream "~s,~s"
                (maybe-round (ns::ns-point-x p))
                (maybe-round (ns::ns-point-y p))))
      (describe-macptr-allocation-and-address p stream))))


;;; NSRange
(define-typed-foreign-struct-class ns::ns-range (:<NSR>ange ns::ns-range-p ns::init-ns-range ns::make-ns-range ns::with-ns-range)
  (location ns::ns-range-location :<NSR>ange.location)
  (length ns::ns-range-length :<NSR>ange.length ))

(defmethod print-object ((r ns::ns-range) stream)
  (print-unreadable-object (r stream :type t :identity t)
    (unless (%null-ptr-p r)
      (format stream "~s/~s"
              (ns::ns-range-location r)
              (ns::ns-range-length r)))
    (describe-macptr-allocation-and-address r stream)))


;;; String might be stack allocated; make a copy before complaining
;;; about it.
(defun check-objc-message-name (string)
  (dotimes (i (length string))
    (let* ((ch (char string i)))
      (unless (or (alpha-char-p ch)
                  (digit-char-p ch 10)
                  (eql ch #\:)
                  (eql ch #\_))
        (error "Illegal character ~s in ObjC message name ~s"
               ch (copy-seq string)))))
  (when (and (position #\: string)
             (not (eql (char string (1- (length string))) #\:)))
    (error "ObjC message name ~s contains colons, but last character is not a colon" (copy-seq string))))
      

(setf (pkg.intern-hook (find-package "NSFUN"))
      'get-objc-message-info)

(set-dispatch-macro-character #\# #\/ 
                              (lambda (stream subchar numarg)
                                (declare (ignorable subchar numarg))
                                (let* ((token (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t))
                                       (attrtab (rdtab.ttab *readtable*)))
                                  (when (peek-char t stream nil nil)
                                    (loop
                                      (multiple-value-bind (char attr)
                                          (%next-char-and-attr stream attrtab)
                                        (unless (eql attr $cht_cnst)
                                          (when char (unread-char char stream))
                                          (return))
                                        (vector-push-extend char token))))
                                  (unless *read-suppress*
                                    (unless (> (length token) 0)
                                      (signal-reader-error stream "Invalid token after #/."))
                                    (check-objc-message-name token)
                                    (intern token "NSFUN")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              Utilities                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return separate lists of the keys and values in a keyword/value list

(defun keys-and-vals (klist)
  (when (oddp (length klist))
    (error "Invalid keyword/value list: ~S" klist))
  (loop for l = klist then (cddr l)
        until (null l)
        collect (first l) into keys
        collect (second l) into vals
        finally (return (values keys vals))))


;;; Return the typestring for an ObjC METHOD 

(defun method-typestring (method)
  (%get-cstring #+apple-objc-2.0
                (#_method_getTypeEncoding method)
                #-apple-objc-2.0
                (pref method :objc_method.method_types)))


;;; Parse the ObjC message from a SENDxxx macro

(defun parse-message (args)
  (let ((f (first args))
	(nargs (length args)))
    (cond ((or (= nargs 1) (= nargs 2))
	   ;; (THING {VARGS})
	   (if (constantp f)
	       (%parse-message (cons (eval f) (rest args)))
	     (values f (rest args) nil)))
	  ;; (THING1 ARG1 ... THINGN ARGN)
	  ((evenp nargs)
	   (multiple-value-bind (ks vs) (keys-and-vals args)
	     (if (every #'constantp ks)
		 (%parse-message (mapcan #'list (mapcar #'eval ks) vs))
	       (values f (rest args) nil))))
	  ;; (THING1 ARG1 ... THINGN ARGN VARGS)
	  (t (multiple-value-bind (ks vs) (keys-and-vals (butlast args))
	       (if (every #'constantp ks)
		   (%parse-message 
		    (nconc (mapcan #'list (mapcar #'eval ks) vs) (last args)))
		 (values f (rest args) nil)))))))


;;; Parse the ObjC message from the evaluated args of a %SENDxxx function

(defun %parse-message (args)
  (let ((f (first args))
	(l (first (last args))))
    (cond ((stringp f)
	   ;; (STRING-with-N-colons ARG1 ... ARGN {LIST}) 
	   (let* ((n (count #\: (the simple-string f)))
                  (message-info (need-objc-message-info f))
		  (args (rest args))
		  (nargs (length args)))
	     (cond ((and (= nargs 1)
                         (getf (objc-message-info-flags message-info)
                               :accepts-varargs))
		    (values f nil l))
		   ((= nargs n) (values f args nil))
		   ((= nargs (1+ n)) (values f (butlast args) l))
		   (t (error "Improperly formatted argument list: ~S" args)))))
	  ((keywordp f)
	   ;; (KEY1 ARG1 ... KEYN ARGN {LIST}) or (KEY LIST)
	   (let ((nargs (length args)))
	     (cond ((and (= nargs 2) (consp l)
                         (let* ((info (need-objc-message-info
                                       (lisp-to-objc-message (list f)))))
                           (getf (objc-message-info-flags info)
                                 :accepts-varargs)))
		    (values (lisp-to-objc-message (list f)) nil l))
		   ((evenp nargs)
		    (multiple-value-bind (ks vs) (keys-and-vals args)
		      (values (lisp-to-objc-message ks) vs nil)))
		   ((and (> nargs 1) (listp l))
		    (multiple-value-bind (ks vs) (keys-and-vals (butlast args))
		      (values (lisp-to-objc-message ks) vs l)))
		 (t (error "Improperly formatted argument list: ~S" args)))))
	  ((symbolp f)
	   ;; (SYMBOL {LIST})
	   (let ((nargs (length (rest args))))
	     (cond ((= nargs 0) (values (lisp-to-objc-message (list f)) nil nil))
		   ((= nargs 1) (values (lisp-to-objc-message (list f)) nil l))
		   (t (error "Improperly formatted argument list: ~S" args)))))
	   (t (error "Improperly formatted argument list: ~S" args)))))


;;; Return the declared type of FORM in ENV

(defun declared-type (form env)
  (cond ((symbolp form)
         (multiple-value-bind (ignore ignore decls) 
                              (variable-information form env)
           (declare (ignore ignore))
           (or (cdr (assoc 'type decls)) t)))
        ((and (consp form) (eq (first form) 'the))
         (second form))
        (t t)))


;;; Return the current optimization setting of KEY in ENV

(defun optimization-setting (key &optional env)
  (cadr (assoc key (declaration-information 'optimize env))))


;;; Return the ObjC class named CNAME

(defun find-objc-class (cname)
  (%objc-class-classptr 
   (if (symbolp cname) 
       (find-class cname)
     (load-objc-class-descriptor cname))))


;;; Return the class object of an ObjC object O, signalling an error
;;; if O is not an ObjC object
                      
(defun objc-class-of (o)
  (if (objc-object-p o)
      (class-of o)
    (progn
      #+debug
      (#_NSLog #@"class name = %s" :address (pref (pref o :objc_object.isa)
                                                  :objc_class.name))
      (error "~S is not an ObjC object" o))))


;;; Returns the ObjC class corresponding to the declared type OTYPE if
;;; possible, NIL otherwise 

(defun get-objc-class-from-declaration (otype)
  (cond ((symbolp otype) (lookup-objc-class (lisp-to-objc-classname otype)))
        ((and (consp otype) (eq (first otype) '@metaclass))
         (let* ((name (second otype))
                (c
                 (typecase name
                   (string (lookup-objc-class name))
                   (symbol (lookup-objc-class (lisp-to-objc-classname name)))
                   (t (error "Improper metaclass typespec: ~S" otype)))))
           (unless (null c) (objc-class-of c))))))


;;; Returns the selector of MSG 

(defun get-selector (msg)
  (%get-selector (load-objc-selector msg)))


;;; Get the instance method structure corresponding to SEL for CLASS 

(defun get-method (class sel)
  (let ((m (class-get-instance-method class sel)))
    (if (%null-ptr-p m)
      (error "Instances of ObjC class ~S cannot respond to the message ~S" 
             (objc-class-name class)
             (lisp-string-from-sel sel))
      m)))


;;; Get the class method structure corresponding to SEL for CLASS

(defun get-class-method (class sel)
  (let ((m (class-get-class-method class sel)))
    (if (%null-ptr-p m)
      (error "ObjC class ~S cannot respond to the message ~S" 
             (objc-class-name class)
             (lisp-string-from-sel sel))
      m)))


;;; For some reason, these types sometimes show up as :STRUCTs even though they
;;; are not structure tags, but type names

(defun fudge-objc-type (ftype)
  (if (equal ftype '(:STRUCT :<NSD>ecimal))
      :<NSD>ecimal
    ftype))


;;; Returns T if the result spec requires a STRET for its return, NIL otherwise
;;; RSPEC may be either a number (in which case it is interpreted as a number
;;; of words) or a foreign type spec acceptable to PARSE-FOREIGN-TYPE. STRETS
;;; must be used when a structure larger than 4 bytes is returned

(defun requires-stret-p (rspec)
  (when (member rspec '(:DOUBLE-FLOAT :UNSIGNED-DOUBLEWORD :SIGNED-DOUBLEWORD) 
		:test #'eq)
    (return-from requires-stret-p nil))
  (setq rspec (fudge-objc-type rspec))
  (if (numberp rspec) 
    (> rspec 1)
    (> (ensure-foreign-type-bits (parse-foreign-type rspec)) target::nbits-in-word)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Stret Convenience Stuff                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Allocate any temporary storage necessary to hold strets required
;;; AT TOPLEVEL in the value forms.  Special recognition is given to
;;; SENDs involving strets and to stret pseudo-functions
;;; NS-MAKE-POINT, NS-MAKE-RANGE, NS-MAKE-RECT and NS-MAKE-SIZE

(defmacro slet (varforms &body body &environment env)
  (multiple-value-bind (clean-body decls) (parse-body body env nil)
    (loop with r and s
          for (var val) in varforms
          do (multiple-value-setq (r s) (sletify val t var))
          collect r into rvarforms
          unless (null s) collect s into stretforms
          finally 
          (return
           `(rlet ,rvarforms
              ,@decls
              ,@stretforms
              ,@clean-body)))))


;;; Note that SLET* does not allow declarations 

(defmacro slet* (varforms &body body &environment env)
  (if (= (length varforms) 1)
      `(slet ,varforms ,@body)
    `(slet ,(list (first varforms))
       (slet* ,(rest varforms) ,@body))))


;;; Collect the info necessary to transform a SLET into an RLET 

(defun sletify (form &optional errorp (var (gensym)))
  (if (listp form)
    (case (first form)
      (ns-make-point 
       (assert (= (length form) 3))
       `(,var :<NSP>oint :x ,(second form) :y ,(third form)))
      (ns-make-rect 
       (assert (= (length form) 5))
       `(,var :<NSR>ect :origin.x ,(second form) :origin.y ,(third form)
               :size.width ,(fourth form) :size.height ,(fifth form)))
      (ns-make-range 
       (assert (= (length form) 3))
       `(,var :<NSR>ange :location ,(second form) :length ,(third form)))
      (ns-make-size
       (assert (= (length form) 3))
       `(,var :<NSS>ize :width ,(second form) :height ,(third form)))
      (send
       (let* ((info (get-objc-message-info (parse-message (cddr form)))))
         (if (null info)
           (error "Can't determine message being sent in ~s" form))
         (let* ((rtype (objc-method-info-result-type
                        (car (objc-message-info-methods info)))))
           (if (getf (objc-message-info-flags info) :returns-structure)
             (values `(,var ,(if (typep rtype 'foreign-type)
                                 (unparse-foreign-type rtype)
                                 rtype))
                     `(send/stret ,var ,@(rest form)))
             (if errorp
               (error "NonSTRET SEND in ~S" form)
               form)))))
      (send-super
       (let* ((info (get-objc-message-info (parse-message (cddr form)))))
         (if (null info)
           (error "Can't determine message being sent in ~s" form))
         (let* ((rtype (objc-method-info-result-type
                        (car (objc-message-info-methods info)))))
           (if (getf (objc-message-info-flags info) :returns-structure)
             (values `(,var ,(if (typep rtype 'foreign-type)
                                 (unparse-foreign-type rtype)
                                 rtype))
                     `(send-super/stret ,var ,@(rest form)))
             (if errorp
               (error "NonSTRET SEND-SUPER in ~S" form)
               form)))))
      (t (if errorp
           (error "Unrecognized STRET call in ~S" form)
           form)))
    (if errorp
      (error "Unrecognized STRET call in ~S" form)
      form)))


;;; Process the arguments to a message send as an implicit SLET, collecting
;;; the info necessary to build the corresponding RLET

(defun sletify-message-args (args)
  (loop with svf and sif
        for a in args
        do (multiple-value-setq (svf sif) (sletify a))
        unless (null sif) collect sif into sifs
        unless (equal svf a)
          do (setf a (first svf))
          and collect svf into svfs
        collect a into nargs
        finally (return (values nargs svfs sifs))))
  
  
;;; Convenience macros for some common Cocoa structures.  More
;;; could be added

(defmacro ns-max-range (r) 
  (let ((rtemp (gensym)))
    `(let ((,rtemp ,r))
       (+ (pref ,rtemp :<NSR>ange.location) (pref ,rtemp :<NSR>ange.length)))))
(defmacro ns-min-x (r) `(pref ,r :<NSR>ect.origin.x))
(defmacro ns-min-y (r) `(pref ,r :<NSR>ect.origin.y))
(defmacro ns-max-x (r)
  (let ((rtemp (gensym)))
    `(let ((,rtemp ,r))
       (+ (pref ,r :<NSR>ect.origin.x) 
          (pref ,r :<NSR>ect.size.width)))))
(defmacro ns-max-y (r)
  (let ((rtemp (gensym)))
    `(let ((,rtemp ,r))
       (+ (pref ,r :<NSR>ect.origin.y)
          (pref ,r :<NSR>ect.size.height)))))
(defmacro ns-mid-x (r)
  (let ((rtemp (gensym)))
    `(let ((,rtemp ,r))
       (* 0.5 (+ (ns-min-x ,rtemp) (ns-max-x ,rtemp))))))
(defmacro ns-mid-y (r)
  (let ((rtemp (gensym)))
    `(let ((,rtemp ,r))
       (* 0.5 (+ (ns-min-y ,rtemp) (ns-max-y ,rtemp))))))
(defmacro ns-height (r) `(pref ,r :<NSR>ect.size.height))
(defmacro ns-width (r) `(pref ,r :<NSR>ect.size.width))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Type Stuff                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar *objc-message-info* (make-hash-table :test #'equal :size 800))

(defun result-type-requires-structure-return (result-type)
  ;; Use objc-msg-send-stret for all methods that return
  ;; record types.
  (or (typep result-type 'foreign-record-type)
      (and (not (typep result-type 'foreign-type))
           (typep (parse-foreign-type result-type) 'foreign-record-type))))

(defvar *objc-method-signatures* (make-hash-table :test #'equal))

(defstruct objc-method-signature-info
  type-signature
  function
  super-function)

(defun objc-method-signature-info (sig)
  (or (gethash sig *objc-method-signatures*)
      (setf (gethash sig *objc-method-signatures*)
            (make-objc-method-signature-info
             :type-signature sig
             :function (compile-send-function-for-signature  sig)
             :super-function (%compile-send-function-for-signature  sig t)))))

(defun concise-foreign-type (ftype)
  (if (typep ftype 'foreign-record-type)
    (let* ((name (foreign-record-type-name ftype)))
      (if name
        `(,(foreign-record-type-kind ftype) ,name)
        (unparse-foreign-type ftype)))
    (if (objc-id-type-p ftype)
      :id
      (if (typep ftype 'foreign-pointer-type)
        (let* ((to (foreign-pointer-type-to ftype)))
          (if (null to)
            '(:* :void)
            `(:* ,(concise-foreign-type to))))
        (if (typep ftype 'foreign-type)
          (unparse-foreign-type ftype)
          ftype)))))


;;; Not a perfect mechanism.
(defclass objc-dispatch-function (funcallable-standard-object)
    ()
  (:metaclass funcallable-standard-class))

(defmethod print-object ((o objc-dispatch-function) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (let* ((name (function-name o)))
      (when name
        (format stream "~s" name)))))




(declaim (inline check-receiver))

;;; Return a NULL pointer if RECEIVER is a null pointer.
;;; Otherwise, insist that it's an ObjC object of some sort, and return NIL.
(defun check-receiver (receiver)
  (if (%null-ptr-p receiver)
    (%null-ptr)
    (let* ((domain (%macptr-domain receiver))
           (valid (eql domain *objc-object-domain*)))
      (declare (fixnum domain))
      (when (zerop domain)
        (if (recognize-objc-object receiver)
          (progn (%set-macptr-domain receiver *objc-object-domain*)
                 (setq valid t))))
      (unless valid
        (report-bad-arg receiver 'objc:objc-object)))))

(defmethod shared-initialize :after ((gf objc-dispatch-function) slot-names &key message-info &allow-other-keys)
  (declare (ignore slot-names))
  (if message-info
    (let* ((ambiguous-methods (getf (objc-message-info-flags message-info) :ambiguous))
           (selector (objc-message-info-selector message-info))
           (first-method (car (objc-message-info-methods message-info))))
      (lfun-bits gf (dpb (1+ (objc-message-info-req-args message-info))
                         $lfbits-numreq
                         (logior (ash
                                  (if (getf (objc-message-info-flags message-info)
                                            :accepts-varargs)
                                    1
                                    0)
                                  $lfbits-rest-bit)
                                 (logandc2 (lfun-bits gf) (ash 1 $lfbits-aok-bit)))))
      (flet ((signature-function-for-method (m)
               (let* ((signature-info (objc-method-info-signature-info m)))
                 (or (objc-method-signature-info-function signature-info)
                     (setf (objc-method-signature-info-function signature-info)
                           (compile-send-function-for-signature
                                    (objc-method-signature-info-type-signature signature-info)))))))
                      
      (if (null ambiguous-methods)
        ;; Pick an arbitrary method, since all methods have the same
        ;; signature.
        (let* ((function (signature-function-for-method first-method)))
          (set-funcallable-instance-function
           gf
           (nfunction
            send-unambiguous-message
            (lambda (receiver &rest args)
               (declare (dynamic-extent args))
               (or (check-receiver receiver)
                   (with-ns-exceptions-as-errors 
                       (apply function receiver selector args)))))))
        (let* ((protocol-pairs (mapcar #'(lambda (pm)
                                           (cons (lookup-objc-protocol
                                                  (objc-method-info-class-name pm))
                                                 (signature-function-for-method
                                                  pm)))
                                       (objc-message-info-protocol-methods message-info)))
               (method-pairs (mapcar #'(lambda (group)
                                         (cons (mapcar #'(lambda (m)
                                                           (get-objc-method-info-class m))
                                                       group)
                                               (signature-function-for-method (car group))))
                                     (objc-message-info-ambiguous-methods message-info)))
               (default-function (if method-pairs
                                   (prog1 (cdar (last method-pairs))
                                     (setq method-pairs (nbutlast method-pairs)))
                                   (prog1 (cdr (last protocol-pairs))
                                     (setq protocol-pairs (nbutlast protocol-pairs))))))
          (set-funcallable-instance-function
           gf
           (nfunction
            send-unambiguous-message
            (lambda (receiver &rest args)
               (declare (dynamic-extent args))
               (or (check-receiver receiver)
                   (let* ((function
                           (or (dolist (pair protocol-pairs)
                                 (when (conforms-to-protocol receiver (car pair))
                                   (return (cdr pair))))
                               (block m
                                 (dolist (pair method-pairs default-function)
                                   (dolist (class (car pair))
                                     (when (typep receiver class)
                                       (return-from m (cdr pair)))))))))
                     (with-ns-exceptions-as-errors
                         (apply function receiver selector args)))))))))))
    (with-slots (name) gf
      (set-funcallable-instance-function
       gf
       #'(lambda (&rest args)
           (error "Unknown ObjC message ~a called with arguments ~s"
                  (symbol-name name) args))))))
                                             

(defun %call-next-objc-method (self class selector sig &rest args)
  (declare (dynamic-extent args))
  (rlet ((s :objc_super #+apple-objc :receiver #+gnu-objc :self self
            #+apple-objc-2.0 :super_class #-apple-objc-2.0 :class
            #+apple-objc-2.0 (#_class_getSuperclass class)
            #-apple-objc-2.0 (pref class :objc_class.super_class)))
    (let* ((siginfo (objc-method-signature-info sig))
           (function (or (objc-method-signature-info-super-function siginfo)
                         (setf (objc-method-signature-info-super-function siginfo)
                               (%compile-send-function-for-signature sig t)))))
      (with-ns-exceptions-as-errors
          (apply function s selector args)))))


(defun %call-next-objc-class-method (self class selector sig &rest args)
  (rlet ((s :objc_super #+apple-objc :receiver #+gnu-objc :self self
            #+apple-objc-2.0 :super_class #-apple-objc-2.0 :class
            #+apple-objc-2.0 (#_class_getSuperclass (pref class :objc_class.isa))
            #-apple-objc-2.0 (pref (pref class #+apple-objc :objc_class.isa #+gnu-objc :objc_class.class_pointer) :objc_class.super_class)))
    (let* ((siginfo (objc-method-signature-info sig))
           (function (or (objc-method-signature-info-super-function siginfo)
                         (setf (objc-method-signature-info-super-function siginfo)
                               (%compile-send-function-for-signature sig t)))))
      (with-ns-exceptions-as-errors
          (apply function s selector args)))))

(defun postprocess-objc-message-info (message-info)
  (let* ((objc-name (objc-message-info-message-name message-info))
         (lisp-name (or (objc-message-info-lisp-name message-info)
                        (setf (objc-message-info-lisp-name message-info)
                              (compute-objc-to-lisp-function-name  objc-name))))
         (gf (or (fboundp lisp-name)
                 (setf (fdefinition lisp-name)
                       (make-instance 'objc-dispatch-function :name lisp-name)))))

    (unless (objc-message-info-selector message-info)
      (setf (objc-message-info-selector message-info)
            (ensure-objc-selector (objc-message-info-message-name message-info))))
    
    (flet ((reduce-to-ffi-type (ftype)
             (concise-foreign-type ftype)))
      (flet ((ensure-method-signature (m)
               (or (objc-method-info-signature m)
                   (setf (objc-method-info-signature m)
                         (let* ((sig 
                                 (cons (reduce-to-ffi-type
                                        (objc-method-info-result-type m))
                                       (mapcar #'reduce-to-ffi-type
                                               (objc-method-info-arglist m)))))
                           (setf (objc-method-info-signature-info m)
                                 (objc-method-signature-info sig))
                           sig)))))
        (let* ((methods (objc-message-info-methods message-info))
               (signatures ())
               (protocol-methods)
               (signature-alist ()))
          (labels ((signatures-equal (xs ys)
                     (and xs
                          ys
                          (do* ((xs xs (cdr xs))
                                (ys ys (cdr ys)))
                               ((or (null xs) (null ys))
                                (and (null xs) (null ys)))
                            (unless (foreign-type-= (ensure-foreign-type (car xs))
                                                    (ensure-foreign-type (car ys)))
                              (return nil))))))
            (dolist (m methods)
              (let* ((signature (ensure-method-signature m)))
                (pushnew signature signatures :test #'signatures-equal)
                (if (getf (objc-method-info-flags m) :protocol)
                  (push m protocol-methods)
                  (let* ((pair (assoc signature signature-alist :test #'signatures-equal)))
                    (if pair
                      (push m (cdr pair))
                      (push (cons signature (list m)) signature-alist)))))))
          (setf (objc-message-info-ambiguous-methods message-info)
                (mapcar #'cdr
                        (sort signature-alist
                              #'(lambda (x y)
                                  (< (length (cdr x))
                                     (length (cdr y)))))))
          (setf (objc-message-info-flags message-info) nil)
          (setf (objc-message-info-protocol-methods message-info)
                protocol-methods)
          (when (cdr signatures)
            (setf (getf (objc-message-info-flags message-info) :ambiguous) t))
          (let* ((first-method (car methods))
                 (first-sig (objc-method-info-signature first-method))
                 (first-sig-len (length first-sig)))
            (setf (objc-message-info-req-args message-info)
                  (1- first-sig-len))
            ;; Whether some arg/result types vary or not, we want to insist
            ;; on (a) either no methods take a variable number of arguments,
            ;; or all do, and (b) either no method uses structure-return
            ;; conventions, or all do. (It's not clear that these restrictions
            ;; are entirely reasonable in the long run; in the short term,
            ;; they'll help get things working.)
            (flet ((method-returns-structure (m)
                     (result-type-requires-structure-return
                      (objc-method-info-result-type m)))
                   (method-accepts-varargs (m)
                     (eq (car (last (objc-method-info-arglist m)))
                         *void-foreign-type*))
                   (method-has-structure-arg (m)
                     (dolist (arg (objc-method-info-arglist m))
                       (when (typep (ensure-foreign-type arg) 'foreign-record-type)
                         (return t)))))
              (when (dolist (method methods)
                      (when (method-has-structure-arg method)
                        (return t)))
                (setf (compiler-macro-function lisp-name)
                      'hoist-struct-constructors))
              (let* ((first-result-is-structure (method-returns-structure first-method))
                     (first-accepts-varargs (method-accepts-varargs first-method)))
                (if (dolist (m (cdr methods) t)
                      (unless (eq (method-returns-structure m)
                                  first-result-is-structure)
                        (return nil)))
                  (if first-result-is-structure
                    (setf (getf (objc-message-info-flags message-info)
                                :returns-structure) t)))
                (if (dolist (m (cdr methods) t)
                      (unless (eq (method-accepts-varargs m)
                                  first-accepts-varargs)
                        (return nil)))
                  (if first-accepts-varargs
                    (progn
                      (setf (getf (objc-message-info-flags message-info)
                                  :accepts-varargs) t)
                      (decf (objc-message-info-req-args message-info)))))))))
        (reinitialize-instance gf :message-info message-info)))))
          
;;; -may- need to invalidate cached info whenever new interface files
;;; are made accessible.  Probably the right thing to do is to insist
;;; that (known) message signatures be updated in that case.
(defun get-objc-message-info (message-name &optional (use-database t))
  (setq message-name (string message-name))
  (or (gethash message-name *objc-message-info*)
      (and use-database
           (let* ((info (lookup-objc-message-info message-name)))
             (when info
               (setf (gethash message-name *objc-message-info*) info)
               (postprocess-objc-message-info info)
               info)))))

(defun need-objc-message-info (message-name)
  (or (get-objc-message-info message-name)
      (error "Undeclared message: ~s" message-name)))

;;; Should be called after using new interfaces that may define
;;; new methods on existing messages.
(defun update-objc-method-info ()
  (maphash #'(lambda (message-name info)
               (lookup-objc-message-info message-name info)
               (postprocess-objc-message-info info))
           *objc-message-info*))


;;; Of the method declarations (OBJC-METHOD-INFO structures) associated
;;; with the message-declaration (OBJC-MESSAGE-INFO structure) M,
;;; return the one that seems to be applicable for the object O.
;;; (If there's no ambiguity among the declared methods, any method
;;; will do; this just tells runtime %SEND functions how to compose
;;; an %FF-CALL).
(defun %lookup-objc-method-info (m o)
  (let* ((methods (objc-message-info-methods m))
         (ambiguous (getf (objc-message-info-flags m) :ambiguous)))
    (if (not ambiguous)
      (car methods)
      (or 
       (dolist (method methods)
         (let* ((mclass (get-objc-method-info-class method)))
           (if (typep o mclass)
             (return method))))
       (error "Can't determine ObjC method type signature for message ~s, object ~s" (objc-message-info-message-name m) o)))))

(defun resolve-existing-objc-method-info (message-info class-name class-p result-type args)
  (let* ((method-info (dolist (m (objc-message-info-methods message-info))
                        (when (and (eq (getf (objc-method-info-flags m) :class-p)
                                       class-p)
                                   (equal (objc-method-info-class-name m)
                                          class-name))
                          (return m)))))
    (when method-info
      (unless (and (foreign-type-= (ensure-foreign-type (objc-method-info-result-type method-info))
                                   (parse-foreign-type result-type))
                   (do* ((existing (objc-method-info-arglist method-info) (cdr existing))
                         (proposed args (cdr proposed)))
                        ((null existing) (null proposed))
                     (unless (foreign-type-= (ensure-foreign-type (car existing))
                                             (parse-foreign-type (car proposed)))
                       (return nil))))
        (cerror "Redefine existing method to have new type signature."
                "The method ~c[~a ~a] is already declared to have type signature ~s; the new declaration ~s is incompatible." (if class-p #\+ #\-) class-name (objc-message-info-message-name message-info) (objc-method-info-signature method-info) (cons result-type args))
        (setf (objc-method-info-arglist method-info) args
              (objc-method-info-result-type method-info) result-type
              (objc-method-info-signature method-info) nil
              (objc-method-info-signature-info method-info) nil))
      method-info)))

(defvar *objc-verbose* nil)

;;; Still not right; we have to worry about type conflicts with
;;; shadowed methods, as well.
(defun %declare-objc-method (message-name class-name class-p result-type args)
  (let* ((info (get-objc-message-info message-name)))
    (unless info
      (when (or *objc-verbose* *compile-print*)
	(format *error-output* "~&; Note: defining new ObjC message ~c[~a ~a]" (if class-p #\+ #\-) class-name message-name))
      (setq info (make-objc-message-info :message-name message-name))
      (setf (gethash message-name *objc-message-info*) info))
    (let* ((was-ambiguous (getf (objc-message-info-flags info) :ambiguous))
           (method-info (or (resolve-existing-objc-method-info info class-name class-p result-type args)
                            (make-objc-method-info :message-info info
                                                   :class-name class-name
                                                   :result-type result-type
                                                   :arglist args
                                                   :flags (if class-p '(:class t))))))
      (pushnew method-info (objc-message-info-methods info))
      (postprocess-objc-message-info info)
      (if (and (getf (objc-message-info-flags info) :ambiguous)
               (not was-ambiguous))
        (warn "previously declared methods on ~s all had the same type signature, but ~s introduces ambiguity" message-name method-info))
           
      (objc-method-info-signature method-info))))



;;; TRANSLATE-FOREIGN-ARG-TYPE doesn't accept :VOID

(defun translate-foreign-result-type (ftype)
  (ensure-foreign-type-bits (parse-foreign-type ftype))
  (if (eq ftype :void)
    :void
    (translate-foreign-arg-type ftype)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Invoking ObjC Methods                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The SEND and SEND/STRET macros

(defmacro send (o msg &rest args &environment env)
  (make-optimized-send o msg args env))

(defmacro send/stret (s o msg &rest args &environment env)
  (make-optimized-send o msg args env s))




;;; Optimize special cases of SEND and SEND/STRET

(defun make-optimized-send (o msg args env  &optional s super sclassname)
  (multiple-value-bind (msg args vargs) (parse-message (cons msg args))
    (let* ((message-info (get-objc-message-info msg)))
      (if (null message-info)
        (error "Unknown message: ~S" msg))
      ;; If a vararg exists, make sure that the message can accept it
      (when (and vargs (not (getf (objc-message-info-flags message-info)
                                  :accepts-varargs)))
        (error "Message ~S cannot accept a variable number of arguments" msg))
      (unless (= (length args) (objc-message-info-req-args message-info))
        (error "Message ~S requires ~a ~d args, but ~d were provided."
               msg
               (if vargs "at least" "exactly")
               (objc-message-info-req-args message-info)
               (length args)))
      (multiple-value-bind (args svarforms sinitforms) (sletify-message-args args)
        (let* ((ambiguous (getf (objc-message-info-flags message-info) :ambiguous))
               (methods (objc-message-info-methods message-info))
               (method (if (not ambiguous) (car methods))))
          (when ambiguous
            (let* ((class (if sclassname 
                            (find-objc-class sclassname)
                            (get-objc-class-from-declaration (declared-type o env)))))
              (if class
                (dolist (m methods)
                  (unless (getf (objc-method-info-flags m) :protocol)
                    (let* ((mclass (or (get-objc-method-info-class m)
                                       (error "Can't find ObjC class named ~s"
                                              (objc-method-info-class-name m)))))
                      (when (and class (subtypep class mclass))
                        (return (setq method m)))))))))
          (if method
            (build-call-from-method-info method
                                         args
                                         vargs
                                         o
                                         msg
                                         svarforms
                                         sinitforms
                                         s
                                         super)
            (build-ambiguous-send-form message-info
                                       args
                                       vargs
                                       o
                                       msg
                                       svarforms
                                       sinitforms
                                       s
                                       super)))))))

    
;;; WITH-NS-EXCEPTIONS-AS-ERRORS is only available in OpenMCL 0.14 and above

#-openmcl-native-threads
(defmacro with-ns-exceptions-as-errors (&body body)
  `(progn ,@body))


;;; Return a call to the method specified by SEL on object O, with the args
;;; specified by ARGSPECS.  This decides whether a normal or stret call is 
;;; needed and, if the latter, uses the memory S to hold the result. If SUPER
;;; is nonNIL, then this builds a send to super.  Finally, this also 
;;; coerces return #$YES/#$NO values to T/NIL. The entire call takes place 
;;; inside an implicit SLET.

(defun build-call (o sel msg argspecs svarforms sinitforms &optional s super)
  `(with-ns-exceptions-as-errors
     (rlet ,svarforms
       ,@sinitforms
       ,(let ((rspec (first (last argspecs))))
          (if (requires-stret-p rspec)
            (if (null s)
              ;; STRET required but not provided
              (error "The message ~S must be sent using SEND/STRET" msg)
              ;; STRET required and provided, use stret send
              (if (null super)
                ;; Regular stret send
                `(progn
                   (objc-message-send-stret ,s ,o ,(cadr sel)
                    ,@(append (butlast argspecs) (list :void)))
                   ,s)
                ;; Super stret send
                `(progn
                   (objc-message-send-super-stret ,s ,super ,(cadr sel)
                    ,@(append (butlast argspecs) (list :void)))
                   ,s)))
            (if (null s)
              ;; STRET not required and not provided, use send
              (if (null super)
                ;; Regular send
                (if (eq rspec :<BOOL>)
                  `(coerce-from-bool
                    (objc-message-send ,o ,(cadr sel) ,@argspecs))
                  `(objc-message-send ,o ,(cadr sel) ,@argspecs))
                ;; Super send
                (if (eq rspec :<BOOL>)
                  `(coerce-from-bool
                    (objc-message-send-super ,super ,(cadr sel) ,@argspecs))
                  `(objc-message-send-super ,super ,(cadr sel) ,@argspecs)))
              ;; STRET not required but provided
              (error "The message ~S must be sent using SEND" msg)))))))

(defun objc-id-type-p (foreign-type)
  (and (typep foreign-type 'foreign-pointer-type)
       (let* ((to (foreign-pointer-type-to foreign-type)))
         (and (typep to 'foreign-record-type)
              (eq :struct (foreign-record-type-kind to))
              (not (null (progn (ensure-foreign-type-bits to) (foreign-record-type-fields to))))
              (let* ((target (foreign-record-field-type (car (foreign-record-type-fields to)))))
                (and (typep target 'foreign-pointer-type)
                     (let* ((target-to (foreign-pointer-type-to target)))
                       (and (typep target-to 'foreign-record-type)
                            (eq :struct (foreign-record-type-kind target-to))
                            (eq :objc_class (foreign-record-type-name target-to))))))))))

(defun unique-objc-classes-in-method-info-list (method-info-list)
  (if (cdr method-info-list)                     ; if more than 1 class
    (flet ((subclass-of-some-other-class (c)
             (let* ((c-class (get-objc-method-info-class c)))
               (dolist (other method-info-list)
                 (unless (eq other c)
                   (when (subtypep c-class (get-objc-method-info-class other))
                   (return t)))))))
      (remove-if #'subclass-of-some-other-class method-info-list))
    method-info-list))
  
(defun get-objc-method-info-class (method-info)
  (or (objc-method-info-class-pointer method-info)
      (setf (objc-method-info-class-pointer method-info)
            (let* ((c (lookup-objc-class (objc-method-info-class-name method-info) nil)))
              (when c
                (let* ((meta-p (getf (objc-method-info-flags method-info) :class)))
                  (if meta-p
                    (with-macptrs ((m (pref c :objc_class.isa)))
                      (canonicalize-registered-metaclass m))
                    (canonicalize-registered-class c))))))))

;;; Generate some sort of CASE or COND to handle an ambiguous message
;;; send (where the signature of the FF-CALL depends on the type of the
;;; receiver.)
;;; AMBIGUOUS-METHODS is a list of lists of OBJC-METHOD-INFO structures,
;;; where the methods in each sublist share the same type signature.  It's
;;; sorted so that more unique method/signature combinations appear first
;;; (and are easier to special-case via TYPECASE.)
(defun build-send-case (ambiguous-methods
                        args
                        vargs
                        receiver
                        msg
                        s
                        super
                        protocol-methods)
  (flet ((method-class-name (m)
           (let* ((mclass (get-objc-method-info-class m)))
             (unless mclass
               (error "Can't find class with ObjC name ~s"
                      (objc-method-info-class-name m)))
             (class-name mclass))))

    (collect ((clauses))
      (let* ((protocol (gensym))
             (protocol-address (gensym)))
        (dolist (method protocol-methods)
          (let* ((protocol-name (objc-method-info-class-name method)))
            (clauses `((let* ((,protocol (lookup-objc-protocol ,protocol-name))
                              (,protocol-address (and ,protocol (objc-protocol-address ,protocol))))
                         (and ,protocol-address
                              (objc-message-send ,receiver
                                                 "conformsToProtocol:"
                                                 :address ,protocol-address
                                                 :<BOOL>)))
                       ,(build-internal-call-from-method-info
                         method args vargs receiver msg s super))))))
      (do* ((methods ambiguous-methods (cdr methods)))
           ((null (cdr methods))
            (when ambiguous-methods
              (clauses `(t
                         ,(build-internal-call-from-method-info
                           (caar methods) args vargs receiver msg s super)))))
        (clauses `(,(if (cdar methods)
                        `(or ,@(mapcar #'(lambda (m)
                                           `(typep ,receiver
                                             ',(method-class-name m)))
                                       (unique-objc-classes-in-method-info-list
                                        (car methods))))
                        `(typep ,receiver ',(method-class-name (caar methods))))
                   ,(build-internal-call-from-method-info
                     (caar methods) args vargs receiver msg s super))))
      `(cond
        ,@(clauses)))))

(defun build-ambiguous-send-form (message-info args vargs o msg svarforms sinitforms s super)
  (let* ((receiver (gensym))
         (caseform (build-send-case
                    (objc-message-info-ambiguous-methods message-info)
                    args
                    vargs
                    receiver
                    msg
                    s
                    super
                    (objc-message-info-protocol-methods message-info))))
    `(with-ns-exceptions-as-errors
      (rlet ,svarforms
        ,@sinitforms
        (let* ((,receiver ,o))
          ,caseform)))))


;;; Generate the "internal" part of a method call; the "external" part
;;; has established ObjC exception handling and handled structure-return
;;  details
(defun build-internal-call-from-method-info (method-info args vargs o msg s super)
  (let* ((arglist ()))
    (collect ((specs))
      (do* ((args args (cdr args))
            (argtypes (objc-method-info-arglist method-info) (cdr argtypes))
            (reptypes (cdr (objc-method-info-signature method-info)) (cdr reptypes)))
           ((null args) (setq arglist (append (specs) vargs)))
        (let* ((reptype (if (objc-id-type-p (car argtypes)) :id (car reptypes)))
               (arg (car args)))
          (specs reptype)
          (specs arg)))
      ;;(break "~& arglist = ~s" arglist)
      (if (result-type-requires-structure-return
           (objc-method-info-result-type method-info))
        (if (null s)
          ;; STRET required but not provided
          (error "The message ~S must be sent using SEND/STRET" msg)
          (if (null super)
            `(objc-message-send-stret ,s ,o ,msg ,@arglist ,(car (objc-method-info-signature method-info)))
            `(objc-message-send-super-stret ,s ,super ,msg ,@arglist ,(car (objc-method-info-signature method-info)))))
        (if s
          ;; STRET provided but not required
          (error "The message ~S must be sent using SEND" msg)
          (let* ((result-spec (car (objc-method-info-signature method-info)))
                 (form (if super
                         `(objc-message-send-super ,super ,msg ,@arglist ,result-spec)
                         `(objc-message-send ,o ,msg ,@arglist ,result-spec))))
            form))))))
  
(defun build-call-from-method-info (method-info args vargs o  msg  svarforms sinitforms s super)
  `(with-ns-exceptions-as-errors
    (rlet ,svarforms
      ,@sinitforms
      ,(build-internal-call-from-method-info
        method-info
        args
        vargs
        o
        msg
        s
        super))))

 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       Instantiating ObjC Class                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A MAKE-INSTANCE like interface to ObjC object creation

(defun make-objc-instance (cname &rest initargs)
  (declare (dynamic-extent initargs))
  (multiple-value-bind (ks vs) (keys-and-vals initargs)
    (declare (dynamic-extent ks vs))
    (let* ((class (etypecase cname
                    (string (canonicalize-registered-class 
                             (find-objc-class cname)))
                    (symbol (find-class cname))
                    (class cname))))
      (send-objc-init-message (#/alloc class) ks vs))))





;;; Provide the BRIDGE module

(provide "BRIDGE")
