;;;-*-Mode: LISP; Package: CCL -*-
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




;;; l1-clos-boot.lisp


(in-package "CCL")

;;; Early accessors.  These functions eventually all get replaced with
;;; generic functions with "real", official names.


(declaim (inline instance-slots %non-standard-instance-slots))
(defun %non-standard-instance-slots (instance typecode)
  (cond ((eql typecode target::subtag-macptr) (foreign-slots-vector instance))
        ((or (typep instance 'standard-generic-function)
             (typep instance 'funcallable-standard-object))
         (gf.slots instance))
        (t  (error "Don't know how to find slots of ~s" instance))))

(defun instance-slots (instance)
  (let* ((typecode (typecode instance)))
    (cond ((eql typecode target::subtag-instance) (instance.slots instance))
          (t (%non-standard-instance-slots instance typecode)))))


;;; True if X is a class but not a foreign-class.
(defun native-class-p (x)
  (if (%standard-instance-p x)
    (< (the fixnum (instance.hash x)) max-class-ordinal)))

(defun %class-name (class)
  (if (native-class-p class)
    (%class.name class)
    (class-name class)))

(defun %class-info (class)
  (if (native-class-p class)
    (%class.info class)
    (class-info class)))
  

(defun %class-kernel-p (class)
  (car (%class-info class)))

(defun (setf %class-kernel-p) (new class)
  (setf (car (%class-info class)) new))

(defun %class-proper-name (class)
  (cdr (%class-info class)))

(defun (setf %class-proper-name) (new class)
  (setf (cdr (%class-info class)) new))


(defun %class-own-wrapper (class)
  (if (native-class-p class)
    (%class.own-wrapper class)
   (class-own-wrapper class)))

(defun (setf %class-own-wrapper) (new class)
  (setf (%class.own-wrapper class) new))

(defun %class-alist (class)
  (%class.alist class))

(defun (setf %class-alist) (new class)
  (if (typep class 'slots-class)
    (setf (%class.alist class) new)
    new))

(defun %class-slots (class)
  (if (native-class-p class)
    (%class.slots class)
    (class-slots class)))

(defun (setf %class-slots) (new class)
  (if (native-class-p class)
    (setf (%class.slots class) new)
    (setf (class-slots class) new)))

(defun %class-direct-slots (class)
  (if (native-class-p class)
    (%class.direct-slots class)
    (class-direct-slots class)))

(defun (setf %class-direct-slots) (new class)
  (if (native-class-p class)
    (setf (%class.direct-slots class) new)
    (setf (class-direct-slots class) new)))






(defun %class-direct-superclasses (class)
  (%class.local-supers class))

(defun (setf %class-direct-superclasses) (new class)
  (setf (%class.local-supers class) new))

(defun %class-direct-subclasses (class)
  (%class.subclasses class))

(defun (setf %class-direct-subclasses) (new class)
  (setf (%class.subclasses class) new))

(defun %class-direct-default-initargs (class)
  (if (typep class 'std-class)
    (%class.local-default-initargs class)))

(defun (setf %class-direct-default-initargs) (new class)
  (if (typep class 'std-class)
    (setf (%class.local-default-initargs class) new)
    new))
  

(defun %class-default-initargs (class)
  (if (typep class 'std-class)
    (%class.default-initargs class)))


(defun (setf %class-default-initargs) (new class)
  (setf (%class.default-initargs class) new))

(defun %slot-definition-name (slotd)
  (standard-slot-definition.name slotd))


(defun %slot-definition-type (slotd)
  (standard-slot-definition.type slotd))

(defun %slot-definition-initargs (slotd)
  (standard-slot-definition.initargs slotd))


(defun %slot-definition-initform (slotd)
  (standard-slot-definition.initform slotd))

(defun %slot-definition-initfunction (slotd)
  (standard-slot-definition.initfunction slotd))

(defun %slot-definition-allocation (slotd)
  (standard-slot-definition.allocation slotd))

(defun %slot-definition-class (slotd)
  (standard-slot-definition.class slotd))

;;; Returns (VALUES BOUNDP VALUE).
(defun %slot-definition-documentation (slotd)
  (let* ((val (%standard-instance-instance-location-access
	       slotd
	       standard-slot-definition.documentation)))
    (if (eq val (%slot-unbound-marker))
      (values nil nil)
      (values t val))))


(defun %slot-definition-location (slotd)
  (standard-effective-slot-definition.location slotd))

(defun (setf %slot-definition-location) (new slotd)
  (setf (standard-effective-slot-definition.location slotd) new))

(defun %slot-definition-readers (slotd)
  (standard-direct-slot-definition.readers slotd))

(defun (setf %slot-definition-readers) (new slotd)
  (setf (standard-direct-slot-definition.readers slotd) new))

(defun %slot-definition-writers (slotd)
  (standard-direct-slot-definition.writers slotd))

(defun (setf %slot-definition-writers) (new slotd)
  (setf (standard-direct-slot-definition.writers slotd) new))

(defun %generic-function-name (gf)
  (sgf.name gf))

(defun %generic-function-method-combination (gf)
  (sgf.method-combination gf))

(defun %generic-function-method-class (gf)
  (sgf.method-class gf))


(defun %method-qualifiers (m)
  (%method.qualifiers m))

(defun %method-specializers (m)
  (%method.specializers m))

(defun %method-function (m)
  (%method.function m))

(defun (setf %method-function) (new m)
  (setf (%method.function m) new))

(defun %method-gf (m)
  (%method.gf m))

(defun (setf %method-gf) (new m)
  (setf (%method.gf m) new))

(defun %method-name (m)
  (%method.name m))

(defun %method-lambda-list (m)
  (%method.lambda-list m))


;;; Map slot-names (symbols) to SLOT-ID objects (which contain unique indices).
(let* ((slot-id-lock (make-lock))
       (next-slot-index 1)              ; 0 is never a valid slot-index
       (slot-id-hash (make-hash-table :test #'eq :weak t)))
  (defun ensure-slot-id (slot-name)
    (setq slot-name (require-type slot-name 'symbol))
    (with-lock-grabbed (slot-id-lock)
      (or (gethash slot-name slot-id-hash)
          (setf (gethash slot-name slot-id-hash)
                (%istruct 'slot-id slot-name (prog1
                                                 next-slot-index
                                               (incf next-slot-index)))))))
  (defun current-slot-index () (with-lock-grabbed (slot-id-lock)
                                 next-slot-index))
  )




(defun %slot-id-lookup-obsolete (instance slot-id)
  (update-obsolete-instance instance)
  (funcall (%wrapper-slot-id->slotd (instance.class-wrapper instance))
           instance slot-id))
(defun slot-id-lookup-no-slots (instance slot-id)
  (declare (ignore instance slot-id)))

(defun %slot-id-ref-obsolete (instance slot-id)
  (update-obsolete-instance instance)
  (funcall (%wrapper-slot-id-value (instance.class-wrapper instance))
           instance slot-id))
(defun %slot-id-ref-missing (instance slot-id)
  (values (slot-missing (class-of instance) instance (slot-id.name slot-id) 'slot-value)))

(defun %slot-id-set-obsolete (instance slot-id new-value)
  (update-obsolete-instance instance)
  (funcall (%wrapper-set-slot-id-value (instance.class-wrapper instance))
           instance slot-id new-value))

(defun %slot-id-set-missing (instance slot-id new-value)
  (slot-missing (class-of instance) instance (slot-id.name slot-id) 'setf new-value)
  new-value
  )



;;; This becomes (apply #'make-instance <method-class> &rest args).
(fset '%make-method-instance
      (nlambda bootstrapping-%make-method-instance (class &key
                                                          qualifiers
                                                          specializers
                                                          function
                                                          name
                                                          lambda-list
                                                          &allow-other-keys)
        (let* ((method
                (%instance-vector (%class-own-wrapper class)
                                  qualifiers
                                  specializers
                                  function
                                  nil
                                  name
                                  lambda-list)))
          (when function
            (let* ((inner (closure-function function)))
              (unless (eq inner function)
                (copy-method-function-bits inner function)))
            (lfun-name function method))
          method)))
  
       
		 
(defun encode-lambda-list (l &optional return-keys?)
  (multiple-value-bind (ok req opttail resttail keytail auxtail)
                       (verify-lambda-list l)
    (when ok
      (let* ((bits 0)
             (temp nil)
             (nreq (length req))
             (num-opt 0)
             (rest nil)
             (lexpr nil)
             (keyp nil)
             (key-list nil)
             (aokp nil)
             (hardopt nil))
        (when (> nreq #.(ldb $lfbits-numreq $lfbits-numreq))
          (return-from encode-lambda-list nil))
        (when (eq (pop opttail) '&optional)
          (until (eq opttail resttail)
            (when (and (consp (setq temp (pop opttail)))
                       (%cadr temp))
              (setq hardopt t))
            (setq num-opt (%i+ num-opt 1))))
        (when (eq (%car resttail) '&rest)
          (setq rest t))
        (when (eq (%car resttail) '&lexpr)
          (setq lexpr t))
        (when (eq (pop keytail) '&key)
          (setq keyp t)
          (labels ((ensure-symbol (x)
                     (if (symbolp x) x (return-from encode-lambda-list nil)))
                   (ensure-keyword (x)
                     (make-keyword (ensure-symbol x))))
            (declare (dynamic-extent #'ensure-symbol #'ensure-keyword))
            (until (eq keytail auxtail)
              (setq temp (pop keytail))
              (if (eq temp '&allow-other-keys)
                (progn
                  (setq aokp t)
                  (unless (eq keytail auxtail)
                    (return-from encode-lambda-list nil)))
                (when return-keys?
                  (push (if (consp temp)
                          (if (consp (setq temp (%car temp))) 
                            (ensure-symbol (%car temp))
                            (ensure-keyword temp))
                          (ensure-keyword temp))
                        key-list))))))
        (when (%i> nreq (ldb $lfbits-numreq -1))
          (setq nreq (ldb $lfbits-numreq -1)))
        (setq bits (dpb nreq $lfbits-numreq bits))
        (when (%i> num-opt (ldb $lfbits-numopt -1))
          (setq num-opt (ldb $lfbits-numopt -1)))
        (setq bits (dpb num-opt $lfbits-numopt bits))
        (when hardopt (setq bits (%ilogior (%ilsl $lfbits-optinit-bit 1) bits)))
        (when rest (setq bits (%ilogior (%ilsl $lfbits-rest-bit 1) bits)))
        (when lexpr (setq bits (%ilogior (%ilsl $lfbits-restv-bit 1) bits)))
        (when keyp (setq bits (%ilogior (%ilsl $lfbits-keys-bit 1) bits)))
        (when aokp (setq bits (%ilogior (%ilsl $lfbits-aok-bit 1) bits)))
        (if return-keys?
          (values bits (and keyp (apply #'vector (nreverse key-list))))
          bits)))))

(defun pair-arg-p (thing &optional lambda-list-ok supplied-p-ok keyword-nesting-ok)
  (or (symbol-arg-p thing lambda-list-ok) ; nil ok in destructuring case
      (and (consp thing)
           (or (null (%cdr thing))
               (and (consp (%cdr thing))
                    (or (null (%cddr thing))
                        (and supplied-p-ok
                             (consp (%cddr thing))
                             (null (%cdddr thing))))))
           (if (not keyword-nesting-ok)
             (req-arg-p (%car thing) lambda-list-ok)
             (or (symbol-arg-p (%car thing) lambda-list-ok)
                 (and (consp (setq thing (%car thing)))
                      (consp (%cdr thing))
                      (null (%cddr thing))
                      (%car thing)
                      (symbolp (%car thing))
                      (req-arg-p (%cadr thing) lambda-list-ok)))))))

(defun req-arg-p (thing &optional lambda-list-ok)
 (or
  (symbol-arg-p thing lambda-list-ok)
  (lambda-list-arg-p thing lambda-list-ok)))

(defun symbol-arg-p (thing nil-ok)
  (and
   (symbolp thing)
   (or thing nil-ok)
   (not (memq thing lambda-list-keywords))))

(defun lambda-list-arg-p (thing lambda-list-ok)
  (and 
   lambda-list-ok
   (listp thing)
   (if (verify-lambda-list thing t t)
     (setq *structured-lambda-list* t))))

(defun opt-arg-p (thing &optional lambda-ok)
  (pair-arg-p thing lambda-ok t nil))

(defun key-arg-p (thing &optional lambda-ok)
  (pair-arg-p thing lambda-ok t t))

(defun proclaimed-ignore-p (sym)
  (cdr (assq sym *nx-proclaimed-ignore*)))

(defun verify-lambda-list (l &optional destructure-p whole-p env-p)
  (let* ((the-keys lambda-list-keywords)
         opttail
         resttail
         keytail
         allowothertail
         auxtail
         safecopy
         whole
         m
         n
         req
         sym
         (*structured-lambda-list* nil))
  (prog ()
    (multiple-value-setq (safecopy whole)
                         (normalize-lambda-list l whole-p env-p))
    (unless (or destructure-p (eq l safecopy) (go LOSE)))
    (setq l safecopy)
    (unless (dolist (key the-keys t)
              (when (setq m (cdr (memq key l)))
                (if (memq key m) (return))))
      (go LOSE))
    (if (null l) (go WIN))
    (setq opttail (memq '&optional l))
    (setq m (or (memq '&rest l)
                (unless destructure-p (memq '&lexpr l))))
    (setq n (if destructure-p (memq '&body l)))
    (if (and m n) (go LOSE) (setq resttail (or m n)))
    (setq keytail (memq '&key l))
    (if (and (setq allowothertail (memq '&allow-other-keys l))
             (not keytail))
      (go LOSE))
    (if (and (eq (car resttail) '&lexpr)
             (or keytail opttail))
      (go lose))
    (setq auxtail (memq '&aux l))
    (loop
      (when (null l) (go WIN))
      (when (or (eq l opttail)
                (eq l resttail)
                (eq l keytail)
                (eq l allowothertail)
                (eq l auxtail))
        (return))
      (setq sym (pop l))
      (unless (and (req-arg-p sym destructure-p)
                   (or (proclaimed-ignore-p sym)
                       (and destructure-p (null sym))
                       (not (memq sym req))))  ; duplicate required args
        (go LOSE))
      (push sym req))
    (when (eq l opttail)
      (setq l (%cdr l))
      (loop
        (when (null l) (go WIN))
        (when (or (eq l resttail)
                  (eq l keytail)
                  (eq l allowothertail)
                  (eq l auxtail))
          (return))
        (unless (opt-arg-p (pop l) destructure-p)
          (go LOSE))))
    (when (eq l resttail)
      (setq l (%cdr l))
      (when (or (null l)
                (eq l opttail)
                (eq l keytail)
                (eq l allowothertail)
                (eq l auxtail))
        (go LOSE))
      (unless (req-arg-p (pop l) destructure-p) (go LOSE)))
    (unless (or (eq l keytail)  ; allowothertail is a sublist of keytail if present
                (eq l auxtail))
      (go LOSE))
    (when (eq l keytail)
      (pop l)
      (loop
        (when (null l) (go WIN))
        (when (or (eq l opttail)
                  (eq l resttail))
          (go LOSE))
        (when (or (eq l auxtail) (setq n (eq l allowothertail)))
          (if n (setq l (%cdr l)))
          (return))
        (unless (key-arg-p (pop l) destructure-p) (go LOSE))))
    (when (eq l auxtail)
      (setq l (%cdr l))
      (loop
        (when (null l) (go WIN))
        (when (or (eq l opttail)
                  (eq l resttail)
                  (eq l keytail))
          (go LOSE))
        (unless (pair-arg-p (pop l)) (go LOSE))))
    (when l (go LOSE))
  WIN
  (return (values
           t
           (nreverse req)
           (or opttail resttail keytail auxtail)
           (or resttail keytail auxtail)
           (or keytail auxtail)
           auxtail
           safecopy
           whole
           *structured-lambda-list*))
  LOSE
  (return (values nil nil nil nil nil nil nil nil nil nil)))))

(defun normalize-lambda-list (x &optional whole-p env-p)
  (let* ((y x) whole env envtail head)
    (setq
     x
     (loop
       (when (atom y)
         (if (or (null y) (eq x y))  (return x))
         (setq x (copy-list x) y x)
         (return
          (loop
            (when (atom (%cdr y))
              (%rplacd y (list '&rest (%cdr y)))
              (return x))
            (setq y (%cdr y)))))
       (setq y (%cdr y))))
    (when env-p
      ;; Trapped in a world it never made ... 
      (when (setq y (memq '&environment x))
        (setq envtail (%cddr y)
              env (%cadr y))
        (cond ((eq y x)
               (setq x envtail))
              (t
               (dolist (v x)
                 (if (eq v '&environment)
                   (return)
                   (push v head)))
               (setq x (nconc (nreverse head) envtail) y (%car envtail))))))
    (when (and whole-p 
               (eq (%car x) '&whole)
               (%cadr x))
      (setq whole (%cadr x) x (%cddr x)))
    (values x whole env)))




(eval-when (eval compile)
  (require 'defstruct-macros))

(eval-when (:compile-toplevel :execute)
  (defmacro make-instance-vector (wrapper len)
    (let* ((instance (gensym))
	   (slots (gensym)))
      `(let* ((,slots (allocate-typed-vector :slot-vector (1+ ,len) (%slot-unbound-marker)))
	      (,instance (gvector :instance 0 ,wrapper ,slots)))
	(setf (instance.hash ,instance) (strip-tag-to-fixnum ,instance)
	      (slot-vector.instance ,slots) ,instance))))
)

(eval-when (:compile-toplevel :execute)
  (defmacro make-structure-vector (size)
    `(%alloc-misc ,size target::subtag-struct nil))

)
;;;;;;;;;;;;;;;;;;;;;;;;;;; defmethod support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(%fhave '%move-method-encapsulations-maybe ; Redefined in encapsulate
        (qlfun boot-%move-method-encapsulations-maybe (m1 m2)
          (declare (ignore m1 m2))
          nil))

(%fhave 'find-unencapsulated-definition  ;Redefined in encapsulate
        (qlfun bootstrapping-find-unencapsulated-definition (fn)
	  fn))

(%fhave 'function-encapsulated-p  ;Redefined in encapsulate
        (qlfun bootstrapping-function-encapsulated-p (fn)
	  (declare (ignore fn))
          nil))

(defparameter *uniquify-dcode* #+unique-dcode t #-unique-dcode nil
  "If true, each gf will get its own unique copy of its dcode.  Not recommended for
   real use (for one thing, it's known to break gf tracing), but may be helpful for
   profiling")

(let* ((class-wrapper-random-state (make-random-state))
       (class-wrapper-random-state-lock (make-lock)))

  (defun  new-class-wrapper-hash-index ()
    ;; mustn't be 0
    (with-lock-grabbed (class-wrapper-random-state-lock)
      (the fixnum (1+ (the fixnum (random target::target-most-positive-fixnum class-wrapper-random-state)))))))


(defun %inner-method-function (method)
  (closure-function
   (find-unencapsulated-definition
    (%method-function method))))

(defun copy-method-function-bits (from to)
  (let ((new-bits (logior (logand (logior (lsh 1 $lfbits-method-bit)
                                          (ash 1 $lfbits-nextmeth-bit)
                                          (ash 1 $lfbits-nextmeth-with-args-bit)
                                          $lfbits-args-mask) 
                                  (lfun-bits from))
                          (logand (lognot (logior (lsh 1 $lfbits-method-bit)
                                                  (ash 1 $lfbits-nextmeth-bit)
                                                  (ash 1 $lfbits-nextmeth-with-args-bit)
                                                  $lfbits-args-mask))
                                  (lfun-bits to)))))
    (lfun-bits to new-bits)
    new-bits))

(defun %ensure-generic-function-using-class (gf function-name &rest keys
						&key 
						&allow-other-keys)
  (if gf
    (apply #'%ensure-existing-generic-function-using-class gf function-name keys)
    (apply #'%ensure-new-generic-function-using-class function-name keys)))

(defun ensure-generic-function (function-name &rest keys &key &allow-other-keys)
  (let* ((def (fboundp function-name)))
    (when (and def (not (typep def 'generic-function)))
      (cerror "Try to remove any global non-generic function or macro definition."
	      (make-condition 'simple-program-error :format-control "The function ~s is defined as something other than a generic function." :format-arguments (list function-name)))
      (fmakunbound function-name)
      (setq def nil))
    (apply #'%ensure-generic-function-using-class def function-name keys)))


(defun %ensure-new-generic-function-using-class
    (function-name &rest keys &key
		   (generic-function-class *standard-generic-function-class* gfc-p)
                   &allow-other-keys)
  (declare (dynamic-extent keys))
  (when gfc-p
    (if (symbolp generic-function-class)
      (setq generic-function-class (find-class generic-function-class)))
    (unless (subtypep generic-function-class *standard-generic-function-class*)
      (error "~s is not a subtype of ~s" generic-function-class *generic-function-class*))
    (remf keys :generic-function-class))
  (let* ((gf (apply #'%make-gf-instance generic-function-class keys)))
    (unless (eq (%gf-method-combination gf) *standard-method-combination*)
      (register-gf-method-combination gf (%gf-method-combination gf)))
    (setf (sgf.name gf) (getf keys :name function-name))
    (setf (fdefinition function-name) gf)))

(defun %ensure-existing-generic-function-using-class
    (gf function-name &key
	(generic-function-class *standard-generic-function-class* gfc-p)
	(method-combination *standard-method-combination* mcomb-p)
	(method-class *standard-method-class* mclass-p)
	(argument-precedence-order nil apo-p)
	declarations
	(lambda-list nil ll-p)
	name)
  (when gfc-p
    (if (symbolp generic-function-class)
      (setq generic-function-class (find-class generic-function-class)))
    (unless (subtypep generic-function-class *standard-generic-function-class*)
      (error "~s is not a subtype of ~s" generic-function-class *standard-generic-function-class*)))
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
  (let* ((old-mc (sgf.method-combination gf)))
    (unless (eq old-mc method-combination)
      (unless (eq old-mc *standard-method-combination*)
	(unregister-gf-method-combination gf method-combination))))
    (setf (sgf.name gf) (or name function-name)
	  (sgf.decls gf) declarations
	  (sgf.method-class gf) method-class
	  (sgf.method-combination gf) method-combination)
    (unless (eq method-combination *standard-method-combination*)
      (register-gf-method-combination gf method-combination))
    (when ll-p
      (if apo-p
        (set-gf-arg-info gf :lambda-list lambda-list
                         :argument-precedence-order argument-precedence-order)
        (set-gf-arg-info gf :lambda-list lambda-list)))
    (setf (fdefinition function-name) gf))

(defun canonicalize-specializers (specializers &optional (copy t))
  (flet ((canonicalize-specializer (spec)
           (if (specializer-p spec)
             spec
             (if (symbolp spec)
               (find-class spec)
               (if (and (consp spec)
                        (eq (car spec) 'eql)
                        (consp (cdr spec))
                        (null (cddr spec)))
                 (intern-eql-specializer (cadr spec))
                 (error "Unknown specializer form ~s" spec))))))
    (if (and (not copy)
             (dolist (s specializers t)
               (unless (specializer-p s) (return nil))))
      specializers
      (mapcar #'canonicalize-specializer specializers))))

(defparameter *sealed-clos-world* nil "When true, class and method definition -at least - are disallowed.")

(defun ensure-method (name specializers &rest keys &key (documentation nil doc-p) qualifiers
                           &allow-other-keys)
  (declare (dynamic-extent keys))
  (if *sealed-clos-world*
    (error "Method (re)definition is not allowed in this environment.")
    (progn
      (setq specializers (canonicalize-specializers specializers))
      (let* ((gf (ensure-generic-function name))
             (method (apply #'%make-method-instance
                            (%gf-method-class gf)
                            :name name
                            :specializers specializers
                            keys))
             (old-method (when (%gf-methods gf)
                           (ignore-errors
                             (find-method gf qualifiers specializers nil)))))

        (%add-method gf method)
        (when (and doc-p *save-doc-strings*)
          (set-documentation method t documentation))
        (when old-method (%move-method-encapsulations-maybe old-method method))
        method))))


(defun %anonymous-method (function specializers qualifiers  lambda-list &optional documentation
                                   &aux name method-class)
  (let ((inner-function (closure-function function)))
    (unless (%method-function-p inner-function)
      (report-bad-arg inner-function 'method-function))   ; Well, I suppose we'll have to shoot you.
    (unless (eq inner-function function)   ; must be closed over
      (copy-method-function-bits inner-function function))
    (setq name (function-name inner-function))
    (if (typep name 'standard-method)     ; method-function already installed.
      (setq name (%method-name name)))
    (setq method-class *standard-method-class*)
    (unless (memq *standard-method-class* (or (%class.cpl method-class)
                                              (%class.cpl (update-class  method-class t))))
      (%badarg method-class 'standard-method))
    #|
    (unless (member qualifiers '(() (:before) (:after) (:around)) :test #'equal)
    (report-bad-arg qualifiers))
    ||#
    (setq specializers (mapcar #'(lambda (s)
                                   (or (and (consp s)
                                            (eq (%car s) 'eql)
                                            (consp (%cdr s))
                                            (null (%cddr s))
                                            (intern-eql-specializer (%cadr s)))
                                       (and (specializer-p s) s)
                                       (find-class s)))
                               specializers))
    (let ((method (%make-method-instance method-class
                      :name name
		      :lambda-list lambda-list
                      :qualifiers qualifiers
                      :specializers specializers
                      :function function)))
      (lfun-name inner-function method)
      (when documentation
        (set-documentation method t documentation))
      method)))

	   
(defun check-defmethod-congruency (gf method)
  (unless (congruent-lambda-lists-p gf method)
    (cerror (format nil
		    "Remove ~d method~:p from the generic-function and change its lambda list."
		    (length (%gf-methods gf)))
	    "Lambda list of method ~S ~%~
is incompatible with that of the generic function ~S.~%~
Method's lambda-list : ~s~%~
Generic-function's   : ~s~%" method (or (generic-function-name gf) gf) (flatten-method-lambda-list (%method-lambda-list method)) (generic-function-lambda-list gf))
    (loop
      (let ((methods (%gf-methods gf)))
        (if methods
          (remove-method gf (car methods))
          (return))))
    (%set-defgeneric-keys gf nil)
    (inner-lfun-bits gf (%ilogior (%ilsl $lfbits-gfn-bit 1)
                                  (%ilogand $lfbits-args-mask
                                            (lfun-bits (%method-function method))))))
  gf)



(defun %method-function-method (method-function)
  (setq method-function
        (closure-function
         (find-unencapsulated-definition method-function)))
  (setq method-function (require-type method-function 'method-function))
  (lfun-name method-function))

(defstatic %defgeneric-methods% (make-hash-table :test 'eq :weak t))

(defun %defgeneric-methods (gf)
   (gethash gf %defgeneric-methods%))

(defun %set-defgeneric-methods (gf &rest methods)
   (if methods
     (setf (gethash gf %defgeneric-methods%) methods)
     (remhash gf %defgeneric-methods%)))

(defun %defgeneric-keys (gf)
  (%gf-dispatch-table-keyvect (%gf-dispatch-table gf)))

(defun %set-defgeneric-keys (gf keyvect)
  (setf (%gf-dispatch-table-keyvect (%gf-dispatch-table gf)) keyvect))

(defun congruent-lfbits-p (gbits mbits)
  (and (eq (ldb $lfbits-numreq gbits) (ldb $lfbits-numreq mbits))
       (eq (ldb $lfbits-numopt gbits) (ldb $lfbits-numopt mbits))
       (eq (or (logbitp $lfbits-rest-bit gbits)
               (logbitp $lfbits-restv-bit gbits)
               (logbitp $lfbits-keys-bit gbits))
           (or (logbitp $lfbits-rest-bit mbits)
               (logbitp $lfbits-restv-bit mbits)
               (logbitp $lfbits-keys-bit mbits)))))

(defun congruent-lambda-lists-p (gf method &optional
                                    error-p gbits mbits gkeys)
  (unless gbits (setq gbits (inner-lfun-bits gf)))
  (unless mbits (setq mbits (lfun-bits (%method-function method))))
  (and (congruent-lfbits-p gbits mbits)
       (or (and (or (logbitp $lfbits-rest-bit mbits)
                    (logbitp $lfbits-restv-bit mbits))
                (not (logbitp $lfbits-keys-bit mbits)))
           (logbitp $lfbits-aok-bit mbits)
           (progn
             (unless gkeys (setq gkeys (%defgeneric-keys gf)))
             (or (null gkeys)
                 (eql 0 (length gkeys))
                 (let ((mkeys (lfun-keyvect
                               (%inner-method-function method))))
                   (dovector (key gkeys t)
                     (unless (find key mkeys :test 'eq)
                       (if error-p
                         (error "~s does not specify keys: ~s" method gkeys))
                       (return nil)))))))))

(defun %add-method (gf method)
  (%add-standard-method-to-standard-gf gf method))

;; Redefined in l1-clos.lisp
(fset 'maybe-remove-make-instance-optimization
      (nlambda bootstrapping-maybe-remove-make-instance-optimization (gfn method)
        (declare (ignore gfn method))
        nil))

(defun %add-standard-method-to-standard-gf (gfn method)
  (when (%method-gf method)
    (error "~s is already a method of ~s." method (%method-gf method)))
  (set-gf-arg-info gfn :new-method method)
  (let* ((dt (%gf-dispatch-table gfn))
	 (methods (sgf.methods gfn))
	 (specializers (%method-specializers method))
	 (qualifiers (%method-qualifiers method)))
    (remove-obsoleted-combined-methods method dt specializers)
    (maybe-remove-make-instance-optimization gfn method)
    (apply #'invalidate-initargs-vector-for-gf gfn specializers)
    (dolist (m methods)
      (when (and (equal specializers (%method-specializers m))
		 (equal qualifiers (%method-qualifiers m)))
	(remove-method gfn m)
	;; There can be at most one match
	(return)))
    (push method (sgf.methods gfn))
    (setf (%gf-dispatch-table-methods dt) (sgf.methods gfn))
    (setf (%method-gf method) gfn)
    (%add-direct-methods method)
    (compute-dcode gfn dt)
    (when (sgf.dependents gfn)
      (map-dependents gfn #'(lambda (d)
			      (update-dependent gfn d 'add-method method)))))
  gfn)

(defstatic *standard-kernel-method-class* nil)

(defun methods-congruent-p (m1 m2)
  (when (and (standard-method-p m1)(standard-method-p m2))
    (when (equal (%method-qualifiers m1) (%method-qualifiers m2))
      (let ((specs (%method-specializers m1)))
        (dolist (msp (%method-specializers m2) t)
          (let ((spec (%pop specs)))
            (unless (eq msp spec)
              (return nil))))))))

(defvar *maintain-class-direct-methods* nil)



;;; CAR is an EQL hash table for objects whose identity is not used by EQL
;;; (numbers and macptrs)
;;; CDR is a weak EQ hash table for other objects.
(defvar *eql-methods-hashes* (cons (make-hash-table :test 'eql)
                                   (make-hash-table :test 'eq :weak :key)))

(defun eql-methods-cell (object &optional addp)
  (let ((hashes *eql-methods-hashes*))
    (without-interrupts
     (let* ((hash (cond
                   ((or (typep object 'number)
                        (typep object 'macptr))
                    (car hashes))
                   (t (cdr hashes))))
            (cell (gethash object hash)))
       (when (and (null cell) addp)
         (setf (gethash object hash) (setq cell (cons nil nil))))
       cell))))




(defun map-classes (function)
  (with-hash-table-iterator (m %find-classes%)
    (loop
      (multiple-value-bind (found name cell) (m)
        (declare (optimize speed) (type class-cell cell))
        (unless found (return))
        (when cell
          (funcall function name (class-cell-class cell)))))))



(defun %class-primary-slot-accessor-info (class accessor-or-slot-name &optional create?)
  (let ((info-list (%class-get class '%class-primary-slot-accessor-info)))
    (or (car (member accessor-or-slot-name info-list
                     :key #'(lambda (x) (%slot-accessor-info.accessor x))))
        (and create?
             (let ((info (%cons-slot-accessor-info class accessor-or-slot-name)))
               (setf (%class-get class '%class-primary-slot-accessor-info)
                     (cons info info-list))
               info)))))

;;; Clear the %class.primary-slot-accessor-info for an added or
;;; removed method's specializers
(defun clear-accessor-method-offsets (gf method)
  (when (or (typep method 'standard-accessor-method)
            (member 'standard-accessor-method
                    (%gf-methods gf)
                    :test #'(lambda (sam meth)
                             (declare (ignore sam))
                             (typep meth 'standard-accessor-method))))
    (labels ((clear-class (class)
               (when (typep class 'standard-class)
                 (let ((info (%class-primary-slot-accessor-info class gf)))
                   (when info
                     (setf (%slot-accessor-info.offset info) nil)))
                 (mapc #'clear-class (%class.subclasses class)))))
      (declare (dynamic-extent #'clear-class))
      (mapc #'clear-class (%method-specializers method)))))

;;; Remove methods which specialize on a sub-class of method's
;;; specializers from the generic-function dispatch-table dt.
(defun remove-obsoleted-combined-methods (method &optional dt
                                                 (specializers (%method-specializers method)))
  (without-interrupts
   (unless dt
     (let ((gf (%method-gf method)))
       (when gf (setq dt (%gf-dispatch-table gf)))))
   (when dt
     (if specializers
       (let* ((argnum (%gf-dispatch-table-argnum dt)))
         (when (>= argnum 0)
           (let ((class (nth argnum specializers))
                 (size (%gf-dispatch-table-size dt))
                 (index 0))
             (clear-accessor-method-offsets (%gf-dispatch-table-gf dt) method)
             (if (typep class 'eql-specializer)
                 (setq class (class-of (eql-specializer-object class))))
             (while (%i< index size)
               (let* ((wrapper (%gf-dispatch-table-ref dt index))
                      hash-index-0?
                      (cpl (and wrapper
                                (not (setq hash-index-0?
                                           (eql 0 (%wrapper-hash-index wrapper))))
                                (%inited-class-cpl
                                 (require-type (%wrapper-class wrapper) 'class)))))
                 (when (or hash-index-0? (and cpl (cpl-index class cpl)))
                   (setf (%gf-dispatch-table-ref dt index) *obsolete-wrapper*
                         (%gf-dispatch-table-ref dt (%i+ index 1)) *gf-dispatch-bug*))
                 (setq index (%i+ index 2)))))))
       (setf (%gf-dispatch-table-ref dt 1) nil)))))   ; clear 0-arg gf cm

;;; SETQ'd below after the GF's exist.
(defvar *initialization-invalidation-alist* nil)

;;; Called by %add-method, %remove-method
(defun invalidate-initargs-vector-for-gf (gf &optional first-specializer &rest other-specializers)
  (declare (ignore other-specializers))
  (when (and first-specializer (typep first-specializer 'class)) ; no eql methods or gfs with no specializers need apply
    (let ((indices (cdr (assq gf *initialization-invalidation-alist*))))
      (when indices
        (labels ((invalidate (class indices)
                   (when (std-class-p class) ; catch the class named T
                     (dolist (index indices)
                       (setf (standard-instance-instance-location-access class index) nil)))
                   (dolist (subclass (%class.subclasses class))
                     (invalidate subclass indices))))
          (invalidate first-specializer indices))))))

;;; Return two values:
;;; 1) the index of the first non-T specializer of method, or NIL if
;;;    all the specializers are T or only the first one is T
;;; 2) the index of the first non-T specializer
(defun multi-method-index (method &aux (i 0) index)
  (dolist (s (%method.specializers method) (values nil index))
    (unless (eq s *t-class*)
      (unless index (setq index i))
      (unless (eql i 0) (return (values index index))))
    (incf i)))

(defun %remove-standard-method-from-containing-gf (method)
  (setq method (require-type method 'standard-method))
  (let ((gf (%method-gf method)))
    (when gf
      (let* ((dt (%gf-dispatch-table gf))
	     (methods (sgf.methods gf)))
        (setf (%method-gf method) nil)
	(setq methods (nremove method methods))
        (setf (%gf-dispatch-table-methods dt) methods
	      (sgf.methods gf) methods)
        (%remove-direct-methods method)
        (remove-obsoleted-combined-methods method dt)
        (apply #'invalidate-initargs-vector-for-gf gf (%method-specializers method))
        (compute-dcode gf dt)
	(when (sgf.dependents gf)
	  (map-dependents
	   gf
	   #'(lambda (d)
	       (update-dependent gf d 'remove-method method)))))))
  method)


(defvar *reader-method-function-proto*
  #'(lambda (instance)
      (slot-value instance 'x)))


(defvar *writer-method-function-proto*
  #'(lambda (new instance)
      (set-slot-value instance 'x new)))

(defun dcode-for-gf (gf dcode)
  (if *uniquify-dcode*
    (let ((new-dcode (%copy-function dcode)))
      (lfun-name new-dcode (list (lfun-name dcode) (lfun-name gf)))
      new-dcode)
    dcode))

(defstatic *non-dt-dcode-functions* () "List of functions which return a dcode function for the GF which is their argument.  The dcode functions will be caled with all of the incoming arguments.")

(defun non-dt-dcode-function (gf)
  (dolist (f *non-dt-dcode-functions*)
    (let* ((dcode (funcall f gf)))
      (when dcode (return dcode)))))

(defun compute-dcode (gf &optional dt)
  (setq gf (require-type gf 'standard-generic-function))
  (unless dt (setq dt (%gf-dispatch-table gf)))
  (let* ((methods (%gf-dispatch-table-methods dt))
         (bits (inner-lfun-bits gf))
         (nreq (ldb $lfbits-numreq bits))
         (0-args? (eql 0 nreq))
         (other-args? (or (not (eql 0 (ldb $lfbits-numopt bits)))
                          (logbitp $lfbits-rest-bit bits)
                          (logbitp $lfbits-restv-bit bits)
                          (logbitp $lfbits-keys-bit bits)
                          (logbitp $lfbits-aok-bit bits)))
         multi-method-index 
	 min-index)
    (when methods
      (unless 0-args?
        (dolist (m methods)
          (multiple-value-bind (mm-index index) (multi-method-index m)
            (when mm-index
              (if (or (null multi-method-index) (< mm-index multi-method-index))
                (setq multi-method-index mm-index)))
            (when index
              (if (or (null min-index) (< index min-index))
                (setq min-index index))))))
      (let* ((non-dt (non-dt-dcode-function gf))
             (dcode (or non-dt
                        (if 0-args?
                          #'%%0-arg-dcode
                          (or (if multi-method-index
                                #'%%nth-arg-dcode)
                              (if (null other-args?)
                                (if (eql nreq 1)
                                  #'%%one-arg-dcode
                                  (if (eql nreq 2)
                                    #'%%1st-two-arg-dcode
                                    #'%%1st-arg-dcode))
                                #'%%1st-arg-dcode))))))
        (setq multi-method-index
              (if multi-method-index
                (if min-index
                  (min multi-method-index min-index)
                  multi-method-index)
                0))
        (let* ((old-dcode (%gf-dcode (find-unencapsulated-definition gf))))
          (when (or non-dt
		    (neq dcode old-dcode)
                    (neq multi-method-index (%gf-dispatch-table-argnum dt)))
            (clear-gf-dispatch-table dt)
            (setf (%gf-dispatch-table-argnum dt) multi-method-index)
            (if (function-encapsulated-p gf)
	      (%set-encapsulated-gf-dcode gf dcode)
	      (setf (%gf-dcode gf) dcode))))
        (values dcode multi-method-index)))))

(defun inherits-from-standard-generic-function-p (class)
  (memq *standard-generic-function-class*
        (%inited-class-cpl (require-type class 'class))))

;;;;;;;;;;; The type system needs to get wedged into CLOS fairly early ;;;;;;;


;;; Could check for duplicates, but not really worth it.  They're all
;;; allocated here
(defun new-type-class (name)
  (let* ((class (%istruct 
                 'type-class 
                 name
                 #'missing-type-method
                 nil
                 nil
                 #'(lambda (x y) (hierarchical-union2 x y))
                 nil
                 #'(lambda (x y) (hierarchical-intersection2 x y))
                 nil
                 #'missing-type-method
                 nil
                 #'missing-type-method)))
    (push (cons name class) *type-classes*)
    class))

;; There are ultimately about a dozen entries on this alist.
(defvar *type-classes* nil)
(declaim (special *wild-type* *empty-type* *universal-type*))
(defvar *type-kind-info* (make-hash-table :test #'equal))

(defun info-type-kind (name)
  (gethash name *type-kind-info*))

(defun (setf info-type-kind) (val name)
  (if val
    (setf (gethash name *type-kind-info*) val)
    (remhash name *type-kind-info*)))

(defun missing-type-method (&rest foo)
  (error "Missing type method for ~S" foo))
          
(new-type-class 'values)
(new-type-class 'function)
(new-type-class 'constant)
(new-type-class 'wild)
(new-type-class 'bottom)
(new-type-class 'named)
(new-type-class 'hairy)
(new-type-class 'unknown)
(new-type-class 'number)
(new-type-class 'array)
(new-type-class 'member)
(new-type-class 'union)
(new-type-class 'foreign)
(new-type-class 'cons)
(new-type-class 'intersection)
(new-type-class 'negation)
(defparameter *class-type-class* (new-type-class 'class))




                        
;;;;;;;;;;;;;;;;;;;;;;;;  Instances and classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline non-standard-instance-class-wrapper))

(defun non-standard-instance-class-wrapper (instance)
  (let* ((typecode (typecode instance)))
    (declare (type (unsigned-byte 8) typecode))
    (cond ((eql typecode target::subtag-struct)
           (%class.own-wrapper
            (class-cell-class (car (%svref instance 0)))))
          ((eql typecode target::subtag-istruct)
           (istruct-cell-info (%svref instance 0)))
          ((eql typecode target::subtag-basic-stream)
           (basic-stream.wrapper instance))
          ((typep instance 'funcallable-standard-object)
           (gf.instance.class-wrapper instance))
          ((eql typecode target::subtag-macptr) (foreign-instance-class-wrapper instance))
          (t (%class.own-wrapper (class-of instance))))))

(defun instance-class-wrapper (instance)
  (if (= (typecode instance)  target::subtag-instance)
    (instance.class-wrapper instance)
    (non-standard-instance-class-wrapper instance)))


(defun std-instance-class-cell-typep (form class-cell)
  (let* ((typecode (typecode form))
         (wrapper (cond ((= typecode target::subtag-instance)
                         (instance.class-wrapper form))
                        ((= typecode target::subtag-basic-stream)
                         (basic-stream.wrapper form))
                        (t nil))))
    (declare (type (unsigned-byte 8) typecode))
    (when wrapper
      (loop
        (let ((class (class-cell-class class-cell)))
          (if class
            (let* ((ordinal (%class-ordinal class))
                   (bits (or (%wrapper-cpl-bits wrapper)
                             (make-cpl-bits (%inited-class-cpl (%wrapper-class wrapper))))))
              (declare (fixnum ordinal))
              (return
                (if bits
                  (locally (declare (simple-bit-vector bits)
                                    (optimize (speed 3) (safety 0)))
                    (if (< ordinal (length bits))
                      (not (eql 0 (sbit bits ordinal))))))))
            (let* ((name (class-cell-name class-cell))
                   (new-cell (find-class-cell name nil)))
              (unless
                  (if (and new-cell (not (eq class-cell new-cell)))
                    (setq class-cell new-cell class (class-cell-class class-cell))
                    (return (typep form name)))))))))))

(defun class-cell-typep (form class-cell)
  (locally (declare (type class-cell  class-cell))
    (loop
    (let ((class (class-cell-class class-cell)))
      (if class
        (let* ((ordinal (%class-ordinal class))
               (wrapper (instance-class-wrapper form))
               (bits (or (%wrapper-cpl-bits wrapper)
                         (make-cpl-bits (%inited-class-cpl (%wrapper-class wrapper))))))
          (declare (fixnum ordinal))
          (return
            (if bits
              (locally (declare (simple-bit-vector bits)
                                (optimize (speed 3) (safety 0)))
                  (if (< ordinal (length bits))
                    (not (eql 0 (sbit bits ordinal))))))))
        (let* ((name (class-cell-name class-cell))
               (new-cell (find-class-cell name nil)))
          (unless
              (if (and new-cell (not (eq class-cell new-cell)))
                (setq class-cell new-cell class (class-cell-class class-cell))
                (return (typep form name))))))))))



(defun %require-type-class-cell (arg class-cell)
  (if (class-cell-typep arg class-cell)
    arg
    (%kernel-restart $xwrongtype arg (car class-cell))))




(defun find-class (name &optional (errorp t) environment)
  (declare (optimize speed))
  (let* ((cell (find-class-cell name nil)))
    (declare (type class-cell cell))
    (or (and cell (class-cell-class cell))
        (let ((defenv (and environment (definition-environment environment))))
          (when defenv
            (dolist (class (defenv.classes defenv))
              (when (eq name (%class.name class))
                (return class)))))
        (when (or errorp (not (symbolp name)))
          (cerror "Try finding the class again"
                  "Class named ~S not found." name)
          (find-class name errorp environment)))))

(fset 'pessimize-make-instance-for-class-name ;; redefined later
      (qlfun bootstrapping-pessimize-make-instance-for-class-name (name) name))

(defun update-class-proper-names (name old-class new-class)
  (when name
    (pessimize-make-instance-for-class-name name))
  (when (and old-class
             (not (eq old-class new-class))
             (eq (%class-proper-name old-class) name))
    (setf (%class-proper-name old-class) nil))
  (when (and new-class (eq (%class-name new-class) name))
    (setf (%class-proper-name new-class) name)))


(fset 'set-find-class (nfunction bootstrapping-set-find-class ; redefined below
                                 (lambda (name class)
                                   (clear-type-cache)
                                   (let* ((cell (find-class-cell name t))
                                          (old-class (class-cell-class cell)))
                                     (when class
                                       (if (eq name (%class.name class))
                                         (setf (info-type-kind name) :instance)))
                                     (setf (class-cell-class cell) class)
                                     (update-class-proper-names name old-class class)
                                     class))))


;;; bootstrapping definition. real one is in "sysutils.lisp"
(fset 'built-in-type-p (nfunction boostrapping-built-in-typep-p
                                  (lambda (name)
                                    (or (type-predicate name)
                                        (memq name '(signed-byte unsigned-byte mod 
                                                     values satisfies member and or not))
                                        (typep (find-class name nil) 'built-in-class)))))



(defun %compile-time-defclass (name environment)
  (note-type-info name 'class environment)
  (unless (find-class name nil environment)
    (let ((defenv (definition-environment environment)))
      (when defenv
        (push (make-instance 'compile-time-class :name name)
              (defenv.classes defenv)))))
  name)

(eval-when (:compile-toplevel :execute)
(declaim (inline standard-instance-p))
)




(defun standard-instance-p (i)
  (eq (typecode i) target::subtag-instance))

(defun check-setf-find-class-protected-class (old-class new-class name)
  (when (and (standard-instance-p old-class)
	     (%class-kernel-p old-class)
	     *warn-if-redefine-kernel*
	     ;; EQL might be necessary on foreign classes
	     (not (eq new-class old-class)))
    (cerror "Setf (FIND-CLASS ~s) to the new class."
	    "The class name ~s currently denotes the class ~s that
marked as being a critical part of the system; an attempt is being made
to replace that class with ~s" name old-class new-class)
    (setf (%class-kernel-p old-class) nil)))


(queue-fixup
 (defun set-find-class (name class)
   (setq name (require-type name 'symbol))
   (let* ((cell (find-class-cell name t))
          (old-class (class-cell-class cell)))
     (declare (type class-cell cell))
     (when old-class
       (when (eq (%class.name old-class) name)
         (setf (info-type-kind name) nil)
         (clear-type-cache))
       (when *warn-if-redefine-kernel*
         (check-setf-find-class-protected-class old-class class name)))
     (when (null class)
       (when cell
         (setf (class-cell-class cell) nil))
       (update-class-proper-names name old-class class)
       (return-from set-find-class nil))
     (setq class (require-type class 'class))
     (when (built-in-type-p name)
       (unless (eq (class-cell-class cell) class)
         (error "Cannot redefine built-in type name ~S" name)))
     (when (eq (%class.name class) name)
       (when (%deftype-expander name)
         (cerror "set ~S anyway, removing the ~*~S definition"
                 "Cannot set ~S because type ~S is already defined by ~S"
                 `(find-class ',name) name 'deftype)
         (%deftype name nil nil))
       (setf (info-type-kind name) :instance))
     (update-class-proper-names name old-class class)
     (setf (class-cell-class cell) class)))
 )                                      ; end of queue-fixup



#||
; This tended to cluster entries in gf dispatch tables too much.
(defvar *class-wrapper-hash-index* 0)
(defun new-class-wrapper-hash-index ()
  (let ((index *class-wrapper-hash-index*))
    (setq *class-wrapper-hash-index*
        (if (< index (- most-positive-fixnum 2))
          ; Increment by two longwords.  This is important!
          ; The dispatch code will break if you change this.
          (%i+ index 3)                 ; '3 = 24 bytes = 6 longwords in lap.
          1))))
||#

(defglobal *next-class-ordinal* 0)

(defun %next-class-ordinal ()
  (%atomic-incf-node 1 '*next-class-ordinal* target::symbol.vcell))

;;; Initialized after built-in-class is made
(defvar *built-in-class-wrapper* nil)

(defun make-class-ctype (class)
  (%istruct 'class-ctype *class-type-class* nil class nil))

(defun %class-ordinal (class &optional no-error)
  (if (standard-instance-p class)
    (instance.hash class)
    (if (typep class 'macptr)
      (foreign-class-ordinal class)
      (unless no-error
        (error "Can't determine ordinal of ~s" class)))))

(defun (setf %class-ordinal) (new class &optional no-error)
  (if (standard-instance-p class)
    (setf (instance.hash class) new)
    (if (typep class 'macptr)
      (setf (foreign-class-ordinal class) new)
      (unless no-error
        (error "Can't set ordinal of class ~s to ~s" class new)))))


(defvar *t-class* (let* ((class (%cons-built-in-class 't)))
                    (setf (instance.hash class) 0)
                    (let* ((cpl (list class))
                           (wrapper (%cons-wrapper class (new-class-wrapper-hash-index))))
                      (setf (%class.cpl class) cpl)
                      (setf (%wrapper-cpl wrapper) cpl
                            (%class.own-wrapper class) wrapper
                            (%wrapper-cpl-bits wrapper)
                            (let* ((bv (make-array 1 :element-type 'bit)))
                                     (setf (aref bv 0) 1)
                                     bv))
                      (setf (%class.ctype class) (make-class-ctype class))
                      (setf (find-class 't) class)
                      class)))

(defun compute-cpl (class)
  (flet ((%real-class-cpl (class)
           (or (%class-cpl class)
               (compute-cpl class))))
    (let* ((predecessors (list (list class))) candidates cpl)
      (dolist (sup (%class-direct-superclasses class))
        (when (symbolp sup) (report-bad-arg sup 'class))
        (dolist (sup (%real-class-cpl sup))
          (unless (assq sup predecessors) (push (list sup) predecessors))))
      (labels ((compute-predecessors (class table)
                 (dolist (sup (%class-direct-superclasses class) table)
                   (compute-predecessors sup table)
                   ;(push class (cdr (assq sup table)))
                   (let ((a (assq sup table))) (%rplacd a (cons class (%cdr a))))
                   (setq class sup))))
        (compute-predecessors class predecessors))
      (setq candidates (list (assq class predecessors)))
      (while predecessors
        (dolist (c candidates (error "Inconsistent superclasses for ~d" class))
          (when (null (%cdr c))
            (setq predecessors (nremove c predecessors))
            (dolist (p predecessors) (%rplacd p (nremove (%car c) (%cdr p))))
            (setq candidates (nremove c candidates))
            (setq cpl (%rplacd c cpl))
            (dolist (sup (%class-direct-superclasses (%car c)))
              (when (setq c (assq sup predecessors)) (push c candidates)))
            (return))))
      (setq cpl (nreverse cpl))
      (do* ((tail cpl (%cdr tail))
            sup-cpl)
           ((null (setq sup-cpl (and (cdr tail) (%real-class-cpl (cadr tail))))))
        (when (equal (%cdr tail) sup-cpl)
          (setf (%cdr tail) sup-cpl)
          (return)))
      cpl)))

(defun make-cpl-bits (cpl)
  (declare (optimize speed))
  (when cpl
    (let* ((max 0))
      (declare (fixnum max))
      (dolist (class cpl)
        (let* ((ordinal (%class-ordinal class)))
          (declare (fixnum ordinal))
          (when (> ordinal max)
            (setq max ordinal))))
      (let* ((bits (make-array (the fixnum (1+ max)) :element-type 'bit)))
        (dolist (class cpl bits)
          (let* ((ordinal (%class-ordinal class)))
            (setf (sbit bits ordinal) 1)))))))

          
(defun make-built-in-class (name &rest supers)
  (if (null supers)
    (setq supers (list *t-class*))
    (do ((supers supers (%cdr supers)))
        ((null supers))
      (when (symbolp (%car supers)) (%rplaca supers (find-class (%car supers))))))
  (let ((class (find-class name nil)))
    (if class
      (progn
        ;Must be debugging.  Give a try at redefinition...
        (dolist (sup (%class.local-supers class))
          (setf (%class.subclasses sup) (nremove class (%class.subclasses sup)))))
      (progn
        (setq class (%cons-built-in-class name))
        (setf (instance.hash class) (%next-class-ordinal))))
    (dolist (sup supers)
      (setf (%class.subclasses sup) (cons class (%class.subclasses sup))))
    (setf (%class.local-supers class) supers)
    (let* ((wrapper (%cons-wrapper class (new-class-wrapper-hash-index)))
           (cpl (compute-cpl class)))
      (setf (%class.cpl class) cpl)
      (setf (%class.own-wrapper class) wrapper)
      (setf (%wrapper-cpl wrapper) cpl
            (%wrapper-cpl-bits wrapper) (make-cpl-bits cpl)
            (%wrapper-class-ordinal wrapper) (%class-ordinal class)))
    (setf (%class.ctype class)  (make-class-ctype class))
    (setf (find-class name) class)
    (dolist (sub (%class.subclasses class))   ; Only non-nil if redefining
      ;Recompute the cpl.
      (apply #'make-built-in-class (%class.name sub) (%class.local-supers sub)))
    class))

(defun make-istruct-class (name &rest supers)
  (let* ((class (apply #'make-built-in-class name supers))
         (cell (register-istruct-cell name)))
    (setf (istruct-cell-info cell) (%class.own-wrapper class))
    class))

;;; This will be filled in below.  Need it defined now as it goes in
;;; the instance.class-wrapper of all the classes that STANDARD-CLASS
;;; inherits from.
(defstatic *standard-class-wrapper* 
  (%cons-wrapper 'standard-class))

(defun make-standard-class (name &rest supers)
  (make-class name *standard-class-wrapper* supers))

(defun make-class (name metaclass-wrapper supers &optional own-wrapper)
  (let ((class (if (find-class name nil)
                 (error "Attempt to remake standard class ~s" name)
                 (%cons-standard-class name metaclass-wrapper))))
    (setf (instance.hash class) (%next-class-ordinal))
    (if (null supers)
      (setq supers (list *standard-class-class*))
      (do ((supers supers (cdr supers))
           sup)
          ((null supers))
        (setq sup (%car supers))
        (if (symbolp sup) (setf (%car supers) (setq sup (find-class (%car supers)))))
        #+nil (unless (or (eq sup *t-class*) (std-class-p sup))
          (error "~a is not of type ~a" sup 'std-class))))
    (setf (%class.local-supers class) supers)
    (let ((cpl (compute-cpl class))
          (wrapper (if own-wrapper
                     (progn
                       (setf (%wrapper-class own-wrapper) class)
                       own-wrapper)
                     (%cons-wrapper class))))
      (setf (%class.cpl class) cpl
            (%wrapper-instance-slots wrapper) (vector)
            (%class.own-wrapper class) wrapper
            (%class.ctype class) (make-class-ctype class)
            (%class.slots class) nil
            (%wrapper-class-ordinal wrapper) (%class-ordinal class)
            (%wrapper-cpl wrapper) cpl
            (%wrapper-cpl-bits wrapper) (make-cpl-bits cpl)
            (find-class name) class
            )
      (dolist (sup supers)
        (setf (%class.subclasses sup) (cons class (%class.subclasses sup))))
      class)))





(defun standard-object-p (thing)
  ;; returns thing's class-wrapper or nil if it isn't a standard-object
  (if (standard-instance-p thing)
    (instance.class-wrapper thing)
    (if (typep thing 'macptr)
      (foreign-instance-class-wrapper thing))))


(defun std-class-p (class)
  ;; (typep class 'std-class)
  ;; but works at bootstrapping time as well
  (let ((wrapper (standard-object-p class)))
    (and wrapper
         (or (eq wrapper *standard-class-wrapper*)
             (memq *std-class-class* (%inited-class-cpl (%wrapper-class wrapper) t))))))

(set-type-predicate 'std-class 'std-class-p)

(defun slots-class-p (class)
  (let ((wrapper (standard-object-p class)))
    (and wrapper
         (or (eq wrapper *slots-class-wrapper*)
             (memq *slots-class* (%inited-class-cpl (%wrapper-class wrapper) t))))))  

(set-type-predicate 'slots-class 'slots-class-p)

(defun specializer-p (thing)
  (memq *specializer-class* (%inited-class-cpl (class-of thing))))

(defstatic *standard-object-class* (make-standard-class 'standard-object *t-class*))

(defstatic *metaobject-class* (make-standard-class 'metaobject *standard-object-class*))

(defstatic *specializer-class* (make-standard-class 'specializer *metaobject-class*))
(defstatic *eql-specializer-class* (make-standard-class 'eql-specializer *specializer-class*))

(defstatic *standard-method-combination*
  (make-instance-vector
   (%class.own-wrapper
    (make-standard-class
     'standard-method-combination
     (make-standard-class 'method-combination *metaobject-class*)))
   1))


(defun eql-specializer-p (x)
  (memq *eql-specializer-class* (%inited-class-cpl (class-of x))))

(setf (type-predicate 'eql-specializer) 'eql-specializer-p)

;;; The *xxx-class-class* instances get slots near the end of this file.
(defstatic *class-class* (make-standard-class 'class *specializer-class*))

(defstatic *slots-class* (make-standard-class 'slots-class *class-class*))
(defstatic *slots-class-wrapper* (%class.own-wrapper *slots-class*))


;;; an implementation class that exists so that
;;; standard-class & funcallable-standard-class can have a common ancestor not
;;; shared by anybody but their subclasses.

(defstatic *std-class-class* (make-standard-class 'std-class *slots-class*))

;;; The class of all objects whose metaclass is standard-class. Yow.
(defstatic *standard-class-class* (make-standard-class 'standard-class *std-class-class*))
;;; Replace its wrapper and the circle is closed.
(setf (%class.own-wrapper *standard-class-class*) *standard-class-wrapper*
      (%wrapper-class *standard-class-wrapper*) *standard-class-class*
      (%wrapper-class-ordinal *standard-class-wrapper*) (%class-ordinal *standard-class-class*)
      (%wrapper-instance-slots *standard-class-wrapper*) (vector))

(defstatic *built-in-class-class* (make-standard-class 'built-in-class *class-class*))
(setf *built-in-class-wrapper* (%class.own-wrapper *built-in-class-class*)
      (instance.class-wrapper *t-class*) *built-in-class-wrapper*)

(defstatic *structure-class-class* (make-standard-class 'structure-class *slots-class*))
(defstatic *structure-class-wrapper* (%class.own-wrapper *structure-class-class*))
(defstatic *structure-object-class* 
  (make-class 'structure-object *structure-class-wrapper* (list *t-class*)))

(defstatic *forward-referenced-class-class*
  (make-standard-class 'forward-referenced-class *class-class*))

(defstatic *function-class* (make-built-in-class 'function))

(defun alias-class (name class)
  (setf (find-class name) class
        (info-type-kind name) :instance)
  class)

;;;Right now, all functions are compiled.


(defstatic *compiled-function-class* *function-class*)
(alias-class 'compiled-function *compiled-function-class*)

(defstatic *compiled-lexical-closure-class* 
  (make-standard-class 'compiled-lexical-closure *function-class*))





(defstatic *funcallable-standard-class-class*
  (make-standard-class 'funcallable-standard-class *std-class-class*))

(defstatic *funcallable-standard-object-class*
  (make-class 'funcallable-standard-object
              (%class.own-wrapper *funcallable-standard-class-class*)
              (list *standard-object-class* *function-class*)))

(defstatic *generic-function-class*
  (make-class 'generic-function
              (%class.own-wrapper *funcallable-standard-class-class*)
              (list *metaobject-class* *funcallable-standard-object-class*)))
(setq *generic-function-class-wrapper* (%class.own-wrapper *generic-function-class*))

(defstatic *standard-generic-function-class*
  (make-class 'standard-generic-function
              (%class.own-wrapper *funcallable-standard-class-class*)
              (list *generic-function-class*)))
(setq *standard-generic-function-class-wrapper*
      (%class.own-wrapper *standard-generic-function-class*))

;;; *standard-method-class* is upgraded to a real class below
(defstatic *method-class* (make-standard-class 'method *metaobject-class*))
(defstatic *standard-method-class* (make-standard-class 'standard-method *method-class*))
(defstatic *accessor-method-class* (make-standard-class 'standard-accessor-method *standard-method-class*))
(defstatic *standard-reader-method-class* (make-standard-class 'standard-reader-method *accessor-method-class*))
(defstatic *standard-writer-method-class* (make-standard-class 'standard-writer-method *accessor-method-class*))
(defstatic *method-function-class* (make-standard-class 'method-function *function-class*))


(defstatic *combined-method-class* (make-standard-class 'combined-method *function-class*))

(defstatic *slot-definition-class* (make-standard-class 'slot-definition *metaobject-class*))
(defstatic direct-slot-definition-class (make-standard-class 'direct-slot-definition
                                                           *slot-definition-class*))
(defstatic effective-slot-definition-class (make-standard-class 'effective-slot-definition
                                                              *slot-definition-class*))
(defstatic *standard-slot-definition-class* (make-standard-class 'standard-slot-definition
                                                                 *slot-definition-class*))
(defstatic *standard-direct-slot-definition-class* (make-class
                                                    'standard-direct-slot-definition
                                                    *standard-class-wrapper*
                                                    (list
                                                     *standard-slot-definition-class*
                                                     direct-slot-definition-class)))

(defstatic *standard-effective-slot-definition-class* (make-class
                                                    'standard-effective-slot-definition
                                                    *standard-class-wrapper*
                                                    (list
                                                     *standard-slot-definition-class*
                                                     effective-slot-definition-class)
))

(defstatic *standard-effective-slot-definition-class-wrapper*
  (%class.own-wrapper *standard-effective-slot-definition-class*))





  

(let ((*dont-find-class-optimize* t)
      (ordinal-type-class-alist ())
      (ordinal-type-class-alist-lock (make-lock)))

  (declare (optimize speed)) ;; make sure everything gets inlined that needs to be.

;; The built-in classes.
  (defstatic *array-class* (make-built-in-class 'array))
  (defstatic *character-class* (make-built-in-class 'character))
  (make-built-in-class 'number)
  (make-built-in-class 'sequence)
  (defstatic *symbol-class* (make-built-in-class 'symbol))
  (defstatic *immediate-class* (make-built-in-class 'immediate)) ; Random immediate
  ;; Random uvectors - these are NOT class of all things represented by a uvector
  ;;type. Just random uvectors which don't fit anywhere else.
  (make-built-in-class 'ivector)        ; unknown ivector
  (make-built-in-class 'gvector)        ; unknown gvector
  (defstatic *istruct-class* (make-built-in-class 'internal-structure)) ; unknown istruct
  
  (defstatic *slot-vector-class* (make-built-in-class 'slot-vector (find-class 'gvector)))
  
  (defstatic *macptr-class* (make-built-in-class 'macptr))
  (defstatic *foreign-standard-object-class*
    (make-standard-class 'foreign-standard-object
                         *standard-object-class* *macptr-class*))

  (defstatic *foreign-class-class*
    (make-standard-class 'foreign-class *foreign-standard-object-class* *slots-class*))
  
  (make-built-in-class 'population)
  (make-built-in-class 'pool)
  (make-built-in-class 'package)
  (defstatic *lock-class* (make-built-in-class 'lock))
  (defstatic *recursive-lock-class* (make-built-in-class 'recursive-lock *lock-class*))
  (defstatic *read-write-lock-class* (make-built-in-class 'read-write-lock *lock-class*))
  
  (make-istruct-class 'lock-acquisition *istruct-class*)
  (make-istruct-class 'semaphore-notification *istruct-class*)
  (make-istruct-class 'class-wrapper *istruct-class*)
  ;; Compiler stuff, mostly
  (make-istruct-class 'faslapi *istruct-class*)
  (make-istruct-class 'faslstate *istruct-class*)
  (make-istruct-class 'var *istruct-class*)
  (make-istruct-class 'afunc *istruct-class*)
  (make-istruct-class 'lexical-environment *istruct-class*)
  (make-istruct-class 'definition-environment *istruct-class*)
  (make-istruct-class 'compiler-policy *istruct-class*)
  (make-istruct-class 'deferred-warnings *istruct-class*)
  (make-istruct-class 'ptaskstate *istruct-class*)
  (make-istruct-class 'entry *istruct-class*)
  (make-istruct-class 'foreign-object-domain *istruct-class*)

  
  (make-istruct-class 'slot-id *istruct-class*)
  (make-built-in-class 'value-cell)
  (make-istruct-class 'restart *istruct-class*)
  (make-istruct-class 'hash-table *istruct-class*)
  (make-istruct-class 'readtable *istruct-class*)
  (make-istruct-class 'pathname *istruct-class*)
  (make-istruct-class 'random-state *istruct-class*)
  (make-istruct-class 'xp-structure *istruct-class*)
  (make-istruct-class 'lisp-thread *istruct-class*)
  (make-istruct-class 'resource *istruct-class*)
  (make-istruct-class 'periodic-task *istruct-class*)
  (make-istruct-class 'semaphore *istruct-class*)
  
  (make-istruct-class 'type-class *istruct-class*)
  
  (defstatic *ctype-class* (make-istruct-class 'ctype *istruct-class*))
  (make-istruct-class 'key-info *istruct-class*)
  (defstatic *args-ctype* (make-istruct-class 'args-ctype *ctype-class*))
  (make-istruct-class 'values-ctype *args-ctype*)
  (make-istruct-class 'function-ctype *args-ctype*)
  (make-istruct-class 'constant-ctype *ctype-class*)
  (make-istruct-class 'named-ctype *ctype-class*)
  (make-istruct-class 'cons-ctype *ctype-class*)
  (make-istruct-class 'unknown-ctype (make-istruct-class 'hairy-ctype *ctype-class*))
  (make-istruct-class 'numeric-ctype *ctype-class*)
  (make-istruct-class 'array-ctype *ctype-class*)
  (make-istruct-class 'member-ctype *ctype-class*)
  (make-istruct-class 'union-ctype *ctype-class*)
  (make-istruct-class 'foreign-ctype *ctype-class*)
  (make-istruct-class 'class-ctype *ctype-class*)
  (make-istruct-class 'negation-ctype *ctype-class*)
  (make-istruct-class 'intersection-ctype *ctype-class*)
  
  (make-istruct-class 'class-cell *istruct-class*)
  (make-istruct-class 'type-cell *istruct-class*)
  (make-istruct-class 'package-ref *istruct-class*)

  (make-istruct-class 'foreign-variable *istruct-class*)
  (make-istruct-class 'external-entry-point *istruct-class*)
  (make-istruct-class 'shlib *istruct-class*)

  (make-built-in-class 'complex (find-class 'number))
  (make-built-in-class 'real (find-class 'number))
  (defstatic *float-class* (make-built-in-class 'float (find-class 'real)))
  (defstatic *double-float-class* (make-built-in-class 'double-float (find-class 'float)))
  (defstatic *single-float-class*  (make-built-in-class 'single-float (find-class 'float)))
  (alias-class 'short-float *single-float-class*)
  (alias-class 'long-float *double-float-class*)

  (make-built-in-class 'rational (find-class 'real))
  (make-built-in-class 'ratio (find-class 'rational))
  (make-built-in-class 'integer (find-class 'rational))
  (defstatic *fixnum-class* (make-built-in-class 'fixnum (find-class 'integer)))

  #+x86-target
  (defstatic *tagged-return-address-class* (make-built-in-class 'tagged-return-address))
  (make-built-in-class 'bignum (find-class 'integer))
  
  (make-built-in-class 'bit *fixnum-class*)
  (make-built-in-class 'unsigned-byte (find-class 'integer))
  (make-built-In-class 'signed-byte (find-class 'integer))


  (make-istruct-class 'logical-pathname (find-class 'pathname))

  (make-istruct-class 'destructure-state *istruct-class*)
  
  (defstatic *base-char-class* (alias-class 'base-char *character-class*))
  (defstatic *standard-char-class* (make-built-in-class 'standard-char *base-char-class*))
  
  (defstatic *keyword-class* (make-built-in-class 'keyword *symbol-class*))
  
  (make-built-in-class 'list (find-class 'sequence))
  (defstatic *cons-class* (make-built-in-class 'cons (find-class 'list)))
  (defstatic *null-class* (make-built-in-class 'null *symbol-class* (find-class 'list)))
  
  (defstatic *vector-class* (make-built-in-class 'vector *array-class* (find-class 'sequence)))
  (defstatic *simple-array-class* (make-built-in-class 'simple-array *array-class*))
  (make-built-in-class 'simple-1d-array *vector-class* *simple-array-class*)
  
  ;;Maybe should do *float-array-class* etc?
  ;;Also, should straighten out the simple-n-dim-array mess...
  (make-built-in-class 'unsigned-byte-vector *vector-class*)
  (make-built-in-class 'simple-unsigned-byte-vector (find-class 'unsigned-byte-vector) (find-class 'simple-1d-array))
  (make-built-in-class 'unsigned-word-vector *vector-class*)
  (make-built-in-class 'simple-unsigned-word-vector (find-class 'unsigned-word-vector) (find-class 'simple-1d-array))
  (make-built-in-class 'fixnum-vector *vector-class*)
  (make-built-in-class 'simple-fixnum-vector (find-class 'fixnum-vector) (find-class 'simple-1d-array))


  (progn
    (make-built-in-class 'double-float-vector *vector-class*)
    (make-built-in-class 'short-float-vector *vector-class*)
    (alias-class 'long-float-vector (find-class 'double-float-vector))
    (alias-class 'single-float-vector (find-class 'short-float-vector))
    (make-built-in-class 'simple-double-float-vector (find-class 'double-float-vector) (find-class 'simple-1d-array))
    (make-built-in-class 'simple-short-float-vector (find-class 'short-float-vector) (find-class 'simple-1d-array))
    (alias-class 'simple-long-float-vector (find-class 'simple-double-float-vector))
    (alias-class 'simple-single-float-vector (find-class 'simple-short-float-vector))
    )

  #+x8664-target
  (progn
    (make-built-in-class 'symbol-vector (find-class 'gvector))
    (make-built-in-class 'function-vector (find-class 'gvector)))

  #+64-bit-target
  (progn
    (make-built-in-class 'doubleword-vector *vector-class*)
    (make-built-in-class 'simple-doubleword-vector (find-class 'doubleword-vector) (find-class 'simple-1d-array))
    (make-built-in-class 'unsigned-doubleword-vector *vector-class*)
    (make-built-in-class 'simple-unsigned-doubleword-vector (find-class 'unsigned-doubleword-vector) (find-class 'simple-1d-array))
    )                                   ; #+64-bit-target

  (make-built-in-class 'long-vector *vector-class*)
  (make-built-in-class 'simple-long-vector (find-class 'long-vector) (find-class 'simple-1d-array))
  (make-built-in-class 'unsigned-long-vector *vector-class*)
  (make-built-in-class 'simple-unsigned-long-vector (find-class 'unsigned-long-vector) (find-class 'simple-1d-array))
  (make-built-in-class 'byte-vector *vector-class*)
  (make-built-in-class 'simple-byte-vector (find-class 'byte-vector) (find-class 'simple-1d-array))
  (make-built-in-class 'bit-vector *vector-class*)
  (make-built-in-class 'simple-bit-vector (find-class 'bit-vector) (find-class 'simple-1d-array))
  (make-built-in-class 'word-vector *vector-class*)
  (make-built-in-class 'simple-word-vector (find-class 'word-vector) (find-class 'simple-1d-array))
  (make-built-in-class 'string *vector-class*)
  (make-built-in-class 'base-string (find-class 'string))
  (make-built-in-class 'simple-string (find-class 'string) (find-class 'simple-1d-array))
  (make-built-in-class 'simple-base-string (find-class 'base-string) (find-class 'simple-string))
  (make-built-in-class 'general-vector *vector-class*)
  (make-built-in-class 'simple-vector (find-class 'general-vector) (find-class 'simple-1d-array))

  (make-built-in-class 'hash-table-vector)
  (make-built-in-class 'catch-frame)
  (make-built-in-class 'code-vector)
  #+ppc32-target
  (make-built-in-class 'creole-object)

  (make-built-in-class 'xfunction)
  (make-built-in-class 'xcode-vector)

  (defun class-cell-find-class (class-cell errorp)
    (unless (istruct-typep class-cell 'class-cell)
      (setq class-cell (%kernel-restart $xwrongtype class-cell 'class-cell)))
    (locally (declare (type class-cell class-cell))
      (let ((class (class-cell-class class-cell)))
        (or class
            (and 
             (setq class (find-class (class-cell-name class-cell) nil))
             (when class 
               (setf (class-cell-class class-cell) class)
               class))
            (if errorp (error "Class ~s not found." (class-cell-name class-cell)) nil)))))

;;; (%wrapper-class (instance.class-wrapper frob))



  (defstatic *general-vector-class* (find-class 'general-vector))

  #+ppc32-target
  (defparameter *ivector-vector-classes*
    (vector (find-class 'short-float-vector)
            (find-class 'unsigned-long-vector)
            (find-class 'long-vector)
            (find-class 'fixnum-vector)
            (find-class 'base-string)
            (find-class 'unsigned-byte-vector)
            (find-class 'byte-vector)
            *t-class*                   ; old base-string
            (find-class 'unsigned-word-vector)
            (find-class 'word-vector)
            (find-class 'double-float-vector)
            (find-class 'bit-vector)))

  #+ppc64-target
  (defparameter *ivector-vector-classes*
    (vector *t-class*
            *t-class*
            *t-class*
            *t-class*
            (find-class 'byte-vector)
            (find-class 'word-vector)
            (find-class 'long-vector)
            (find-class 'doubleword-vector)
            (find-class 'unsigned-byte-vector)
            (find-class 'unsigned-word-vector)
            (find-class 'unsigned-long-vector)
            (find-class 'unsigned-doubleword-vector)
            *t-class*
            *t-class*
            (find-class 'short-float-vector)
            (find-class 'fixnum-vector)
            *t-class*
            *t-class*
            *t-class*
            (find-class 'double-float-vector)
            (find-class 'base-string)
            *t-class*
            (find-class 'base-string)
            *t-class*
            *t-class*
            *t-class*
            *t-class*
            *t-class*
            *t-class*
            (find-class 'bit-vector)
            *t-class*
            *t-class*))

  #+x8632-target
  (defparameter *ivector-vector-classes*
    (vector (find-class 'short-float-vector)
            (find-class 'unsigned-long-vector)
            (find-class 'long-vector)
            (find-class 'fixnum-vector)
            (find-class 'base-string)
            (find-class 'unsigned-byte-vector)
            (find-class 'byte-vector)
            *t-class*
            (find-class 'unsigned-word-vector)
            (find-class 'word-vector)
            (find-class 'double-float-vector)
            (find-class 'bit-vector)))

  #+x8664-target
  (progn
    (defparameter *immheader-0-classes*
      (vector *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              (find-class 'word-vector)
              (find-class 'unsigned-word-vector)
              (find-class 'base-string) ;old
              (find-class 'byte-vector)
              (find-class 'unsigned-byte-vector)
              (find-class 'bit-vector)))

    (defparameter *immheader-1-classes*
      (vector *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              (find-class 'base-string)
              (find-class 'long-vector)
              (find-class 'unsigned-long-vector)
              (find-class 'short-float-vector)))

    (defparameter *immheader-2-classes*
      (vector *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              *t-class*
              (find-class 'fixnum-vector)
              (find-class 'doubleword-vector)
              (find-class 'unsigned-doubleword-vector)
              (find-class 'double-float-vector))))

  #+arm-target
  (defparameter *ivector-vector-classes*
    (vector (find-class 'short-float-vector)
            (find-class 'unsigned-long-vector)
            (find-class 'long-vector)
            (find-class 'fixnum-vector)
            (find-class 'base-string)
            (find-class 'unsigned-byte-vector)
            (find-class 'byte-vector)
            *t-class*                   ; old base-string
            (find-class 'unsigned-word-vector)
            (find-class 'word-vector)
            (find-class 'double-float-vector)
            (find-class 'bit-vector)))




  (defun make-foreign-object-domain (&key index name recognize class-of classp
                                          instance-class-wrapper
                                          class-own-wrapper
                                          slots-vector class-ordinal
                                          set-class-ordinal)
    (%istruct 'foreign-object-domain index name recognize class-of classp
              instance-class-wrapper class-own-wrapper slots-vector
              class-ordinal set-class-ordinal))
  
  (let* ((n-foreign-object-domains 0)
         (foreign-object-domains (make-array 10))
         (foreign-object-domain-lock (make-lock)))
    (defun register-foreign-object-domain (name
                                           &key
                                           recognize
                                           class-of
                                           classp
                                           instance-class-wrapper
                                           class-own-wrapper
                                           slots-vector
                                           class-ordinal
                                           set-class-ordinal)
      (with-lock-grabbed (foreign-object-domain-lock)
        (dotimes (i n-foreign-object-domains)
          (let* ((already (svref foreign-object-domains i)))
            (when (eq name (foreign-object-domain-name already))
              (setf (foreign-object-domain-recognize already) recognize
                    (foreign-object-domain-class-of already) class-of
                    (foreign-object-domain-classp already) classp
                    (foreign-object-domain-instance-class-wrapper already)
                    instance-class-wrapper
                    (foreign-object-domain-class-own-wrapper already)
                    class-own-wrapper
                    (foreign-object-domain-slots-vector already) slots-vector
                    (foreign-object-domain-class-ordinal already) class-ordinal
                    (foreign-object-domain-set-class-ordinal already)
                    set-class-ordinal)
              (return-from register-foreign-object-domain i))))
        (let* ((i n-foreign-object-domains)
               (new (make-foreign-object-domain :index i
                                                :name name
                                                :recognize recognize
                                                :class-of class-of
                                                :classp classp
                                                :instance-class-wrapper
                                                instance-class-wrapper
                                                :class-own-wrapper
                                                class-own-wrapper
                                                :slots-vector
                                                slots-vector
                                                :class-ordinal class-ordinal
                                                :set-class-ordinal set-class-ordinal)))
          (incf n-foreign-object-domains)
          (if (= i (length foreign-object-domains))
            (setq foreign-object-domains (%extend-vector i foreign-object-domains (* i 2))))
          (setf (svref foreign-object-domains i) new)
          i)))
    (defun foreign-class-of (p)
      (funcall (foreign-object-domain-class-of (svref foreign-object-domains (%macptr-domain p))) p))
    (defun foreign-classp (p)
      (funcall (foreign-object-domain-classp (svref foreign-object-domains (%macptr-domain p))) p))
    (defun foreign-instance-class-wrapper (p)
      (funcall (foreign-object-domain-instance-class-wrapper (svref foreign-object-domains (%macptr-domain p))) p))
    (defun foreign-class-own-wrapper (p)
      (funcall (foreign-object-domain-class-own-wrapper (svref foreign-object-domains (%macptr-domain p))) p))
    (defun foreign-slots-vector (p)
      (funcall (foreign-object-domain-slots-vector (svref foreign-object-domains (%macptr-domain p))) p))
    (defun foreign-class-ordinal (p)
      (funcall (foreign-object-domain-class-ordinal (svref foreign-object-domains (%macptr-domain p))) p))
    (defun (setf foreign-class-ordinal) (new p)
      (funcall (foreign-object-domain-set-class-ordinal (svref foreign-object-domains (%macptr-domain p))) p new))
    (defun classify-foreign-pointer (p)
      (do* ((i (1- n-foreign-object-domains) (1- i)))
           ((zerop i) (error "this can't happen"))
        (when (funcall (foreign-object-domain-recognize (svref foreign-object-domains i)) p)
          (%set-macptr-domain p i)
          (return p)))))

  (defun constantly (x)
    "Return a function that always returns VALUE."
    #'(lambda (&rest ignore)
        (declare (dynamic-extent ignore)
                 (ignore ignore))
        x))

  (defun %register-type-ordinal-class (foreign-type class-name)
    ;; ordinal-type-class shouldn't already exist
    (with-lock-grabbed (ordinal-type-class-alist-lock)
      (or (let* ((class (cdr (assq foreign-type ordinal-type-class-alist))))
            (if (and class (eq class-name (class-name class)))
              class))
          (let* ((class (make-built-in-class class-name 'macptr)))
            (push (cons foreign-type class) ordinal-type-class-alist)
            class))))

  (defun %ordinal-type-class-for-macptr (p)
    (with-lock-grabbed (ordinal-type-class-alist-lock)
      (or (unless (%null-ptr-p p)
            (cdr (assoc (%macptr-type p) ordinal-type-class-alist :key #'foreign-type-ordinal)))
          *macptr-class*)))
                  

  (register-foreign-object-domain :unclassified
                                  :recognize #'(lambda (p)
                                                 (declare (ignore p))
                                                 (error "Shouldn't happen"))
                                  :class-of #'(lambda (p)
                                                (foreign-class-of
                                                 (classify-foreign-pointer p)))
                                  :classp #'(lambda (p)
                                              (foreign-classp
                                               (classify-foreign-pointer p)))
                                  :instance-class-wrapper
                                  #'(lambda (p)
                                      (foreign-instance-class-wrapper
                                       (classify-foreign-pointer p)))
                                  :class-own-wrapper
                                  #'(lambda (p)
                                      (foreign-class-own-wrapper 
                                       (classify-foreign-pointer p)))
                                  :slots-vector
                                  #'(lambda (p)
                                      (foreign-slots-vector
                                       (classify-foreign-pointer p))))

;;; "Raw" macptrs, that aren't recognized as "standard foreign objects"
;;; in some other domain, should always be recognized as such (and this
;;; pretty much has to be domain #1.)

  (register-foreign-object-domain :raw
                                  :recognize #'true
                                  :class-of #'%ordinal-type-class-for-macptr
                                  :classp #'false
                                  :instance-class-wrapper
                                  (lambda (p)
                                    (%class.own-wrapper (%ordinal-type-class-for-macptr p)))
                                  :class-own-wrapper #'false
                                  :slots-vector #'false)

  (defstatic *class-table*
      (let* ((v (make-array 256 :initial-element nil))
             (class-of-function-function
              #'(lambda (thing)
                  (let ((bits (lfun-bits-known-function thing)))
                    (declare (fixnum bits))
                    (if (logbitp $lfbits-trampoline-bit bits)
                      ;; closure
                      (let ((inner-fn (closure-function thing)))
                        (if (neq inner-fn thing)
                          (let ((inner-bits (lfun-bits inner-fn)))
                            (if (logbitp $lfbits-method-bit inner-bits)
                              *compiled-lexical-closure-class*
                              (if (logbitp $lfbits-gfn-bit inner-bits)
                                (%wrapper-class (gf.instance.class-wrapper thing))
                                (if (logbitp $lfbits-cm-bit inner-bits)
                                  *combined-method-class*
                                  *compiled-lexical-closure-class*))))
                          *compiled-lexical-closure-class*))
                      (if (logbitp  $lfbits-method-bit bits)
                        *method-function-class* 
                        (if (logbitp $lfbits-gfn-bit bits)
                          (%wrapper-class (gf.instance.class-wrapper thing))
                          (if (logbitp $lfbits-cm-bit bits)
                            *combined-method-class*
                            *compiled-function-class*))))))))
        ;; Make one loop through the vector, initializing fixnum & list
        ;; cells.  Set all immediates to *immediate-class*, then
        ;; special-case characters later.
        #+ppc32-target
        (do* ((slice 0 (+ 8 slice)))
             ((= slice 256))
          (declare (type (unsigned-byte 8) slice))
          (setf (%svref v (+ slice ppc32::fulltag-even-fixnum)) *fixnum-class*
                (%svref v (+ slice ppc32::fulltag-odd-fixnum))  *fixnum-class*
                (%svref v (+ slice ppc32::fulltag-cons)) *cons-class*
                (%svref v (+ slice ppc32::fulltag-nil)) *null-class*
                (%svref v (+ slice ppc32::fulltag-imm)) *immediate-class*))
        #+ppc64-target
        (do* ((slice 0 (+ 16 slice)))
             ((= slice 256))
          (declare (type (unsigned-byte 8) slice))
          (setf (%svref v (+ slice ppc64::fulltag-even-fixnum)) *fixnum-class*
                (%svref v (+ slice ppc64::fulltag-odd-fixnum))  *fixnum-class*
                (%svref v (+ slice ppc64::fulltag-cons)) *cons-class*
                (%svref v (+ slice ppc64::fulltag-imm-0)) *immediate-class*
                (%svref v (+ slice ppc64::fulltag-imm-1)) *immediate-class*
                (%svref v (+ slice ppc64::fulltag-imm-2)) *immediate-class*
                (%svref v (+ slice ppc64::fulltag-imm-3)) *immediate-class*))
        #+x8632-target
        (do* ((slice 0 (+ 8 slice))
	      (cons-fn #'(lambda (x) (if (null x) *null-class* *cons-class*))))
             ((= slice 256))
          (declare (type (unsigned-byte 8) slice))
          (setf (%svref v (+ slice x8632::fulltag-even-fixnum)) *fixnum-class*
                (%svref v (+ slice x8632::fulltag-odd-fixnum))  *fixnum-class*
                (%svref v (+ slice x8632::fulltag-cons)) cons-fn
                (%svref v (+ slice x8632::fulltag-tra)) *tagged-return-address-class*
                (%svref v (+ slice x8632::fulltag-imm)) *immediate-class*))
        #+x8664-target
        (do* ((slice 0 (+ 16 slice)))
             ((= slice 256))
          (declare (type (unsigned-byte 8) slice))
          (setf (%svref v (+ slice x8664::fulltag-even-fixnum)) *fixnum-class*
                (%svref v (+ slice x8664::fulltag-odd-fixnum))  *fixnum-class*
                (%svref v (+ slice x8664::fulltag-cons)) *cons-class*
                (%svref v (+ slice x8664::fulltag-imm-0)) *immediate-class*
                (%svref v (+ slice x8664::fulltag-imm-1)) *immediate-class*
                (%svref v (+ slice x8664::fulltag-tra-0)) *tagged-return-address-class*
                (%svref v (+ slice x8664::fulltag-tra-1)) *tagged-return-address-class*
                (%svref v (+ slice x8664::fulltag-nil)) *null-class*))
        #+arm-target
        (do* ((slice 0 (+ 8 slice)))
             ((= slice 256))
          (declare (type (unsigned-byte 8) slice))
          (setf (%svref v (+ slice arm::fulltag-even-fixnum)) *fixnum-class*
                (%svref v (+ slice arm::fulltag-odd-fixnum))  *fixnum-class*
                (%svref v (+ slice arm::fulltag-cons)) *cons-class*
                (%svref v (+ slice arm::fulltag-nil)) *null-class*
                (%svref v (+ slice arm::fulltag-imm)) *immediate-class*))

        (macrolet ((map-subtag (subtag class-name)
                     `(setf (%svref v ,subtag) (find-class ',class-name))))
          ;; immheader types map to built-in classes.
          (map-subtag target::subtag-bignum bignum)
          (map-subtag target::subtag-double-float double-float)
          (map-subtag target::subtag-single-float short-float)
          (map-subtag target::subtag-dead-macptr ivector)
          #+ppc32-target
          (map-subtag ppc32::subtag-code-vector code-vector)
          #+ppc64-target
          (map-subtag ppc64::subtag-code-vector code-vector)
          #+arm-target
          (map-subtag arm::subtag-code-vector code-vector)
          #+ppc32-target
          (map-subtag ppc32::subtag-creole-object creole-object)
          (map-subtag target::subtag-xcode-vector xcode-vector)
          (map-subtag target::subtag-xfunction xfunction)
          (map-subtag target::subtag-single-float-vector simple-short-float-vector)
          #+64-bit-target
          (map-subtag target::subtag-u64-vector simple-unsigned-doubleword-vector)
          #+64-bit-target
          (map-subtag target::subtag-s64-vector simple-doubleword-vector)
          (map-subtag target::subtag-fixnum-vector simple-fixnum-vector)
          (map-subtag target::subtag-u32-vector simple-unsigned-long-vector)
          (map-subtag target::subtag-s32-vector simple-long-vector)
          (map-subtag target::subtag-u8-vector simple-unsigned-byte-vector)
          (map-subtag target::subtag-s8-vector simple-byte-vector)
          (map-subtag target::subtag-simple-base-string simple-base-string)
          (map-subtag target::subtag-u16-vector simple-unsigned-word-vector)
          (map-subtag target::subtag-s16-vector simple-word-vector)
          (map-subtag target::subtag-double-float-vector simple-double-float-vector)
          (map-subtag target::subtag-bit-vector simple-bit-vector)
          ;; Some nodeheader types map to built-in-classes; others require
          ;; further dispatching.
          (map-subtag target::subtag-ratio ratio)
          (map-subtag target::subtag-complex complex)
          (map-subtag target::subtag-catch-frame catch-frame)
          (map-subtag target::subtag-hash-vector hash-table-vector)
          (map-subtag target::subtag-value-cell value-cell)
          (map-subtag target::subtag-pool pool)
          (map-subtag target::subtag-weak population)
          (map-subtag target::subtag-package package)
          (map-subtag target::subtag-simple-vector simple-vector)
          (map-subtag target::subtag-slot-vector slot-vector)
          #+x8664-target (map-subtag x8664::subtag-symbol symbol-vector)
          #+x8664-target (map-subtag x8664::subtag-function function-vector))
        (setf (%svref v target::subtag-arrayH)
              #'(lambda (x)
                  (if (logbitp $arh_simple_bit
                               (the fixnum (%svref x target::arrayH.flags-cell)))
                    *simple-array-class*
                    *array-class*)))
        ;; These need to be special-cased:
        (setf (%svref v target::subtag-macptr) #'foreign-class-of)
        (setf (%svref v target::subtag-character)
              #'(lambda (c) (let* ((code (%char-code c)))
                              (if (or (eq c #\NewLine)
                                      (and (>= code (char-code #\space))
                                           (< code (char-code #\rubout))))
                                *standard-char-class*
                                *base-char-class*))))
        (setf (%svref v target::subtag-struct)
              #'(lambda (s) (%structure-class-of s))) ; need DEFSTRUCT
        (setf (%svref v target::subtag-istruct)
              #'(lambda (i)
                  (let* ((cell (%svref i 0))
                         (wrapper (istruct-cell-info  cell)))
                    (if wrapper
                      (%wrapper-class wrapper)
                      (or (find-class (istruct-cell-name cell) nil)
                          *istruct-class*)))))
        (setf (%svref v target::subtag-basic-stream)
              #'(lambda (b) (%wrapper-class (basic-stream.wrapper b))))
        (setf (%svref v target::subtag-instance)
              #'%class-of-instance)
        (setf (%svref v #+ppc-target target::subtag-symbol
                      #+arm-target target::subtag-symbol
		      #+x8632-target target::subtag-symbol
		      #+x8664-target target::tag-symbol)
              #-ppc64-target
              #'(lambda (s) (if (eq (symbol-package s) *keyword-package*)
                              *keyword-class*
                              *symbol-class*))
              #+ppc64-target
              #'(lambda (s)
                  (if s
                    (if (eq (symbol-package s) *keyword-package*)
                      *keyword-class*
                      *symbol-class*)
                    *null-class*)))
        
        (setf (%svref v
                      #+ppc-target target::subtag-function
                      #+arm-target target::subtag-function
                      #+x8632-target target::subtag-function
                      #+x8664-target target::tag-function) 
              class-of-function-function)
        (setf (%svref v target::subtag-vectorH)
              #'(lambda (v)
                  (let* ((subtype (%array-header-subtype v)))
                    (declare (fixnum subtype))
                    (if (eql subtype target::subtag-simple-vector)
                      *general-vector-class*
                      #-x8664-target
                      (%svref *ivector-vector-classes*
                              #+ppc32-target
                              (ash (the fixnum (- subtype ppc32::min-cl-ivector-subtag))
                                   (- ppc32::ntagbits))
                              #+arm-target
                              (ash (the fixnum (- subtype arm::min-cl-ivector-subtag))
                                   (- arm::ntagbits))
                              #+ppc64-target
                              (ash (the fixnum (logand subtype #x7f)) (- ppc64::nlowtagbits))
			      #+x8632-target
			      (ash (the fixnum (- subtype x8632::min-cl-ivector-subtag))
				   (- x8632::ntagbits)))
                      #+x8664-target
                      (let* ((class (logand x8664::fulltagmask subtype))
                             (idx (ash subtype (- x8664::ntagbits))))
                        (cond ((= class x8664::fulltag-immheader-0)
                               (%svref *immheader-0-classes* idx))
                              ((= class x8664::fulltag-immheader-1)
                               (%svref *immheader-1-classes* idx))
                              ((= class x8664::fulltag-immheader-2)
                               (%svref *immheader-2-classes* idx))
                              (t *t-class*)))
                               
                      ))))
        (setf (%svref v target::subtag-lock)
              #'(lambda (thing)
                  (case (%svref thing target::lock.kind-cell)
                    (recursive-lock *recursive-lock-class*)
                    (read-write-lock *read-write-lock-class*)
                    (t *lock-class*))))
        v))





  (defun no-class-error (x)
    (error "Bug (probably): can't determine class of ~s" x))
  

                                        ; return frob from table




  )                                     ; end let



(defun classp (x)
  (if (%standard-instance-p x)
    (< (the fixnum (instance.hash x)) max-class-ordinal)
    (and (typep x 'macptr) (foreign-classp x))))

(set-type-predicate 'class 'classp)

(defun subclassp (c1 c2)
  (and (classp c1)
       (classp c2)
       (not (null (memq c2 (%inited-class-cpl c1 t))))))

(defun %class-get (class indicator &optional default)
  (let ((cell (assq indicator (%class-alist class))))
    (if cell (cdr cell) default)))

(defun %class-put (class indicator value)
  (let ((cell (assq indicator (%class-alist class))))
    (if cell
      (setf (cdr cell) value)
      (push (cons indicator value) (%class-alist class))))
  value)
  
(defsetf %class-get %class-put)

(defun %class-remprop (class indicator)
  (let* ((handle (cons nil (%class-alist class)))
         (last handle))
    (declare (dynamic-extent handle))
    (while (cdr last)
      (if (eq indicator (caar (%cdr last)))
        (progn
          (setf (%cdr last) (%cddr last))
          (setf (%class-alist class) (%cdr handle)))
        (setf last (%cdr last))))))    


(pushnew :primary-classes *features*)

(defun %class-primary-p (class)
  (if (typep class 'slots-class)
    (%class-get class :primary-p)
    t))

(defun (setf %class-primary-p) (value class)
  (if value
    (setf (%class-get class :primary-p) value)
    (progn
      (%class-remprop class :primary-p)
      nil)))

;;; Returns the first element of the CPL that is primary
(defun %class-or-superclass-primary-p (class)
  (unless (class-has-a-forward-referenced-superclass-p class)
    (dolist (super (%inited-class-cpl class t))
      (when (and (typep super 'standard-class) (%class-primary-p super))
	(return super)))))


;;; Bootstrapping version of union
(unless (fboundp 'union)
  (fset 'union (nlambda bootstrapping-union (l1 l2)
                 (dolist (e l1)
                   (unless (memq e l2)
                     (push e l2)))
                 l2))
)

(defun %add-direct-methods (method)
  (dolist (spec (%method-specializers method))
    (%do-add-direct-method spec method)))

(defun %do-add-direct-method (spec method)
  (pushnew method (specializer.direct-methods spec)))

(defun %remove-direct-methods (method)
  (dolist (spec (%method-specializers method))
    (%do-remove-direct-method spec method)))

(defun %do-remove-direct-method (spec method)
  (setf (specializer.direct-methods spec)
	(nremove method (specializer.direct-methods spec))))

(ensure-generic-function 'initialize-instance
			 :lambda-list '(instance &rest initargs &key &allow-other-keys))

(defmethod find-method ((generic-function standard-generic-function)
                        method-qualifiers specializers &optional (errorp t))
  (dolist (m (%gf-methods generic-function)
	   (when errorp
             (cerror "Try finding the method again"
                     "~s has no method for ~s ~s"
                     generic-function method-qualifiers specializers)
             (find-method generic-function method-qualifiers specializers
                          errorp)))
    (flet ((err ()
	     (error "Wrong number of specializers: ~s" specializers)))
      (let ((ss (%method-specializers m))
	    (q (%method-qualifiers m))
	    s)
	(when (equal q method-qualifiers)
	  (dolist (spec (canonicalize-specializers specializers nil)
		   (if (null ss)
		     (return-from find-method m)
		     (err)))
	    (unless (setq s (pop ss))
	      (err))
	    (unless (eq s spec)
	      (return))))))))

(defmethod create-reader-method-function ((class slots-class)
					  (reader-method-class standard-reader-method)
					  (dslotd direct-slot-definition))
  #+ppc-target
  (gvector :function
           (uvref *reader-method-function-proto* 0)
           (ensure-slot-id (%slot-definition-name dslotd))
           'slot-id-value
           nil				;method-function name
           (dpb 1 $lfbits-numreq (ash 1 $lfbits-method-bit)))
  #+x86-target
  (%clone-x86-function
   *reader-method-function-proto*
   (ensure-slot-id (%slot-definition-name dslotd))
   'slot-id-value
   nil				;method-function name
   (dpb 1 $lfbits-numreq (ash 1 $lfbits-method-bit)))
  #+arm-target
  (gvector :function
           arm::*function-initial-entrypoint*
           (uvref *reader-method-function-proto* 1)
           (ensure-slot-id (%slot-definition-name dslotd))
           'slot-id-value
           nil				;method-function name
           (dpb 1 $lfbits-numreq (ash 1 $lfbits-method-bit))))

(defmethod create-writer-method-function ((class slots-class)
					  (writer-method-class standard-writer-method)
					  (dslotd direct-slot-definition))
  #+ppc-target
  (gvector :function
           (uvref *writer-method-function-proto* 0)
           (ensure-slot-id (%slot-definition-name dslotd))
           'set-slot-id-value
           nil
           (dpb 2 $lfbits-numreq (ash 1 $lfbits-method-bit)))
  #+x86-target
    (%clone-x86-function
     *writer-method-function-proto*
     (ensure-slot-id (%slot-definition-name dslotd))
     'set-slot-id-value
     nil
     (dpb 2 $lfbits-numreq (ash 1 $lfbits-method-bit)))
    #+arm-target
    (gvector :function
             arm::*function-initial-entrypoint*
             (uvref *writer-method-function-proto* 1)
             (ensure-slot-id (%slot-definition-name dslotd))
             'set-slot-id-value
             nil
             (dpb 2 $lfbits-numreq (ash 1 $lfbits-method-bit)))
  )






(defun %make-instance (class-cell &rest initargs)
  (declare (dynamic-extent initargs))
  (declare (optimize speed)) ;; make sure everything gets inlined that needs to be.
  (apply #'make-instance
         (or (class-cell-class class-cell) (class-cell-name  (the class-cell class-cell)))
         initargs))


(defmethod make-instance ((class symbol) &rest initargs)
  (declare (dynamic-extent initargs))
  (apply 'make-instance (find-class class) initargs))


(defmethod make-instance ((class standard-class) &rest initargs &key &allow-other-keys)
  (declare (dynamic-extent initargs))
  (%make-std-instance class initargs))

(defmethod make-instance ((class std-class) &rest initargs &key &allow-other-keys)
  (declare (dynamic-extent initargs))
  (%make-std-instance class initargs))


(defun %make-std-instance (class initargs)
  (setq initargs (default-initargs class initargs))
  (when initargs
    (apply #'check-initargs
           nil class initargs t
           #'initialize-instance #'allocate-instance #'shared-initialize
           nil))
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

(defun default-initargs (class initargs)
  (unless (std-class-p class)
    (setq class (require-type class 'std-class)))
  (when (null (%class.cpl class)) (update-class class t))
  (let ((defaults ()))
    (dolist (key.form (%class-default-initargs class))
      (unless (pl-search initargs (%car key.form))
        (setq defaults
              (list* (funcall (caddr key.form))
                     (%car key.form)
                     defaults))))
    (when defaults
      (setq initargs (append initargs (nreverse defaults))))
    initargs))


(defun %allocate-std-instance (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let* ((wrapper (%class.own-wrapper class))
         (len (length (%wrapper-instance-slots wrapper))))
    (declare (fixnum len))
    (make-instance-vector wrapper len)))




(defmethod copy-instance ((instance standard-object))
  (let* ((new-slots (copy-uvector (instance.slots instance)))
	 (copy (gvector :instance 0 (instance-class-wrapper instance) new-slots)))
    (setf (instance.hash copy) (strip-tag-to-fixnum copy)
	  (slot-vector.instance new-slots) copy)))

(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (declare (dynamic-extent initargs))
  (apply 'shared-initialize instance t initargs))


(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (declare (dynamic-extent initargs))
  (when initargs
    (check-initargs 
     instance nil initargs t #'reinitialize-instance #'shared-initialize))
  (apply 'shared-initialize instance nil initargs))

(defmethod shared-initialize ((instance standard-object) slot-names &rest initargs)
  (declare (dynamic-extent initargs))
  (%shared-initialize instance slot-names initargs))

(defmethod shared-initialize ((instance standard-generic-function) slot-names
                              &rest initargs)
  (declare (dynamic-extent initargs))
  (%shared-initialize instance slot-names initargs))


;;; Slot-value, slot-boundp, slot-makunbound, etc.
(declaim (inline find-slotd))
(defun find-slotd (name slots)
  (dolist (slotd slots)
    (when (eq name (standard-slot-definition.name slotd))
      (return slotd))))

(declaim (inline %std-slot-vector-value))

(defun %std-slot-vector-value (slot-vector slotd)
  (let* ((loc (standard-effective-slot-definition.location slotd)))
    (symbol-macrolet ((instance (slot-vector.instance slot-vector)))
      (typecase loc
	(fixnum
	 (%slot-ref slot-vector loc))
	(cons
	 (let* ((val (%cdr loc)))
	   (if (eq val (%slot-unbound-marker))
	     (slot-unbound (class-of instance) instance (standard-effective-slot-definition.name slotd))
	   val)))
      (t
       (error "Slot definition ~s has invalid location ~s (allocation ~s)."
 	      slotd loc (slot-definition-allocation slotd)))))))


(defmethod slot-value-using-class ((class standard-class)
				   instance
				   (slotd standard-effective-slot-definition))
  (ecase (standard-slot-definition.allocation slotd)
    ((:instance :class)
     (%std-slot-vector-value (instance-slots instance) slotd))))

(defun %maybe-std-slot-value-using-class (class instance slotd)
  (if (and (eql (typecode class) target::subtag-instance)
	   (eql (typecode slotd) target::subtag-instance)
	   (eq *standard-effective-slot-definition-class-wrapper*
	       (instance.class-wrapper slotd))
	   (eq *standard-class-wrapper* (instance.class-wrapper class))
           (let* ((allocation (standard-effective-slot-definition.location slotd)))
             (or (eq allocation :instance) (eq allocation :class))))
    (%std-slot-vector-value (instance-slots instance) slotd)
    (if (= (the fixnum (typecode instance)) target::subtag-struct)
      (struct-ref instance (standard-effective-slot-definition.location slotd))
      (slot-value-using-class class instance slotd))))


(declaim (inline  %set-std-slot-vector-value))

(defun %set-std-slot-vector-value (slot-vector slotd  new)
  (let* ((loc (standard-effective-slot-definition.location slotd))
	 (type (standard-effective-slot-definition.type slotd))
	 (type-predicate (standard-effective-slot-definition.type-predicate slotd)))
    (unless (or (eq new (%slot-unbound-marker))
                (null type-predicate)
		(funcall type-predicate new))
      (error 'bad-slot-type
	     :instance (slot-vector.instance slot-vector)
	     :datum new :expected-type type
	     :slot-definition slotd))
    (typecase loc
      (fixnum
       (setf (%svref slot-vector loc) new))
      (cons
       (setf (%cdr loc) new))
      (t
       (error "Slot definition ~s has invalid location ~s (allocation ~s)."
	      slotd loc (slot-definition-allocation slotd))))))
  
  
(defmethod (setf slot-value-using-class)
    (new
     (class standard-class)
     instance
     (slotd standard-effective-slot-definition))
  (ecase (standard-slot-definition.allocation slotd)
    ((:instance :class)
     (%set-std-slot-vector-value (instance-slots instance) slotd new))))


(defun %maybe-std-setf-slot-value-using-class (class instance slotd new)
  (if (and (eql (typecode class) target::subtag-instance)
	   (eql (typecode slotd) target::subtag-instance)
	   (eq *standard-effective-slot-definition-class-wrapper*
	       (instance.class-wrapper slotd))
	   (eq *standard-class-wrapper* (instance.class-wrapper class))
           (let* ((allocation (standard-effective-slot-definition.allocation slotd)))
             (or (eq allocation :instance) (eq allocation :class))))
    ;; Not safe to use instance.slots here, since the instance is not
    ;; definitely of type SUBTAG-INSTANCE.  (Anyway, INSTANCE-SLOTS
    ;; should be inlined here.)
    (%set-std-slot-vector-value (instance-slots instance) slotd new)
    (if (structurep instance)
      (setf (struct-ref instance (standard-effective-slot-definition.location slotd))
            new)
      (setf (slot-value-using-class class instance slotd) new))))

(defmethod slot-value-using-class ((class funcallable-standard-class)
				   instance
				   (slotd standard-effective-slot-definition))
  (%std-slot-vector-value (gf.slots instance) slotd))

(defmethod (setf slot-value-using-class)
    (new
     (class funcallable-standard-class)
     instance
     (slotd standard-effective-slot-definition))
  (%set-std-slot-vector-value (gf.slots instance) slotd new))

(defun slot-value (instance slot-name)
  (let* ((wrapper
          (let* ((w (instance-class-wrapper instance)))
            (if (eql 0 (%wrapper-hash-index w))
              (instance.class-wrapper (update-obsolete-instance instance))
              w)))
         (class (%wrapper-class wrapper))
         (slotd (find-slotd slot-name (if (%standard-instance-p class)
                                        (%class.slots class)
                                        (class-slots class)))))
    (if slotd
      (%maybe-std-slot-value-using-class class instance slotd)
      (if (typep slot-name 'symbol)
        (restart-case
         (values (slot-missing class instance slot-name 'slot-value))
         (continue ()
                   :report "Try accessing the slot again"
                   (slot-value instance slot-name))
         (use-value (value)
                    :report "Return a value"
                    :interactive (lambda ()
                                   (format *query-io* "~&Value to use: ")
                                   (list (read *query-io*)))
                    value))
        (report-bad-arg slot-name 'symbol)))))


(defmethod slot-unbound (class instance slot-name)
  (declare (ignore class))
  (restart-case (error 'unbound-slot :name slot-name :instance instance)
    (use-value (value)
      :report "Return a value"
      :interactive (lambda ()
                     (format *query-io* "~&Value to use: ")
                     (list (read *query-io*)))
      value)))



(defmethod slot-makunbound-using-class ((class slots-class)
					instance
					(slotd standard-effective-slot-definition))
  (setf (slot-value-using-class class instance slotd) (%slot-unbound-marker))
  instance)

(defmethod slot-missing (class object slot-name operation &optional new-value)
  (declare (ignore class operation new-value))
  (error "~s has no slot named ~s." object slot-name))


(defun set-slot-value (instance name value)
  (let* ((wrapper
          (let* ((w (instance-class-wrapper instance)))
            (if (eql 0 (%wrapper-hash-index w))
              (instance.class-wrapper (update-obsolete-instance instance))
              w)))
         (class (%wrapper-class wrapper))
         (slotd (find-slotd name (if (%standard-instance-p class)
                                   (%class.slots class)
                                   (class-slots class)))))
    (if slotd
      (%maybe-std-setf-slot-value-using-class class instance slotd value)
      (if (typep name 'symbol)
        (progn	    
          (slot-missing class instance name 'setf value)
          value)
        (report-bad-arg name 'symbol)))))

(defsetf slot-value set-slot-value)

(defun slot-makunbound (instance name)
  (let* ((class (class-of instance))
	 (slotd (find-slotd name (%class-slots class))))
    (if slotd
      (slot-makunbound-using-class class instance slotd)
      (slot-missing class instance name 'slot-makunbound))
    instance))

(defun %std-slot-vector-boundp (slot-vector slotd)
  (let* ((loc (standard-effective-slot-definition.location slotd)))
    (typecase loc
      (fixnum
       (not (eq (%svref slot-vector loc) (%slot-unbound-marker))))
      (cons
       (not (eq (%cdr loc) (%slot-unbound-marker))))
      (t
       (error "Slot definition ~s has invalid location ~s (allocation ~s)."
		slotd loc (slot-definition-allocation slotd))))))

(defun %maybe-std-slot-boundp-using-class (class instance slotd)
  (if (and (eql (typecode class) target::subtag-instance)
	   (eql (typecode slotd) target::subtag-instance)
	   (eq *standard-effective-slot-definition-class-wrapper*
	       (instance.class-wrapper slotd))
	   (eq *standard-class-wrapper* (instance.class-wrapper class))
           (let* ((allocation (standard-slot-definition.allocation slotd)))
             (or (eq allocation :class)
                 (eq allocation :instance))))
    (%std-slot-vector-boundp (instance-slots instance) slotd)
    (slot-boundp-using-class class instance slotd)))


(defmethod slot-boundp-using-class ((class standard-class)
				    instance
				    (slotd standard-effective-slot-definition))
  (ecase (standard-slot-definition.allocation slotd)
    ((:instance :class)
     (%std-slot-vector-boundp (instance-slots instance) slotd))))

(defmethod slot-boundp-using-class ((class funcallable-standard-class)
				    instance
				    (slotd standard-effective-slot-definition))
  (%std-slot-vector-boundp (gf.slots instance) slotd))



(defun slot-boundp (instance name)
  (let* ((wrapper
          (let* ((w (instance-class-wrapper instance)))
            (if (eql 0 (%wrapper-hash-index w))
              (instance.class-wrapper (update-obsolete-instance instance))
              w)))
         (class (%wrapper-class wrapper))
         (slotd (find-slotd name (if (%standard-instance-p class)
                                   (%class.slots class)
                                   (class-slots class)))))
    (if slotd
      (%maybe-std-slot-boundp-using-class class instance slotd)
      (if (typep name 'symbol)
        (values (slot-missing class instance name 'slot-boundp))
        (report-bad-arg name 'symbol)))))

(defun slot-value-if-bound (instance name &optional default)
  (if (slot-boundp instance name)
    (slot-value instance name)
    default))

(defun slot-exists-p (instance name)
  (let* ((class (class-of instance))
	 (slots  (class-slots class)))
    (find-slotd name slots)))


(defun slot-id-value (instance slot-id)
  (let* ((wrapper (instance-class-wrapper instance)))
    (funcall (%wrapper-slot-id-value wrapper) instance slot-id)))

(defun set-slot-id-value (instance slot-id value)
  (let* ((wrapper (instance-class-wrapper instance)))
    (funcall (%wrapper-set-slot-id-value wrapper) instance slot-id value)))

(defun slot-id-boundp (instance slot-id)
  (let* ((wrapper (instance-class-wrapper instance))
         (class (%wrapper-class wrapper))
         (slotd (funcall (%wrapper-slot-id->slotd wrapper) instance slot-id)))
    (if slotd
      (%maybe-std-slot-boundp-using-class class instance slotd)
      (values (slot-missing class instance (slot-id.name slot-id) 'slot-boundp)))))
  
;;; returns nil if (apply gf args) wil cause an error because of the
;;; non-existance of a method (or if GF is not a generic function or the name
;;; of a generic function).
(defun method-exists-p (gf &rest args)
  (declare (dynamic-extent args))
  (when (symbolp gf)
    (setq gf (fboundp gf)))
  (when (typep gf 'standard-generic-function)
    (or (null args)
        (let* ((methods (sgf.methods gf)))
          (dolist (m methods)
            (when (null (%method-qualifiers m))
              (let ((specializers (%method-specializers m))
                    (args args))
                (when (dolist (s specializers t)
                        (unless (cond ((typep s 'eql-specializer) 
				       (eql (eql-specializer-object s)
					    (car args)))
                                      (t (memq s (%inited-class-cpl
                                                  (class-of (car args))))))
                          (return nil))
                        (pop args))
                  (return-from method-exists-p m)))))
          nil))))

(defun funcall-if-method-exists (gf &optional default &rest args)
  (declare (dynamic-extent args))
  (if (apply #'method-exists-p gf args)
    (apply gf args)
    (if default (apply default args))))


(defun find-specializer (specializer)
  (if (and (listp specializer) (eql (car specializer) 'eql))
    (intern-eql-specializer (cadr specializer))
    (find-class specializer)))

(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class)))

(defmethod make-instances-obsolete ((class standard-class))
  (let ((wrapper (%class-own-wrapper class)))
    (when wrapper
      (setf (%class-own-wrapper class) nil)
      (make-wrapper-obsolete wrapper)))
  class)

(defmethod make-instances-obsolete ((class funcallable-standard-class))
  (let ((wrapper (%class.own-wrapper class)))
    (when wrapper
      (setf (%class-own-wrapper class) nil)
      (make-wrapper-obsolete wrapper)))
  class)

(defmethod make-instances-obsolete ((class structure-class))
  ;; could maybe warn that instances are obsolete, but there's not
  ;; much that we can do about that.
  class)



;;; A wrapper is made obsolete by setting the hash-index & instance-slots to 0
;;; The instance slots are saved for update-obsolete-instance
;;; by consing them onto the class slots.
;;; Method dispatch looks at the hash-index.
;;; slot-value & set-slot-value look at the instance-slots.
;;; Each wrapper may have an associated forwarding wrapper, which must
;;; also be made obsolete.  The forwarding-wrapper is stored in the
;;; hash table below keyed on the wrapper-hash-index of the two
;;; wrappers.
(defvar *forwarding-wrapper-hash-table* (make-hash-table :test 'eq))  


(defun make-wrapper-obsolete (wrapper)
  (without-interrupts
   (let ((forwarding-info
          (unless (eql 0 (%wrapper-instance-slots wrapper))   ; already forwarded or obsolete?
            (%cons-forwarding-info (%wrapper-instance-slots wrapper)
                                   (%wrapper-class-slots wrapper)))))
     (when forwarding-info
       (setf (%wrapper-hash-index wrapper) 0
             (%wrapper-cpl wrapper) nil
             (%wrapper-cpl-bits wrapper) nil
             (%wrapper-instance-slots wrapper) 0
             (%wrapper-forwarding-info wrapper) forwarding-info
	     (%wrapper-slot-id->slotd wrapper) #'%slot-id-lookup-obsolete
	     (%wrapper-slot-id-value wrapper) #'%slot-id-ref-obsolete
	     (%wrapper-set-slot-id-value wrapper) #'%slot-id-set-obsolete
             ))))
  wrapper)

(defun %clear-class-primary-slot-accessor-offsets (class)
  (let ((info-list (%class-get class '%class-primary-slot-accessor-info)))
    (dolist (info info-list)
      (setf (%slot-accessor-info.offset info) nil))))

(defun primary-class-slot-offset (class slot-name)
  (dolist (super (%class.cpl class))
    (let* ((pos (and (typep super 'standard-class)
                     (%class-primary-p super)
                     (dolist (slot (%class-slots class))
		       (when (eq (%slot-definition-allocation slot)
				 :instance)
			 (when (eq slot-name (%slot-definition-name slot))
			   (return (%slot-definition-location slot))))))))
      (when pos (return pos)))))

;;; Called by the compiler-macro expansion for slot-value
;;; info is the result of a %class-primary-slot-accessor-info call.
;;; value-form is specified if this is set-slot-value.
;;; Otherwise it's slot-value.
(defun primary-class-slot-value (instance info &optional (value-form nil value-form-p))
  (let ((slot-name (%slot-accessor-info.slot-name info)))
    (prog1
      (if value-form-p
        (setf (slot-value instance slot-name) value-form)
        (slot-value instance slot-name))
      (setf (%slot-accessor-info.offset info)
            (primary-class-slot-offset (class-of instance) slot-name)))))

(defun primary-class-accessor (instance info &optional (value-form nil value-form-p))
  (let ((accessor (%slot-accessor-info.accessor info)))
    (prog1
      (if value-form-p
        (funcall accessor value-form instance)
        (funcall accessor instance))
      (let ((methods (compute-applicable-methods
                      accessor
                      (if value-form-p (list value-form instance) (list instance))))
            method)
        (when (and (eql (length methods) 1)
                   (typep (setq method (car methods)) 'standard-accessor-method))
          (let* ((slot-name (method-slot-name method)))
            (setf (%slot-accessor-info.offset info)
                  (primary-class-slot-offset (class-of instance) slot-name))))))))

(defun exchange-slot-vectors-and-wrappers (a b)
  (if (typep a 'funcallable-standard-object)
    (let* ((temp-wrapper (gf.instance.class-wrapper a))
           (orig-a-slots (gf.slots a))
           (orig-b-slots (gf.slots b)))
      (setf (gf.instance.class-wrapper a) (gf.instance.class-wrapper b)
            (gf.instance.class-wrapper b) temp-wrapper
            (gf.slots a) orig-b-slots
            (gf.slots b) orig-a-slots
            (slot-vector.instance orig-a-slots) b
            (slot-vector.instance orig-b-slots) a))    
    (let* ((temp-wrapper (instance.class-wrapper a))
           (orig-a-slots (instance.slots a))
           (orig-b-slots (instance.slots b)))
      (setf (instance.class-wrapper a) (instance.class-wrapper b)
            (instance.class-wrapper b) temp-wrapper
            (instance.slots a) orig-b-slots
            (instance.slots b) orig-a-slots
            (slot-vector.instance orig-a-slots) b
            (slot-vector.instance orig-b-slots) a))))




;;; How slot values transfer (from PCL):
;;;
;;; local  --> local        transfer 
;;; local  --> shared       discard
;;; local  -->  --          discard
;;; shared --> local        transfer
;;; shared --> shared       discard
;;; shared -->  --          discard
;;;  --    --> local        added
;;;  --    --> shared        --
;;;
;;; See make-wrapper-obsolete to see how we got here.
;;; A word about forwarding.  When a class is made obsolete, the
;;; %wrapper-instance-slots slot of its wrapper is set to 0.
;;; %wrapper-class-slots = (instance-slots . class-slots)
;;; Note: this should stack-cons the new-instance if we can reuse the
;;; old instance or it's forwarded value.
(defun update-obsolete-instance (instance)
  (let* ((added ())
	 (discarded ())
	 (plist ()))
    (without-interrupts			; Not -close- to being correct
     (let* ((old-wrapper (standard-object-p instance)))
       (unless old-wrapper
         (when (typep instance 'funcallable-standard-object)
           (setq old-wrapper (gf.instance.class-wrapper instance)))
         (unless old-wrapper
           (report-bad-arg instance '(or standard-object funcallable-standard-object))))
       (when (eql 0 (%wrapper-instance-slots old-wrapper)) ; is it really obsolete?
         (let* ((class (%wrapper-class old-wrapper))
                (new-wrapper (or (%class.own-wrapper class)
                                 (progn
                                   (update-class class t)
                                   (%class.own-wrapper class))))
                (forwarding-info (%wrapper-forwarding-info old-wrapper))
                (old-class-slots (%forwarding-class-slots forwarding-info))
                (old-instance-slots (%forwarding-instance-slots forwarding-info))
                (new-instance-slots (%wrapper-instance-slots new-wrapper))
                (new-class-slots (%wrapper-class-slots new-wrapper))
		(new-instance (allocate-instance class))
		(old-slot-vector (instance-slots instance))
		(new-slot-vector (instance-slots new-instance)))
           ;; Lots to do.  Hold onto your hat.
           (let* ((old-size (uvsize old-instance-slots))
                  (new-size (uvsize new-instance-slots)))
             (declare (fixnum old-size new-size))
             (dotimes (i old-size)
               (declare (fixnum i))
               (let* ((slot-name (%svref old-instance-slots i))
                      (pos (%vector-member slot-name new-instance-slots))
                      (val (%svref old-slot-vector (%i+ i 1))))
                 (if pos
                   (setf (%svref new-slot-vector (%i+ pos 1)) val)
                   (progn
                     (push slot-name discarded)
                     (unless (eq val (%slot-unbound-marker))
                       (setf (getf plist slot-name) val))))))
             ;; Go through old class slots
             (dolist (pair old-class-slots)
               (let* ((slot-name (%car pair))
                      (val (%cdr pair))
                      (pos (%vector-member slot-name new-instance-slots)))
                 (if pos
                   (setf (%svref new-slot-vector (%i+ pos 1)) val)
                   (progn
                     (push slot-name discarded)
                     (unless (eq val (%slot-unbound-marker))
                       (setf (getf plist slot-name) val))))))
                                        ; Go through new instance slots
             (dotimes (i new-size)
               (declare (fixnum i))
               (let* ((slot-name (%svref new-instance-slots i)))
                 (unless (or (%vector-member slot-name old-instance-slots)
                             (assoc slot-name old-class-slots))
                   (push slot-name added))))
             ;; Go through new class slots
             (dolist (pair new-class-slots)
               (let ((slot-name (%car pair)))
                 (unless (or (%vector-member slot-name old-instance-slots)
                             (assoc slot-name old-class-slots))
                   (push slot-name added))))
             (exchange-slot-vectors-and-wrappers new-instance instance))))))
    ;; run user code with interrupts enabled.
    (update-instance-for-redefined-class instance added discarded plist))
  instance)
            
          
(defmethod update-instance-for-redefined-class ((instance standard-object)
						added-slots
						discarded-slots
						property-list
						&rest initargs)
  (declare (ignore discarded-slots property-list))
  (when initargs
    (check-initargs
     instance nil initargs t
     #'update-instance-for-redefined-class #'shared-initialize))
  (apply #'shared-initialize instance added-slots initargs))

(defmethod update-instance-for-redefined-class ((instance standard-generic-function)
						added-slots
						discarded-slots
						property-list
						&rest initargs)
  (declare (ignore discarded-slots property-list))
  (when initargs
    (check-initargs
     instance nil initargs t
     #'update-instance-for-redefined-class #'shared-initialize))
  (apply #'shared-initialize instance added-slots initargs))

(defun check-initargs (instance class initargs errorp &rest functions)
  (declare (dynamic-extent functions))
  (declare (list functions))
  (setq class (require-type (or class (class-of instance)) 'class))
  (unless (getf initargs :allow-other-keys)
    (let ((initvect (initargs-vector instance class functions)))
      (when (eq initvect t) (return-from check-initargs nil))
      (do* ((tail initargs (cddr tail))
	    (initarg (car tail) (car tail))
	    bad-keys? bad-key)
	   ((null (cdr tail))
	    (if bad-keys?
	      (if errorp
		(signal-program-error
		 "~s is an invalid initarg to ~s for ~s.~%~
                                    Valid initargs: ~s."
		 bad-key
		 (function-name (car functions))
		 class (coerce initvect 'list))
		(values bad-keys? bad-key))))
	(if (eq initarg :allow-other-keys)
	  (if (cadr tail)
	    (return))                   ; (... :allow-other-keys t ...)
	  (unless (or bad-keys? (%vector-member initarg initvect))
	    (setq bad-keys? t
		  bad-key initarg)))))))

(defun initargs-vector (instance class functions)
  (let ((index (cadr (assq (car functions) *initialization-invalidation-alist*))))
    (unless index
      (error "Unknown initialization function: ~s." (car functions)))
    (let ((initvect (%svref (instance-slots class) index)))
      (unless initvect
        (setf (%svref (instance-slots class) index) 
              (setq initvect (compute-initargs-vector instance class functions))))
      initvect)))


;; This is used for compile-time defclass option checking.
(defun class-keyvect (class-arg initargs)
  (let* ((class (if (typep class-arg 'class) class-arg (find-class class-arg nil)))
	 (meta-arg (getf initargs :metaclass (if (and class (not (typep class 'forward-referenced-class)))
					       (class-of class)
					       *standard-class-class*)))
	 (meta-spec (if (quoted-form-p meta-arg) (%cadr meta-arg) meta-arg))
	 (meta (if (typep meta-spec 'class) meta-spec (find-class meta-spec nil))))
    (if (and meta (not (typep meta 'forward-referenced-class)))
      (compute-initargs-vector class meta (list #'initialize-instance #'allocate-instance #'shared-initialize) t)
      t)))

(defun compute-initargs-vector (instance class functions &optional require-rest)
  (let ((initargs (class-slot-initargs class))
        (cpl (%inited-class-cpl class)))
    (dolist (f functions)         ; for all the functions passed
      #+no
      (if (logbitp $lfbits-aok-bit (lfun-bits f))
	(return-from compute-initargs-vector t))
      (dolist (method (%gf-methods f))   ; for each applicable method
        (let ((spec (car (%method-specializers method))))
          (when (if (typep spec 'eql-specializer)
                  (eql instance (eql-specializer-object spec))
                  (memq spec cpl))
            (let* ((func (%inner-method-function method))
                   (keyvect (if (and (logbitp $lfbits-aok-bit (lfun-bits func))
				     (or (not require-rest)
					 (logbitp $lfbits-rest-bit (lfun-bits func))))
			      (return-from compute-initargs-vector t)
                              (lfun-keyvect func))))
              (dovector (key keyvect)
                (pushnew key initargs)))))))   ; add all of the method's keys
    (apply #'vector initargs)))



;;; A useful function
(defun class-make-instance-initargs (class)
  (setq class (require-type (if (symbolp class) (find-class class) class)
                            'std-class))
  (flet ((iv (class &rest functions)
           (declare (dynamic-extent functions))
           (initargs-vector (class-prototype class) class functions)))
    (let ((initvect (apply #'iv
                           class
                           #'initialize-instance #'allocate-instance #'shared-initialize
                           nil)))
      (if (eq initvect 't)
        t
        (concatenate 'list initvect)))))

                                   

;;; This is part of the MOP
;;; Maybe it was, at one point in the distant past ...
(defmethod class-slot-initargs ((class slots-class))
  (collect ((initargs))
    (dolist (slot (%class-slots class) (initargs))
      (dolist (i (%slot-definition-initargs slot))
        (initargs i)))))

  
(defun maybe-update-obsolete-instance (instance)
  (let ((wrapper (standard-object-p instance)))
    (unless wrapper
              (when (typep instance 'funcallable-standard-object)
          (setq wrapper (gf.instance.class-wrapper instance)))
      
      (unless wrapper
        (report-bad-arg instance '(or standard-object funcallable-standard-object))))
    (when (eql 0 (%wrapper-hash-index wrapper))
      (update-obsolete-instance instance)))
  instance)


;;; If you ever reference one of these through anyone who might call
;;; update-obsolete-instance, you will lose badly.
(defun %maybe-forwarded-instance (instance)
  (maybe-update-obsolete-instance instance)
  instance)



(defmethod change-class (instance
			 (new-class symbol)
			 &rest initargs &key &allow-other-keys)
  (declare (dynamic-extent initargs))
  (apply #'change-class instance (find-class new-class) initargs))

(defmethod change-class ((instance standard-object)
			 (new-class standard-class)
			  &rest initargs &key &allow-other-keys)
  (declare (dynamic-extent initargs))
  (%change-class instance new-class initargs))


(defun %change-class (object new-class initargs)
  (let* ((old-class (class-of object))
	 (old-wrapper (%class.own-wrapper old-class))
	 (new-wrapper (or (%class.own-wrapper new-class)
			  (progn
			    (update-class new-class t)
			    (%class.own-wrapper new-class))))
	 (old-instance-slots-vector (%wrapper-instance-slots old-wrapper))
	 (new-instance-slots-vector (%wrapper-instance-slots new-wrapper))
	 (num-new-instance-slots (length new-instance-slots-vector))
	 (new-object (allocate-instance new-class)))
    (declare (fixnum num-new-instance-slots)
	     (simple-vector new-instance-slots-vector old-instance-slots-vector))
    ;; Retain local slots shared between the new class and the old.
    (do* ((new-pos 0 (1+ new-pos))
	  (new-slot-location 1 (1+ new-slot-location)))
	 ((= new-pos num-new-instance-slots))
      (declare (fixnum new-pos new-slot-location))
      (let* ((old-pos (position (svref new-instance-slots-vector new-pos)
				old-instance-slots-vector :test #'eq)))
	(when old-pos
	  (setf (%standard-instance-instance-location-access
		 new-object
		 new-slot-location)
		(%standard-instance-instance-location-access
		 object
		 (the fixnum (1+ (the fixnum old-pos))))))))
    ;; If the new class defines a local slot whos name matches
    ;; that of a shared slot in the old class, the shared slot's
    ;; value is used to initialize the new instance's local slot.
    (dolist (shared-slot (%wrapper-class-slots old-wrapper))
      (destructuring-bind (name . value) shared-slot
	(let* ((new-slot-pos (position name new-instance-slots-vector
				       :test #'eq)))
	  (if new-slot-pos
	    (setf (%standard-instance-instance-location-access
		   new-object
		   (the fixnum (1+ (the fixnum new-slot-pos))))
		  value)))))
    (exchange-slot-vectors-and-wrappers object new-object)
    (apply #'update-instance-for-different-class new-object object initargs)
    object))

(defmethod update-instance-for-different-class ((previous standard-object)
                                                (current standard-object)
                                                &rest initargs)
  (declare (dynamic-extent initargs))
  (%update-instance-for-different-class previous current initargs))

(defun %update-instance-for-different-class (previous current initargs)
  (when initargs
    (check-initargs
     current nil initargs t
     #'update-instance-for-different-class #'shared-initialize))
  (let* ((previous-slots (class-slots (class-of previous)))
	 (current-slots (class-slots (class-of current)))
	 (added-slot-names ()))
    (dolist (s current-slots)
      (let* ((name (%slot-definition-name s)))
	(unless (find-slotd name previous-slots)
	  (push name added-slot-names))))
    (apply #'shared-initialize
	   current
	   added-slot-names
	   initargs)))




;;; Clear all the valid initargs caches.
(defun clear-valid-initargs-caches ()
  (map-classes #'(lambda (name class)
                   (declare (ignore name))
                   (when (std-class-p class)
                     (setf (%class.make-instance-initargs class) nil
                           (%class.reinit-initargs class) nil
                           (%class.redefined-initargs class) nil
                           (%class.changed-initargs class) nil)))))

(defun clear-clos-caches ()
  (clear-all-gf-caches)
  (clear-valid-initargs-caches))

(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (%allocate-std-instance class))

(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (%allocate-gf-instance class))

(unless *initialization-invalidation-alist*
  (setq *initialization-invalidation-alist*
        (list (list #'initialize-instance %class.make-instance-initargs)
              (list #'allocate-instance %class.make-instance-initargs)
              (list #'reinitialize-instance %class.reinit-initargs)
              (list #'shared-initialize 
                    %class.make-instance-initargs %class.reinit-initargs
                    %class.redefined-initargs %class.changed-initargs)
              (list #'update-instance-for-redefined-class
                    %class.redefined-initargs)
              (list #'update-instance-for-different-class
                    %class.changed-initargs))))


(defstatic *initialization-function-lists*
  (list (list #'initialize-instance #'allocate-instance #'shared-initialize)
        (list #'reinitialize-instance #'shared-initialize)
        (list #'update-instance-for-redefined-class #'shared-initialize)
        (list #'update-instance-for-different-class #'shared-initialize)))



(unless *clos-initialization-functions*
  (setq *clos-initialization-functions*
        (list #'initialize-instance #'allocate-instance #'shared-initialize
              #'reinitialize-instance
              #'update-instance-for-different-class #'update-instance-for-redefined-class)))

(defun compute-initialization-functions-alist ()
  (let ((res nil)
        (lists *initialization-function-lists*))
    (dolist (cell *initialization-invalidation-alist*)
      (let (res-list)
        (dolist (slot-num (cdr cell))
          (push
           (ecase slot-num
             (#.%class.make-instance-initargs 
              (assq #'initialize-instance lists))
             (#.%class.reinit-initargs
              (assq #'reinitialize-instance lists))
             (#.%class.redefined-initargs
              (assq #'update-instance-for-redefined-class lists))
             (#.%class.changed-initargs
              (assq #'update-instance-for-different-class lists)))
           res-list))
        (push (cons (car cell) (nreverse res-list)) res)))
    (setq *initialization-functions-alist* res)))

(compute-initialization-functions-alist)

                  




;;; Need to define this for all of the BUILT-IN-CLASSes.
(defmethod class-prototype ((class class))
  (%class.prototype class))

(defmethod class-prototype ((class std-class))
  (or (%class.prototype class)
      (setf (%class.prototype class) (allocate-instance class))))


(defun gf-class-prototype (class)
  (%allocate-gf-instance class))



(defmethod class-prototype ((class structure-class))
  (or (%class.prototype class)
      (setf (%class.prototype class)
            (let* ((sd (gethash (class-name class) %defstructs%))
                   (slots (class-slots class))
                   (proto (allocate-typed-vector :struct (1+ (length slots)))))
              (setf (uvref proto 0) (sd-superclasses sd))
              (dolist (slot slots proto)
                (setf (slot-value-using-class class proto slot)
                      (funcall (slot-definition-initfunction slot))))))))


(defmethod remove-method ((generic-function standard-generic-function)
                          (method standard-method))
  (when (eq generic-function (%method-gf method))
    (%remove-standard-method-from-containing-gf method))
  generic-function)



(defmethod function-keywords ((method standard-method))
  (let ((f (%inner-method-function method)))
    (values
     (concatenate 'list (lfun-keyvect f))
     (%ilogbitp $lfbits-aok-bit (lfun-bits f)))))

(defmethod no-next-method ((generic-function standard-generic-function)
                           (method standard-method)
                           &rest args)
  (error "There is no next method for ~s~%args: ~s" method args))

(defmethod add-method ((generic-function standard-generic-function) (method standard-method))
  (%add-standard-method-to-standard-gf generic-function method))

(defmethod no-applicable-method (gf &rest args)
  (cerror "Try calling it again"
          "There is no applicable method for the generic function:~%  ~s~%when called with arguments:~%  ~s" gf args)
  (apply gf args))


(defmethod no-applicable-primary-method (gf methods)
  (%method-combination-error "No applicable primary methods for ~s~@
                              Applicable methods: ~s" gf methods))

(defmethod compute-applicable-methods ((gf standard-generic-function) args)
  (%compute-applicable-methods* gf args))

(defmethod compute-applicable-methods-using-classes ((gf standard-generic-function) args)
  (let ((res (%compute-applicable-methods* gf args t)))
    (if (eq res :undecidable)
      (values nil nil)
      (values res t))))

(defun %compute-applicable-methods+ (gf &rest args)
  (declare (dynamic-extent args))
  (%compute-applicable-methods* gf args))

(defun %compute-applicable-methods* (gf args &optional using-classes-p)
  (let* ((methods (%gf-methods gf))
         (args-length (length args))
         (bits (inner-lfun-bits gf))
         arg-count res)
    (when methods
      (setq arg-count (length (%method-specializers (car methods))))
      (unless (<= arg-count args-length)
        (error "Too few args to ~s" gf))
      (unless (or (logbitp $lfbits-rest-bit bits)
                  (logbitp $lfbits-restv-bit bits)
                  (logbitp $lfbits-keys-bit bits)
                  (<= args-length 
                      (+ (ldb $lfbits-numreq bits) (ldb $lfbits-numopt bits))))
        (error "Too many args to ~s" gf))
      (let ((cpls (make-list arg-count)))
        (declare (dynamic-extent cpls))
        (do* ((args-tail args (cdr args-tail))
              (cpls-tail cpls (cdr cpls-tail)))
            ((null cpls-tail))
          (setf (car cpls-tail)
                (%class-precedence-list (if using-classes-p
                                          ;; extension for use in source location support
                                          (if (typep (car args-tail) 'eql-specializer)
                                            (class-of (eql-specializer-object (car args-tail)))
                                            (car args-tail))
                                          (class-of (car args-tail))))))
        (dolist (m methods)
          (let ((appp (%method-applicable-p m args cpls using-classes-p)))
            (when appp
              (when (eq appp :undecidable) ;; can only happen if using-classes-p
                (return-from %compute-applicable-methods* appp))
              (push m res))))
        (sort-methods res cpls (%gf-precedence-list gf))))))


(defun %method-applicable-p (method args cpls &optional using-classes-p)
  (do* ((specs (%method-specializers method) (%cdr specs))
        (args args (%cdr args))
        (cpls cpls (%cdr cpls)))
      ((null specs) t)
    (let ((spec (%car specs))
          (arg (%car args)))
      (if (typep spec 'eql-specializer)
        (if using-classes-p
          (if (typep arg 'eql-specializer) ;; extension for use in source location support
            (unless (eql (eql-specializer-object arg) (eql-specializer-object spec))
              (return nil))
            (if (typep (eql-specializer-object spec) arg)
              ;; Can't tell if going to be applicable or not based on class alone
              ;; Except for the special case of NULL which is a singleton
              (unless (eq arg *null-class*)
                (return :undecidable))
              (return nil)))
          (unless (eql arg (eql-specializer-object spec))
            (return nil)))
        (unless (memq spec (%car cpls))
          (return nil))))))


;;; Need this so that (compute-applicable-methods
;;; #'class-precedence-list ...)  will not recurse.
(defun %class-precedence-list (class)
  (if (eq (class-of class) *standard-class-class*)
    (%inited-class-cpl class)
    (class-precedence-list class)))

(defmethod class-precedence-list ((class class))
  (%inited-class-cpl class))


(defun make-all-methods-kernel ()
  (dolist (f (population.data %all-gfs%))
    (let ((smc *standard-method-class*))
      (dolist (method (slot-value-if-bound f 'methods))
	(when (eq (class-of method) smc)
	  (change-class method *standard-kernel-method-class*))))))


(defun make-all-methods-non-kernel ()
  (dolist (f (population.data %all-gfs%))
    (let ((skmc *standard-kernel-method-class*))
      (dolist (method (slot-value-if-bound f 'methods))
	(when (eq (class-of method) skmc)
	  (change-class method *standard-method-class*))))))


(defun required-lambda-list-args (l)
  (multiple-value-bind (ok req) (verify-lambda-list l)
    (unless ok (error "Malformed lambda-list: ~s" l))
    req))


(defun check-generic-function-lambda-list (ll &optional (errorp t))
  (multiple-value-bind (ok reqsyms opttail resttail keytail auxtail)
                       (verify-lambda-list ll)
    (declare (ignore reqsyms resttail))
    (when ok 
      (block checkit
        (when (eq (car opttail) '&optional)
          (dolist (elt (cdr opttail))
            (when (memq elt lambda-list-keywords) (return))
            (unless (or (symbolp elt)
                        (and (listp elt)
                             (non-nil-symbol-p (car elt))
                             (null (cdr elt))))
              (return-from checkit (setq ok nil)))))
        (dolist (elt (cdr keytail))
          (when (memq elt lambda-list-keywords) (return))
          (unless (or (symbolp elt)
                      (and (listp elt)
                           (or (non-nil-symbol-p (car elt))
                               (and (listp (car elt))
                                    (non-nil-symbol-p (caar elt))
                                    (non-nil-symbol-p (cadar elt))
                                    (null (cddar elt))))
                           (null (cdr elt))))
            (return-from checkit (setq ok nil))))
        (when auxtail (setq ok nil))))
    (when (and errorp (not ok))
      (signal-program-error "Bad generic function lambda list: ~s" ll))
    ok))


(defun canonicalize-argument-precedence-order (apo req)
  (cond ((equal apo req) nil)
        ((not (eql (length apo) (length req)))
         (signal-program-error "Lengths of ~S and ~S differ." apo req))
        (t (let ((res nil))
             (dolist (arg apo (nreverse res))
               (let ((index (position arg req)))
                 (if (or (null index) (memq index res))
                   (error "Missing or duplicate arguments in ~s" apo))
                 (push index res)))))))


(defun %defgeneric (function-name lambda-list method-combination generic-function-class
                                  options)
  (setq generic-function-class (find-class generic-function-class))
  (setq method-combination 
        (find-method-combination
         (class-prototype generic-function-class)
         (car method-combination)
         (cdr method-combination)))
  (let ((gf (fboundp function-name)))
    (when gf
      (dolist (method (%defgeneric-methods gf))
        (remove-method gf method))))
  (record-source-file function-name 'function)
  (record-arglist function-name lambda-list)
  (apply #'ensure-generic-function 
         function-name
         :lambda-list lambda-list
         :method-combination method-combination
         :generic-function-class generic-function-class
         options))




;;; Redefined in lib;method-combination.lisp
(defmethod find-method-combination ((gf standard-generic-function) type options)
  (unless (and (eq type 'standard) (null options))
    (error "non-standard method-combination not supported yet."))
  *standard-method-combination*)



(defmethod add-direct-method ((spec specializer) (method method))
  (pushnew method (specializer.direct-methods spec)))

(setf (fdefinition '%do-add-direct-method) #'add-direct-method)

(defmethod remove-direct-method ((spec specializer) (method method))
  (setf (specializer.direct-methods spec)
	(nremove method (specializer.direct-methods spec))))

(setf (fdefinition '%do-remove-direct-method) #'remove-direct-method)





				   



(defvar *make-load-form-saving-slots-hash* (make-hash-table :test 'eq))

(defun make-load-form-saving-slots (object &key
					   (slot-names nil slot-names-p)
					   environment)
  (declare (ignore environment))
  (let* ((class (class-of object))
         (class-name (class-name class))
         (structurep (structurep object))
         (sd (and structurep (require-type (gethash class-name %defstructs%) 'vector))))
    (unless (or structurep
                (standard-instance-p object))
      (%badarg object '(or standard-object structure-object)))
    (if slot-names-p
      (dolist (slot slot-names)
        (unless (slot-exists-p object slot)
          (error "~s has no slot named ~s" object slot)))
      (setq slot-names
            (if structurep
              (let ((res nil))
                (dolist (slot (sd-slots sd))
                  (unless (fixnump (car slot))
                    (push (%car slot) res)))
                (nreverse res))
              (mapcar '%slot-definition-name
                      (extract-instance-effective-slotds
                       (class-of object))))))
    (values
     (let* ((form (gethash class-name *make-load-form-saving-slots-hash*)))
       (or (and (consp form)
                (eq (car form) 'allocate-instance)
                form)
           (setf (gethash class-name *make-load-form-saving-slots-hash*)
                 `(allocate-instance (find-class ',class-name)))))
     ;; initform is NIL when there are no slots
     (when slot-names
       `(%set-slot-values
         ',object
         ',slot-names
         ',(let ((temp #'(lambda (slot)
                           (if (slot-boundp object slot)
                             (slot-value object slot)
                             (%slot-unbound-marker)))))
             (declare (dynamic-extent temp))
             (mapcar temp slot-names)))))))


    

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (let* ((class-name (%class-name class))
         (sd (or (gethash class-name %defstructs%)
                 (error "Can't find structure named ~s" class-name)))
         (res (make-structure-vector (sd-size sd))))
    (setf (%svref res 0) (mapcar (lambda (x)
                                   (find-class-cell x t)) (sd-superclasses sd)))
    res))


(defun %set-slot-values (object slots values)
  (dolist (slot slots)
    (let ((value (pop values)))
      (if (eq value (%slot-unbound-marker))
        (slot-makunbound object slot)
        (setf (slot-value object slot) value)))))


(defun %recache-class-direct-methods ()
  (let ((*maintain-class-direct-methods* t))   ; in case we get an error
    (dolist (f (population-data %all-gfs%))
      (when (standard-generic-function-p f)
        (dolist (method (%gf-methods f))
          (%add-direct-methods method)))))
  (setq *maintain-class-direct-methods* t))   ; no error, all is well

