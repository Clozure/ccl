;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
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
;;;

;;; At this point in the load sequence, the handful of extant basic classes
;;; exist only in skeletal form (without direct or effective slot-definitions.)

(in-package "CCL")

(defun extract-slotds-with-allocation (allocation slotds)
  (collect ((right-ones))
    (dolist (s slotds (right-ones))
      (if (eq (%slot-definition-allocation s) allocation)
        (right-ones s)))))

(defun extract-instance-direct-slotds (class)
  (extract-slotds-with-allocation :instance (%class-direct-slots class)))

(defun extract-class-direct-slotds (class)
  (extract-slotds-with-allocation :class (%class-direct-slots class)))

(defun extract-instance-effective-slotds (class)
  (extract-slotds-with-allocation :instance (%class-slots class)))

(defun extract-class-effective-slotds (class)
  (extract-slotds-with-allocation :class (%class-slots class)))

(defun extract-instance-and-class-slotds (slotds)
  (collect ((instance-slots)
	    (shared-slots))
    (dolist (s slotds (values (instance-slots) (shared-slots)))
      (if (eq (%slot-definition-allocation s) :class)
        (shared-slots s)
        (instance-slots s)))))



(defun direct-instance-and-class-slotds (class)
  (extract-instance-and-class-slotds (%class-direct-slots class)))

(defun effective-instance-and-class-slotds (class)
  (extract-instance-and-class-slotds (%class-slots class)))

(defun %early-shared-initialize (instance slot-names initargs)
  (unless (or (listp slot-names) (eq slot-names t))
    (report-bad-arg slot-names '(or list (eql t))))
  ;; Check that initargs contains valid key/value pairs,
  ;; signal a PROGRAM-ERROR otherwise.  (Yes, this is
  ;; an obscure way to do so.)
  (destructuring-bind (&key &allow-other-keys) initargs)
  (let* ((wrapper (instance-class-wrapper instance))
         (class (%wrapper-class wrapper)))
    (when (eql 0 (%wrapper-hash-index wrapper)) ; obsolete
      (update-obsolete-instance instance)
      (setq wrapper (instance-class-wrapper instance)))
    (dolist (slotd (%class-slots class))
      (let* ((loc (%slot-definition-location slotd)))
        (multiple-value-bind (ignore new-value foundp)
            (get-properties initargs
                            (%slot-definition-initargs slotd))
          (declare (ignore ignore))
          (if foundp
	    (progn
	      (unless (funcall (standard-effective-slot-definition.type-predicate slotd) new-value)
		(error 'bad-slot-type-from-initarg
		       :slot-definition slotd
		       :instance instance
		       :datum new-value
		       :expected-type  (%slot-definition-type slotd)
		       :initarg-name (car foundp)))
	      (if (consp loc)
		(rplacd loc new-value)
		(setf (standard-instance-instance-location-access instance loc)
		      new-value)))
            (if (or (eq slot-names t)
                    (member (%slot-definition-name slotd)
                            slot-names
			    :test #'eq))
              (let* ((curval (if (consp loc)
                               (cdr loc)
                               (%standard-instance-instance-location-access
				instance loc))))
                (if (eq curval (%slot-unbound-marker))
                  (let* ((initfunction (%slot-definition-initfunction slotd)))
                    (if initfunction
                      (let* ((newval (funcall initfunction)))
			(unless (funcall (standard-effective-slot-definition.type-predicate slotd) newval)
			  (error 'bad-slot-type-from-initform
				 :slot-definition slotd
				 :expected-type (%slot-definition-type slotd)
				 :datum newval
				 :instance instance))
                        (if (consp loc)
                          (rplacd loc newval)
                          (setf (standard-instance-instance-location-access
				 instance loc)
				newval)))))))))))))
  instance)

(setf (fdefinition '%shared-initialize) #'%early-shared-initialize)

;;; This is redefined (to call MAKE-INSTANCE) below.
(setf (fdefinition '%make-direct-slotd)
      #'(lambda (slotd-class &key
			     name
			     initfunction
			     initform
			     initargs
			     (allocation :instance)
			     class
			     (type t)
			     (documentation (%slot-unbound-marker))
			     readers
			     writers)
	  (declare (ignore slotd-class))
	  (%instance-vector
	   (%class.own-wrapper *standard-direct-slot-definition-class*)
	   name type initfunction initform initargs allocation
	   documentation class readers writers)))

;;; Also redefined below, after MAKE-INSTANCE is possible.
(setf (fdefinition '%make-effective-slotd)
      #'(lambda (slotd-class &key
			     name
			     initfunction
			     initform
			     initargs
			     allocation
			     class
			     type
			     documentation)
	  (declare (ignore slotd-class))
	  (%instance-vector
	   (%class.own-wrapper *standard-effective-slot-definition-class*)
	   name type initfunction initform initargs allocation
	   documentation class nil (ensure-slot-id name) #'true)))


(defmethod compile-time-class-p ((class class)) nil)

(defmethod direct-slot-definition-class ((class std-class) &key (allocation :instance) &allow-other-keys)
  (unless (member allocation '(:instance :class))
    (report-bad-arg allocation '(member (:instance :class))))
  *standard-direct-slot-definition-class*)

(defmethod effective-slot-definition-class ((class std-class) &key (allocation :instance) &allow-other-keys)
  (unless (member allocation '(:instance :class))
    (report-bad-arg allocation '(member (:instance :class))))
  *standard-effective-slot-definition-class*)

(defun make-direct-slot-definition (class initargs)
  (apply #'%make-direct-slotd
	 (apply #'direct-slot-definition-class class initargs)
	 :class class
	 initargs))

(defun make-effective-slot-definition (class &rest initargs)
  (declare (dynamic-extent initargs))
  (apply #'%make-effective-slotd
	 (apply #'effective-slot-definition-class class initargs)
	 initargs))

;; Bootstrapping version, replaced in l1-typesys
(fset 'standardized-type-specifier
      (nlambda bootstrapping-standardized-type-specifier (spec)
        (when (and (consp spec)
                   (memq (%car spec) '(and or))
                   (consp (%cdr spec))
                   (null (%cddr spec)))
          (setq spec (%cadr spec)))
        (if (consp spec)
          (cons (%car spec) (mapcar #'standardized-type-specifier (%cdr spec)))
          (or (cdr (assoc spec '((string . base-string))))
              spec))))

;;; The type of an effective slot definition is the intersection of
;;; the types of the direct slot definitions it's initialized from.
(defun dslotd-type-intersection (direct-slots)
  (or (dolist (dslotd direct-slots t)
        (unless (eq t (%slot-definition-type dslotd))
          (return)))
      (standardized-type-specifier
       `(and ,@(mapcar #'(lambda (d) (or (%slot-definition-type d) t))
                       direct-slots)))))

(defmethod compute-effective-slot-definition ((class slots-class)
                                              name
                                              direct-slots)
  
  (let* ((initer (dolist (s direct-slots)
                   (when (%slot-definition-initfunction s)
                     (return s))))
         (documentor (dolist (s direct-slots)
		       (when (%slot-definition-documentation s)
                         (return s))))
         (first (car direct-slots))
         (initargs (let* ((initargs nil))
                     (dolist (dslot direct-slots initargs)
                       (dolist (dslot-arg (%slot-definition-initargs  dslot))
                         (pushnew dslot-arg initargs :test #'eq))))))
    (make-effective-slot-definition
     class
     :name name
     :allocation (%slot-definition-allocation first)
     :documentation (when documentor (nth-value
				      1
				      (%slot-definition-documentation
				       documentor)))
     :class (%slot-definition-class first)
     :initargs initargs
     :initfunction (if initer (%slot-definition-initfunction initer))
     :initform (if initer (%slot-definition-initform initer))
     :type (dslotd-type-intersection direct-slots))))

(defmethod compute-slots ((class slots-class))
  (let* ((slot-name-alist ()))
    (labels ((note-direct-slot (dslot)
               (let* ((sname (%slot-definition-name dslot))
                      (pair (assq sname slot-name-alist)))
                 (if pair
                   (push dslot (cdr pair))
                   (push (list sname dslot) slot-name-alist))))
             (rwalk (tail)
               (when tail
                 (rwalk (cdr tail))
		 (let* ((c (car tail)))
		   (unless (eq c *t-class*)
		     (dolist (dslot (%class-direct-slots c))
		       (note-direct-slot dslot)))))))
      (rwalk (class-precedence-list class)))
    (collect ((effective-slotds))
      (dolist (pair (nreverse slot-name-alist) (effective-slotds))
        (effective-slotds (compute-effective-slot-definition class (car pair) (cdr pair)))))))


(defmethod compute-slots :around ((class std-class))
  (let* ((cpl (%class.cpl class)))
    (multiple-value-bind (instance-slots class-slots)
        (extract-instance-and-class-slotds (call-next-method))
      (setq instance-slots (sort-effective-instance-slotds instance-slots class cpl))
      (do* ((loc 1 (1+ loc))
            (islotds instance-slots (cdr islotds)))
           ((null islotds))
        (declare (fixnum loc))
        (setf (%slot-definition-location (car islotds)) loc))
      (dolist (eslotd class-slots)
        (setf (%slot-definition-location eslotd) 
              (assoc (%slot-definition-name eslotd)
                     (%class-get (%slot-definition-class eslotd)
				 :class-slots)
		     :test #'eq)))
      (append instance-slots class-slots))))

(defmethod compute-slots :around ((class structure-class))
  (let* ((slots (call-next-method))	 )
      (do* ((loc 1 (1+ loc))
            (islotds slots (cdr islotds)))
           ((null islotds) slots)
        (declare (fixnum loc))
        (setf (%slot-definition-location (car islotds)) loc))))

;;; Should eventually do something here.
;(defmethod compute-slots ((s structure-class))
;  (call-next-method))

(defmethod direct-slot-definition-class ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-direct-slot-definition))

(defmethod effective-slot-definition-class ((class structure-class) &rest  initargs)
  (declare (ignore initargs))
  (find-class 'structure-effective-slot-definition))


(defmethod compute-default-initargs ((class slots-class))
  (let* ((initargs ()))
    (dolist (c (%class-precedence-list class) (nreverse initargs))
      (if (typep c 'forward-referenced-class)
	(error
	 "Class precedence list of ~s contains FORWARD-REFERENCED-CLASS ~s ."
	 class c)
	(dolist (i (%class-direct-default-initargs c))
	  (pushnew i initargs :test #'eq :key #'car))))))




(defvar *update-slots-preserve-existing-wrapper* nil)

(defun update-slots (class eslotds)
  (let* ((instance-slots (extract-slotds-with-allocation :instance eslotds))
         (new-ordering
          (let* ((v (make-array (the fixnum (length instance-slots))))
                 (i 0))
            (declare (simple-vector v) (fixnum i))
            (dolist (e instance-slots v)
              (setf (svref v i)
                    (%slot-definition-name e))
              (incf i))))
         (old-wrapper (%class-own-wrapper class))
         (new-wrapper
          (cond ((null old-wrapper)
                 (%cons-wrapper class))
                ((and old-wrapper *update-slots-preserve-existing-wrapper*)
                 old-wrapper)
                (t
                 (make-instances-obsolete class)
                 (%cons-wrapper class)))))
    (setf (%class-slots class) eslotds)
    (setf (%wrapper-instance-slots new-wrapper) new-ordering
          (%wrapper-class-slots new-wrapper) (%class-get class :class-slots)
          (%class-own-wrapper class) new-wrapper)
    (setup-slot-lookup new-wrapper eslotds)))


  
(defun setup-slot-lookup (wrapper eslotds)
  (when eslotds
    (let* ((nslots (length eslotds))
	   (total-slot-ids (current-slot-index))
	   (small (< nslots 255))
	   (map
	    (if small
	      (make-array total-slot-ids :element-type '(unsigned-byte 8))
	      (make-array total-slot-ids :element-type '(unsigned-byte 32))))
	   (table (make-array (the fixnum (1+ nslots))))
	   (i 0))
      (declare (fixnum nslots total-slot-ids i) (simple-vector table))
      (setf (svref table 0) nil)
      (dolist (slotd eslotds)
	(incf i)
        (setf (svref table i) slotd)
        (if small
          (locally (declare (type (simple-array (unsigned-byte 8) (*)) map))
            (setf (aref map
                        (slot-id.index
                         (standard-effective-slot-definition.slot-id slotd)))
                  i))
          (locally (declare (type (simple-array (unsigned-byte 32) (*)) map))
            (setf (aref map
                        (slot-id.index
                         (standard-effective-slot-definition.slot-id slotd)))
                  i))))
      (let* ((lookup-f
              #+ppc-target
              (gvector :function
				(%svref (if small
					  #'%small-map-slot-id-lookup
					  #'%large-map-slot-id-lookup) 0)
				map
				table
				(dpb 1 $lfbits-numreq
				     (ash -1 $lfbits-noname-bit)))
              #+x86-target
              (%clone-x86-function (if small
					  #'%small-map-slot-id-lookup
					  #'%large-map-slot-id-lookup)
                                   map
                                   table
                                   (dpb 1 $lfbits-numreq
				     (ash -1 $lfbits-noname-bit))))
	     (class (%wrapper-class wrapper))
	     (get-f
              #+ppc-target
              (gvector :function
                       (%svref (if small
                                 #'%small-slot-id-value
                                 #'%large-slot-id-value) 0)
                       map
                       table
                       class
                       #'%maybe-std-slot-value-using-class
                       #'%slot-id-ref-missing
                       (dpb 2 $lfbits-numreq
                            (ash -1 $lfbits-noname-bit)))
              #+x86-target
              (%clone-x86-function (if small
                                     #'%small-slot-id-value
                                     #'%large-slot-id-value)
                                   map
                                   table
                                   class
                                   #'%maybe-std-slot-value-using-class
                                   #'%slot-id-ref-missing
                                   (dpb 2 $lfbits-numreq
                                        (ash -1 $lfbits-noname-bit))))
	     (set-f
              #+ppc-target
              (gvector :function
                       (%svref (if small
                                 #'%small-set-slot-id-value
                                 #'%large-set-slot-id-value) 0)
                       map
                       table
                       class
                       #'%maybe-std-setf-slot-value-using-class
                       #'%slot-id-set-missing
                       (dpb 3 $lfbits-numreq
                            (ash -1 $lfbits-noname-bit)))
              #+x86-target
              (%clone-x86-function
               (if small
                 #'%small-set-slot-id-value
                 #'%large-set-slot-id-value)
               map
               table
               class
               #'%maybe-std-setf-slot-value-using-class
               #'%slot-id-set-missing
               (dpb 3 $lfbits-numreq
                    (ash -1 $lfbits-noname-bit)))))
	(setf (%wrapper-slot-id->slotd wrapper) lookup-f
	      (%wrapper-slot-id-value wrapper) get-f
	      (%wrapper-set-slot-id-value wrapper) set-f
	      (%wrapper-slot-id-map wrapper) map
	      (%wrapper-slot-definition-table wrapper) table))))
  wrapper)

                       
    

(defmethod validate-superclass ((class class) (super class))
  (or (eq super *t-class*)
      (let* ((class-of-class (class-of class))
             (class-of-super (class-of super)))
        (or (eq class-of-class class-of-super)
            (and (eq class-of-class *standard-class-class*)
                 (eq class-of-super *funcallable-standard-class-class*))
            (and (eq class-of-class *funcallable-standard-class-class*)
                 (eq class-of-super *standard-class-class*))))))

(defmethod validate-superclass ((class foreign-class) (super standard-class))
  t)

(defmethod validate-superclass ((class std-class) (super forward-referenced-class))
  t)


(defmethod add-direct-subclass ((class class) (subclass class))
  (pushnew subclass (%class-direct-subclasses class))
  subclass)

(defmethod remove-direct-subclass ((class class) (subclass class))
  (setf (%class-direct-subclasses class)
        (remove subclass (%class-direct-subclasses class)))
  subclass)

(defun add-direct-subclasses (class new)
  (dolist (n new)
    (unless (memq class (%class-direct-subclasses  class))
      (add-direct-subclass n class))))

(defun remove-direct-subclasses (class old-supers new-supers)
  (dolist (o old-supers)
    (unless (memq o new-supers)
      (remove-direct-subclass o class))))

;;; Built-in classes are always finalized.
(defmethod class-finalized-p ((class class))
  t)

;;; Standard classes are finalized if they have a wrapper and that
;;; wrapper has an instance-slots vector; that implies that
;;; both UPDATE-CPL and UPDATE-SLOTS have been called on the class.
(defmethod class-finalized-p ((class std-class))
  (let* ((w (%class-own-wrapper class)))
    (and w (typep (%wrapper-instance-slots w) 'vector))))

(defmethod finalize-inheritance ((class std-class))
  (update-class class t))


(defmethod finalize-inheritance ((class forward-referenced-class))
  (error "Class ~s can't be finalized." class))

(defmethod class-primary-p ((class slots-class))
  (%class-primary-p class))

(defmethod (setf class-primary-p) (new (class std-class))
  (setf (%class-primary-p class) new))

(defmethod class-primary-p ((class class))
  t)

(defmethod (setf class-primary-p) (new (class class))
  new)


(defun forward-referenced-class-p (class)
  (and (%standard-instance-p class)
       (eq (%class-of-instance class) *forward-referenced-class-class*)))

;;; This uses the primary class information to sort the slots of a class.
(defun sort-effective-instance-slotds (slotds class cpl)
  (let (primary-slotds
        primary-slotds-class
        (primary-slotds-length 0))
    (declare (fixnum primary-slotds-length))
    (dolist (sup (cdr cpl))
      (unless (eq sup *t-class*)      
        (when (class-primary-p sup)
          (let ((sup-slotds (extract-instance-effective-slotds sup)))
            (if (null primary-slotds-class)
              (setf primary-slotds-class sup
                    primary-slotds sup-slotds
                    primary-slotds-length (length sup-slotds))
              (let ((sup-slotds-length (length sup-slotds)))
                (do* ((i 0 (1+ i))
                      (n (min sup-slotds-length primary-slotds-length))
                      (sup-slotds sup-slotds (cdr sup-slotds))
                      (primary-slotds primary-slotds (cdr primary-slotds)))
                     ((= i n))
                  (unless (eq (%slot-definition-name (car sup-slotds))
                              (%slot-definition-name (car primary-slotds)))
                    (error "While initializing ~s:~%~
                            attempt to mix incompatible primary classes:~%~
                            ~s and ~s"
                           class sup primary-slotds-class)))
                (when (> sup-slotds-length primary-slotds-length)
                  (setq primary-slotds-class sup
                        primary-slotds sup-slotds
                        primary-slotds-length sup-slotds-length))))))))
    (if (null primary-slotds-class)
      slotds
      (flet ((slotd-position (slotd)
               (let* ((slotd-name (%slot-definition-name slotd)))
                 (do* ((i 0 (1+ i))
                       (primary-slotds primary-slotds (cdr primary-slotds)))
                      ((= i primary-slotds-length) primary-slotds-length)
                   (declare (fixnum i))
                   (when (eq slotd-name
                                (%slot-definition-name (car primary-slotds)))
                   (return i))))))
        (declare (dynamic-extent #'slotd-position))
        (sort-list slotds '< #'slotd-position)))))




(defun update-cpl (class cpl)
  (if (class-finalized-p class)
    (unless (equal (%class.cpl class) cpl)
      (setf (%class.cpl class) cpl)
      #|(force-cache-flushes class)|#)
    (setf (%class.cpl class) cpl))
  cpl)


(defun class-has-a-forward-referenced-superclass-p (original)
  (labels ((scan-forward-refs (class seen)
             (unless (memq class seen)
               (or (if (forward-referenced-class-p class) class)
                   (let ((seen (cons class seen)))
		     (declare (dynamic-extent seen))
                     (dolist (s (%class-direct-superclasses class))
                       (when (eq s original)
                         (error "circular class hierarchy: the class ~s is a superclass of at least one of its superclasses (~s)." original class))
                       (let* ((fwdref (scan-forward-refs s seen)))
                         (when fwdref (return fwdref)))))))))
    (or (compile-time-class-p original)
        (scan-forward-refs original ()))))

(defun class-forward-referenced-superclasses (original)
  (labels ((scan-forward-refs (class seen fwdrefs)
             (unless (memq class seen)
	       (if (forward-referenced-class-p class)
		 (push class fwdrefs)
		 (let ((seen (cons class seen)))
		   (declare (dynamic-extent seen))
		   (dolist (s (%class-direct-superclasses class))
		     (when (eq s original)
		       (error "circular class hierarchy: the class ~s is a superclass of at least one of its superclasses (~s)." original class))
		     (setq fwdrefs (scan-forward-refs s seen fwdrefs))))))
	     fwdrefs))
    (scan-forward-refs original () ())))
  


(defmethod compute-class-precedence-list ((class class))
  (let* ((fwdrefs (class-forward-referenced-superclasses class)))
    (if fwdrefs
      (if (cdr fwdrefs)
	(error "Class ~s can't be finalized because superclasses ~s are not defined yet"
	       class (mapcar #'%class-name fwdrefs))
	(error "Class ~s can't be finalized because superclass ~s is not defined yet"
	       class (%class-name (car fwdrefs))))
      (compute-cpl class))))

;;; Classes that can't be instantiated via MAKE-INSTANCE have no
;;; initargs caches.
(defmethod %flush-initargs-caches ((class class))
  )

;;; Classes that have initargs caches should flush them when the
;;; class is finalized.
(defmethod %flush-initargs-caches ((class std-class))
  (setf (%class.make-instance-initargs class) nil
	(%class.reinit-initargs class) nil
	(%class.redefined-initargs class) nil
	(%class.changed-initargs class) nil))

(defun update-class (class finalizep)
  ;;
  ;; Calling UPDATE-SLOTS below sets the class wrapper of CLASS, which
  ;; makes the class finalized.  When UPDATE-CLASS isn't called from
  ;; FINALIZE-INHERITANCE, make sure that this finalization invokes
  ;; FINALIZE-INHERITANCE as per AMOP.  Note, that we can't simply
  ;; delay the finalization when CLASS has no forward referenced
  ;; superclasses because that causes bootstrap problems.
  (when (and (not (or finalizep (class-finalized-p class)))
	     (not (class-has-a-forward-referenced-superclass-p class)))
    (finalize-inheritance class)
    (return-from update-class))
  (when (or finalizep (class-finalized-p class))
    (let* ((cpl (update-cpl class (compute-class-precedence-list  class))))
      ;; This -should- be made to work for structure classes
      (update-slots class (compute-slots class))
      (setf (%class-default-initargs class) (compute-default-initargs class))
      (%flush-initargs-caches class)
      (let* ((wrapper (%class-own-wrapper class)))
        (when wrapper
          (setf (%wrapper-cpl wrapper) cpl
                (%wrapper-cpl-bits wrapper) (make-cpl-bits cpl))))))
  (unless finalizep
    (dolist (sub (%class-direct-subclasses class))
      (update-class sub nil))))

(defun add-accessor-methods (class dslotds)
  (dolist (dslotd dslotds)
    (dolist (reader (%slot-definition-readers dslotd))
      (add-reader-method class
                         (ensure-generic-function reader)
                         dslotd))
    (dolist (writer (%slot-definition-writers dslotd))
      (add-writer-method class
			 (ensure-generic-function writer)
			 dslotd))))

(defun remove-accessor-methods (class dslotds)
  (dolist (dslotd dslotds)
    (dolist (reader (%slot-definition-readers dslotd))
      (remove-reader-method class (ensure-generic-function reader :lambda-list '(x))))
    (dolist (writer (%slot-definition-writers dslotd))
      (remove-writer-method class (ensure-generic-function writer :lambda-list '(x y))))))

(defmethod reinitialize-instance :before ((class std-class)  &key direct-superclasses)
  (remove-accessor-methods class (%class-direct-slots class))
  (remove-direct-subclasses class (%class-direct-superclasses class) direct-superclasses))
   
(defmethod shared-initialize :after
  ((class slots-class)
   slot-names &key
   (direct-superclasses nil direct-superclasses-p)
   (direct-slots nil direct-slots-p)
   (direct-default-initargs nil direct-default-initargs-p)
   (documentation nil doc-p)
   (primary-p nil primary-p-p))
  (declare (ignore slot-names))
  (if direct-superclasses-p
    (progn
      (setq direct-superclasses
            (or direct-superclasses
                (list (if (typep class 'funcallable-standard-class)
                        *funcallable-standard-object-class*
                        *standard-object-class*))))
      (dolist (superclass direct-superclasses)
        (unless (validate-superclass class superclass)
          (error "The class ~S was specified as a~%super-class of the class ~S;~%~
                    but the meta-classes ~S and~%~S are incompatible."
                 superclass class (class-of superclass) (class-of class))))
      (setf (%class-direct-superclasses class) direct-superclasses))
    (setq direct-superclasses (%class-direct-superclasses class)))
  (setq direct-slots
	(if direct-slots-p
          (setf (%class-direct-slots class)
                (mapcar #'(lambda (initargs)
			    (make-direct-slot-definition class initargs))
			direct-slots))
          (%class-direct-slots class)))
  (if direct-default-initargs-p
    (setf (%class-direct-default-initargs class)  direct-default-initargs)
    (setq direct-default-initargs (%class-direct-default-initargs class)))
  (let* ((new-class-slot-cells ())
         (old-class-slot-cells (%class-get class :class-slots)))
    (dolist (slot direct-slots)
      (when (eq (%slot-definition-allocation slot) :class)
        (let* ((slot-name (%slot-definition-name slot))
               (pair (assq slot-name old-class-slot-cells)))
          ;;; If the slot existed as a class slot in the old
          ;;; class, retain the definition (even if it's unbound.)
          (unless pair
            (let* ((initfunction (%slot-definition-initfunction slot)))
              (setq pair (cons slot-name
                               (if initfunction
                                 (funcall initfunction)
                                 (%slot-unbound-marker))))))
          (push pair new-class-slot-cells))))
    (when new-class-slot-cells
      (setf (%class-get class :class-slots) new-class-slot-cells)))
  (when doc-p
    (set-documentation class 'type documentation))
  (when primary-p-p
    (setf (class-primary-p class) primary-p))

  (add-direct-subclasses class direct-superclasses)
  (update-class class nil)
  (add-accessor-methods class direct-slots))

(defmethod initialize-instance :before ((class class) &key &allow-other-keys)
  (setf (%class-ordinal class) (%next-class-ordinal))
  (setf (%class.ctype class) (make-class-ctype class)))

(defun ensure-class-metaclass-and-initargs (class args)
  (let* ((initargs (copy-list args))
         (missing (cons nil nil))
         (supplied-meta (getf initargs :metaclass missing))
         (supplied-supers (getf initargs :direct-superclasses missing))
         (supplied-slots (getf initargs :direct-slots missing))
         (metaclass (cond ((not (eq supplied-meta missing))
			   (if (typep supplied-meta 'class)
			     supplied-meta
			     (find-class supplied-meta)))
                          ((or (null class)
                               (typep class 'forward-referenced-class))
                           *standard-class-class*)
                          (t (class-of class)))))
    (declare (dynamic-extent missing))
    (flet ((fix-super (s)
             (cond ((classp s) s)
                   ((not (and s (symbolp s)))
                    (error "~s is not a class or a legal class name." s))
                   (t
                    (or (find-class s nil)
			(setf (find-class s)
			      (make-instance 'forward-referenced-class :name s))))))
           (excise-all (keys)
             (dolist (key keys)
               (loop (unless (remf initargs key) (return))))))
      (excise-all '(:metaclass :direct-superclasses :direct-slots))
      (values metaclass
              `(,@ (unless (eq supplied-supers missing)
                     `(:direct-superclasses ,(mapcar #'fix-super supplied-supers)))
                ,@ (unless (eq supplied-slots missing)
                     `(:direct-slots ,supplied-slots))
               ,@initargs)))))


;;; This defines a new class.
(defmethod ensure-class-using-class ((class null) name &rest keys &key &allow-other-keys)
  (multiple-value-bind (metaclass initargs)
      (ensure-class-metaclass-and-initargs class keys)
    (let* ((class (apply #'make-instance metaclass :name name initargs)))
      (setf (find-class name) class))))

(defmethod ensure-class-using-class ((class forward-referenced-class) name &rest keys &key &allow-other-keys)
  (multiple-value-bind (metaclass initargs)
      (ensure-class-metaclass-and-initargs class keys)
    (apply #'change-class class metaclass initargs)
    (apply #'reinitialize-instance class initargs)
    (setf (find-class name) class)))
	   
;; Can't go with optimize-make-instance-for-class-name because
;; ensure-class-using-class is called before that is defined.
(defun pessimize-make-instance-for-class-name (class-name)
  (let ((cell (find-class-cell class-name nil)))
    (when cell
      (setf (class-cell-instantiate cell) '%make-instance))))

;;; Redefine an existing (not forward-referenced) class.
(defmethod ensure-class-using-class ((class class) name &rest keys &key)
  (multiple-value-bind (metaclass initargs)
      (ensure-class-metaclass-and-initargs class keys)
    (unless (eq (class-of class) metaclass)
      (error "Can't change metaclass of ~s to ~s." class metaclass))
    (pessimize-make-instance-for-class-name name)
    (apply #'reinitialize-instance class initargs)
    (setf (find-class name) class)))


(defun ensure-class (name &rest keys &key &allow-other-keys)
  (declare (special *sealed-clos-world*))
  (if *sealed-clos-world*
    (error "Class (re)definition is not allowed in this environment")
    (apply #'ensure-class-using-class (find-class name nil) name keys)))

(defparameter *defclass-redefines-improperly-named-classes-pedantically* 
   t
  "ANSI CL expects DEFCLASS to redefine an existing class only when
the existing class is properly named, the MOP function ENSURE-CLASS
redefines existing classes regardless of their CLASS-NAME.  This variable
governs whether DEFCLASS makes that distinction or not.")

(defun ensure-class-for-defclass (name &rest keys &key &allow-other-keys)
  (declare (special *sealed-clos-world*))
  (if *sealed-clos-world*
    (error "Class (re)definition is not allowed in this environment")
    (progn
      (record-source-file name 'class)
      ;; Maybe record source-file information for accessors as well
      ;; We should probably record them as "accessors of the class", since
      ;; there won't be any other explicit defining form associated with
      ;; them.
      (let* ((existing-class (find-class name nil)))
        (when (and *defclass-redefines-improperly-named-classes-pedantically* 
                   existing-class 
                   (not (eq (class-name existing-class) name)))
          ;; Class isn't properly named; act like it didn't exist
          (setq existing-class nil))
        (apply #'ensure-class-using-class existing-class name keys)))))




(defmethod method-slot-name ((m standard-accessor-method))
  (standard-direct-slot-definition.name (%accessor-method.slot-definition m)))


(defun %ensure-class-preserving-wrapper (&rest args)
  (declare (dynamic-extent args))
  (let* ((*update-slots-preserve-existing-wrapper* t))
    (apply #'ensure-class args)))

(defun %find-direct-slotd (class name)
  (dolist (dslotd (%class-direct-slots class)
           (error "Direct slot definition for ~s not found in ~s" name class))
    (when (eq (%slot-definition-name dslotd) name)
      (return dslotd))))

(defun %add-slot-readers (class-name pairs)
  (let* ((class (find-class class-name)))
    (dolist (pair pairs)
      (destructuring-bind (slot-name &rest readers) pair
        (setf (%slot-definition-readers (%find-direct-slotd class slot-name)) readers)))
    (add-accessor-methods class (%class-direct-slots class))))

(defun %add-slot-writers (class-name pairs)
  (let* ((class (find-class class-name)))
    (dolist (pair pairs)
      (destructuring-bind (slot-name &rest readers) pair
        (setf (%slot-definition-writers (%find-direct-slotd class slot-name)) readers)))
    (add-accessor-methods class (%class-direct-slots class))))


(%ensure-class-preserving-wrapper
 'standard-method
 :direct-superclasses '(method)
 :direct-slots `((:name qualifiers :initargs (:qualifiers) :initfunction ,#'false :initform nil)
                 (:name specializers :initargs (:specializers) :initfunction ,#'false :initform nil)
                 (:name function :initargs (:function))
                 (:name generic-function :initargs (:generic-function) :initfunction ,#'false :initform nil)
                 (:name name :initargs (:name) :initfunction ,#'false :initform nil)
		 (:name lambda-list :initform nil :initfunction ,#'false
		  :initargs (:lambda-list)))
 :primary-p t)

(defmethod shared-initialize :after ((method standard-method)
                                     slot-names
                                     &key function &allow-other-keys)
  (declare (ignore slot-names))
  (when function
    (let* ((inner (closure-function function)))
      (unless (eq inner function)
	(copy-method-function-bits inner function)))    
    (lfun-name function method)))

;;; Reader & writer methods classes.
(%ensure-class-preserving-wrapper
 'standard-accessor-method
 :direct-superclasses '(standard-method)
 :direct-slots '((:name slot-definition :initargs (:slot-definition)))
 :primary-p t)

(%ensure-class-preserving-wrapper
 'standard-reader-method
 :direct-superclasses '(standard-accessor-method))

(%ensure-class-preserving-wrapper
 'standard-writer-method
 :direct-superclasses '(standard-accessor-method))

(defmethod reader-method-class ((class standard-class)
				(dslotd standard-direct-slot-definition)
				&rest initargs)
  (declare (ignore initargs))
  *standard-reader-method-class*)

(defmethod reader-method-class ((class funcallable-standard-class)
				(dslotd standard-direct-slot-definition)
				&rest initargs)
  (declare (ignore  initargs))
  *standard-reader-method-class*)

(defmethod add-reader-method ((class slots-class) gf dslotd)
  (let* ((initargs
	  `(:qualifiers nil
	    :specializers ,(list class)
	    :lambda-list (,(or (%class-name class) 'instance))
	    :name ,(function-name gf)
	    :slot-definition ,dslotd))
	 (reader-method-class
	  (apply #'reader-method-class class dslotd initargs))
	 (method-function (create-reader-method-function
			   class (class-prototype reader-method-class) dslotd))
         (method (apply #'make-instance reader-method-class
			:function method-function
			initargs)))
    (declare (dynamic-extent initargs))
    (record-source-file method 'reader-method)
    (add-method gf method)))

(defmethod remove-reader-method ((class std-class) gf)
  (let* ((method (find-method gf () (list class) nil)))
    (when method (remove-method gf method))))

(defmethod writer-method-class ((class standard-class)
				(dslotd standard-direct-slot-definition)
				&rest initargs)
  (declare (ignore initargs))
  *standard-writer-method-class*)

(defmethod writer-method-class ((class funcallable-standard-class)
				(dslotd standard-direct-slot-definition)
				&rest initargs)
  (declare (ignore initargs))
  *standard-writer-method-class*)


(defmethod add-writer-method ((class slots-class) gf dslotd)
  (let* ((initargs
	  `(:qualifiers nil
	    :specializers ,(list *t-class* class)
	    :lambda-list (new-value ,(or (%class-name class) 'instance))
	    :name ,(function-name gf)
	    :slot-definition ,dslotd))
	 (method-class (apply #'writer-method-class class dslotd initargs))
	 (method 
	  (apply #'make-instance
		 method-class
		 :function (create-writer-method-function
			    class
			    (class-prototype method-class)
			    dslotd)
		 initargs)))
    (declare (dynamic-extent initargs))
    (record-source-file method 'writer-method)
    (add-method gf method)))

(defmethod remove-writer-method ((class std-class) gf)
  (let* ((method (find-method gf () (list *t-class* class) nil)))
    (when method (remove-method gf method))))

;;; We can now define accessors.  Fix up the slots in the classes defined
;;; thus far.

(%add-slot-readers 'standard-method '((qualifiers method-qualifiers)
				      (specializers method-specializers)
				      (name method-name)
				      ;(function method-function)
				      (generic-function method-generic-function)
				      (lambda-list method-lambda-list)))

(%add-slot-writers 'standard-method '((function (setf method-function))
				      (generic-function (setf method-generic-function))))


(defmethod method-function ((m standard-method))
  (%method.function m))


(%add-slot-readers 'standard-accessor-method
		   '((slot-definition accessor-method-slot-definition)))


(%ensure-class-preserving-wrapper
 'specializer
 :direct-superclasses '(metaobject)
 :direct-slots `((:name direct-methods
		  :readers (specializer-direct-methods)
		  :initform nil :initfunction ,#'false))
 :primary-p t)
		  
(%ensure-class-preserving-wrapper
 'eql-specializer
 :direct-superclasses '(specializer)
 :direct-slots '((:name object :initargs (:object) :readers (eql-specializer-object)))
 :primary-p t)


(%ensure-class-preserving-wrapper
 'class
 :direct-superclasses '(specializer)
 :direct-slots
 `((:name prototype :initform nil :initfunction ,#'false)
   (:name name :initargs (:name) :initform nil :initfunction ,#'false :readers (class-name))
   (:name precedence-list :initform nil  :initfunction ,#'false)
   (:name own-wrapper :initform nil  :initfunction ,#'false :readers (class-own-wrapper) :writers ((setf class-own-wrapper)))
   (:name direct-superclasses  :initform nil  :initfunction ,#'false :readers (class-direct-superclasses))
   (:name direct-subclasses  :initform nil  :initfunction ,#'false :readers (class-direct-subclasses))
   (:name dependents :initform nil :initfunction ,#'false)
   (:name class-ctype :initform nil :initfunction ,#'false)
   (:name direct-slots :initform nil :initfunction ,#'false
                  :readers (class-direct-slots)
		  :writers ((setf class-direct-slots)))
   (:name slots :initform nil :initfunction ,#'false
    :readers (class-slots)
    :writers ((setf class-slots)))
   (:name info :initform (cons nil nil) :initfunction ,(lambda () (cons nil nil)) :readers (class-info))
   (:name direct-default-initargs  :initform nil  :initfunction ,#'false :readers (class-direct-default-initargs))
   (:name default-initargs :initform nil  :initfunction ,#'false :readers (class-default-initargs)))
 :primary-p t)

(%ensure-class-preserving-wrapper
 'forward-referenced-class
 :direct-superclasses '(class))



(%ensure-class-preserving-wrapper
 'built-in-class
 :direct-superclasses '(class))


(%ensure-class-preserving-wrapper
 'slots-class
 :direct-superclasses '(class)
 :direct-slots `((:name alist :initform nil  :initfunction ,#'false))
 :primary-p t)

;;; This class exists only so that standard-class & funcallable-standard-class
;;; can inherit its slots.
(%ensure-class-preserving-wrapper
 'std-class
 :direct-superclasses '(slots-class)
 :direct-slots `(
                 (:name make-instance-initargs :initform nil  :initfunction ,#'false)
                 (:name reinit-initargs :initform nil  :initfunction ,#'false)
                 (:name redefined-initargs :initform nil :initfunction ,#'false)
                 (:name changed-initargs :initform nil  :initfunction ,#'false))
 :primary-p t)



(%ensure-class-preserving-wrapper
 'standard-class
 :direct-superclasses '(std-class))

(%ensure-class-preserving-wrapper
 'funcallable-standard-class
 :direct-superclasses '(std-class))


(%ensure-class-preserving-wrapper
 'funcallable-standard-object
#|| 
 :direct-superclasses '(standard-object function)
||#
 :direct-slots `((:name name :initargs (:name) :readers (generic-function-name)))
 :metaclass 'funcallable-standard-class)

(%ensure-class-preserving-wrapper
 'generic-function
 :direct-superclasses '(metaobject funcallable-standard-object)
 :direct-slots `(
		 (:name method-combination :initargs (:method-combination)
                  :initform *standard-method-combination*
                  :initfunction ,#'(lambda () *standard-method-combination*)
		  :readers (generic-function-method-combination))
                 (:name method-class :initargs (:method-class)
                  :initform *standard-method-class*
                  :initfunction ,#'(lambda () *standard-method-class*)
		  :readers (generic-function-method-class))
		 (:name methods :initargs (:methods)
		  :initform nil :initfunction ,#'false
		  :readers (generic-function-methods))
		 (:name declarations
		  :initargs (:declarations)
		  :initform nil :initfunction ,#'false
		  :readers (generic-function-declarations))
                 (:name %lambda-list
                  :initform :unspecified
                  :initfunction ,(constantly :unspecified))
		 (:name dependents
		  :initform nil :initfunction ,#'false)) 
 :metaclass 'funcallable-standard-class)



(%ensure-class-preserving-wrapper
 'standard-generic-function
 :direct-superclasses '(generic-function)

 :metaclass 'funcallable-standard-class
 :primary-p t)

(%ensure-class-preserving-wrapper
 'standard-generic-function
 :direct-superclasses '(generic-function)

 :metaclass 'funcallable-standard-class)

(%ensure-class-preserving-wrapper
 'structure-class
 :direct-superclasses '(slots-class))

(%ensure-class-preserving-wrapper
 'slot-definition
 :direct-superclasses '(metaobject)
  :direct-slots `((:name name :initargs (:name) :readers (slot-definition-name)
		  :initform nil :initfunction ,#'false)
		 (:name type :initargs (:type) :readers (slot-definition-type)
		  :initform t :initfunction ,#'true)
		 (:name initfunction :initargs (:initfunction) :readers (slot-definition-initfunction)
		  :initform nil :initfunction ,#'false)
		 (:name initform :initargs (:initform) :readers (slot-definition-initform)
		  :initform nil :initfunction ,#'false)
		 (:name initargs :initargs (:initargs) :readers (slot-definition-initargs)
		  :initform nil :initfunction ,#'false)
		 (:name allocation :initargs (:allocation) :readers (slot-definition-allocation)
		  :initform :instance :initfunction ,(constantly :instance))
		 (:name documentation :initargs (:documentation) :readers (slot-definition-documentation)
		  :initform nil :initfunction ,#'false)
		 (:name class :initargs (:class) :readers (slot-definition-class)))
  
 :primary-p t)

(%ensure-class-preserving-wrapper
 'direct-slot-definition
 :direct-superclasses '(slot-definition)
 :direct-slots `((:name readers :initargs (:readers) :initform nil
		  :initfunction ,#'false :readers (slot-definition-readers))
		 (:name writers :initargs (:writers) :initform nil
		  :initfunction ,#'false :readers (slot-definition-writers))))

(%ensure-class-preserving-wrapper
 'effective-slot-definition
 :direct-superclasses '(slot-definition)
 :direct-slots `((:name location :initform nil :initfunction ,#'false
		  :readers (slot-definition-location))
		 (:name slot-id :initform nil :initfunction ,#'false
                  :readers (slot-definition-slot-id))
		 (:name type-predicate :initform nil
		  :initfunction ,#'false
		  :readers (slot-definition-predicate))
		 )
 
 :primary-p t)

(%ensure-class-preserving-wrapper
 'standard-slot-definition
 :direct-superclasses '(slot-definition)
)







(%ensure-class-preserving-wrapper
 'standard-direct-slot-definition
 :direct-superclasses '(standard-slot-definition direct-slot-definition)
)

(%ensure-class-preserving-wrapper
 'standard-effective-slot-definition
 :direct-superclasses '(standard-slot-definition effective-slot-definition))

		 


      
                             



;;; Fake method-combination, redefined in lib;method-combination.
(defclass method-combination (metaobject) 
  ((name :initarg :name)))




(defclass standard-method-combination (method-combination) ())

(initialize-instance *standard-method-combination* :name 'standard)

(setq *standard-kernel-method-class*
  (defclass standard-kernel-method (standard-method)
    ()))

(unless *standard-method-combination*
  (setq *standard-method-combination*
        (make-instance 'standard-method-combination :name 'standard)))

;;; For %compile-time-defclass
(defclass compile-time-class (class) ())

(defmethod compile-time-class-p ((class compile-time-class))
  t)

(defmethod class-finalized-p ((class compile-time-class))
  nil)


(defclass structure-slot-definition (slot-definition) ())
(defclass structure-effective-slot-definition (structure-slot-definition
					       effective-slot-definition)
    ())

(defclass structure-direct-slot-definition (structure-slot-definition
					    direct-slot-definition)
    ())

(defmethod shared-initialize :after ((class structure-class)
                                     slot-names
                                     &key
                                     (direct-superclasses nil direct-superclasses-p)
				     &allow-other-keys)
  (declare (ignore slot-names))
  (labels ((obsolete (class)
             (dolist (sub (%class-direct-subclasses class)) (obsolete sub))
             ;;Need to save old class info in wrapper for obsolete
             ;;instance access...
             (setf (%class.cpl class) nil)))
    (obsolete class)
    (when direct-superclasses-p
      (let* ((old-supers (%class-direct-superclasses class))
             (new-supers direct-superclasses))
        (dolist (c old-supers)
          (unless (memq c new-supers)
            (remove-direct-subclass c class)))
        (dolist (c new-supers)
          (unless (memq c old-supers)
            (add-direct-subclass c class)))
        (setf (%class.local-supers class) new-supers)))
    (let* ((wrapper (or (%class-own-wrapper class)
                        (setf (%class-own-wrapper class) (%cons-wrapper class))))
           (cpl (compute-cpl class)))
      (setf (%class.cpl class) cpl)
      (setf (%wrapper-cpl wrapper) cpl
            (%wrapper-cpl-bits wrapper) (make-cpl-bits cpl)))))
              

                                     
                                     
;;; Called from DEFSTRUCT expansion.
(defun %define-structure-class (sd)
  (let* ((dslots ()))
    (dolist (ssd (cdr (sd-slots sd)) (setq dslots (nreverse dslots)))
      (let* ((type (ssd-type ssd))
	     (refinfo (ssd-refinfo ssd)))
	(unless (logbitp $struct-inherited refinfo)
	  (let* ((name (ssd-name ssd))
		 (initform (cadr ssd))
		 (initfunction (constantly initform)))
	    (push `(:name ,name :type ,type :initform ,initform :initfunction ,initfunction) dslots)))))
    (ensure-class (sd-name sd)
		  :metaclass 'structure-class
		  :direct-superclasses (list (or (cadr (sd-superclasses sd)) 'structure-object))
		  :direct-slots  dslots 
		  )))


(defun standard-instance-access (instance location)
  (etypecase location
    (fixnum (%standard-instance-instance-location-access instance location))
    (cons (%cdr location))))

(defun (setf standard-instance-access) (new instance location)
  (etypecase location
    (fixnum (setf (standard-instance-instance-location-access instance location)
		  new))
    (cons (setf (%cdr location) new))))

(defun funcallable-standard-instance-access (instance location)
  (etypecase location
    (fixnum (%standard-generic-function-instance-location-access instance location))
    (cons (%cdr location))))

(defun (setf funcallable-standard-instance-access) (new instance location)
  (etypecase location
    (fixnum (setf (%standard-generic-function-instance-location-access instance location) new))
    (cons (setf (%cdr location) new))))

;;; Handle a trap from %slot-ref
(defun %slot-unbound-trap (slotv idx frame-ptr)
  (let* ((instance nil)
	 (class nil)
	 (slot nil))
    (if (and (eq (typecode slotv) target::subtag-slot-vector)
	     (setq instance (slot-vector.instance slotv))
	     (setq slot
		   (find idx (class-slots (setq class (class-of instance)))
			 :key #'slot-definition-location)))
      (slot-unbound class instance (slot-definition-name slot))
      (%error "Unbound slot at index ~d in ~s" (list idx slotv) frame-ptr))))


;;;
;;; Now that CLOS is nominally bootstrapped, it's possible to redefine some
;;; of the functions that really should have been generic functions ...
(setf (fdefinition '%class-name) #'class-name
      (fdefinition '%class-default-initargs) #'class-default-initargs
      (fdefinition '%class-direct-default-initargs) #'class-direct-default-initargs
      (fdefinition '(setf %class-direct-default-initargs))
      #'(lambda (new class)
	  (if (typep class 'slots-class)
	    (setf (slot-value class 'direct-default-initargs) new)
	    new))
      (fdefinition '%class-direct-slots) #'class-direct-slots
      (fdefinition '(setf %class-direct-slots))
		   #'(setf class-direct-slots)
      (fdefinition '%class-slots) #'class-slots
      (fdefinition '%class-direct-superclasses) #'class-direct-superclasses
      (fdefinition '(setf %class-direct-superclasses))
      #'(lambda (new class)
	  (setf (slot-value class 'direct-superclasses) new))
      (fdefinition '%class-direct-subclasses) #'class-direct-subclasses
      ;(fdefinition '%class-own-wrapper) #'class-own-wrapper
      (fdefinition '(setf %class-own-wrapper)) #'(setf class-own-wrapper)
)



(setf (fdefinition '%slot-definition-name) #'slot-definition-name
      (fdefinition '%slot-definition-type) #'slot-definition-type
      (fdefinition '%slot-definition-initargs) #'slot-definition-initargs
      (fdefinition '%slot-definition-allocation) #'slot-definition-allocation
      (fdefinition '%slot-definition-location) #'slot-definition-location
      (fdefinition '%slot-definition-readers) #'slot-definition-readers
      (fdefinition '%slot-definition-writers) #'slot-definition-writers)


(setf (fdefinition '%method-qualifiers) #'method-qualifiers
      (fdefinition '%method-specializers) #'method-specializers
      (fdefinition '%method-function) #'method-function
      (fdefinition '(setf %method-function)) #'(setf method-function)
      (fdefinition '%method-gf) #'method-generic-function
      (fdefinition '(setf %method-gf)) #'(setf method-generic-function)
      (fdefinition '%method-name) #'method-name
      (fdefinition '%method-lambda-list) #'method-lambda-list
      )

(setf (fdefinition '%add-method) #'add-method)
		   
      
;;; Make a direct-slot-definition of the appropriate class.
(defun %make-direct-slotd (slotd-class &rest initargs)
  (declare (dynamic-extent initargs))
  (apply #'make-instance slotd-class initargs))

;;; Likewise, for an effective-slot-definition.
(defun %make-effective-slotd (slotd-class &rest initargs)
  (declare (dynamic-extent initargs))
  (apply #'make-instance slotd-class initargs))

;;; Likewise, for methods
(defun %make-method-instance (class &rest initargs)
  (apply #'make-instance class initargs))

(defmethod initialize-instance :after ((slotd effective-slot-definition) &key name)
  (setf (standard-effective-slot-definition.slot-id slotd)
        (ensure-slot-id name)))

  
(defmethod specializer-direct-generic-functions ((s specializer))
  (let* ((gfs ())
	 (methods (specializer-direct-methods s)))
    (dolist (m methods gfs)
      (let* ((gf (method-generic-function m)))
	(when gf (pushnew gf gfs))))))

(defmethod generic-function-lambda-list ((gf standard-generic-function))
  (%maybe-compute-gf-lambda-list gf (car (generic-function-methods gf))))

(defmethod generic-function-argument-precedence-order
    ((gf standard-generic-function))
  (let* ((req (required-lambda-list-args (generic-function-lambda-list gf)))
	 (apo (%gf-dispatch-table-precedence-list
	       (%gf-dispatch-table gf))))
    (if (null apo)
      req
      (mapcar #'(lambda (n) (nth n req)) apo))))

(defun normalize-egf-keys (keys gf)
  (let* ((missing (cons nil nil))
	 (env (getf keys :environment nil)))
    (declare (dynamic-extent missing))
    (remf keys :environment)
    (let* ((gf-class (getf keys :generic-function-class missing))
	   (mcomb (getf keys :method-combination missing))
	   (method-class (getf keys :method-class missing)))
      (if (eq gf-class missing)
	(setf gf-class (if gf (class-of gf) *standard-generic-function-class*))
	(progn
	  (remf keys :generic-function-class)
	  (if (typep gf-class 'symbol)
	    (setq gf-class
		  (find-class gf-class t env)))
	  (unless (or (eq gf-class *standard-generic-function-class*)
		      (subtypep gf-class *generic-function-class*))
	    (error "Class ~S is not a subclass of ~S"
                   gf-class *generic-function-class*))))
      (unless (eq mcomb missing)
	(unless (typep mcomb 'method-combination)
	  (setf (getf keys :method-combination)
		(find-method-combination (class-prototype gf-class)
					 (car mcomb)
					 (cdr mcomb)))))
      (unless (eq method-class missing)
	(if (typep method-class 'symbol)
	  (setq method-class (find-class method-class t env)))
	(unless (subtypep method-class *method-class*)
	  (error "~s is not a subclass of ~s" method-class *method-class*))
	(setf (getf keys :method-class) method-class))
      (values gf-class keys))))
    
(defmethod ensure-generic-function-using-class
    ((gf null)
     function-name
     &rest keys
     &key
     &allow-other-keys)
  (declare (dynamic-extent keys))
  (multiple-value-bind (gf-class initargs)
      (normalize-egf-keys keys nil)
    (let* ((gf (apply #'make-instance gf-class
		      :name function-name
		      initargs)))
      (setf (fdefinition function-name) gf))))

(defparameter *error-on-gf-class-redefinition* nil
  "The MOP spec requires ENSURE-GENERIC-FUNCTION-USING-CLASS of an
   existing gf to signal an error if the :GENERIC-FUNCTION-CLASS
   argument specifies a class other than the existing gf's class.
   ANSI CL allows this kind of redefinition if the classes are
   \"compatible\", but doesn't define what compatibility means
   in this case.  When *ERROR-ON-GF-CLASS-REDEFINITION* is true,
   a continuable error is signaled.

   Historically, Clozure CL CERRORed, but didn't offer a useful
   CHANGE-CLASS method that would change the GF's class")

(defmethod ensure-generic-function-using-class
    ((gf generic-function)
     function-name
     &rest keys
     &key
     &allow-other-keys)
  (declare (dynamic-extent keys) (ignorable function-name))
  (multiple-value-bind (gf-class initargs)
      (normalize-egf-keys keys gf)
    (unless (eq gf-class (class-of gf))
      (when *error-on-gf-class-redefinition*
        (cerror (format nil "Change the class of ~s to ~s." gf gf-class)
                "The class of the existing generic function ~s is not ~s"
                gf gf-class))
      (change-class gf gf-class))
    (apply #'reinitialize-instance gf initargs)))


(defmethod initialize-instance :before ((instance generic-function)
                                       &key &allow-other-keys)
  (setf (%gf-dcode instance)  #'%%0-arg-dcode))

(defmethod initialize-instance :after ((gf standard-generic-function)
				       &key
				       (lambda-list nil ll-p)
				       (argument-precedence-order nil apo-p)
				       &allow-other-keys)
  (if (and apo-p (not ll-p))
    (error
     "Cannot specify :ARGUMENT-PRECEDENCE-ORDER without specifying :LAMBDA-LIST"))
  (if ll-p
    (progn
      (unless (verify-lambda-list lambda-list)
	(error "~s is not a valid generic function lambda list" lambda-list))
      (if apo-p
	(set-gf-arg-info gf :lambda-list lambda-list
			 :argument-precedence-order argument-precedence-order)
	(set-gf-arg-info gf :lambda-list lambda-list)))
    (set-gf-arg-info gf))
  (if (gf-arg-info-valid-p gf)
    (compute-dcode gf (%gf-dispatch-table gf)))
  gf)

(defmethod reinitialize-instance :after ((gf standard-generic-function)
					 &rest args
					 &key
					 (lambda-list nil ll-p)
					 (argument-precedence-order nil apo-p)
					 &allow-other-keys)
  (if (and apo-p (not ll-p))
    (error
     "Cannot specify :ARGUMENT-PRECEDENCE-ORDER without specifying :LAMBDA-LIST"))
  (if ll-p
    (progn
      (unless (verify-lambda-list lambda-list)
	(error "~s is not a valid generic function lambda list" lambda-list))
      (if apo-p
	(set-gf-arg-info gf :lambda-list lambda-list
			 :argument-precedence-order argument-precedence-order)
	(set-gf-arg-info gf :lambda-list lambda-list)))
    (set-gf-arg-info gf))
  (if (and (gf-arg-info-valid-p gf)
	   args
	   (or ll-p (cddr args)))
    (compute-dcode gf (%gf-dispatch-table gf)))
  (when (sgf.dependents gf)
    (map-dependents gf #'(lambda (d)
			   (apply #'update-dependent gf d args))))
  gf)
  

(defun decode-method-lambda-list (method-lambda-list)
  (flet ((bad ()
	   (error "Invalid lambda-list syntax in ~s" method-lambda-list)))
    (collect ((specnames)
                    (required))
       (do* ((tail method-lambda-list (cdr tail))
	     (head (car tail) (car tail)))
	    ((or (null tail) (member head lambda-list-keywords))
	     (if (verify-lambda-list tail)
	       (values (required) tail (specnames))
	       (bad)))
	 (cond ((atom head)
		(unless (typep head 'symbol) (bad))
		(required head)
		(specnames t))
	       (t
		(unless (and (typep (car head) 'symbol)
			     (consp (cdr head))
			     (null (cddr head)))
		  (bad))
		(required (car head))
		(specnames (cadr head))))))))
  
(defun extract-specializer-names (method-lambda-list)
  (nth-value 2 (decode-method-lambda-list method-lambda-list)))

(defun extract-lambda-list (method-lambda-list)
  (multiple-value-bind (required tail)
      (decode-method-lambda-list method-lambda-list)
    (nconc required tail)))

(setf (fdefinition '%ensure-generic-function-using-class)
      #'ensure-generic-function-using-class)


(defmethod shared-initialize :after ((gf generic-function) slot-names
				     &key
				     (documentation nil doc-p))
  (declare (ignore slot-names))
  (when doc-p
    (if documentation (check-type documentation string))
    (set-documentation gf t documentation)))




(defmethod allocate-instance ((b built-in-class) &rest initargs)
  (declare (ignore initargs))
  (error "Can't allocate instances of BUILT-IN-CLASS."))

(defmethod reinitialize-instance ((m method) &rest initargs)
  (declare (ignore initargs))
  (error "Can't reinitialze ~s ~s" (class-of m) m))

(defmethod add-dependent ((class class) dependent)
  (pushnew dependent (%class.dependents class)))

(defmethod add-dependent ((gf standard-generic-function) dependent)
  (pushnew dependent (sgf.dependents gf)))

(defmethod remove-dependent ((class class) dependent)
  (setf (%class.dependents class)
	(delete dependent (%class.dependents class))))

(defmethod remove-dependent ((gf standard-generic-function) dependent)
  (setf (sgf.dependents gf)
	(delete dependent (sgf.dependents gf))))

(defmethod map-dependents ((class class) function)
  (dolist (d (%class.dependents class))
    (funcall function d)))

(defmethod map-dependents ((gf standard-generic-function) function)
  (dolist (d (sgf.dependents gf))
    (funcall function d)))

(defgeneric update-dependent (metaobject dependent &rest initargs))

(defmethod reinitialize-instance :after ((class std-class) &rest initargs)
  (map-dependents class #'(lambda (d)
			    (apply #'update-dependent class d initargs))))


(defun %allocate-gf-instance (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let* ((wrapper (%class.own-wrapper class))
         (gf-p (member *generic-function-class* (%class-cpl class)))
	 (len (length (%wrapper-instance-slots wrapper)))
	 (dt (if gf-p (make-gf-dispatch-table)))
	 (slots (allocate-typed-vector :slot-vector (1+ len) (%slot-unbound-marker)))
	 (fn
          #+ppc-target
           (gvector :function
                    *unset-fin-code*
                    wrapper
                    slots
                    dt
                    #'false
                    0
                    (logior (ash 1 $lfbits-gfn-bit)
                            (ash 1 $lfbits-aok-bit)))
           #+x86-target
           (%clone-x86-function #'unset-fin-trampoline
                                wrapper
                                slots
                                dt
                                #'false
                                0
                                (logior (ash 1 $lfbits-gfn-bit)
                                        (ash 1 $lfbits-aok-bit)))))
    (setf 
	  (slot-vector.instance slots) fn)
    (when dt
      (setf (%gf-dispatch-table-gf dt) fn))
    (if gf-p
      (push fn (population.data %all-gfs%)))
    fn))


(defmethod slot-value-using-class ((class structure-class)
				   instance
				   (slotd structure-effective-slot-definition))
  (let* ((loc (standard-effective-slot-definition.location slotd)))
      (typecase loc
	(fixnum
	 (struct-ref  instance loc))
	(t
	 (error "Slot definition ~s has invalid location ~s (allocation ~s)."
		slotd loc (slot-definition-allocation slotd))))))

;;; Some STRUCTURE-CLASS leftovers.
(defmethod (setf slot-value-using-class)
    (new
     (class structure-class)
     instance
     (slotd structure-effective-slot-definition))
  (let* ((loc (standard-effective-slot-definition.location slotd))
	 (type (standard-effective-slot-definition.type slotd)))
    (if (and type (not (eq type t)))
      (unless (or (eq new (%slot-unbound-marker))
		  (typep new type))
	(setq new (require-type new type))))
    (typecase loc
      (fixnum
       (setf (struct-ref instance loc) new))
      (t
       (error "Slot definition ~s has invalid location ~s (allocation ~s)."
	      slotd loc (slot-definition-allocation slotd))))))

(defmethod slot-boundp-using-class ((class structure-class)
				    instance
				    (slotd structure-effective-slot-definition))
  (declare (ignore instance))
  t)

;;; This has to be somewhere, so it might as well be here.
(defmethod make-load-form ((s slot-id) &optional env)
  (declare (ignore env))
  `(ensure-slot-id ,(slot-id.name s)))

(defmethod make-load-form ((c class-cell) &optional env)
  (declare (ignore env))
  `(find-class-cell ',(class-cell-name c) t))



(defmethod (setf class-name) (new (class class))
  (check-type new symbol)
  (when (and (standard-instance-p class)
             (%class-kernel-p class)
             (not (eq new (%class.name class)))
             *warn-if-redefine-kernel*)
    (cerror "Change the name of ~s to ~s."
            "The class ~s may be a critical part of the system;
changing its name to ~s may have serious consequences." class new))
  (let* ((old-name (class-name class)))
    (if (eq (find-class old-name nil) class)
      (progn
        (setf (info-type-kind old-name) nil)
        (clear-type-cache))))
  (when (eq (find-class new nil) class)
    (when (%deftype-expander new)
      (cerror "Change the name of ~S anyway, removing the DEFTYPE definition."
              "Changing the name of ~S to ~S would conflict with the type defined by DEFTYPE."
              class new)
      (%deftype new nil nil))
    (setf (info-type-kind new) :instance)
    (clear-type-cache))
  (reinitialize-instance class :name new)
  (setf (%class-proper-name class)
        (if (eq (find-class new nil) class)
          new))
  new)


;;; From Tim Moore, as part of a set of patches to support funcallable
;;; instances.

;;; Support for objects with metaclass funcallable-instance-class that are not
;;; standard-generic-function. The objects still look a lot like generic
;;; functions, complete with vestigial dispatch
;;; tables. set-funcallable-instance-function will work on generic functions,
;;; though after that it won't be much of a generic function.





(defun set-funcallable-instance-function (funcallable-instance function)
  (unless (typep funcallable-instance 'funcallable-standard-object)
    (error "~S is not a funcallable instance" funcallable-instance))
  (unless (functionp function)
    (error "~S is not a function" function))
  (setf (%gf-dcode funcallable-instance) function))

(defmethod reinitialize-instance ((slotd slot-definition) &key &allow-other-keys)
  (error "Can't reinitialize ~s" slotd))

(defmethod (setf generic-function-name) (new-name (gf generic-function))
  (reinitialize-instance gf :name new-name))

;;; Are we CLOS yet ?

(defun %shared-initialize (instance slot-names initargs)
  (unless (or (listp slot-names) (eq slot-names t))
    (report-bad-arg slot-names '(or list (eql t))))
  ;; Check that initargs contains valid key/value pairs,
  ;; signal a PROGRAM-ERROR otherwise.  (Yes, this is
  ;; an obscure way to do so.)
  (destructuring-bind (&key &allow-other-keys) initargs)
  ;; I'm not sure if there's a more portable way of detecting
  ;; obsolete instances.  This'll eventually call
  ;; UPDATE-INSTANCE-FOR-REDEFINED-CLASS if it needs to.
  (let* ((wrapper (if (eq (typecode instance) target::subtag-instance)
                    (instance.class-wrapper instance)
                    (instance-class-wrapper instance)))
         (class (%wrapper-class wrapper)))
    (when (eql 0 (%wrapper-hash-index wrapper)) ; obsolete
      (update-obsolete-instance instance))
    ;; Now loop over all of the class's effective slot definitions.
    (dolist (slotd (class-slots class))
      ;; Anything that inherits from STANDARD-EFFECTIVE-SLOT-DEFINITION
      ;; in OpenMCL will have a CCL::TYPE-PREDICATE slot.  It's not
      ;; well-defined to inherit from EFFECTIVE-SLOT-DEFINITION without
      ;; also inheriting from STANDARD-EFFECTIVE-SLOT-DEFINITION,
      ;; and I'd rather not check here.  If you really want to
      ;; create that kind of slot definition, write your own SHARED-INITIALIZE
      ;; method for classes that use such slot definitions ...
      (let* ((predicate (slot-definition-predicate slotd)))
        (multiple-value-bind (ignore new-value foundp)
            (get-properties initargs (slot-definition-initargs slotd))
          (declare (ignore ignore))
          (cond (foundp
                 ;; an initarg for the slot was passed to this function
                 ;; Typecheck the new-value, then call
                 ;; (SETF SLOT-VALUE-USING-CLASS)
                 (unless (or (null predicate)
                             (funcall predicate new-value))
                   (error 'bad-slot-type-from-initarg
                          :slot-definition slotd
                          :instance instance
                          :datum new-value
                          :expected-type  (slot-definition-type slotd)
                          :initarg-name (car foundp)))
                 (setf (slot-value-using-class class instance slotd) new-value))
                ((and (or (eq slot-names t)
                          (member (slot-definition-name slotd)
                                  slot-names
                                  :test #'eq))
                      (not (slot-boundp-using-class class instance slotd)))
                 ;; If the slot name is among the specified slot names, or
                 ;; we're reinitializing all slots, and the slot is currently
                 ;; unbound in the instance, set the slot's value based
                 ;; on the initfunction (which captures the :INITFORM).
                 (let* ((initfunction (slot-definition-initfunction slotd)))
                   (if initfunction
                     (let* ((newval (funcall initfunction)))
                       (unless (or (null predicate)
                                   (funcall predicate newval))
                         (error 'bad-slot-type-from-initform
                                :slot-definition slotd
                                :expected-type (slot-definition-type slotd)
                                :datum newval
                                :instance instance))
                       (setf (slot-value-using-class class instance slotd)
                             newval))))))))))
  instance)

;;; Sometimes you can do a lot better at generic function dispatch than the
;;; default. This supports that for the one-arg-dcode case.
(defmethod override-one-method-one-arg-dcode ((generic-function t) (method t))
  nil)

(defun optimize-generic-function-dispatching ()
  (dolist (gf (population.data %all-gfs%))
    (optimize-dispatching-for-gf gf)))

(defun optimize-dispatching-for-gf (gf)
  (let* ((dcode (%gf-dcode gf))
         (name (function-name dcode)))
    (when (or (eq name '%%one-arg-dcode)
              (eq name '%%nth-arg-dcode))
      (let ((methods (generic-function-methods gf)))
        (when (and methods (null (cdr methods)))
          (when (or (eq #'%%one-arg-dcode dcode)
                    (and (eq #'%%nth-arg-dcode dcode)
                         (let ((spec (method-specializers (car methods)))
                               (argnum (%gf-dispatch-table-argnum
                                        (%gf-dispatch-table gf))))
                           (and (eql 2 (length spec))
                                (and (eql argnum 1) (eq (car spec) *t-class*))))))
            (override-one-method-one-arg-dcode gf (car methods))))))))

(defparameter *unique-reader-dcode-functions* t)

;;; dcode for a GF with a single reader method which accesses
;;; a slot in a class that has no subclasses (that restriction
;;; makes typechecking simpler and also ensures that the slot's
;;; location is correct.)
(defun singleton-reader-dcode (dt instance)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((wrapper (%svref dt %gf-dispatch-table-first-data))
         (location (%svref dt (1+ %gf-dispatch-table-first-data))))
    (if (eq (if (eq (typecode instance) target::subtag-instance)
              (instance.class-wrapper instance))
            wrapper)
      (%slot-ref (instance.slots instance) location)
      (cond ((and (eq (typecode instance) target::subtag-instance)
                  (eq 0 (%wrapper-hash-index (instance.class-wrapper instance)))
                  (progn (update-obsolete-instance instance)
                         (eq (instance.class-wrapper instance) wrapper)))
             (%slot-ref (instance.slots instance) location))
            (t (no-applicable-method (%gf-dispatch-table-gf dt) instance))))))
(register-dcode-proto #'singleton-reader-dcode *gf-proto-one-arg*)

;;; Dcode for a GF whose methods are all reader-methods which access a
;;; slot in one or more classes which have multiple subclasses, all of
;;; which (by luck or design) have the same slot-definition location.
(defun reader-constant-location-dcode (dt instance)
  (declare (optimize (speed 3) (safety 0)))
    (if (memq (if (eq (typecode instance) target::subtag-instance)
              (%class-of-instance instance))
              (%svref dt %gf-dispatch-table-first-data))
      (%slot-ref (instance.slots instance) (%svref dt (1+ %gf-dispatch-table-first-data)))
      (no-applicable-method (%gf-dispatch-table-gf dt) instance)))
(register-dcode-proto #'reader-constant-location-dcode *gf-proto-one-arg*)

;;; Dcode for a GF whose methods are all reader-methods which access a
;;; slot in one or more classes which have multiple subclasses, all of
;;; which (by luck or design) have the same slot-definition location.
;;; The number of classes for which the method is applicable is
;;; potentially large, but all are subclasses of a single class
(defun reader-constant-location-inherited-from-single-class-dcode (dt instance)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((defining-class-ordinal (%svref dt %gf-dispatch-table-first-data))
         (bits  (let* ((wrapper
                        (if (eq (typecode instance) target::subtag-instance)
                          (instance.class-wrapper instance))))
                  (when wrapper (or (%wrapper-cpl-bits wrapper)
                                    (make-cpl-bits (%inited-class-cpl
                                                    (%wrapper-class wrapper))))))))
    (declare (fixnum defining-class-ordinal))
    (if (and bits
             (< defining-class-ordinal (the fixnum (uvsize bits)))
             (not (eql 0 (sbit bits defining-class-ordinal))))
      (%slot-ref (instance.slots instance) (%svref dt (1+ %gf-dispatch-table-first-data)))
      (no-applicable-method (%gf-dispatch-table-gf dt) instance))))
(register-dcode-proto #'reader-constant-location-inherited-from-single-class-dcode *gf-proto-one-arg*)

;;; It may be faster to make individual functions that take their
;;; "parameters" (defining class ordinal, slot location) as constants.
;;; It may not be.  Use *unique-reader-dcode-functions* to decide
;;; whether or not to do so.
(defun make-reader-constant-location-inherited-from-single-class-dcode
    (defining-class-ordinal location gf)
  (if *unique-reader-dcode-functions*
    (let* ((gf-name (function-name gf)))
      (values
       (%make-function 
        `(slot-reader for ,gf-name)
        `(lambda (instance)
          (locally (declare (optimize (speed 3) (safety 0)))
            (let* ((bits (let* ((wrapper
                                 (if (eq (typecode instance) target::subtag-instance)
                                   (instance.class-wrapper instance))))
                           (when wrapper (or (%wrapper-cpl-bits wrapper)
                                             (make-cpl-bits (%inited-class-cpl
                                                             (%wrapper-class wrapper))))))))
              (if (and bits
                       (< ,defining-class-ordinal (the fixnum (uvsize bits)))
                       (not (eql 0 (sbit bits ,defining-class-ordinal))))
                (%slot-ref (instance.slots instance) ,location)
                (no-applicable-method (function ,gf-name) instance)))))
        nil)
       #'funcallable-trampoline))
    (let* ((dt (gf.dispatch-table gf)))
      (setf (%svref dt %gf-dispatch-table-first-data)
            defining-class-ordinal
            (%svref dt (1+ %gf-dispatch-table-first-data))
            location)
      (values
       (dcode-for-gf gf #'reader-constant-location-inherited-from-single-class-dcode)
       (cdr (assq #'reader-constant-location-inherited-from-single-class-dcode dcode-proto-alist))))))

;;; Dcode for a GF whose methods are all reader-methods which access a
;;; slot in one or more classes which have multiple subclasses, all of
;;; which (by luck or design) have the same slot-definition location.
;;; The number of classes is for which the method is applicable is
;;; large, but all are subclasses of one of a (small) set of defining classes.
(defun reader-constant-location-inherited-from-multiple-classes-dcode (dt instance)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((wrapper (if (eq (typecode instance) target::subtag-instance)
                    (instance.class-wrapper instance)))
         (bits (if wrapper (or (%wrapper-cpl-bits wrapper)
                               (make-cpl-bits (%inited-class-cpl (%wrapper-class wrapper))))))
         (nbits (if bits (uvsize bits) 0)))
    (declare (fixnum nbits))
    (if (dolist (ordinal (%svref dt %gf-dispatch-table-first-data))
          (declare (fixnum ordinal))
          (when (and (< ordinal nbits)
                     (not (eql 0 (sbit bits ordinal))))
            (return t)))
      (%slot-ref (instance.slots instance) (%svref dt (1+ %gf-dispatch-table-first-data)))
      (no-applicable-method (%gf-dispatch-table-gf dt) instance))))
(register-dcode-proto #'reader-constant-location-inherited-from-multiple-classes-dcode *gf-proto-one-arg*)


;;; Similar to the case above, but we use an alist to map classes
;;; to their non-constant locations.
(defun reader-variable-location-dcode (dt instance)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((alist (%svref dt %gf-dispatch-table-first-data))
         (location (cdr
                    (assq
                     (if (eq (typecode instance) target::subtag-instance)
                       (%class-of-instance instance))
                     alist))))
    (if location
      (%slot-ref (instance.slots instance) location)
      (no-applicable-method (%gf-dispatch-table-gf dt) instance))))
(register-dcode-proto #'reader-variable-location-dcode *gf-proto-one-arg*)

(defun class-and-slot-location-alist (classes slot-name)
  (let* ((alist nil))
    (labels ((add-class (c)
               (unless (assq c alist)
                 (let* ((slots (class-slots c)))
                   (unless slots
                     (finalize-inheritance c)
                     (setq slots (class-slots c)))
                   (push (cons c (slot-definition-location (find-slotd slot-name slots))) alist))
                 (dolist (sub (class-direct-subclasses c))
                   (add-class sub)))))
      (dolist (class classes) (add-class class))
      ;; Building the alist the way that we have should often approximate
      ;; this ordering; the idea is that leaf classes are more likely to
      ;; be instantiated than non-leaves.
      (sort alist (lambda (c1 c2)
                    (< (length (class-direct-subclasses c1))
                       (length (class-direct-subclasses c2))))
            :key #'car))))

;;; Return a list of all classes in CLASS-LIST that aren't subclasses
;;; of any other class in the list.
(defun remove-subclasses-from-class-list (class-list)
  (if (null (cdr class-list))
    class-list
    (collect ((unique))
      (dolist (class class-list (unique))
        (when (dolist (other class-list t)
                (unless (eq class other)
                  (when (subtypep class other) (return nil))))
          (unique class))))))



;;; Try to replace gf dispatch with something faster in f.
(defun %snap-reader-method (f)
  (when (slot-boundp f 'methods)
    (let* ((methods (generic-function-methods f)))
      (when (and methods
                 (every (lambda (m) (eq (class-of m) *standard-reader-method-class*)) methods)
                 (every (lambda (m) (subtypep (class-of (car (method-specializers m))) *standard-class-class*)) methods)
                 (every (lambda (m) (null (method-qualifiers m))) methods))
        (let* ((m0 (car methods))
               (name (slot-definition-name (accessor-method-slot-definition m0))))
          (when (every (lambda (m)
                         (eq name (slot-definition-name (accessor-method-slot-definition m))))
                       (cdr methods))
            ;; All methods are *STANDARD-READER-METHODS* that
            ;; access the same slot name.  Build an alist of
            ;; mapping all subclasses of all classes on which those
            ;; methods are specialized to the effective slot's
            ;; location in that subclass.
            (let* ((classes (mapcar #'(lambda (m) (car (method-specializers m)))
                                    methods))
                   (alist (class-and-slot-location-alist classes name))
                   (loc (cdar alist))
                   (dt (gf.dispatch-table f)))
              ;; Only try to handle the case where all slots have
              ;; :allocation :instance (and all locations - the CDRs
              ;; of the alist pairs - are small, positive fixnums.
              (when (every (lambda (pair) (typep (cdr pair) 'fixnum)) alist)
                (clear-gf-dispatch-table dt)
                (setf (%gf-dispatch-table-argnum dt) -1) ;mark as non-standard
                (cond ((null (cdr alist))
                       ;; Method is only applicable to a single class.
                       (destructuring-bind (class . location) (car alist)
                         (setf (%svref dt %gf-dispatch-table-first-data) (%class.own-wrapper class)
                               (%svref dt (1+ %gf-dispatch-table-first-data)) location
                               (gf.dcode f) (dcode-for-gf f #'singleton-reader-dcode))))
                      ((dolist (other (cdr alist) t)
                         (unless (eq (cdr other) loc)
                           (return)))
                       ;; All classes have the slot in the same location,
                       ;; by luck or design.
                       (cond
                         ((< (length alist) 10)
                          ;; Only a small number of classes, just do MEMQ
                          (setf (%svref dt %gf-dispatch-table-first-data)
                                (mapcar #'car alist)
                                (%svref dt (1+ %gf-dispatch-table-first-data))
                                loc
                                (gf.dcode f) (dcode-for-gf f #'reader-constant-location-dcode)))
                         ((null (cdr (setq classes (remove-subclasses-from-class-list classes))))
                          ;; Lots of classes, all subclasses of a single class
                          (multiple-value-bind (dcode trampoline)
                              (make-reader-constant-location-inherited-from-single-class-dcode (%class-ordinal (car classes)) loc f)
                            (setf (gf.dcode f) dcode)
                            (replace-function-code f trampoline)))
                         (t
                          ;; Multple classes.  We should probably check
                          ;; to see they're disjoint
                          (setf (%svref dt %gf-dispatch-table-first-data)
                                (mapcar #'%class-ordinal classes)
                                (%svref dt (1+ %gf-dispatch-table-first-data))
                                loc
                                (gf.dcode f)
                                (dcode-for-gf f #'reader-constant-location-inherited-from-multiple-classes-dcode)))))
                      (t
                       ;; Multiple classes; the slot's location varies.
                       (setf (%svref dt %gf-dispatch-table-first-data)
                             alist
                             
                             (gf.dcode f) (dcode-for-gf f #'reader-variable-location-dcode))))))))))))

;;; Hack-o-rama: GF has nothing but primary methods, first (and only non-T)
;;; specializers are all EQL specializers whose objects are symbols.
;;; The effective method applicable for each symbol is stored on the
;;; plist of the symbol under a property EQ to the dispatch table (which
;;; is mostly ignored, otherwise.)
(defun %%1st-arg-eql-method-hack-dcode (dt args)
  (let* ((sym (if (listp args) (car args)(%lexpr-ref args (%lexpr-count args) 0)))
         (mf (if (symbolp sym) (get sym dt))))
    (if mf
      (if (listp args)
        (apply mf args)
        (%apply-lexpr-tail-wise mf args))
      ;;; Let %%1st-arg-dcode deal with it.
      (%%1st-arg-dcode dt args))))
(register-dcode-proto #'%%1st-arg-eql-method-hack-dcode *gf-proto*)

(defun %%1st-two-arg-eql-method-hack-dcode (dt arg1 arg2)
  (let* ((mf (if (typep arg1 'symbol) (get arg1 dt))))
    (if mf
      (funcall mf arg1 arg2)
      (%%1st-two-arg-dcode dt arg1 arg2))))
(register-dcode-proto #'%%1st-two-arg-eql-method-hack-dcode *gf-proto-two-arg*)

(defun %%one-arg-eql-method-hack-dcode (dt arg)
  (let* ((mf (if (typep arg 'symbol) (get arg dt))))
    (if mf
      (funcall mf arg))))
(register-dcode-proto #'%%one-arg-eql-method-hack-dcode *gf-proto-one-arg*)

(defun install-eql-method-hack-dcode (gf)
  (let* ((bits (inner-lfun-bits gf))
         (nreq (ldb $lfbits-numreq bits))
         (other-args? (or (not (eql 0 (ldb $lfbits-numopt bits)))
                          (logbitp $lfbits-rest-bit bits)
                          (logbitp $lfbits-restv-bit bits)
                          (logbitp $lfbits-keys-bit bits)
                          (logbitp $lfbits-aok-bit bits))))
    (setf (%gf-dcode gf)
          (dcode-for-gf gf
                        (cond ((and (eql nreq 1) (null other-args?))
                               #'%%one-arg-eql-method-hack-dcode)
                              ((and (eql nreq 2) (null other-args?))
                               #'%%1st-two-arg-eql-method-hack-dcode)
                              (t
                               #'%%1st-arg-eql-method-hack-dcode))))))

(defun maybe-hack-eql-methods (gf)
  (let* ((methods (generic-function-methods gf)))
    (when (and methods
               (every #'(lambda (method)
                          (let* ((specializers (method-specializers method))
                                      (first (car specializers)))
                                 (and (typep first 'eql-specializer)
                                      (typep (eql-specializer-object first) 'symbol)
                                      (dolist (s (cdr specializers) t)
                                        (unless (eq s *t-class*)
                                          (return nil)))
                                      (null (cdr (compute-applicable-methods gf (cons (eql-specializer-object first) (make-list (length (cdr specializers))))))))))
                      methods))
      (let* ((dt (%gf-dispatch-table gf)))
        (dolist (m methods)
          (let* ((sym (eql-specializer-object (car (method-specializers m))))
                 (f (method-function m)))
            (setf (get sym dt) f)))
        (install-eql-method-hack-dcode gf)
        t))))


            
                            
;;; Return a list of :after methods for INITIALIZE-INSTANCE on the
;;; class's prototype, and a boolean that's true if no other qualified
;;; methods are defined.
(defun initialize-instance-after-methods (proto class)
  (let* ((method-list (compute-method-list (sort-methods
                            (compute-applicable-methods #'initialize-instance (list proto))
                            (list (class-precedence-list class))))))
    (if (atom method-list)
      (values nil t)
      (if (null (car method-list))
        (values (cadr method-list) t)
        ;; :around or :before methods, give up
        (values nil nil)))))

(defparameter *typecheck-slots-in-optimized-make-instance* t)


;;; Return a lambda form or NIL.
(defun make-instantiate-lambda-for-class-cell (cell)
  (let* ((class (class-cell-class cell))
         (after-methods nil))
    (when (and (typep class 'standard-class)
               (progn (unless (class-finalized-p class)
                        (finalize-inheritance class))
                      t)
               (null (cdr (compute-applicable-methods #'allocate-instance (list class))))
               (let* ((proto (class-prototype class)))
                 (and (multiple-value-bind (afters ok)
                          (initialize-instance-after-methods proto class)
                        (when ok
                          (setq after-methods afters)
                          t))
                      (null (cdr (compute-applicable-methods #'shared-initialize (list proto t)))))))
      (let* ((slotds (sort (copy-list (class-slots class))
                           #'(lambda (x y)
                               (if (consp x) x (if (consp y) y (< x y))))
                           :key #'slot-definition-location))
             (default-initargs (class-default-initargs class)))
        (collect ((keys)
                  (binds)
                  (class-binds)
                  (ignorable)
                  (class-slot-inits)
                  (after-method-forms)
                  (forms))
          (flet ((generate-type-check (form type &optional spvar)
                   (if (or (null *typecheck-slots-in-optimized-make-instance*)
                           (eq type t)
                           (and (quoted-form-p type) (eq (cadr type) t)))
                     form
                     (if spvar
                       `(if ,spvar
                         (require-type ,form ',type)
                         ,form)
                       `(require-type ,form ',type)))))
            (dolist (slot slotds)
              (let* ((initargs (slot-definition-initargs slot))
                     (initfunction (slot-definition-initfunction slot))
                     (initform (slot-definition-initform slot))
                     (location (slot-definition-location slot))
                     (location-var nil)
                     (class-init-p nil)
                     (one-initarg-p (null (cdr initargs)))
                     (name (slot-definition-name slot))
                     (type (slot-definition-type slot)))
                (when (consp location)
                  (setq location-var (gensym "LOCATION")))
                (when initfunction
                  (setq initform
                        (if (self-evaluating-p initform)
                            initform
                            `(funcall ,initfunction))))
                (cond ((null initargs)
                       (let ((initial-value-form
                              (if initfunction
                                  (generate-type-check initform type)
                                  `(%slot-unbound-marker))))
                         (if location-var
                             (when initfunction
                               (setq class-init-p t)
                               (class-slot-inits
                                `(when (eq (%slot-unbound-marker) (cdr ,location-var))
                                   (setf (cdr ,location-var) ,initial-value-form))))
                             (forms initial-value-form))))
                      (t (collect ((cond-clauses))
                           (let ((last-cond-clause nil))
                             (dolist (initarg initargs)
                               (let* ((spvar nil)
                                      (name (if one-initarg-p
                                                name
                                                (gensym (string name))))
                                      (initial-value-form
                                       (if (and initfunction
                                                one-initarg-p
                                                (null location-var))
                                           initform
                                           (progn
                                             (when initarg
                                               (setq spvar (make-symbol
                                                            (concatenate
                                                             'string
                                                             (string initarg)
                                                             "-P"))))
                                             (and one-initarg-p
                                                  (null location-var)
                                                  (if initfunction
                                                      initform
                                                      `(%slot-unbound-marker))))))
                                      (default (assq initarg default-initargs)))
                                 (when spvar (ignorable spvar))
                                 (when default
                                   (destructuring-bind (form function)
                                       (cdr default)
                                     (setq default
                                           (if (self-evaluating-p form)
                                               form
                                               `(funcall ,function)))))
                                 (keys (list*
                                        (list initarg name)
                                        (if (and default one-initarg-p (null location-var))
                                            default
                                            initial-value-form)
                                        (if spvar (list spvar))))
                                 (if one-initarg-p
                                   (if location-var
                                     (progn
                                       (setq class-init-p t)
                                       (class-slot-inits
                                        `(if ,spvar
                                           (setf (cdr ,location-var)
                                                 ,(generate-type-check
                                                   name type))
                                           ,(if default
                                              `(setf (cdr ,location-var)
                                                     ,(generate-type-check
                                                       default type))
                                              (when initfunction
                                                `(when (eq (%slot-unbound-marker)
                                                           (cdr ,location-var))
                                                   (setf (cdr ,location-var)
                                                         ,(generate-type-check
                                                           initform type))))))))
                                     (forms `,(generate-type-check name type spvar)))
                                     (progn (cond-clauses `(,spvar ,name))
                                            (when (and default (null last-cond-clause))
                                              (setq last-cond-clause
                                                    `(t ,default)))))))
                             (when (cond-clauses)
                               (when last-cond-clause
                                 (cond-clauses last-cond-clause))
                               (cond ((null location-var)
                                      (unless last-cond-clause
                                        (cond-clauses `(t ,initform)))
                                      (forms (generate-type-check
                                              `(cond ,@(cond-clauses))
                                              type)))
                                     (t
                                      (let ((initform-p-var
                                             (unless last-cond-clause
                                               (make-symbol "INITFORM-P")))
                                            (value-var (make-symbol "VALUE")))
                                        (unless last-cond-clause
                                          (cond-clauses
                                           `(t (setq ,initform-p-var t)
                                               ,(if initfunction
                                                    initform
                                                    `(%slot-unbound-marker)))))
                                        (setq class-init-p t)
                                        (class-slot-inits
                                         `(let* (,@(and initform-p-var
                                                        (list `(,initform-p-var nil)))
                                                 (,value-var
                                                  ,(generate-type-check
                                                    `(cond ,@(cond-clauses)) type)))
                                            (when
                                                ,(if initform-p-var
                                                     `(or (null ,initform-p-var)
                                                          (and (eq (cdr ,location-var)
                                                                   (%slot-unbound-marker))
                                                               (not (eq ,value-var
                                                                        (%slot-unbound-marker)))))
                                                     t)
                                                (setf (cdr ,location-var) ,value-var))))))))))))
                (when class-init-p
                  (class-binds `(,location-var
                                 (load-time-value
                                  (slot-definition-location ',slot))))))))
          (let* ((cell (make-symbol "CLASS-CELL"))
                 (args (make-symbol "ARGS"))
                 (slots (make-symbol "SLOTS"))
                 (instance (make-symbol "INSTANCE")))
            (dolist (after after-methods)
              (after-method-forms `(apply ,(method-function after) ,instance ,args)))
            (when after-methods
              (after-method-forms instance))
            (binds `(,slots (gvector :slot-vector nil ,@(forms))))
            (binds `(,instance (gvector :instance 0 (class-cell-extra ,cell) ,slots)))
            `(lambda (,cell ,@(when after-methods `(&rest ,args)) &key ,@(keys) ,@(when after-methods '(&allow-other-keys)))
              (declare (ignorable ,@(ignorable)))
              ,@(when after-methods `((declare (dynamic-extent ,args))))
              (let (,@(class-binds))
                ,@(class-slot-inits))
              (let* (,@(binds))
                (setf (instance.hash ,instance) (strip-tag-to-fixnum ,instance)
                      (%svref ,slots 0) ,instance)
                ,@(after-method-forms)))))))))

(defun optimize-make-instance-for-class-cell (cell)
  (setf (class-cell-instantiate cell) '%make-instance)
  (let* ((lambda (make-instantiate-lambda-for-class-cell cell)))
    (when lambda
      (setf (class-cell-instantiate cell) (compile nil lambda)
            (class-cell-extra cell) (%class.own-wrapper
                                     (class-cell-class cell)))
      t)))

(defun optimize-make-instance-for-class-name (class-name)
  (optimize-make-instance-for-class-cell (find-class-cell class-name t)))

(defun optimize-named-class-make-instance-methods ()
  (maphash (lambda (class-name class-cell)
             (handler-case (optimize-make-instance-for-class-cell class-cell)
               (error (c)
                      (warn "error optimizing make-instance for ~s:~&~a"
                            class-name c))))
           %find-classes%))

;; Redefined from bootstrapping verison in l1-clos-boot.lisp
;; Remove the make-instance optimization if the user is adding
;; a method on initialize-instance, allocate-instance, or shared-initialize
(defun maybe-remove-make-instance-optimization (gfn method)
  (when (or (eq gfn #'allocate-instance)
            (eq gfn #'initialize-instance)
            (eq gfn #'shared-initialize))
    (let* ((specializer (car (method-specializers method)))
           (cell (and (typep specializer 'class)
                      (gethash (class-name specializer) %find-classes%))))
      (when cell
        (setf (class-cell-instantiate cell) '%make-instance)))))            

;;; Iterate over all known GFs; try to optimize their dcode in cases
;;; involving reader methods.

(defun snap-reader-methods (&key known-sealed-world
                                 (check-conflicts t)
                                 (optimize-make-instance t))
  (declare (ignore check-conflicts)
           (special *sealed-clos-world*))
  (unless known-sealed-world
    (cerror "Proceed, if it's known that no new classes or methods will be defined."
            "Optimizing reader methods in this way is only safe if it's known that no new classes or methods will be defined."))
  (when optimize-make-instance
    (optimize-named-class-make-instance-methods))
  (let* ((ngf 0)
         (nwin 0))
    (dolist (f (population.data %all-gfs%))
      (incf ngf)
      (when (%snap-reader-method f)
        (incf nwin)))
    (setq *sealed-clos-world* t)
    (values ngf nwin 0)))

(defun register-non-dt-dcode-function (f)
  (flet ((symbol-or-function-name (x)
           (etypecase x
             (symbol x)
             (function (function-name x)))))
    (let* ((already (member (symbol-or-function-name f) *non-dt-dcode-functions* :key #'symbol-or-function-name)))
      (if already
        (setf (car already) f)
        (push f *non-dt-dcode-functions*))
      f)))

(defun pessimize-clos ()
  (declare (special *sealed-clos-world*))
  (when *sealed-clos-world*
    ;; Undo MAKE-INSTANCE optimization
    (maphash (lambda (class-name class-cell)
               (declare (ignore class-name))
               (setf (class-cell-instantiate class-cell) '%make-instance))
             %find-classes%)
    ;; Un-snap reader methods, undo other GF optimizations.
    (dolist (f (population-data %all-gfs%))
      (let* ((dt (%gf-dispatch-table f)))
        (clear-gf-dispatch-table dt)
        (compute-dcode f)))
    (setq *sealed-clos-world* nil)
    t))

;;; If there's a single method (with standard method combination) on
;;; GF and all of that method's arguments are specialized to the T
;;; class - and if the method doesn't accept &key - we can just have
;;; the generic function call the method-function
(defun dcode-for-universally-applicable-singleton (gf)
  (when (eq (generic-function-method-combination gf)
            *standard-method-combination*)
    (let* ((methods (generic-function-methods gf))
           (method (car methods)))
      (when (and method
                 (null (cdr methods))
                 (null (method-qualifiers method))
                 (not (logbitp $lfbits-keys-bit (lfun-bits (method-function method))))
                 (dolist (spec (method-specializers method) t)
                   (unless (eq spec *t-class*)
                     (return nil))))
        (method-function method)))))

(register-non-dt-dcode-function #'dcode-for-universally-applicable-singleton)
