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

; defstruct-lds.lisp

(in-package "CCL")

(eval-when (eval compile)
  (require 'defstruct-macros)
)




(defun uvector-subtype-p (thing subtype-number)
  (= (the fixnum (typecode thing)) subtype-number))

(defun uvector (subtype &rest p)
  (declare (dynamic-extent p))
  (let ((n (length p)) (uv))
    (setq uv  (%alloc-misc n subtype))
    (dotimes (i (the fixnum n)) (declare (fixnum i)) (uvset uv i (pop p)))
    uv))

;(defmacro test (&rest args) `(macroexpand-1 (defstruct ,@args)))

;--> To do: compiler transform for copier, possibly constructor.
(defmacro defstruct (options &rest slots &environment env)
  "DEFSTRUCT {Name | (Name Option*)} {Slot | (Slot [Default] {Key Value}*)}
   Define the structure type Name. Instances are created by MAKE-<name>, 
   which takes &KEY arguments allowing initial slot values to the specified.
   A SETF'able function <name>-<slot> is defined for each slot to read and
   write slot values. <name>-p is a type predicate.

   Popular DEFSTRUCT options (see manual for others):

   (:CONSTRUCTOR Name)
   (:PREDICATE Name)
       Specify the name for the constructor or predicate.

   (:CONSTRUCTOR Name Lambda-List)
       Specify the name and arguments for a BOA constructor
       (which is more efficient when keyword syntax isn't necessary.)

   (:INCLUDE Supertype Slot-Spec*)
       Make this type a subtype of the structure type Supertype. The optional
       Slot-Specs override inherited slot options.

   Slot options:

   :TYPE Type-Spec
       Asserts that the value of this slot is always of the specified type.

   :READ-ONLY {T | NIL}
       If true, no setter function is defined for this slot."
  ;There's too much state to keep around here to break it up into little
  ;functions, so what the hell, let's do it all inline...
  (prog (struct-name type conc-name constructor copier predicate include
         print-function print-object  named initial-offset boa-constructors print-p
         documentation (slot-list ()) (offset 0) superclasses sd
         refnames)
    ;Parse options
    (if (atom options)
      (setq struct-name options options ())
      (setq struct-name (pop options)))
    (unless (symbolp struct-name) (signal-program-error $XNotSym struct-name))
    (let (name args constructor-p predicate-p)
      (while options
        (if (atom (car options))
          (setq name (%car options) args ())
          (setq name (%caar options) args (%cdar options)))
        (case name
          (:conc-name
           (when conc-name (go dup-options))
           (when (cdr args) (go bad-options))
           (setq conc-name (or args (list nil))))
          (:constructor
           (when (cddr args) (go bad-options))
           (cond ((cdr args) (push args boa-constructors))
                 (t (when constructor (go dup-options))
                    (unless (symbolp (%car args)) (go bad-options))
                    (setq constructor-p t constructor args))))
          (:copier
           (when copier (go dup-options))
           (when (or (cdr args) (not (symbolp (%car args)))) (go bad-options))
           (setq copier args))
          (:predicate
           (when predicate (go dup-options))
           (when (or (cdr args) (not (symbolp (%car args)))) (go bad-options))
           (setq predicate-p t predicate args))
          (:include
           (when include (go dup-options))
           (when (or (null args) (not (symbolp (car args)))) (go bad-options))
           (setq include args))
          ((:print-function :print-object)
           (when print-function (go dup-options))
           (when (or (cdr args)
                     (not (or (symbolp (%car args))
                              (and (consp (%car args)) (eq (%caar args) 'lambda)))))
             (go bad-options))
           (setq print-p t
		 print-function (%car args)
		 print-object (eq name :print-object)))
          (:type
           (when type (go dup-options))
           (when (cdr args) (go bad-options))
           (unless (eq (setq type (%car args)) 'list)
             (when (eq type 'vector) (setq type '(vector t)))
             (when (or (atom type) (neq (%car type) 'vector) (cdr (%cdr type)))
               (go bad-options))))
          (:named
           (when args (go bad-options))
           (setq named t))
          (:initial-offset
           (when initial-offset (go dup-options))
           (when (or (cdr args) (not (fixnump (%car args))) (%i< (%car args) 0))
             (go bad-options))
           (setq initial-offset (%car args)))
          (t (go bad-options)))
        (setq options (%cdr options)))
      ;Options parsed!  Do defaulting and some consistency checking.
      (cond (type
             (when (null (defstruct-reftype type)) ;e.g. (vector NIL)
               (bad-named-arg :type type))
             (when print-p
               (error "Cannot specify ~S with ~S" :print-function :type))
             (if (and named (consp type) (eq (car type) 'vector)
                      (cadr type) (not (subtypep 'symbol (cadr type))))
               (error "Cannot specify ~S with type: ~S" :named type))
             )
            ((built-in-type-p struct-name)
             (error "Cannot redefine built-in type ~S" struct-name))
            (initial-offset
             (error "Cannot use ~S without ~S" :initial-offset :type))
            (t (setq named t)))
      (if (not named)
        (when predicate-p
          (unless (null (setq predicate (%car predicate)))
            (error "Cannot specify :PREDICATE for an unnamed structure")))
        (setq predicate (if (null predicate)
                          (concat-pnames struct-name "-P")
                          (%car predicate))))
      (setq conc-name
            (if (null conc-name) (%str-cat (symbol-name struct-name) "-")
                (if (%car conc-name) (string (%car conc-name)))))
      (unless (and boa-constructors (not constructor-p))
        (setq constructor
              (if (null constructor)
                (concat-pnames "MAKE-" struct-name) (%car constructor))))
      (setq copier
            (if (null copier) (concat-pnames "COPY-" struct-name) (%car copier))))
    ;Process included slots
    (when include
      (let* ((included-name (%car include))
             (sub-sd (or (let* ((defenv (definition-environment env)))
                          (when defenv (%cdr (assq included-name (defenv.structures defenv)))))
                         (gethash included-name %defstructs%)))
            (slots (%cdr include))
            name args ssd)
        (unless sub-sd (error "No such structure: ~S" (cons :include include)))
        (unless (eq (defstruct-reftype type)
                    (defstruct-reftype (sd-type sub-sd)))
          (error "Incompatible structure type ~S for ~S"
                 (sd-type sub-sd) (cons :include include)))
        (dolist (ssd (sd-slots sub-sd)) (push
					 (let* ((new-ssd (copy-ssd ssd)))
					   (ssd-set-inherited new-ssd)
					   new-ssd)
					   slot-list))
        (while slots
          (if (atom (car slots))
            (setq name (%car slots) args ())
            (setq name (%caar slots) args (%cdar slots)))
          (unless (symbolp name) (signal-program-error $XNotSym name))
          (unless (setq ssd (named-ssd name slot-list))
            (error "~S has no ~S slot, in ~S"
                   (sd-name sub-sd) name (cons :include include)))
          (ssd-set-initform ssd (pop args))
          (while args
            (when (atom (cdr args)) (signal-program-error "~S is not a proper list" (cdr args)))
            (cond ((eq (%car args) :type) )
                  ((eq (%car args) :read-only)
                   (when (and (not (%cadr args)) (ssd-r/o ssd))
                     (signal-program-error "Slot ~S in ~S must be read-only" name (sd-name sub-sd)))
                   (when (%cadr args) (ssd-set-r/o ssd)))
                  (t (signal-program-error "~S must be  (member :type :read-only)." (%car args))))
            (setq args (%cddr args)))
          (setq slots (%cdr slots)))
        (setq offset (sd-size sub-sd))
        (setq superclasses (sd-superclasses sub-sd))))
    (push struct-name superclasses)
    ;Now add own slots
    (setq offset (%i+ offset (or initial-offset 0)))
    (when (and named (or type (not include)))
      (push (make-ssd 0 (if type `',struct-name `',superclasses) offset t) slot-list)
      (setq named offset offset (%i+ offset 1)))
    (when (stringp (%car slots))
      (setq documentation (%car slots) slots (%cdr slots)))
    (let (name args read-only initform slot-type)
      (while slots
         (if (atom (%car slots))
           (setq name (%car slots) args ())
           (setq name (%caar slots) args (%cdar slots)))
         (unless (symbolp name) (go bad-slot))
         (setq read-only nil initform (pop args) slot-type t)
         (while args
            (when (atom (cdr args)) (go bad-slot))
            ;; To do: check for multiple/incompatible options.
            (cond ((eq (%car args) :type)
                   (setq slot-type (%cadr args)))
                  ((eq (%car args) :read-only)
                   (setq read-only (%cadr args)))
                  (t (go bad-slot)))
            (setq args (%cddr args)))
         (specifier-type slot-type env) ;; Check for validity (signals program error)
         (push (make-ssd name initform offset read-only slot-type) slot-list)
         (setq slots (%cdr slots) offset (%i+ offset 1))))
    (setq slot-list (nreverse slot-list))
    (when (and (null type) include)
      (ssd-set-initform (car slot-list) `',superclasses))
    (progn ;when conc-name
      (dolist (slot slot-list)
        (unless (fixnump (ssd-name slot))
          (push (if conc-name
                  (concat-pnames conc-name (ssd-name slot))
                  (ssd-name slot))
                refnames)))
      (setq refnames (nreverse refnames)))
    (setq sd (vector type slot-list superclasses offset constructor () refnames))
    (return
     `(progn
	,@(when (null (sd-type sd))
		`((when (memq ',struct-name *nx-known-declarations*)
		    (check-declaration-redefinition ',struct-name 'defstruct))))
       (remove-structure-defs  ',struct-name) ; lose any previous defs
        ,.(defstruct-slot-defs sd refnames env)
        ,.(if constructor (list (defstruct-constructor sd constructor)))
        ,.(defstruct-boa-constructors sd boa-constructors)
        ,.(if copier (defstruct-copier sd copier env))
        ,.(if predicate (defstruct-predicate sd named predicate env))
        (eval-when (:compile-toplevel)
          (define-compile-time-structure 
            ',sd 
            ',refnames 
            ,(if (and predicate (null (sd-type sd))) `',predicate)
            ,env))        
        (%defstruct-do-load-time
         ',sd
         ,(if (and predicate (null (sd-type sd))) `',predicate)
         ,.(if documentation (list documentation)))
        ,.(%defstruct-compile sd refnames env)
       ;; Wait until slot accessors are defined, to avoid
       ;; undefined function warnings in the print function/method.
       (%defstruct-set-print-function
	',sd
	,(if print-function
	  (if (symbolp print-function)
	    `',print-function
	    `#',print-function)
	  (unless print-p (if include 0)))
	,print-object)
        ',struct-name))

    dup-options
     (error "Duplicate ~S options not allowed" (%car options))
    bad-options
     (signal-program-error "Bad defstruct option ~S." (%car options))
    bad-slot
    (signal-program-error "Bad defstruct slot spec ~S." (%car slots))))

(defun concat-pnames (name1 name2)
  (intern (%str-cat (string name1) (string name2))))

(defun wrap-with-type-check (value slot &aux (slot-type (ssd-type slot)))
  (if (eq t slot-type)
    value
    `(require-type ,value ',slot-type)))

(defun make-class-cells-list (class-names)
  (if (and (consp class-names)
           (eq (car class-names) 'quote)
           (consp (cdr class-names))
           (null (cddr class-names))
           (listp (cadr class-names))
           (every #'symbolp (cadr class-names)))
    `',(mapcar (lambda (name) (find-class-cell name t)) (cadr class-names))
    class-names))

(defun defstruct-constructor (sd constructor &aux (offset 0)
                                                  (args ())
                                                  (values ())
                                                  slot-offset
                                                  name)
  (dolist (slot (sd-slots sd))
    (setq slot-offset (ssd-offset slot))
    #-bccl (when (%i< slot-offset offset)
             (error "slots out of order! ~S" (sd-slots sd)))
    (while (%i< offset slot-offset)
      (push nil values)
      (setq offset (%i+ offset 1)))
    (if (fixnump (setq name (ssd-name slot)))
      (if (eql 0 name)
        (push (make-class-cells-list (ssd-initform slot)) values) 
        (push (wrap-with-type-check (ssd-initform slot) slot) values))
      (let* ((temp (make-symbol (symbol-name name))))
        (push (list (list (make-keyword name) temp) (ssd-initform slot)) args)
        (push (wrap-with-type-check temp slot) values)))
    (setq offset (%i+ offset 1)))
  (setq values (nreverse values))
  `(defun ,constructor (&key ,@(nreverse args))
     ,(case (setq name (defstruct-reftype (sd-type sd)))
          (#.$defstruct-nth `(list ,@values))
          (#.target::subtag-simple-vector `(vector ,@values))
          ((#.target::subtag-struct #.$defstruct-struct)
           `(gvector :struct ,@values))
          (t `(uvector ,name ,@values)))))

(defun defstruct-boa-constructors (sd boas &aux (list ()))
  (dolist (boa boas list)
    (push (defstruct-boa-constructor sd boa) list)))

(defun defstruct-boa-constructor (sd boa &aux (args ())
                                     (used-slots ())
                                     (values ())
                                     (offset 0)
                                     arg-kind slot slot-offset)
  (unless (verify-lambda-list (cadr boa))
    (error "Invalid lambda-list in ~S ." (cons :constructor boa)))
  (dolist (arg (cadr boa))
    (cond ((memq arg lambda-list-keywords)
           (setq arg-kind arg))
          ((setq slot (named-ssd arg (sd-slots sd)))
           (when (or (eq arg-kind '&optional) (eq arg-kind '&key)
                     ;; for &aux variables, init value is
                     ;; implementation-defined, however it's not
                     ;; supposed to signal a type error until slot is
                     ;; assigned, so might as well just use the
                     ;; initform.
                     (eq arg-kind '&aux))
             (setq arg (list arg (ssd-initform slot))))
           (push slot used-slots))
          ((and (consp arg) (setq slot (named-ssd (if (consp (%car arg)) (%cadar arg) (%car arg)) (sd-slots sd))))
           (push slot used-slots))
          (t nil))
    (push arg args))
  (dolist (slot (sd-slots sd))
    (setq slot-offset (ssd-offset slot))
    #-bccl (when (%i< slot-offset offset) (error "slots out of order! ~S" sd))
    (while (%i< offset slot-offset)
      (push nil values)
      (setq offset (%i+ offset 1)))
    (push (if (memq slot used-slots) (ssd-name slot)
            (if (eql 0 (ssd-name slot))
              (make-class-cells-list (ssd-initform slot))
              (if (constantp (ssd-initform slot)) (ssd-initform slot)
                (progn
                  (unless (eq arg-kind '&aux)
                    (push (setq arg-kind '&aux) args))
                  (push (list (ssd-name slot) (ssd-initform slot)) args)
                  (ssd-name slot)))))
          values)
    (setq offset (%i+ offset 1)))
  (setq values (mapcar #'wrap-with-type-check (nreverse values) (sd-slots sd)))
  `(defun ,(car boa) ,(nreverse args)
    ,(case (setq slot (defstruct-reftype (sd-type sd)))
           (#.$defstruct-nth `(list ,@values))
           (#.target::subtag-simple-vector `(vector ,@values))
           ((#.target::subtag-struct #.$defstruct-struct)
            `(gvector :struct ,@values))
           (t `(uvector ,slot ,@values)))))

(defun defstruct-copier (sd copier env)
  `((eval-when (:compile-toplevel)
      (record-function-info ',copier ',*one-arg-defun-def-info* ,env))
    (fset ',copier
          ,(if (eq (sd-type sd) 'list) '#'copy-list '#'copy-uvector))
    (record-source-file ',copier 'function)))

(defun defstruct-predicate (sd named predicate env)
  (declare (ignore env))
  (let* ((arg (gensym))
         (sd-name (sd-name sd))
         (body
          (case (sd-type sd)
            ((nil) `(structure-typep ,arg ',(find-class-cell sd-name t)))
            ((list) `(and (consp ,arg) (eq (nth ,named ,arg) ',sd-name)))
            (t `(and (uvector-subtype-p ,arg ,(defstruct-reftype (sd-type sd)))
               (< ,named (uvsize ,arg))
               (eq (uvref ,arg ,named) ',sd-name))))))
    `((defun ,predicate (,arg) ,body))))

; End of defstruct-lds.lisp
