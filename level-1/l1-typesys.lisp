;;;-*- Mode: Lisp; Package: CCL -*-
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

;; This is a hacked-up version of the CMU CL type system.

(in-package "CCL")



;;; This condition is signalled whenever we make a UNKNOWN-TYPE so that
;;; compiler warnings can be emitted as appropriate.
;;;
(define-condition parse-unknown-type (condition)
  ((specifier :reader parse-unknown-type-specifier :initarg :specifier))
  (:report (lambda (c s) (print-unreadable-object (c s :type t)
			   (format s "unknown type ~A" (parse-unknown-type-specifier c))))))

(defun parse-lambda-list (list)
  (let* ((required)
         (optional)
         (keys)
         (aux))
    (let ((restp nil)
          (rest nil)
          (keyp nil)
          (allowp nil)
          (state :required))
      (dolist (arg list)
        (if (and (symbolp arg)
                 (let ((name (symbol-name arg)))
                   (and (/= (length name) 0)
                        (char= (char name 0) #\&))))
          (case arg
            (&optional
             (unless (eq state :required)
               (error "Misplaced &optional in lambda-list: ~S." list))
             (setq state '&optional))
            (&rest
             (unless (member state '(:required &optional))
               (error "Misplaced &rest in lambda-list: ~S." list))
             (setq state '&rest))
            (&key
             (unless (member state '(:required &optional :post-rest
                                     ))
               (error "Misplaced &key in lambda-list: ~S." list))
             (setq keyp t)
             (setq state '&key))
            (&allow-other-keys
             (unless (eq state '&key)
               (error "Misplaced &allow-other-keys in lambda-list: ~S." list))
             (setq allowp t  state '&allow-other-keys))
            (&aux
             (when (member state '(&rest))
               (error "Misplaced &aux in lambda-list: ~S." list))
             (setq state '&aux))
            (t
             (error "Unknown &keyword in lambda-list: ~S." arg)))
          (case state
            (:required (push arg required))
            (&optional (push arg optional))
            (&rest
             (setq restp t  rest arg  state :post-rest))
            (&key (push arg keys))
            (&aux (push arg aux))
            (t
             (error "Found garbage in lambda-list when expecting a keyword: ~S." arg)))))
      
      (values (nreverse required) (nreverse optional) restp rest keyp (nreverse keys) allowp (nreverse aux)))))

(defvar %deftype-expanders% (make-hash-table :test #'eq))
(defvar *type-translators* (make-hash-table :test #'eq))
(defvar *builtin-type-info* (make-hash-table :test #'equal))
(defvar %builtin-type-cells% (make-hash-table :test 'equal))

(defvar *use-implementation-types* t)

(defun info-type-builtin (name)
  (gethash name *builtin-type-info*))

(defun (setf info-type-builtin) (val name)
  (setf (gethash name *builtin-type-info*) val))

(defun info-type-translator (name)
  (gethash name *type-translators*))




;;; Allow bootstrapping: mostly, allow us to bootstrap the type system
;;; by having DEFTYPE expanders defined on built-in classes (the user
;;; shouldn't be allowed to do so, at least not easily.

;(defvar *type-system-initialized* nil)

(defun %deftype (name fn doc)
  (clear-type-cache)
  (cond ((null fn)
         (remhash name %deftype-expanders%))
        ((and *type-system-initialized*
              (or (built-in-type-p name)
                  (let ((c (find-class name nil)))
                    (and c (eq (class-name c) name)))))
         (error "Cannot redefine type ~S" name))
        (t (setf (gethash name %deftype-expanders%) fn)
           (record-source-file name 'type)))
  (set-documentation name 'type doc)   ; nil clears it.
  name)

(defun %define-type-translator (name fn doc)
  (declare (ignore doc))
  (setf (gethash name *type-translators*) fn)
  name)

;;;(defun %deftype-expander (name)
;;;  (or (gethash name %deftype-expanders%)
;;;      (and *compiling-file* (%cdr (assq name *compile-time-deftype-expanders*)))))
(defun %deftype-expander (name)
  (gethash name %deftype-expanders%))

(defun process-deftype-arglist (arglist &aux (in-optional? nil))
  "Returns a NEW list similar to arglist except
    inserts * as the default default for &optional args."
  (mapcar #'(lambda (item)
              (cond ((eq item '&optional) (setq in-optional? t) item)
                    ((memq item lambda-list-keywords) (setq in-optional? nil) item)
                    ((and in-optional? (symbolp item)) (list item ''*))
                    (t item)))
          arglist))


(defun expand-type-macro (definer name arglist body env)
  (setq name (require-type name 'symbol))
  (multiple-value-bind (lambda doc)
      (parse-macro-internal name arglist body env '*)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (,definer ',name
                   (nfunction ,name ,lambda)
                   ,doc))))

(defmacro deftype (name arglist &body body &environment env)
  "Define a new type, with syntax like DEFMACRO."
  (expand-type-macro '%deftype name arglist body env))

(defmacro def-type-translator (name arglist &body body &environment env)
  (expand-type-macro '%define-type-translator name arglist body env))


(defun type-expand (form &optional env &aux def)
  (while (setq def (cond ((symbolp form)
                          (gethash form %deftype-expanders%))
                         ((and (consp form) (symbolp (%car form)))
                          (gethash (%car form) %deftype-expanders%))
                         (t nil)))
    (setq form (funcall def (if (consp form) form (list form)) env)))
  form)

(defmethod print-object ((tc type-class) stream)
  (print-unreadable-object (tc stream :type t :identity t)
    (format stream "~s" (type-class-name tc))))

(defmethod print-object ((c ctype) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~S" (type-specifier c))))

(defmethod make-load-form ((c ctype) &optional env)
  (declare (ignore env))
  `(specifier-type ',(type-specifier c)))

(defmethod make-load-form ((cell type-cell) &optional env)
  (declare (ignore env))
  `(register-type-cell `,(type-cell-type-specifier cell)))

(defmethod print-object ((cell type-cell) stream)
  (print-unreadable-object (cell stream :type t :identity t)
    (format stream "for ~s" (type-cell-type-specifier cell))))

(defun make-key-info (&key name type)
  (%istruct 'key-info name type))

(defun type-class-or-lose (name)
  (or (cdr (assq name *type-classes*))
      (error "~S is not a defined type class." name)))

(eval-when (:compile-toplevel :execute)

(defconstant type-class-function-slots
  '((:simple-subtypep . #.type-class-simple-subtypep)
    (:complex-subtypep-arg1 . #.type-class-complex-subtypep-arg1)
    (:complex-subtypep-arg2 . #.type-class-complex-subtypep-arg2)
    (:simple-union . #.type-class-simple-union)
    (:complex-union . #.type-class-complex-union)
    (:simple-intersection . #.type-class-simple-intersection)
    (:complex-intersection . #.type-class-complex-intersection)
    (:simple-= . #.type-class-simple-=)
    (:complex-= . #.type-class-complex-=)
    (:unparse . #.type-class-unparse)))

)

(defun class-typep (form class)
  (memq class (%inited-class-cpl (class-of form))))

;;; CLASS-FUNCTION-SLOT-OR-LOSE  --  Interface
;;;
(defun class-function-slot-or-lose (name)
  (or (cdr (assoc name type-class-function-slots))
      (error "~S is not a defined type class method." name)))


(eval-when (:compile-toplevel :execute)

;;; INVOKE-TYPE-METHOD  --  Interface
;;;
;;;    Invoke a type method on TYPE1 and TYPE2.  If the two types have the same
;;; class, invoke the simple method.  Otherwise, invoke any complex method.  If
;;; there isn't a distinct complex-arg1 method, then swap the arguments when
;;; calling type1's method.  If no applicable method, return DEFAULT.
;;;

(defmacro invoke-type-method (simple complex-arg2 type1 type2 &key
                                     (default '(values nil t))
                                     complex-arg1)
  (let ((simple (class-function-slot-or-lose simple))
        (cslot1 (class-function-slot-or-lose (or complex-arg1 complex-arg2)))
        (cslot2 (class-function-slot-or-lose complex-arg2)))
    (once-only ((n-type1 type1)
                (n-type2 type2))
      (once-only ((class1 `(ctype-class-info ,n-type1))
                  (class2 `(ctype-class-info ,n-type2)))
        `(if (eq ,class1 ,class2)
           (funcall (%svref ,class1 ,simple) ,n-type1 ,n-type2)
           ,(once-only ((complex1 `(%svref ,class1 ,cslot1))
                        (complex2 `(%svref ,class2 ,cslot2)))
              `(cond (,complex2 (funcall ,complex2 ,n-type1 ,n-type2))
                     (,complex1
                      ,(if complex-arg1
                         `(funcall ,complex1 ,n-type1 ,n-type2)
                         `(funcall ,complex1 ,n-type2 ,n-type1)))
                     (t ,default))))))))


;;;; Utilities:

;;; ANY-TYPE-OP, EVERY-TYPE-OP  --  Interface
;;;
;;;    Like ANY and EVERY, except that we handle two-arg uncertain predicates.
;;; If the result is uncertain, then we return Default from the block PUNT.
;;; If LIST-FIRST is true, then the list element is the first arg, otherwise
;;; the second.
;;;
(defmacro any-type-op (op thing list &key (default '(values nil nil))
			        list-first)
  (let ((n-this (gensym))
	  (n-thing (gensym))
	  (n-val (gensym))
	  (n-win (gensym))
	  (n-uncertain (gensym)))
    `(let ((,n-thing ,thing)
	     (,n-uncertain nil))
       (dolist (,n-this ,list
			      (if ,n-uncertain
			        (return-from PUNT ,default)
			        nil))
	   (multiple-value-bind (,n-val ,n-win)
			            ,(if list-first
				         `(,op ,n-this ,n-thing)
				         `(,op ,n-thing ,n-this))
	     (unless ,n-win (setq ,n-uncertain t))
	     (when ,n-val (return t)))))))
;;;
(defmacro every-type-op (op thing list &key (default '(values nil nil))
			          list-first)
  (let ((n-this (gensym))
	  (n-thing (gensym))
	  (n-val (gensym))
	  (n-win (gensym)))
    `(let ((,n-thing ,thing))
       (dolist (,n-this ,list t)
	   (multiple-value-bind (,n-val ,n-win)
			            ,(if list-first
				         `(,op ,n-this ,n-thing)
				         `(,op ,n-thing ,n-this))
	     (unless ,n-win (return-from PUNT ,default))
	     (unless ,n-val (return nil)))))))

)

  
;;; VANILLA-INTERSECTION  --  Interface
;;;
;;;    Compute the intersection for types that intersect only when one is a
;;; hierarchical subtype of the other.
;;;
(defun vanilla-intersection (type1 type2)
  (multiple-value-bind (stp1 win1)
		           (csubtypep type1 type2)
    (multiple-value-bind (stp2 win2)
			       (csubtypep type2 type1)
      (cond (stp1 (values type1 t))
	      (stp2 (values type2 t))
	      ((and win1 win2) (values *empty-type* t))
	      (t
	       (values type1 nil))))))


;;; VANILLA-UNION  --  Interface
;;;
(defun vanilla-union (type1 type2)
  (cond ((csubtypep type1 type2) type2)
	((csubtypep type2 type1) type1)
	(t nil)))

(defun hierarchical-intersection2 (type1 type2)
  (multiple-value-bind (subtypep1 win1) (csubtypep type1 type2)
    (multiple-value-bind (subtypep2 win2) (csubtypep type2 type1)
      (cond (subtypep1 type1)
	    (subtypep2 type2)
	    ((and win1 win2) *empty-type*)
	    (t nil)))))

(defun hierarchical-union2 (type1 type2)
  (cond ((csubtypep type1 type2) type2)
	((csubtypep type2 type1) type1)
	(t nil)))

;;; DELEGATE-COMPLEX-{SUBTYPEP-ARG2,INTERSECTION}  --  Interface
;;;
;;;    These functions are used as method for types which need a complex
;;; subtypep method to handle some superclasses, but cover a subtree of the
;;; type graph (i.e. there is no simple way for any other type class to be a
;;; subtype.)  There are always still complex ways, namely UNION and MEMBER
;;; types, so we must give TYPE1's method a chance to run, instead of
;;; immediately returning NIL, T.
;;;
(defun delegate-complex-subtypep-arg2 (type1 type2)
  (let ((subtypep-arg1
	 (type-class-complex-subtypep-arg1
	  (ctype-class-info type1))))
    (if subtypep-arg1
	(funcall subtypep-arg1 type1 type2)
	(values nil t))))
;;;
(defun delegate-complex-intersection (type1 type2)
  (let ((method (type-class-complex-intersection (ctype-class-info type1))))
    (if (and method (not (eq method #'delegate-complex-intersection)))
	(funcall method type2 type1)
	(hierarchical-intersection2 type1 type2))))

;;; HAS-SUPERCLASSES-COMPLEX-SUBTYPEP-ARG1  --  Internal
;;;
;;;    Used by DEFINE-SUPERCLASSES to define the SUBTYPE-ARG1 method.  Info is
;;; a list of conses (SUPERCLASS-CLASS . {GUARD-TYPE-SPECIFIER | NIL}).  Will
;;; never be called with a hairy type as type2, since the hairy type type2
;;; method gets first crack.
;;;
#|
(defun has-superclasses-complex-subtypep-arg1 (type1 type2 info)
  (values
   (and (typep type2 'class)
	(dolist (x info nil)
	  (when (or (not (cdr x))
		    (csubtypep type1 (specifier-type (cdr x))))
	    (return
	     (or (eq type2 (car x))
		 (let ((inherits (layout-inherits (class-layout (car x)))))
		   (dotimes (i (length inherits) nil)
		     (when (eq type2 (layout-class (svref inherits i)))
		       (return t)))))))))
   t))
|#

(eval-when (:compile-toplevel :execute)
;;; DEFINE-SUPERCLASSES  --  Interface
;;;
;;;    Takes a list of specs of the form (superclass &optional guard).
;;; Consider one spec (with no guard): any instance of type-class is also a
;;; subtype of SUPERCLASS and of any of its superclasses.  If there are
;;; multiple specs, then some will have guards.  We choose the first spec whose
;;; guard is a supertype of TYPE1 and use its superclass.  In effect, a
;;; sequence of guards G0, G1, G2 is actually G0, (and G1 (not G0)),
;;; (and G2 (not (or G0 G1))).
;;;
#|
(defmacro define-superclasses (type-class &rest specs)
  (let ((info
	 (mapcar #'(lambda (spec)
		     (destructuring-bind (super &optional guard)
					 spec
		       (cons (find-class super) guard)))
		 specs)))
    `(progn
      (setf (type-class-complex-subtypep-arg1
	     (type-class-or-lose ',type-class))
	    #'(lambda (type1 type2)
		(has-superclasses-complex-subtypep-arg1 type1 type2 ',info)))
       
       (setf (type-class-complex-subtypep-arg2
	      (type-class-or-lose ',type-class))
	     #'delegate-complex-subtypep-arg2)
       
       (setf (type-class-complex-intersection
	      (type-class-or-lose ',type-class))
	     #'delegate-complex-intersection))))
|#

); eval-when (compile eval)


(defun reparse-unknown-ctype (type)
  (if (unknown-ctype-p type)
    (specifier-type (type-specifier type))
    type))

(defun swapped-args-fun (f)
  #'(lambda (x y)
      (funcall f y x)))

(defun equal-but-no-car-recursion (x y)
  (cond ((eql x y) t)
	((consp x)
	 (and (consp y)
	      (eql (car x) (car y))
	      (equal-but-no-car-recursion (cdr x) (cdr y))))
	(t nil)))

(defun any/type (op thing list)
  (declare (type function op))
  (let ((certain? t))
    (dolist (i list (values nil certain?))
      (multiple-value-bind (sub-value sub-certain?) (funcall op thing i)
	(if sub-certain?
	    (when sub-value (return (values t t)))
	    (setf certain? nil))))))

(defun every/type (op thing list)
  (declare (type function op))
  (let ((certain? t))
    (dolist (i list (if certain? (values t t) (values nil nil)))
      (multiple-value-bind (sub-value sub-certain?) (funcall op thing i)
	(if sub-certain?
	    (unless sub-value (return (values nil t)))
	    (setf certain? nil))))))

(defun invoke-complex-=-other-method (type1 type2)
  (let* ((type-class (ctype-class-info type1))
	 (method-fun (type-class-complex-= type-class)))
    (if method-fun
	(funcall (the function method-fun) type2 type1)
	(values nil t))))

(defun invoke-complex-subtypep-arg1-method (type1 type2 &optional subtypep win)
  (let* ((type-class (ctype-class-info type1))
	 (method-fun (type-class-complex-subtypep-arg1 type-class)))
    (if method-fun
      (funcall (the function method-fun) type1 type2)
      (values subtypep win))))

(defun type-might-contain-other-types-p (type)
  (or (hairy-ctype-p type)
      (negation-ctype-p type)
      (union-ctype-p type)
      (intersection-ctype-p type)))


(eval-when (:compile-toplevel :execute)

(defmacro define-type-method ((class method &rest more-methods)
			            lambda-list &body body)
  `(progn
     (let* ((fn (nfunction (,class ,method ,@more-methods)
                           (lambda ,lambda-list ,@body))))
       ,@(mapcar #'(lambda (method)
		         `(setf (%svref
			           (type-class-or-lose ',class)
                             ,(class-function-slot-or-lose method))
			          fn))
		     (cons method more-methods)))
     nil))

)


(defun ctype-p (x)
  (and (eql (typecode x) target::subtag-istruct)
       (memq (istruct-type-name x)
             '#.(cons 'ctype 
                      (cons 'unknown-ctype                             
                            (append (mapcar #'class-name 
                                            (class-direct-subclasses (find-class 'args-ctype)))
                                    (mapcar #'class-name 
                                            (class-direct-subclasses (find-class 'ctype)))))))))


(setf (type-predicate 'ctype) 'ctype-p)


;;;; Function and Values types.
;;;
;;;    Pretty much all of the general type operations are illegal on VALUES
;;; types, since we can't discriminate using them, do SUBTYPEP, etc.  FUNCTION
;;; types are acceptable to the normal type operations, but are generally
;;; considered to be equivalent to FUNCTION.  These really aren't true types in
;;; any type theoretic sense, but we still parse them into CTYPE structures for
;;; two reasons:
;;; -- Parsing and unparsing work the same way, and indeed we can't tell
;;;    whether a type is a function or values type without parsing it.
;;; -- Many of the places that can be annotated with real types can also be
;;;    annotated function or values types.

;; Methods on the VALUES type class.

(defun make-values-ctype (&key
                          required
                          optional
                          rest
                          keyp
                          keywords
                          allowp)
  (%istruct 'values-ctype
            (type-class-or-lose 'values)
            nil
            required
            optional
            rest
            keyp
            keywords
            allowp
           ))

(defun values-ctype-p (x) (istruct-typep x 'values-ctype))
(setf (type-predicate 'values-ctype) 'values-ctype-p)


(define-type-method (values :simple-subtypep :complex-subtypep-arg1)
		    (type1 type2)
  (declare (ignore type2))
  (error "Subtypep is illegal on this type:~%  ~S" (type-specifier type1)))

(define-type-method (values :complex-subtypep-arg2)
		    (type1 type2)
  (declare (ignore type1))
  (error "Subtypep is illegal on this type:~%  ~S" (type-specifier type2)))


(define-type-method (values :unparse) (type)
  (cons 'values (unparse-args-types type)))


;;; TYPE=-LIST  --  Internal
;;;
;;;    Return true if List1 and List2 have the same elements in the same
;;; positions according to TYPE=.  We return NIL, NIL if there is an uncertain
;;; comparison. 
;;;
(defun type=-list (list1 list2)
  (declare (list list1 list2))
  (do ((types1 list1 (cdr types1))
       (types2 list2 (cdr types2)))
      ((or (null types1) (null types2))
       (if (or types1 types2)
	   (values nil t)
	   (values t t)))
    (multiple-value-bind (val win)
			       (type= (first types1) (first types2))
      (unless win
	  (return (values nil nil)))
      (unless val
	  (return (values nil t))))))

(define-type-method (values :simple-=) (type1 type2)
  (let ((rest1 (args-ctype-rest type1))
	(rest2 (args-ctype-rest type2)))
    (cond ((or (args-ctype-keyp type1) (args-ctype-keyp type2)
	       (args-ctype-allowp type1) (args-ctype-allowp type2))
	     (values nil nil))
	    ((and rest1 rest2 (type/= rest1 rest2))
	     (type= rest1 rest2))
	    ((or rest1 rest2)
	     (values nil t))
	    (t
	     (multiple-value-bind (req-val req-win)
		 (type=-list (values-ctype-required type1)
			     (values-ctype-required type2))
	       (multiple-value-bind (opt-val opt-win)
		   (type=-list (values-ctype-optional type1)
			       (values-ctype-optional type2))
	         (values (and req-val opt-val) (and req-win opt-win))))))))


;; Methods on the FUNCTION type class.


(defun make-function-ctype (&key
                            required
                            optional
                            rest
                            keyp
                            keywords
                            allowp
                            wild-args
                            returns)
  (%istruct 'function-ctype
            (type-class-or-lose 'function)
            nil
            required
            optional
            rest
            keyp
            keywords
            allowp
            wild-args
            returns
           ))

(defun function-ctype-p (x) (istruct-typep x 'function-ctype))
(setf (type-predicate 'function-ctype) 'function-ctype-p)

;;; A flag that we can bind to cause complex function types to be unparsed as
;;; FUNCTION.  Useful when we want a type that we can pass to TYPEP.
;;;
(defvar *unparse-function-type-simplify* nil)

(define-type-method (function :unparse) (type)
  (if *unparse-function-type-simplify*
    'function
    (list 'function
	    (if (function-ctype-wild-args type)
		'*
		(unparse-args-types type))
	    (type-specifier
	     (function-ctype-returns type)))))

;;; Since all function types are equivalent to FUNCTION, they are all subtypes
;;; of each other.
;;;

(define-type-method (function :simple-subtypep) (type1 type2)
 (flet ((fun-type-simple-p (type)
          (not (or (function-ctype-rest type)
                   (function-ctype-keyp type))))
        (every-csubtypep (types1 types2)
          (loop
             for a1 in types1
             for a2 in types2
             do (multiple-value-bind (res sure-p)
                    (csubtypep a1 a2)
                  (unless res (return (values res sure-p))))
             finally (return (values t t)))))
   (macrolet ((3and (x y)
                `(multiple-value-bind (val1 win1) ,x
                   (if (and (not val1) win1)
                       (values nil t)
                       (multiple-value-bind (val2 win2) ,y
                         (if (and val1 val2)
                             (values t t)
                             (values nil (and win2 (not val2)))))))))
     (3and (values-subtypep (function-ctype-returns type1)
                            (function-ctype-returns type2))
           (cond ((function-ctype-wild-args type2) (values t t))
                 ((function-ctype-wild-args type1)
                  (cond ((function-ctype-keyp type2) (values nil nil))
                        ((not (function-ctype-rest type2)) (values nil t))
                        ((not (null (function-ctype-required type2))) (values nil t))
                        (t (3and (type= *universal-type* (function-ctype-rest type2))
                                 (every/type #'type= *universal-type*
                                             (function-ctype-optional type2))))))
                 ((not (and (fun-type-simple-p type1)
                            (fun-type-simple-p type2)))
                  (values nil nil))
                 (t (multiple-value-bind (min1 max1) (function-type-nargs type1)
                      (multiple-value-bind (min2 max2) (function-type-nargs type2)
                        (cond ((or (> max1 max2) (< min1 min2))
                               (values nil t))
                              ((and (= min1 min2) (= max1 max2))
                               (3and (every-csubtypep (function-ctype-required type1)
                                                      (function-ctype-required type2))
                                     (every-csubtypep (function-ctype-optional type1)
                                                      (function-ctype-optional type2))))
                              (t (every-csubtypep
                                  (concatenate 'list
                                               (function-ctype-required type1)
                                               (function-ctype-optional type1))
                                  (concatenate 'list
                                               (function-ctype-required type2)
                                               (function-ctype-optional type2)))))))))))))


                   
;(define-superclasses function (function))       


;;; The union or intersection of two FUNCTION types is FUNCTION.
;;; (unless the types are type=)
;;;
(define-type-method (function :simple-union) (type1 type2)
  (if (type= type1 type2)
    type1
    (specifier-type 'function)))

;;;
(define-type-method (function :simple-intersection) (type1 type2)
  (if (type= type1 type2)
    type1
    (specifier-type 'function)))


;;; ### Not very real, but good enough for redefining transforms according to
;;; type:
;;;
(define-type-method (function :simple-=) (type1 type2)
  (values (equalp type1 type2) t))

;;; The CONSTANT-TYPE structure represents a use of the CONSTANT-ARGUMENT "type
;;; specifier", which is only meaningful in function argument type specifiers
;;; used within the compiler.
;;;

(defun clone-type-class-methods (src-tc dest-tc)
  (do* ((n (uvsize src-tc))
        (i 2 (1+ i)))
       ((= i n) dest-tc)
    (declare (fixnum i n))
    (setf (%svref dest-tc i)
          (%svref src-tc i))))

(clone-type-class-methods (type-class-or-lose 'values) (type-class-or-lose 'constant))

(defun make-constant-ctype (&key type)
  (%istruct 'constant-ctype
            (type-class-or-lose 'constant)
            nil
            type))

(defun constant-ctype-p (x) (istruct-typep x 'constant-ctype))
(setf (type-predicate 'constant-ctype) 'constant-ctype-p)

(define-type-method (constant :unparse) (type)
  `(constant-argument ,(type-specifier (constant-ctype-type type))))

(define-type-method (constant :simple-=) (type1 type2)
  (type= (constant-ctype-type type1) (constant-ctype-type type2)))

(def-type-translator constant-argument (type &environment env)
  (make-constant-ctype :type (specifier-type type env)))


;;; Parse-Args-Types  --  Internal
;;;
;;;    Given a lambda-list like values type specification and a Args-Type
;;; structure, fill in the slots in the structure accordingly.  This is used
;;; for both FUNCTION and VALUES types.
;;;

(defun parse-args-types (lambda-list result &optional env)
  (multiple-value-bind (required optional restp rest keyp keys allowp aux)
		           (parse-lambda-list lambda-list)
    (when aux
      (error "&Aux in a FUNCTION or VALUES type: ~S." lambda-list))
    (flet ((parse (spec) (specifier-type spec env)))
      (setf (args-ctype-required result) (mapcar #'parse required))
      (setf (args-ctype-optional result) (mapcar #'parse optional))
      (setf (args-ctype-rest result) (if restp (parse rest) nil))
      (setf (args-ctype-keyp result) keyp)
      (let* ((key-info ()))
        (dolist (key keys)
	  (when (or (atom key) (/= (length key) 2))
	    (signal-program-error "Keyword type description is not a two-list: ~S." key))
	  (let ((kwd (first key)))
	    (when (member kwd key-info :test #'eq :key #'(lambda (x) (key-info-name x)))
	      (signal-program-error "Repeated keyword ~S in lambda list: ~S." kwd lambda-list))
	    (push (make-key-info :name kwd
                                 :type (parse (second key))) key-info)))
        (setf (args-ctype-keywords result) (nreverse key-info)))
      (setf (args-ctype-allowp result) allowp))))

;;; Unparse-Args-Types  --  Internal
;;;
;;;    Return the lambda-list like type specification corresponding
;;; to a Args-Type.
;;;
(defun unparse-args-types (type)
  (let* ((result ()))

    (dolist (arg (args-ctype-required type))
      (push (type-specifier arg) result))

    (when (args-ctype-optional type)
      (push '&optional result)
      (dolist (arg (args-ctype-optional type))
	  (push (type-specifier arg) result)))

    (when (args-ctype-rest type)
      (push '&rest result)
      (push (type-specifier (args-ctype-rest type)) result))

    (when (args-ctype-keyp type)
      (push '&key result)
      (dolist (key (args-ctype-keywords type))
	  (push (list (key-info-name key)
                    (type-specifier (key-info-type key))) result)))

    (when (args-ctype-allowp type)
      (push '&allow-other-keys result))

    (nreverse result)))

(def-type-translator function (&optional (args '*) (result '*) &environment env)
  (let ((res (make-function-ctype
	        :returns (values-specifier-type result env))))
    (if (eq args '*)
	(setf (function-ctype-wild-args res) t)
	(parse-args-types args res env))
    res))

(def-type-translator values (&rest values &environment env)
  (let ((res (make-values-ctype)))
    (parse-args-types values res env)
    (when (or (values-ctype-keyp res) (values-ctype-allowp res))
      (signal-program-error "&KEY or &ALLOW-OTHER-KEYS in values type: ~s"
			    res))
    res))

;;; Single-Value-Type  --  Interface
;;;
;;;    Return the type of the first value indicated by Type.  This is used by
;;; people who don't want to have to deal with values types.
;;;
(defun single-value-type (type)
  (declare (type ctype type))
  (cond ((values-ctype-p type)
	 (or (car (args-ctype-required type))
	     (if (args-ctype-optional type)
                 (type-union (car (args-ctype-optional type))
			     (specifier-type 'null)))
	     (args-ctype-rest type)
	     (specifier-type 'null)))
	((eq type *wild-type*)
	 *universal-type*)
	(t
	 type)))


;;; FUNCTION-TYPE-NARGS  --  Interface
;;;
;;;    Return the minmum number of arguments that a function can be called
;;; with, and the maximum number or NIL.  If not a function type, return
;;; NIL, NIL.
;;;
(defun function-type-nargs (type)
  (declare (type ctype type))
  (if (function-ctype-p type)
    (let ((fixed (length (args-ctype-required type))))
	(if (or (args-ctype-rest type)
		  (args-ctype-keyp type)
		  (args-ctype-allowp type))
        (values fixed nil)
        (values fixed (+ fixed (length (args-ctype-optional type))))))
    (values nil nil)))


;;; Values-Types  --  Interface
;;;
;;;    Determine if Type corresponds to a definite number of values.  The first
;;; value is a list of the types for each value, and the second value is the
;;; number of values.  If the number of values is not fixed, then return NIL
;;; and :Unknown.
;;;
(defun values-types (type)
  (declare (type ctype type))
  (cond ((eq type *wild-type*)
	   (values nil :unknown))
	  ((not (values-ctype-p type))
	   (values (list type) 1))
	  ((or (args-ctype-optional type)
	       (args-ctype-rest type)
	       (args-ctype-keyp type)
	       (args-ctype-allowp type))
	   (values nil :unknown))
	  (t
	   (let ((req (args-ctype-required type)))
	     (values (mapcar #'single-value-type req) (length req))))))


;;; Values-Type-Types  --  Internal
;;;
;;;    Return two values:
;;; 1] A list of all the positional (fixed and optional) types.
;;; 2] The rest type (if any).  If keywords allowed, *universal-type*.  If no
;;;    keywords or rest, *empty-type*.
;;;
(defun values-type-types (type &optional (default-type *empty-type*))
  (declare (type values-type type))
  (values (append (args-ctype-required type)
		  (args-ctype-optional type))
	    (cond ((args-ctype-keyp type) *universal-type*)
		  ((args-ctype-rest type))
		  (t default-type))))


;;; Fixed-Values-Op  --  Internal
;;;
;;;    Return a list of Operation applied to the types in Types1 and Types2,
;;; padding with Rest2 as needed.  Types1 must not be shorter than Types2.  The
;;; second value is T if Operation always returned a true second value.
;;;
(defun fixed-values-op (types1 types2 rest2 operation)
  (declare (list types1 types2) (type ctype rest2) (type function operation))
  (let ((exact t))
    (values (mapcar #'(lambda (t1 t2)
			      (multiple-value-bind (res win)
				  (funcall operation t1 t2)
			        (unless win (setq exact nil))
			        res))
		        types1
		        (append types2
				(make-list (- (length types1) (length types2))
					   :initial-element rest2)))
	      exact)))

;;; Coerce-To-Values  --  Internal
;;;
;;; If Type isn't a values type, then make it into one:
;;;    <type>  ==>  (values type &rest t)
;;;
(defun coerce-to-values (type)
  (declare (type ctype type))
  (if (values-ctype-p type)
    type
    (make-values-ctype :required (list type))))


;;; Args-Type-Op  --  Internal
;;;
;;;    Do the specified Operation on Type1 and Type2, which may be any type,
;;; including Values types.  With values types such as:
;;;    (values a0 a1)
;;;    (values b0 b1)
;;;
;;; We compute the more useful result:
;;;    (values (<operation> a0 b0) (<operation> a1 b1))
;;;
;;; Rather than the precise result:
;;;    (<operation> (values a0 a1) (values b0 b1))
;;;
;;; This has the virtue of always keeping the values type specifier outermost,
;;; and retains all of the information that is really useful for static type
;;; analysis.  We want to know what is always true of each value independently.
;;; It is worthless to know that IF the first value is B0 then the second will
;;; be B1.
;;;
;;; If the values count signatures differ, then we produce result with the
;;; required value count chosen by Nreq when applied to the number of required
;;; values in type1 and type2.  Any &key values become &rest T (anyone who uses
;;; keyword values deserves to lose.)
;;;
;;; The second value is true if the result is definitely empty or if Operation
;;; returned true as its second value each time we called it.  Since we
;;; approximate the intersection of values types, the second value being true
;;; doesn't mean the result is exact.
;;;
(defun args-type-op (type1 type2 operation nreq default-type)
  (declare (type ctype type1 type2 default-type)
	   (type function operation nreq))
  (if (eq type1 type2)
    (values type1 t)
    (if (or (values-ctype-p type1) (values-ctype-p type2))
      (let ((type1 (coerce-to-values type1))
	    (type2 (coerce-to-values type2)))
	(multiple-value-bind (types1 rest1)
	    (values-type-types type1 default-type)
	  (multiple-value-bind (types2 rest2)
	      (values-type-types type2 default-type)
	    (multiple-value-bind (rest rest-exact)
		(funcall operation rest1 rest2)
	      (multiple-value-bind
		  (res res-exact)
		  (if (< (length types1) (length types2))
		    (fixed-values-op types2 types1 rest1 operation)
		    (fixed-values-op types1 types2 rest2 operation))
		(let* ((req (funcall nreq
				     (length (args-ctype-required type1))
				     (length (args-ctype-required type2))))
		       (required (subseq res 0 req))
		       (opt (subseq res req))
		       (opt-last (position rest opt :test-not #'type=
					   :from-end t)))
		  (if (find *empty-type* required :test #'type=)
		    (values *empty-type* t)
		    (values (make-values-ctype
			     :required required
			     :optional (if opt-last
					 (subseq opt 0 (1+ opt-last))
					 ())
			     :rest (if (eq rest *empty-type*) nil rest))
			    (and rest-exact res-exact)))))))))
      (funcall operation type1 type2))))

;;; Values-Type-Union, Values-Type-Intersection  --  Interface
;;;
;;;    Do a union or intersection operation on types that might be values
;;; types.  The result is optimized for utility rather than exactness, but it
;;; is guaranteed that it will be no smaller (more restrictive) than the
;;; precise result.
;;;

(defun values-type-union (type1 type2)
  (declare (type ctype type1 type2))
  (cond ((or (eq type1 *wild-type*) (eq type2 *wild-type*)) *wild-type*)
	((eq type1 *empty-type*) type2)
	((eq type2 *empty-type*) type1)
	(t
	 (values (args-type-op type1 type2 #'type-union #'min *empty-type*)))))

(defun values-type-intersection (type1 type2)
  (declare (type ctype type1 type2))
  (cond ((eq type1 *wild-type*) (values type2 t))
	((eq type2 *wild-type*) (values type1 t))
	(t
	 (args-type-op type1 type2 #'type-intersection #'max
		       (specifier-type 'null)))))


;;; Values-Types-Intersect  --  Interface
;;;
;;;    Like Types-Intersect, except that it sort of works on values types.
;;; Note that due to the semantics of Values-Type-Intersection, this might
;;; return {T, T} when there isn't really any intersection (?).
;;;
(defun values-types-intersect (type1 type2)
  (cond ((or (eq type1 *empty-type*) (eq type2 *empty-type*))
	   (values t t))
	  ((or (values-ctype-p type1) (values-ctype-p type2))
	   (multiple-value-bind (res win)
			            (values-type-intersection type1 type2)
	     (values (not (eq res *empty-type*))
		       win)))
	  (t
	   (types-intersect type1 type2))))

;;; Values-Subtypep  --  Interface
;;;
;;;    A subtypep-like operation that can be used on any types, including
;;; values types.
;;;

(defun values-subtypep (type1 type2)
  (declare (type ctype type1 type2))
  (cond ((eq type2 *wild-type*) (values t t))
	((eq type1 *wild-type*)
	 (values (eq type2 *universal-type*) t))
	((not (values-types-intersect type1 type2))
	 (values nil t))
	(t
	 (if (or (values-ctype-p type1) (values-ctype-p type2))
	   (let ((type1 (coerce-to-values type1))
		 (type2 (coerce-to-values type2)))
	     (multiple-value-bind (types1 rest1)
		 (values-type-types type1)
	       (multiple-value-bind (types2 rest2)
		   (values-type-types type2)
		 (cond ((< (length (values-ctype-required type1))
			   (length (values-ctype-required type2)))
			(values nil t))
		       ((< (length types1) (length types2))
			(values nil nil))
		       ((or (values-ctype-keyp type1)
			    (values-ctype-keyp type2))
			(values nil nil))
		       (t
			(do ((t1 types1 (rest t1))
			     (t2 types2 (rest t2)))
			    ((null t2)
			     (csubtypep rest1 rest2))
			  (multiple-value-bind
			      (res win-p)
			      (csubtypep (first t1) (first t2))
			    (unless win-p
			      (return (values nil nil)))
			    (unless res
			      (return (values nil t))))))))))
	   (csubtypep type1 type2)))))
  

;;;; Type method interfaces:

;;; Csubtypep  --  Interface
;;;
;;;    Like subtypep, only works on Type structures.
;;;
(defun csubtypep (type1 type2)
  (declare (type ctype type1 type2))
  (unless (typep type1 'ctype)
    (report-bad-arg type1 'ctype))
  (unless (typep type2 'ctype)
    (report-bad-arg type2 'ctype))
  (cond ((or (eq type1 type2)
	     (eq type1 *empty-type*)
	     (eq type2 *wild-type*))
	 (values t t))
	(t
	 (invoke-type-method :simple-subtypep :complex-subtypep-arg2
			     type1 type2
			     :complex-arg1 :complex-subtypep-arg1))))

;;; Type1 is a type-epecifier; type2 is a TYPE-CELL which may cache
;;; a mapping between a type-specifier and a CTYPE.
(defun cell-csubtypep-2 (type-specifier type-cell)
  (let* ((type1 (specifier-type type-specifier))
         (type2 (or (type-cell-ctype type-cell)
                    (let* ((ctype (specifier-type
                                   (type-cell-type-specifier type-cell))))
                      (when (cacheable-ctype-p ctype)
                        (setf (type-cell-ctype type-cell) ctype))
                      ctype))))
    (cond ((or (eq type1 type2)
               (eq type1 *empty-type*)
               (eq type2 *wild-type*))
           (values t t))
          (t
           (invoke-type-method :simple-subtypep :complex-subtypep-arg2
                               type1 type2
                               :complex-arg1 :complex-subtypep-arg1)))))
                              


;;; Type=  --  Interface
;;;
;;;    If two types are definitely equivalent, return true.  The second value
;;; indicates whether the first value is definitely correct.  This should only
;;; fail in the presence of Hairy types.
;;;

(defun type= (type1 type2)
   (declare (type ctype type1 type2))
   (if (eq type1 type2)
     (values t t)
     (invoke-type-method :simple-= :complex-= type1 type2)))

;;; TYPE/=  --  Interface
;;;
;;;    Not exactly the negation of TYPE=, since when the relationship is
;;; uncertain, we still return NIL, NIL.  This is useful in cases where the
;;; conservative assumption is =.
;;;
(defun type/= (type1 type2)
  (declare (type ctype type1 type2))
  (multiple-value-bind (res win)
      (type= type1 type2)
    (if win
	(values (not res) t)
	(values nil nil))))

;;; Type-Union  --  Interface
;;;
;;;    Find a type which includes both types.  Any inexactness is represented
;;; by the fuzzy element types; we return a single value that is precise to the
;;; best of our knowledge.  This result is simplified into the canonical form,
;;; thus is not a UNION type unless there is no other way to represent the
;;; result.
;;; 

(defun type-union (&rest input-types)
  (%type-union input-types))

(defun %type-union (input-types)
  (let* ((simplified (simplify-unions input-types)))
    (cond ((null simplified) *empty-type*)
	  ((null (cdr simplified)) (car simplified))
	  (t (make-union-ctype simplified)))))

(defun simplify-unions (types)
  (when types
    (multiple-value-bind (first rest)
	(if (union-ctype-p (car types))
	  (values (car (union-ctype-types (car types)))
		  (append (cdr (union-ctype-types (car types)))
			  (cdr types)))
	  (values (car types) (cdr types)))
      (let ((rest (simplify-unions rest)) u)
	(dolist (r rest (cons first rest))
	  (when (setq u (type-union2 first r))
	    (return (simplify-unions (nsubstitute u r rest)))))))))

(defun type-union2 (type1 type2)
  (declare (type ctype type1 type2))
  (setq type1 (reparse-unknown-ctype type1))
  (setq type2 (reparse-unknown-ctype type2))
  (cond ((eq type1 type2) type1)
	((csubtypep type1 type2) type2)
	((csubtypep type2 type1) type1)
	(t
	 (flet ((1way (x y)
		  (invoke-type-method :simple-union :complex-union
				      x y
				      :default nil)))
	   (or (1way type1 type2)
	       (1way type2 type1))))))

;;; Return as restrictive and simple a type as we can discover that is
;;; no more restrictive than the intersection of TYPE1 and TYPE2. At
;;; worst, we arbitrarily return one of the arguments as the first
;;; value (trying not to return a hairy type).
(defun type-approx-intersection2 (type1 type2)
  (cond ((type-intersection2 type1 type2))
	((hairy-ctype-p type1) type2)
	(t type1)))


;;; Type-Intersection  --  Interface
;;;
;;;    Return as restrictive a type as we can discover that is no more
;;; restrictive than the intersection of Type1 and Type2.  The second value is
;;; true if the result is exact.  At worst, we randomly return one of the
;;; arguments as the first value (trying not to return a hairy type).
;;;

(defun type-intersection (&rest input-types)
  (%type-intersection input-types))

(defun %type-intersection (input-types)
  (let ((simplified (simplify-intersections input-types)))
    ;;(declare (type (vector ctype) simplified))
    ;; We want to have a canonical representation of types (or failing
    ;; that, punt to HAIRY-TYPE). Canonical representation would have
    ;; intersections inside unions but not vice versa, since you can
    ;; always achieve that by the distributive rule. But we don't want
    ;; to just apply the distributive rule, since it would be too easy
    ;; to end up with unreasonably huge type expressions. So instead
    ;; we try to generate a simple type by distributing the union; if
    ;; the type can't be made simple, we punt to HAIRY-TYPE.
    (if (and (cdr simplified) (some #'union-ctype-p simplified))
      (let* ((first-union (find-if #'union-ctype-p simplified))
             (other-types (remove first-union simplified))
             (distributed (maybe-distribute-one-union first-union other-types)))
        (if distributed
          (apply #'type-union distributed)
          (make-hairy-ctype
           :specifier `(and ,@(mapcar #'type-specifier simplified)))))
      (cond
        ((null simplified) *universal-type*)
        ((null (cdr simplified)) (car simplified))
        (t (make-intersection-ctype
            (some #'(lambda (c) (ctype-enumerable c)) simplified)
            simplified))))))

(defun simplify-intersections (types)
  (when types
    (multiple-value-bind (first rest)
	(if (intersection-ctype-p (car types))
	    (values (car (intersection-ctype-types (car types)))
		    (append (cdr (intersection-ctype-types (car types)))
			    (cdr types)))
	    (values (car types) (cdr types)))
      (let ((rest (simplify-intersections rest)) u)
	(dolist (r rest (cons first rest))
	  (when (setq u (type-intersection2 first r))
	    (return (simplify-intersections (nsubstitute u r rest)))))))))

(defun type-intersection2 (type1 type2)
  (declare (type ctype type1 type2))
  (setq type1 (reparse-unknown-ctype type1))
  (setq type2 (reparse-unknown-ctype type2))
  (cond ((eq type1 type2)
	 type1)
	((or (intersection-ctype-p type1)
	     (intersection-ctype-p type2))
	 ;; Intersections of INTERSECTION-TYPE should have the
	 ;; INTERSECTION-CTYPE-TYPES values broken out and intersected
	 ;; separately. The full TYPE-INTERSECTION function knows how
	 ;; to do that, so let it handle it.
	 (type-intersection type1 type2))
	;;
	;; (AND (FUNCTION (T) T) GENERIC-FUNCTION) for instance, but
	;; not (AND (FUNCTION (T) T) (FUNCTION (T) T)).
	((let ((function (specifier-type 'function)))
	   (or (and (function-ctype-p type1)
		    (not (or (function-ctype-p type2) (eq function type2)))
		    (csubtypep type2 function)
		    (not (csubtypep function type2)))
	       (and (function-ctype-p type2)
		    (not (or (function-ctype-p type1) (eq function type1)))
		    (csubtypep type1 function)
		    (not (csubtypep function type1)))))
	 nil)
	(t
	 (flet ((1way (x y)
		  (invoke-type-method :simple-intersection
				      :complex-intersection
				      x y
				      :default :no-type-method-found)))
	   (let ((xy (1way type1 type2)))
	     (or (and (not (eql xy :no-type-method-found)) xy)
		 (let ((yx (1way type2 type1)))
		   (or (and (not (eql yx :no-type-method-found)) yx)
		       (cond ((and (eql xy :no-type-method-found)
				   (eql yx :no-type-method-found))
			      *empty-type*)
			     (t
			      nil))))))))))



(defun maybe-distribute-one-union (union-type types)
  (let* ((intersection (apply #'type-intersection types))
	 (union (mapcar (lambda (x) (type-intersection x intersection))
			(union-ctype-types union-type))))
    (if (notany (lambda (x)
		  (or (hairy-ctype-p x)
		      (intersection-ctype-p x)))
		union)
	union
	nil)))

;;; Types-Intersect  --  Interface
;;;
;;;    The first value is true unless the types don't intersect.  The second
;;; value is true if the first value is definitely correct.  NIL is considered
;;; to intersect with any type.  If T is a subtype of either type, then we also
;;; return T, T.  This way we consider hairy types to intersect with T.
;;;
(defun types-intersect (type1 type2)
  (declare (type ctype type1 type2))
  (if (or (eq type1 *empty-type*) (eq type2 *empty-type*))
      (values t t)
      (let ((intersection2 (type-intersection2 type1 type2)))
	(cond ((not intersection2)
	       (if (or (csubtypep *universal-type* type1)
		       (csubtypep *universal-type* type2))
		   (values t t)
		   (values t nil)))
	      ((eq intersection2 *empty-type*) (values nil t))
	      (t (values t t))))))

;;; Type-Specifier  --  Interface
;;;
;;;    Return a Common Lisp type specifier corresponding to this type.
;;;
(defun type-specifier (type)
  (unless (ctype-p type)
    (setq type (require-type type 'ctype)))
  (locally 
      (declare (type ctype type))
    (funcall (type-class-unparse (ctype-class-info type)) type)))


(defconstant compound-only-type-specifiers
  ;; See CLHS Figure 4-4.
  '(and mod satisfies eql not values member or))


;;; VALUES-SPECIFIER-TYPE  --  Interface
;;;
;;;    Return the type structure corresponding to a type specifier.  We pick
;;; off Structure types as a special case.
;;;

(defun values-specifier-type-internal (orig env)
  (or (info-type-builtin orig) ; this table could contain bytes etal and ands ors nots of built-in types - no classes
      
      ;; Now that we have our hands on the environment, we could pass it into type-expand,
      ;; but we'd have no way of knowing whether the expansion depended on the env, so
      ;; we wouldn't know if the result is safe to cache.   So for now don't let type
      ;; expanders see the env, which just means they won't see compile-time types.
      (let ((spec (type-expand orig #+not-yet env)))
        (cond
         ((and (not (eq spec orig))
               (info-type-builtin spec)))
         ((or (eq (info-type-kind spec) :instance)
              (and (symbolp spec)
                   (typep (find-class spec nil env) 'compile-time-class)))
          (let* ((class-ctype (%class.ctype (find-class spec t env))))
            (or (class-ctype-translation class-ctype)
                class-ctype)))
         ((typep spec 'class)
          (let* ((class-ctype (%class.ctype spec)))
            (or (class-ctype-translation class-ctype)
                class-ctype)))
         ((let ((cell (find-builtin-cell spec nil)))
           (and cell (cdr cell))))
         (t
          (when (member spec compound-only-type-specifiers)
            (error 'invalid-type-specifier :typespec spec))
          (let* ((lspec (if (atom spec) (list spec) spec))
                 (fun (info-type-translator (car lspec))))
            (cond (fun (funcall fun lspec env))
                  ((or (and (consp spec)
                            (symbolp (car spec))
                            (not (or (find-class (car spec) nil env)
                                     (info-type-builtin (car spec)))))
                       (symbolp spec))
                   (when *type-system-initialized*
                     (signal 'parse-unknown-type :specifier spec))
                   ;;
                   ;; Inhibit caching...
                   nil)
                  (t
                   (error 'invalid-type-specifier :typespec spec)))))))))

(eval-when (:compile-toplevel :execute)
  (defconstant type-cache-size (ash 1 12))
  (defconstant type-cache-mask (1- type-cache-size)))

(defun compile-time-ctype-p (ctype)
  (and (typep ctype 'class-ctype)
       (typep (class-ctype-class ctype) 'compile-time-class)))


;;; We can get in trouble if we try to cache certain kinds of ctypes,
;;; notably MEMBER types which refer to objects which might
;;; be stack-allocated or might be EQUAL without being EQL.
(defun cacheable-ctype-p (ctype)
  (case (istruct-cell-name (%svref ctype 0))
    (member-ctype
     (dolist (m (member-ctype-members ctype) t)
       (when (or (typep m 'cons)
		 (typep m 'array))
	 (return nil))))
    (union-ctype
     (every #'cacheable-ctype-p (union-ctype-types ctype)))
    (intersection-ctype
     (every #'cacheable-ctype-p (intersection-ctype-types ctype)))
    (array-ctype
     (cacheable-ctype-p (array-ctype-element-type ctype)))
    ((values-ctype function-ctype)
     (and (every #'cacheable-ctype-p (values-ctype-required ctype))
	  (every #'cacheable-ctype-p (values-ctype-optional ctype))
	  (let* ((rest (values-ctype-rest ctype)))
	    (or (null rest) (cacheable-ctype-p rest)))
	  (every #'(lambda (info)
		     (cacheable-ctype-p (key-info-type info)))
		 (values-ctype-keywords ctype))
	  (or (not (eq (istruct-cell-name (%svref ctype 0)) 'function-ctype))
	      (let* ((result (function-ctype-returns ctype)))
		(or (null result)
		    (cacheable-ctype-p result))))))
    (negation-ctype
     (cacheable-ctype-p (negation-ctype-type ctype)))
    (cons-ctype
     (and (cacheable-ctype-p (cons-ctype-car-ctype ctype))
	  (cacheable-ctype-p (cons-ctype-cdr-ctype ctype))))
    (unknown-ctype nil)
    (class-ctype
     (not (typep (class-ctype-class ctype) 'compile-time-class)))
    ;; Anything else ?  Simple things (numbers, classes) can't lose.
    (t t)))
		
      
    

(defun hash-type-specifier (spec)
  (logand (sxhash spec) type-cache-mask))

(let* ((type-cache-specs (make-array type-cache-size))
       (type-cache-ctypes (make-array type-cache-size))
       (probes 0)
       (hits 0)
       (ncleared 0)
       (locked nil))
  
  (defun clear-type-cache ()
    (%init-misc 0 type-cache-specs)
    (%init-misc 0 type-cache-ctypes)
    (incf ncleared)
    nil)

  (defun values-specifier-type (spec &optional env)
    (if (typep spec 'class)
      (let* ((class-ctype (%class.ctype spec)))
        (or (class-ctype-translation class-ctype) class-ctype))
      (if locked
        (or (values-specifier-type-internal spec env)
            (make-unknown-ctype :specifier spec))
        (unwind-protect
          (progn
            (setq locked t)
            (if (or (symbolp spec)
                    (and (consp spec) (symbolp (car spec))))
              (let* ((idx (hash-type-specifier spec)))
                (incf probes)
                (if (equal (svref type-cache-specs idx) spec)
                  (progn
                    (incf hits)
                    (svref type-cache-ctypes idx))
                  (let* ((ctype (values-specifier-type-internal spec env)))
                    (if ctype
		      (progn
			(when (cacheable-ctype-p ctype)
			  (setf (svref type-cache-specs idx) (copy-tree spec)       ; in case it was stack-consed
				(svref type-cache-ctypes idx) ctype))
			ctype)
                      (make-unknown-ctype :specifier spec)))))
              (values-specifier-type-internal spec env)))
          (setq locked nil)))))
  
  (defun type-cache-hit-rate ()
    (values hits probes))
  
  (defun type-cache-locked-p ()
    locked)

  (defun lock-type-cache ()
    (setq locked t)))

                    

  

;;; SPECIFIER-TYPE  --  Interface
;;;
;;;    Like VALUES-SPECIFIER-TYPE, except that we guarantee to never return a
;;; VALUES type.
;;; 
(defun specifier-type (x &optional env)
  (let ((res (values-specifier-type x env)))
    (when (values-ctype-p res)
      (signal-program-error "VALUES type illegal in this context:~%  ~S" x))
    res))

(defun single-value-specifier-type (x &optional env)
  (let ((res (specifier-type x env)))
    (if (eq res *wild-type*)
        *universal-type*
        res)))

(defun standardized-type-specifier (spec &optional env)
  (handler-case
      (type-specifier (specifier-type spec env))
    (invalid-type-specifier () spec)
    (parse-unknown-type () spec)))

(defun modified-numeric-type (base
			      &key
			      (class      (numeric-ctype-class      base))
			      (format     (numeric-ctype-format     base))
			      (complexp   (numeric-ctype-complexp   base))
			      (low        (numeric-ctype-low        base))
			      (high       (numeric-ctype-high       base))
			      (enumerable (ctype-enumerable base)))
  (make-numeric-ctype :class class
		     :format format
		     :complexp complexp
		     :low low
		     :high high
		     :enumerable enumerable))

;;; Precompute-Types  --  Interface
;;;
;;;    Take a list of type specifiers, compute the translation and define it as
;;; a builtin type.
;;;
 
(defun precompute-types (specs)
  (dolist (spec specs)
    (let ((res (specifier-type spec)))
      (when (numeric-ctype-p res)
        (let ((pred (make-numeric-ctype-predicate res)))
          (when pred (setf (numeric-ctype-predicate res) pred))))
      (unless (unknown-ctype-p res)
        (setf (info-type-builtin spec) res)
        (setf (info-type-kind spec) :primitive)))))

;;;; Builtin types.

;;; The NAMED-TYPE is used to represent *, T and NIL.  These types must be
;;; super or sub types of all types, not just classes and * & NIL aren't
;;; classes anyway, so it wouldn't make much sense to make them built-in
;;; classes.
;;;

(defun define-named-ctype (name)
  (let* ((ctype (%istruct 'named-ctype
                          (type-class-or-lose 'named)
                          nil
                          name)))
    (setf (info-type-kind name) :builtin
          (info-type-builtin name) ctype)))


(defvar *wild-type* (define-named-ctype '*))
(defvar *empty-type* (define-named-ctype nil))
(defvar *universal-type* (define-named-ctype t))

(defun named-ctype-p (x)
  (istruct-typep x 'named-ctype))

(setf (type-predicate 'named-ctype) 'named-ctype-p)

(define-type-method (named :simple-=) (type1 type2)
  (values (eq type1 type2) t))

(define-type-method (named :complex-=) (type1 type2)
  (cond
    ((and (eq type2 *empty-type*)
	  (intersection-ctype-p type1)
	  ;; not allowed to be unsure on these... FIXME: keep the list
	  ;; of CL types that are intersection types once and only
	  ;; once.
	  (not (or (type= type1 (specifier-type 'ratio))
		   (type= type1 (specifier-type 'keyword)))))
     ;; things like (AND (EQL 0) (SATISFIES ODDP)) or (AND FUNCTION
     ;; STREAM) can get here.  In general, we can't really tell
     ;; whether these are equal to NIL or not, so
     (values nil nil))
    ((type-might-contain-other-types-p type1)
     (invoke-complex-=-other-method type1 type2))
    (t (values nil t))))


(define-type-method (named :simple-subtypep) (type1 type2)
  (values (or (eq type1 *empty-type*) (eq type2 *wild-type*)) t))

(define-type-method (named :complex-subtypep-arg1) (type1 type2)
  (cond ((eq type1 *empty-type*)
	 t)
	(;; When TYPE2 might be the universal type in disguise
	 (type-might-contain-other-types-p type2)
	 ;; Now that the UNION and HAIRY COMPLEX-SUBTYPEP-ARG2 methods
	 ;; can delegate to us (more or less as CALL-NEXT-METHOD) when
	 ;; they're uncertain, we can't just barf on COMPOUND-TYPE and
	 ;; HAIRY-TYPEs as we used to. Instead we deal with the
	 ;; problem (where at least part of the problem is cases like
	 ;;   (SUBTYPEP T '(SATISFIES FOO))
	 ;; or
	 ;;   (SUBTYPEP T '(AND (SATISFIES FOO) (SATISFIES BAR)))
	 ;; where the second type is a hairy type like SATISFIES, or
	 ;; is a compound type which might contain a hairy type) by
	 ;; returning uncertainty.
	 (values nil nil))
	(t
	 ;; By elimination, TYPE1 is the universal type.
	 (assert (or (eq type1 *wild-type*) (eq type1 *universal-type*)))
	 ;; This case would have been picked off by the SIMPLE-SUBTYPEP
	 ;; method, and so shouldn't appear here.
	 (assert (not (eq type2 *universal-type*)))
	 ;; Since TYPE2 is not EQ *UNIVERSAL-TYPE* and is not the
	 ;; universal type in disguise, TYPE2 is not a superset of TYPE1.
	 (values nil t))))


(define-type-method (named :complex-subtypep-arg2) (type1 type2)
  (assert (not (eq type2 *wild-type*))) ; * isn't really a type.
  (cond ((eq type2 *universal-type*)
	 (values t t))
	((type-might-contain-other-types-p type1)
	 ;; those types can be *EMPTY-TYPE* or *UNIVERSAL-TYPE* in
	 ;; disguise.  So we'd better delegate.
	 (invoke-complex-subtypep-arg1-method type1 type2))
	(t
	 ;; FIXME: This seems to rely on there only being 2 or 3
	 ;; NAMED-TYPE values, and the exclusion of various
	 ;; possibilities above. It would be good to explain it and/or
	 ;; rewrite it so that it's clearer.
	 (values (not (eq type2 *empty-type*)) t))))


(define-type-method (named :complex-intersection) (type1 type2)
  (hierarchical-intersection2 type1 type2))

(define-type-method (named :unparse) (x)
  (named-ctype-name x))


;;;; Hairy and unknown types:

;;; The Hairy-Type represents anything too wierd to be described
;;; reasonably or to be useful, such as SATISFIES.  We just remember
;;; the original type spec.
;;;

(defun make-hairy-ctype (&key specifier (enumerable t))
  (%istruct 'hairy-ctype
            (type-class-or-lose 'hairy)
            enumerable
            specifier))

(defun hairy-ctype-p (x)
  (or (istruct-typep x 'hairy-ctype)
      (istruct-typep x 'unknown-ctype)))

(setf (type-predicate 'hairy-ctype) 'hairy-ctype-p)

(define-type-method (hairy :unparse) (x) (hairy-ctype-specifier x))

(define-type-method (hairy :simple-subtypep) (type1 type2)
  (let ((hairy-spec1 (hairy-ctype-specifier type1))
	(hairy-spec2 (hairy-ctype-specifier type2)))
    (cond ((equal-but-no-car-recursion hairy-spec1 hairy-spec2)
	   (values t t))
	  (t
	   (values nil nil)))))

(define-type-method (hairy :complex-subtypep-arg2) (type1 type2)
  (invoke-complex-subtypep-arg1-method type1 type2))

(define-type-method (hairy :complex-subtypep-arg1) (type1 type2)
  (declare (ignore type1 type2))
  (values nil nil))

(define-type-method (hairy :complex-=) (type1 type2)
  (if (and (unknown-ctype-p type2)
	   (let* ((specifier2 (unknown-ctype-specifier type2))
                  (name2 (if (consp specifier2)
			   (car specifier2)
			   specifier2)))
             (info-type-kind name2)))
      (let ((type2 (specifier-type (unknown-ctype-specifier type2))))
        (if (unknown-ctype-p type2)
            (values nil nil)
            (type= type1 type2)))
  (values nil nil)))

(define-type-method (hairy :simple-intersection :complex-intersection)
		    (type1 type2)
  (if (type= type1 type2)
    type1
    nil))


(define-type-method (hairy :simple-union) 
    (type1 type2)
  (if (type= type1 type2)
      type1
      nil))

(define-type-method (hairy :simple-=) (type1 type2)
  (if (equal-but-no-car-recursion (hairy-ctype-specifier type1)
				  (hairy-ctype-specifier type2))
      (values t t)
      (values nil nil)))



(def-type-translator satisfies (&whole x fun)
  (unless (symbolp fun)
    (report-bad-arg fun 'symbol))
  (make-hairy-ctype :specifier x))


;;; Negation Ctypes
(defun make-negation-ctype (&key type (enumerable t))
  (%istruct 'negation-ctype
	    (type-class-or-lose 'negation)
	    enumerable
	    type))

(defun negation-ctype-p (x)
  (istruct-typep x 'negation-ctype))

(setf (type-predicate 'negation-ctype) 'negation-ctype-p)

(define-type-method (negation :unparse) (x)
  `(not ,(type-specifier (negation-ctype-type x))))

(define-type-method (negation :simple-subtypep) (type1 type2)
  (csubtypep (negation-ctype-type type2) (negation-ctype-type type1)))

(define-type-method (negation :complex-subtypep-arg2) (type1 type2)
  (let* ((complement-type2 (negation-ctype-type type2))
	 (intersection2 (type-intersection type1 complement-type2)))
    (if intersection2
	;; FIXME: if uncertain, maybe try arg1?
	(type= intersection2 *empty-type*)
	(invoke-complex-subtypep-arg1-method type1 type2))))

(define-type-method (negation :complex-subtypep-arg1) (type1 type2)
  (block nil
    ;; (Several logical truths in this block are true as long as
    ;; b/=T. As of sbcl-0.7.1.28, it seems impossible to construct a
    ;; case with b=T where we actually reach this type method, but
    ;; we'll test for and exclude this case anyway, since future
    ;; maintenance might make it possible for it to end up in this
    ;; code.)
    (multiple-value-bind (equal certain)
	(type= type2 *universal-type*)
      (unless certain
	(return (values nil nil)))
      (when equal
	(return (values t t))))
    (let ((complement-type1 (negation-ctype-type type1)))
      ;; Do the special cases first, in order to give us a chance if
      ;; subtype/supertype relationships are hairy.
      (multiple-value-bind (equal certain) 
	  (type= complement-type1 type2)
	;; If a = b, ~a is not a subtype of b (unless b=T, which was
	;; excluded above).
	(unless certain
	  (return (values nil nil)))
	(when equal
	  (return (values nil t))))
      ;; KLUDGE: ANSI requires that the SUBTYPEP result between any
      ;; two built-in atomic type specifiers never be uncertain. This
      ;; is hard to do cleanly for the built-in types whose
      ;; definitions include (NOT FOO), i.e. CONS and RATIO. However,
      ;; we can do it with this hack, which uses our global knowledge
      ;; that our implementation of the type system uses disjoint
      ;; implementation types to represent disjoint sets (except when
      ;; types are contained in other types).  (This is a KLUDGE
      ;; because it's fragile. Various changes in internal
      ;; representation in the type system could make it start
      ;; confidently returning incorrect results.) -- WHN 2002-03-08
      (unless (or (type-might-contain-other-types-p complement-type1)
		  (type-might-contain-other-types-p type2))
	;; Because of the way our types which don't contain other
	;; types are disjoint subsets of the space of possible values,
	;; (SUBTYPEP '(NOT AA) 'B)=NIL when AA and B are simple (and B
	;; is not T, as checked above).
	(return (values nil t)))
      ;; The old (TYPE= TYPE1 TYPE2) branch would never be taken, as
      ;; TYPE1 and TYPE2 will only be equal if they're both NOT types,
      ;; and then the :SIMPLE-SUBTYPEP method would be used instead.
      ;; But a CSUBTYPEP relationship might still hold:
      (multiple-value-bind (equal certain)
	  (csubtypep complement-type1 type2)
	;; If a is a subtype of b, ~a is not a subtype of b (unless
	;; b=T, which was excluded above).
	(unless certain
	  (return (values nil nil)))
	(when equal
	  (return (values nil t))))
      (multiple-value-bind (equal certain)
	  (csubtypep type2 complement-type1)
	;; If b is a subtype of a, ~a is not a subtype of b.  (FIXME:
	;; That's not true if a=T. Do we know at this point that a is
	;; not T?)
	(unless certain
	  (return (values nil nil)))
	(when equal
	  (return (values nil t))))
      ;; old CSR comment ca. 0.7.2, now obsoleted by the SIMPLE-CTYPE?
      ;; KLUDGE case above: Other cases here would rely on being able
      ;; to catch all possible cases, which the fragility of this type
      ;; system doesn't inspire me; for instance, if a is type= to ~b,
      ;; then we want T, T; if this is not the case and the types are
      ;; disjoint (have an intersection of *empty-type*) then we want
      ;; NIL, T; else if the union of a and b is the *universal-type*
      ;; then we want T, T. So currently we still claim to be unsure
      ;; about e.g. (subtypep '(not fixnum) 'single-float).
      ;;
      ;; OTOH we might still get here:
      (values nil nil))))

(define-type-method (negation :complex-=) (type1 type2)
  ;; (NOT FOO) isn't equivalent to anything that's not a negation
  ;; type, except possibly a type that might contain it in disguise.
  (declare (ignore type2))
  (if (type-might-contain-other-types-p type1)
      (values nil nil)
      (values nil t)))

(define-type-method (negation :simple-intersection) (type1 type2)
  (let ((not1 (negation-ctype-type type1))
	(not2 (negation-ctype-type type2)))
    (cond
      ((csubtypep not1 not2) type2)
      ((csubtypep not2 not1) type1)
      ;; Why no analagous clause to the disjoint in the SIMPLE-UNION2
      ;; method, below?  The clause would read
      ;;
      ;; ((EQ (TYPE-UNION NOT1 NOT2) *UNIVERSAL-TYPE*) *EMPTY-TYPE*)
      ;;
      ;; but with proper canonicalization of negation types, there's
      ;; no way of constructing two negation types with union of their
      ;; negations being the universal type.
      (t
       nil))))

(define-type-method (negation :complex-intersection) (type1 type2)
  (cond
    ((csubtypep type1 (negation-ctype-type type2)) *empty-type*)
    ((eq (type-intersection type1 (negation-ctype-type type2)) *empty-type*)
     type1)
    (t nil)))

(define-type-method (negation :simple-union) (type1 type2)
  (let ((not1 (negation-ctype-type type1))
	(not2 (negation-ctype-type type2)))
    (cond
      ((csubtypep not1 not2) type1)
      ((csubtypep not2 not1) type2)
      ((eq (type-intersection not1 not2) *empty-type*)
       *universal-type*)
      (t nil))))

(define-type-method (negation :complex-union) (type1 type2)
  (cond
    ((csubtypep (negation-ctype-type type2) type1) *universal-type*)
    ((eq (type-intersection type1 (negation-ctype-type type2)) *empty-type*)
     type2)
    (t nil)))

(define-type-method (negation :simple-=) (type1 type2)
  (type= (negation-ctype-type type1) (negation-ctype-type type2)))

(def-type-translator not (typespec &environment env)
  (let* ((not-type (specifier-type typespec env))
	 (spec (type-specifier not-type)))
    (cond
      ;; canonicalize (NOT (NOT FOO))
      ((and (listp spec) (eq (car spec) 'not))
       (specifier-type (cadr spec) env))
      ;; canonicalize (NOT NIL) and (NOT T)
      ((eq not-type *empty-type*) *universal-type*)
      ((eq not-type *universal-type*) *empty-type*)
      ((and (numeric-ctype-p not-type)
	    (null (numeric-ctype-low not-type))
	    (null (numeric-ctype-high not-type)))
       (make-negation-ctype :type not-type))
      ((numeric-ctype-p not-type)
       (type-union
	(make-negation-ctype
	 :type (modified-numeric-type not-type :low nil :high nil))
	(cond
	  ((null (numeric-ctype-low not-type))
	   (modified-numeric-type
	    not-type
	    :low (let ((h (numeric-ctype-high not-type)))
		   (if (consp h) (car h) (list h)))
	    :high nil))
	  ((null (numeric-ctype-high not-type))
	   (modified-numeric-type
	    not-type
	    :low nil
	    :high (let ((l (numeric-ctype-low not-type)))
		    (if (consp l) (car l) (list l)))))
	  (t (type-union
	      (modified-numeric-type
	       not-type
	       :low nil
	       :high (let ((l (numeric-ctype-low not-type)))
		       (if (consp l) (car l) (list l))))
	      (modified-numeric-type
	       not-type
	       :low (let ((h (numeric-ctype-high not-type)))
		      (if (consp h) (car h) (list h)))
	       :high nil))))))
      ((intersection-ctype-p not-type)
       (apply #'type-union
	      (mapcar #'(lambda (x)
			  (specifier-type `(not ,(type-specifier x)) env))
		      (intersection-ctype-types not-type))))
      ((union-ctype-p not-type)
       (apply #'type-intersection
	      (mapcar #'(lambda (x)
			  (specifier-type `(not ,(type-specifier x)) env))
		      (union-ctype-types not-type))))
      ((member-ctype-p not-type)
       (let ((members (member-ctype-members not-type)))
	 (if (some #'floatp members)
	   (let (floats)
	     (dolist (pair '((0.0f0 . -0.0f0) (0.0d0 . -0.0d0)))
	       (when (member (car pair) members)
		 (assert (not (member (cdr pair) members)))
		 (push (cdr pair) floats)
		 (setf members (remove (car pair) members)))
	       (when (member (cdr pair) members)
		 (assert (not (member (car pair) members)))
		 (push (car pair) floats)
		 (setf members (remove (cdr pair) members))))
	     (apply #'type-intersection
		    (if (null members)
		      *universal-type*
		      (make-negation-ctype
		       :type (make-member-ctype :members members)))
		    (mapcar
		     (lambda (x)
		       (let ((type (ctype-of x)))
			 (type-union
			  (make-negation-ctype
			   :type (modified-numeric-type type
							  :low nil :high nil))
			    (modified-numeric-type type
						   :low nil :high (list x))
			    (make-member-ctype :members (list x))
			    (modified-numeric-type type
						   :low (list x) :high nil))))
		     floats)))
	     (make-negation-ctype :type not-type))))
      ((and (cons-ctype-p not-type)
	    (eq (cons-ctype-car-ctype not-type) *universal-type*)
	    (eq (cons-ctype-cdr-ctype not-type) *universal-type*))
       (make-negation-ctype :type not-type))
      ((cons-ctype-p not-type)
       (type-union
	(make-negation-ctype :type (specifier-type 'cons env))
	(cond
	  ((and (not (eq (cons-ctype-car-ctype not-type) *universal-type*))
		(not (eq (cons-ctype-cdr-ctype not-type) *universal-type*)))
	   (type-union
	    (make-cons-ctype
	     (specifier-type `(not ,(type-specifier
				     (cons-ctype-car-ctype not-type))) env)
	     *universal-type*)
	    (make-cons-ctype
	     *universal-type*
	     (specifier-type `(not ,(type-specifier
				     (cons-ctype-cdr-ctype not-type))) env))))
	  ((not (eq (cons-ctype-car-ctype not-type) *universal-type*))
	   (make-cons-ctype
	    (specifier-type `(not ,(type-specifier
				    (cons-ctype-car-ctype not-type))) env)
	    *universal-type*))
	  ((not (eq (cons-ctype-cdr-ctype not-type) *universal-type*))
	   (make-cons-ctype
	    *universal-type*
	    (specifier-type `(not ,(type-specifier
				    (cons-ctype-cdr-ctype not-type))) env)))
	  (t (error "Weird CONS type ~S" not-type)))))
      (t (make-negation-ctype :type not-type)))))


;;;; Numeric types.

;;; A list of all the float formats, in order of decreasing precision.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant float-formats
    '(long-float double-float single-float short-float)))

;;; The type of a float format.
;;;
(deftype float-format () `(member ,@float-formats))

(defun type-bound-number (x)
  (if (consp x)
      (destructuring-bind (result) x result)
      x))

(defun make-numeric-ctype (&key class 
                                format
                                (complexp :real)
                                low
                                high
                                enumerable
                                predicate)
  ;; if interval is empty
  (if (and low
	   high
	   (if (or (consp low) (consp high)) ; if either bound is exclusive
	     (>= (type-bound-number low) (type-bound-number high))
	     (> low high)))
    *empty-type*
    (multiple-value-bind (canonical-low canonical-high)
	(case class
	  (integer
	   ;; INTEGER types always have their LOW and HIGH bounds
	   ;; represented as inclusive, not exclusive values.
	   (values (if (consp low)
		     (1+ (type-bound-number low))
		     low)
		   (if (consp high)
		     (1- (type-bound-number high))
		     high)))
	  (t 
	   ;; no canonicalization necessary
	   (values low high)))
      (when (and (eq class 'rational)
		 (integerp canonical-low)
		 (integerp canonical-high)
		 (= canonical-low canonical-high))
	(setf class 'integer))
      (%istruct 'numeric-ctype
		(type-class-or-lose 'number)
		enumerable
		class
		format
		complexp
		canonical-low
		canonical-high
		predicate))))
    

(defun make-numeric-ctype-predicate (ctype)
  (let ((class (numeric-ctype-class ctype))
        (lo (numeric-ctype-low ctype))
        (hi (numeric-ctype-high ctype)))
    (if (eq class 'integer)
      (if (and hi
               lo
               (<= hi target::target-most-positive-fixnum)
               (>= lo target::target-most-negative-fixnum))      
        #'(lambda (n)
            (and (fixnump n)
                 (locally (declare (fixnum n hi lo))
                   (and (%i>= n lo)
                        (%i<= n hi)))))))))

(defun numeric-ctype-p (x)
  (istruct-typep x 'numeric-ctype))

(setf (type-predicate 'numeric-ctype) 'numeric-ctype-p)

(define-type-method (number :simple-=) (type1 type2)
  (values
   (and (eq (numeric-ctype-class type1) (numeric-ctype-class type2))
	(eq (numeric-ctype-format type1) (numeric-ctype-format type2))
	(eq (numeric-ctype-complexp type1) (numeric-ctype-complexp type2))
	(equalp (numeric-ctype-low type1) (numeric-ctype-low type2))
	(equalp (numeric-ctype-high type1) (numeric-ctype-high type2)))
   t))

(define-type-method (number :unparse) (type)
  (let* ((complexp (numeric-ctype-complexp type))
	 (low (numeric-ctype-low type))
	 (high (numeric-ctype-high type))
	 (base (case (numeric-ctype-class type)
		 (integer 'integer)
		 (rational 'rational)
		 (float (or (numeric-ctype-format type) 'float))
		 (t 'real))))
    (let ((base+bounds
	   (cond ((and (eq base 'integer) high low)
		  (let ((high-count (logcount high))
			(high-length (integer-length high)))
		    (cond ((= low 0)
			   (cond ((= high 0) '(integer 0 0))
				 ((= high 1) 'bit)
				 ((and (= high-count high-length)
				       (plusp high-length))
				  `(unsigned-byte ,high-length))
				 (t
				  `(mod ,(1+ high)))))
			  ((and (= low target::target-most-negative-fixnum)
				(= high target::target-most-positive-fixnum))
			   'fixnum)
			  ((and (= low (lognot high))
				(= high-count high-length)
				(> high-count 0))
			   `(signed-byte ,(1+ high-length)))
			  (t
			   `(integer ,low ,high)))))
		 (high `(,base ,(or low '*) ,high))
		 (low
		  (if (and (eq base 'integer) (= low 0))
		      'unsigned-byte
		      `(,base ,low)))
		 (t base))))
      (ecase complexp
	(:real
	 base+bounds)
	(:complex
	 (if (eq base+bounds 'real)
	     'complex
	     `(complex ,base+bounds)))
	((nil)
	 (assert (eq base+bounds 'real))
	 'number)))))

;;; Numeric-Bound-Test  --  Internal
;;;
;;;    Return true if X is "less than or equal" to Y, taking open bounds into
;;; consideration.  Closed is the predicate used to test the bound on a closed
;;; interval (e.g. <=), and Open is the predicate used on open bounds (e.g. <).
;;; Y is considered to be the outside bound, in the sense that if it is
;;; infinite (NIL), then the test suceeds, whereas if X is infinite, then the
;;; test fails (unless Y is also infinite).
;;;
;;;    This is for comparing bounds of the same kind, e.g. upper and upper.
;;; Use Numeric-Bound-Test* for different kinds of bounds.
;;;
(defmacro numeric-bound-test (x y closed open)
  `(cond ((not ,y) t)
	   ((not ,x) nil)
	   ((consp ,x)
	    (if (consp ,y)
	      (,closed (car ,x) (car ,y))
	      (,closed (car ,x) ,y)))
	   (t
	    (if (consp ,y)
	      (,open ,x (car ,y))
	      (,closed ,x ,y)))))

;;; Numeric-Bound-Test*  --  Internal
;;;
;;;    Used to compare upper and lower bounds.  This is different from the
;;; same-bound case:
;;; -- Since X = NIL is -infinity, whereas y = NIL is +infinity, we return true
;;;    if *either* arg is NIL.
;;; -- an open inner bound is "greater" and also squeezes the interval, causing
;;;    us to use the Open test for those cases as well.
;;;
(defmacro numeric-bound-test* (x y closed open)
  `(cond ((not ,y) t)
         ((not ,x) t)
         ((consp ,x)
          (if (consp ,y)
	      (,open (car ,x) (car ,y))
	      (,open (car ,x) ,y)))
         (t
          (if (consp ,y)
	      (,open ,x (car ,y))
	      (,closed ,x ,y)))))

;;; Numeric-Bound-Max  --  Internal
;;;
;;;    Return whichever of the numeric bounds X and Y is "maximal" according to
;;; the predicates Closed (e.g. >=) and Open (e.g. >).  This is only meaningful
;;; for maximizing like bounds, i.e. upper and upper.  If Max-P is true, then
;;; we return NIL if X or Y is NIL, otherwise we return the other arg.
;;;
(defmacro numeric-bound-max (x y closed open max-p)
  (once-only ((n-x x)
	      (n-y y))
    `(cond
      ((not ,n-x) ,(if max-p nil n-y))
      ((not ,n-y) ,(if max-p nil n-x))
      ((consp ,n-x)
       (if (consp ,n-y)
	 (if (,closed (car ,n-x) (car ,n-y)) ,n-x ,n-y)
	 (if (,open (car ,n-x) ,n-y) ,n-x ,n-y)))
      (t
       (if (consp ,n-y)
	 (if (,open (car ,n-y) ,n-x) ,n-y ,n-x)
	 (if (,closed ,n-y ,n-x) ,n-y ,n-x))))))


(define-type-method (number :simple-subtypep) (type1 type2)
  (let ((class1 (numeric-ctype-class type1))
	  (class2 (numeric-ctype-class type2))
	  (complexp2 (numeric-ctype-complexp type2))
	  (format2 (numeric-ctype-format type2))
	  (low1 (numeric-ctype-low type1))
	  (high1 (numeric-ctype-high type1))
	  (low2 (numeric-ctype-low type2))
	  (high2 (numeric-ctype-high type2)))
    ;;
    ;; If one is complex and the other isn't, they are disjoint.
    (cond ((not (or (eq (numeric-ctype-complexp type1) complexp2)
		        (null complexp2)))
	     (values nil t))
	    ;;
	    ;; If the classes are specified and different, the types are
	    ;; disjoint unless type2 is rational and type1 is integer.
	    ((not (or (eq class1 class2) (null class2)
		        (and (eq class1 'integer) (eq class2 'rational))))
	     (values nil t))
	    ;;
	    ;; If the float formats are specified and different, the types
	    ;; are disjoint.
	    ((not (or (eq (numeric-ctype-format type1) format2)
		        (null format2)))
	     (values nil t))
	    ;;
	    ;; Check the bounds.
	    ((and (numeric-bound-test low1 low2 >= >)
		    (numeric-bound-test high1 high2 <= <))
	     (values t t))
	    (t
	     (values nil t)))))

;(define-superclasses number (generic-number))

;;; NUMERIC-TYPES-ADJACENT  --  Internal
;;;
;;;    If the high bound of Low is adjacent to the low bound of High, then
;;; return T, otherwise NIL.
;;;
(defun numeric-types-adjacent (low high)
  (let ((low-bound (numeric-ctype-high low))
	(high-bound (numeric-ctype-low high)))
    (cond ((not (and low-bound high-bound)) nil)
	    ((consp low-bound)
	     (eql (car low-bound) high-bound))
	    ((consp high-bound)
	     (eql (car high-bound) low-bound))
	    ((and (eq (numeric-ctype-class low) 'integer)
		    (eq (numeric-ctype-class high) 'integer))
	     (eql (1+ low-bound) high-bound))
	    (t
	     nil))))

;;;
;;; Return a numeric type that is a supertype for both type1 and type2.
;;; 
(define-type-method (number :simple-union) (type1 type2)
  (declare (type numeric-ctype type1 type2))
  (cond ((csubtypep type1 type2) type2)
	((csubtypep type2 type1) type1)
	(t
	 (let ((class1 (numeric-ctype-class type1))
	       (format1 (numeric-ctype-format type1))
	       (complexp1 (numeric-ctype-complexp type1))
	       (class2 (numeric-ctype-class type2))
	       (format2 (numeric-ctype-format type2))
	       (complexp2 (numeric-ctype-complexp type2)))
	   (cond
             ((and (eq class1 class2)
                   (eq format1 format2)
                   (eq complexp1 complexp2)
                   (or (numeric-types-intersect type1 type2)
                       (numeric-types-adjacent type1 type2)
                       (numeric-types-adjacent type2 type1)))
              (make-numeric-ctype
               :class class1
               :format format1
               :complexp complexp1
               :low (numeric-bound-max (numeric-ctype-low type1)
                                       (numeric-ctype-low type2)
                                       <= < t)
               :high (numeric-bound-max (numeric-ctype-high type1)
                                        (numeric-ctype-high type2)
                                        >= > t)))
             ;; FIXME: These two clauses are almost identical, and the
             ;; consequents are in fact identical in every respect.
             ((and (eq class1 'rational)
                   (eq class2 'integer)
                   (eq format1 format2)
                   (eq complexp1 complexp2)
                   (integerp (numeric-ctype-low type2))
                   (integerp (numeric-ctype-high type2))
                   (= (numeric-ctype-low type2) (numeric-ctype-high type2))
                   (or (numeric-types-adjacent type1 type2)
                       (numeric-types-adjacent type2 type1)))
              (make-numeric-ctype
               :class 'rational
               :format format1
               :complexp complexp1
               :low (numeric-bound-max (numeric-ctype-low type1)
                                       (numeric-ctype-low type2)
                                       <= < t)
               :high (numeric-bound-max (numeric-ctype-high type1)
                                        (numeric-ctype-high type2)
                                        >= > t)))
             ((and (eq class1 'integer)
                   (eq class2 'rational)
                   (eq format1 format2)
                   (eq complexp1 complexp2)
                   (integerp (numeric-ctype-low type1))
                   (integerp (numeric-ctype-high type1))
                   (= (numeric-ctype-low type1) (numeric-ctype-high type1))
                   (or (numeric-types-adjacent type1 type2)
                       (numeric-types-adjacent type2 type1)))
              (make-numeric-ctype
               :class 'rational
               :format format1
               :complexp complexp1
               :low (numeric-bound-max (numeric-ctype-low type1)
                                       (numeric-ctype-low type2)
                                       <= < t)
               :high (numeric-bound-max (numeric-ctype-high type1)
                                        (numeric-ctype-high type2)
                                        >= > t)))
             (t nil))))))

(setf (info-type-kind 'number) :primitive
      (info-type-builtin 'number) (make-numeric-ctype :complexp nil))

(def-type-translator complex (&optional spec &environment env)
  (if (eq spec '*)
      (make-numeric-ctype :complexp :complex)
      (labels ((not-numeric ()
                 (error "Component type for Complex is not numeric: ~S." spec))
               (not-real ()
                 (error "Component type for Complex is not a subtype of real: ~S." spec))
               (complex1 (component-type)
                 (unless (numeric-ctype-p component-type)
                   (not-numeric))
                 (when (eq (numeric-ctype-complexp component-type) :complex)
                   (not-real))
                 (let ((res (copy-uvector component-type)))
                   (setf (numeric-ctype-complexp res) :complex)
                   (setf (numeric-ctype-predicate res) nil) ; <<
                   res))
               (do-complex (ctype)
                 (cond
                   ((eq ctype *empty-type*) *empty-type*)
                   ((eq ctype *universal-type*) (not-real))
                   ((numeric-ctype-p ctype) (complex1 ctype))
                   ((union-ctype-p ctype)
                    (apply #'type-union
                           (mapcar #'do-complex (union-ctype-types ctype))))
                   ((member-ctype-p ctype)
                    (apply #'type-union
                           (mapcar (lambda (x) (do-complex (ctype-of x)))
                                   (member-ctype-members ctype))))
                   ((and (intersection-ctype-p ctype)
                         ;; just enough to handle simple types like RATIO.
                         (let ((numbers (remove-if-not
                                         #'numeric-ctype-p
                                         (intersection-ctype-types ctype))))
                           (and (car numbers)
                                (null (cdr numbers))
                                (eq (numeric-ctype-complexp (car numbers)) :real)
                                (complex1 (car numbers))))))
                   (t                   ; punt on harder stuff for now
                    (not-real)))))
        (let ((ctype (specifier-type spec env)))
          (do-complex ctype)))))

;;; Check-Bound  --  Internal
;;;
;;;    Check that X is a well-formed numeric bound of the specified Type.
;;; If X is *, return NIL, otherwise return the bound.
;;;
(defmacro check-bound (x type)
  `(cond ((eq ,x '*) nil)
	   ((or (typep ,x ',type)
	        (and (consp ,x) (typep (car ,x) ',type) (null (cdr ,x))))
	    ,x)
	   (t
	    (error "Bound is not *, a ~A or a list of a ~A: ~S" ',type ',type ,x))))

(def-type-translator integer (&optional low high)
  (let* ((l (check-bound low integer))
         (lb (if (consp l) (1+ (car l)) l))
         (h (check-bound high integer))
         (hb (if (consp h) (1- (car h)) h)))
    (if (and hb lb (< hb lb))
      *empty-type*
      (make-numeric-ctype :class 'integer  :complexp :real
                          :enumerable (not (null (and l h)))
                          :low lb
                          :high hb))))

(deftype mod (n)
  (unless (and (integerp n) (> n 0))
    (error "Bad N specified for MOD type specifier: ~S." n))
  `(integer 0 ,(1- n)))


(defmacro def-bounded-type (type class format)
  `(def-type-translator ,type (&optional low high)
     (let ((lb (check-bound low ,type))
	     (hb (check-bound high ,type)))
       (unless (numeric-bound-test* lb hb <= <)
	   (error "Lower bound ~S is not less than upper bound ~S." low high))
       (make-numeric-ctype :class ',class :format ',format :low lb :high hb))))

(def-bounded-type rational rational nil)

(defun coerce-bound (bound type inner-coerce-bound-fun)
  (declare (type function inner-coerce-bound-fun))
  (cond ((eql bound '*)
	 bound)
	((consp bound)
	 (destructuring-bind (inner-bound) bound
	   (list (funcall inner-coerce-bound-fun inner-bound type))))
	(t
	 (funcall inner-coerce-bound-fun bound type))))

(defun inner-coerce-real-bound (bound type)
  (ecase type
    (rational (rationalize bound))
    (float (if (floatp bound)
	       bound
	       ;; Coerce to the widest float format available, to
	       ;; avoid unnecessary loss of precision:
	       (coerce bound 'long-float)))))

(defun coerced-real-bound (bound type)
  (coerce-bound bound type #'inner-coerce-real-bound))

(defun coerced-float-bound (bound type)
  (coerce-bound bound type #'coerce))

(def-type-translator real (&optional (low '*) (high '*))
  (specifier-type `(or (float ,(coerced-real-bound  low 'float)
			      ,(coerced-real-bound high 'float))
		       (rational ,(coerced-real-bound  low 'rational)
				 ,(coerced-real-bound high 'rational)))))

(def-type-translator float (&optional (low '*) (high '*))
  (specifier-type 
   `(or (single-float ,(coerced-float-bound  low 'single-float)
		      ,(coerced-float-bound high 'single-float))
	(double-float ,(coerced-float-bound  low 'double-float)
		      ,(coerced-float-bound high 'double-float)))))

(def-bounded-type float float nil)
(def-bounded-type real nil nil)

(defmacro define-float-format (f)
  `(def-bounded-type ,f float ,f))

(define-float-format short-float)
(define-float-format single-float)
(define-float-format double-float)
(define-float-format long-float)

(defun numeric-types-intersect (type1 type2)
  (declare (type numeric-ctype type1 type2))
  (let* ((class1 (numeric-ctype-class type1))
	 (class2 (numeric-ctype-class type2))
	 (complexp1 (numeric-ctype-complexp type1))
	 (complexp2 (numeric-ctype-complexp type2))
	 (format1 (numeric-ctype-format type1))
	 (format2 (numeric-ctype-format type2))
	 (low1 (numeric-ctype-low type1))
	 (high1 (numeric-ctype-high type1))
	 (low2 (numeric-ctype-low type2))
	 (high2 (numeric-ctype-high type2)))
    ;;
    ;; If one is complex and the other isn't, then they are disjoint.
    (cond ((not (or (eq complexp1 complexp2)
		    (null complexp1) (null complexp2)))
	   nil)
	  ;;
	  ;; If either type is a float, then the other must either be specified
	  ;; to be a float or unspecified.  Otherwise, they are disjoint.
	  ((and (eq class1 'float) (not (member class2 '(float nil)))) nil)
	  ((and (eq class2 'float) (not (member class1 '(float nil)))) nil)
	  ;;
	  ;; If the float formats are specified and different, the types
	  ;; are disjoint.
	  ((not (or (eq format1 format2) (null format1) (null format2)))
	   nil)
	  (t
	   ;;
	   ;; Check the bounds.  This is a bit odd because we must always have
	   ;; the outer bound of the interval as the second arg.
	   (if (numeric-bound-test high1 high2 <= <)
	     (or (and (numeric-bound-test low1 low2 >= >)
		      (numeric-bound-test* low1 high2 <= <))
		 (and (numeric-bound-test low2 low1 >= >)
		      (numeric-bound-test* low2 high1 <= <)))
	     (or (and (numeric-bound-test* low2 high1 <= <)
		      (numeric-bound-test low2 low1 >= >))
		 (and (numeric-bound-test high2 high1 <= <)
		      (numeric-bound-test* high2 low1 >= >))))))))

;;; Round-Numeric-Bound  --  Internal
;;;
;;;    Take the numeric bound X and convert it into something that can be used
;;; as a bound in a numeric type with the specified Class and Format.  If up-p
;;; is true, then we round up as needed, otherwise we round down.  Up-p true
;;; implies that X is a lower bound, i.e. (N) > N.
;;;
;;; This is used by Numeric-Type-Intersection to mash the bound into the
;;; appropriate type number.  X may only be a float when Class is Float.
;;;
;;; ### Note: it is possible for the coercion to a float to overflow or
;;; underflow.  This happens when the bound doesn't fit in the specified
;;; format.  In this case, we should really return the appropriate
;;; {Most | Least}-{Positive | Negative}-XXX-Float float of desired format.
;;; But these conditions aren't currently signalled in any useful way.
;;;
;;; Also, when converting an open rational bound into a float we should
;;; probably convert it to a closed bound of the closest float in the specified
;;; format.  In general, open float bounds are fucked.
;;;
(defun round-numeric-bound (x class format up-p)
  (if x
    (let ((cx (if (consp x) (car x) x)))
	(ecase class
	  ((nil rational) x)
	  (integer
	   (if (and (consp x) (integerp cx))
	     (if up-p (1+ cx) (1- cx))
	     (if up-p (ceiling cx) (floor cx))))
	  (float
	   (let ((res (if format (coerce cx format) (float cx))))
	     (if (consp x) (list res) res)))))
    nil))

;;; Number :Simple-Intersection type method  --  Internal
;;;
;;;    Handle the case of Type-Intersection on two numeric types.  We use
;;; Types-Intersect to throw out the case of types with no intersection.  If an
;;; attribute in Type1 is unspecified, then we use Type2's attribute, which
;;; must be at least as restrictive.  If the types intersect, then the only
;;; attributes that can be specified and different are the class and the
;;; bounds.
;;;
;;;    When the class differs, we use the more restrictive class.  The only
;;; interesting case is rational/integer, since rational includes integer.
;;;
;;;    We make the result lower (upper) bound the maximum (minimum) of the
;;; argument lower (upper) bounds.  We convert the bounds into the
;;; appropriate numeric type before maximizing.  This avoids possible confusion
;;; due to mixed-type comparisons (but I think the result is the same).
;;;
(define-type-method (number :simple-intersection) (type1 type2)
  (declare (type numeric-type type1 type2))
  (if (numeric-types-intersect type1 type2)
    (let* ((class1 (numeric-ctype-class type1))
	   (class2 (numeric-ctype-class type2))
	   (class (ecase class1
		    ((nil) class2)
		    ((integer float) class1)
		    (rational (if (eq class2 'integer) 'integer 'rational))))
	   (format (or (numeric-ctype-format type1)
		       (numeric-ctype-format type2))))
      (make-numeric-ctype
       :class class
       :format format
       :complexp (or (numeric-ctype-complexp type1)
		     (numeric-ctype-complexp type2))
       :low (numeric-bound-max
	     (round-numeric-bound (numeric-ctype-low type1)
				  class format t)
	     (round-numeric-bound (numeric-ctype-low type2)
				  class format t)
	     > >= nil)
       :high (numeric-bound-max
	      (round-numeric-bound (numeric-ctype-high type1)
				   class format nil)
	      (round-numeric-bound (numeric-ctype-high type2)
				   class format nil)
	      < <= nil)))
    *empty-type*))

;;; Float-Format-Max  --  Interface
;;;
;;;    Given two float formats, return the one with more precision.  If either
;;; one is null, return NIL.
;;;
(defun float-format-max (f1 f2)
  (when (and f1 f2)
    (dolist (f float-formats (error "Bad float format: ~S." f1))
      (when (or (eq f f1) (eq f f2))
	  (return f)))))


;;; Numeric-Contagion  --  Interface
;;;
;;;    Return the result of an operation on Type1 and Type2 according to the
;;; rules of numeric contagion.  This is always NUMBER, some float format
;;; (possibly complex) or RATIONAL.  Due to rational canonicalization, there
;;; isn't much we can do here with integers or rational complex numbers.
;;;
;;;    If either argument is not a Numeric-Type, then return NUMBER.  This is
;;; useful mainly for allowing types that are technically numbers, but not a
;;; Numeric-Type. 
;;;
(defun numeric-contagion (type1 type2)
  (if (and (numeric-ctype-p type1) (numeric-ctype-p type2))
    (let ((class1 (numeric-ctype-class type1))
	    (class2 (numeric-ctype-class type2))
	    (format1 (numeric-ctype-format type1))
	    (format2 (numeric-ctype-format type2))
	    (complexp1 (numeric-ctype-complexp type1))
	    (complexp2 (numeric-ctype-complexp type2)))
	(cond ((or (null complexp1)
		   (null complexp2))
	       (specifier-type 'number))
	      ((eq class1 'float)
	       (make-numeric-ctype
		  :class 'float
		  :format (ecase class2
			      (float (float-format-max format1 format2))
			      ((integer rational) format1)
			      ((nil)
			       ;; A double-float with any real number is a
			       ;; double-float.
			       (if (eq format1 'double-float)
				 'double-float
				 nil)))
		  :complexp (if (or (eq complexp1 :complex)
				    (eq complexp2 :complex))
			      :complex
			      :real)))
	      ((eq class2 'float) (numeric-contagion type2 type1))
	      ((and (eq complexp1 :real) (eq complexp2 :real))
	       (make-numeric-ctype
		  :class (and class1 class2 'rational)
		  :complexp :real))
	      (t
	       (specifier-type 'number))))
    (specifier-type 'number)))




;;;; Array types:

;;; The Array-Type is used to represent all array types, including things such
;;; as SIMPLE-STRING.
;;;

(defun make-array-ctype (&key
                         (dimensions '*)
                         (complexp '*)
                         element-type
                         (specialized-element-type *wild-type*))
  (%istruct 'array-ctype
            (type-class-or-lose 'array)
            nil
            dimensions
            complexp
            element-type
            specialized-element-type
            (unless (eq specialized-element-type *wild-type*)
              (ctype-subtype specialized-element-type))))

(defun array-ctype-p (x) (istruct-typep x 'array-ctype))
(setf (type-predicate 'array-ctype) 'array-ctype-p)

;;; Specialized-Element-Type-Maybe  --  Internal
;;;
;;;      What this does depends on the setting of the
;;; *use-implementation-types* switch.  If true, return the specialized element
;;; type, otherwise return the original element type.
;;;
(defun specialized-element-type-maybe (type)
  (declare (type array-ctype type))
  (if *use-implementation-types*
    (array-ctype-specialized-element-type type)
    (array-ctype-element-type type)))

(define-type-method (array :simple-=) (type1 type2)
  (if (or (unknown-ctype-p (array-ctype-element-type type1))
	  (unknown-ctype-p (array-ctype-element-type type2)))
    (multiple-value-bind (equalp certainp)
	(type= (array-ctype-element-type type1)
	       (array-ctype-element-type type2))
      (assert (not (and (not equalp) certainp)))
      (values equalp certainp))
    (values (and (equal (array-ctype-dimensions type1)
			(array-ctype-dimensions type2))
		 (eq (array-ctype-complexp type1)
		     (array-ctype-complexp type2))
		 (type= (specialized-element-type-maybe type1)
			(specialized-element-type-maybe type2)))
	    t)))

(define-type-method (array :unparse) (type)
  (let ((dims (array-ctype-dimensions type))
	  (eltype (type-specifier (array-ctype-element-type type)))
	  (complexp (array-ctype-complexp type)))
    (cond ((eq dims '*)
	     (if (eq eltype '*)
	       (if complexp 'array 'simple-array)
	       (if complexp `(array ,eltype) `(simple-array ,eltype))))
	    ((= (length dims) 1) 
	     (if complexp
	       (if (eq (car dims) '*)
		   (case eltype
		     (bit 'bit-vector)
		     ((character base-char) 'base-string)
		     (* 'vector)
		     (t `(vector ,eltype)))
		   (case eltype
		     (bit `(bit-vector ,(car dims)))
		     ((character base-char) `(base-string ,(car dims)))
		     (t `(vector ,eltype ,(car dims)))))
	       (if (eq (car dims) '*)
		   (case eltype
		     (bit 'simple-bit-vector)
		     ((base-char character) 'simple-base-string)
		     ((t) 'simple-vector)
		     (t `(simple-array ,eltype (*))))
		   (case eltype
		     (bit `(simple-bit-vector ,(car dims)))
		     ((base-char character) `(simple-base-string ,(car dims)))
		     ((t) `(simple-vector ,(car dims)))
		     (t `(simple-array ,eltype ,dims))))))
	    (t
	     (if complexp
	       `(array ,eltype ,dims)
	       `(simple-array ,eltype ,dims))))))

(define-type-method (array :simple-subtypep) (type1 type2)
  (let ((dims1 (array-ctype-dimensions type1))
	(dims2 (array-ctype-dimensions type2))
	(complexp2 (array-ctype-complexp type2)))
    (cond (;; not subtypep unless dimensions are compatible
	   (not (or (eq dims2 '*)
		    (and (not (eq dims1 '*))
			 (= (length (the list dims1))
			    (length (the list dims2)))
			 (every (lambda (x y)
				  (or (eq y '*) (eql x y)))
				(the list dims1)
				(the list dims2)))))
	   (values nil t))
	  ;; not subtypep unless complexness is compatible
	  ((not (or (eq complexp2 :maybe)
		    (eq (array-ctype-complexp type1) complexp2)))
	   (values nil t))
	  ;; Since we didn't fail any of the tests above, we win
	  ;; if the TYPE2 element type is wild.
	  ((eq (array-ctype-element-type type2) *wild-type*)
	   (values t t))
	  (;; Since we didn't match any of the special cases above, we
	   ;; can't give a good answer unless both the element types
	   ;; have been defined.
	   (or (unknown-ctype-p (array-ctype-element-type type1))
	       (unknown-ctype-p (array-ctype-element-type type2)))
	   (values nil nil))
	  (;; Otherwise, the subtype relationship holds iff the
	   ;; types are equal, and they're equal iff the specialized
	   ;; element types are identical.
	   t
	   (values (type= (specialized-element-type-maybe type1)
			  (specialized-element-type-maybe type2))
		   t)))))

; (define-superclasses array (string string) (vector vector) (array))


(defun array-types-intersect (type1 type2)
  (declare (type array-ctype type1 type2))
  (let ((dims1 (array-ctype-dimensions type1))
	(dims2 (array-ctype-dimensions type2))
	(complexp1 (array-ctype-complexp type1))
	(complexp2 (array-ctype-complexp type2)))
    ;; See whether dimensions are compatible.
    (cond ((not (or (eq dims1 '*) (eq dims2 '*)
		    (and (= (length dims1) (length dims2))
			 (every (lambda (x y)
				  (or (eq x '*) (eq y '*) (= x y)))
				dims1 dims2))))
	   (values nil t))
	  ;; See whether complexpness is compatible.
	  ((not (or (eq complexp1 :maybe)
		    (eq complexp2 :maybe)
		    (eq complexp1 complexp2)))
	   (values nil t))
	  ((or (eq (array-ctype-specialized-element-type type1) *wild-type*)
	       (eq (array-ctype-specialized-element-type type2) *wild-type*)
	       (type= (specialized-element-type-maybe type1)
		      (specialized-element-type-maybe type2)))
	   (values t t))
	  (t
	   (values nil t)))))

(define-type-method (array :simple-intersection) (type1 type2)
  (declare (type array-ctype type1 type2))
  (if (array-types-intersect type1 type2)
    (let ((dims1 (array-ctype-dimensions type1))
          (dims2 (array-ctype-dimensions type2))
          (complexp1 (array-ctype-complexp type1))
          (complexp2 (array-ctype-complexp type2))
          (eltype1 (array-ctype-element-type type1))
          (eltype2 (array-ctype-element-type type2)))
      (specialize-array-type
       (make-array-ctype
        :dimensions (cond ((eq dims1 '*) dims2)
                          ((eq dims2 '*) dims1)
                          (t
                           (mapcar #'(lambda (x y) (if (eq x '*) y x))
                                   dims1 dims2)))
        :complexp (if (eq complexp1 :maybe) complexp2 complexp1)
        :element-type (cond
                        ((eq eltype1 *wild-type*) eltype2)
                        ((eq eltype2 *wild-type*) eltype1)
                        (t (type-intersection eltype1 eltype2))))))
      *empty-type*))

;;; Check-Array-Dimensions  --  Internal
;;;
;;;    Check a supplied dimension list to determine if it is legal.
;;;
(defun check-array-dimensions (dims)
  (typecase dims
    ((member *) dims)
    (integer
     (when (minusp dims)
       (signal-program-error "Arrays can't have a negative number of dimensions: ~D." dims))
     (when (>= dims array-rank-limit)
       (signal-program-error "Array type has too many dimensions: ~S." dims))
     (make-list dims :initial-element '*))
    (list
     (when (>= (length dims) array-rank-limit)
       (signal-program-error "Array type has too many dimensions: ~S." dims))
     (dolist (dim dims)
       (unless (eq dim '*)
	   (unless (and (integerp dim)
		          (>= dim 0) (< dim array-dimension-limit))
	     (signal-program-error "Bad dimension in array type: ~S." dim))))
     dims)
    (t
     (signal-program-error "Array dimensions is not a list, integer or *:~%  ~S"
			   dims))))

(def-type-translator array (&optional element-type dimensions &environment env)
  (specialize-array-type
   (make-array-ctype :dimensions (check-array-dimensions dimensions)
		     :complexp :maybe
		     :element-type (specifier-type element-type env))))

(def-type-translator simple-array (&optional element-type dimensions &environment env)
  (specialize-array-type
   (make-array-ctype :dimensions (check-array-dimensions dimensions)
		         :element-type (specifier-type element-type env)
		         :complexp nil)))

;;; Order matters here.
(defparameter specialized-array-element-types
  '(nil bit (unsigned-byte 8) (signed-byte 8) (unsigned-byte 16)
    (signed-byte 16) (unsigned-byte 32) #+32-bit-target fixnum (signed-byte 32)
    #+64-bit-target (unsigned-byte 64)
    #+64-bit-target fixnum
    #+64-bit-target (signed-byte 64)
    character  short-float double-float))

(defun specialize-array-type (type)
  (let* ((eltype (array-ctype-element-type type))
         (specialized-type (if (eq eltype *wild-type*)
                             *wild-type*
                             (dolist (stype-name specialized-array-element-types
                                      *universal-type*)
                               (let ((stype (specifier-type stype-name)))
                                 (when (csubtypep eltype stype)
                                   (return stype)))))))
    
    (setf (array-ctype-specialized-element-type type) specialized-type
          (array-ctype-typecode type) (unless (eq specialized-type *wild-type*)
                                        (ctype-subtype specialized-type)))
    type))


;;;; Member types.

;;; The Member-Type represents uses of the MEMBER type specifier.  We bother
;;; with this at this level because MEMBER types are fairly important and union
;;; and intersection are well defined.

(defun %make-member-ctype (members)
  (%istruct 'member-ctype
            (type-class-or-lose 'member)
            t
            members))

(defun make-member-ctype (&key members)
  (let* ((singlep (subsetp '(-0.0f0 0.0f0) members))
	 (doublep (subsetp '(-0.0d0 0.0d0) members))
	 (union-types
	  (if singlep
	    (if doublep
	      (list *ctype-of-single-float-0* *ctype-of-double-float-0*)
	      (list *ctype-of-single-float-0*))
	    (if doublep
	      (list *ctype-of-double-float-0*)))))
    (if union-types
      (progn
	(if singlep
	  (setq members (set-difference '(-0.0f0 0.0f0) members)))
	(if doublep
	  (setq members (set-difference '(-0.d00 0.0d0) members)))
	(make-union-ctype (if (null members)
			    union-types
			    (cons (%make-member-ctype members) union-types))))
      (%make-member-ctype members))))
	

(defun member-ctype-p (x) (istruct-typep x 'member-ctype))
(setf (type-predicate 'member-ctype) 'member-ctype-p)

(define-type-method (member :unparse) (type)
  (if (type= type (specifier-type 'standard-char))
    'standard-char
    (let ((members (member-ctype-members type)))
      (if (equal members '(nil))
	'null
	`(member ,@members)))))

(define-type-method (member :simple-subtypep) (type1 type2)
  (values (subsetp (member-ctype-members type1) (member-ctype-members type2))
	    t))


(define-type-method (member :complex-subtypep-arg1) (type1 type2)
  (every/type (swapped-args-fun #'ctypep)
	      type2
	      (member-ctype-members type1)))

;;; We punt if the odd type is enumerable and intersects with the member type.
;;; If not enumerable, then it is definitely not a subtype of the member type.
;;;
(define-type-method (member :complex-subtypep-arg2) (type1 type2)
  (cond ((not (ctype-enumerable type1)) (values nil t))
	  ((types-intersect type1 type2)
	   (invoke-complex-subtypep-arg1-method type1 type2))
	  (t
	   (values nil t))))

(define-type-method (member :simple-intersection) (type1 type2)
  (let ((mem1 (member-ctype-members type1))
	(mem2 (member-ctype-members type2)))
    (values (cond ((subsetp mem1 mem2) type1)
		  ((subsetp mem2 mem1) type2)
		  (t
		   (let ((res (intersection mem1 mem2)))
		     (if res
		       (make-member-ctype :members res)
		       *empty-type*))))
	    t)))

(define-type-method (member :complex-intersection) (type1 type2)
  (block PUNT
    (collect ((members))
      (let ((mem2 (member-ctype-members type2)))
        (dolist (member mem2)
	  (multiple-value-bind (val win) (ctypep member type1)
	    (unless win
	      (return-from punt nil))
	    (when val (members member))))
	(cond ((subsetp mem2 (members)) type2)
	      ((null (members)) *empty-type*)
	      (t
	       (make-member-ctype :members (members))))))))

;;; We don't need a :COMPLEX-UNION, since the only interesting case is a union
;;; type, and the member/union interaction is handled by the union type
;;; method.
(define-type-method (member :simple-union) (type1 type2)
  (let ((mem1 (member-ctype-members type1))
	(mem2 (member-ctype-members type2)))
    (cond ((subsetp mem1 mem2) type2)
	  ((subsetp mem2 mem1) type1)
	  (t
	   (make-member-ctype :members (union mem1 mem2))))))


(define-type-method (member :simple-=) (type1 type2)
  (let ((mem1 (member-ctype-members type1))
	(mem2 (member-ctype-members type2)))
    (values (and (subsetp mem1 mem2) (subsetp mem2 mem1))
	    t)))

(define-type-method (member :complex-=) (type1 type2)
  (if (ctype-enumerable type1)
    (multiple-value-bind (val win)
			       (csubtypep type2 type1)
	(if (or val (not win))
        (values nil nil)
        (values nil t)))
    (values nil t)))

(def-type-translator member (&rest members)
  (if members
    (collect ((non-numbers) (numbers))
      (dolist (m (remove-duplicates members))
	(if (and (numberp m)
		 (not (and (floatp m) (zerop m))))
	  (numbers (ctype-of m))
	  (non-numbers m)))
      (apply #'type-union
	     (if (non-numbers)
	       (make-member-ctype :members (non-numbers))
	       *empty-type*)
	     (numbers)))
    *empty-type*))



;;;; Union types:

;;; The Union-Type represents uses of the OR type specifier which can't be
;;; canonicalized to something simpler.  Canonical form:
;;;
;;; 1] There is never more than one Member-Type component.
;;; 2] There are never any Union-Type components.
;;;

(defun make-union-ctype (types)
  (declare (list types))
  (%istruct 'union-ctype
            (type-class-or-lose 'union)
            (every #'(lambda (x) (ctype-enumerable x)) types)
            types))

(defun union-ctype-p (x) (istruct-typep x 'union-ctype))
(setf (type-predicate 'union-ctype) 'union-ctype-p)


;;;    If List, then return that, otherwise the OR of the component types.
;;;
(define-type-method (union :unparse) (type)
  (declare (type ctype type))
    (cond
      ((type= type (specifier-type 'list)) 'list)
      ((type= type (specifier-type 'float)) 'float)
      ((type= type (specifier-type 'real)) 'real)
      ((type= type (specifier-type 'sequence)) 'sequence)
      ((type= type (specifier-type 'bignum)) 'bignum)
      (t `(or ,@(mapcar #'type-specifier (union-ctype-types type))))))



(define-type-method (union :simple-=) (type1 type2)
  (multiple-value-bind (subtype certain?)
      (csubtypep type1 type2)
    (if subtype
      (csubtypep type2 type1)
      (if certain?
	(values nil t)
	(multiple-value-bind (subtype certain?)
	    (csubtypep type2 type1)
	  (declare (ignore subtype))
	  (values nil certain?))))))


(define-type-method (union :complex-=) (type1 type2)
  (declare (ignore type1))
  (if (some #'type-might-contain-other-types-p 
	    (union-ctype-types type2))
    (values nil nil)
    (values nil t)))


(defun union-simple-subtypep (type1 type2)
  (every/type (swapped-args-fun #'union-complex-subtypep-arg2)
	      type2
	      (union-ctype-types type1)))

(define-type-method (union :simple-subtypep) (type1 type2)
  (union-simple-subtypep type1 type2))

(defun union-complex-subtypep-arg1 (type1 type2)
  (every/type (swapped-args-fun #'csubtypep)
	      type2
	      (union-ctype-types type1)))

(define-type-method (union :complex-subtypep-arg1) (type1 type2)
  (union-complex-subtypep-arg1 type1 type2))

(defun union-complex-subtypep-arg2 (type1 type2)
  (multiple-value-bind (sub-value sub-certain?)
      (progn
	(assert (union-ctype-p type2))
	(assert (not (union-ctype-p type1)))
	(type= type1
	       (apply #'type-union
		      (mapcar (lambda (x) (type-intersection type1 x))
			      (union-ctype-types type2)))))
    (if sub-certain?
      (values sub-value sub-certain?)
      (invoke-complex-subtypep-arg1-method type1 type2))))

(define-type-method (union :complex-subtypep-arg2) (type1 type2)
  (union-complex-subtypep-arg2 type1 type2))

(define-type-method (union :simple-intersection :complex-intersection)
    (type1 type2)
  (assert (union-ctype-p type2))
  (cond ((and (union-ctype-p type1)
	      (union-simple-subtypep type1 type2)) type1)
	((and (union-ctype-p type1)
	      (union-simple-subtypep type2 type1)) type2)
	((and (not (union-ctype-p type1))
	      (union-complex-subtypep-arg2 type1 type2))
	 type1)
	((and (not (union-ctype-p type1))
	      (union-complex-subtypep-arg1 type2 type1))
	 type2)
	(t 
	 (let ((accumulator *empty-type*))
	   (dolist (t2 (union-ctype-types type2) accumulator)
	     (setf accumulator
		   (type-union accumulator
			       (type-intersection type1 t2))))))))



(def-type-translator or (&rest type-specifiers &environment env)
  (apply #'type-union
	 (mapcar #'(lambda (spec) (specifier-type spec env)) type-specifiers)))


;;; Intersection types
(defun make-intersection-ctype (enumerable types)
  (%istruct 'intersection-ctype
	    (type-class-or-lose 'intersection)
	    enumerable
	    types))

(defun intersection-ctype-p (x)
  (istruct-typep x 'intersection-ctype))
(setf (type-predicate 'intersection-ctype) 'intersection-ctype-p)

(define-type-method (intersection :unparse) (type)
  (declare (type ctype type))
  (or (find type '(ratio keyword) :key #'specifier-type :test #'type=)
      `(and ,@(mapcar #'type-specifier (intersection-ctype-types type)))))

;;; shared machinery for type equality: true if every type in the set
;;; TYPES1 matches a type in the set TYPES2 and vice versa
(defun type=-set (types1 types2)
  (flet (;; true if every type in the set X matches a type in the set Y
	 (type<=-set (x y)
	   (declare (type list x y))
	   (every (lambda (xelement)
		    (position xelement y :test #'type=))
		  x)))
    (values (and (type<=-set types1 types2)
		 (type<=-set types2 types1))
	    t)))

(define-type-method (intersection :simple-=) (type1 type2)
  (type=-set (intersection-ctype-types type1)
	     (intersection-ctype-types type2)))

(defun %intersection-complex-subtypep-arg1 (type1 type2)
  (type= type1 (type-intersection type1 type2)))

(defun %intersection-simple-subtypep (type1 type2)
  (every/type #'%intersection-complex-subtypep-arg1
	      type1
	      (intersection-ctype-types type2)))

(define-type-method (intersection :simple-subtypep) (type1 type2)
  (%intersection-simple-subtypep type1 type2))
  
(define-type-method (intersection :complex-subtypep-arg1) (type1 type2)
  (%intersection-complex-subtypep-arg1 type1 type2))

(defun %intersection-complex-subtypep-arg2 (type1 type2)
  (every/type #'csubtypep type1 (intersection-ctype-types type2)))

(define-type-method (intersection :complex-subtypep-arg2) (type1 type2)
  (%intersection-complex-subtypep-arg2 type1 type2))

(define-type-method (intersection :simple-union :complex-union)
    (type1 type2)
  (assert (intersection-ctype-p type2))
  (cond ((and (intersection-ctype-p type1)
	      (%intersection-simple-subtypep type1 type2)) type2)
	((and (intersection-ctype-p type1)
	      (%intersection-simple-subtypep type2 type1)) type1)
	((and (not (intersection-ctype-p type1))
	      (%intersection-complex-subtypep-arg2 type1 type2))
	 type2)
	((and (not (intersection-ctype-p type1))
	      (%intersection-complex-subtypep-arg1 type2 type1))
	 type1)
	((and (csubtypep type2 (specifier-type 'ratio))
	      (numeric-ctype-p type1)
	      (csubtypep type1 (specifier-type 'integer))
	      (csubtypep type2
			 (make-numeric-ctype
			  :class 'rational
			  :complexp nil
			  :low (if (null (numeric-ctype-low type1))
				 nil
				 (list (1- (numeric-ctype-low type1))))
			  :high (if (null (numeric-ctype-high type1))
				  nil
				  (list (1+ (numeric-ctype-high type1)))))))
	 (type-union type1
		     (apply #'type-intersection
			    (remove (specifier-type '(not integer))
				    (intersection-ctype-types type2)
				    :test #'type=))))
	(t
	 (let ((accumulator *universal-type*))
	   (do ((t2s (intersection-ctype-types type2) (cdr t2s)))
	       ((null t2s) accumulator)
	     (let ((union (type-union type1 (car t2s))))
	       (when (union-ctype-p union)
		 (if (and (eq accumulator *universal-type*)
			  (null (cdr t2s)))
		     (return union)
		     (return nil)))
	       (setf accumulator
		     (type-intersection accumulator union))))))))

(def-type-translator and (&rest type-specifiers &environment env)
  (apply #'type-intersection
	 (mapcar #'(lambda (spec) (specifier-type spec env))
		 type-specifiers)))

;;; cons-ctype
(defun wild-ctype-to-universal-ctype (c)
  (if (type= c *wild-type*)
    *universal-type*
    c))

(defun make-cons-ctype (car-ctype-value cdr-ctype-value)
  (if (or (eq car-ctype-value *empty-type*)
	  (eq cdr-ctype-value *empty-type*))
    *empty-type*
    (%istruct 'cons-ctype
	      (type-class-or-lose 'cons)
	      nil
	      (wild-ctype-to-universal-ctype car-ctype-value)
	      (wild-ctype-to-universal-ctype cdr-ctype-value))))

(defun cons-ctype-p (x)
  (istruct-typep x 'cons-ctype))

(setf (type-predicate 'cons-ctype) 'cons-ctype-p)
  
(def-type-translator cons (&optional (car-type-spec '*) (cdr-type-spec '*) &environment env)
  (make-cons-ctype (specifier-type car-type-spec env)
                   (specifier-type cdr-type-spec env)))

(define-type-method (cons :unparse) (type)
  (let* ((car-spec (type-specifier (cons-ctype-car-ctype type)))
         (cdr-spec (type-specifier (cons-ctype-cdr-ctype type))))
    (if (and (member car-spec '(t *))
             (member cdr-spec '(t *)))
      'cons
      `(cons ,car-spec ,cdr-spec))))

(define-type-method (cons :simple-=) (type1 type2)
  (declare (cons-ctype type1 type2))
  (and (type= (cons-ctype-car-ctype type1) (cons-ctype-car-ctype type2))
       (type= (cons-ctype-cdr-ctype type1) (cons-ctype-cdr-ctype type2))))

(define-type-method (cons :simple-subtypep) (type1 type2)
  (declare (cons-ctype type1 type2))
  (multiple-value-bind (val-car win-car)
      (csubtypep (cons-ctype-car-ctype type1) (cons-ctype-car-ctype type2))
    (multiple-value-bind (val-cdr win-cdr)
	(csubtypep (cons-ctype-cdr-ctype type1) (cons-ctype-cdr-ctype type2))
      (if (and val-car val-cdr)
	(values t (and win-car win-cdr))
	(values nil (or win-car win-cdr))))))

(define-type-method (cons :simple-union) (type1 type2)
  (declare (type cons-ctype type1 type2))
  (let ((car-type1 (cons-ctype-car-ctype type1))
	(car-type2 (cons-ctype-car-ctype type2))
	(cdr-type1 (cons-ctype-cdr-ctype type1))
	(cdr-type2 (cons-ctype-cdr-ctype type2))
        (car-not1)
        (car-not2))
    (macrolet ((frob-car (car1 car2 cdr1 cdr2
                          &optional (not1 nil not1p))
		 `(type-union
		   (make-cons-ctype ,car1 (type-union ,cdr1 ,cdr2))
		   (make-cons-ctype
		    (type-intersection
                     ,car2
                     ,(if not1p
                          not1
                          `(specifier-type
                            `(not ,(type-specifier ,car1))))) 
		    ,cdr2))))
      (cond ((type= car-type1 car-type2)
	     (make-cons-ctype car-type1
                              (type-union cdr-type1 cdr-type2)))
	    ((type= cdr-type1 cdr-type2)
	     (make-cons-ctype (type-union car-type1 car-type2)
			      cdr-type1))
	    ((csubtypep car-type1 car-type2)
	     (frob-car car-type1 car-type2 cdr-type1 cdr-type2))
	    ((csubtypep car-type2 car-type1)
	     (frob-car car-type2 car-type1 cdr-type2 cdr-type1))
            ;; more general case of the above, but harder to compute
            ((progn
               (setf car-not1 (specifier-type
                               `(not ,(type-specifier car-type1))))
               (not (csubtypep car-type2 car-not1)))
             (frob-car car-type1 car-type2 cdr-type1 cdr-type2 car-not1))
            ((progn
               (setf car-not2 (specifier-type
                               `(not ,(type-specifier car-type2))))
               (not (csubtypep car-type1 car-not2)))
             (frob-car car-type2 car-type1 cdr-type2 cdr-type1 car-not2))))))
	    
(define-type-method (cons :simple-intersection) (type1 type2)
  (declare (type cons-type type1 type2))
  (let ((car-int2 (type-intersection2 (cons-ctype-car-ctype type1)
				      (cons-ctype-car-ctype type2)))
	(cdr-int2 (type-intersection2 (cons-ctype-cdr-ctype type1)
				      (cons-ctype-cdr-ctype type2))))
    (cond ((and car-int2 cdr-int2)
	   (make-cons-ctype car-int2 cdr-int2))
	  (car-int2
	   (make-cons-ctype car-int2
			    (type-intersection (cons-ctype-cdr-ctype type1)
					       (cons-ctype-cdr-ctype type2))))
	  (cdr-int2
	   (make-cons-ctype (type-intersection (cons-ctype-car-ctype type1)
					       (cons-ctype-car-ctype type2))
			    cdr-int2)))))


;;; An UNKNOWN-TYPE is a type not known to the type system (not yet defined).
;;; We make this distinction since we don't want to complain about types that
;;; are hairy but defined.
;;;

(defun make-unknown-ctype (&key specifier (enumerable t))
  (%istruct 'unknown-ctype
            (type-class-or-lose 'hairy)
            enumerable
            specifier))

(defun unknown-ctype-p (x)
  (istruct-typep x 'unknown-ctype))

(setf (type-predicate 'unknown-ctype) 'unknown-ctype-p)





;;;; foreign-type types


(defun %make-foreign-ctype (foreign-type)
  (%istruct 'foreign-ctype
            (type-class-or-lose 'foreign)
            nil
            foreign-type))

(defun foreign-ctype-p (x) (istruct-typep x 'foreign-ctype))
(setf (type-predicate 'foreign-ctype) 'foreign-ctype-p)

(define-type-method (foreign :unparse) (type)
  `(foreign ,(unparse-foreign-type (foreign-ctype-foreign-type type))))

(define-type-method (foreign :simple-subtypep) (type1 type2)
  (values (foreign-subtype-p (foreign-ctype-foreign-type type1)
			           (foreign-ctype-foreign-type type2))
	    t))

;(define-superclasses foreign (foreign-value))

(define-type-method (foreign :simple-=) (type1 type2)
  (let ((foreign-type-1 (foreign-ctype-foreign-type type1))
	  (foreign-type-2 (foreign-ctype-foreign-type type2)))
    (values (or (eq foreign-type-1 foreign-type-2)
		    (foreign-type-= foreign-type-1 foreign-type-2))
	      t)))

(def-type-translator foreign (&optional (foreign-type nil))
  (typecase foreign-type
    (null
     (make-foreign-ctype))
    (foreign-type
     (make-foreign-ctype foreign-type))
    (t
     (make-foreign-ctype (parse-foreign-type foreign-type)))))

(defun make-foreign-ctype (&optional foreign-type)
  (if foreign-type
      (let ((lisp-rep-type (compute-lisp-rep-type foreign-type)))
	(if lisp-rep-type
	    (specifier-type lisp-rep-type)
	    (%make-foreign-ctype foreign-type)))
      *universal-type*))


;;; CLASS-CTYPES are supposed to help integrate CLOS and the CMU type system.
;;; They mostly just contain a backpointer to the CLOS class; the CPL is then
;;;  used to resolve type relationships.

(defun class-ctype-p (x) (istruct-typep x 'class-ctype))
(setf (type-predicate 'class-ctype) 'class-ctype-p)

(defun args-ctype-p (x) (and (eql (typecode x) target::subtag-istruct)
                             (member (istruct-type-name x)
                                     '(args-ctype values-ctype function-ctype))))

(setf (type-predicate 'args-ctype) 'args-ctype-p
      (type-predicate 'function-ctype) 'function-ctype-p
      (type-predicate 'values-ctype) 'values-ctype-p)


;;; Simple methods for TYPE= and SUBTYPEP should never be called when the two
;;; classes are equal, since there are EQ checks in those operations.
;;;
(define-type-method (class :simple-=) (type1 type2)
  (assert (not (eq type1 type2)))
  (values nil t))

(define-type-method (class :simple-subtypep) (type1 type2)
  (assert (not (eq type1 type2)))
  (let* ((class1 (if (class-ctype-p type1) (class-ctype-class type1)))
         (class2 (if (class-ctype-p type2) (class-ctype-class type2))))
    (if (and class1 class2)
      (let* ((ordinal2 (%class-ordinal class2))
             (wrapper1 (%class.own-wrapper class1))
             (bits1 (if wrapper1 (%wrapper-cpl-bits wrapper1))))
        (if bits1
          (locally (declare (simple-bit-vector bits1)
                            (optimize (speed 3) (safety 0)))
            (values (if (< ordinal2 (length bits1))
                      (not (eql 0 (sbit bits1 ordinal2))))
                    t))
          (if (%standard-instance-p class1)
            (if (memq class2 (%class.local-supers class1))
              (values t t)
              (if (eq (%class-of-instance class1)
                      *forward-referenced-class-class*)
                (values nil nil)
                ;; %INITED-CLASS-CPL will return NIL if class1 can't
                ;; be finalized; in that case, we don't know the answer.
                (let ((supers (%inited-class-cpl class1)))
                  (if (memq class2 supers)
                    (values t t)
                    (values nil (not (null supers)))))))
            (values nil t))))
      (values nil t))))

(defun find-class-intersection (c1 c2)
  (labels ((walk-subclasses (class f)
	     (dolist (sub (class-direct-subclasses class))
	       (walk-subclasses sub f))
	     (funcall f class)))
    (let* ((intersection nil))
      (walk-subclasses c1 #'(lambda (c)
			      (when (subclassp c c2)
				(pushnew (%class.ctype c) intersection))))
      (when intersection
	(%type-union intersection)))))

(define-type-method (class :simple-intersection) (type1 type2)
  (assert (not (eq type1 type2)))
  (let* ((class1 (if (class-ctype-p type1) (class-ctype-class type1)))
         (class2 (if (class-ctype-p type2) (class-ctype-class type2))))
    (if (and class1
             (not (typep class1 'compile-time-class))
             class2
             (not (typep class2 'compile-time-class)))
      (cond ((subclassp class1 class2)
             type1)
            ((subclassp class2 class1)
             type2)
	    ;;; In the STANDARD-CLASS case where neither's
	    ;;; a subclass of the other, there may be
	    ;;; one or mor classes that're a subclass of both.  We
	    ;;; -could- try to find all such classes, but
	    ;;; punt instead.
            (t (or (find-class-intersection class1 class2)
		 *empty-type*)))
      nil)))

(define-type-method (class :complex-subtypep-arg2) (type1 class2)
  (if (and (intersection-ctype-p type1)
	   (> (count-if #'class-ctype-p (intersection-ctype-types type1)) 1))
      (values nil nil)
      (invoke-complex-subtypep-arg1-method type1 class2 nil t)))

(define-type-method (class :complex-subtypep-arg1) (type1 type2)
  (if (and (function-ctype-p type2)
	   (eq type1 (specifier-type 'function))
	   (function-ctype-wild-args type2)
	   (eq *wild-type* (function-ctype-returns type2)))
      (values t t)
      (values nil t)))

(define-type-method (class :unparse) (type)
  (class-name (class-ctype-class type)))


;;; TYPE-DIFFERENCE  --  Interface
;;;
;;;    Return the type that describes all objects that are in X but not in Y.
;;; If we can't determine this type, then return NIL.
;;;
;;;    For now, we only are clever dealing with union and member types.  If
;;; either type is not a union type, then we pretend that it is a union of just
;;; one type.  What we do is remove from X all the types that are a subtype any
;;; type in Y.  If any type in X intersects with a type in Y but is not a
;;; subtype, then we give up.
;;;
;;;    We must also special-case any member type that appears in the union.  We
;;; remove from X's members all objects that are TYPEP to Y.  If Y has any
;;; members, we must be careful that none of those members are CTYPEP to any
;;; of Y's non-member types.  We give up in this case, since to compute that
;;; difference we would have to break the type from X into some collection of
;;; types that represents the type without that particular element.  This seems
;;; too hairy to be worthwhile, given its low utility.
;;;
(defun type-difference (x y)
  (let ((x-types (if (union-ctype-p x) (union-ctype-types x) (list x)))
	(y-types (if (union-ctype-p y) (union-ctype-types y) (list y))))
    (collect ((res))
      (dolist (x-type x-types)
	(if (member-ctype-p x-type)
	    (collect ((members))
	      (dolist (mem (member-ctype-members x-type))
		(multiple-value-bind (val win) (ctypep mem y)
		  (unless win (return-from type-difference nil))
		  (unless val
		    (members mem))))
	      (when (members)
		(res (make-member-ctype :members (members)))))
	    (dolist (y-type y-types (res x-type))
	      (multiple-value-bind (val win) (csubtypep x-type y-type)
		(unless win (return-from type-difference nil))
		(when val (return))
		(when (types-intersect x-type y-type)
		  (return-from type-difference nil))))))
      (let ((y-mem (find-if #'member-ctype-p y-types)))
	(when y-mem
	  (let ((members (member-ctype-members y-mem)))
	    (dolist (x-type x-types)
	      (unless (member-ctype-p x-type)
		(dolist (member members)
		  (multiple-value-bind (val win) (ctypep member x-type)
		    (when (or (not win) val)
		      (return-from type-difference nil)))))))))
      (apply #'type-union (res)))))

;;; CTypep  --  Interface
;;;
;;;    If Type is a type that we can do a compile-time test on, then return the
;;; whether the object is of that type as the first value and second value
;;; true.  Otherwise return NIL, NIL.
;;;
;;; We give up on unknown types, pick off FUNCTION and UNION types.  For
;;; structure types, we require that the type be defined in both the current
;;; and compiler environments, and that the INCLUDES be the same.
;;;
(defun ctypep (obj type)
  (declare (type ctype type))
  (etypecase type
    ((or numeric-ctype named-ctype member-ctype array-ctype cons-ctype)
     (values (%typep obj type) t))
    (class-ctype
     (values (not (null (class-typep  obj (class-ctype-class type)))) t)
)
    (union-ctype
     (any/type #'ctypep obj (union-ctype-types type)))
    (intersection-ctype
     (every/type #'ctypep obj (intersection-ctype-types type)))
    (function-ctype
     (values (functionp obj) t))
    (unknown-ctype
     (values nil nil))
    (foreign-ctype
     (values (foreign-typep obj (foreign-ctype-foreign-type type)) t))
    (negation-ctype
     (multiple-value-bind (res win)
	 (ctypep obj (negation-ctype-type type))
       (if win
	   (values (not res) t)
	   (values nil nil))))
    (hairy-ctype
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-ctype-specifier type))
	    (symbol (if (consp hairy-spec) (car hairy-spec) hairy-spec)))
       (ecase symbol
	 (and				; how would this get there ?
	  (if (atom hairy-spec)
	    (values t t)
	    (dolist (spec (cdr hairy-spec) (values t t))
	      (multiple-value-bind (res win)
		  (ctypep obj (specifier-type spec))
		(unless win (return (values nil nil)))
		(unless res (return (values nil t)))))))
	   (not				; how would this get there ?
	    (multiple-value-bind
	      (res win)
		(ctypep obj (specifier-type (cadr hairy-spec)))
	      (if win
		(values (not res) t)
		(values nil nil))))
	   (satisfies
	    (let ((fun (second hairy-spec)))
	      (cond ((and (symbolp fun) (fboundp fun))
                     ;; Binding *BREAK-ON-SIGNALS* here is a modularity
                     ;; violation intended to improve the signal-to-noise
                     ;; ratio on a mailing list.
		     (values (not (null (let* ((*break-on-signals* nil))
                                          (ignore-errors (funcall fun obj))))) t))
		    (t
		     (values nil nil))))))))))

;;; %TYPEP -- internal.
;;;
;;; The actual typep engine.  The compiler only generates calls to this
;;; function when it can't figure out anything more intelligent to do.
;;;
; lose 1 function call -MAYBE
(defun %typep (object specifier)
  (%%typep object
           (if (typep specifier 'ctype)
	     specifier
	     (specifier-type specifier))))

(eval-when (:compile-toplevel)
  (declaim (inline numeric-%%typep
                   array-%%typep
                   member-%%typep
                   cons-%%typep)))

(defun numeric-%%typep (object type)
  (let ((pred (numeric-ctype-predicate type)))
    (if pred
      (funcall pred object)
      (and (numberp object)
           (let ((num (if (complexp object) (realpart object) object)))
             (ecase (numeric-ctype-class type)
               (integer (integerp num))
               (rational (rationalp num))
               (float
                (ecase (numeric-ctype-format type)
                  (single-float (typep num 'single-float))
                  (double-float (typep num 'double-float))
                  ((nil) (floatp num))))
               ((nil) t)))
           (flet ((bound-test (val)
                    (let ((low (numeric-ctype-low type))
                          (high (numeric-ctype-high type)))
                      (and (cond ((null low) t)
                                 ((listp low) (> val (car low)))
                                 (t (>= val low)))
                           (cond ((null high) t)
                                 ((listp high) (< val (car high)))
                                 (t (<= val high)))))))
             (ecase (numeric-ctype-complexp type)
               ((nil) t)
               (:complex
                (and (complexp object)
                     (bound-test (realpart object))
                     (bound-test (imagpart object))))
               (:real
                (and (not (complexp object))
                     (bound-test object)))))))))

(defun array-%%typep (object type)
  (let* ((typecode (typecode object)))
    (declare (type (unsigned-byte 8) typecode))
    (and (>= typecode target::subtag-arrayH)
         (ecase (array-ctype-complexp type)
           ((t) (not (simple-array-p object)))
           ((nil) (simple-array-p object))
           ((* :maybe) t))
         (let* ((ctype-dimensions (array-ctype-dimensions type)))
           (or (eq ctype-dimensions '*)
	       (if (eql typecode target::subtag-arrayH)
		   (let* ((rank (%svref object target::arrayH.rank-cell)))
		     (declare (fixnum rank))
		     (and (eql rank (length ctype-dimensions))
			  (do* ((i 0 (1+ i))
				(dim target::arrayH.dim0-cell (1+ dim))
				(want (array-ctype-dimensions type) (cdr want))
				(got (%svref object dim) (%svref object dim)))
			       ((eql i rank) t)
			    (unless (or (eq (car want) '*)
					(eql (%car want) (the fixnum got)))
			      (return nil)))))
		   (and (null (cdr ctype-dimensions))
			(or (eq (%car ctype-dimensions) '*)
			    (eql (%car ctype-dimensions)
                                 (if (eql typecode target::subtag-vectorH)
                                   (%svref object target::vectorH.physsize-cell)
                                   (uvsize object))))))))
	 (or (eq (array-ctype-element-type type) *wild-type*)
	     (eql (array-ctype-typecode type)
		  (if (> typecode target::subtag-vectorH)
                      typecode
                      (ldb target::arrayH.flags-cell-subtag-byte (the fixnum (%svref object target::arrayH.flags-cell)))))
	     (type= (array-ctype-specialized-element-type type)
		    (specifier-type (array-element-type object)))))))


(defun member-%%typep (object type)
  (not (null (member object (member-ctype-members type)))))

(defun cons-%%typep (object type) 
  (and (consp object)
       (%%typep (car object) (cons-ctype-car-ctype type))
       (%%typep (cdr object) (cons-ctype-cdr-ctype type)))) 


(defun %%typep (object type)
  ;(if (not (typep type 'ctype))(setq type (specifier-type type)))
  (locally (declare (type ctype type))
    (etypecase type
      (named-ctype
       (ecase (named-ctype-name type)
         ((* t) t)
         ((nil) nil)))
      (numeric-ctype
       (numeric-%%typep object type))
      (array-ctype
       (array-%%typep object type))
      (member-ctype
       (member-%%typep object type))
      (class-ctype
       (not (null (class-typep object (class-ctype-class type)))))
      (union-ctype
       (dolist (type (union-ctype-types type))
         (when (%%typep object type)
           (return t))))
      (intersection-ctype
       (dolist (type (intersection-ctype-types type) t)
         (unless (%%typep object type) (return nil))))
      (cons-ctype
       (cons-%%typep object type))
      (unknown-ctype
       ;; Parse it again to make sure it's really undefined.
       (let ((reparse (specifier-type (unknown-ctype-specifier type))))
         (if (typep reparse 'unknown-ctype)
           (error "Unknown type specifier: ~S"
                  (unknown-ctype-specifier reparse))
           (%%typep object reparse))))
      (negation-ctype
       (not (%%typep object (negation-ctype-type type))))
      (hairy-ctype
       ;; Now the tricky stuff.
       (let* ((hairy-spec (hairy-ctype-specifier type))
              (symbol (if (consp hairy-spec) (car hairy-spec) hairy-spec)))
         (ecase symbol
           (and
            (or (atom hairy-spec)
                (dolist (spec (cdr hairy-spec) t)
                  (unless (%%typep object (specifier-type spec))
                    (return nil)))))
           (not
            (unless (and (listp hairy-spec) (= (length hairy-spec) 2))
              (error "Invalid type specifier: ~S" hairy-spec))
            (not (%%typep object (specifier-type (cadr hairy-spec)))))
           (satisfies
            (unless (and (listp hairy-spec) (= (length hairy-spec) 2))
              (error "Invalid type specifier: ~S" hairy-spec))
            (let ((fn (cadr hairy-spec)))
              (if (funcall (typecase fn
                             (function fn)
                             (symbol (symbol-function fn))
                             (t
                              (coerce fn 'function)))
                           object)
                t
                nil))))))
      #|
    (foreign-ctype
     (foreign-typep object (foreign-ctype-foreign-type type)))
|#
      (function-ctype
       (error "Function types are not a legal argument to TYPEP:~%  ~S"
              (type-specifier type))))))


;;; Ctype-Of  --  Interface
;;;
;;;    Like Type-Of, only returns a Type structure instead of a type
;;; specifier.  We try to return the type most useful for type checking, rather
;;; than trying to come up with the one that the user might find most
;;; informative.
;;;

(defun float-format-name (x)
  (declare (float x))
  (etypecase x
    (single-float "SINGLE-FLOAT")
    (double-float "DOUBLE-FLOAT")))

(defun ctype-of-number (x)
  (let ((num (if (complexp x) (realpart x) x)))
    (multiple-value-bind (complexp low high)
	(if (complexp x)
	    (let ((imag (imagpart x)))
	      (values :complex (min num imag) (max num imag)))
	    (values :real num num))
      (make-numeric-ctype :class (etypecase num
				   (integer (if (complexp x)
                                                (if (integerp (imagpart x))
                                                    'integer
                                                    'rational)
                                                'integer))
				   (rational 'rational)
				   (float 'float))
			  :format (and (floatp num)
				       (if (typep num 'double-float)
					 'double-float
					 'single-float))
			  :complexp complexp
			  :low low
			  :high high))))

(defun ctype-of (x)
  (typecase x
    (function (specifier-type 'function)) ; GFs ..
    (symbol
     (make-member-ctype :members (list x)))
    (number (ctype-of-number x))
    (array
     (let ((etype (specifier-type (array-element-type x))))
       (make-array-ctype :dimensions (array-dimensions x)
			 :complexp (not (typep x 'simple-array))
			 :element-type etype
			 :specialized-element-type etype)))
    (t
     (%class.ctype (class-of x)))))

(defvar *ctype-of-double-float-0* (ctype-of 0.0d0))
(defvar *ctype-of-single-float-0* (ctype-of 0.0f0))




; These DEFTYPES should only happen while initializing.

(progn
(let-globally ((*type-system-initialized* nil))


(deftype bit () '(integer 0 1))

(deftype eql (val) `(member ,val))

(deftype signed-byte (&optional s)
  (cond ((eq s '*) 'integer)
	  ((and (integerp s) (> s 0))
	   (let ((bound (ash 1 (1- s))))
	     `(integer ,(- bound) ,(1- bound))))
	  (t
	   (signal-program-error "Bad size specified for SIGNED-BYTE type specifier: ~S." s))))
  
(deftype unsigned-byte (&optional s)
  (cond ((eq s '*) '(integer 0))
	((and (integerp s) (> s 0))
	 `(integer 0 ,(1- (ash 1 s))))
	(t
	 (error "Bad size specified for UNSIGNED-BYTE type specifier: ~S." s))))

(deftype vector (&optional element-type size)
  `(array ,element-type (,size)))

(deftype simple-vector (&optional size)
  `(simple-array t (,size)))

(deftype base-string (&optional size)
  `(array base-char (,size)))
(deftype simple-base-string (&optional size)
  `(simple-array base-char (,size)))



(deftype string (&optional size)
  `(array character (,size)))

(deftype simple-string (&optional size)
  `(simple-array character (,size)))

(deftype extended-string (&optional size)
  (declare (ignore size))
  'nil)

(deftype simple-extended-string (&optional size)
  (declare (ignore size))
  'nil)

(deftype bit-vector (&optional size)
  `(array bit (,size)))

(deftype simple-bit-vector (&optional size)
  `(simple-array bit (,size)))

; TYPE-OF sometimes returns random symbols that aren't really type specifiers.

(deftype simple-unsigned-word-vector (&optional size)
  `(simple-array (unsigned-byte 16) (,size)))

(deftype simple-unsigned-byte-vector (&optional size)
  `(simple-array (unsigned-byte 8) (,size)))

(deftype simple-unsigned-long-vector (&optional size)
  `(simple-array (unsigned-byte 32) (,size)))

(deftype simple-signed-word-vector (&optional size)
  `(simple-array (signed-byte 16) (,size)))

(deftype simple-signed-byte-vector (&optional size)
  `(simple-array (signed-byte 8) (,size)))

(deftype simple-signed-long-vector (&optional size)
  `(simple-array (signed-byte 32) (,size)))



(deftype simple-short-float-vector (&optional size)
  `(simple-array short-float (,size)))

(deftype unsigned-word-vector (&optional size)
  `(vector (unsigned-byte 16) ,size))

(deftype single-float-vector (&optional size)
  `(vector short-float ,size))

(deftype unsigned-byte-vector (&optional size)
  `(vector (unsigned-byte 8) ,size))

(deftype unsigned-long-vector (&optional size)
  `(vector (unsigned-byte 32) ,size))

(deftype long-float-vector (&optional size)
  `(vector double-float ,size))

(deftype long-vector (&optional size)
  `(vector (signed-byte 32) ,size))

(deftype double-float-vector (&optional size)
  `(vector double-float ,size))

(deftype byte-vector (&optional size)
  `(vector (signed-byte 8) ,size))

(deftype general-vector (&optional size)
  `(vector t ,size))

(deftype word-vector (&optional size)
  `(vector (signed-byte 16) ,size))

(deftype short-float-vector (&optional size)
  `(vector single-float ,size))

(deftype simple-1d-array (&optional size)
  `(simple-array * (,size)))

(deftype simple-long-vector (&optional size)
  `(simple-array (signed-byte 32) (,size)))

(deftype simple-word-vector (&optional size)
  `(simple-array (signed-byte 16) (,size)))

(deftype simple-short-float-vector (&optional size)
  `(simple-array single-float (,size)))

(deftype simple-byte-vector (&optional size)
  `(simple-array (signed-byte 8) (,size)))

(deftype simple-double-float-vector (&optional size)
  `(simple-array double-float (,size)))

(deftype simple-single-float-vector (&optional size)
  `(simple-array single-float (,size)))

(deftype simple-long-float-vector (&optional size)
  `(simple-array double-float (,size)))

(deftype simple-fixnum-vector (&optional size)
  `(simple-array fixnum (,size)))

(deftype fixnum-vector (&optional size)
  `(array fixnum (,size)))

#+64-bit-target
(deftype simple-doubleword-vector (&optional size)
  `(simple-array (signed-byte 64) (,size)))

#+64-bit-target
(deftype simple-unsigned-doubleword-vector (&optional size)
  `(simple-array (unsigned-byte 64) (,size)))


(deftype short-float (&optional low high)
  `(single-float ,low ,high))

(deftype long-float (&optional low high)
  `(double-float ,low ,high))

#||
;;; As empty a type as you're likely to find ...
(deftype extended-char ()
  "Type of CHARACTERs that aren't BASE-CHARs."
  nil)
||#

(deftype natural ()
  `(unsigned-byte ,target::nbits-in-word))

(deftype signed-natural ()
  `(signed-byte ,target::nbits-in-word))
)


(let* ((builtin-translations 
        `((array . array)
          (simple-array . simple-array)
          (cons . cons)
          (vector . vector)
          (null . (member nil))
          (list . (or cons null))
          (sequence . (or list vector))
          (simple-vector . simple-vector)
          (bit-vector . bit-vector)
          (simple-bit-vector . simple-bit-vector)
          (simple-string . simple-string)
          (simple-base-string . simple-base-string)
          (string . string)
          (base-string . base-string)
          (real . real)
          (complex . complex)
          (float . float)
          (double-float . double-float)
          (long-float . double-float)
          (single-float . single-float)
	  (short-float . single-float)

          (rational . rational)
          (integer . integer)
          (ratio . (and rational (not integer)))
          (fixnum . (integer ,target::target-most-negative-fixnum
                     ,target::target-most-positive-fixnum))
          (bignum . (or (integer * (,target::target-most-negative-fixnum))
                         (integer (,target::target-most-positive-fixnum) *)))
          
          )))
  (dolist (spec builtin-translations)
    (setf (info-type-kind (car spec)) :primitive
          (info-type-builtin (car spec)) (specifier-type (cdr spec)))))





       
(precompute-types '((mod 2) (mod 4) (mod 16) (mod #x100) (mod #x10000)
                    #-cross-compiling
		    (mod #x100000000)
		    (unsigned-byte 1) 
		    (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32)
                    (unsigned-byte 64)
		    (signed-byte 8) (signed-byte 16) (signed-byte 32)
                    (signed-byte 64)
                    (or function symbol)
                    ))


(precompute-types *cl-types*)

;;; Treat CHARACTER and BASE-CHAR as equivalent.
(setf (info-type-builtin 'character) (info-type-builtin 'base-char))
;;; And EXTENDED-CHAR as empty.
(setf (info-type-builtin 'extended-char) *empty-type*)

(defparameter *null-type* (specifier-type 'null))


(flet ((set-builtin-class-type-translation (thing)
         (let* ((class-name (if (atom thing) thing (car thing)))
                (spec (if (atom thing) thing (cadr thing)))
                (spectype (specifier-type spec)))
           (setf (class-ctype-translation
                  (%class.ctype (find-class class-name))) spectype))))
  (mapc #'set-builtin-class-type-translation
        '(
          ;; Root Of All Evil
          t
          ;; Numbers:
          number real ratio complex rational fixnum
          ;;  Integers:
          signed-byte  unsigned-byte bit bignum integer
          ;;  Floats
           float  double-float single-float
          ;; Arrays
          array
          ;;  Simple Arrays
          simple-array
          ;;  Vectors
          vector string base-string bit-vector
          unsigned-byte-vector unsigned-word-vector unsigned-long-vector
          byte-vector word-vector long-vector
          single-float-vector double-float-vector
          general-vector
          fixnum-vector
          #+64-bit-target
          doubleword-vector
          #+64-bit-target
          unsigned-doubleword-vector
          ;;   Simple 1-Dimensional Arrays
          simple-1d-array  simple-string simple-base-string simple-bit-vector
          simple-unsigned-byte-vector
          simple-unsigned-long-vector
          simple-unsigned-word-vector
          simple-byte-vector
          simple-word-vector
          simple-long-vector 
          simple-single-float-vector 
          simple-double-float-vector
          simple-vector
          simple-fixnum-vector
          #+64-bit-target
          simple-doubleword-vector
          #+64-bit-target
          simple-unsigned-doubleword-vector
          ;; Sequence types
          sequence list  cons null
          
 )
                                                         
        ))
)
;(setq *type-system-initialized* t)




; These deftypes help the CMUCL compiler; the type system doesn't depend on them.

;;; Since OpenMCL's DEFTYPE tries to globally define the type
;;; at compile-time as well as load- and execute time, hide
;;; the definition of these "built-in" types.  (It'd be cleaner
;;; to make DEFTYPE do something saner at compile-time.)
(let* ()                                ; make the following be non-toplevel
(deftype boolean () '(member t nil))

(deftype atom () '(not cons))
;;;
;;; A type specifier.
(deftype type-specifier () '(or list symbol class))
;;;
;;; An index into an array.   Also used for sequence index. 
(deftype index () `(integer 0 (,array-dimension-limit)))
;;;
;;; Array rank, total size...
(deftype array-rank () `(integer 0 (,array-rank-limit)))
(deftype array-total-size () `(integer 0 (,array-total-size-limit)))
;;;
;;; Some thing legal in an evaluated context.
(deftype form () t)
;;;
;;; Maclisp compatibility...
(deftype stringlike () '(or string symbol))
(deftype stringable () '(or string symbol character))
;;;
;;; Save a little typing...
(deftype truth () '(member t))
;;;
;;; A thing legal in places where we want the name of a file.
(deftype filename () '(or string pathname))
;;;
;;; A legal arg to pathname functions.
(deftype pathnamelike () '(or string pathname stream))
;;;
;;; A thing returned by the irrational functions.  We assume that they never
;;; compute a rational result.
(deftype irrational () '(or float (complex float)))
;;;
;;; Character components:
(deftype char-code () `(integer 0 (,char-code-limit)))
;;;
;;; A consed sequence result.  If a vector, is a simple array.
(deftype consed-sequence () '(or list (simple-array * (*))))
;;;
;;; The :end arg to a sequence...
(deftype sequence-end () '(or null index))
;;;
;;; A valid argument to a stream function...
(deftype streamlike () '(or stream (member nil t)))
;;;
;;; A thing that can be passed to funcall & friends.
(deftype callable () '(or function symbol))

;;; Until we decide if and how to wedge this into the type system, make it
;;; equivalent to t.
;;;
(deftype void () t)
;;;
;;; An index into an integer.
(deftype bit-index () `(integer 0 ,target::target-most-positive-fixnum))
;;;
;;; Offset argument to Ash (a signed bit index).
(deftype ash-index () 'fixnum)

;;; Not sure how to do this without SATISFIES.
(deftype setf-function-name () `(satisfies setf-function-name-p))

;;; Better than nothing, arguably.
(deftype function-name () `(or symbol setf-function-name))

(deftype valid-char-code () `(satisfies valid-char-code-p))

)                                       ; end of LET* sleaze

(defun array-or-union-ctype-element-type (ctype)
  (if (typep ctype 'array-ctype)
    (type-specifier (array-ctype-element-type ctype))
    (if (typep ctype 'union-ctype)
      `(or ,@(mapcar #'array-or-union-ctype-element-type 
                     (union-ctype-types ctype))))))


(defvar *simple-predicate-function-prototype*
  #'(lambda (thing)
      (%%typep thing #.(specifier-type t))))

(defun make-simple-type-predicate (function datum)
  #+ppc-target
  (gvector :function
           (uvref *simple-predicate-function-prototype* 0)
           datum
           function
           nil
           (dpb 1 $lfbits-numreq 0))
  #+x86-target
  (%clone-x86-function
   *simple-predicate-function-prototype*
   datum
   function
   nil
   (dpb 1 $lfbits-numreq 0)))

(defun check-ctypep (thing ctype)
  (multiple-value-bind (win sure) (ctypep thing ctype)
    (or win (not sure))))


(defun generate-predicate-for-ctype (ctype)
  (typecase ctype
    (numeric-ctype
     (or (numeric-ctype-predicate ctype)
         (make-simple-type-predicate 'numeric-%%typep ctype)))
    (array-ctype
     (make-simple-type-predicate 'array-%%typep ctype))
    (member-ctype
     (make-simple-type-predicate 'member-%%typep ctype))
    (named-ctype
     (case (named-ctype-name ctype)
       ((* t) #'true)
       (t #'false)))
    (cons-ctype
     (make-simple-type-predicate 'cons-%%typep ctype))
    (function-ctype
     #'functionp)
    (class-ctype
     (make-simple-type-predicate 'class-cell-typep (find-class-cell (class-name (class-ctype-class ctype)) t)))
    (t
     (make-simple-type-predicate 'check-ctypep ctype))))
    
        

   

;;; Ensure that standard EFFECTIVE-SLOT-DEFINITIONs have a meaningful
;;; type predicate, if we can.
(defmethod shared-initialize :after ((spec effective-slot-definition)
				     slot-names
				     &key 
				     &allow-other-keys)
  (declare (ignore slot-names))
  (let* ((type (slot-definition-type spec)))
    (setf (slot-value spec 'type-predicate)
	  (or (and (typep type 'symbol)
                   (not (eq type 't))
		   (type-predicate type))
              (handler-case
                  (let* ((ctype (specifier-type type)))
                    (unless (eq ctype *universal-type*)
                      (generate-predicate-for-ctype ctype)))
                (invalid-type-specifier ()
                  (warn "Invalid type soecifier ~s in slot definition for ~s in class ~s." type (slot-definition-name spec) (slot-definition-class spec))
                  (lambda (v)
                    (cerror "Allow the assignment or initialization."
                            "Can't determine whether or not the value ~s should be used to initialize or assign to the slot ~&named ~s in an instance of ~s, because the slot is declared ~&to be of the invalid type ~s."
                            v (slot-definition-name spec) (slot-definition-class spec) (slot-definition-type spec))
                    ;; Suppress further checking, at least for things that use this effective slotd.
                    ;; (It's hard to avoid this, and more trouble than it's worth to do better.)
                    (setf (slot-value spec 'type-predicate) nil)
                    t))
                (parse-unknown-type (c)
                   (declare (ignore c))
                   #'(lambda (value)
                       ;; If the type's now known, install a new predicate.
                       (let* ((nowctype (specifier-type type)))
                         (unless (typep nowctype 'unknown-ctype)
                           (setf (slot-value spec 'type-predicate)
                                 (generate-predicate-for-ctype nowctype)))
                         (multiple-value-bind (win sure)
                             (ctypep value nowctype)
                           (or (not sure) win))))))))))

