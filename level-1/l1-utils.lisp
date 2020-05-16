;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

; L1-utils.lisp

(in-package "CCL")

;The following forms (up thru defn of %DEFUN) must come before any DEFUN's.
;Any (non-kernel) functions must be defined before they're used! 
;In fact, ALL functions must be defined before they're used!  How about that ?



(setq %lisp-system-fixups% nil)


;;; Kludge for record-source-file bootstrapping

(%fhave 'full-pathname (qlfun bootstrapping-full-pathname (name) name))


; real one is  in setf.lisp
(%fhave '%setf-method (qlfun bootstripping-setf-fsname (spec)
                                   spec nil))

(fset 'physical-pathname-p (lambda (file)(declare (ignore file)) nil)) ; redefined later

(setq *record-source-file* t)

(fset 'level-1-record-source-file
      (qlfun level-1-record-source-file (name def-type &optional (source (or *loading-toplevel-location*
                                                                             *loading-file-source-file*)))
        ;; Level-0 puts stuff on plist of name.  Once we're in level-1, names can
        ;; be more complicated than just a symbol, so just collect all calls until
        ;; the real record-source-file is loaded.
        (when *record-source-file*
          (unless (listp *record-source-file*)
            (setq *record-source-file* nil))
          (push (list name def-type source) *record-source-file*))))

(fset 'record-source-file #'level-1-record-source-file)

(defun inherit-from-p (ob parent)
  (memq (if (symbolp parent) (find-class parent nil) parent)
        (%inited-class-cpl (class-of ob))))

;;; returns new plist with value spliced in or key, value consed on.
(defun setprop (plist key value &aux loc)
  (if (setq loc (pl-search plist key))
    (progn (%rplaca (%cdr loc) value) plist)
    (cons key (cons value plist))))

(defun getf-test (place indicator test &optional default)
  (loop
    (when (null place)
      (return default))
    (when (funcall test indicator (car place))
      (return (cadr place)))
    (setq place (cddr place))))

(defun setprop-test (plist indicator test value)
  (let ((tail plist))
    (loop
      (when (null tail)
        (return (cons indicator (cons value plist))))
      (when (funcall test indicator (car tail))
        (setf (cadr tail) value)
        (return plist))
      (setq tail (cddr tail)))))

(defun plistp (p &aux len)
  (and (listp p)
       (setq len (list-length p))
       (not (%ilogbitp 0 len))))  ; (evenp p)

(defun %imax (i1 i2)
 (if (%i> i1 i2) i1 i2))

(defun %imin (i1 i2)
  (if (%i< i1 i2) i1 i2))




;|#


(eval-when (:compile-toplevel :execute)
  (require "NUMBER-MACROS"))




(defun loading-file-source-file ()
  *loading-file-source-file*)

(setq *save-local-symbols* t)

(%fhave 'require-type (nfunction bootstrapping-require-type
                                 (lambda (thing type)
                                   (declare (ignore type))
                                   thing)))
(%fhave '%require-type 
        (nfunction bootstrapping-%require-type
                   (lambda (thing predicate)
                     (declare (ignore predicate))
                     thing)))

(setf (type-predicate 'macptr) 'macptrp)

;;; Once we're bootstrapped, we should move the real PREPARE-TO-DESTRUCTURE
;;; here.
(defun %early-prepare-to-destructure (list &rest ignore)
  (declare (ignore ignore))
  list)

(setf (fdefinition 'prepare-to-destructure) #'%early-prepare-to-destructure)







(defun %keyword-present-p (keys keyword)
  (let* ((not-there (cons nil nil)))
    (declare (dynamic-extent not-there))
    (not (eq (getf keys keyword not-there) not-there))))

(defun check-keywords (keys actual allow-others)
  (let* ((len (ignore-errors (list-length actual))))
    (if (null len)
      (signal-simple-program-error "Circular or dotted keyword list: ~s" actual)
      (if (oddp len)
	(signal-simple-program-error "Odd length keyword list: ~s" actual))))
  (setq allow-others (or allow-others (getf actual :allow-other-keys)))
  (do* ((a actual (cddr a))
	(k (car a) (car a)))
       ((null a))
    (unless (typep k 'symbol)
      (signal-simple-program-error
       "Invalid keyword argument ~s in ~s.  ~&Valid keyword arguments are ~s." k actual keys))
    (unless (or allow-others
		(eq k :allow-other-keys)
		(member k keys))
      (signal-simple-program-error "Unknown keyword argument ~s in ~s.  ~&Valid keyword arguments are ~s." k actual keys))))

(%fhave 'set-macro-function #'%macro-have)   ; redefined in sysutils.

;;; Define special forms.
(dolist (sym '(block catch compiler-let eval-when
               flet function go if labels let let* macrolet
               multiple-value-call multiple-value-prog1
               progn progv quote return-from setq tagbody
               the throw unwind-protect locally load-time-value
	       symbol-macrolet
               ;; These are implementation-specific special forms :
	       nfunction
               #+ppc-target
	       ppc-lap-function
               #+arm-target
               arm-lap-function
               #+x86-target
               x86-lap-function
               fbind
               with-c-frame with-variable-c-frame))
  (%macro-have sym sym))


(defun %macro (named-fn &optional doc &aux arglist)
  ;; "doc" is either a string or a list of the form :
  ;; (doc-string-or-nil . (body-pos-or-nil . arglist-or-nil))
  (if (listp doc)
    (setq arglist (cddr doc)
          doc (car doc)))
  (let* ((name (function-name named-fn)))
    (record-source-file name 'function)
    (set-macro-function name named-fn)
    (when (and doc *save-doc-strings*)
      (set-documentation name 'function doc))
    (when arglist
      (record-arglist name arglist))
    (when *fasload-print* (format t "~&~S~%" name))
    name))


(defun %defvar (var &optional doc)
  "Returns boundp"
  (%proclaim-special var)
  (record-source-file var 'variable)
  (when (and doc *save-doc-strings*)
    (set-documentation var 'variable doc))
  (cond ((not (boundp var))
         (when *fasload-print* (format t "~&~S~%" var))
         nil)
        (t t)))

(defun %defparameter (var value &optional doc)
  (%proclaim-special var)
  (record-source-file var 'variable)
  (when (and doc *save-doc-strings*)
    (set-documentation var 'variable doc))
  (when *fasload-print* (format t "~&~S~%" var))
  (set var value)
  var)


(defun %defglobal (var value &optional doc)
  (%symbol-bits var (logior (ash 1 $sym_vbit_global) (the fixnum (%symbol-bits var))))
  (%defparameter var value doc))

;Needed early for member etc.
(defun identity (x)
  "This function simply returns what was passed to it."
  x)

(defun coerce-to-function (arg)
  (if (functionp arg)
    arg
    (if (symbolp arg)
      (%function arg)
      (report-bad-arg arg 'function))))

;;; takes arguments in arg_x, arg_y, arg_z, returns "multiple values" 
;;; Test(-not) arguments are NOT validated beyond what is done
;;; here.
;;; if both :test and :test-not supplied, signal error.
;;; if test provided as #'eq or 'eq, return first value 'eq.
;;; if test defaulted, provided as 'eql, or provided as #'eql, return
;;; first value 'eql.
;;; if test-not provided as 'eql or provided as #'eql, return second
;;; value 'eql.
;;; if key provided as either 'identity or #'identity, return third value nil.
(defun %key-conflict (test-fn test-not-fn key)
  (let* ((eqfn #'eq)
         (eqlfn #'eql)
         (idfn #'identity))
    (if (or (eq key 'identity) (eq key idfn))
      (setq key nil))
    (if test-fn
      (if test-not-fn
        (%err-disp $xkeyconflict ':test test-fn ':test-not test-not-fn)
        (if (eq test-fn eqfn)
          (values 'eq nil key)
          (if (eq test-fn eqlfn)
            (values 'eql nil key)
            (values test-fn nil key))))
      (if test-not-fn
        (if (eq test-not-fn eqfn)
          (values nil 'eq key)
          (if (eq test-not-fn eqlfn)
            (values nil 'eql key)
            (values nil test-not-fn key)))
        (values 'eql nil key)))))




;;; Assoc.

;;; (asseql item list) <=> (assoc item list :test #'eql :key #'identity)



;;; (assoc-test item list test-fn) 
;;;   <=> 
;;;     (assoc item list :test test-fn :key #'identity)
;;; test-fn may not be FUNCTIONP, so we coerce it here.
(defun assoc-test (item list test-fn)
  (dolist (pair list)
    (if pair
      (if (funcall test-fn item (car pair))
	(return pair)))))



; (assoc-test-not item list test-not-fn) 
;   <=> 
;     (assoc item list :test-not test-not-fn :key #'identity)
; test-not-fn may not be FUNCTIONP, so we coerce it here.
(defun assoc-test-not (item list test-not-fn)
  (dolist (pair list)
    (if pair
      (if (not (funcall test-not-fn item (car pair)))
	(return pair)))))

(defun assoc (item list &key test test-not key)
  "Return the cons in ALIST whose car is equal (by a given test or EQL) to
   the ITEM."
  (multiple-value-bind (test test-not key) (%key-conflict test test-not key)
    (if (null key)
      (if (eq test 'eq)
        (assq item list)
        (if (eq test 'eql)
          (asseql item list)
          (if test
            (assoc-test item list test)
            (assoc-test-not item list test-not))))
      (if test
        (dolist (pair list)
          (if pair
            (if (funcall test item (funcall key (car pair)))
              (return pair))))
        (dolist (pair list)
          (if pair
            (unless (funcall test-not item (funcall key (car pair)))
              (return pair))))))))


;;;; Member.

;;; (member-test-not item list test-not-fn) 
;;;   <=> 
;;;     (member item list :test-not test-not-fn :key #'identity)
(defun member-test-not (item list test-not-fn)
  (do* ((l list (cdr l)))
       ((endp l))
    (unless (funcall test-not-fn item (%car l)) (return l))))

(defun member (item list &key test test-not key)
  "Return the tail of LIST beginning with first element satisfying EQLity,
   :TEST, or :TEST-NOT with the given ITEM."
  (multiple-value-bind (test test-not key) (%key-conflict test test-not key)
    (if (null key)
      (if (eq test 'eq)
        (memq item list)
        (if (eq test 'eql)
          (memeql item list)
          (if test
            (member-test item list test)
            (member-test-not item list test-not))))
      (if test
        (do* ((l list (cdr l)))
             ((endp l))
          (if (funcall test item (funcall key (car l)))
              (return l)))
        (do* ((l list (cdr l)))
             ((null l))
          (unless (funcall test-not item (funcall key (car l)))
              (return l)))))))


(defun adjoin (item list &key test test-not key)
  "Add ITEM to LIST unless it is already a member"
  (if (and (not test)(not test-not)(not key))
    (if (not (memeql item list))(cons item list) list)
    (multiple-value-bind (test test-not key) (%key-conflict test test-not key)
      (if
        (if (null key)
          (if (eq test 'eq)
            (memq item list)
            (if (eq test 'eql)
              (memeql item list)
              (if test
                (member-test item list test)
                (member-test-not item list test-not))))
          (if test
            (member (funcall key item) list :test test :key key)
            (member (funcall key item) list :test-not test-not :key key)))
        list
        (cons item list)))))

(defun adjoin-eq (elt list)
  (if (memq elt list)
    list
    (cons elt list)))

(defun adjoin-eql (elt list)
  (if (memeql elt list)
    list
    (cons elt list)))

(defun union-eq (list1 list2)
  (let ((res list2))
    (dolist (elt list1)
      (unless (memq elt res)
        (push elt res)))
    res))

(defun union-eql (list1 list2)
  (let ((res list2))
    (dolist (elt list1)
      (unless (memeql elt res)
        (push elt res)))
    res))

;;; Fix this someday.  Fix EQUALP, while you're at it ...
(defun similar-as-constants-p (x y)
  (or (eq x y)                          ; Redefinition of constants to themselves.
      (if (and (stringp x) (stringp y)) ;The most obvious case where equalp & s-a-c-p need to differ...
        (string= x y)
        (equalp x y))))

(defun undefine-constant (var)
  (%set-sym-global-value var (%unbound-marker-8)))

(defparameter *cerror-on-constant-redefinition* t)

(defun define-constant (var value)
  (block nil
    (if (constant-symbol-p var)
      (let* ((old-value (%sym-global-value var)))
	(unless (eq old-value (%unbound-marker-8))
	  (if (or (eql old-value value)
                  (and (not *strict-checking*) (similar-as-constants-p old-value value)))
	    (return)
	    ;; This should really be a cell error, allow options other than
	    ;; redefining (such as don't redefine and continue)...
            (when *cerror-on-constant-redefinition*
              (cerror "Redefine ~S to have new value ~*~s"
                      "Constant ~S is already defined with a different value (~s)"
                      var old-value value))))))
    (%symbol-bits var 
                  (%ilogior (%ilsl $sym_bit_special 1) (%ilsl $sym_bit_const 1)
                            (%symbol-bits var)))
    (%set-sym-global-value var value))
  var)

(defun %defconstant (var value &optional doc)
  (%proclaim-special var)
  (record-source-file var 'constant)
  (define-constant var value)
  (when (and doc *save-doc-strings*)
    (set-documentation var 'variable doc))
  (when *fasload-print* (format t "~&~S~%" var))
  var)

(defparameter *nx1-compiler-special-forms* ())
(defparameter *nx-proclaimed-types* ())
(defparameter *nx-proclaimed-ftypes* nil)

(defun compiler-special-form-p (sym)
  (or (eq sym 'quote)
      (if (memq sym *nx1-compiler-special-forms*) t)))



(defparameter *nx-known-declarations* ())
(defparameter *nx-proclaimed-inline* ())
(defparameter *nx-proclaimed-ignore* ())
(defparameter *nx-globally-inline* ())



(defconstant *cl-types* '(
array
atom
base-char
bignum
bit
bit-vector 
character
#|
lisp:common
|#
compiled-function 
complex 
cons                    
double-float
extended-char
fixnum
float
function
hash-table
integer
keyword
list 
long-float
nil 
null
number  
package
pathname 
random-state  
ratio
rational
readtable
real
sequence 
short-float
signed-byte 
simple-array
simple-bit-vector
simple-string 
simple-base-string
simple-vector 
single-float
standard-char
stream  
string
#|
lisp:string-char
|#
symbol
t
unsigned-byte 
vector
))

;; Redefined in sysutils.
(%fhave 'specifier-type-if-known
        (qlfun bootstrapping-type-specifier-p (name &optional env &key &allow-other-keys)
          (declare (ignore env))
          (memq name *cl-types*)))



(defun proclaim (spec)
  (case (car spec)
    (special (apply #'proclaim-special (%cdr spec)))
    (notspecial (apply #'proclaim-notspecial (%cdr spec)))
    (optimize (%proclaim-optimize (%cdr spec)))
    (inline (apply #'proclaim-inline t (%cdr spec)))
    (notinline (apply #'proclaim-inline nil (%cdr spec)))
    (declaration (apply #'proclaim-declaration (%cdr spec)))
    (ignore (apply #'proclaim-ignore t (%cdr spec)))
    (unignore (apply #'proclaim-ignore nil (%cdr spec)))
    (type (apply #'proclaim-type (%cdr spec)))
    (ftype (apply #'proclaim-ftype (%cdr spec)))
    (function (apply #'proclaim-type spec))
    (t (unless (memq (%car spec) *nx-known-declarations*)
         ;; Any type name is now (ANSI CL) a valid declaration.
         (if (specifier-type-if-known (%car spec))
           (apply #'proclaim-type spec)
           (signal-program-error "Unknown declaration specifier ~s in ~S" (%car spec) spec))))))

(defun bad-proclaim-spec (spec)
  (signal-program-error "Invalid declaration specifier ~s" spec))


(defun using-linear-scan () *backend-use-linear-scan*)


(defun proclaim-type (type &rest vars)
  (declare (dynamic-extent vars))
  ;; Called too early to use (every #'symbolp vars)
  (unless (loop for v in vars always (symbolp v)) (bad-proclaim-spec `(,type ,@vars)))
  (when *type-system-initialized*
    ;; Check the type.  This will signal program-error's in case of invalid types, let it.
    ;; Do not signal anything about unknown types though -- it should be ok to have forward
    ;; references here, before anybody needs the info.
    (specifier-type type))
  (dolist (var vars)
    (let ((spec (assq var *nx-proclaimed-types*)))
      (if spec
        (rplacd spec type)
        (push (cons var type) *nx-proclaimed-types*)))))

(defun proclaim-ftype (ftype &rest names)
  (declare (dynamic-extent names))
  (unless (every (lambda (v) (or (symbolp v) (setf-function-name-p v))) names)
    (bad-proclaim-spec `(ftype ,ftype ,@names)))
  (unless *nx-proclaimed-ftypes*
    (setq *nx-proclaimed-ftypes* (make-hash-table :test #'eq)))
  ;; Check the type.  This will signal program-error's in case of invalid types, let it.
  ;; Do not signal anything about unknown types though -- it should be ok to have forward
  ;; references here, before anybody needs the info.
  (let* ((ctype (specifier-type ftype)))
    ;; If know enough to complain now, do so.
    (when (types-disjoint-p ctype (specifier-type 'function))
      (bad-proclaim-spec `(ftype ,ftype ,@names))))
  (dolist (name names)
    (setf (gethash (maybe-setf-function-name name) *nx-proclaimed-ftypes*) ftype)))



(defun proclaimed-ftype (name)
  (when *nx-proclaimed-ftypes*
    (gethash (ensure-valid-function-name name) *nx-proclaimed-ftypes*)))


(defun proclaim-special (&rest vars)
  (declare (dynamic-extent vars))
  (unless (every #'symbolp vars) (bad-proclaim-spec `(special ,@vars)))
  (dolist (sym vars) (%proclaim-special sym)))


(defun proclaim-notspecial (&rest vars)
  (declare (dynamic-extent vars))
  (unless (every #'symbolp vars) (bad-proclaim-spec `(special ,@vars)))
  (dolist (sym vars) (%proclaim-notspecial sym)))

(defun proclaim-inline (t-or-nil &rest names)
  (declare (dynamic-extent names))
  ;;This is just to make it more likely to detect forgetting about the
  ;;first arg...
  (unless (or (eq nil t-or-nil) (eq t t-or-nil)) (report-bad-arg t-or-nil '(member t nil)))
  (unless (loop for v in names always (or (symbolp v) (setf-function-name-p v)))
    (bad-proclaim-spec `(,(if t-or-nil 'inline 'notinline) ,@names)))
  (dolist (name names)
    (setq name (maybe-setf-function-name name))
    (if (listp *nx-proclaimed-inline*)
      (setq *nx-proclaimed-inline*
          (alist-adjoin name
                        (or t-or-nil (if (compiler-special-form-p name) t))
                        *nx-proclaimed-inline*))
      (setf (gethash name *nx-proclaimed-inline*)
            (or t-or-nil (if (compiler-special-form-p name) t))))))

(defun proclaim-declaration (&rest syms)
  (declare (dynamic-extent syms))
  (unless (every #'symbolp syms) (bad-proclaim-spec `(declaration ,@syms)))
  (dolist (sym syms)
    (when (type-specifier-p sym)
      (error "Cannot define declaration ~s because it is the name of a type" sym))
    (setq *nx-known-declarations* 
          (adjoin sym *nx-known-declarations* :test 'eq))))

(defun check-declaration-redefinition (name why)
  (when (memq name *nx-known-declarations*)
    (cerror "Undeclare the declaration ~*~s"
	    "Cannot ~a ~s because ~:*~s has been declared as a declaration name" why name)
    (setq *nx-known-declarations* (remove name *nx-known-declarations*))))

(defun proclaim-ignore (t-or-nil &rest syms)
  (declare (dynamic-extent syms))
  ;;This is just to make it more likely to detect forgetting about the
  ;;first arg...
  (unless (or (eq nil t-or-nil) (eq t t-or-nil)) (report-bad-arg t-or-nil '(member t nil)))
  (unless (every #'symbolp syms) (bad-proclaim-spec `(,(if t-or-nil 'ignore 'unignore) ,@syms)))
  (dolist (sym syms)
    (setq *nx-proclaimed-ignore*
          (alist-adjoin sym t-or-nil *nx-proclaimed-ignore*))))


(queue-fixup
 (when (listp *nx-proclaimed-inline*)
  (let ((table (make-hash-table :size 100 :test #'eq)))
    (dolist (x *nx-proclaimed-inline*)
      (let ((name (car x)) (value (cdr x)))
        (when (symbolp name)
          (setf (gethash name table) value))))
    (setq *nx-proclaimed-inline* table))))

(defun proclaimed-special-p (sym)
  (%ilogbitp $sym_vbit_special (%symbol-bits sym)))

(defun proclaimed-inline-p (sym)
  (if (listp *nx-proclaimed-inline*)
    (%cdr (assq sym *nx-proclaimed-inline*))
    (gethash sym *nx-proclaimed-inline*)))

(defun proclaimed-notinline-p (sym)
  (if (listp *nx-proclaimed-inline*)
    (and (setq sym (assq sym *nx-proclaimed-inline*))
         (null (%cdr sym)))
    (null (gethash sym *nx-proclaimed-inline* t))))


(defun self-evaluating-p (form)
  (and (atom form)
       (or (not (non-nil-symbol-p form))
           (eq form t)
           (keywordp form))))

(defun constantp (form &optional env)
  "True of any Lisp object that has a constant value: types that eval to
  themselves, keywords, constants, and list whose car is QUOTE."
   (or (self-evaluating-p form)
       (quoted-form-p form)
       (constant-symbol-p form)
       (and env
	    (symbolp form)
	    (eq :constant (variable-information form env)))))


(defun eval-constant (form)
  (if (quoted-form-p form) (%cadr form)
    (if (constant-symbol-p form) (symbol-value form)
      (if (self-evaluating-p form) form
	(report-bad-arg form '(satisfies constantp))))))

(defvar *lfun-names*)


(defvar %lambda-lists% (make-hash-table :test #'eq :weak t))
(defparameter *save-arglist-info* t)


(defun record-arglist (name args)
  "Used by defmacro & defgeneric"
  (when (or *save-arglist-info* *save-local-symbols*)
    (setf (gethash name %lambda-lists%) args)))


;;;Support the simple case of defsetf.
(%fhave 'store-setf-method
        (qlfun bootstrapping-store-setf-method (name fn &optional doc)
          (declare (ignore doc))
          (put name 'bootstrapping-setf-method (require-type fn 'symbol))))
(%fhave '%setf-method
        (qlfun bootstrapping-%setf-method (name)
          (get name 'bootstrapping-setf-method)))


;;; defmacro uses (setf (assq ...) ...) for &body forms.
(defun adjoin-assq (indicator alist value)
  (let ((cell (assq indicator alist)))
    (if cell 
      (setf (cdr cell) value)
      (push (cons indicator value) alist)))
  alist)

(defmacro setf-assq (indicator place value)
  (let ((res (gensym)))
    `(let (,res)
       (setf ,place (adjoin-assq ,indicator ,place (setq ,res ,value)))
       ,res)))

(defsetf assq setf-assq)
(defsetf %typed-miscref %typed-miscset)

(defun quoted-form-p (form)
   (and (consp form)
        (eq (%car form) 'quote)
        (consp (%cdr form))
        (null (%cdr (%cdr form)))))

(defun lambda-expression-p (form)
  (and (consp form)
       (eq (%car form) 'lambda)
       (consp (%cdr form))
       (listp (%cadr form))))

;;;;;FUNCTION BINDING Functions

(defun fdefinition (name)
  (or (fboundp name) ;Our fboundp returns the binding
      (prog1 (%err-disp $xfunbnd name))))

(defun symbol-function (name)
  "Return the definition of NAME, even if it is a macro or a special form.
   Error if NAME doesn't have a definition."
  (fdefinition (require-type name 'symbol)))


(defun %make-function (name fn env)
  (compile-user-function fn name env))
    
;;;;;;;;; VALUE BINDING Functions

;; Lock used only to make sure calls to GENSYM don't step on each other.  Users who
;; modify *gensym-counter* in multi-threaded apps will need to do their own locking.
(defparameter *gensym-lock* (make-lock))

(defun gensym (&optional (string-or-integer nil string-or-integer-p))
  "Creates a new uninterned symbol whose name is a prefix string (defaults
   to \"G\"), followed by a decimal number. Thing, when supplied, will
   alter the prefix if it is a string, or be used for the decimal number
   if it is a number, of this symbol. The default value of the number is
   the current value of *gensym-counter* which is incremented each time
   it is used."
  (let ((prefix "G")
        (counter nil))
    (when string-or-integer-p
      (etypecase string-or-integer
        (integer (setq counter string-or-integer)) ; & emit-style-warning
        (string (setq prefix (ensure-simple-string string-or-integer)))))
    (unless counter
      (unless (and (numberp *gensym-counter*)(not (minusp *gensym-counter*)))
        (error 'type-error :datum *gensym-counter* :expected-type 'unsigned-byte))
      (with-lock-grabbed (*gensym-lock*) ; 
        (setq *gensym-counter* (1+ (setq counter *gensym-counter*)))))
    (make-symbol (%str-cat prefix (%integer-to-string counter)))))

(defun make-keyword (name)
  (if (and (symbolp name) (eq (symbol-package name) *keyword-package*))
    name
    (values (intern (string name) *keyword-package*))))




; destructive, removes first match only
(defun remove-from-alist (thing alist)
 (let ((start alist))
  (if (eq thing (%caar alist))
   (%cdr alist)
   (let* ((prev start)
          (this (%cdr prev))
          (next (%cdr this)))
    (while this
     (if (eq thing (%caar this))
      (progn
       (%rplacd prev next)
       (return-from remove-from-alist start))
      (setq prev this
            this next
            next (%cdr next))))
    start))))

;destructive
(defun add-to-alist (thing val alist &aux (pair (assq thing alist)))
  (if pair
    (progn (%rplacd pair thing) alist)
    (cons (cons thing val) alist)))

;non-destructive...
(defun alist-adjoin (thing val alist &aux (pair (assq thing alist)))
  (if (and pair (eq (%cdr pair) val))
    alist
    (cons (cons thing val) alist)))

(defun %str-assoc (str alist)
  (assoc str alist :test #'string-equal))

(defstatic *pathname-escape-character*
  #+windows-target #\>
  #-windows-target #\\
  "Not CL.  A Coral addition for compatibility between CL spec and the shell.")

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))

(defun caaar (x) (car (car (car x))))
(defun caadr (x) (car (car (cdr x))))
(defun cadar (x) (car (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cdaar (x) (cdr (car (car x))))
(defun cdadr (x) (cdr (car (cdr x))))
(defun cddar (x) (cdr (cdr (car x))))
(defun cdddr (x) (cdr (cdr (cdr x))))

(%fhave 'type-of #'%type-of)



(defun pointerp (thing &optional errorp)
  (if (macptrp thing)
    t
    (if errorp (error "~S is not a pointer" thing) nil)))


;Add an item to a dialog items list handle.  HUH ?
(defun %rsc-string (n)
  (or (cdr (assq n *error-format-strings*))
  (%str-cat "Error #" (%integer-to-string n))))

(defun string-arg (arg)
 (or (string-argp arg) (error "~S is not a string" arg)))

(defun string-argp (arg)
  (cond ((symbolp arg) (symbol-name arg))
        ((typep arg 'character) (string arg))
        ((stringp arg) (ensure-simple-string arg))
        (t nil)))
  
(defun symbol-arg (arg)
  (unless (symbolp arg)
    (report-bad-arg arg 'symbol))
  arg)

(defun %cstrlen (ptr)
  ;;(#_strlen ptr)
  (do* ((i 0 (1+ i)))
       ((zerop (the fixnum (%get-byte ptr i))) i)
    (declare (fixnum i))))


(defun %set-cstring (ptr string)
  (%cstr-pointer string ptr)
  string)

(defsetf %get-cstring %set-cstring)

;;; Deprecated, but used by UFFI.
(defun %put-cstring (ptr str &optional (offset 0))
  (setf (%get-cstring (%inc-ptr ptr offset)) str)
  ;; 0 is the traditional, not-very-useful return value ...
  0)






;;; Returns a simple string and adjusted start and end, such that
;;; 0<= start <= end <= (length simple-string).
(defun get-sstring (str &optional (start 0) (end (length (require-type str 'string))))
  (multiple-value-bind (sstr offset) (array-data-and-offset (string str))
    (setq start (+ start offset) end (+ end offset))
    (when (< (length sstr) end)(setq end (length sstr)))
    (when (< end start) (setq start end))
    (values sstr start end)))

;e.g. (bad-named-arg :key key 'function)
(defun bad-named-arg (name arg &optional (type nil type-p))
  (if type-p
    (%err-disp $err-bad-named-arg-2 name arg type)
    (%err-disp $err-bad-named-arg name arg)))

(defun verify-arg-count (call min &optional max)
  "If call contains less than MIN number of args, or more than MAX
   number of args, error. Otherwise, return call.
   If Max is NIL, the maximum args for the fn are infinity."
 (or (verify-call-count (car call) (%cdr call) min max) call))

(defun verify-call-count (sym args min &optional max &aux argcount)
  (if (%i< (setq argcount  (list-length args)) min)
    (%err-disp $xtoofew (cons sym args))
    (if (if max (%i> argcount max))
      (%err-disp $xtoomany (cons sym args)))))

(defun getf (place key &optional (default ()))
  "Search the property list stored in Place for an indicator EQ to INDICATOR.
  If one is found, return the corresponding value, else return DEFAULT."
  (let ((p (pl-search place key))) (if p (%cadr p) default)))

(defun remprop (symbol key)
  "Look on property list of SYMBOL for property with specified
  INDICATOR. If found, splice this indicator and its value out of
  the plist, and return the tail of the original list starting with
  INDICATOR. If not found, return () with no side effects.

  NOTE: The ANSI specification requires REMPROP to return true (not false)
  or false (the symbol NIL). Portable code should not rely on any other value."
  (do* ((prev nil plist)
        (plist (symbol-plist symbol) tail)
        (tail (cddr plist) (cddr tail)))
       ((null plist))
    (when (eq (car plist) key)
      (if prev
        (rplacd (cdr prev) tail)
        (setf (symbol-plist symbol) tail))
      (return t))))



;;; If this returns non-nil, safe to do %rplaca of %cdr to update.
(defun pl-search (plist key)
  (unless (plistp plist)
    (report-bad-arg plist '(satisfies plistp)))
  (%pl-search plist key))


(defun rassoc (item alist &key (test #'eql test-p) test-not (key #'identity))
  (declare (list alist))
  "Return the cons in ALIST whose CDR is equal (by a given test or EQL) to
   the ITEM."
  (if (or test-p (not test-not))
    (progn
      (if test-not (error "Cannot specify both :TEST and :TEST-NOT."))
      (dolist (pair alist)
        (if (atom pair)
          (if pair (error "Invalid alist containing ~S: ~S" pair alist))
          (when (funcall test item (funcall key (cdr pair))) (return pair)))))
    (progn
      (unless test-not (error "Must specify at least one of :TEST or :TEST-NOT"))
      (dolist (pair alist)
        (if (atom pair)
          (if pair (error "Invalid alist containing ~S: ~S" pair alist))
          (unless (funcall test-not item (funcall key (cdr pair))) (return pair)))))))

(defun *%saved-method-var%* ()
  (declare (special %saved-method-var%))
  %saved-method-var%)

(defun set-*%saved-method-var%* (new-value)
  (declare (special %saved-method-var%))
  (setq %saved-method-var% new-value))

(defsetf *%saved-method-var%* set-*%saved-method-var%*)






(setf (symbol-function 'clear-type-cache) #'false)      ; bootstrapping

(defun make-array-1 (dims element-type element-type-p
                          displaced-to
                          displaced-index-offset
                          adjustable
                          fill-pointer
                          initial-element initial-element-p
                          initial-contents initial-contents-p
                          size)
  (let ((subtype (element-type-subtype element-type)))
    (when (and element-type (null subtype))
      (error "Unknown element-type ~S" element-type))
    (when (null size)
      (cond ((listp dims)
             (setq size 1)
             (dolist (dim dims)
               (when (< dim 0)
                 (report-bad-arg dim '(integer 0 *)))
               (setq size (* size dim))))
            (t (setq size dims)))) ; no need to check vs. array-dimension-limit
    (cond
     (displaced-to
      (when (or initial-element-p initial-contents-p)
        (error "Cannot specify initial values for displaced arrays"))
      (when (and element-type-p
                 (neq (array-element-subtype displaced-to) subtype))
        (error "The ~S array ~S is not of ~S ~S"
               :displaced-to displaced-to :element-type element-type))
      (%make-displaced-array dims displaced-to
                             fill-pointer adjustable displaced-index-offset t))
     (t
      (when displaced-index-offset
        (error "Cannot specify ~S for non-displaced-array" :displaced-index-offset))
      (when (null subtype)
        (error "Cannot make an array of empty type ~S" element-type))
      (make-uarray-1 subtype dims adjustable fill-pointer 
                     initial-element initial-element-p
                     initial-contents initial-contents-p
                     nil size)))))

(defun %make-simple-array (subtype dims)
  (let* ((size (if (listp dims) (apply #'* dims) dims))
         (vector (%alloc-misc size subtype)))
    (if (and (listp dims)
             (not (eql (length dims) 1)))
      (let* ((array (%make-displaced-array dims vector)))
        (%set-simple-array-p array)
        array)
      vector)))

(defun make-uarray-1 (subtype dims adjustable fill-pointer
                              initial-element initial-element-p
                              initial-contents initial-contents-p
                              temporary 
                              size)
  (declare (ignore temporary))
  (when (null size)(setq size (if (listp dims)(apply #'* dims) dims)))
  (let ((vector (%alloc-misc size subtype)))  ; may not get here in that case
    (if initial-element-p
      (cond ((and (eql subtype target::subtag-simple-base-string)
                  (eql initial-element #\Null)))
            ((and (eql subtype target::subtag-double-float-vector)
                  (eql initial-element 0.0d0)))
            ((and (eql subtype target::subtag-single-float-vector)
                  (eql initial-element 0.0s0)))
            (t (or (eql initial-element 0)
                   (%init-misc initial-element vector))))
      (if initial-contents-p
        (if (null dims) (uvset vector 0 initial-contents)
            (init-uvector-contents vector 0 dims initial-contents))))
    (if (and (null fill-pointer)
             (not adjustable)
             dims
             (or (atom dims) (null (%cdr dims))))
      vector
      (let ((array (%make-displaced-array dims vector 
                                          fill-pointer adjustable nil)))
        (when (and (null fill-pointer) (not adjustable))
          (%set-simple-array-p array))
        array))))

(defun init-uvector-contents (vect offset dims contents
                              &aux (len (length contents)))
  "Returns final offset. Assumes dims not ()."
  (unless (eq len (if (atom dims) dims (%car dims)))
    (error "~S doesn't match array dimensions of ~S ."  contents vect))
  (cond ((or (atom dims) (null (%cdr dims)))
         (if (listp contents)
           (let ((contents-tail contents))
             (dotimes (i len)
               (declare (fixnum i))
               (uvset vect offset (pop contents-tail))
               (setq offset (%i+ offset 1))))
           (dotimes (i len)
             (declare (fixnum i))
             (uvset vect offset (elt contents i))
             (setq offset (%i+ offset 1)))))
        (t (setq dims (%cdr dims))
           (if (listp contents)
             (let ((contents-tail contents))
               (dotimes (i len)
                 (declare (fixnum i))
                 (setq offset
                       (init-uvector-contents vect offset dims (pop contents-tail)))))
             (dotimes (i len)
               (declare (fixnum i))
               (setq offset
                     (init-uvector-contents vect offset dims (elt contents i)))))))
  offset)

(defun %get-signed-long-long (ptr &optional (offset 0))
  (%%get-signed-longlong ptr offset))

(defun %set-signed-long-long (ptr arg1
				  &optional
				  (arg2 (prog1 arg1 (setq arg1 0))))
  (%%set-signed-longlong ptr arg1 arg2))
				  
(defun %get-unsigned-long-long (ptr &optional (offset 0))
  (%%get-unsigned-longlong ptr offset))

(defun %set-unsigned-long-long (ptr arg1
				  &optional
				  (arg2 (prog1 arg1 (setq arg1 0))))
  (%%set-unsigned-longlong ptr arg1 arg2))

(defun %composite-pointer-ref (size pointer offset)
  (declare (ignorable size))
  (%inc-ptr pointer offset))

(defun %set-composite-pointer-ref (size pointer offset new)
  (#_memmove (%inc-ptr pointer offset)
             new
             size))


(defsetf %composite-pointer-ref %set-composite-pointer-ref)


(defsetf pathname-encoding-name set-pathname-encoding-name)

;end of L1-utils.lisp

