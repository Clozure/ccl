;;;-*-Mode: LISP; Package: CL-DOCUMENTATION -*-

;;; ----------------------------------------------------------------------------
;;;
;;;      cl-documentation.lisp
;;;
;;;      copyright (c) 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      This code is moronically simple, but surprisingly useful.
;;;      It adds a documentation tool for CL functions to the Context-Menu mechanism.
;;;      Right-Click displays a list of submenus.  The submenus are functional groups.
;;;      Popping the submenu displays entries for all CL functions belonging to that
;;;      functional group.  Selecting a function open a documentation dialog.
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History, most recent first:
;;;      9/2/9   Added a second menu, providing an alphabetical index.
;;;      8/31/9  version 0.1b1
;;;              First cut.
;;;
;;; ----------------------------------------------------------------------------

(defpackage "CL-DOCUMENTATION" (:nicknames "CLDOC") (:use :cl :ccl))
(in-package "CL-DOCUMENTATION")

(require :context-menu-cm)
(cmenu:check-hyperspec-availability "CL-Documentation-CM")

(defparameter *cl-documentation-menu* nil "The cl-documentation-menu instance.")
(defparameter *cl-alphabetical-menu* nil "The cl-alphabetical-menu instance.")


;;; ----------------------------------------------------------------------------
;;;
(defClass CL-DOCUMENTATION-MENU (ns:ns-menu) 
  ((tool-menu :initform nil :accessor tool-menu)
   (sub-title :initform "functional groups" :reader sub-title)
   (doc-path :initform (merge-pathnames ";ReadMe.rtf" cl-user::*cl-documentation-directory*) :reader doc-path)
   (text-view :initform nil :accessor text-view))
  (:documentation "A menu containing CL functions sorted into functional groups.")
  (:metaclass ns:+ns-object))

(objc:defmethod (#/clDocumentationAction: :void) ((m cl-documentation-menu) (sender :id))
  (display-cl-doc (item-symbol sender) (text-view m)))

(objc:defmethod (#/update :void) ((m cl-documentation-menu))
  (cmenu:update-tool-menu m (tool-menu m) :sub-title (sub-title m))
  (call-next-method))

(defmethod initialize-instance :after ((m cl-documentation-menu) &key)
  (setf (tool-menu m) (cmenu:add-default-tool-menu m :doc-file (doc-path m))))

(defun display-cl-doc (symbol text-view)
  "Display the documentation for SYMBOL."
  ;; If Hemlock-Commands is loaded, this will be
  ;; redefined there to use the documentation dialog.
  (gui::lookup-hyperspec-symbol symbol text-view))

(setq *cl-documentation-menu* (make-instance 'cl-documentation-menu))

;;; ----------------------------------------------------------------------------
;;;
(defClass CL-CATEGORY-MENU-ITEM (ns:ns-menu-item) 
  ((symbol :initform nil :accessor item-symbol))
  (:documentation "Support for the documentation menu.")
  (:metaclass ns:+ns-object))

(defun populate-submenu (menu symbol-list)
  "Create and add menu-items for each functional group in SYMBOL-LIST."
  (dolist (sym symbol-list)
    (let ((menu-item (make-instance 'cl-category-menu-item))
          (attributed-string (#/initWithString:attributes:
                              (#/alloc ns:ns-attributed-string) 
                              (ccl::%make-nsstring (string-downcase (string sym)))
                              cmenu:*hemlock-menu-dictionary*)))
      (#/setAttributedTitle: menu-item attributed-string)
      (#/setAction: menu-item (ccl::@selector "clDocumentationAction:"))
      (#/setTarget: menu-item  *cl-documentation-menu*)
      (setf (item-symbol menu-item) sym)
      (#/addItem: menu menu-item))))

(defun make-submenu-item (title symbol-list)
  "Create a menu-item with a submenu and populate the submenu based on SYMBOL-LIST."
  (let ((menu-item (make-instance ns:ns-menu-item))
        (attributed-string (#/initWithString:attributes:
                            (#/alloc ns:ns-attributed-string) 
                            (ccl::%make-nsstring title)
                            cmenu:*hemlock-menu-dictionary*))
        (submenu (make-instance ns:ns-menu)))
    (#/setAttributedTitle: menu-item attributed-string)
    (#/setSubmenu: menu-item submenu)
    (populate-submenu submenu symbol-list)
    menu-item))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *evaluation-and-compilation-symbol-list*
  (list 'compile 'compiler-macro-function 'constantp 'declaim 'declare 'define-compiler-macro 
        'define-symbol-macro 'defmacro 'eval 'eval-when 'lambda 'load-time-value 'locally 
        'macroexpand 'macroexpand-1 'macro-function 'proclaim 'special-operator-p 'symbol-macrolet
        'the 'quote))

(defParameter *evaluation-and-compilation*
  (make-submenu-item "evaluation and compilation" *evaluation-and-compilation-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *types-and-classes-symbol-list*
  (list 'coerce 'deftype 'subtypep 'type-error-datum 'type-error-expected-type 'type-of 'typep))

(defParameter *types-and-classes*
  (make-submenu-item "types and classes" *types-and-classes-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *control-and-data-flow-symbol-list*
  (list 'and 'apply 'block 'case 'catch 'ccase 'compiled-function-p 'complement 'cond 
        'constantly 'ctypecase 'defconstant 'define-modify-macro 'define-setf-expander 
        'defparameter 'defsetf 'defun 'defvar 'destructuring-bind 'ecase 'eq 'eql 'equal 'equalp 
        'etypecase 'every 'fboundp 'fdefinition 'flet 'fmakunbound 'funcall 'function 
        'function-lambda-expression 'functionp 'labels 'get-setf-expansion 'go 'identity 'if 
        'let 'let* 'macrolet 'multiple-value-bind 'multiple-value-call 'multiple-value-list 
        'multiple-value-prog1 'multiple-value-setq 'not 'notany 'notevery 'nth-value 'or 'prog 
        'prog* 'prog1 'prog2 'progn 'progv 'psetf 'psetq 'return 'return-from 'rotatef 'setf 
        'setq 'shiftf 'some 'tagbody 'throw 'typecase 'unless 'unwind-protect 'values 
        'values-list 'when))

(defParameter *control-and-data-flow*
  (make-submenu-item "control and data flow" *control-and-data-flow-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *iteration-symbol-list*
  (list 'do 'do* 'dolist 'dotimes 'loop))

(defParameter *iteration*
  (make-submenu-item "iteration" *iteration-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *objects-symbol-list*
  (list 'add-method 'allocate-instance 'call-method 'call-next-method 'change-class 'class-name 
        'class-of 'compute-applicable-methods 'defclass 'defgeneric 'define-method-combination 
        'defmethod 'ensure-generic-function 'find-class 'find-method 'function-keywords 
        'initialize-instance 'make-instance 'make-instances-obsolete 'make-load-form 
        'make-load-form-saving-slots 'method-qualifiers 'next-method-p 'no-applicable-method 
        'no-next-method 'reinitialize-instance 'remove-method 'shared-initialize 'slot-boundp 
        'slot-exists-p 'slot-makunbound 'slot-missing 'slot-unbound 'slot-value 'with-accessors 
        'with-slots 'unbound-slot-instance 'update-instance-for-different-class 
        'update-instance-for-redefined-class))

(defParameter *objects*
  (make-submenu-item "objects" *objects-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *structures-symbol-list*
  (list 'copy-structure 'defstruct))

(defParameter *structures*
  (make-submenu-item "structures" *structures-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *conditions-symbol-list*
  (list 'abort 'assert 'break 'cell-error-name 'cerror 'check-type 'compute-restarts 'continue 
        'define-condition 'error 'find-restart 'handler-bind 'handler-case 'ignore-errors 
        'invalid-method-error 'invoke-debugger 'invoke-restart 'invoke-restart-interactively 
        'make-condition 'method-combination-error 'muffle-warning 'restart-bind 'restart-case 
        'restart-name 'signal 'simple-condition-format-arguments 'simple-condition-format-control 
        'store-value 'use-value 'warn 'with-condition-restarts 'with-simple-restart))

(defParameter *conditions*
  (make-submenu-item "conditions" *conditions-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *symbols-symbol-list*
  (list 'boundp 'copy-symbol 'gensym 'gentemp 'get 'keywordp 'make-symbol 'makunbound 'set 
        'symbol-function 'symbol-name 'symbolp 'symbol-package 'symbol-plist 'symbol-value 
        'remprop))

(defParameter *symbols*
  (make-submenu-item "symbols" *symbols-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *packages-symbol-list*
  (list 'defpackage 'delete-package 'do-all-symbols 'do-external-symbols 'do-symbols 'export 
        'find-all-symbols 'find-package 'find-symbol 'import 'in-package 'intern 
        'list-all-packages 'make-package 'package-error-package 'package-name 'package-nicknames 
        'packagep 'package-shadowing-symbols 'package-used-by-list 'package-use-list 
        'rename-package 'shadow 'shadowing-import 'unexport 'unintern 'unuse-package 
        'with-package-iterator))

(defParameter *packages*
  (make-submenu-item "packages" *packages-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *numbers-symbol-list*
  (list 'abs 'acos 'acosh 'arithmetic-error-operands 'arithmetic-error-operation 'ash 'asin 
        'asinh 'atan 'atanh 'boole 'byte 'byte-position 'byte-size 'ceiling 'cis 'complex 
        'complexp 'conjugate 'cos 'cosh 'decf 'decode-float 'denominator 'deposit-field 'dpb 
        'evenp 'exp 'expt 'fceiling 'ffloor 'float 'float-digits 'floatp 'float-precision 
        'float-radix 'float-sign 'floor 'fround 'ftruncate 'gcd 'imagpart 'incf 
        'integer-decode-float 'integer-length 'integerp 'isqrt 'lcm 'ldb 'ldb-test 'log 'logand 
        'logandc1 'logandc2 'logbitp 'logcount 'logeqv 'logior 'lognand 'lognor 'lognot 'logorc1 
        'logorc2 'logtest 'logxor 'make-random-state 'mask-field 'max 'min 'minusp 'mod 'numberp 
        'numerator 'oddp 'parse-integer 'phase 'plusp 'random 'random-state-p 'rational 
        'rationalize 'rationalp 'realp 'realpart 'rem 'round 'scale-float 'signum 'sin 'sinh 
        'sqrt 'tan 'tanh 'truncate 'upgraded-complex-part-type 'zerop '= '/= '> '< '<= '>= '* 
        '+ '- '/ '1+ '1- ))

(defParameter *numbers*
  (make-submenu-item "numbers" *numbers-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *characters-symbol-list*
  (list 'alpha-char-p 'both-case-p 'alphanumericp 'character 'characterp 'char-code 
        'char-downcase 'char-greaterp 'char-equal 'char-int 'char-lessp 'char-name 
        'char-not-greaterp 'char-not-equal 'char-not-lessp 'char-upcase 'char= 'char/= 
        'char> 'char< 'char<= 'char>= 'code-char 'digit-char 'digit-char-p 'graphic-char-p 
        'lower-case-p 'name-char 'standard-char-p 'upper-case-p))

(defParameter *characters*
  (make-submenu-item "characters" *characters-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *conses-symbol-list*
  (list 'acons 'adjoin 'append 'assoc 'assoc-if 'assoc-if-not 'atom 'butlast 'nbutlast 'car 'cdr 
        'cons 'consp 'copy-alist 'copy-list 'copy-tree 'endp 'first 'getf 'get-properties 
        'intersection 'nintersection 'last 'ldiff 'list 'list-length 'listp 'make-list 'mapc 
        'mapcan 'mapcar 'mapcon 'mapl 'maplist 'member 'member-if 'member-if-not 'nconc 'nth 
        'nthcdr 'null 'pairlis 'pop 'push 'pushnew 'rassoc 'rassoc-if 'rassoc-if-not 'remf 'rest 
        'revappend 'nreconc 'rplaca 'rplacd 'set-difference 'nset-difference 'set-exclusive-or 
        'nset-exclusive-or 'sublis 'nsublis 'subsetp 'subst 'nsubst 'subst-if 'nsubst-if 
        'subst-if-not 'nsubst-if-not 'tailp 'tree-equal 'union 'nunion))

(defParameter *conses*
  (make-submenu-item "conses" *conses-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *arrays-symbol-list*
  (list  'adjustable-array-p 'adjust-array 'aref 'array-dimension 'array-dimensions  
         'array-displacement 'array-element-type 'array-has-fill-pointer-p 'array-in-bounds-p  
         'arrayp 'array-rank 'array-row-major-index 'array-total-size 'bit 'bit-and 'bit-andc1  
         'bit-andc2 'bit-eqv 'bit-ior 'bit-nand 'bit-nor 'bit-not 'bit-orc1 'bit-orc2 'bit-xor  
         'bit-vector-p 'fill-pointer 'make-array 'row-major-aref 'sbit 'simple-bit-vector-p  
         'simple-vector-p 'svref 'upgraded-array-element-type 'vector 'vectorp 'vector-pop  
         'vector-push 'vector-push-extend))

(defParameter *arrays*
  (make-submenu-item "arrays" *arrays-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *strings-symbol-list*
  (list 'char 'make-string 'schar 'simple-string-p 'string 'string-capitalize 'nstring-capitalize 
        'string-downcase 'nstring-downcase 'string-equal 'string-greaterp 'string-upcase 
        'nstring-upcase 'string-left-trim 'string-lessp 'string-not-equal 'string-not-greaterp 
        'string-not-lessp 'stringp 'string-right-trim 'string-trim 'string= 'string/= 'string< 
        'string> 'string<= 'string>=))

(defParameter *strings*
  (make-submenu-item "strings" *strings-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *sequences-symbol-list*
  (list 'concatenate 'copy-seq 'count 'count-if 'elt 'fill 'find 'find-if 'find-if-not 'length 
        'make-sequence 'map 'map-into 'merge 'mismatch 'position 'position-if 'position-if-not 
        'reduce 'remove 'delete 'remove-duplicates 'delete-duplicates 'remove-if 'delete-if 
        'remove-if-not 'delete-if-not 'replace 'reverse 'nreverse 'search 'sort 'stable-sort 
        'subseq 'substitute 'nsubstitute 'substitute-if 'nsubstitute-if 'substitute-if-not 
        'nsubstitute-if-not))

(defParameter *sequences*
  (make-submenu-item "sequences" *sequences-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *hash-tables-symbol-list*
  (list 'clrhash 'gethash 'hash-table-count 'hash-table-p 'hash-table-rehash-size 'hash-table-rehash-threshold 'hash-table-size 'hash-table-test 'make-hash-table 'maphash 'remhash 'sxhash 'with-hash-table-iterator))

(defParameter *hash-tables*
  (make-submenu-item "hash tables" *hash-tables-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *filenames-symbol-list*
  (list 'directory-namestring 'enough-namestring 'file-namestring 'host-namestring 
        'load-logical-pathname-translations 'logical-pathname 'logical-pathname-translations 
        'make-pathname 'merge-pathnames 'namestring 'parse-namestring 'pathname 'pathname-host 
        'pathname-device 'pathname-directory 'pathname-match-p 'pathname-name 'pathnamep 
        'pathname-type 'pathname-version 'translate-logical-pathname 'translate-pathname 
        'wild-pathname-p))

(defParameter *filenames*
  (make-submenu-item "filenames" *filenames-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *files-symbol-list*
  (list 'delete-file 'directory 'ensure-directories-exist 'file-author 'file-error-pathname 
        'file-write-date 'probe-file 'rename-file 'truename))

(defParameter *files*
  (make-submenu-item "files" *files-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *streams-symbol-list*
  (list 'broadcast-stream-streams 'clear-input 'clear-output 'close 'concatenated-stream-streams 
        'echo-stream-input-stream 'echo-stream-output-stream 'file-length 'file-position 
        'file-string-length 'finish-output 'force-output 'fresh-line 'get-output-stream-string 
        'input-stream-p 'interactive-stream-p 'listen 'make-broadcast-stream 
        'make-concatenated-stream 'make-echo-stream 'make-string-input-stream 
        'make-string-output-stream 'make-synonym-stream 'make-two-way-stream 'open 
        'open-stream-p 'output-stream-p 'peek-char 'read-byte 'read-char 'read-char-no-hang 
        'read-line 'read-sequence 'stream-element-type 'stream-error-stream 
        'stream-external-format 'streamp 'synonym-stream-symbol 'terpri 
        'two-way-stream-input-stream 'two-way-stream-output-stream 'unread-char 
        'with-input-from-string 'with-open-file 'with-open-stream 'with-output-to-string 
        'write-byte 'write-char 'write-line 'write-sequence 'write-string 'yes-or-no-p 'y-or-n-p))

(defParameter *streams*
  (make-submenu-item "streams" *streams-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *printer-symbol-list*
  (list 'copy-pprint-dispatch 'format 'formatter 'pprint 'pprint-dispatch 
        'pprint-exit-if-list-exhausted 'pprint-fill 'pprint-indent 'pprint-linear 
        'pprint-logical-block 'pprint-newline 'pprint-pop 'pprint-tab 'pprint-tabular 'princ 
        'princ-to-string 'print 'print-object 'print-not-readable-object 'print-unreadable-object 
        'prin1 'prin1-to-string 'set-pprint-dispatch 'write 'write-to-string))

(defParameter *printer*
  (make-submenu-item "printer" *printer-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *reader-symbol-list*
  (list 'copy-readtable 'get-dispatch-macro-character 'get-macro-character 
        'make-dispatch-macro-character 'read 'read-delimited-list 'read-from-string 
        'read-preserving-whitespace 'readtable-case 'readtablep 'set-dispatch-macro-character 
        'set-macro-character 'set-syntax-from-char 'with-standard-io-syntax))

(defParameter *reader*
  (make-submenu-item "reader" *reader-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *system-construction-symbol-list*
  (list 'copy-readtable 'get-dispatch-macro-character 'get-macro-character 
        'make-dispatch-macro-character 'read 'read-delimited-list 'read-from-string 
        'read-preserving-whitespace 'readtable-case 'readtablep 'set-dispatch-macro-character 
        'set-macro-character 'set-syntax-from-char 'with-standard-io-syntax))

(defParameter *system-construction*
  (make-submenu-item "system construction" *system-construction-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *environment-symbol-list*
  (list 'apropos 'apropos-list 'decode-universal-time 'describe 'describe-object 'disassemble 
        'documentation 'dribble 'ed 'encode-universal-time 'get-decoded-time 
        'get-internal-real-time 'get-internal-run-time 'get-universal-time 'inspect 
        'lisp-implementation-type 'lisp-implementation-version 'long-site-name 'machine-instance 
        'machine-type 'machine-version 'room 'short-site-name 'sleep 'software-type 
        'software-version 'step 'time 'trace 'untrace 'user-homedir-pathname))

(defParameter *environment*
  (make-submenu-item "environment" *environment-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *constants-and-variables-symbol-list*
  (list 'array-dimension-limit 'array-rank-limit 'array-total-size 'boole-1 '*break-on-signals* 
        'call-arguments-limit 'char-code-limit '*compile-file-pathname* '*compile-file-truename* 
        '*compile-print* '*compile-verbose* '*debug-io* '*debugger-hook* 
        '*default-pathname-defaults* 'short-float-epsilon 'single-float-epsilon 
        'double-float-epsilon 'long-float-epsilon 'short-float-negative-epsilon 
        'single-float-negative-epsilon 'double-float-negative-epsilon 
        'long-float-negative-epsilon '*error-output* '*features* '*gensym-counter* 
        'internal-time-units-per-second 'lambda-list-keywords 'lambda-parameters-limit 
        'least-negative-short-float 'least-negative-single-float 'least-negative-double-float 
        'least-negative-long-float 'least-negative-normalized-short-float 
        'least-negative-normalized-single-float 'least-negative-normalized-double-float 
        'least-negative-normalized-long-float 'least-positive-short-float 
        'least-positive-single-float 'least-positive-double-float 'least-positive-long-float
        'least-positive-normalized-short-float 'least-positive-normalized-single-float 
        'least-positive-normalized-double-float 'least-positive-normalized-long-float
        '*load-pathname* '*load-print* '*load-truename* '*load-verbose* '*macroexpand-hook* 
        '*modules* 'most-negative-fixnum 'most-negative-short-float 'most-negative-single-float 
        'most-negative-double-float 'most-negative-long-float 'most-positive-fixnum
        'most-positive-short-float 'most-positive-single-float 'most-positive-double-float 
        'most-positive-long-float 'multiple-values-limit 'nil '*package* 'pi '*print-array* 
        '*print-base* '*print-case* '*print-circle* '*print-escape* '*print-gensym* 
        '*print-length* '*print-level* '*print-lines* '*print-miser-width* 
        '*print-pprint-dispatch* '*print-pretty* '*print-radix* '*print-readably* 
        '*print-right-margin* '*query-io* '*random-state* '*read-base* 
        '*read-default-float-format* '*read-eval* '*read-suppress* '*readtable* 
        '*standard-input* '*standard-output* 't '*terminal-io* '*trace-output* 
        '* '** '*** '+ '++ '+++ '- '/ '// '///))

(defParameter *constants-and-variables*
  (make-submenu-item "constants and variables" *constants-and-variables-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *type-specifiers-symbol-list*
  (list 'and 'array 'simple-array 'base-string 'simple-base-string 'bit-vector 'simple-bit-vector 
        'complex 'cons 'eql 'float 'short-float 'single-float 'double-float 'long-float 'function 
        'integer 'member 'mod 'not 'or 'rational 'real 'satisfies 'signed-byte 'string 
        'simple-string 'unsigned-byte 'values 'vector 'simple-vector))

(defParameter *type-specifiers*
  (make-submenu-item "type specifiers" *type-specifiers-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defun add-cl-documentation-submenus (menu)
  (let ((submenus '(*evaluation-and-compilation* *types-and-classes* 
                                                 *control-and-data-flow* *iteration* *objects* *structures* 
                                                 *conditions* *symbols* *packages* *numbers* *characters* 
                                                 *conses* *arrays* *strings* *sequences* *hash-tables* 
                                                 *filenames* *files* *streams* *printer* *reader* 
                                                 *system-construction* *environment* *constants-and-variables* 
                                                 *type-specifiers*)))
    (dolist (submenu submenus)
      (#/addItem: menu (symbol-value submenu)))))

;;; ----------------------------------------------------------------------------
;;;
(defParameter *cl-symbol-lists*
  (list 
   *evaluation-and-compilation-symbol-list* *types-and-classes-symbol-list* 
   *control-and-data-flow-symbol-list* *iteration-symbol-list* *objects-symbol-list* 
   *structures-symbol-list* *conditions-symbol-list* *symbols-symbol-list* *packages-symbol-list* 
   *numbers-symbol-list* *characters-symbol-list* *conses-symbol-list* *arrays-symbol-list* 
   *strings-symbol-list* *sequences-symbol-list* *hash-tables-symbol-list* *filenames-symbol-list* 
   *files-symbol-list* *streams-symbol-list* *printer-symbol-list* *reader-symbol-list* 
   *system-construction-symbol-list* *environment-symbol-list* 
   *constants-and-variables-symbol-list* *type-specifiers-symbol-list*))

;;; ----------------------------------------------------------------------------
;;;
(defun test-symbol-list (sym-list &optional package)
  (dolist (sym (rest sym-list))
    (unless (find-symbol (string-upcase (format nil "~A" sym)) (or package :cl))
      (format t "~%~A" sym))))

;;; (dolist (list *cl-symbol-lists*) (test-symbol-list list)) 

(add-cl-documentation-submenus *cl-documentation-menu*)

(defun get-cl-documentation-menu (view event) 
  (cond ((logtest #$NSCommandKeyMask (#/modifierFlags event))
         (setf (text-view *cl-alphabetical-menu*) view)           
         *cl-alphabetical-menu*)
        (t
         (setf (text-view *cl-documentation-menu*) view)           
         *cl-documentation-menu*)))

(cmenu:register-tool "CL-Documentation-CM" #'get-cl-documentation-menu)
