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

(in-package "CCL")

(eval-when (:compile-toplevel :execute :load-toplevel)

(defconstant most-positive-short-float (make-short-float-from-fixnums (1- (ash 1 23)) 254 0))
(defconstant most-negative-short-float (make-short-float-from-fixnums (1- (ash 1 23)) 254 -1))
(defconstant most-positive-single-float (make-short-float-from-fixnums (1- (ash 1 23)) 254 0))
(defconstant most-negative-single-float (make-short-float-from-fixnums (1- (ash 1 23)) 254 -1))


(defconstant least-positive-short-float (make-short-float-from-fixnums 1 0 0))
(defconstant least-negative-short-float (make-short-float-from-fixnums 1 0 -1))
(defconstant least-positive-single-float (make-short-float-from-fixnums 1 0 0))
(defconstant least-negative-single-float (make-short-float-from-fixnums 1 0 -1))

(defconstant short-float-epsilon (make-short-float-from-fixnums 1 103 0))
(defconstant short-float-negative-epsilon (make-short-float-from-fixnums 1 102 0))
(defconstant single-float-epsilon (make-short-float-from-fixnums 1 103 0))
(defconstant single-float-negative-epsilon (make-short-float-from-fixnums 1 102 0))

(defconstant least-positive-normalized-short-float (make-short-float-from-fixnums 1 1 0))
(defconstant least-negative-normalized-short-float (make-short-float-from-fixnums 1 1 -1))
(defconstant least-positive-normalized-single-float (make-short-float-from-fixnums 1 1 0))
(defconstant least-negative-normalized-single-float (make-short-float-from-fixnums 1 1 -1))

(let ((bigfloat (make-float-from-fixnums #x1ffffff #xfffffff #x7fe 0)))
  ; do it this way if you want to be able to compile before reading floats works  
  (defconstant most-positive-double-float bigfloat)
  (defconstant most-positive-long-float bigfloat)
  )

(let ((littleposfloat (make-float-from-fixnums 0 1 0 0 )))
  (defconstant least-positive-double-float littleposfloat)
  (defconstant least-positive-long-float littleposfloat)
  )

(let ((littlenegfloat (make-float-from-fixnums 0 1 0 -1)))  
  (defconstant least-negative-double-float littlenegfloat)
  (defconstant least-negative-long-float littlenegfloat)
  )

(let ((bignegfloat (make-float-from-fixnums #x1ffffff #xfffffff #x7fe -1)))
  (defconstant most-negative-double-float bignegfloat)
  (defconstant most-negative-long-float bignegfloat)
  )

(let ((eps (make-float-from-fixnums #x1000000 1 #x3ca 0))) ;was wrong
  (defconstant double-float-epsilon eps)
  (defconstant long-float-epsilon eps)
  )

(let ((eps- (make-float-from-fixnums #x1000000 1 #x3c9 1)))
  (defconstant double-float-negative-epsilon eps-)
  (defconstant long-float-negative-epsilon eps-)
  )

(let ((norm (make-float-from-fixnums 0 0 1 0)))
  (defconstant least-positive-normalized-double-float norm)
  (defconstant least-positive-normalized-long-float norm)
  )

(let ((norm- (make-float-from-fixnums 0 0 1 -1)))
  (defconstant least-negative-normalized-double-float norm-)
  (defconstant least-negative-normalized-long-float norm-)
  )

(defconstant pi (make-float-from-fixnums #x921fb5 #x4442d18 #x400 0))

)



(defconstant boole-clr 0
  "Boole function op, makes BOOLE return 0.")
(defconstant boole-set 1
  "Boole function op, makes BOOLE return -1.")
(defconstant boole-1 2
  "Boole function op, makes BOOLE return integer1.")
(defconstant boole-2 3
  "Boole function op, makes BOOLE return integer2.")
(defconstant boole-c1 4
  "Boole function op, makes BOOLE return complement of integer1.")
(defconstant boole-c2 5
  "Boole function op, makes BOOLE return complement of integer2.")
(defconstant boole-and 6
  "Boole function op, makes BOOLE return logand of integer1 and integer2.")
(defconstant boole-ior 7
  "Boole function op, makes BOOLE return logior of integer1 and integer2.")
(defconstant boole-xor 8
  "Boole function op, makes BOOLE return logxor of integer1 and integer2.")
(defconstant boole-eqv 9
  "Boole function op, makes BOOLE return logeqv of integer1 and integer2.")
(defconstant boole-nand 10
  "Boole function op, makes BOOLE return log nand of integer1 and integer2.")
(defconstant boole-nor 11
  "Boole function op, makes BOOLE return lognor of integer1 and integer2.")
(defconstant boole-andc1 12
  "Boole function op, makes BOOLE return logandc1 of integer1 and integer2.")
(defconstant boole-andc2 13
  "Boole function op, makes BOOLE return logandc2 of integer1 and integer2.")
(defconstant boole-orc1 14
  "Boole function op, makes BOOLE return logorc1 of integer1 and integer2.")
(defconstant boole-orc2 15
  "Boole function op, makes BOOLE return logorc2 of integer1 and integer2.")



(defconstant internal-time-units-per-second #+64-bit-target 1000000 #-64-bit-target 1000
  "The number of internal time units that fit into a second. See
  GET-INTERNAL-REAL-TIME and GET-INTERNAL-RUN-TIME.")

(defconstant char-code-limit #.(arch::target-char-code-limit
                                (backend-target-arch *target-backend*))
  "the upper exclusive bound on values produced by CHAR-CODE")

(defconstant array-rank-limit (floor #x8000 target::node-size)
  "the exclusive upper bound on the rank of an array")
(defconstant multiple-values-limit 200
  "The exclusive upper bound on the number of multiple VALUES that you can
  return.")
(defconstant lambda-parameters-limit (floor #x8000 target::node-size)
  "The exclusive upper bound on the number of parameters which may be specifed
  in a given lambda list. This is actually the limit on required and &OPTIONAL
  parameters. With &KEY and &AUX you can get more.")
(defconstant call-arguments-limit (floor #x8000 target::node-size)
  "The exclusive upper bound on the number of arguments which may be passed
  to a function, including &REST args."
)

; Currently, vectors can be at most (expt 2 22) bytes, and
; the largest element (double-float or long-float) is 8 bytes:
#| to get largest element size...
(apply #'max (mapcar #'(lambda (type)
                         (%vect-byte-size (make-array 1 :element-type type)))
                     *cl-types*))
|#

(defconstant array-dimension-limit array-total-size-limit
  "the exclusive upper bound on any given dimension of an array")

(defconstant most-positive-fixnum target::target-most-positive-fixnum
  "the fixnum closest in value to positive infinity")
(defconstant most-negative-fixnum target::target-most-negative-fixnum
  "the fixnum closest in value to negative infinity")

(defstatic *least-positive-bignum* (1+ target::target-most-positive-fixnum)
  "used internally; value should be treated as a constant")


(defconstant lambda-list-keywords 
  '(&OPTIONAL &REST &AUX &KEY &ALLOW-OTHER-KEYS &BODY &ENVIRONMENT &WHOLE)
  "symbols which are magical in a lambda list")

(defstatic *type-system-initialized* nil)

(defparameter %toplevel-catch% ':toplevel)

(defvar *read-default-float-format* 'single-float)

(defvar *read-suppress* nil
  "Suppress most interpreting in the reader when T.")

(defvar *read-base* 10.
  "the radix that Lisp reads numbers in")


(defparameter *warn-if-redefine-kernel* nil
  "When true, attempts to redefine (via DEFUN or DEFMETHOD) functions and
methods that are marked as being predefined signal continuable errors.")

(defvar *next-screen-context-lines* 2 "Number of lines to show of old screen
  after a scroll-up or scroll-down.")

(defparameter *compiling-file* nil 
  "Name of outermost file being compiled or NIL if not compiling a file.")

(defvar *eval-fn-name* nil)


(defvar *compile-definitions* t
  "When non-NIL and the evaluator's lexical environment contains no
  lexical entities, causes FUNCTION and NFUNCTION forms to be compiled.")
#|
(defvar *fast-eval* ()
  "If non-nil, compile-and-call any forms which would be expensive to evaluate.")
|#
(defvar *declaration-handlers* ())


(defvar *lisp-system-pointer-functions* nil)
(defvar *lisp-user-pointer-functions* nil)
(defvar *lisp-cleanup-functions* nil)   ; list of (0-arg) functions to call before quitting Lisp
(defvar *lisp-startup-functions* nil)   ; list of funs to call after startup.
(defvar %lisp-system-fixups% nil)


(setf (*%saved-method-var%*) nil)

; The GC expects these to be NIL or a function of no args
(defvar *pre-gc-hook* nil)
(defvar *post-gc-hook* nil)

; These are used by add-gc-hook, delete-gc-hook
(defvar *pre-gc-hook-list* nil)
(defvar *post-gc-hook-list* nil)

(defvar *backtrace-dialogs* nil)
;(defvar *stepper-running* nil)
(defparameter *last-mouse-down-time* 0)
(defparameter *last-mouse-down-position* 0)

(defvar %handlers% ())


#|
(defvar %restarts% (list (list (%cons-restart 'abort
                                              #'(lambda (&rest ignore)
                                                  (declare (ignore ignore))
                                                  (throw :toplevel nil))
                                              "Restart the toplevel loop."
                                              nil
                                              nil))))
|#

(defvar %restarts% nil)

(defvar ccl::*kernel-restarts* nil)
(defvar *condition-restarts* nil "explicit mapping between c & r")
(declaim (list %handlers% %restarts% ccl::*kernel-restarts* *condition-restarts*))




(defparameter *%periodic-tasks%* nil)
(defparameter *dribble-stream* nil)

(defconstant *keyword-package* *keyword-package*)
(defconstant *common-lisp-package* *common-lisp-package*)
(defconstant *ccl-package* *ccl-package*)

(defparameter *load-print* nil "the default for the :PRINT argument to LOAD")
(defparameter *loading-files* nil)
(defparameter *break-level* 0)
(defparameter *last-break-level* 0)
(defparameter *warn-if-redefine* nil)
(defvar *record-source-file*)           ; set in l1-utils.
(defparameter *level-1-loaded* nil)     ; set t by l1-boot
(defparameter *save-definitions* nil)
(defparameter *save-local-symbols* t)
(defparameter *save-source-locations* T
  "Controls whether source location information is saved, both for definitions (names) and
in function objects.

If NIL we don't store any source locations (other than the filename if *record-source-file* is non-NIL).

If T we store as much source location information as we have available.

If :NO-TEXT we don't store a copy of the original source text.  This is a space optimization useful
for compiling files that are not expected to change.")

(defparameter *record-pc-mapping* t "True to record pc -> source mapping (but only if
*save-source-locations* is also true)")

(defvar *modules* nil
  "This is a list of module names that have been loaded into Lisp so far.
   The names are case sensitive strings.  It is used by PROVIDE and REQUIRE.")





(defparameter *eof-value* (cons nil nil))

(defvar *gc-event-status-bits*)         ; also initialized by kernel

(defparameter *top-listener* nil)







(defvar *listener-indent* nil)

(defparameter *autoload-lisp-package* nil)   ; Make 'em suffer
(defparameter *apropos-case-sensitive-p* nil)

(defloadvar *total-gc-microseconds* (let* ((timeval-size
                                            #.(%foreign-type-or-record-size
                                               :timeval :bytes))
                                           (p (malloc (* 5 timeval-size))))
                                      (#_memset p 0 (* 5 timeval-size))
                                      p))


(defloadvar *total-bytes-freed* (let* ((p (malloc 8)))
                                  (setf (%get-long p 0) 0
                                        (%get-long p 4) 0)
                                  p))



(defvar *terminal-character-encoding-name* nil
  "NIL (implying :ISO-8859-1), or a keyword which names a defined
character encoding to be used for *TERMINAL-IO* and other predefined
initial streams.  The value of *TERMINAL-CHARACTER-ENCODING-NAME*
persists across calls to SAVE-APPLICATION; it can be specified via
the command-line argument --terminal-encoding (-K)")


(defconstant +null-ptr+ (%null-ptr))

;;; end of L1-init.lisp

