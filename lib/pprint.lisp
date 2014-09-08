;-*-syntax:COMMON-LISP;Package:"CCL"-*-

;;	Change History (most recent first):
;;  2 4/8/97   akh  pretty-loop dont loop
;;  3 12/13/95 Alice Hartley no call compiler at load time
;;  3 3/2/95   akh  will promote strings to fat strings if needed
;;  (do not edit before this line!!)


;------------------------------------------------------------------------

;Copyright 1989,1990 by the Massachusetts Institute of Technology, Cambridge, 
;Massachusetts.

;Permission to use, copy, modify, and distribute this software and its
;documentation for any purpose and without fee is hereby granted,
;provided that this copyright and permission notice appear in all
;copies and supporting documentation, and that the name of M.I.T. not
;be used in advertising or publicity pertaining to distribution of the
;software without specific, written prior permission. M.I.T. makes no
;representations about the suitability of this software for any
;purpose.  It is provided "as is" without express or implied warranty.

;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;    SOFTWARE.

;------------------------------------------------------------------------

;This file "XP.LISP" implements an efficient pretty printer for Common
;Lisp.  The functions in this file are documented fully in MIT/AIM-1102a, July
;1989.  This report can be obtained by sending $3.25 to

;              Publications
;	       MIT AI Laboratory
;	       545 Tech. Sq.
;	       Cambridge MA 02139

;This file attempts to be as compatible with pure Common Lisp as possible.
;It has been tested on the following Common Lisps to date (7/31/89).
;  Symbolics CL version 7 (does not work in version 6),
;  LUCID CL version 3.0.2 on a sun.
;  Allegro CL version 1.2.1 on a Macintosh.
;  CMU CL.

;The companion file "XPTEST.LISP" contains a set of 600+ tests.  You should
;run these tests after the first time you compile this file on a new system.

;The companion file "XPDOC.TXT" contains brief documentation
; 04/05/97 akh  pretty-loop fix for *print-level* exceeded
; 10/26/95 slh   %gvector -> %istruct
; 08/26/93 bill  indentation
; -------- 3.0d12
; 06/26/93 alice stream-fresh-line (xp-stream) was producing premature newlines
; 05/24/93 alice *free-xps* and *free-circularity-hash-tables* are global
; 03/04/93 alice set *error-print-circle* to T
; 02/23/93 alice get-printer - look in others table before def.., with.. hack
; 02/15/93 alice don't unwind-protect in pprint-logical-block+
; 12/21/92 alice lets not print loop as #'
; 06/23/92 alice change set-pprint-dispatch+ and priority-> so '(0) is less than 0
;--------------- 2.0
; 02/22/92 (alice from "post 2.0f2c5:pprint-defmethod-patch") fix DEFMETHOD-LIKE.
; -------- 2.0f2c5
; 01/29/92 gb    pretty-structure calls structure-print-function.
; -------- 2.0f2
; 10/11/91 alice dont print generic-function as #'
; 10/09/91 alice write+ don't deal with structures and arrays - prior fix was brain dead
;    p.s. technically we shouldn't special case strings, fixnums and symbols either
; 10/03/91 alice write+ - if print-object method for structure use it.
; 09/25/91 alice fix circularity-process so we can rebind *print-circle* in mid stream 
; 09/25/91 alice pretty-structure - no dangling space if no slots
; 09/24/91 alice fix pretty-structure bogus keyword printing
; 09/11/91 alice keep first pass output until first circularity in case no circularities
; 09/09/91 alice fix print circle in case circularity detected after first line (geez)
; 		dont die if *print-pprint-dispatch* is nil
;--------------- 2.0b3
; 08/21/91 gb xp-stream-stream
; 07/21/91 gb def-accessors vice defstruct.
; 07/09/91 alice allow write+ to tail call 
; 07/01/91 bind level and length as (f *print-readably*)
; 07/01/91 generic-function & reinstate some MLY hacks for "def.." "with-.." etc.
; 06/24/91 added pretty-structure
; 05/22/91 Modified for MCL 2.0b
;;;;;;;;;;;;;;
;;; lisp: => cl:
;;; string-char => character (or base-character?)
;;; #-ccl-2 compiled format and format and much else
;;;  put the xp-stream in the xp-structure
;;; write-char => write-char+ in pretty-loop
;;; nuke *last-abbreviated-printing*
;;; Teach it about fred-special-indent-alist
;;; in fred-alist 2 means defun-like, 0 is progn-like
;;;   3 is defsetf-print , 1 is block-like
;;; Put circularity table & number in the structure? - didn't do it
;;; Nuke the xp package
;;; Added progn-print
;;; MAYBELAB take xp-stream or xp-structure
;;; Gave up on #+/#-ccl-2
;;; Could save a few hundred bytes by (funcall (formatter ...)) to (format ... )) - maybe not
;;; The dispatch table could use some compacting: done!
;;;  an entry contains test: 0 - must be some predicate if not of the other form
;;;			fn: ok
;;;                     full-spec: '((0)(cons (member defvar)))
;;; Nuke *print-shared* and *parents*
;;; This version has a new special *xp-current-object* but doesnt gratuitously cons.
;;; Fixed circle doing it twice when it needn't (what does this break?)
;;; member => memq
;;; Apply optimizations as Mly did in prior conversion, i.e. liberal doses
;;; of %i+, (declare (fixnum ...)), dont fetch a stucture field 15 times
;;; when once will suffice, no char=, fewer position & find
;;; Now about same speed as old one. (actually 10% slower) & it conses less
;;; In pprint-dispatch just store the function if (cons (member blah)) & (0) or 0.
;;; nuke some entries in pprint-dispatch where same info is in fred-special-indent-alist
;;; Size is down from 23K larger to 18K larger.
;;; maybe-print-fast iff readtable-case is :upcase
;;; add defmethod-like for guess what
;;;  nuke *abbreviation-happened*


(in-package "CCL")

(defvar *ipd* nil ;see initialization at end of file.
  "initial print dispatch table.")

(eval-when (:compile-toplevel :execute)
  (declaim (inline xp-structure-p)))

(defun xp-structure-p (x)
  (istruct-typep x 'xp-structure))


(defun entry-p (x)
  (istruct-typep x 'entry))

  

;default (bad) definitions for the non-portable functions

(eval-when (:execute :load-toplevel :compile-toplevel)
(defun structure-type-p (x) (structurep x))
(defun output-width     (&optional (s *standard-output*))
  (when (streamp s)(line-length s)))
(defun output-position  (&optional (s *standard-output*))
  (when (streamp s)(column s)))
)

(defvar *logical-block-p* nil
  "True if currently inside a logical block.")

(defvar *locating-circularities* nil
  "Integer if making a first pass over things to identify circularities.
   Integer used as counter for #n= syntax.")

(def-standard-initial-binding *free-circularity-hash-tables* nil)

(defun get-circularity-hash-table ()
  (let ((table (pop *free-circularity-hash-tables*)))
    (if table table (make-hash-table :test 'eq))))

;If you call this, then the table gets efficiently recycled.
(defun free-circularity-hash-table (table)
  (clrhash table)
  (pushnew table *free-circularity-hash-tables*))


;                       ---- DISPATCHING ----

(cl:defstruct (pprint-dispatch-table (:conc-name nil) (:copier nil))
  (conses-with-cars (make-hash-table :test #'eq) :type (or null hash-table))
  (parent-table nil)
  (others nil :type list)
  (commit-hook nil))

;;; We'd of course get finer-grained locking if each dispatch-table had
;;; its own lock, but we want to make creation of a pprint-dispatch-table
;;; as cheap as we can make it
(defstatic *pprint-dispatch-table-lock* (make-lock))


(defmethod print-object ((dispatch pprint-dispatch-table) stream)
  (print-unreadable-object (dispatch stream :type t :identity t)))

(defstatic *standard-pprint-dispatch-table* nil) ;set below

;;;The list and the hash-tables contain entries of the
;;;following form.  When stored in the hash tables, the test entry is 
;;;the number of entries in the OTHERS list that have a higher priority.

(defun make-entry (&key test fn full-spec)
  (%istruct 'entry test fn full-spec))


(defun copy-pprint-dispatch-table-conses-with-cars (table)
  (let* ((old (conses-with-cars table)))
    (when old
      (let* ((new (make-hash-table :test #'eq :size (max (hash-table-count old) 32))))
        (maphash (lambda (key value)
                   (setf (gethash key new)
                         (if (istruct-typep value 'entry)(copy-uvector value) value)))
                 old)
        new))))

(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
  (if (null table)
    (make-pprint-dispatch-table
     :conses-with-cars nil
     :others (copy-list (others *ipd*))
     :parent-table *ipd*
     :commit-hook (commit-hook *ipd*))
    (let* ((table (require-type table 'pprint-dispatch-table)))
      (with-lock-grabbed (*pprint-dispatch-table-lock*)
        (make-pprint-dispatch-table
         :others (copy-list (others table))
         :conses-with-cars (copy-pprint-dispatch-table-conses-with-cars table)
         :commit-hook (commit-hook table)
         :parent-table (parent-table table)
         :commit-hook (commit-hook table))))))


(defun set-pprint-dispatch (type-specifier function
			    &optional (priority 0) (table *print-pprint-dispatch*))
  (when (or (not (numberp priority)) (complexp priority))
    (error "invalid PRIORITY argument ~A to SET-PPRINT-DISPATCH" priority))
  (when (eq table *standard-pprint-dispatch-table*)
    (error "The standard pprint dispatch table must never be modified."))
  (with-lock-grabbed (*pprint-dispatch-table-lock*)
    (let* ((parent (parent-table table)))
      (when parent
        (setf (conses-with-cars table)
              (copy-pprint-dispatch-table-conses-with-cars parent)
              (others table) (copy-list (others parent))
              (parent-table table) nil))
      (set-pprint-dispatch+ type-specifier function priority table))))


(defun set-pprint-dispatch+ (type-specifier function priority table)
  (let* ((category (specifier-category type-specifier))
	 (pred
	   (if (not (eq category 'other)) nil
	       (let ((pred (specifier-fn type-specifier)))
		 (if (symbolp pred)
                  (symbol-function pred)
                  ; checking for (lambda (x) (foo x)) => #'foo 
		  (if (and (consp (caddr pred))
			   (symbolp (caaddr pred)) 
			   (equal (cdaddr pred) '(x)))
                    (symbol-function (caaddr pred))
                    ; calling the compiler at load time is an indictable offense
                    (compile nil pred))))))
	 (entry (if function (make-entry :test pred
					 :fn function
					 :full-spec (list priority type-specifier)))))
    (case category
      (cons-with-car
       (let ((key (cadadr type-specifier)) ;(cons (member FOO))
             (cons-tbl (or (conses-with-cars table)
                           (setf (conses-with-cars table)
                                 (make-hash-table :test #'eq)))))
	(cond ((null function) (remhash key cons-tbl))
	      (T (let ((num 
		       (count-if #'(lambda (e)
				     (priority-> e priority))
				 (others table))))
                   (cond ((and (or ;(eq priority 0)
                                   (and (consp priority)(eq (%car priority) 0)))
                               (eq num 0))
                          (setq entry function))
                         (t (setf (entry-test entry) num)))
		   (setf (gethash key cons-tbl) entry))))))
      (T ;other
	 (let ((old (car (member type-specifier (others table) :test #'equal
				 :key #'(lambda (e) (cadr (entry-full-spec e)))))))
	   (when old
	     (setf (others table) (delete old (others table)))
	     (adjust-counts table (car (entry-full-spec old)) -1)))
	 (when entry
	   (let ((others (cons nil (others table))))
	      (do ((l others (cdr l)))
		  ((null (cdr l)) (rplacd l (list entry)))
		(when (priority-> priority (car (entry-full-spec (cadr l))))
		  (rplacd l (cons entry (cdr l)))
		  (return nil)))
	      (setf (others table) (cdr others)))
	   (adjust-counts table priority 1)))))
  nil)

(defun priority-> (entry-x entry-y)
  (flet ((foo (e)
              (cond ((istruct-typep e 'entry)(car (entry-full-spec e)))
                    ((or (numberp e)(consp  e)) e)
                    (t '(0)))))
    (let ((x (foo entry-x))
          (y (foo entry-y)))      
      (if (consp x)
        (if (consp y) (> (car x) (car y)) nil)
        (if (consp y) T (> x y))))))


(defun adjust-counts (table priority delta)
  (maphash #'(lambda (key value)
	       (when (priority-> priority value)
                 (when (not (istruct-typep value 'entry))
                   (setf (gethash key (conses-with-cars table))
                         (setq value (make-entry :fn value :test 0 :full-spec '(0)))))
                 (incf (entry-test value) delta)))
	   (conses-with-cars table)))

(defun pprint-dispatch (object &optional (table *print-pprint-dispatch*))
  (flet ((non-pretty-print (s object)
           (write-not-pretty s object
                             (if (get-*print-frob* '*print-level*)
                               (- *print-level* *current-level*))
                             nil nil)))
    (let ((fn (get-printer object table)))
      (values (or fn #'non-pretty-print) (not (null fn))))))

(defun get-printer-internal (object hash others)
  (let* (entry)
      (cond ((consp object)
             (setq entry (gethash (%car object) hash))
             (when (not entry)
               (setq entry (find object others :test #'fits))
               (if entry
                 (setq entry (entry-fn entry))))))
      (if (not entry)
        (setq entry (find object others :test #'fits))
        (if (istruct-typep entry 'entry)
          (let ((test (entry-test entry)))
            (when (numberp test)
              (do ((i test (1- i))
                   (l others (cdr l)))
                  ((zerop i))
                (when (fits object (car l)) (setq entry (car l)) (return nil)))))))
      (when entry
        (if (istruct-typep entry 'entry)(entry-fn entry) entry))))

(defun get-printer (object table)
  (let* ((parent (parent-table table)))
    (if parent
      (get-printer-internal object (conses-with-cars parent) (others parent))
      (with-lock-grabbed (*pprint-dispatch-table-lock*)
        (get-printer-internal object (conses-with-cars table) (others table))))))

(defun fits (obj entry) 
  (funcall (entry-test entry) obj))

(defun specifier-category (spec)
  (cond ((and (consp spec)
	      (eq (car spec) 'cons)
	      (consp (cdr spec))
	      (null (cddr spec))
	      (consp (cadr spec))
	      (eq (caadr spec) 'member)
	      (consp (cdadr spec))
	      (null (cddadr spec)))
	 'cons-with-car)
	(T 'other)))

; lets make fewer things fns that compile at load time, esp anything
; we do - really none should
(defun specifier-fn (spec) 
  (if (and (consp spec)(eq (car spec) 'satisfies)(symbolp (cadr spec)))
    (cadr spec)
    (if (and (symbolp spec)(type-predicate spec))  ; ccl specific
      (type-predicate spec)
      `(lambda (x) ,(convert-body spec)))))

(defun convert-body (spec)
  (cond ((atom spec) `(typep x ',spec))
	((member (car spec) '(and or not))
	 (cons (car spec) (mapcar #'convert-body (cdr spec))))
	((eq (car spec) 'member)
	 `(member x ',(copy-list (cdr spec))))
	((eq (car spec) 'cons)
	 `(and (consp x)
	       ,@(if (cdr spec) `((let ((x (car x)))
				    ,(convert-body (cadr spec)))))
	       ,@(if (cddr spec) `((let ((x (cdr x)))
				     ,(convert-body (caddr spec)))))))
	((eq (car spec) 'satisfies)
	 `(funcall (function ,(cadr spec)) x))
	(T `(typep x ',spec))))

;               ---- XP STRUCTURES, AND THE INTERNAL ALGORITHM ----

(eval-when (:execute :compile-toplevel) ;not used at run time.
  (defvar block-stack-entry-size 1)
  (defvar prefix-stack-entry-size 5)
  (defvar queue-entry-size 7)
  (defvar buffer-entry-size 1)
  (defvar prefix-entry-size 1)
  (defvar suffix-entry-size 1))

(eval-when (:execute :load-toplevel :compile-toplevel) ;used at run time
  (defvar block-stack-min-size #.(* 35. block-stack-entry-size))
  (defvar prefix-stack-min-size #.(* 30. prefix-stack-entry-size))
  (defvar queue-min-size #.(* 75. queue-entry-size))
  (defvar buffer-min-size 256.)
  (defvar prefix-min-size 256.)
  (defvar suffix-min-size 256.)) 

(progn
  (setf (fdefinition 'xp-stream-stream) #'(lambda (s) (xp-stream s)))

  (defmethod streamp ((x xp-structure)) t)
  (defmethod streamp ((x xp-stream)) t)

  (defmethod output-stream-p ((x xp-structure)) t)
  (defmethod output-stream-p ((x xp-stream)) t)
  
  (defun make-xp-structure ()
    (%istruct
     'xp-structure
     nil                                ; xp-base-stream
     nil                                ; xp-linel
     nil                                ; xp-line-limit
     nil                                ; xp-line-no
     nil                                ; xp-char-mode
     nil                                ; xp-char-mode-counter
     nil                                ; xp-depth-in-blocks
     (make-array #.block-stack-min-size) ; xp-block-stack
     nil                                ; xp-block-stack-ptr
     (make-array #.buffer-min-size :element-type 'base-char)
                                        ; use make-string and let it default?
                                        ; xp-buffer
     nil                                ; xp-charpos
     nil                                ; xp-buffer-ptr
     nil                                ; xp-buffer-offset
     (make-array #.queue-min-size)      ; xp-queue
     0                                  ; xp-qleft
     0                                  ; xp-qright
     (make-array #.buffer-min-size :element-type 'base-char)
                                        ; xp-prefix
     (make-array #.prefix-stack-min-size) ; xp-prefix-stack
     nil                                ; xp-prefix-stack-ptr
     (make-array #.buffer-min-size :element-type 'base-char)
                                        ; xp-suffix
     nil                                ; xp-stream
     nil                                ; xp-string-stream
     ))                            ; XP-STRUCTURE is a built-in class.

  (defmethod write-internal-1 ((xp-struc xp-structure) object level list-kludge)
    (write-internal-1 (xp-stream xp-struc) object level list-kludge))



  (defun get-xp-stream (pp)
    (xp-stream pp))
  )

 
(eval-when (:compile-toplevel :execute)
(defmacro LP<-BP (xp &optional (ptr nil))
  (if (null ptr) (setq ptr `(xp-buffer-ptr ,xp)))
  `(the fixnum (%i+ ,ptr (xp-charpos ,xp))))
(defmacro TP<-BP (xp)
  `(the fixnum (%i+ (xp-buffer-ptr ,xp) (xp-buffer-offset ,xp))))
(defmacro BP<-LP (xp ptr)
  `(the fixnum (%i- ,ptr (xp-charpos ,xp))))
(defmacro BP<-TP (xp ptr)
  `(the fixnum (%i- ,ptr (xp-buffer-offset ,xp))))
;This does not tell you the line position you were at when the TP
;was set, unless there have been no newlines or indentation output 
;between ptr and the current output point.
(defmacro LP<-TP (xp ptr)
  `(LP<-BP ,xp (BP<-TP ,xp ,ptr)))

;We don't use adjustable vectors or any of that, because we seldom have
;to actually extend and non-adjustable vectors are a lot faster in
;many Common Lisps.

(defmacro xp-check-size (FORM ptr min-size entry-size
                           &optional (type '(simple-array * (*))))
  `(let ((.old. ,form)
         (.ptr. ,ptr))
     (declare (type ,type .old.) (type fixnum .ptr.))
     (if (and (ccl::%i> .ptr. ,(- min-size entry-size)) ;seldom haxpens
              (ccl::%i> .ptr. (- (length (the ,type .old.)) ,entry-size)))
         (let ((.new. ,(let ((l `(ccl::%i+ .ptr. ,(if (= entry-size 1)
                                                    50
                                                    (* 10 entry-size)))))
                         `(make-array ,l :element-type (array-element-type .old.)))))
           ;;>>
           (replace .new. .old.)
           (setf ,form .new.))
         .old.)))

(defmacro section-start (xp) `(svref (xp-block-stack ,xp) (xp-block-stack-ptr ,xp)))
) ; eval-when

;		---- CCL specific METHODS --------
(progn
(defmethod stream-write-char ((stream xp-stream) char)
  (write-char+ char (slot-value stream 'xp-structure))
  char)

(defmethod stream-write-char ((stream xp-structure) char)
  (write-char+ char stream)
  char)

(defmethod stream-write-string ((stream xp-stream) string &optional (start 0) end)
  (setq end (check-sequence-bounds string start end))
  (write-string+ string (slot-value stream 'xp-structure) start end)
  string)

(defmethod stream-write-string ((stream xp-structure) string &optional (start 0) end)
  (setq end (check-sequence-bounds string start end))
  (write-string+ string stream start end)
  string)

; If we really don't care about the value returned then just
; plain (pprint-newline+ :fresh xp) is fine.
(defmethod stream-fresh-line ((stream xp-stream))
  (let ((xp (slot-value stream 'xp-structure)))
    (attempt-to-output xp nil nil)  ; was (attempt-to-output xp T T)
    (prog1 (not (zerop (LP<-BP xp)))      
      (pprint-newline+ :fresh xp))))


(defmethod stream-finish-output ((stream xp-stream))
  (attempt-to-output (slot-value stream 'xp-structure) t t))

(defmethod stream-force-output ((stream xp-stream))
  (attempt-to-output (slot-value stream 'xp-structure) t t)
  nil)

(defmethod stream-clear-output ((stream xp-stream))
  (let ((*locating-circularities* 1)) ;hack to prevent visible output
    (attempt-to-output (slot-value stream 'xp-structure) T T))
  nil)

(defmethod stream-line-column ((stream xp-stream))
  (LP<-BP (slot-value stream 'xp-structure)))

(defmethod stream-line-length ((stream xp-stream))
  (xp-linel (slot-value stream 'xp-structure)))

)


(defun push-block-stack (xp)
  (let ((ptr (%i+ (xp-block-stack-ptr xp) #.block-stack-entry-size)))
    (setf (xp-block-stack-ptr xp) ptr)
    (xp-check-size (xp-block-stack xp) ptr
                   #.block-stack-min-size #.block-stack-entry-size)))

(eval-when (:compile-toplevel :execute)
(defmacro prefix-ptr (xp)
  `(svref (xp-prefix-stack ,xp) (xp-prefix-stack-ptr ,xp)))
(defmacro suffix-ptr (xp)
  `(svref (xp-prefix-stack ,xp) (%i+ (xp-prefix-stack-ptr ,xp) 1)))
(defmacro non-blank-prefix-ptr (xp)
  `(svref (xp-prefix-stack ,xp) (%i+ (xp-prefix-stack-ptr ,xp) 2)))
(defmacro initial-prefix-ptr (xp)
  `(svref (xp-prefix-stack ,xp) (%i+ (xp-prefix-stack-ptr ,xp) 3)))
(defmacro section-start-line (xp)
  `(svref (xp-prefix-stack ,xp) (%i+ (xp-prefix-stack-ptr ,xp) 4)))

(defmacro stk-prefix-ptr (stk ptr)
  `(svref ,stk ,ptr))
(defmacro stk-suffix-ptr (stk ptr)
  `(svref ,stk (%i+ ,ptr 1)))
(defmacro stk-non-blank-prefix-ptr (stk ptr)
  `(svref ,stk (%i+ ,ptr 2)))
) ; EVAL-when


; saves 100 bytes and a microsecond or 2
(defun push-prefix-stack (xp)
  (let ((old-prefix 0)
        (old-suffix 0) 
        (old-non-blank 0)
        (stack (xp-prefix-stack xp))
        (ptr (xp-prefix-stack-ptr xp)))
    (declare (fixnum ptr))
    (when (>= ptr 0)
      (setq old-prefix (stk-prefix-ptr stack ptr)
	    old-suffix (stk-suffix-ptr stack ptr)
	    old-non-blank (stk-non-blank-prefix-ptr stack ptr)))
    (setq ptr (%i+ ptr #.prefix-stack-entry-size))
    (setf (xp-prefix-stack-ptr xp) ptr)
    (setq stack
          (xp-check-size (xp-prefix-stack xp) ptr
                   #.prefix-stack-min-size #.prefix-stack-entry-size))
    (setf (stk-prefix-ptr stack ptr) old-prefix)
    (setf (stk-suffix-ptr stack ptr) old-suffix)
    (setf (stk-non-blank-prefix-ptr stack ptr) old-non-blank)))



(eval-when (:compile-toplevel :execute)
(defmacro Qtype   (xp index) `(svref (xp-queue ,xp) ,index))
(defmacro Qkind   (xp index) `(svref (xp-queue ,xp) (1+ ,index)))
(defmacro Qpos    (xp index) `(svref (xp-queue ,xp) (+ ,index 2)))
(defmacro Qdepth  (xp index) `(svref (xp-queue ,xp) (+ ,index 3)))
(defmacro Qend    (xp index) `(svref (xp-queue ,xp) (+ ,index 4)))
(defmacro Qoffset (xp index) `(svref (xp-queue ,xp) (+ ,index 5)))
(defmacro Qarg    (xp index) `(svref (xp-queue ,xp) (+ ,index 6)))
(defmacro xpq-type (queue index)
  `(svref ,queue ,index))
(defmacro xpq-kind (queue index)
  `(svref ,queue (ccl::%i+ ,index 1)))
(defmacro xpq-pos (queue index)
  `(svref ,queue (ccl::%i+ ,index 2)))
(defmacro xpq-depth (queue index)
  `(svref ,queue (ccl::%i+ ,index 3)))
(defmacro xpq-end (queue index)
  `(svref ,queue (ccl::%i+ ,index 4)))
(defmacro xpq-offset (queue index)
  `(svref ,queue (ccl::%i+ ,index 5)))
(defmacro xpq-arg (queue index)
  `(svref ,queue (ccl::%i+ ,index 6)))
) ; eval-when

;we shift the queue over rather than using a circular queue because
;that works out to be a lot faster in practice.  Note, short printout
;does not ever cause a shift, and even in long printout, the queue is
;shifted left for free every time it happens to empty out.

(defun enqueue (xp type kind &optional arg)  
  (let ((queue (xp-queue xp))
        (qright (ccl::%i+ (xp-qright xp) #.queue-entry-size))
        (qleft (xp-qleft xp)))
    (declare (type fixnum qright qleft) (type simple-vector queue))
    (when (ccl::%i> qright #.(- queue-min-size queue-entry-size))
      ;;>> generic
      (replace queue queue :start2 qleft :end2 qright)
      (setf (xp-qleft xp) 0
            qright (ccl::%i- qright qleft)))
    (setq queue (xp-check-size (xp-queue  xp) qright
                               #.queue-min-size #.queue-entry-size))
    (setf (xp-qright xp) qright
          (xpq-type queue qright) type
          (xpq-kind queue qright) kind
          (xpq-pos queue qright) (TP<-BP xp)
          (xpq-depth queue qright) (xp-depth-in-blocks xp)
          (xpq-end queue qright) nil
          (xpq-offset queue qright) nil
          (xpq-arg queue qright) arg)))

(defmacro Qnext (index) `(%i+ ,index #.queue-entry-size))


;This maintains a list of XP structures.  We save them
;so that we don't have to create new ones all of the time.
;We have separate objects so that many can be in use at once.

;(Note should really be doing some locking here, but CL does not have the
;primitives for it.  There is a tiny probability here that two different
;processes could end up trying to use the same xp-stream)

(def-standard-initial-binding *free-xps* nil) ;free list of XP stream objects

(defun get-pretty-print-stream (stream)
  (let ((xp (without-interrupts (pop *free-xps*))))
    (when (not xp)(setq xp (make-xp-structure)))
    (initialize-xp xp stream)
    (let ((the-xp-stream (make-instance  'xp-stream)))
      (setf (slot-value the-xp-stream 'xp-structure) xp)
      (setf (xp-stream xp) the-xp-stream) ; lets be circular
      the-xp-stream)))

;If you call this, the xp-stream gets efficiently recycled.

(defun free-pretty-print-stream (xp)
  (setf (xp-base-stream xp) nil)
  (pushnew xp *free-xps*))

;This is called to initialize things when you start pretty printing.

(defun initialize-xp (xp stream)
  (setf (xp-base-stream xp) stream)
  (setf (xp-linel xp) (max 0 (cond (*print-right-margin*)
				           ((output-width stream))
				           (T *default-right-margin*))))
  (setf (xp-line-limit xp) *print-lines*)
  (setf (xp-line-no xp) 1)
  (setf (xp-char-mode xp) nil)
  (setf (xp-char-mode-counter xp) 0)
  (setf (xp-depth-in-blocks xp) 0)
  (setf (xp-block-stack-ptr xp) 0)
  (setf (xp-charpos xp) (cond ((output-position stream)) (T 0)))
  (setf (section-start xp) 0)
  (setf (xp-buffer-ptr xp) 0)
  (setf (xp-buffer-offset xp) (xp-charpos xp))
  (setf (xp-qleft xp) 0)
  (setf (xp-qright xp) #.(- queue-entry-size))
  (setf (xp-prefix-stack-ptr xp) #.(- prefix-stack-entry-size))
  (let ((s (xp-string-stream xp)))
    (when s (stream-position s 0)))
  xp)

;The char-mode stuff is a bit tricky.
;one can be in one of the following modes:
;NIL no changes to characters output.
;:UP CHAR-UPCASE used.
;:DOWN CHAR-DOWNCASE used.
;:CAP0 capitalize next alphanumeric letter then switch to :DOWN.
;:CAP1 capitalize next alphanumeric letter then switch to :CAPW
;:CAPW downcase letters.  When a word break letter found, switch to :CAP1.
;It is possible for ~(~) to be nested in a format string, but note that
;each mode specifies what should happen to every letter.  Therefore, inner
;nested modes never have any effect.  You can just ignore them.

(defun push-char-mode (xp new-mode)
  (if (zerop (xp-char-mode-counter xp))
      (setf (xp-char-mode xp) new-mode))
  (incf (xp-char-mode-counter xp)))

(defun pop-char-mode (xp)
  (decf (xp-char-mode-counter xp))
  (if (zerop (xp-char-mode-counter xp))
      (setf (xp-char-mode xp) nil)))

;Assumes is only called when char-mode is non-nil
(defun handle-char-mode (xp char)
  (case (xp-char-mode xp)
    (:CAP0 (cond ((not (alphanumericp char)) char)
		 (T (setf (xp-char-mode xp) :DOWN) (char-upcase char))))
    (:CAP1 (cond ((not (alphanumericp char)) char)
		 (T (setf (xp-char-mode xp) :CAPW) (char-upcase char))))
    (:CAPW (cond ((alphanumericp char) (char-downcase char))
		 (T (setf (xp-char-mode xp) :CAP1) char)))
    (:UP (char-upcase char))
    (T (char-downcase char)))) ;:DOWN

;All characters output are passed through the handler above.  However, it must
;be noted that on-each-line prefixes are only processed in the context of the
;first place they appear.  They stay the same later no matter what.  Also
;non-literal newlines do not count as word breaks.


;This handles the basic outputting of characters.  note + suffix means that
;the stream is known to be an XP stream, all inputs are mandatory, and no
;error checking has to be done.  Suffix ++ additionally means that the
;output is guaranteed not to contain a newline char.

(defun write-char+ (char xp)
  (if (eql char #\newline) (pprint-newline+ :unconditional xp)
      (write-char++ char xp)))

(defun write-string+ (string xp start end)
  (let ((sub-end nil) next-newline)
    (loop (setq next-newline
		(if (typep string 'simple-string)
                  (%str-member #\newline string start end)
                  (position #\newline string :start start :end end :test #'eq )))
	  (setq sub-end (if next-newline next-newline end))
	  (write-string++ string xp start sub-end)
	  (when (null next-newline) (return nil))
	  (pprint-newline+ :unconditional xp)
	  (setq start (%i+ 1 sub-end)))))



;note this checks (> BUFFER-PTR LINEL) instead of (> (LP<-BP) LINEL)
;this is important so that when things are longer than a line they
;end up getting printed in chunks of size LINEL.

(defun write-char++ (char xp)
  (when (> (xp-buffer-ptr xp) (xp-linel xp))
    (force-some-output xp))
  (let ((new-buffer-end (%i+ 1 (xp-buffer-ptr xp))))
    (xp-check-size (xp-buffer xp) new-buffer-end #.buffer-min-size #.buffer-entry-size)
    (if (xp-char-mode xp) (setq char (handle-char-mode xp char)))
    (setf (schar (xp-buffer xp) (xp-buffer-ptr xp)) char)    
    (setf (xp-buffer-ptr xp) new-buffer-end)))


(defun force-some-output (xp)
  (attempt-to-output xp nil nil)
  (when (> (xp-buffer-ptr xp) (xp-linel xp)) ;only if printing off end of line
    (attempt-to-output xp T T)))

(defun write-string++ (string xp start end)
  (when (> (xp-buffer-ptr xp) (xp-linel xp))
    (force-some-output xp))
  (write-string+++ string xp start end))

;never forces output; therefore safe to call from within output-line.

(defun write-string+++ (string xp start end)
  (declare (fixnum start end))
  (let ((new-buffer-end (%i+ (xp-buffer-ptr xp) (- end start))))
    (xp-check-size (xp-buffer xp) new-buffer-end #.buffer-min-size #.buffer-entry-size)
    (do ((buffer (xp-buffer xp))
	 (i (xp-buffer-ptr xp) (1+ i))
	 (j start (1+ j)))
	((= j end))
      (declare (fixnum i j))
      (let ((char (char string j)))
	(if (xp-char-mode xp) (setq char (handle-char-mode xp char)))      
	(setf (schar buffer i) char)))
    (setf (xp-buffer-ptr xp) new-buffer-end)))

(defun pprint-tab+ (kind colnum colinc xp)
  (let ((indented? nil) (relative? nil))
    (declare (fixnum colnum colinc))
    (case kind
      (:section (setq indented? T))
      (:line-relative (setq relative? T))
      (:section-relative (setq indented? T relative? T)))
    (when (or (not indented?)
              (and *print-pretty* *logical-block-p*))
      (let* ((current
              (if (not indented?) (LP<-BP xp)
                  (%i- (TP<-BP xp) (section-start xp))))
             (new
              (if (zerop colinc)
                  (if relative? (+ current colnum) (max colnum current))
                  (cond (relative?
                         (* colinc (floor (+ current colnum colinc -1) colinc)))
                        ((> colnum current) colnum)
                        (T (+ colnum
                              (* colinc
                                 (floor (+ current (- colnum) colinc) colinc)))))))
             (length (- new current)))
        (declare (fixnum current new length))
        (when (plusp length)
          (if (xp-char-mode xp) (handle-char-mode xp #\space))
          (let ((end (%i+ (xp-buffer-ptr xp) length)))
            (xp-check-size (xp-buffer xp) end #.buffer-min-size #.buffer-entry-size)
            (fill (xp-buffer xp) #\space :start (xp-buffer-ptr xp) :end end)
            (setf (xp-buffer-ptr xp) end)))))))

;note following is smallest number >= x that is a multiple of colinc
;  (* colinc (floor (+ x (1- colinc)) colinc))

(defun pprint-newline+ (kind xp)
  (enqueue xp :newline kind)
  (let ((queue (xp-queue xp))
        (qright (xp-qright xp)))
    (declare (fixnum qright))
    (do ((ptr (xp-qleft xp) (Qnext ptr))) ;find sections we are ending
        ((not (< ptr qright)))            ;all but last
      (declare (fixnum ptr))
      (when (and (null (xpq-end queue ptr))
                 (not (%i> (xp-depth-in-blocks xp) (xpq-depth queue ptr)))
                 (memq (xpq-type queue ptr) '(:newline :start-block)))
        (setf (xpq-end queue ptr) (- qright ptr))))
    (setf (section-start xp) (TP<-BP xp))
    (when (and (memq kind '(:fresh :unconditional)) (xp-char-mode xp))
      (handle-char-mode xp #\newline))
    (when (memq kind '(:fresh :unconditional :mandatory))
      (attempt-to-output xp T nil))))

(defun start-block (xp prefix-string on-each-line? suffix-string)
  (macrolet ((push-block-stack (xp)
               `(let ((ptr (%i+ (xp-block-stack-ptr ,xp) #.block-stack-entry-size)))
                  (setf (xp-block-stack-ptr ,xp) ptr)
                  (xp-check-size (xp-block-stack ,xp) ptr
                                 #.block-stack-min-size #.block-stack-entry-size))))
    (let ((length (if prefix-string (length (the string prefix-string)) 0)))        
      (declare (fixnum length))
      (when prefix-string (write-string++ prefix-string xp 0 length))    
      (if (and (xp-char-mode xp) on-each-line?)
        (let ((ptr (xp-buffer-ptr xp)))
          (declare (fixnum ptr))
          (setq prefix-string
	        (%substr (xp-buffer xp) (- ptr length) ptr))))
      (push-block-stack xp)
      (enqueue xp :start-block nil
	       (if on-each-line? (cons suffix-string prefix-string) suffix-string))
      (setf (xp-depth-in-blocks xp)(%i+ 1 (xp-depth-in-blocks xp)))      ;must be after enqueue
      (setf (section-start xp) (TP<-BP xp)))))

(defun end-block (xp suffix)
  (macrolet ((pop-block-stack (xp)
               `(decf (the fixnum (xp-block-stack-ptr ,xp)) #.block-stack-entry-size)))
    ;(unless (eq *abbreviation-happened* '*print-lines*)
      (when suffix (write-string+ suffix xp 0 (length suffix)))
      (decf (xp-depth-in-blocks xp))
      (enqueue xp :end-block nil suffix)
      (let ((queue (xp-queue xp))
            (qright (xp-qright xp)))
        (declare (fixnum qright))
        (do ((ptr (xp-qleft xp) (Qnext ptr))) ;looking for start of block we are ending
	    ((not (< ptr qright)))    ;all but last
          (declare (fixnum ptr))
          (when (and (= (the fixnum (xp-depth-in-blocks xp)) (the fixnum (xpq-depth queue ptr)))
		     (eq (xpq-type queue ptr) :start-block)
		     (null (xpq-offset queue ptr)))
	    (setf (xpq-offset queue ptr) (- qright ptr))
	    (return nil)))	;can only be 1
        (pop-block-stack xp)))) ;)

(defun pprint-indent+ (kind n xp)
  (when (and *print-pretty* *logical-block-p*)
    (enqueue xp :ind kind n)))

; The next function scans the queue looking for things it can do.
;it keeps outputting things until the queue is empty, or it finds
;a place where it cannot make a decision yet.

(eval-when (:compile-toplevel :execute)
(defmacro maybe-too-large (xp Qentry queue linel)
  `(let ((.limit. ,linel)
         (.qend. (xpq-end ,queue ,qentry)))
     (declare (fixnum .limit.))
     (when (eql (xp-line-limit ,xp) (xp-line-no ,xp)) ;prevents suffix overflow
       (decf .limit. 2) ;3 for " .." minus 1 for space (heuristic)
       (when (not (minusp (xp-prefix-stack-ptr ,xp)))
	 (decf .limit. (suffix-ptr ,xp))))
     (cond (.qend.
	    (%i> (LP<-TP ,xp (xpq-pos ,queue (%i+ ,Qentry .qend.))) .limit.))
	   ((or force-newlines? (%i> (LP<-BP ,xp) .limit.)) T)
	   (T (return nil)))))	;wait until later to decide.

(defmacro misering? (xp left)
  `(<= ,left
       (the fixnum (initial-prefix-ptr ,xp))))
) ; eval-when

;If flush-out? is T and force-newlines? is NIL then the buffer,
;prefix-stack, and queue will be in an inconsistent state after the call.
;You better not call it this way except as the last act of outputting.


(defun attempt-to-output (xp force-newlines? flush-out?)
  (macrolet ((pop-prefix-stack (xp)             
             `(decf (the fixnum (xp-prefix-stack-ptr ,xp))
                #.prefix-stack-entry-size)))
  (let* ((width  *print-miser-width*)
         (linel (xp-linel xp))
         (left  (if width (- linel width) most-positive-fixnum)))
    (declare (fixnum linel left))
  (do ((qleft (xp-qleft xp))
       (queue (xp-queue xp)(xp-queue xp)))
      ((%i> qleft (xp-qright xp))
	  (setf (xp-qleft xp) 0)
	  (setf (xp-qright xp) #.(- queue-entry-size))) ;saves shifting
    ; initial-prefix-ptr cant be referenced initially - prefix-stack-ptr is negative
    (case (xpq-type queue qleft)
      (:ind
       (unless (misering? xp left)
	 (set-indentation-prefix xp
	   (case (xpq-kind queue qleft)
	     (:block (%i+ (initial-prefix-ptr xp) (xpq-arg queue qleft)))
	     (T ; :current
	       (%i+ (LP<-TP xp (xpq-pos queue qleft))
		  (xpq-arg queue qleft)))))) )
      (:start-block
       (cond ((maybe-too-large xp qleft queue linel)
	      (push-prefix-stack xp)
	      (setf (initial-prefix-ptr xp) (prefix-ptr xp))
	      (set-indentation-prefix xp (LP<-TP xp (xpq-pos queue qleft)))
	      (let ((arg (xpq-arg queue qleft)))
		(when (consp arg) (set-prefix xp (cdr arg)))
		(setf (initial-prefix-ptr xp) (prefix-ptr xp))
		(cond ((not (listp arg)) (set-suffix xp arg))
		      ((car arg) (set-suffix xp (car arg)))))
	      (setf (section-start-line xp) (xp-line-no xp)))
	     (T (setq qleft (%i+ qleft (xpq-offset queue qleft))))) )
      (:end-block (pop-prefix-stack xp))
      (T ; :newline
       (when (case (xpq-kind queue qleft)
	       (:fresh (not (%izerop (LP<-BP xp))))
	       (:miser (misering? xp left))
	       (:fill (or (misering? xp left)
			  (%i> (xp-line-no xp) (section-start-line xp))
			  (maybe-too-large xp qleft queue linel)))
	       (T T)) ;(:linear :unconditional :mandatory) 
	 (output-line-and-setup-for-next xp qleft))))
    (setf (xp-qleft xp) (setq qleft (qnext qleft))))
  (when flush-out? (flush xp)))))


(defun flush (xp)
  (let ((ostream (xp-out-stream xp))
        (len (xp-buffer-ptr xp)))
    (when (and *print-pprint-dispatch* (commit-hook *print-pprint-dispatch*))
      (funcall (commit-hook *print-pprint-dispatch*) xp len 0))
    (when ostream      
      (write-string (xp-buffer xp) ostream :start 0 :end len))
    (incf (xp-buffer-offset xp) len)
    (incf (xp-charpos xp) len)
    (setf (xp-buffer-ptr xp) 0)))


(defun xp-out-stream (xp)
  (let ((lc *locating-circularities*))
    (cond 
     ((null lc)
      (xp-base-stream xp))
     ((= lc 0)
      (if  (null (xp-string-stream xp))
        (setf (xp-string-stream xp) (make-string-output-stream))
        (xp-string-stream xp))))))
  

;This prints out a line of stuff.

(defun output-line-and-setup-for-next (xp Qentry)
  (let* ((queue (xp-queue xp))
         (out-point (BP<-TP xp (xpq-pos queue Qentry)))
         (unconditional-p (memq (xpq-kind queue Qentry) '(:fresh :unconditional)))
         (end (if unconditional-p
                out-point
                (let ((buffer (xp-buffer xp)))
                  (declare (type simple-base-string buffer))
                  (do ((i (%i- out-point 1) (%i- i 1)))
                      ((%i< i 0) 0)
                    (when (or (neq (schar buffer i) #\Space)
                              ;; Don't match possibly-quoted space ("possibly" because the #\\ itself might be 
                              ;; quoted; don't bother checking for that, no big harm leaving the space even if
                              ;; not totally necessary).
                              (and (%i< 0 i) (eq (schar buffer (%i- i 1)) #\\)))
                      (return (%i+ i 1)))))))
         (prefix-end
          (if unconditional-p (non-blank-prefix-ptr xp) (prefix-ptr xp)))
         (old-ptr (xp-buffer-ptr xp))
         (new-ptr (%i+ old-ptr (%i- prefix-end out-point)))
         (line-limit-exit (and (xp-line-limit xp) (not (%i> (xp-line-limit xp) (xp-line-no xp))))))
    (when line-limit-exit
      (setf (xp-buffer-ptr xp) end)          ;truncate pending output.
      (write-string+++ " .." xp 0 3)
      (reverse-string-in-place (xp-suffix xp) 0 (suffix-ptr xp))
      (write-string+++ (xp-suffix xp) xp 0 (suffix-ptr xp))
      (setf (xp-qleft xp) (qnext (xp-qright xp)))
      ;(setq *abbreviation-happened* '*print-lines*)
      (throw 'line-limit-abbreviation-exit T))
    (setf (xp-line-no xp)(%i+ 1 (xp-line-no xp)))
    (when (and *print-pprint-dispatch* (commit-hook *print-pprint-dispatch*))
      (funcall (commit-hook *print-pprint-dispatch*) xp out-point prefix-end))
    (let ((bstream (xp-out-stream xp)))
      (when bstream
        (write-string (xp-buffer xp) bstream :start 0 :end end)
        (stream-write-char bstream #\newline)))
    (setf (xp-charpos xp) 0)
    (when (%i> new-ptr old-ptr)                  ;almost never happens
      (xp-check-size (xp-buffer xp) new-ptr #.buffer-min-size #.buffer-entry-size))
    (setf (xp-buffer-ptr xp) new-ptr)
    (decf (xp-buffer-offset xp) (- prefix-end out-point))
    (let ((buffer (xp-buffer xp)))
      (replace buffer buffer :start1 prefix-end :start2 out-point :end2 old-ptr)
      (replace buffer (xp-prefix xp) :end2 prefix-end)
      (unless unconditional-p
        (setf (section-start-line xp) (xp-line-no xp))))))



(defun set-indentation-prefix (xp new-position)
  (let ((new-ind (max (non-blank-prefix-ptr xp) new-position)))
    (declare (fixnum new-ind))
    (setf (prefix-ptr xp) (initial-prefix-ptr xp))
    (xp-check-size (xp-prefix xp) new-ind #.prefix-min-size #.prefix-entry-size)
    (when (%i> new-ind (prefix-ptr xp))
      (fill (xp-prefix xp) #\space :start (prefix-ptr xp) :end new-ind))
    (setf (prefix-ptr xp) new-ind)))

(defun set-prefix (xp prefix-string)
  (declare (string prefix-string))
  (replace (xp-prefix xp) prefix-string
	   :start1 (%i- (prefix-ptr xp) (length prefix-string)))
  (setf (non-blank-prefix-ptr xp) (prefix-ptr xp)))

(defun set-suffix (xp suffix-string)
  (declare (string suffix-string))
  (let* ((end (length suffix-string))
	 (new-end (%i+ (suffix-ptr xp) end)))
    (declare (fixnum end new-end))
    (xp-check-size (xp-suffix xp) new-end #.suffix-min-size #.suffix-entry-size)
    (do ((i (1- new-end) (1- i)) (j 0 (1+ j))) ((= j end))
      (declare (fixnum i j))
      (setf (char (xp-suffix xp) i) (char suffix-string j)))
    (setf (suffix-ptr xp) new-end)))

(defun reverse-string-in-place (string start end)
  (declare (fixnum start end))
  (do ((i start (1+ i)) (j (1- end) (1- j))) ((not (< i j)) string)
    (declare (fixnum i j))
    (let ((c (schar string i)))
      (setf (schar string i) (schar string j))
      (setf (schar string j) c))))

;		   ---- BASIC INTERFACE FUNCTIONS ----

;The internal functions in this file, and the (formatter "...") expansions
;use the '+' forms of these functions directly (which is faster) because,
;they do not need error checking of fancy stream coercion.  The '++' forms
;additionally assume the thing being output does not contain a newline.

(defun maybe-initiate-xp-printing (fn stream &rest args)
  (if (xp-structure-p stream) (apply fn stream args)
    (if (typep stream 'xp-stream)
      (apply fn (slot-value stream 'xp-structure) args)
      (let ((*locating-circularities* (if *print-circle* 0 nil))
            (*circularity-hash-table*
             (if *print-circle* (get-circularity-hash-table) nil)))
        (prog1 (xp-print fn (decode-stream-arg stream) args)
          (if *circularity-hash-table*
            (free-circularity-hash-table *circularity-hash-table*)))))))

(defun xp-print (fn stream args)
  (flet ((do-it (fn stream args)
           (prog1 (do-xp-printing fn stream args)
             (when *locating-circularities*
               (setq *locating-circularities* nil)
               (do-xp-printing fn stream args)))))
    (cond (*print-readably*
           (let* ((*print-level* nil)
                  (*print-length* nil)
                  (*print-lines* nil)
                  (*print-escape* t)
                  (*print-gensym* t)
                  (*print-array* nil))
             (do-it fn stream args)))
          (t (do-it fn stream args)))))

(defun decode-stream-arg (stream)
  (cond ((eq stream T) *terminal-io*)
	((null stream) *standard-output*)
	(T stream)))

(defun do-xp-printing (fn stream args)
  (let ((xp (slot-value (get-pretty-print-stream stream) 'xp-structure))
	(*current-level* 0)
        (*xp-current-object* nil)
	(result nil))
    (declare (special *foo-string*))
    (catch 'line-limit-abbreviation-exit
      (start-block xp nil nil nil)
      (setq result (apply fn xp args))
      (end-block xp nil))
    (when (and *locating-circularities*
	       (zerop *locating-circularities*)	;No circularities.
               ;(= (xp-line-no xp) 1)	     	;Didn't suppress line.
	       ;(zerop (xp-buffer-offset xp))
               )	;Didn't suppress partial line.
      (setq *locating-circularities* nil)
      (let ((s (xp-string-stream xp)))
        (when s
          (stream-write-entire-string (xp-base-stream xp)
                                      (get-output-stream-string s)))))
    (when (catch 'line-limit-abbreviation-exit
	    (attempt-to-output xp nil T)
            nil)
      (attempt-to-output xp T T))
    (free-pretty-print-stream xp)
    result))



(defun write+ (object xp &optional interior-cdr circle)
  (let ((pretty *print-pretty*)) ;((*parents* *parents*))
    (when (or circle
              (not (and *circularity-hash-table*
		        (eq (setq circle (circularity-process xp object interior-cdr)) :subsequent))))
      (when *circularity-hash-table*
        (setq *xp-current-object* object))	
      (let ((printer (if pretty (get-printer object *print-pprint-dispatch*) nil))
	    #|type|#)
	(cond (printer
	       (funcall printer xp object))
	      ((and pretty (maybe-print-fast xp object)))
              (t (write-not-pretty xp object
                                   (if *print-level*
                                     (- *print-level* *current-level*)
                                     most-positive-fixnum)
                                   interior-cdr circle)))))))

;It is vital that this function be called EXACTLY once for each occurrence of 
;  each thing in something being printed.
;Returns nil if printing should just continue on.
;  Either it is not a duplicate, or we are in the first pass and do not know.
;returns :FIRST if object is first occurrence of a DUPLICATE.
;  (This can only be returned on a second pass.)
;  After an initial code (printed by this routine on the second pass)
;  printing should continue on for the object.
;returns :SUBSEQUENT if second or later occurrence.
;  Printing is all taken care of by this routine.

;Note many (maybe most) lisp implementations have characters and small numbers
;represented in a single word so that the are always eq when they are equal and the
;reader takes care of properly sharing them (just as it does with symbols).
;Therefore, we do not want circularity processing applied to them.  However,
;some kinds of numbers (e.g., bignums) undoubtedly are complex structures that
;the reader does not share.  However, they cannot have circular pointers in them
;and it is therefore probably a waste to do circularity checking on them.  In
;any case, it is not clear that it easy to tell exactly what kinds of numbers a
;given implementation of CL is going to have the reader automatically share.

; if not pretty print a space before dot

(defun circularity-process (xp object interior-cdr? &aux (not-pretty (not *print-pretty*)))
  (declare (ftype function invalid-hash-key-p))
  (unless (or (numberp object)
	      (characterp object)
              (invalid-hash-key-p object)
	      (and (symbolp object)	;Reader takes care of sharing.
		   (or (null *print-gensym*) (symbol-package object))))
    (let ((id (gethash object *circularity-hash-table*)))
      (if (and *locating-circularities* *print-circle*) ; << was *locating-circularities*
        (progn ;(push (list object id info-p) barf)
          (cond ((null id)	;never seen before
                 ;(when *parents* (push object *parents*))
                 (setf (gethash object *circularity-hash-table*) 0)
                 nil)
                ((zerop id) ;possible second occurrence
                 (setf (gethash object *circularity-hash-table*)
                       (incf *locating-circularities*))
                 :subsequent)
                (T :subsequent)));third or later occurrence
        (progn ;(push (list object id info-p interior-cdr?) barf2)          
          (cond 
           ((or (null id)	;never seen before (note ~@* etc. conses)
                (zerop id));no duplicates
            nil)
           (t (when interior-cdr?
                (write-string++ (if not-pretty " . #" ". #")
                                            xp 0
                                            (if not-pretty 4 3)))
              (cond ((plusp id)
                     (cond (interior-cdr?
                            (decf *current-level*))
                           (T (write-char++ #\# xp)))
                     (print-fixnum xp id)
                     (write-char++ #\= xp)
                     (setf (gethash object *circularity-hash-table*) (- id))
                     :first)
                    (T (when (not interior-cdr?) (write-char++ #\# xp))
                       (print-fixnum xp (- id))
                       (write-char++ #\# xp)
                       :subsequent)))))))))

;This prints a few very common, simple atoms very fast.
;Pragmatically, this turns out to be an enormous savings over going to the
;standard printer all the time.  There would be diminishing returns from making
;this work with more things, but might be worth it.
; does this really win?

(defun maybe-print-fast (xp object)
  (cond ((stringp object)
	 (cond ((null *print-escape*) (write-string+ object xp 0 (length object)) T)
	       ((every #'(lambda (c) (not (or (eq c #\") (eq c #\\))))
		       object)
		(write-char++ #\" xp)
		(write-string+ object xp 0 (length object))
		(write-char++ #\" xp) T)))
	((typep object 'fixnum)
	 (when (and (null *print-radix*) (= *print-base* 10.))
	   (when (minusp object)
	     (write-char++ #\- xp)
	     (setq object (- object)))
	   (print-fixnum xp object) T))
	((symbolp object)
         (if (> *print-base* 10) ; may need to escape potential numbers
           (write-a-symbol object (xp-stream xp))
           (let ((s (symbol-name object))
                 (p (symbol-package object))
                 (is-key (keywordp object))
                 (mode (case *print-case*
                         (:downcase :down)
                         (:capitalize :cap1)
                         (T nil)))) ; note no-escapes-needed requires all caps
             (declare (string s))
             (cond ((and (or is-key (eq p *package*)
                             (and  ;*package* ;can be NIL on symbolics
                              (multiple-value-bind (symbol type) (find-symbol s)
                                (and type (eq object symbol)))))
                         (eq (readtable-case *readtable*) :upcase)
                         (neq *print-case* :studly)
                         (no-escapes-needed s))
                    (when (and is-key *print-escape*)
                      (write-char++ #\: xp))
                    (if mode (push-char-mode xp mode))
                    (write-string++ s xp 0 (length s))
                    (if mode (pop-char-mode xp)) T)))))))
         
(defun print-fixnum (xp fixnum)
  (multiple-value-bind (digits d)
      (truncate fixnum 10)
    (unless (zerop digits)
      (print-fixnum xp digits))
    (write-char++ (code-char (+ #.(char-code #\0) d)) xp)))

;just wants to succeed fast in a lot of common cases.
;assumes no funny readtable junk for the characters shown.

(defun no-escapes-needed (s)
  (declare (string s))
  (let ((n (length s)))
    (declare (fixnum n))
    (and (not (zerop n))
	 (let ((c (schar s 0)))
	   (or (and (alpha-char-p c) (upper-case-p c)) (%str-member c "*<>")))
	 (do ((i 1 (1+ i))) ((= i n) T)
           (declare (fixnum i))
	   (let ((c (schar s i)))
	     (if (not (or (digit-char-p c)
                          (and (alpha-char-p c) (upper-case-p c))
			  (%str-member c "*+<>-")))
		 (return nil)))))))


(without-duplicate-definition-warnings  ;; override l1-io version.
 (defun pprint (object &optional (stream *standard-output*))
   "Prettily output OBJECT preceded by a newline."
   (setq stream (decode-stream-arg stream))
   (terpri stream)
   (let ((*print-escape* T) (*print-pretty* T))
     (write-1 object stream))
   (values)))


;Any format string that is converted to a function is always printed
;via an XP stream (See formatter).

(defvar *format-string-cache* nil)

(defun process-format-string (string-or-fn force-fn?)
  (declare (ignore force-fn?))
  string-or-fn)


;Each of these causes the stream to be pessimistic and insert
;newlines wherever it might have to, when forcing the partial output
;out.  This is so that things will be in a consistent state if
;output continues to the stream later.

(defmethod stream-force-output ((xp xp-structure))
  (attempt-to-output xp t t))

(defmethod stream-finish-output ((xp xp-structure))
  (attempt-to-output xp t t))

(defun pprint-recording-positions (form stream recorder)
  ;; The hair here comes from the fact that the pretty printer backtracks to insert newlines.
  (let* ((old-table *print-pprint-dispatch*)
         (rec-pending nil)
         (record (require-type recorder 'function)))
    (flet ((rec-pprint (xp object)
             #+gz (assert (or (null rec-pending)
                              (<= (caar rec-pending) (xp-buffer-ptr xp))))
             (let ((real-printer (get-printer object old-table)))
               (when real-printer
                 (push (list* (xp-buffer-ptr xp) t object) rec-pending)
                 (funcall real-printer xp object)
                 (push (list* (xp-buffer-ptr xp) nil object) rec-pending))))
           (rec-commit (xp commited inserted)
             (loop with change = (- inserted commited)
               as last = nil then pending
               as pending = rec-pending then (cdr pending) while pending
               do (when (<= (caar pending) commited) ;; commit the rest.
                    (if last
                      (setf (cdr last) nil)
                      (setf rec-pending nil))
                    (loop with start = (stream-position (xp-out-stream xp))
                      for (offset open-p . object) in (nreverse pending)
                      do (funcall record object open-p (+ start offset)))
                    (return nil))
               do (incf (caar pending) change))))
      (let* ((*print-pretty* t)
             (*print-circle* nil)
             (*print-length* nil)
             (*print-level* nil)
             (*print-lines* nil)
             (*print-miser-width* nil)
             (*read-suppress* nil)
             (*print-pprint-dispatch* (make-pprint-dispatch-table :commit-hook #'rec-commit)))
        (set-pprint-dispatch 'cons #'rec-pprint)
        (write-1 form stream)
        #+gz (assert (null rec-pending))))
    form))



;           ---- FUNCTIONAL INTERFACE TO DYNAMIC FORMATTING ----

;The internal functions in this file, and the (formatter "...") expansions
;use the '+' forms of these functions directly (which is faster) because,
;they do not need error checking or fancy stream coercion.  The '++' forms
;additionally assume the thing being output does not contain a newline.


(defun pprint-newline (kind &optional (stream *standard-output*))
    "Output a conditional newline to STREAM (which defaults to
   *STANDARD-OUTPUT*) if it is a pretty-printing stream, and do
   nothing if not. KIND can be one of:
     :LINEAR - A line break is inserted if and only if the immediatly
        containing section cannot be printed on one line.
     :MISER - Same as LINEAR, but only if ``miser-style'' is in effect.
        (See *PRINT-MISER-WIDTH*.)
     :FILL - A line break is inserted if and only if either:
       (a) the following section cannot be printed on the end of the
           current line,
       (b) the preceding section was not printed on a single line, or
       (c) the immediately containing section cannot be printed on one
           line and miser-style is in effect.
     :MANDATORY - A line break is always inserted.
   When a line break is inserted by any type of conditional newline, any
   blanks that immediately precede the conditional newline are ommitted
   from the output and indentation is introduced at the beginning of the
   next line. (See PPRINT-INDENT.)"
    (when (not (memq kind '(:linear :miser :fill :mandatory)))
      (signal-type-error kind '(member :linear :miser :fill :mandatory) 
                         "Invalid KIND argument ~A to PPRINT-NEWLINE"))
    (when (and *print-pretty* *logical-block-p*)    
      (setq stream (decode-stream-arg stream))
      (cond ((xp-structure-p stream)
             (pprint-newline+ kind stream))
            ((typep stream 'xp-stream)
             (pprint-newline+ kind (slot-value stream 'xp-structure)))
            (t (pp-newline stream kind))))
    nil)

(defun pprint-indent (relative-to n &optional (stream *standard-output*))
  "Specify the indentation to use in the current logical block if STREAM
   (which defaults to *STANDARD-OUTPUT*) is it is a pretty-printing stream
   and do nothing if not. (See PPRINT-LOGICAL-BLOCK.)  N is the indentation
   to use (in ems, the width of an ``m'') and RELATIVE-TO can be either:
     :BLOCK - Indent relative to the column the current logical block
        started on.
     :CURRENT - Indent relative to the current column.
   The new indentation value does not take effect until the following line
   break."
  (setq stream (decode-stream-arg stream))
  (when (not (memq relative-to '(:block :current)))
    (error "Invalid KIND argument ~A to PPRINT-INDENT" relative-to))
  (cond ((xp-structure-p stream)
         (pprint-indent+ relative-to (truncate n) stream))
        ((typep stream 'xp-stream)
         (pprint-indent+ relative-to (truncate n) (slot-value stream 'xp-structure)))
        (t nil)) ; ???(break)))
  nil)

(defun pprint-tab (kind colnum colinc &optional (stream *standard-output*))
  "If STREAM (which defaults to *STANDARD-OUTPUT*) is a pretty-printing
   stream, perform tabbing based on KIND, otherwise do nothing. KIND can
   be one of:
     :LINE - Tab to column COLNUM. If already past COLNUM tab to the next
       multiple of COLINC.
     :SECTION - Same as :LINE, but count from the start of the current
       section, not the start of the line.
     :LINE-RELATIVE - Output COLNUM spaces, then tab to the next multiple of
       COLINC.
     :SECTION-RELATIVE - Same as :LINE-RELATIVE, but count from the start
       of the current section, not the start of the line."
  (setq stream (decode-stream-arg stream))
  (when (not (memq kind '(:line :section :line-relative :section-relative)))
    (error "Invalid KIND argument ~A to PPRINT-TAB" kind))

  (when (and *print-pretty* *logical-block-p*)
    (cond ((xp-structure-p stream)
           (pprint-tab+ kind colnum colinc stream))
          ((typep stream 'xp-stream)
           (pprint-tab+ kind colnum colinc (slot-value stream 'xp-structure)))))
  nil)

;                        ---- COMPILED FORMAT ----

;Note that compiled format strings always print through xp streams even if
;they don't have any xp directives in them.  As a result, the compiled code
;can depend on the fact that the stream being operated on is an xp
;stream not an ordinary one.


(eval-when (:compile-toplevel :load-toplevel :execute)
; called by formatter frobs
(defun do-sub-format-0 (s control-string args)
    (setq s (if (xp-structure-p s)(xp-stream s)
              (if (output-stream-p s)
                s
                (require-type s '(satisfies output-stream-p)))))
                
    (let ((*format-control-string* control-string)
          (*format-top-level* t))      
      (cond ((and (or *print-pretty* *print-circle*)
                  (not (typep s 'xp-stream)))
             (maybe-initiate-xp-printing
              #'do-sub-format-1 s args))
            (t (do-sub-format-1 s args)))))

; called from above, format, and logical-block-sub
(defun do-sub-format-1 (stream args)
  (let ((*format-original-arguments* args)
        (*format-arguments* args)
        (*format-colon-rest* 'error))
    (declare (special *format-colon-rest*))
    (if (xp-structure-p stream)(setq stream (xp-stream stream)))
    (do-sub-format stream)
    ; copylist cause args is dynamic extent in format & formatter
    ; n.b. when printing lisp code its nearly always nil
    (setq args *format-arguments*)
    (if (and (consp args) *format-top-level*)(copy-list args) args)))

(defmacro formatter (control-string) ; maybe-initiate-xp-printing?
  (setq control-string (require-type control-string 'string))
  `(function 
    (lambda (s &rest args)
      ; IFFY because things can end up in the same place on the stack
      ; appearing EQ giving bogus circularity detection
      ; But now we have fixed things so we don't circle check rest args (ha!)
      (do-sub-format-0 s ,control-string args))))

(defmacro pprint-pop+ (args xp)
  `(if (pprint-pop-check+ ,args ,xp)
       (return-from logical-block nil)
       (pop ,args)))

(defun pprint-pop-check+ (args xp)
  (let ((current-length *current-length*))
    (declare (fixnum current-length))
    (setq current-length (setq *current-length* (1+ *current-length*)))
    (cond ((not (listp args))  ;must be first so supersedes length abbrev
	   (write-string++ ". " xp 0 2)
	   (write+ args xp)
	   T)
	  ((and *print-length* ;must supersede circle check
	        (not (< current-length *print-length*)))
	   (write-string++ "..." xp 0 3)
	   ;(setq *abbreviation-happened* T)
	   T)
	  ((and *circularity-hash-table* (not *format-top-level*)
                (not (zerop current-length)))
           (let ((circle (circularity-process xp args T)))
	     (case circle
	       (:first ;; note must inhibit rechecking of circularity for args.
                (write+ args xp T circle)
                T)
	       (:subsequent T)
	       (T nil)))))))

(defun check-block-abbreviation (xp args circle-check?)
  (cond ((not (listp args)) (write+ args xp) T)
	((and *print-level* (> *current-level* *print-level*))
	 (write-char++ #\# XP) 
         ;(setq *abbreviation-happened* T)
         T)
	((and *circularity-hash-table* circle-check? (neq args *xp-current-object*)
	      (eq (circularity-process xp args nil) :subsequent))
         T)
	(T nil)))


)


;                ---- PRETTY PRINTING FORMATS ----

(defun pretty-array (xp array)
  (when (typep xp 'xp-stream)(setq xp (slot-value xp 'xp-structure)))
  (cond ((vectorp array) (pretty-vector xp array))
	((zerop (array-rank array))
	 (write-string++ "#0A" xp 0 3)
	 (write+ (aref array) xp))
	(T (pretty-non-vector xp array))))

(defun pretty-vector (xp v)
  (pprint-logical-block (xp nil :prefix "#(" :suffix ")")
    (let ((end (length v)) (i 0))
      (declare (fixnum end i))
      (when (plusp end)
	(loop (pprint-pop)   ;HUH
	      (write+ (aref v i) xp)
	      (if (= (incf i) end) (return nil))
	      (write-char++ #\space xp)
	      (pprint-newline+ :fill xp))))))

(defun pretty-non-vector (xp array)
  (let* ((bottom (1- (array-rank array)))
	 (indices (make-list (1+ bottom) :initial-element 0))
	 (dims (array-dimensions array)))
    (funcall (formatter "#~DA") xp (1+ bottom))
    (labels ((pretty-slice (slice)
	       (pprint-logical-block (xp nil :prefix "(" :suffix ")")
		 (let ((end (nth slice dims))
		       (spot (nthcdr slice indices))
		       (i 0))
		   (when (plusp end)
		     (loop (pprint-pop)
			   (setf (car spot) i)
			   (if (= slice bottom)
			       (write+ (apply #'aref array indices) xp)
			       (pretty-slice (1+ slice)))
			   (if (= (incf i) end) (return nil))
			   (write-char++ #\space xp)
			   (pprint-newline+ (if (= slice bottom) :fill :linear) xp)))))))
      (pretty-slice 0))))

(defun pretty-structure (xp struc &aux (class (struct-def struc)) (slots (sd-slots class)))
  (when (typep xp 'xp-stream)(setq xp (slot-value xp 'xp-structure)))
  (let* ((class (ccl::struct-def struc)) ;;guaranteed non-NIL if this function is called
         (pf (structure-print-function class)))
    (cond 
     (pf
      (if (consp pf)
        (funcall (car pf) struc (xp-stream xp))
	(funcall pf struc (xp-stream xp) *current-level*)))
     (t 
      (pprint-logical-block (xp nil :prefix "#S(" :suffix ")")
        (pprint-pop)
        (write+ (sd-name class) xp)
        (start-block xp (if (cdr slots) " " "") nil "")
        (when slots
          (let ((pcase *print-case*))
            (loop 
              (let* ((slot (pop slots))(name (ssd-name slot)))
                (cond
                 ((symbolp name)
                  (pprint-pop)
                  (write-char++ #\: xp)
                  (write-pname (symbol-name name) pcase xp)
                  (write-char++ #\space xp)
                  (pprint-pop)
                  (write+ (uvref struc (ssd-offset slot)) xp)              
                  (when (null slots)(return nil))
                  (write-char++ #\space xp)
                  (pprint-newline+ :fill xp))
                 ((null slots)(return nil)))))))
        (end-block xp ""))))))



;Must use pprint-logical-block (no +) in the following three, because they are
;exported functions.

(defun pprint-linear (s list &optional (colon? T) atsign?)
  "Output LIST to STREAM putting :LINEAR conditional newlines between each
   element. If COLON? is NIL (defaults to T), then no parens are printed
   around the output. ATSIGN? is ignored (but allowed so that PPRINT-LINEAR
   can be used with the ~/.../ format directive."
  (declare (ignore atsign?))
  (pprint-logical-block (s list :prefix (if colon? "(" "")
			        :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (write-char++ #\space s)
	  (pprint-newline+ :linear s))))

(defun pprint-fill (s list &optional (colon? T) atsign?)
  "Output LIST to STREAM putting :FILL conditional newlines between each
   element. If COLON? is NIL (defaults to T), then no parens are printed
   around the output. ATSIGN? is ignored (but allowed so that PPRINT-FILL
   can be used with the ~/.../ format directive."
  (declare (ignore atsign?))
  (pprint-logical-block (s list :prefix (if colon? "(" "")
			        :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (write-char++ #\space s)
	  (pprint-newline+ :fill s))))

(defun pprint-tabular (s list &optional (colon? T) atsign? (tabsize nil))
  "Output LIST to STREAM tabbing to the next column that is an even multiple
   of TABSIZE (which defaults to 16) between each element. :FILL style
   conditional newlines are also output between each element. If COLON? is
   NIL (defaults to T), then no parens are printed around the output.
   ATSIGN? is ignored (but allowed so that PPRINT-TABULAR can be used with
   the ~/.../ format directive."
  (declare (ignore atsign?))
  (when (null tabsize) (setq tabsize 16))
  (pprint-logical-block (s list :prefix (if colon? "(" "")
			        :suffix (if colon? ")" ""))    
    (pprint-exit-if-list-exhausted)
    (loop (write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (write-char++ #\space s)
	  (pprint-tab+ :section-relative 0 tabsize s)
	  (pprint-newline+ :fill s))))

; perhaps should use alternate-fn-call instead
(defun fn-call (xp list)
  (funcall (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~_~}~:>") xp list))

;Although idiosyncratic, I have found this very useful to avoid large
;indentations when printing out code.

(defun alternative-fn-call (xp list)
  (if (> (length (symbol-name (car list))) 12)
      (funcall (formatter "~:<~1I~@{~W~^ ~_~}~:>") xp list)
      (funcall (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~_~}~:>") xp list)))

(defun bind-list (xp list &rest args)
  (declare (ignore args))
  (if (do ((i 50 (1- i))
	   (ls list (cdr ls))) ((null ls) t)
	(when (or (not (consp ls)) (not (symbolp (car ls))) (minusp i))
	  (return nil)))
      (pprint-fill xp list)
      (funcall (formatter "~:<~@{~:/pprint-fill/~^ ~_~}~:>") xp list)))

(defun block-like (xp list &rest args)
    (declare (ignore args))
  (funcall (formatter "~:<~1I~^~W~^ ~@_~W~^~@{ ~_~W~^~}~:>") xp list))

(defun defun-like (xp list &rest args)
    (declare (ignore args))
  (funcall (formatter "~:<~1I~W~^ ~@_~W~^ ~@_~:/pprint-fill/~^~@{ ~_~W~^~}~:>")
	    xp list))

(defun defvar-like (xp list &rest args)
    (declare (ignore args))
  (funcall (formatter "~:<~1I~W~^ ~@_~W~^ ~@_~W~^~@{ ~_~W~^~}~:>")
	    xp list))

(defun print-fancy-fn-call (xp list template)
  (let ((i 0) (in-first-section T))
    (declare (fixnum i))
    (pprint-logical-block+ (xp list "(" ")" nil T nil)
      (write+ (pprint-pop) xp)
      (pprint-indent+ :current 1 xp)
      (loop
	(pprint-exit-if-list-exhausted)
	(write-char++ #\space xp)
	(when (eq i (car template))
	  (pprint-indent+ :block (cadr template) xp)
	  (setq template (cddr template))
	  (setq in-first-section nil))
	(pprint-newline (cond ((and (zerop i) in-first-section) :miser)
			      (in-first-section :fill)
			      (T :linear))
			xp)
	(write+ (pprint-pop) xp)
	(incf i)))))

(defun defmethod-like (xp list &rest args)
  (declare (ignore args))
  (cond ((and (consp (cdr list))(consp (cddr list))(listp (caddr list)))
         (defun-like xp list))
        (t (defsetf-print xp list))))


(defun maybelab (xp item &rest args)
    (declare (ignore args) (special need-newline indentation))
  (when (typep xp 'xp-stream)(setq xp (slot-value xp 'xp-structure)))
  (when need-newline (pprint-newline+ :mandatory xp))
  (cond ((and item (symbolp item))
	 (write+ item xp)
	 (setq need-newline nil))
	(T (pprint-tab+ :section indentation 0 xp)
	   (write+ item xp)
	   (setq need-newline T))))

(defun function-call-p (x)
  (and (consp x) (symbolp (car x)) (fboundp (car x))))



;THE FOLLOWING STUFF SETS UP THE DEFAULT *PRINT-PPRINT-DISPATCH*
 
;This is an attempt to specify a correct format for every form in the CL book
;that does not just get printed out like an ordinary function call 
;(i.e., most special forms and many macros).  This of course does not 
;cover anything new you define.

(defun let-print (xp obj)
  (funcall (formatter "~:<~1I~W~^ ~@_~/ccl::bind-list/~^~@{ ~_~W~^~}~:>") xp obj))

(defun cond-print (xp obj)
  (funcall (formatter "~:<~W~^ ~:I~@_~@{~:/pprint-linear/~^ ~_~}~:>") xp obj))

(defun dmm-print (xp list)
  (print-fancy-fn-call xp list '(3 1)))

(defun defsetf-print (xp list)
  (print-fancy-fn-call xp list '(3 1)))

(defun do-print (xp obj)
  (funcall 
 (formatter "~:<~W~^ ~:I~@_~/ccl::bind-list/~^ ~_~:/pprint-linear/ ~1I~^~@{ ~_~W~^~}~:>")
           xp obj))


(defun flet-print (xp obj)
  (funcall (formatter "~:<~1I~W~^ ~@_~:<~@{~/ccl::block-like/~^ ~_~}~:>~^~@{ ~_~W~^~}~:>")
	   xp obj))

(defun function-print (xp list)
  (if (and *print-abbreviate-quote* (consp (cdr list)) (null (cddr list)))
      (format (xp-stream xp) "#'~W" (cadr list))
      (fn-call xp list)))

(defun mvb-print (xp list)
  (print-fancy-fn-call xp list '(1 3 2 1)))

(defun prog-print (xp list)
  (let ((need-newline T) (indentation (1+ (length (symbol-name (car list)))))) ; less?
    (declare (special need-newline indentation))
    (funcall (formatter "~:<~W~^ ~:/pprint-fill/~^ ~@{~/ccl::maybelab/~^ ~}~:>")
	     xp list)))


(defun progn-print (xp list)
  (funcall (formatter "~:<~1I~@{~W~^ ~_~}~:>") xp list))

(defun setq-print (xp obj)
  (funcall (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~:_~W~^ ~_~}~:>") xp obj))

(defun quote-print (xp list)
  (if (and (consp (cdr list)) (null (cddr list)))
      (format (xp-stream xp) "'~W" (cadr list))
      (pprint-fill xp list)))

(defun tagbody-print (xp list)
  (let ((need-newline (and (consp (cdr list))
			   (symbolp (cadr list)) (cadr list)))
	(indentation (1+ (length (symbol-name (car list))))))
    (declare (special need-newline indentation))
    (funcall (formatter "~:<~W~^ ~@{~/ccl::maybelab/~^ ~}~:>") xp list)))

(defun up-print (xp list)
  (print-fancy-fn-call xp list '(0 3 1 1)))

;here is some simple stuff for printing LOOP

;The challange here is that we have to effectively parse the clauses of the
;loop in order to know how to print things.  Also you want to do this in a 
;purely incremental way so that all of the abbreviation things work, and
;you wont blow up on circular lists or the like.  (More aesthic output could
;be produced by really parsing the clauses into nested lists before printing them.)

;The following program assumes the following simplified grammar of the loop
;clauses that explains how to print them.  Note that it does not bare much
;resemblence to the right parsing grammar, however, it produces half decent
;output.  The way to make the output better is to make the grammar more
;detailed.  
;
;loop == (LOOP {clause}*)      ;one clause on each line.
;clause == block | linear | cond | finally
;block == block-head {expr}*   ;as many exprs as possible on each line.
;linear == linear-head {expr}* ;one expr on each line.
;finally == FINALLY [DO | DOING | RETURN] {expr}* ;one expr on each line.
;cond == cond-head [expr]
;          clause
;	   {AND clause}*       ;one AND on each line.
;        [ELSE
;          clause
;	   {AND clause}*]      ;one AND on each line.
;        [END]
;block-head == FOR | AS | WITH | AND
;              | REPEAT | NAMED | WHILE | UNTIL | ALWAYS | NEVER | THEREIS | RETURN
;              | COLLECT | COLLECTING | APPEND | APPENDING | NCONC | NCONCING | COUNT
;              | COUNTING | SUM | SUMMING | MAXIMIZE | MAXIMIZING | MINIMIZE | MINIMIZING 
;linear-head == DO | DOING | INITIALLY
;var-head == FOR | AS | WITH
;cond-head == IF | WHEN | UNLESS
;expr == <anything that is not a head symbol>

;Note all the string comparisons below are required to support some
;existing implementations of LOOP.
(defun token-type (token &aux string)
  (cond ((not (symbolp token)) :expr)
	((string= (setq string (string token)) "FINALLY") :finally)
	((member string '("IF" "WHEN" "UNLESS") :test #'string=) :cond-head)
	((member string '("DO" "DOING" "INITIALLY") :test #'string=) :linear-head)
	((member string '("FOR" "AS" "WITH" "AND" "END" "ELSE"
			  "REPEAT" "NAMED" "WHILE" "UNTIL" "ALWAYS" "NEVER"
			  "THEREIS" "RETURN" "COLLECT" "COLLECTING" "APPEND"
			  "APPENDING" "NCONC" "NCONCING" "COUNT" "COUNTING"
			  "SUM" "SUMMING" "MAXIMIZE" "MAXIMIZING"
			  "MINIMIZE" "MINIMIZING")
		 :test #'string=)
	 :block-head)
	(T :expr)))

; maybe put in a separate file (replace write-char by write-char+)
(defun pretty-loop (xp loop)
  (if (not (and (consp (cdr loop)) (symbolp (cadr loop)))) ; old-style loop
      (tagbody-print xp loop)
      (pprint-logical-block (xp loop :prefix "(" :suffix ")")
	(let (token type)
	  (labels ((next-token ()
		     (pprint-exit-if-list-exhausted)
		     (setq token (pprint-pop))
		     (setq type (token-type token)))
		   (print-clause (xp)
		     (case type
		       (:linear-head (print-exprs xp nil :mandatory))
		       (:cond-head (print-cond xp))
		       (:finally (print-exprs xp T :mandatory))
		       (otherwise (print-exprs xp nil :fill))))
		   (print-exprs (xp skip-first-non-expr newline-type)
		     (pprint-logical-block (xp nil)
		       (write+ token xp)
		       (next-token)
		       (when (and skip-first-non-expr (not (eq type :expr)))
			 (write-char+ #\space xp)
			 (write+ token xp)
			 (next-token))
		       (when (eq type :expr)
			 (write-char+ #\space xp)
			 (pprint-indent :current 0 xp)
			 (loop (write+ token xp)
			       (next-token)
			       (when (not (eq type :expr)) (return nil))
			       (write-char+ #\space xp)
			       (pprint-newline newline-type xp)))))
		   (print-cond (xp)
		     (pprint-logical-block (xp nil)
		       (write+ token xp)
		       (next-token)
		       (when (eq type :expr)
			 (write-char+ #\space xp)
			 (write+ token xp)
			 (next-token))
		       (write-char+ #\space xp)
		       (pprint-indent :block 2 xp)
		       (pprint-newline :linear xp)
		       (print-clause xp)
		       (print-and-list xp)
		       (when (string= (string token) "ELSE")
			 (print-else-or-end xp)
			 (write-char+ #\space xp)
			 (pprint-newline :linear xp)
			 (print-clause xp)
			 (print-and-list xp))
		       (when (string= (string token) "END")
			 (print-else-or-end xp))))
		   (print-and-list (xp)
		     (loop (when (not (string= (string token) "AND")) (return nil))
			   (write-char+ #\space xp)
			   (pprint-newline :mandatory xp)
			   (write+ token xp)
			   (next-token)
			   (write-char+ #\space xp)
			   (print-clause xp)))
		   (print-else-or-end (xp)
		     (write-char+ #\space xp)
		     (pprint-indent :block 0 xp)
		     (pprint-newline :linear xp)
		     (write+ token xp)
		     (next-token)
		     (pprint-indent :block 2 xp)))
	    (pprint-exit-if-list-exhausted)
	    (write+ (pprint-pop) xp)
	    (next-token)
	    (write-char+ #\space xp)
	    (pprint-indent :current 0 xp)
	    (loop (print-clause xp)
		  (write-char+ #\space xp)
		  (pprint-newline :linear xp)
                  ; without this we can loop forever
                  (if (and *print-level*
			   (>= *current-level* *print-level*))
		    (return))))))))

;Backquote is a big problem we MUST do all this reconsing of structure in
;order to get a list that will trigger the right formatting functions to
;operate on it.  On the other side of the coin, we must use a non-list structure 
;for the little backquote printing markers to ensure that they will always
;print out the way we want no matter what the code printers say.
;  Note that since it is sometimes possible to write the same
;backquote form in several ways, this might not necessarily print out a
;form in exactly the way you wrote it.  For example '`(a .,b) and '`(a ,@b)
;both print out as `'(a .,b), because the backquote reader produces the
;same code in both cases.


(setq *IPD* (make-pprint-dispatch-table))

(set-pprint-dispatch+ '(satisfies function-call-p) #'alternative-fn-call '(-5) *IPD*)
(set-pprint-dispatch+ 'cons #'pprint-fill '(-10) *IPD*)

(set-pprint-dispatch+ '(cons (member defstruct)) #'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member block)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member case)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member catch)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member ccase)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member compiler-let)) #'let-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member cond)) #'cond-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member ctypecase)) #'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member defclass)) #'defun-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member ctypecase)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member defconstant)) #'defvar-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member define-setf-expander)) #'defun-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member defmacro)) #'defun-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member define-modify-macro)) #'dmm-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member defparameter)) #'defvar-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member defsetf)) #'defsetf-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member define-setf-expander)) #'defun-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member cl:defstruct)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member deftype)) #'defun-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member defun)) #'defun-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member defmethod)) #'defmethod-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member defvar)) #'defvar-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member do)) #'do-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member do*)) #'do-print '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member do-all-symbols)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member do-external-symbols)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member do-symbols)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member dolist)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member dotimes)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member ecase)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member etypecase)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member eval-when)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member flet)) #'flet-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member function)) #'function-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member generic-function)) #'fn-call '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member labels)) #'flet-print '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member lambda)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member let)) #'let-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member let*)) #'let-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member locally)) #'block-like '(0) *IPD*)

(set-pprint-dispatch+ '(cons (member loop)) #'pretty-loop '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member macrolet)) #'flet-print '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member multiple-value-bind)) #'mvb-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member multiple-value-setq)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member prog)) #'prog-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member prog*)) #'prog-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member progv)) #'defvar-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member psetf)) #'setq-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member psetq)) #'setq-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member quote)) #'quote-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member return-from)) #'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member setf)) #'setq-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member setq)) #'setq-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member tagbody)) #'tagbody-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member throw)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member typecase)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member unless)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member unwind-protect)) #'up-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member when)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member with-input-from-string)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member with-open-file)) #'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member with-open-stream)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member with-output-to-string)) #'block-like '(0) *IPD*) 


;so only happens first time is loaded. - why doesn't this work right?
; cause we have *print-pprin... bound to NIL
(when  t ;(eq *print-pprint-dispatch* T)
  (setq *print-pprint-dispatch* (copy-pprint-dispatch nil)))

(setq *error-print-circle* t)  ; now we can circle-print

; 82 bytes shorter but uglier
(defun write-not-pretty (stream object level list-kludge circle)
  (declare (type fixnum level) (type (or null fixnum) list-kludge))
  (when (xp-structure-p stream)(setq stream (xp-stream stream)))  
  (cond ((eq circle :subsequent)
         (if  list-kludge (stream-write-char stream #\)))
         (return-from write-not-pretty nil))
        ((not list-kludge))
        ((null object)(return-from write-not-pretty nil))
        ((and (not (consp object)) (not circle))
         (stream-write-entire-string stream " . "))
        ((eq circle :first)
         (when (consp object) (stream-write-char stream #\())
         (write-a-frob object stream level list-kludge)
         (when (consp object) (stream-write-char stream #\)))
         (return-from write-not-pretty nil))                     
        (t (stream-write-char stream #\space)))
  (write-a-frob object stream level list-kludge))

(def-standard-initial-binding *PRINT-PPRINT-DISPATCH* (copy-pprint-dispatch nil)) ; We have to support this.

(setq *standard-pprint-dispatch-table* (copy-pprint-dispatch nil))

(eval-when (:load-toplevel :execute) 
  (setq *error-print-circle* t))

;changes since last documentation.
;~/fn/ only refers to global function values, not lexical.

;------------------------------------------------------------------------

;Copyright 1989,1990 by the Massachusetts Institute of Technology, Cambridge, 
;Massachusetts.

;Permission to use, copy, modify, and distribute this software and its
;documentation for any purpose and without fee is hereby granted,
;provided that this copyright and permission notice appear in all
;copies and supporting documentation, and that the name of M.I.T. not
;be used in advertising or publicity pertaining to distribution of the
;software without specific, written prior permission. M.I.T. makes no
;representations about the suitability of this software for any
;purpose.  It is provided "as is" without express or implied warranty.

;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;    SOFTWARE.

;------------------------------------------------------------------------

#|
	Change History (most recent last):
	2	12/29/94	akh	merge with d13
|# ;(do not edit past this line!!)
