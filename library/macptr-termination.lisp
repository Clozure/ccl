; -*- Mode: Lisp; Package: CCL; -*-
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

;;; macptr-termination.lisp
;;;
;;; Allows you to associate a termination function with a macptr.
;;; The termination function will be called with the macptr as
;;; its single arg when the macptr is no longer accessible in the
;;; mac heap (i.e. when the garbage collector decides that its
;;; storage can be recycled).
;;;
;;; This file is provided primarily for backward compatibility.
;;; You can use terminate-when-unreachable for new code.

;; Modification History
;;
;; 11/26/96 bill Remove cons-terminable-macptr from the PPC version of the code.
;;               It referenced undefined $macptr-size and it was not used.
;; ------------- 4.0
;; 09/12/96 bill *slave-macptrs-table* is for non-terminable slaves.
;;               *terminable-slaves-table* is for terminable slaves.
;;               *terminable-slaves-table* is not weak, *slave-macptrs-table* still is.
;;               *terminable-slaves-table* is an EQL hash table which maps a copy of the
;;               slave to the master.
;;               When a slave is terminated, its entry is explicitly removed from *terminable-slaves-table*.
;;               This means that a master will be removed on the next GC after all of
;;               its slaves are terminated. Not optimal, but it guarantees that all the slaves are
;;               disposed before the master.
;; 08/23/96 bill A *save-exit-function* to clear-terminable-macptrs
;; 08/21/96 bill add the SK8 register-slave-macptr & teminable-macptr-p functions
;;               and the :deactivate-only keyword to deactivate-macptr
;; ------------- 4.0b1
;; 02/28/96 bill Make it work in PPC MCL
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Documentation
;;;

#|

SET-POST-EGC-HOOK-ENABLED-P value
  This package works by setting MCL's undocumented ccl::*post-gc-hook*.
The hook is normally called only after a full GC. If you want it to
be called after each ephemeral GC as well, call this with a true value.

POST-EGC-HOOK-ENABLED-P
  Returns true if the post gc hook will be called after EGC as well as
after full GC.¬

ADD-PRE-GC-HOOK hook
DELETE-PRE-GC-HOOK hook
ADD-POST-GC-HOOK hook
DELETE-POST-GC-HOOK hook
  MCL's ccl::*pre-gc-hook* and ccl::*post-gc-hook* can each contain
either a function or NIL. These four functions extend that functionality
by maintaining a list of functions for each of the two hooks. Hooks are
compared with EQ, so it is best to pass a symbol that has a global
definition (see the last form in this file).

MAKE-TERMINABLE-MACPTR macptr termination-function &key master
  Creates and returns a terminable macptr. It will point at the same Mac
Heap address as the macptr arg. When the return value becomes scavengeable
(e.g. no longer accessible in the Lisp heap), will call the
termination-function with a single arg, the returned macptr. If the
termination-function's return value is non-NIL, will free the macptr.
Otherwise, will assume that you decided not to terminate it, and will
call the termination-function again the next time the GC runs and
it is scavengeable.  If master is supplied, then
initialize the new terminable macptr as a slave to the given master.
All slave terminable macptrs are terminated before their master is terminated.
Raise an error if macptr is not a macptr or the supplied master
is not a terminable macptr.

REGISTER-SLAVE-MACPTR slave-macptr master-macptr
  Registers a macptr as the slave of a terminable macptr.
A master terminable macptr is not terminated until all of its slaves
have been GC'ed (and terminated if appropriate).
Raise an error if master-macptr is not a terminable macptr.

TERMINABLE-MACPTR-P thing
returns t if thing is an active terminable or gcable macptr;
otherwise returns  nil.

DEACTIVATE-MACPTR macptr &key deactivate-only
  If macptr has an associated termination action,
cancel that action. If deactivate-only is nil, call the
termination action before canceling it, and change
the macptr to a dead macptr.  Raise an error if macptr
is not a macptr.  Return nil if not a terminable macptr
or if deactivate-only is nil and disposal function returns
nil;  otherwise return true.

|#

(in-package "CCL")

(provide "MACPTR-TERMINATION")

(export '(set-post-egc-hook-enabled-p post-egc-hook-enabled-p
          add-pre-gc-hook delete-pre-gc-hook add-post-gc-hook delete-post-gc-hook
          make-terminable-macptr register-slave-macptr terminable-macptr-p deactivate-macptr))

; Map slave-macptr to master-macptr
; This holds on to the master until the slave is GC'd
(defvar *slave-macptrs-table*
  (make-hash-table :test 'eq :weak :key))

; Map a copy of a terminable slave to its master
; This holds on to the master until the slave is terminated
(defvar *terminable-slaves-table*
  (make-hash-table :test 'eql))

(defun register-slave-macptr (slave-macptr master-macptr)
  (unless (terminable-macptr-p master-macptr)
    (error "~s is not a terminable macptr" master-macptr))
  (unless (macptrp slave-macptr)
    (setq slave-macptr (require-type slave-macptr 'macptr)))
  (if (terminable-macptr-p slave-macptr)
    (setf (gethash (%inc-ptr slave-macptr 0) *terminable-slaves-table*) master-macptr)
    (setf (gethash slave-macptr *slave-macptrs-table*) master-macptr)))

(defun dispose-gcable-macptr (macptr)
  (let ((flags (macptr-flags macptr)))
    ; we set to $flags_normal before calling the dispose function.
    ; (client code can and does depend on this).
    ; hence, if it aborts a memory leak results.
    ; if we were to wait until after the user function returns
    ; to put in the $flags_normal, then it will get called again
    ; and might try to free something twice: crash!
    (setf (macptr.flags macptr) #.$flags_normal)
    (case flags
      (#.$flags_normal nil)
      (#.$flags_DisposHandle (#_DisposeHandle macptr) t)
      (#.$flags_DisposPtr    (#_DisposePtr    macptr) t)
      (#.$flags_DisposWindow (#_DisposeWindow macptr) t)
      (#.$flags_DisposGWorld (#_DisposeGWorld macptr) t)
      (otherwise (error "Macptr has bogus flags")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The PPC version uses the new general termination support
;;;

#+ppc-target
(progn

(eval-when (:compile-toplevel :execute)
  (require "LISPEQU"))

(defvar *macptr-termination-population*
  (%cons-terminatable-alist))

(defun make-terminable-macptr (macptr termination-function &key master)
  (let* ((p (%inc-ptr macptr 0))
         (cell (list (cons p termination-function)))
         (population *macptr-termination-population*))
    (without-interrupts
     (setf (cdr cell) (population-data population)
           (population-data population) cell))
    (when master
      (register-slave-macptr p master))
    p))

(defun terminable-macptr-p (thing)
  (or (not (eql $flags_normal (macptr-flags thing)))
      (member thing (population-data *macptr-termination-population*)
              :key 'car)))

(defun deactivate-macptr (macptr &key (deactivate-only t))
  (unless (macptrp macptr)
    (setq macptr (require-type macptr 'macptr)))
  (let ((termination-function nil)
        (population *macptr-termination-population*))
    (flet ((test (macptr cell) (and (eq macptr (car cell)) (setq termination-function (cdr cell)))))
      (declare (dynamic-extent #'test))
      (without-interrupts
       (setf (population-data population)
             (delete macptr (population-data population)
                     :test #'test
                     :count 1))))
    (when termination-function
      (remhash macptr *terminable-slaves-table*))
    (if deactivate-only
      termination-function
      (prog1
        (if termination-function
          (funcall termination-function macptr)
          (progn
            (dispose-gcable-macptr macptr)
            (remhash macptr *slave-macptrs-table*)))
        (macptr->dead-macptr macptr)))))

; The post GC hook
(defun terminate-macptrs ()
  (let ((population *macptr-termination-population*)
        list cell)
    (loop
      (without-interrupts
       (setq list (population-termination-list population))
       (unless list (return))
       (setf cell (car list)
             (population-termination-list population) (cdr list)
             (cdr list) nil))
      (let ((macptr (car cell)))
        (if (funcall (cdr cell) macptr)
          (remhash macptr *terminable-slaves-table*)
          (without-interrupts
           (setf (cdr list) (population-data population)
                 (population-data population) list)))))))

(defun macptr->dead-macptr (macptr)
  (if (macptrp macptr)
    (%macptr->dead-macptr macptr)
    (macptr->dead-macptr (require-type macptr 'macptr))))



; Call this before save-application.
; It makes no sense to keep terminable macptrs around after that.
; They'll be dead-macptr's then causing lots of grief.
(defun clear-terminable-macptrs ()
  (let ((population *macptr-termination-population*))
    (setf (population-data population) nil
          (population-termination-list population) nil)
    (clrhash *slave-macptrs-table*)))

)  ; end of #+ppc-target progn


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The 68K version needs to work harder.
;;; It also requires a kernel patch.
;;; It won't work in a vanilla MCL 3.0 (or 2.0).
;;;

#-ppc-target
(progn

(eval-when (:compile-toplevel :execute)
  (require "LAPMACROS")

  (defconstant $flags_terminable 5)
  (defconstant $flags_terminate_when_ready 6)
  
  (defconstant $gc-finalize-macptrs-bit (- 26 $fixnumshift))
  (defconstant $gc-post-egc-hook-p (- 25 $fixnumshift))
  
  (def-accessors () %svref
    nil                                   ; macptr.ptr
    nil                                   ; macptr.flags
    macptr.link
    macptr.id
    macptr-size)
  
  ; This is not exported from the kernel. In future MCL versions, it
  ; will be and this definition will not be necessary.
  ; This value came from the lisp-8.map file for the new kernel
  (defconstant $gcable_ptrs (- #xD84 #x1000))
  )

(defun gcable-ptrs-head ()
  (lap-inline ()
    (move.l (a5 $gcable_ptrs) acc)))

(defun (setf macptr-flags) (value p)
  (setq p (require-type p 'macptr))
  (setq value (require-type value 'fixnum))
  (lap-inline (value p)
    (move.l arg_z atemp0)
    (getint arg_y)
    (move.l arg_y (atemp0 $macptr.flags)))
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; cons-terminable-macptr & map-over-terminated-macptrs
;;; are the low-level interface to this package.
;;;

; Create a terminable macptr from another macptr
(defun cons-terminable-macptr (macptr &optional (id 0))
  (setq macptr (require-type macptr 'macptr))
  (setq id (require-type id 'fixnum))
  (let ((p (make-uvector macptr-size $v_macptr :initial-element 0)))
    (%setf-macptr p macptr)
    (setf (macptr-flags p) $flags_terminable
          (macptr.id p) id)
    (lap-inline (p)
      (move.l arg_z atemp0)
      (move.l (a5 $gcable_ptrs) (svref atemp0 macptr.link))
      (move.l atemp0 (a5 $gcable_ptrs)))
    p))

; Calls function with each terminated macptr.
; If function returns NIL, will not reap the macptr;
; it will reappear in the list of terminated macptrs after the next GC
; (assuming FUNCTION didn't store it somewhere).
(defun map-over-terminated-macptrs (function)
  (declare (fixnum *gc-event-status-bits*))
  (when (logbitp $gc-finalize-macptrs-bit *gc-event-status-bits*)
    (let ((done? nil))
      (unwind-protect
        (let ((p (gcable-ptrs-head)))
          (setq *gc-event-status-bits*
                (the fixnum 
                     (bitclr $gc-finalize-macptrs-bit *gc-event-status-bits*)))
          (loop
            (when (eql 0 p)
              (return))
            (when (eql $flags_terminate_when_ready (macptr-flags p))
              ; We set to $flags_normal BEFORE calling the user function.
              ; Hence, if it aborts a memory leak results.
              ; If we were to wait until after the user function returns
              ; to put in the $flags_normal, then it will get called again
              ; and might try to free something twice: CRASH!
              (setf (macptr-flags p) $flags_normal)
              (unless (funcall function p)
                (setf (macptr-flags p) $flags_terminable)))
            (setq p (macptr.link p)))
          (setq done? t))
        (unless done?
          (setq *gc-event-status-bits*
                (the fixnum
                     (bitset $gc-finalize-macptrs-bit *gc-event-status-bits*))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; make-terminable-macptr is the user entry point.
;;;

; This table cannot be weak on key since hash tables are reaped before gcable-macptrs.
(defvar *termination-functions-table* (make-hash-table :test 'eql))

(defvar *terminable-macptr-max-id* most-negative-fixnum)
(defvar *free-terminable-macptr-ids* nil)

(defun make-terminable-macptr (macptr termination-function &key master)
  (let* ((id (or (pop *free-terminable-macptr-ids*)
                 (incf *terminable-macptr-max-id*)))
         (p (cons-terminable-macptr macptr id)))
    (setf (gethash id *termination-functions-table*) termination-function
          (gethash nil *termination-functions-table*) nil)       ; clear cache
    (when master
      (register-slave-macptr p master))
    p))

(defun terminable-macptr-p (thing)
  (not (eql $flags_normal (macptr-flags thing))))

(defun terminate-macptrs ()
  (map-over-terminated-macptrs
   #'(lambda (p)
       (let* ((id (macptr.id p))
              (termination-function (gethash id *termination-functions-table*)))
         (if termination-function
           (when (funcall termination-function p)
             (remhash id *termination-functions-table*)
             (remhash p *terminable-slaves-table*)
             (push id *free-terminable-macptr-ids*)
             t)
           (progn
             (cerror "Continue." "Can't find ~s in ~s"
                     p '*termination-functions-table*)
             t))))))

(defun deactivate-macptr (macptr &key (deactivate-only t))
  (setq macptr (require-type macptr 'macptr))
  (let ((flags (macptr-flags macptr))
        (termination-function nil))
    (unless (eql $flags_normal flags)
      (when (or (eql flags $flags_terminable)
                (eql flags $flags_terminate_when_ready))
        (setf (macptr-flags macptr) $flags_normal)
        (let ((id (macptr.id macptr)))
          (setq termination-function
                (if deactivate-only
                  t
                  (gethash id *termination-functions-table*)))
          (remhash id *termination-functions-table*)
          (push id *free-terminable-macptr-ids*)
          (remhash macptr *terminable-slaves-table*)))
      (if deactivate-only
        termination-function
        (prog1
          (if termination-function
            (funcall termination-function macptr)
            (progn
              (dispose-gcable-macptr macptr)
              (remhash macptr *slave-macptrs-table*)))
          (macptr->dead-macptr macptr))))))

#+ccl-3
(defun macptr->dead-macptr (macptrObject)
  (require-type macptrObject 'macptr)
  (lap-inline ()
    (:variable macptrobject)
    (move.l (varg macptrObject) atemp0)
    (set_vsubtype ($ $v_badptr) atemp0 da))
  macptrObject)
  
#-ccl-3
(defun macptr->dead-macptr (macptrObject)
  (require-type macptrObject 'macptr)
  (lap
    (move.l (varg macptrObject) atemp0)
    (move.b ($ $v_badptr) (atemp0 $v_subtype)))
  macptrObject)

; Call this before save-application.
; It makes no sense to keep terminable macptrs around after that.
; They'll be dead-macptr's then causing lots of grief.
(defun clear-terminable-macptrs ()
  (clrhash *termination-functions-table*)
  (clrhash *slave-macptrs-table*))

)  ; End of #-ppc-target progn

(pushnew 'clear-terminable-macptrs *save-exit-functions*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Backward compatibility for the gc-hook maintenance functions.
;;;

(defun add-pre-gc-hook (hook)
  (add-gc-hook hook :pre-gc))

(defun delete-pre-gc-hook (hook)
  (remove-gc-hook hook :pre-gc))

(defun add-post-gc-hook (hook)
  (add-gc-hook hook :post-gc))

(defun delete-post-gc-hook (hook)
  (remove-gc-hook hook :post-gc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Enabling the ccl::*post-gc-hook* after EGC
;;;

#|  ; These are built in now

(defun post-egc-hook-enabled-p ()
  (declare (fixnum *gc-event-status-bits*))
  (logbitp $gc-post-egc-hook-p *gc-event-status-bits*))

(defun set-post-egc-hook-enabled-p (value)
  (declare (fixnum *gc-event-status-bits*))
  (setq *gc-event-status-bits* 
        (if (setq value (not (null value)))
          (the fixnum (bitset $gc-post-egc-hook-p *gc-event-status-bits*))
          (the fixnum (bitclr $gc-post-egc-hook-p *gc-event-status-bits*))))
  value)

|#
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Install the hook
;;;

(add-post-gc-hook 'terminate-macptrs)
