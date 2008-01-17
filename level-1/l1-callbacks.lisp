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

;;; l1-callbacks.lisp

(in-package "CCL")

(defstatic *callback-lock* (make-lock))


;;; MacOS toolbox routines were once written mostly in Pascal, so some
;;; code still refers to callbacks from foreign code as "pascal-callable
;;; functions".

; %Pascal-Functions% Entry
(def-accessor-macros %svref
  pfe.routine-descriptor
  pfe.proc-info
  pfe.lisp-function
  pfe.sym
  pfe.without-interrupts
  pfe.trace-p)

(defun %cons-pfe (routine-descriptor proc-info lisp-function sym without-interrupts)
  (vector routine-descriptor proc-info lisp-function sym without-interrupts nil))

;;; (defcallback ...) expands into a call to this function.
(defun define-callback-function (lisp-function  &optional doc-string (without-interrupts t) monitor-exception-ports
                                                   &aux name trampoline)
  (unless (functionp lisp-function)
    (setq lisp-function (require-type lisp-function 'function)))
  (unless (and (symbolp (setq name (function-name lisp-function)))
               ;;Might as well err out now before do any _Newptr's...
               (not (constant-symbol-p name)))
    (report-bad-arg name '(and symbol (not (satisfies constantp)))))
  (with-lock-grabbed (*callback-lock*)
    (let ((len (length %pascal-functions%)))
      (declare (fixnum len))
      (when (and name (boundp name))
        (let ((old-tramp (symbol-value name)))
          (dotimes (i len)
            (let ((pfe (%svref %pascal-functions% i)))
              (when (and (vectorp pfe)
                         (eql old-tramp (pfe.routine-descriptor pfe)))
                
                (setf (pfe.without-interrupts pfe) without-interrupts)
                (setf (pfe.lisp-function pfe) lisp-function)
                (setq trampoline old-tramp))))))
      (unless trampoline
        (let ((index (dotimes (i (length %pascal-functions%)
                               (let* ((new-len (if (zerop len) 32 (* len 2)))
                                      (new-pf (make-array (the fixnum new-len))))
                                 (declare (fixnum new-len))
                                 (dotimes (i len)
                                   (setf (%svref new-pf i) (%svref %pascal-functions% i)))
                                 (do ((i len (1+ i)))
                                     ((>= i new-len))
                                   (declare (fixnum i))
                                   (setf (%svref new-pf i) nil))
                                 (setq %pascal-functions% new-pf)
                                 len))
                       (unless (%svref %pascal-functions% i)
                         (return i)))))
          (setq trampoline (make-callback-trampoline index))
          (setf (%svref %pascal-functions% index)
                (%cons-pfe trampoline monitor-exception-ports lisp-function name without-interrupts)))))
    ;;(%proclaim-special name)          ;
    ;; already done by defpascal expansion
    (when name (set name trampoline))
    (record-source-file name 'defcallback)
    (when (and doc-string *save-doc-strings*)
      (setf (documentation name 'variable) doc-string))
    (when *fasload-print* (format t "~&~S~%" name))
    (or name trampoline)))


(defun %lookup-pascal-function (index)
  (declare (optimize (speed 3) (safety 0)))
  (with-lock-grabbed (*callback-lock*)
    (let* ((pfe (svref %pascal-functions% index)))
      (values (pfe.lisp-function pfe)
              (pfe.without-interrupts pfe)
	      (pfe.trace-p pfe)))))


(defun %callback-function (pointer)
  (if (typep pointer 'symbol)
    (setq pointer (symbol-value pointer)))
  (with-lock-grabbed (*callback-lock*)
    (let* ((index (dotimes (i (length %pascal-functions%))
                    (when (eql (pfe.routine-descriptor (svref %pascal-functions% i)) pointer)
                      (return i)))))
      (when index
        (let* ((entry (svref %pascal-functions% index)))
          (pfe.lisp-function entry))))))

  
(defun %delete-pascal-function (pointer)
  (with-lock-grabbed (*callback-lock*)
    (let* ((index (dotimes (i (length %pascal-functions%))
                    (when (eql (pfe.routine-descriptor (svref %pascal-functions% i)) pointer)
                      (return i)))))
      (when index
        (let* ((entry (svref %pascal-functions% index))
               (sym (pfe.sym entry)))
          (setf (svref %pascal-functions% index) nil)
          (when (and sym
                     (boundp sym)
                     (eql (symbol-value sym)
                          (pfe.routine-descriptor entry)))
            (set (symbol-value sym) nil))
          (free (pfe.routine-descriptor entry))
          t)))))


;; The kernel only really knows how to call back to one function,
;; and you're looking at it ...
(defun %pascal-functions% (index args-ptr-fixnum)
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (lisp-function without-interrupts *callback-trace-p*)
      (%lookup-pascal-function index)
    (declare (special *callback-trace-p*))
    (if without-interrupts
	(without-interrupts (funcall lisp-function args-ptr-fixnum))
      (funcall lisp-function args-ptr-fixnum))))

(defstatic *callback-alloc-lock* (make-lock))

;;; 
(defun %make-executable-page ()
  (#_mmap (%null-ptr)
          (#_getpagesize)
          (logior #$PROT_READ #$PROT_WRITE #$PROT_EXEC)
          (logior #$MAP_PRIVATE #$MAP_ANON)
          -1
          0))

(defstatic *available-bytes-for-callbacks* 0)
(defstatic *current-callback-page* nil)

(defun reset-callback-storage ()
  (setq *available-bytes-for-callbacks* (#_getpagesize)
        *current-callback-page* (%make-executable-page)))

(defun %allocate-callback-pointer (n)
  (with-lock-grabbed (*callback-alloc-lock*)
    (when (< *available-bytes-for-callbacks* n)
      (reset-callback-storage))
    (decf *available-bytes-for-callbacks* n)
    (values (%inc-ptr *current-callback-page* *available-bytes-for-callbacks*))))

