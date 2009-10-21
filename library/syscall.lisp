;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2001-2009 Clozure Associates
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

;;; "Generic" syscall sypport.

(in-package "CCL")

(defpackage "SYSCALLS" (:use))

(defstruct syscall
  (idx 0 :type fixnum)
  (arg-specs () :type list)
  (result-spec nil :type symbol)
  (min-args 0 :type fixnum))

(defvar *os-syscall-definitions* ())

(defun platform-syscall-definitions (platform-os)
  (or (getf *os-syscall-definitions* platform-os)
      (setf (getf *os-syscall-definitions* platform-os)
            (make-hash-table :test 'eq))))

(defun backend-syscall-definitions (backend)
  (platform-syscall-definitions (backend-platform-syscall-mask backend)))



(defmacro define-syscall (platform name idx (&rest arg-specs) result-spec
			       &key (min-args (length arg-specs)))
  `(progn
    (setf (gethash ',name (platform-syscall-definitions ,platform))
     (make-syscall :idx ,idx
      :arg-specs ',arg-specs
      :result-spec ',result-spec
      :min-args ,min-args))
    ',name))

(defmacro syscall (name &rest args)
  (let* ((info (or (gethash name (backend-syscall-definitions *target-backend*))
		   (error "Unknown system call: ~s" name)))
	 (idx (syscall-idx info))
	 (arg-specs (syscall-arg-specs info))
	 (n-argspecs (length arg-specs))
	 (n-args (length args))
	 (min-args (syscall-min-args info))
	 (result (syscall-result-spec info)))
    (unless (and (>= n-args min-args) (<= n-args n-argspecs))
      (error "wrong number of args in ~s" args))
    (do* ((call ())
	  (specs arg-specs (cdr specs))
	  (args args (cdr args)))
	 ((null args)
	  `(%syscall ,idx ,@(nreverse (cons result call))))
      (push (car specs) call)
      (push (car args) call))))
