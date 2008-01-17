;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: HEMLOCK-EXT; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Lisp Implementation Dependent Stuff for Hemlock
;;;   Created: 2002-11-07
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

(in-package :hemlock-ext)

#+CLISP
(progn
  (setf custom:*FLOATING-POINT-CONTAGION-ANSI* t)
  (setf custom:*WARN-ON-FLOATING-POINT-CONTAGION* nil))

(defun getenv (name) 
  #.(or
     #+EXCL  '(sys:getenv name)
     #+CLISP '(ext:getenv name)
     #+CMU   '(cdr (assoc name ext:*environment-list* :test #'string=))
     #+scl   '(cdr (assoc name ext:*environment-list* :test #'string-equal))
     #+sbcl  '(sb-ext:posix-getenv name)
     #+openmcl '(ccl::getenv name)
     (error "Find an implementation of getenv for your Lisp.")))

(defmacro without-interrupts (&body body)
  `(#+EXCL   excl:without-interrupts
    #+CMU    sys:without-interrupts
    #+sbcl   sb-sys:without-interrupts
    #+openmcl ccl:without-interrupts
    #-(or EXCL CMU sbcl openmcl) progn
    ,@body))

(defmacro fixnump (object)
  #+CMU   `(ext:fixnump ,object)
  #+scl   `(ext:fixnump ,object)
  #+EXCL  `(excl:fixnump ,object)
  #+CLISP `(sys::fixnump ,object)
  #-(or CMU EXCL CLISP scl) `(typep ,object 'fixnum))

(defun file-writable (pathname)
  "File-writable accepts a pathname and returns T if the current
  process can write it, and NIL otherwise. Also if the file does
  not exist return T."
  #+(or CMU scl)
  (ext:file-writable pathname)
  #-(or cmu scl)
  (handler-case (let ((io (open pathname
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist nil)))
                  (if io
                      (close io :abort t)
                      ;; more complicate situation:
                      ;; we want test if we can create the file.
                      (let ((io (open pathname
                                      :direction :output
                                      :if-exists nil
                                      :if-does-not-exist :create)))
                        (if io
                            (progn
                              (close io)
                              (delete-file io))
                            t))))
    (file-error (err)
                (declare (ignore err))
                nil)) )
  

(defmacro without-gcing (&body body)
  `(progn ,@body))
