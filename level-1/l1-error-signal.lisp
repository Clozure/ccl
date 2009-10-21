;;;-*-Mode: LISP; Package: CCL -*-
;;;
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

(defun %kernel-restart (error-type &rest args)
  (%kernel-restart-internal error-type args (%get-frame-ptr)))

(defun %kernel-restart-internal (error-type args frame-ptr)
  ;(declare (dynamic-extent args))
  (dolist (f *kernel-restarts* (%err-disp-internal error-type args frame-ptr))
    (when (eq (car f) error-type)
      (return (apply (cdr f) frame-ptr args)))))

;;; this is the def of %err-disp.
;;; Yup.  That was my first guess.
(defun %err-disp (err-num &rest errargs)
  (%err-disp-internal err-num errargs (%get-frame-ptr)))

(defun %errno-disp (errno &rest errargs)
  (%errno-disp-internal errno errargs (%get-frame-ptr)))

#+windows-target
(defun %windows-error-disp (errno &rest errargs)
  (%err-disp-common errno 0 (%windows-error-string errno) errargs (%get-frame-ptr)))
  
(defun %errno-disp-internal (errno errargs frame-ptr)
  (declare (fixnum errno))
  (let* ((err-type (max (ash errno -16) 0))
	 (errno (%word-to-int errno))
	 (error-string (%strerror errno))
	 (format-string (if errargs
			  (format nil "~a : ~a" error-string "~s")
			  error-string)))
    (%err-disp-common nil err-type  format-string errargs frame-ptr)))


(defun %err-disp-internal (err-num errargs frame-ptr)
  (declare (fixnum err-num))
  ;;; The compiler (finally !) won't tail-apply error.  But we kind of
  ;;; expect it to ...
  (let* ((err-typ (max (ash err-num -16) 0))
         (err-num (%word-to-int err-num))
         (format-string (%rsc-string err-num)))
    (%err-disp-common err-num err-typ format-string errargs frame-ptr)))

(defparameter *foreign-error-condition-recognizers* ())


(defun %err-disp-common (err-num err-typ format-string errargs frame-ptr)
  (let* ((condition-name (or (uvref *simple-error-types* err-typ)
                             (%cdr (assq err-num *kernel-simple-error-classes*)))))
    ;;(dbg format-string)
    (if condition-name      
      (funcall '%error
               (case condition-name
                 (type-error
                  (if (cdr errargs)
                    (make-condition condition-name
                                             :format-control format-string
                                             :datum (car errargs)
                                             :expected-type (%type-error-type (cadr errargs)))
                    (make-condition condition-name
                                             :format-control format-string
                                             :datum (car errargs))))
		 (improper-list (make-condition condition-name
						:datum (car errargs)))
                 (simple-file-error (make-condition condition-name
                                             :pathname (car errargs)
                                             :error-type format-string
                                             :format-arguments (cdr errargs)))
                 (undefined-function (make-condition condition-name
                                                     :name (car errargs)))
                 (call-special-operator-or-macro
                  (make-condition condition-name
                                  :name (car errargs)
                                  :function-arguments (cadr errargs)))
                 (sequence-index-type-error
                  (make-sequence-index-type-error (car errargs) (cadr errargs)))
		 (cant-construct-arglist
		  (make-condition condition-name
				  :datum (car errargs)
				  :format-control format-string))
                 (array-element-type-error
                  (let* ((array (cadr errargs)))
                    (make-condition condition-name
                                    :format-control format-string
                                    :datum (car errargs)
                                    :expected-type (array-element-type array)
                                    :array array)))
                                  
                 (t (make-condition condition-name 
                                    :format-control format-string
                                    :format-arguments errargs)))
               nil
               frame-ptr)
      (let* ((cond nil))
        (if (and (eql err-num $XFOREIGNEXCEPTION)
                 (dolist (recog *foreign-error-condition-recognizers*)
                   (let* ((c (funcall recog (car errargs))))
                     (when c (return (setq cond c))))))
          (funcall '%error cond nil frame-ptr)
          (funcall '%error format-string errargs frame-ptr))))))

(defun error (condition &rest args)
  "Invoke the signal facility on a condition formed from DATUM and ARGUMENTS.
  If the condition is not handled, the debugger is invoked."
  #|
  #+ppc-target
  (with-pstrs ((pstr (if (stringp condition) condition "Error")))
    (#_DebugStr pstr))
  |#
  (%error condition args (%get-frame-ptr)))

(defun cerror (cont-string condition &rest args)
  (let* ((fp (%get-frame-ptr)))
    (restart-case (%error condition (if (condition-p condition) nil args) fp)
      (continue ()
                :report (lambda (stream) 
                            (apply #'format stream cont-string args))
                nil))))

(defun %error (condition args error-pointer)
  (setq *error-reentry-count* 0)
  (setq condition (condition-arg condition args 'simple-error))
  (signal condition)
  (unless *interactive-streams-initialized*
    (bug (format nil "Error during early application initialization:~%
~a" condition))
    (#_exit #-windows-target #$EX_SOFTWARE #+windows-target #$EXIT_FAILURE))
  (application-error *application* condition error-pointer)
  (application-error
   *application*
   (condition-arg "~s returned. It shouldn't.~%If it returns again, I'll throw to toplevel."
                  '(application-error) 'simple-error)
   error-pointer)
  (toplevel))

(defun make-sequence-index-type-error (idx sequence)
  (let* ((upper (length sequence)))
    (make-condition 'sequence-index-type-error
                    :datum idx
                    :sequence sequence
                    :expected-type `(integer 0 (,upper)))))
