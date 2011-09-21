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


; l1-boot-lds.lisp

(in-package "CCL")





(defun command-line-arguments ()
  *command-line-argument-list*)

(defun startup-ccl (&optional init-file)
  ;; Many of the things done here could enter a break loop on error.
  ;; If that break loop is exited via :q, quietly exit to here.
  (catch :toplevel
    (with-simple-restart (abort "Abort startup.")
      (let ((init-files (if (listp init-file) init-file (list init-file))))
        (dolist (init-file init-files)
          (with-simple-restart (continue "Skip loading init file.")
            (when (load init-file :if-does-not-exist nil :verbose nil)
              (return)))))
      (flet ((eval-string (s)
               (with-simple-restart (continue "Skip evaluation of ~a" s)
                 (eval (read-from-string s))))
             (load-file (name)
               (with-simple-restart (continue "Skip loading ~s" name)
                 (load name))))
        (dolist (p *lisp-startup-parameters*)
          (let* ((param (cdr p)))
            (case (car p)
              (:gc-threshold
               (multiple-value-bind (n last) (parse-integer param :junk-allowed t)
                 (when n
                   (if (< last (length param))
                     (case (schar param last)
                       ((#\k #\K) (setq n (ash n 10)))
                       ((#\m #\M) (setq n (ash n 20)))))
                   (set-lisp-heap-gc-threshold n)
                   (use-lisp-heap-gc-threshold))))
              (:eval (eval-string param))
              (:load (load-file param)))))))))


(defun listener-function ()
  (progn
    (unless (or *inhibit-greeting* *quiet-flag*)
      (format t "~&Welcome to ~A ~A!~%"
	      (lisp-implementation-type)
	      (lisp-implementation-version)))
    (toplevel-loop)))


(defun make-mcl-listener-process (procname
                                  input-stream
                                  output-stream
                                  cleanup-function
                                  &key
                                  (initial-function #'listener-function)
                                  (close-streams t)
                                  (class 'process)
                                  (control-stack-size *default-control-stack-size*)
                                  (auto-flush t)
                                  (value-stack-size *default-value-stack-size*)
                                  (temp-stack-size *default-temp-stack-size*)
                                  (echoing t)
                                  (process)
                                  (initargs nil))
  (let ((p (if (typep process class)
             (progn
               (setf (process-thread process)
                     (new-thread procname control-stack-size value-stack-size  temp-stack-size))
               process)
             (make-process procname
                           :class class :initargs initargs
                           :stack-size control-stack-size
                           :vstack-size value-stack-size
                           :tstack-size temp-stack-size))))
    (process-preset p #'(lambda ()
                          (let ((*terminal-io*
                                 (if echoing
                                   (make-echoing-two-way-stream
                                    input-stream output-stream)
                                   (make-two-way-stream
                                    input-stream output-stream))))
			    (unwind-protect
				 (progn
                                   (when auto-flush
                                     (add-auto-flush-stream output-stream))
				   (let* ((shared-input
					   (input-stream-shared-resource
					    input-stream)))
				     (when shared-input
				       (setf (shared-resource-primary-owner
					      shared-input)
					     *current-process*)))
                                   (application-ui-operation
                                    *application*
                                    :note-current-package *package*)
				   (funcall initial-function))
                              (remove-auto-flush-stream output-stream)
			      (funcall cleanup-function)
			      (when close-streams
				(close input-stream)
				(close output-stream))))))
    (process-enable p)
    p))


; End of l1-boot-lds.lisp
