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

(defloadvar *did-show-marketing-blurb* nil)

(defparameter *marketing-blurb* "
For more information about CCL, please see https://ccl.clozure.com.

CCL is free software.
It is distributed under the terms of the Apache License, Version 2.0.
")

(defun listener-function ()
  (unless (or *inhibit-greeting* *quiet-flag*)
    (format t "~&~A ~A~%"
	    (lisp-implementation-type)
	    (lisp-implementation-version))
    (unless *did-show-marketing-blurb*
      (write-string *marketing-blurb* t)
      (setq *did-show-marketing-blurb* t)))
  (toplevel-loop))

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
