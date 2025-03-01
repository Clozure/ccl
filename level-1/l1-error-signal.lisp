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
  (if (eql err-num $XARRLIMIT)
    (%error (make-condition 'vector-size-limitation
                            :subtag (cadr errargs)
                            :element-count (car errargs))
            nil
            frame-ptr)
    (let* ((err-typ (max (ash err-num -16) 0))
           (err-num (%word-to-int err-num))
           (format-string (%rsc-string err-num)))
      (%err-disp-common err-num err-typ format-string errargs frame-ptr))))

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
                                             :format-control format-string
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
                 (division-by-zero (make-condition condition-name
                                                   :operation '/
                                                   :operands (if errargs
                                                               (list (car errargs)
                                                                     0)
                                                               (list 0))))
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
    (#_exit #-windows-target #-android-target #$EX_SOFTWARE #+android-target 70 #+windows-target #$EXIT_FAILURE))
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
