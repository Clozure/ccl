;;;-*-Mode: LISP; Package: (CHUD (:USE CL CCL)) -*-
;;;
;;;   Copyright (C) 2005 Clozure Associates and contributors
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

;;; Some of this is based on work done by Dan Knapp and Hamilton Link
;;; (and possibly others.)

;;; CHUD 4.4.3-5 claims to offer 64-bit support; however, the library
;;; which provides the API to control CHUD metering functions still
;;; seems to be 32-bit only.  Conditionalization for x86-64 and
;;; for 64-bit targets is (so far) just an exercise.

(defpackage "CHUD"
  (:use "CL" "CCL")
  (:export "METER" "PREPARE-METERING" "SHARK-SESSION-PATH"
           "LAUNCH-SHARK" "CLEANUP-SPATCH-FILES" "RESET-METERING"))
  
(in-package "CHUD")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    #-darwin-target
    (error "This code is Darwin/MacOSX-specific.")))


(defparameter *shark-session-path* nil)

(defparameter *shark-session-native-namestring* nil)

(defparameter *shark-config-file* nil "Full pathname of .cfg file to use for profiling, or NIL.")

(defun finder-open-file (namestring)
  "Open the file named by NAMESTRING, as if it was double-clicked on
in the finder"
  (run-program "/usr/bin/open" (list namestring) :output nil))

(defun ensure-shark-session-path ()
  (unless *shark-session-path*
    (multiple-value-bind (second minute hour date month year)
	(decode-universal-time (get-universal-time))
      (let* ((subdir (format nil "profiling-session-~A-~d_~d-~d-~d_~d.~d.~d"
			     (pathname-name
			      (car
			       ccl::*command-line-argument-list*))
			     (ccl::getpid)
			     month
			     date
			     year
			     hour
			     minute
			     second))
	     (dir (make-pathname :directory (append (pathname-directory (user-homedir-pathname)) (list subdir)) :defaults nil))
	     (native-name (ccl::native-untranslated-namestring dir)))
	(ensure-directories-exist dir)
	(finder-open-file native-name)
	(setenv "SHARK_SEARCH_PATH_PATCH_FILES" native-name)
	(setq *shark-session-native-namestring*
	      native-name
	      *shark-session-path* dir))))
  *shark-session-path*)

;;; This is cheesy: it should watch for directory changes (or something)
;;; rather than guessing how long it'll take for an mshark file to appear
;;; in the session directory.
(defun wait-and-open-mshark-file (path delay)
  (process-run-function "mshark file watch"
			(lambda ()
			  (sleep delay)
			  (let* ((path (make-pathname
					:host nil
					:directory
					(pathname-directory path)
					:name "*"
					:type "mshark"
					:defaults nil))
				 (mshark
				  (ignore-errors (car (last (directory path))))))
			    (when mshark
			      (finder-open-file
			       (ccl::native-untranslated-namestring mshark)))))))


  

(defvar *shark-process* nil)
(defvar *sampling* nil)

#+ppc-target
(defun get-static-function-area-bounds ()
  (ccl::do-gc-areas (a)
    (when (eql(ccl::%fixnum-ref a target::area.code)
               ccl::area-readonly)
      (return
        (values (ash (ccl::%fixnum-ref a target::area.low) target::fixnumshift)
                (ash (ccl::%fixnum-ref a target::area.active) target::fixnumshift))))))

(defun safe-shark-function-name (function)
  (let* ((name (format nil "~s" function)))
    (subseq (nsubstitute #\0 #\# (nsubstitute #\. #\Space name)) 1)))

(defun print-shark-spatch-record (fn &optional (stream t))
  (let* ((code-vector #+ppc-target (uvref fn 0) #-ppc-target fn)
         (startaddr (+ (ccl::%address-of code-vector)
                       #+ppc32-target target::misc-data-offset
		       #-ppc32-target 0))
         (endaddr (+ startaddr (* 4 (- (uvsize code-vector)
				       #+ppc64-target 2
				       #-ppc64-target 1)))))
    ;; i hope all lisp sym characters are allowed... we'll see
    (format stream "{~%~@
                        ~a~@
                        ~@?~@
                        ~@?~@
                        }~%"
            (safe-shark-function-name fn)
            #+32-bit-target "0x~8,'0x" #+64-bit-target "0x~16,'0x"
            startaddr
            #+32-bit-target "0x~8,'0x" #+64-bit-target "0x~16,'0x"
            endaddr)))

(defun identify-functions-with-pure-code (pure-low pure-high)
  (let* ((hash (make-hash-table :test #'eq)))
    (ccl::%map-lfuns #'(lambda (f)
                         (let* ((code-vector #+ppc-target (ccl:uvref f 0)
                                             #+x8664-target (ccl::function-to-function-vector f))
                                (startaddr (+ (ccl::%address-of code-vector)
                                              target::misc-data-offset)))
                           (when (and (>= startaddr pure-low)
                                      (< startaddr pure-high))
                             (push f (gethash code-vector hash))))))
    (let* ((n 0))
      (declare (fixnum n))
      (maphash #'(lambda (k v)
                   (declare (ignore k))
                   (if (null (cdr v))
                     (incf n)))
               hash)
      (let* ((functions (make-array n))
             (i 0))
        (maphash #'(lambda (k v)
                     (declare (ignore k))
                     (when (null (cdr v))
                       (setf (svref functions i) (car v)
                             i (1+ i))))
                 hash)
        (sort functions
              #'(lambda (x y)
                  (< (ccl::%address-of #+ppc-target (uvref x 0)
                                       #+x8664-target x)
                     (ccl::%address-of #+ppc-target (uvref y 0)
                                       #+x8664-target y))))))))
        
                           
(defun generate-shark-spatch-file ()
  #+ppc-target (ccl::purify)
  #+x86-target (ccl::freeze)
  (multiple-value-bind (pure-low pure-high)
      (get-static-function-area-bounds)
    (let* ((functions (identify-functions-with-pure-code pure-low pure-high)))
      (with-open-file (f (make-pathname
                          :host nil
                          :directory (pathname-directory
                                      (ensure-shark-session-path))
                          :name (format nil "~a_~D"
                                        (pathname-name
                                         (car
                                          ccl::*command-line-argument-list*))
                                        (ccl::getpid))
                          :type "spatch")
                         :direction :output
                         :if-exists :supersede)
        (format f "!SHARK_SPATCH_BEGIN~%")
        (dotimes (i (length functions))
          (print-shark-spatch-record (svref functions i) f))
        (format f "!SHARK_SPATCH_END~%"))) t))

(defun terminate-shark-process ()
  (when *shark-process*
    (signal-external-process *shark-process* #$SIGUSR2))
  (setq *shark-process* nil
	*sampling* nil))

(defun toggle-sampling ()
  (if *shark-process*
    (progn
      (signal-external-process *shark-process* (if *sampling* #$SIGUSR2 #$SIGUSR1))
      (setq *sampling* (not *sampling*)))
    (warn "No active shark procsss")))

(defun enable-sampling ()
  (unless *sampling* (toggle-sampling)))

(defun disable-sampling ()
  (when *sampling* (toggle-sampling)))

(defun ensure-shark-process (reset)
  (when (or (null *shark-process*) reset)
    (terminate-shark-process)
    (generate-shark-spatch-file)
    (let* ((args (list "-b" "-r" "-a" (format nil "~d" (ccl::getpid))
			     "-d" *shark-session-native-namestring*)))
      (when *shark-config-file*
	(push (ccl::native-untranslated-namestring *shark-config-file*)
	      args)
	(push "-m" args))
      (setq *shark-process*
	    (run-program "/usr/bin/shark"
			 args
			 :output t
			 :wait nil))
      (sleep 5))))


(defmacro meter (form &key reset)
    `(progn
      (ensure-shark-process ,reset)
      (unwind-protect
         (progn
           (enable-sampling)
           ,form)
        (disable-sampling)
	(wait-and-open-mshark-file *shark-session-path* 5))))


