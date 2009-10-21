;;;-*-Mode: LISP; Package: (CHUD (:USE CL CCL)) -*-
;;;
;;;   Copyright (C) 2005,2008 Clozure Associates and contributors
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

;;; Some of this is based on work done by Dan Knapp and Hamilton Link
;;; (and possibly others.)

;;; CHUD 4.4.3-5 claims to offer 64-bit support; however, the library
;;; which provides the API to control CHUD metering functions still
;;; seems to be 32-bit only.  Conditionalization for x86-64 and
;;; for 64-bit targets is (so far) just an exercise.

(defpackage "CHUD"
  (:use "CL" "CCL")
  (:export "METER" "*SHARK-CONFIG-FILE*"))
  
(in-package "CHUD")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    #-darwin-target
    (error "This code is Darwin/MacOSX-specific.")))


(defparameter *shark-session-path* nil)

(defloadvar *written-spatch-file* nil)

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
	(setenv "SHARK_SEARCH_PATH_PATCH_FILES" native-name)
	(setq *shark-session-native-namestring*
	      native-name
	      *shark-session-path* dir))))
  *shark-session-path*)


  

(defloadvar *shark-process* nil)
(defloadvar *sampling* nil)

(defvar *debug-shark-process-output* nil)


(defun safe-shark-function-name (function)
  (let* ((name (format nil "~s" function)))
    (subseq (nsubstitute #\0 #\# (nsubstitute #\. #\Space name)) 1)))

(defun print-shark-spatch-record (fn &optional (stream t))
  (let* ((code-vector #+ppc-target (uvref fn 0) #-ppc-target fn)
         (startaddr (+ (ccl::%address-of code-vector)
                       #+x8664-target 0
                       #+ppc32-target target::misc-data-offset
		       #-ppc32-target 0))
         (endaddr (+ startaddr
                     #+x8664-target
                     (1+ (ash (1- (ccl::%function-code-words fn)
                                  ) target::word-shift))
                     #+ppc-target
                     (* 4 (- (uvsize code-vector)
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

#+x8664-target
(ccl::defx86lapfunction dynamic-dnode ((x arg_z))
  (movq (% x) (% imm0))
  (ref-global x86::heap-start arg_y)
  (subq (% arg_y) (% imm0))
  (shrq ($ x8664::dnode-shift) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

#+x8632-target
(ccl::defx8632lapfunction dynamic-dnode ((x arg_z))
  (movl (% x) (% imm0))
  (ref-global x86::heap-start arg_y)
  (subl (% arg_y) (% imm0))
  (shrl ($ x8632::dnode-shift) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

#+x8664-target
(defun identify-functions-with-pure-code ()
  (ccl::freeze)
  (ccl::collect ((functions))
    (block walk
      (let* ((frozen-dnodes (ccl::frozen-space-dnodes)))
        (ccl::%map-areas (lambda (o)
                           (when (>= (dynamic-dnode o) frozen-dnodes)
                             (return-from walk nil))
                           (when (typep o 'ccl::function-vector)
                             (functions (ccl::function-vector-to-function o))))
                         ccl::area-dynamic
                         ccl::area-dynamic
                         )))
    (functions)))

#+x8632-target
(defun identify-functions-with-pure-code ()
  (ccl::freeze)
  (ccl::collect ((functions))
    (block walk
      (let* ((frozen-dnodes (ccl::frozen-space-dnodes)))
        (ccl::%map-areas (lambda (o)
                           (when (>= (dynamic-dnode o) frozen-dnodes)
                             (return-from walk nil))
                           (when (typep o 'function)
                             (functions o)))
                         ccl::area-dynamic
                         ccl::area-dynamic
                         )))
    (functions)))

#+ppc-target
(defun identify-functions-with-pure-code ()
  (ccl::purify)
  (multiple-value-bind (pure-low pure-high)
                                 
      (ccl::do-gc-areas (a)
        (when (eql(ccl::%fixnum-ref a target::area.code)
                  ccl::area-readonly)
          (return
            (values (ash (ccl::%fixnum-ref a target::area.low) target::fixnumshift)
                    (ash (ccl::%fixnum-ref a target::area.active) target::fixnumshift)))))
    (let* ((hash (make-hash-table :test #'eq)))
      (ccl::%map-lfuns #'(lambda (f)
                           (let* ((code-vector  (ccl:uvref f 0))
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
        (let* ((functions ()))
          (maphash #'(lambda (k v)
                       (declare (ignore k))
                       (when (null (cdr v))
                         (push (car v) functions)))
                   hash)
          (sort functions
                #'(lambda (x y)
                    (< (ccl::%address-of (uvref x 0) )
                       (ccl::%address-of  (uvref y 0))))))))))
        
                           


(defun generate-shark-spatch-file ()
  (let* ((functions (identify-functions-with-pure-code)))
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
      (dolist (fun functions)
        (print-shark-spatch-record fun f))
      (format f "!SHARK_SPATCH_END~%"))))

(defun terminate-shark-process ()
  (when *shark-process*
    (signal-external-process *shark-process* #$SIGUSR2))
  (setq *shark-process* nil
	*sampling* nil))

(defun toggle-sampling ()
  (if *shark-process*
    (progn
      (signal-external-process *shark-process* #$SIGUSR1)
      (setq *sampling* (not *sampling*)))
    (warn "No active shark procsss")))

(defun enable-sampling ()
  (unless *sampling* (toggle-sampling)))

(defun disable-sampling ()
  (when *sampling* (toggle-sampling)))

(defun ensure-shark-process (reset hook)
  (when (or (null *shark-process*) reset)
    (terminate-shark-process)
    (when (or reset (not *written-spatch-file*))
      (generate-shark-spatch-file))
    (let* ((args (list "-b" "-1" "-a" (format nil "~d" (ccl::getpid))
			     "-d" *shark-session-native-namestring*)))
      (when *shark-config-file*
	(push (ccl::native-untranslated-namestring *shark-config-file*)
	      args)
	(push "-m" args))
      (setq *shark-process*
	    (run-program "/usr/bin/shark"
			 args
			 :output :stream
			 :status-hook hook
			 :wait nil))
      (let* ((output (external-process-output-stream *shark-process*)))
	(do* ((line (read-line output nil nil) (read-line output nil nil)))
	     ((null line))
	  (when *debug-shark-process-output*
	    (format t "~&~a" line))
	  (when (search "ready." line :key #'char-downcase)
            (sleep 1)
	    (return)))))))

(defun display-shark-session-file (line)
  (let* ((last-quote (position #\' line :from-end t))
	 (first-quote (and last-quote (position #\' line :end (1- last-quote) :from-end t)))
	 (path (and first-quote  (subseq line (1+ first-quote) last-quote))))
    (when path (finder-open-file path))))
    
(defun scan-shark-process-output (p)
  (with-interrupts-enabled 
      (let* ((out (ccl::external-process-output p)))
	(do* ((line (read-line out nil nil) (read-line out nil nil)))
	     ((null line))
	  (when *debug-shark-process-output*
	    (format t "~&~a" line))
	  (when (search "Created session file:" line)
	    (display-shark-session-file line)
	    (return))))))



(defmacro meter (form &key reset debug-output)
  (let* ((hook (gensym))
	 (block (gensym))
	 (process (gensym)))
    `(block ,block
      (flet ((,hook (p)
	       (when (or (eq (external-process-status p) :exited)
			 (eq (external-process-status p) :signaled))
		 (setq *shark-process* nil
		       *sampling* nil))))
	(let* ((*debug-shark-process-output* ,debug-output))
	  (ensure-shark-process ,reset #',hook)
	  (unwind-protect
	       (progn
		 (enable-sampling)
		 ,form)
	    (disable-sampling)
	    (let* ((,process *shark-process*))
	      (when ,process
		(scan-shark-process-output ,process)))))))))

;;; Try to clean up after ourselves when the lisp quits.
(pushnew 'terminate-shark-process ccl::*save-exit-functions*)
