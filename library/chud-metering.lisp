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
  (:export "METER" "PREPARE-METERING" "*SPATCH-DIRECTORY-PATH*"
           "LAUNCH-SHARK" "CLEANUP-SPATCH-FILES" "RESET-METERING"))
  
(in-package "CHUD")


(defparameter *CHUD-library-path*
  "/System/Library/PrivateFrameworks/CHUD.Framework/CHUD"
  "This seems to move around with every release.")

(defparameter *shark-app-path* "/Developer/Applications/Performance\ Tools/Shark.app")

(defparameter *spatch-directory-path* nil
  "If non-NIL, should be a pathname whose directory component matches the
\"Patch FIles\" search path in Shark's Preferences.  When this variable
is NIL, USER-HOMEDIR-PATHNAME is used instead.")

(eval-when (:load-toplevel :execute)
  (open-shared-library (namestring *CHUD-library-path*)))

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :chud))

;;; CHUD apparently has this notion of global, persistent
;;; "status" (the result returned by the last operation.)
;;; I have not idea whether or not that's thread-specific;
;;; there doesn't seem to be any other way of getting a
;;; string that describes an error code.
(defun chud-get-status-string ()
  (with-macptrs ((s (#_chudGetStatusStr)))
    (if (%null-ptr-p s)
      ""
      (%get-cstring s))))

(defun chud-check-error (result context)
  (or (eql result #$chudSuccess)
      (error "CHUD error ~d (~a) while ~a. " result (chud-get-status-string) context)))
  
(defun chud-is-initialized ()
  (not (eql (#_chudIsInitialized) 0)))

(defparameter *chud-supported-major-version* 4)
(defparameter *chud-supported-minor-version* 1)

;; Don't know if it makes sense to worry about max supported versions
;; as well.

(defun check-chud-version ()
  (let* ((version (#_chudFrameworkVersion))
         (major (ldb (byte 8 24) version))
         (minor (ldb (byte 8 12) version)))
    (or (and (>= major *chud-supported-major-version*)
             (when (= major *chud-supported-major-version*)
               (>= minor *chud-supported-minor-version*)))
        (warn "The installed CHUD framework is version ~d.~d.  ~
The minimum version supported by this interface is ~d.~d."
              major minor *chud-supported-major-version*
              *chud-supported-minor-version*))))
    

(defun initialize-chud ()
  (or (chud-is-initialized)
      (and (check-chud-version)
           (chud-check-error (#_chudInitialize) "initializing CHUD"))))

(defun acquired-remote-access ()
  (eql #$true (#_chudIsRemoteAccessAcquired)))
  
;;; If we've already successfully called (#_chudAcquireRemoteAccess),
;;; we can call it again without error (e.g., it's a no-op in that
;;; case.)  However, we can successfully release it at most once.

(defun acquire-remote-access ()
  (or (acquired-remote-access)
      (chud-check-error (#_chudAcquireRemoteAccess) "acquiring remote access")))

(defun release-remote-access ()
  (chud-check-error (#_chudReleaseRemoteAccess) "releasing remote access"))

(defun start-remote-perf-monitor (label)
  (with-cstrs ((clabel (format nil "~a" label)))
    (chud-check-error (#_chudStartRemotePerfMonitor clabel)
                      "starting performance monitor")))

(defun stop-remote-perf-monitor ()
  (chud-check-error (#_chudStopRemotePerfMonitor)
                    "stopping performance monitor"))

(defun setup-timer (duration frequency)
  (#_chudSetupTimer frequency
                    #$chudMicroSeconds
                    0
                    #$chudMicroSeconds
                    duration))

(defun get-readonly-area-bounds ()
  (ccl::do-gc-areas (a)
    (when (eql(ccl::%fixnum-ref a target::area.code)
              #+ppc-target ccl::area-readonly
              #+x8664-target ccl::area-managed-static)
      (return
        (values (ash (ccl::%fixnum-ref a target::area.low) target::fixnumshift)
                (ash (ccl::%fixnum-ref a target::area.active) target::fixnumshift))))))

(defun safe-shark-function-name (function)
  (let* ((name (format nil "~s" function)))
    (subseq (nsubstitute #\0 #\# (nsubstitute #\. #\Space name)) 1)))

(defun print-shark-spatch-record (fn &optional (stream t))
  (let* ((code-vector (uvref fn 0))
         (startaddr (+ (ccl::%address-of code-vector)
                       target::misc-data-offset))
         (endaddr (+ startaddr (* target::node-size (uvsize code-vector)))))
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
  (ccl::purify)
  (multiple-value-bind (pure-low pure-high)
      (get-readonly-area-bounds)
    (let* ((functions (identify-functions-with-pure-code pure-low pure-high)))
      (with-open-file (f (make-pathname
                          :host nil
                          :directory (pathname-directory
                                      (or *spatch-directory-path*
                                          (user-homedir-pathname)))
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

(defun cleanup-spatch-files ()
  (dolist (f (directory
              (make-pathname
               :host nil
               :directory
               (pathname-directory
                (or *spatch-directory-path*
                    (user-homedir-pathname)))
               :name :wild
               :type "spatch")))
    (delete-file f)))


(defun launch-shark ()
  (run-program "/usr/bin/open" (list *shark-app-path*)))

  
(defun reset-metering ()
  (when (acquired-remote-access)
    (release-remote-access)
    (format t "~&Note: it may be desirable to quit and restart Shark.")
    t))
    
(defun prepare-metering ()
  (launch-shark)
  (generate-shark-spatch-file)
  (initialize-chud)
  (loop
    (when (ignore-errors (acquire-remote-access))
      (return))
    ;; Yes, this is lame.
    (loop (when (y-or-n-p "Is Shark in Remote mode yet?")
            (return)))))

(defmacro meter (form &key (duration 0) (frequency 1))
  (let* ((started (gensym)))
    `(let* ((,started nil))
      (unless (and (chud-is-initialized)
                   (acquired-remote-access))
        (prepare-metering))
      (setup-timer ,duration ,frequency)
      (unwind-protect
         (progn
           (setq ,started (start-remote-perf-monitor ',form))
           ,form)
        (when ,started (stop-remote-perf-monitor))))))

(defun chud-cleanup ()
  (when (chud-is-initialized)
    (when (acquired-remote-access)
      (ignore-errors (release-remote-access)))
    (#_chudCleanup))
  (cleanup-spatch-files))
  
(pushnew 'chud-cleanup *lisp-cleanup-functions*)
