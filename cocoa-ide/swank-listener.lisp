;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: cl-user -*-
;;;
;;;   Copyright (C) 2011 Clozure Associates
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

;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          swank.lisp
;;;; Project:       CCL IDE
;;;; Purpose:       CCL's swank loader
;;;;
;;;; ***********************************************************************

;;; ABOUT
;;; ------------------------------------------------------------------------
;;; implements tools used to locate and load a swank server at app startup.

(in-package :GUI)

(defparameter *active-gui-swank-listener-port* nil)
(defparameter *default-swank-listener-port* 4884)

;;; preference-swank-listener-port
;;; returns the current value of the "Swank Port" user preference
(defun preference-swank-listener-port ()
  (with-autorelease-pool
    (let* ((defaults (handler-case (#/values (#/sharedUserDefaultsController ns:ns-user-defaults-controller))
                       (error (c)
                         (progn (log-debug "~%ERROR: Unable to get preferences from the Shared User Defaults Controller: ~A"
                                           c)
                                nil))))
           (swank-port-pref (and defaults (#/valueForKey: defaults #@"swankListenerPort"))))
      (cond
        ;; the user default is not initialized
        ((or (null swank-port-pref)
             (%null-ptr-p swank-port-pref)) nil)
        ;; examine the user default
        ((or (typep swank-port-pref 'ns:ns-number)
             (typep swank-port-pref 'ns:ns-string)) 
         (handler-case (let* ((port (#/intValue swank-port-pref)))
                         (or port *default-swank-listener-port*))
           ;; parsing the port number failed
           (error (c)
             (declare (ignore c))
             (#_NSLog #@"\nError starting swank listener; the user preference is not a valid port number: %@\n"
                    :id swank-port-pref)
             nil)))
        ;; the user default value is incomprehensible
        (t (progn
             (#_NSLog #@"\nERROR: Unrecognized value type in user preference 'swankListenerPort': %@"
                    :id swank-port-pref)
             nil))))))

;;; preference-start-swank-listener?  
;;; returns the current value of the "Start swank listener?" user
;;; preference
(defun preference-start-swank-listener? ()
  (with-autorelease-pool
   (let* ((defaults (handler-case (#/values (#/sharedUserDefaultsController ns:ns-user-defaults-controller))
                     (error (c) 
                       (progn (log-debug "~%ERROR: Unable to get preferences from the Shared User Defaults Controller: ~a" c)
                              nil))))
         (start-swank-pref (if (and defaults (not (%null-ptr-p defaults))) 
                               (#/valueForKey: defaults #@"startSwankListener")         
                               nil)))
    (cond
      ;; the user default is not initialized
      ((or (null start-swank-pref)
           (%null-ptr-p start-swank-pref)) nil)
      ;; examine the user default
      ;; intValue works on NSNumber or NSString
      ;; BUG? if a string value is not a valid representation of an integer,
      ;;      intValue returns 0, which means any non-numeric string will have the
      ;;      same effect as "0"
      ((or (typep start-swank-pref 'ns:ns-number)
           (typep start-swank-pref 'ns:ns-string))
       (case (#/intValue start-swank-pref)
         ;; don't start swank listener
         (0 nil)
         ;; start swank listener
         (1 t)
         ;; the user default value is incomprehensible
         (otherwise (progn
                      (log-debug "~%ERROR: Unrecognized value in user preference 'startSwankServer': ~S"
                                 start-swank-pref)
                      nil))))
      ;; the user default value is incomprehensible
      (t (progn
           (log-debug "~%ERROR: Unrecognized value type in user preference 'startSwankServer': ~S"
                      start-swank-pref)
           nil))))))

;;; maybe-start-swank-listener
;;; -----------------------------------------------------------------
;;; checks whether to start the ccl swank listener, and starts it if
;;; warranted.
(defun maybe-start-swank-listener (&key (override-user-preference nil))
    ;; try to determine the user preferences concerning the
    ;; swank-listener port number and whether the swank listener
    ;; should be started. If the user says start it, and we can
    ;; determine a valid port for it, start it up
    (let* ((start-swank-listener? (or (preference-start-swank-listener?) override-user-preference))
           (swank-listener-port (or (preference-swank-listener-port) *default-swank-listener-port*)))
      (unless (and start-swank-listener?
		   (eql swank-listener-port *active-gui-swank-listener-port*)
                   ccl::*swank-loader-process*
                   (not (process-exhausted-p ccl::*swank-loader-process*)))
	(ccl::stop-swank-loader)
        (setf *active-gui-swank-listener-port* nil)
        (when (and start-swank-listener?
                   swank-listener-port
                   (ccl::start-swank-loader swank-listener-port))
          (setf *active-gui-swank-listener-port* swank-listener-port)))))

(provide :swank-listener)
