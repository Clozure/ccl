;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: cl-user -*-
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
;;; implements tools used to loacte and load a swank server at app startup.
;;; provides an interface through which a client program can request
;;; loading of a specific copy of swank for use with SLIME
;;;
;;; ccl == the command-line lisp executable
;;; CCL == the Cocoa lisp application
;;;
;;; CCL/ccl starts a swank server in one of the following ways:
;;;
;;; 1. Emacs starts ccl as an inferior SLIME process
;;;    In this case, emacs tells ccl at startup where to get the swank
;;;    loader. ccl loads the swank indicated by the input from emacs
;;;    and starts it up
;;;
;;; 2. Emacs connects to an already-running CCL

;;;    If CCL starts up from the Finder, not under the control of an
;;;    emacs process, it starts a swank listener. The swank listener
;;;    listens on a port for connections using the swank protocol.


(in-package :GUI)

(defparameter *default-gui-swank-port* 4564)
(defparameter *active-gui-swank-port* nil)
(defparameter *ccl-swank-active-p* nil)

(defparameter *default-swank-listener-port* 4884)
(defparameter *active-gui-swank-listener-port* nil)
(defparameter *ccl-swank-listener-active-p* nil)

(load #P"ccl:cocoa-ide;slime;swank-loader.lisp")
(swank-loader::load-swank)

;;; preference-start-swank?  
;;; returns the current value of the "Start swank server?" user
;;; preference
(defun preference-start-swank? ()
  (let* ((defaults (handler-case (#/values (#/sharedUserDefaultsController ns:ns-user-defaults-controller))
                     (serious-condition (c) 
                       (progn (format t "~%ERROR: Unable to get preferences from the Shared User Defaults Controller")
                              (force-output)
                              nil))))
         (start-swank-pref (and defaults (#/valueForKey: defaults #@"startSwankServer"))))
    (cond
      ;; the user default is not initialized
      ((or (null start-swank-pref)
           (%null-ptr-p start-swank-pref)) nil)
      ;; examine the user default
      ((typep start-swank-pref 'ns:ns-number) 
       (case (#/intValue start-swank-pref)
         ;; don't start swank
         (0 nil)
         ;; start swank
         (1 t)
         ;; the user default value is incomprehensible
         (otherwise (progn
                      (format t "~%ERROR: Unrecognized value in user preference 'startSwankServer': ~S"
                              start-swank-pref)
                      (force-output)
                      nil))))
      ;; the user default value is incomprehensible
      (t (progn
           (format t "~%ERROR: Unrecognized value type in user preference 'startSwankServer': ~S"
                   start-swank-pref)
           (force-output)
           nil)))))

;;; preference-swank-port
;;; returns the current value of the "Swank Port" user preference
(defun preference-swank-port ()
  (let* ((defaults (handler-case (#/values (#/sharedUserDefaultsController ns:ns-user-defaults-controller))
                     (serious-condition (c) 
                       (progn (format t "~%ERROR: Unable to get preferences from the Shared User Defaults Controller")
                              (force-output)
                              nil))))
         (swank-port-pref (and defaults (#/valueForKey: defaults #@"swankPort"))))
    (cond
      ;; the user default is not initialized
      ((or (null swank-port-pref)
           (%null-ptr-p swank-port-pref)) nil)
      ;; examine the user default
      ((typep swank-port-pref 'ns:ns-string) 
       (handler-case (let* ((port-str (lisp-string-from-nsstring swank-port-pref))
                            (port (parse-integer port-str :junk-allowed nil)))
                       (or port *default-gui-swank-port*))
         ;; parsing the port number failed
         (ccl::parse-integer-not-integer-string (c)
           (setf *ccl-swank-active-p* nil)
           (format t "~%Error starting swank server; the swank-port user preference is not a valid port number: ~S~%"
                   port-str)
           (force-output)
           nil)))
      ;; the user default value is incomprehensible
      (t (progn
           (format t "~%ERROR: Unrecognized value type in user preference 'swankPort': ~S"
                   swank-port-pref)
           (force-output)
           nil)))))

;;; try-starting-swank (&key (force nil))
;;; attempts to start the swank server. If :force t is supplied,
;;; ignores the "Start Swank Server?" user preference and starts the
;;; server no matter what its value

(defun try-starting-swank (&key (force nil))
  (unless *ccl-swank-active-p*
    ;; try to determine the user preferences concerning the swank port number
    ;; and whether the swank server should be started. If the user says start
    ;; it, and we can determine a valid port for it, start it up
    (let* ((start-swank? (or (preference-start-swank?) force))
           (swank-port (or (preference-swank-port) *default-gui-swank-port*)))
      (if (and start-swank? swank-port)
          ;; try to start the swank server
          (handler-case (progn
                          (swank:create-server :port swank-port :dont-close t)
                          (setf *ccl-swank-active-p* t)
                          (setf *active-gui-swank-port* swank-port)
                          swank-port)
            ;; swank server creation failed
            (serious-condition (c)
              (setf *ccl-swank-active-p* nil)
              (setf *active-gui-swank-port* nil)
              (format t "~%Error starting swank server: ~A~%" c)
              (force-output)
              nil))
          ;; don't try to start the swank server
          (progn
            (setf *ccl-swank-active-p* nil)
            (setf *active-gui-swank-port* nil)
            nil)))))


;;; start-swank-listener
;;; -----------------------------------------------------------------
;;; starts up CCL's swank-listener server on the specified port

;;; aux utils

(defun not-ready-yet (nm)
  (error "Not yet implemented: ~A" nm))

(defun read-swank-ping (tcp-stream) 
  (not-ready-yet 'read-swank-ping))

(defun parse-swank-ping (string) 
  (not-ready-yet 'parse-swank-ping))

(defun make-swank-loader-pathname (string) 
  (not-ready-yet 'make-swank-loader-pathname))

(defun load-swank (pathname) 
  (not-ready-yet 'load-swank))

(defun send-swank-ready (tcp-stream swank-status)
  (not-ready-yet 'send-swank-ready))

(defun send-swank-load-failed (tcp-stream swank-status)
  (not-ready-yet 'send-swank-load-failed))

;;; the real deal
;;; if it succeeds, it returns a PROCESS object
;;; if it fails, it returns a CONDITION object
(defun start-swank-listener (&optional (port *default-swank-listener-port*))
  (flet ((handle-swank-client (c)
           (let* ((msg (read-swank-ping c))
                  (swank-path (parse-swank-ping msg))
                  (swank-loader (make-swank-loader-pathname swank-path))
                  (swank-status (load-swank swank-loader)))
             (if (swank-ready? swank-status)
                 (send-swank-ready c swank-status)
                 (send-swank-load-failed c swank-status)))))
    (handler-case (with-open-socket (sock :type :stream :connect :passive :local-port port :reuse-address t)
                    (loop
                       (let ((client-sock (accept-connection sock)))
                         (process-run-function "CCL Swank Listener"
                                               #'%handle-swank-client client-sock))))
      (ccl::socket-creation-error (c) (nslog-condition c "Unable to create a socket for the swank-listener: ") c)
      (ccl::socket-error (c) (nslog-condition c "Swank-listener failed trying to accept a client conection: ") c)
      (serious-condition (c) (nslog-condition c "Unable to start up the swank listener") c))))



;;; maybe-start-swank-listener
;;; -----------------------------------------------------------------
;;; checks whether to start the ccl swank listener, and starts it if
;;; warranted.
(defun maybe-start-swank-listener (&optional (force nil))
  (unless *ccl-swank-active-p*
    ;; try to determine the user preferences concerning the swank port number
    ;; and whether the swank listener should be started. If the user says start
    ;; it, and we can determine a valid port for it, start it up
    (let* ((start-swank-listener? (or (preference-start-swank?) force))
           (swank-listener-port (or (preference-swank-port) *default-gui-swank-port*)))
      (if (and start-swank-listener? swank-port)
          ;; try to start the swank listener
          (handler-case (progn
                          (start-swank-listener swank-listener-port)
                          (setf *active-gui-swank-listener-port* swank-listener-port)
                          (setf *ccl-swank-listener-active-p* t)
                          swank-listener-port)
            ;; swank listener creation failed
            (serious-condition (c)
              (setf *active-gui-swank-listener-port* nil)
              (setf *ccl-swank-listener-active-p* nil)
              (format t "~%Error starting swank server: ~A~%" c)
              (force-output)
              nil))
          ;; don't try to start the swank listener
          (progn
            (setf *active-gui-swank-listener-port* nil)
            (setf *ccl-swank-listener-active-p* nil)
            nil)))))

(provide :swank)