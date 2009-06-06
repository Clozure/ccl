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
;;; implements tools used to locate and load a swank server at app startup.

(in-package :GUI)

(defparameter *ccl-swank-active-p* nil)
(defparameter *default-swank-listener-port* 4884)
(defparameter *active-gui-swank-listener-port* nil)
(defparameter *ccl-swank-listener-active-p* nil)
(defvar *swank-listener-process* nil)

(defun swank-listener-active? ()
  (and *swank-listener-process*
       (typep *swank-listener-process* 'process)
       (not (member (process-whostate *swank-listener-process*)
                    '("Reset" "Exhausted")
                    :test 'string-equal))))

;;; preference-swank-listener-port
;;; returns the current value of the "Swank Port" user preference
(defun preference-swank-listener-port ()
  (with-autorelease-pool
    (let* ((defaults (handler-case (#/values (#/sharedUserDefaultsController ns:ns-user-defaults-controller))
                       (serious-condition (c) 
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
           (serious-condition (c)
             (declare (ignore c))
             (setf *ccl-swank-listener-active-p* nil)
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
                     (serious-condition (c) 
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

;;; start-swank-listener
;;; -----------------------------------------------------------------
;;; starts up CCL's swank-listener server on the specified port

;;; aux utils

(defvar $emacs-ccl-swank-request-marker "[emacs-ccl-swank-request]")

(defvar $last-swank-message-sent nil)

(defun swank-server-running? ()
  (and (find-package :swank)
       (let ((active-listeners (symbol-value (intern "*LISTENER-SOCKETS*" :swank))))
         (and (not (null active-listeners))
              (first active-listeners)))))

(defstruct (swank-status (:conc-name swank-))
  (active? nil :read-only t)
  (message nil :read-only t)
  (requested-loader nil :read-only t)
  (requested-port nil :read-only t))

(defun read-swank-ping (tcp-stream) 
  (read-line tcp-stream nil nil nil))

(defun parse-swank-ping (p) 
  (let ((sentinel-end (length $emacs-ccl-swank-request-marker)))
    (if (typep p 'string)
        (if (string= p $emacs-ccl-swank-request-marker :start1 0 :end1 sentinel-end)
            (let* ((request (subseq p sentinel-end))
                   (split-pos (position #\: request))
                   (port-str (if split-pos
                                 (subseq request 0 split-pos)
                                 nil))
                   (port (when port-str (parse-integer port-str :junk-allowed nil)))
                   (path-str (if split-pos
                                 (subseq request (1+ split-pos))
                                 request)))
              (values (string-trim '(#\space #\tab #\return #\newline) path-str) port))
            nil)
        nil)))


(defun load-and-start-swank (path requested-port) 
  (handler-case (let* ((active-swank-port (swank-server-running?))
                       (msg (format nil "A swank server is already running on port ~A" active-swank-port)))
                  (if active-swank-port
                      (progn
                        (log-debug msg)
                        (make-swank-status :active? t :message msg :requested-loader path :requested-port requested-port))
                      (progn
                        (load path)
                        (let ((swank-loader-package (find-package :swank-loader)))
                          (if swank-loader-package
                              ;; swank loaded. start the server
                              (progn
                                (funcall (intern "LOAD-SWANK" swank-loader-package))
                                (funcall (intern "CREATE-SERVER" (find-package :swank)) :port requested-port :dont-close t)
                                (make-swank-status :active? t :requested-loader path :requested-port requested-port))
                              ;; swank failed to load. return failure status
                              (make-swank-status :active? nil :message "swank load failed" :requested-loader path :requested-port requested-port))))))
    (ccl::socket-creation-error (e) (log-debug "Unable to start a swank server on port: ~A; ~A"
                                               requested-port e)
                                (make-swank-status :active? nil :message "socket-creation error"
                                                   :requested-loader path :requested-port requested-port))
    (serious-condition (e) (log-debug "There was a problem creating the swank server on port ~A: ~A"
                                      requested-port e)
                       (make-swank-status :active? nil :message "error loading or starting swank"
                                          :requested-loader path :requested-port requested-port))))

(defun swank-ready? (status)
  (swank-active? status))

(defun send-swank-response (tcp-stream status)
  (let ((response 
         (let ((*print-case* :downcase))
           (format nil "(:active ~S :loader ~S :message ~S :port ~D)"
                   (swank-active? status)
                   (swank-requested-loader status)
                   (swank-message status)
                   (swank-requested-port status)))))
    (format tcp-stream response)
    (finish-output tcp-stream)))

(defun handle-swank-client (c)
  (let* ((msg (read-swank-ping c)))
    (multiple-value-bind (swank-path requested-port)
        (parse-swank-ping msg)
      (load-and-start-swank swank-path requested-port))))

(defun stop-swank-listener ()
  (process-kill *swank-listener-process*)
  (setq *swank-listener-process* nil))

;;; the real deal
;;; if it succeeds, it returns a PROCESS object
;;; if it fails, it returns a CONDITION object
(defun start-swank-listener (&optional (port *default-swank-listener-port*))
  (handler-case 
      (if (swank-listener-active?)
          (log-debug "in start-swank-listener: the swank listener process is already running")
          (setq *swank-listener-process*
                (process-run-function "Swank Listener"
                                      #'(lambda ()
                                          (with-open-socket (sock :type :stream :connect :passive 
                                                                  :local-port port :reuse-address t :auto-close t)
                                            (loop
                                               (let* ((client-sock (accept-connection sock))
                                                      (status (handle-swank-client client-sock)))
                                                 (send-swank-response client-sock status))))))))
    (ccl::socket-creation-error (c) (nslog-condition c "Unable to create a socket for the swank-listener: ") c)
    (ccl::socket-error (c) (nslog-condition c "Swank-listener failed trying to accept a client connection: ") c)
    (serious-condition (c) (nslog-condition c "Error starting in the swank-listener:") c)))

;;; maybe-start-swank-listener
;;; -----------------------------------------------------------------
;;; checks whether to start the ccl swank listener, and starts it if
;;; warranted.
(defun maybe-start-swank-listener (&key (override-user-preference nil))
  (unless *ccl-swank-listener-active-p*
    ;; try to determine the user preferences concerning the
    ;; swank-listener port number and whether the swank listener
    ;; should be started. If the user says start it, and we can
    ;; determine a valid port for it, start it up
    (let* ((start-swank-listener? (or (preference-start-swank-listener?) override-user-preference))
           (swank-listener-port (or (preference-swank-listener-port) *default-swank-listener-port*)))
      (if (and start-swank-listener? swank-listener-port)
          ;; try to start the swank listener
          (handler-case (let ((swank-listener (start-swank-listener swank-listener-port)))
                          (if (typep swank-listener 'process)
                              (progn
                                (setf *active-gui-swank-listener-port* swank-listener-port)
                                (setf *ccl-swank-listener-active-p* t)
                                swank-listener-port)
                              (progn
                                (setf *active-gui-swank-listener-port* nil)
                                (setf *ccl-swank-listener-active-p* nil)
                                nil)))
            ;; swank listener creation failed
            (serious-condition (c)
              (setf *active-gui-swank-listener-port* nil)
              (setf *ccl-swank-listener-active-p* nil)
              (log-debug "~%Error starting swank listener: ~A~%" c)
              nil))
          ;; don't try to start the swank listener
          (progn
            (setf *active-gui-swank-listener-port* nil)
            (setf *ccl-swank-listener-active-p* nil)
            nil)))))

(provide :swank-listener)