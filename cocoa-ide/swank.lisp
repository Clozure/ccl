(in-package :GUI)

(defparameter *default-gui-swank-port* 4564)
(defparameter *active-gui-swank-port* nil)
(defparameter *ccl-swank-active-p* nil)

(load #P"ccl:cocoa-ide;slime;swank-loader.lisp")
(swank-loader::load-swank)

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

(provide :swank)