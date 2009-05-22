(in-package "GUI")

(defparameter *standalone-cocoa-ide* nil)

(if (< #&NSAppKitVersionNumber 824)
    (error "This application requires features introduced in OSX 10.4."))

(def-cocoa-default  *ccl-directory* :string "" nil
                    #+no #'(lambda (old new)
                             (when (equal new "") (setq new nil))
                             (unless (and new (equal old new))
                               (init-interfaces-root)
                               (ccl::replace-base-translation
                                "ccl:"
                                (or new (find-ccl-directory))))))

;; If there are interfaces inside the bundle, use those rather than the ones
;; in CCL:, since they're more likely to be valid.  CCL: could be some random
;; old sources we're just using for meta-.
(defun init-interfaces-root ()
  (let* ((subpath (ccl::cdb-subdirectory-path))
         (path (pathname-directory (ccl::ccl-directory))))
    (when (and *standalone-cocoa-ide*
               (equalp (last path 2) '("Contents" "MacOS")))
      (setq path (butlast path))
      (when (or (probe-file (make-pathname :directory (append path subpath)))
                (probe-file (make-pathname :directory (append (setq path `(,@path "Resources")) subpath))))
        (setq ccl::*interfaces-root* (make-pathname :directory path))))))

(defun find-ccl-directory ()
  (let* ((path (ccl::ccl-directory))
         (dir (pathname-directory path)))
    (if (equalp (last dir 2) '("Contents" "MacOS"))
        (make-pathname :directory (butlast dir 3))
        path)))


(defmethod ccl::ui-object-do-operation ((o ns:ns-application)
                                        operation
                                        &rest args)
  (declare (ignore operation args))
  ;; Do nothing.  Would it be better to warn and/or log this ?
  )

(defmethod ccl::ui-object-do-operation ((o ns:ns-application)
                                        (operation (eql :note-current-package))
                                        &rest args)
  (ui-object-note-package o (car args)))

(defmethod ccl::ui-object-do-operation ((o ns:ns-application)
                                        (operation (eql :eval-selection))
                                        &rest args)
  (ui-object-eval-selection o (car args)))

(defmethod ccl::ui-object-do-operation ((o ns:ns-application)
                                        (operation (eql :enter-backtrace-context))
                                        &rest args)
  (ui-object-enter-backtrace-context o (car args)))

(defmethod ccl::ui-object-do-operation ((o ns:ns-application)
                                        (operation (eql :exit-backtrace-context))
                                        &rest args)
  (ui-object-exit-backtrace-context o (car args)))


;;; Support for saving a stand-alone IDE


(defclass cocoa-application (application)
  ())

(defmethod ccl::application-error ((a cocoa-application) condition error-pointer)
  (ccl::break-loop-handle-error condition error-pointer))


(defmethod ccl::application-init-file ((a cocoa-application))
  '("home:ccl-init" "home:\\.ccl-init"))

;;; If we're launched via the Finder, the only argument we'll
;;; get is of the form -psnXXXXXX.  That's meaningless to us;
;;; it's easier to pretend that we didn't get any arguments.
;;; (If it seems like some of this needs to be thought out a
;;; bit better ... I'd tend to agree.)
(defmethod ccl::parse-application-arguments ((a cocoa-application))
  (values nil nil nil nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
    (require :swank))

(defun try-starting-swank ()
  (unless *ccl-swank-active-p*
    ;; try to determine the user preferences concerning the swank port number
    ;; and whether the swank server should be started. If the user says start
    ;; it, and we can determine a valid port for it, start it up
    (let* ((defaults (handler-case (#/values (#/sharedUserDefaultsController ns:ns-user-defaults-controller))
                       (serious-condition (c) 
                         (progn (format t "~%ERROR: Unable to get preferences from the Shared User Defaults Controller")
                                nil))))
           (start-swank-pref (and defaults (#/valueForKey: defaults #@"startSwankServer")))
           (start-swank? (cond
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
                                           nil))))
                           ;; the user default value is incomprehensible
                           (t (progn
                                (format t "~%ERROR: Unrecognized value type in user preference 'startSwankServer': ~S"
                                        start-swank-pref)
                                nil))))
           (swank-port-pref (and defaults (#/valueForKey: defaults #@"swankPort")))
           (swank-port (cond
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
                              nil)))
                         ;; the user default value is incomprehensible
                         (t (progn
                              (format t "~%ERROR: Unrecognized value type in user preference 'swankPort': ~S"
                                      swank-port-pref)
                              nil)))))
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
              nil))
          ;; don't try to start the swank server
          (progn
            (setf *ccl-swank-active-p* nil)
            (setf *active-gui-swank-port* nil)
            nil)))
    (force-output)))

(defmethod toplevel-function ((a cocoa-application) init-file)
  (declare (ignore init-file))
  (when (< #&NSAppKitVersionNumber 824)
    (#_NSLog #@"This application requires features introduced in OSX 10.4.")
    (#_ _exit -1))
  (setq *standalone-cocoa-ide* t)
  ;; It's probably reasonable to do this here: it's not really IDE-specific
  (try-starting-swank)
  (try-connecting-to-altconsole)
  ;; TODO: to avoid confusion, should now reset *cocoa-application-path* to
  ;; actual bundle path where started up.
  (start-cocoa-application))




(defun build-ide (bundle-path)
  (setq bundle-path (ensure-directory-pathname bundle-path))

  ;; The bundle is expected to exist, we'll just add the executable into it.
  (assert (probe-file bundle-path))

  ;; Wait until we're sure that the Cocoa event loop has started.
  (wait-on-semaphore *cocoa-application-finished-launching*)

  (require :easygui)

  (ccl::maybe-map-objc-classes t)
  (let* ((missing ()))
    (ccl::do-interface-dirs (d)
      (ccl::cdb-enumerate-keys
       (ccl::db-objc-classes d)
       (lambda (name)
         (let* ((class (ccl::lookup-objc-class name nil)))
           (unless (ccl::objc-class-id  class) (push name missing))))))
    (when missing
      (break "ObjC classes ~{~&~a~} are declared but not defined." missing)))

  (ccl::touch bundle-path)

  (let ((image-file (make-pathname :name (ccl::standard-kernel-name) :type nil :version nil
                                   :defaults (merge-pathnames ";Contents;MacOS;" bundle-path))))
    (format *error-output* "~2%Saving application to ~a~2%" (truename bundle-path))
    (force-output *error-output*)
    (ensure-directories-exist image-file)
    (save-application image-file
                      :prepend-kernel t
                      :application-class 'cocoa-application)))

;;; If we're running as a standalone .app, try to see if a bundle named
;;; AltConsole.app exists in our Resources directory.  If so, execute
;;; that bundle'es executable file, with its standard input/output/error
;;; descriptors connected to one end of a socketpair, and connect
;;; lisp's *TERMINAL-IO* and C's stdin/stdout/stderr to the other end
;;; of the socket.

(defun try-connecting-to-altconsole ()
  (with-autorelease-pool
      (let* ((main-bundle (#/mainBundle ns:ns-bundle))
             (resource-path (#/resourcePath main-bundle)))
        (block exit
          (when (%null-ptr-p resource-path)
            (return-from exit nil))
          (let* ((altconsole-bundle
                  (make-instance ns:ns-bundle
                                 :with-path
                                 (#/stringByAppendingPathComponent:
                                  resource-path
                                  #@"AltConsole.app"))))
            (when (%null-ptr-p altconsole-bundle)
              (return-from exit nil))
            (let* ((executable-path (#/executablePath altconsole-bundle)))
              (when (%null-ptr-p executable-path)
                (return-from exit nil))
              (let* ((nbytes (1+ (#/lengthOfBytesUsingEncoding:
                                  executable-path
                                  #$NSUTF8StringEncoding))))
                (%stack-block ((c-executable-path nbytes))
                  (unless (#/getCString:maxLength:encoding:
                           executable-path
                           c-executable-path
                           nbytes
                           #$NSUTF8StringEncoding)
                    (return-from exit nil))
                  (rletz ((argv (:array :address 2))
                          (envp (:array :address 1))
                          (sockets (:array :int 2)))
                    (setf (paref argv (:array :address) 0) c-executable-path)
                    (unless (eql 0 (#_socketpair #$AF_UNIX #$SOCK_STREAM 0 sockets))
                      (return-from exit nil))
                    (let* ((parent-socket (paref sockets (:array :int) 0))
                           (child-socket (paref sockets (:array :int) 1))
                           (pid (#_fork)))
                      (case pid
                        (-1
                         ;; Fork failed
                         (#_close parent-socket)
                         (#_close child-socket)
                         (return-from exit nil))
                        (0
                         ;; This runs in the child.
                         (#_close parent-socket)
                         (#_dup2 child-socket 0)
                         (#_dup2 child-socket 1)
                         (#_dup2 child-socket 2)
                         (#_execve c-executable-path
                                   argv
                                   envp)
                         ;; If the #_exec fails, there isn't
                         ;; much to do or say about it.
                         (#__exit 1))
                        (t
                         ;; We're the parent.
                         (#_close child-socket)
                         (when (eq t (ccl::check-pid pid))
                           (flet ((set-lisp-stream-fd (stream fd)
                                    (setf (ccl::ioblock-device (ccl::stream-ioblock stream t))
                                          fd)))
                             (ff-call (ccl::%kernel-import target::kernel-import-open-debug-output)
                                      :int parent-socket
                                      :int)
                             (#_dup2 parent-socket 0)
                             (set-lisp-stream-fd ccl::*stdin* parent-socket)
                             (set-lisp-stream-fd ccl::*stdout* parent-socket))
                           ;; Ensure that output to the stream ccl::*stdout* -
                           ;; which is connected to fd 1 - is flushed periodically
                           ;; by the housekeeping task.  (ccl::*stdout* is
                           ;; typically the output side of the two-way stream
                           ;; which is the global/static value of *TERMINAL-IO*;
                           ;; many standard streams are synonym streams to
                           ;; *TERMINAL-IO*.
                           (ccl::add-auto-flush-stream ccl::*stdout*)
                           pid)))))))))))))
                      
                    
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(start-cocoa-application)
