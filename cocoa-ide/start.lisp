(in-package "GUI")

(defparameter *standalone-cocoa-ide* nil)

#-cocotron
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

(defmethod ccl::ui-object-do-operation ((o ns:ns-application) (operation (eql :break-options-string)) &rest args)
  (unless (typep ccl::*current-process* 'appkit-process)
    (destructuring-bind (continuablep) args
      (if continuablep
        "~&> Type cmd-/ to continue, cmd-. to abort, cmd-\\ for a list of available restarts."
        "~&> Type cmd-. to abort, cmd-\\ for a list of available restarts.~%"))))

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
    (require :swank-listener))

(defmethod toplevel-function ((a cocoa-application) init-file)
  (declare (ignore init-file))
  #-cocotron
  (when (< #&NSAppKitVersionNumber 824)
    (#_NSLog #@"This application requires features introduced in OSX 10.4.")
    (#_ _exit -1))
  (setq *standalone-cocoa-ide* t)
  (maybe-start-swank-listener)
  (with-slots  (have-interactive-terminal-io) ccl::*current-process*
    (when (and (eql (nth-value 4 (ccl::%stat "/dev/null"))
                    (nth-value 4 (ccl::%fstat 0)))
             ;; Should compare st_dev, too
             )
      (setq have-interactive-terminal-io nil)
      
      ;; It's probably reasonable to do this here: it's not really IDE-specific
      (when (try-connecting-to-altconsole)
        (setq have-interactive-terminal-io t)))
    ;; TODO: to avoid confusion, should now reset *cocoa-application-path* to
    ;; actual bundle path where started up.
    (start-cocoa-application)))




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
  #-cocotron
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
