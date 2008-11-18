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


(defmethod toplevel-function ((a cocoa-application) init-file)
  (declare (ignore init-file))
  (when (< #&NSAppKitVersionNumber 824)
    (#_NSLog #@"This application requires features introduced in OSX 10.4.")
    (#_ _exit -1))
  (setq *standalone-cocoa-ide* t)
  ;; TODO: to avoid confusion, should now reset *cocoa-application-path* to
  ;; actual bundle path where started up.
  (start-cocoa-application))


;;; The saved image will be an instance of COCOA-APPLICATION (mostly
;;; so that it'll ignore its argument list.)  When it starts up, it'll
;;; run the Cocoa event loop in the cocoa event process.
;;; If you use an init file ("home:ccl-init"), it'll be loaded
;;; in an environment in which *STANDARD-INPUT* always generates EOF
;;; and where output and error streams are directed to the OSX console
;;; (see below).  If that causes problems, you may want to suppress
;;; the loading of your init file (via an :INIT-FILE nil arg to
;;; the call to SAVE-APPLICATION, below.)

(defun build-ide (bundle-path)
  (setq bundle-path (ensure-directory-pathname bundle-path))

  ;; The bundle is expected to exists, we'll just add the executable into it.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(start-cocoa-application)
