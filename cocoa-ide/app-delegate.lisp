;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;; Copyright 2007 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "GUI")

(defclass ide-application-delegate (ns:ns-object)
    ()
  (:metaclass ns:+ns-object))

;;; This method is a good place to:
;;;  * register value transformer names
;;;  * register default user defaults
(objc:defmethod (#/initialize :void) ((self +ide-application-delegate))
  (#/setValueTransformer:forName: ns:ns-value-transformer
				  (make-instance 'font-to-name-transformer)
				  #@"FontToName")

  (let* ((domain (#/standardUserDefaults ns:ns-user-defaults))
	 (initial-values (cocoa-defaults-initial-values))
	 (dict (#/mutableCopy initial-values)))
    (declare (special *standalone-cocoa-ide*))
    #+cocotron
    (#/setObject:forKey:
     dict
     (#/dictionaryWithObjectsAndKeys:
      ns:ns-dictionary
      #@"Control" #@"LeftControl"
      #@"Alt" #@"LeftAlt"
      #@"Command" #@"RightControl"
      #@"Alt" #@"RightAlt"
      +null-ptr+)
     #@"NSModifierFlagMapping")
    (#/registerDefaults: domain dict)
    (#/release dict)
    (update-cocoa-defaults)
    #-mac-app-store
    (when *standalone-cocoa-ide*
      (init-ccl-directory-for-ide))))

(defun init-ccl-directory-for-ide ()
  (let* ((bundle-path (#/bundlePath (#/mainBundle ns:ns-bundle)))
         (parent (#/stringByDeletingLastPathComponent bundle-path))
         (path (ccl::ensure-directory-pathname
                (lisp-string-from-nsstring parent))))
    (ccl::replace-base-translation "ccl:" path)))


(defvar *ccl-ide-init-file* "home:ccl-ide-init")

;;; Errors that occur while this file is loading will enter a break
;;; loop, with *DEBUG-IO* connected to the terminal/Emacs, to AltConsole,
;;; or to /dev/null and syslog.
(defun load-ide-init-file ()
  (with-simple-restart (continue "Skip loading IDE init file.")
    (load *ccl-ide-init-file* :if-does-not-exist nil :verbose nil)))



(objc:defmethod (#/applicationWillFinishLaunching: :void)
    ((self ide-application-delegate) notification)
  (declare (ignore notification))
  (initialize-user-interface)
  (initialize-menus))

(objc:defmethod (#/applicationWillTerminate: :void)
		((self ide-application-delegate) notification)
  (declare (ignore notification))
  ;; UI has decided to quit; terminate other lisp threads.
  (ccl::prepare-to-quit))

(defloadvar *preferences-window-controller* nil)

(objc:defmethod (#/showPreferences: :void) ((self ide-application-delegate)
					    sender)
  (declare (ignore sender))
  (when (null *preferences-window-controller*)
    (setf *preferences-window-controller*
	  (make-instance 'ccl-preferences-window-controller)))
  (#/showWindow: *preferences-window-controller* self))

(defloadvar *processes-window-controller* nil)

(objc:defmethod (#/showProcessesWindow: :void) ((self ide-application-delegate)
						sender)
  (declare (ignore sender))
  (when (null *processes-window-controller*)
    (setf *processes-window-controller*
	  (make-instance 'processes-window-controller)))
  (#/showWindow: *processes-window-controller* self))

(defloadvar *apropos-window-controller* nil)

(objc:defmethod (#/showAproposWindow: :void) ((self ide-application-delegate)
						sender)
  (declare (ignore sender))
  (when (null *apropos-window-controller*)
    (setf *apropos-window-controller*
	  (make-instance 'apropos-window-controller)))
  (#/showWindow: *apropos-window-controller* self))

(defloadvar *xapropos-window-controller* nil)

(objc:defmethod (#/showXaproposWindow: :void) ((self ide-application-delegate)
						sender)
  (declare (ignore sender))
  (when (null *xapropos-window-controller*)
    (setf *xapropos-window-controller*
	  (make-instance 'xapropos-window-controller)))
  (#/showWindow: *xapropos-window-controller* self))

(objc:defmethod (#/showNewInspector: :void) ((self ide-application-delegate)
                                             sender)
  (declare (ignore sender))
  (#/showWindow: (make-instance 'inspector::xinspector-window-controller
                   :inspector (inspector::make-inspector *package*)) self))

(objc:defmethod (#/showSearchFiles: :void) ((self ide-application-delegate)
                                            sender)
  ;;If function key is pressed, always make a new window
  ;;otherwise bring frontmost search files window to the front
  (declare (ignore sender))
  (let ((w nil))
    (if (or (current-event-modifier-p #$NSFunctionKeyMask)
            (null (setf w (first-window-with-controller-type 'search-files-window-controller))))
      (let* ((wc (make-instance 'search-files-window-controller)))
        (#/showWindow: wc self)
        (set-search-files-default-dir wc))
      (progn
        (#/makeKeyAndOrderFront: w self)
        (set-search-files-default-dir (#/windowController w))))))

(objc:defmethod (#/showListDefinitions: :void) ((self hemlock-text-view)
                                               sender)
  (declare (ignore sender))
  (let ((view (hemlock-view self)))
    (hi::handle-hemlock-event view
                              #'(lambda () 
                                  (hemlock::show-list-definitions-window view)))))

(objc:defmethod (#/newListener: :void) ((self ide-application-delegate)
                                        sender)
  (declare (ignore sender))
  (#/openUntitledDocumentOfType:display:
   (#/sharedDocumentController ns:ns-document-controller) #@"Listener" t))

(defun active-listener-windows ()
  (let* ((listener-windows ())
         (all-windows (#/orderedWindows *NSApp*)))
    (dotimes (i (#/count all-windows) (nreverse listener-windows))
      (let* ((w (#/objectAtIndex: all-windows i))
             (wc (#/windowController w)))
        (when (and (typep wc 'hemlock-listener-window-controller)
                   (#/isVisible w))
          (let* ((doc (#/document wc)))
            (unless (%null-ptr-p doc)
              (when (hemlock-document-process doc)
                (push w listener-windows)))))))))
        
                
(objc:defmethod (#/showListener: :void) ((self ide-application-delegate)
                                        sender)
  (declare (ignore sender))
  (let* ((key-window (#/keyWindow *NSApp*))
	 (listener-windows (active-listener-windows))
	 (top-listener nil))
    (setq top-listener (car listener-windows))
    (cond 
     ((null listener-windows)
      (#/newListener: self +null-ptr+))
     ((eql key-window top-listener)
      ;; The current window is a listener.  If there is more than
      ;; one listener, bring the rear-most forward.
      (let* ((w (car (last listener-windows))))
	(if (eql top-listener w)
	  (#_NSBeep)
	  (#/makeKeyAndOrderFront: w +null-ptr+))))
     (t
      (#/makeKeyAndOrderFront: top-listener +null-ptr+)))))

(objc:defmethod (#/ensureListener: :void) ((self ide-application-delegate)
					   sender)
  (declare (ignore sender))
  (let ((top-listener-document (#/topListener hemlock-listener-document)))
    (when (eql top-listener-document +null-ptr+)
      (let* ((dc (#/sharedDocumentController ns:ns-document-controller))
	     (wc nil))
	(setq top-listener-document
              #+cocotron (#/makeUntitledDocumentOfType: dc #@"Listener")
	      #-cocotron (#/makeUntitledDocumentOfType:error: dc #@"Listener" +null-ptr+))
	(#/addDocument: dc top-listener-document)
	(#/makeWindowControllers top-listener-document)
	(setq wc (#/lastObject (#/windowControllers top-listener-document)))
	(#/orderFront: (#/window wc) +null-ptr+)))))

(defvar *cocoa-ide-finished-launching* (make-semaphore)
  "Semaphore that's signaled when the application's finished launching ...")

(objc:defmethod (#/applicationDidFinishLaunching: :void)
    ((self ide-application-delegate) notification)
  (declare (ignore notification))
  (unless (shift-key-now-p)
    (load-ide-init-file))
  (signal-semaphore *cocoa-ide-finished-launching*))

(objc:defmethod (#/applicationShouldOpenUntitledFile: #>BOOL)
    ((self ide-application-delegate) app)
  (declare (ignore app))
  t)

(objc:defmethod (#/applicationOpenUntitledFile: :<BOOL>)
    ((self ide-application-delegate) app)
  (when (zerop *cocoa-listener-count*)
    (#/newListener: self app)
    t))

(objc:defmethod (#/loadFile: :void) ((self ide-application-delegate) sender)
  (declare (ignore sender))
  (let ((filename (cocoa-choose-file-dialog
		   :button-string "Load"
		   :file-types (list (pathname-type *.lisp-pathname*)
				     (pathname-type *.fasl-pathname*)))))
    (when filename
      (#/ensureListener: self nil)
      (let ((process (top-listener-process)))
	(process-interrupt process #'(lambda ()
				       (load filename)
				       (fresh-line)))))))

(objc:defmethod (#/compileFile: :void) ((self ide-application-delegate) sender)
  (declare (ignore sender))
  (let ((filename (cocoa-choose-file-dialog
		   :button-string "Compile"
		   :file-types (list (pathname-type *.lisp-pathname*)))))
    (when filename
      (#/ensureListener: self nil)
      (let ((process (top-listener-process)))
	(process-interrupt process #'(lambda ()
				       (compile-file filename)
				       (fresh-line)))))))

(objc:defmethod (#/exitBreak: :void) ((self ide-application-delegate) sender)
  (let* ((top-listener (#/topListener hemlock-listener-document)))
    (unless (%null-ptr-p top-listener)
      (#/exitBreak: top-listener sender))))

(objc:defmethod (#/continue: :void) ((self ide-application-delegate) sender)
  (let* ((top-listener (#/topListener hemlock-listener-document)))
    (unless (%null-ptr-p top-listener)
      (#/continue: top-listener sender))))

(objc:defmethod (#/restarts: :void) ((self ide-application-delegate) sender)
  (let* ((top-listener (#/topListener hemlock-listener-document)))
    (unless (%null-ptr-p top-listener)
      (#/restarts: top-listener sender))))

(objc:defmethod (#/backtrace: :void) ((self ide-application-delegate) sender)
  (let* ((top-listener (#/topListener hemlock-listener-document)))
    (unless (%null-ptr-p top-listener)
      (#/backtrace: top-listener sender))))

(objc:defmethod (#/validateMenuItem: #>BOOL) ((self ide-application-delegate) item)
  (let* ((action (#/action item)))
    (cond ((or (eql action (@selector "exitBreak:"))
               (eql action (@selector "continue:"))
               (eql action (@selector "restarts:"))
               (eql action (@selector "backtrace:")))
           (let* ((top-listener (#/topListener hemlock-listener-document)))
             (unless (%null-ptr-p top-listener)      
               (#/validateMenuItem: top-listener item))))
          (t t))))

(objc:defmethod (#/showManual: :void) ((self ide-application-delegate) sender)
  (declare (ignore sender))
  (ccl::open-url-in-browser "http://ccl.clozure.com/docs/ccl.html"))

(defloadvar *hemlock-commands-window-controller* nil)

(objc:defmethod (#/showHemlockCommands: :void) ((self ide-application-delegate)
                                               sender)
  (declare (ignore sender))
  (when (null *hemlock-commands-window-controller*)
    (setf *hemlock-commands-window-controller*
	  (make-instance 'hemlock-commands-window-controller)))
  (#/showWindow: *hemlock-commands-window-controller* +null-ptr+))


(defun set-window-line-wrapping (window wrapping)
  (let* ((pane (slot-value window 'pane))
         (text-view (slot-value pane 'text-view))
         (scroll-view (#/enclosingScrollView text-view))
         (content-size (#/contentSize scroll-view))
         (container (#/textContainer text-view)))
    (#/setMinSize: text-view content-size)
    (setf (slot-value window 'wrap-lines-to-window) wrapping)
    (cond (wrapping
           (let ((size (ns:make-ns-size (ns:ns-size-width content-size)
                                        large-number-for-text)))
             (#/setContainerSize: container size)
             (#/setConstrainedFrameSize: text-view size)
             (#/setWidthTracksTextView: container t)
             (#/setHorizontallyResizable: text-view nil)
             (#/setHasHorizontalScroller: scroll-view nil)))
          (t
           (#/setContainerSize: container (ns:make-ns-size large-number-for-text
                                                           large-number-for-text))
           (#/setWidthTracksTextView: container nil)
           (#/setHorizontallyResizable: text-view t)
           (#/setHasHorizontalScroller: scroll-view t)))
    text-view))

(defun enable-window-line-wrapping (window)
  (set-window-line-wrapping window t))

(defun disable-window-line-wrapping (window)
  (set-window-line-wrapping window nil))

(objc:defmethod (#/toggleWindowLineWrapping: :void) ((self hemlock-frame)
                                                     sender)
  (with-slots (wrap-lines-to-window) self
    (cond (wrap-lines-to-window
           (disable-window-line-wrapping self)
           (#/setState: sender #$NSOffState))
          (t
           (enable-window-line-wrapping self)
           (#/setState: sender #$NSOnState)))))

(objc:defmethod (#/validateMenuItem: :<BOOL>) ((self hemlock-frame)
                                               item)
  (let* ((action (#/action item))
         (wrapping (slot-value self 'wrap-lines-to-window)))
    (cond ((eql action (@selector #/toggleWindowLineWrapping:))
           (if wrapping
               (#/setState: item #$NSOnState)
               (#/setState: item #$NSOffState))
           t)
          (t #+cocotron t #-cocotron (call-next-method item)))))
