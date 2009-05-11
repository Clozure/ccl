(in-package "CL-USER")


;;; This is supposed to be a (close) translation of Apple's "MyRecorder"
;;; example, which shows a simple use of QTKit's QTCaptureView, which
;;; was introduced in Leopard.  The example requires a companion nib
;;; file - "QTVidCapture.nib" - which is basically identical to the
;;; "MainMenu.nib" from Apple's example (with the "MainMenu" menu removed
;;; and the window's title and position changed.)  There's a little
;;; utility function at the bottom of this file that runs an "Open" panel
;;; that lets you select this nib file and tries to open a video capture
;;; window.

;;; Apple's MyRecorder example is at:
;;; <http://developer.apple.com/samplecode/MYRecorder/index.html>
;;;
;;; Related guides/tutorials are available at:
;;; <http://developer.apple.com/documentation/QuickTime/Conceptual/QTKitCaptureProgrammingGuide/>


;;; I tried to point out some issues of syntax and bridge arcana that
;;; people new to Cocoa programming in CCL might need help with (I'm
;;; sure that I missed some of that.)  I know very little about QTKit
;;; and QT video capture; the hope is that people looking to do
;;; something more ambituous will find Apple's guides and tutorials
;;; easier to understand after reading this.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "COCOA"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Some of this requires OSX 10.5, which is software-version 9
  ;; as far as we're concerned.
  (if (< (parse-integer (software-version) :junk-allowed t) 9)
    (error "This code requires OSX 10.5 or later"))
  ;; 
  ;; Many class names in QTKit are prefixed with "QT".  Persuade the
  ;; bridge that it should treat "QT" specially when mapping from
  ;; ObjC class names to lisp.
  (ccl::define-special-objc-word "QT")
  (objc:load-framework "QTKit" :qtkit))


;;; The .nib file refers to this class via the name
;;; "MyRecorderController"; in lisp, that's basically any symbol whose
;;; pname is "MY-RECORDER-CONTROLLER".  Likewise, there are references
;;; to the slot "m-capture-view" - or "mCaptureView" in ObjC - in
;;; the nib file.
;;;
;;; If you open the "QTVidCapture.nib" file in IB 3.0, select the
;;; "My Recorder" object in the main view, then choose the "Connections
;;; Inspector" item (cmd-5) from the "Tools" menu, you'll see that
;;; there's an "outlet" from the mCaptureView slot to the window's
;;; Capture View.  This basically means that this slot in the
;;; MY-WINDOW-CONTROLLER instance will be initialized to refer to
;;; the window's QTCaptureView when the .nib file is loaded (if and
;;; only if there's a slot with that name.  Changing the name of the
;;; outlet in the nib file - or the name of the slot in the class
;;; definition below - could keep this initialization from working.

(defclass my-recorder-controller (ns:ns-object)
  ((m-capture-view :foreign-type :id)
   (m-capture-session :foreign-type :id)
   (m-capture-movie-file-output :foreign-type :id)
   (m-capture-video-device-input :foreign-type :id)
   (m-capture-audio-device-input :foreign-type :id))
  (:metaclass ns:+ns-object))


;;; This method will be called (if it's defined) on all objects
;;; in the .nib file have been initialized in ways specified in
;;; the init file (i.e., "outlet" instance variables are set up,
;;; "action" messages are associated with the objects that implement
;;; them, etc.)  The order in which #/awakeFromNib methods are
;;; called isn't specified, but we can count on the m-capture-view
;;; slot being initialized to the QTCaptureView in the window.
;;; For a better explanation of what this code does, see the
;;; Apple tutorial.
;;; The Apple sample code from which this was derived was pretty
;;; casual about reporting errors; this code is equally casual.
;;; Most of the things that can cause errors (missing devices
;;; or resources, etc.) will store an NSError object in the
;;; location that the "perror" pointer points to; this NSError
;;; object can be used to report error conditons and (in some
;;; cases) try to recover from them.  Real code should certainly
;;; try to address those issues.

(objc:defmethod (#/awakeFromNib :void) ((self my-recorder-controller))
  (rlet ((perror (:* :id) +null-ptr+))
    ;; In ObjC, it's generally possible to refer to a class's slots
    ;; as if they were simple variables in a method specialized on
    ;; that class.  OBJC:DEFMETHOD doesn't do that for us, but if
    ;; we want to do that we can use WITH-SLOTS to get a similar
    ;; effect.  (I tend to use SETQ whenever its legal to do so;
    ;; other people use SETF.)
    (with-slots (m-capture-view
                 m-capture-session 
                 m-capture-movie-file-output
                 m-capture-video-device-input
                 m-capture-audio-device-input) self
      ;; Using MAKE-INSTANCE (with no initargs) to create an instance
      ;; of an ObjC class is entirely equivalent to calling #/init
      ;; on the value returned by calling #/alloc on the class,
      (setq m-capture-session (make-instance 'ns:qt-capture-session))
      
      ;; Some of the values below are (non-constant) ObjC variables.
      ;; The #& reader macro lets us access those ObjC variables
      ;; more-or-less as if they were lisp global variables.
      (let* ((video-device (#/defaultInputDeviceWithMediaType:
                            ns:qt-capture-device
                            #&QTMediaTypeVideo))
             (success (#/open: video-device perror)))
        (unless success
          (setq video-device (#/defaultInputDeviceWithMediaType:
                              ns:qt-capture-device
                              #&QTMediaTypeMuxed)
                success (#/open: video-device perror)))
        (when success
          ;; (MAKE-INSTANCE objc-class-or-class-name :with-WHATEVER ...)
          ;; is basically the same as using #/initWithWhatever:
          ;; to initialize a newly-allocated instance of that class.
          ;; MAKE-INSTANCE can also deal with the case where a class
          ;; has a mixture of ObjC and Lisp slots.)
          (setq m-capture-video-device-input 
                (make-instance 'ns:qt-capture-device-input
                               :with-device video-device))
          (setq success (#/addInput:error: m-capture-session m-capture-video-device-input
                                           perror)))
        (when success
          (unless (or (#/hasMediaType: video-device #&QTMediaTypeSound)
                      (#/hasMediaType: video-device #&QTMediaTypeMuxed))
            (let* ((audio-device (#/defaultInputDeviceWithMediaType:
                                  ns:qt-capture-device
                                  #&QTMediaTypeSound)))
              (setq success (#/open: audio-device perror))
              (when success
                (setq m-capture-audio-device-input
                      (make-instance 'ns:qt-capture-device-input
                                     :with-device audio-device)
                      success (#/addInput:error: m-capture-session
                                                 m-capture-audio-device-input
                                                 perror))))))
        (when success
          (setq m-capture-movie-file-output 
                (make-instance 'ns:qt-capture-movie-file-output)
                success (#/addOutput:error: m-capture-session m-capture-movie-file-output perror)))
        (when success
          (#/setDelegate: m-capture-movie-file-output self)
          (let* ((connection-enumerator 
                  (#/objectEnumerator (#/connections m-capture-movie-file-output))))
            (do* ((connection (#/nextObject connection-enumerator)
                              (#/nextObject connection-enumerator)))
                 ((%null-ptr-p connection))
              (let* ((media-type (#/mediaType connection))
                     (compression-options
                      (cond ((#/isEqualToString: media-type #&QTMediaTypeVideo)
                             (#/compressionOptionsWithIdentifier:
                              ns:qt-compression-options
                              #@"QTCompressionOptions240SizeH264Video"))
                            ((#/isEqualToString: media-type #&QTMediaTypeSound)
                             (#/compressionOptionsWithIdentifier:
                              ns:qt-compression-options
                              #@"QTCompressionOptionsHighQualityAACAudio"))
                            (t +null-ptr+))))
                (#/setCompressionOptions:forConnection: m-capture-movie-file-output
                                                       compression-options
                                                       connection))))
          (#/setCaptureSession: m-capture-view m-capture-session)
          (#/startRunning m-capture-session)
          )))))

;;; Similarly, we use WITH-SLOTS here so that we can access slots
;;; as if they were simple variables.  We're basically just trying
;;; to close/free resources that have been associated with this
;;; MY-WINDOW-CONTROLLER instance.
;;; This method is called because the MY-RECORDER-CONTROLLER was
;;; specified as the "delegate" of the window in the nib file.
(objc:defmethod (#/windowWillClose: :void) ((self my-recorder-controller)
                                           notification)
  (declare (ignorable notification))
  (with-slots (m-capture-session
               m-capture-video-device-input
               m-capture-audio-device-input) self
    (unless (%null-ptr-p m-capture-session)
      (#/stopRunning m-capture-session))
    (unless (%null-ptr-p m-capture-video-device-input)
      (if (#/isOpen (#/device m-capture-video-device-input))
        (#/close (#/device m-capture-video-device-input))))
    (unless (%null-ptr-p m-capture-audio-device-input)
      (if (#/isOpen (#/device m-capture-audio-device-input))
        (#/close (#/device m-capture-video-device-input))))))

;;; This method is called when the MY-RECORDER-INSTANCE has had
;;; its reference count go to 0   It basically decrements the
;;; reference counts of the things it has allocated (possibly
;;; causing #/dealloc to be invoked on them), then calls the
;;; superclass method to deallocate itself.
;;; A lisp pointer to the MY-WINDOW-CONTROLLER object might
;;; continue to believe that the object its pointing to is
;;; still a MY-WINDOW-CONTROLLER, even after the actual object
;;; has been deallocated (basically, "deallocated" means "turned
;;; into free memory.)  There isn't currently a good solution
;;; to this problem (such a solution involves deeper integration
;;; between the Lisp and its GC and the ObjC memory-management
;;; system.)  It's a little hard to do this and the issue doesn't
;;; come up that often, but it's worth remembering that there is
;;; an issue here.
(objc:defmethod (#/dealloc :void) ((self my-recorder-controller))
  (with-slots (m-capture-session
               m-capture-video-device-input
               m-capture-audio-device-input
               m-capture-movie-file-output) self
    (#/release m-capture-session)
    (#/release m-capture-video-device-input)
    (#/release m-capture-audio-device-input)
    (#/release m-capture-movie-file-output)
    (call-next-method)))

;;; This is an "action" method (specified in the nib file) that's
;;; invoked whenever the "start" button is pressed.  "action" methods
;;; recieve the object that invoked the method as a "sender" argument.
;;; (In this case, the "sender" is the "start" button.)  We don't
;;; care who sent the message and might ordinarily declare "sender"
;;; to be ignored.  It's hard to know when the bridge might cause
;;; objc:defmethod to expand into code that includes incidental
;;; references to an argument, so it's generally best to say that
;;; something that we don't use is "ignorable": we don't intend to
;;; reference the variable, but don't really care if there are
;;; incidental references to it in the macroexpansion of OBJC:DEFMETHOD.

(objc:defmethod (#/startRecording: :void) ((self my-recorder-controller) sender)
  (declare (ignorable sender))
  (#/recordToOutputFileURL: (slot-value self 'm-capture-movie-file-output)
                            (#/fileURLWithPath: ns:ns-url
                                                #@"/Users/Shared/My Recorded Movie.mov")))

;;; Likewise, another action method here.
(objc:defmethod (#/stopRecording: :void) ((self my-recorder-controller) sender)
  (declare (ignorable sender))
  (#/recordToOutputFileURL: (slot-value self 'm-capture-movie-file-output)
                            +null-ptr+))

;;; This message is sent to us because we're the delegate object of
;;; our output-capture object
(objc:defmethod
    (#/captureOutput:didFinishRecordingToOutputFileAtURL:forConnections:dueToError: :void)
    ((self my-recorder-controller) captureoutput output-file-url connections error)
  (declare (ignorable captureoutput connections error))
  (#/openURL: (#/sharedWorkspace ns:ns-workspace) output-file-url))


;;; That's the end of the transliterated code.  Here's a little function
;;; that runs an "Open" panel to allow the selection of a nib file, then
;;; tries to use a standard NSWindowController object to create and
;;; show a window using that nib file.


(defun open-window-using-selected-nib (&optional (prompt "Pick a nib file.  Any nib file"))
  ;;; There are a bunch of issues that make it easier to do all of the
  ;;; work on the main Cocoa event thread, which is the value of
  ;;; GUI:::*COCOA-EVENT-PROCESS*.  
  (process-interrupt
   gui::*cocoa-event-process*
   (lambda ()
     (let* ((panel (#/openPanel ns:ns-open-panel)))
       ;; CCL::%MAKE-NSSTRING should probably be moved to some
       ;; other package and exported from there.
       (#/setTitle: panel (ccl::%make-nsstring prompt))
       (#/setAllowsMultipleSelection: panel nil)
       (let* ((types (#/arrayWithObject: ns:ns-array #@"nib"))
              (button (#/runModalForTypes: panel types)))
         (when (eql button #$NSOKButton)
           (let* ((filenames (#/filenames panel)))
             (when (eql 1 (#/count filenames))
               (let* ((wc (make-instance 'ns:ns-window-controller
                                         :with-window-nib-path
                                         (#/objectAtIndex: filenames 0)
                                         :owner (#/sharedApplication ns:ns-application))))
                 (unless (%null-ptr-p wc)
                   (#/showWindow: wc +null-ptr+)))))))))))

                       

           
                                                  
