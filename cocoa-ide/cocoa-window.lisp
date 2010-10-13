;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2002-2007 Clozure Associates
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


(in-package "GUI")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-cocoa-default *default-font-name* :string "Courier" "Name of font to use in editor windows")
  (def-cocoa-default *default-font-size* :float 12.0f0 "Size of font to use in editor windows, as a positive SINGLE-FLOAT")
  (def-cocoa-default *tab-width* :int 8 "Width of editor tab stops, in characters"))

(defun init-cocoa-application ()
  (with-autorelease-pool
      (#/standardUserDefaults ns:ns-user-defaults)
      (let* ((bundle (open-main-bundle))
	     (dict (#/infoDictionary  bundle))
	     (classname (#/objectForKey: dict #@"NSPrincipalClass"))
	     (mainnibname (#/objectForKey: dict  #@"NSMainNibFile"))
	     (progname (#/objectForKey: dict #@"CFBundleName")))
	(if (%null-ptr-p classname)
	  (error "problems loading bundle: can't determine class name"))
	(if (%null-ptr-p mainnibname)
	  (error "problems loading bundle: can't determine main nib name"))
	(unless (%null-ptr-p progname)
          (#/setProcessName: (#/processInfo ns:ns-process-info) progname))
	(let* ((appclass (#_NSClassFromString classname))
	       (app (#/sharedApplication appclass)))
          (#/loadNibNamed:owner: ns:ns-bundle mainnibname  app)
	  app))))



#+apple-objc
(defun trace-dps-events (flag)
  (external-call "__DPSSetEventsTraced"
		 :unsigned-byte (if flag #$YES #$NO)
		 :void))

(defclass appkit-process (process)
    ((have-interactive-terminal-io :initform t)))

(defmethod event-loop-can-have-interactive-terminal-io ((process appkit-process))
  #+windows-target t
  #-windows-target (slot-value process 'have-interactive-terminal-io))

;;; Interrupt the AppKit event process, by enqueing an event (if the
;;; application event loop seems to be running.)  It's possible that
;;; the event loop will stop after the calling thread checks; in that
;;; case, the application's probably already in the process of
;;; exiting, and isn't that different from the case where asynchronous
;;; interrupts are used.
(defmethod process-interrupt ((process appkit-process) function &rest args)
  (if (eq process *current-process*)
    (apply function args)
    (if (and *NSApp* (#/isRunning *NSApp*))
      (queue-for-gui #'(lambda () (apply function args)) :at-start t)
      #+not-yet
      (let* ((invoked nil)
             (f (lambda ()
                  (unless invoked
                    (setq invoked t)
                    (apply function args)))))
        (queue-for-gui f :at-start t)
        (call-next-method process f))
      (call-next-method))))

(defparameter *debug-in-event-process* t)

(defparameter *event-process-reported-conditions* () "Things that we've already complained about on this event cycle.")

(defmethod ccl::process-debug-condition ((process appkit-process) condition frame-pointer)
  "Better than nothing.  Not much better."
  (when *debug-in-event-process*
    (let* ((c (if (typep condition 'ccl::ns-lisp-exception)
                (ccl::ns-lisp-exception-condition condition)
                condition)))
      (unless (or (not (typep c 'error)) (member c *event-process-reported-conditions*))
        (push c *event-process-reported-conditions*)
        (cond ((slot-value process 'have-interactive-terminal-io)
               (ccl::application-error ccl::*application* c frame-pointer))
              (t
               (catch 'need-a-catch-frame-for-backtrace
                 (let* ((*debug-in-event-process* nil)
                        (context
                         (ccl::new-backtrace-info nil
                                                  frame-pointer
                                                  (if ccl::*backtrace-contexts*
                                                      (or (ccl::child-frame
                                                           (ccl::bt.youngest
                                                            (car ccl::*backtrace-contexts*))
                                                           nil)
                                                          (ccl::last-frame-ptr))
                                                      (ccl::last-frame-ptr))
                                                  (ccl::%current-tcr)
                                                  condition
                                                  (ccl::%current-frame-ptr)
                                                  #+ppc-target ccl::*fake-stack-frames*
                                                  #+x86-target (ccl::%current-frame-ptr)
                                                  (ccl::db-link)
                                                  (1+ ccl::*break-level*)))
                        (ccl::*backtrace-contexts* (cons context ccl::*backtrace-contexts*)))  
                   (format t "~%~%*** Error in event process: ~a~%~%" condition)
                   (print-call-history :context context :detailed-p t :count 20
                                       :origin frame-pointer)
                   (format t "~%~%~%")
                   (force-output t)
                   ))))))))



(defvar *default-ns-application-proxy-class-name*
    "LispApplicationDelegate")


(defun enable-foreground ()
  #+apple-objc
  (rlet ((psn :<P>rocess<S>erial<N>umber))
    (#_GetCurrentProcess psn)
    (#_TransformProcessType psn #$kProcessTransformToForegroundApplication)
    (eql 0 (#_SetFrontProcess psn))))

#+nil
(objc:defmethod (#/showPreferences: :void) ((self lisp-application) sender)
  (declare (ignore sender))
  (#/show (#/sharedPanel lisp-preferences-panel)))

(objc:defmethod (#/toggleConsole: :void) ((self lisp-application) sender)
  (let* ((console (console self)))
    (unless (%null-ptr-p console)
      (mark-console-output-available console nil)
      (if (setf (console-window-hidden-by-user console) (#/isVisible console))
        (#/orderOut: console sender)
        (#/orderFront: console sender)))))

(objc:defmethod (#/validateMenuItem: :<BOOL>) ((self lisp-application)
                                               item)
  (let* ((action (#/action item)))
    (cond ((eql action (@selector #/toggleConsole:))
           (let* ((console (console self)))
             (unless (%null-ptr-p console)
               (if (#/isVisible console)
                 (#/setTitle: item #@"Hide System Console")
                 (#/setTitle: item #@"Show System Console"))
               t)))
          (t #+cocotron t #-cocotron (call-next-method item)))))

(defmethod ccl::process-exit-application ((process appkit-process) thunk)
  (when (eq process ccl::*initial-process*)
    (%set-toplevel thunk)
    (#/terminate: *NSApp* +null-ptr+)))

(defun run-event-loop ()
  (%set-toplevel nil)
  (change-class *cocoa-event-process* 'appkit-process)
  (event-loop))

(defun stop-event-loop ()
  (#/stop: *nsapp* +null-ptr+))

(defun event-loop (&optional end-test)
  (let* ((app *NSApp*)
         (thread ccl::*current-process*))
    (loop
      (if (event-loop-can-have-interactive-terminal-io thread)
        (with-simple-restart (abort "Process the next event")
          (#/run app))
        (let* ((ccl::*break-on-errors* nil))
          (handler-case (let* ((*event-process-reported-conditions* nil))
                          (if end-test
                            (#/run app)
                            #|(#/runMode:beforeDate: (#/currentRunLoop ns:ns-run-loop)
                                                     #&NSDefaultRunLoopMode
                                                     (#/distantFuture ns:ns-date))|#
                            (#/run app)))
            (error (c) (nslog-condition c)))))
      #+debug (log-debug "~&runMode exited, end-test: ~s isRunning ~s quitting: ~s" end-test (#/isRunning app) ccl::*quitting*)
      (when (or (and end-test (funcall end-test))
		(and ccl::*quitting* (not (#/isRunning app))))
	(return)))))

(defun start-cocoa-application (&key
				(application-proxy-class-name
				 *default-ns-application-proxy-class-name*))
  
  (flet ((cocoa-startup ()
	   ;; Start up a thread to run periodic tasks.
           (ccl::with-standard-initial-bindings
               (process-run-function "housekeeping" #'ccl::housekeeping-loop)
               (with-autorelease-pool
                   (enable-foreground)
                 (or *NSApp* (setq *NSApp* (init-cocoa-application)))
                 #-cocotron
                 (let* ((icon (#/imageNamed: ns:ns-image #@"NSApplicationIcon")))
                   (unless (%null-ptr-p icon)
                     (#/setApplicationIconImage: *NSApp* icon)))
                 (setf (ccl::application-ui-object *application*) *NSApp*)
                 (when application-proxy-class-name
                   (let* ((classptr (ccl::%objc-class-classptr
                                     (ccl::load-objc-class-descriptor application-proxy-class-name)))
                          (instance (#/init (#/alloc classptr))))
                     
                     (#/setDelegate: *NSApp* instance))))
               (run-event-loop))))
    (process-interrupt *cocoa-event-process* #'(lambda ()
                                                 (%set-toplevel 
                                                  #'cocoa-startup)
                                                 (toplevel)))))

(defparameter *font-attribute-names*
  '((:bold . #.#$NSBoldFontMask)
    (:italic . #.#$NSItalicFontMask)
    (:small-caps . #.#$NSSmallCapsFontMask)))


;;; The NSFont method #/isFixedPitch has returned random answers
;;; in many cases for the last few OSX releases.  Try to return
;;; a reasonable answer, by checking to see if the width of the
;;; advancement for the #\i glyph matches that of the advancement
;;; of the #\m glyph.

#-cocotron
(defun is-fixed-pitch-font (font)
  (= (ns:ns-size-width (#/advancementForGlyph: font (#/glyphWithName: font #@"i")))
     (ns:ns-size-width (#/advancementForGlyph: font (#/glyphWithName: font #@"m")))))

#+cocotron
(defun is-fixed-pitch-font (font)
  (#/isFixedPitch font))

;;; Try to find the specified font.  If it doesn't exist (or isn't
;;; fixed-pitch), try to find a fixed-pitch font of the indicated size.
(defun default-font (&key (name *default-font-name*)
			  (size *default-font-size*)
			  (attributes ()))
				
  (setq size (cgfloat size))
  (with-cstrs ((name name))
    (with-autorelease-pool
	(rletz ((matrix (:array :<CGF>loat 6)))
	  (setf (paref matrix (:* :<CGF>loat) 0) size
                (paref matrix (:* :<CGF>loat) 3) size)
          (let* ((fontname (#/stringWithCString: ns:ns-string name))
		 (font (#/fontWithName:matrix: ns:ns-font fontname matrix))
		 
		 (implemented-attributes ()))
	    (if (or (%null-ptr-p font)
		    (and 
		     (not (is-fixed-pitch-font font))))
	      (setq font (#/userFixedPitchFontOfSize: ns:ns-font size)))
	    (when attributes
	      (dolist (attr-name attributes)
		(let* ((pair (assoc attr-name *font-attribute-names*))
		       (newfont))
		  (when pair
		    (setq newfont
                          (#/convertFont:toHaveTrait:
                           (#/sharedFontManager ns:ns-font-manager) font (cdr pair)))
		    (unless (eql font newfont)
		      (setq font newfont)
		      (push attr-name implemented-attributes))))))
	    (values (#/retain font) implemented-attributes))))))


;;; Create a paragraph style, mostly so that we can set tabs reasonably.
(defun create-paragraph-style (font line-break-mode)
  (let* ((p (make-instance 'ns:ns-mutable-paragraph-style))
	 (charwidth (fround (nth-value 1 (size-of-char-in-font font)))))
    (#/setLineBreakMode: p
                         (ecase line-break-mode
                           (:char #$NSLineBreakByCharWrapping)
                           (:word #$NSLineBreakByWordWrapping)
                           ;; This doesn't seem to work too well.
                           ((nil) #$NSLineBreakByClipping)))
    ;; Clear existing tab stops.
    (#/setTabStops: p (#/array ns:ns-array))
    ;; And set the "default tab interval".
    (#/setDefaultTabInterval: p (cgfloat (* *tab-width* charwidth)))
    p))
    
(defun create-text-attributes (&key (font (default-font))
				    (line-break-mode :char)
				    (color nil)
                                    (obliqueness nil)
                                    (stroke-width nil))
  (let* ((dict (make-instance 'ns:ns-mutable-dictionary :with-capacity 5)))
    (#/setObject:forKey: dict (create-paragraph-style font line-break-mode)
			 #&NSParagraphStyleAttributeName)
    (#/setObject:forKey: dict font #&NSFontAttributeName)
    (when color
      (#/setObject:forKey: dict color #&NSForegroundColorAttributeName))
    (when stroke-width
      (#/setObject:forKey: dict (#/numberWithFloat: ns:ns-number stroke-width)
			   #&NSStrokeWidthAttributeName))
    (when obliqueness
      (#/setObject:forKey: dict (#/numberWithFloat: ns:ns-number obliqueness)
			   #&NSObliquenessAttributeName))
    dict))


(defun get-cocoa-window-flag (w flagname)
  (case flagname
    (:accepts-mouse-moved-events
     (#/acceptsMouseMovedEvents w))
    (:cursor-rects-enabled
     (#/areCursorRectsEnabled w))
    (:auto-display
     (#/isAutodisplay w))))



(defun (setf get-cocoa-window-flag) (value w flagname)
  (case flagname
    (:accepts-mouse-moved-events
     (#/setAcceptsMouseMovedEvents: w value))
    (:auto-display
     (#/setAutodisplay: w value))))



(defun activate-window (w)
  ;; Make w the "key" and frontmost window.  Make it visible, if need be.
  (#/makeKeyAndOrderFront: w nil))

(defun set-window-title (window title)
  (#/setTitle: window (if title
                        (if (typep title 'ns:ns-string)
                          title
                          (%make-nsstring title))
                        #@"") ))

(defmethod allocate-instance ((class ns:+ns-window)
                              &rest initargs
                              &key
                              (with-content-rect nil content-rect-p)
                              (style-mask 0 style-mask-p)
                              (x 200)
                              (y 200)
                              (width 500)
                              (height 200)
                              (closable t)
                              (iconifyable t)
                              (expandable t)
                              (metal nil)
                              (backing :buffered)
                              (defer t defer-p)
                              &allow-other-keys)
  (declare (ignore defer with-content-rect))
  (unless content-rect-p
    (setq initargs (cons :with-content-rect
                         (cons (ns:make-ns-rect x y width height)
                               initargs))))
  (unless (and style-mask-p (typep style-mask 'fixnum))
    (setq initargs (cons :style-mask
                         (cons (logior #$NSTitledWindowMask
                                       (if closable #$NSClosableWindowMask 0)
                                       (if iconifyable #$NSMiniaturizableWindowMask 0)
                                       (if expandable #$NSResizableWindowMask 0)
                                       (if metal #$NSTexturedBackgroundWindowMask 0))
                               initargs))))
  (unless (typep (getf initargs :backing) 'fixnum)
    (setq initargs
          (cons :backing
                (cons (ecase backing
                        ((t :retained) #$NSBackingStoreRetained)
                        ((nil :nonretained) #$NSBackingStoreNonretained)
                        (:buffered #$NSBackingStoreBuffered))
                      initargs))))
  (unless defer-p
    (setq initargs (cons :defer (cons t initargs))))
  (apply #'call-next-method class initargs))

(defmethod initialize-instance :after ((w ns:ns-window)
                                       &key
                                       (title nil)
                                       (x 200.0)
                                       (y 200.0)
                                       (height 200.0)
                                       (width 500.0)
                                       (closable t)
                                       (iconifyable t)
                                       (metal nil)
                                       (expandable t)
                                       (backing :buffered)
                                       (defer t)
                                       (accepts-mouse-moved-events nil)
                                       (auto-display t)
                                       (activate nil)
                                       &allow-other-keys)
  ;; Several of the keyword args we claim to accept are actually processed
  ;; by the ALLOCATE-INSTANCE method above and are ignored here.
  (declare (ignore x y width height closable iconifyable expandable metal
                   backing defer))
  (setf (get-cocoa-window-flag w :accepts-mouse-moved-events)
        accepts-mouse-moved-events
        (get-cocoa-window-flag w :auto-display)
        auto-display)
  ;;; Should maybe have a way of controlling this.
  (#/setBackgroundColor: w (#/whiteColor ns:ns-color))
  (when title
    (set-window-title w title))
  (when activate
    (activate-window w)))


(defmethod allocate-instance ((class ns:+ns-view)
                              &rest initargs
                              &key
                              (with-frame nil with-frame-p)
                              (x 0)
                              (y 0)
                              (width 0)
                              (height 0)
                              &allow-other-keys)
  (declare (ignorable with-frame))
  (unless with-frame-p
    (setq initargs (cons :with-frame
                         (cons (ns:make-ns-rect x y width height) initargs))))
  (apply #'call-next-method class initargs))


(defmethod initialize-instance :after ((view ns:ns-view)
                                       &key
                                       (horizontally-resizable nil hrp)
                                       (vertically-resizable nil vrp)
                                       (max-x-margin nil maxxp)
                                       (min-x-margin nil minxp)
                                       (max-y-margin nil maxyp)
                                       (min-y-margin nil minyp)
                                       (resizes-subviews t rsp)
                                       view-container
                                       &allow-other-keys)
  (let* ((mask (#/autoresizingMask view))
         (newmask mask))
    (when hrp
      (setq newmask (if horizontally-resizable
                      (logior newmask #$NSViewWidthSizable)
                      (logandc2 newmask #$NSViewWidthSizable))))
    (when vrp
      (setq newmask (if vertically-resizable
                      (logior newmask #$NSViewHeightSizable)
                      (logandc2 newmask #$NSViewHeightSizable))))
    (when minxp
      (setq newmask (if min-x-margin
                      (logior newmask #$NSViewMinXMargin)
                      (logandc2 newmask #$NSViewMinXMargin))))
    (when maxxp
      (setq newmask (if max-x-margin
                      (logior newmask #$NSViewMaxXMargin)
                      (logandc2 newmask #$NSViewMaxXMargin))))
    (when minyp
      (setq newmask (if min-y-margin
                      (logior newmask #$NSViewMinYMargin)
                      (logandc2 newmask #$NSViewMinYMargin))))
    (when maxyp
      (setq newmask (if max-y-margin
                      (logior newmask #$NSViewMaxYMargin)
                      (logandc2 newmask #$NSViewMaxYMargin))))
    (unless (eql mask newmask)
      (#/setAutoresizingMask: view newmask)))
  (when rsp
    (#/setAutoresizesSubviews: view resizes-subviews))
  (when view-container
    (install-view-in-container view view-container)))
                              

(defun new-cocoa-window (&key
                         (class (find-class 'ns:ns-window))
                         (title nil)
                         (x 200.0)
                         (y 200.0)
                         (height 200.0)
                         (width 500.0)
                         (closable t)
                         (iconifyable t)
                         (metal nil)
                         (expandable t)
                         (backing :buffered)
                         (defer t)
                         (accepts-mouse-moved-events nil)
                         (auto-display t)
                         (activate t))
  (make-instance class
                 :title title
                 :x x
                 :y y
                 :height height
                 :width width
                 :closable closable
                 :iconifyable iconifyable
                 :metal metal
                 :expandable expandable
                 :backing backing
                 :defer defer
                 :accepts-mouse-moved-events accepts-mouse-moved-events
                 :auto-display auto-display
                 :activate activate))

(defmethod view-window ((view ns:ns-view))
  (let* ((w (#/window view)))
    (unless (%null-ptr-p w)
      w)))

(defmethod install-view-in-container ((view ns:ns-view) (container ns:ns-view))
  (#/addSubview: container view))

(defmethod install-view-in-container ((view ns:ns-view) (container ns:ns-window))
  (#/addSubview: (#/contentView container) view))
