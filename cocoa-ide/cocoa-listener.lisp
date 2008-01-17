;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates

(in-package "GUI")

(def-cocoa-default *listener-input-font* :font #'(lambda ()
						   (#/fontWithName:size:
						    ns:ns-font
						    #@"Monaco" 10.0))
		   "Default font for listener input")
(def-cocoa-default *listener-output-font* :font #'(lambda ()
						    (#/fontWithName:size:
						     ns:ns-font
						     #@"Monaco" 10.0))
		   "Default font for listener output")

(def-cocoa-default *listener-rows* :int 16 "Initial height of listener windows, in characters")
(def-cocoa-default *listener-columns* :int 80 "Initial height of listener windows, in characters")

(def-cocoa-default hi::*listener-output-style* :int 1 "Text style index for listener output")

(def-cocoa-default hi::*listener-input-style* :int 0 "Text style index for listener output")

(def-cocoa-default *listener-background-color* :color '(1.0 1.0 1.0 1.0) "Listener default background color")

(def-cocoa-default *read-only-listener* :bool t "Do not allow editing old listener output")

;;; Setup the server end of a pty pair.
(defun setup-server-pty (pty)
  (set-tty-raw pty)
  pty)

;;; Setup the client end of a pty pair.
(defun setup-client-pty (pty)
  ;; Since the same (Unix) process will be reading from and writing
  ;; to the pty, it's critical that we make the pty non-blocking.
  ;; Has this been true for the last few years (native threads) ?
  ;(fd-set-flag pty #$O_NONBLOCK)
  (set-tty-raw pty)
  #+no
  (disable-tty-local-modes pty (logior #$ECHO #$ECHOCTL #$ISIG))
  #+no
  (disable-tty-output-modes pty #$ONLCR)  
  pty)


(defloadvar *cocoa-listener-count* 0)

(defclass cocoa-listener-process (process)
    ((input-stream :reader cocoa-listener-process-input-stream)
     (output-stream :reader cocoa-listener-process-output-stream)
     (input-peer-stream :reader cocoa-listener-process-input-peer-stream)
     (backtrace-contexts :initform nil
                         :accessor cocoa-listener-process-backtrace-contexts)
     (window :reader cocoa-listener-process-window)
     (buffer :initform nil :reader cocoa-listener-process-buffer)))
  

(defun new-cocoa-listener-process (procname input-fd output-fd peer-fd window buffer)
  (let* ((input-stream (ccl::make-selection-input-stream
                        input-fd
                        :peer-fd peer-fd
                        :elements-per-buffer (#_fpathconf
                                              input-fd
                                              #$_PC_MAX_INPUT)
                        :encoding :utf-8))
         (output-stream (ccl::make-fd-stream output-fd :direction :output
					     :sharing :lock
					     :elements-per-buffer
					     (#_fpathconf
					      output-fd
					      #$_PC_MAX_INPUT)
					     :encoding :utf-8))
         (peer-stream (ccl::make-fd-stream peer-fd :direction :output
					   :sharing :lock
					   :elements-per-buffer
					   (#_fpathconf
					    peer-fd
					    #$_PC_MAX_INPUT)
					   :encoding :utf-8))
         (proc
          (ccl::make-mcl-listener-process 
           procname
           input-stream
           output-stream
           #'(lambda ()
               (let* ((buf (find *current-process* hi:*buffer-list*
                                 :key #'hi::buffer-process))
                      (doc (if buf (hi::buffer-document buf))))
                 (when doc
                   (setf (hi::buffer-process buf) nil)
                   (#/performSelectorOnMainThread:withObject:waitUntilDone:
                    doc
                    (@selector #/close)
                    +null-ptr+
                    nil))))
           :initial-function
           #'(lambda ()
               (setq ccl::*listener-autorelease-pool* (create-autorelease-pool))
               (ccl::listener-function))
           :class 'cocoa-listener-process)))
    (setf (slot-value proc 'input-stream) input-stream)
    (setf (slot-value proc 'output-stream) output-stream)
    (setf (slot-value proc 'input-peer-stream) peer-stream)
    (setf (slot-value proc 'window) window)
    (setf (slot-value proc 'buffer) buffer)
    proc))
         

(defclass hemlock-listener-frame (hemlock-frame)
    ()
  (:metaclass ns:+ns-object))
(declaim (special hemlock-listener-frame))


(defclass hemlock-listener-window-controller (hemlock-editor-window-controller)
    ((filehandle :foreign-type :id)	;Filehandle for I/O
     (clientfd :foreign-type :int)	;Client (listener)'s side of pty
     (nextra :foreign-type :int)        ;count of untranslated bytes remaining
     (translatebuf :foreign-type :address) ;buffer for utf8 translation
     (bufsize :foreign-type :int)       ;size of translatebuf
     )
  (:metaclass ns:+ns-object)
  )
(declaim (special hemlock-listener-window-controller))

;;; Listener documents are never (or always) ediited.  Don't cause their
;;; close boxes to be highlighted.
(objc:defmethod (#/setDocumentEdited: :void)
    ((self hemlock-listener-window-controller) (edited :<BOOL>))
  (declare (ignorable edited)))
 

(objc:defmethod #/initWithWindow: ((self hemlock-listener-window-controller) w)
  (let* ((new (call-next-method w)))
    (unless (%null-ptr-p new)
      (multiple-value-bind (server client) (ignore-errors (open-pty-pair))
	(when server
	  (let* ((fh (make-instance
		      'ns:ns-file-handle
		      :with-file-descriptor (setup-server-pty server)
		      :close-on-dealloc t)))
	    (setf (slot-value new 'filehandle) fh)
	    (setf (slot-value new 'clientfd) (setup-client-pty client))
            (let* ((bufsize #$BUFSIZ)
                   (buffer (#_malloc bufsize)))
              (setf (slot-value new 'translatebuf) buffer
                    (slot-value new 'bufsize) bufsize
                    (slot-value new 'nextra) 0))
            (#/addObserver:selector:name:object:
             (#/defaultCenter ns:ns-notification-center)
             new
             (@selector #/gotData:)
             #&NSFileHandleReadCompletionNotification
             fh)
            (#/readInBackgroundAndNotify fh)))))
    new))

(objc:defmethod (#/gotData: :void) ((self hemlock-listener-window-controller)
                                    notification)
  (with-slots (filehandle nextra translatebuf bufsize) self
    (let* ((data (#/objectForKey: (#/userInfo notification)
                                  #&NSFileHandleNotificationDataItem))
	   (document (#/document self))
           (encoding (load-time-value (get-character-encoding :utf-8)))
	   (data-length (#/length data))
	   (buffer (hemlock-document-buffer document))
           (n nextra)
           (cursize bufsize)
           (need (+ n data-length))
           (xlate translatebuf)
	   (fh filehandle))
      (when (> need cursize)
        (let* ((new (#_malloc need)))
          (dotimes (i n) (setf (%get-unsigned-byte new i)
                               (%get-unsigned-byte xlate i)))
          (#_free xlate)
          (setq xlate new translatebuf new bufsize need)))
      #+debug (#_NSLog #@"got %d bytes of data" :int data-length)
      (with-macptrs ((target (%inc-ptr xlate n)))
        (#/getBytes:range: data target (ns:make-ns-range 0 data-length)))
      (let* ((total (+ n data-length)))
        (multiple-value-bind (nchars noctets-used)
            (funcall (ccl::character-encoding-length-of-memory-encoding-function encoding)
                     xlate
                     total
                     0)
          (let* ((string (make-string nchars)))
            (funcall (ccl::character-encoding-memory-decode-function encoding)
                     xlate
                     noctets-used
                     0
                     string)
            (unless (zerop (setq n (- total noctets-used)))
              ;; By definition, the number of untranslated octets
              ;; can't be more than 3.
              (dotimes (i n)
                (setf (%get-unsigned-byte xlate i)
                      (%get-unsigned-byte xlate (+ noctets-used i)))))
            (setq nextra n)
            (hi::enqueue-buffer-operation
             buffer
             #'(lambda ()
                 (unwind-protect
                      (progn
                        (hi::buffer-document-begin-editing buffer)
                        (hemlock::append-buffer-output buffer string))
                   (hi::buffer-document-end-editing buffer))))
            (#/readInBackgroundAndNotify fh)))))))
	     


(objc:defmethod (#/dealloc :void) ((self hemlock-listener-window-controller))
  (#/removeObserver: (#/defaultCenter ns:ns-notification-center) self)
  (call-next-method))

(objc:defmethod #/windowTitleForDocumentDisplayName: ((self hemlock-listener-window-controller) name)
  (let* ((doc (#/document self)))
    (if (or (%null-ptr-p doc)
            (not (%null-ptr-p (#/fileURL doc))))
      (call-next-method name)
      (let* ((buffer (hemlock-document-buffer doc))
             (bufname (if buffer (hi::buffer-name buffer))))
        (if bufname
          (%make-nsstring bufname)
          (call-next-method name))))))


;;; The HemlockListenerDocument class.


(defclass hemlock-listener-document (hemlock-editor-document)
    ()
  (:metaclass ns:+ns-object))
(declaim (special hemlock-listener-document))

(defmethod update-buffer-package ((doc hemlock-listener-document) buffer)
  (declare (ignore buffer)))

(defmethod hi::document-encoding-name ((doc hemlock-listener-document))
  "UTF-8")

(defmethod user-input-style ((doc hemlock-listener-document))
  hi::*listener-input-style*)
  
(defmethod textview-background-color ((doc hemlock-listener-document))
  *listener-background-color*)


(defun hemlock::listener-document-send-string (document string)
  (let* ((buffer (hemlock-document-buffer document))
         (process (if buffer (hi::buffer-process buffer))))
    (if process
      (hi::send-string-to-listener-process process string))))


(objc:defmethod #/topListener ((self +hemlock-listener-document))
  (let* ((all-documents (#/orderedDocuments *NSApp*)))
    (dotimes (i (#/count all-documents) +null-ptr+)
      (let* ((doc (#/objectAtIndex: all-documents i)))
	(when (eql (#/class doc) self)
	  (return doc))))))

(defun symbol-value-in-top-listener-process (symbol)
  (let* ((listenerdoc (#/topListener hemlock-listener-document))
	 (buffer (unless (%null-ptr-p listenerdoc)
		   (hemlock-document-buffer listenerdoc)))
	 (process (if buffer (hi::buffer-process buffer))))
     (if process
       (ignore-errors (symbol-value-in-process symbol process))
       (values nil t))))
  
(defun hi::top-listener-output-stream ()
  (let* ((doc (#/topListener hemlock-listener-document)))
    (unless (%null-ptr-p doc)
      (let* ((buffer (hemlock-document-buffer doc))
             (process (if buffer (hi::buffer-process buffer))))
        (when (typep process 'cocoa-listener-process)
          (cocoa-listener-process-output-stream process))))))



(objc:defmethod (#/isDocumentEdited :<BOOL>) ((self hemlock-listener-document))
  nil)



(objc:defmethod #/init ((self hemlock-listener-document))
  (let* ((doc (call-next-method)))
    (unless (%null-ptr-p doc)
      (let* ((listener-name (if (eql 1 (incf *cocoa-listener-count*))
			    "Listener"
			    (format nil
				    "Listener-~d" *cocoa-listener-count*)))
	     (buffer (hemlock-document-buffer doc)))
	(setf (hi::buffer-pathname buffer) nil
	      (hi::buffer-minor-mode buffer "Listener") t
	      (hi::buffer-name buffer) listener-name)
        (hi::sub-set-buffer-modeline-fields buffer hemlock::*listener-modeline-fields*)))
    doc))

(def-cocoa-default *initial-listener-x-pos* :float -100.0f0 "X position of upper-left corner of initial listener")

(def-cocoa-default *initial-listener-y-pos* :float 100.0f0 "Y position of upper-left corner of initial listener")

(defloadvar *next-listener-x-pos* nil) ; set after defaults initialized
(defloadvar *next-listener-y-pos* nil) ; likewise

(objc:defmethod (#/close :void) ((self hemlock-listener-document))
  (if (zerop (decf *cocoa-listener-count*))
    (setq *next-listener-x-pos* nil
          *next-listener-y-pos* nil))
  (call-next-method))

(objc:defmethod (#/makeWindowControllers :void) ((self hemlock-listener-document))
  (let* ((textstorage (slot-value self 'textstorage))
         (window (%hemlock-frame-for-textstorage
                  hemlock-listener-frame
                  textstorage
                  *listener-columns*
                  *listener-rows*
                  t
                  (textview-background-color self)
                  (user-input-style self)))
	 (listener-styles (#/arrayWithObjects: ns:ns-mutable-array
					       (rme-create-text-attributes
						:font *listener-input-font*)
					       (rme-create-text-attributes
						:font *listener-output-font*)
					       +null-ptr+))
	 (controller (make-instance
		      'hemlock-listener-window-controller
		      :with-window window))
	 (listener-name (hi::buffer-name (hemlock-document-buffer self))))
    (with-slots (styles) textstorage
      ;; We probably should be more disciplined about
      ;; Cocoa memory management.  Having retain/release in
      ;; random places all over the code is going to get
      ;; unwieldy.
      (#/release styles)
      (setf styles (#/retain listener-styles)))
    ;; Disabling background layout on listeners is an attempt to work
    ;; around a bug.  The bug's probably gone ...
    (let* ((layout-managers (#/layoutManagers textstorage)))
      (dotimes (i (#/count layout-managers))
        (let* ((layout (#/objectAtIndex: layout-managers i)))
          (#/setBackgroundLayoutEnabled: layout nil))))
    (#/setDelegate: (text-pane-text-view (slot-value window 'pane)) self)    
    (#/addWindowController: self controller)
    (#/release controller)
    (ns:with-ns-point (current-point
                       (or *next-listener-x-pos*
                           (x-pos-for-window window *initial-listener-x-pos*))
                       (or *next-listener-y-pos*
                           (y-pos-for-window window *initial-listener-y-pos*)))
      (let* ((new-point (#/cascadeTopLeftFromPoint: window current-point)))
        (setf *next-listener-x-pos* (ns:ns-point-x new-point)
              *next-listener-y-pos* (ns:ns-point-y new-point))))
    (setf (hi::buffer-process (hemlock-document-buffer self))
	  (let* ((tty (slot-value controller 'clientfd))
		 (peer-tty (#/fileDescriptor (slot-value controller 'filehandle))))
	    (new-cocoa-listener-process listener-name tty tty peer-tty window (hemlock-document-buffer self))))
    controller))

(objc:defmethod (#/textView:shouldChangeTextInRange:replacementString: :<BOOL>)
    ((self hemlock-listener-document)
     tv
     (range :<NSR>ange)
     string)
  (declare (ignore tv string))
  (let* ((range-start (ns:ns-range-location range))
         (range-end (+ range-start (ns:ns-range-length range)))
         (buffer (hemlock-document-buffer self))
         (protected-region (hi::buffer-protected-region buffer)))
    (if protected-region
      (let* ((prot-start (mark-absolute-position (hi::region-start protected-region)))
             (prot-end (mark-absolute-position (hi::region-end protected-region))))
        (not (or (and (>= range-start prot-start)
                      (< range-start prot-end))
                 (and (>= range-end prot-start)
                      (< range-end prot-end)))))
      t)))
    
    
;;; Action methods
(objc:defmethod (#/interrupt: :void) ((self hemlock-listener-document) sender)
  (declare (ignore sender))
  (let* ((buffer (hemlock-document-buffer self))
         (process (if buffer (hi::buffer-process buffer))))
    (when (typep process 'cocoa-listener-process)
      (ccl::force-break-in-listener process))))



(objc:defmethod (#/exitBreak: :void) ((self hemlock-listener-document) sender)
  (declare (ignore sender))
  (let* ((buffer (hemlock-document-buffer self))
         (process (if buffer (hi::buffer-process buffer))))
    (when (typep process 'cocoa-listener-process)
      (process-interrupt process #'abort-break))))

(defmethod listener-backtrace-context ((proc cocoa-listener-process))
  (car (cocoa-listener-process-backtrace-contexts proc)))

(objc:defmethod (#/backtrace: :void) ((self hemlock-listener-document) sender)
  (let* ((buffer (hemlock-document-buffer self))
         (process (if buffer (hi::buffer-process buffer))))
    (when (typep process 'cocoa-listener-process)
      (let* ((context (listener-backtrace-context process)))
        (when context
          (#/showWindow: (backtrace-controller-for-context context) sender))))))

(defun restarts-controller-for-context (context)
  (or (car (ccl::bt.restarts context))
      (setf (car (ccl::bt.restarts context))
            (let* ((tcr (ccl::bt.tcr context))
                   (tsp-range (inspector::make-tsp-stack-range tcr context))
                   (vsp-range (inspector::make-vsp-stack-range tcr context))
                   (csp-range (inspector::make-csp-stack-range tcr context))
                   (process (context-process context)))
              (make-instance 'sequence-window-controller
                             :sequence (cdr (ccl::bt.restarts context))
                             :result-callback #'(lambda (r)
                                                  (process-interrupt
                                                   process
                                                   #'invoke-restart-interactively
                                                   r))
                             :display #'(lambda (item stream)
                                          (let* ((ccl::*aux-vsp-ranges* vsp-range)
                                                 (ccl::*aux-tsp-ranges* tsp-range)
                                                 (ccl::*aux-csp-ranges* csp-range))
                                          (princ item stream)))
                             :title (format nil "Restarts for ~a(~d), break level ~d"
                                            (process-name process)
                                            (process-serial-number process)
                                            (ccl::bt.break-level context)))))))
                            
(objc:defmethod (#/restarts: :void) ((self hemlock-listener-document) sender)
  (let* ((buffer (hemlock-document-buffer self))
         (process (if buffer (hi::buffer-process buffer))))
    (when (typep process 'cocoa-listener-process)
      (let* ((context (listener-backtrace-context process)))
        (when context
          (#/showWindow: (restarts-controller-for-context context) sender))))))

(objc:defmethod (#/continue: :void) ((self hemlock-listener-document) sender)
  (declare (ignore sender))
  (let* ((buffer (hemlock-document-buffer self))
         (process (if buffer (hi::buffer-process buffer))))
    (when (typep process 'cocoa-listener-process)
      (let* ((context (listener-backtrace-context process)))
        (when context
          (process-interrupt process #'invoke-restart-interactively 'continue))))))






;;; Menu item action validation.  It'd be nice if we could distribute this a
;;; bit better, so that this method didn't have to change whenever a new
;;; action was implemented in this class.  For now, we have to do so.

(defmethod document-validate-menu-item ((doc hemlock-listener-document) item)
  ;; Return two values: the first is true if the second is definitive.
  ;; So far, all actions demand that there be an underlying process, so
  ;; check for that first.
  (let* ((buffer (hemlock-document-buffer doc))
         (process (if buffer (hi::buffer-process buffer))))
    (if (typep process 'cocoa-listener-process)
      (let* ((action (#/action item)))
        (cond
          ((or (eql action (@selector #/revertDocumentToSaved:))
	       (eql action (@selector #/saveDocument:))
	       (eql action (@selector #/saveDocumentAs:)))
           (values t nil))
          ((eql action (@selector #/interrupt:)) (values t t))
          ((eql action (@selector #/continue:))
           (let* ((context (listener-backtrace-context process)))
             (values
              t
              (and context
                   (find 'continue (cdr (ccl::bt.restarts context))
                         :key #'restart-name)))))
          ((or (eql action (@selector #/backtrace:))
               (eql action (@selector #/exitBreak:))
               (eql action (@selector #/restarts:)))
           (values t
                   (not (null (listener-backtrace-context process)))))))
      (values nil nil))))

(objc:defmethod (#/validateMenuItem: :<BOOL>)
    ((self hemlock-listener-document) item)
  (multiple-value-bind (have-opinion opinion)
      (document-validate-menu-item self item)
    (if have-opinion
      opinion
      (call-next-method item))))

(defun shortest-package-name (package)
  (let* ((name (package-name package))
         (len (length name)))
    (dolist (nick (package-nicknames package) name)
      (let* ((nicklen (length nick)))
        (if (< nicklen len)
          (setq name nick len nicklen))))))

(defmethod ui-object-note-package ((app ns:ns-application) package)
  (with-autorelease-pool
      (process-interrupt *cocoa-event-process*
			 #'(lambda (proc name)
			     (dolist (buf hi::*buffer-list*)
			       (when (eq proc (hi::buffer-process buf))
				 (setf (hi::variable-value 'hemlock::current-package :buffer buf) name))))
			 *current-process*
			 (shortest-package-name package))))

;;; This is basically used to provide INPUT to the listener process, by
;;; writing to an fd which is conntected to that process's standard
;;; input.
(defmethod hi::send-string-to-listener-process ((process cocoa-listener-process)
                                                string &key path package)
  (let* ((stream (cocoa-listener-process-input-peer-stream process)))
    (labels ((out-raw-char (ch)
               (write-char ch stream))
             (out-ch (ch)
               (when (or (eql ch #\^v)
                         (eql ch #\^p)
                         (eql ch #\newline)
                         (eql ch #\^q)
                         (eql ch #\^d))
                 (out-raw-char #\^q))
               (out-raw-char ch))
             (out-string (s)
               (dotimes (i (length s))
                 (out-ch (char s i)))))
      (out-raw-char #\^p)
      (when package (out-string package))
      (out-raw-char #\newline)
      (out-raw-char #\^v)
      (when path (out-string path))
      (out-raw-char #\newline)
      (out-string string)
      (out-raw-char #\^d)
      (force-output stream))))


(defun hemlock::evaluate-input-selection (selection)
  (ccl::application-ui-operation *application* :eval-selection selection))
  
(defmethod ui-object-choose-listener-for-selection ((app ns:ns-application)
						    selection)
  (declare (ignore selection))
  (#/performSelectorOnMainThread:withObject:waitUntilDone:
   (#/delegate *NSApp*) (@selector #/ensureListener:) +null-ptr+ #$YES)
  (let* ((top-listener-document (#/topListener hemlock-listener-document)))
    (if top-listener-document
      (let* ((buffer (hemlock-document-buffer top-listener-document)))
	(if buffer
	  (let* ((proc (hi::buffer-process buffer)))
	    (if (typep proc 'cocoa-listener-process)
	      proc)))))))

(defmethod ui-object-eval-selection ((app ns:ns-application)
				     selection)
  (let* ((target-listener (ui-object-choose-listener-for-selection
			   app selection)))
    (if (typep target-listener 'cocoa-listener-process)
        (destructuring-bind (package path string) selection
        (hi::send-string-to-listener-process target-listener string :package package :path path)))))

(defmethod ui-object-load-buffer ((app ns:ns-application) selection)
  (let* ((target-listener (ui-object-choose-listener-for-selection app nil)))
    (if (typep target-listener 'cocoa-listener-process)
        (destructuring-bind (package path) selection
          (let ((string (format nil "(load ~S)" path)))
            (hi::send-string-to-listener-process target-listener string :package package :path path))))))

(defmethod ui-object-compile-buffer ((app ns:ns-application) selection)
  (let* ((target-listener (ui-object-choose-listener-for-selection app nil)))
    (if (typep target-listener 'cocoa-listener-process)
        (destructuring-bind (package path) selection
          (let ((string (format nil "(compile-file ~S)" path)))
            (hi::send-string-to-listener-process target-listener string :package package :path path))))))

(defmethod ui-object-compile-and-load-buffer ((app ns:ns-application) selection)
  (let* ((target-listener (ui-object-choose-listener-for-selection app nil)))
    (if (typep target-listener 'cocoa-listener-process)
        (destructuring-bind (package path) selection
          (let ((string (format nil "(progn (compile-file ~S)(load ~S))" 
                                path
                                (make-pathname :directory (pathname-directory path)
                                               :name (pathname-name path)
                                               :type (pathname-type path)))))
            (hi::send-string-to-listener-process target-listener string :package package :path path))))))

       
  
