; -*- Mode: Lisp; Package: GUI -*-
;;;
;;; Copyright 2016 Clozure Associates
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

(defmethod list-from-ns-array (thing) (error "~S is not an instance of NS:NS-ARRAY" thing))
(defmethod list-from-ns-array ((nsa ns:ns-array))
  (let ((result (list))
        (c (#/count nsa)))
    (dotimes (i c) (setf result (push (#/objectAtIndex: nsa i) result)))
    (reverse result)))

(defclass key-select-table-view (ns:ns-table-view)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/keyDown: :void) ((self key-select-table-view) event)
  (let* ((code (#/keyCode event)))
    (if (and (>= (#/selectedRow self) 0)
             (= code 36)) ; return key
      (#/sendAction:to:from: *NSApp* (#/doubleAction self) (#/target self) self)
      (call-next-method event))))

(defclass sequence-window-controller (ns:ns-window-controller)
    ((table-view :foreign-type :id :reader sequence-window-controller-table-view)
     (sequence :initform nil :initarg :sequence :type sequence :reader sequence-window-controller-sequence)
     (result-callback :initarg :result-callback)
     (fontsize :initarg :fontsize :initform 12 :reader fontsize)
     (display :initform #'(lambda (item stream) (prin1 item stream)) :initarg :display)
     (title :initform "Sequence dialog" :initarg :title)
     (before-close-function :initarg :before-close-function :initform nil))
  (:metaclass ns:+ns-object))


(objc:defmethod #/init ((self sequence-window-controller))
  (call-next-method)
  (let* ((w (new-cocoa-window :activate nil))
         (contentview (#/contentView w))
         (contentframe (#/frame contentview))
         (scrollview (make-instance 'ns:ns-scroll-view :with-frame contentframe)))
    (#/setWindow: self w)
    (#/release w)
    (#/setDelegate: w self)
    (#/setWindowController: w self)
    (#/setHasVerticalScroller: scrollview t)
    (#/setHasHorizontalScroller: scrollview t)
    (#/setAutohidesScrollers: scrollview t)
    (#/setRulersVisible: scrollview nil)
    (#/setAutoresizingMask: scrollview (logior
                                        #$NSViewWidthSizable
                                        #$NSViewHeightSizable))
    (#/setAutoresizesSubviews: (#/contentView scrollview) t)
    (let* ((table-view (make-instance 'key-select-table-view)))
      (#/setDocumentView: scrollview table-view)
      (#/setRowHeight: table-view (gui::cgfloat (+ 2 (fontsize self))))
      (#/release table-view)
      #-cocotron
      (#/setColumnAutoresizingStyle: table-view #$NSTableViewUniformColumnAutoresizingStyle)
      (setf (slot-value self 'table-view) table-view)
      (let* ((column (make-instance 'ns:ns-table-column :with-identifier #@"")))
        (#/setEditable: column nil)
        (let* ((cell (#/dataCell column))
               (font (#/userFontOfSize: ns:ns-font (gui::cgfloat (fontsize self)))))
          (#/setFont: cell font))
        #-cocotron
	(#/setResizingMask: column #$NSTableColumnAutoresizingMask)
        (#/addTableColumn: table-view column)
	(#/release column))
      (#/setAutoresizingMask: table-view (logior
                                          #$NSViewWidthSizable
                                          #$NSViewHeightSizable))
      (#/sizeToFit table-view)
      (#/setDataSource: table-view self)
      (#/setTarget: table-view self)
      (#/setHeaderView: table-view +null-ptr+)
      (#/setUsesAlternatingRowBackgroundColors: table-view t)
      (#/setDoubleAction: table-view (@selector #/sequenceDoubleClick:))
      (#/addSubview: contentview scrollview)
      (#/release scrollview)
      self)))

(objc:defmethod (#/dealloc :void) ((self sequence-window-controller))
  (objc:remove-lisp-slots self)
  (call-next-method))

(objc:defmethod (#/windowWillClose: :void) ((self sequence-window-controller)
					    notification)
  (declare (ignore notification))
  (#/setDataSource: (slot-value self 'table-view) +null-ptr+)
  (with-slots (before-close-function) self
    (when (functionp before-close-function)
      (funcall before-close-function self)))
  (#/autorelease self))

(objc:defmethod (#/sequenceDoubleClick: :void)
    ((self sequence-window-controller) sender)
  (let* ((n (#/selectedRow sender)))
    (when (>= n 0)
      (with-slots (sequence result-callback) self
        (funcall result-callback (elt sequence n))))))

(objc:defmethod (#/numberOfRowsInTableView: :<NSI>nteger)
    ((self sequence-window-controller) view)
  (declare (ignore view))
  (length (slot-value self 'sequence)))


(objc:defmethod #/tableView:objectValueForTableColumn:row:
    ((self sequence-window-controller) view column (row :<NSI>nteger))
  (declare (ignore column view))
  (with-slots (display sequence) self
    (#/autorelease
     (%make-nsstring (with-output-to-string (s)
		       (funcall display (elt sequence row) s))))))

(defmethod initialize-instance :after ((self sequence-window-controller) &key &allow-other-keys)
  (let* ((window (#/window self)))
    (with-slots (title) self
      (when title (#/setTitle: window (%make-nsstring title))))
    (#/reloadData (sequence-window-controller-table-view self))
    (#/performSelectorOnMainThread:withObject:waitUntilDone:
     self
     (@selector #/showWindow:)
     +null-ptr+
     nil)))

;;; Looks like a "util" to me ...
(defun pathname-to-url (pathname)
  (make-instance 'ns:ns-url
                 :file-url-with-path
                 (%make-nsstring (native-translated-namestring pathname))))

(defun cgfloat (number)
  (float number ccl::+cgfloat-zero+))

(defun color-values-to-nscolor (red green blue &optional alpha)
  (#/retain (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color
                                                       (cgfloat red)
                                                       (cgfloat green)
                                                       (cgfloat blue)
                                                       (cgfloat (or alpha 1.0)))))

(defun map-windows (fn)
  (let ((win-arr (#/orderedWindows *NSApp*)))
    (dotimes (i (#/count win-arr))
      (funcall fn (#/objectAtIndex: win-arr i)))))

(defun windows ()
  (let* ((ret nil))
    (map-windows #'(lambda (w) (push w ret)))
    (nreverse ret)))

#+IGNORE
;Wow. Nothing like shooting a sparrow with a cannon.
(defun front-window ()
  (map-windows #'(lambda (win) (return-from front-window win))))

(defun front-window ()
  (#/keyWindow *NSApp*))

(defun target ()
  "Returns the second window in the list returned by (windows)."
  (let ((first? nil))
    (map-windows #'(lambda (win)
                     (if first?
                       (return-from target win)
                       (setf first? t))))))

(defun first-window-satisfying-predicate (pred)
  (block foo
    (map-windows #'(lambda (w) (when (funcall pred w)
                                 (return-from foo w))))))  

(defun first-window-with-controller-type (controller-type)
  (first-window-satisfying-predicate #'(lambda (w) (typep (#/windowController w) controller-type))))


(defun new-listener (&key (inhibit-greeting ccl::*inhibit-greeting*))
  (let ((wptr (execute-in-gui (lambda ()
                                (declare (special hemlock-listener-document))
                                ;; TODO: fix this.
                                (let ((old ccl::*inhibit-greeting*))
                                  (unwind-protect
                                      (progn
                                        (setq ccl::*inhibit-greeting* inhibit-greeting)
                                        (#/newListener: (#/delegate *NSApp*) (%null-ptr)))
                                    (setq ccl::*inhibit-greeting* old)))
                                (let ((doc (#/topListener hemlock-listener-document)))
                                  (unless (%null-ptr-p doc)
                                    (#/window (#/lastObject (#/windowControllers doc)))))))))
    (when wptr (hemlock-view wptr))))

(defun cocoa-close (object &optional wait-p)
  (if (eq *current-process* ccl::*initial-process*)
    (#/close object)
    (#/performSelectorOnMainThread:withObject:waitUntilDone:
     object
     (@selector #/close)
     +null-ptr+
     wait-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defvar *log-callback-errors* :backtrace)

(defun maybe-log-callback-error (condition)
  (when *log-callback-errors*
    ;; Put these in separate ignore-errors, so at least some of it can get thru
    (let ((emsg (ignore-errors (princ-to-string condition))))
      (ignore-errors (clear-output *debug-io*))
      (ignore-errors (format *debug-io* "~&Lisp error: ~s" (or emsg condition)))
      (when (eq *log-callback-errors* :backtrace)
        (let* ((err (nth-value 1 (ignore-errors (ccl:print-call-history :detailed-p t)))))
          (when err
            (ignore-errors (format *debug-io* "~&Error printing call history - "))
            (ignore-errors (print err *debug-io*))
            (ignore-errors (princ err *debug-io*))
            (ignore-errors (force-output *debug-io*))))))))

(defmacro with-callback-context (description &body body)
  (let ((saved-debug-io (gensym)))
    `(ccl::with-standard-abort-handling ,(format nil "Abort ~a" description)
       (let ((,saved-debug-io *debug-io*))
         (handler-bind ((error #'(lambda (condition)
                                   (let ((*debug-io* ,saved-debug-io))
                                     (maybe-log-callback-error condition)
                                     (abort)))))
           ,@body)))))


;;; previously used names
(defun execute-in-gui (thunk &key context)
  (declare (ignore context))
  (ccl::call-in-event-process thunk))

(defun queue-for-gui (thunk &key result-handler context at-start)
  (declare (ignore result-handler context at-start))
  (ccl::queue-for-event-process thunk))


(defmethod current-event-modifier-p (modifier-mask)
  (let* ((event (#/currentEvent *nsapp*))
         (modifiers (#/modifierFlags event)))
    (logtest modifier-mask modifiers)))

(defun current-event-command-key-p ()
  (current-event-modifier-p #$NSCommandKeyMask))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; debugging

(defun double-%-in (string)
  ;; Replace any % characters in string with %%, to keep them from
  ;; being treated as printf directives.
  (let* ((%pos (position #\% string)))
    (if %pos
      (concatenate 'string (subseq string 0 %pos) "%%" (double-%-in (subseq string (1+ %pos))))
      string)))

(defun log-debug (format-string &rest args)
  (let ((string (apply #'format nil format-string args)))
    (#_NSLog (ccl::%make-nsstring (double-%-in string)))))

(pushnew '(log-debug . 0) ccl::*format-arg-functions* :test #'equal)

(defun nslog-condition (c &optional (msg "Error in event loop: "))
  (let* ((rep (format nil "~a" c)))
    (with-cstrs ((str rep)
                 (msg-str msg))
      (with-nsstr (nsstr str (length rep))
        (with-nsstr (nsmsg msg-str (length msg))
         (#_NSLog #@"%@: %@" :address nsmsg :address nsstr))))))

(defun nsstring-for-lisp-condition (cond)
  (%make-nsstring (double-%-in (or (ignore-errors (princ-to-string cond))
                                   "#<error printing error message>"))))



(defun assume-cocoa-thread ()
  (assert (eq *current-process* ccl::*initial-process*)))

(defmethod assume-not-editing ((whatever t)))

;;; -----------------------------------------------------------------
;;; utility to display a Cocoa alert window
;;; -----------------------------------------------------------------
(defun alert-window (&key 
                     (title "Alert")
                     (message "Something happened.")
                     (default-button "Okay")
                     alternate-button
                     other-button)
  "Returns :DEFAULT, :ALTERNATE, or :OTHER depending on which button
was clicked."
  (let ((nstitle (%make-nsstring title))
        (nsmessage (%make-nsstring message))
        (ns-default-button (%make-nsstring default-button))
        (ns-alternate-button (or (and alternate-button (%make-nsstring alternate-button))
                                 +null-ptr+))
        (ns-other-button (or (and other-button (%make-nsstring other-button))
                             +null-ptr+))
        result)
    (setf result (#_NSRunAlertPanel nstitle nsmessage ns-default-button ns-alternate-button ns-other-button))
    (#/release nstitle)
    (#/release nsmessage)
    (#/release ns-default-button)
    (unless (eql ns-alternate-button +null-ptr+)
      (#/release ns-alternate-button))
    (unless (eql ns-other-button +null-ptr+)
      (#/release ns-other-button))
    (ecase result
      (#.#$NSAlertDefaultReturn :default)
      (#.#$NSAlertAlternateReturn :alternate)
      (#.#$NSAlertOtherReturn :other)
      (#.#$NSAlertErrorReturn (error "Error running alert panel")))))

;;; -----------------------------------------------------------------
;;; utility to display a Cocoa progress window
;;; -----------------------------------------------------------------

(defparameter *progress-window-controller* nil)

(defclass progress-window-controller (ns:ns-window-controller)
    ((progress-window :foreign-type :id :reader progress-window)
     (message-field :foreign-type :id :reader progress-window-message-field)
     (progress-bar :foreign-type :id :reader progress-window-progress-bar))
  (:metaclass ns:+ns-object))

(defun get-progress-window ()
  (unless *progress-window-controller*
    (setf *progress-window-controller* 
          (make-instance 'progress-window-controller))
    (#/initWithWindowNibName: *progress-window-controller* #@"ProgressWindow"))
  (unless (#/isWindowLoaded *progress-window-controller*)
    (#/loadWindow *progress-window-controller*))
  (let ((window (progress-window *progress-window-controller*)))
    (if (or (null window)
            (%null-ptr-p window))
        nil
        window)))

(defmacro with-modal-progress-dialog (title message &body body)
  `(let* ((nstitle (%make-nsstring ,title))
          (nsmessage (%make-nsstring ,message))
          (window (get-progress-window))
          (progress-bar (progress-window-progress-bar *progress-window-controller*))
          (message-field (progress-window-message-field *progress-window-controller*)))
     (unwind-protect 
          (if window
              (progn
                (#/setTitle: window nstitle)
                (#/setIndeterminate: progress-bar #$YES)
                (#/setUsesThreadedAnimation: progress-bar #$YES)
                (#/setStringValue: message-field nsmessage)
                (#/makeKeyAndOrderFront: window +null-ptr+)
                (let ((modal-session (#/beginModalSessionForWindow: ccl::*nsapp* window)))
                  (#/startAnimation: progress-bar +null-ptr+)
                  (let ((result (progn ,@body)))
                    (#/stopAnimation: progress-bar +null-ptr+)
                    (#/orderOut: window +null-ptr+)
                    (#/endModalSession: ccl::*nsapp* modal-session)
                    result)))
              (progn
                (alert-window :title "Failure"
                            :message "Unable to load the modal progress window")
                nil))
       (#/release nstitle)
       (#/release nsmessage))))

(defun post-tiger-p ()
  #+cocotron t
  #-cocotron 
  (rlet ((p :int))
    (#_Gestalt #$gestaltSystemVersion p)
    (>= (%get-long p) #x1050)))


;; This works even if an event loop is not running.

#+windows-target
(defun shift-key-now-p ()
  (logbitp 15 (#_GetAsyncKeyState #$VK_SHIFT)))

#+darwin-target
(defun shift-key-now-p ()
  (let* ((event (#_CGEventCreate +null-ptr+))
	 (flags (#_CGEventGetFlags event)))
    (prog1
	(logtest flags #$kCGEventFlagMaskShift)
      (#_CFRelease event))))

#+windows-target
(defun caps-lock-key-now-p ()
  (logbitp 15 (#_GetAsyncKeyState #$VK_CAPITAL)))

#+darwin-target
(defun caps-lock-key-now-p ()
  (let* ((event (#_CGEventCreate +null-ptr+))
	 (flags (#_CGEventGetFlags event)))
    (prog1
	(logtest flags #$kCGEventFlagMaskAlphaShift)
      (#_CFRelease event))))

;;; I would remove this, but I think that people use it...

(defclass abstract-ns-lisp-string (ns:ns-string)
    ()
  (:metaclass ns:+ns-object))

(defgeneric ns-lisp-string-string (abstract-ns-lisp-string)
  (:method ((self abstract-ns-lisp-string)) nil))

(objc:defmethod (#/length :<NSUI>nteger) ((self abstract-ns-lisp-string))
    (length (ns-lisp-string-string self)))

(objc:defmethod (#/characterAtIndex: :unichar) ((self abstract-ns-lisp-string) (index :<NSUI>nteger))
  (char-code (char (ns-lisp-string-string self) index)))

(defclass ns-lisp-string (abstract-ns-lisp-string)
  ((lisp-string :initarg :string :reader ns-lisp-string-string))
  (:metaclass ns:+ns-object))
