;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates

(in-package "GUI")

(defclass stack-descriptor ()
  ((context :initarg :context :reader stack-descriptor-context)
   (filter :initform nil :initarg :filter :reader stack-descriptor-filter)
   (interruptable-p :initform t :accessor stack-descriptor-interruptable-p)
   (segment-size :initform 50 :reader stack-descriptor-segment-size)
   (frame-count :initform -1 :reader stack-descriptor-frame-count)
   (frame-cache :initform (make-hash-table) :reader stack-descriptor-frame-cache)))

(defun make-stack-descriptor (context &rest keys)
  (apply #'make-instance 'stack-descriptor
         ;; For some reason backtrace context is an anonymous vector
         :context (require-type context 'simple-vector)
         keys))

(defmethod initialize-instance :after ((sd stack-descriptor) &key &allow-other-keys)
  (with-slots (frame-count) sd
    (setf frame-count (count-stack-descriptor-frames sd))))

(defmethod stack-descriptor-refresh ((sd stack-descriptor))
  (clrhash (stack-descriptor-frame-cache sd)))

(defmethod stack-descriptor-origin ((sd stack-descriptor))
  (ccl::bt.youngest (stack-descriptor-context sd)))

(defmethod stack-descriptor-process ((sd stack-descriptor))
  (ccl::tcr->process (ccl::bt.tcr (stack-descriptor-context sd))))

(defmethod stack-descriptor-condition ((sd stack-descriptor))
  (ccl::bt.break-condition (stack-descriptor-context sd)))

(defmethod map-stack-frames (sd function &optional start end)
  (ccl:map-call-frames function
                       :origin (stack-descriptor-origin sd)
                       :process (stack-descriptor-process sd)
                       :test (stack-descriptor-filter sd)
                       :start-frame-number (or start 0)
                       :count (- (or end most-positive-fixnum)
                                 (or start 0))))

(defmethod count-stack-descriptor-frames ((sd stack-descriptor))
  (let ((count 0))
    (map-stack-frames sd (lambda (fp context)
                           (declare (ignore fp context))
                           (incf count)))
    count))

;; Function must be side-effect free, it may be restarted or aborted.
(defun collect-stack-frames (sd function &optional start end)
  (let ((process (stack-descriptor-process sd)))
    ;; In general, it's best to run backtrace printing in the error process, since
    ;; printing often depends on the dynamic state (e.g. bound vars) at the point of
    ;; error.  However, if the erring process is wedged in some way, getting at it
    ;; from outside may be better than nothing.
    (if (or (not (stack-descriptor-interruptable-p sd))
            (eq process *current-process*))
      (let* ((results nil)
	     (*print-level* *backtrace-print-level*)
	     (*print-length* *backtrace-print-length*)
	     (*print-circle* (null *print-level*)))
        (map-stack-frames sd (lambda (fp context)
                               (push (funcall function fp context) results))
                          start end)
        (nreverse results))
      (let ((s (make-semaphore))
            (res :none))
        (process-interrupt process
                           (lambda ()
                             (ignore-errors (setq res (collect-stack-frames sd function start end)))
                             (signal-semaphore s)))
        (timed-wait-on-semaphore s 2) ;; give it 2 seconds before going to plan B...
        (if (eq res :none)
          (progn
            (setf (stack-descriptor-interruptable-p sd) nil)
            (collect-stack-frames sd function start end))
          res)))))

(defclass frame-descriptor ()
  ((data :initarg :data :reader frame-descriptor-data)
   (label :initarg :label :reader frame-descriptor-label)
   (values :initarg :values :reader frame-descriptor-values)))

(defun make-frame-descriptor (fp context)
  (let* ((args (ccl:frame-supplied-arguments fp context))
         (vars (ccl:frame-named-variables fp context))
         (lfun (ccl:frame-function fp context)))
    (make-instance 'frame-descriptor
      :data (cons fp context)
      :label (if lfun
               (with-output-to-string (stream)
                 (format stream "(~S" (or (ccl:function-name lfun) lfun))
                 (if (eq args (ccl::%unbound-marker))
                   (format stream " #<Unknown Arguments>")
                   (loop for arg in args
                     do (if (eq arg (ccl::%unbound-marker))
                          (format stream " #<Unavailable>")
                          (format stream " ~:['~;~]~s" (ccl::self-evaluating-p arg) arg))))
                 (format stream ")"))
               ":kernel")
      :values (if lfun
                (map 'vector
                     (lambda (var.val)
                       (destructuring-bind (var . val) var.val
                         (let ((label (format nil "~:[~s~;~a~]: ~s"
                                              (stringp var) var val)))
                           (cons label var.val))))
                     (cons `("Function" . ,lfun)
                           (and (not (eq vars (ccl::%unbound-marker))) vars)))
                ))))

(defmethod stack-descriptor-frame ((sd stack-descriptor) index)
  (let ((cache (stack-descriptor-frame-cache sd)))
    (or (gethash index cache)
        ;; get a bunch at once.
        (let* ((segment-size (stack-descriptor-segment-size sd))
               (start (- index (rem index segment-size)))
               (end (+ start segment-size))
               (frames (collect-stack-frames sd #'make-frame-descriptor start end)))
          (loop for n upfrom start as frame in frames do (setf (gethash n cache) frame))
          (gethash index cache)))))

(defun frame-descriptor-function (frame)
  (destructuring-bind (fp . context) (frame-descriptor-data frame)
    (ccl:frame-function fp context)))

;; Don't bother making first-class frame value descriptors = frame + index

(defun frame-descriptor-value-count (frame)
  (length (frame-descriptor-values frame)))

(defun frame-descriptor-value-label (frame index)
  (car (svref (frame-descriptor-values frame) index)))

(defun frame-descriptor-value (frame index)
  (destructuring-bind (var . val)
                      (cdr (svref (frame-descriptor-values frame) index))
    (values val var)))

(defun show-frame-source (frame)
  (multiple-value-bind (lfun pc) (frame-descriptor-function frame)
    (when lfun
      (let ((source (or (and pc (ccl:find-source-note-at-pc lfun pc))
                        (ccl:function-source-note lfun))))
        (if (source-note-p source)
          (hemlock-ext:execute-in-file-view
           (ccl:source-note-filename source)
           (lambda  ()
             (hemlock::move-to-source-note source)))
          (hemlock::edit-definition lfun))))))

(defun backtrace-frame-default-action (frame &optional index)
  (if index
    (inspect (frame-descriptor-value frame index))
    (show-frame-source frame)))

;; Cocoa layer

;; General utils, should be moved elsewhere
(defclass abstract-ns-lisp-string (ns:ns-string)
    ()
  (:metaclass ns:+ns-object))

(defgeneric ns-lisp-string-string (abstract-ns-lisp-string))

(objc:defmethod (#/length :<NSUI>nteger) ((self abstract-ns-lisp-string))
    (length (ns-lisp-string-string self)))

(objc:defmethod (#/characterAtIndex: :unichar) ((self abstract-ns-lisp-string) (index :<NSUI>nteger))
  (char-code (char (ns-lisp-string-string self) index)))

(defclass ns-lisp-string (abstract-ns-lisp-string)
  ((lisp-string :initarg :string :reader ns-lisp-string-string))
  (:metaclass ns:+ns-object))

(defclass frame-label (abstract-ns-lisp-string)
    ((frame-number  :foreign-type :int :accessor frame-label-number)
     (controller :foreign-type :id :reader frame-label-controller))
  (:metaclass ns:+ns-object))

(defmethod frame-label-descriptor ((self frame-label))
  (stack-descriptor-frame
    (backtrace-controller-stack-descriptor (frame-label-controller self))
    (frame-label-number self)))
  
(defmethod ns-lisp-string-string ((self frame-label))
  (frame-descriptor-label (frame-label-descriptor self)))

(objc:defmethod #/initWithFrameNumber:controller: ((self frame-label) (frame-number :int) controller)
  (let* ((obj (#/init self)))
    (unless (%null-ptr-p obj)
      (setf (slot-value obj 'frame-number) frame-number
            (slot-value obj 'controller) controller))
    obj))


(defclass item-label (abstract-ns-lisp-string)
    ((frame-label :foreign-type :id :accessor item-label-label)
     (index :foreign-type :int :accessor item-label-index))
  (:metaclass ns:+ns-object))

(defmethod ns-lisp-string-string ((self item-label))
  (frame-descriptor-value-label (frame-label-descriptor (item-label-label self))
                                (item-label-index self)))

(objc:defmethod #/initWithFrameLabel:index: ((self item-label) the-frame-label (index :int))
  (let* ((obj (#/init self)))
    (unless (%null-ptr-p obj)
      (setf (slot-value obj 'frame-label) the-frame-label
            (slot-value obj 'index) index))
    obj))

(defclass backtrace-window-controller (ns:ns-window-controller)
    ((context :initarg :context :reader backtrace-controller-context)
     (stack-descriptor :initform nil :reader backtrace-controller-stack-descriptor)
     (outline-view :foreign-type :id :reader backtrace-controller-outline-view)
     (message-field :foreign-type :id :accessor message-field)
     (action-menu :foreign-type :id :accessor action-menu)
     (action-popup-button :foreign-type :id :accessor action-popup-button)
     (contextual-menu :foreign-type :id :accessor contextual-menu))
  (:metaclass ns:+ns-object))

(defmethod backtrace-controller-process ((self backtrace-window-controller))
  (let ((context (backtrace-controller-context self)))
    (and context (ccl::tcr->process (ccl::bt.tcr context)))))

(defmethod backtrace-controller-break-level ((self backtrace-window-controller))
  (let ((context (backtrace-controller-context self)))
    (and context (ccl::bt.break-level context))))

(objc:defmethod #/windowNibName ((self backtrace-window-controller))
  #@"backtrace")

(objc:defmethod (#/close :void) ((self backtrace-window-controller))
  (setf (slot-value self 'context) nil)
  (call-next-method))

(defmethod our-frame-label-p ((self backtrace-window-controller) thing)
  (and (typep thing 'frame-label)
       (eql self (frame-label-controller thing))))

(def-cocoa-default *backtrace-font-name* :string #+darwin-target "Monaco"
                   #-darwin-target "Courier" "Name of font used in backtrace views")
(def-cocoa-default *backtrace-font-size* :float 9.0f0 "Size of font used in backtrace views")


(objc:defmethod (#/windowDidLoad :void) ((self backtrace-window-controller))
  (let* ((outline (slot-value self 'outline-view))
         (font (default-font :name *backtrace-font-name* :size *backtrace-font-size*)))
    (unless (%null-ptr-p outline)
      (#/setTarget: outline self)
      #-cocotron ; crashes
      (#/setRowHeight: outline  (size-of-char-in-font font))
      (#/setDoubleAction: outline (@selector #/backtraceDoubleClick:))
      (#/setShouldCascadeWindows: self nil)
      (let* ((columns (#/tableColumns outline)))
        (dotimes (i (#/count columns))
          (let* ((column (#/objectAtIndex:  columns i))
                 (data-cell (#/dataCell column)))
            (#/setEditable: data-cell nil)
            (#/setFont: data-cell font)
            (when (eql i 0)
              (let* ((header-cell (#/headerCell column))
                     (sd (backtrace-controller-stack-descriptor self))
                     (break-condition (stack-descriptor-condition sd))
                     (break-condition-string
                      (let* ((*print-level* 5)
                             (*print-length* 5)
                             (*print-circle* t))
                        (format nil "~a: ~a"
                                (class-name (class-of break-condition))
                                break-condition))))
                (ccl::with-autoreleased-nsstring (s break-condition-string)
                  (#/setStringValue: (slot-value self 'message-field) s))
                (#/setFont: header-cell (default-font :name "Courier" :size 10 :attributes '(:bold)))
                (#/setStringValue: header-cell (%make-nsstring break-condition-string))))))))
    (let* ((window (#/window  self)))
      (unless (%null-ptr-p window)
        (let* ((process (backtrace-controller-process self))
               (listener-window (if (typep process 'cocoa-listener-process)
                                  (cocoa-listener-process-window process))))
          (when listener-window
            (let* ((listener-frame (#/frame listener-window))
                   (backtrace-width (ns:ns-rect-width (#/frame window)))
                   (new-x (- (+ (ns:ns-rect-x listener-frame)
                                (/ (ns:ns-rect-width listener-frame) 2))
                             (/ backtrace-width 2))))
              (ns:with-ns-point (p new-x (+ (ns:ns-rect-y listener-frame) (ns:ns-rect-height listener-frame)))
                (#/setFrameOrigin: window p))))
          (#/setTitle:  window (%make-nsstring
                                (format nil "Backtrace for ~a(~d), break level ~d"
                                        (process-name process)
                                        (process-serial-number process)
                                        (backtrace-controller-break-level self)))))))))

(objc:defmethod (#/continue: :void) ((self backtrace-window-controller) sender)
  (declare (ignore sender))
  (let ((process (backtrace-controller-process self)))
    (when process (process-interrupt process #'continue))))

(objc:defmethod (#/exitBreak: :void) ((self backtrace-window-controller) sender)
  (declare (ignore sender))
  (let ((process (backtrace-controller-process self)))
    (when process (process-interrupt process #'abort-break))))

(objc:defmethod (#/restarts: :void) ((self backtrace-window-controller) sender)
  (let* ((context (backtrace-controller-context self)))
    (when context
      (#/showWindow: (restarts-controller-for-context context) sender))))

(objc:defmethod (#/backtraceDoubleClick: :void)
    ((self backtrace-window-controller) sender)
  (let* ((row (#/clickedRow sender)))
    (if (>= row 0)
      (let* ((item (#/itemAtRow: sender row)))
        (cond ((typep item 'frame-label)
               (let ((frame (frame-label-descriptor item)))
                 (backtrace-frame-default-action frame)))
              ((typep item 'item-label)
               (let* ((frame (frame-label-descriptor (item-label-label item)))
                      (index (item-label-index item)))
                 (backtrace-frame-default-action frame index))))))))

(objc:defmethod (#/outlineView:isItemExpandable: :<BOOL>)
    ((self backtrace-window-controller) view item)
  (declare (ignore view))
  (or (%null-ptr-p item)
      (and (our-frame-label-p self item)
           (> (frame-descriptor-value-count (frame-label-descriptor item)) 0))))

(objc:defmethod (#/outlineView:numberOfChildrenOfItem: :<NSI>nteger)
    ((self backtrace-window-controller) view item)
    (declare (ignore view))
    (let* ((sd (backtrace-controller-stack-descriptor self)))
      (cond ((%null-ptr-p item)
             (stack-descriptor-frame-count sd))
            ((our-frame-label-p self item)
             (let ((frame (stack-descriptor-frame sd (frame-label-number item))))
               (frame-descriptor-value-count frame)))
            (t -1))))

(objc:defmethod #/outlineView:child:ofItem:
    ((self backtrace-window-controller) view (index :<NSI>nteger) item)
  (declare (ignore view))
  (cond ((%null-ptr-p item)
         (make-instance 'frame-label
           :with-frame-number index
           :controller self))
        ((our-frame-label-p self item)
         (make-instance 'item-label
           :with-frame-label item
           :index index))
        (t (break) (%make-nsstring "Huh?"))))

(objc:defmethod #/outlineView:objectValueForTableColumn:byItem:
    ((self backtrace-window-controller) view column item)
  (declare (ignore view column))
  (if (%null-ptr-p item)
    #@"Open this"
    (%setf-macptr (%null-ptr) item)))

(defmethod initialize-instance :after ((self backtrace-window-controller)
                                       &key &allow-other-keys)
  (setf (slot-value self 'stack-descriptor)
        (make-stack-descriptor (backtrace-controller-context self))))

(defun backtrace-controller-for-context (context)
  (let ((bt (ccl::bt.dialog context)))
    (when bt
      (stack-descriptor-refresh (backtrace-controller-stack-descriptor bt)))
    (or bt
        (setf (ccl::bt.dialog context)
              (make-instance 'backtrace-window-controller
                :with-window-nib-name #@"backtrace"
                :context context)))))

#+debug
(objc:defmethod (#/willLoad :void) ((self backtrace-window-controller))
  (#_NSLog #@"will load %@" :address  (#/windowNibName self)))

;; Called when current process is about to enter a breakloop
(defmethod ui-object-enter-backtrace-context ((app ns:ns-application)
                                              context)
  (let* ((proc *current-process*))
    (when (typep proc 'cocoa-listener-process)
      (push context (cocoa-listener-process-backtrace-contexts proc)))))

(defmethod ui-object-exit-backtrace-context ((app ns:ns-application)
                                              context)
  (let* ((proc *current-process*))
    (when (typep proc 'cocoa-listener-process)
      (when (eq context (car (cocoa-listener-process-backtrace-contexts proc)))
        (setf (cocoa-listener-process-backtrace-contexts proc)
              (cdr (cocoa-listener-process-backtrace-contexts proc)))
        (let* ((btwindow (prog1 (ccl::bt.dialog context)
                           (setf (ccl::bt.dialog context) nil)))
               (restartswindow
                (prog1 (car (ccl::bt.restarts context))
                           (setf (ccl::bt.restarts context) nil))))
          (when btwindow
            (#/performSelectorOnMainThread:withObject:waitUntilDone: btwindow (@selector #/close)  +null-ptr+ t))
          (when restartswindow
            (#/performSelectorOnMainThread:withObject:waitUntilDone: restartswindow (@selector #/close)  +null-ptr+ t)))))))

  
(objc:defmethod (#/validateToolbarItem: #>BOOL) ((self backtrace-window-controller)
                                                 toolbar-item)
  (let* ((outline-view (backtrace-controller-outline-view self))
         (row (#/selectedRow outline-view))
         (identifier (#/itemIdentifier toolbar-item)))
    (if (or (#/isEqualToString: identifier #@"expand all")
            (#/isEqualToString: identifier #@"collapse all"))
      #$YES
      (when (/= row -1)
        (let ((item (#/itemAtRow: outline-view row)))
          (cond ((typep item 'frame-label)
                 (if (#/isEqualToString: identifier #@"inspect")
                   #$NO
                   #$YES))
              ((typep item 'item-label)
               #$YES)
              (t
               #$NO)))))))

(objc:defmethod (#/validateMenuItem: #>BOOL) ((self backtrace-window-controller)
                                                 menu-item)
  (when (eql (contextual-menu self) (#/menu menu-item))
    (let* ((ov (backtrace-controller-outline-view self))
           (clicked-row (#/clickedRow ov))
           (row (#/selectedRow ov))
           (tag (#/tag menu-item)))
      (when (/= clicked-row -1)
        (setq row clicked-row))
      (when (/= row -1)
        (let ((item (#/itemAtRow: ov row)))
          (cond ((= tag $inspect-item-tag)
                 (typep item 'item-label))
                ((= tag $source-item-tag)
                 t)))))))

(defun frame-descriptor-from-item (item)
  (etypecase item
    (frame-label (frame-label-descriptor item))
    (item-label (frame-label-descriptor (item-label-label item)))))

(objc:defmethod (#/inspect: :void) ((self backtrace-window-controller) sender)
  (declare (ignore sender))
  (let* ((ov (backtrace-controller-outline-view self))
         (row (#/selectedRow ov)))
    (when (/= row -1)
      (let ((item (#/itemAtRow: ov row)))
        (cond ((typep item 'item-label)
               (let* ((frame (frame-label-descriptor (item-label-label item)))
                      (index (item-label-index item)))
                 (inspect (frame-descriptor-value frame index)))))))))

(objc:defmethod (#/source: :void) ((self backtrace-window-controller) sender)
  (declare (ignore sender))
  (let* ((ov (backtrace-controller-outline-view self))
         (row (#/selectedRow ov))
         (clicked-row (#/clickedRow ov)))
    (when (/= clicked-row -1)
      (setq row clicked-row))
    (when (/= row -1)
      (let* ((item (#/itemAtRow: ov row))
             (frame (frame-descriptor-from-item item)))
        (show-frame-source frame)))))

(objc:defmethod (#/expandAll: :void) ((self backtrace-window-controller) sender)
  (declare (ignore sender))
  (let ((ov (backtrace-controller-outline-view self)))
    (#/expandItem:expandChildren: ov +null-ptr+ #$YES)))

(objc:defmethod (#/collapseAll: :void) ((self backtrace-window-controller) sender)
  (declare (ignore sender))
  (let ((ov (backtrace-controller-outline-view self)))
    (#/collapseItem:collapseChildren: ov +null-ptr+ #$YES)))

  