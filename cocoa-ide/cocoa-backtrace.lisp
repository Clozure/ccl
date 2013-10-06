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
                           (when (ccl::function-frame-p fp context)
                             (incf count))))
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

(defvar $backtrace-label-max-len 300)

(defun make-frame-descriptor (fp context)
  (let* ((args (ccl:frame-supplied-arguments fp context))
         (vars (ccl:frame-named-variables fp context))
         (lfun (ccl:frame-function fp context)))
    (macrolet ((with-truncating-output ((streamvar) &body body)
                 `(multiple-value-bind (str truncated-p)
                                       (ccl::with-output-to-truncating-string-stream
                                          (,streamvar $backtrace-label-max-len)
                                         ,@body)
                    (if truncated-p (concatenate 'string str "...") str))))
      (make-instance 'frame-descriptor
        :data (cons fp context)
        :label (if lfun
               (with-truncating-output (stream)
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
                       (let* ((var (car var.val))
			      (val (cdr var.val))
			      (label (with-truncating-output (stream)
                                      (format stream "~:[~s~;~a~]: ~s" (stringp var) var val))))
                         (cons label var.val)))
                     (cons `("Function" . ,lfun)
                           (and (not (eq vars (ccl::%unbound-marker))) vars))))))))

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

;;; Cocoa layer

;; An NSOutlineView wants to manage a collection of item objects.
;; These guys fill the bill.
(defclass backtrace-ov-item (ns:ns-object)
  ((label :foreign-type :id)
   (container :accessor container :initarg :container
	      :documentation "A stack-descriptor or frame-descriptor")
   (index :accessor index :initarg :index)
   (children :initform nil :accessor children :initarg :children))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/dealloc :void) ((self backtrace-ov-item))
  (with-slots (label children) self
    (#/release label)
    (map nil #'(lambda (item) (#/release item)) children))
  (objc:remove-lisp-slots self)
  (call-next-method))

;; Lazily create the NSString instances for backtrace-ov-item objects.
(defmethod label ((self backtrace-ov-item))
  (with-slots (label container index) self
    (when (%null-ptr-p label)
      (etypecase container
	(stack-descriptor
	 (let ((frame (stack-descriptor-frame container index)))
	   (setf label (%make-nsstring (frame-descriptor-label frame)))))
	(frame-descriptor
	 (setf label (%make-nsstring (frame-descriptor-value-label container index))))))
    label))

(defun generate-ov-items (stack-descriptor)
  (let ((items '())
        (nframes (count-stack-descriptor-frames stack-descriptor)))
    (dotimes (i nframes)
      (let* ((frame (stack-descriptor-frame stack-descriptor i))
             (item (make-instance 'backtrace-ov-item :index i
                     :container stack-descriptor))
             (nvalues (frame-descriptor-value-count frame))
             (children nil))
        ;; create items for frame values
        (unless (zerop (frame-descriptor-value-count frame))
          (dotimes (j nvalues)
            (let ((item (make-instance 'backtrace-ov-item :index j
                          :container frame)))
              (push item children))))
        (setf (children item) (nreverse children))
        (push item items)))
    (coerce (nreverse items) 'vector)))

(defclass backtrace-window-controller (ns:ns-window-controller)
  ((context :initarg :context :reader backtrace-controller-context)
   (stack-descriptor :initform nil :reader backtrace-controller-stack-descriptor)
   (outline-view :foreign-type :id :reader backtrace-controller-outline-view)
   (message-field :foreign-type :id :accessor message-field)
   (action-menu :foreign-type :id :accessor action-menu)
   (action-popup-button :foreign-type :id :accessor action-popup-button)
   (contextual-menu :foreign-type :id :accessor contextual-menu)
   (ov-items :accessor ov-items :initarg :ov-items :initform nil))
  (:metaclass ns:+ns-object))

(defmethod backtrace-controller-process ((self backtrace-window-controller))
  (let ((context (backtrace-controller-context self)))
    (and context (ccl::tcr->process (ccl::bt.tcr context)))))

(defmethod backtrace-controller-break-level ((self backtrace-window-controller))
  (let ((context (backtrace-controller-context self)))
    (and context (ccl::bt.break-level context))))

(objc:defmethod #/windowNibName ((self backtrace-window-controller))
  #@"backtrace")

(defmethod initialize-instance :after ((self backtrace-window-controller)
                                       &key &allow-other-keys)
  (let ((sd (make-stack-descriptor (backtrace-controller-context self))))
    (setf (slot-value self 'stack-descriptor) sd)
    (setf (slot-value self 'ov-items) (generate-ov-items sd))))

(objc:defmethod (#/close :void) ((self backtrace-window-controller))
  (setf (slot-value self 'context) nil)
  (call-next-method))

(objc:defmethod (#/dealloc :void) ((self backtrace-window-controller))
  (with-slots (ov-items) self
    (map nil #'(lambda (item) (#/release item)) ov-items))
  (objc:remove-lisp-slots self)
  (call-next-method))

(objc:defmethod (#/windowDidLoad :void) ((self backtrace-window-controller))
  (let* ((outline (slot-value self 'outline-view)))
    (unless (%null-ptr-p outline)
      (#/setTarget: outline self)
      (#/setDoubleAction: outline (@selector #/backtraceDoubleClick:))
      (#/setShouldCascadeWindows: self nil))
    
    (let* ((sd (backtrace-controller-stack-descriptor self))
	   (break-condition (stack-descriptor-condition sd))
	   (break-condition-string (let* ((*print-level* 5)
					  (*print-length* 5)
					  (*print-circle* t))
				     (format nil "~a: ~a"
					     (class-name
					      (class-of break-condition))
					     break-condition))))
      (ccl::with-autoreleased-nsstring (s break-condition-string)
	(#/setStringValue: (slot-value self 'message-field) s)))

    (let* ((window (#/window self)))
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
              (ns:with-ns-point (p new-x (+ (ns:ns-rect-y listener-frame)
					    (ns:ns-rect-height listener-frame)))
                (#/setFrameOrigin: window p))))
	  (let ((title (format nil "Backtrace for ~a(~d), break level ~d"
			       (process-name process)
			       (process-serial-number process)
			       (backtrace-controller-break-level self))))
	    (ccl::with-autoreleased-nsstring (s title)
	      (#/setTitle: window s))))
	;; make initial contents appear
	#+cocotron
	(#/reloadData (slot-value self 'outline-view))))))

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
      (let* ((item (#/itemAtRow: sender row))
	     (container (container item))
	     (index (index item)))
        (cond ((typep container 'stack-descriptor)
               (let ((frame (stack-descriptor-frame container index)))
                 (backtrace-frame-default-action frame)))
              ((typep container 'frame-descriptor)
	       (backtrace-frame-default-action container index)))))))

;;; outline view data source methods

(objc:defmethod (#/outlineView:isItemExpandable: #>BOOL)
    ((self backtrace-window-controller) view item)
  (declare (ignore view))
  (if (%null-ptr-p item)
    #$YES
    (let ((container (container item)))
      (cond ((typep container 'stack-descriptor)
	     (let ((frame (stack-descriptor-frame container (index item))))
	       (if (plusp (frame-descriptor-value-count frame))
		 #$YES
		 #$NO)))
	    (t
	     #$NO)))))

(objc:defmethod (#/outlineView:numberOfChildrenOfItem: #>NSInteger)
    ((self backtrace-window-controller) view item)
    (declare (ignore view))
    (let ((sd (backtrace-controller-stack-descriptor self)))
      (if (%null-ptr-p item)
	(stack-descriptor-frame-count sd)
	(let ((container (container item)))
	  (cond ((typep container 'stack-descriptor)
		 (let ((frame (stack-descriptor-frame container (index item))))
		   (frame-descriptor-value-count frame)))
		(t 0))))))

(objc:defmethod #/outlineView:child:ofItem:
    ((self backtrace-window-controller) view (index :<NSI>nteger) item)
  (declare (ignore view))
  (cond ((%null-ptr-p item)
	 (elt (ov-items self) index))
	((typep (container item) 'stack-descriptor)
	 (elt (children item) index))
	(t +null-ptr+)))

(objc:defmethod #/outlineView:objectValueForTableColumn:byItem:
    ((self backtrace-window-controller) view column item)
  (declare (ignore view column))
  (if (%null-ptr-p item)
    #@"Open this"
    (label item)))

(objc:defmethod (#/outlineView:isGroupItem: #>BOOL)
    ((self backtrace-window-controller) ov item)
  (#/outlineView:isItemExpandable: self ov item))

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

(objc:defmethod (#/windowWillClose: :void) ((self backtrace-window-controller) notification)
  (declare (ignore notification))
  (with-slots (context) self
    (when context
      (setf (ccl::bt.dialog context) nil)))
  #-cocotron
  (#/autorelease self))


(defmethod backtrace-context-backtrace-window ((context vector))
  (ccl::bt.dialog context))
(defmethod (setf backtrace-context-backtrace-window) (obj (context vector))
  (setf (ccl::bt.dialog context) obj))

(defmethod backtrace-context-restarts-window ((context vector))
  (car (ccl::bt.restarts context)))
(defmethod (setf backtrace-context-restarts-window) (obj (context vector))
  (setf (car (ccl::bt.restarts context)) obj))

;; Called when current process is about to enter a breakloop
(defmethod ui-object-enter-backtrace-context ((app ns:ns-application) context)
  (enter-backtrace-context *current-process* context))

(defmethod ui-object-exit-backtrace-context ((app ns:ns-application) context)
  (exit-backtrace-context *current-process* context))

(defmethod enter-backtrace-context ((process process) context)
  (declare (ignore context)))

(defmethod exit-backtrace-context ((process process) context)
  (declare (ignore context)))

(defmethod enter-backtrace-context ((process cocoa-listener-process) context)
  (push context (cocoa-listener-process-backtrace-contexts process)))

(defmethod exit-backtrace-context ((process cocoa-listener-process) context)
  (when (eq context (car (cocoa-listener-process-backtrace-contexts process)))
    (pop (cocoa-listener-process-backtrace-contexts process))
    (let ((w (backtrace-context-backtrace-window context)))
      (when w
        (setf (backtrace-context-backtrace-window context) nil)
        (cocoa-close w t)))
    (let ((w (backtrace-context-restarts-window context)))
      (when w
        (setf (backtrace-context-restarts-window context) nil)
        (cocoa-close w t)))))


(objc:defmethod (#/validateToolbarItem: #>BOOL) ((self backtrace-window-controller)
                                                 toolbar-item)
  (let* ((outline-view (backtrace-controller-outline-view self))
         (row (#/selectedRow outline-view))
	 (clicked-row (#/clickedRow outline-view))
         (identifier (#/itemIdentifier toolbar-item)))
    (when (/= clicked-row -1)
      (setq row clicked-row))
    (if (or (#/isEqualToString: identifier #@"expand all")
            (#/isEqualToString: identifier #@"collapse all"))
      #$YES
      (when (/= row -1)
        (let ((item (#/itemAtRow: outline-view row)))
          (cond ((typep (container item) 'stack-descriptor)
                 (if (#/isEqualToString: identifier #@"inspect")
                   #$NO
                   #$YES))
              ((typep (container item) 'frame-descriptor)
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
                 (typep (container item) 'frame-descriptor))
                ((= tag $source-item-tag)
                 t)))))))

(defun frame-descriptor-from-item (item)
  (let ((container (container item))
	(index (index item)))
    (etypecase (container item)
      (stack-descriptor (stack-descriptor-frame container index))
      (frame-descriptor container))))

(objc:defmethod (#/inspect: :void) ((self backtrace-window-controller) sender)
  (declare (ignore sender))
  (let* ((ov (backtrace-controller-outline-view self))
         (row (#/selectedRow ov))
	 (clicked-row (#/clickedRow ov)))
    (when (/= clicked-row -1)
      (setq row clicked-row))
    (when (/= row -1)
      (let ((item (#/itemAtRow: ov row)))
        (cond ((typep (container item) 'frame-descriptor)
               (let* ((frame (container item))
                      (index (index item)))
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

  
