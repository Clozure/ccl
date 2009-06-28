;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates

(in-package "GUI")

(defclass ns-lisp-string (ns:ns-string)
    ((lisp-string :initarg :string :reader ns-lisp-string-string))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/length :<NSUI>nteger) ((self ns-lisp-string))
    (length (ns-lisp-string-string self)))

(objc:defmethod (#/characterAtIndex: :unichar) ((self ns-lisp-string) (index :<NSUI>nteger))
  (char-code (schar (ns-lisp-string-string self) index)))

(objc:defmethod (#/dealloc :void) ((self ns-lisp-string))
  (ccl::%remove-lisp-slot-vector self)
  (call-next-method))


(defclass frame-label (ns-lisp-string)
    ((frame-number  :foreign-type :int :accessor frame-label-number)
     (controller :foreign-type :id :reader frame-label-controller)
     (frame-inspector :initform nil :accessor frame-label-frame-inspector))
  (:metaclass ns:+ns-object))

(objc:defmethod #/initWithFrameNumber:controller: ((self frame-label) (frame-number :int) controller)
  (let* ((obj (#/init self)))
    (unless (%null-ptr-p obj)
      (setf (slot-value obj 'frame-number) frame-number
            (slot-value obj 'controller) controller))
    obj))


(defclass item-label (ns-lisp-string)
    ((frame-label :foreign-type :id :accessor item-label-label)
     (index :foreign-type :int :accessor item-label-index))
  (:metaclass ns:+ns-object))

(objc:defmethod #/initWithFrameLabel:index: ((self item-label) the-frame-label (index :int))
  (let* ((obj (#/init self)))
    (unless (%null-ptr-p obj)
      (setf (slot-value obj 'frame-label) the-frame-label
            (slot-value obj 'index) index))
    obj))

(defclass backtrace-window-controller (ns:ns-window-controller)
    ((context :initarg :context :reader backtrace-controller-context)
     (inspector :initform nil :reader backtrace-controller-inspector)
     (outline-view :foreign-type :id :reader backtrace-controller-outline-view))
  (:metaclass ns:+ns-object))

(objc:defmethod #/windowNibName ((self backtrace-window-controller))
  #@"backtrace")

(objc:defmethod (#/close :void) ((self backtrace-window-controller))
  (setf (slot-value self 'context) nil)
  (call-next-method))

(defmethod our-frame-label-p ((self backtrace-window-controller) thing)
  (and (typep thing 'frame-label)
       (eql self (frame-label-controller thing))))

(def-cocoa-default *backtrace-font-name* :string "Monaco" "Name of font used in backtrace views")
(def-cocoa-default *backtrace-font-size* :float 9.0f0 "Size of font used in backtrace views")


(defun context-process (context)
  (and context (ccl::tcr->process (ccl::bt.tcr context))))

(objc:defmethod (#/windowDidLoad :void) ((self backtrace-window-controller))
  (let* ((outline (slot-value self 'outline-view))
         (font (default-font :name *backtrace-font-name* :size *backtrace-font-size*)))
    (unless (%null-ptr-p outline)
      (#/setTarget: outline self)
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
                     (inspector (backtrace-controller-inspector self))
                     (break-condition
                      (inspector::break-condition
                                 (inspector::inspector-object inspector)))
                     (break-condition-string
                      (let* ((*print-level* 5)
                             (*print-length* 5)
                             (*print-circle* t))
                        (format nil "~a: ~a"
                                (class-name (class-of break-condition))
                                break-condition))))
                (#/setFont: header-cell (default-font :name "Courier" :size 10 :attributes '(:bold)))
                (#/setStringValue: header-cell (%make-nsstring break-condition-string))))))))
    (let* ((window (#/window  self)))
      (unless (%null-ptr-p window)
        (let* ((context (backtrace-controller-context self))
               (process (context-process context))
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
                                        (ccl::bt.break-level context)))))))))

(objc:defmethod (#/continue: :void) ((self backtrace-window-controller) sender)
  (declare (ignore sender))
  (let* ((context (backtrace-controller-context self))
         (process (context-process context)))
    (when process (process-interrupt process #'continue))))

(objc:defmethod (#/exitBreak: :void) ((self backtrace-window-controller) sender)
  (declare (ignore sender))
  (let* ((context (backtrace-controller-context self))
         (process (context-process context)))
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
             (val-p nil)
             (value nil))
        (cond ((typep item 'frame-label)
               (let* ((controller (frame-label-controller item))
                      (inspector (backtrace-controller-inspector controller))
                      (frame-number (frame-label-number item)))
                 (setq val-p t value (inspector::line-n inspector frame-number))))
              ((typep item 'item-label)
               (let* ((the-frame-label (item-label-label item))
                      (frame-inspector (frame-label-frame-inspector the-frame-label))
                      (index (item-label-index item))
                      (rawval (inspector::line-n frame-inspector index)))
                 (if (and (consp rawval)
                          (typep (car rawval) 'keyword))
                 (setq val-p t value (cddr rawval))))))
        (if val-p
          (inspect value))))))




(objc:defmethod (#/outlineView:isItemExpandable: :<BOOL>)
    ((self backtrace-window-controller) view item)
    (declare (ignore view))
    (or (%null-ptr-p item)
        (our-frame-label-p self item)))

(objc:defmethod (#/outlineView:numberOfChildrenOfItem: :<NSI>nteger)
    ((self backtrace-window-controller) view item)
    (declare (ignore view))
    (let* ((inspector (backtrace-controller-inspector self)))
      (cond ((%null-ptr-p item)
             (inspector::inspector-line-count inspector))
            ((our-frame-label-p self item)
             (let* ((frame-inspector
                     (or (frame-label-frame-inspector item)
                         (setf (frame-label-frame-inspector item)
                               (make-instance
                                'inspector::stack-frame-inspector
                                :frame-number (frame-label-number item)
                                :object (inspector::inspector-object inspector)
				:update-line-count t)))))
               (inspector::inspector-line-count frame-inspector)))
            (t -1))))

(objc:defmethod #/outlineView:child:ofItem:
    ((self backtrace-window-controller) view (index :<NSI>nteger) item)
  (declare (ignore view))
  (let* ((inspector (backtrace-controller-inspector self)))
    (cond ((%null-ptr-p item)
           (let* ((label
                   (make-instance 'frame-label
                                  :with-frame-number index
                                  :controller self
                                  :string
                                  (let* ((value 
                                          (inspector::line-n inspector index)))
                                    (if value
                                      (ccl::%lfun-name-string value)
                                      ":kernel")))))
             label))
          ((our-frame-label-p self item)
           (let* ((frame-inspector
                   (or (frame-label-frame-inspector item)
                       (setf (frame-label-frame-inspector item)
                             (make-instance
                              'inspector::stack-frame-inspector
                              :frame-number (frame-label-number item)
                              :object (inspector::inspector-object inspector)
                              :update-line-count t)))))
             (make-instance 'item-label
                            :with-frame-label item
                            :index index
                            :string
                            (let* ((ccl::*aux-vsp-ranges* (inspector::vsp-range inspector))
                                   (ccl::*aux-tsp-ranges* (inspector::tsp-range inspector))
                                   (ccl::*aux-csp-ranges* (inspector::csp-range inspector)))
                              (with-output-to-string (s)
                                                     (let* ((value
                                                             (inspector::line-n
                                                              frame-inspector
                                                              index)))
                                                       (inspector::prin1-value
                                                        frame-inspector
                                                        s
                                                        value)))))))
          (t (break) (%make-nsstring "Huh?")))))

(objc:defmethod #/outlineView:objectValueForTableColumn:byItem:
    ((self backtrace-window-controller) view column item)
  (declare (ignore view column))
  (if (%null-ptr-p item)
    #@"Open this"
    (%setf-macptr (%null-ptr) item)))

(defmethod initialize-instance :after ((self backtrace-window-controller)
                                       &key &allow-other-keys)
  (setf (slot-value self 'inspector)
        (make-instance 'inspector::stack-inspector :context (backtrace-controller-context self) :update-line-count t)))

(defun backtrace-controller-for-context (context)
  (or (ccl::bt.dialog context)
      (setf (ccl::bt.dialog context)
            (make-instance 'backtrace-window-controller
                           :with-window-nib-name #@"backtrace"
                           :context context))))

#+debug
(objc:defmethod (#/willLoad :void) ((self backtrace-window-controller))
  (#_NSLog #@"will load %@" :address  (#/windowNibName self)))

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

  





