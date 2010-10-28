(require :cocoa)

(defclass progress-example-view (ns:ns-view)
  ((progress-indicator)
   (timer :initform +null-ptr+))
  (:documentation "")
  (:metaclass ns:+ns-object))

(objc:defmethod (#/init :id) ((self progress-example-view))
  (let ((self (call-next-method)))
    (with-slots (progress-indicator) self
      (setf progress-indicator (make-instance 'ns:ns-progress-indicator
                                 :with-frame (ns:make-ns-rect 10 10 280 30)))
      (setf (#/style progress-indicator) #$NSProgressIndicatorBarStyle)
      (#/setIndeterminate: progress-indicator nil)
      (#/addSubview: self progress-indicator))
    (let ((start-button (make-instance 'ns:ns-button
                          :with-frame (ns:make-ns-rect 10 150 135 30)))
          (pause-button (make-instance 'ns:ns-button
                          :with-frame (ns:make-ns-rect 155 150 135 30)))
          (reset-button (make-instance 'ns:ns-button
                          :with-frame (ns:make-ns-rect 10 110 280 30)))
          (indeterminate-checkbox (make-instance 'ns:ns-button
                                    :with-frame (ns:make-ns-rect 10 70 280 30)))
          (spinning-checkbox (make-instance 'ns:ns-button
                               :with-frame (ns:make-ns-rect 10 50 280 30))))
      (setf (#/title start-button) #@"Start"
            (#/action start-button) (objc:@selector #/start)
            (#/target start-button) self
            (#/title pause-button) #@"Pause"
            (#/action pause-button) (objc:@selector #/pause)
            (#/target pause-button) self
            (#/title reset-button) #@"Reset"
            (#/action reset-button) (objc:@selector #/reset)
            (#/target reset-button) self
            (#/buttonType indeterminate-checkbox) #$NSSwitchButton
            (#/title indeterminate-checkbox) #@"use indeterminate progress indicator"
            (#/action indeterminate-checkbox) (objc:@selector #/toggleIndeterminate:)
            (#/target indeterminate-checkbox) self
            (#/buttonType spinning-checkbox) #$NSSwitchButton
            (#/title spinning-checkbox) #@"use spinning progress indicator"
            (#/action spinning-checkbox) (objc:@selector #/toggleSpinning:)
            (#/target spinning-checkbox) self)
      (#/addSubview: self start-button)
      (#/addSubview: self pause-button)
      (#/addSubview: self reset-button)
      (#/addSubview: self indeterminate-checkbox)
      (#/addSubview: self spinning-checkbox))
    self))

(defun make-button (title)
  (let ((button (make-instance 'ns:ns-button)))
    (setf (#/title button) (ccl::%make-nsstring title))
    button))

(objc:defmethod (#/incrementProgressIndicator: :void)
                ((self progress-example-view) timer)
  (declare (ignore timer))
  (with-slots (progress-indicator) self
    (#/incrementBy: progress-indicator 1d0)
    (when (<= (#/maxValue progress-indicator) (#/doubleValue progress-indicator))
      (#/pause self))))

(objc:defmethod (#/start :void) ((self progress-example-view))
  (with-slots (progress-indicator timer) self
    (when (and (not (#/isIndeterminate progress-indicator)) (%null-ptr-p timer))
      (setf timer
            (#/retain (#/scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:
                       ns:ns-timer 0.05d0 self (objc:@selector #/incrementProgressIndicator:) +null-ptr+ t))))
    (#/startAnimation: progress-indicator self)))

(objc:defmethod (#/pause :void) ((self progress-example-view))
  (with-slots (progress-indicator timer) self
    (#/invalidate timer)
    (#/release timer)
    (setf timer +null-ptr+)
    (#/stopAnimation: progress-indicator self)))

(objc:defmethod (#/reset :void) ((self progress-example-view))
  (with-slots (progress-indicator) self
    (#/pause self)
    (setf (#/doubleValue progress-indicator) 0d0)))

(objc:defmethod (#/toggleIndeterminate: :void) ((self progress-example-view) sender)
  (with-slots (progress-indicator) self
    (#/reset self)
    (#/setIndeterminate: progress-indicator (eql (#/state sender) #$NSOnState))))

(objc:defmethod (#/toggleSpinning: :void) ((self progress-example-view) sender)
  (with-slots (progress-indicator) self
    (setf (#/style progress-indicator)
          (if (eql (#/state sender) #$NSOnState)
            #$NSProgressIndicatorSpinningStyle
            #$NSProgressIndicatorBarStyle))))

(defun show-progress-window ()
  (ccl::with-autorelease-pool
   (let* ((window (make-window "Progress Indicator Example")))
     (#/setContentView: window
                        (#/autorelease (make-instance 'progress-example-view)))
     (display-window window))))

;;; boilerplate

(defun make-window (title)
  (let* ((window (make-instance 'ns:ns-window
                   :with-content-rect (ns:make-ns-rect 0 0 300 200)
                   :style-mask (logior #$NSTitledWindowMask
                                       #$NSClosableWindowMask
                                       #$NSMiniaturizableWindowMask)
                   :backing #$NSBackingStoreBuffered
                   :defer t)))
    (#/setTitle: window (ccl::%make-nsstring title))
    (#/center window)
    window))

(defun display-window (window)
  (#/orderFront: window nil)
  (#/contentView window))
