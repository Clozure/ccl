(in-package :easygui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; view protocol

(defgeneric initialize-view (view)
  (:documentation "Initializes the view with a cocoa object, sets it up
according to initargs."))

(defgeneric add-1-subview (view super-view)
  (:documentation "Adds a subview to another view in the view hierarchy."))

(defgeneric remove-1-subview (view super-view)
  (:documentation "Removes a view from its superview, possibly deallocating it.
To avoid deallocation, use RETAINING-OBJECTS"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mixins

(defclass value-mixin () ())
(defclass string-value-mixin (value-mixin) ())
(defclass numeric-value-mixin (value-mixin) ())

(macrolet ((def-type-accessor (class lisp-type cocoa-reader cocoa-writer
                                     &key new-value-form return-value-converter)
               (let ((name (intern (format nil "~A-VALUE-OF" lisp-type))))
                 `(progn
                    (defmethod ,name ((o ,class))
                      ,(if return-value-converter
                           `(,return-value-converter
                             (dcc (,cocoa-reader (cocoa-ref o))))
                           `(dcc (,cocoa-reader (cocoa-ref o)))))
                    (defmethod (setf ,name) (new-value (o ,class))
                      (dcc (,cocoa-writer (cocoa-ref o)
                                          ,(or new-value-form
                                               'new-value))))))))
  (def-type-accessor string-value-mixin string #/stringValue #/setStringValue:
                     :return-value-converter lisp-string-from-nsstring )

  (def-type-accessor numeric-value-mixin integer #/intValue #/setIntValue:)
  (def-type-accessor numeric-value-mixin float
    #/floatValue #/setFloatValue:
    :new-value-form (coerce new-value 'single-float))
  (def-type-accessor numeric-value-mixin double
    #/doubleValue #/setDoubleValue:
    :new-value-form (coerce new-value 'double-float)))

(defclass view-text-mixin ()
     ((text :initarg :text)))
(defclass view-text-via-stringvalue-mixin (view-text-mixin string-value-mixin)
     ())
(defclass view-text-via-title-mixin (view-text-mixin)
     ((text :initarg :title)))

(defmethod view-text ((view view-text-via-stringvalue-mixin))
  (string-value-of view))

(defmethod view-text ((view view-text-via-title-mixin))
  (lisp-string-from-nsstring (dcc (#/title (cocoa-ref view)))))

(defmethod (setf view-text) (new-text (view view-text-via-stringvalue-mixin))
  (setf (string-value-of view) new-text))

(defmethod (setf view-text) (new-text (view view-text-via-title-mixin))
  (dcc (#/setTitle: (cocoa-ref view) new-text)))

(defmethod initialize-view :after ((view view-text-mixin))
  (when (slot-boundp view 'text)
    (setf (view-text view) (slot-value view 'text))))

(defclass editable-mixin () ())

(defmethod editable-p ((view editable-mixin))
  (dcc (#/isEditable (cocoa-ref view))))

(defmethod (setf editable-p) (editable-p (view editable-mixin))
  (check-type editable-p boolean)
  (dcc (#/setEditable: (cocoa-ref view) editable-p)))

(defclass one-selection-mixin () ())

(defmethod (setf selection) (selection (view one-selection-mixin))
  (dcc (#/setSelectedRange: (cocoa-ref view) (range-nsrange selection))))

(defmethod selection ((view one-selection-mixin))
  (let ((range (dcc (#/selectedRange (cocoa-ref view)))))
    (if (= (ns:ns-range-location range) #$NSNotFound)
        nil
        (range (ns:ns-range-location range)
               (ns:ns-range-length range)))))

(defclass content-view-mixin ()
     (content-view))

(defmethod initialize-view :after ((view content-view-mixin))
  (setf (slot-value view 'content-view)
        (make-instance 'view
           :cocoa-ref (dcc (#/contentView (cocoa-ref view))))))

(defmethod content-view ((view content-view-mixin))
  (assert (eql (cocoa-ref (slot-value view 'content-view))
               (dcc (#/contentView (cocoa-ref view)))))
  (slot-value view 'content-view))

(defmethod (setf content-view) (new-content-view (view content-view-mixin))
  (setf (slot-value view 'content-view) new-content-view)
  (dcc (#/setContentView: (cocoa-ref view) (cocoa-ref new-content-view))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the actual views (when adding a new class,
;;; consider *view-class-to-ns-class-map*):

(defclass view (easy-cocoa-object)
     ((position :initarg :position :reader view-position)
      (size :initarg :size :reader view-size)
      (frame-inited-p :initform nil)))

(defclass window (content-view-mixin view-text-via-title-mixin view)
     ((text :initarg :title :initform "" :reader window-title)
      (zoomable-p :initarg :zoomable-p :initform t :reader window-zoomable-p)
      (minimizable-p :initarg :minimizable-p :initform t
                     :reader window-minimizable-p)
      (resizable-p :initarg :resizable-p :initform t
                   :reader window-resizable-p)
      (closable-p :initarg :closable-p :initform t :reader window-closable-p)))

(defclass static-text-view (view view-text-via-stringvalue-mixin) ())

(defclass text-input-view (view editable-mixin view-text-via-stringvalue-mixin
                                ;; XXX: requires NSTextView, but this is an
                                ;; NSTextField:
                                #+not-yet one-selection-mixin)
     ((input-locked-p :initform nil :initarg :input-locked-p
                      :reader text-input-locked-p)))

(defclass password-input-view (text-input-view)
     ())

(defclass push-button-view (view view-text-via-title-mixin)
     ((default-button-p :initarg :default-button-p :initform nil
                        :reader default-button-p)))

(defclass form-view (view)
     ((autosize-cells-p :initarg :autosize-cells-p :initform nil)
      (interline-spacing :initarg :interline-spacing :initform 9)
      ;; cell width
      ))

(defclass form-cell-view (view editable-mixin view-text-via-stringvalue-mixin)
     ())

(defclass box-view (content-view-mixin view-text-via-title-mixin view) ())

(defclass drawing-view (view)
     (
      ;; TODO: make this a mixin
      (accept-key-events-p :initform nil :initarg :accept-key-events-p
                           :accessor accept-key-events-p)))

(defclass slider-view (view numeric-value-mixin)
     ((max-value :initarg :max-value)
      (min-value :initarg :min-value)
      (tick-mark-count :initarg :tick-mark-count)
      (discrete-tick-marks-p :initarg :discrete-tick-marks-p)))

(defparameter *view-class-to-ns-class-map*
              '((static-text-view . ns:ns-text-field)
                (password-input-view . ns:ns-secure-text-field)
                (text-input-view . ns:ns-text-field)
                (push-button-view . ns:ns-button)
                (form-view . ns:ns-form)
                (form-cell-view . ns:ns-form-cell)
                (box-view . ns:ns-box)
                (drawing-view . cocoa-drawing-view)
                (slider-view . ns:ns-slider)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; view initialization:

(defmethod shared-initialize :around ((view view) new-slots &rest initargs)
  (declare (ignore new-slots initargs))
  (call-next-method)
  (running-on-main-thread ()
    (initialize-view view)))

(defmethod initialize-view ((view view))
  "Initializes the view via the class-to-ns-class map."
  (when (slot-boundp view 'ref)
    (return-from initialize-view nil))
  (let ((ns-view-class (cdr (assoc (class-name (class-of view))
                                   *view-class-to-ns-class-map*
                                   :test #'subtypep))))
    (when ns-view-class
      (setf (cocoa-ref view)
            (cond
              ((and (slot-boundp view 'position)
                    (slot-boundp view 'size))
               (setf (slot-value view 'frame-inited-p) t)
               (make-instance ns-view-class
                  :with-frame (with-slots (position size) view
                                 (ns-rect-from-points position size))))
              (t (make-instance ns-view-class)))))))

(defmethod initialize-view ((win window))
  "Initialize size, title, flags."
  (with-slots (position size) win
     (let ((content-rect
            (multiple-value-call
                #'ns:make-ns-rect
              (if (slot-boundp win 'position)
                  (values (point-x position) (point-y position))
                  (values *window-position-default-x*
                          *window-position-default-y*))
              (if (slot-boundp win 'size)
                  (values (point-x size) (point-y size))
                  (values *window-size-default-x*
                          *window-size-default-y*))))
           (style-mask (logior ;; (flag-mask :zoomable-p (zoomable-p win))
                        (flag-mask :resizable-p
                                   (window-resizable-p win))
                        (flag-mask :minimizable-p
                                   (window-minimizable-p win))
                        (flag-mask :closable-p
                                   (window-closable-p win))
                        #$NSTitledWindowMask)))
       (setf (cocoa-ref win) (make-instance 'ns:ns-window
                                :with-content-rect content-rect
                                :style-mask style-mask
                                :backing #$NSBackingStoreBuffered ; TODO?
                                :defer nil)))))

(defmethod initialize-view :after ((view text-input-view))
  (setf (editable-p view) (not (text-input-locked-p view))))

(defmethod initialize-view :after ((view static-text-view))
  (dcc (#/setEditable: (cocoa-ref view) nil))
  (dcc (#/setBordered: (cocoa-ref view) nil))
  (dcc (#/setBezeled: (cocoa-ref view) nil))
  (dcc (#/setDrawsBackground: (cocoa-ref view) nil)))

(defmethod initialize-view :after ((view push-button-view))
  (dcc (#/setBezelStyle: (cocoa-ref view) #$NSRoundedBezelStyle))
  (let ((default-button-p (slot-value view 'default-button-p)))
    (typecase default-button-p
      (cons
       (dcc (#/setKeyEquivalent: (cocoa-ref view) (string
                                                   (first default-button-p))))
       (dcc (#/setKeyEquivalentModifierMask:
         (cocoa-ref view)
         (apply #'logior (mapcar #'key-mask (cdr default-button-p))))))
      (string
       (dcc (#/setKeyEquivalent: (cocoa-ref view) default-button-p)))
      (null)
      (t
       (dcc (#/setKeyEquivalent: (cocoa-ref view) (ccl::@ #.(string #\return))))))))

(defmethod initialize-view :after ((view form-view))
  (when (slot-boundp view 'interline-spacing)
    (dcc (#/setInterlineSpacing: (cocoa-ref view)
                             (coerce (slot-value view 'interline-spacing)
                                     'double-float)))))

(defmethod initialize-view :after ((view slider-view))
  (with-slots (discrete-tick-marks-p tick-mark-count min-value max-value) view
     (cond 
       #| BUG: tick-mark-values is not defined.
       ((and (not (slot-boundp view 'tick-mark-count))
       (slot-boundp view 'discrete-tick-marks-p)
       (/= (length tick-mark-values) tick-mark-count))
       (error "Incompatible tick mark specification: ~A doesn't match ~
       count of ~A" tick-mark-values tick-mark-values))|#
       ((or (not (slot-boundp view 'max-value))
            (not (slot-boundp view 'min-value)))
        (error "A slider view needs both :min-value and :max-value set.")))
     (dcc (#/setMinValue: (cocoa-ref view) (float min-value ns:+cgfloat-zero+)))
     (dcc (#/setMaxValue: (cocoa-ref view) (float max-value ns:+cgfloat-zero+)))
     (when (slot-boundp view 'tick-mark-count)
       (dcc (#/setNumberOfTickMarks: (cocoa-ref view) tick-mark-count))
       (dcc (#/setAllowsTickMarkValuesOnly:
             (cocoa-ref view) (not (not discrete-tick-marks-p)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; view hierarchies:

(defmethod add-1-subview :around ((view view) (cw-view content-view-mixin))
  (add-1-subview view (content-view cw-view)))

(defmethod add-1-subview :around ((view view) (super-view view))
  "Correctly initialize view positions"
  (call-next-method)
  (with-slots (position size frame-inited-p) view
     (unless frame-inited-p
       (dcc (#/setFrameOrigin: (cocoa-ref view)
                               (ns:make-ns-point (point-x position)
                                                 (point-y position))))
       (if (slot-boundp view 'size)
           (dcc (#/setFrameSize: (cocoa-ref view)
                                 (ns:make-ns-point (point-x size)
                                                   (point-y size))))
           (dcc (#/sizeToFit (cocoa-ref view)))))
     (dcc (#/setNeedsDisplay: (cocoa-ref view) t))
     (dcc (#/setNeedsDisplay: (cocoa-ref super-view) t))))

(defmethod add-1-subview ((view view) (super-view view))
  (dcc (#/addSubview: (cocoa-ref super-view) (cocoa-ref view))))

(defun add-subviews (superview subview &rest subviews)
  (add-1-subview subview superview)
  (dolist (subview subviews)
    (add-1-subview subview superview))
  superview)

(defmethod remove-1-subview ((view view) (cw-view content-view-mixin))
  (remove-1-subview view (content-view cw-view)))

(defmethod remove-1-subview ((view view) (super-view view))
  (assert (eql (cocoa-ref super-view) (#/superview (cocoa-ref view))))
  (maybe-invalidating-object (view)
    (#/removeFromSuperview (cocoa-ref view))))

(defun remove-subviews (superview subview &rest subviews)
  (remove-1-subview subview superview)
  (dolist (subview subviews)
    (remove-1-subview subview superview))
  superview)

(defmethod window-show ((window window))
  (dcc (#/makeKeyAndOrderFront: (cocoa-ref window) nil))
  window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forms:

(defmethod add-entry (entry (view form-view))
  (make-instance 'form-cell-view
     :cocoa-ref (dcc (#/addEntry: (cocoa-ref view) entry))))

(defun add-entries (view &rest entries)
  (prog1 (mapcar (lambda (entry) (add-entry entry view)) entries)
         (dcc (#/setAutosizesCells: (cocoa-ref view)
                                    (slot-value view 'autosize-cells-p)))))

(defmethod cell-count ((view form-view))
  (dcc (#/numberOfRows (cocoa-ref view))))

(defmethod nth-cell (index view)
  (assert (< index (cell-count view)))
  (let ((cocoa-cell (dcc (#/cellAtIndex: (cocoa-ref view) index))))
    (when cocoa-cell
      (make-instance 'form-cell-view :cocoa-ref cocoa-cell))))

(defmethod (setf entry-text) (text view index)
  (setf (view-text (nth-cell index view)) text))

(defmethod entry-text (view index)
  (view-text (nth-cell index view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Drawing:

(defclass cocoa-drawing-view (ns:ns-view)
     ((easygui-view :initarg :eg-view :reader easygui-view-of))
  (:metaclass ns:+ns-view))

(defmethod initialize-view :after ((view drawing-view))
  (setf (slot-value (cocoa-ref view) 'easygui-view) view))

(objc:defmethod (#/drawRect: :void) ((self cocoa-drawing-view)
                                     (rect :<NSR>ect))
  (draw-view-rectangle (easygui-view-of self) (nsrect-rectangle rect)))

(objc:defmethod (#/acceptsFirstReponder: :boolean) ((view cocoa-drawing-view))
  (accept-key-events-p (easygui-view-of view)))

(defgeneric draw-view-rectangle (view rectangle)
  (:method ((view drawing-view) rectangle)
    (declare (ignore view rectangle))
    nil))

(defmethod redisplay ((view drawing-view)
                      &key rect)
  (setf rect (if rect
                 (rectangle-nsrect rect)
                 (#/bounds (cocoa-ref view))))
  (#/setNeedsDisplayInRect: (cocoa-ref view) rect))

(define-useful-mouse-event-handling-routines cocoa-drawing-view)
