(in-package :easygui)

; ----------------------------------------------------------------------
; This is the Clozure Common Lisp file named 'views.lisp', March 2009,
; in the folder ccl/examples/cocoa/easygui/
; It has been modified by AWSC (arthur.cater@ucd.ie), based upon
; an earlier contribution by an unknown author,  borrowing also from
; the 'Seuss.lisp' contribution of 'KD'.
; Permission to use, further modify, disseminate, is hereby granted.
; No warranty is expressed or implied.
; Suggestions for - or accomplishment of - further improvement are welcome.
; Accompanying documentation for this and related files will be written
; and placed in ccl/examples/cocoa/easygui/documentation.txt
; Testing has been only with Mac OS 10.5.6 on a 32 bit PPC
; A demo of some capabilities is in 'easygui-demo-2.lisp'
; ----------------------------------------------------------------------
; It extends previous work in the following principal ways:
; - windows, views and subviews may have nicknames
; - checkboxes and radio-buttons are provided
; - menus (pop-up, pull-down, contextual, and main-menu) are provided
; - MCL-like coordinates (Y increases downward) may optionally be used
;   for placing windows on the screen, placing subviews within windows,
;   and graphics within drawing views.
; - views can generally respond to mouse entry, exit, movement
; - static text views can respond to mouse clicks
; - text views can have colored text and colored background
; - windows can decline to close, and/or invoke daemons upon closing.
; - views and windows can have specific OBJC subclassed counterparts
; - Shift, Command, Control and Option keys may be interrogated
; ----------------------------------------------------------------------

(defmacro running-on-this-thread ((&key (waitp t)) &rest body)
;; The purpose of this trivial macro is to mark places where it is thought possible that
;; it may be preferable to use running-on-main-thread.
  (declare (ignore waitp))
  `(progn ,@body))


(defparameter *screen-flipped* nil
"When NIL, window positions are taken as referring to their bottom left,
as per Cocoa's native coordinate system.
When non-NIL, window positions are taken to refer to their top left,
as per - for instance - Digitool's MCL.
The default orientation for graphics within a drawing view is set to
correspond at the time of creation of that drawing view.")

(defvar *cocoa-event* nil "Allows SHIFT-KEY-P & friends to operate on mouse clicks")

(defvar *suppress-window-flushing* nil "
When T, graphics output produced with calls to With-Focused-View will not be immediately
flushed. This can reduce flicker and increase speed when there are many related uses of
With-Focused-View. It is then necessary though to make sure that somebody somewhere
calls Flush-Graphics at an appropriate time.
The same effect can be obtained for an individual use of With-Focused-View by giving
:WITHOUT-FLUSH as the first form in its body.")

(defvar *report-flipping-errors* nil "
This variable determines whether messages are printed when an error occurs inside the
Obj-C methods named 'isFlipped'. Intermittent errors have been happening, having
something to do with SLOT-VECTORs of instances of NSObjects not having the expected
number of elements. When it is evident that these problems have been fixed, this
variable should be removed and the two isFlipped methods redefined. For now, it is
useful to have a way to tell that such errors have occurred and carry on regardless.")

(defun ns-point-from-point (eg-point)  ;; probably belongs in new-cocoa-bindings.lisp
  (ns:make-ns-point (point-x eg-point) (point-y eg-point)))

(defmacro with-focused-view (cocoa-view &body forms)
;; From KD's SEUSS.LISP but with added :WITHOUT-FLUSH syntax element
;; If the first of forms is the keyword :WITHOUT-FLUSH, or if dynamically
;; the value of *suppress-window-flushing* is non-NIL, then graphics output is not
;; immediately flushed.
  (let ((noflush (eq (first forms) ':without-flush)))
    `(if (dcc (#/lockFocusIfCanDraw ,cocoa-view))
       (unwind-protect
           (progn ,@forms)
         (dcc (#/unlockFocus ,cocoa-view))
         ,(unless noflush
            `(unless *suppress-window-flushing* (flush-graphics ,cocoa-view)))))))

(defun flush-graphics (cocoa-view)
  (running-on-this-thread ()
    (dcc (#/flushGraphics (#/currentContext ns:ns-graphics-context)))
    (dcc (#/flushWindow (#/window cocoa-view)))))

(defun cocoa-null (ptr)
  (equalp ptr ccl:+null-ptr+))




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
;;;
;;; Some view classes have an associated 'value', which can be accessed and set through
;;; accessors STRING-VALUE-OF, INTEGER-VALUE-OF, FLOAT-VALUE-OF, DOUBLE-VALUE-OF
;;; Such classes include STATIC-TEXT-VIEW, TEXT-INPUT-VIEW, PASSWORD-INPUT-VIEW, FORM-CELL-VIEW, SLIDER-VIEW
;;;
;;; Some view classes have an associated 'title', accessible and settable through VIEW-TEXT
;;; Such classes include WINDOW, PUSH-BUTTON-VIEW, BOX-VIEW, RADIO-BUTTON-VIEW, CHECK-BOX-VIEW, MENU-VIEW, MENU-ITEM-VIEW
;;;
;;; Some view classes have an associated 'text', also accessible and settable through VIEW-TEXT
;;; Such classes include STATIC-TEXT-VIEW, TEXT-INPUT-VIEW, PASSWORD-INPUT-VIEW, FORM-CELL-VIEW
;;;
;;; Most of those, apart from STATIC-TEXT-VIEW, may be manually 'editable'.
;;;
;;; Some view classes have an associated 'action'.
;;; Such classes include PUSH-BUTTON-VIEW, SLIDER-VIEW, RADIO-BUTTON-VIEW, CHECK-BOX-VIEW, MENU-ITEM-VIEW
;;;
;;; Some view classes cannot ever have a contextual menu attached to them, even though their superview
;;; and their subviews (if any) possibly do.
;;; Such classes include PUSH-BUTTON-VIEW, RADIO-BUTTON-VIEW, CHECK-BOX-VIEW, MENU-VIEW, MENU-ITEM-VIEW
;;; Perhaps these should be the same classes as those with actions.
;;;
;;; No view classes inherit from 'one-selection-mixin'
;;; Apparently it was intended that TEXT-INPUT-VIEW might do so some day.
;;;
;;; Some view classes have a single 'content view'.
;;; Such classes include WINDOW, BOX-VIEW.
;;;
;;; Some view classes inherit from 'background-coloring-mixin'
;;; Such classes include STATIC-TEXT-VIEW ... for now
;;;
;;; Some view classes inherit from 'text-coloring-mixin'
;;; Such classes include ...
;;;
;;; Some view classes inherit from 'fonting-mixin'
;;; Such classes include ...
;;;
;;; Some view classes inherit from 'mouse-updownabout-mixin'
;;; Such classes include ...
;;;
;;; Some view classes inherit from 'mouse-tracking-mixin'
;;; Such classes include ...



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mixins dealing with text string and numeric equivalents

(defclass value-mixin () ())

(defclass string-value-mixin (value-mixin) ())

(defclass numeric-value-mixin (value-mixin) ())

(defclass view-text-mixin ()
     ((text :initarg :text :initarg :dialog-item-text)))

(defclass view-text-via-stringvalue-mixin (view-text-mixin string-value-mixin)
     ())

(defclass view-text-via-title-mixin (view-text-mixin)
     ((text :initarg :title)))

(macrolet ((def-type-accessor (class lisp-type cocoa-reader cocoa-writer
                                     &key (new-value-form 'new-value) (return-value-converter 'identity))
               (let ((name (intern (format nil "~A-VALUE-OF" lisp-type))))
                 `(progn
                    (defmethod ,name ((o ,class))
                      (,return-value-converter (dcc (,cocoa-reader (cocoa-ref o)))))
                    (defmethod (setf ,name) (new-value (o ,class))
                      (dcc (,cocoa-writer (cocoa-ref o) ,new-value-form)))))))
  (def-type-accessor string-value-mixin string   #/stringValue #/setStringValue:
    :return-value-converter lisp-string-from-nsstring )
  (def-type-accessor numeric-value-mixin integer #/intValue #/setIntValue:)
  (def-type-accessor numeric-value-mixin float   #/floatValue #/setFloatValue:
    :new-value-form (coerce new-value 'single-float))
  (def-type-accessor numeric-value-mixin double  #/doubleValue #/setDoubleValue:
    :new-value-form (coerce new-value 'double-float)))

(defmethod view-text ((view view-text-via-stringvalue-mixin))
  (string-value-of view))

(defmethod view-text ((view view-text-via-title-mixin))
  (lisp-string-from-nsstring (dcc (#/title (cocoa-ref view)))))

(defmethod (setf view-text) (new-text (view view-text-via-stringvalue-mixin))
  (setf (string-value-of view) (ccl::%make-nsstring new-text)))

(defmethod (setf view-text) (new-text (view view-text-via-title-mixin))
  (dcc (#/setTitle: (cocoa-ref view) (ccl::%make-nsstring new-text)))
  new-text)

(defmethod initialize-view :after ((view view-text-mixin))
  (when (slot-boundp view 'text)
    (setf (view-text view) (slot-value view 'text))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mixins dealing with mouse sensitivity (1)

(defclass action-view-mixin ()
  ((action :initarg :action)
   (enabled :accessor dialog-item-enabled-p :initarg :dialog-item-enabled-p :initform t)))

(defclass decline-menu-mixin () ())

(defmethod set-dialog-item-enabled-p ((view action-view-mixin) value)
  (unless (eq (not value) (not (dialog-item-enabled-p view)))
    (setf (dialog-item-enabled-p view) value)
    (dcc (#/setEnabled: (cocoa-ref view) (if value #$YES #$NO)))))

(defmethod initialize-view :after ((view action-view-mixin))
  (when (and (slot-boundp view 'action) (slot-value view 'action))
    (setf (action view) (slot-value view 'action)))
  (unless (dialog-item-enabled-p view)
    (dcc (#/setEnabled: (cocoa-ref view) #$NO))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mixins dealing with text properties - font, foreground and background colors, editability

(defclass text-coloring-mixin () ())

(defclass text-fonting-mixin () ())

(defclass editable-mixin () ())

(defclass background-coloring-mixin ()
  ((drawsbackground     :initform t :initarg :draws-background)))

(defmethod initialize-view :after ((view background-coloring-mixin))
  (dcc (#/setDrawsBackground: (cocoa-ref view) (slot-value view 'drawsbackground)))
  (when (and (cocoa-ref view) (slot-boundp view 'background))
      (dcc (#/setBackgroundColor: (cocoa-ref view) (slot-value view 'background)))))

(defmethod editable-p ((view editable-mixin))
  (dcc (#/isEditable (cocoa-ref view))))

(defmethod (setf editable-p) (editable-p (view editable-mixin))
  (check-type editable-p boolean)
  (dcc (#/setEditable: (cocoa-ref view) editable-p))
  editable-p)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mixin dealing with selection: Possibly obsolete?

(defclass one-selection-mixin () ())

(defmethod (setf selection) (selection (view one-selection-mixin))
  (dcc (#/setSelectedRange: (cocoa-ref view) (range-nsrange selection)))
  selection)

(defmethod selection ((view one-selection-mixin))
  (let ((range (dcc (#/selectedRange (cocoa-ref view)))))
    (if (= (ns:ns-range-location range) #$NSNotFound)
        nil
        (range (ns:ns-range-location range)
               (ns:ns-range-length range)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mixin for content views: window, box, perhaps others.

(defclass content-view-mixin ()
  ((content-view)
   (flipped :initarg :flipped :initform *screen-flipped*)))

(defmethod initialize-view :after ((view content-view-mixin))
  (unless (slot-boundp view 'content-view)
    (let ((containee (make-instance 'contained-view
                       :cocoa-ref (dcc (#/contentView (cocoa-ref view)))
                       :view-nick-name '%CONTENT-OF-CONTENT-VIEW%
                       :flipped (slot-value view 'flipped))))
      (setf (slot-value view 'content-view) containee
            (slot-value containee 'parent) view))))

(defmethod (setf content-view) (new-content-view (view content-view-mixin))
  (setf (slot-value view 'content-view) new-content-view)
  (dcc (#/setContentView: (cocoa-ref view) (cocoa-ref new-content-view)))
  new-content-view)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mixin for views that can respond to mouse entry, exit, and movement

(defclass mouse-tracking-mixin ()
  ((mouse-target :reader view-mouse-target :initform nil)
   (mouse-enter :accessor view-mouse-enter :initarg :mouse-enter :initform nil)
   (mouse-exit :accessor view-mouse-exit :initarg :mouse-exit :initform nil)
   (mouse-move :accessor view-mouse-move :initarg :mouse-move :initform nil)))

(defclass easygui-mouse-target (ns:ns-object)
  ((view :initarg :view :reader mouse-target-view :initform nil))
  (:metaclass ns:+ns-object))

(defmethod initialize-view :after ((view mouse-tracking-mixin))
  (let ((cocoaview (cocoa-ref view)))
   (when cocoaview
      (let ((target (make-instance 'easygui-mouse-target :view view)))
        (setf (slot-value view 'mouse-target) target)
        (dcc (#/retain target))
        (dcc (#/addTrackingRect:owner:userData:assumeInside:
         cocoaview
         (dcc (#/bounds cocoaview))
         target
         ccl:+null-ptr+
         #$NO))))))

(objc:define-objc-method ((:void :mouse-entered (:id event)) easygui-mouse-target)
  (let* ((view (mouse-target-view self))
         (fn (view-mouse-enter view)))
    (when fn (funcall fn view :event event :allow-other-keys t))))

(objc:define-objc-method ((:void :mouse-exited (:id event)) easygui-mouse-target)
  (let* ((view (mouse-target-view self))
         (fn (view-mouse-exit view)))
    (when fn (funcall fn view :event event :allow-other-keys t))))

(objc:define-objc-method ((:void :mouse-move (:id event)) easygui-mouse-target)
  (let* ((view (mouse-target-view self))
         (fn (view-mouse-move view)))
    (when fn (funcall fn view :event event :allow-other-keys t))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class for the sort of view that is contained by a content view.

(defclass contained-view (view)
  ((flipped :initarg :flipped)))

(defmethod content-view ((view content-view-mixin))
  (assert (eql (cocoa-ref (slot-value view 'content-view))
               (dcc (#/contentView (cocoa-ref view)))))
  (slot-value view 'content-view))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the actual views (when adding a new class,
;;; consider *view-class-to-ns-class-map*):

(defclass view (easy-cocoa-object)
     ((position :initarg :position :reader view-position)
      (size :initarg :size :reader view-size)
      (frame-inited-p :initform nil)
      (parent :reader view-container :initform nil)
      (subviews :reader view-subviews :initarg :subviews :initform nil)
      ;; When adding/removing multiple subviews, prevent multiple redraws.
      ;; But - what code does those redraws?
      (subviews-busy :accessor view-subviews-busy :initform nil)
      (nickname :accessor view-nick-name :initarg :view-nick-name :initform nil)
      (contextmenu :initarg :contextual-menu :initform nil)
      (background :initarg :back-color :initform (#/whiteColor ns:ns-color))
      (foreground :initarg :fore-color :initform (#/blackColor ns:ns-color))
      (font :reader view-font :initarg :font :initarg :view-font :initform nil)
      (specifically :reader view-specifically :initarg :specifically :initform nil)
      ;; Next three not yet operative
      (tip :initarg :tip :reader view-tip :initform nil)
      (tiptag :initform nil)))

(defclass window (content-view-mixin view-text-via-title-mixin view)
     ((text :initarg :title :initform "" :reader window-title)
      (zoomable-p :initarg :zoomable-p :initform t :reader window-zoomable-p)
      (minimizable-p :initarg :minimizable-p :initform t
                     :reader window-minimizable-p)
      (resizable-p :initarg :resizable-p :initform t
                   :reader window-resizable-p)
      (closable-p :initarg :closable-p :initform t :reader window-closable-p)
      (level :initarg :window-level :accessor window-level
             :initform #+cocotron 0
	               #-cocotron (dcc (#_CGWindowLevelForKey #$kCGNormalWindowLevelKey)))
      (hidden :initarg :hidden :reader window-hidden :initform nil)
      (window-needs-display-on-show :initform t)
      (optimized :initarg :optimized :initform t) ; Set to NIL if you anticipate overlapping views in this window
      (style :initarg :window-style :initform #$NSTitledWindowMask))
  (:default-initargs :specifically 'cocoa-contained-view))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defmethod clear-page ((view view))
  (let* ((cview (cocoa-ref view))
         (rect (dcc (#/bounds cview)))
         (color (slot-value view 'background)))
    (with-focused-view cview
      (dcc (#/setFill color))
      (dcc (#_NSRectFill rect)))))

(defmethod clear-page ((window content-view-mixin))
  (clear-page (content-view window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defclass static-text-view (view view-text-via-stringvalue-mixin action-view-mixin text-coloring-mixin text-fonting-mixin background-coloring-mixin mouse-tracking-mixin)
  ((mousedown           :initform nil :initarg :mouse-down    :accessor static-text-view-mouse-down)
   (mouseup             :initform nil :initarg :mouse-up      :accessor static-text-view-mouse-up)
   (mousedragged        :initform nil :initarg :mouse-dragged :accessor static-text-view-mouse-dragged)))

(defclass text-input-view (view editable-mixin text-coloring-mixin text-fonting-mixin view-text-via-stringvalue-mixin
                                ;; XXX: requires NSTextView, but this is an
                                ;; NSTextField:
                                #+not-yet one-selection-mixin
                                mouse-tracking-mixin)
     ((input-locked-p :initform nil :initarg :input-locked-p
                      :reader text-input-locked-p)))

(defclass password-input-view (text-input-view)
     ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defclass push-button-view (view view-text-via-title-mixin action-view-mixin decline-menu-mixin)
     ((default-button-p :initarg :default-button-p :initform nil
                        :reader default-button-p)
      (bezelstyle       :reader bezel-style        :initarg :bezel-style      :initform :rounded)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defclass form-view (view)
     ((autosize-cells-p :initarg :autosize-cells-p :initform nil)
      (interline-spacing :initarg :interline-spacing :initform 9)
      ;; cell width
      ))

(defclass form-cell-view (view editable-mixin view-text-via-stringvalue-mixin)
     ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defclass box-view (content-view-mixin view-text-via-title-mixin view) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defclass drawing-view (view mouse-tracking-mixin)
     (
      ;; TODO: make this a mixin
      (accept-key-events-p :initform nil :initarg :accept-key-events-p
                           :accessor accept-key-events-p)
      (flipped             :initform *screen-flipped* :initarg :flipped :reader flipped-p)
      (mousedown           :initform nil :initarg :mouse-down    :accessor drawing-view-mouse-down)
      (mouseup             :initform nil :initarg :mouse-up      :accessor drawing-view-mouse-up)
      (mousedragged        :initform nil :initarg :mouse-dragged :accessor drawing-view-mouse-dragged)
      (draw-fn             :initform nil :initarg :draw-fn :accessor draw-fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defclass slider-view (view numeric-value-mixin action-view-mixin)
     ((max-value :initarg :max-value)
      (min-value :initarg :min-value)
      (tick-mark-count :initarg :tick-mark-count)
      (tick-mark-values :initarg :tick-mark-values)
      (discrete-tick-marks-p :initarg :discrete-tick-marks-p)))

; ----------------------------------------------------------------------
; Specialisations of ns-xxx classes always begin 'cocoa-'.
; They allow such things as
; - finding the easygui window associated with a ns-view & easygui::view
; - flipped windows, flipped drawing-views
; - clickable static text, editable text fields
; - tooltips
; ----------------------------------------------------------------------

(defun calculate-ns-tooltip (cview)
  ;; Returns a Lisp string to bhe used as a tooltip, or NIL.
  ;; Easygu Views may or may not be created with a specific :TIP keyword argument.
  ;; If there is none, there will be no tooltip displayed for the corresponding cocoa-view.
  ;; Otherwise, if the argument is
  ;;   - a string, that string is used
  ;;   - a function, then if its return value is
  ;;        - a string, that string is used
  ;;        - NIL, a string informing that the tooltip is null and cocoa-describing the cocoa-view
  ;;               (possibly useful for identifying this view if it turns up in errors or inspector)
  ;;        - else a string naming the type of the result returned (possibly useful for debugging)
  ;;   - the keyword :IDENTIFY, the cocoa-description of the cocoa-view
  ;;   - anything else, a string informing what type the argument is.
  (let* ((egview (when (slot-boundp cview 'easygui-view) (slot-value cview 'easygui-view)))
         (tip (when egview (slot-value egview 'tip))))
    (cond
     ((stringp tip)
      tip)
     ((functionp tip)
      (let ((it (funcall tip)))
        (cond
         ((stringp it)  it)
         ((null it)     (format nil "Null tooltip for ~a" (lisp-string-from-nsstring (dcc (#/description cview)))))
         (t (format nil "** Tooltip function returned non-string object of type ~s **" (type-of it))))))
     ((eq tip :identify) (lisp-string-from-nsstring (dcc (#/description cview))))
     ((null egview) 
      (format nil "** Cocoa view ~s has no EasyGui-View **" cview))
     ((null tip) (format nil "No tooltip for ~a" (lisp-string-from-nsstring (dcc (#/description cview)))))
     (t (format nil "** Tip slot of Cocoa view ~s~%is of type ~s,~%not a string or a function or :IDENTIFY. **" cview tip)))))

(defmacro define-tooltip-accessor (cocoa-class)
  `(progn
     (objc:defmethod #/view:stringForToolTip:point:userData:
                     ((self ,cocoa-class)
		      view
                      (tag :<NST>ool<T>ip<T>ag)
                      (point :<NSP>oint)
                      (userdata :address))
       (declare (ignorable tag point userdata))
       (ccl::%make-nsstring (or (calculate-ns-tooltip view) "")))
     (objc:defmethod #/toolTip ((view ,cocoa-class))
       (ccl::%make-nsstring (or (calculate-ns-tooltip view) "")))))

(defclass cocoa-window (ns:ns-window)
  ((easygui-window :reader easygui-window-of))
  (:metaclass ns:+ns-object))

(defmethod print-object ((object cocoa-window) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((egview (if (slot-boundp object 'easygui-window) (easygui-window-of object) nil)))
      (format stream "[~:[~;~s~]]" egview (when egview (view-nick-name egview)))))
  object)

(defmethod easygui-window-of ((eview view))
  (if (cocoa-ref eview) (easygui-window-of (cocoa-ref eview)) nil))

(defmethod easygui-window-of ((nsview ns:ns-view))
  (let ((nswindow (dcc (#/window nsview))))
    (if (typep nswindow 'cocoa-window) (easygui-window-of nswindow) nil)))

(defclass cocoa-extension-mixin ()
  ((easygui-view :initarg :eg-view :reader easygui-view-of)))

(defmethod print-object ((object cocoa-extension-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((egview (if (slot-boundp object 'easygui-view) (easygui-view-of object) nil)))
      (format stream "[~:[~;~s~]]" egview (when egview (view-nick-name egview)))))
  object)

(defclass cocoa-text-field (cocoa-extension-mixin ns:ns-text-field) ()
  (:metaclass ns:+ns-object))

(define-tooltip-accessor cocoa-text-field)

(defclass cocoa-mouseable-text-field (cocoa-text-field) ()
  (:metaclass ns:+ns-object))

(define-tooltip-accessor cocoa-mouseable-text-field)

(defclass cocoa-contained-view (cocoa-extension-mixin ns:ns-view)
  ((flipped :initarg :flipped :initform *screen-flipped*))
  (:metaclass ns:+ns-object))

(define-tooltip-accessor cocoa-contained-view)

(defclass cocoa-secure-text-field (cocoa-extension-mixin ns:ns-secure-text-field) ()
  (:metaclass ns:+ns-object))

(define-tooltip-accessor cocoa-secure-text-field)

(defclass cocoa-button (cocoa-extension-mixin ns:ns-button) ()
  (:metaclass ns:+ns-object))

(define-tooltip-accessor cocoa-button)

(defclass cocoa-pop-up-button (cocoa-extension-mixin ns:ns-pop-up-button) ()
  (:metaclass ns:+ns-object))

(define-tooltip-accessor cocoa-pop-up-button)

(defclass cocoa-menu-item (cocoa-extension-mixin ns:ns-menu-item) ()
  (:metaclass ns:+ns-object))

(define-tooltip-accessor cocoa-menu-item)

(defclass cocoa-form (cocoa-extension-mixin ns:ns-form) ()
  (:metaclass ns:+ns-object))

(define-tooltip-accessor cocoa-form)

(defclass cocoa-form-cell (cocoa-extension-mixin ns:ns-form-cell) ()
  (:metaclass ns:+ns-object))

(define-tooltip-accessor cocoa-form-cell)

(defclass cocoa-box (cocoa-extension-mixin ns:ns-box) ()
  (:metaclass ns:+ns-object))

(define-tooltip-accessor cocoa-box)

(defclass cocoa-drawing-view (cocoa-extension-mixin ns:ns-view)
  ((flipped :initarg :flipped :initform *screen-flipped*))
  (:metaclass ns:+ns-object))

(define-tooltip-accessor cocoa-drawing-view)

(defclass cocoa-slider (cocoa-extension-mixin ns:ns-slider) ()
  (:metaclass ns:+ns-object))

(define-tooltip-accessor cocoa-slider)

(defparameter *view-class-to-ns-class-map*
              '((static-text-view     . cocoa-mouseable-text-field)
                (password-input-view  . cocoa-secure-text-field)
                (text-input-view      . cocoa-text-field)
                (push-button-view     . cocoa-button)
                (check-box-view       . cocoa-button)
                (radio-button-view    . cocoa-button)
                (menu-view            . cocoa-pop-up-button)
                (menu-item-view       . cocoa-menu-item)
                (form-view            . cocoa-form)
                (form-cell-view       . cocoa-form-cell)
                (box-view             . cocoa-box)
                (drawing-view         . cocoa-drawing-view)
                (slider-view          . cocoa-slider)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; view initialization:

(defmethod shared-initialize :around ((view view) new-slots &rest initargs)
  (declare (ignore new-slots initargs))
  (call-next-method)
  (running-on-main-thread ()
    (initialize-view view)))

(defmethod initialize-view ((view view))
  "Initializes the view using the class-to-ns-class map both as constraint
on valid values of the :SPECIFICALLY initarg, and as source of default value.
Also attaches contextual menu if there is one."
  (when (slot-boundp view 'ref)
    (return-from initialize-view nil))
  (let ((ns-view-class (cdr (assoc (class-name (class-of view))
                                   *view-class-to-ns-class-map*
                                   :test #'subtypep)))
        (specifically (view-specifically view))
        cocoaview)
    (when specifically
      (cond
       ((not (find-class specifically nil))
        (cerror "Ignore ~a and use ~a default" "~a value for :SPECIFICALLY does not name a class" specifically ns-view-class))
       ((or (null ns-view-class) (subtypep specifically ns-view-class))
        (setf ns-view-class specifically))
       (t (cerror "Ignore ~a and use ~a default" "~a value for :SPECIFICALLY is not a subclass of ~a" specifically ns-view-class))))
    (if ns-view-class
      (setf cocoaview
            (cond
              ((and (slot-boundp view 'position)
                    (slot-boundp view 'size))
               (setf (slot-value view 'frame-inited-p) t)
               (make-instance ns-view-class
                  :with-frame (with-slots (position size) view
                                 (ns-rect-from-points position size))))
              (t (make-instance ns-view-class)))
            (cocoa-ref view) cocoaview)
      (cerror "Continue with cocoa-ref unset" "No view class found for type ~a" (class-of view)))
    (when (and cocoaview (slot-boundp view 'contextmenu))
      (let ((menu (slot-value view 'contextmenu)))
        (cond
         ((null menu))
         ((null ns-view-class))
         ((typep menu 'menu-view)
          (dcc (#/setMenu: cocoaview (slot-value menu 'ns-menu))))
         (t (warn "Ignoring contextmenu value ~s for view ~s" menu view)))))
   (when (and cocoaview
              (slot-value view 'tip)
              (dcc (#/respondsToSelector: cocoaview (\@selector #/bounds))))
     (let ((bounds (#/bounds cocoaview)))
       (setf (slot-value view 'tiptag)
             (dcc (#/addToolTipRect:owner:userData: cocoaview bounds cocoaview ccl:+null-ptr+)))))))

(defun screen-height nil
  (running-on-this-thread ()
    (ns:ns-rect-height (dcc (#/frame (#/objectAtIndex: (#/screens ns:ns-screen) 0))))))

(defmethod view-content-rect ((view view) &optional hidden)
  (if hidden
    (ns:make-ns-rect 0 0 0 0)
    (with-slots (position size) view
      ;(if (slot-boundp view 'size)
      ;  (format t "~&View ~s has size ~s~%" view size)
      ;  (format t "~&View ~s has size unbound~%" view))
      (let* ((height (if (slot-boundp view 'size) (point-y size) *window-size-default-y*))
             (stated (if (slot-boundp view 'position) (point-y position) *window-position-default-y*))
             (screentop (screen-height))  ;; TODO: dtrt for multiple screens
             (bottom (if (and *screen-flipped* (typep view 'window))
                       (- screentop height stated)
                       stated)))
        (ns:make-ns-rect
         (if (slot-boundp view 'position) (point-x position) *window-position-default-x*)
         bottom
         (if (slot-boundp view 'size) (point-x size) *window-size-default-x*)
         height)))))

(defmethod initialize-view ((win window))
  "Initialize size, title, flags."
  (with-slots (level hidden optimized style flipped specifically) win
    (unless (and (find-class specifically nil) (subtypep specifically 'cocoa-contained-view))
      (cerror "Ignore ~a and create content view of type ~a"
              "Value given for \":specifically\" is ~a which is not a subtype of ~a"
              specifically 'cocoa-contained-view)
      (setf specifically 'cocoa-contained-view))
     (let* ((content-rect (view-content-rect win hidden))
            (style-mask (logior ;; (flag-mask :zoomable-p (zoomable-p win))
                         (flag-mask :resizable-p   (window-resizable-p win))
                         (flag-mask :minimizable-p (window-minimizable-p win))
                         (flag-mask :closable-p    (window-closable-p win))
                         (if (or (window-resizable-p win) (window-minimizable-p win) (window-closable-p win))
                           #$NSTitledWindowMask
                           0)
                         style))
            (c-win
             (make-instance 'cocoa-window
               :with-content-rect content-rect
               :style-mask style-mask
               :backing #$NSBackingStoreBuffered ; TODO?
               :defer t))
            (containee (make-instance specifically)))
       (setf (slot-value containee 'flipped) flipped)
       (dcc (#/setFrame: containee content-rect))
       (dcc (#/setContentView: c-win containee))
       (dcc (#/setDelegate: c-win c-win))
       (dcc (#/setBackgroundColor: c-win (slot-value win 'background)))
       (dcc (#/setLevel: c-win level))
       (when optimized (dcc (#/useOptimizedDrawing: c-win #$YES)))
       (setf (cocoa-ref win) c-win)
       (setf (slot-value c-win 'easygui-window) win)
       (if hidden
         (dcc (#/disableFlushWindow c-win))
         (window-show win))
       c-win)))

(defmethod initialize-view :after ((view text-input-view))
  (setf (editable-p view) (not (text-input-locked-p view)))
  (setf (slot-value (cocoa-ref view) 'easygui-view) view))

(defmethod initialize-view :after ((view static-text-view))
  (dcc (#/setEditable: (cocoa-ref view) nil))
  (dcc (#/setBordered: (cocoa-ref view) nil))
  (dcc (#/setBezeled: (cocoa-ref view) nil))
  (setf (slot-value (cocoa-ref view) 'easygui-view) view))

(defparameter *bezelstyle-alist*
  `((:round                    . #.#$NSRoundedBezelStyle)
    (:square                   . #.#$NSRegularSquareBezelStyle)
    (:regular-square           . #.#$NSRegularSquareBezelStyle)
    (:thick-square             . #.#$NSThickSquareBezelStyle)
    (:thicker-square           . #.#$NSThickerSquareBezelStyle)
    (:disclosure               . #.#$NSDisclosureBezelStyle)
    (:Shadowless-square        . #.#$NSShadowlessSquareBezelStyle)
    (:circular                 . #.#$NSCircularBezelStyle)
    (:textured-square          . #.#$NSTexturedSquareBezelStyle)
    (:help-button              . #.#$NSHelpButtonBezelStyle)
    (:small-square             . #.#$NSSmallSquareBezelStyle)
    (:textured-rounded         . #.#$NSTexturedRoundedBezelStyle)
    (:round-rect               . #.#$NSRoundRectBezelStyle)
    (:recessed                 . #.#$NSRecessedBezelStyle)
    (:rounded-disclosure       . #.#$NSRoundedDisclosureBezelStyle)))

(defun bezel-style-lookup (key)
  (rest (or (assoc key *bezelstyle-alist*) (first *bezelstyle-alist*))))

(defmethod (setf bezel-style) (stylename (view push-button-view))
  (setf (slot-value view 'bezelstyle) (if (assoc stylename *bezelstyle-alist*) stylename :round))
  (dcc (#/setBezelStyle: (cocoa-ref view) (bezel-style-lookup (slot-value view 'bezelstyle))))
  stylename)

(defmethod initialize-view :after ((view push-button-view))
  (dcc (#/setBezelStyle: (cocoa-ref view) (bezel-style-lookup (bezel-style view))))
  (let ((default-button-p (slot-value view 'default-button-p)))
    (typecase default-button-p
      (cons
       (dcc (#/setKeyEquivalent: (cocoa-ref view) 
                                 (ccl::%make-nsstring (string (first default-button-p)))))
       (dcc (#/setKeyEquivalentModifierMask:
         (cocoa-ref view)
         (apply #'logior (mapcar #'key-mask (cdr default-button-p))))))
      (string
       (dcc (#/setKeyEquivalent: (cocoa-ref view) (ccl::%make-nsstring default-button-p))))
      (null)
      (t
       (dcc (#/setKeyEquivalent: (cocoa-ref view) (ccl::@ #.(string #\return)))))))
  (setf (slot-value (cocoa-ref view) 'easygui-view) view))

(defmethod initialize-view :after ((view box-view))
  (setf (slot-value (cocoa-ref view) 'easygui-view) view))

(defmethod initialize-view :after ((view form-view))
  (when (slot-boundp view 'interline-spacing)
    (dcc (#/setInterlineSpacing: (cocoa-ref view)
                             (gui::cgfloat (slot-value view 'interline-spacing)))))
  (setf (slot-value (cocoa-ref view) 'easygui-view) view))

(defmethod initialize-view :after ((view slider-view))
  (with-slots (discrete-tick-marks-p tick-mark-count tick-mark-values min-value max-value) view
     (cond ((and (slot-boundp view 'tick-mark-count)
                 (slot-boundp view 'discrete-tick-marks-p)
                 (slot-boundp view 'tick-mark-values)
                 (/= (length tick-mark-values) tick-mark-count))
            (error "Incompatible tick mark specification: ~A doesn't match ~
                     count of ~A" tick-mark-count tick-mark-values))
           ((or (not (slot-boundp view 'max-value))
                (not (slot-boundp view 'min-value)))
            (error "A slider view needs both :min-value and :max-value set.")))
     (dcc (#/setMinValue: (cocoa-ref view) (float min-value (or 1.0d0 ns:+cgfloat-zero+))))
     (dcc (#/setMaxValue: (cocoa-ref view) (float max-value (or 1.0d0 ns:+cgfloat-zero+))))
     (when (slot-boundp view 'tick-mark-count)
       (dcc (#/setNumberOfTickMarks: (cocoa-ref view) tick-mark-count))
       (dcc (#/setAllowsTickMarkValuesOnly:
             (cocoa-ref view) (not (not discrete-tick-marks-p))))))
  (setf (slot-value (cocoa-ref view) 'easygui-view) view))

(defmethod initialize-view :after ((view text-coloring-mixin))
  (dcc (#/setTextColor: (cocoa-ref view) (slot-value view 'foreground))))

(defmethod initialize-view :after ((view text-fonting-mixin))
  (when (slot-value view 'font)
    (dcc (#/setFont: (cocoa-ref view) (slot-value view 'font)))))

(defmethod (setf view-font) ((new ns:ns-font) (view view))
  (setf (slot-value view 'font) new)
  (dcc (#/setFont: (cocoa-ref view) new)))

; ----------------------------------------------------------------------
; Modifying position / size    of    view / window
; ----------------------------------------------------------------------

(defmethod (setf view-position) (point (self view))
  (running-on-main-thread ()
    (setf (slot-value self 'position) point)
    (when (slot-value self 'frame-inited-p)
      (dcc (#/setFrame: (cocoa-ref self) (view-content-rect self)))
      (dcc (#/setNeedsDisplay (cocoa-ref self))))))

(defmethod (setf view-position) (point (self window))
  (running-on-main-thread ()
    (setf (slot-value self 'position) point)
    (unless (window-hidden self)
      (let* ((contentrect (view-content-rect self nil))
             (framerect (dcc (#/frameRectForContentRect: (cocoa-ref self) contentrect))))
        (dcc (#/setFrame:display: (cocoa-ref self) framerect t))))))

(defmethod (setf view-size) (point (self view))
  (running-on-main-thread ()
    (setf (slot-value self 'size) point)
    (when (slot-value self 'frame-inited-p)
      (dcc (#/setFrame: (cocoa-ref self) (view-content-rect self)))
      (dcc (#/setNeedsDisplay (cocoa-ref self))))))

(defmethod (setf view-size) (point (self window))
  (running-on-main-thread ()
    (setf (slot-value self 'size) point)
    (unless (window-hidden self)
      (let* ((contentrect (view-content-rect self nil))
             (framerect (dcc (#/frameRectForContentRect: (cocoa-ref self) contentrect))))
        (dcc (#/setFrame:display: (cocoa-ref self) framerect t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; view hierarchies:

(defmethod set-needs-display ((view view) flag)
  (running-on-this-thread ()
    (dcc (#/setNeedsDisplay: (cocoa-ref view) flag))))

(defmethod set-needs-display ((view content-view-mixin) flag)
  (set-needs-display (content-view view) flag))

(defmethod set-needs-display ((view window) flag)
  (if (window-hidden view)
    (setf (slot-value view 'window-needs-display-on-show) flag)
    (set-needs-display (content-view view) flag)))

(defmethod add-1-subview :around ((view view) (cw-view content-view-mixin))
  (add-1-subview view (content-view cw-view)))

(defmethod add-1-subview :around ((view view) (super-view view))
  "Correctly initialize view positions"
  (call-next-method)
  (with-slots (position size frame-inited-p) view
    (unless frame-inited-p
      (setf frame-inited-p t)
      (running-on-this-thread ()
        (let ((cocoa-view (cocoa-ref view)))
          (dcc (#/setFrameOrigin: cocoa-view (ns-point-from-point position)))
          (if (slot-boundp view 'size)
            (dcc (#/setFrameSize: cocoa-view (ns-point-from-point size)))
            (dcc (#/sizeToFit cocoa-view))))))
    (set-needs-display view t)
    (unless (view-subviews-busy super-view) (set-needs-display super-view t))))

(defmethod add-1-subview ((view view) (super-view view))
  (setf (slot-value view 'parent) super-view)
  (push view (slot-value super-view 'subviews))
  (dcc (#/addSubview: (cocoa-ref super-view) (cocoa-ref view))))

(defun add-subviews (superview subview &rest subviews)
  (running-on-main-thread ()
     (setf (view-subviews-busy superview) t)
     (add-1-subview subview superview)
     (dolist (subview subviews)
       (add-1-subview subview superview))
     (set-needs-display superview t)
     (setf (view-subviews-busy superview) nil))
  superview)

(defmethod remove-1-subview ((view view) (cw-view content-view-mixin))
  (remove-1-subview view (content-view cw-view)))

(defmethod remove-1-subview ((view view) (super-view view))
  (assert (eql (cocoa-ref super-view) (dcc (#/superview (cocoa-ref view)))))
  (assert (member view (view-subviews super-view)))
  (assert (eq super-view (slot-value view 'parent)))
  (maybe-invalidating-object (view)
    (setf (slot-value super-view 'subviews) (delete view (slot-value super-view 'subviews)))
    (setf (slot-value view 'parent) nil)
    (running-on-this-thread ()
      (dcc (#/removeFromSuperview (cocoa-ref view))))))

(defun remove-subviews (superview subview &rest subviews)
  (running-on-main-thread ()
    (setf (view-subviews-busy superview) t)
    (remove-1-subview subview superview)
    (dolist (subview subviews)
      (remove-1-subview subview superview))
    (set-needs-display superview t)
    (setf (view-subviews-busy superview) nil))
  superview)

(defmethod window-show ((window window))
  (running-on-this-thread ()
    (let ((cwin (cocoa-ref window)))
      (when (window-hidden window)
        (setf (slot-value window 'hidden) nil)
        (let* ((contentrect (view-content-rect window nil))
               (framerect (dcc (#/frameRectForContentRect: (cocoa-ref window) contentrect))))
          (dcc (#/setFrame:display: (cocoa-ref window) framerect nil)))
        (when (dcc (#/isMiniaturized cwin)) (dcc (#/deminiaturize: cwin cwin)))
        (when (slot-value window 'window-needs-display-on-show)
          (setf (slot-value window 'window-needs-display-on-show) nil)
          (dcc (#/setNeedsDisplay: (cocoa-ref (content-view window)) t))))
      (dcc (#/makeKeyAndOrderFront: cwin nil))
      (when (dcc (#/isFlushWindowDisabled cwin))
        (dcc (#/enableFlushWindow cwin))
        (dcc (#/flushWindow cwin)))
      window)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forms:

(defmethod add-entry (entry (view form-view))
  (make-instance 'form-cell-view
     :cocoa-ref (dcc (#/addEntry: (cocoa-ref view) (ccl::%make-nsstring entry)))))

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
;;; Window closing
         
(defmethod window-may-close ((w window))
"This generic is intended to allow applications to define :BEFORE and/or :AFTER methods
invoked when windows are closed. The default primary method returns T to indicate that
the window may close. If an overriding primary method returns NIL, the window will not
close in response to user action but will still close if the application quits.
(This is because window-may-close is called when the COCOA-WINDOW (specialised NS:NS-WINDOW)
that is attached to an EASYGUI::WINDOW object receives a performClose: message, as when
a user clicks the close button for example.)"
  (declare (ignore w))
  t)

(defmethod perform-close ((w window))
"This generic is intended to allow applications to mimic the user clicking a window's
close button."
  (running-on-this-thread ()
    (dcc (#/performClose: (cocoa-ref w)  ccl:+null-ptr+))))

(objc:define-objc-method ((:<BOOL> :window-should-close (:id sender)) cocoa-window)
  (declare (ignore sender))  ; The cocoa-window has been set up as its own delegate. Naughty?
  (if (window-may-close (easygui-window-of self)) #$YES #$NO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Drawing:

(defmethod clear-page ((cocoa-view cocoa-drawing-view))
  (let* ((view (easygui-view-of cocoa-view))
         (rect (dcc (#/bounds cocoa-view)))
         (color (slot-value view 'background)))
    (with-focused-view cocoa-view
      (dcc (#/setFill color))
      (dcc (#_NSRectFill rect)))))
         
(objc::defmethod (#/isFlipped :<BOOL>) ((self cocoa-drawing-view))
  (handler-case (if (slot-value self 'flipped) #$YES #$NO)
    (simple-error (condition)
      (when *report-flipping-errors* (format t "'isFlipped ~s' ignores error~%" self))
      (values (if *screen-flipped* #$YES #$NO) condition))))

(objc::defmethod (#/isFlipped :<BOOL>) ((self cocoa-contained-view))
  (handler-case (if (slot-value self 'flipped) #$YES #$NO)
    (simple-error (condition)
      (when *report-flipping-errors* (format t "'isFlipped ~s' ignores error~%" self))
      (values (if *screen-flipped* #$YES #$NO) condition))))

#| ------------
When the problem of wrong-size SLOT-VECTORs is clearly gone, the two definitions above
should be replaced with these simpler versions:
(objc::defmethod (#/isFlipped :<BOOL>) ((self cocoa-drawing-view))
  (if (slot-value self 'flipped) #$YES #$NO))

(objc::defmethod (#/isFlipped :<BOOL>) ((self cocoa-contained-view))
  (if (slot-value self 'flipped) #$YES #$NO))
------------ |#

(defmethod initialize-view :after ((view drawing-view))
  (setf (slot-value (cocoa-ref view) 'flipped) (slot-value view 'flipped))
  (setf (slot-value (cocoa-ref view) 'easygui-view) view))

(objc:defmethod (#/drawRect: :void) ((self cocoa-drawing-view)
                                     (rect :<NSR>ect))
  (dcc (draw-view-rectangle (easygui-view-of self) (nsrect-rectangle rect))))

(objc:defmethod (#/acceptsFirstReponder: :boolean) ((view cocoa-drawing-view))
  (accept-key-events-p (easygui-view-of view)))

(defgeneric draw-view-rectangle (view rectangle)
  (:method ((view drawing-view) rectangle)
    (declare (ignorable view rectangle))
    (when (draw-fn view)
      (let ((cview (cocoa-ref view)))
        (with-focused-view cview (funcall (draw-fn view) view cview))))
    nil))

(defmethod redisplay ((view drawing-view)
                      &key rect)
  (setf rect (if rect
                 (rectangle-nsrect rect)
                 (dcc (#/bounds (cocoa-ref view)))))
  (dcc (#/setNeedsDisplayInRect: (cocoa-ref view) rect)))

(define-useful-mouse-event-handling-routines cocoa-drawing-view)
(define-useful-mouse-event-handling-routines cocoa-mouseable-text-field)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Around methods for VIEW provide bindings for *modifier-key-pattern* for all kinds of views,
;; allowing for Shift-Key-P and friends.
;; Primary methods do nothing, but may be overridden by user code.

;(defmethod mouse-down :around ((view view) &key cocoa-event location button click-count delta)
;  (declare (ignorable cocoa-event location button click-count delta))
;  (let ((*cocoa-event* cocoa-event)
;        (*modifier-key-pattern* (dcc (#/modifierFlags cocoa-event))))
;    (call-next-method)))
  
(defmethod mouse-down ((view view) &key cocoa-event location button click-count delta)
  (declare (ignorable view cocoa-event location button click-count delta))
  nil)
  
;(defmethod mouse-up :around ((view view) &key cocoa-event location button click-count delta)
;  (declare (ignorable cocoa-event location button click-count delta))
;  (let ((*cocoa-event* cocoa-event)
;        (*modifier-key-pattern* (dcc (#/modifierFlags cocoa-event))))
;    (call-next-method)))
  
(defmethod mouse-up ((view view) &key cocoa-event location button click-count delta)
  (declare (ignorable view cocoa-event location button click-count delta))
  nil)

;(defmethod mouse-dragged :around ((view view) &key cocoa-event location button click-count delta)
;  (declare (ignorable cocoa-event location button click-count delta))
;  (let ((*cocoa-event* cocoa-event)
;        (*modifier-key-pattern* (dcc (#/modifierFlags cocoa-event))))
;    (call-next-method)))
  
(defmethod mouse-dragged ((view view) &key cocoa-event location button click-count delta)
  (declare (ignorable view cocoa-event location button click-count delta))
  nil)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primary methods for DRAWING-VIEW. What now is the point?

(defmethod mouse-down ((view drawing-view) &key cocoa-event location button click-count delta)
  (let ((mousefn (drawing-view-mouse-down view))
        (*cocoa-event* cocoa-event)
        (*modifier-key-pattern* (dcc (#/modifierFlags cocoa-event))))
    (when mousefn
      (funcall mousefn view
               :location location
               :allow-other-keys t
               :button button
               :cocoa-event cocoa-event
               :click-count click-count
               :delta delta))))

(defmethod mouse-up ((view drawing-view) &key cocoa-event location button click-count delta)
  (let ((mousefn (drawing-view-mouse-up view))
        (*cocoa-event* cocoa-event)
        (*modifier-key-pattern* (dcc (#/modifierFlags cocoa-event))))
    (when mousefn
      (funcall mousefn view
               :location location
               :allow-other-keys t
               :button button
               :cocoa-event cocoa-event
               :click-count click-count
               :delta delta))))

(defmethod mouse-dragged ((view drawing-view) &key cocoa-event location button click-count delta)
  (let ((mousefn (drawing-view-mouse-dragged view))
        (*cocoa-event* cocoa-event)
        (*modifier-key-pattern* (dcc (#/modifierFlags cocoa-event))))
    (when mousefn
      (funcall mousefn view
               :location location
               :allow-other-keys t
               :button button
               :cocoa-event cocoa-event
               :click-count click-count
               :delta delta))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primary methods for STATIC-TEXT-VIEW. What now is the point?

(defmethod mouse-down ((view static-text-view) &key cocoa-event location button click-count delta)
  (let ((mousefn (static-text-view-mouse-down view))
        (*cocoa-event* cocoa-event)
        (*modifier-key-pattern* (dcc (#/modifierFlags cocoa-event))))
    (when mousefn
      (funcall mousefn view
               :location location
               :allow-other-keys t
               :button button
               :cocoa-event cocoa-event
               :click-count click-count
               :delta delta))))

(defmethod mouse-up ((view static-text-view) &key cocoa-event location button click-count delta)
  (let ((mousefn (static-text-view-mouse-up view))
        (*cocoa-event* cocoa-event)
        (*modifier-key-pattern* (dcc (#/modifierFlags cocoa-event))))
    (when mousefn
      (funcall mousefn view
               :location location
               :allow-other-keys t
               :button button
               :cocoa-event cocoa-event
               :click-count click-count
               :delta delta))))

(defmethod mouse-dragged ((view static-text-view) &key cocoa-event location button click-count delta)
  (let ((mousefn (static-text-view-mouse-dragged view))
        (*cocoa-event* cocoa-event)
        (*modifier-key-pattern* (dcc (#/modifierFlags cocoa-event))))
    (when mousefn
      (funcall mousefn view
               :location location
               :allow-other-keys t
               :button button
               :cocoa-event cocoa-event
               :click-count click-count
               :delta delta))))

; -------------------
(defmethod view-named (name (view view))
  (find name (view-subviews view) :key #'view-nick-name))

(defmethod view-named (name (container content-view-mixin))
  (view-named name (content-view container)))

(defmethod view-subviews ((w content-view-mixin))
  (view-subviews (content-view w)))

; ----------------------

(defmethod view-nickname-chain ((view view) &optional include-everything) "
Yields two values:
- a list of nicknames of containing views, starting with outermost container
- the view or window that contains the view with the first name in the list,
  or NIL if the first name belongs to a window.
If include-everything is NIL (the default), the list does not contain the
autogenerated name for content views of windows or boxes, and contains names
of views or windows that have non-NIL names. The second value may then be
a view or window that has no nickname of its own.
If include-everything is T, the list does contain the autogenerated name of
content views of windows or boxes, it does contain NIL for views named NIL,
and the second value will always be NIL."
  (do (chain
       nickname
       (outermost view (view-container outermost)))
      ((or (null outermost)
           (and (null (setf nickname (view-nick-name outermost)))
                (not include-everything)))                
       (values chain outermost))
    (when (or include-everything (not (eq nickname '%CONTENT-OF-CONTENT-VIEW%)))
      (push (view-nick-name outermost) chain))))

; ----------------------

(defclass check-box-view (view view-text-via-title-mixin action-view-mixin decline-menu-mixin)
  ((checked :initarg :checked :initform nil)))

(defmethod check-box-check ((self check-box-view) &optional perform)
  (running-on-this-thread ()
    (unless (eql (dcc (#/state (cocoa-ref self))) #$NSOnState)
      (if perform
        (dcc (#/performClick: (cocoa-ref self) nil))
        (dcc (#/setState: (cocoa-ref self) #$NSOnState)))
      t)))

(defmethod initialize-view :after ((view check-box-view))
  (when (cocoa-ref view)
    (dcc (#/setButtonType: (cocoa-ref view) #$NSSwitchButton))
    (when (slot-value view 'checked) (check-box-check view))
    (setf (slot-value (cocoa-ref view) 'easygui-view) view)))

(defmethod check-box-uncheck ((self check-box-view) &optional perform)
  (running-on-this-thread ()
    (unless (eql (dcc (#/state (cocoa-ref self))) #$NSOffState)
      (if perform
        (dcc (#/performClick: (cocoa-ref self) nil))
        (dcc (#/setState: (cocoa-ref self) #$NSOffState)))
      t)))

(defmethod check-box-checked-p ((self check-box-view))
  (eql (dcc (#/state (cocoa-ref self))) #$NSOnState))

(defmethod (setf check-box-checked-p) (new (self check-box-view))
  (if new
    (check-box-check self)
    (check-box-uncheck self))
  new)

; -------------------------
(defclass radio-button-view (view view-text-via-title-mixin action-view-mixin decline-menu-mixin)
  ((selected :initarg :selected :reader radio-button-selected-p :initform nil)
   (cluster  :initarg :cluster  :initform '#:default-cluster))
  (:default-initargs :action #'(lambda () nil)))

(defun deselect-radio-button-cohorts (radio-button-view)
  (when (view-container radio-button-view)
    (dolist (sibling (view-subviews (view-container radio-button-view)))
      (when (and (not (eq sibling radio-button-view))
                 (typep sibling 'radio-button-view)
                 (eq (slot-value radio-button-view 'cluster) (slot-value sibling 'cluster))
                 (eql (dcc (#/state (cocoa-ref sibling))) #$NSOnState))
        (setf (slot-value sibling 'selected) nil)
        (dcc (#/setState: (cocoa-ref sibling) #$NSOffState))))))
  
(defmethod radio-button-select ((self radio-button-view) &optional perform)
  (running-on-this-thread ()
    (if perform
      (dcc (#/performClick: (cocoa-ref self) nil))
      (progn
        (deselect-radio-button-cohorts self)
        (setf (slot-value self 'selected) t)
        (dcc (#/setState: (cocoa-ref self) #$NSOnState))))))

(defmethod initialize-view :after ((self radio-button-view))
  (when (cocoa-ref self)
    (dcc (#/setButtonType: (cocoa-ref self) #$NSRadioButton))
    (when (slot-value self 'selected) (radio-button-select self))
    (setf (slot-value (cocoa-ref self) 'easygui-view) self)))

(defmethod radio-button-deselect ((self radio-button-view))
  (running-on-this-thread ()
    (dcc (#/setState: (cocoa-ref self) #$NSOffState))
    (prog1
      (radio-button-selected-p self)
      (setf (slot-value self 'selected) nil))))

(defmethod (setf action) (handler (view radio-button-view))
  (call-next-method
   (lambda ()
     (deselect-radio-button-cohorts view)
     (setf (slot-value view 'selected) t)
     (funcall handler))
   view)
  handler)

; ----------------------------------------------------------------------
; INVALIDATE-VIEW
; ----------------------------------------------------------------------

(defmethod invalidate-view ((view view) &optional total)
  (declare (ignorable total))
  (let ((cview (cocoa-ref view)))
    (dcc (#/setNeedsDisplay: cview #$YES))))

(defmethod invalidate-view ((window window) &optional total)
  (declare (ignorable total))
  (let* ((cocoaview (cocoa-ref window))
         (contentview (dcc (#/contentView cocoaview))))
    (dcc (#/setNeedsDisplay: contentview #$YES))))

; ----------------------------------------------------------------------
; Methods to    GET- & SET-    FORE- & BACK-    COLOR
; ----------------------------------------------------------------------

(defmethod set-fore-color ((view view) (color ns:ns-color))
  (setf (slot-value view 'foreground) color))

(defmethod set-fore-color :before ((view view-text-via-stringvalue-mixin) (color ns:ns-color))
  (dcc (#/setTextColor: (cocoa-ref view) color)))

(defmethod set-fore-color ((view cocoa-extension-mixin) (color ns:ns-color))
  (set-fore-color (easygui-view-of view) color))

(defmethod set-back-color ((view view) (color ns:ns-color) &optional redisplay-p)
  (setf (slot-value view 'background) color)
  (when redisplay-p (invalidate-view view)))

(defmethod set-back-color :after ((view static-text-view) (color ns:ns-color) &optional redisplay-p)
  (dcc (#/setBackgroundColor: (cocoa-ref view) color))
  (when redisplay-p (invalidate-view view)))

(defmethod set-back-color ((view cocoa-extension-mixin) (color ns:ns-color) &optional redisplay-p)
  (set-back-color (easygui-view-of view) color redisplay-p))

(defmethod get-fore-color ((view view))
  (slot-value view 'foreground))

(defmethod get-fore-color ((view cocoa-extension-mixin))
  (get-fore-color (easygui-view-of view)))

(defmethod get-back-color ((view view))
  (slot-value view 'background))

(defmethod get-back-color ((view cocoa-extension-mixin))
  (get-back-color (easygui-view-of view)))

; --------------------- Menus Begin ---------------------

(defmethod view-text ((self ns:ns-menu))
  (lisp-string-from-nsstring (dcc (#/title self))))

(defmethod (setf view-text) (new (self ns:ns-menu))
  (running-on-this-thread ()
    (dcc (#/setTitle: self (ccl::%make-nsstring new)))
    new))

(defclass menu-view (view view-text-via-title-mixin decline-menu-mixin)
  ((selection   :initarg :selection   :reader menu-selection :initform nil)
   (menu-kind   :initarg :menu-kind   :reader menu-kind      :initform :pull-down-menu)
   (menu-items  :initarg :menu-items  :reader menu-items     :initform nil)
   ns-menu
   %result))

(defclass menu-item-view (view view-text-via-title-mixin action-view-mixin decline-menu-mixin)
  (parent-menu
   action
   submenu)
  (:default-initargs :action #'(lambda () nil)))

;(defmethod (setf view-text) :after (new (menu menu-view))
;  (declare (ignorable new))
;  (dcc (#/setNeedsDisplay: (cocoa-ref menu) t)))

(defmethod initialize-instance :after ((self menu-view) &rest args &key menu-items selection)
  (declare (ignorable args selection))
  (let ((ns-menu nil))
    (if (slot-boundp self 'ns-menu)
      (setf ns-menu (slot-value self 'ns-menu))
      (setf ns-menu (dcc (#/menu (cocoa-ref self)))
            (slot-value self 'ns-menu) ns-menu))
    ;(format t "~&Initializing menu ~a with ~a items~%" self (length menu-items))
    (dolist (item menu-items)
      ;(format t "~&Adding ~a to menu ~a~%" item self)
      (cond
       ((typep item 'menu-view)
        (let ((intermediary (make-instance 'menu-item-view
                              :title (view-text item))))
          (setf (slot-value intermediary 'submenu) item)
          (dcc (#/setSubmenu: (cocoa-ref intermediary) (slot-value item 'ns-menu)))
          (dcc (#/addItem: ns-menu (cocoa-ref intermediary)))))
       ((not (typep item 'menu-item-view))
        (warn "Ignoring so-called menu item ~s" item))
       ((slot-boundp item 'parent-menu)
        (warn "Ignoring menu item ~s, which is already an item in some menu" item))
       (t (let ((coco (cocoa-ref item)))
            (dcc (#/addItem: ns-menu coco))
            (setf (slot-value item 'parent-menu) self)))))))

(defmethod (setf action) (new (menu-item menu-item-view))
  (call-next-method
   #'(lambda ()
       (if (slot-boundp menu-item 'parent-menu)
         (let ((parent (slot-value menu-item 'parent-menu)))
           (setf (slot-value parent 'selection) menu-item)
           (setf (slot-value parent '%result) (funcall new)))
         (funcall new)))
   menu-item)
  new)

(defmethod set-menu-item-title ((menu-item menu-item-view) title)
  (running-on-this-thread ()
    (dcc (#/setTitle: (cocoa-ref menu-item) (ccl::%make-nsstring title)))))

(defmethod set-menu-item-title ((menu-item ns:ns-menu-item) title)
  (running-on-this-thread ()
    (dcc (#/setTitle: menu-item (ccl::%make-nsstring title)))))

; -------------------
(defclass pop-up-menu (menu-view)
  ()
  (:default-initargs :menu-kind :pop-up-menu))

(defmethod initialize-instance :after ((self pop-up-menu) &rest args &key selection)
  (declare (ignorable args))
  (with-slots (ns-menu menu-items) self
    (setf (view-text self)
          (cond
           ((null menu-items)
            "<No Items>")
           ((null selection)
            (setf (slot-value self 'selection) (first menu-items))
            (view-text (first menu-items)))
           ((stringp selection)
            selection)
           ((member selection menu-items)
            (setf (slot-value self 'selection) selection)
            (view-text selection))
           (t "<Selection Invalid>"))))
  (setf (slot-value (cocoa-ref self) 'easygui-view) self))

; ----------------------
(defclass pull-down-menu (menu-view)
  ()
  (:default-initargs :menu-kind :pull-down-menu))

(defmethod initialize-instance :after ((self pull-down-menu) &rest args &key title)
  (declare (ignorable args))
  (running-on-this-thread ()
    (dcc (#/insertItemWithTitle:atIndex: (cocoa-ref self) (ccl::%make-nsstring (or title "<No Title>")) 0))))

(defmethod initialize-view :after ((self pull-down-menu))
  (running-on-this-thread ()
    (when (cocoa-ref self)
      (dcc (#/setPullsDown: (cocoa-ref self) #$YES))
      (setf (slot-value (cocoa-ref self) 'easygui-view) self))))

; -----------------------
(defclass contextual-menu (menu-view)
  ()
  (:default-initargs :menu-kind :contextual-menu))

(defgeneric add-contextual-menu (container menu &optional subviews))

(defmethod add-contextual-menu ((window window) (menu menu-view) &optional subviews)
  (add-contextual-menu (content-view window) menu subviews))

(defmethod add-contextual-menu ((view view) (menu menu-view) &optional subviews)
  (running-on-this-thread ()
    (dcc (#/setMenu: (cocoa-ref view) (slot-value menu 'ns-menu)))
    (when subviews
      (dolist (sub (view-subviews view))
        (unless (or (not (cocoa-null (dcc (#/menu (cocoa-ref sub)))))
                    (typep sub 'decline-menu-mixin))
          (add-contextual-menu sub menu subviews))))))

(defmethod add-contextual-menu ((view menu-view) (refusenik decline-menu-mixin) &optional subviews)
  (declare (ignore subviews))
  (error "Cannot add a contextual menu to a view of class ~s" (type-of refusenik)))

; -------------------------
(defun application-object nil
  (dcc (#/sharedApplication ns:ns-application)))

(defun application-main-menu nil
  (dcc (#/mainMenu (application-object))))

(defgeneric navigate-menu (titles menu))

(defmethod navigate-menu ((titles list) (menu menu-view))
;; Returns NIL if the path of titles leads nowhere, when no appropriately titled menu-item or submenu exists;
;; Returns a EasyGui MENU-ITEM if the path of titles leads to a leaf item;
;; Returns a EasyGui MENU-VIEW if the path of titles leads to a submenu.
  (cond
   ((null titles) menu)
   (t (let ((it (find (first titles) (menu-items menu) :test #'equalp :key #'view-text)))
        (when it (navigate-menu (rest titles) it))))))

(defun navigate-native-menu (titles menu)
;; Returns a NIL or a NS:NS-MENU-ITEM or a NS:NS-MENU
;; Returns a NS:NS-MENU when the title path leads to a submenu,
;; Returns a NS;NS-MENU-ITEM when the title path leads to a leaf menu item,
;; Returns NIL when the title path leads nowhere.
  (running-on-this-thread ()
    (if (null titles)
      menu
      (do ((number (dcc (#/numberOfItems menu)))
           (index 0 (1+ index))
           item found)
          ((or found (>= index number))
           (cond
            ((or (null found) (null (rest titles))) found)
            ((null (dcc (#/hasSubmenu found))) nil)
            (t (navigate-native-menu (rest titles) (dcc (#/submenu found))))))
        (setf item (dcc (#/itemAtIndex: menu index)))
        (if (or (equalp (first titles) (lisp-string-from-nsstring (dcc (#/title item))))
                ; The Apple menu item has title "" but its submenu has title "Apple", hence ...
                (and (dcc (#/hasSubmenu item))
                     (equalp (first titles) (lisp-string-from-nsstring (dcc (#/title (dcc (#/submenu item))))))))
          (setf found item))))))

(defmethod navigate-topbar ((titles list))
  (navigate-native-menu titles (application-main-menu)))

(defun add-menu-item (menu titles &optional action)
;; Adds a chain of submenus and a final leaf item with the indicated action.
;; If the final leaf item already exists, its action will be changed. Perhaps this is too dangerous.
;; The Apple submenu may not be altered; the application's submenu cannot be found.
  (cond
   ((null titles)
    (cerror "Return NIL" "No title path supplied"))
   ((not (and (consp titles) (stringp (first titles))))
    (cerror "Return NIL, some empty submenus may have been created" "Title path is not a list of strings"))
   ((not (typep menu 'ns:ns-menu))
    (cerror "Return NIL" "Not a Cocoa menu: ~s" menu))
   (t (let* ((ns-title (ccl::%make-nsstring (first titles)))
             (item (dcc (#/itemWithTitle: menu ns-title)))
             (ns-nullstring (ccl::%make-nsstring "")))
        (flet ((linkup (leaf action) ;; Modelled on code in easygui/action-targets.lisp
                 (let ((target (make-instance 'generic-easygui-target :handler (or action #'(lambda () nil)) :shooter leaf)))
                   (dcc (#/setTarget: leaf target))
                   (dcc (#/setAction: leaf (\@selector #/activateAction))))))
          (cond
           ((equalp (first titles) "-")
            (if (rest titles)
              (cerror "Leave menu unchanged" "A menu separator (an item having title \"-\") may not have a submenu")
              (dcc (#/addItem: menu (dcc (#/separatorItem ns:ns-menu-item))))))
           ((cocoa-null item) ;; No such item, something must be added
            (if (rest titles)
              (let ((number (dcc (#/numberOfItems menu)))
                    (submenu (make-instance 'ns:ns-menu)))
                (running-on-this-thread ()
                  (dcc (#/addItemWithTitle:action:keyEquivalent: menu ns-title ccl:+null-ptr+ ns-nullstring))
                  (setf item (dcc (#/itemAtIndex: menu number))) ;; That's where it got put
                  (dcc (#/initWithTitle: submenu ns-title))
                  (dcc (#/setSubmenu: item submenu)))
                (add-menu-item submenu (rest titles) action))
              (let ((number (dcc (#/numberOfItems menu))))
                (running-on-this-thread ()
                  (dcc (#/addItemWithTitle:action:keyEquivalent: menu ns-title ccl:+null-ptr+ ns-nullstring))
                  (setf item (dcc (#/itemAtIndex: menu number))))
                (linkup item action))))
           ((and (null (rest titles)) (dcc (#/hasSubmenu item)))
            (cerror "Leave menu unchanged" "An Action may not be added to any item with a submenu"))
           ((and (rest titles) (dcc (#/hasSubmenu item)))
            (add-menu-item (dcc (#/submenu item)) (rest titles) action))
           ((rest titles)
            (cerror "Leave menu unchanged" "An existing menu item cannot be converted to have a submenu"))
           (t (linkup item action)))))))) ;; Change the action of an existing item: desirable, or dangerous?            

(defun add-topbar-item (titles &optional action)
  (if (and (consp titles) (rest titles))
    (add-menu-item (application-main-menu) titles action)
    (cerror "Return NIL" "Title path must be a list with at least two elements: ~s" titles)))

(defun remove-menu-item (menu titles retain-if-empty)
  (if (not (and (consp titles) (stringp (first titles))))
    (cerror "Return NIL" "Title path is not a list of strings")
    (do ((number (dcc (#/numberOfItems menu)))
         (index 0 (1+ index))
         item found)
        ((or found (>= index number))
         (when found
           (if (rest titles)
             (when (dcc (#/hasSubmenu found))
               (remove-menu-item (dcc (#/submenu found)) (rest titles) retain-if-empty)
               (unless (or retain-if-empty (> (dcc (#/numberOfItems (dcc (#/submenu found)))) 0))
                 (dcc (#/removeItem: menu found))))
             (dcc (#/removeItem: menu found)))))
      (setf item (dcc (#/itemAtIndex: menu index)))
      (when (equalp (first titles) (lisp-string-from-nsstring (dcc (#/title item))))
        (setf found item)))))

(defun remove-topbar-item (titles &key retain-if-empty)
  (when (and (consp titles)
             (not (member (first titles) '("" "Apple") :test #'equalp)))
    (remove-menu-item (application-main-menu) titles retain-if-empty)))

(defun add-application-submenu (title &rest trees) "
Adds a menu to the topbar application-menu with the given title.
Its menu-items names are got from the CARs of the trees.
The CDRs of these trees may consist either of further trees, allowing arbitrarily
deep menu structures, or of a one-element list that is expected to be a parameterless
function to be used as the Action of a leaf menu item.
Example:
  (add-application-submenu \"Beeps\"
     '(\"Normal\" #'normal-beep)
     '(\"Stupid\" #'stupid-beep)
     '(\"Choose\" (\"Custom beep 1\" #'custom-beep-1-not-implemented)
                (\"Custom beep 2\" #'custom-beep-2-not-implemented)))
"
  (labels ((valid-tree (tree)
             (and (consp tree) (stringp (first tree))))
           (prepending (seq tree)
             (cond
              ((every #'valid-tree (rest tree))
               (dolist (subtree (rest tree))
                 (prepending (append seq (list (first subtree))) (rest subtree))))
              ((and (consp tree) (stringp (first tree)) (consp (rest tree)) (null (cddr tree)))
               (add-topbar-item (append seq (list (first tree))) (second tree)))
              (t (cerror "Ignore this tree" "Malformed tree ~s" tree)))))
    (if (every #'valid-tree trees)
      (dolist (subtree trees) (prepending (list title) subtree))
      (cerror "Return NIL" "Malformed top-level trees"))))

; ---------------
; Keyboard input handling

(defmethod view-key-event-handler ((view window) char)
  (declare (ignorable char))
  nil)

(objc:define-objc-method ((:void :key-down (:id event)) cocoa-window)
  (let ((*cocoa-event* event)
        (*modifier-key-pattern* (#/modifierFlags event)))
    (view-key-event-handler
     (easygui-window-of self)
     (schar (lisp-string-from-nsstring (dcc (#/charactersIgnoringModifiers event))) 0))))

(defun shift-key-p nil
  (not (zerop (logand *modifier-key-pattern* (key-mask :shift)))))

(defun control-key-p nil
  (not (zerop (logand *modifier-key-pattern* (key-mask :control)))))

(defun alt-key-p nil
  (not (zerop (logand *modifier-key-pattern* (key-mask :alt)))))

(defun command-key-p nil
  (not (zerop (logand *modifier-key-pattern* (key-mask :command)))))

(defmacro with-modifier-key-information (parameterless-function)
;; NOT TESTED YET!
"Wraps the function into a context where control-key-p &c will get their current values.
To be used primarily when placing a call to a function in another process."
  (let ((gvar (gensym)))
    `(let ((,gvar *modifier-key-pattern*))
       (function (lambda nil
                   (let ((*modifier-key-pattern* ,gvar))
                     (funcall ,parameterless-function)))))))

(defun view-mouse-position (view)
  (let* ((w (cocoa-ref (easygui-window-of view)))
         (mouselocation (dcc (#/mouseLocationOutsideOfEventStream w)))
         (cview (if (typep view 'window) (content-view view) view))
         (nspt (dcc (#/convertPoint:fromView: (cocoa-ref cview) mouselocation NIL))))
    ;; todo: check point is inside bounds, lest negative coords
    (point (ns:ns-point-x nspt) (ns:ns-point-y nspt))))
