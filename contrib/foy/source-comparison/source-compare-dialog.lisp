;;;-*- Mode: Lisp; Package: SOURCE-COMPARE -*-

;;; ----------------------------------------------------------------------------
;;;
;;;      source-compare-dialog.lisp, version 0.1b1
;;;
;;;      copyright © 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      This file provides a GUI for Mark Kantrowitz's source-compare.lisp.
;;;      See source-compare.lisp for documentation.  
;;;
;;;      The GUI portion is straight forward.  The browse buttons let you browse 
;;;      to select the two target files.  The Hemlock buttons will pull in the file 
;;;      in the top Hemlock window.  
;;;
;;;      When the utility prints a diff specification, Alt-Double-Click it to
;;;      pull up the relevant code in Hemlock windows.  There are various types of
;;;      diff specs.  A typical one looks like this: 559,565c544,546
;;;      
;;;      The most recent version will be available at: www.clairvaux.org/downloads/
;;;
;;;      This code is offered "as is" without warranty of any kind.
;;;
;;; ----------------------------------------------------------------------------

(in-package "SOURCE-COMPARE")

(defConstant %dialog-width% 675)
(defConstant %dialog-height% 410)

(defParameter *source-compare-dialog* nil)

(defun open-source-compare-dialog ()
  (#/makeKeyAndOrderFront: *source-compare-dialog* nil))

#|
(setq *source-compare-dialog* nil)

(gui::execute-in-gui 'open-source-compare-dialog)
|#

;;; This includes a work-around for what appears to be a bug in the hemlock-frame
;;; #/close method.  After a #/close, the window remains on the (#/orderedWindows *NSApp*)
;;; list, but (hi::buffer-document buffer) in NIL.  Therefore the extra tests:
(defun display-hemlock-position (path start-line &optional end-line)
  (labels ((window-buffer (w)
             (let* ((pane (slot-value w 'gui::pane))
                    (hemlock-view (gui::text-pane-hemlock-view pane)))
               (hi::hemlock-view-buffer hemlock-view)))
           (window-with-path (target-path)
             (gui::first-window-satisfying-predicate 
              #'(lambda (w)
                  (when (and (typep w 'gui::hemlock-frame)
                             (not (typep w 'gui::hemlock-listener-frame)))
                    (let* ((buffer (window-buffer w))
                           (document (when buffer (hi::buffer-document buffer)))
                           (buffer-path (when buffer (hi::buffer-pathname buffer))))
                      (when (and document (stringp buffer-path))
                        (string-equal target-path buffer-path))))))))
    (let* ((w (window-with-path path))
           (hemlock-view (cond (w 
                                (gui::hemlock-view w))
                               (t
                                (let ((view (gui::cocoa-edit path)))
                                  (when view
                                    (setq w (#/window (hi::hemlock-view-pane view)))
                                    view)))))
           (text-pane (when w (slot-value w 'gui::pane)))
           (text-view (when text-pane (gui::text-pane-text-view text-pane)))
           (buffer (when hemlock-view (hi::hemlock-view-buffer hemlock-view)))
           (hi::*current-buffer* buffer)
           (start-mark (when (and buffer start-line)
                         (let ((temp (hi::copy-mark (hi::buffer-start-mark buffer) :temporary)))
                           (when (hi::line-offset temp (1- start-line))
                             temp))))
           (start-pos (when start-mark (hi::mark-absolute-position start-mark)))
           (end-mark (when (and buffer end-line)
                         (let ((temp (hi::copy-mark (hi::buffer-start-mark buffer) :temporary)))
                           (when (hi::line-offset temp (1- end-line))
                             (hi::line-end temp)))))
           (end-pos (if end-mark 
                      (hi::mark-absolute-position end-mark)
                      (when (and start-pos start-mark)
                        (let ((temp (hi::copy-mark start-mark :temporary)))
;                          (when (hi::line-offset temp 1)
                          (hi::mark-absolute-position (hi::line-end temp)))))))
      (when (and w text-view start-mark start-pos)
        (#/makeKeyAndOrderFront: w nil)
        (when (and start-pos end-pos)
          (ns:with-ns-range (range start-pos (- end-pos start-pos))
            (#/scrollRangeToVisible: text-view range)
            (#/setSelectedRange: text-view range))
          (hi::move-mark (hi::buffer-point buffer) start-mark)
          (gui::update-paren-highlight text-view))))))

;;; ----------------------------------------------------------------------------
;;;
(defclass sc-text-view (ns:ns-text-view)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/mouseDown: :void) ((self sc-text-view) event)
  (cond ((and (logtest #$NSAlternateKeyMask (#/modifierFlags event))
              (= (#/clickCount event) 2))
         ; (#/selectWord: self self)
         (call-next-method event)
         (let* ((range (#/selectedRange self))
                (substring (#/substringWithRange: (#/string self) range)))
           (process-diff-string (#/window self) (ccl::lisp-string-from-nsstring substring))))
        (t
         (call-next-method event))))

;;; ----------------------------------------------------------------------------
;;;
(defClass SOURCE-COMPARE-WINDOW (ns:ns-window)
  ((path-1 :initform nil :accessor path-1)
   (path-1-field :foreign-type :id :initform nil :accessor path-1-field)
   (path-2 :initform nil :accessor path-2)
   (path-2-field :foreign-type :id :initform nil :accessor path-2-field)
   (difference-pane :foreign-type :id :initform nil :accessor difference-pane)
   (ignore-case-check-box :foreign-type :id :initform nil :accessor ignore-case-check-box)
   (ignore-whitespace-check-box :foreign-type :id :initform nil :accessor ignore-whitespace-check-box)
   (ignore-comments-check-box :foreign-type :id :initform nil :accessor ignore-comments-check-box)
   (ignore-blank-lines-check-box :foreign-type :id :initform nil :accessor ignore-blank-lines-check-box)
   (print-context-check-box :foreign-type :id :initform nil :accessor print-context-check-box)
   (print-fancy-header-check-box :foreign-type :id :initform nil :accessor print-fancy-header-check-box)
   (compare-button :initform nil :accessor compare-button)
   (action-alist :initform nil :accessor action-alist))
  (:metaclass ns:+ns-object))

;;; This is called for all GUI actions.  The source-compare-window is always the target.
;;; Doing it this way means we can use lambdas in the code below rather than
;;; writing a bunch of objc functions.  Old MCL habits die hard.
(objc:defmethod (#/interfaceAction: :void) ((w source-compare-window) (sender :id))
  (let ((pair (assoc sender (action-alist w))))
    (cond (pair
           ;; dispatch:
           (funcall (cdr pair) sender))
          (t
           (error "~%Action function not found for ~S" sender)))))

(defmethod clear-difference-pane ((w source-compare-window))
  (#/setString: (difference-pane w) #@""))

(defmethod process-diff-string ((w source-compare-window) string)
  (when (and string
             (every #'(lambda (char)
                        (member char
                                '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                                      #\c #\a #\d #\,)))
                    string))
    (let* ((alpha-char-pos (find-if #'(lambda (char) (alpha-char-p char)) string))
           (position (position alpha-char-pos string))
           (lhs (subseq string 0 position))
           (rhs (subseq string (1+ position)))
           (lhs-comma (position #\, lhs))
           (rhs-comma (position #\, rhs))
           lhs-start lhs-end rhs-start rhs-end)
      
      (cond (lhs-comma
             (setf lhs-start (read-from-string (subseq lhs 0 lhs-comma)))
             (setf lhs-end (read-from-string (subseq lhs (1+ lhs-comma))))
             (display-hemlock-position (path-1 w) lhs-start lhs-end))
            (t
             (setf lhs-start (read-from-string lhs))
             (display-hemlock-position (path-1 w) lhs-start)))
      
      (cond (rhs-comma
             (setf rhs-start (read-from-string (subseq rhs 0 rhs-comma)))
             (setf rhs-end (read-from-string (subseq rhs (1+ rhs-comma))))
             (display-hemlock-position (path-2 w) rhs-start rhs-end))
            (t
             (setf rhs-start (read-from-string rhs))
             ;; single line 
             (display-hemlock-position (path-2 w) rhs-start))))))
          
(defMethod get-scmp-items ((w source-compare-window))
  (append 
   (make-path-items w)
   (make-button-items w)
   (make-check-boxes w)
   (make-miscel-items w)))

(defMethod make-check-boxes ((w source-compare-window))
  (flet ((make-check-box (title x-coord y-coord x-dim y-dim checked-p)
           (let ((box (#/alloc ns:ns-button)))
             (ns:with-ns-rect (frame x-coord y-coord x-dim y-dim)
               (#/initWithFrame: box frame))
             (#/setButtonType: box #$NSSwitchButton)
             (#/setTitle: box title)
             (#/setState: box (if checked-p #$NSOnState #$NSOffState))
             box)))
    (list
     (setf (ignore-case-check-box w)
           (make-check-box #@"ignore case" 10 30 130 20 t))

     (setf (ignore-comments-check-box w)
           (make-check-box #@"ignore comments" 160 30 130 20 t))

     (setf (ignore-whitespace-check-box w)
           (make-check-box #@"ignore whitespace" 310 30 130 20 t))

     (setf (ignore-blank-lines-check-box w)
           (make-check-box #@"ignore blank lines" 10 10 130 20 t))

     (setf (print-context-check-box w)
           (make-check-box #@"ignore context lines" 160 10 140 20 t))

     (setf (print-fancy-header-check-box w)
           (make-check-box #@"print fancy header" 310 10 140 20 nil)))))

(defMethod make-button-items ((w source-compare-window))
  (flet ((make-button (title x-coord y-coord x-dim y-dim lambda)
           (let ((button (#/alloc ns:ns-button)))
             (ns:with-ns-rect (frame x-coord y-coord x-dim y-dim)
               (#/initWithFrame: button frame)
               (#/setButtonType: button #$NSMomentaryPushInButton)
               (#/setImagePosition: button #$NSNoImage)
               (#/setBezelStyle: button #$NSRoundedBezelStyle))
             (#/setTitle: button title)
             (#/setTarget: button w)
             (#/setAction: button (ccl::@selector "interfaceAction:"))
             (pushnew (cons button lambda) (action-alist w))
             button))
         (front-hemlock-window ()
           (gui::first-window-satisfying-predicate 
            #'(lambda (w)
                (and (typep w 'gui::hemlock-frame)
                     (not (typep w 'gui::hemlock-listener-frame))))))
                     ;; (#/isKeyWindow w)))))
         (window-pathname (w)
           (when w
             (let* ((pane (slot-value w 'gui::pane))
                    (text-view (gui::text-pane-text-view pane))
                    (buffer (gui::hemlock-buffer text-view)))
               (hi::buffer-pathname buffer)))))

    (list (make-button #@"Browse" 480 368 80 32
                       #'(lambda (item)
                           (declare (ignore item))
                           (let ((path (gui::cocoa-choose-file-dialog :button-string "select")))
                             (when path
                               (clear-difference-pane w)
                               (setf (path-1 w) path)
                               (#/setStringValue: (path-1-field w) (ccl::%make-nsstring path))))))

          (make-button #@"Browse" 480 338 80 32
                       #'(lambda (item)
                           (declare (ignore item))
                           (let ((path (gui::cocoa-choose-file-dialog :button-string "select")))
                             (when path
                               (clear-difference-pane w)
                               (setf (path-2 w) path)
                               (#/setStringValue: (path-2-field w) (ccl::%make-nsstring path))))))

          (make-button #@"Hemlock" 570 368 90 32
                       #'(lambda (item)
                           (declare (ignore item))
                           (let* ((window (front-hemlock-window))
                                  (path (when window (window-pathname window))))
                             (when path 
                               (clear-difference-pane w)
                               (setf (path-1 w) path)
                               (#/setStringValue: (path-1-field w) (ccl::%make-nsstring path))))))

          (make-button #@"Hemlock" 570 338 90 32
                       #'(lambda (item)
                           (declare (ignore item))
                           (let* ((window (front-hemlock-window))
                                  (path (when window (window-pathname window))))
                             (when path 
                               (clear-difference-pane w)
                               (setf (path-2 w) path)
                               (#/setStringValue: (path-2-field w) (ccl::%make-nsstring path))))))

          (make-button #@"Cancel" 480 10 80 32
                       #'(lambda (item)
                           (declare (ignore item))
                           (#/close w)))

          (setf (compare-button w)
                (make-button #@"Compare" 570 10 90 32
                             #'(lambda (item)
                                 (declare (ignore item))
                                 (compare w)))))))

(defMethod compare ((w source-compare-window))

  (cond ((and (path-1 w) (path-2 w))
         (unless (probe-file (path-1 w))
           (format t "~%; • File: ~A does not exist." (path-1 w))
           (return-from compare))
         (unless (probe-file (path-2 w))
           (format t "~%; • File: ~A does not exist." (path-2 w))
           (return-from compare))

         (let ((stream (make-string-output-stream)))         
           ;; out with the old 
           (#/setString: (difference-pane w) #@" ")
           (source-compare (path-1 w) (path-2 w)
                           :output-stream stream
                           :ignore-case (eql (#/state (ignore-case-check-box w)) #$NSOnState)
                           :ignore-whitespace (eql (#/state (ignore-whitespace-check-box w)) #$NSOnState)
                           :ignore-comments (eql (#/state (ignore-comments-check-box w)) #$NSOnState)
                           :ignore-blank-lines (eql (#/state (ignore-blank-lines-check-box w)) #$NSOnState)
                           :print-context (eql (#/state (print-context-check-box w)) #$NSOnState)
                           :print-fancy-header (eql (#/state (print-fancy-header-check-box w)) #$NSOnState))
           (#/setString: (difference-pane w) (ccl::%make-nsstring (ccl::get-output-stream-string stream)))))
        (t
         (#/setString: (difference-pane w) #@"First enter two valid paths."))))

(defMethod make-path-items ((w source-compare-window))
  (let* ((small-sys-size (#/smallSystemFontSize ns:ns-font))
         (small-sys-font (#/systemFontOfSize: ns:ns-font small-sys-size)))
    (list
     (setf (path-1-field w)
           (let ((field (#/alloc ns:ns-text-field)))
             (ns:with-ns-rect (frame 30 375 435 15)
               (#/initWithFrame: field frame)
               (#/setEditable: field nil)
               (#/setDrawsBackground: field nil)
               (#/setBordered: field nil)
               (#/setFont: field small-sys-font)
               (#/setStringValue: field #@""))
             field))
     
     (setf (path-2-field w)
           (let ((field (#/alloc ns:ns-text-field)))
             (ns:with-ns-rect (frame 30 345 435 15)
               (#/initWithFrame: field frame)
               (#/setEditable: field nil)
               (#/setDrawsBackground: field nil)
               (#/setBordered: field nil)
               (#/setFont: field small-sys-font)
               (#/setStringValue: field #@""))
             field)))))

(defMethod make-miscel-items ((w source-compare-window))
  (list
   (let* ((scroll-view (#/alloc ns:ns-scroll-view))
          (view (#/init (#/alloc sc-text-view))))
     (ns:with-ns-rect (frame 4 60 650 200)
       (#/initWithFrame: scroll-view frame))
     (ns:with-ns-rect (frame 4 60 650 200)
       (#/initWithFrame: view frame))
     (#/insertText: view #@" ")
     (#/setHasVerticalScroller: scroll-view t)
     (#/setHasHorizontalScroller: scroll-view t)
     (#/setBorderType: scroll-view #$NSBezelBorder)
     (#/setDocumentView: scroll-view view)
     (#/setEditable: view nil)
     (setf (difference-pane w) view)
     scroll-view)

   (let* ((title (#/alloc ns:ns-text-field)))
     (ns:with-ns-rect (frame 5 370 22 22)
       (#/initWithFrame: title frame))
     (#/setEditable: title nil)
     (#/setDrawsBackground: title nil)
     (#/setBordered: title nil)
     ;; (#/setFont: title style-font)
     (#/setStringValue: title #@"1:")
     title)

   (let ((box (#/alloc ns:ns-box)))
      (ns:with-ns-rect (frame 25 370 450 40)
        (#/initWithFrame: box frame))
     (#/setTitle: box #@"")
     box)

   (let* ((title (#/alloc ns:ns-text-field)))
     (ns:with-ns-rect (frame 5 340 22 22)
       (#/initWithFrame: title frame))
     (#/setEditable: title nil)
     (#/setDrawsBackground: title nil)
     (#/setBordered: title nil)
     ;; (#/setFont: title style-font)
     (#/setStringValue: title #@"2:")
     title)

   (let ((box (#/alloc ns:ns-box)))
      (ns:with-ns-rect (frame 25 340 450 40)
        (#/initWithFrame: box frame))
     (#/setTitle: box #@"")
     box)

   (let* ((title (#/alloc ns:ns-text-field)))
     (ns:with-ns-rect (frame 10 310 500 22)
       (#/initWithFrame: title frame))
     (#/setEditable: title nil)
     (#/setDrawsBackground: title nil)
     (#/setBordered: title nil)
     ;; (#/setFont: title style-font)
     (#/setStringValue: title #@"Mods required to make file 1 equivalent to file 2:")
     title)

  (let* ((small-sys-size (#/smallSystemFontSize ns:ns-font))
         (small-sys-font (#/systemFontOfSize: ns:ns-font small-sys-size))
         (title (#/alloc ns:ns-text-field)))
    (ns:with-ns-rect (frame 10 290 500 22)
      (#/initWithFrame: title frame))
    (#/setEditable: title nil)
    (#/setDrawsBackground: title nil)
    (#/setBordered: title nil)
    (#/setFont: title small-sys-font)
    (#/setStringValue: title #@"(a = add, d = delete, c = change, < = file 1, > = file 2)")
    title)

  (let* ((small-sys-size (#/smallSystemFontSize ns:ns-font))
         (small-sys-font (#/systemFontOfSize: ns:ns-font small-sys-size))
         (title (#/alloc ns:ns-text-field)))
    (ns:with-ns-rect (frame 10 270 500 22)
      (#/initWithFrame: title frame))
    (#/setEditable: title nil)
    (#/setDrawsBackground: title nil)
    (#/setBordered: title nil)
    (#/setFont: title small-sys-font)
    (#/setStringValue: title #@"(To display the relevant code, alt-double-click the difference spec, ie 559,565c544,546)")
    title)))

(setq *source-compare-dialog*
      (let ((dialog (#/alloc source-compare-window)))
        (ns:with-ns-rect (r 100 100 %dialog-width% %dialog-height%)
          (#/initWithContentRect:styleMask:backing:defer: 
           dialog
           r
           (logior  #$NSTitledWindowMask 
                    #$NSClosableWindowMask  
                    #$NSMiniaturizableWindowMask)
           #$NSBackingStoreBuffered
           #$NO))
        (#/setTitle: dialog #@"Source Comparison")
        (dolist (item (get-scmp-items dialog))
          (#/addSubview: (#/contentView  dialog) item))
        (#/setDefaultButtonCell: dialog (compare-button dialog))
        (#/setReleasedWhenClosed: dialog nil)
        (#/center dialog)
        dialog))

;;; ----------------------------------------------------------------------------
;;; update the Tools Menu
;;;
(defParameter *tools-menu* 
  (#/submenu (#/itemWithTitle: (#/mainMenu (ccl::application-ui-object ccl::*application*)) #@"Tools")))

(let ((item (#/itemWithTitle: *tools-menu* #@"Source Compare…")))
  (unless (%null-ptr-p item) (#/removeItem: *tools-menu* item))
  (#/addItem: *tools-menu* (#/separatorItem ns:ns-menu-item))
  (setf item (#/initWithTitle:action:keyEquivalent: (#/alloc ns:ns-menu-item)
                                                    #@"Source Compare…"
                                                    (ccl::@selector "interfaceAction:")
                                                    #@""))
  (#/setTarget: item *source-compare-dialog*)
  (#/addItem: *tools-menu* item)
  (pushnew (cons item
                 #'(lambda (sender)
                     (declare (ignore sender))
                     (open-source-compare-dialog)))
           (action-alist *source-compare-dialog*)))










