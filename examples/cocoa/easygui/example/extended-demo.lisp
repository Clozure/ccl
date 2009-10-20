(in-package :easygui-demo)

; ---------------
; Demo for the new control types for EasyGui within Clozure CL
; Contributed March 2009 by AWSC (arthur.cater@ucd.ie)
; based upon earlier work by an unknown author.
; Permission is given to disseminate, use, and modify.
; No warranty is expressed or implied.
;
; Tested in cclv1.2 on 32-bitPPC, in cclv1.3 on 32-bitPPC and 64-bitIntel Macs.
; Tested only in images constructed using (require :cocoa-application).

(setf easygui::*debug-cocoa-calls* nil)

(defclass view-hierarchy-demo-window (window)
     ((with :initarg :with :initform :button)
      (textbox :initarg :textbox :initform nil :accessor demo-textbox))
  (:default-initargs :size (point 480 270)
    :position (point 125 513)
    :resizable-p nil
    :minimizable-p t
    :title "View tree demo")
  (:documentation "Shows a window with a simple view hierarchy and one or more controls
 that manipulate this hierarchy."))

(defclass brown-drawing-view (drawing-view) ())

(defmethod draw-view-rectangle ((drawing brown-drawing-view) rectangle)
  (declare (ignore rectangle))
  (let* ((cocoa-view (cocoa-ref drawing))
         (rect (dcc (#/bounds cocoa-view)))
         (brown (dcc (#/brownColor ns:ns-color))))
    (with-focused-view cocoa-view
      (dcc (#/setFill brown))
      (dcc (#_NSRectFill rect)))))
         
(defmethod initialize-view :after ((w view-hierarchy-demo-window))
  (let (left-box right-box button left-button right-button checkbox popup pulldown slider
        drawing text (leftp t)
        (normalfont (gui::default-font :name "Monaco" :size 10.0 :attributes nil))
        (shoutfont (gui::default-font :name "Courier" :size 36.0 :attributes '(:bold :italic))))
    (labels ((to-left ()
               (retaining-objects (text)
                 (cond ((not leftp)
                        (remove-subviews right-box text)
                        (add-subviews left-box text))))
               (setf leftp t))
            (to-right ()
             (retaining-objects (text)
                (cond (leftp
                       (remove-subviews left-box text)
                       (add-subviews right-box text))))
             (setf leftp nil))
            (to-other ()
             (retaining-objects (text)
                (cond ((not leftp)
                       (remove-subviews right-box text)
                       (add-subviews left-box text))
                      (leftp
                       (remove-subviews left-box text)
                       (add-subviews right-box text))))
             (setf leftp (not leftp)))
            (generate-menu-items ()
              (list (make-instance 'menu-item-view :title "Left" :action #'to-left)
                    (make-instance 'menu-item-view :title "Right" :action #'to-right)
                    (make-instance 'menu-item-view :title "Other" :action #'to-other)
                    (make-instance 'pop-up-menu :title "Text Options"
                      :menu-items
                      (list (make-instance 'menu-item-view :title "Oink"
                              :action #'(lambda () (setf (view-font text) normalfont)
                                                   (setf (view-text text) "Oink!")
                                                   (setf (view-size text) (point 60 20))
                                                   (setf (view-position text) (point 37 112))))
                            (make-instance 'menu-item-view :title "SHOUT!"
                              :action #'(lambda () (setf (view-font text) shoutfont)
                                                   (setf (view-text text) "HEY!")
                                                   (setf (view-size text) (point 160 60))
                                                   (setf (view-position text) (point 17 10))))
                            (make-instance 'pop-up-menu :title "Whisper"
                              :menu-items
                              (list (make-instance 'menu-item-view :title "sh!"
                                      :action #'(lambda () (setf (view-font text) normalfont)
                                                           (setf (view-text text) "sh!")))
                                    (make-instance 'menu-item-view :title "psst!"
                                      :action #'(lambda () (setf (view-font text) normalfont)
                                                           (setf (view-text text) "psst!"))))))))))
      (setf left-box (make-instance 'box-view
                       :position (point 17 51)
                       :size (point 208 199)
                       :title "Left"
                       :tip #'(lambda nil (unless leftp "The occupied box has no tooltip"))
                       :view-nick-name :leftbox)
            right-box (make-instance 'box-view
                        :position (point 255 51)
                        :size (point 208 199)
                        :tip #'(lambda nil (if leftp "The occupied box has no tooltip"))
                        :title "Right"
                        :view-nick-name :rightbox)
            button (make-instance 'push-button-view
                       :position (point 173 12)
                       :text "Change side"
                       :tip #'(lambda nil "Button tip does not work!")
                       :view-nick-name :push-button
                       :action #'to-other)
            left-button (make-instance 'radio-button-view
                          :position (point 103 12)
                          :text "Left side"
                          :selected t
                          :view-nick-name :leftbutton
                          :tip #'(lambda nil (format nil
                                                       "Where's the amazing tooltip?~%The text is in the box on the ~:[right~;left~]"
                                                       leftp))
                          :action #'to-left)
            right-button (make-instance 'radio-button-view
                           :position (point 243 12)
                           :text "Right side"
                           :view-nick-name :rightbutton
                           :tip #'(lambda nil (format nil
                                                        "Where's the amazing tooltip?~%The text is in the box on the ~:[right~;left~]"
                                                        leftp))
                   
                           :action #'to-right)
            checkbox (make-instance 'check-box-view
                       :position (point 173 12)
                       :text "Right side"
                       :view-nick-name :checkbox
                       :tip #'(lambda nil (format nil
                                                    "Where's the amazing tooltip?~%The text is in the box on the ~:[right~;left~]"
                                                    leftp))
                       :action #'to-other)
            popup (make-instance 'pop-up-menu
                   :position (point 173 12)
                   :size (point 120 24)
                   :title "Command?"
                   :tip #'(lambda nil (format nil "Pop up menus can have tooltips,~%however their menu items cannot."))
                   :view-nick-name :pop-up-menu
                   :menu-items (generate-menu-items))
            pulldown (make-instance 'pull-down-menu
                   :position (point 173 12)
                   :size (point 120 24)
                   :title "Command?"
                   :tip #'(lambda nil (format nil "Pull down menus can have tooltips,~%however their menu items cannot."))
                   :view-nick-name :pull-down-menu
                   :menu-items (generate-menu-items))
            drawing (make-instance 'brown-drawing-view
                   :position (point 173 12)
                   :size (point 120 24)
                   :view-nick-name :drawing
                   :tip #'(lambda nil (format nil
                                                "See the amazing tooltip!~%The text is in the box on the ~:[right~;left~]"
                                                leftp))
                   :mouse-down #'(lambda (view &key &allow-other-keys)
                                   (declare (ignore view))
                                   (if (shift-key-p) (to-left) (to-other))))
            text (make-instance 'static-text-view
                   :text "Oink!"
                   :view-font normalfont
                   :tip :identify
                   :position (point 37 112)
                   :size (point 60 20)
                   :fore-color (make-rgb :red 0 :green 0 :blue 255)
                   :back-color (make-rgb :red 255 :green 220 :blue 200))
            slider (make-instance 'slider-view :min-value 0 :max-value 1
                     ;            :text "How right"   ;; No provision for title or text?
                     :position (point 173 12) :size (point 136 24)
                     :view-nick-name :slider
                     :tip #'(lambda nil (format nil
                                                "See the amazing tooltip!~%The text is in the box on the ~:[right~;left~]"
                                                leftp))
                     :action #'(lambda ()
                                 (if (> (dcc (#/floatValue (cocoa-ref slider))) 0.5)
                                   (to-right)
                                   (to-left)))))
      (add-subviews w left-box right-box)
      (case (slot-value w 'with)
        (:button (add-subviews w button))
        (:radio (add-subviews w left-button right-button))
        (:check (add-subviews w checkbox))
        (:popup  (add-subviews w popup))
        (:pulldown (add-subviews w pulldown))
        (:slider (add-subviews w slider))
        (:drawing (add-subviews w drawing))
        (otherwise (format t "~&** The WITH slot is ~s, it must be either :BUTTON :RADIO :CHECK :POPUP ~
                           :PULLDOWN :DRAWING or :SLIDER~%"
                           (slot-value w 'with))))
      (add-subviews left-box text)
      (setf (demo-textbox w) text)
      (add-contextual-menu w
                           (make-instance 'contextual-menu
                             :menu-items (generate-menu-items))
                           t)
      (window-show w))))

(defparameter *w nil)

(defparameter *run-file-chooser-anyway* nil)

(defvar *closing-under-program-control* nil
"Used in demonstrating tailored window-closing behaviour.")
 
(defmethod window-may-close ((w view-hierarchy-demo-window))
  (or *closing-under-program-control*
      (when (y-or-n-dialog "Do you really want to close the window like this?")
        (setf *w nil)
        t)))

(defun run-demo ()
  (flet ((example (with)
           (when *w (let ((*closing-under-program-control* t)) (perform-close *w)))
           (setf *w (make-instance 'view-hierarchy-demo-window :with with))))
    (dolist (spec `(("Did you know?" "Contextual Menus"        ,#'(lambda nil (y-or-n-dialog
                                                                               (format nil
                                                                                       "Did you know there are contextual menus ~
                                                                                       available - in many but not all places - ~
                                                                                       when you press control-click?"))))
                    ("Did you know?" "New TOOLS item"          ,#'(lambda nil (y-or-n-dialog
                                                                               (format nil
                                                                                       "Did you know there is a \"Choose Color\" ~
                                                                                       item added to the TOOLS menu?~%
                                                                                       (Sadly however there is no keyboard ~
                                                                                       shortcut for it and it simply prints the ~
                                                                                       chosen color in the console window.)"))))
                    ("Did you know?" "Tooltips"                ,#'(lambda nil (y-or-n-dialog
                                                                               (format nil
                                                                                       "Did you know that some sorts of view ~
                                                                                       have tooltips attached?~%~
                                                                                       (Sadly however some do not work as intended.)~%~
                                                                                       These may be fixed strings, dynamically ~
                                                                                       generated strings, or cocoa descriptions."))))
                    ("Did you know?" "Choose File menu items (not working)"
                                                               ,#'(lambda nil (y-or-n-dialog
                                                                               (format nil
                                                                                       "Did you know that there are items in the File menu ~
                                                                                       and in the Easygui Demo menu that let you use a ~
                                                                                       Choose-File-Dialog? Sadly however they do not work properly ~
                                                                                       right now and will probably crash your CCL session. ~
                                                                                       If you want to go ahead anyway, first select the ~
                                                                                       \"Run File Chooser Anyway\" item."))))
                    ("Did you know?" "Flipped screen mode"     ,#'(lambda nil (y-or-n-dialog
                                                                               (format nil
                                                                                       "Did you know that it is possible to position windows ~
                                                                                       and items within them as if screen coordinates had their ~
                                                                                       origin at screen top-left, as in Digitool's MCL?"))))
                    ("Did you know?" "Cocoa tracing"           ,#'(lambda nil (y-or-n-dialog
                                                                               (format nil
                                                                                       "Did you know that debugging messages can be ~
                                                                                       produced when Cocoa calls are made? ~
                                                                                       This relies on the DCC macro being used conscientiously, ~
                                                                                       it is not automatic."))))
                    ("Did you know?" "Font and Color support"  ,#'(lambda nil (y-or-n-dialog
                                                                               (format nil
                                                                                       "Did you know that there is some limited support for ~
                                                                                       handling text fonts and colors and backgrounds? ~
                                                                                       Try out the \"SHOUT!\" options in a demo window menu."))))
                    ("Did you know?" "Window Close behaviour"  ,#'(lambda nil (y-or-n-dialog
                                                                               (format nil
                                                                                       "Did you know that windows can be made to invoke Lisp ~
                                                                                       code when they are told to close? The primary method ~
                                                                                       for Window-Should-Close decides whether the window ~
                                                                                       should close or not, before- and after-methods could ~
                                                                                       be used for other purposes. The Demo Window behaves ~
                                                                                       differently when you close the window as part of ~
                                                                                       creating a new one, and when you press its close button."))))
                    ("Give Example" "With Button"              ,#'(lambda nil (example :button)))
                    ("Give Example" "With Radio Buttons"       ,#'(lambda nil (example :radio)))
                    ("Give Example" "With Checkbox"            ,#'(lambda nil (example :check)))
                    ("Give Example" "With Popup Menu"          ,#'(lambda nil (example :popup)))
                    ("Give Example" "With Pulldown Menu"       ,#'(lambda nil (example :pulldown)))
                    ("Give Example" "With Drawing"             ,#'(lambda nil (example :drawing)))
                    ("Give Example" "With Slider"              ,#'(lambda nil (example :slider)))
                    ("Flipping" "New windows are flipped"      ,#'(lambda nil (setf *screen-flipped* t)))
                    ("Flipping" "New windows are not flipped"  ,#'(lambda nil (setf *screen-flipped* nil)))
                    ("Tracing" "Cocoa Calls are traced"        ,#'(lambda nil (setf easygui::*debug-cocoa-calls* t)))
                    ("Tracing" "Cocoa Calls are not traced"    ,#'(lambda nil (setf easygui::*debug-cocoa-calls* nil)))
                    ("Color Picker" "Text"                     ,#'(lambda nil
                                                                    (cl-user::process-run-function "Pick color for text in box"
                                                                     #'(lambda nil
                                                                         (gui::with-autorelease-pool
                                                                             (let* ((textbox (if *w (demo-textbox *w)))
                                                                                    (color (if textbox
                                                                                             (get-fore-color textbox)
                                                                                             (make-rgb :red 0 :green 0 :blue 255))))
                                                                               (setf color (user-pick-color :color color
                                                                                              :prompt "Pick a text color"))
                                                                               (when textbox
                                                                                 (set-fore-color textbox color)
                                                                                 (invalidate-view textbox))))))))
                    ("Color Picker" "Background"               ,#'(lambda nil
                                                                    (cl-user::process-run-function "Pick color for text in box"
                                                                     #'(lambda nil
                                                                         (gui::with-autorelease-pool
                                                                             (let* ((textbox (if *w (demo-textbox *w)))
                                                                                    (color (if textbox
                                                                                             (get-back-color textbox)
                                                                                             (make-rgb :red 255 :green 220 :blue 200))))
                                                                               (setf color (user-pick-color :color color
                                                                                              :prompt "Pick a background color"))
                                                                               (when textbox
                                                                                 (set-back-color textbox color t))))))))
                    ("Destroy this menu"                       ,#'(lambda nil (remove-topbar-item (list "Easygui Demo"))))
                    ("Run File Chooser Anyway"                 ,#'(lambda nil (setf *run-file-chooser-anyway* t)))
                    ("File" "Get a pathname"                   ,#'(lambda nil
                                                                    (when *run-file-chooser-anyway*
                                                                      (print "Getting a pathname(Easygui Demo Menu)...doomed to failure!")
                                                                      (choose-file-dialog :button-string "Get a pathname(EG)"))))))
      (add-topbar-item (cons "Easygui Demo" (butlast spec)) (first (last spec))))
    (add-topbar-item '("Tools" "Choose Color")                 #'(lambda nil
                                                                   (print (user-pick-color))))
    (add-topbar-item '("File" "Get a pathname")                #'(lambda nil
                                                                   (when *run-file-chooser-anyway*
                                                                     (running-on-main-thread ()
                                                                       (print "Getting a pathname(File Menu)...doomed to failure")
                                                                       (print (choose-file-dialog :button-string "Get a pathname(FILE)"))))))
    (y-or-n-dialog "Have you spotted the new \"Easygui Demo\" item in the menubar?")))

; (easygui-demo::run-extended-demo)
