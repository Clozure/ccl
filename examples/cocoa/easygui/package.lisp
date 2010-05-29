(cl:defpackage :easygui
  (:use :cl)
  (:import-from :ccl with-autorelease-pool @selector lisp-string-from-nsstring +null-ptr+)
  (:import-from :gui execute-in-gui queue-for-gui)
  (:export #:point #:ns-point-from-point #:range #:rectangle #:window
           #:point-x #:point-y #:rectangle-x #:rectangle-y #:rectangle-width
           #:rectangle-height
           ;; cocoa stuff
           #:retain-object #:release-object #:retaining-objects
           ;; view classes
           #:view #:static-text-view #:text-input-view #:password-input-view
           #:push-button-view
           #:form-view #:form-cell-view #:box-view #:drawing-view #:slider-view
           #:check-box-view #:radio-button-view
           #:menu-item-view #:pop-up-menu #:pull-down-menu #:contextual-menu 
           ;; event methods
           #:mouse-down #:mouse-dragged #:mouse-up  #:view-key-event-handler
           ;; operators
           #:cocoa-ref
           #:add-subviews #:remove-subviews #:view-subviews
           #:window-show #:set-window-title
           #:content-view #:view-container
           #:initialize-view #:action #:view-text
           #:add-entry #:add-entries #:editable-p
           #:draw-view-rectangle
           #:entry-text #:cell-count #:nth-cell #:selection #:redisplay
           #:string-value-of #:integer-value-of #:float-value-of
           #:double-value-of
           #:view-named #:view-nick-name
           #:view-size #:view-position
           #:view-mouse-position
           #:view-font #:with-focused-view
           #:clear-page
           #:check-box-check #:check-box-uncheck #:check-box-checked-p
           #:radio-button-selected-p #:radio-button-select #:radio-button-deselect
           #:dialog-item-enabled-p #:set-dialog-item-enabled-p
           #:shift-key-p #:control-key-p #:alt-key-p #:command-key-p
           #:get-fore-color #:get-back-color #:set-fore-color #:set-back-color
           #:invalidate-view
           #:menu-selection #:menu-items #:set-menu-item-title #:add-contextual-menu
           #:application-main-menu
           #:navigate-menu #:navigate-topbar #:add-topbar-item
           #:make-rgb #:rgb-red #:rgb-green #:rgb-blue #:rgb-opacity
           ;; canned dialogs
           #:y-or-n-dialog #:user-pick-color
           #:choose-file-dialog #:choose-new-file-dialog #:choose-directory-dialog          
         
           #:dcc
           #:perform-close #:window-may-close

	   #:execute-in-gui
	   #:queue-for-gui
           ;; variables
           #:*screen-flipped*
           #:*suppress-window-flushing*))

(cl:defpackage :easygui-demo
  (:use :cl :easygui)
  (:export #:converter-window #:tiny-demo-window))

(cl:defpackage :easygui-user
  (:use :cl :easygui))
