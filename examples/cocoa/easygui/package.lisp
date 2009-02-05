(cl:defpackage :easygui
  (:use :cl)
  (:import-from :ccl with-autorelease-pool @selector lisp-string-from-nsstring +null-ptr+)
  (:export #:point #:range #:rectangle #:window
           #:point-x #:point-y #:rectangle-x #:rectangle-y #:rectangle-width
           #:rectangle-height
           ;; cocoa stuff
           #:retain-object #:release-object #:retaining-objects
           ;; view classes
           #:view #:static-text-view #:text-input-view #:password-input-view
           #:push-button-view
           #:form-view #:form-cell-view #:box-view #:drawing-view #:slider-view
           ;; event methods
           #:mouse-down #:mouse-dragged #:mouse-up
           ;; operators
           #:cocoa-ref
           #:add-subviews #:remove-subviews #:window-show #:set-window-title
           #:content-view
           #:initialize-view #:action #:view-text
           #:add-entry #:add-entries #:editable-p
           #:draw-view-rectangle
           #:entry-text #:cell-count #:nth-cell #:selection #:redisplay
           #:string-value-of #:integer-value-of #:float-value-of
           #:double-value-of))

(cl:defpackage :easygui-demo
  (:use :cl :easygui)
  (:export #:converter-window #:tiny-demo-window))

(cl:defpackage :easygui-user
  (:use :cl :easygui))