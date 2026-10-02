;;;; 10 — IDE-shaped menu validation (stock call-next-method flet).
;;;; Marker: 10-IDE-SHAPED-VALIDATE-OK
(in-package :ccl)
(load "ccl:tools;darwin-cocoa-apps;apps-lib.lisp")
(require "OBJC-SUPPORT")

(unless (fboundp '%invoke-objc-send-function)
  (error "image missing tip CNM; run tools/install-cnm-funcall-into-image.lisp"))

;;; Base returns T (like NSDocument/NSResponder defaults in the IDE chain).
(defclass ide10-base (ns:ns-object) () (:metaclass ns:+ns-object))
(objc:defmethod (#/validateMenuItem: :<BOOL>) ((self ide10-base) item)
  (declare (ignore item))
  t)

;;; Listener-doc shape: opinion or CNM.
(defclass ide10-doc (ide10-base) () (:metaclass ns:+ns-object))
(objc:defmethod (#/validateMenuItem: :<BOOL>) ((self ide10-doc) item)
  (let ((action (#/action item)))
    (cond ((eql action (@selector "copy:"))
           t)
          (t (call-next-method item)))))

;;; App shape: special-case then CNM.
(defclass ide10-app (ide10-base) () (:metaclass ns:+ns-object))
(objc:defmethod (#/validateMenuItem: :<BOOL>) ((self ide10-app) item)
  (let ((action (#/action item)))
    (cond ((eql action (@selector "toggleConsole:"))
           t)
          (t (call-next-method item)))))

(defclass ide10-app-sub (ide10-app) () (:metaclass ns:+ns-object))
(objc:defmethod (#/validateMenuItem: :<BOOL>) ((self ide10-app-sub) item)
  (call-next-method item))

(handler-case
    (progn
      (cocoa-apps-on-main
       (lambda ()
         (objc:with-autorelease-pool
           (#/sharedApplication ns:ns-application)
           (let* ((doc (make-instance 'ide10-doc))
                  (app (make-instance 'ide10-app-sub))
                  (menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"File"))
                  (item (#/initWithTitle:action:keyEquivalent:
                          (#/alloc ns:ns-menu-item) #@"Open" +null-ptr+ #@"")))
             (#/setAutoenablesItems: menu #$YES)
             (#/setTarget: item doc)
             (#/addItem: menu item)
             (let ((rd (#/validateMenuItem: doc item))
                   (ra (#/validateMenuItem: app item)))
               (format t "~&10 doc => ~s app => ~s~%" rd ra)
               (finish-output)
               (unless (eq rd t) (error "doc => ~s" rd))
               (unless (eq ra t) (error "app => ~s" ra)))
             (#/update menu)
             (format t "~&10 menu update ok~%")
             (finish-output)
             t))))
      (format t "~&10-IDE-SHAPED-VALIDATE-OK~%")
      (quit 0))
  (serious-condition (c)
    (format t "~&10-FAIL: ~a~%" c)
    (finish-output)
    (quit 1)))
