;;;; 08 — Menu open path: NSMenu #/update (AppKit's pre-track validation).
;;;; This is the validation wave that runs when a menu bar menu opens — the
;;;; surface that historically killed darwinarm64 IDE on File click.
;;;; Marker: 08-MENU-TRACKING-OK
(in-package :ccl)
(load "ccl:tools;darwin-cocoa-apps;apps-lib.lisp")
(require "OBJC-SUPPORT")

(defvar *mt-ticks* 0)
(defvar *mt-validated* 0)

(defclass cocoa-apps-menu-target (ns:ns-object)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/validateMenuItem: :<BOOL>) ((self cocoa-apps-menu-target) item)
  (declare (ignore item))
  (incf *mt-validated*)
  (let ((*mt-ticks* *mt-ticks*))
    (unwind-protect
         (progv '(*mt-ticks*) (list (1+ *mt-ticks*))
           t)
      nil)))

(objc:defmethod (#/menuAction: :void) ((self cocoa-apps-menu-target) sender)
  (declare (ignore sender))
  (incf *mt-ticks*))

(cocoa-apps-on-main
 (lambda ()
   (objc:with-autorelease-pool
     (let* ((app (#/sharedApplication ns:ns-application))
            (target (make-instance 'cocoa-apps-menu-target))
            (main (#/initWithTitle: (#/alloc ns:ns-menu) #@"MainMenu"))
            (file-item (#/initWithTitle:action:keyEquivalent:
                         (#/alloc ns:ns-menu-item) #@"File" +null-ptr+ #@""))
            (file-menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"File"))
            (action (#/initWithTitle:action:keyEquivalent:
                      (#/alloc ns:ns-menu-item)
                      #@"DoIt"
                      (@selector #/menuAction:)
                      #@""))
            (rl (#/currentRunLoop ns:ns-run-loop))
            (until (#/dateWithTimeIntervalSinceNow: ns:ns-date 0.2d0)))
       (#/setActivationPolicy: app 1)
       (#/setAutoenablesItems: file-menu #$YES)
       (#/setTarget: action target)
       (#/addItem: file-menu action)
       (#/setSubmenu:forItem: main file-menu file-item)
       (#/addItem: main file-item)
       (#/setMainMenu: app main)

       ;; Direct validate (baseline).
       (setq *mt-validated* 0)
       (dotimes (i (#/numberOfItems file-menu))
         (#/validateMenuItem: target (#/itemAtIndex: file-menu i)))
       (unless (> *mt-validated* 0)
         (error "direct validate never called"))

       ;; Menu-open path: #/update walks items and calls validateMenuItem:.
       (setq *mt-validated* 0)
       (#/update file-menu)
       (#/runMode:beforeDate: rl #&NSDefaultRunLoopMode until)

       (format t "~&validated-via-update=~s~%" *mt-validated*)
       (unless (> *mt-validated* 0)
         (error "NSMenu update did not validate items"))

       ;; Activate the item (post-validation action send).
       (#/performActionForItemAtIndex: file-menu 0)
       (unless (> *mt-ticks* 0)
         (error "menuAction: not invoked"))
       t))))

(format t "~&08-MENU-TRACKING-OK~%")
(quit 0)
