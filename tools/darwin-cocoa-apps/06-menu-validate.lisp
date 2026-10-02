;;;; 06 — NSMenu + validateMenuItem: with specials/UWP (no #/run yet).
;;;; Marker: 06-MENU-VALIDATE-OK
(in-package :ccl)
(load "ccl:tools;darwin-cocoa-apps;apps-lib.lisp")
(require "OBJC-SUPPORT")

(defvar *mv-validations* 0)
(defvar *mv-phase* :count)
(defvar *mv-cleanup* nil)
(defvar *mv-progv-ok* nil)

(defclass cocoa-apps-validator (ns:ns-object)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/validateMenuItem: :<BOOL>) ((self cocoa-apps-validator) item)
  (declare (ignore item))
  (ecase *mv-phase*
    (:count
     (incf *mv-validations*)
     (setq *mv-cleanup* nil)
     (let ((*mv-progv-ok* nil))
       (unwind-protect
            (progv '(*mv-progv-ok*) '(t)
              (unless (eq (symbol-value '*mv-progv-ok*) t)
                (error "progv bind failed"))
              t)
         (setq *mv-cleanup* :count-cleaned))))
    (:throw
     (setq *mv-cleanup* :before-throw)
     (catch 'mv-out
       (unwind-protect
            (progv '(*mv-progv-ok*) '(99)
              (throw 'mv-out nil))
         (setq *mv-cleanup* :throw-cleaned)))
     (unless (eq *mv-cleanup* :throw-cleaned)
       (error "cleanup not run: ~s" *mv-cleanup*))
     nil)))

(cocoa-apps-on-main
 (lambda ()
   (objc:with-autorelease-pool
     (#/sharedApplication ns:ns-application)
     (let* ((validator (make-instance 'cocoa-apps-validator))
            (menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"Test"))
            (item (#/initWithTitle:action:keyEquivalent:
                    (#/alloc ns:ns-menu-item)
                    #@"Action"
                    +null-ptr+
                    #@"")))
       (#/setTarget: item validator)
       (#/addItem: menu item)
       (setq *mv-validations* 0 *mv-phase* :count *mv-cleanup* nil)
       (unless (#/validateMenuItem: validator item)
         (error "validateMenuItem: expected true"))
       (unless (eql *mv-validations* 1)
         (error "validations => ~s" *mv-validations*))
       (unless (eq *mv-cleanup* :count-cleaned)
         (error "uwp marker => ~s" *mv-cleanup*))
       (setq *mv-phase* :throw)
       (when (#/validateMenuItem: validator item)
         (error "validate after throw expected false"))
       (unless (eq *mv-cleanup* :throw-cleaned)
         (error "throw cleanup marker => ~s" *mv-cleanup*))
       t))))

(format t "~&06-MENU-VALIDATE-OK~%")
(quit 0)
