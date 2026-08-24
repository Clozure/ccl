;;;; 09 — Lisp→Lisp ObjC call-next-method (IDE validateMenuItem: death).
;;;;
;;;; Cause: (apply send-fn …) / (apply #'%call-next-objc-method …) on arm64
;;;; corrupts nested ff-call scalar returns:
;;;;   :<BOOL> → coerce-from-bool garbage or 0
;;;;   then CLASS-CELL-TYPEP FAR #x30000000015BF4 (File-menu death).
;;;; Fix: heap objc_super + FUNCALL-by-arity; objc:defmethod flet passes the
;;;; &rest list as one arg to %-apply (bridge.lisp / objc-runtime.lisp).
;;;;
;;;; Marker: 09-OBJC-CNM-OK
(in-package :ccl)
(setq *warn-if-redefine-kernel* nil)
(load "ccl:tools;darwin-cocoa-apps;apps-lib.lisp")
(require "OBJC-SUPPORT")
(load "ccl:tools;cnm-funcall-defs.lisp")

(defclass cnm09-super (ns:ns-object) () (:metaclass ns:+ns-object))
(objc:defmethod (#/validateMenuItem: :<BOOL>) ((self cnm09-super) item)
  (declare (ignore item))
  t)

(defclass cnm09-sub (cnm09-super) () (:metaclass ns:+ns-object))
(objc:defmethod (#/validateMenuItem: :<BOOL>) ((self cnm09-sub) item)
  (%call-next-objc-method-apply self (@class "Cnm09Sub")
                                 (@selector "validateMenuItem:")
                                 '(:<BOOL> :id)
                                 (list item)))

(defclass cnm09-b0 (ns:ns-object) () (:metaclass ns:+ns-object))
(objc:defmethod (#/okp :<BOOL>) ((self cnm09-b0)) t)
(defclass cnm09-b1 (cnm09-b0) () (:metaclass ns:+ns-object))
(objc:defmethod (#/okp :<BOOL>) ((self cnm09-b1))
  (%call-next-objc-method-apply self (@class "Cnm09B1")
                                 (@selector "okp")
                                 '(:<BOOL>)
                                 '()))

(defclass cnm09-i0 (ns:ns-object) () (:metaclass ns:+ns-object))
(objc:defmethod (#/ident :id) ((self cnm09-i0)) self)
(defclass cnm09-i1 (cnm09-i0) () (:metaclass ns:+ns-object))
(objc:defmethod (#/ident :id) ((self cnm09-i1))
  (%call-next-objc-method-apply self (@class "Cnm09I1")
                                 (@selector "ident")
                                 '(:id)
                                 '()))

(defvar *cnm09-void-hits* 0)
(defclass cnm09-v0 (ns:ns-object) () (:metaclass ns:+ns-object))
(objc:defmethod (#/bump :void) ((self cnm09-v0))
  (incf *cnm09-void-hits*))
(defclass cnm09-v1 (cnm09-v0) () (:metaclass ns:+ns-object))
(objc:defmethod (#/bump :void) ((self cnm09-v1))
  (incf *cnm09-void-hits*)
  (%call-next-objc-method-apply self (@class "Cnm09V1")
                                 (@selector "bump")
                                 '(:void)
                                 '()))

(handler-case
    (progn
      (cocoa-apps-on-main
       (lambda ()
         (objc:with-autorelease-pool
           (#/sharedApplication ns:ns-application)
           (let* ((o (make-instance 'cnm09-sub))
                  (item (#/initWithTitle:action:keyEquivalent:
                          (#/alloc ns:ns-menu-item) #@"Y" +null-ptr+ #@""))
                  (r (#/validateMenuItem: o item)))
             (format t "~&09 validateMenuItem => ~s~%" r)
             (finish-output)
             (unless (eq r t) (error "validateMenuItem => ~s" r)))
           (let ((r (#/okp (#/init (#/alloc cnm09-b1)))))
             (format t "~&09 bool0 => ~s~%" r)
             (finish-output)
             (unless (eq r t) (error "bool0 => ~s" r)))
           (let* ((o (#/init (#/alloc cnm09-i1)))
                  (r (#/ident o)))
             (format t "~&09 id ptr-eql=~s~%" (%ptr-eql o r))
             (finish-output)
             (unless (%ptr-eql o r) (error "id not ptr-eql")))
           (setq *cnm09-void-hits* 0)
           (#/bump (#/init (#/alloc cnm09-v1)))
           (format t "~&09 void hits=~s~%" *cnm09-void-hits*)
           (finish-output)
           (unless (= *cnm09-void-hits* 2) (error "void hits=~s" *cnm09-void-hits*))
           t)))
      (format t "~&09-OBJC-CNM-OK~%")
      (quit 0))
  (serious-condition (c)
    (format t "~&09-FAIL: ~a~% type=~s~%" c (type-of c))
    (finish-output)
    (quit 1)))
