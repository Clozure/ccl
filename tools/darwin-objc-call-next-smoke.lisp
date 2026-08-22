;;;; Darwin/arm64: ObjC Lisp #/init + call-next-method + BOOL CNM smoke.
;;;; Requires tip %throwing-through-cleanup-p + heap objc_super +
;;;; funcall-by-arity %call-next (no APPLY onto send-fn).
;;;;
;;;;   ./darm64cl --no-init --batch < tools/darwin-objc-call-next-smoke.lisp
(in-package :ccl)
(setq *warn-if-redefine-kernel* nil)

(unless (fboundp '%throwing-through-cleanup-p)
  (error "missing %throwing-through-cleanup-p"))

(require "OBJC-SUPPORT")
(load "ccl:tools;cnm-funcall-defs.lisp")

;; Compiled normal UWP must not look like a throw (propagate-throw false positive).
(defun %cnm-smoke-uwp ()
  (unwind-protect 'ok
    (when (%throwing-through-cleanup-p)
      (error "compiled normal uwp falsely throwing: ~s"
             (%throwing-through-cleanup-p)))))
(%cnm-smoke-uwp)
(format t "~&compiled-normal-uwp=NIL~%")

(defclass cnm-smoke (ns:ns-object) () (:metaclass ns:+ns-object))
(objc:defmethod #/init ((self cnm-smoke))
  (%call-next-objc-method-apply self (@class "CnmSmoke")
                                 (@selector "init") '(:id) '()))

(let ((o (make-instance 'cnm-smoke)))
  (format t "~&cnm-init => ~s~%" o)
  (unless (typep o 'cnm-smoke)
    (error "make-instance cnm-smoke => ~s" o)))

(defclass cnm-smoke-bool0 (ns:ns-object) () (:metaclass ns:+ns-object))
(objc:defmethod (#/okp :<BOOL>) ((self cnm-smoke-bool0)) t)
(defclass cnm-smoke-bool1 (cnm-smoke-bool0) () (:metaclass ns:+ns-object))
(objc:defmethod (#/okp :<BOOL>) ((self cnm-smoke-bool1))
  (%call-next-objc-method-apply self (@class "CnmSmokeBool1")
                                 (@selector "okp") '(:<BOOL>) '()))

(let ((r (#/okp (#/init (#/alloc cnm-smoke-bool1)))))
  (format t "~&cnm-bool => ~s~%" r)
  (unless (eq r t) (error "bool CNM => ~s" r)))

(defclass cnm-smoke-void (ns:ns-object) () (:metaclass ns:+ns-object))
(objc:defmethod (#/cnmPing :void) ((self cnm-smoke-void))
  nil)
(#/cnmPing (#/alloc cnm-smoke-void))
(format t "~&void-callback ok~%")

(format t "~&DARWIN-OBJC-CALL-NEXT-SMOKE-OK~%")
(quit 0)
