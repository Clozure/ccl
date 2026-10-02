;;;; Shared tip defs: CNM without APPLY onto send-fn or %call-next.
(in-package :ccl)
(setq *warn-if-redefine-kernel* nil)

(defun %invoke-objc-send-function (function receiver selector args)
  (let ((n (length args)))
    (declare (fixnum n))
    (case n
      (0 (funcall function receiver selector))
      (1 (funcall function receiver selector (car args)))
      (2 (funcall function receiver selector (car args) (cadr args)))
      (3 (funcall function receiver selector
                  (car args) (cadr args) (caddr args)))
      (4 (funcall function receiver selector
                  (car args) (cadr args) (caddr args) (cadddr args)))
      (5 (funcall function receiver selector
                  (car args) (cadr args) (caddr args) (cadddr args)
                  (nth 4 args)))
      (6 (funcall function receiver selector
                  (car args) (cadr args) (caddr args) (cadddr args)
                  (nth 4 args) (nth 5 args)))
      (t (error "%invoke-objc-send-function: ~d args not supported" n)))))

(defun %call-next-objc-method-apply (self class selector sig args)
  (let* ((args (if (listp args) (copy-list args) (list args)))
         (siginfo (objc-method-signature-info sig))
         (function (or (objc-method-signature-info-super-function siginfo)
                       (setf (objc-method-signature-info-super-function siginfo)
                             (%compile-send-function-for-signature sig t))))
         (s (make-record :objc_super
                         :receiver self
                         :super_class (#_class_getSuperclass class))))
    (unwind-protect
         (with-ns-exceptions-as-errors
           (%invoke-objc-send-function function s selector args))
      (free s))))

(defun %call-next-objc-method (self class selector sig &rest args)
  (%call-next-objc-method-apply self class selector sig args))

(defun %call-next-objc-class-method-apply (self class selector sig args)
  (let* ((args (if (listp args) (copy-list args) (list args)))
         (siginfo (objc-method-signature-info sig))
         (function (or (objc-method-signature-info-super-function siginfo)
                       (setf (objc-method-signature-info-super-function siginfo)
                             (%compile-send-function-for-signature sig t))))
         (s (make-record :objc_super
                         :receiver self
                         :super_class
                         (#_class_getSuperclass (#_object_getClass class)))))
    (unwind-protect
         (with-ns-exceptions-as-errors
           (%invoke-objc-send-function function s selector args))
      (free s))))

(defun %call-next-objc-class-method (self class selector sig &rest args)
  (%call-next-objc-class-method-apply self class selector sig args))
