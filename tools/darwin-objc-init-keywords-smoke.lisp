;;;; make-instance :with-frame must call initWithFrame: (instancetype inits).
;;;;   ./darm64cl --no-init --batch < tools/darwin-objc-init-keywords-smoke.lisp
(in-package :ccl)
(setq *warn-if-redefine-kernel* nil)
(require "OBJC-SUPPORT")
(use-interface-dir :cocoa)

;; Reload tip process-init-message + send-init if image is stale.
(let* ((src (merge-pathnames "objc-bridge/objc-support.lisp" (ccl-directory)))
       (wanted '(objc-init-result-type-p process-init-message
                 send-init-message-for-class)))
  (with-open-file (s src)
    (loop for f = (read s nil s)
          until (eq f s)
          when (and (consp f) (eq (car f) 'defun) (member (cadr f) wanted))
          do (eval f))))

(clrhash *class-init-keywords*)
(register-objc-init-messages)

(assert (objc-init-result-type-p :id))
(assert (objc-init-result-type-p :instancetype))
(assert (not (objc-init-result-type-p :void)))

(let ((keys (all-init-keywords-for-class (find-class 'ns:ns-view))))
  (assert (find :with-frame keys :key #'car :test #'member)
          () "initWithFrame: not registered for ns:ns-view: ~s" keys))

(objc:with-autorelease-pool
  (let* ((r (ns:make-ns-rect 10d0 20d0 30d0 40d0))
         (v (make-instance 'ns:ns-view :with-frame r))
         (f (#/frame v)))
    (assert (= 10d0 (ns:ns-rect-x f)))
    (assert (= 20d0 (ns:ns-rect-y f)))
    (assert (= 30d0 (ns:ns-rect-width f)))
    (assert (= 40d0 (ns:ns-rect-height f)))))

(format t "~&DARWIN-OBJC-INIT-KEYWORDS-OK~%")
(quit 0)
