;;;; Darwin/arm64: HFA *argument* packing for ObjC sends (NSPoint/NSRect).
;;;;   ./tools/run-darwin-ide-smoke.sh 90 tools/darwin-hfa-arg-smoke.lisp DARWIN-HFA-ARG-OK
(in-package :ccl)
(setq *warn-if-redefine-kernel* nil)
(format t "~&;; darwin-hfa-arg-smoke~%")
(finish-output)

(defun %p (fmt &rest a)
  (apply #'format t fmt a) (terpri) (force-output))

(defun flat-ff-call (form)
  (cond ((atom form) nil)
        ((eq (car form) '%ff-call) form)
        (t (or (flat-ff-call (car form))
               (flat-ff-call (cdr form))))))

(defun count-reps (ff type)
  ;; Counts FP field slots for TYPE: either per-field specs (:double-float
  ;; value ...) or the atomic HFA spec '(:double-float . N).
  (loop for (a . rest) on (cddr ff) by #'cddr
        while rest
        sum (cond ((eq a type) 1)
                  ((and (consp a) (eq (car a) 'quote)
                        (consp (cadr a)) (eq (car (cadr a)) type))
                   (cdr (cadr a)))
                  (t 0))))

(require "COCOA")
(%p "finished=~s" (timed-wait-on-semaphore gui::*cocoa-ide-finished-launching* 60))

;; 1) Macroexpansion must expand HFAs to N float loads.
(let* ((pt-ff (flat-ff-call
               (macroexpand-1
                '(objc:objc-message-send s "drawAtPoint:withAttributes:"
                  :<NSP>oint p :id attrs :void))))
       (rect-ff (flat-ff-call
                 (macroexpand-1
                  '(objc:objc-message-send s "drawInRect:withAttributes:"
                    :<NSR>ect r :id attrs :void))))
       (pt-d (count-reps pt-ff :double-float))
       (rect-d (count-reps rect-ff :double-float)))
  (%p "expand drawAtPoint doubles=~s" pt-d)
  (%p "expand drawInRect doubles=~s" rect-d)
  (unless (and (>= pt-d 2) (>= rect-d 4))
    (error "HFA expand failed")))

;; 2) Live #/ signature send-fn expansion.
(let* ((info (get-objc-message-info "drawAtPoint:withAttributes:"))
       (sig (objc-method-info-signature (car (objc-message-info-methods info))))
       (f (compile-send-function-for-signature sig))
       (args (objc-gen-message-arglist (length (cdr sig))))
       (body (let* ((return-type-spec (car sig))
                    (arg-type-specs (cdr sig))
                    (receiver (gensym))
                    (selector (gensym))
                    (call ()))
                (do ((a args (cdr a))
                     (spec (pop arg-type-specs) (pop arg-type-specs)))
                    ((null a) (push return-type-spec call))
                  (push spec call)
                  (push (car a) call))
                (message-send-form-for-call
                 receiver selector (nreverse call) nil nil)))
       (ff (flat-ff-call (macroexpand-1 body)))
       (doubles (count-reps ff :double-float)))
  (declare (ignore f))
  (%p "sig=~s doubles=~s" sig doubles)
  (unless (>= doubles 2)
    (error "send-fn HFA expand failed for ~s" sig)))

;; 3) Runtime on Cocoa event thread: offscreen NSImage lockFocus.
(call-in-initial-process
 #'(lambda ()
     (objc:with-autorelease-pool
       (let* ((img (#/initWithSize:
                    (#/alloc ns:ns-image)
                    (ns:make-ns-size 200.0d0 40.0d0))))
         (#/lockFocus img)
         (let* ((s (#/autorelease (%make-nsstring "HFA")))
                (attrs (#/dictionary ns:ns-dictionary))
                (pt (ns:make-ns-point 5.0d0 10.0d0))
                (r (ns:make-ns-rect 0.0d0 0.0d0 100.0d0 20.0d0)))
           (%p "runtime make-ns-point x=~s y=~s"
               (ns:ns-point-x pt) (ns:ns-point-y pt))
           (%p "runtime drawAtPoint…")
           (#/drawAtPoint:withAttributes: s pt attrs)
           (%p "runtime drawAtPoint ok")
           (%p "runtime drawInRect…")
           (#/drawInRect:withAttributes: s r attrs)
           (%p "runtime drawInRect ok")
           ;; Explicit objc-message-send (same expand-ff-call path).
           (%p "runtime explicit objc-message-send drawAtPoint…")
           (objc:objc-message-send s "drawAtPoint:withAttributes:"
                                   :<NSP>oint pt :id attrs :void)
           (%p "runtime explicit ok"))
         (#/unlockFocus img)
         (#/release img)))))

(format t "~&DARWIN-HFA-ARG-OK~%")
(force-output)
(#_exit 0)
