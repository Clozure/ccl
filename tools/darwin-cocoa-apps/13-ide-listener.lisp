;;;; 13 — IDE Listener create after launch (stepped).
;;;; Marker: IDE-LISTENER-OK
;;;;
;;;; Relies on foreign-types NSRange typedef→record completion (see
;;;; tools/darwin-nsrange-foreign-type-smoke.lisp). Optional targeted
;;;; precompile below; avoid mass eager-compile after COCOA.
;;;;
;;;;   ./tools/run-darwin-ide-smoke.sh 120 tools/darwin-cocoa-apps/13-ide-listener.lisp IDE-LISTENER-OK
(in-package :ccl)

(defun %ide13-log (fmt &rest args)
  (apply #'format t fmt args) (terpri) (force-output)
  (with-open-file (s "/tmp/ide-listener-13.log" :direction :output
                     :if-exists :append :if-does-not-exist :create)
    (apply #'format s fmt args) (terpri s) (force-output s)))

(ignore-errors (delete-file "/tmp/ide-listener-13.log"))

(unless (fboundp '%invoke-objc-send-function)
  (error "missing tip CNM"))

(defun %ide13-install-send (sig)
  "Compile SIG; install on interned signature-info only (methods share it)."
  (let* ((f (compile-send-function-for-signature sig))
         (info (objc-method-signature-info sig)))
    (setf (objc-method-signature-info-function info) f)
    f))

(defun %ide13-precompile-listener-sends ()
  "Signatures are (return . msg-arg-types) — no implicit :id/<SEL>."
  (dolist (sig '((:void :<NSI>nteger :<NSI>nteger :<NSI>nteger)
                 (:void :<NSUI>nteger (:struct :<NSR>ange) :<NSI>nteger)
                 (:void :<NSUI>nteger :<NSR>ange :<NSI>nteger)
                 (:<NSR>ange :<NSR>ect (:* (:struct :<NST>ext<C>ontainer)))
                 (:<NSR>ange :<NSR>ange (:* :<NSR>ange))
                 (:<NSR>ange (:* (:struct :<NSS>tring)))))
    (handler-case
        (%ide13-log "precompile ~s numreq=~s"
                    sig
                    (ldb $lfbits-numreq (lfun-bits (%ide13-install-send sig))))
      (error (c) (%ide13-log "precompile fail ~s: ~a" sig c)))))

(defun %ide13-listener-probe ()
  (objc:with-autorelease-pool
    (#/setActivationPolicy: *nsapp* 0)
    (#/activateIgnoringOtherApps: *nsapp* #$YES)
    (%ide13-log "step0 windows=~s" (#/count (#/orderedWindows *nsapp*)))
    (let* ((dc (#/sharedDocumentController ns:ns-document-controller))
           (cls (symbol-value (intern "HEMLOCK-LISTENER-DOCUMENT" "GUI"))))
      (%ide13-log "step1 makeUntitled…")
      (let ((doc (#/makeUntitledDocumentOfType:error: dc #@"Listener" +null-ptr+)))
        (%ide13-log "step1 doc null=~s class=~s"
                    (%null-ptr-p doc)
                    (unless (%null-ptr-p doc) (#/className doc)))
        (when (%null-ptr-p doc) (error "makeUntitled returned null"))
        (%ide13-log "step2 addDocument…")
        (#/addDocument: dc doc)
        (%ide13-log "step2 ok")
        (%ide13-log "step3 makeWindowControllers…")
        (handler-case
            (#/makeWindowControllers doc)
          (error (c)
            (%ide13-log "step3 ERROR: ~a" c)
            (ignore-errors
              (with-open-file (s "/tmp/ide-listener-13-bt.log" :direction :output
                                 :if-exists :supersede :if-does-not-exist :create)
                (let ((*standard-output* s) (*error-output* s))
                  (print-call-history :count 40 :detailed-p nil))))
            (error c)))
        (%ide13-log "step3 ok")
        (let* ((wcs (#/windowControllers doc))
               (wc (#/lastObject wcs))
               (w (if (%null-ptr-p wc) +null-ptr+ (#/window wc))))
          (%ide13-log "step4 controllers=~s wc-null=~s w-null=~s"
                      (#/count wcs) (%null-ptr-p wc) (%null-ptr-p w))
          (unless (%null-ptr-p w)
            (#/makeKeyAndOrderFront: w (%null-ptr))
            (%ide13-log "step4 visible=~s" (#/isVisible w))))
        (%ide13-log "step5 topListener null=~s"
                    (%null-ptr-p (#/topListener cls)))
        (%ide13-log "step5 windows=~s" (#/count (#/orderedWindows *nsapp*)))))
    t))

(%ide13-log "require COCOA…")
(require "COCOA")
(%ide13-log "modules loaded; wait finished-launching…")

(let ((ok (timed-wait-on-semaphore gui::*cocoa-ide-finished-launching* 45)))
  (%ide13-log "finished-launching => ~s" ok)
  (unless ok (error "IDE did not finish launching")))

(%ide13-precompile-listener-sends)

(call-in-initial-process #'%ide13-listener-probe)

(format t "~&IDE-LISTENER-OK~%")
(force-output)
;; Creating Listener windows leaves NSApp running; plain QUIT can hang the
;; batch process.  Hard-exit after the marker so the smoke harness sees it.
(#_exit 0)
