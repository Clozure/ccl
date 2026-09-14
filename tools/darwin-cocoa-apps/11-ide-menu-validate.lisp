;;;; IDE File-menu diagnostic — probe compiled before require (messages already in tip image).
;;;; Marker: IDE-MENU-VALIDATE-OK
;;;;
;;;;   ./tools/run-darwin-ide-smoke.sh 90 tools/darwin-cocoa-apps/11-ide-menu-validate.lisp IDE-MENU-VALIDATE-OK
;;;;
;;;; Note: do not compile large #/ lambdas *after* (require "COCOA") on arm64 —
;;;; that path hits corrupt-uvector GC aborts.  Pre-define the probe; only FUNCALL
;;;; it after finished-launching.
(in-package :ccl)

(defun %ide11-log (fmt &rest args)
  (apply #'format t fmt args) (terpri) (force-output)
  (with-open-file (s "/tmp/ide-menu-11.log" :direction :output
                     :if-exists :append :if-does-not-exist :create)
    (apply #'format s fmt args) (terpri s) (force-output s)))

(ignore-errors (delete-file "/tmp/ide-menu-11.log"))

(unless (fboundp '%invoke-objc-send-function)
  (error "missing tip CNM"))

(defun %ide11-menu-probe ()
  (objc:with-autorelease-pool
    (#/setActivationPolicy: *nsapp* 0)
    (#/activateIgnoringOtherApps: *nsapp* #$YES)
    (%ide11-log "nsapp=~s running=~s menu-items=~s"
                *nsapp*
                (#/isRunning *nsapp*)
                (#/numberOfItems (#/mainMenu *nsapp*)))
    (%ide11-log "windows=~s" (#/count (#/orderedWindows *nsapp*)))
    (let* ((main (#/mainMenu *nsapp*))
           (file nil))
      (dotimes (i (#/numberOfItems main))
        (let* ((item (#/itemAtIndex: main i))
               (title (lisp-string-from-nsstring (#/title item)))
               (sub (#/submenu item)))
          (when (and (not (%null-ptr-p sub))
                     (search "File" title :test #'char-equal))
            (setq file sub)
            (return))))
      (unless file (error "no File submenu"))
      (%ide11-log "File items=~s" (#/numberOfItems file))
      (dotimes (i (#/numberOfItems file))
        (let* ((item (#/itemAtIndex: file i))
               (sep (#/isSeparatorItem item))
               (title (if sep "<sep>"
                        (lisp-string-from-nsstring (#/title item))))
               (target (if sep +null-ptr+ (#/target item))))
          (%ide11-log "[~d] ~s tgt-null=~s" i title (%null-ptr-p target))
          (unless (or sep (%null-ptr-p target))
            (when (#/respondsToSelector: target
                    (@selector "validateMenuItem:"))
              (let ((r (#/validateMenuItem: target item)))
                (%ide11-log "  validate => ~s" r))))))
      (%ide11-log "#/update…")
      (#/update file)
      (%ide11-log "#/update ok"))
    t))

(%ide11-log "require COCOA…")
(require "COCOA")
(%ide11-log "modules loaded; wait finished-launching…")

(let ((ok (timed-wait-on-semaphore gui::*cocoa-ide-finished-launching* 45)))
  (%ide11-log "finished-launching => ~s" ok)
  (unless ok (error "IDE did not finish launching")))

(call-in-initial-process #'%ide11-menu-probe)

(format t "~&IDE-MENU-VALIDATE-OK~%")
(quit 0)
