;;;; REGRESSION smoke for the (fixed) GC-invisible ff-call state bug.
;;;;
;;;;   ./tools/run-darwin-ide-smoke.sh 240 tools/darwin-launch-layout-repro.lisp LAUNCH-LAYOUT-OK
;;;;
;;;; Historically (2026-08, before the .SPffcall fix) this exact load
;;;; sequence died during (require "COCOA") on affected heap layouts:
;;;;   TYPE-ERROR: #<BOGUS object @ #x3020....DDEC> is not ... MACPTR
;;;;   in RELEASE-AUTORELEASE-POOL, process Initial(0)
;;;; The same failure family: fresh-IDE "can't determine class of object
;;;; tag=4 typecode=76 bogus=T", package-htab symbol corruption, and
;;;; intermittent "GC: object claims N suffix dnodes - corrupt uvector
;;;; header" aborts.
;;;;
;;;; ROOT CAUSE (fixed in lisp-kernel/arm64-spentry.s .SPffcall):
;;;;   * the raw return PC parked on the vstack was parsed by the vstack
;;;;     walkers as an ivector HEADER (immheader fulltag), making mark
;;;;     and forward skip a bogus multi-GB "ivector" — every older
;;;;     vstack slot invisible to the GC (now parked fixnum-boxed);
;;;;   * save0-save2 were not spilled before going foreign, so boxed
;;;;     values there were invisible while any GC ran (now spilled).
;;;;
;;;; The failure was heap-layout-sensitive (any extra toplevel form
;;;; hid it), which is why it presented as an intermittent heisenbug.
;;;; Keep this file byte-stable: the defun before the require is part
;;;; of the layout that reproduced the original bug.

(in-package :ccl)
(defun %p (fmt &rest args) (apply #'format t fmt args) (terpri) (force-output))
(require "COCOA")
(timed-wait-on-semaphore gui::*cocoa-ide-finished-launching* 60)
(%p "cocoa up")
(%p "LAUNCH-LAYOUT-OK")
(#_exit 0)
