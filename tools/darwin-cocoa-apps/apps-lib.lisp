;;;; Shared helpers for darwin-cocoa-apps (loaded by 04+).
(in-package :ccl)

(defun cocoa-apps-on-main (thunk)
  "Run THUNK on *initial-process* (AppKit main thread)."
  (call-in-initial-process thunk))
