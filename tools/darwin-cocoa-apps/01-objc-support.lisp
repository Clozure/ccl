;;;; 01 — OBJC-SUPPORT loads and NSString round-trips.
;;;; No AppKit event loop. Marker: 01-OBJC-SUPPORT-OK
(in-package :ccl)
(require "OBJC-SUPPORT")
(unless (and (find-package "NS") (find-package "OBJC"))
  (error "NS/OBJC packages missing after OBJC-SUPPORT"))
(let* ((s (%make-nsstring "darwinarm64-cocoa-apps"))
       (len (#/length s)))
  (unless (eql len 22)
    (error "NSString length => ~s" len)))
(format t "~&01-OBJC-SUPPORT-OK~%")
(quit 0)
